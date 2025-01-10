{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
{                                                         }
{        Originally written by Jan Baumgarten             }
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
{  http://zeoslib.sourceforge.net  (FORUM)                }
{  http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER) }
{  http://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{  http://www.sourceforge.net/projects/zeoslib.           }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcDuckDBResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit
uses
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types{$IFNDEF NO_UNIT_CONTNRS}, Contnrs{$ENDIF}{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZPlainDuckDb, {ZSysUtils,} ZDbcIntfs, ZDbcResultSet, ZDbcLogging,{$IFDEF ZEOS73UP}FmtBCD, ZVariant, {$ENDIF}
  ZDbcResultSetMetadata, ZCompatibility;

type
  {** Implements DBC Layer Proxy ResultSet. }
  TZDbcDuckDBResultSet = class(TZAbstractReadOnlyResultSet, IZResultSet)
  private
    FPlainDriver: TZDuckDBPlainDriver;
    FResult: TDuckDB_Result;
    FByteBuffer: TBytes;
  protected
    {$IFNDEF NEXTGEN}
    FAnsiBuffer: AnsiString;
    {$ENDIF}
    FWideBuffer: ZWideString;
    FStringBuffer: UTF8String;
    /// <summary>
    ///  Opens this recordset.
    /// </summary>
    procedure Open; override;
  public
    /// <summary>
    ///  Constructs this object, assignes main properties and
    ///  opens the record set.
    /// </summary>
    /// <param name="Connection">
    ///  The DBC Proxy connection interface that returned the result set data.
    /// </param>
    /// <param name="SQL">
    ///  The SQL String that generated the result set.
    /// </param>
    /// <param name="ResultStr">
    ///  A string containing the XML exncoded result set.
    /// </param>
    constructor Create(Const Statement: IZStatement; const Connection: IZConnection; const SQL: string; const Result: TDuckDB_Result);
    /// <summary>
    ///  Indicates if the value of the designated column in the current row
    ///  of this ResultSet object is Null.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  if the value is SQL NULL, the
    ///  value returned is true. false otherwise.
    /// </returns>
    function IsNull(ColumnIndex: Integer): Boolean;
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>PAnsiChar</c> in the Delphi
    ///   programming language.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <param name="Len">
    ///  the Length of the PAnsiChar String
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>NULL</c>, the value returned is <c>nil</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a PWideChar.
    /// </summary>
    /// <param name="columnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <param name="Len">
    ///  the Length of the PWideChar String in Words.
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>NULL</c>, the value returned is <c>nil</c>.
    ///  Also <c>LastWasNull</c> is set accordingly.
    /// </returns>
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>String</c>.
    /// </summary>
    /// <param name="columnIndex">
    ///  first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>null</c>, the value returned is <c>''</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetString(ColumnIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as an <c>AnsiString</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///  first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>null</c>, the value returned is <c>''</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>UTF8String</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///  first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  the column value. If the value is SQL <c>null</c>, the value returned is <c>''</c>
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>UTF8String</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///   the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>''</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    /// <summary>
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a <c>String</c> in the Delphi programming
    ///  language.
    /// </summary>
    /// <param name="columnIndex">
    ///   the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>''</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetBinaryString(ColumnIndex: Integer): RawByteString;
    /// <summary>
    ///  Gets the value of the designated column in the current row of this <c>ResultSet</c> object as a <c>UnicodeString</c> in the Delphi programming language.
    /// </summary>
    /// <param name="columnIndex">
    ///   the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>''</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetUnicodeString(ColumnIndex: Integer): ZWideString;
    /// <summary>
    /// Gets the value of the designated column in the current row of this
    /// <c>ResultSet</c> object as a <c>Boolean</c>.
    /// </summary>
    /// <param name="ColumnIndex">
    ///  the first column is 1, the second is 2, ...
    /// </param>
    /// <returns>
    ///  The column value. If the value is SQL <c>NULL</c>, the value returned is <c>false</c>.
    ///  Also <c>LastWasNull</c> is set.
    /// </returns>
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    function GetBigDecimal(ColumnIndex: Integer): TBCD; overload;
    function GetBytes(ColumnIndex: Integer): TBytes; overload;
    function GetDate(ColumnIndex: Integer): TDateTime; overload;
    function GetTime(ColumnIndex: Integer): TDateTime; overload;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;

    function GetUInt(ColumnIndex: Integer): Cardinal;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD); overload;
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); overload;
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); overload;

    function MoveAbsolute(Row: Integer): Boolean; override;
    /// <summary>
    /// Gets the number of updated rows in the database.
    /// </summary>
    /// <returns>
    ///  The number of rows that were updated during the execution of the query.
    /// </returns>
    function GetUpdateCount: Integer;
  end;

  TZDbcDuckDBResultSetMetadata = Class(TZAbstractResultSetMetadata)
    constructor Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
  End;

{$ENDIF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} Math,
  ZMessages, ZEncoding, ZFastCode, ZDbcMetadata, ZClasses, ZDbcDuckDBUtils, ZSysUtils,
  TypInfo, Variants, ZBase64, ZExceptions {$IFNDEF FPC},xmldom{$ENDIF} {$IFDEF WITH_OMNIXML}, Xml.omnixmldom{$ENDIF};

function BoolToInt(Value: Boolean): Integer;
begin
  if Value then Result := 1 else Result := 0;
end;

constructor TZDbcDuckDBResultSetMetadata.Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
begin
  inherited;
  //Loaded := true;
end;

{ TZDbcProxyResultSet }

constructor TZDbcDuckDBResultSet.Create(const Statement: IZStatement; const Connection: IZConnection; const SQL: string; const Result: TDuckDB_Result);
var
  ConSettings: PZConSettings;
  Metadata: IZDatabaseMetadata;
begin
  ConSettings := Connection.GetConSettings;
  Metadata := Connection.GetMetadata;
  FResult := Result;
  FPlainDriver := (Connection.GetIZPlainDriver as IZDuckDBPlainDriver).GetInstance as TZDuckDBPlainDriver;

  inherited Create(Statement, SQL,
    TZDbcDuckDBResultSetMetadata.Create(Metadata, SQL, Self), ConSettings);

  ResultSetType := rtScrollInsensitive;
  SetConcurrency(rcReadOnly);

  Open;
end;

function DuckTypeToZSqlType(DuckType: TDuckDB_Type): TZSQLType;
begin
  case DuckType of
    DUCKDB_TYPE_INVALID: Result := stUnknown;
    DUCKDB_TYPE_BOOLEAN: Result := stBoolean;   // bool
    DUCKDB_TYPE_TINYINT: Result := stShort;     // int8_t
    DUCKDB_TYPE_SMALLINT: Result := stSmall;    // int16_t
    DUCKDB_TYPE_INTEGER: Result := stInteger;   // int32_t
    DUCKDB_TYPE_BIGINT: Result := stLong;       // int64_t
    DUCKDB_TYPE_UTINYINT: Result := stByte;     // uint8_t
    DUCKDB_TYPE_USMALLINT: Result := stWord;    // uint16_t
    DUCKDB_TYPE_UINTEGER: Result := stLongWord; // uint32_t
    DUCKDB_TYPE_UBIGINT: Result := stULong;     // uint64_t
    DUCKDB_TYPE_FLOAT: Result := stFloat;       // float
    DUCKDB_TYPE_DOUBLE: Result := stDouble;     // double
    DUCKDB_TYPE_TIMESTAMP: Result := stTimestamp; // duckdb_timestamp, in microseconds
    DUCKDB_TYPE_DATE: Result := stDate;         // duckdb_date
    DUCKDB_TYPE_TIME: Result := stTime;         // duckdb_time
    DUCKDB_TYPE_INTERVAL: Result := stUnknown;  // duckdb_interval
    DUCKDB_TYPE_HUGEINT: Result := stUnknown;   // duckdb_hugeint
    DUCKDB_TYPE_UHUGEINT: Result := stUnknown;  // duckdb_uhugeint
    DUCKDB_TYPE_VARCHAR: Result := stUnicodeStream; // const char*
    DUCKDB_TYPE_BLOB: Result := stBinaryStream; // duckdb_blob
    DUCKDB_TYPE_DECIMAL: Result := stBigDecimal;   // decimal
    DUCKDB_TYPE_TIMESTAMP_S: Result := stUnknown;  // duckdb_timestamp, in seconds
    DUCKDB_TYPE_TIMESTAMP_MS: Result := stUnknown; // duckdb_timestamp, in milliseconds
    DUCKDB_TYPE_TIMESTAMP_NS: Result := stUnknown; // duckdb_timestamp, in nanoseconds
    DUCKDB_TYPE_ENUM: Result := stUnknown; // enum type, only useful as logical type
    DUCKDB_TYPE_LIST: Result := stUnknown; // list type, only useful as logical type
    DUCKDB_TYPE_STRUCT: Result := stUnknown; // struct type, only useful as logical type
    DUCKDB_TYPE_MAP: Result := stUnknown; // map type, only useful as logical type
    DUCKDB_TYPE_ARRAY: Result := stUnknown; // duckdb_array, only useful as logical type
    DUCKDB_TYPE_UUID: Result := stGUID; // duckdb_hugeint
    DUCKDB_TYPE_UNION: Result := stUnknown; // union type, only useful as logical type
    DUCKDB_TYPE_BIT: Result := stUnknown; // duckdb_bit
    DUCKDB_TYPE_TIME_TZ: Result := stUnknown; // duckdb_time_tz
    DUCKDB_TYPE_TIMESTAMP_TZ: Result := stUnknown; // duckdb_timestamp
    DUCKDB_TYPE_ANY: Result := stUnknown; // ANY type
    DUCKDB_TYPE_VARINT: Result := stUnknown; // duckdb_varint
    DUCKDB_TYPE_SQLNULL: Result := stUnknown; // SQLNULL type
    else Result := stUnknown;
  end;
end;

procedure TZDbcDuckDBResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  LastRowNo := FPlainDriver.DuckDB_Row_Count(@FResult);

  { Fills the column info. }
  ColumnsInfo.Clear;

  for I := 0 to FPlainDriver.DuckDB_Column_Count(@FResult) - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do begin
      ColumnType := DuckTypeToZSqlType(FPlainDriver.DuckDB_Column_Type(@FResult, I));
      ColumnLabel := FPlainDriver.DuckDB_Column_Name(@FResult, I);


//      Precision := StrToInt(ColumnNode.Attributes['precision']);
//      {$IFDEF UNICODE}
//      ColumnLabel := PRawToUnicode(P, Precision, ConSettings^.ClientCodePage^.CP);
//      {$ELSE}
//      if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
//        ColumnLabel := BufferToStr(P, Precision)
//      else
//        ColumnLabel := ZUnicodeToString(PRawToUnicode(P, Precision, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
//      {$ENDIF}

//    addProperty('codepage', IntToStr(MD.GetColumnCodePage(x)));  // is this needed? All data is unicode in the end?
//    h‰? codepage und hasdefaultvalue gibts nicht am columntype?

      // todo: kl‰ren, was hier vonwegen der oben auskommentierten Unicodegeschichten rein muﬂ...

      {
      CatalogName := ColumnNode.Attributes['catalogname'];
      ColumnLabel := ColumnNode.Attributes['label'];
      ColumnName := ColumnNode.Attributes['name'];
      ColumnType := TZSQLType(GetEnumValue(TypeInfo(TZSQLType), ColumnNode.Attributes['type']));
      case ColumnType of
        stString, stUnicodeString:
          begin
            ColumnType := stUnicodeString;
            ColumnCodePage := zCP_UTF16;
          end;
        stAsciiStream, stUnicodeStream:
          begin
            ColumnType := stUnicodeStream;
            ColumnCodePage := zCP_UTF16;
          end;
      end;
      DefaultValue := ColumnNode.Attributes['defaultvalue'];
      Precision := StrToInt(ColumnNode.Attributes['precision']);
      Scale := StrToInt(ColumnNode.Attributes['scale']);
      SchemaName := ColumnNode.Attributes['schemaname'];
      TableName := ColumnNode.Attributes['tablename'];
      AutoIncrement := StrToBool(ColumnNode.Attributes['isautoincrement']);
      CaseSensitive := StrToBool(ColumnNode.Attributes['iscasesensitive']);
      Currency := StrToBool(ColumnNode.Attributes['iscurrency']);
      DefinitelyWritable := StrToBool(ColumnNode.Attributes['isdefinitlywritable']);
      Nullable := TZColumnNullableType(GetEnumValue(TypeInfo(TZColumnNullableType), ColumnNode.Attributes['isnullable']));
      ReadOnly := StrToBool(ColumnNode.Attributes['isreadonly']);
      Searchable := StrToBool(ColumnNode.Attributes['issearchable']);
      Signed := StrToBool(ColumnNode.Attributes['issigned']);
      Writable := StrToBool(ColumnNode.Attributes['iswritable']);
      if ColumnType = stString then
        ColumnType := stUnicodeString;
      }
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  RowNo := 0;

  inherited Open;
end;

function TZDbcDuckDBResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if not Assigned(FCurrentRowNode) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  Result := FPlainDriver.DuckDB_Value_Is_Null(@FResult, ColumnIndex, RowNo - 1);
end;

function TZDbcDuckDBResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  TempStr: TDuckDB_String;
begin
  LastWasNull := IsNull(ColumnIndex);

  if not LastWasNull then begin
    GetUTF8String(ColumnIndex);
    Len := Length(FStringBuffer);
    if Len = 0
    then Result := PEmptyAnsiString
    else Result := PAnsiChar(FStringBuffer);
  end else begin
    Result := nil;
    Len := 0
  end;
end;

function TZDbcDuckDBResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
begin
  LastWasNull := IsNull(ColumnIndex);

  if not LastWasNull then begin
    GetUTF8String(ColumnIndex);
    FWideBuffer := UTF8Decode(FStringBuffer);
    Len := Length(FWideBuffer);
    if Len = 0
    then Result := PEmptyUnicodeString
    else Result := PWideChar(FWideBuffer);
  end else begin
    Result := nil;
    Len := 0
  end;
end;

function TZDbcDuckDBResultSet.GetString(ColumnIndex: Integer): String;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Result := GetUTF8String(ColumnIndex);
end;

{$IFNDEF NO_ANSISTRING}
function TZDbcDuckDBResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Result := GetUTF8String(ColumnIndex);
end;
{$ENDIF}

function TZDbcDuckDBResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  TempStr: TDuckDB_String;
begin
  LastWasNull := IsNull(ColumnIndex);

  if not LastWasNull then begin
    TempStr := FPlainDriver.DuckDB_Value_String(@FResult, ColumnIndex, RowNo - 1);
    try
      SetLength(FStringBuffer, TempStr.size);
      Move(TempStr.data^, PAnsiChar(FStringBuffer)^, TempStr.size);
      Result := FStringBuffer;
    finally
      FPlainDriver.DuckDB_free(TempStr.data);
    end;
  end else begin
    Result := '';
  end;
end;

function TZDbcDuckDBResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Result := GetUTF8String(ColumnIndex);
end;

function TZDbcDuckDBResultSet.GetBinaryString(ColumnIndex: Integer): RawByteString;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Result := GetUTF8String(ColumnIndex);
end;

function TZDbcDuckDBResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then begin
    Result := '';
    exit;
  end;

  Result := UTF8Decode(GetUTF8String(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZDbcDuckDBResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Str: String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then begin
    Result := FPlainDriver.DuckDB_Value_Boolean(@FResult, ColumnIndex, RowNo - 1);
  end else begin
    Result := false;
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
function TZDbcDuckDBResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Result := FPlainDriver.DuckDB_Value_Int32(@FResult, ColumnIndex, RowNo - 1);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcDuckDBResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Result := FPlainDriver.DuckDB_Value_Int64(@FResult, ColumnIndex, RowNo - 1);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcDuckDBResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Result := FPlainDriver.DuckDB_Value_Int64(@FResult, ColumnIndex, RowNo - 1);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcDuckDBResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Result := FPlainDriver.DuckDB_Value_Float(@FResult, ColumnIndex, RowNo - 1);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDbcDuckDBResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Result := FPlainDriver.DuckDB_Value_Double(@FPlainDriver, ColumnIndex, RowNo - 1);
end;

function TZDbcDuckDBResultSet.GetBigDecimal(ColumnIndex: Integer): TBcd;
var
  Val: Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result :=  IntegerToBcd(0);
    exit;
  end;

  Val := GetDouble(ColumnIndex);
  Result := DoubleToBCD(Val);
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
function TZDbcDuckDBResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  TempBlob: TDuckDB_Blob;
begin
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    //Result := '';
    exit;
  end;

  TempBlob := FPlainDriver.DuckDB_Value_Blob(@FResult, ColumnIndex, RowNo - 1);
  try
    //if TempBlob.size = 0 then begin

    //end else begin
      SetLength(Result, TempBlob.size);
      if TempBlob.size > 0 then
        Move(TempBlob.data^, Result[0], TempBlob.size);
    //end;
  finally
    FPlainDriver.DuckDB_free(TempBlob.data);
  end;
end;

function TZDbcDuckDBResultSet.GetCurrency(
  ColumnIndex: Integer): Currency;
var
  Val: Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Val := GetDouble(ColumnIndex);
  Result := Val;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDbcDuckDBResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Val: TDuckDb_Date;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Val := FPlainDriver.DuckDB_Value_Date(@FResult, ColumnIndex, RowNo - 1);
  Result := Val.days + DuckDBDateShift;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDbcDuckDBResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Val: TDuckDb_Time;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Val := FPlainDriver.DuckDB_Value_Time(@FResult, ColumnIndex, RowNo - 1);
  Result := Val.micros / DuckDBMicrosecondsPerDay;
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
function TZDbcDuckDBResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Val: TDuckDb_TimeStamp;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Val := FPlainDriver.DuckDB_Value_Timestamp(@FResult, ColumnIndex, RowNo - 1);
  Result := Val.micros / DuckDBMicrosecondsPerDay + DuckDBDateShift;
end;

function TZDbcDuckDBResultSet.GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  Bytes: TBytes;
begin
  if LobStreamMode <> lsmRead then
    raise EZSQLException.Create('No lob stream mode besides lsmRead is supported.');

  {$IFNDEF DISABLE_CHECKING}
    CheckColumnConvertion(ColumnIndex, stInteger);
  {$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := nil;
    exit;
  end;

  Bytes := GetBytes(ColumnIndex);
  Result := TZAbstractBlob.CreateWithData(@Bytes[0], Length(Bytes)) as IZBlob;
end;

function TZDbcDuckDBResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := 0;
    exit;
  end;

  Result := FPlainDriver.DuckDB_Value_UInt32(@FResult, ColumnIndex, RowNo - 1);
end;

procedure TZDbcDuckDBResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
begin
  Result := GetBigDecimal(ColumnIndex)
end;

procedure TZDbcDuckDBResultSet.GetGUID(ColumnIndex: Integer; var Result: TGUID);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);

  if LastWasNull then begin
    Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
    exit;
  end;

  raise EZSQLException.Create('Cannot fetch GUIDs from DuckDB yet.');
end;

function TZDbcDuckDBResultSet.GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte;
begin
  FByteBuffer := GetBytes(ColumnIndex);
  Result := @FByteBuffer[0];
  Len := Length(FByteBuffer);
end;

procedure TZDbcDuckDBResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
begin
  DecodeDateTimeToDate(GetDate(ColumnIndex), Result);
end;

procedure TZDbcDuckDBResultSet.GetTime(ColumnIndex: Integer; Var Result: TZTime);
begin
  DecodeDateTimeToTime(GetTime(ColumnIndex), Result);
end;

procedure TZDbcDuckDBResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
begin
  DecodeDateTimeToTimeStamp(GetTimestamp(ColumnIndex), Result);
end;

function TZDbcDuckDBResultSet.GetUpdateCount: Integer;
begin
  Result := FPlainDriver.DuckDB_Rows_Changed(@FResult);
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
function TZDbcDuckDBResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  { Checks for maximum row. }
{$IFDEF FPC} // I suppose FPC compiler needs this initial assignment...?
   Result := False;
{$ENDIF}
  { Processes negative rows. }
  if Row < 0 then begin
    Row := LastRowNo + Row + 1;
    if Row < 0 then Row := 0;
  end;

  if (ResultSetType <> rtForwardOnly) or (Row >= RowNo) then begin
    if Row < 1 then begin
      RowNo := 0;
      Result := False;
    end else if Row > LastRowNo then begin
      RowNo := LastRowNo + 1;
      Result := False;
    end else begin
      RowNo := Row;
      Result := True;
    end;
  end else begin
    raise EZSQLException.Create('This resultset is forward only.');
  end;
end;

{$ENDIF ZEOS_DISABLE_DUCKDB} //if set we have an empty unit
end.
