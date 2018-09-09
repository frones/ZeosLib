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
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  {$IFDEF NO_UNIT_CONTNRS}ZClasses{$ELSE}Contnrs{$ENDIF},
  ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZCompatibility, ZDbcCache,
  ZDbcCachedResultSet, ZDbcGenericResolver, ZDbcMySqlStatement,
  ZPlainMySqlDriver, ZPlainMySqlConstants, ZSelectSchema;

type
  {** Implements MySQL ResultSet Metadata. }
  TZMySQLResultSetMetadata = class(TZAbstractResultSetMetadata)
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
    function GetColumnType(ColumnIndex: Integer): TZSQLType; override;
    function GetSchemaName(ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  end;

  {** Implements MySQL ResultSet. }
  TZAbstractMySQLResultSet = class(TZAbstractResultSet)
  private //common
    FFirstRowFetched: boolean; //we can't seek to a negative index -> hook BeforeFirst state
    FFieldCount: ULong;
    FPMYSQL: PPMYSQL; //address of the MYSQL connection handle
    FMYSQL_aligned_BINDs: PMYSQL_aligned_BINDs; //offset descriptor structures
    FIsOutParamResult: Boolean; //is the result a `Result of Out params?
    procedure InitColumnBinds(Bind: PMYSQL_aligned_BIND; MYSQL_FIELD: PMYSQL_FIELD;
      FieldOffsets: PMYSQL_FIELDOFFSETS; Iters: Integer);
  private //connection resultset
    FQueryHandle: PZMySQLResult; //a query handle
    FRowHandle: PZMySQLRow; //current row handle
    FPlainDriver: TZMySQLPlainDriver; //our api holder object
    FLengthArray: PULongArray; //for non prepareds a Length array
    fServerCursor: Boolean; //fetch row by row from server?
   private { prepared stmt }
    fBindBufferAllocated: Boolean; //are the bindbuffers mysql writes in allocated?
    FFetchStatus: Integer; //the FFetchStatus of mysql_stmt_fetch
    FMYSQL_STMT: PMYSQL_STMT; //the prepared stmt handle
    FPMYSQL_STMT: PPMYSQL_STMT; //address of the prepared stmt handle
    FBindOffSets: PMYSQL_BINDOFFSETS;
    FColBuffer: Pointer; //the buffer mysql writes in
    FTempBlob: IZBlob; //temporary Lob
    FSmallLobBuffer: array[Byte] of Byte; //for tiny reads of unbound col-buffers
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver;
      const Statement: IZStatement; const SQL: string; IsOutParamResult: Boolean;
      PMYSQL: PPMYSQL; PMYSQL_STMT: PPMYSQL_STMT; AffectedRows: PInteger;
      out OpenCursorCallback: TOpenCursorCallback);
    destructor Destroy; override;
    procedure Close; override;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;

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
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions = [jcoEndJSONObject]); override;
    {$ENDIF USE_SYNCOMMONS}
    //EH: keep that override 4 all descendants: seek_data is dead slow in a forward only mode
    function Next: Boolean; override;

    procedure ResetCursor; override;
    procedure OpenCursor; virtual;
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

  {** Implements a cached resolver with MySQL specific functionality. }
  TZMySQLCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FPMYSQL: PPMySQL;
    FMYSQL_STMT: PPMYSQL_STMT;
    FPlainDriver: TZMySQLPlainDriver;
    FAutoColumnIndex: Integer;
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver; MySQL: PPMySQL;
      MYSQL_STMT: PPMYSQL_STMT; const Statement: IZStatement;
      const Metadata: IZResultSetMetadata);

    function FormWhereClause(Columns: TObjectList;
      OldRowAccessor: TZRowAccessor): string; override;
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;

    // --> ms, 31/10/2005
    function FormCalculateStatement(Columns: TObjectList): string; override;
    // <-- ms
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; {%H-}UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; const {%H-}Resolver: IZCachedResolver); override;
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
  end;

  TZMySQLPreparedClob = Class(TZAbstractClob)
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver; Bind: PMYSQL_aligned_BIND;
      StmtHandle: PPMYSQL_STMT; ColumnIndex: Cardinal; const Sender: IImmediatelyReleasable);
  End;

  TZMySQLPreparedBlob = Class(TZAbstractBlob)
  public
    constructor Create(const PlainDriver: TZMySQLPlainDriver; Bind: PMYSQL_aligned_BIND;
      StmtHandle: PMySql_Stmt; ColumnIndex: Cardinal; const Sender: IImmediatelyReleasable);
  End;

implementation

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF}
  ZFastCode, ZSysUtils, ZMessages, ZEncoding, {$IFNDEF NO_UNIT_CONTNRS}ZClasses,{$ENDIF}
  ZDbcMySqlUtils, ZDbcMySQL, ZDbcUtils, ZDbcMetadata, ZDbcLogging;

{ TZMySQLResultSetMetadata }

procedure TZMySQLResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
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
constructor TZMySQLResultSetMetadata.Create(const Metadata: IZDatabaseMetadata;
  const SQL: string; ParentResultSet: TZAbstractResultSet);
begin
  inherited Create(Metadata, SQL, ParentResultSet);
  FHas_ExtendedColumnInfos := TZMySQLPlainDriver(MetaData.GetConnection.GetIZPlainDriver.GetInstance).mysql_get_client_version > 40000;
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZMySQLResultSetMetadata.GetCatalogName(ColumnIndex: Integer): string;
begin
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).CatalogName;
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZMySQLResultSetMetadata.GetColumnName(ColumnIndex: Integer): string;
begin
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
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
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
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
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
begin
  if not FHas_ExtendedColumnInfos then
    inherited LoadColumns
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

{ TZAbstractMySQLResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractMySQLResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var
  C: Cardinal;
  H, I: Integer;
  P: PAnsiChar;
  Bind: PMYSQL_aligned_BIND;
  MS: Word;
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
    Bind := @FMYSQL_aligned_BINDs[C];
    if fBindBufferAllocated then begin
      if Bind^.is_null = 1 then
        if JSONWriter.Expand then begin
          if not (jcsSkipNulls in JSONComposeOptions) then begin
            JSONWriter.AddString(JSONWriter.ColNames[I]);
            JSONWriter.AddShort('null,')
          end;
        end else
          JSONWriter.AddShort('null,')
      else begin
        if JSONWriter.Expand then
          JSONWriter.AddString(JSONWriter.ColNames[I]);
        case Bind^.buffer_type_address^ of
          //FIELD_TYPE_DECIMAL,
          FIELD_TYPE_TINY       : if Bind^.is_unsigned_address^ = 0
                                  then JSONWriter.Add(PShortInt(Bind^.Buffer)^)
                                  else JSONWriter.AddU(PByte(Bind^.Buffer)^);
          FIELD_TYPE_SHORT      : if Bind^.is_unsigned_address^ = 0
                                  then JSONWriter.Add(PSmallInt(Bind^.Buffer)^)
                                  else JSONWriter.AddU(PWord(Bind^.Buffer)^);
          FIELD_TYPE_LONG       : if Bind^.is_unsigned_address^ = 0
                                  then JSONWriter.Add(PInteger(Bind^.Buffer)^)
                                  else JSONWriter.AddU(PLongWord(Bind^.Buffer)^);
          FIELD_TYPE_FLOAT      : JSONWriter.AddSingle(PSingle(Bind^.Buffer)^);
          FIELD_TYPE_DOUBLE     : JSONWriter.AddDouble(PDouble(Bind^.Buffer)^);
          FIELD_TYPE_LONGLONG   : if Bind^.is_unsigned_address^ = 0
                                  then JSONWriter.Add(PInt64(Bind^.Buffer)^)
                                  else JSONWriter.AddNoJSONEscapeUTF8(ZFastCode.IntToRaw(PUInt64(Bind^.Buffer)^));
          FIELD_TYPE_YEAR       : JSONWriter.AddU(PWord(Bind^.Buffer)^);
          FIELD_TYPE_NULL       : JSONWriter.AddShort('null');
          FIELD_TYPE_TIMESTAMP,
          FIELD_TYPE_DATETIME   : begin
                                    if jcoDATETIME_MAGIC in JSONComposeOptions then
                                      JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                    else if jcoMongoISODate in JSONComposeOptions then
                                      JSONWriter.AddShort('ISODate("')
                                    else JSONWriter.Add('"');
                                    DateToIso8601PChar(@FSmallLobBuffer[0], True, PMYSQL_TIME(Bind^.Buffer)^.Year,
                                      PMYSQL_TIME(Bind^.Buffer)^.Month, PMYSQL_TIME(Bind^.Buffer)^.Day);
                                    MS := ((PMYSQL_TIME(Bind^.Buffer)^.second_part) * Byte(ord(jcoMilliseconds in JSONComposeOptions)) div 1000000);
                                    FSmallLobBuffer[10] := Ord('T');
                                    TimeToIso8601PChar(@FSmallLobBuffer[11], True, PMYSQL_TIME(Bind^.Buffer)^.Hour,
                                      PMYSQL_TIME(Bind^.Buffer)^.Minute, PMYSQL_TIME(Bind^.Buffer)^.Second, MS, 'T', jcoMilliseconds in JSONComposeOptions);
                                    if (jcoMilliseconds in JSONComposeOptions) and not (jcoMongoISODate in JSONComposeOptions)
                                    then JSONWriter.AddNoJSONEscape(@FSmallLobBuffer[0],23)
                                    else JSONWriter.AddNoJSONEscape(@FSmallLobBuffer[0],19);
                                    if jcoMongoISODate in JSONComposeOptions
                                    then JSONWriter.AddShort('Z")')
                                    else JSONWriter.Add('"');
                                  end;
          FIELD_TYPE_DATE,
          FIELD_TYPE_NEWDATE    : begin
                                    if jcoDATETIME_MAGIC in JSONComposeOptions then
                                      JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                    else if jcoMongoISODate in JSONComposeOptions then
                                      JSONWriter.AddShort('ISODate("')
                                    else JSONWriter.Add('"');
                                    DateToIso8601PChar(@FSmallLobBuffer[0], True, PMYSQL_TIME(Bind^.Buffer)^.Year,
                                      PMYSQL_TIME(Bind^.Buffer)^.Month, PMYSQL_TIME(Bind^.Buffer)^.Day);
                                    JSONWriter.AddNoJSONEscape(@FSmallLobBuffer[0],10);
                                    if jcoMongoISODate in JSONComposeOptions
                                    then JSONWriter.AddShort('Z")')
                                    else JSONWriter.Add('"');
                                  end;
          FIELD_TYPE_TIME       : begin
                                    if jcoMongoISODate in JSONComposeOptions then
                                      JSONWriter.AddShort('ISODate("0000-00-00')
                                    else if jcoDATETIME_MAGIC in JSONComposeOptions then
                                      JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                    else
                                      JSONWriter.AddShort('"T');
                                    MS := (PMYSQL_TIME(Bind^.Buffer)^.second_part) div 1000000;
                                    TimeToIso8601PChar(@FSmallLobBuffer[0], True, PMYSQL_TIME(Bind^.Buffer)^.Hour,
                                      PMYSQL_TIME(Bind^.Buffer)^.Minute, PMYSQL_TIME(Bind^.Buffer)^.Second, MS, 'T', jcoMilliseconds in JSONComposeOptions);
                                    if jcoMilliseconds in JSONComposeOptions
                                    then JSONWriter.AddNoJSONEscape(@FSmallLobBuffer[0],12)
                                    else JSONWriter.AddNoJSONEscape(@FSmallLobBuffer[0],8);
                                    if jcoMongoISODate in JSONComposeOptions
                                    then JSONWriter.AddShort('Z)"')
                                    else JSONWriter.Add('"');
                                  end;
          FIELD_TYPE_BIT        : if Bind^.Length[0] = 1
                                  then JSONWriter.AddShort(JSONBool[PByte(Bind^.Buffer)^ <> 0])
                                  else JSONWriter.WrBase64(Pointer(Bind^.Buffer), Bind^.Length[0], True);
          FIELD_TYPE_ENUM,
          FIELD_TYPE_SET,
          FIELD_TYPE_VARCHAR,
          FIELD_TYPE_VAR_STRING,
          FIELD_TYPE_STRING,
          FIELD_TYPE_TINY_BLOB,
          FIELD_TYPE_MEDIUM_BLOB,
          FIELD_TYPE_LONG_BLOB,
          FIELD_TYPE_BLOB,
          FIELD_TYPE_GEOMETRY   :
                        if (Bind^.Buffer <> nil) then
                          if Bind^.Binary then
                            JSONWriter.WrBase64(Pointer(Bind^.Buffer), Bind^.Length[0], True)
                          else begin
                            JSONWriter.Add('"');
                            JSONWriter.AddJSONEscape(Pointer(Bind^.Buffer), Bind^.Length[0]);
                            JSONWriter.Add('"');
                          end
                        else if Bind^.Length[0] < SizeOf(FSmallLobBuffer) then begin
                          Bind^.buffer_address^ := @FSmallLobBuffer[0];
                          Bind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1; //mysql sets $0 on to of data and corrupts our mem
                          FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, Bind^.mysql_bind, C, 0);
                          Bind^.buffer_address^ := nil;
                          Bind^.buffer_Length_address^ := 0;
                          if Bind^.binary then
                            JSONWriter.WrBase64(@FSmallLobBuffer[0], Bind^.Length[0], True)
                          else begin
                            JSONWriter.Add('"');
                            JSONWriter.AddJSONEscape(@FSmallLobBuffer[0], Bind^.Length[0]);
                            JSONWriter.Add('"');
                          end;
                        end else if Bind^.binary then begin
                          FTempBlob := TZMySQLPreparedBlob.Create(FplainDriver,
                            Bind, FPMYSQL^, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Self);
                          JSONWriter.WrBase64(FTempBlob.GetBuffer, FTempBlob.Length, True)
                        end else begin
                          JSONWriter.Add('"');
                          FTempBlob := TZMySQLPreparedClob.Create(FplainDriver,
                            Bind, FPMYSQL^, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Self);
                          P := FTempBlob.GetPAnsiChar(zCP_UTF8);
                          JSONWriter.AddJSONEscape(P, FTempBlob.Length);
                          JSONWriter.Add('"');
                        end;
        end;
      end
    end else begin
      P := PMYSQL_ROW(FRowHandle)[C];
      if P = nil then
        if JSONWriter.Expand then begin
          if not (jcsSkipNulls in JSONComposeOptions) then begin
            JSONWriter.AddString(JSONWriter.ColNames[I]);
            JSONWriter.AddShort('null,')
          end;
        end else
          JSONWriter.AddShort('null,')
      else begin
        if JSONWriter.Expand then
          JSONWriter.AddString(JSONWriter.ColNames[I]);
        case Bind.buffer_type_address^ of
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
          FIELD_TYPE_STRING     : if not Bind.binary then begin
                                    JSONWriter.Add('"');
                                    JSONWriter.AddJSONEscape(P, FLengthArray^[C]);
                                    JSONWriter.Add('"');
                                  end else
                                    JSONWriter.WrBase64(P, FLengthArray^[C], True);
          FIELD_TYPE_GEOMETRY   : JSONWriter.WrBase64(P, FLengthArray^[C], True);
        end;
      end;
    end;
    JSONWriter.Add(',');
  end;
  if jcoEndJSONObject in JSONComposeOptions then
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
  const Statement: IZStatement; const SQL: string; IsOutParamResult: Boolean;
  PMYSQL: PPMYSQL; PMYSQL_STMT: PPMYSQL_STMT; AffectedRows: PInteger;
  out OpenCursorCallback: TOpenCursorCallback);
var ClientVersion: ULong;
begin
  inherited Create(Statement, SQL, TZMySQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
      Statement.GetConnection.GetConSettings);
  fServerCursor := Self is TZMySQL_Use_ResultSet;
  FColBuffer := nil;
  FMYSQL_aligned_BINDs := nil;
  FPMYSQL := PMYSQL;
  FPMYSQL_STMT := PMYSQL_STMT;
  FMYSQL_STMT  := FPMYSQL_STMT^;
  FQueryHandle := nil;
  FRowHandle := nil;
  FFieldCount := 0;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  OpenCursorCallback := OpenCursor;
  //hooking very old versions -> we use this struct for some more logic
  ClientVersion := FPlainDriver.mysql_get_client_version;
  FBindOffsets := GetBindOffsets(FPlainDriver.IsMariaDBDriver, Max(40101, ClientVersion));
  FIsOutParamResult := IsOutParamResult;
  Open;
  if Assigned(AffectedRows) then
    AffectedRows^ := LastRowNo;
end;

destructor TZAbstractMySQLResultSet.Destroy;
begin
  ReallocBindBuffer(FColBuffer, FMYSQL_aligned_BINDs, FBindOffsets, FFieldCount, 0, Ord(fBindBufferAllocated));
  inherited Destroy;
end;

{**
  Opens this recordset.
}
procedure TZAbstractMySQLResultSet.Open;
const One: PAnsiChar = '1';
var
  I: Integer;
  FieldHandle: PMYSQL_FIELD;
  QueryHandle: PZMySQLResult;
  FieldOffsets: PMYSQL_FIELDOFFSETS;
  MySQL_FieldType_Bit_1_IsBoolean: Boolean;
begin
  FieldOffsets := GetFieldOffsets(FPlainDriver.mysql_get_client_version);
  MySQL_FieldType_Bit_1_IsBoolean := (GetStatement.GetConnection as IZMySQLConnection).MySQL_FieldType_Bit_1_IsBoolean;
  if FPMYSQL_STMT^ = nil then begin
    OpenCursor;
    QueryHandle := FQueryHandle;
  end else begin
    if FIsOutParamResult then begin
      FMYSQL_STMT := FPMYSQL_STMT^;
      if Assigned(FPlainDriver.mysql_stmt_attr_set517UP)
      then FPlainDriver.mysql_stmt_attr_set517UP(FPMYSQL_STMT^,STMT_ATTR_UPDATE_MAX_LENGTH,one)
      else FPlainDriver.mysql_stmt_attr_set(FPMYSQL_STMT^,STMT_ATTR_UPDATE_MAX_LENGTH,one);
      if FPlainDriver.mysql_stmt_store_result(FPMYSQL_STMT^) <> 0 then
        checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcOther, 'mysql_stmt_store_result', Self);
    end;
    if FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) > 0
    then QueryHandle := FPlainDriver.mysql_stmt_result_metadata(FMYSQL_STMT)
    else QueryHandle := nil;
    fBindBufferAllocated := True;
  end;
  if QueryHandle = nil then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  { Fills the column info. }
  ColumnsInfo.Clear;
  FFieldCount := FPlainDriver.mysql_num_fields(QueryHandle);

  { We use the refetch logic of
    https://bugs.mysql.com/file.php?id=12361&bug_id=33086 }
  ReallocBindBuffer(FColBuffer, FMYSQL_aligned_BINDs, FBindOffsets, 0,
    FFieldCount, 1);

  for I := 0 to FFieldCount -1 do begin
    FPlainDriver.mysql_field_seek(QueryHandle, I);
    FieldHandle := FPlainDriver.mysql_fetch_field(QueryHandle);
    if FieldHandle = nil then
      Break;
    {$R-}
    InitColumnBinds(@FMYSQL_aligned_BINDs[I], FieldHandle, FieldOffsets, Ord(fBindBufferAllocated));
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    ColumnsInfo.Add(GetMySQLColumnInfoFromFieldHandle(FieldHandle, FieldOffsets,
      ConSettings, MySQL_FieldType_Bit_1_IsBoolean));
  end;

  if fBindBufferAllocated then begin
    FPlainDriver.mysql_free_result(QueryHandle);
    OpenCursor;
  end;
  inherited Open;
end;

procedure TZAbstractMySQLResultSet.OpenCursor;
var
  I: Integer;
  Bind: PMYSQL_aligned_BIND;
begin
  if (FMYSQL_STMT = nil) and (FPMYSQL_STMT <> nil) and (FPMYSQL_STMT^ <> nil) then
    FMYSQL_STMT := FPMYSQL_STMT^;
  if FMYSQL_STMT <> nil then begin
    if not fBindBufferAllocated then begin
      for I := 0 to Self.ColumnsInfo.Count -1 do begin
        {$R-}
        Bind := @FMYSQL_aligned_BINDs[I];
        {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
        if Bind^.buffer_length_address^ > 0 then begin
          GetMem(Bind^.Buffer, (((bind^.buffer_length_address^-Byte(Ord(bind^.buffer_type_address^ <> FIELD_TYPE_STRING))) shr 3)+1) shl 3); //8Byte aligned
          Bind^.buffer_address^ := Bind^.buffer;
        end;
      end;
      fBindBufferAllocated := True;
    end;
    if (FPlainDriver.mysql_stmt_bind_result(FMYSQL_STMT,FColBuffer)<>0) then
      checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcOther, 'mysql_stmt_bind_result', Self);
  end;
end;

procedure TZAbstractMySQLResultSet.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
begin
  FQueryHandle := nil;
  FRowHandle := nil;
  FMYSQL_STMT := nil;
  inherited ReleaseImmediat(Sender);
end;

procedure TZAbstractMySQLResultSet.ResetCursor;
var Handle: Pointer;
begin
  if not Closed then
    FFirstRowFetched := False;
    if fBindBufferAllocated then begin
      Handle := FMYSQL_STMT;
      FMYSQL_STMT := nil;
      //test if more results are pendding
      if (Handle <> nil) and (FIsOutParamResult or ({not FPlainDriver.IsMariaDBDriver and} (Assigned(FPlainDriver.mysql_stmt_more_results) and (FPlainDriver.mysql_stmt_more_results(FPMYSQL_STMT^) = 1))))
      then Close
      else inherited ResetCursor;
    end else begin
      Handle := FQueryHandle;
      FQueryHandle := nil;
      if (Handle <> nil) and (FPlainDriver.mysql_more_results(FPMYSQL^) = 1)
      then Close
      else inherited ResetCursor;
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
procedure TZAbstractMySQLResultSet.Close;
begin
  try
    inherited Close;
  finally
    FQueryHandle := nil;
    FRowHandle := nil;
    FMYSQL_STMT := nil;
  end;
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
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  if fBindBufferAllocated
  {$R-}
  then Result := FMYSQL_aligned_BINDs[ColumnIndex].is_null = 1
  else Result := (FRowHandle = nil) or (PMYSQL_ROW(FRowHandle)[ColumnIndex] = nil);
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := Result;
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
  if (Closed) or (fBindBufferAllocated and not Assigned(FMYSQL_STMT)) or
     ((MaxRows > 0) and (RowNo >= MaxRows)) or (RowNo > LastRowNo) then
    Exit;
  if (RowNo = 0) and FFirstRowFetched then begin //if moveAbsolute(0) was called
    Result := True;
    FFirstRowFetched := False;
  end else if fBindBufferAllocated then begin
    FFetchStatus := FPlainDriver.mysql_stmt_fetch(FMYSQL_STMT);
    if FFetchStatus in [STMT_FETCH_OK, MYSQL_DATA_TRUNCATED] then
      Result := True
    else if FFetchStatus = STMT_FETCH_ERROR then
      checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcOther, '', Self);
  end else begin
    if (FQueryHandle = nil) then begin
      FQueryHandle := FPlainDriver.mysql_store_result(FPMYSQL^);
      if Assigned(FQueryHandle) then
        LastRowNo := FPlainDriver.mysql_num_rows(FQueryHandle);
    end;
    FRowHandle := FPlainDriver.mysql_fetch_row(FQueryHandle);
    if FRowHandle <> nil then begin
      Result := True;
      FLengthArray := FPlainDriver.mysql_fetch_lengths(FQueryHandle);
    end else
      FLengthArray := nil;
  end;
  if Result then begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
  end else if fServerCursor then begin
    LastRowNo := RowNo;
    RowNo := RowNo+1;
  end else
    RowNo := RowNo+1;
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
var
  TmpDateTime, TmpDateTime2: TDateTime;
  ColBind: PMYSQL_aligned_BIND;
  Status: Integer;
begin
  {$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (fBindBufferAllocated and (FMYSQL_STMT = nil)) or
     (not fBindBufferAllocated and (FRowHandle = nil)) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
  {$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  if fBindBufferAllocated then begin
    {$R-}
    ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := ColBind^.is_null =1;
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
            Result := ColBind^.buffer;
            Len := ColBind^.length[0];
            Exit;
          end;
        FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
            if ColBind.buffer <> nil then begin
              Result := ColBind^.buffer;
              Len := ColBind^.length[0];
              Exit;
            end else if ColBind^.Length[0] < SizeOf(FSmallLobBuffer) then begin
              ColBind^.buffer_address^ := @FSmallLobBuffer[0];
              ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1; //mysql sets $0 on to of data and corrupts our mem
              Status := FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, ColBind^.mysql_bind, ColumnIndex, 0);
              ColBind^.buffer_address^ := nil;
              ColBind^.buffer_Length_address^ := 0;
              Result := @FSmallLobBuffer[0];
              if Status <> 0 then
                if Status = STMT_FETCH_ERROR
                then raise EZSQLException.Create('Fetch error')
                else checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcOther, 'mysql_stmt_fetch_column', Self);
              Len := ColBind^.Length[0];
              Exit;
            end else begin
              FTempBlob := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
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
  end else begin
    {$R-}
    Len := FLengthArray^[ColumnIndex];
    Result := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Result = nil;
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
function TZAbstractMySQLResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var Len: NativeUInt;
begin
  Result := GetPAnsiChar(ColumnIndex, Len);
end;

procedure TZAbstractMySQLResultSet.InitColumnBinds(Bind: PMYSQL_aligned_BIND;
  MYSQL_FIELD: PMYSQL_FIELD; FieldOffsets: PMYSQL_FIELDOFFSETS; Iters: Integer);
begin
  Bind^.is_unsigned_address^ := Ord(PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.flags)^ and UNSIGNED_FLAG <> 0);
  bind^.buffer_type_address^ := PMysqlFieldType(NativeUInt(MYSQL_FIELD)+FieldOffsets._type)^; //safe initialtype
  if FieldOffsets.charsetnr > 0
  then bind^.binary := (PUInt(NativeUInt(MYSQL_FIELD)+NativeUInt(FieldOffsets.charsetnr))^ = 63) and (PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.flags)^ and BINARY_FLAG <> 0)
  else bind^.binary := (PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.flags)^ and BINARY_FLAG <> 0);

  case bind^.buffer_type_address^ of
    FIELD_TYPE_BIT: case PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.length)^ of
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
        if PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.length)^ = 12 then begin
          Bind^.Length[0] := SizeOf(Single);
          bind^.buffer_type_address^ := FIELD_TYPE_FLOAT
        end else begin
          Bind^.Length[0] := SizeOf(Double);
          bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
        end;
        bind^.decimals := PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.decimals)^;
      end;
    FIELD_TYPE_BLOB,
    FIELD_TYPE_TINY_BLOB,
    FIELD_TYPE_MEDIUM_BLOB,
    FIELD_TYPE_LONG_BLOB,
    FIELD_TYPE_GEOMETRY:    if FIsOutParamResult
                            then Bind^.Length[0] := PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.max_length)^
                            else Bind^.Length[0] := 0;//http://bugs.mysql.com/file.php?id=12361&bug_id=33086
    FIELD_TYPE_VARCHAR,
    FIELD_TYPE_VAR_STRING,
    FIELD_TYPE_STRING,
    FIELD_TYPE_ENUM, FIELD_TYPE_SET: begin
        bind^.buffer_type_address^ := FIELD_TYPE_STRING;
        Bind^.Length[0] := PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.length)^;//+Byte(Ord(not bind^.binary));
      end;
    FIELD_TYPE_NEWDECIMAL,
    FIELD_TYPE_DECIMAL:
      if PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.decimals)^ = 0 then begin
        if PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.length)^ <= Byte(2+(Ord(PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.flags)^ and UNSIGNED_FLAG <> 0))) then begin
          bind^.buffer_type_address^ := FIELD_TYPE_TINY;
          Bind^.Length[0] := 1;
        end else if PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.length)^ <= Byte(4+(Ord(PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.flags)^ and UNSIGNED_FLAG <> 0))) then begin
          Bind^.Length[0] := 2;
          bind^.buffer_type_address^ := FIELD_TYPE_SHORT;
        end else if PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.length)^ <= Byte(9+(Ord(PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.flags)^ and UNSIGNED_FLAG <> 0))) then begin
          bind^.buffer_type_address^ := FIELD_TYPE_LONG;
          Bind^.Length[0] := 4;
        end else begin
          bind^.buffer_type_address^ := FIELD_TYPE_LONGLONG;
          Bind^.Length[0] := 8;
        end
      end else begin //force binary conversion to double values!
        bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
        Bind^.Length[0] := 8;
        bind^.decimals := PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.decimals)^;
      end;
    FIELD_TYPE_NULL: Bind^.Length[0] := 8;
    else
      Bind^.Length[0] := (((PUInt(NativeUInt(MYSQL_FIELD)+FieldOffsets.length)^) shr 3)+1) shl 3; //8Byte Aligned
    //Length := MYSQL_FIELD^.length;
  end;
  if (bind^.Length[0] = 0) or (Iters = 0)
  then Bind^.Buffer := nil
  else GetMem(Bind^.Buffer, ((((bind^.Length^[0]+Byte(Ord(bind^.buffer_type_address^ in [FIELD_TYPE_STRING]))) shr 3)+1) shl 3) ); //8Byte aligned
  Bind^.buffer_address^ := Bind^.buffer;
  Bind^.buffer_length_address^ := bind^.Length[0];
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
  ColBind: PMYSQL_aligned_BIND;
  TmpDateTime, TmpDateTime2: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
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
              ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1;//mysql sets $0 on to of data and corrupts our mem
              FPlainDriver.mysql_stmt_fetch_column(FMYSQL_STMT, Colbind^.mysql_bind, ColumnIndex, 0);
              Colbind^.buffer_address^ := nil;
              ColBind^.buffer_Length_address^ := 0;
              ZSetString(PAnsiChar(@FSmallLobBuffer[0]), ColBind^.Length[0] , Result);
            end else
              Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString
          else ZSetString(PAnsiChar(ColBind^.buffer), ColBind^.Length[0] , Result);
        else
          raise EZSQLException.Create(Format(SErrorConvertionField,
            ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
              DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
      end
    else Result := '';
  end else begin
    {$R-}
    Len := FLengthArray^[ColumnIndex];
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    ZSetString(Buffer, Len, Result);
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
function TZAbstractMySQLResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Buffer: PAnsiChar;
  Len: ULong;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
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
            ColBind^.buffer_address^ := @FSmallLobBuffer[0];
            ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1;//mysql sets $0 on to of data and corrupts our mem
            FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, ColBind^.mysql_bind, ColumnIndex, 0);
            ColBind^.buffer_address^ := nil;
            ColBind^.buffer_Length_address^ := 0;
            Result := StrToBoolEx(PAnsiChar(@FSmallLobBuffer[0]));
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      if ColBind^.buffer_type_address^ = FIELD_TYPE_BIT then begin
        {$R-}
        Len := FLengthArray[ColumnIndex];
        {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        case Len of
          1: Result := PByte(Buffer)^ <> 0;
          2: Result := ReverseWordBytes(Buffer) <> 0;
          3, 4: Result := ReverseLongWordBytes(Buffer, Len) <> 0;
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Buffer, Len) <> 0;
        end
      end else Result := StrToBoolEx(Buffer, True, False);
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
function TZAbstractMySQLResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  Buffer: PAnsiChar;
  Len: Ulong;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Result := 0;
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_TINY:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PShortInt(ColBind^.buffer)^
                          else Result := PByte(ColBind^.buffer)^;
        FIELD_TYPE_SHORT: if ColBind^.is_unsigned_address^ = 0
                          then Result := PSmallInt(ColBind^.buffer)^
                          else Result := PWord(ColBind^.buffer)^;
        FIELD_TYPE_LONG:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PLongInt(ColBind^.buffer)^
                          else Result := Integer(PLongWord(ColBind^.buffer)^);
        FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
        FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
        FIELD_TYPE_LONGLONG:  if ColBind^.is_unsigned_address^ = 0
                              then Result := Integer(PInt64(ColBind^.buffer)^)
                              else Result := Integer(PUInt64(ColBind^.buffer)^);
        FIELD_TYPE_YEAR: Result := PWord(ColBind^.buffer)^;
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
        FIELD_TYPE_TINY_BLOB,
        FIELD_TYPE_MEDIUM_BLOB,
        FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB,
        FIELD_TYPE_GEOMETRY:
          if not ColBind^.binary and ( ColBind^.Length[0]  > 0 ) and
             (ColBind^.Length[0]  < 13{Max Int32 Length = 11+#0} ) then begin
            ColBind^.buffer_address^ := @FSmallLobBuffer[0];
            ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1;//mysql sets $0 on to of data and corrupts our mem
            FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, ColBind^.mysql_bind, ColumnIndex, 0);
            ColBind^.buffer_address^ := nil;
            ColBind^.buffer_Length_address^ := 0;
            Result := RawToIntDef(@FSmallLobBuffer[0], 0);
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      if ColBind.buffer_type_address^ = FIELD_TYPE_BIT then begin
        case Len of
          1: Result := PByte(Buffer)^;
          2: Result := ReverseWordBytes(Buffer);
          3, 4: Result := ReverseLongWordBytes(Buffer, Len);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Buffer, Len);
        end
      end else Result := RawToIntDef(Buffer, 0);
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
function TZAbstractMySQLResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  Buffer: PAnsiChar;
  Len: ULong;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Result := 0;
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_TINY:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PShortInt(ColBind^.buffer)^
                          else Result := PByte(ColBind^.buffer)^;
        FIELD_TYPE_SHORT: if ColBind^.is_unsigned_address^ = 0
                          then Result := PSmallInt(ColBind^.buffer)^
                          else Result := PWord(ColBind^.buffer)^;
        FIELD_TYPE_LONG:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PLongInt(ColBind^.buffer)^
                          else Result := PLongWord(ColBind^.buffer)^;
        FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
        FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
        FIELD_TYPE_LONGLONG:  if ColBind^.is_unsigned_address^ = 0
                              then Result := PInt64(ColBind^.buffer)^
                              else Result := PUInt64(ColBind^.buffer)^;
        FIELD_TYPE_YEAR: Result := PWord(ColBind^.buffer)^;
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
          if not ColBind^.binary and ( ColBind^.Length[0]  > 0 ) and
             (ColBind^.Length[0]  < 22{Max Int64 Length = 20+#0}) then
          begin
            ColBind^.buffer_address^ := @FSmallLobBuffer[0];
            ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1;//mysql sets $0 on to of data and corrupts our mem
            FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, ColBind^.mysql_bind, ColumnIndex, 0);
            ColBind^.buffer_address^ := nil;
            ColBind^.buffer_Length_address^ := 0;
            Result := RawToInt64Def(@FSmallLobBuffer[0], 0);
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      if ColBind.buffer_type_address^ = FIELD_TYPE_BIT then
        case Len of
          1: Result := PByte(Buffer)^;
          2: Result := ReverseWordBytes(Buffer);
          3, 4: Result := ReverseLongWordBytes(Buffer, Len);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Buffer, Len);
        end
      else Result := RawToInt64Def(Buffer, 0);
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
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractMySQLResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  Buffer: PAnsiChar;
  Len: ULong;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  Result := 0;
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_TINY:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PShortInt(ColBind^.buffer)^
                          else Result := PByte(ColBind^.buffer)^;
        FIELD_TYPE_SHORT: if ColBind^.is_unsigned_address^ = 0
                          then Result := PSmallInt(ColBind^.buffer)^
                          else Result := PWord(ColBind^.buffer)^;
        FIELD_TYPE_LONG:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PLongInt(ColBind^.buffer)^
                          else Result := PLongWord(ColBind^.buffer)^;
        FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(ColBind^.buffer)^);
        FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(ColBind^.buffer)^);
        FIELD_TYPE_LONGLONG:  if ColBind^.is_unsigned_address^ = 0
                              then Result := PInt64(ColBind^.buffer)^
                              else Result := PUInt64(ColBind^.buffer)^;
        FIELD_TYPE_YEAR: Result := PWord(ColBind^.buffer)^;
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
            ColBind^.buffer_address^ := @FSmallLobBuffer[0];
            ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1;//mysql sets $0 on to of data and corrupts our mem
            FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, ColBind^.mysql_bind, ColumnIndex, 0);
            ColBind^.buffer_address^ := nil;
            ColBind^.buffer_Length_address^ := 0;
            Result := RawToUInt64Def(@FSmallLobBuffer[0], 0);
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      if ColBind.buffer_type_address^ = FIELD_TYPE_BIT then
        case Len of
          1: Result := PByte(Buffer)^;
          2: Result := ReverseWordBytes(Buffer);
          3, 4: Result := ReverseLongWordBytes(Buffer, Len);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(Buffer, Len);
        end
      else Result := RawToUInt64Def(Buffer, 0);
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
function TZAbstractMySQLResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := GetDouble(ColumnIndex);
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
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Result := 0;
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_TINY:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PShortInt(ColBind^.buffer)^
                          else Result := PByte(ColBind^.buffer)^;
        FIELD_TYPE_SHORT: if ColBind^.is_unsigned_address^ = 0
                          then Result := PSmallInt(ColBind^.buffer)^
                          else Result := PWord(ColBind^.buffer)^;
        FIELD_TYPE_LONG:  if ColBind^.is_unsigned_address^ = 0
                          then Result := PLongInt(ColBind^.buffer)^
                          else Result := Integer(PLongWord(ColBind^.buffer)^);
        FIELD_TYPE_FLOAT:   if ColBind^.decimals < 20
                            then Result := RoundTo(PSingle(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PSingle(ColBind^.buffer)^;
        FIELD_TYPE_DOUBLE:  if ColBind^.decimals < 20
                            then Result := RoundTo(PDouble(ColBind^.buffer)^, ColBind^.decimals*-1)
                            else Result := PDouble(ColBind^.buffer)^;
        FIELD_TYPE_LONGLONG:  if ColBind^.is_unsigned_address^ = 0
                              then Result := Integer(PInt64(ColBind^.buffer)^)
                              else Result := Integer(PUInt64(ColBind^.buffer)^);
        FIELD_TYPE_YEAR: Result := PWord(ColBind^.buffer)^;
        FIELD_TYPE_STRING,
        FIELD_TYPE_ENUM,
        FIELD_TYPE_SET:
          ZSysUtils.SQLStrToFloatDef(PAnsiChar(ColBind^.buffer), 0, Result, ColBind^.Length[0] );
        FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
          if ( ColBind^.Length[0]  > 0 ) and
             (ColBind^.Length[0]  < 30{Max Extended Length = 28 ??} ) then begin
            ColBind^.buffer_address^ := @FSmallLobBuffer[0];
            ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1;//mysql sets $0 on to of data and corrupts our mem
            FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, ColBind^.mysql_bind, ColumnIndex, 0);
            ColBind^.buffer_address^ := nil;
            ColBind^.buffer_Length_address^ := 0;
            RawToFloatDef(PAnsichar(@FSmallLobBuffer[0]), {$IFDEF NO_ANSICHAR}Ord{$ENDIF}('.'), 0, Result);
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      ZSysUtils.SQLStrToFloatDef(Buffer, 0, Result, Len);
  end;
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
begin
  Result := GetDouble(ColumnIndex);
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
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Result := nil;
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        FIELD_TYPE_STRING,
        FIELD_TYPE_ENUM, FIELD_TYPE_SET:
          Result := BufferToBytes(Pointer(ColBind^.buffer), ColBind^.Length[0] );
        FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
          if ColBind^.Length[0] < SizeOf(FSmallLobBuffer) then begin
            ColBind^.buffer_address^ := @FSmallLobBuffer[0];
            ColBind^.buffer_Length_address^ := SizeOf(FSmallLobBuffer)-1;//mysql sets $0 on to of data and corrupts our mem
            FPlainDriver.mysql_stmt_fetch_column(FPMYSQL^, ColBind^.mysql_bind, ColumnIndex, 0);
            ColBind^.buffer_address^ := nil;
            ColBind^.buffer_Length_address^ := 0;
            Result := BufferToBytes(@FSmallLobBuffer[0], ColBind^.Length[0] );
          end else
            Result := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetBytes;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      Result := BufferToBytes(Buffer, Len);
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
function TZAbstractMySQLResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  Failed := False;
  if fBindBufferAllocated then begin
    {$R-}
    ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_LONG:
          if ColBind^.is_unsigned_address^ = 0 then
            Result := PLongInt(ColBind^.buffer)^
          else
            Result := PLongWord(ColBind^.buffer)^;
        FIELD_TYPE_FLOAT:     Result := PSingle(ColBind^.buffer)^;
        FIELD_TYPE_DOUBLE:    Result := PDouble(ColBind^.buffer)^;
        FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_DATETIME,
        FIELD_TYPE_NEWDATE:
          if not sysUtils.TryEncodeDate(
              PMYSQL_TIME(ColBind^.buffer)^.Year,
              PMYSQL_TIME(ColBind^.buffer)^.Month,
              PMYSQL_TIME(ColBind^.buffer)^.Day, Result) then
            Result := encodeDate(1900, 1, 1);
        FIELD_TYPE_LONGLONG:
          if ColBind^.is_unsigned_address^ = 0
          then Result := PInt64(ColBind^.buffer)^
          else Result := PUInt64(ColBind^.buffer)^;
        FIELD_TYPE_YEAR: TryEncodeDate(PWord(ColBind^.buffer)^, 1,1, Result);
        FIELD_TYPE_STRING:
          begin
            if ColBind^.Length[0]  = ConSettings^.ReadFormatSettings.DateFormatLen then
              Result := RawSQLDateToDateTime(PAnsiChar(ColBind^.buffer),
                ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed)
            else
              Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                RawSQLTimeStampToDateTime(PAnsiChar(ColBind^.buffer),
                  ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Failed;
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then begin
      if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
        Result := RawSQLDateToDateTime(Buffer,  Len, ConSettings^.ReadFormatSettings, Failed)
      else
        Result := Int(RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
      LastWasNull := Failed;
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
function TZAbstractMySQLResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  Failed := False;
  if fBindBufferAllocated then begin
    {$R-}
    ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_LONG:
          if ColBind^.is_unsigned_address^ = 0
          then Result := PLongInt(ColBind^.buffer)^
          else Result := PLongWord(ColBind^.buffer)^;
        FIELD_TYPE_FLOAT:     Result := PSingle(ColBind^.buffer)^;
        FIELD_TYPE_DOUBLE:    Result := PDouble(ColBind^.buffer)^;
        FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME, FIELD_TYPE_TIME:
          if not sysUtils.TryEncodeTime(
              PMYSQL_TIME(ColBind^.buffer)^.Hour,
              PMYSQL_TIME(ColBind^.buffer)^.Minute,
              PMYSQL_TIME(ColBind^.buffer)^.Second,
              0{PMYSQL_TIME(ColBind^.buffer)^.second_part div 1000}, Result) then
            Result := 0;
        FIELD_TYPE_LONGLONG:
          if ColBind^.is_unsigned_address^ = 0
          then Result := PInt64(ColBind^.buffer)^
          else Result := PUInt64(ColBind^.buffer)^;
        FIELD_TYPE_YEAR: TryEncodeDate(PWord(ColBind^.buffer)^, 1,1, Result);
        FIELD_TYPE_STRING: begin
            if PByte(PAnsiChar(ColBind^.buffer)+2)^ = Ord(':') then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(PAnsiChar(ColBind^.buffer),
                ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed)
            else
              Result := Frac(RawSQLTimeStampToDateTime(PAnsiChar(ColBind^.buffer),
                ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Failed;
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then begin
      if PByte(Buffer+2)^ = Ord(':') then //possible date if Len = 10 then
        Result := RawSQLTimeToDateTime(Buffer,Len, ConSettings^.ReadFormatSettings, Failed)
      else
        Result := Frac(RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
      LastWasNull := Failed;
    end;
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
  tmp: TDateTime;
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  Result := 0;
  Failed := False;
  if fBindBufferAllocated then begin
    {$R-}
    ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := ColBind^.is_null =1;
    if not LastWasNull then
      //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
      Case ColBind^.buffer_type_address^ of
        FIELD_TYPE_FLOAT:     Result := PSingle(ColBind^.buffer)^;
        FIELD_TYPE_DOUBLE:    Result := PDouble(ColBind^.buffer)^;
        FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
          if not sysUtils.TryEncodeDate(
              PMYSQL_TIME(ColBind^.buffer)^.Year,
              PMYSQL_TIME(ColBind^.buffer)^.Month,
              PMYSQL_TIME(ColBind^.buffer)^.Day, Result) then
            Result := encodeDate(1900, 1, 1);
        FIELD_TYPE_TIME: sysUtils.TryEncodeTime(
              PMYSQL_TIME(ColBind^.buffer)^.Hour,
              PMYSQL_TIME(ColBind^.buffer)^.Minute,
              PMYSQL_TIME(ColBind^.buffer)^.Second,
              0{PMYSQL_TIME(ColBind^.buffer)^.second_part div 1000}, Result);
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
          if ColBind^.is_unsigned_address^ = 0
          then Result := PInt64(ColBind^.buffer)^
          else Result := PUInt64(ColBind^.buffer)^;
        FIELD_TYPE_YEAR: TryEncodeDate(PWord(ColBind^.buffer)^, 1,1, Result);
        FIELD_TYPE_STRING: begin
            if PByte(PAnsiChar(ColBind^.buffer)+2)^ = Ord(':') then
              Result := RawSQLTimeToDateTime(PAnsiChar(ColBind^.buffer),
                ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed)
            else if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - ColBind^.Length[0] ) <= 4 then
              Result := RawSQLTimeStampToDateTime(PAnsiChar(ColBind^.buffer), ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed)
            else
              Result := RawSQLDateToDateTime(PAnsiChar(ColBind^.buffer), ColBind^.Length[0] , ConSettings^.ReadFormatSettings, Failed);
            LastWasNull := Failed;
          end;
      end
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      if PByte(Buffer+2)^ = Ord(':') then
        Result := RawSQLTimeToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed)
      else
        if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4 then
          Result := RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed)
        else
          Result := RawSQLDateToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed);
  end;
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
  ColBind: PMYSQL_aligned_BIND;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex - 1;
  {$ENDIF}
  {$R-}
  ColBind := @FMYSQL_aligned_BINDs[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Result := nil;
  if fBindBufferAllocated then begin
    LastWasNull := ColBind^.is_null = 1;
    if not LastWasNull then
      if ColBind^.buffer_address^ = nil then
        if ColBind^.binary
        then Result := TZMySQLPreparedBlob.Create(FplainDriver,
            ColBind, FMYSQL_STMT, ColumnIndex, Self)
        else Result := TZMySQLPreparedClob.Create(FplainDriver,
            ColBind, FMYSQL_STMT, ColumnIndex, Self)
      else begin
          Buffer := GetPAnsiChar(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Len);
          Result := TZAbstractClob.CreateWithData(Buffer, Len,
            ConSettings^.ClientCodePage^.CP, ConSettings);
        end;
  end else begin
    {$R-}
    Buffer := PMYSQL_ROW(FRowHandle)[ColumnIndex];
    Len := FLengthArray^[ColumnIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    LastWasNull := Buffer = nil;
    if not LastWasNull then
      if ColBind^.binary
      then Result := TZAbstractBlob.CreateWithData(Buffer, Len)
      else Result := TZAbstractClob.CreateWithData(Buffer, Len,
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
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZMySQL_Store_ResultSet.MoveAbsolute(Row: Integer): Boolean;
  function Seek(const RowIndex: ULongLong): Boolean;
  begin
    if fBindBufferAllocated then begin
      FPlainDriver.mysql_stmt_data_seek(FMYSQL_STMT, RowIndex);
      Result := FPlainDriver.mysql_stmt_fetch(FMYSQL_STMT) = 0;
    end else begin
      FPlainDriver.mysql_data_seek(FQueryHandle, RowIndex);
      FRowHandle := FPlainDriver.mysql_fetch_row(FQueryHandle);
      Result := FRowHandle <> nil;
      if Result
      then FLengthArray := FPlainDriver.mysql_fetch_lengths(FQueryHandle)
      else FLengthArray := nil;
    end;
  end;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or ((MaxRows > 0) and (Row > MaxRows)) then
    Exit;

  if not fBindBufferAllocated and (FQueryHandle = nil) then begin
    FQueryHandle := FPlainDriver.mysql_store_result(FPMYSQL^);
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
    if (Row = 0) and (Row < LastRowNo) then begin//handle beforefirst state
      if RowNo > 1
      then Result := Seek(0) //seek back to first position
      else Result := True;   //we're on first pos already
      FFirstRowFetched := RowNo > 0; //indicate the FirstRow is obtained already
      RowNo := 0; //set BeforeFirst state
    end else begin
      RowNo := Row;
      if (Row >= 1) and (Row <= LastRowNo) then
        Result := Seek(RowNo - 1)
      else begin
        if not fBindBufferAllocated then begin
          FRowHandle := nil;
          FLengthArray := nil;
        end;
        Result := False;
      end;
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZMySQL_Store_ResultSet.OpenCursor;
begin
  inherited OpenCursor;
  if FPMYSQL_STMT^ <> nil then begin
    if not FIsOutParamResult then begin
      FMYSQL_STMT := FPMYSQL_STMT^;
      if FPlainDriver.mysql_stmt_store_result(FMYSQL_STMT)=0
      then LastRowNo := FPlainDriver.mysql_stmt_num_rows(FMYSQL_STMT)
      else checkMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcOther, 'mysql_stmt_store_result', Self)
    end;
  end else begin
    FQueryHandle := FPlainDriver.mysql_store_result(FPMYSQL^);
    if Assigned(FQueryHandle)
    then LastRowNo := FPlainDriver.mysql_num_rows(FQueryHandle)
    else CheckMySQLError(FPlainDriver, FPMYSQL^, FMYSQL_STMT, lcOther, 'mysql_store_result', Self)
  end;
end;

procedure TZMySQL_Store_ResultSet.ResetCursor;
begin
  if not Closed then begin
    if fBindBufferAllocated then begin
      if Assigned(FMYSQL_STMT) then
        if FPlainDriver.mysql_stmt_free_result(FMYSQL_STMT) <> 0 then
          checkMySQLError(FPlainDriver,FPMYSQL^, FMYSQL_STMT, lcOther, '', Self);
    end else if FQueryHandle <> nil then
      FPlainDriver.mysql_free_result(FQueryHandle);
    inherited ResetCursor;
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
constructor TZMySQLCachedResolver.Create(const PlainDriver: TZMySQLPlainDriver;
  MySQL: PPMySQL; MYSQL_STMT: PPMYSQL_STMT; const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FPlainDriver := PlainDriver;
  FPMYSQL := MySQL;
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
          (OldRowAccessor.GetULong(FAutoColumnIndex, LastWasNull)=0)))
  then {if FMYSQL_STMT <> nil
    then NewRowAccessor.SetULong(FAutoColumnIndex, FPlainDriver.mysql_stmt_insert_id(FMYSQL_STMT^))  //EH: why does it not work!?
    else }NewRowAccessor.SetULong(FAutoColumnIndex, FPlainDriver.mysql_insert_id(FPMYSQL^)); //and this also works with the prepareds??!
end;

{ TZMySQLPreparedClob }
constructor TZMySQLPreparedClob.Create(const PlainDriver: TZMySQLPlainDriver;
  Bind: PMYSQL_aligned_BIND; StmtHandle: PPMYSQL_STMT;
  ColumnIndex: Cardinal; const Sender: IImmediatelyReleasable);
var
  offset: ULong;
  Status: Integer;
begin
  inherited Create;
  FConSettings := Sender.GetConSettings;
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  FBlobSize := Bind^.Length[0]+1; //MySQL sets a trailing #0 on top of data
  GetMem(FBlobData, FBlobSize);
  offset := 0;
  Bind^.buffer_Length_address^ := Bind^.Length[0]; //indicate size of Buffer
  Bind^.buffer_address^ := FBlobData;
  Status := PlainDriver.mysql_stmt_fetch_column(StmtHandle, bind^.mysql_bind, ColumnIndex, offset); //move data to buffer
  Bind^.buffer_address^ := nil; //set nil again
  Bind^.buffer_Length_address^ := 0;
  if Status = 1 then
    checkMySQLError(PlainDriver, nil, StmtHandle^, lcOther, '', Sender);
  PByte(PAnsiChar(FBlobData)+FBlobSize-1)^ := Ord(#0);
End;

{ TZMySQLPreparedBlob }
constructor TZMySQLPreparedBlob.Create(const PlainDriver: TZMySQLPlainDriver;
  Bind: PMYSQL_aligned_BIND; StmtHandle: PMySql_Stmt;
  ColumnIndex: Cardinal; const Sender: IImmediatelyReleasable);
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
    checkMySQLError(PlainDriver, nil, StmtHandle, lcOther, '', Sender);
End;

{ TZMySQL_Use_ResultSet }

procedure TZMySQL_Use_ResultSet.OpenCursor;
begin
  inherited OpenCursor;
  if FMYSQL_STMT = nil then
    FQueryHandle := FPlainDriver.mysql_use_result(FPMYSQL^)
end;

procedure TZMySQL_Use_ResultSet.ResetCursor;
begin
  if not Closed then begin
    if fBindBufferAllocated then begin
      if FMYSQL_STMT <> nil then
        while FPlainDriver.mysql_stmt_fetch(FMYSQL_STMT) in [0, MYSQL_DATA_TRUNCATED] do;
    end else if FQueryHandle <> nil then
      {need to fetch all temporary until handle = nil else all other queries are out of sync
       see: http://dev.mysql.com/doc/refman/5.0/en/mysql-use-result.html}
      while FPlainDriver.mysql_fetch_row(FQueryHandle) <> nil do;
    inherited ResetCursor;
  end;
end;

end.
