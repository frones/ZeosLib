{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         DBLib Resultset common functionality            }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcDbLibResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses
{$IFNDEF FPC}
  DateUtils,
{$ENDIF}
  {$IFDEF MORMOT2}
  mormot.db.core, mormot.core.datetime, mormot.core.text, mormot.core.base,
  {$ELSE MORMOT2} {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS} {$ENDIF MORMOT2}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types{$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF}{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  FmtBCD,
  ZDbcIntfs, ZDbcResultSet, ZCompatibility, ZDbcResultsetMetadata,
  ZDbcGenericResolver, ZDbcCachedResultSet, ZDbcCache, ZDbcDBLib,
  ZPlainDBLibDriver;

type
  TZAbstractDblibDataProvider = Class
    protected
      FPLainDriver: TZDBLIBPLainDriver;
      FHandle: PDBPROCESS;
      FConnection: IZDBLibConnection;
      FNeedDbCanQuery: Boolean;
    public
      property needDbCanQuery: Boolean read FNeedDbCanQuery;
      constructor Create(Connection: IZDBLibConnection); virtual;
      function Next: Boolean; virtual; abstract;
      procedure GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer); virtual; abstract;
  end;

  TZPlainDblibDataProvider = class(TZAbstractDblibDataProvider)
    protected
      FCheckDbDead: Boolean;
    public
      constructor Create(Connection: IZDBLibConnection; const CheckDbDead: Boolean); reintroduce;
      function Next: Boolean; override;
      procedure GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer); override;
  end;

  TZCachedDblibField = record
    IsNull: Boolean;
    Data: TBytes;
  end;

  TZCachedDblibRow = class
    Fields: Array of TZCachedDblibField;
    NextRow: TZCachedDblibRow;
  end;

  TZCachedDblibDataProvider = class(TZAbstractDblibDataProvider)
    protected
      FRootRow: TZCachedDblibRow;
    public
      procedure LoadData;
      constructor Create(Connection: IZDBLibConnection); override;
      destructor Destroy; override;
      function Next: Boolean; override;
      procedure GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer); override;
  end;

  IZDblibResultSetMetadata = interface(IZResultSetMetaData)
  ['{48B1C2EC-DBD4-4A5D-B41B-E75F1E22F909}']
    procedure LoadColumns;
  end;

  TZDblibResultSetMetadata = class(TZAbstractResultSetMetadata, IZDblibResultSetMetadata)
  protected
    procedure SetColumnCodePageFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
    procedure SetColumnPrecisionFromGetColumnsRS(
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
    procedure SetColumnScaleFromGetColumnsRS(
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
  end;

  TZDBLIBColumnInfo = class(TZColumnInfo)
  public
    TDSType: TTDSType;
  end;
  {** Implements DBLib ResultSet. }
  TZDBLibResultSet = class(TZAbstractReadOnlyResultSet_A,IZResultSet)
  private
    FSQL: string;
    FCheckDBDead: Boolean;
    FHandle: PDBPROCESS;
    DBLibColumnCount: Integer;
    FDecimalSep: Char;
    FClientCP: Word;
    FByteBuffer: PByteBuffer;
    procedure CheckColumnIndex(ColumnIndex: Integer);
  protected
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: TZDBLIBPLainDriver;
    FDataProvider: TZAbstractDblibDataProvider;
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string);
    destructor Destroy; override;

    procedure BeforeClose; override;

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; Var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); overload;
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); overload;
    function GetBlob(ColumnIndex: Integer;
      LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    {$IFDEF WITH_COLUMNS_TO_JSON}
    procedure ColumnsToJSON(ResultsWriter: {$IFDEF MORMOT2}TResultsWriter{$ELSE}TJSONWriter{$ENDIF}; JSONComposeOptions: TZJSONComposeOptions); overload; virtual;
    {$ENDIF}
    function Next: Boolean; override;
  end;

  {** Implements a cached resolver with mssql and sybase specific functionality. }
  TZDBLibCachedResolver = class (TZGenerateSQLCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

uses ZMessages, ZDbcLogging, ZDbcDBLibUtils, ZEncoding, ZSysUtils, ZFastCode,
  ZDbcUtils, ZDbcMetadata, ZExceptions
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

constructor TZAbstractDblibDataProvider.Create(Connection: IZDBLibConnection);
begin
  inherited Create;
  FConnection := Connection;
  FPLainDriver := TZDBLIBPLainDriver(Connection.GetIZPlainDriver.GetInstance);
  FHandle := Connection.GetConnectionHandle;
end;

constructor TZPlainDblibDataProvider.Create(Connection: IZDBLibConnection; const CheckDbDead: Boolean);
begin
  inherited Create(Connection);
  FCheckDbDead := CheckDbDead;
  FNeedDbCanQuery := True;
end;

function TZPlainDblibDataProvider.Next: Boolean;
begin
  Result := False;
  if FCheckDBDead then
    if FPlainDriver.dbDead(FHandle) <> 0 then
      Exit;
//!!! maybe an error message other than dbconnection is dead should be raised
  case FPlainDriver.dbnextrow(FHandle) of
    REG_ROW: Result := True;
    NO_MORE_ROWS: ;
    DBFAIL: FConnection.CheckDBLibError(lcFetch, 'NEXT', nil);
    BUF_FULL: raise EZSQLException.Create('The data doesn''t fit the dblib buffer');//should not happen because we are not using dblibc buffering.
  else
   // If a compute row is read, the computeid of the row is returned
    Result := False;
  end;
end;

procedure TZPlainDblibDataProvider.GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer);
begin
  DatPtr := FPLainDriver.dbData(FHandle, ColIndex);
  DatLen := FPLainDriver.dbDatLen(FHandle, ColIndex);
end;

constructor TZCachedDblibDataProvider.Create(Connection: IZDBLibConnection);
begin
  inherited;
  FRootRow := nil;
  FNeedDbCanQuery := false;
end;

destructor TZCachedDblibDataProvider.Destroy;
begin
  while Next do ; // next removes one row after the other
  inherited;
end;

function TZCachedDblibDataProvider.Next: Boolean;
var
  currentRow: TZCachedDblibRow;
begin
  Result := false;
  if Assigned(FRootRow) then begin
    currentRow := FRootRow;
    FRootRow := currentRow.NextRow;
    SetLength(currentRow.Fields, 0);
    FreeAndNil(currentRow);
    Result := Assigned(FRootRow);
  end;
end;

procedure TZCachedDblibDataProvider.GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer);
begin
  Dec(ColIndex);
  DatPtr := nil;
  DatLen := 0;
  if Assigned(FRootRow) then begin
    if (ColIndex >= 0) and (ColIndex <= High(FRootRow.Fields)) then begin
      DatLen := Length(FRootRow.Fields[ColIndex].Data);
      if DatLen > 0 then begin
        DatPtr := Pointer(@(FRootRow.Fields[ColIndex].Data[0]))
      end else begin
        if FRootRow.Fields[ColIndex].IsNull
        then DatPtr := nil
        else DatPtr := Pointer(High(NativeUInt));
      end;
    end;
  end;
end;

procedure TZCachedDblibDataProvider.LoadData;
var
  colCount: Integer;
  plainProvider: TZPlainDblibDataProvider;
  DatLen: Integer;
  Data: Pointer;
  currentRow: TZCachedDblibRow;
  x: Integer;

begin
  colCount := FPlainDriver.dbnumcols(FHandle);
  plainProvider := TZPlainDblibDataProvider.Create(FConnection, false);
  try
    FRootRow := TZCachedDblibRow.Create; // this is just a dummy to be on for being before the first row.
    currentRow := FRootRow;
    SetLength(currentRow.Fields, colCount);
    while plainProvider.Next do begin
      currentRow.NextRow := TZCachedDblibRow.Create;
      currentRow := currentRow.NextRow;

      SetLength(currentRow.Fields, colCount);
      for x := 0 to colCount - 1 do begin
        plainProvider.GetColData(x + 1, Data, DatLen);
        currentRow.Fields[x].IsNull := (Data = nil) and (DatLen = 0);
        if DatLen > 0 then begin
          SetLength(currentRow.Fields[x].Data, DatLen);
          Move(Data^, currentRow.Fields[x].Data[0], DatLen);
        end;
      end;
    end;
  finally
    FreeAndNil(plainProvider);
  end;
  FPLainDriver.dbCanQuery(FHandle);
end;


{ TZDBLibResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param Handle a DBLib specific query handle.
}
constructor TZDBLibResultSet.Create(const Statement: IZStatement; const SQL: string);
begin
  inherited Create(Statement, SQL,
    TZDblibResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  Statement.GetConnection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := TZDBLIBPLainDriver(FDBLibConnection.GetIZPlainDriver.GetInstance);
  FHandle := FDBLibConnection.GetConnectionHandle;
  FByteBuffer := FDBLibConnection.GetByteBufferAddress;
  FSQL := SQL;
  FCheckDBDead := FPlainDriver.GetProtocol = 'mssql';
  //FDataProvider := TZPlainDblibDataProvider.Create(Statement.GetConnection as IZDbLibConnection , FCheckDBDead);
  FDataProvider := TZCachedDblibDataProvider.Create(FDBLibConnection);
  FClientCP := ConSettings.ClientCodePage.CP;
  Open;
end;

destructor TZDBLibResultSet.Destroy;
begin
  if Assigned(FDataProvider) then
    FreeAndNil(FDataProvider);
  inherited;
end;

{**
  Opens this recordset.
}
procedure TZDBLibResultSet.Open;
var
  ColumnInfo: TZDBLIBColumnInfo;
  I: Integer;
  ColInfo: DBCOL;
  tdsColInfo: TTDSDBCOL;
  NeedsLoading: Boolean;
  RetVal: RETCODE;
label AssignGeneric;
  procedure AssignGenericColumnInfoFromZDBCOL(ColumnInfo: TZDBLIBColumnInfo; ColInfo: ZDBCOL);
  begin
    with ColumnInfo do begin
      TDSType := TTDSType(ColInfo.Typ);
      if TDSType in [tdsNumeric, tdsDecimal] then begin
        Scale := ColInfo.Scale;
        Precision := ColInfo.Precision;
      end else if TDSType = tdsMoney then begin
        Scale := 4;
        Precision := 19;
        Currency := True;
      end else begin
        Precision := ColInfo.MaxLength;
        if (TDSType in [tdsChar, tdsBigBinary, tdsBigChar, tdsBigNChar]) or
           ((TDSType = tdsBinary) and not ColInfo.VarLength)
        then Scale := Precision
        else Scale := 0;
      end;
      ColumnType := ConvertTDSTypeToSqlType(TDSType, Precision, Scale);
      if (ColumnType = stBytes) and (Precision = 2147483647) then begin
        Precision := 0;
        Scale := 0;
      end;
      if ColumnType = stUnknown
      then NeedsLoading := true;
      if (TDSType = tdsNumeric) and (Scale = 0) and (Precision = 19)
      then NeedsLoading := true; // we cannot be sure if it is bigint or numeric - let somebody else deal with this...
      CaseSensitive := ColInfo.CaseSensitive = 1;
      Nullable := TZColumnNullableType(ColInfo.Null);
      ReadOnly := not (ColInfo.Updatable = 1);
      Writable := ColInfo.Updatable = 1;
      AutoIncrement := ColInfo.Identity;
      Signed := (ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stFloat, stCurrency, stDouble, stBigDecimal]);
    end;
  end;
  procedure ValueToString(P: PAnsiChar; var Result: String);
  var L: NativeUInt;
  begin
    L := ZFastCode.StrLen(P);
    {$IFDEF UNICODE}
    PRawToUnicode(P, L, FClientCP, Result);
    {$ELSE}
    ZSetString(P, L, Result{$IFDEF WITH_RAWBYTESRING}, FClientCP{$ENDIF});
    {$ENDIF}
  end;
begin
  NeedsLoading := false;
//Check if the current statement can return rows
  if FPlainDriver.dbCmdRow(FHandle) <> DBSUCCEED then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  { Fills the column info }
  ColumnsInfo.Clear;
  DBLibColumnCount := FPlainDriver.dbnumcols(FHandle);
  for i := 1 to DBLibColumnCount do
  begin
    ColumnInfo := TZDBLIBColumnInfo.Create;
    if FPlainDriver.DBLibraryVendorType = lvtFreeTDS then begin
      tdsColInfo.SizeOfStruct := SizeOf(TTDSDBCOL);
      FillChar(tdsColInfo.Name[0], tdsColInfo.SizeOfStruct- SizeOf(DBInt), #0);
      RetVal := DBFAIL;
      if Assigned(FPlainDriver.dbtablecolinfo) then begin
        RetVal := FPlainDriver.dbtablecolinfo(FHandle, I, @tdsColInfo);
        if RetVal <> DBSUCCEED then
          FDBLibConnection.CheckDBLibError(lcFetch, 'dbtablecolinfo', IImmediatelyReleasable(FWeakImmediatRelPtr));
      end;
      if RetVal <> DBSUCCEED then
        RetVal := FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @tdsColInfo); //might be possible for computed or cursor columns
      if RetVal <> DBSUCCEED then
          goto AssignGeneric;
      ValueToString(@tdsColInfo.Name[0], ColumnInfo.ColumnName );
      if Byte(tdsColInfo.ActualName[0]) = Ord(#0)
      then ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
      else ValueToString(@tdsColInfo.ActualName[0], ColumnInfo.ColumnLabel);
      if Byte(tdsColInfo.TableName[0]) <> Ord(#0)
      then ValueToString(@tdsColInfo.TableName[0], ColumnInfo.TableName);
      AssignGenericColumnInfoFromZDBCOL(ColumnInfo, tdsColInfo.ColInfo);
    end else if FDBLibConnection.GetProvider = dpMsSQL then begin
      ColInfo.SizeOfStruct := SizeOf(DBCOL); //before execute dbcolinfo we need to set the record size -> 122 Byte or we fail
      if FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @ColInfo) <> DBSUCCEED then //might be possible for computed or cursor columns
        goto AssignGeneric;
      ValueToString(@ColInfo.Name[0], ColumnInfo.ColumnName);
      if Byte(ColInfo.ActualName[0]) = Ord(#0)
      then ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
      else ValueToString(@ColInfo.ActualName[0], ColumnInfo.ColumnLabel);
      if (Byte(ColInfo.TableName[0]) <> Ord(#0)) then
        ValueToString(@ColInfo.TableName[0], ColumnInfo.TableName);
      AssignGenericColumnInfoFromZDBCOL(ColumnInfo, ColInfo.ColInfo);
    end else with ColumnInfo do begin
AssignGeneric:  {this is the old way we did determine the ColumnInformations}
      ValueToString(FPlainDriver.dbColSource(FHandle, I), ColumnName);
      ValueToString(FPlainDriver.dbColName(FHandle, I), ColumnLabel);
      TDSType := TTDSType(FPlainDriver.dbColtype(FHandle, I));
      Precision := FPlainDriver.dbCollen(FHandle, I);
      Scale := 0;
      ColumnType := ConvertTDSTypeToSqlType(TDSType, Precision, Scale);
      if ColumnInfo.ColumnType = stUnknown
      then NeedsLoading := true;
      Currency := TDSType in [tdsMoney, tdsMoney4, tdsMoneyN];
      Signed := not (TDSType = tdsInt1);
    end;
    if (ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then begin
      ColumnInfo.ColumnCodePage := FClientCP;
      if (ColumnInfo.ColumnType in [stString, stUnicodeString])
      then ColumnInfo.Precision := ColumnInfo.Precision div ConSettings.ClientCodePage.CharWidth
      else ColumnInfo.Precision := -1;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  if FDataProvider is TZCachedDblibDataProvider then begin
    (FDataProvider as TZCachedDblibDataProvider).LoadData;
    FCursorLocation := rctClient;
    if NeedsLoading then
      (GetMetaData as IZDblibResultSetMetadata).LoadColumns;
  end else
    FCursorLocation := rctServer;

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
procedure TZDBLibResultSet.BeforeClose;
begin
{ TODO -ofjanos -cGeneral : Maybe it needs a dbcanquery here. }
  if FDataProvider.needDbCanQuery then
    if Assigned(FHandle) then
      if FPlainDriver.dbDead(FHandle) = 0 then
        if FPlainDriver.dbCanQuery(FHandle) <> DBSUCCEED then
          FDBLibConnection.CheckDBLibError(lcFetch, 'CLOSE QUERY', IImmediatelyReleasable(FWeakImmediatRelPtr));
  FHandle := nil;
  inherited BeforeClose;
end;

{**
  Checks if the columnindex is in the proper range.
  An exception is generated if somthing is not ok.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZDBLibResultSet.CheckColumnIndex(ColumnIndex: Integer);
begin
  if (ColumnIndex > DBLibColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) or
     (ColumnIndex < FirstDbcIndex) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
end;

{$IFDEF WITH_COLUMNS_TO_JSON}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "XYZ" not used} {$ENDIF}
procedure TZDBLibResultSet.ColumnsToJSON(ResultsWriter: {$IFDEF MORMOT2}TResultsWriter{$ELSE}TJSONWriter{$ENDIF};
  JSONComposeOptions: TZJSONComposeOptions);
begin
  raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}
{$ENDIF}

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZDBLibResultSet.IsNull(ColumnIndex: Integer): Boolean;
var dbData: Pointer;
  DatLen: Integer;
begin
  CheckColumnIndex(ColumnIndex);
  FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, dbData, DatLen);
  Result := (dbData = nil) and (DatLen = 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the string in bytes
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  dbData: Pointer;
  dbDatLen: Integer;
begin
  Len := 0;
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, dbData, dbDatLen);
    Result := dbData;
    Len := dbDatLen;
    LastWasNull := Result = nil;
    if not LastWasNull then
      case TdsType of
        tdsBinary, tdsBigBinary, tdsVarBinary, tdsBigVarBinary, tdsVarChar,
        tdsBigVarChar, tdsText:
            Exit;
        tdsChar, tdsBigChar: begin
              Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(Result, dbDatLen);
            end;
        tdsUnique: begin
            Result := PAnsiChar(fByteBuffer);
            GUIDToBuffer(dbData, Result, [guidWithBrackets]);
            Len := 38;
          end;
        else begin
          Result := PAnsiChar(FByteBuffer);
          Len := FPlainDriver.dbconvert(FHandle, Ord(TDSType), dbData, Len,
            Ord(tdsVarChar), Pointer(Result), SizeOf(TByteBuffer)-1);
          //FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar');
        end;
      end;
  end;
end;

function TZDBLibResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(columnIndex);
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZDBLibResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
  Result := GetLong(ColumnIndex);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

function TZDBLibResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWidechar;
var
  dbData: PAnsiChar;
  dbDatLen: Integer;
label set_From_W, set_from_a, ConvCCP2W, ConvASCII;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Pointer(dbData), dbDatLen);
    Len := dbDatLen;
    LastWasNull := dbData = nil;
    if LastWasNull then
      Result := nil
    else case TdsType of
        tdsBinary, tdsBigBinary, tdsVarBinary, tdsBigVarBinary: begin
ConvASCII:   fUniTemp := Ascii7ToUnicodeString(dbData, Len);
            goto Set_From_W;
          end;
        tdsChar, tdsBigChar: begin
              Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(dbData, dbDatLen);
              goto set_from_a;
            end;
        tdsVarChar, tdsBigVarChar, tdsText:
set_from_a:if (FPLainDriver.DBLibraryVendorType <> lvtMS) or (ColumnCodePage <> zCP_NONE) then begin
ConvCCP2W:  PRawToUnicode(dbData, Len, ColumnCodePage, fUniTemp);
            goto Set_From_W;
          end else case ZDetectUTF8Encoding(dbData, Len) of
            etUTF8: begin
                ColumnCodePage := zCP_UTF8;
                goto ConvCCP2W;
              end;
            etAnsi: begin
                ColumnCodePage := FClientCP;
                goto ConvCCP2W;
              end;
            else goto ConvASCII;
          end;
        tdsUnique: begin
            Result := PWideChar(FByteBuffer);
            GUIDToBuffer(dbData, Result, [guidWithBrackets]);
            Len := 38;
          end;
        else begin
          Len := FPlainDriver.dbconvert(FHandle, Ord(TDSType), Pointer(dbData), Len,
            Ord(tdsVarChar), PByte(FByteBuffer), SizeOF(TByteBuffer)-1);
          FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar', IImmediatelyReleasable(FWeakImmediatRelPtr));
          fUniTemp := Ascii7ToUnicodeString(dbData, Len);
set_From_W: if Pointer(fUniTemp) = nil
          then Result := PEmptyUnicodeString
          else Result := Pointer(fUniTemp);
          FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar', IImmediatelyReleasable(FWeakImmediatRelPtr));
        end;
      end
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
function TZDBLibResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;
    Result := False;
    if not LastWasNull then
      if TDSType = tdsBit
      then Result := System.PByte(Data)^ <> 0
      else FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsBit),
          @Result, SizeOf(Result));
    FDBLibConnection.CheckDBLibError(lcOther, 'GETBOOLEAN', IImmediatelyReleasable(FWeakImmediatRelPtr));
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
function TZDBLibResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
var
  DL: Integer;
  Data: Pointer;
begin
  //DBLib -----> Col/Param starts whith index 1
  FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL);
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBYTES', IImmediatelyReleasable(FWeakImmediatRelPtr));
  LastWasNull := Data = nil;
  Result := Data;
  Len := DL;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;
    Result := 0;
    if Data <> nil then
      if TDSType = tdsInt4
      then Result := PInteger(Data)^
      else FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsInt4),
          @Result, SizeOf(Result));
    FDBLibConnection.CheckDBLibError(lcOther, 'GETINT', IImmediatelyReleasable(FWeakImmediatRelPtr));
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
function TZDBLibResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;

    Result := 0;
    if Data <> nil then
      case TDSType of
        tdsInt1: Result := PByte(Data)^;
        tdsInt2: Result := PSmallInt(Data)^;
        tdsInt4: Result := PInteger(Data)^;
        tdsInt8: Result := PInt64(Data)^;
        else FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsInt8),
          @Result, SizeOf(Int64));
      end;
    FDBLibConnection.CheckDBLibError(lcOther, 'GETLONG', IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;

    if Data <> nil then
      if TDSType = tdsFlt4
      then Result := PSingle(Data)^
      else FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsFlt4),
          @Result, SizeOf(Result))
    else Result := 0;
    FDBLibConnection.CheckDBLibError(lcOther, 'GETFLOAT', IImmediatelyReleasable(FWeakImmediatRelPtr));
  end;
end;

procedure TZDBLibResultSet.GetGUID(ColumnIndex: Integer; var Result: TGUID);
var
  dbData: Pointer;
  dbDatLen: Integer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, dbData, dbDatLen);
    LastWasNull := dbData = nil;
    if LastWasNull then
      FillChar(Result, SizeOf(TGUID), #0)
    else case TdsType of
      tdsBinary, tdsBigBinary, tdsVarBinary, tdsBigVarBinary:
        if dbDatLen = 16 then
          Result := PGUID(dbData)^;
      tdsVarChar, tdsBigVarChar, tdsText:
          Exit;
      tdsChar, tdsBigChar: begin
            dbDatLen := ZDbcUtils.GetAbsorbedTrailingSpacesLen(PAnsiChar(dbData), dbDatLen);
          end;
      tdsUnique: Result := PGUID(dbData)^;
      else FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar', IImmediatelyReleasable(FWeakImmediatRelPtr));
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
function TZDBLibResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;
    if Data <> nil then
      case TDStype of
        tdsInt1: Result := PByte(Data)^;
        tdsInt2: Result := PSmallInt(Data)^;
        tdsInt4: Result := PInteger(Data)^;
        tdsInt8: Result := PInt64(Data)^;
        tdsFlt4: Result := PSingle(Data)^;
        tdsFlt8: Result := PDouble(Data)^;
        else FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsFlt8),
            @Result, SizeOf(Result));
      end
    else Result := 0;
    FDBLibConnection.CheckDBLibError(lcOther, 'GETDOUBLE', IImmediatelyReleasable(FWeakImmediatRelPtr));
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
procedure TZDBLibResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    {EH: we can link against ntwdblib/FreeTDS-dblib and sysdblib
      we've a different record definition for the numeric records.
      The more sql-server + ntwdblib uses endian little but using the
      other two libs we've big endians. Common way is using the string conversions }
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;
    if Data <> nil then begin
      if TDStype <> tdsVarChar then begin
        DL := FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsVarchar),
            PByte(FByteBuffer), SizeOf(TByteBuffer));
        if DL = -1 then
          FDBLibConnection.CheckDBLibError(lcOther, 'GETBIGDECIMAL', IImmediatelyReleasable(FWeakImmediatRelPtr));
      end;
      LastWasNull := not TryRawToBCD(PAnsiChar(FByteBuffer), DL, Result, '.');
    end else Result := NullBCD;
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
function TZDBLibResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    {EH: we can link against ntwdblib/FreeTDS-dblib and sysdblib
      we've a different record definition for the numeric records.
      The more sql-server + ntwdblib uses endian little but using the
      other two libs we've big endians
      also conversion/using to tdsMoney fails using FreeTDS ->
      using the varychars was the only save way i found}
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;
    if Data <> nil then begin
      DL := FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsVarchar),
          PByte(FByteBuffer), SizeOf(TByteBuffer));
      SQLStrToFloatDef(PAnsiChar(FByteBuffer), 0, FDecimalSep, Result, DL);
    end else Result := 0;
    //FDBLibConnection.CheckDBLibError(lcOther, 'GETCURRENCY');
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
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZDBLibResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
var TS: TZTimeStamp;
begin
  GetTimestamp(ColumnIndex, TS);
  ZSysUtils.DateFromTimeStamp(TS, Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZDBLibResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
var TS: TZTimeStamp;
begin
  GetTimestamp(ColumnIndex, TS);
  ZSysUtils.TimeFromTimeStamp(TS, Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
procedure TZDBLibResultSet.GetTimestamp(ColumnIndex: Integer;
  Var Result: TZTimeStamp);
var
  DL: Integer;
  Data: Pointer;
  tdsTempDate: TTDSDBDATETIME;
  DT: TDateTime;
label Fill, Enc;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;
    if LastWasNull then
Fill: Fillchar(Result, SizeOf(TZTimeStamp), #0)
    else begin
      //Perfect conversion no need to crack and reencode the date.
      if TDSType = tdsDateTime then begin
Enc:    if FPlainDriver.DBLibraryVendorType = lvtFreeTDS //type diff
        then DT := PTDSDBDATETIME(Data)^.dtdays + 2 + (PTDSDBDATETIME(Data)^.dttime / 25920000)
        else DT := PDBDATETIME(Data)^.dtdays + 2 + (PDBDATETIME(Data)^.dttime / 25920000);
        DecodeDateTimeToTimeStamp(DT, Result);
      end else if (TDSType in [tdsNText, tdsNVarChar, tdsText, tdsVarchar, tdsChar]) then begin
        if (TDSType in [tdsNText, tdsChar]) then
          DL := ZDbcUtils.GetAbsorbedTrailingSpacesLen(PAnsichar(Data), DL);
        LastWasNull := not ZSysUtils.TryPCharToTimeStamp(PAnsichar(Data), DL, ConSettings^.ReadFormatSettings, Result);
        if LastWasNull then
          goto Fill;
      end else begin
        FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsDateTime),
          @tdsTempDate, SizeOf(tdsTempDate));
        FDBLibConnection.CheckDBLibError(lcOther, 'GETTIMESTAMP', IImmediatelyReleasable(FWeakImmediatRelPtr));
        Data :=  @tdsTempDate.dtdays;
        goto Enc;
      end;
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
function TZDBLibResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  DL: Integer;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;

    Result := nil;

    if LobStreamMode <> lsmRead then
      raise CreateReadOnlyException;
    if Data <> nil then
      case ColumnType of
        stBytes, stBinaryStream, stGUID:
          Result := TZLocalMemBLob.CreateWithData(Data, DL);
        stAsciiStream, stUnicodeStream: begin
            if (DL = 1) and (PByte(Data)^ = Ord(' ')) then DL := 0; //improve empty lobs, where len = 1 but string should be ''
            Result := TZLocalMemCLob.CreateWithData(Data, DL,
              FDBLibConnection.GetServerAnsiCodePage, ConSettings);
          end;
        stString, stUnicodeString: if ColumnCodePage = zCP_NONE then
            case ZDetectUTF8Encoding(Data, DL) of
              etUTF8: begin
                  ColumnCodePage := zCP_UTF8;
                  Result := TZLocalMemCLob.CreateWithData(Data, DL, zCP_UTF8, ConSettings);
                end;
              etAnsi: begin
                  ColumnCodePage := FClientCP;
                  Result := TZLocalMemCLob.CreateWithData(Data, DL, FClientCP, ConSettings);
                end;
              else //ASCII7
                Result := TZLocalMemCLob.CreateWithData(Data, DL, FClientCP, ConSettings);
            end
          else Result := TZLocalMemCLob.CreateWithData(Data, DL, ColumnCodePage, ConSettings);
        else raise CreateCanNotAccessBlobRecordException(ColumnIndex, ColumnType);
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
function TZDBLibResultSet.Next: Boolean;
begin
  Result := FDataProvider.Next;
  if not Result and not LastRowFetchLogged and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
end;


{ TZDBLibCachedResolver }

{**
  Creates a DBLib specific cached resolver object.
  @param PlainDriver a native DBLib plain driver.
  @param Handle a DBLib specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZDBLibCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
begin
  inherited Create(Statement, Metadata);

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := -1;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZDBLibCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  I: Integer;
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  { Defines an index of autoincrement field. }
  if FAutoColumnIndex = -1 then
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if Metadata.IsAutoIncrement(I) then
      begin
        FAutoColumnIndex := I;
        Break;
      end;

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    Statement := Connection.CreateStatement;
    ResultSet := Statement.ExecuteQuery('SELECT @@IDENTITY');
    try
      if ResultSet.Next then
        NewRowAccessor.SetLong(FAutoColumnIndex, ResultSet.GetLong(FirstDbcIndex));
    finally
      ResultSet.Close;
      Statement.Close;
    end;
  end;
end;

{ TZDblibResultSetMetadata }
procedure TZDblibResultSetMetadata.SetColumnCodePageFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo;
  const TableColumns: IZResultSet);
var ColTypeName: string;
  P: PChar;
begin
  if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then begin
    ColTypeName := TableColumns.GetString(TableColColumnTypeNameIndex);
    P := Pointer(ColTypeName);
    if P = nil
    then ColumnInfo.ColumnCodePage := FConSettings^.ClientCodePage^.CP
    else if (Length(ColTypeName) > 3) and SameText(P, PChar('UNI'), 3) //sybase only and FreeTDS does not map the type to UTF8?
      then ColumnInfo.ColumnCodePage := zCP_UTF16{UNICHAR, UNIVARCHAR}
      else if (Ord(P^) or $20 = Ord('n')) and not FConSettings^.ClientCodePage^.IsStringFieldCPConsistent
        then ColumnInfo.ColumnCodePage := zCP_UTF8 {NTEXT, NVARCHAR, NCHAR}
        else ColumnInfo.ColumnCodePage := FConSettings^.ClientCodePage^.CP; //assume server CP instead
  end else
    ColumnInfo.ColumnCodePage := zCP_NONE;
end;

procedure TZDblibResultSetMetadata.SetColumnPrecisionFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  if (ColumnInfo.ColumnType = stString) or (ColumnInfo.ColumnType = stUnicodeString) then
    ColumnInfo.Precision := TableColumns.GetInt(TableColColumnSizeIndex);
end;

procedure TZDblibResultSetMetadata.SetColumnScaleFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  if (ColumnInfo.ColumnType = stBytes) then
    ColumnInfo.Scale := TableColumns.GetInt(TableColColumnDecimalDigitsIndex);
end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
