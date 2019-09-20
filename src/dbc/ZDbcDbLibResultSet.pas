{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         DBLib Resultset common functionality            }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcDbLibResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses
{$IFNDEF FPC}
  DateUtils,
{$ENDIF}
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  FmtBCD,
  ZDbcIntfs, ZDbcResultSet, ZCompatibility, ZDbcResultsetMetadata,
  ZDbcGenericResolver, ZDbcCachedResultSet, ZDbcCache, ZDbcDBLib,
  ZPlainDbLibConstants, ZPlainDBLibDriver;

type
  TZAbstractDblibDataProvider = class
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
  end;

  TZDBLIBColumnInfo = class(TZColumnInfo)
  public
    TDSType: TTDSType;
  end;
  {** Implements DBLib ResultSet. }
  TZDBLibResultSet = class(TZAbstractReadOnlyResultSet_A,IZResultSet{TZSimpleResultSet})
  private
    FSQL: string;
    FCheckDBDead: Boolean;
    FHandle: PDBPROCESS;
    DBLibColumnCount: Integer;
    FUserEncoding: TZCharEncoding;
    FDecimalSep: Char;
    FClientCP: Word;
    procedure CheckColumnIndex(ColumnIndex: Integer);
  protected
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: TZDBLIBPLainDriver;
    FDataProvider: TZAbstractDblibDataProvider;
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      UserEncoding: TZCharEncoding = ceDefault);
    destructor Destroy; override;

    procedure BeforeClose; override;

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetBoolean(ColumnIndex: Integer): Boolean; //override;
    function GetInt(ColumnIndex: Integer): Integer; //override;
    function GetUInt(ColumnIndex: Integer): Cardinal; //override;
    function GetLong(ColumnIndex: Integer): Int64; //override;
    function GetULong(ColumnIndex: Integer): UInt64; //override;
    function GetFloat(ColumnIndex: Integer): Single; //override;
    function GetDouble(ColumnIndex: Integer): Double; //override;
    function GetCurrency(ColumnIndex: Integer): Currency; //override;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);// override;
    procedure GetGUID(ColumnIndex: Integer; Var Result: TGUID); //override;
    function GetBytes(ColumnIndex: Integer): TBytes; //override;
    function GetDate(ColumnIndex: Integer): TDateTime; //override;
    function GetTime(ColumnIndex: Integer): TDateTime; //override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; //override;
    function GetBlob(ColumnIndex: Integer): IZBlob; //override;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions); overload; virtual;
    {$ENDIF}
    function Next: Boolean; override;
  end;

  {** Implements a cached resolver with mssql and sybase specific functionality. }
  TZDBLibCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

uses ZMessages, ZDbcLogging, ZDbcDBLibUtils, ZEncoding, ZSysUtils, ZFastCode,
  ZClasses, ZDbcUtils, ZDbcMetadata
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
    DBFAIL: FConnection.CheckDBLibError(lcOther, 'NEXT');
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
constructor TZDBLibResultSet.Create(const Statement: IZStatement; const SQL: string;
  UserEncoding: TZCharEncoding);
begin
  inherited Create(Statement, SQL,
    TZDblibResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  Statement.GetConnection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := TZDBLIBPLainDriver(FDBLibConnection.GetIZPlainDriver.GetInstance);
  FHandle := FDBLibConnection.GetConnectionHandle;
  FSQL := SQL;
  FCheckDBDead := FPlainDriver.GetProtocol = 'mssql';
  FUserEncoding := UserEncoding;
  //FDataProvider := TZPlainDblibDataProvider.Create(Statement.GetConnection as IZDbLibConnection , FCheckDBDead);
  FDataProvider := TZCachedDblibDataProvider.Create(Statement.GetConnection as IZDbLibConnection);
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
        Scale := 0;
        Precision := ColInfo.MaxLength;
      end;
      ColumnType := ConvertTDSTypeToSqlType(TDSType, Precision, Scale, ConSettings.CPType);
      if ColumnType = stUnknown
      then NeedsLoading := true;
      if (TDSType = tdsNumeric) and (Scale = 0) and (Precision = 19)
      then NeedsLoading := true; // we cannot be sure if it is bigint or numeric - let somebody else deal with this...
      CaseSensitive := ColInfo.CaseSensitive = 1;
      Nullable := TZColumnNullableType(ColInfo.Null);
      ReadOnly := not (ColInfo.Updatable = 1);
      Writable := ColInfo.Updatable = 1;
      AutoIncrement := ColInfo.Identity;
      Signed := ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stFloat, stCurrency, stDouble, stBigDecimal];
    end;
  end;
  function ValueToString(P: PAnsiChar): String;
  begin
    {$IFDEF UNICODE}
    Result := PRawToUnicode(P, ZFastCode.StrLen(P), FClientCP);
    {$ELSE}
    ZSetString(P, ZFastCode.StrLen(P), Result);
    Result := ConSettings^.ConvFuncs.ZRawToString(Result,
          FClientCP, ConSettings^.CTRL_CP);
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
    if FDBLibConnection.FreeTDS then begin
      tdsColInfo.SizeOfStruct := SizeOf(TTDSDBCOL);
      FillChar(tdsColInfo.Name[0], tdsColInfo.SizeOfStruct- SizeOf(DBInt), #0);
      if FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @tdsColInfo) <> DBSUCCEED then //might be possible for computed or cursor columns
        goto AssignGeneric;
      ColumnInfo.ColumnName := ValueToString(@tdsColInfo.Name[0]);
      if Byte(tdsColInfo.ActualName[0]) = Ord(#0) then
        ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
      else
        ColumnInfo.ColumnLabel := ValueToString(@tdsColInfo.ActualName[0]);
      if Byte(tdsColInfo.TableName[0]) <> Ord(#0) then
        ColumnInfo.TableName := ValueToString(@tdsColInfo.TableName[0]);
      AssignGenericColumnInfoFromZDBCOL(ColumnInfo, tdsColInfo.ColInfo);
    end else if FDBLibConnection.GetProvider = dpMsSQL then begin
      ColInfo.SizeOfStruct := SizeOf(DBCOL); //before execute dbcolinfo we need to set the record size -> 122 Byte or we fail
      if FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @ColInfo) <> DBSUCCEED then //might be possible for computed or cursor columns
        goto AssignGeneric;
      ColumnInfo.ColumnName := ValueToString(@ColInfo.Name[0]);
      if Byte(ColInfo.ActualName[0]) = Ord(#0) then
        ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
      else
        ColumnInfo.ColumnLabel := ValueToString(@ColInfo.ActualName[0]);
      if (Byte(ColInfo.TableName[0]) <> Ord(#0)) then
        ColumnInfo.TableName := ValueToString(@ColInfo.TableName[0]);
      AssignGenericColumnInfoFromZDBCOL(ColumnInfo, ColInfo.ColInfo);
    end else with ColumnInfo do begin
AssignGeneric:  {this is the old way we did determine the ColumnInformations}
      ColumnName := ValueToString(FPlainDriver.dbColSource(FHandle, I));
      ColumnLabel := ValueToString(FPlainDriver.dbColName(FHandle, I));
      TDSType := TTDSType(FPlainDriver.dbColtype(FHandle, I));
      Precision := FPlainDriver.dbCollen(FHandle, I);
      Scale := 0;
      ColumnType := ConvertTDSTypeToSqlType(TDSType, Precision, Scale, ConSettings.CPType);
      if ColumnInfo.ColumnType = stUnknown
      then NeedsLoading := true;
      Currency := TDSType in [tdsMoney, tdsMoney4, tdsMoneyN];
      Signed := not (TDSType = tdsInt1);
    end;
    if (ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then
      if (FPlainDriver.DBLibraryVendorType <> lvtMS) then
        ColumnInfo.ColumnCodePage := FClientCP
      else if (ColumnInfo.ColumnType in [stAsciiStream, stUnicodeStream]) then
        ColumnInfo.ColumnCodePage := FClientCP
      else if (FUserEncoding = ceUTF8)
        then ColumnInfo.ColumnCodePage := zCP_UTF8
        else ColumnInfo.ColumnCodePage := zCP_NONE;
    ColumnsInfo.Add(ColumnInfo);
  end;

  if FDataProvider is TZCachedDblibDataProvider then begin
    (FDataProvider as TZCachedDblibDataProvider).LoadData;
    if NeedsLoading then
      (GetMetaData as IZDblibResultSetMetadata).LoadColumns;
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
procedure TZDBLibResultSet.BeforeClose;
begin
{ TODO -ofjanos -cGeneral : Maybe it needs a dbcanquery here. }
  if FDataProvider.needDbCanQuery then
    if Assigned(FHandle) then
      if FPlainDriver.dbDead(FHandle) = 0 then
        if FPlainDriver.dbCanQuery(FHandle) <> DBSUCCEED then
          FDBLibConnection.CheckDBLibError(lcDisconnect, 'CLOSE QUERY');
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

{$IFDEF USE_SYNCOMMONS}
procedure TZDBLibResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
begin
  raise Exception.Create(SUnsupportedOperation);
end;
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
            Result := @fTinyBuffer[0];
            GUIDToBuffer(dbData, Result, [guidWithBrackets]);
            Len := 38;
          end;
        else begin
          Result := @fTinyBuffer[0];
          Len := FPlainDriver.dbconvert(FHandle, Ord(TDSType), dbData, Len,
            Ord(tdsVarChar), Pointer(Result), SizeOF(fTinyBuffer)-1);
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
ConvCCP2W:  fUniTemp := PRawToUnicode(dbData, Len, ColumnCodePage);
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
            Result := @fTinyBuffer[0];
            GUIDToBuffer(dbData, Result, [guidWithBrackets]);
            Len := 38;
          end;
        else begin
          Len := FPlainDriver.dbconvert(FHandle, Ord(TDSType), Pointer(dbData), Len,
            Ord(tdsVarChar), @fTinyBuffer[0], SizeOF(fTinyBuffer)-1);
          FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar');
          fUniTemp := Ascii7ToUnicodeString(dbData, Len);
set_From_W: if Pointer(fUniTemp) = nil
          then Result := PEmptyUnicodeString
          else Result := Pointer(fUniTemp);
          FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar');
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
    FDBLibConnection.CheckDBLibError(lcOther, 'GETBOOLEAN');
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
    FDBLibConnection.CheckDBLibError(lcOther, 'GETINT');
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
    FDBLibConnection.CheckDBLibError(lcOther, 'GETLONG');
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
    FDBLibConnection.CheckDBLibError(lcOther, 'GETFLOAT');
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
      else FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar');
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
    FDBLibConnection.CheckDBLibError(lcOther, 'GETDOUBLE');
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
            @fTinyBuffer[0], SizeOf(fTinyBuffer));
        if DL = -1 then
          FDBLibConnection.CheckDBLibError(lcOther, 'GETBIGDECIMAL');
      end;
      LastWasNull := not TryRawToBCD(PAnsiChar(@fTinyBuffer[0]), DL, Result, '.');
    end else Result := NullBCD;
  end;
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
function TZDBLibResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  DL: Integer;
  Data: Pointer;
begin
  //DBLib -----> Col/Param starts whith index 1
  FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBYTES');
  LastWasNull := Data = nil;

  Result := BufferToBytes(Data, DL);
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
          @fTinyBuffer[0], SizeOf(fTinyBuffer));
      SQLStrToFloatDef(PAnsiChar(@fTinyBuffer[0]), 0, FDecimalSep, Result, DL);
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
function TZDBLibResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  Result := System.Int(GetTimestamp(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  Result := Frac(GetTimestamp(ColumnIndex));
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
function TZDBLibResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  DL: Integer;
  Data: Pointer;
  TempDate: TDBDATETIME;
  tdsTempDate: TTDSDBDATETIME;
  Failed: Boolean;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;
    Result := 0;
    if Data <> nil then begin
      //Perfect conversion no need to crack and reencode the date.
      if TDSType = tdsDateTime then
        if FDBLibConnection.FreeTDS //type diff
        then Result := PTDSDBDATETIME(Data)^.dtdays + 2 + (PTDSDBDATETIME(Data)^.dttime / 25920000)
        else Result := PDBDATETIME(Data)^.dtdays + 2 + (PDBDATETIME(Data)^.dttime / 25920000)
      else if (TDSType in [tdsNText, tdsNVarChar, tdsText, tdsVarchar, tdsChar]) then
        if PByte(PAnsiChar(Data)+2)^ = Ord(':') then
          Result := RawSQLTimeToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
        else if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Word(DL)) <= 4
          then Result := RawSQLTimeStampToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
          else Result := RawSQLTimeToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
      else begin
        if FDBLibConnection.FreeTDS then begin//type diff
          FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsDateTime),
            @tdsTempDate, SizeOf(tdsTempDate));
          Result := tdsTempDate.dtdays + 2 + (tdsTempDate.dttime / 25920000);
        end else begin
          FPlainDriver.dbconvert(FHandle, Ord(TDSType), Data, DL, Ord(tdsDateTime),
            @TempDate, SizeOf(TempDate));
          Result := TempDate.dtdays + 2 + (TempDate.dttime / 25920000);
        end;
        FDBLibConnection.CheckDBLibError(lcOther, 'GETTIMESTAMP');
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
function TZDBLibResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  DL: Integer;
  Len: NativeUInt;
  Data: Pointer;
begin
  with TZDBLIBColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    //DBLib -----> Col/Param starts whith index 1
    FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, Data, DL); //hint DBLib isn't #0 terminated @all
    LastWasNull := Data = nil;

    Result := nil;
    if Data <> nil then
      case ColumnType of
        stBytes, stBinaryStream:
          Result := TZAbstractBlob.CreateWithData(Data, DL);
        stAsciiStream, stUnicodeStream: begin
            if (DL = 1) and (PByte(Data)^ = Ord(' ')) then DL := 0; //improve empty lobs, where len = 1 but string should be ''
            Result := TZAbstractClob.CreateWithData(Data, DL,
              FDBLibConnection.GetServerAnsiCodePage, ConSettings);
          end;
        stString, stUnicodeString: if ColumnCodePage = zCP_NONE then
            case ZDetectUTF8Encoding(Data, DL) of
              etUTF8: begin
                  ColumnCodePage := zCP_UTF8;
                  Result := TZAbstractClob.CreateWithData(Data, DL, zCP_UTF8, ConSettings);
                end;
              etAnsi: begin
                  ColumnCodePage := FClientCP;
                  Result := TZAbstractClob.CreateWithData(Data, DL, FClientCP, ConSettings);
                end;
              else //ASCII7
                Result := TZAbstractClob.CreateWithData(Data, DL, zCP_us_ascii, ConSettings);
            end
          else Result := TZAbstractClob.CreateWithData(Data, DL,
              TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage, ConSettings);
        else begin
          Data := GetPAnsiChar(ColumnIndex, Len);
          Result := TZAbstractClob.CreateWithData(Data,
            Len, FClientCP, ConSettings);
        end;
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
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
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

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
