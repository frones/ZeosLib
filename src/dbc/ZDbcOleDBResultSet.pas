{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcOleDBResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Contnrs, Types{$ENDIF},
  Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX, FmtBCD,
  ZSysUtils, ZDbcIntfs, ZDbcGenericResolver, ZPlainOleDBDriver, ZDbcOleDBUtils,
  ZDbcCache, ZDbcCachedResultSet, ZDbcResultSet, ZDbcResultsetMetadata,
  ZCompatibility, ZClasses, ZDbcOleDB;

type
  {** Implements Ado ResultSet. }
  TZAbstractOleDBResultSet = class(TZAbstractReadOnlyResultSet, IZResultSet)
  private
    FRowSet: IRowSet;
    FZBufferSize: Integer;
    FDBBindingArray: TDBBindingDynArray;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FRowSize: NativeInt;
    FAccessor:HACCESSOR;
    FRowCount: DBROWCOUNT;
    FCurrentBufRowNo: DBROWOFFSET;
    FRowsObtained: DBCOUNTITEM;
    FHROWS: PHROWS_Array;
    FColBuffer: TByteDynArray;
    FRowStates: TDBROWSTATUSDynArray;
    fpcColumns: DBORDINAL;
    fTempBlob: IZBlob;
    fClientCP: Word;
    FOleDBConnection: IZOleDBConnection;
    FByteBuffer: PByteBuffer;
    FGetNextRowsStatus: HResult;
  private
    FData: Pointer;
    FLength: DBLENGTH;
    FwType: DBTYPE;
    FColBind: PDBBINDING;
    procedure ReleaseFetchedRows;
    procedure CreateAccessors;
    function CreateOleDbConvertError(ColumnIndex: Integer; SQLType: TZSQLType): EZSQLException;
  public
    //reintroduce is a performance thing (self tested and confirmed for OLE the access is pushed x2!):
    //direct dispatched methods for the interfaces makes each call as fast as using a native object!
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual
    //BUT!!! all GetXXXByName methods don't reach the code here any more
    //This needs to be done by IZResultSet(Self).GetXXXByName
    function IsNull(ColumnIndex: Integer): Boolean;
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
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
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); reintroduce; overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); reintroduce; overload;
    procedure GetTimeStamp(ColumnIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;

    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
  end;

  TZOleDBResultSet = class(TZAbstractOleDBResultSet, IZResultSet)
  protected
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      const RowSet: IRowSet; ZBufferSize: Integer;
      const {%H-}EnhancedColInfo: Boolean = True);
    /// <summary>Resets the Cursor position to beforeFirst, releases server and
    ///  client resources.</summary>
    procedure ResetCursor; override;
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; override;
  end;

  TZOleDBParamResultSet = class(TZAbstractOleDBResultSet, IZResultSet)
  public
    constructor Create(const Statement: IZStatement; const ParamBuffer: TByteDynArray;
      const ParamBindings: TDBBindingDynArray; const ParamNameArray: TStringDynArray);
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; override;
    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  TZOleDBMSSQLResultSetMetadata = class(TZAbstractResultSetMetadata)
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

  {** Implements a cached resolver with MSSQL specific functionality. }
  TZOleDBMSSQLCachedResolver = class (TZGenerateSQLCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
    FResultSet: IZResultSet;
    fStmt: IZPreparedStatement;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    destructor Destroy; override;

    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

  { TZOleDBLob }

  TZOleDBLob = Class(TZAbstractStreamedLob, IZLob)
  private
    FRowSet: IRowSet;
    FAccessor: HACCESSOR;
    FwType: DBTYPE;
    FCurrentRow: HROW;
    FLength: DBLENGTH;
    FOleConnection: IZOleDBConnection;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public
    constructor Create(const RowSet: IRowSet; ColumnIndex: Integer; wType: DBTYPE;
      CurrentRow: HROW; const Connection: IZOleDBConnection;
      ALength: DBLENGTH; const OpenLobStreams: TZSortedList);
    destructor Destroy; override;
  public
    function Length: Integer; override;
    function IsEmpty: Boolean; override;
    procedure Clear; override;
  end;

  { TZOleLobStream }

  TZOleLobStream = class(TZImmediatelyReleasableLobStream)
  private
    FSequentialStream: ISequentialStream;
    FDescriptor: TZOleDBLob;
    FPosition: Int64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(const Descriptor: TZOleDBLob; const OpenLobStreams: TZSortedList);
  public
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
  end;

  TZOleDBBLob = Class(TZOleDBLob, IZBlob)
  public
    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
  End;

  TZOleDBCLob = Class(TZOleDBLob, IZBlob, IZCLob)
  public
    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
  End;

  TZOleDBCachedResultSet = class(TZCachedResultSet)
  private
    FResultSet: TZAbstractOleDBResultSet;
  protected
    function Fetch: Boolean; override;
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    constructor Create(ResultSet: TZAbstractOleDBResultSet; const SQL: string;
      const Resolver: IZCachedResolver; ConSettings: PZConSettings);
  end;

  { TZOleDBRowAccessor }

  TZOleDbRowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
  end;

  {$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit

uses
  Variants, Math, DateUtils,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZMessages, ZEncoding, ZFastCode, ZTokenizer,
  ZSelectSchema, ZGenericSqlAnalyser,
  ZDbcOleDBStatement, ZDbcMetaData, ZDbcUtils, ZDbcLogging;

var
  LobReadObj: TDBObject;
  LobDBBinding: TDBBinding;

{ TZOleDBLob }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZOleDBLob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  Result := TZOleLobStream.Create(Self, FOpenLobStreams);
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

destructor TZOleDBLob.Destroy;
var
  FAccessorRefCount: DBREFCOUNT;
  Status: HResult;
begin
  if FAccessor > 0 then begin
    Status := (fRowSet As IAccessor).ReleaseAccessor(FAccessor, @FAccessorRefCount);
    if Status <> S_OK then
      FOleConnection.HandleErrorOrWarning(Status, lcOther, 'IAccessor.ReleaseAccessor',
        FOleConnection);
  end;
  inherited Destroy;
end;

function TZOleDBLob.IsEmpty: Boolean;
begin
  Result := False;
end;

function TZOleDBLob.Length: Integer;
begin
  Result := FLength;
end;

procedure TZOleDBLob.Clear;
begin
  raise CreateReadOnlyException;
end;

constructor TZOleDBLob.Create(const RowSet: IRowSet; ColumnIndex: Integer;
  wType: DBTYPE; CurrentRow: HROW; const Connection: IZOleDBConnection;
  ALength: DBLENGTH; const OpenLobStreams: TZSortedList);
var ColumnCodePage: Word;
  Status: HResult;
begin
  if wType = DBTYPE_BYTES then
    ColumnCodePage := zCP_Binary
  else if wType = DBTYPE_STR then
    ColumnCodePage := Connection.GetConSettings.ClientCodePage.CP
  else ColumnCodePage := zCP_UTF16;
  inherited Create(ColumnCodePage, OpenLobStreams);
  FConSettings := Connection.GetConSettings;
  FRowSet := RowSet;
  fWType := wType;
  FCurrentRow := CurrentRow;
  FLength := ALength;
  LobDBBinding.iOrdinal := ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF};
  FOleConnection := Connection;
  Status := (FRowSet as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA, 1,
    @LobDBBinding, 0, @FAccessor, nil);
  if Status <> S_OK then
    FOleConnection.HandleErrorOrWarning(Status, lcOther,
      'IAccessor.CreateAccessor', FOleConnection);
end;

{ TZOleLobStream }

constructor TZOleLobStream.Create(const Descriptor: TZOleDBLob;
  const OpenLobStreams: TZSortedList);
var Status: HResult;
begin
  inherited Create(Descriptor, Descriptor.FOleConnection, OpenLobStreams);
  FDescriptor := Descriptor;
  Status := Descriptor.FRowSet.GetData(Descriptor.FCurrentRow, Descriptor.FAccessor, @FSequentialStream);
  if Status <> S_OK then
    Descriptor.FOleConnection.HandleErrorOrWarning(Status, lcOther,
      'IRowSet.GetData', Descriptor.FOleConnection);
end;

function TZOleLobStream.GetSize: Int64;
begin
  Result := FDescriptor.FLength;
end;

function TZOleLobStream.Read(var Buffer; Count: Longint): Longint;
var pcbRead: ULong;
    Status: HResult;
begin
  if FSequentialStream = nil then begin
    Status := FDescriptor.FRowSet.GetData(FDescriptor.FCurrentRow, FDescriptor.FAccessor, @FSequentialStream);
    if Status <> S_OK then
      FDescriptor.FOleConnection.HandleErrorOrWarning(Status, lcOther,
        'IRowSet.GetData', Self);
  end;
  FSequentialStream.Read(@Buffer, Count, @pcbRead);
  Result := pcbRead;
  Inc(FPosition, Result);
end;

function TZOleLobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if Origin = soFromEnd then
    Result := FPosition - OffSet
  else if Origin = soFromCurrent then
    Result := FPosition + OffSet
  else begin
    Result := OffSet;
    if Offset = 0 then begin
      FSequentialStream := nil;
      FPosition := 0;
    end;
  end;
  if Result <> FPosition then
    raise EZSQLException.Create(SOperationIsNotAllowed1);
end;

function TZOleLobStream.Write(const Buffer; Count: Longint): Longint;
var pcbWritten: ULong;
    Status: HResult;
begin
  if FSequentialStream = nil then begin
    Status := FDescriptor.FRowSet.GetData(FDescriptor.FCurrentRow, FDescriptor.FAccessor, @FSequentialStream);
    if Status <> S_OK then
      FDescriptor.FOleConnection.HandleErrorOrWarning(Status, lcOther,
        'IRowSet.GetData', Self);
  end;
  FSequentialStream.Write(@Buffer, Count, @pcbWritten);
  Result := pcbWritten;
  Inc(FPosition, Result)
end;

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractOleDBResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var I, C, H: Integer;
    P: PAnsiChar;
    L: NativeUint;
    MS: Word;
label jmpCLob;
begin
  //init
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    if IsNull(C+FirstDbcIndex) then
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
      case FwType of
        DBTYPE_EMPTY,
        DBTYPE_IDISPATCH,
        DBTYPE_IUNKNOWN:  JSONWriter.AddShort('""');
        DBTYPE_NULL:      JSONWriter.AddShort('null');
        DBTYPE_I2:        JSONWriter.Add(PSmallInt(FData)^);
        DBTYPE_I4,
        DBTYPE_ERROR:     JSONWriter.Add(PInteger(FData)^);
        DBTYPE_R4:        JSONWriter.AddSingle(PSingle(FData)^);
        DBTYPE_R8:        JSONWriter.AddDouble(PDouble(FData)^);
        DBTYPE_CY:        JSONWriter.AddCurr64(PInt64(FData)^);
        DBTYPE_DATE:      JSONWriter.AddDateTime(PDateTime(FData), 'T', '"');
        DBTYPE_BOOL:      JSONWriter.AddShort(JSONBool[PWord(FData)^ <> 0]);
        DBTYPE_VARIANT: begin
            JSONWriter.Add('"');
            FUniTemp := POleVariant(FData)^;
            JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
            JSONWriter.Add('"');
          end;
        //DBTYPE_DECIMAL = 14;
        DBTYPE_UI1:       JSONWriter.AddU(PByte(FData)^);
        DBTYPE_I1:        JSONWriter.Add(PShortInt(FData)^);
        DBTYPE_UI2:       JSONWriter.AddU(PWord(FData)^);
        DBTYPE_UI4:       JSONWriter.AddU(PCardinal(FData)^);
        DBTYPE_I8:        JSONWriter.Add(PInt64(FData)^);
        DBTYPE_UI8:       JSONWriter.AddQ(PUInt64(FData)^);
        DBTYPE_GUID:      begin
                            JSONWriter.Add('"');
                            JSONWriter.Add(PGUID(FData)^);
                            JSONWriter.Add('"');
                          end;
        DBTYPE_BYTES:
          if FColBind.cbMaxLen = 0 then begin //streamed
            fTempBlob := TZOleDBBLOB.Create(FRowSet, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
              DBTYPE_BYTES, FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams);
            P := fTempBlob.GetBuffer(fRawTemp, L);
            JSONWriter.WrBase64(P, L, true); // withMagic=true
            fTempBlob := nil;
          end else
            JSONWriter.WrBase64(FData,FLength, True);
        DBTYPE_STR: begin
            JSONWriter.Add('"');
            if FColBind.cbMaxLen = 0
            then goto jmpCLob
            else begin
              if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                FLength := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData), FLength);
              FUniTemp := PRawToUnicode(PAnsiChar(FData), FLength, fClientCP);
              JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
            end;
            JSONWriter.Add('"');
          end;
        DBTYPE_WSTR, DBTYPE_XML: begin
            JSONWriter.Add('"');
            if FColBind.cbMaxLen = 0 then begin
jmpCLob:      fTempBlob := TZOleDBCLOB.Create(FRowSet, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
                FwType, FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams);
              P := Pointer(fTempBlob.GetPWideChar(FUniTemp, L));
              JSONWriter.AddJSONEscapeW(Pointer(P), L);
              fTempBlob := nil;
            end else begin
              FLength := FLength shr 1;
              if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                FLength := GetAbsorbedTrailingSpacesLen(PWideChar(FData), FLength);
              JSONWriter.AddJSONEscapeW(FData, FLength);
            end;
            JSONWriter.Add('"');
          end;
        DBTYPE_NUMERIC: begin
                          FLength := SQL_MAX_NUMERIC_LEN;
                          SQLNumeric2Raw(FData, PAnsiChar(FByteBuffer), FLength);
                          JSONWriter.AddJSONEscape(PAnsiChar(FByteBuffer), FLength);
                        end;
        //DBTYPE_UDT = 132;
        DBTYPE_DBDATE:    begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            if PDBDate(FData)^.year < 0 then
                              JSONWriter.Add('-');
                            DateToIso8601PChar(PUTF8Char(FByteBuffer), True, Abs(PDBDate(FData)^.year),
                              PDBDate(FData)^.month, PDBDate(FData)^.day);
                            JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer),10);
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('T00:00:00Z")')
                            else JSONWriter.Add('"');
                          end;
        DBTYPE_DBTIME:    begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("0000-00-00')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            end else
                              JSONWriter.Add('"');
                            TimeToIso8601PChar(PUTF8Char(FByteBuffer), True, PDBTime(FData)^.hour,
                              PDBTime(FData)^.minute, PDBTime(FData)^.second, 0, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer),9+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z)"')
                            else JSONWriter.Add('"');
                          end;
        DBTYPE_DBTIMESTAMP: begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            if PDBTimeStamp(FData)^.year < 0 then
                              JSONWriter.Add('-');
                            DateToIso8601PChar(PUTF8Char(FByteBuffer), True, Abs(PDBTimeStamp(FData)^.Year),
                               PDBTimeStamp(FData)^.Month, PDBTimeStamp(FData)^.Day);
                            MS := (PDBTimeStamp(FData)^.fraction * Byte(ord(jcoMilliseconds in JSONComposeOptions))) div 1000000;
                            TimeToIso8601PChar(PUTF8Char(FByteBuffer)+10, True, PDBTimeStamp(FData)^.Hour,
                              PDBTimeStamp(FData)^.Minute, PDBTimeStamp(FData)^.Second, MS, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer),19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
        DBTYPE_HCHAPTER:  JSONWriter.{$IFDEF CPU64}AddQ{$ELSE}AddU{$ENDIF}(PCHAPTER(FData)^);
        //DBTYPE_FILETIME = 64;
        //DBTYPE_PROPVARIANT = 138;
        DBTYPE_VARNUMERIC: begin
                            SQLNumeric2Raw(FData, PAnsiChar(FByteBuffer), FLength);
                            JSONWriter.AddJSONEscape(PUTF8Char(FByteBuffer), FLength);
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

procedure TZAbstractOleDBResultSet.CreateAccessors;
var Status: HResult;
begin
  Status := (FRowSet as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA,
    fpcColumns, Pointer(FDBBindingArray), FRowSize, @FAccessor,
    Pointer(FDBBINDSTATUSArray));
  if Status <> S_OK then
    FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
      'CreateAccessor', Self);
end;

function TZAbstractOleDBResultSet.CreateOleDbConvertError(ColumnIndex: Integer;
  SQLType: TZSQLType): EZSQLException;
begin
  Result := CreateConversionError(ColumnIndex, SQLType, TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType)
end;

procedure TZAbstractOleDBResultSet.ReleaseFetchedRows;
var Status: HResult;
begin
  if (FRowsObtained > 0) then begin
    Status := fRowSet.ReleaseRows(FRowsObtained,FHROWS,nil,nil,Pointer(FRowStates));
    if Status <> S_OK then
      FOleDBConnection.HandleErrorOrWarning(Status, lcOther, 'IRowSet.ReleaseRows', Self);
    FOleDBConnection.GetMalloc.Free(FHROWS);
    FHROWS := nil;
    FRowsObtained := 0;
  end;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractOleDBResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {.$R-}
  FColBind := @FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  fwType := FColBind.wType;
  Result := PDBSTATUS(@FColBuffer[FColBind.obStatus+NativeUInt(FRowSize*FCurrentBufRowNo)])^ = DBSTATUS_S_ISNULL;
  LastWasNull := Result;
  if Result then begin
    FData := nil;
    FLength := 0;
  end else begin
    //note FData is valid only if no Lobs DBPART_VALUE was set on binding!!!
    if FColBind.dwPart and DBPART_VALUE <> 0 //test if lob's are bound
    then FData := @FColBuffer[FColBind.obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]
    else FData := nil;
    if (FColBind.wType and DBTYPE_BYREF = DBTYPE_BYREF) then begin
      if (FData <> nil) then
        FData := PPointer(FData)^;
      FwType := fWType and not DBTYPE_BYREF;
    end;
    //note FLength is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
    if FColBind.dwPart and DBPART_LENGTH <> 0 //fixed types don't have a length indicator
    then FLength := PDBLENGTH(@FColBuffer[FColBind.obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^
    else FLength := 0;
  end;
  //{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in operating system encoding.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractOleDBResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var P: Pointer;
  L: NativeUInt;
begin
  case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
    DBTYPE_VARIANT,
    DBTYPE_STR, DBTYPE_STR or DBTYPE_BYREF,
    DBTYPE_WSTR, DBTYPE_WSTR or DBTYPE_BYREF: begin
        P := GetPWideChar(ColumnIndex, L);
        Result := PUnicodeToRaw(P, L, zOSCodePage);
      end
    else begin
        P := GetPAnsiChar(ColumnIndex, L);
        System.SetString(Result, PAnsichar(P), L);
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractOleDBResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var P: Pointer;
  L: NativeUInt;
begin
  case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
    DBTYPE_VARIANT,
    DBTYPE_STR, DBTYPE_STR or DBTYPE_BYREF,
    DBTYPE_WSTR, DBTYPE_WSTR or DBTYPE_BYREF: begin
        P := GetPWideChar(ColumnIndex, L);
        Result := PUnicodeToRaw(P, L, zCP_UTF8);
      end
    else begin
        P := GetPAnsiChar(ColumnIndex, L);
        ZSetString(PAnsiChar(P), L, Result);
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the value in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractOleDBResultSet.GetPAnsiChar(ColumnIndex: Integer;
  out Len: NativeUInt): PAnsiChar;
label set_from_tmp, set_from_buf, set_from_num;
begin
  if IsNull(ColumnIndex) then begin //Sets LastWasNull, FData, FLength!!
    Result := nil;
    Len := 0;
  end else case FwType of
    DBTYPE_BOOL:      if PWordBool(FData)^ then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
    DBTYPE_I1:        begin
                        IntToRaw(Integer(PShortInt(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I2:        begin
                        IntToRaw(Integer(PSmallInt(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I4,
    DBTYPE_ERROR:     begin
                        IntToRaw(PInteger(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I8:        begin
                        IntToRaw(PInt64(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI1:       begin
                        IntToRaw(Cardinal(PByte(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI2:       begin
                        IntToRaw(Cardinal(PWord(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFNDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI4:       begin
                        IntToRaw(PCardinal(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI8:       begin
                        IntToRaw(PUInt64(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_R4:        begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PSingle(FData)^, Result);
                      end;
    DBTYPE_R8:        begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PDouble(FData)^, Result);
                      end;
    DBTYPE_CY:        begin
                        CurrToRaw(PCurrency(FData)^, '.', PAnsiChar(fByteBuffer), @Result);
set_from_buf:           Len := Result - PAnsiChar(fByteBuffer);
                        Result := PAnsiChar(fByteBuffer);
                      end;
    DBTYPE_DATE:      begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := DateTimeToRawSQLTimeStamp(PDateTime(FData)^,
                          Result, ConSettings.ReadFormatSettings, False);
                      end;
    DBTYPE_DBDATE:    begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := DateToRaw(Abs(PDBDate(FData)^.year),
                          PDBDate(FData)^.month, PDBDate(FData)^.day,
                          Result, ConSettings.ReadFormatSettings.DateFormat,
                          False, PDBDate(FData)^.year < 0);
                      end;
    DBTYPE_DBTIME:    begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := TimeToRaw(PDBTime(FData)^.hour,
                          PDBTime(FData)^.minute, PDBTime(FData)^.second,0,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
    DBTYPE_DBTIMESTAMP: begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := DateTimeToRaw(Word(Abs(PDBTimeStamp(FData)^.year)),
                          PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day,
                          PDBTimeStamp(FData)^.hour, PDBTimeStamp(FData)^.minute,
                          PDBTimeStamp(FData)^.second, PDBTimeStamp(FData)^.fraction,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat,
                          False, PDBTimeStamp(FData)^.year < 0);
                      end;
    DBTYPE_VARIANT:   begin
                        FUniTemp := POleVariant(FData)^;
                        FRawTemp := ZUnicodeToRaw(FuniTemp, fClientCP);
                        goto set_from_tmp;
                      end;
    DBTYPE_GUID:      begin
                        GUIDToBuffer(FData, PAnsiChar(fByteBuffer), []);
                        Len := 36;
                        Result := PAnsiChar(fByteBuffer);
                      end;
    DBTYPE_STR:       if FColBind.cbMaxLen = 0 then begin
                        fTempBlob := GetBlob(ColumnIndex);
                        Result := fTempBlob.GetPAnsiChar(fClientCP, fRawTemp, Len);
                        fTempBlob := nil;
                      end else begin
                        Result := FData;
                        if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0
                        then Len := GetAbsorbedTrailingSpacesLen(Result, FLength)
                        else Len := FLength;
                      end;
    DBTYPE_WSTR:    if FColBind.cbMaxLen = 0 then begin
                      fTempBlob := GetBlob(ColumnIndex);
                      Result := fTempBlob.GetPAnsiChar(GetW2A2WConversionCodePage(ConSettings), fRawTemp, Len);
                      fTempBlob := nil;
                    end else begin
                      Result := FData;
                      FLength := Flength shr 1;
                      if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0
                      then Len := GetAbsorbedTrailingSpacesLen(PWideChar(Result), FLength)
                      else Len := FLength;
                      FRawTemp := PUnicodeToRaw(PWideChar(Result), Len, GetW2A2WConversionCodePage(ConSettings));
set_from_tmp:         Len := Length(FRawTemp);
                      if Len = 0
                      then Result := PEmptyAnsiString
                      else Result := Pointer(FRawTemp);
                    end;
    DBTYPE_NUMERIC: begin
                      Len := SQL_MAX_NUMERIC_LEN;
                      goto set_from_num;
                    end;
    DBTYPE_VARNUMERIC: begin
                      Len := FLength;
set_from_num:         Result := PAnsiChar(fByteBuffer);
                      SQLNumeric2Raw(fData, Result, Len);
                    end;
    //DBTYPE_UDT	= 132;
    //DBTYPE_FILETIME	= 64;
    //DBTYPE_PROPVARIANT	= 138;
    else raise CreateOleDbConvertError(ColumnIndex, stString);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the value in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractOleDBResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
label set_from_tmp, set_from_buf, set_from_clob, set_from_num;
begin
  if IsNull(ColumnIndex) then begin //Sets LastWasNull, FData, FLength!!
    Result := nil;
    Len := 0;
  end else case FwType of
    DBTYPE_BOOL:      if PWordBool(FData)^ then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
    DBTYPE_I1:        begin
                        IntToUnicode(Integer(PShortInt(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I2:        begin
                        IntToUnicode(Integer(PSmallInt(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I4,
    DBTYPE_ERROR:     begin
                        IntToUnicode(PInteger(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I8:        begin
                        IntToUnicode(PInt64(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI1:       begin
                        IntToUnicode(Cardinal(PByte(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI2:       begin
                        IntToUnicode(Cardinal(PWord(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFNDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI4:       begin
                        IntToUnicode(PCardinal(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI8:       begin
                        IntToUnicode(PUInt64(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_R4:        begin
                        Result := PWideChar(fByteBuffer);
                        Len := FloatToSQLUnicode(PSingle(FData)^, Result);
                      end;
    DBTYPE_R8:        begin
                        Result := PWideChar(fByteBuffer);
                        Len := FloatToSQLUnicode(PDouble(FData)^, Result);
                      end;
    DBTYPE_CY:        begin
                        CurrToUnicode(PCurrency(FData)^, '.', PWideChar(fByteBuffer), @Result);
set_from_buf:           Len := Result - PWideChar(fByteBuffer);
                        Result := PWideChar(fByteBuffer);
                      end;
    DBTYPE_DATE:      begin
                        Result := PWideChar(fByteBuffer);
                        Len := DateTimeToUnicodeSQLTimeStamp(PDateTime(FData)^,
                          Result, ConSettings.ReadFormatSettings, False);
                      end;
    DBTYPE_DBDATE:    begin
                        Result := PWideChar(fByteBuffer);
                        Len := DateToUni(Abs(PDBDate(FData)^.year),
                          PDBDate(FData)^.month, PDBDate(FData)^.day,
                          Result, ConSettings.ReadFormatSettings.DateFormat,
                          False, PDBDate(FData)^.year < 0);
                      end;
    DBTYPE_DBTIME:    begin
                        Result := PWideChar(fByteBuffer);
                        Len := TimeToUni(PDBTime(FData)^.hour,
                          PDBTime(FData)^.minute, PDBTime(FData)^.second,0,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
    DBTYPE_DBTIMESTAMP: begin
                        Result := PWideChar(fByteBuffer);
                        Len := DateTimeToUni(Abs(PDBTimeStamp(FData)^.year),
                          PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day,
                          PDBTimeStamp(FData)^.hour, PDBTimeStamp(FData)^.minute,
                          PDBTimeStamp(FData)^.second, PDBTimeStamp(FData)^.fraction,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat,
                          False, PDBTimeStamp(FData)^.year < 0);
                      end;
    DBTYPE_VARIANT:   begin
                        FUniTemp := POleVariant(FData)^;
                        goto set_from_tmp;
                      end;
    DBTYPE_GUID:      begin
                        GUIDToBuffer(FData, PWideChar(fByteBuffer), []);
                        Len := 36;
                        Result := PWideChar(fByteBuffer);
                      end;
    DBTYPE_STR: begin
                  if FColBind.cbMaxLen = 0 then
                    goto set_from_clob
                  else if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
                    FUniTemp := PRawToUnicode(PAnsiChar(FData), FLength, fClientCP)
                  else begin
                    Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData), FLength);
                    FUniTemp := PRawToUnicode(PAnsiChar(FData), Len, fClientCP);
                  end;
set_from_tmp:     Len := Length(FUniTemp);
                  if Len > 0
                  then Result := Pointer(FUniTemp)
                  else Result := PEmptyUnicodeString;
                end;
    DBTYPE_WSTR: if FColBind.cbMaxLen = 0 then begin
set_from_clob:    fTempBlob := GetBlob(ColumnIndex); //localize
                  Result := fTempBlob.GetPWideChar(fUniTemp, Len);
                  fTempBlob := nil;
                end else begin
                  Result := PWideChar(FData);
                  Len := FLength shr 1;
                  if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                    Len := GetAbsorbedTrailingSpacesLen(Result, Len);
                end;
    DBTYPE_NUMERIC: begin
                      Len := SQL_MAX_NUMERIC_LEN;
                      goto set_from_num;
                    end;
    DBTYPE_VARNUMERIC: begin
                      Len := FLength;
set_from_num:         Result := PWideChar(fByteBuffer);
                      SQLNumeric2Uni(fData, Result, Len);
                    end;
    //DBTYPE_UDT	= 132;
    //DBTYPE_FILETIME	= 64;
    //DBTYPE_PROPVARIANT	= 138;
    else raise CreateOleDbConvertError(ColumnIndex, stUnicodeString);
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
function TZAbstractOleDBResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  Result := False;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength, fwType!!
    case FwType of
      DBTYPE_I2:        Result := PSmallInt(FData)^ <> 0;
      DBTYPE_I4:        Result := PInteger(FData)^ <> 0;
      DBTYPE_R4:        Result := PSingle(FData)^  <> 0;
      DBTYPE_R8:        Result := PDouble(FData)^ <> 0;
      DBTYPE_CY:        Result := PCurrency(FData)^ <> 0;
      DBTYPE_DATE:      Result := PDateTime(FData)^ <> 0;
      DBTYPE_ERROR:     Result := PInteger(FData)^ <> 0;
      DBTYPE_BOOL:      Result := PWordBool(FData)^;
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^ <> 0;
      DBTYPE_I1:        Result := PShortInt(FData)^ <> 0;
      DBTYPE_UI2:       Result := PWord(FData)^ <> 0;
      DBTYPE_UI4:       Result := PCardinal(FData)^ <> 0;
      DBTYPE_I8:        Result := PInt64(FData)^ <> 0;
      DBTYPE_UI8:       Result := PUInt64(FData)^ <> 0;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := StrToBoolEx(PA, PA+Len);
          FTempBlob := nil;
        end else
          Result := StrToBoolEx(PAnsiChar(FData),
            True, FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := StrToBoolEx(PW, PW+Len);
          FTempBlob := nil;
        end else
          Result := StrToBoolEx(PWideChar(FData),
            True, FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0);
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^ <> 0;
      else raise CreateOleDbConvertError(ColumnIndex, stBoolean);
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
function TZAbstractOleDBResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
  Result := nil;
  Len := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_GUID:
        begin
          SetLength(FRawTemp, SizeOf(TGUID));
          Result := Pointer(FRawTemp);
          Len := SizeOf(TGUID);
          PGUID(Result)^ := PGUID(FData)^;
        end;
      DBTYPE_BYTES:
        begin
          Result := FData;
          Len := FLength;
        end;
      else LastWasNull := True;
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
function TZAbstractOleDBResultSet.GetInt(ColumnIndex: Integer): Integer;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToIntDef(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToIntDef(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToIntDef(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToIntDef(PWideChar(FData),0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stInteger);
    end
  else Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractOleDBResultSet.GetLong(ColumnIndex: Integer): Int64;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_ERROR,
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToInt64Def(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToInt64Def(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToInt64Def(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToInt64Def(PWideChar(FData),0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stLong);
    end
  else Result := 0;
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
function TZAbstractOleDBResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToUInt64Def(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToUInt64Def(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToUInt64Def(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToUInt64Def(PWideChar(FData),0);
      //DBTYPE_NUMERIC = 131;
      //DBTYPE_UDT = 132;
      //DBTYPE_DBDATE = 133;
      //DBTYPE_DBTIME = 134;
      //DBTYPE_DBTIMESTAMP = 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT = 138;
      //DBTYPE_VARNUMERIC = 139;
      else raise CreateOleDbConvertError(ColumnIndex, stLongWord);
    end
  else Result := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}

function TZAbstractOleDBResultSet.GetULong(ColumnIndex: Integer): UInt64;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL = 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToUInt64Def(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToUInt64Def(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToUInt64Def(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToUInt64Def(PWideChar(FData),0);
      //DBTYPE_NUMERIC = 131;
      //DBTYPE_UDT = 132;
      //DBTYPE_DBDATE = 133;
      //DBTYPE_DBTIME = 134;
      //DBTYPE_DBTIMESTAMP = 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME = 64;
      //DBTYPE_PROPVARIANT = 138;
      //DBTYPE_VARNUMERIC = 139;
      else raise CreateOleDbConvertError(ColumnIndex, stULong);
    end
  else Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractOleDBResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_WSTR:
                        SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stFloat);
    end
  else Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UUID</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>Zero-UUID</code>
}
procedure TZAbstractOleDBResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
label Fail;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_GUID: Result := PGUID(FData)^;
      DBTYPE_STR: if FColBind.cbMaxLen = 0 then
                    goto Fail
                  else begin
                    if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                      FLength := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData), FLength);
                    ValidGUIDToBinary(PAnsiChar(FData), @Result.D1);
                  end;
      DBTYPE_WSTR: if FColBind.cbMaxLen = 0 then
                    goto Fail
                  else begin
                    FLength := FLength shr 1;
                    if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                      FLength := GetAbsorbedTrailingSpacesLen(PWideChar(FData), FLength);
                    ValidGUIDToBinary(PWideChar(FData), @Result.D1);
                  end;
      else
Fail:        raise CreateOleDbConvertError(ColumnIndex, stGUID);
    end
  else FillChar(Result, SizeOf(TGUID), #0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractOleDBResultSet.GetDate(ColumnIndex: Integer;
  var Result: TZDate);
label Fill;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_DATE: DecodeDateTimeToDate(PDateTime(FData)^, Result);
      DBTYPE_WSTR: LastWasNull := not TryPCharToDate(PWideChar(FData), FLength shr 1,
        ConSettings.ReadFormatSettings, Result);
      DBTYPE_DBDATE: begin
              Result.Year := Abs(PDBDate(FData)^.year);
              Result.Month := PDBDate(FData)^.month;
              Result.Day := PDBDate(FData)^.day;
              Result.IsNegative := PDBDate(FData)^.year < 0;
            end;
      DBTYPE_DBTIME: goto Fill;
      DBTYPE_DBTIMESTAMP: begin
              Result.Year := Abs(PDBTimeStamp(FData)^.year);
              Result.Month := PDBTimeStamp(FData)^.month;
              Result.Day := PDBTimeStamp(FData)^.day;
              Result.IsNegative := PDBTimeStamp(FData)^.year < 0;
            end;
      DBTYPE_DBTIMESTAMPOFFSET: begin
              Result.Year := Abs(PDBTIMESTAMPOFFSET(FData)^.year);
              Result.Month := PDBTIMESTAMPOFFSET(FData)^.month;
              Result.Day := PDBTIMESTAMPOFFSET(FData)^.day;
              Result.IsNegative := PDBTIMESTAMPOFFSET(FData)^.year < 0;
            end;
      else DecodeDateTimeToDate(GetDouble(ColumnIndex), Result);
    end
  else
Fill: PInt64(@Result.Year)^ := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractOleDBResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_WSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stDouble);
    end
  else Result := 0;
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
procedure TZAbstractOleDBResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var P: Pointer;
  L: NativeUInt;
label Fail;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_I2:        ScaledOrdinal2Bcd(PSmallInt(FData)^, 0, Result);
      DBTYPE_I4:        ScaledOrdinal2Bcd(PInteger(FData)^, 0, Result);
      DBTYPE_R4:        Result := DoubleToBCD(PSingle(FData)^);
      DBTYPE_R8:        Result := DoubleToBCD(PDouble(FData)^);
      DBTYPE_CY:        Currency2Bcd(PCurrency(FData)^, Result);
      DBTYPE_DATE:      Result := DoubleToBCD(PDateTime(FData)^);
      DBTYPE_STR: begin
          P := GetPAnsichar(ColumnIndex, L);
          if not ZSysUtils.TryRawToBcd(P, L, Result, '.') then
            goto fail;
        end;
      DBTYPE_WSTR: begin
          P := GetPWidechar(ColumnIndex, L);
          if not ZSysUtils.TryUniToBcd(P, L, Result, '.') then
            goto fail;
        end;
      DBTYPE_VARIANT:   Result := VarToBCD(POleVariant(FData)^);
      DBTYPE_ERROR:     ScaledOrdinal2Bcd(PInteger(FData)^, 0, Result);
      DBTYPE_BOOL:      ScaledOrdinal2Bcd(Word(Ord(PWord(FData)^ <> 0)), 0, Result);
      DBTYPE_UI1:       ScaledOrdinal2Bcd(Word(PByte(FData)^), 0, Result, False);
      DBTYPE_I1:        ScaledOrdinal2Bcd(SmallInt(PShortInt(FData)^), 0, Result);
      DBTYPE_UI2:       ScaledOrdinal2Bcd(PWord(FData)^, 0, Result);
      DBTYPE_UI4:       ScaledOrdinal2Bcd(PCardinal(FData)^, 0, Result, False);
      DBTYPE_I8:        ScaledOrdinal2Bcd(PInt64(FData)^, 0, Result);
      DBTYPE_UI8:       ScaledOrdinal2Bcd(PUInt64(FData)^, 0, Result, False);
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  ScaledOrdinal2Bcd(PCHAPTER(FData)^, 0, Result, False);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      DBTYPE_NUMERIC:   SQLNumeric2BCD(FData, Result, SQL_MAX_NUMERIC_LEN);
      DBTYPE_VARNUMERIC:SQLNumeric2BCD(FData, Result, FLength);
      else
Fail:    raise CreateOleDbConvertError(ColumnIndex, stBigDecimal);
    end
  else
    FillChar(Result, SizeOf(TBCD), #0)
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractOleDBResultSet.GetCurrency(ColumnIndex: Integer): Currency;
label jmpFail;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_WSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_NUMERIC,
      DBTYPE_VARNUMERIC:if PDB_NUMERIC(FData).precision < 19
                        then Result := DBNumeric2Curr_LE(FData, 0)
                        else goto jmpFail;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      else
jmpFail:raise CreateOleDbConvertError(ColumnIndex, stCurrency);
    end
  else Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZAbstractOleDBResultSet.GetTime(ColumnIndex: Integer;
  var Result: TZTime);
label Fill;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_DATE:    DecodeDateTimeToTime(PDateTime(FData)^, Result);
      DBTYPE_WSTR:    LastWasNull := not TryPCharToTime(PWideChar(FData), FLength shr 1,
                        ConSettings^.ReadFormatSettings, Result);
      DBTYPE_DBDATE:  goto fill;
      DBTYPE_DBTIME: begin
                      Result.Hour := PDBTime(FData)^.hour;
                      Result.Minute := PDBTime(FData)^.minute;
                      Result.Second := PDBTime(FData)^.second;
                      Result.Fractions := 0;
                      Result.IsNegative := False;
                    end;
      DBTYPE_DBTIME2: begin
                      Result.Hour := PDBTime2(FData)^.hour;
                      Result.Minute := PDBTime2(FData)^.minute;
                      Result.Second := PDBTime2(FData)^.second;
                      Result.Fractions := PDBTime2(FData)^.fraction;
                      Result.IsNegative := False;
                    end;
      DBTYPE_DBTIMESTAMP: begin
                      Result.Hour := PDBTimeStamp(FData)^.hour;
                      Result.Minute := PDBTimeStamp(FData)^.minute;
                      Result.Second := PDBTimeStamp(FData)^.second;
                      Result.Fractions := PDBTimeStamp(FData)^.fraction;
                      Result.IsNegative := False;
                    end;
      else DecodeDateTimeToTime(GetDouble(ColumnIndex), Result);
    end
  else
Fill: FillChar(Result, SizeOf(TZTimeStamp), #0);
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
{$IFDEF FPC} {$PUSH} {$WARN 6018 off : Warning unreachable code} {$ENDIF}
procedure TZAbstractOleDBResultSet.GetTimestamp(ColumnIndex: Integer;
  var Result: TZTimeStamp);
label Fill;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_DBTIMESTAMP: begin
            if SizeOf(TDBTimeStamp) = SizeOf(TZTimeStamp)-6 then
              PDBTimeStamp(@Result.Year)^ := PDBTimeStamp(FData)^
            else begin
              Result.Month := PDBTimeStamp(FData)^.month;
              Result.Day := PDBTimeStamp(FData)^.day;
              Result.Hour := PDBTimeStamp(FData)^.hour;
              Result.Minute := PDBTimeStamp(FData)^.minute;
              Result.Second := PDBTimeStamp(FData)^.second;
              Result.Fractions := PDBTimeStamp(FData)^.fraction;
            end;
            Result.Year := Abs(PDBTimeStamp(FData)^.year);
            PCardinal(@Result.TimeZoneHour)^ := 0;
            Result.IsNegative := PDBTimeStamp(FData)^.year < 0;
          end;
      DBTYPE_DBTIMESTAMPOFFSET: begin
            if SizeOf(TDBTIMESTAMPOFFSET) = SizeOf(TZTimeStamp)-2 then
              PDBTIMESTAMPOFFSET(@Result.Year)^ := PDBTIMESTAMPOFFSET(FData)^
            else begin
              Result.Month := PDBTIMESTAMPOFFSET(FData)^.month;
              Result.Day := PDBTIMESTAMPOFFSET(FData)^.day;
              Result.Hour := PDBTIMESTAMPOFFSET(FData)^.hour;
              Result.Minute := PDBTIMESTAMPOFFSET(FData)^.minute;
              Result.Second := PDBTIMESTAMPOFFSET(FData)^.second;
              Result.Fractions := PDBTIMESTAMPOFFSET(FData)^.fraction;
              Result.TimeZoneHour := PDBTIMESTAMPOFFSET(FData)^.timezone_hour;
              Result.TimeZoneMinute := PDBTIMESTAMPOFFSET(FData)^.timezone_minute;
            end;
            Result.Year := Abs(PDBTIMESTAMPOFFSET(FData)^.year);
            Result.IsNegative := PDBTIMESTAMPOFFSET(FData)^.year < 0;
          end;
      DBTYPE_DBDATE: begin
              Result.Year := Abs(PDBDate(FData)^.year);
              Result.Month := PDBDate(FData)^.month;
              Result.Day := PDBDate(FData)^.day;
              PInt64(@Result.Hour)^ := 0;
              PInt64(@Result.Fractions)^ := 0;
              Result.IsNegative := PDBDate(FData)^.year < 0;
            end;
      DBTYPE_DBTIME: begin
              PInt64(@Result.Year)^ := 0;
              Result.Hour := PDBTime(FData)^.hour;
              Result.Minute := PDBTime(FData)^.minute;
              Result.Second := PDBTime(FData)^.second;
              PInt64(@Result.Fractions)^ := 0;
              Result.IsNegative := False;
            end;
      DBTYPE_DBTIME2: begin
              PInt64(@Result.Year)^ := 0;
              Result.Hour := PDBTime2(FData)^.hour;
              Result.Minute := PDBTime2(FData)^.minute;
              Result.Second := PDBTime2(FData)^.second;
              Result.Fractions := PDBTime2(FData)^.fraction;
              PCardinal(@Result.TimeZoneHour)^ := 0;
              Result.IsNegative := False;
            end;
      DBTYPE_DATE: DecodeDateTimeToTimeStamp(PDateTime(FData)^,Result);
      DBTYPE_WSTR: begin
          LastWasNull := not TryPCharToTimeStamp(PWideChar(FData), FLength shr 1,
                ConSettings^.ReadFormatSettings, Result);
          if LastWasNull then
            goto Fill;
        end;
      else DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex),Result);
    end
  else
Fill: FillChar(Result, SizeOf(TZTimeStamp), #0);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractOleDBResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
begin
  Result := nil;
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_GUID:
        Result := TZLocalMemBLob.CreateWithData(Pointer(FData), 16, FOpenLobStreams);
      DBTYPE_BYTES:
        if FColBind.cbMaxLen = 0 then
          Result := TZOleDBBLOB.Create(FRowSet, ColumnIndex, DBTYPE_BYTES,
            FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams)
        else
          Result := TZLocalMemBLob.CreateWithData(Pointer(FData), FLength, FOpenLobStreams);
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then
          Result := TZOleDBCLOB.Create(FRowSet, ColumnIndex, DBTYPE_STR,
            FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams)
        else
          Result := TZLocalMemCLob.CreateWithData(PAnsiChar(FData),
            FLength, FClientCP, ConSettings, FOpenLobStreams);
      DBTYPE_WSTR, DBTYPE_XML:
        if FColBind.cbMaxLen = 0 then
          Result := TZOleDBCLOB.Create(FRowSet, ColumnIndex, FwType,
            FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams)
        else
          Result := TZLocalMemCLob.CreateWithData(PWideChar(FData), FLength shr 1, ConSettings, FOpenLobStreams);
      else raise CreateCanNotAccessBlobRecordException(ColumnIndex, TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType);
    end;
end;


{ TZOleDBMSSQLCachedResolver }

{**
  Creates a OleDB specific cached resolver object.
}
constructor TZOleDBMSSQLCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte,stShort,stWord,stSmall,stLongWord,stInteger,stULong,stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
  fStmt := TZOleDBPreparedStatement.Create(Statement.GetConnection, 'SELECT SCOPE_IDENTITY()', nil);
end;

destructor TZOleDBMSSQLCachedResolver.Destroy;
begin
  if fStmt <> nil then
    fStmt.Close;
  inherited Destroy;
end;
{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZOleDBMSSQLCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    FResultSet := fStmt.ExecuteQueryPrepared;
    if Assigned(FResultSet) and FResultSet.Next then
      NewRowAccessor.SetLong(FAutoColumnIndex, FResultSet.GetLong(FAutoColumnIndex));
  end;
end;

{ TZOleDBCachedResultSet }

constructor TZOleDBCachedResultSet.Create(ResultSet: TZAbstractOleDBResultSet;
  const SQL: string; const Resolver: IZCachedResolver; ConSettings: PZConSettings);
begin
  inherited Create(ResultSet, SQL, Resolver, ConSettings);
  FResultSet := ResultSet;
end;

{$IFDEF FPC}
  {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
{$ENDIF}
function TZOleDBCachedResultSet.Fetch: Boolean;
var
  I: Integer;
  TempRow: PZRowBuffer;
  DBBINDING: PDBBINDING;
  FData: PPointer;
  FLength: PDBLENGTH;
  Len: NativeUInt;
  BCD: TBCD; //one val on stack 4 all
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
  procedure MoveAsLocalLob(wType: DBTYPE; ColumnIndex: Integer);
  var OleDBBLob, LocalLob: IZBlob;
    OleDBCLob: IZCLob;
  begin
    case wType of
      DBTYPE_BYTES: begin
          OleDBBLob := TZOleDBBLOB.Create(FResultSet.FRowSet,
            I, DBTYPE_BYTES, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
            FResultSet.FOleDBConnection, Flength^, FOpenLobStreams);
          LocalLob := TZLocalMemBLob.CreateFromBlob(OleDBBLob, FOpenLobStreams);
        end;
      DBTYPE_STR: begin
          OleDBCLob := TZOleDBCLOB.Create(FResultSet.FRowSet,
            I, DBTYPE_STR, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
            FResultSet.FOleDBConnection, Flength^, FOpenLobStreams);
          LocalLob := TZLocalMemCLob.CreateFromClob(OleDBCLob, ConSettings.ClientCodePage.CP, ConSettings, FOpenLobStreams);
        end;
      else begin
          OleDBCLob := TZOleDBCLOB.Create(FResultSet.FRowSet,
            I, DBTYPE_WSTR, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
            FResultSet.FOleDBConnection, Flength^, FOpenLobStreams);
          LocalLob := TZLocalMemCLob.CreateFromClob(OleDBCLob, zCP_UTF16, ConSettings, FOpenLobStreams);
        end;
    end;
    RowAccessor.SetBlob(ColumnIndex, LocalLob);
  end;
begin
  if Assigned(FResultSet)
  then Result := FResultSet.Next
  else Result := False;
  if not Result or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then
    Exit;

  TempRow := RowAccessor.RowBuffer;
  FData := @FResultSet.FData;
  FLength := @FResultSet.FLength;
  RowAccessor.Alloc;
  RowAccessor.RowBuffer.Index := GetNextRowIndex;
  RowAccessor.RowBuffer.UpdateType := utUnmodified;
  try
    for I := FirstDbcIndex to {$IFDEF GENERIC_INDEX}High{$ELSE}Length{$ENDIF}(FResultSet.FDBBindingArray) do
      if FResultSet.IsNull(I) then
        RowAccessor.SetNull(I)
      else begin
        DBBINDING := @FResultSet.FDBBindingArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
        case DBBINDING.wType of
          DBTYPE_EMPTY,
          DBTYPE_NULL                 : RowAccessor.SetNull(I);
          DBTYPE_I2                   : RowAccessor.SetSmall(I, PSmallInt(FData^)^);
          DBTYPE_I4,
          DBTYPE_ERROR,
          DBTYPE_HCHAPTER             : RowAccessor.SetInt(I, PInteger(FData^)^);
          DBTYPE_R4	                  : RowAccessor.SetFloat(I, PSingle(FData^)^);
          DBTYPE_R8                   : RowAccessor.SetDouble(I, PDouble(FData^)^);
          DBTYPE_CY                   : RowAccessor.SetCurrency(I, PCurrency(FData^)^);
          DBTYPE_DATE                 : begin
                                          DecodeDateTimeToTimeStamp(PDateTime(FData^)^, TS);
                                          RowAccessor.SetTimeStamp(I, TS);
                                        end;
          DBTYPE_WSTR                 : if DBBINDING.cbMaxLen = 0
                                        then MoveAsLocalLob(DBTYPE_WSTR, I)
                                        else begin
                                          Len := FLength^ shr 1;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            Len := GetAbsorbedTrailingSpacesLen(PWideChar(FData^), Len);
                                          RowAccessor.SetPWideChar(I, FData^, Len);
                                        end;
          //DBTYPE_IDISPATCH = 9;
          DBTYPE_BOOL                 : RowAccessor.SetBoolean(I, PWordBool(FData^)^);
          DBTYPE_VARIANT              : case RowAccessor.GetColumnType(I) of
                                          stBoolean: RowAccessor.SetBoolean(I, POleVariant(FData^)^);
                                          stByte        : RowAccessor.SetByte(I, POleVariant(FData^)^);
                                          stShort       : RowAccessor.SetShort(I, POleVariant(FData^)^);
                                          stWord        : RowAccessor.SetWord(I, POleVariant(FData^)^);
                                          stSmall       : RowAccessor.SetSmall(I, POleVariant(FData^)^);
                                          stLongWord    : RowAccessor.SetUInt(I, POleVariant(FData^)^);
                                          stInteger     : RowAccessor.SetInt(I, POleVariant(FData^)^);
                                          stULong       : RowAccessor.SetULong(I, POleVariant(FData^)^);
                                          stLong        : RowAccessor.SetLong(I, POleVariant(FData^)^);
                                          stFloat       : RowAccessor.SetFloat(I, POleVariant(FData^)^);
                                          stDouble      : RowAccessor.SetDouble(I, POleVariant(FData^)^);
                                          stCurrency    : RowAccessor.SetCurrency(I, POleVariant(FData^)^);
                                          stBigDecimal  : RowAccessor.SetBigDecimal(I, DoubleToBCD(POleVariant(FData^)^));
                                          {stDate, stTime, stTimestamp,
                                          stGUID,
                                          //now varying size types in equal order
                                          stString, stUnicodeString, stBytes,
                                          stAsciiStream, stUnicodeStream, stBinaryStream,
                                          //finally the object types
                                          stArray, stDataSet}
                                          {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
                                        end;
          //DBTYPE_IUNKNOWN = 13;
          //DBTYPE_DECIMAL = 14;
          DBTYPE_UI1                  : RowAccessor.SetByte(I, PByte(FData^)^);
          DBTYPE_I1                   : RowAccessor.SetShort(I, PShortInt(FData^)^);
          DBTYPE_UI2                  : RowAccessor.SetWord(I, PWord(FData^)^);
          DBTYPE_UI4                  : RowAccessor.SetUInt(I, PCardinal(FData^)^);
          DBTYPE_I8                   : RowAccessor.SetLong(I, PInt64(FData^)^);
          DBTYPE_UI8                  : RowAccessor.SetULong(I, PInt64(FData^)^);
          DBTYPE_GUID                 : RowAccessor.SetGUID(I, PGUID(FData^)^);
          DBTYPE_BYTES                : if DBBINDING.cbMaxLen = 0
                                        then MoveAsLocalLob(DBTYPE_BYTES, I)
                                        else RowAccessor.SetBytes(I, FData^, FLength^);
          DBTYPE_STR                  : if DBBINDING.cbMaxLen = 0
                                        then MoveAsLocalLob(DBTYPE_STR, I)
                                        else begin
                                          Len := FLength^;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData^), Len);
                                          RowAccessor.SetPAnsiChar(I, FData^, Len);
                                        end;
          DBTYPE_NUMERIC              : begin
                                          SQLNumeric2BCD(PDB_NUMERIC(FData^), BCD, SQL_MAX_NUMERIC_LEN);
                                          RowAccessor.SetBigDecimal(I, BCD);
                                        end;
          //DBTYPE_UDT = 132;
          DBTYPE_DBDATE               : begin
                                          D.Year := Abs(PDBDate(FData^)^.year);
                                          D.Month := PDBDate(FData^)^.month;
                                          D.Day := PDBDate(FData^)^.day;
                                          D.IsNegative := PDBDate(FData^)^.year < 0;
                                          RowAccessor.SetDate(I, D);
                                        end;
          DBTYPE_DBTIME               : begin
                                          T.Hour := PDBTime(FData^)^.hour;
                                          T.Minute := PDBTime(FData^)^.minute;
                                          T.Second := PDBTime(FData^)^.second;
                                          T.Fractions := 0;
                                          T.IsNegative := False;
                                          RowAccessor.SetTime(I, T);
                                        end;
          DBTYPE_DBTIMESTAMP          : begin
                                          TS.Year := Abs(PDBTimeStamp(FData^)^.year);
                                          Ts.Month := PDBTimeStamp(FData^)^.month;
                                          TS.Day := PDBTimeStamp(FData^)^.day;
                                          TS.Hour := PDBTimeStamp(FData^)^.hour;
                                          TS.Minute := PDBTimeStamp(FData^)^.minute;
                                          TS.Second := PDBTimeStamp(FData^)^.second;
                                          TS.Fractions := PDBTimeStamp(FData^)^.fraction;
                                          Ts.TimeZoneHour := 0;
                                          TS.TimeZoneMinute := 0;
                                          TS.IsNegative := PDBTimeStamp(FData^)^.year < 0;
                                          RowAccessor.SetTimestamp(I, TS);
                                        end;
          {SQL Server types only }
          DBTYPE_XML                  : MoveAsLocalLob(DBTYPE_WSTR, I);
          //DBTYPE_TABLE = 143; // introduced in SQL 2008
          DBTYPE_DBTIME2              : begin
                                          T.Hour := PDBTime2(FData^)^.hour;
                                          T.Minute := PDBTime2(FData^)^.minute;
                                          T.Second := PDBTime2(FData^)^.second;
                                          T.Fractions := PDBTime2(FData^)^.fraction;
                                          T.IsNegative := False;
                                          RowAccessor.SetTime(I, T);
                                        end;
          //DBTYPE_DBTIMESTAMPOFFSET = 146; // introduced in SQL 2008
          //DBTYPE_FILETIME = 64;
          //DBTYPE_PROPVARIANT = 138;
          DBTYPE_VARNUMERIC           : begin
                                          SQLNumeric2BCD(PDB_NUMERIC(FData^), BCD, FLength^);
                                          RowAccessor.SetBigDecimal(I, BCD);
                                        end;
        end;
      end;
      RowsList.Add(RowAccessor.RowBuffer);
      LastRowNo := RowsList.Count;
    finally
      RowAccessor.RowBuffer := TempRow;
    end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

class function TZOleDBCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZOleDbRowAccessor;
end;


{ TZOleDBMSSQLResultSetMetadata }

procedure TZOleDBMSSQLResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

function TZOleDBMSSQLResultSetMetadata.GetCatalogName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).CatalogName;
end;

function TZOleDBMSSQLResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnName;
end;

function TZOleDBMSSQLResultSetMetadata.GetColumnType(
  ColumnIndex: Integer): TZSQLType;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType;
end;

function TZOleDBMSSQLResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).SchemaName;
end;

function TZOleDBMSSQLResultSetMetadata.GetTableName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).TableName;
end;

function TZOleDBMSSQLResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).AutoIncrement;
end;

procedure TZOleDBMSSQLResultSetMetadata.LoadColumns;
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
  Connection := Metadata.GetConnection;
  Driver := Connection.GetDriver;
  Analyser := Driver.GetStatementAnalyser;
  Tokenizer := Driver.GetTokenizer;
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
  Loaded := True;
end;

{ TZOleDBParamResultSet }

{**
  Creates this object and assignes the main properties.
}
constructor TZOleDBParamResultSet.Create(const Statement: IZStatement;
  const ParamBuffer: TByteDynArray; const ParamBindings: TDBBindingDynArray;
  const ParamNameArray: TStringDynArray);
var
  i, j: Integer;
  ColumnInfo: TZColumnInfo;
begin
  FOleDBConnection := (Statement.GetConnection as IZOleDBConnection);
  FByteBuffer := FOleDBConnection.GetByteBufferAddress;
  inherited Create(Statement, Statement.GetSQL, nil, FOleDBConnection.GetConSettings);
  fClientCP := ConSettings.ClientCodePage.CP;
  FColBuffer := ParamBuffer;
  J := 0;
  for I := Low(ParamBindings) to High(ParamBindings) do
    if ParamBindings[i].eParamIO <> DBPARAMIO_INPUT then
      Inc(J);
  FRowSize := Length(ParamBuffer);
  SetLength(FDBBindingArray,J);
  SetLength(FDBBINDSTATUSArray, J);
  FRowSize := Length(ParamBuffer);
  J := 0;
  for I := Low(ParamBindings) to High(ParamBindings) do
    if ParamBindings[i].eParamIO <> DBPARAMIO_INPUT then begin
      Move(ParamBindings[i], FDBBindingArray[j], SizeOf(TDBBinding)); //copy all offsets
      ColumnInfo := TZColumnInfo.Create;
      if I<=High(ParamNameArray) then
        ColumnInfo.ColumnLabel := ParamNameArray[i];
      ColumnInfo.ColumnType := ConvertOleDBTypeToSQLType(
        ParamBindings[i].wType and not DBTYPE_BYREF,
        ParamBindings[i].dwFlags and DBCOLUMNFLAGS_ISLONG <> 0,
        ParamBindings[i].bPrecision, ParamBindings[i].bScale);
      ColumnInfo.Scale := ParamBindings[i].bScale;
      ColumnInfo.Precision := ParamBindings[i].bPrecision;
      ColumnInfo.CharOctedLength := ParamBindings[i].cbMaxLen;
      ColumnsInfo.Add(ColumnInfo);
      Inc(J);
    end;
  Open;
end;

function TZOleDBParamResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := not Closed and ((Row = 1) or (Row = 0));
  if (Row >= 0) and (Row <= 2) then
    RowNo := Row;
end;

function TZOleDBParamResultSet.Next: Boolean;
begin
  Result := not Closed and (RowNo = 0);
  if RowNo = 0 then
    RowNo := 1
  else if RowNo = 1 then
    RowNo := 2; //set AfterLast
end;

{ TZOleDBResultSet }

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param AdoRecordSet a ADO recordset object, the source of the ResultSet.
}
constructor TZOleDBResultSet.Create(const Statement: IZStatement;
  const SQL: string; const RowSet: IRowSet; ZBufferSize: Integer;
  const EnhancedColInfo: Boolean);
begin
  FOleDBConnection := Statement.GetConnection as IZOleDBConnection;
  {if (Statement <> nil) and (Statement.GetConnection.GetServerProvider = spMSSQL)
  then inherited Create(Statement, SQL, TZOleDBMSSQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self), Statement.GetConnection.GetConSettings)
  else}
  FByteBuffer := FOleDBConnection.GetByteBufferAddress;
  inherited Create(Statement, SQL, nil, FOleDBConnection.GetConSettings);
  FRowSet := RowSet;
  FZBufferSize := ZBufferSize;
  FAccessor := 0;
  FCurrentBufRowNo := 0;
  FRowsObtained := 0;
  FHROWS := nil;
  fClientCP := ConSettings.ClientCodePage.CP;
  Open;
end;

function TZOleDBResultSet.Next: Boolean;
var
  I: NativeInt;
  stmt: IZOleDBPreparedStatement;
  Status: HResult;
label NoSuccess;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  stmt := nil;
  if (RowNo > LastRowNo) or Closed or
    ((RowNo = LastRowNo) and (FGetNextRowsStatus = DB_S_ENDOFROWSET)) or
    ((MaxRows > 0) and (RowNo >= MaxRows)) or
    ((not Closed) and (FRowSet = nil) and (not (Supports(Statement, IZOleDBPreparedStatement, Stmt) and Stmt.GetNewRowSet(FRowSet)))) then
    goto NoSuccess;

  if (FRowsObtained > 0) and (FCurrentBufRowNo < DBROWCOUNT(FRowsObtained)-1)
  then Inc(FCurrentBufRowNo)
  else begin
    {release old rows}
    if (RowNo = 0)
    then CreateAccessors
    else ReleaseFetchedRows;

    FGetNextRowsStatus := fRowSet.GetNextRows(DB_NULL_HCHAPTER,0,FRowCount, FRowsObtained, FHROWS);
    if Failed(FGetNextRowsStatus) then
      FOleDBConnection.HandleErrorOrWarning(FGetNextRowsStatus, lcOther, 'IRowSet.GetNextRows', Self);
    if (RowNo = 0) then begin
      if (FGetNextRowsStatus = DB_S_ROWLIMITEXCEEDED) or (FGetNextRowsStatus = DB_S_STOPLIMITREACHED) then begin
        FRowCount := FRowsObtained;
        FGetNextRowsStatus := S_OK; //indicate success
      end;
      if (FGetNextRowsStatus = DB_S_ENDOFROWSET)
      then I := FRowsObtained
      else I := FRowCount;
      SetLength(FColBuffer, I * FRowSize);
      SetLength(FRowStates, I);
    end;
    if (FGetNextRowsStatus = S_OK) or (FGetNextRowsStatus = DB_S_ENDOFROWSET) then
      LastRowNo := RowNo + NativeInt(FRowsObtained);
    FCurrentBufRowNo := 0; //reset Buffer offsett
    if FRowsObtained > 0 then begin
      {fetch data into the buffer}
      for i := 0 to FRowsObtained -1 do begin
        Status := fRowSet.GetData(FHROWS[i], FAccessor, @FColBuffer[I*FRowSize]);
        if Status <> S_OK then
          FOleDBConnection.HandleErrorOrWarning(Status, lcOther, 'IRowSet.GetData', Self);
      end;
    end else goto NoSuccess;
  end;

  RowNo := RowNo + 1;
  Result := True;
  Exit;
NoSuccess:
  if (RowNo = LastRowNo) then begin
    if not LastRowFetchLogged and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    I := LastRowNo; // remainder
    ResetCursor; //free resources
    LastRowNo := I; //apply again
  end;
  if RowNo <= LastRowNo then
    RowNo := LastRowNo + 1;
end;

{**
  Opens this recordset and initializes the Column information.
}
procedure TZOleDBResultSet.Open;
var
  OleDBColumnsInfo: IColumnsInfo;
  prgInfo, OriginalprgInfo: PDBColumnInfo;
  ppStringsBuffer: PWideChar;
  I: Integer;
  FieldSize: Integer;
  ColumnInfo: TZColumnInfo;
label jmpFixedAndSize;
begin
  if not Assigned(FRowSet) or
     Failed(FRowSet.QueryInterface(IID_IColumnsInfo, OleDBColumnsInfo)) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  OleDBColumnsInfo.GetColumnInfo(fpcColumns{%H-}, prgInfo, ppStringsBuffer);
  OriginalprgInfo := prgInfo; //save pointer for Malloc.Free
  try
    { Fills the column info }
    ColumnsInfo.Clear;
    if Assigned(prgInfo) then
      if prgInfo.iOrdinal = 0 then // skip possible bookmark column
      begin
        Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));
        Dec(fpcColumns);
      end;
    SetLength(FDBBINDSTATUSArray, fpcColumns);
    FRowSize := PrepareOleColumnDBBindings(fpcColumns, FDBBindingArray, prgInfo);
    FRowCount := Max(1, FZBufferSize div NativeInt(FRowSize));
    if (MaxRows > 0) and (FRowCount > MaxRows) then
      FRowCount := MaxRows; //fetch only wanted count of rows

    for I := prgInfo.iOrdinal-1 to fpcColumns-1 do
    begin
      ColumnInfo := TZColumnInfo.Create;
      if (prgInfo.pwszName<>nil) and (prgInfo.pwszName^<>#0) then
        {$IFDEF UNICODE}
        System.SetString(ColumnInfo.ColumnLabel, prgInfo^.pwszName, {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(prgInfo^.pwszName));
        {$ELSE}
        ColumnInfo.ColumnLabel := PUnicodeToRaw(prgInfo^.pwszName, {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(prgInfo^.pwszName), zCP_UTF8);
        {$ENDIF}
      ColumnInfo.ColumnType := ConvertOleDBTypeToSQLType(prgInfo^.wType,
        prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG <> 0,
        prgInfo.bScale, prgInfo.bPrecision);

      if prgInfo^.ulColumnSize >= Cardinal(MaxInt) then
        FieldSize := 0
      else
        FieldSize := prgInfo^.ulColumnSize;
      case ColumnInfo.ColumnType of
        stGUID:         ColumnInfo.Precision := 38;
        stString:       begin
                          ColumnInfo.ColumnCodePage := ConSettings.ClientCodePage.CP;
                          goto jmpFixedAndSize;
                        end;
        stUnicodeString:begin
                          ColumnInfo.ColumnCodePage := zCP_UTF16;
                          goto jmpFixedAndSize;
                        end;
        stBytes:        begin
jmpFixedAndSize:          ColumnInfo.Precision := FieldSize;
                          ColumnInfo.Signed := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH) <> 0;
                        end;
        stCurrency,
        stBigDecimal:   begin
                          ColumnInfo.Precision := prgInfo.bPrecision;
                          if (prgInfo^.wType = DBTYPE_CY)
                          then ColumnInfo.Scale := 4
                          else ColumnInfo.Scale := prgInfo.bScale;

                        end;
        stAsciiStream:  begin
                          ColumnInfo.Precision := -1;
                          ColumnInfo.ColumnCodePage := ConSettings.ClientCodePage.CP
                        end;
        stUnicodeStream:begin
                          ColumnInfo.Precision := -1;
                          ColumnInfo.ColumnCodePage := zCP_UTF16
                        end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
      ColumnInfo.Currency := prgInfo.wType = DBTYPE_CY;
      ColumnInfo.AutoIncrement := prgInfo.dwFlags and DBCOLUMNFLAGS_ISROWID = DBCOLUMNFLAGS_ISROWID;
      if (ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stFloat, stDouble, stCurrency, stBigDecimal]) then
        ColumnInfo.Signed := True;
      ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0);
      ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0);
      ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
      ColumnsInfo.Add(ColumnInfo);
      Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));  //M.A. Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));
    end;
  finally
    if Assigned(ppStringsBuffer) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(ppStringsBuffer);
    if Assigned(OriginalprgInfo) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(OriginalprgInfo);
  end;
  inherited Open;
end;

procedure TZOleDBResultSet.ResetCursor;
var
  FAccessorRefCount: DBREFCOUNT;
  Status: HResult;
begin
  if not Closed then begin
    try
      fTempBlob := nil;
      ReleaseFetchedRows;
      if FAccessor > 0 then begin
        Status := (fRowSet As IAccessor).ReleaseAccessor(FAccessor, @FAccessorRefCount);
        if Status <> S_OK then
          FOleDBConnection.HandleErrorOrWarning(Status, lcOther, 'IAccessor.ReleaseAccessor', Self);
      end;
    finally
      FRowSet := nil;
      FAccessor := 0;
      RowNo := 0;
      FCurrentBufRowNo := 0;
      FRowsObtained := 0;
      FGetNextRowsStatus := S_OK;
    end;
    FRowSet := nil;//handle 'Object is in use Exception'
    inherited ResetCursor;
  end;
end;

{ TZOleDBBLob }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZOleDBBLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var P: Pointer;
  R: RawByteString;
  L: NativeUint;
begin
  R := '';
  P := GetBuffer(R, L);
  Result := TZLocalMemBLob.CreateWithData(P, L, FOpenLobStreams);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZOleDBCLob }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZOleDBCLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var P: Pointer;
  R: RawByteString;
  U: UnicodeString;
  L: NativeUint;
  CP: Word;
  ConSettings: PZConSettings;
begin
  ConSettings := FOleConnection.GetConSettings;
  if FwType <> DBTYPE_STR then begin
    U := '';
    P := GetPWideChar(U, L);
    Result := TZLocalMemCLob.CreateWithData(PWideChar(P), L, ConSettings, FOpenLobStreams);
  end else begin
    CP := ConSettings.ClientCodePage.CP;
    R := '';
    P := GetPAnsiChar(CP, R, L);
    Result := TZLocalMemCLob.CreateWithData(P, L, CP, ConSettings, FOpenLobStreams);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZOleDbRowAccessor }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "CachedLobs" not used} {$ENDIF}
constructor TZOleDbRowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  CachedLobs: WordBool);
var TempColumns: TObjectList;
  I: Integer;
  Current: TZColumnInfo;
begin
  {EH: usually this code is NOT nessecary if we would handle the types as the
  providers are able to. But in current state we just copy all the incompatibilities
  from the DataSets into dbc... grumble.}
  TempColumns := TObjectList.Create(True);
  CopyColumnsInfo(ColumnsInfo, TempColumns);
  for I := 0 to TempColumns.Count -1 do begin
    Current := TZColumnInfo(TempColumns[i]);
    {if Current.ColumnType in [stAsciiStream, stUnicodeStream, stBinaryStream] then begin
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-3); // no streams available using OleDB ?
      Current.Precision := -1;
    end;}
    if Current.ColumnType in [stString, stAsciiStream] then begin
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)+1); // no raw chars in 4 OleDB
      Current.ColumnCodePage := zCP_UTF16;
    end else if Current.ColumnType in [stUnicodeString, stUnicodeStream] then
      Current.ColumnCodePage := zCP_UTF16
    else if Current.ColumnType = stBytes then
      Current.ColumnCodePage := zCP_Binary;
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, True); //we can not use uncached lobs with OleDB
  TempColumns.Free;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

initialization
  {init some reusable records (: }
  LobReadObj.dwFlags := STGM_READ;
  LobReadObj.iid := IID_ISequentialStream; //i also testet it with IStream but this does not mork on GetData

  LobDBBinding.obValue := 0;
  LobDBBinding.pTypeInfo := nil;
  LobDBBinding.pObject := @LobReadObj;
  LobDBBinding.pBindExt := nil;
  LobDBBinding.dwPart := DBPART_VALUE;
  LobDBBinding.dwMemOwner := DBMEMOWNER_CLIENTOWNED;
  LobDBBinding.eParamIO := DBPARAMIO_NOTPARAM;
  LobDBBinding.cbMaxLen := 0;
  LobDBBinding.dwFlags := 0;// DBCOLUMNFLAGS_ISLONG;  {}
  LobDBBinding.wType := DBTYPE_IUNKNOWN;
  LobDBBinding.bPrecision := 0;
  LobDBBinding.bScale := 0;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
end.
