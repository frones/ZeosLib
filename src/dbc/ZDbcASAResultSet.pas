{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcASAResultSet;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ASA}

uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}
  System.Types{$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF}
  {$ELSE}
    {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} Types
  {$ENDIF},
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcASA, ZCompatibility, ZDbcCache,
  ZDbcResultSetMetadata, ZDbcASAUtils, ZMessages, ZPlainASADriver,
  ZDbcCachedResultSet, ZClasses;

type

  {** Implements ASA ResultSet. }
  TZASAAbstractResultSet = class(TZAbstractReadOnlyResultSet_A, IZResultSet)
  private
    FSQLDA: PASASQLDA;
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FCursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
    FStmtNum: SmallInt;
    FSqlData: IZASASQLDA;
    FASAConnection: IZASAConnection;
    FPlainDriver: TZASAPlainDriver;
    FByteBuffer: PByteBuffer;
  private
    procedure CheckIndex(const Index: Word);
    procedure CheckRange(const Index: Word);
  protected
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
      const SqlData: IZASASQLDA; CachedBlob: boolean);

    procedure BeforeClose; override;
    procedure AfterClose; override;
    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean;
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
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); reintroduce; overload;
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); reintroduce; overload;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;

    property SQLData: IZASASQLDA read FSQLData;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
  end;

  TZASAParamererResultSet = Class(TZASAAbstractResultSet)
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      var StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}; const SqlData: IZASASQLDA;
      CachedBlob: boolean);
    function Next: Boolean; override;
  end;

  TZASANativeResultSet = Class(TZASAAbstractResultSet)
  public
    function Last: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function MoveRelative(Rows: Integer): Boolean; override;
    function Previous: Boolean; override;
    function Next: Boolean; override;
  end;

  TZASACachedResultSet = Class(TZCachedResultSet)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  End;

  { TZASARowAccessor }

  TZASARowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
    procedure FetchLongData(AsStreamedType: TZSQLType; const ResultSet: IZResultSet;
      ColumnIndex: Integer; Data: PPZVarLenData); override;
  end;

  TZASALob = class;

  TZASAStream = class(TZImmediatelyReleasableLobStream)
  private
    FASASQLDA: PASASQLDA;
    FPlainDriver: TZASAPlainDriver;
    FOwnerLob: TZASALob;
    FAllocated_Arr_len: LongInt;
    FPosition: LongInt;
    Fsqlca: PZASASQLCA;
    FCursorName: PAnsiChar;
    FColumnNumber: Word;
    FsqlInd: SmallInt;
    FConSettings: PZConSettings;
  protected
    function GetSize: Int64; override;
  public
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  public
    constructor Create(const OwnerLob: TZASALob);
    destructor Destroy; override;
  end;

  TZASALob = Class(TZAbstractStreamedLob, IZLob, IZBlob, IImmediatelyReleasable)
  private
    FConnection: IZASAConnection;
    FASASQLDA: PASASQLDA;
    FCursorName: RawByteString;
    FColumnIndex: Integer;
    FReleased: Boolean;
    FCurrentRowAddr: PInteger;
    FLobRowNo: Integer;
    FPlainDriver: TZASAPlainDriver;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public //IImmediatelyReleasable
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost);
    function GetConSettings: PZConSettings;
  public
    constructor Create(const Connection: IZASAConnection; ASASQLDA: PASASQLDA;
      const CursorName: RawByteString; ColumnIndex: Integer;
      LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
      const OpenLobStreams: TZSortedList; CurrentRowAddr: PInteger);
  public
    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
  public
    function Length: Integer; override;
  End;

  TZASABLob = class(TZASALob);

  TZASACLob = class(TZASALob, IZClob);

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
 Math, ZFastCode, ZDbcLogging, ZEncoding, ZDbcUtils;

{ TZASAResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZASAAbstractResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var L: NativeUInt;
    P: Pointer;
    C, H, I: SmallInt;
    procedure AddClob(ColumnCodePage: Word);
    var Clob: IZCLob;
    begin
      JSONWriter.Add('"');
      Clob := TZASAClob.Create(FASAConnection, FSQLDA, FCursorName,
        C, lsmRead, ColumnCodePage, FOpenLobStreams, @FRowNo);
      P := Clob.GetPAnsiChar(zCP_UTF8, FRawTemp, L);
      JSONWriter.AddJSONEscape(P, L);
      JSONWriter.Add('"');
      Clob := nil;
    end;
    procedure AddBlob;
    var Blob: IZBLob;
    begin
      JSONWriter.Add('"');
      Blob := TZASABlob.Create(FASAConnection, FSQLDA, FCursorName,
        C, lsmRead, zCP_Binary, FOpenLobStreams, @FRowNo);
      P := Blob.GetBuffer(FRawTemp, L);
      JSONWriter.WrBase64(P, L, True);
      JSONWriter.Add('"');
      Blob := nil;
    end;
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
    {$R-}
    with FSQLDA.sqlvar[C], TZColumnInfo(ColumnsInfo[C]) do
      if (sqlind <> nil) and (sqlind^ < 0) then
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
        case sqlType and $FFFE of
          DT_NOTYPE           : JSONWriter.AddShort('""');
          DT_SMALLINT         : JSONWriter.Add(PSmallint(sqldata)^);
          DT_INT              : JSONWriter.Add(PInteger(sqldata)^);
          //DT_DECIMAL bound to double
          DT_FLOAT            : JSONWriter.AddSingle(PSingle(sqldata)^);
          DT_DOUBLE           : JSONWriter.AddDouble(PDouble(sqldata)^);
          //DT_DATE bound to TIMESTAMP_STRUCT
          DT_STRING,
          DT_NSTRING,
          DT_FIXCHAR,
          DT_NFIXCHAR,
          DT_VARCHAR,
          DT_NVARCHAR         : begin
                                  JSONWriter.Add('"');
                                  if ColumnCodePage = zCP_UTF8 then
                                    JSONWriter.AddJSONEscape(@PZASASQLSTRING(sqlData).data[0], PZASASQLSTRING(sqlData).length)
                                  else begin
                                    FUniTemp := PRawToUnicode(@PZASASQLSTRING(sqlData).data[0], PZASASQLSTRING(sqlData).length, ConSettings^.ClientCodePage^.CP);
                                    JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
                                  end;
                                  JSONWriter.Add('"');
                                end;
          DT_LONGNVARCHAR,
          DT_LONGVARCHAR      : AddClob(ColumnCodePage);
          DT_TIME,
          DT_TIMESTAMP,
          DT_TIMESTAMP_STRUCT : begin
                                  if jcoMongoISODate in JSONComposeOptions then
                                    JSONWriter.AddShort('ISODate("')
                                  else if jcoDATETIME_MAGIC in JSONComposeOptions then
                                    JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                  else
                                    JSONWriter.Add('"');
                                  if PZASASQLDateTime(sqlData).Year < 0 then
                                    JSONWriter.Add('-');
                                  if (TZColumnInfo(ColumnsInfo[C]).ColumnType <> stTime) then begin
                                    DateToIso8601PChar(PUTF8Char(fByteBuffer), True, Abs(PZASASQLDateTime(sqlData).Year),
                                    PZASASQLDateTime(sqlData).Month + 1, PZASASQLDateTime(sqlData).Day);
                                    JSONWriter.AddNoJSONEscape(PUTF8Char(fByteBuffer),10);
                                  end else if jcoMongoISODate in JSONComposeOptions then
                                    JSONWriter.AddShort('0000-00-00');
                                  if (TZColumnInfo(ColumnsInfo[C]).ColumnType <> stDate) then begin
                                    TimeToIso8601PChar(PUTF8Char(fByteBuffer), True, PZASASQLDateTime(sqlData).Hour,
                                    PZASASQLDateTime(sqlData).Minute, PZASASQLDateTime(sqlData).Second,
                                    PZASASQLDateTime(sqlData).MicroSecond div 1000, 'T', jcoMilliseconds in JSONComposeOptions);
                                    JSONWriter.AddNoJSONEscape(PUTF8Char(fByteBuffer),9 + (4*Ord(jcoMilliseconds in JSONComposeOptions)));
                                  end;
                                  if jcoMongoISODate in JSONComposeOptions
                                  then JSONWriter.AddShort('Z)"')
                                  else JSONWriter.Add('"');
                                end;
          DT_BINARY           : JSONWriter.WrBase64(@PZASASQLSTRING(sqlData).data[0], PZASASQLSTRING(sqlData).length, True);
          DT_LONGBINARY       : AddBlob;
          //DT_VARIABLE: ?
          DT_TINYINT          : JSONWriter.Add(PByte(sqldata)^);
          DT_BIGINT           : JSONWriter.Add(PInt64(sqldata)^);
          DT_UNSINT           : JSONWriter.AddU(PCardinal(sqldata)^);
          DT_UNSSMALLINT      : JSONWriter.AddU(PWord(sqldata)^);
          DT_UNSBIGINT        : JSONWriter.AddQ(PUInt64(sqldata)^);
          DT_BIT              : JSONWriter.AddShort(JSONBool[PByte(sqldata)^ <> 0]);
          else
            raise FSqlData.CreateException(Format(SErrorConvertionField,
              [ FSqlData.GetFieldName(C), ConvertASATypeToString(sqlType)]));
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
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZASAAbstractResultSet.Create(const Statement: IZStatement;
  const SQL: string; StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
  const SqlData: IZASASQLDA; CachedBlob: boolean);
begin
  inherited Create(Statement, SQL, nil,Statement.GetConnection.GetConSettings);

  FFetchStat := 0;
  FSqlData := SqlData;
  Self.FSQLDA := FSqlData.GetData;
  FCursorName := CursorName;
  FCachedBlob := CachedBlob;
  FASAConnection := Statement.GetConnection as IZASAConnection;
  FByteBuffer := FASAConnection.GetByteBufferAddress;
  FPlainDriver := TZASAPlainDriver(FASAConnection.GetIZPlainDriver.GetInstance);
  FStmtNum := StmtNum;
  ResultSetType := rtScrollSensitive;
  ResultSetConcurrency := rcReadOnly;
  Open;
end;

{**
   Check range count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZASAAbstractResultSet.CheckIndex(const Index: Word);
begin
  Assert(Assigned(FSQLDA), 'SQLDA not initialized.');
  Assert(Index < Word(FSQLDA.sqld), 'Out of Range.');
end;

procedure TZASAAbstractResultSet.CheckRange(const Index: Word);
begin
  CheckIndex(Index);
  Assert(Assigned(FSQLDA.sqlVar[Index].sqlData),
    'No memory for variable in SQLDA.');
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZASAAbstractResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  CheckRange(ColumnIndex);
  with FSQLDA.sqlvar[ColumnIndex] do
    Result := Assigned(sqlind) and (sqlind^ < 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZASAAbstractResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := False;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^ <> 0;
        DT_BIT         : Result := PByte(sqldata)^ <> 0;
        DT_SMALLINT    : Result := PSmallint(sqldata)^ <> 0;
        DT_UNSSMALLINT : Result := PWord(sqldata)^ <> 0;
        DT_INT         : Result := PInteger(sqldata)^ <> 0;
        DT_UNSINT      : Result := PCardinal(sqldata)^ <> 0;
        DT_BIGINT      : Result := PInt64(sqldata)^ <> 0;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^ <> 0;
        DT_FLOAT       : Result := PSingle(sqldata)^ <> 0;
        DT_DOUBLE      : Result := PDouble(sqldata)^ <> 0;
        DT_NVARCHAR,
        DT_VARCHAR     :Result := StrToBoolEx(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PAnsiChar(@PZASASQLSTRING(sqlData).data[0])+PZASASQLSTRING(sqlData).length, True);
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            Result := StrToBoolEx(FRawTemp);
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
  {$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := nil;
  Len := 0;
  if not LastWasNull then  with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
    case sqlType and $FFFE of
      DT_BINARY: begin
          Result := @PZASASQLSTRING(sqlData).data;
          Len := PZASASQLSTRING(sqlData).length;
        end;
      DT_LONGBINARY: begin
          Len := PZASABlobStruct( sqlData).untrunc_len;
          FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
          Result := Pointer(FRawTemp);
        end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>Cardinal</code> in Pascal.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PCardinal(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_NVARCHAR,
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt64(FRawTemp);
           end;
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            Result := ZFastCode.RawToInt64(FRawTemp);
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PCardinal(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_NVARCHAR,
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt(FRawTemp);
           end;
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            Result := ZFastCode.RawToInt(FRawTemp);
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PCardinal(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_NVARCHAR,
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToUInt64(FRawTemp);
           end;
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            Result := ZFastCode.RawToUInt64(FRawTemp);
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
      end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PCardinal(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_NVARCHAR,
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt64(FRawTemp);
           end;
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            Result := ZFastCode.RawToInt64(FRawTemp);
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PCardinal(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := PSingle(sqldata)^;
        DT_DOUBLE      : Result := PDouble(sqldata)^;
        DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), 0, Result, PZASASQLSTRING(sqlData).length);
        DT_NVARCHAR,
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            SQLStrToFloatDef(PAnsiChar(Pointer(FRawTemp)), 0, Result, Length(fRawTemp));
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(columnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UUID</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>Zero-UUID</code>
}
procedure TZASAAbstractResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
label Fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    FillChar(Result, SizeOf(TGUID), #0)
  else with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
    case sqlType and $FFFE of
      DT_NVARCHAR,
      DT_VARCHAR    : if (PZASASQLSTRING(sqlData).length = 36) or (PZASASQLSTRING(sqlData).length = 38)
                      then ValidGUIDToBinary(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), @Result.D1)
                      else goto Fail;
      DT_BINARY     : if PZASASQLSTRING(sqlData).length = SizeOf(TGUID) then
                      Move(PZASASQLSTRING(sqlData).data[0], Result.D1, SizeOf(TGUID))
                      else goto Fail;
    else begin
fail:  FillChar(Result, SizeOf(TGUID), #0);
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
      end;
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
function TZASAAbstractResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PCardinal(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := PSingle(sqldata)^;
        DT_DOUBLE      : Result := PDouble(sqldata)^;
        DT_NVARCHAR,
        DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), 0, Result, PZASASQLSTRING(sqlData).length);
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            SQLStrToFloatDef(PAnsiChar(Pointer(FRawTemp)), 0, Result, Length(fRawTemp));
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(columnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
procedure TZASAAbstractResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    FillChar(Result, SizeOf(TBCD), #0)
  else with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
    case sqlType and $FFFE of
      DT_TINYINT     : ScaledOrdinal2BCD(SmallInt(PShortInt(sqldata)^), 0, Result);
      DT_BIT         : ScaledOrdinal2BCD(Word(PByte(sqldata)^), 0, Result, False);
      DT_SMALLINT    : ScaledOrdinal2BCD(PSmallint(sqldata)^, 0, Result);
      DT_UNSSMALLINT : ScaledOrdinal2BCD(PWord(sqldata)^, 0, Result, False);
      DT_INT         : ScaledOrdinal2BCD(PInteger(sqldata)^, 0, Result);
      DT_UNSINT      : ScaledOrdinal2BCD(PCardinal(sqldata)^, 0, Result, False);
      DT_BIGINT      : ScaledOrdinal2BCD(PInt64(sqldata)^, 0, Result);
      DT_UNSBIGINT   : ScaledOrdinal2BCD(PUInt64(sqldata)^, 0, Result, False);
      DT_FLOAT       : Double2BCD(PSingle(sqldata)^, Result);
      DT_DOUBLE      : Double2BCD(PDouble(sqldata)^, Result);
      DT_NVARCHAR,
      DT_VARCHAR     : TryRawToBCD(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, Result, '.');
      DT_LONGNVARCHAR,
      DT_LONGVARCHAR : begin
          FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
          TryRawToBCD(FRawTemp, Result, '.');
        end;
    else
      raise FSqlData.CreateException(Format(SErrorConvertionField,
        [ FSqlData.GetFieldName(columnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
    end;
end;

function TZASAAbstractResultSet.GetCurrency(ColumnIndex: Integer): Currency;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PCardinal(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := PSingle(sqldata)^;
        DT_DOUBLE      : Result := PDouble(sqldata)^;
        DT_NVARCHAR,
        DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), 0, Result, PZASASQLSTRING(sqlData).length);
        DT_LONGNVARCHAR,
        DT_LONGVARCHAR : begin
            FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
            SQLStrToFloatDef(PAnsiChar(Pointer(FRawTemp)), 0, Result, length(FRawTemp));
          end;
      else
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(columnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
procedure TZASAAbstractResultSet.GetDate(ColumnIndex: Integer; Var Result: TZDate);
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_NVARCHAR,
        DT_VARCHAR:
          begin
            P := @PZASASQLSTRING(sqlData).data[0];
            Len := PZASASQLSTRING(sqlData).length;
            LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, Result);
          end;
       DT_TIMESTAMP_STRUCT: begin
            Result.Year := Abs(PZASASQLDateTime(sqlData).Year);
            Result.Month := PZASASQLDateTime(sqlData).Month+1;
            Result.Day := PZASASQLDateTime(sqlData).Day;
            Result.IsNegative := PZASASQLDateTime(sqlData).Year < 0;
          end;
    else
      raise FSqlData.CreateException(Format(SErrorConvertionField,
        [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
procedure TZASAAbstractResultSet.GetTime(ColumnIndex: Integer; Var Result: TZTime);
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_NVARCHAR,
        DT_VARCHAR:
          begin
            P := @PZASASQLSTRING(sqlData).data[0];
            Len := PZASASQLSTRING(sqlData).length;
            LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, Result);
          end;
        DT_TIMESTAMP_STRUCT:
          begin
            Result.Hour := PZASASQLDateTime(sqlData)^.Hour;
            Result.Minute := PZASASQLDateTime(sqlData)^.Minute;
            Result.Second := PZASASQLDateTime(sqlData)^.Second;
            Result.Fractions := PZASASQLDateTime(sqlData).MicroSecond * 1000;
            Result.IsNegative := False;
          end;
        else
          raise FSqlData.CreateException(Format(SErrorConvertionField,
            [FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
procedure TZASAAbstractResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
    with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case sqlType and $FFFE of
        DT_NVARCHAR,
        DT_VARCHAR:
          begin
            P := @PZASASQLSTRING(sqlData).data[0];
            Len := PZASASQLSTRING(sqlData).length;
            LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings, Result);
          end;
        DT_TIMESTAMP_STRUCT: begin
            Result.Year := Abs(PZASASQLDateTime(sqlData).Year);
            Result.Month := PZASASQLDateTime(sqlData).Month+1;
            Result.Day := PZASASQLDateTime(sqlData).Day;
            Result.Hour := PZASASQLDateTime(sqlData)^.Hour;
            Result.Minute := PZASASQLDateTime(sqlData)^.Minute;
            Result.Second := PZASASQLDateTime(sqlData)^.Second;
            PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
            Result.Fractions := PZASASQLDateTime(sqlData).MicroSecond * 1000;
            Result.IsNegative := PZASASQLDateTime(sqlData).Year < 0;
          end;
        else
          raise FSqlData.CreateException(Format(SErrorConvertionField,
            [FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length in bytes of the raw String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAAbstractResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
label set_Results;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := nil;
  Len := 0;
  if not LastWasNull then
  with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
    case sqlType and $FFFE of
      DT_TINYINT    : begin
                        IntToRaw(Cardinal(PByte(sqldata)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_BIT        : if PByte(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      DT_SMALLINT   : begin
                        IntToRaw(Integer(PSmallInt(sqldata)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_UNSSMALLINT: begin
                        IntToRaw(Cardinal(PWord(sqldata)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_INT        : begin
                        IntToRaw(PInteger(sqldata)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_UNSINT     : begin
                        IntToRaw(PCardinal(sqldata)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_BIGINT     : begin
                        IntToRaw(PInt64(sqldata)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_UNSBIGINT  : begin
                        IntToRaw(PUInt64(sqldata)^, PAnsiChar(fByteBuffer), @Result);
set_Results:            Len := Result - PAnsiChar(fByteBuffer);
                        Result := PAnsiChar(fByteBuffer);
                      end;
      DT_FLOAT      : begin
                        Len := FloatToSQLRaw(PSingle(sqldata)^, PAnsiChar(fByteBuffer));
                        Result := PAnsiChar(fByteBuffer);
                      end;
      DT_DOUBLE     : begin
                        Len := FloatToSQLRaw(PDouble(sqldata)^, PAnsiChar(fByteBuffer));
                        Result := PAnsiChar(fByteBuffer);
                      end;
      DT_VARCHAR,
      DT_NVARCHAR,
      DT_BINARY     : begin
                        Result := @PZASASQLSTRING(sqlData).data[0];
                        Len := PZASASQLSTRING(sqlData).length;
                      end;
      DT_LONGBINARY,
      DT_LONGNVARCHAR,
      DT_LONGVARCHAR: begin
                        Len := PZASABlobStruct( sqlData).untrunc_len;
                        FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
                        Len := Length(FRawTemp);
                        if Len = 0
                        then Result := PEmptyAnsiString
                        else Result := Pointer(FRawTemp);
                      end;
      DT_TIMESTAMP_STRUCT : begin
                      Result := PAnsiChar(fByteBuffer);
                      case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType of
                        stDate: Len := DateToRaw(PZASASQLDateTime(SQLData).Year,
                                  PZASASQLDateTime(SQLData).Month +1, PZASASQLDateTime(SQLData).Day,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        stTime: Len := TimeToRaw(PZASASQLDateTime(SQLData).Hour,
                                  PZASASQLDateTime(SQLData).Minute, PZASASQLDateTime(SQLData).Second,
                                  PZASASQLDateTime(SQLData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        else    Len := DateTimeToRaw(PZASASQLDateTime(SQLData).Year,
                                  PZASASQLDateTime(SQLData).Month +1, PZASASQLDateTime(SQLData).Day,
                                  PZASASQLDateTime(SQLData).Hour, PZASASQLDateTime(SQLData).Minute,
                                  PZASASQLDateTime(SQLData).Second, PZASASQLDateTime(SQLData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        end;
                      end;

    else begin
        Result := nil;
        Len := 0;
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
end;

function TZASAAbstractResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
label set_Results, set_from_uni;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := nil;
  Len := 0;
  if not LastWasNull then
  with FSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}], TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    case sqlType and $FFFE of
      DT_TINYINT    : begin
                        IntToUnicode(Cardinal(PByte(sqldata)^), PWideChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_BIT        : if PByte(sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      DT_SMALLINT   : begin
                        IntToUnicode(Integer(PSmallInt(sqldata)^), PWideChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_UNSSMALLINT: begin
                        IntToUnicode(Cardinal(PWord(sqldata)^), PWideChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_INT        : begin
                        IntToUnicode(PInteger(sqldata)^, PWideChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_UNSINT     : begin
                        IntToUnicode(PCardinal(sqldata)^, PWideChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_BIGINT     : begin
                        IntToUnicode(PInt64(sqldata)^, PWideChar(fByteBuffer), @Result);
                        goto set_Results;
                      end;
      DT_UNSBIGINT  : begin
                        IntToUnicode(PUInt64(sqldata)^, PWideChar(fByteBuffer), @Result);
set_Results:            Len := Result - PWideChar(fByteBuffer);
                        Result := PWideChar(fByteBuffer);
                      end;
      DT_FLOAT      : begin
                        Len := FloatToSQLUnicode(PSingle(sqldata)^, PWideChar(fByteBuffer));
                        Result := PWideChar(fByteBuffer);
                      end;
      DT_DOUBLE     : begin
                        Len := FloatToSQLUnicode(PDouble(sqldata)^, PWideChar(fByteBuffer));
                        Result := PWideChar(fByteBuffer);
                      end;
      DT_NVARCHAR,
      DT_VARCHAR    : begin
                        fUniTemp := PRawToUnicode(@PZASASQLSTRING(sqlData).data[0],
                          PZASASQLSTRING(sqlData).length, ColumnCodePage);
                        goto set_from_uni;
                      end;
      DT_BINARY     : begin
                        fUniTemp := Ascii7ToUnicodeString(@PZASASQLSTRING(sqlData).data[0],
                          PZASASQLSTRING(sqlData).length);
                        goto set_from_uni;
                      end;
      DT_LONGNVARCHAR,
      DT_LONGVARCHAR: begin
                        FSqlData.ReadBlobToString(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, FRawTemp);
                        FUniTemp := ZRawtoUnicode(FRawTemp, ColumnCodePage);
set_from_uni:           Len := Length(FUniTemp);
                        if Len = 0
                        then Result := PEmptyUnicodeString
                        else Result := Pointer(FUniTemp);
                      end;
      DT_TIMESTAMP_STRUCT : begin
                      Result := PWideChar(fByteBuffer);
                      case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType of
                        stDate: Len := DateToUni(Abs(PZASASQLDateTime(SQLData).Year),
                                  PZASASQLDateTime(SQLData).Month +1, PZASASQLDateTime(SQLData).Day,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, PZASASQLDateTime(SQLData).Year < 0);
                        stTime: Len := TimeToUni(PZASASQLDateTime(SQLData).Hour,
                                  PZASASQLDateTime(SQLData).Minute, PZASASQLDateTime(SQLData).Second,
                                  PZASASQLDateTime(SQLData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                        else    Len := DateTimeToUni(Abs(PZASASQLDateTime(SQLData).Year),
                                  PZASASQLDateTime(SQLData).Month +1, PZASASQLDateTime(SQLData).Day,
                                  PZASASQLDateTime(SQLData).Hour, PZASASQLDateTime(SQLData).Minute,
                                  PZASASQLDateTime(SQLData).Second, PZASASQLDateTime(SQLData).MicroSecond * 1000,
                                  Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, PZASASQLDateTime(SQLData).Year < 0);
                        end;
                      end;

    else begin
        Result := nil;
        Len := 0;
        raise FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  TempBytes: TBytes;
  Buffer: Pointer;
  Len: NativeUint;
begin
  CheckBlobColumn(ColumnIndex);

  Result := nil;
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then with TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    case ColumnType of
      stAsciiStream, stUnicodeStream:
        Result := TZASAClob.Create(FASAConnection, FSQLDA, FCursorName,
          ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, LobStreamMode, ColumnCodePage, FOpenLobStreams, @FRowNo);
      stBinaryStream:
        Result := TZASABlob.Create(FASAConnection, FSQLDA, FCursorName,
          ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, LobStreamMode, ColumnCodePage, FOpenLobStreams, @FRowNo);
      stGUID, stBytes: begin
          TempBytes := GetBytes(ColumnIndex);
          Result := TZLocalMemBLob.CreateWithData(Pointer(TempBytes), Length(TempBytes));
        end;
      stString, stUnicodeString: begin
          Buffer := GetPAnsiChar(ColumnIndex, Len);
          Result := TZLocalMemCLob.CreateWithData(PAnsiChar(Buffer), Len, ColumnCodePage, ConSettings);
        end;
      else raise CreateCanNotAccessBlobRecordException(ColumnIndex, ColumnType)
    end;
end;

{**
  Opens this recordset.
}
procedure TZASAAbstractResultSet.Open;
var
  i: Integer;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
begin
  if FStmtNum = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
  for i := 0 to FSqlData.GetFieldCount - 1 do begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo, FSqlData  do
    begin
      FieldSqlType := GetFieldSqlType(I);
      ColumnLabel := GetFieldName(I);
      ColumnType := FieldSqlType;
      if FieldSqlType in [stUnicodeString, stUnicodeStream] then begin
        ColumnCodePage := zCP_UTF8;
        if ColumnType = stUnicodeString then begin//ASA calcutates the n column different
          CharOctedLength := GetFieldLength(I);
          Precision := CharOctedLength shr 2; //default UTF8 has 3 bytes only whereas n-cols have 4 bytes
          Signed := FSQLDA.sqlvar[I].sqlType and $FFFE = DT_NFIXCHAR;
        end;
      end else if FieldSqlType in [stString, stAsciiStream] then begin
        ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        if ColumnType = stString then begin
          CharOctedLength := GetFieldLength(I);
          Precision := CharOctedLength div ConSettings^.ClientCodePage^.CharWidth;
          Signed := FSQLDA.sqlvar[I].sqlType and $FFFE = DT_FIXCHAR;
        end;
      end else if FieldSqlType = stBytes then begin
        Precision := GetFieldLength(I);
        CharOctedLength := Precision;
      end else if FieldSqlType = stBigDecimal then begin
        Precision := GetFieldLength(I);
        Scale := GetFieldScale(I);
        if (Scale <= 4) and (Precision <= sAlignCurrencyScale2Precision[Scale]) then
          ColumnType := stCurrency;
        Signed := True;
      end else if FieldSqlType in [stTime, stTimeStamp] then begin
        Precision := GetFieldLength(I);
        Scale := GetFieldScale(I);
      end else
        Signed := ColumnType in [stShort, stSmall, stInteger, stLong, stCurrency, stBigDecimal];
      ReadOnly := False;

      if IsNullable(I)
      then Nullable := ntNullable
      else Nullable := ntNoNulls;
      Nullable := ntNullable;
      AutoIncrement := False;
      CaseSensitive := False;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;
  FSqlData.InitFields; //EH: init fields AFTER retrieving col infos!
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
procedure TZASAAbstractResultSet.AfterClose;
begin
  FCursorName := EmptyRaw;
  inherited AfterClose;
end;

procedure TZASAAbstractResultSet.BeforeClose;
begin
  FSqlData := nil;
  inherited BeforeClose; //Calls ResetCursor so db_close is called!
end;

{**
  Resets cursor position of this recordset to beginning and
  the overrides should reset the prepared handles.
}
procedure TZASAAbstractResultSet.ResetCursor;
begin
  if FCursorName <> EmptyRaw then
    FPLainDriver.dbpp_close(FASAConnection.GetDBHandle, Pointer(FCursorName));
  inherited ResetCursor;
end;

{ TZASAParamererResultSet }

constructor TZASAParamererResultSet.Create(const Statement: IZStatement;
  const SQL: string; var StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
  const SqlData: IZASASQLDA; CachedBlob: boolean);
begin
  inherited Create(Statement, SQL, StmtNum, CursorName, SqlData, CachedBlob);
  SetType(rtForwardOnly);
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
function TZASAParamererResultSet.Next: Boolean;
begin
  Result := (not Closed) and (RowNo = 0);
  if Result then RowNo := 1;
end;

{ TZASANativeResultSet }

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZASANativeResultSet.Last: Boolean;
begin
  if LastRowNo <> MaxInt then
    Result := MoveAbsolute(LastRowNo)
  else
    Result := MoveAbsolute(-1);
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
function TZASANativeResultSet.MoveAbsolute(Row: Integer): Boolean;
var ASASQLCA: PZASASQLCA;
begin
  Result := False;
  if Closed or ((MaxRows > 0) and (Row >= MaxRows)) then
    Exit;
  ASASQLCA := FASAConnection.GetDBHandle;
  FPlainDriver.dbpp_fetch(ASASQLCA,
    Pointer(FCursorName), CUR_ABSOLUTE, Row, FSQLDA, BlockSize, CUR_FORREGULAR);
  if ASASQLCA.sqlCode = SQLE_CURSOR_NOT_OPEN then Exit;
  if (ASASQLCA.sqlCode <> SQLE_NOERROR) and (ASASQLCA.sqlCode <> SQLE_NOTFOUND) then
    FASAConnection.HandleErrorOrWarning(lcOther, 'dbpp_fetch', Self);

  if ASASQLCA.sqlCode <> SQLE_NOTFOUND then begin
    RowNo := Row;
    Result := True;
    FFetchStat := 0;
  end else begin
    FFetchStat := ASASQLCA.sqlerrd[2];
    if FFetchStat > 0 then
      LastRowNo := Max(Row - FFetchStat, 0);
    if not LastRowFetchLogged and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
  end;
end;

{**
  Moves the cursor a relative number of rows, either positive or negative.
  Attempting to move beyond the first/last row in the
  result set positions the cursor before/after the
  the first/last row. Calling <code>relative(0)</code> is valid, but does
  not change the cursor position.

  <p>Note: Calling the method <code>relative(1)</code>
  is different from calling the method <code>next()</code>
  because is makes sense to call <code>next()</code> when there
  is no current row,
  for example, when the cursor is positioned before the first row
  or after the last row of the result set.

  @return <code>true</code> if the cursor is on a row;
    <code>false</code> otherwise
}
function TZASANativeResultSet.MoveRelative(Rows: Integer): Boolean;
var ASASQLCA: PZASASQLCA;
begin
  Result := False;
  if Closed or ((RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows))) then
    Exit;
  ASASQLCA := FASAConnection.GetDBHandle;
  FPlainDriver.dbpp_fetch(ASASQLCA,
    Pointer(FCursorName), CUR_RELATIVE, Rows, FSQLDA, BlockSize, CUR_FORREGULAR);
  //handle a known null resultset issue (cursor not open)
  //EH: is this correct?
  if ASASQLCA.sqlCode = SQLE_CURSOR_NOT_OPEN then Exit;
  if (ASASQLCA.sqlCode <> SQLE_NOERROR) and (ASASQLCA.sqlCode <> SQLE_NOTFOUND) then
    FASAConnection.HandleErrorOrWarning(lcOther, 'dbpp_fetch', Self);

  if ASASQLCA.sqlCode <> SQLE_NOTFOUND then begin
    RowNo := RowNo + Rows;
    if Rows > 0 then
      LastRowNo := RowNo;
    Result := True;
    FFetchStat := 0;
  end else begin
    FFetchStat := FASAConnection.GetDBHandle.sqlerrd[2];
    if (FFetchStat > 0) and (RowNo > 0) then
      LastRowNo := Max(RowNo + Rows - FFetchStat, 0);
    if Rows > 0 then
      RowNo := LastRowNo + 1;
  end;
end;

{**
  Moves the cursor to the previous row in this
  <code>ResultSet</code> object.

  <p><B>Note:</B> Calling the method <code>previous()</code> is not the same as
  calling the method <code>relative(-1)</code> because it
  makes sense to call</code>previous()</code> when there is no current row.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if it is off the result set
}
function TZASANativeResultSet.Previous: Boolean;
begin
  Result := MoveRelative(-1);
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
function TZASANativeResultSet.Next: Boolean;
begin
  Result := MoveRelative(1);
end;

{ TZASACachedResultSet }

class function TZASACachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZASARowAccessor;
end;

{ TZASARowAccessor }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "CachedLobs" not used} {$ENDIF}
constructor TZASARowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  CachedLobs: WordBool);
var TempColumns: TObjectList;
  I: Integer;
  Current: TZColumnInfo;
begin
  TempColumns := TObjectList.Create(True);
  CopyColumnsInfo(ColumnsInfo, TempColumns);
  for I := 0 to TempColumns.Count -1 do begin
    Current := TZColumnInfo(TempColumns[i]);
    if Current.ColumnType in [stUnicodeString, stUnicodeStream] then
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-1); // no national chars in 4 asa
   { eh: we can stream the data with asa, but we need always to fetch the
     current row and we have no descriptor so we cache the data }
   if Current.ColumnType in [stAsciiStream, stBinaryStream] then
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-3);
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, False);
  TempColumns.Free;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZASARowAccessor.FetchLongData(AsStreamedType: TZSQLType;
  const ResultSet: IZResultSet; ColumnIndex: Integer; Data: PPZVarLenData);
const
  MaxBufSize = $F000;
var Lob: IZBlob;
  Stream: TStream;
  Len, BufSize, ReadBytes: Cardinal;
  buf: PAnsiChar;
begin
  if Data^ <> nil then
    FreeMem(Data^);
  Lob := ResultSet.GetBlob(ColumnIndex);
  Stream := Lob.GetStream;
  try
    Len := Stream.Size;
    if Len = 0 then Exit;
    if Len > MaxBufSize
    then BufSize := MaxBufSize
    else BufSize := Len;
    GetMem(Data^, SizeOf(Cardinal)+Len+Byte(AsStreamedType=stAsciiStream));
    Data^.Len := Len;
    buf := @Data^.Data;
    while Len <> 0 do begin
      ReadBytes := Stream.Read(Buf^, BufSize);
      Dec(Len, ReadBytes);
      Inc(Buf, readBytes);
      if Len < BufSize then
        BufSize := Len;
    end;
    if AsStreamedType = stAsciiStream then
      PByte(Buf)^ := 0;
  finally
    Stream.Free;
    Lob := nil;
  end;
end;

{ TZASAStream }

constructor TZASAStream.Create(const OwnerLob: TZASALob);
begin
  inherited Create(OwnerLob, OwnerLob.FConnection, OwnerLob.FOpenLobStreams);
  FOwnerLob := OwnerLob;
  FPlainDriver := OwnerLob.FConnection.GetPlainDriver;
  FASASQLDA := FPlainDriver.alloc_sqlda(1);
  FASASQLDA.sqld := FASASQLDA.sqln;
  Fsqlca := OwnerLob.FConnection.GetDBHandle;
  FCursorName := Pointer(OwnerLob.FCursorName);
  FConSettings := OwnerLob.FConnection.GetConSettings;
  if FCursorName = nil then
    FCursorName := PEmptyAnsiString;
  FColumnNumber := OwnerLob.FColumnIndex + 1;
  with FASASQLDA.sqlVar[0] do begin
    sqlType := OwnerLob.FASASQLDA.sqlVar[OwnerLob.FColumnIndex].sqlType;
    sqllen := SizeOf( TZASABlobStruct)-1;
    sqlInd := @FsqlInd;
    sqlData := nil;
    sqlname.length := 0;
    sqlname.data[0] := AnsiChar(#0);
  end;
end;

destructor TZASAStream.Destroy;
begin
  if FASASQLDA <> nil then begin
    if FASASQLDA.sqlVar[0].sqlData <> nil then
      FreeMem(FASASQLDA.sqlVar[0].sqlData);
    FPlainDriver.free_sqlda(FASASQLDA);
    FASASQLDA := nil;
  end;
  inherited Destroy;
end;

function TZASAStream.GetSize: Int64;
begin
  if FReleased
  then Result := 0
  else Result := PZASABlobStruct(FOwnerLob.FASASQLDA.sqlVar[FOwnerLob.FColumnIndex].sqlData).untrunc_len;
end;

function TZASAStream.Read(var Buffer; Count: Longint): Longint;
var ASASQLVAR: PZASASQLVAR;
begin
  if FReleased or (Count = 0)
  then Result := 0
  else begin
    if FOwnerLob.FLobStreamMode = lsmWrite then
      raise CreateWriteOnlyException;
    ASASQLVAR := @FASASQLDA.sqlVar[0];
    if Count > FAllocated_Arr_len then begin
      ReallocMem(ASASQLVAR.sqlData, SizeOf(TZASABlobStruct)+Count);
      PZASABlobStruct(ASASQLVAR.sqlData).stored_len := 0;
      PZASABlobStruct(ASASQLVAR.sqlData).untrunc_len := 0;
      PZASABlobStruct(ASASQLVAR.sqlData).arr[0] := AnsiChar(#0);
      FAllocated_Arr_len := Count;
    end;
    PZASABlobStruct(ASASQLVAR.sqlData).array_len := Count;
    FPlainDriver.dbpp_get_data(Fsqlca, FCursorName, FColumnNumber, FPosition, FASASQLDA, 0);
    if (Fsqlca.sqlCode <> SQLE_NOERROR) then
      FOwnerLob.FConnection.HandleErrorOrWarning(lcOther, 'dbpp_get_data', Self);
    Result := PZASABlobStruct(ASASQLVAR.sqlData)^.stored_len;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(
      PZASABlobStruct(ASASQLVAR.sqlData)^.arr[0], Buffer, Result);
    FPosition := FPosition+Result;
  end;
end;

function TZASAStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Origin = soEnd then
    Result := Int64(PZASABlobStruct(FOwnerLob.FASASQLDA.sqlVar[FOwnerLob.FColumnIndex].sqlData).untrunc_len) - OffSet
  else if Origin = soCurrent then
    Result := FPosition + OffSet
  else
    Result := OffSet;
  if Result <> FPosition then
    FPosition := Result;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "Buffer/Count" not used}
{$ENDIF}
function TZASAStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZASALob }

procedure TZASALob.Clear;
begin
  raise CreateReadOnlyException;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZASALob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
begin
  Result := nil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

constructor TZASALob.Create(const Connection: IZASAConnection;
  ASASQLDA: PASASQLDA; const CursorName: RawByteString; ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList; CurrentRowAddr: PInteger);
begin
  inherited Create(ColumnCodePage, OpenLobStreams);
  FConnection := Connection;
  FASASQLDA := ASASQLDA;
  FCursorName := CursorName;
  FColumnIndex := ColumnIndex;
  FLobStreamMode := LobStreamMode;
  FCurrentRowAddr := CurrentRowAddr;
  FLobRowNo := CurrentRowAddr^;
  FPlainDriver := Connection.GetPlainDriver;
  FConSettings := Connection.GetConSettings;
end;

function TZASALob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
var ASASQLCA: PZASASQLCA;
begin
  if FReleased
  then Result := nil
  else begin
    ASASQLCA := FConnection.GetDBHandle;
    if LobStreamMode <> lsmRead then
      raise CreateReadOnlyException;
    if FCurrentRowAddr^ <> FLobRowNo then begin
      FPlainDriver.dbpp_fetch(ASASQLCA,
        Pointer(FCursorName), CUR_ABSOLUTE, FLobRowNo, FASASQLDA, BlockSize, CUR_FORREGULAR);
      if (ASASQLCA.sqlCode <> SQLE_NOERROR) then
        FConnection.HandleErrorOrWarning(lcOther, 'dbpp_fetch', Self);
    end;
    Result := TZASAStream.Create(Self);
    if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
      Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
  end;
end;

function TZASALob.GetConSettings: PZConSettings;
begin
  if FConnection = nil
  then Result := nil
  else Result := FConnection.GetConSettings;
end;

function TZASALob.IsEmpty: Boolean;
begin
  Result := False;
end;

function TZASALob.Length: Integer;
begin
  if (FASASQLDA = nil) or (FASASQLDA.sqlVar[FColumnIndex].sqlData = nil)
  then Result := -1
  else Result := PZASABlobStruct(FASASQLDA.sqlVar[FColumnIndex].sqlData).untrunc_len;
end;

procedure TZASALob.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var Imm: IImmediatelyReleasable;
begin
  if (FConnection <> nil) and (FConnection.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and
     (Sender <> imm) then
    Imm.ReleaseImmediat(Sender, AError);
  FASASQLDA := nil;
  FConnection := nil;
  FReleased := true;
end;

initialization
{$ENDIF ZEOS_DISABLE_ASA}
end.
