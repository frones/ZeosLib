{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
{                                                         }
{           Originally written by EgonHugeist             }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcFirebirdResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit

uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  FmtBCD, {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZPlainFirebird, ZCompatibility, ZClasses,
  ZDbcResultSet, ZDbcInterbase6Utils,
  ZDbcFirebird, ZDbcCachedResultSet, ZDbcCache, ZDbcResultSetMetadata,
  ZPlainFirebirdInterbaseDriver,
  ZPlainFirebirdDriver, ZDbcFirebirdInterbase, ZDbcLogging, ZDbcIntfs;

type
  IZFirebirdResultSet = Interface(IZResultSet)
    ['{44E775F4-4E7D-4F92-9B97-5C5E504019F9}']
    function GetConnection: IZFirebirdConnection;
  End;

  PIResultSet = ^IResultSet;

  TZAbstractFirebirdResultSet = Class(TZAbstractInterbaseFirebirdResultSet,
    IZResultSet, IZFirebirdResultSet)
  private
    FStatus: IStatus;
    FDataBuffer: Pointer;
    FFBConnection: IZFirebirdConnection;
    FFBTransaction: IZFirebirdTransaction;
    FPlainDriver: TZInterbaseFirebirdPlainDriver;
    FFirstRow: Boolean;
    procedure DeRegisterCursor;
  public //implement IZFirebirdResultSet
    function GetConnection: IZFirebirdConnection;
  public
    procedure RegisterCursor;
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      MessageMetadata: IMessageMetadata; Status: IStatus;
      DataBuffer: Pointer);
  public
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
  end;

  TZFirebirdResultSet = class(TZAbstractFirebirdResultSet)
  private
    FResultset: IResultset;
    FResultSetAddr: PIResultSet;
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      MessageMetadata: IMessageMetadata; Status: IStatus;
      DataBuffer: Pointer; ResultSet: PIResultSet);
    procedure ResetCursor; override;

  public { Traversal/Positioning }
    function IsBeforeFirst: Boolean; override;
    function IsAfterLast: Boolean; override;
    function First: Boolean; override;
    function Last: Boolean; override;
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function MoveRelative(Rows: Integer): Boolean; override;
    function Previous: Boolean; override;
  end;

  TZFirebirdOutParamResultSet = class(TZAbstractFirebirdResultSet)
  public
    Constructor Create(const Statement: IZStatement; const SQL: String;
      MessageMetadata: IMessageMetadata; Status: IStatus;
      DataBuffer: Pointer);
  public { Traversal/Positioning }
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  TZFirebirdLob = class;

  {** EH: implements a Firebird large object stream }
  TZFirebirdLobStream = class(TZImmediatelyReleasableLobStream, IImmediatelyReleasable)
  private
    FPlainDriver: TZInterbaseFirebirdPlainDriver;
    FAttachment: IAttachment;
    FFBTransaction: ITransaction;
    FStatus: IStatus;
    FBlob: IBlob;
    FLobIsOpen: Boolean;
    FPosition: Integer;
    FOwnerLob: TZFirebirdLob;
  protected
    procedure FillBlobInfo;
    function GetSize: Int64; override;
  public
    constructor Create(const OwnerLob: TZFirebirdLob);
    destructor Destroy; override;
  public
    BlobId: TISC_QUAD;
    Updated: Boolean;
    BlobInfo: PIbBlobInfo;
  public
    procedure OpenLob;
    procedure CloseLob;
    procedure CreateLob;
    procedure CancelLob;
  public //TStream overrides
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
  end;

  IZInterbaseLob = interface(IZLob)
    ['{85E3AA45-07A5-476E-83A7-15D40C4DEFCE}']
    function GetBlobId: TISC_QUAD;
  end;

  { TZFirebirdLob }

  TZFirebirdLob = Class(TZAbstractStreamedLob, IZLob, IZBlob,
    IImmediatelyReleasable, IZInterbaseLob)
  private
    FLobStream: TZFirebirdLobStream;
    FPlainDriver: TZFirebird3UpPlainDriver;
    FBlobId: TISC_QUAD;
    FFBConnection: IZFirebirdConnection;
    FReleased: Boolean;
    FBlobInfo: TIbBlobInfo;
    FBlobInfoFilled: Boolean;
    FIsTemporary: Boolean;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public //IImmediatelyReleasable
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
    function GetConSettings: PZConSettings;
  public
    function GetBlobId: TISC_QUAD;
  public
    function Clone(LobStreamMode: TZLobStreamMode): IZBlob;
    function IsEmpty: Boolean; override;
    procedure Clear; override;
  public //obsolate
    function Length: Integer; override;
  public
    constructor Create(const Connection: IZFirebirdConnection; const BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
      const OpenLobStreams: TZSortedList);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  End;

  TZFirebirdClob = Class(TZFirebirdLob, IZCLob)
  public
    constructor Create(const Connection: IZFirebirdConnection;
      BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode;
      ColumnCodePage: Word; const OpenLobStreams: TZSortedList);
  End;

  TZFirebirdBLob = Class(TZFirebirdLob)
  public
    constructor Create(const Connection: IZFirebirdConnection; BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
  End;

  {**
    Implements Firebird cached ResultSet. This class should be extended
    with database specific logic to form SQL data manipulation statements.
  }
  TZFirebirdCachedResultSet = Class(TZCachedResultset)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    function CreateLob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode): IZBlob{IZLob}; override;
  End;

  TZFirebirdRowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
  end;

  IZFirebirdLob = interface(IZLob)
    ['{A6AFDB99-1472-4FCD-86A5-79506532FE68}']
    function GetBlobId: TISC_QUAD;
  end;

  function ConvertIB_FBType2SQLType(AType, ASubType: Cardinal; Scale: Integer): TZSQLType;

{$ENDIF ZEOS_DISABLE_FIREBIRD}
implementation
{$IFNDEF ZEOS_DISABLE_FIREBIRD}

uses SysUtils, ZDbcUtils, ZSysUtils, ZFastCode, ZEncoding, ZMessages;

function ConvertIB_FBType2SQLType(AType, ASubType: Cardinal; Scale: Integer): TZSQLType;
begin
  case AType of
    SQL_VARYING, SQL_TEXT:
      if ASubType = CS_BINARY {Octets}
      then Result := stBytes
      else Result := stString;
    SQL_LONG:
        if Scale = 0 then
          Result := stInteger
        else if Scale >= -4 then
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_SHORT:
        if Scale = 0 then
          Result := stSmall
        else if Scale >= -4 then
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_FLOAT:
      Result := stFloat;
    SQL_DOUBLE, SQL_D_FLOAT:
      Result := stDouble;
    SQL_BOOLEAN, SQL_BOOLEAN_FB:
      Result := stBoolean;
    SQL_DATE: Result := stTimestamp;
    SQL_TYPE_TIME: Result := stTime;
    SQL_TYPE_DATE: Result := stDate;
    SQL_INT64:
        //https://firebirdsql.org/file/documentation/reference_manuals/fblangref25-en/html/fblangref25-datatypes-fixedtypes.html
        if Scale = 0 then
          Result := stLong
        else if Scale = -4 then //EH firebird supports a max precision of 18 only
          Result := stCurrency
        else
          Result := stBigDecimal;
    SQL_QUAD, SQL_BLOB:
        if ASubType = isc_blob_text
        then Result := stAsciiStream
        else Result := stBinaryStream;
    SQL_ARRAY: Result := stArray;
    else  Result := stUnknown;
  end;
end;
{ TZAbstractFirebirdResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractFirebirdResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var L, H, I: Integer;
    P: Pointer;
    C, SQLCode: ISC_SHORT;
    TempDate: TZTimeStamp;//TCTimeStructure;
  procedure WConvert(P: PAnsiChar; L: ISC_SHORT; CP: word); //no _UStrClr in method
  begin
    FUniTemp := PRawToUnicode(P, L, CP);
    JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp))
  end;
  procedure ReadUTF8CLob(const BlobId: TISC_QUAD);
  var Stream: TStream;
    IbStream: TZFirebirdLobStream;
    Clob: IZCLob;
    Buf: PAnsiChar;
    L, R, Size: LongInt;
  begin
    CLob := TZFirebirdClob.Create(FFBConnection, BlobId, lsmRead, zCP_UTF8, FOpenLobStreams);
    Stream := Clob.GetStream(zCP_UTF8);
    IbStream := Stream as TZFirebirdLobStream;
    Buf := nil;
    try
      IbStream.OpenLob;
      Size := IbStream.BlobInfo.TotalSize;
      if Size = 0 then Exit;
      { read chunked as firebird supports it }
      L := IbStream.BlobInfo.MaxSegmentSize;
      GetMem(Buf, L);
      repeat
        R := Stream.Read(Buf^, L);
        JSONWriter.AddJSONEscape(Buf, R); //is not #0 terminated
        Dec(Size, R);
        if L > Size then
          L := Size;
      until (R = 0){should not happen} or
            (Size = 0){if segmentsize < total};
    finally
      Stream.Free;
      FreeMem(Buf);
      Clob := nil;
    end;
  end;
  procedure ReadAsWCLob(const BlobId: TISC_QUAD; ColumnCodePage: Word);
  var Clob: IZCLob;
    PW: Pointer;
    L: NativeUInt;
  begin
    CLob := TZFirebirdClob.Create(FFBConnection, BlobId, lsmRead, ColumnCodePage, FOpenLobStreams);
    try
      PW := CLob.GetPWideChar(FUniTemp, L);
      JSONWriter.AddJSONEscapeW(PW, L); //is not #0 terminated
      FUniTemp := '';
    finally
      Clob := nil;
    end;
  end;
  procedure ReadBLob(const BlobId: TISC_QUAD);
  var Blob: IZBLob;
    P: Pointer;
    L: NativeUInt;
  begin
    Blob := TZFirebirdBlob.Create(FFBConnection, BlobId, lsmRead, FOpenLobStreams);
    try
      P := Blob.GetBuffer(FRawTemp, L); //base 64 can not be added in chunks ):
      JSONWriter.WrBase64(P, L, True);
      FRawTemp := '';
    finally
      Blob := nil;
    end;
  end;
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
    with TZInterbaseFirebirdColumnInfo(ColumnsInfo[c]) do
      if (sqlind <> nil) and (sqlind^ = ISC_NULL) then
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
        SQLCode := (sqltype and not(1));
        case SQLCode of
          SQL_VARYING   : if sqlsubtype = CS_BINARY {octets} then
                            JSONWriter.WrBase64(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, True)
                          else begin
                            JSONWriter.Add('"');
                            if ColumnCodePage = zCP_UTF8
                            then JSONWriter.AddJSONEscape(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen)
                            else WConvert(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, ColumnCodePage);
                            JSONWriter.Add('"');
                          end;
          SQL_TEXT      : if sqlsubtype = CS_BINARY then
                            JSONWriter.WrBase64(sqldata, CharOctedLength, True)
                          else begin
                            JSONWriter.Add('"');
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), CharOctedLength);
                            if ColumnCodePage = zCP_UTF8
                            then JSONWriter.AddJSONEscape(sqldata, L)
                            else WConvert(sqldata, L, ColumnCodePage);
                            JSONWriter.Add('"');
                          end;
          SQL_D_FLOAT,
          SQL_DOUBLE    : JSONWriter.AddDouble(PDouble(sqldata)^);
          SQL_FLOAT     : JSONWriter.AddSingle(PSingle(sqldata)^);
          SQL_SHORT     : if sqlscale = 0 then
                            JSONWriter.Add(PISC_SHORT(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(Integer(PISC_SHORT(sqldata)^), @FTinyBuffer, @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PAnsiChar(P)-PAnsiChar(@FTinyBuffer[0]));
                          end;
          SQL_LONG      : if sqlscale = 0 then
                            JSONWriter.Add(PISC_LONG(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(PISC_LONG(sqldata)^, @FTinyBuffer, @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PAnsiChar(P)-PAnsiChar(@FTinyBuffer[0]));
                          end;
          SQL_INT64     : if (sqlscale = 0) then
                            JSONWriter.Add(PISC_INT64(sqldata)^)
                          else if sqlScale = -4 then
                            JSONWriter.AddCurr64(PISC_INT64(sqldata)^)
                          else begin
                            ScaledOrdinal2Raw(PISC_INT64(sqldata)^, @FTinyBuffer, @P, -sqlscale);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0], PAnsiChar(P)-PAnsiChar(@FTinyBuffer[0]));
                          end;
          SQL_TIMESTAMP : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                              TempDate.Year, TempDate.Month, Tempdate.Day);
                            DateToIso8601PChar(@FTinyBuffer[0], True, TempDate.Year, TempDate.Month, TempDate.Day);
                            isc_decode_time(PISC_TIMESTAMP(sqldata).timestamp_time,
                              TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                            TimeToIso8601PChar(@FTinyBuffer[10], True, TempDate.Hour, TempDate.Minute,
                              TempDate.Second, TempDate.Fractions div 10, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
          SQL_QUAD,
          SQL_BLOB      : begin
                            if SqlSubType = isc_blob_text then begin
                              JSONWriter.Add('"');
                              if ColumnCodePage = zCP_UTF8
                              then ReadUTF8CLob(PISC_QUAD(sqldata)^)
                              else ReadAsWCLob(PISC_QUAD(sqldata)^, ColumnCodePage);
                              JSONWriter.Add('"');
                            end else ReadBlob(PISC_QUAD(sqldata)^);
                          end;
          //SQL_ARRAY     : JSONWriter.AddShort('"Array"');
          SQL_TYPE_TIME : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("0000-00-00')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            end else
                              JSONWriter.Add('"');
                            isc_decode_time(PISC_TIME(sqldata)^, TempDate.Hour,
                              TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                            TimeToIso8601PChar(@FTinyBuffer[0], True, TempDate.Hour, TempDate.Minute,
                              TempDate.Second,  TempDate.Fractions div 10, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],8+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z)"')
                            else JSONWriter.Add('"');
                          end;
          SQL_TYPE_DATE : begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                            else
                              JSONWriter.Add('"');
                            isc_decode_date(PISC_DATE(sqldata)^, TempDate.Year, TempDate.Month, Tempdate.Day);
                            DateToIso8601PChar(@FTinyBuffer[0], True, TempDate.Year, TempDate.Month, Tempdate.Day);
                            JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],10);
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
          SQL_BOOLEAN   : JSONWriter.AddShort(JSONBool[PISC_BOOLEAN(sqldata)^ <> 0]);
          SQL_BOOLEAN_FB: JSONWriter.AddShort(JSONBool[PISC_BOOLEAN_FB(sqldata)^ <> 0]);
          else          raise ZDbcUtils.CreateConversionError(C, ColumnType, stString);
        end;
        JSONWriter.Add(',');
      end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF USE_SYNCOMMONS}

constructor TZAbstractFirebirdResultSet.Create(const Statement: IZStatement;
  const SQL: String; MessageMetadata: IMessageMetadata;
  Status: IStatus; DataBuffer: Pointer);
var I, Len, OffSet: Cardinal;
  CP_ID: Word;
  ColumnInfo: TZInterbaseFirebirdColumnInfo;
  P: PAnsiChar;
  ZCodePageInfo: PZCodePage;
label jmpLen;
begin
  inherited Create(Statement, SQL);
  FStatus := Status;
  FFBConnection := Statement.GetConnection as IZFirebirdConnection;
  FPlainDriver := FFBConnection.GetPlainDriver;
  FFirstRow := True;
  FDataBuffer := DataBuffer;

  I := MessageMetadata.getCount(FStatus);
  if i = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  ColumnsInfo.Capacity := I;
  for i := 0 to I -1 do begin
    {$IFDEF UNICODE}
    if ConSettings.ClientCodePage.ID = CS_NONE
    then CP_ID := zCP_UTF8
    else CP_ID := ConSettings.ClientCodePage.CP;
    {$ENDIF UNICODE}
    ColumnInfo := TZInterbaseFirebirdColumnInfo.Create;
    ColumnsInfo.Add(ColumnInfo);
    with ColumnInfo do begin
      P := MessageMetadata.getRelation(FStatus, I);
      Len := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      TableName := PRawToUnicode(P, Len, CP_ID);
      {$ELSE}
      System.SetString(TableName, P, Len);
      {$ENDIF}
      if TableName <> '' then begin //firebird does not corectly clear the name
        //see TestColumnTypeAndTableDetermination we get a 'ADD' in buffer back
        P := MessageMetadata.getField(FStatus, I);
        Len := ZFastCode.StrLen(P);
        {$IFDEF UNICODE}
        ColumnName := PRawToUnicode(P, Len, CP_ID);
        {$ELSE}
        System.SetString(ColumnName, P, Len);
        {$ENDIF}
      end;
      P := MessageMetadata.getAlias(FStatus, I);
      Len := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      ColumnLabel := PRawToUnicode(P, Len, CP_ID);
      {$ELSE}
      System.SetString(ColumnLabel, P, Len);
      {$ENDIF}
      sqltype := MessageMetadata.getType(FStatus, I);
      sqlsubType := MessageMetadata.getSubType(FStatus, I);
      Len := MessageMetadata.getLength(FStatus, I);
      sqlscale := MessageMetadata.getScale(FStatus, I);
      Scale := -sqlscale;
      if (sqltype = SQL_TEXT) or (sqltype = SQL_VARYING) then begin //SQL_BLOB
        CP_ID := Word(MessageMetadata.getCharSet(FStatus, I)) and 255;
        ColumnType := ConvertIB_FBType2SQLType(sqltype, CP_ID, sqlscale);
      end else
        ColumnType := ConvertIB_FBType2SQLType(sqltype, sqlsubtype, sqlscale);
      if FGUIDProps.ColumnIsGUID(ColumnType, len, ColumnName) then
        ColumnType := stGUID;
      if MessageMetadata.isNullable(FStatus, I) then begin
        OffSet := MessageMetadata.getNullOffset(FStatus, I);
        sqlind := PISC_SHORT(PAnsiChar(FDataBuffer)+OffSet);
        Nullable := ntNullable;
      end;
      OffSet := MessageMetadata.getOffset(FStatus, I);
      sqldata := PAnsiChar(FDataBuffer)+OffSet;
      if sqlind = sqldata then
        sqlind := nil;
      case ColumnType of
        stString, stGUID: begin
            //see test Bug#886194, we retrieve 565 as CP... the modula returns the FBID of CP
            CP_ID := Word(MessageMetadata.getCharSet(FStatus, I)) and 255;
            //see: http://sourceforge.net/p/zeoslib/tickets/97/
            if (CP_ID = ConSettings^.ClientCodePage^.ID)
            then ZCodePageInfo := ConSettings^.ClientCodePage
            else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CP_ID); //get column CodePage info}
            ColumnCodePage := ZCodePageInfo.CP;
            if (ColumnType = stGUID) or (ConSettings^.ClientCodePage^.ID = CS_NONE) then begin
  jmpLen:     Precision := Len;
              CharOctedLength := Precision;
            end else begin
              CharOctedLength := len;
              Precision := len div Cardinal(ZCodePageInfo^.CharWidth);
            end;
            Signed := sqltype = SQL_TEXT;
          end;
        stAsciiStream, stUnicodeStream: if ConSettings^.ClientCodePage^.ID = CS_NONE
          then if FIsMetadataResultSet
            then ColumnCodePage := zCP_UTF8
            else begin //connected with CS_NONE no transliterations are made by FB
              CP_ID := FFBConnection.GetSubTypeTextCharSetID(TableName,ColumnName);
              if CP_ID = CS_NONE
              then ZCodePageInfo := ConSettings^.ClientCodePage
              else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CP_ID);
              ColumnCodePage := ZCodePageInfo.CP;
            end else ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        stBytes: begin
            ColumnCodePage := zCP_Binary;
            goto jmpLen;
          end;
        stBinaryStream: ColumnCodePage := zCP_Binary;
        else begin
          ColumnCodePage := zCP_NONE;
          case ColumnType of
            stShort, stSmall, stInteger, stLong: Signed := True;
            stCurrency, stBigDecimal: begin
              Signed  := True;
              Scale   := Scale;
              //first digit does not count because of overflow (FB does not allow this)
              case sqltype of
                SQL_SHORT:  Precision := 4;
                SQL_LONG:   Precision := 9;
                SQL_INT64:  Precision := 18;
              end;
            end;
            stTime, stTimeStamp: Scale := {-}4; //fb supports 10s of millisecond fractions
          end;
        end;
      end;
      ReadOnly := (TableName = '') or (ColumnName = '') or
        (ColumnName = 'RDB$DB_KEY') or (ColumnType = ZDbcIntfs.stUnknown);
      Writable := not ReadOnly;
      CaseSensitive := UpperCase(ColumnName) <> ColumnName; //non quoted fiels are uppercased by default
    end;
  end;
  Open;
end;

procedure TZAbstractFirebirdResultSet.DeRegisterCursor;
begin
  FFBTransaction.DeRegisterOpencursor(IZResultSet(TransactionResultSet));
  FFBTransaction := nil;
end;

function TZAbstractFirebirdResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var
  BlobId: TISC_QUAD;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  with TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    if (sqlind <> nil) and (sqlind^ = ISC_NULL)
    then Result := nil
    else if sqltype = SQL_BLOB then begin
      BlobId := PISC_QUAD(sqldata)^;
      if ColumnType = stBinaryStream
      then Result := TZFirebirdBLob.Create(FFBConnection, BlobId,
        lsmRead, FOpenLobStreams)
      else Result := TZFirebirdClob.Create(FFBConnection, BlobId,
        lsmRead, ColumnCodePage, FOpenLobStreams);
    end else raise CreateCanNotAccessBlobRecordException(ColumnIndex, ColumnType);
  end;
  LastWasNull := Result = nil;
end;

function TZAbstractFirebirdResultSet.GetConnection: IZFirebirdConnection;
begin
  Result := FFBConnection;
end;

procedure TZAbstractFirebirdResultSet.RegisterCursor;
begin
  FFBTransaction := FFBConnection.GetActiveTransaction;
  FFBTransaction.RegisterOpencursor(IZResultSet(TransactionResultSet));
end;

{ TZFirebirdResultSet }

constructor TZFirebirdResultSet.Create(const Statement: IZStatement;
  const SQL: String; MessageMetadata: IMessageMetadata;
  Status: IStatus; DataBuffer: Pointer; ResultSet: PIResultSet);
begin
  inherited Create(Statement, SQL, MessageMetadata, Status, DataBuffer);
  FResultset := ResultSet^;
  FResultSetAddr := ResultSet;
end;

{**
  Moves the cursor to the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
  <code>false</code> if there are no rows in the result set
}
function TZFirebirdResultSet.First: Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchFirst(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        LastRowNo := 0;
        RowNo := 1; //set AfterLast
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchFirst', Self, lcExecute);
    end else begin
      RowNo := 1;
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end else Result := False;
end;

{**
  Indicates whether the cursor is after the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is after the last row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZFirebirdResultSet.IsAfterLast: Boolean;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Result := FResultset.isEof(FStatus)
  end else Result := True;
end;

{**
  Indicates whether the cursor is before the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is before the first row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZFirebirdResultSet.IsBeforeFirst: Boolean;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Result := FResultset.isBof(FStatus)
  end else Result := True;
end;

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZFirebirdResultSet.Last: Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchLast(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if RowNo = 0 then
          RowNo := 1; //else ?? which row do we have now?
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchLast', Self, lcExecute);
    end else begin
      //how to know a rowno now?
    end;
  end else Result := False;
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
function TZFirebirdResultSet.MoveAbsolute(Row: Integer): Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchAbsolute(FStatus, Row, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        RowNo := Row;
        if LastRowNo >= Row then
          LastRowNo := Row -1;
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchAbsolute', Self, lcExecute);
    end else begin
      RowNo := Row;
      if LastRowNo < Row then
        LastRowNo := Row;
    end;
  end else
    Result := False;
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
function TZFirebirdResultSet.MoveRelative(Rows: Integer): Boolean;
var Status: Integer;
begin
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchRelative(FStatus, Rows, FDataBuffer);
    RowNo := RowNo + Rows;
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if LastRowNo >= RowNo then
          LastRowNo := RowNo -1;
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchAbsolute', Self, lcExecute);
    end else begin
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end else
    Result := False;
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
function TZFirebirdResultSet.Next: Boolean;
var Status: Integer;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or (RowNo > LastRowNo ) or ((MaxRows > 0) and (LastRowNo >= MaxRows) or (FResultSetAddr^ = nil)) then
    Exit;
  if not Closed then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchNext(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if LastRowNo < RowNo then
          LastRowNo := RowNo;
        RowNo := RowNo +1; //set AfterLast
        if GetType = rtForwardOnly then begin
          FResultSet.Close(FStatus); //dereister cursor from Txn
          if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
            FFBConnection.HandleError(FStatus, 'IResultSet.close', Self, lcOther);
          FResultSet.release;
          FResultSet := nil;
          if (FFBTransaction <> nil) then
            DeRegisterCursor;
        end;
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchNext', Self, lcExecute);
    end else begin
      RowNo := RowNo +1;
      if LastRowNo < RowNo then
        LastRowNo := RowNo;
    end;
  end else Result := False;
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
function TZFirebirdResultSet.Previous: Boolean;
var Status: Integer;
begin
  if not Closed and (RowNo > 0) then begin
    if FResultSet = nil then begin
      FResultSet := FResultSetAddr^;
      RegisterCursor;
    end;
    Status := FResultSet.fetchPrior(FStatus, FDataBuffer);
    Result := Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_OK{$ELSE}IStatus_RESULT_OK{$ENDIF};
    RowNo := RowNo -1;
    if not Result then begin
      if Status = {$IFDEF WITH_CLASS_CONST}IStatus.RESULT_NO_DATA{$ELSE}IStatus_RESULT_NO_DATA{$ENDIF} then begin
        if LastRowNo < RowNo then
          LastRowNo := RowNo;
      end else
        FFBConnection.HandleError(FStatus, 'IResultSet.fetchPrior', Self, lcExecute);
    end;
  end else Result := False;
end;

procedure TZFirebirdResultSet.ResetCursor;
begin
  inherited;
  if FResultSet <> nil then begin
    FResultSet.close(FStatus);
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
      FFBConnection.HandleError(FStatus, 'IResultSet.close', Self, lcOther);
    FResultSet.release;
    FResultSet := nil;
  end;
  if FFBTransaction <> nil then
    DeRegisterCursor;
end;

{ TZFirebirdOutParamResultSet }

constructor TZFirebirdOutParamResultSet.Create(const Statement: IZStatement;
  const SQL: String; MessageMetadata: IMessageMetadata;
  Status: IStatus; DataBuffer: Pointer);
begin
  inherited Create(Statement, SQL, MessageMetadata, Status, DataBuffer);
  LastRowNo := 1;
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
function TZFirebirdOutParamResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := not Closed and ((Row = 1) or (Row = 0));
  RowNo := Row;
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
function TZFirebirdOutParamResultSet.Next: Boolean;
begin
  Result := not Closed and (RowNo = 0);
  if RowNo = 0 then
    RowNo := 1
  else if RowNo = 1 then
    RowNo := 1
end;

{ TZFirebirdLobStream }

procedure TZFirebirdLobStream.CancelLob;
begin
  if not FReleased then begin
    Assert(Updated);
    Assert(FLobIsOpen);
    try
      FBlob.cancel(FStatus);
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
      FOwnerLob.FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(FOwnerLob);
    finally
      FLobIsOpen := False;
      Updated := False;
      FPosition := 0;
      PInt64(@BlobId)^ := 0;
      PInt64(@FOwnerLob.FBlobId)^ := 0;
      BlobInfo.TotalSize := 0;
    end;
  end;
end;

procedure TZFirebirdLobStream.CloseLob;
begin
  Assert(FLobIsOpen);
  FBlob.close(FStatus);
  if ((Fstatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0) then
    FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.close', Self, lcOther);
  FLobIsOpen := False;
  FPosition := 0;
end;

constructor TZFirebirdLobStream.Create(const OwnerLob: TZFirebirdLob);
begin
  inherited Create(OwnerLob, OwnerLob, OwnerLob.FOpenLobStreams);
  FOwnerLob := OwnerLob;
  BlobId := OwnerLob.FBlobId;
  FPlainDriver := OwnerLob.FPlainDriver;
  FFBTransaction := OwnerLob.FFBConnection.GetActiveTransaction.GetTransaction;
  FFBTransaction.AddRef;
  BlobInfo :=  @FOwnerLob.FBlobInfo;
  FStatus := OwnerLob.FFBConnection.GetStatus;
  FAttachment := OwnerLob.FFBConnection.GetAttachment;
  FAttachment.addRef;
end;

procedure TZFirebirdLobStream.CreateLob;
var WasRegistered: Boolean;
begin
  if FBlob <> nil then
    FBlob.release;
  WasRegistered := Int64(BlobId) <> 0;
  { create blob handle }
  FBlob := FAttachment.createBlob(Fstatus, FFBTransaction, @BlobId, 0, nil);
  if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
    FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
  if not WasRegistered then
    FOwnerLob.FFBConnection.GetActiveTransaction.RegisterOpenUnCachedLob(FOwnerLob);
  FOwnerLob.FBlobId := BlobId; //write back to descriptor
  Updated := True;
  FLobIsOpen := True;
  FOwnerLob.FIsTemporary := True;
  FOwnerLob.FBlobInfoFilled := True;
  BlobInfo.NumSegments := 0;
  BlobInfo.TotalSize := 0;
  BlobInfo.BlobType := Byte(FOwnerLob.IsClob);
  BlobInfo.MaxSegmentSize := High(Word);
end;

destructor TZFirebirdLobStream.Destroy;
begin
  try
    if not FReleased then begin
      if (FBlob <> nil) then begin
        if FLobIsOpen then { close blob handle }
          FBlob.close(FStatus);
        if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
          FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.close', Self, lcOther);
        FBlob.release;
        if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
          FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.release', Self, lcOther);
        FBlob := nil;
      end;
    end;
  finally
    FOwnerLob.FLobStream := nil;
    FOwnerLob.FIsUpdated := Updated;
    FAttachment.release;
    FFBTransaction.release
  end;
  inherited;
end;

procedure TZFirebirdLobStream.FillBlobInfo;
var
  Items: array[0..3] of Byte;
  Results: array[0..99] of AnsiChar;
  pBuf, pBufStart: PAnsiChar;
  Item, ItemVal: Integer;
begin
  Items[0] := isc_info_blob_num_segments;
  Items[1] := isc_info_blob_max_segment;
  Items[2] := isc_info_blob_total_length;
  Items[3] := isc_info_blob_type;

  FBlob.getInfo(FStatus, 4, @Items[0], SizeOf(Results), @Results[0]);
  if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
     FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
  pBufStart := @Results[0];
  pBuf := pBufStart;
  while pBuf - pBufStart <= SizeOf(Results) do
  begin
    Item := Byte(pBuf^);
    if Item = isc_info_end then
      Break;

    Inc(pBuf);
    ItemVal := ReadInterbase6NumberWithInc(FPlainDriver, pBuf);

    case Item of
      isc_info_blob_num_segments:
        FOwnerLob.FBlobInfo.NumSegments := ItemVal;
      isc_info_blob_max_segment:
        FOwnerLob.FBlobInfo.MaxSegmentSize := ItemVal;
      isc_info_blob_total_length:
        FOwnerLob.FBlobInfo.TotalSize := ItemVal;
      isc_info_blob_type:
        FOwnerLob.FBlobInfo.BlobType := ItemVal;
    end;
  end;
  FOwnerLob.FBlobInfoFilled := True;
end;

function TZFirebirdLobStream.GetSize: Int64;
begin
  if Int64(BlobID) = 0 then
    Result := 0
  else begin
    if not FLobIsOpen then
      OpenLob;
    if FReleased
    then Result := 0
    else Result := FOwnerLob.FBlobInfo.TotalSize;
  end;
end;

procedure TZFirebirdLobStream.OpenLob;
begin
  if not FLobIsOpen then begin
    if (Int64(BlobID) <> 0) then begin
      FBlob := FAttachment.openBlob(FStatus, FFBTransaction, @BlobID, 0, nil);
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
      FillBlobInfo;
    end else
      CreateLob;
    FLobIsOpen := True;
  end;
  //isc_blob_gen_bpb2
end;

function TZFirebirdLobStream.Read(var Buffer; Count: Longint): Longint;
var BytesRead, SegLen: Cardinal;
  Status: ISC_STATUS;
var PBuf: PAnsiChar;
begin
  Result := 0;
  if not FReleased then begin
    if FOwnerLob.FLobStreamMode = lsmWrite then
      raise CreateWriteOnlyException;
    if not FLobIsOpen then
      OpenLob;

    PBuf := @Buffer;
    while Count > 0 do begin
      if Count > LongInt(FOwnerLob.FBlobInfo.MaxSegmentSize)
      then SegLen := FOwnerLob.FBlobInfo.MaxSegmentSize
      else SegLen := Word(Count);
      Status := FBlob.getSegment(FStatus, SegLen, PBuf, @BytesRead);
      if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
        FOwnerLob.FFBConnection.HandleError(FStatus, '', Self, lcOther);
      case Status of
        {$IFDEF WITH_CLASS_CONST}
        IStatus.RESULT_OK, IStatus.RESULT_SEGMENT
        {$ELSE}
        IStatus_RESULT_OK, IStatus_RESULT_SEGMENT
        {$ENDIF}: begin
            Inc(Result, Integer(BytesRead));
            Dec(Count, BytesRead);
            Inc(PBuf, BytesRead);
          end;
        {$IFDEF WITH_CLASS_CONST}
        IStatus.RESULT_NO_DATA
        {$ELSE}
        IStatus_RESULT_NO_DATA
        {$ENDIF}: begin
           Inc(Result, Integer(BytesRead));
           Break;
          end
        else FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.getSegment', Self, lcOther);
      end;
    end;
  end;
  FPosition := FPosition + Result;
end;

function TZFirebirdLobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if Origin = soFromEnd then
    Result := FOwnerLob.FBlobInfo.TotalSize - OffSet
  else if Origin = soFromCurrent then
    Result := FPosition + OffSet
  else
    Result := OffSet;
  if (Result <> 0) then begin
    Result := FBlob.seek(FStatus, Origin, Offset);
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
      FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.seek', Self, lcOther);
  end;
  FPosition := Result;
end;

function TZFirebirdLobStream.Write(const Buffer; Count: Longint): Longint;
var
  SegLen: Integer;
  TempBuffer: PAnsiChar;
begin
  Result := 0;
  if FReleased then Exit;
  if FOwnerLob.FLobStreamMode = lsmRead then
    raise EZSQLException.Create(SOperationIsNotAllowed2);
  if (FPosition = 0) and FOwnerLob.FIsTemporary then begin
    if FLobIsOpen and Updated
    then CancelLob
    else CreateLob;
  end;
  if not FLobIsOpen then
    OpenLob;
  { put data to blob }
  TempBuffer := @Buffer;
  while (Count > 0) do begin
    if Count > LongInt(BlobInfo.MaxSegmentSize)
    then SegLen := BlobInfo.MaxSegmentSize
    else SegLen := Count;
    FBlob.putSegment(FStatus, SegLen, TempBuffer);
    if (FStatus.getState and {$IFDEF WITH_CLASS_CONST}IStatus.STATE_ERRORS{$ELSE}IStatus_STATE_ERRORS{$ENDIF}) <> 0 then
      FOwnerLob.FFBConnection.HandleError(FStatus, 'IBlob.putSegment', Self, lcOther);
    Inc(Result, SegLen);
    Inc(TempBuffer, SegLen);
    Dec(Count, SegLen);
  end;
  { in write mode we always have a new LOB }
  BlobInfo.TotalSize := BlobInfo.TotalSize + Result;
  Updated := True;
  FPosition := FPosition + Result;
end;

{ TZFirebirdCachedResultSet }

function TZFirebirdCachedResultSet.CreateLob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var SQLType: TZSQLType;
  FirebirdResultSet: IZFirebirdResultSet;
  FBConnection: IZFirebirdConnection;
  BlobID: TISC_Quad;
  i64: Int64 absolute BlobID;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  if ResultSet.QueryInterface(IZFirebirdResultSet, FirebirdResultSet) = S_OK then begin
    {$IFNDEF GENERIC_INDEX}Dec(ColumnIndex);{$ENDIF}
    SQLType := TZInterbaseFirebirdColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType;
    if (Byte(SQLType) >= Byte(stAsciiStream)) and (Byte(SQLType) <= Byte(stBinaryStream)) then begin
      FBConnection := FirebirdResultSet.GetConnection;
      i64 := 0;
      if (SQLType = stBinaryStream)
      then Result := TZFirebirdBlob.Create(FBConnection, BlobID, LobStreamMode, fOpenLobStreams)
      else Result := TZFirebirdClob.Create(FBConnection, BlobID, LobStreamMode,
        TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage, FOpenLobStreams);
      UpdateLob(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, Result);
    end else raise CreateCanNotAccessBlobRecordException(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, SQLType);
  end;
end;

class function TZFirebirdCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZFirebirdRowAccessor;
end;

{ TZFirebirdRowAccessor }

constructor TZFirebirdRowAccessor.Create(ColumnsInfo: TObjectList;
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
    if Current.ColumnType in [stUnicodeString, stUnicodeStream] then
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-1); // no National streams 4 IB/FB
    if Current.ColumnType in [stBytes, stBinaryStream] then
      Current.ColumnCodePage := zCP_Binary;
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, CachedLobs);
  TempColumns.Free;
end;

{ TZFirebirdClob }

constructor TZFirebirdClob.Create(const Connection: IZFirebirdConnection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, ColumnCodePage, OpenLobStreams);
  FConSettings := Connection.GetConSettings;
end;

{ TZFirebirdBLob }

constructor TZFirebirdBLob.Create(const Connection: IZFirebirdConnection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, zCP_Binary, OpenLobStreams);
end;

{ TZFirebirdLob }

procedure TZFirebirdLob.AfterConstruction;
begin
  FFBConnection.GetActiveTransaction.RegisterOpenUnCachedLob(Self);
  inherited;
end;

procedure TZFirebirdLob.BeforeDestruction;
begin
  FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
  inherited;
end;

procedure TZFirebirdLob.Clear;
begin
  if Int64(FBlobID) <> 0 then try
    if FIsTemporary and (FLobStream <> nil) and FLobStream.FLobIsOpen then begin
      FLobStream.CancelLob;
      FreeAndNil(FLobStream);
    end;
  finally
    FIsTemporary := False;
    PInt64(@FBlobID)^ := 0;
    FIsUpdated := True;
    FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
  end;
end;

function TZFirebirdLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var Lob: TZFirebirdLob;
    ALobID: TISC_QUAD;
    P: Pointer;
    SegmentSize, Count: LongInt;
    ReadStream, WriteStream: TStream;
begin
  PInt64(@ALobID)^ := 0;
  if FColumnCodePage = zCP_Binary
  then Lob := TZFirebirdBLob.Create(FFBConnection, ALobID, lsmWrite, FOpenLobStreams)
  else Lob := TZFirebirdClob.Create(FFBConnection, ALobID, lsmWrite, FColumnCodePage, FOpenLobStreams);
  Result := Lob;
  if LobStreamMode <> lsmWrite then begin
    ReadStream := CreateLobStream(FColumnCodePage, lsmRead);
    Lob.Open(lsmWrite); //create a lob descriptor
    WriteStream := Lob.CreateLobStream(FColumnCodePage, lsmWrite);
    P := nil;
    try
      if FBlobInfo.TotalSize > 0 then begin
        segmentsize := FBlobInfo.MaxSegmentSize;
        GetMem(P, SegmentSize);
        while true do begin
          Count := ReadStream.Read(P^, SegmentSize);
          WriteStream.Write(P^, Count);
          if (Count < SegmentSize) or
            ((Count = SegmentSize) and (FBlobInfo.MaxSegmentSize = FBlobInfo.TotalSize)) then
            Break;
        end;
      end;
    finally
      if P <> nil then
        FreeMem(P);
      FreeAndNil(ReadStream);
      FreeAndNil(WriteStream);
    end;
  end;
  Lob.FLobStreamMode := LobStreamMode;
end;

constructor TZFirebirdLob.Create(const Connection: IZFirebirdConnection;
  const BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(ColumnCodePage, OpenLobStreams);
  Assert(LobStreamMode <> lsmReadWrite);
  FLobStreamMode := LobStreamMode;
  FPlainDriver := Connection.GetPlainDriver;
  FFBConnection := Connection;
  FBlobId := BlobId;
end;

function TZFirebirdLob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  FLobStreamMode := LobStreamMode;
  FLobStream := TZFirebirdLobStream.Create(Self);
  Result := FLobStream;
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;

function TZFirebirdLob.GetBlobId: TISC_QUAD;
begin
  Result := FBlobId;
end;

function TZFirebirdLob.GetConSettings: PZConSettings;
begin
  if FFBConnection <> nil
  then Result := FFBConnection.GetConSettings
  else Result := nil;
end;

function TZFirebirdLob.IsEmpty: Boolean;
begin
  Result := Int64(FBlobId) = 0;
end;

function TZFirebirdLob.Length: Integer;
var Stream: TStream;
begin
  if FReleased or (PInt64(@FBlobID)^ = 0)
  then Result := 0
  else begin
    if not FBlobInfoFilled then begin
      Stream := CreateLobStream(FColumnCodePage, lsmRead);
      if Stream <> nil then
        FLobStream.FillBlobInfo;
    end;
    Result := FBlobInfo.TotalSize
  end;
end;

procedure TZFirebirdLob.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var Imm: IImmediatelyReleasable;
begin
  if (FFBConnection <> nil) and (FFBConnection.GetActiveTransaction <> nil) and
     (FFBConnection.GetActiveTransaction.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and
     (Sender <> imm) then begin
    FFBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
    Imm.ReleaseImmediat(Sender, AError);
    if FlobStream <> nil then begin
      FlobStream.FReleased := True;
      FreeAndNil(FlobStream);
    end;
  end;
  FReleased := true;
end;

initialization
{$ENDIF ZEOS_DISABLE_FIREBIRD}
end.
