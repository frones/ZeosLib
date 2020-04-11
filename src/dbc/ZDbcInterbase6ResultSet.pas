{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6ResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZDbcIntfs, ZDbcResultSet, ZDbcInterbase6, ZPlainFirebirdInterbaseConstants,
  ZPlainFirebirdDriver, ZCompatibility, ZDbcResultSetMetadata, ZMessages,
  ZPlainDriver, ZDbcInterbase6Utils, ZSelectSchema, ZDbcUtils, ZClasses,
  ZDbcCache, ZDbcGenericResolver, ZDbcCachedResultSet;

type
  IZInterbaseResultSet = Interface(IZResultSet)
    ['{1CFF9886-0B1A-47E1-BD52-2D58ABC2B3CF}']
    function GetConnection: IZInterbase6Connection;
  End;

  {** Implements Interbase ResultSet. }
  TZInterbase6XSQLDAResultSet = class(TZAbstractReadOnlyResultSet, IZResultSet,
    IZInterbaseResultSet)
  private
    FStmtHandle: TISC_STMT_HANDLE;
    FStmtHandleAddr: PISC_STMT_HANDLE;
    FXSQLDA: PXSQLDA;
    FIZSQLDA: IZSQLDA;
    FPISC_DB_HANDLE: PISC_DB_HANDLE;
    FPlainDriver: TZInterbasePlainDriver;
    FDialect: Word;
    FStmtType: TZIbSqlStatementType;
    FGUIDProps: TZInterbase6StatementGUIDProps;
    FISC_TR_HANDLE: TISC_TR_HANDLE;
    FIBConnection: IZInterbase6Connection;
    FIBTransaction: IZIBTransaction;
    FIsMetadataResultSet: Boolean;
    procedure RegisterCursor;
    procedure DeRegisterCursor;
    function CreateIBConvertError(ColumnIndex: Integer; DataType: ISC_SHORT): EZIBConvertError;
  protected
    procedure Open; override;
  public
    //EH: this field is a weak resultset reference
    //it may be an address of a cached resultset which owns this instance.
    //this pointer should be registered as open cursor on the TA
    //aim is: if a transaction commit is called the TA checks if
    //all open resultsets a scollable. If so a fetchall will be done by TA.
    //finally the TA can commit the handle (i.e. changing the AutoCommit mode)
    //which would ususally close all pending cursors
    TransactionResultSet: Pointer;
  public //implement IZInterbaseResultSet
    function GetConnection: IZInterbase6Connection;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      StmtHandleAddr: PISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
      CachedBlob: boolean; StmtType: TZIbSqlStatementType);

    procedure AfterClose; override;
    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
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
    procedure GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
    function Next: Boolean; reintroduce;
  end;

  TZInterbase6Lob = class;

  {** EH: implements a sequential Firebird/Interbase large object stream }
  TZInterbaseLobStream = class(TZImmediatelyReleasableLobStream, IImmediatelyReleasable)
  private
    FPlainDriver: TZInterbasePlainDriver;
    FDB_HANDLE: PISC_DB_HANDLE;
    FTransactionHandle: PISC_TR_HANDLE;
    FStatusVector: TARRAY_ISC_STATUS;
    FBlobHandle: TISC_BLOB_HANDLE;
    FLobIsOpen: Boolean;
    FPosition: Integer;
    FOwnerLob: TZInterbase6Lob;
  protected
    procedure FillBlobInfo;
    function GetSize: Int64; override;
  public
    constructor Create(const OwnerLob: TZInterbase6Lob);
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

  { TZInterbase6Lob }

  TZInterbase6Lob = Class(TZAbstractStreamedLob, IZLob, IZBlob,
    IImmediatelyReleasable, IZInterbaseLob)
  private
    FLobStream: TZInterbaseLobStream;
    FPlainDriver: TZInterbasePlainDriver;
    FBlobId: TISC_QUAD;
    FIBConnection: IZInterbase6Connection;
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
    constructor Create(const Connection: IZInterbase6Connection; BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
      const OpenLobStreams: TZSortedList);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  End;

  TZInterbase6Clob = Class(TZInterbase6Lob, IZCLob)
  public
    constructor Create(const Connection: IZInterbase6Connection;
      BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode;
      ColumnCodePage: Word; const OpenLobStreams: TZSortedList);
  End;

  TZInterbase6Blob = Class(TZInterbase6Lob)
  public
    constructor Create(const Connection: IZInterbase6Connection; BlobId: TISC_QUAD;
      LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
  End;

  {** Implements Interbase ResultSetMetadata object. }
  TZInterbaseResultSetMetadata = Class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
    procedure SetColumnPrecisionFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
    procedure SetColumnTypeFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
  public
    function GetCatalogName({%H-}ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetSchemaName({%H-}ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement({%H-}ColumnIndex: Integer): Boolean; override;
  End;

  {** Implements a specialized cached resolver for Interbase/Firebird. }
  TZInterbase6CachedResolver = class(TZGenerateSQLCachedResolver)
  private
    FInsertReturningFields: TStrings;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    destructor Destroy; override;
    function FormCalculateStatement(const RowAccessor: TZRowAccessor;
      const ColumnsLookup: TZIndexPairList): string; override;
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver); override;
  end;

  {** Implements a specialized cached resolver for Firebird version 2.0 and up. }
  TZCachedResolverFirebird2up = class(TZInterbase6CachedResolver)
  public
    procedure FormWhereClause(const SQLWriter: TZSQLStringWriter;
      const OldRowAccessor: TZRowAccessor; var Result: SQLString); override;
  end;

  {**
    Implements Firebird cached ResultSet. This class should be extended
    with database specific logic to form SQL data manipulation statements.
  }
  TZInterbaseCachedResultSet = Class(TZCachedResultset)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    function CreateLob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode): IZBlob{IZLob}; override;
  End;

  TZInterbaseRowAccessor = class(TZRowAccessor)
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; CachedLobs: WordBool); override;
  end;


{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  ZEncoding, ZFastCode, ZSysUtils, ZDbcMetadata, ZDbcLogging, ZVariant,
  ZDbcProperties;

procedure GetPCharFromTextVar(XSQLVAR: PXSQLVAR; out P: PAnsiChar; out Len: NativeUInt); {$IF defined(WITH_INLINE)} inline; {$IFEND}
begin
  if (XSQLVAR.sqltype and not(1) = SQL_TEXT) then begin
    P := XSQLVAR.sqldata;
    Len := GetAbsorbedTrailingSpacesLen(P, XSQLVAR.sqllen);
  end else begin
    P := @PISC_VARYING(XSQLVAR.sqldata).str[0];
    Len := PISC_VARYING(XSQLVAR.sqldata).strlen;
  end;
end;

{ TZInterbase6XSQLDAResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZInterbase6XSQLDAResultSet.Create(const Statement: IZStatement;
  const SQL: string; StmtHandleAddr: PISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
  CachedBlob: Boolean; StmtType: TZIbSqlStatementType);
begin
  inherited Create(Statement, SQL, TZInterbaseResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    XSQLDA.GetConSettings);
  FIZSQLDA := XSQLDA; //localize the interface to avoid automatic free the object
  FXSQLDA := XSQLDA.GetData; // localize buffer for fast access

  FIBConnection := Statement.GetConnection as IZInterbase6Connection;
  FPISC_DB_HANDLE := FIBConnection.GetDBHandle;
  FISC_TR_HANDLE := FIBConnection.GetTrHandle^;
  FPlainDriver := FIBConnection.GetPlainDriver;
  FDialect := FIBConnection.GetDialect;
  FStmtType := StmtType; //required to know how to fetch the columns for ExecProc

  FStmtHandleAddr := StmtHandleAddr;
  FStmtHandle := StmtHandleAddr^;
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;
  FIsMetadataResultSet := (ConSettings.ClientCodePage.ID = CS_NONE) and
    (Statement.GetParameters.Values[DS_Props_IsMetadataResultSet] = 'True');

  Open;
end;

function TZInterbase6XSQLDAResultSet.CreateIBConvertError(
  ColumnIndex: Integer; DataType: ISC_SHORT): EZIBConvertError;
begin
  Result := EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}),
        GetNameSqlType(DataType and not(1))]));
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
procedure TZInterbase6XSQLDAResultSet.AfterClose;
begin
  FreeAndNil(FGUIDProps);
  { Free output allocated memory }
  FXSQLDA := nil;
  FIZSQLDA := nil;
  FStmtHandle := 0; //don't forget!
  inherited AfterClose;
end;

{$IFDEF USE_SYNCOMMONS}
procedure TZInterbase6XSQLDAResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
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
  procedure RaiseConvertError; //no _U/LStrClr in method
  begin
    raise EZIBConvertError.Create(Format(SErrorConvertionField,
      [FIZSQLDA.GetFieldAliasName(C), GetNameSqlType(SQLCode)]));
  end;
  procedure ReadUTF8CLob(const BlobId: TISC_QUAD);
  var Stream: TStream;
    IbStream: TZInterbaseLobStream;
    Clob: IZCLob;
    Buf: PAnsiChar;
    L, R, Size: LongInt;
  begin
    CLob := TZInterbase6Clob.Create(FIBConnection, BlobId, lsmRead, zCP_UTF8, FOpenLobStreams);
    Stream := Clob.GetStream(zCP_UTF8);
    IbStream := Stream as TZInterbaseLobStream;
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
    CLob := TZInterbase6Clob.Create(FIBConnection, BlobId, lsmRead, ColumnCodePage, FOpenLobStreams);
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
    Blob := TZInterbase6Blob.Create(FIBConnection, BlobId, lsmRead, FOpenLobStreams);
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
    with FXSQLDA.sqlvar[C], TZColumnInfo(ColumnsInfo[c]) do
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
                            JSONWriter.WrBase64(sqldata, sqllen, True)
                          else begin
                            JSONWriter.Add('"');
                            if ColumnCodePage = zCP_UTF8
                            then JSONWriter.AddJSONEscape(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen)
                            else WConvert(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, ColumnCodePage);
                            JSONWriter.Add('"');
                          end;
          SQL_TEXT      : if sqlsubtype = CS_BINARY then
                            JSONWriter.WrBase64(sqldata, sqllen, True)
                          else begin
                            JSONWriter.Add('"');
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(sqldata), sqllen);
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
          else          RaiseConvertError;
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

{$IFNDEF NO_ANSISTRING}
function TZInterbase6XSQLDAResultSet.GetAnsiString(
  ColumnIndex: Integer): AnsiString;
var XSQLVAR: PXSQLVAR;
  P: Pointer;
  Len: NativeUint;
  RBS: RawByteString absolute Result;
  procedure FromLob(ColumnIndex: Integer; var Result: AnsiString);
  var Lob: IZBlob;
    P: Pointer;
    Len: NativeUint;
    RBS: RawByteString absolute Result;
  begin
    Lob := GetBlob(ColumnIndex);
    if Lob.IsClob
    then Lob.GetPAnsiChar(ZOSCodePage, RBS, Len)
    else begin
      P := Lob.GetBuffer(FRawTemp, Len);
      ZSetString(PAnsiChar(P), Len, Result);
      FRawTemp := '';
    end;
  end;
label SetFromPChar, jmpA2W2A;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL)
  then LastWasNull := True
  else with TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TEXT      : begin
                        P := XSQLVAR.sqldata;
                        if (ColumnCodePage = CS_BINARY) then begin
                          Len := XSQLVAR.sqllen;
                          goto SetFromPChar;
                        end else begin
                          Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(XSQLVAR.sqldata), XSQLVAR.sqllen);
                          if (ColumnCodePage = ZOSCodePage)
                          then goto SetFromPChar
                          else goto jmpA2W2A;
                        end;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(XSQLVAR.sqldata).str[0];
                        Len := PISC_VARYING(XSQLVAR.sqldata).strlen;
                        if (ColumnCodePage = ZOSCodePage) or (ColumnCodePage = zCP_Binary)
                        then goto SetFromPChar else
jmpA2W2A:                 PRawToRawConvert(P, Len, ColumnCodePage, ZOSCodePage, RBS);
                      end;
      SQL_BLOB:       FromLob(ColumnIndex, Result);
      else  begin
              P := GetPAnsiChar(ColumnIndex, Len);
SetFromPChar: ZSetString(P, Len, Result);
            end;
    end;
  end;
end;
{$ENDIF NO_ANSISTRING}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
procedure TZInterbase6XSQLDAResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var
  XSQLVAR: PXSQLVAR;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := NullBCD
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_BOOLEAN   : ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN(XSQLVAR.sqldata)^ <> 0), 0, Result);
      SQL_BOOLEAN_FB: ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ <> 0), 0, Result);
      SQL_SHORT     : ScaledOrdinal2Bcd(PISC_SHORT(XSQLVAR.sqldata)^, Byte(-XSQLVAR.sqlscale), Result);
      SQL_LONG      : ScaledOrdinal2Bcd(PISC_LONG(XSQLVAR.sqldata)^, Byte(-XSQLVAR.sqlscale), Result);
      SQL_INT64     : ScaledOrdinal2Bcd(PISC_INT64(XSQLVAR.sqldata)^, Byte(-XSQLVAR.sqlscale), Result);
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        LastWasNull := not TryRawToBCD(P, Len, Result, '.');
                      end;
      SQL_D_FLOAT,
      SQL_DOUBLE,
      SQL_FLOAT,
      SQL_TIMESTAMP,
      SQL_TYPE_DATE,
      SQL_TYPE_TIME : Double2BCD(GetDouble(ColumnIndex), Result);
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
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
function TZInterbase6XSQLDAResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var
  BlobId: TISC_QUAD;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Result := nil;
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := nil;
  end else with TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_QUAD,
      SQL_BLOB: begin
          BlobId := PISC_QUAD(XSQLVAR.sqldata)^;
          if ColumnType = stBinaryStream
          then Result := TZInterbase6BLob.Create(FIBConnection, PISC_QUAD(XSQLVAR.sqldata)^,
            lsmRead, FOpenLobStreams)
          else Result := TZInterbase6Clob.Create(FIBConnection, BlobId,
            lsmRead, ColumnCodePage, FOpenLobStreams);
        end
      else raise CreateCanNotAccessBlobRecordException(ColumnIndex, ColumnType);
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
function TZInterbase6XSQLDAResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := False;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(XSQLVAR.sqldata)^ <> 0;
      SQL_FLOAT     : Result := PSingle(XSQLVAR.sqldata)^ <> 0;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(XSQLVAR.sqldata)^ <> 0;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ <> 0;
      SQL_LONG      : Result := PISC_LONG(XSQLVAR.sqldata)^ <> 0;
      SQL_SHORT     : Result := PISC_SHORT(XSQLVAR.sqldata)^ <> 0;
      SQL_QUAD,
      SQL_INT64     : Result := PISC_INT64(XSQLVAR.sqldata)^ <> 0;
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := StrToBoolEx(P, P+Len);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
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
function TZInterbase6XSQLDAResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
var
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := nil;
    Len := 0;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_VARYING: begin
          Result := @PISC_VARYING(XSQLVAR.sqldata).str[0];
          Len := PISC_VARYING(XSQLVAR.sqldata).strlen;
        end;
      SQL_QUAD,
      SQL_BLOB,
      SQL_ARRAY: begin
          FRawTemp := GetBlob(ColumnIndex).GetString;
          Result := Pointer(FRawTemp);
          Len := Length(FRawTemp);
        end;
      else begin
        Result := PByte(XSQLVAR.sqldata);
        Len := XSQLVAR.sqllen;
      end;
    end;
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
procedure TZInterbase6XSQLDAResultSet.GetDate(ColumnIndex: Integer; var Result: TZDate);
var
  Len: NativeUInt;
  P: PAnsiChar;
  SQLCode: SmallInt;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    PInt64(@Result.Year)^ := 0
  end else begin
    LastWasNull := False;
    with XSQLVAR^ do begin
      SQLCode := (sqltype and not(1));
      case SQLCode of
        SQL_TIMESTAMP : begin
                          isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
                            Result.Year, Result.Month, Result.Day);
                          Result.IsNegative := False;
                        end;
        SQL_TYPE_DATE : begin
                          isc_decode_date(PISC_DATE(sqldata)^, Result.Year, Result.Month, Result.Day);
                          Result.IsNegative := False;
                        end;
        SQL_TYPE_TIME : PInt64(@Result.Year)^ := 0;
        SQL_TEXT,
        SQL_VARYING: begin
            GetPCharFromTextVar(XSQLVAR, P, Len);
            LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, Result)
          end;
        else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype)
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
function TZInterbase6XSQLDAResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
  tDT, dDT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := 0
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(XSQLVAR.sqldata)^;
      SQL_LONG      : if XSQLVAR.sqlscale = 0
                      then Result := PISC_LONG(XSQLVAR.sqldata)^
                      else Result := PISC_LONG(XSQLVAR.sqldata)^ / IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_FLOAT     : Result := PSingle(XSQLVAR.sqldata)^;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(XSQLVAR.sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(XSQLVAR.sqldata)^;
      SQL_SHORT     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_SHORT(XSQLVAR.sqldata)^
                      else Result := PISC_SHORT(XSQLVAR.sqldata)^ / IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_INT64     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_INT64(XSQLVAR.sqldata)^
                      else Result := PISC_INT64(XSQLVAR.sqldata)^    / IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        if not TryEncodeDate(TempDate.Year, TempDate.Month, TempDate.Day, dDT) then
                          dDT := 0;
                        if not TryEncodeTime(TempDate.Hour, TempDate.Minute,
                                TempDate.Second, TempDate.Fractions div 10, tDT) then
                          tDT :=0;
                        if dDT < 0
                        then Result := dDT-tDT
                        else Result := dDT+tDT;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := SysUtils.EncodeDate(TempDate.Year,TempDate.Month, TempDate.Day);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := SysUtils.EncodeTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
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
function TZInterbase6XSQLDAResultSet.GetConnection: IZInterbase6Connection;
begin
  Result := FIBConnection;
end;

function TZInterbase6XSQLDAResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  P: PAnsiChar;
  Len: NativeUInt;
  I64: Int64 absolute Result;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := 0
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(XSQLVAR.sqldata)^;
      SQL_LONG      : if XSQLVAR.sqlscale = -4 then
                        I64 := PISC_LONG(XSQLVAR.sqldata)^
                      else if XSQLVAR.sqlscale > -4  then
                        I64 := PISC_LONG(XSQLVAR.sqldata)^ * IBScaleDivisor[-4-XSQLVAR.sqlscale]
                      else
                        I64 := PISC_LONG(XSQLVAR.sqldata)^ div IBScaleDivisor[-4-XSQLVAR.sqlscale];
      SQL_FLOAT     : Result := PSingle(XSQLVAR.sqldata)^;
      SQL_BOOLEAN   : Result := Ord(PISC_BOOLEAN(XSQLVAR.sqldata)^ <> 0);
      SQL_BOOLEAN_FB: Result := Ord(PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ <> 0);
      SQL_SHORT     : if XSQLVAR.sqlscale = -4 then
                        I64 := PISC_SHORT(XSQLVAR.sqldata)^
                      else if XSQLVAR.sqlscale > -4  then
                        I64 := PISC_SHORT(XSQLVAR.sqldata)^ * IBScaleDivisor[(-4-XSQLVAR.sqlscale)]
                      else
                        I64 := PISC_SHORT(XSQLVAR.sqldata)^ div IBScaleDivisor[-4-XSQLVAR.sqlscale];
      SQL_INT64     : if XSQLVAR.sqlscale = -4 then
                        I64 := PISC_INT64(XSQLVAR.sqldata)^
                      else if XSQLVAR.sqlscale > -4  then
                        I64 := PISC_INT64(XSQLVAR.sqldata)^ * IBScaleDivisor[-4-XSQLVAR.sqlscale]
                      else
                        I64 := PISC_INT64(XSQLVAR.sqldata)^ div IBScaleDivisor[-4-XSQLVAR.sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
    end;
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
function TZInterbase6XSQLDAResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  TempDate: TZTimeStamp;
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
  tDT, dDT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := 0
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := PDouble(XSQLVAR.sqldata)^;
      SQL_LONG      : if XSQLVAR.sqlscale = 0
                      then Result := PISC_LONG(XSQLVAR.sqldata)^
                      else Result := PISC_LONG(XSQLVAR.sqldata)^ / IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_FLOAT     : Result := PSingle(XSQLVAR.sqldata)^;
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(XSQLVAR.sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(XSQLVAR.sqldata)^;
      SQL_SHORT     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_SHORT(XSQLVAR.sqldata)^
                      else Result := PISC_SHORT(XSQLVAR.sqldata)^ / IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_INT64     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_INT64(XSQLVAR.sqldata)^
                      else Result := PISC_INT64(XSQLVAR.sqldata)^    / IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        if not TryEncodeDate(TempDate.Year, TempDate.Month, TempDate.Day, dDT) then
                          dDT := 0;
                        if not TryEncodeTime(TempDate.Hour, TempDate.Minute,
                                TempDate.Second, TempDate.Fractions div 10, tDT) then
                          tDT :=0;
                        if dDT < 0
                        then Result := dDT-tDT
                        else Result := dDT+tDT;
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := SysUtils.EncodeDate(TempDate.Year,TempDate.Month, TempDate.Day);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := SysUtils.EncodeTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
    end;
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
procedure TZInterbase6XSQLDAResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
var XSQLVAR: PXSQLVAR;
  P: PAnsiChar;
  Len: NativeUint;
  CP_ID: ISC_SHORT;
label SetFromPChar, Fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    FillChar(Result, SizeOf(TGUID), #0);
  end else begin
    LastWasNull := False;
    CP_ID := XSQLVAR.sqlsubtype and 255;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TEXT      : begin
                        P := XSQLVAR.sqldata;
                        if (CP_ID = CS_BINARY)
                        then Len := XSQLVAR.sqllen
                        else Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(XSQLVAR.sqldata), XSQLVAR.sqllen);
                        goto SetFromPChar;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(XSQLVAR.sqldata).str[0];
                        Len := PISC_VARYING(XSQLVAR.sqldata).strlen;
SetFromPChar:           if (CP_ID = CS_BINARY) and (Len = SizeOf(TGUID))
                        then Move(P^, Result.D1, SizeOf(TGUID))
                        else if (CP_ID <> CS_BINARY) and ((Len = 36) or (Len = 38))
                          then ValidGUIDToBinary(P, @Result.D1)
                          else goto Fail;
                      end;
      else
Fail:   raise Self.CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype)
    end;
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
function TZInterbase6XSQLDAResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := 0;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := Trunc(PDouble(XSQLVAR.sqldata)^);
      SQL_FLOAT     : Result := Trunc(PSingle(XSQLVAR.sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(XSQLVAR.sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(XSQLVAR.sqldata)^;
      SQL_LONG      : if XSQLVAR.sqlscale = 0
                      then Result := PISC_LONG(XSQLVAR.sqldata)^
                      else Result := PISC_LONG(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_SHORT     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_SHORT(XSQLVAR.sqldata)^
                      else Result := PISC_SHORT(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_QUAD,
      SQL_INT64     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_INT64(XSQLVAR.sqldata)^
                      else Result := PISC_INT64(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := RawToIntDef(P, P+Len, 0);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
    end;
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
function TZInterbase6XSQLDAResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := 0;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_DOUBLE    : Result := Trunc(PDouble(XSQLVAR.sqldata)^);
      SQL_D_FLOAT,
      SQL_FLOAT     : Result := Trunc(PSingle(XSQLVAR.sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(XSQLVAR.sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(XSQLVAR.sqldata)^;
      SQL_LONG      : if XSQLVAR.sqlscale = 0
                      then Result := PISC_LONG(XSQLVAR.sqldata)^
                      else Result := PISC_LONG(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_SHORT     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_SHORT(XSQLVAR.sqldata)^
                      else Result := PISC_SHORT(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_QUAD,
      SQL_INT64     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_INT64(XSQLVAR.sqldata)^
                      else Result := PISC_INT64(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := RawToInt64Def(P, P+Len, 0);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
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
procedure TZInterbase6XSQLDAResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
Fill: PCardinal(@Result.Hour)^ := 0;
    PInt64(@Result.Second)^ := 0;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TIMESTAMP : begin
            isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
              Result.Hour, Result.Minute, Result.Second, Result.Fractions);
            Result.Fractions := Result.Fractions * 100000;
            Result.IsNegative := False;
          end;
      SQL_TYPE_DATE : goto Fill;
      SQL_TYPE_TIME : begin
            isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
              Result.Hour, Result.Minute, Result.Second, Result.Fractions);
            Result.Fractions := Result.Fractions * 100000;
            Result.IsNegative := False;
          end;
      SQL_TEXT, SQL_VARYING: begin
          GetPCharFromTextVar(XSQLVAR, P, Len);
          LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, Result);
        end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype)
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
procedure TZInterbase6XSQLDAResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    FillChar(Result, SizeOf(TZTimeStamp), #0);
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TIMESTAMP :
        begin
          isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
            Result.Year, Result.Month, Result.Day);
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
            Result.Hour, Result.Minute, Result.Second, Result.Fractions);
          Result.Fractions := Result.Fractions * 100000;
          Result.IsNegative := False;
        end;
      SQL_TYPE_DATE :
        begin
          PInt64(@Result.Hour)^ := 0;
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
            Result.Year, Result.Month, Result.Day);
          Result.IsNegative := False;
        end;
      SQL_TYPE_TIME :
        begin
          PInt64(@Result.Year)^ := 0;
          PInt64(PAnsiChar(@Result.TimeZoneHour)-2)^ := 0;
          isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
            Result.Hour, Result.Minute, Result.Second, Result.Fractions);
          Result.Fractions := Result.Fractions * 100000;
          Result.IsNegative := False;
        end;
      SQL_TEXT, SQL_VARYING:
        begin
          GetPCharFromTextVar(XSQLVAR, P, Len);
          LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings, Result);
        end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype)
    end;
  end;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZInterbase6XSQLDAResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  {$IFNDEF DISABLE_CHECKING}
  Assert((ColumnIndex >= 0) and (ColumnIndex <= FXSQLDA.sqln), 'Index out of Range.');
  {$ENDIF}
  {$R-}
  with FXSQLDA.sqlvar[ColumnIndex] do
    Result := (sqlind <> nil) and (sqlind^ = ISC_NULL);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZAnsiRec</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZInterbase6XSQLDAResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  TempDate: TZTimeStamp;
  XSQLVAR: PXSQLVAR;
  function GetLobBufAndLen(ColumnIndex: Integer; out Len: NativeUInt): Pointer;
  var BlobTemp: IZBlob;
  begin
    BlobTemp := GetBlob(ColumnIndex);
    Result := BlobTemp.GetBuffer(fRawTemp, Len);
  end;
  label set_Results;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Len := 0;
    Result := nil;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : begin
                        Len := FloatToSQLRaw(PDouble(XSQLVAR.sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_LONG      : if XSQLVAR.sqlscale = 0 then begin
                        IntToRaw(PISC_LONG(XSQLVAR.sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(PISC_LONG(XSQLVAR.sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result, Byte(-XSQLVAR.sqlscale));
                        goto set_Results;
                      end;
      SQL_FLOAT     : begin
                        Len := FloatToSQLRaw(PSingle(XSQLVAR.sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_BOOLEAN   : if PISC_BOOLEAN(XSQLVAR.sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      SQL_BOOLEAN_FB: if PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      SQL_SHORT     : if XSQLVAR.sqlscale = 0 then begin
                        IntToRaw(Integer(PISC_SHORT(XSQLVAR.sqldata)^), PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(Integer(PISC_SHORT(XSQLVAR.sqldata)^), PAnsiChar(@FTinyBuffer[0]), @Result, Byte(-XSQLVAR.sqlscale));
                        goto set_Results;
                      end;
      SQL_QUAD,
      SQL_INT64     : if XSQLVAR.sqlscale = 0 then begin
                        IntToRaw(PISC_INT64(XSQLVAR.sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Raw(PISC_INT64(XSQLVAR.sqldata)^, PAnsiChar(@FTinyBuffer[0]), @Result, Byte(-XSQLVAR.sqlscale));
set_Results:            Len := Result - PAnsiChar(@FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_TEXT,
      SQL_VARYING   : GetPCharFromTextVar(XSQLVAR, Result, Len);
      SQL_BLOB      : Result := GetLobBufAndLen(ColumnIndex, Len);
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := DateTimeToRaw(TempDate.Year, TempDate.Month,
                          TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := @FTinyBuffer[0];
                        Len := DateToRaw(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := TimeToRaw(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of UCS2 string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
var
  TempDate: TZTimeStamp;
  XSQLVAR: PXSQLVAR;
  P: PAnsiChar;
  label set_Results;
  function GetLobBufAndLen(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
  var BlobTemp: IZBlob;
  begin
    BlobTemp := GetBlob(ColumnIndex, lsmRead);
    if BlobTemp.IsClob then begin
      Result := BlobTemp.GetPWideChar(FUniTemp, Len);
    end else begin
      Result := BlobTemp.GetBuffer(FRawTemp, Len);
      FUniTemp := Ascii7ToUnicodeString(Pointer(Result), Len);
      FRawTemp := '';
      Result := Pointer(FUniTemp);
      Len := Length(FUniTemp);
    end;
  end;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Len := 0;
    Result := nil;
  end else with TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : begin
                        Len := FloatToSQLUnicode(PDouble(XSQLVAR.sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_FLOAT     : begin
                        Len := FloatToSQLUnicode(PSingle(XSQLVAR.sqldata)^, @FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_BOOLEAN   : if PISC_BOOLEAN(XSQLVAR.sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      SQL_BOOLEAN_FB: if PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ <> 0 then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      SQL_SHORT     : if XSQLVAR.sqlscale = 0 then begin
                        IntToUnicode(Integer(PISC_SHORT(XSQLVAR.sqldata)^), PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(Integer(PISC_SHORT(XSQLVAR.sqldata)^), PWideChar(@FTinyBuffer[0]), @Result, Byte(-XSQLVAR.sqlscale));
                        goto set_Results;
                      end;
      SQL_LONG      : if XSQLVAR.sqlscale = 0 then begin
                        IntToUnicode(PISC_LONG(XSQLVAR.sqldata)^, PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(PISC_LONG(XSQLVAR.sqldata)^, PWideChar(@FTinyBuffer[0]), @Result, Byte(-XSQLVAR.sqlscale));
                        goto set_Results;
                      end;
      SQL_QUAD,
      SQL_INT64     : if XSQLVAR.sqlscale = 0 then begin
                        IntToUnicode(PISC_INT64(XSQLVAR.sqldata)^, PWideChar(@FTinyBuffer[0]), @Result);
                        goto set_Results;
                      end else begin
                        ScaledOrdinal2Unicode(PISC_INT64(XSQLVAR.sqldata)^, PWideChar(@FTinyBuffer[0]), @Result, Byte(-XSQLVAR.sqlscale));
set_Results:            Len := Result - PWideChar(@FTinyBuffer[0]);
                        Result := @FTinyBuffer[0];
                      end;
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
                        if ColumnCodePage = CS_BINARY
                        then fUniTemp := Ascii7ToUnicodeString(P, Len)
                        else fUniTemp := PRawToUnicode(P, Len, ColumnCodePage);
                        Len := Length(fUniTemp);
                        if Len <> 0
                        then Result := Pointer(fUniTemp)
                        else Result := PEmptyUnicodeString;
                      end;
      SQL_BLOB      : Result := GetLobBufAndLen(ColumnIndex, Len);
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := DateTimeToUni(TempDate.Year,
                          TempDate.Month, TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 10000,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := @FTinyBuffer[0];
                        Len := DateToUni(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := TimeToUni(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions * 100000,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZInterbase6XSQLDAResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := 0;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Result := Trunc(PDouble(XSQLVAR.sqldata)^);
      SQL_FLOAT     : Result := Trunc(PSingle(XSQLVAR.sqldata)^);
      SQL_BOOLEAN   : Result := PISC_BOOLEAN(XSQLVAR.sqldata)^;
      SQL_BOOLEAN_FB: Result := PISC_BOOLEAN_FB(XSQLVAR.sqldata)^;
      SQL_LONG      : if XSQLVAR.sqlscale = 0
                      then Result := PISC_LONG(XSQLVAR.sqldata)^
                      else Result := PISC_LONG(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_SHORT     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_SHORT(XSQLVAR.sqldata)^
                      else Result := PISC_SHORT(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_QUAD,
      SQL_INT64     : if XSQLVAR.sqlscale = 0
                      then Result := PISC_INT64(XSQLVAR.sqldata)^
                      else Result := PISC_INT64(XSQLVAR.sqldata)^ div IBScaleDivisor[XSQLVAR.sqlscale];
      SQL_BLOB,
      SQL_TEXT,
      SQL_VARYING   : begin
                        P := GetPAnsiChar(ColumnIndex, Len);
                        Result := RawToUInt64Def(P, P+Len, 0);
                      end;
      else raise CreateIBConvertError(ColumnIndex, XSQLVAR.sqltype);
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{$IFNDEF NO_UTF8STRING}
function TZInterbase6XSQLDAResultSet.GetUTF8String(
  ColumnIndex: Integer): UTF8String;
var XSQLVAR: PXSQLVAR;
  P: Pointer;
  Len: NativeUint;
  RBS: RawByteString absolute Result;
  procedure FromLob(ColumnIndex: Integer; var Result: UTF8String);
  var Lob: IZBlob;
    P: Pointer;
    Len: NativeUint;
    RBS: RawByteString absolute Result;
  begin
    Lob := GetBlob(ColumnIndex);
    if Lob.IsClob
    then Lob.GetPAnsiChar(zCP_UTF8, RBS, Len)
    else begin
      P := Lob.GetBuffer(FRawTemp, Len);
      ZSetString(PAnsiChar(P), Len, Result);
      FRawTemp := '';
    end;
  end;
label SetFromPChar, jmpA2W2A;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL)
  then  LastWasNull := True
  else with TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_TEXT      : begin
                        P := XSQLVAR.sqldata;
                        if (ColumnCodePage = CS_BINARY) then begin
                          Len := XSQLVAR.sqllen;
                          goto SetFromPChar;
                        end else begin
                          Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(XSQLVAR.sqldata), XSQLVAR.sqllen);
                          if (ColumnCodePage = zCP_UTF8)
                          then goto SetFromPChar
                          else goto jmpA2W2A;
                        end;
                      end;
      SQL_VARYING   : begin
                        P := @PISC_VARYING(XSQLVAR.sqldata).str[0];
                        Len := PISC_VARYING(XSQLVAR.sqldata).strlen;
                        if (ColumnCodePage = zCP_UTF8) or (ColumnCodePage = zCP_Binary)
                        then goto SetFromPChar else
jmpA2W2A:                 PRawToRawConvert(P, Len, ColumnCodePage, zCP_UTF8, RBS);
                      end;
      SQL_BLOB:       FromLob(ColumnIndex, Result);
      else  begin
              P := GetPAnsiChar(Columnindex, Len);
SetFromPChar: ZSetString(P, Len, Result);
            end;
    end;
  end;
end;
{$ENDIF NO_UTF8STRING}

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
function TZInterbase6XSQLDAResultSet.Next: Boolean;
var
  StatusVector: TARRAY_ISC_STATUS;
  Status: ISC_STATUS;
label CheckE;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or (RowNo > LastRowNo ) or ((MaxRows > 0) and (LastRowNo >= MaxRows) or (FStmtHandleAddr^ = 0)) then
    Exit;

  { Fetch row. }
  if (FStmtType <> stExecProc) then begin //AVZ - Test for ExecProc - this is for multiple rows
    if (RowNo = 0) then
      FStmtHandle := FStmtHandleAddr^;
    Status := FPlainDriver.isc_dsql_fetch(@StatusVector,
      @FStmtHandle, FDialect, FXSQLDA);
    if Status = 0 then begin
      if (RowNo = 0) then RegisterCursor;
      RowNo := RowNo + 1;
      LastRowNo := RowNo;
      Result := True;
    end else if Status = 100  then begin
      {no error occoured -> notify IsAfterLast and close the recordset}
      RowNo := RowNo + 1;
      if FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0 then
        goto CheckE;
      FStmtHandle := 0;
      if (FIBTransaction <> nil) then
        DeRegisterCursor;
    end else
CheckE:CheckInterbase6Error(FPlainDriver, StatusVector, Self);
  end else if RowNo = 0 then begin
    Result := True;
    RowNo := 1;
    LastRowNo := 1;
  end else if RowNo = 1 then
    RowNo := 2; //notify AfterLast
end;

{**
  Opens this recordset.
}
procedure TZInterbase6XSQLDAResultSet.Open;
var
  I: Word;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
  ZCodePageInfo: PZCodePage;
  CPID: Word;
  XSQLVAR: PXSQLVAR;
label jmpLen;
begin
  if FStmtHandle=0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  FGUIDProps := TZInterbase6StatementGUIDProps.Create(Statement);

  ColumnsInfo.Clear;
  if FXSQLDA.sqld > 0 then  //keep track we have a column to avoid range issues see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=10595
    for I := 0 to FXSQLDA.sqld {FieldCount} - 1 do begin
      {$R-}
      XSQLVAR := @FXSQLDA.sqlvar[i];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
      ColumnInfo := TZColumnInfo.Create;
      with ColumnInfo do begin
        TableName := FIZSQLDA.GetFieldRelationName(I);
        if TableName <> '' then
          ColumnName := FIZSQLDA.GetFieldSqlName(I);
        ColumnLabel := FIZSQLDA.GetFieldAliasName(I);
        FieldSqlType := FIZSQLDA.GetFieldSqlType(I);
        if FGUIDProps.ColumnIsGUID(FieldSqlType, XSQLVAR.sqllen, ColumnName) then
          FieldSqlType := stGUID;
        ColumnType := FieldSqlType;

        case FieldSqlType of
          stString, stUnicodeString: begin
              //see test Bug#886194, we retrieve 565 as CP... the modula get returns the FBID of CP
              CPID := XSQLVAR.sqlsubtype and 255;
              //see: http://sourceforge.net/p/zeoslib/tickets/97/
              if (CPID = ConSettings^.ClientCodePage^.ID)
              then ZCodePageInfo := ConSettings^.ClientCodePage
              else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CPID); //get column CodePage info}
              ColumnCodePage := ZCodePageInfo.CP;
              if ConSettings^.ClientCodePage^.ID = CS_NONE then begin
jmpLen:         Precision := XSQLVAR.sqllen;
                CharOctedLength := Precision;
              end else begin
                CharOctedLength := XSQLVAR.sqllen;
                Precision := XSQLVAR.sqllen div ZCodePageInfo^.CharWidth;
              end;
              Signed := XSQLVAR.sqltype and not 1 = SQL_TEXT;
            end;
          stAsciiStream, stUnicodeStream: if ConSettings^.ClientCodePage^.ID = CS_NONE
            then if FIsMetadataResultSet
              then ColumnCodePage := zCP_UTF8
              else begin //connected with CS_NONE no transliterions are made by FB
                CPID := FIBConnection.GetSubTypeTextCharSetID(TableName,ColumnName);
                if CPID = CS_NONE
                then ZCodePageInfo := ConSettings^.ClientCodePage
                else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CPID);
                ColumnCodePage := ZCodePageInfo.CP;
              end else ColumnCodePage := ConSettings^.ClientCodePage^.CP;
          stBytes: begin
              ColumnCodePage := zCP_Binary;
              goto jmpLen;
            end;
          stBinaryStream: ColumnCodePage := zCP_Binary;
          else begin
            ColumnCodePage := zCP_NONE;
            case FieldSqlType of
              stShort, stSmall, stInteger, stLong: Signed := True;
              stCurrency, stBigDecimal: begin
                Signed  := True;
                Scale   := -XSQLVAR.sqlscale;
                //first digit does not count because of overflow (FB does not allow this)
                case XSQLVAR.sqltype and not (1) of
                  SQL_SHORT:  Precision := 4;
                  SQL_LONG:   Precision := 9;
                  SQL_INT64:  Precision := 18;
                end;
              end;
              stTime, stTimeStamp: Scale := {-}4; //fb supports 10s of milli second fractions
            end;
          end;
        end;
        ReadOnly := (TableName = '') or (ColumnName = '') or
          (ColumnName = 'RDB$DB_KEY') or (FieldSqlType = ZDbcIntfs.stUnknown);
        Writable := not ReadOnly;
        Nullable := TZColumnNullableType(Ord(FIZSQLDA.IsNullable(I)));
        Scale := FIZSQLDA.GetFieldScale(I);
        CaseSensitive := UpperCase(ColumnName) <> ColumnName; //non quoted fiels are uppercased by default
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;
  inherited Open;
end;

procedure TZInterbase6XSQLDAResultSet.RegisterCursor;
begin
  FIBTransaction := FIBConnection.GetActiveTransaction;
  FIBTransaction.RegisterOpencursor(IZResultSet(TransactionResultSet));
end;

procedure TZInterbase6XSQLDAResultSet.ResetCursor;
var StatusVector: TARRAY_ISC_STATUS;
begin
  if not Closed then begin
    if (FStmtHandle <> 0) then begin
      if (FStmtType <> stExecProc) then begin
         if (FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0) then
          CheckInterbase6Error(FPlainDriver, StatusVector, Self, lcOther, 'isc_dsql_free_statement');
        FStmtHandle := 0;
        if FIBTransaction <> nil then
          DeRegisterCursor;
      end else
        FStmtHandle := 0;
    end;
    inherited ResetCursor;
  end;
end;

procedure TZInterbase6XSQLDAResultSet.DeRegisterCursor;
begin
  FIBTransaction.DeRegisterOpencursor(IZResultSet(TransactionResultSet));
  FIBTransaction := nil;
end;

{ TZInterbaseResultSetMetadata }

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZInterbaseResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
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
function TZInterbaseResultSetMetadata.GetCatalogName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZInterbaseResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnName;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZInterbaseResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZInterbaseResultSetMetadata.GetTableName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbaseResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := False; //not supported by FB/IB
end;

{**
  Initializes columns with additional data.
}
procedure TZInterbaseResultSetMetadata.LoadColumns;
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

procedure TZInterbaseResultSetMetadata.SetColumnPrecisionFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then
    ColumnInfo.Precision := TableColumns.GetInt(TableColColumnSizeIndex);
end;

procedure TZInterbaseResultSetMetadata.SetColumnTypeFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
var Precision: Integer;
begin
  //FB native ResultSet can't give use users choosen precision for the Numeric/Decimal Fields
  //so a ISC_INT64 type with scale smaller then four will always be
  //mapped to stBigDecimal while it could be a stCurrency type. Let's test it!
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then begin
    Precision := TableColumns.GetInt(TableColColumnSizeIndex);
    if (ColumnInfo.ColumnType = stBigDecimal) and (ColumnInfo.Scale <= 4) and
       (Precision < sAlignCurrencyScale2Precision[ColumnInfo.Scale]) then
      ColumnInfo.ColumnType := stCurrency;
  end else
    inherited SetColumnTypeFromGetColumnsRS(ColumnInfo, TableColumns);
end;

{ TZInterbase6CachedResolver }

constructor TZInterbase6CachedResolver.Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  Fields: string;
begin
  inherited;
  Fields := Statement.GetParameters.Values[DSProps_InsertReturningFields];
  if Fields <> '' then
    FInsertReturningFields := ExtractFields(Fields, [';', ',']);
end;

destructor TZInterbase6CachedResolver.Destroy;
begin
  inherited;
  FreeAndNil(FInsertReturningFields);
end;

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZInterbase6CachedResolver.FormCalculateStatement(
  const RowAccessor: TZRowAccessor; const ColumnsLookup: TZIndexPairList): string;
// --> ms, 30/10/2005
var
   iPos: Integer;
begin
  Result := inherited FormCalculateStatement(RowAccessor, ColumnsLookup);
  if Result <> '' then begin
    iPos := ZFastCode.pos('FROM', uppercase(Result));
    if iPos > 0
    then Result := copy(Result, 1, iPos+3) + ' RDB$DATABASE'
    else Result := Result + ' FROM RDB$DATABASE';
  end;
// <-- ms
end;

procedure TZInterbase6CachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) then
    UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
end;

procedure TZInterbase6CachedResolver.UpdateAutoIncrementFields(
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; const
  OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver);
var
  I, ColumnIdx: Integer;
  RS: IZResultSet;
begin
  //inherited;

  RS := InsertStatement.GetResultSet;
  if RS = nil then
    Exit;

  for I := 0 to FInsertReturningFields.Count - 1 do
  begin
    ColumnIdx := Metadata.FindColumn(FInsertReturningFields[I]);
    if ColumnIdx = InvalidDbcIndex then
      raise EZSQLException.Create(Format(SColumnWasNotFound, [FInsertReturningFields[I]]));
    NewRowAccessor.SetValue(ColumnIdx, RS.GetValueByName(FInsertReturningFields[I]));
  end;

  RS.Close; { Without Close RS keeps circular ref to Statement causing mem leak }
end;

{ TZCachedResolverFirebird2up }

procedure TZCachedResolverFirebird2up.FormWhereClause(
  const SQLWriter: TZSQLStringWriter; const OldRowAccessor: TZRowAccessor;
  var Result: SQLString);
var
  I, idx: Integer;
  Tmp, S: SQLString;
begin
  if WhereColumns.Count > 0 then
    SQLWriter.AddText(' WHERE ', Result);
  for I := 0 to WhereColumns.Count - 1 do begin
    idx := PZIndexPair(WhereColumns[I]).ColumnIndex;
    if I > 0 then
      SQLWriter.AddText(' AND ', Result);
    S := MetaData.GetColumnName(idx);
    Tmp := IdentifierConvertor.Quote(S);
    SQLWriter.AddText(Tmp, Result);
    if (Metadata.IsNullable(Idx) = ntNullable)
    then SQLWriter.AddText(' IS NOT DISTINCT FROM ?', Result)
    else SQLWriter.AddText('=?', Result);
  end;
end;

{ TZInterbaseLobStream }

procedure TZInterbaseLobStream.CancelLob;
begin
  if not FReleased then begin
    Assert(Updated);
    Assert(FLobIsOpen);
    try
      if FPlainDriver.isc_cancel_blob(@FStatusVector, @FBlobHandle) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
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

procedure TZInterbaseLobStream.CloseLob;
begin
  Assert(FLobIsOpen);
  if FPlainDriver.isc_close_blob(@FStatusVector, @FBlobHandle) <> 0 then
    CheckInterbase6Error(FPlainDriver, FStatusVector, FOwner);
  FLobIsOpen := False;
  FPosition := 0;
end;

constructor TZInterbaseLobStream.Create(const OwnerLob: TZInterbase6Lob);
begin
  inherited Create(OwnerLob, OwnerLob, OwnerLob.FOpenLobStreams);
  FOwnerLob := OwnerLob;
  BlobId := OwnerLob.FBlobId;
  FPlainDriver := OwnerLob.FPlainDriver;
  FDB_HANDLE := OwnerLob.FIBConnection.GetDBHandle;
  FTransactionHandle := OwnerLob.FIBConnection.GetTrHandle;
  BlobInfo :=  @FOwnerLob.FBlobInfo;
end;

procedure TZInterbaseLobStream.CreateLob;
begin
  { create blob handle }
  if FPlainDriver.isc_create_blob2(@FStatusVector, FDB_HANDLE, FTransactionHandle,
     @FBlobHandle, @BlobId, 0, nil) <> 0 then //EH: what about BPB
    CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
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

destructor TZInterbaseLobStream.Destroy;
begin
  try
    if not FReleased and FLobIsOpen then
  {    close blob handle }
      if FPlainDriver.isc_close_blob(@FStatusVector, @FBlobHandle) <> 0 then
        CheckInterbase6Error(FPlainDriver, FStatusVector, FOwner);
  finally
    FOwnerLob.FLobStream := nil;
    FOwnerLob.FIsUpdated := Updated;
  end;
  inherited;
end;

procedure TZInterbaseLobStream.FillBlobInfo;
var
  Items: array[0..3] of Byte;
  Results: array[0..99] of AnsiChar;
  pBuf, pBufStart: PAnsiChar;
  Item, ItemVal: Integer;
  StatusVector: TARRAY_ISC_STATUS;
begin
  Items[0] := isc_info_blob_num_segments;
  Items[1] := isc_info_blob_max_segment;
  Items[2] := isc_info_blob_total_length;
  Items[3] := isc_info_blob_type;

  if FPlainDriver.isc_blob_info(@StatusVector, @FBlobHandle, 4, @items[0],
      SizeOf(Results), @Results[0]) <> 0 then
    CheckInterbase6Error(FPlainDriver, StatusVector, Self);
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

function TZInterbaseLobStream.GetSize: Int64;
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

procedure TZInterbaseLobStream.OpenLob;
begin
  if not FLobIsOpen then begin
    if (Int64(BlobID) <> 0) then begin
       if FPlainDriver.isc_open_blob2(@FStatusVector, FDB_HANDLE,
       FTransactionHandle, @FBlobHandle, @BlobId, 0 , nil) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
      FillBlobInfo;
    end else
      CreateLob;
    FLobIsOpen := True;
  end;
  //isc_blob_gen_bpb2
end;

function TZInterbaseLobStream.Read(var Buffer; Count: Longint): Longint;
var BytesRead, SegLen: ISC_USHORT;
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
      Status := FPlainDriver.isc_get_segment(@FStatusVector, @FBlobHandle,
             @BytesRead, SegLen, PBuf);
      case Status of
        0, isc_segment: begin
            Inc(Result, BytesRead);
            Dec(Count, BytesRead);
            Inc(PBuf, BytesRead);
          end;
        isc_segstr_eof: begin
           Inc(Result, BytesRead);
           Break;
          end
        else CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
      end;
    end;
  end;
  FPosition := FPosition + Result;
end;

function TZInterbaseLobStream.Seek(Offset: Longint; Origin: Word): Longint;
var P: Pointer;
begin
  if Origin = soFromEnd then
    Result := FOwnerLob.FBlobInfo.TotalSize - OffSet
  else if Origin = soFromCurrent then
    Result := FPosition + OffSet
  else begin
    Result := OffSet;
    if (Result = 0) and (FPosition > 0) then //seek to bos ?
      CloseLob;
  end;
  if FPosition > Result //backward seeking is not supported
  then raise EZSQLException.Create(SOperationIsNotAllowed1)
  else if Result > FPosition then //seek forward?
    if FOwnerLob.FLobStreamMode = lsmRead then begin //allowed on reading mode only
      GetMem(P, FOwnerLob.FBlobInfo.MaxSegmentSize);
      try
        while Result > FPosition do begin
          OffSet := Result - FPosition;
          if OffSet > LongInt(FOwnerLob.FBlobInfo.MaxSegmentSize)
          then Origin := FOwnerLob.FBlobInfo.MaxSegmentSize
          else Origin := Word(OffSet);
          Inc(FPosition, Read(P^, Origin));
        end;
      finally
        FreeMem(P);
      end;
    end
  else raise CreateWriteOnlyException;
  FPosition := Result;
end;

function TZInterbaseLobStream.Write(const Buffer; Count: Longint): Longint;
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
    if FPlainDriver.isc_put_segment(@FStatusVector, @FBlobHandle, SegLen, TempBuffer) <> 0 then
      CheckInterbase6Error(FPlainDriver, FStatusVector, Self);
    Inc(Result, SegLen);
    Inc(TempBuffer, SegLen);
    Dec(Count, SegLen);
  end;
  { in write mode we always have a new LOB }
  BlobInfo.TotalSize := BlobInfo.TotalSize + Result;
  Updated := True;
  FPosition := FPosition + Result;
end;

{ TZInterbaseCachedResultSet }

function TZInterbaseCachedResultSet.CreateLob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var SQLType: TZSQLType;
  InterbaseResultSet: IZInterbaseResultSet;
  IBConnection: IZInterbase6Connection;
var BlobID: TISC_Quad;
  i64: Int64 absolute BlobID;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  if ResultSet.QueryInterface(IZInterbaseResultSet, InterbaseResultSet) = S_OK then begin
    {$IFNDEF GENERIC_INDEX}Dec(ColumnIndex);{$ENDIF}
    SQLType := TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType;
    if (Byte(SQLType) >= Byte(stAsciiStream)) and (Byte(SQLType) <= Byte(stBinaryStream)) then begin
      IBConnection := InterbaseResultSet.GetConnection;
      i64 := 0;
      if (SQLType = stBinaryStream)
      then Result := TZInterbase6Blob.Create(IBConnection, BlobID, LobStreamMode, fOpenLobStreams)
      else Result := TZInterbase6Clob.Create(IBConnection, BlobID, LobStreamMode,
        TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage, FOpenLobStreams);
      UpdateLob(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, Result);
    end else raise CreateCanNotAccessBlobRecordException(ColumnIndex{$IFNDEF GENERIC_INDEX} + 1{$ENDIF}, SQLType);
  end;
end;

class function TZInterbaseCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZInterbaseRowAccessor;
end;

{ TZInterbase6Lob }

procedure TZInterbase6Lob.AfterConstruction;
begin
  FIBConnection.GetActiveTransaction.RegisterOpenUnCachedLob(Self);
  inherited;
end;

procedure TZInterbase6Lob.BeforeDestruction;
begin
  if (FLobStream <> nil) then begin
    FIBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
    //FLobStream.FTransaction.DeRegisterOpenUnCachedLob(Self);
    if FLobStream.FLobIsOpen then
      FLobStream.CloseLob;
    FreeAndNil(FLobStream);
  end;
  inherited;
end;

procedure TZInterbase6Lob.Clear;
begin
  if PInt64(@FBlobID)^ <> 0 then try
    if FIsTemporary and (FLobStream <> nil) and FLobStream.FLobIsOpen then begin
      FLobStream.CancelLob;
      FreeAndNil(FLobStream);
    end;
  finally
    FIsTemporary := False;
    PInt64(@FBlobID)^ := 0;
    FIsUpdated := True;
  end;
end;

function TZInterbase6Lob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var Lob: TZInterbase6Lob;
    ALobID: TISC_QUAD;
    P: Pointer;
    SegmentSize, Count: LongInt;
    ReadStream, WriteStream: TStream;
begin
  PInt64(@ALobID)^ := 0;
  if FColumnCodePage = zCP_Binary
  then Lob := TZInterbase6BLob.Create(FIBConnection, ALobID, lsmWrite, FOpenLobStreams)
  else Lob := TZInterbase6Clob.Create(FIBConnection, ALobID, lsmWrite, FColumnCodePage, FOpenLobStreams);
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

constructor TZInterbase6Lob.Create(const Connection: IZInterbase6Connection; BlobId: TISC_QUAD;
  LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(ColumnCodePage, OpenLobStreams);
  Assert(LobStreamMode <> lsmReadWrite);
  FLobStreamMode := LobStreamMode;
  FPlainDriver := Connection.GetPlainDriver;
  FIBConnection := Connection;
  FBlobId := BlobId;
end;

function TZInterbase6Lob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  FLobStreamMode := LobStreamMode;
  FLobStream := TZInterbaseLobStream.Create(Self);
  Result := FLobStream;
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;

function TZInterbase6Lob.GetBlobId: TISC_QUAD;
begin
  Result := FBlobId;
end;

function TZInterbase6Lob.GetConSettings: PZConSettings;
begin
  if FIBConnection <> nil
  then Result := FIBConnection.GetConSettings
  else Result := nil;
end;

function TZInterbase6Lob.IsEmpty: Boolean;
begin
  Result := Int64(FBlobId) = 0;
end;

function TZInterbase6Lob.Length: Integer;
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

procedure TZInterbase6Lob.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var Imm: IImmediatelyReleasable;
begin
  if (FIBConnection <> nil) and (FIBConnection.GetActiveTransaction <> nil) and
     (FIBConnection.GetActiveTransaction.QueryInterface(IImmediatelyReleasable, imm) = S_OK) and
     (Sender <> imm) then begin
    FIBConnection.GetActiveTransaction.DeRegisterOpenUnCachedLob(Self);
    Imm.ReleaseImmediat(Sender, AError);
    if FlobStream <> nil then begin
      FlobStream.FReleased := True;
      FreeAndNil(FlobStream);
      FreeAndNil(FlobStream);
    end;
  end;
  FReleased := true;
end;

{ TZInterbase6Clob }

constructor TZInterbase6Clob.Create(const Connection: IZInterbase6Connection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, ColumnCodePage, OpenLobStreams);
  FConSettings := Connection.GetConSettings;
end;

{ TZInterbase6Blob }

constructor TZInterbase6Blob.Create(const Connection: IZInterbase6Connection;
  BlobId: TISC_QUAD; LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
begin
  inherited Create(Connection, BlobId, LobStreamMode, zCP_Binary, OpenLobStreams);
end;

{ TZInterbaseRowAccessor }

constructor TZInterbaseRowAccessor.Create(ColumnsInfo: TObjectList;
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
      Current.ColumnType := TZSQLType(Byte(Current.ColumnType)-1); // no streams 4 sqlite
    if Current.ColumnType in [stBytes, stUnicodeStream] then
      Current.ColumnCodePage := zCP_Binary;
  end;
  inherited Create(TempColumns, ConSettings, OpenLobStreams, CachedLobs);
  TempColumns.Free;
end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
