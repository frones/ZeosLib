{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           ODBC Database Connectivity Classes            }
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

unit ZDbcODBCResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit
uses
  {$IFDEF MORMOT2}
  mormot.db.core, mormot.core.datetime, mormot.core.text, mormot.core.base,
  {$ELSE MORMOT2} {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS} {$ENDIF MORMOT2}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}
  System.Types{$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF}
  {$ELSE}
    {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} Types
  {$ENDIF},
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  ZSysUtils, ZDbcIntfs, ZClasses, ZDbcCachedResultSet, ZDbcCache,
  ZCompatibility, ZDbcResultSet, ZFastCode, ZDbcResultsetMetadata,
  ZPlainODBCDriver, ZDbcODBCCon, ZDbcODBCUtils, ZDbcStatement;

type
  /// <author>EgonHugeist</author>
  /// <summary>implements a ODBC specific resultset metadata object</summary>
  TZODBCResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    /// <summary>Clears specified column information.</summary>
    /// <param>"ColumnInfo" a column information object.</param>
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>implements a ODBC specific columninfo object</summary>
  TZODBCColumnInfo = class(TZColumnInfo)
  private
    fODBC_CType: SQLSMALLINT;
    fFixedWidth, fBound: Boolean;
    fColumnBuffer: PAnsiChar;
    fStrLen_or_IndArray: PSQLLENArray;
  public
    destructor Destroy; override;
    property ODBC_CType: SQLSMALLINT read fODBC_CType write fODBC_CType;
    property FixedWidth: Boolean read fFixedWidth write fFixedWidth;
    property Bound: Boolean read fBound write fBound;
    property ColumnBuffer: PAnsiChar read fColumnBuffer write fColumnBuffer;
    property StrLen_or_IndArray: PSQLLENArray read fStrLen_or_IndArray write fStrLen_or_IndArray;
  end;

  { Interbase Error Class}

  TAbstractODBCResultSet = Class(TZAbstractReadOnlyResultSet)
  private
    fPHSTMT: PSQLHSTMT; //direct reference the handle of the stmt/metadata
    FODBCConnection: IZODBCConnection;
    fPlainDriver: TZODBC3PlainDriver;
    fZBufferSize: Integer;
    fEnhancedColInfo, FIsMetaData: Boolean;
    fColumnCount: SQLSMALLINT;
    fMaxFetchableRows, fFetchedRowCount, fCurrentBufRowNo: SQLULEN;
    fStrLen_or_Ind: SQLLEN;
    fColDataPtr: Pointer;
    fIsUnicodeDriver: Boolean;
    fFreeHandle, fCursorOpened: Boolean;
    fSQL_GETDATA_EXTENSIONS: SQLUINTEGER;
    fFirstGetDataIndex, fLastGetDataIndex: Integer;
    FClientCP: Word;
    FTempLob: IZBlob;
    FByteBuffer: PByteBuffer;
    procedure LoadUnBoundColumns;
    function CreateODBCConvertError(ColumnIndex: Integer; DataType: TZSQLType): EZSQLException;
  protected
    procedure CheckStmtError(RETCODE: SQLRETURN);
  public
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
    function IsNull(ColumnIndex: Integer): Boolean;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetUInt(ColumnIndex: Integer): Cardinal;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); reintroduce; overload;
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); reintroduce; overload;
    procedure GetTimestamp(ColumnIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    {$IFDEF WITH_COLUMNS_TO_JSON}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF WITH_COLUMNS_TO_JSON}
  End;

  TAbstractColumnODBCResultSet = class(TAbstractODBCResultSet, IZResultSet)
  protected
    procedure ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray; var Result: String); virtual; abstract;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; virtual; abstract;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo); virtual; abstract;
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; const SQL: String; const Connection: IZODBCConnection;
      ZBufferSize: Integer; const EnhancedColInfo: Boolean = True); virtual;
    constructor CreateForMetadataCall(out StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC;
      {$IFNDEF FPC}const{$ENDIF} Connection: IZODBCConnection); virtual; //fpc skope for (GetConnection as IZODBCConnection) is different to dephi and crashs
    function Next: Boolean; reintroduce;
    procedure ResetCursor; override;
    procedure BeforeClose; override;
  end;

  TODBCResultSetW = class(TAbstractColumnODBCResultSet)
  private
    fPlainW: TODBC3UnicodePlainDriver;
  protected
    procedure ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray; var Result: String); override;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; override;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo); override;
  public
    constructor Create(const Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; const SQL: String; const Connection: IZODBCConnection;
      ZBufferSize: Integer; const EnhancedColInfo: Boolean = True); override;
  end;

  TODBCResultSetA = class(TAbstractColumnODBCResultSet)
  private
    fPlainA: TODBC3RawPlainDriver;
  protected
    procedure ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray; var Result: String); override;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; override;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo); override;
  public
    constructor Create(const Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; const SQL: String; const Connection: IZODBCConnection;
      ZBufferSize: Integer; const EnhancedColInfo: Boolean = True); override;
  end;

  TZODBCBlob = class(TZLocalMemBLob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; const PlainDriver: TZODBC3PlainDriver;
      const OpenLobStreams: TZSortedList);
  end;

  TZODBCClobA = class(TZLocalMemCLob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; const PlainDriver: TZODBC3PlainDriver;
      ConSettings: PZConSettings; const OpenLobStreams: TZSortedList);
  end;

  TZODBCClobW = class(TZAbstractCLob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; const PlainDriver: TZODBC3PlainDriver;
      ConSettings: PZConSettings; const OpenLobStreams: TZSortedList);
  end;

  TZODBCachedResultSetW = class(TZCachedResultSet)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  end;

  { TZODBCRowAccessorW }

  TZODBCRowAccessorW = class(TZRowAccessor)
  protected
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; LobCacheMode: TLobCacheMode); override;
  end;

  TZODBCachedResultSetA = class(TZCachedResultSet)
  protected
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  end;

  { TZODBCRowAccessorW }

  TZODBCRowAccessorA = class(TZRowAccessor)
  protected
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
  public
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; LobCacheMode: TLobCacheMode); override;
  end;

  TZODBCOutParamColumnInfo = class(TZODBCColumnInfo)
  public
    destructor Destroy; override;
  end;

  TZParamODBCResultSet = class(TAbstractODBCResultSet, IZResultSet)
  public
    constructor Create(const Statement: IZStatement; const SQL: String;
       BindList: TZBindList);
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; reintroduce;
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

const
  StringStreamTypes: Array[Boolean] of SQLSMALLINT = (SQL_C_CHAR, SQL_C_WCHAR);

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit

uses Math,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZMessages, ZEncoding, ZDbcProperties, TypInfo,
  ZDbcUtils, ZDbcLogging;

{ TAbstractODBCResultSet }

procedure TAbstractODBCResultSet.CheckStmtError(RETCODE: SQLRETURN);
  procedure HandleError;
  begin
    if Statement <> nil
    then FODBCConnection.HandleErrorOrWarning(RETCODE, fPHSTMT^, SQL_HANDLE_STMT, Statement.GetSQL, lcExecute, Self)
    else FODBCConnection.HandleErrorOrWarning(RETCODE, fPHSTMT^, SQL_HANDLE_STMT, 'MetaData-call', lcExecute, Self);
  end;
begin
  if RETCODE <> SQL_SUCCESS then
    HandleError;
end;

{$IFDEF WITH_COLUMNS_TO_JSON}
procedure TAbstractODBCResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var C, H, I: Integer;
    P: Pointer;
    L: NativeUint;
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
    else with TZODBCColumnInfo(ColumnsInfo[C]) do begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[I]);
      case ColumnType of
        stBoolean:    JSONWriter.AddShort(JSONBool[PByte(fColDataPtr)^ <> 0]);
        stByte:       JSONWriter.AddU(PByte(fColDataPtr)^);
        stShort:      JSONWriter.Add(PShortInt(fColDataPtr)^);
        stWord:       JSONWriter.AddU(PWord(fColDataPtr)^);
        stSmall:      JSONWriter.Add(PSmallInt(fColDataPtr)^);
        stLongWord:   JSONWriter.AddU(PCardinal(fColDataPtr)^);
        stInteger:    JSONWriter.Add(PInteger(fColDataPtr)^);
        stULong:      JSONWriter.AddQ(PUInt64(fColDataPtr)^);
        stLong:       JSONWriter.Add(PInt64(fColDataPtr)^);
        stFloat:      JSONWriter.AddSingle(PSingle(fColDataPtr)^);
        stCurrency:   JSONWriter.{$IFDEF MORMOT2}AddCurr{$ELSE}AddCurr64{$ENDIF}(ODBCNumeric2Curr(fColDataPtr));
        stBigDecimal: begin
                        L := SQL_MAX_NUMERIC_LEN;
                        SQLNumeric2Raw(fColDataPtr, PAnsiChar(fByteBuffer), L);
                        JSONWriter.AddNoJSONEscape(PAnsiChar(FByteBuffer), L);
                      end;
        stDouble: JSONWriter.AddDouble(PDouble(fColDataPtr)^);
        stBytes:      JSONWriter.WrBase64(fColDataPtr,fStrLen_or_Ind,True);
        stGUID:       begin
                        {$IFDEF MORMOT2}
                        JSONWriter.Add(PGUID(fColDataPtr), '"');
                        {$ELSE}
                        JSONWriter.Add('"');
                        JSONWriter.Add(PGUID(fColDataPtr)^);
                        JSONWriter.Add('"');
                        {$ENDIF}
                      end;
        stTime:       begin
                        if jcoMongoISODate in JSONComposeOptions then
                          JSONWriter.AddShort('ISODate("0000-00-00')
                        else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                          {$IFDEF MORMOT2}
                          JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                          {$ELSE}
                          JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                          {$ENDIF}
                        end else
                          JSONWriter.Add('"');
                        if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                          TimeToIso8601PChar(Pointer(FByteBuffer), True, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                            PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second,
                            PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000, 'T', jcoMilliseconds in JSONComposeOptions)
                        else
                          TimeToIso8601PChar(Pointer(FByteBuffer), True, PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                            PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0, 'T', jcoMilliseconds in JSONComposeOptions);
                        JSONWriter.AddNoJSONEscape(Pointer(FByteBuffer),9+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                        if jcoMongoISODate in JSONComposeOptions
                        then JSONWriter.AddShort('Z)"')
                        else JSONWriter.Add('"');
                      end;
        stDate:       begin
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
                        if PSQL_DATE_STRUCT(fColDataPtr)^.year < 0 then
                          JSONWriter.Add('-');
                        DateToIso8601PChar(Pointer(FByteBuffer), True, Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
                        JSONWriter.AddNoJSONEscape(Pointer(FByteBuffer),10);
                        if jcoMongoISODate in JSONComposeOptions
                        then JSONWriter.AddShort('Z")')
                        else JSONWriter.Add('"');
                      end;
        stTimeStamp:  begin
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
                        if PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year < 0 then
                          JSONWriter.Add('-');
                        DateToIso8601PChar(Pointer(FByteBuffer), True, Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
                          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day);
                        TimeToIso8601PChar(Pointer(PAnsiChar(FByteBuffer)+10), True, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute,
                          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction, 'T', jcoMilliseconds in JSONComposeOptions);
                        JSONWriter.AddNoJSONEscape(Pointer(FByteBuffer),19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                        if jcoMongoISODate in JSONComposeOptions
                        then JSONWriter.AddShort('Z")')
                        else JSONWriter.Add('"');
                      end;
        stString, stUnicodeString: begin
            JSONWriter.Add('"');
            if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then begin
              if FixedWidth then
                while (PWideChar(fColDataPtr)+(fStrLen_or_Ind shr 1)-1)^ = ' ' do Dec(fStrLen_or_Ind, 2);
              JSONWriter.AddJSONEscapeW(fColDataPtr, fStrLen_or_Ind shr 1)
            end else begin
              if FixedWidth then
                while (PAnsiChar(fColDataPtr)+(fStrLen_or_Ind)-1)^ = ' ' do Dec(fStrLen_or_Ind);
              if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
                JSONWriter.AddJSONEscape(fColDataPtr, fStrLen_or_Ind)
              else begin
                PRawToUnicode(fColDataPtr, fStrLen_or_Ind, ConSettings^.ClientCodePage^.CP, FUniTemp);
                JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
                FUniTemp := '';
              end;
            end;
            JSONWriter.Add('"');
          end;
        stAsciiStream, stUnicodeStream: begin
            JSONWriter.Add('"');
            if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or (ConSettings^.ClientCodePage^.CP <> zCP_UTF8) then begin
              P := IZBlob(fColDataPtr).GetPWideChar(fUniTemp, L);
              JSONWriter.AddJSONEscapeW(P, L);
            end else begin
              P := IZBlob(fColDataPtr).GetPAnsiChar(zCP_UTF8, fRawTemp, L);
              JSONWriter.AddJSONEscape(P, L);
            end;
            JSONWriter.Add('"');
          end;
        stBinaryStream: begin
          P := IZBlob(fColDataPtr).GetBuffer(fRawTemp, L);
          JSONWriter.WrBase64(P, L, True);
        end else //stArray, stDataSet:
          JSONWriter.AddShort('null') ;
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

function TAbstractODBCResultSet.CreateODBCConvertError(ColumnIndex: Integer;
  DataType: TZSQLType): EZSQLException;
begin
  Result := EZSQLException.Create(Format(SErrorConvertionField,
        [TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnLabel,
        TypInfo.GetEnumName(TypeInfo(TZSQLType), Ord(DataType))]));
end;

{$IFNDEF NO_ANSISTRING}
function TAbstractODBCResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var P: Pointer;
  L: NativeUInt;
begin
  Result := '';
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    if (Ord(ColumnType) < Ord(stString)) or (ColumnType in [stBytes, stBinaryStream]) then begin
      PAnsiChar(P) := GetPAnsiChar(ColumnIndex, L);
      System.SetString(Result, PAnsiChar(P), L);
    end else if IsNull(ColumnIndex) then  //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
      Result := ''
    else case ColumnType of
      stString, stUnicodeString: begin
          if fIsUnicodeDriver then begin
            L := fStrLen_or_Ind shr 1;
            if FixedWidth then
              L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
            PUnicodeToRaw(fColDataPtr, L, zOSCodePage, RawByteString(Result));
          end else begin
            L := fStrLen_or_Ind;
            if FixedWidth then
              L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
            if FClientCP = zOSCodePage then
              System.SetString(Result, PAnsiChar(fColDataPtr), L)
            else begin
              PRawToUnicode(fColDataPtr, l, FClientCP, FUniTemp);
              PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), zOSCodePage, RawByteString(Result));
            end;
          end;
        end;
      stAsciiStream, stUnicodeStream: begin
          FTempLob := GetBlob(ColumnIndex);
          if FTempLob <> nil then begin
            Result := FTempLob.GetAnsiString;
            FTempLob := nil;
          end;
        end;
      else raise CreateODBCConvertError(ColumnIndex, stString);
    end;
  end;
end;
{$ENDIF}

procedure TAbstractODBCResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case ColumnType of
      stBoolean,
      stByte:       ScaledOrdinal2BCD(Word(PByte(fColDataPtr)^), 0, Result, False);
      stShort:      ScaledOrdinal2BCD(SmallInt(PShortInt(fColDataPtr)^), 0, Result);
      stWord:       ScaledOrdinal2BCD(PWord(fColDataPtr)^, 0, Result, False);
      stSmall:      ScaledOrdinal2BCD(PSmallInt(fColDataPtr)^, 0, Result);
      stLongWord:   ScaledOrdinal2BCD(PCardinal(fColDataPtr)^, 0, Result, False);
      stInteger:    ScaledOrdinal2BCD(PInteger(fColDataPtr)^, 0, Result);
      stULong:      ScaledOrdinal2BCD(PUInt64(fColDataPtr)^, 0, Result, False);
      stLong:       ScaledOrdinal2BCD(PInt64(fColDataPtr)^, 0, Result);
      stDate, stTime, stTimeStamp, stFloat,
      stDouble:     Double2BCD(GetDouble(ColumnIndex), Result);
      stCurrency:   SQLNumeric2BCD(fColDataPtr, Result, SizeOf(Int64));
      stBigDecimal: SQLNumeric2BCD(fColDataPtr, Result, SQL_MAX_NUMERIC_LEN);
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  LastWasNull := not TryUniToBCD(PWideChar(fColDataPtr), L, Result, '.');
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  LastWasNull := not TryRawToBCD(PAnsiChar(fColDataPtr), L, Result, '.');
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := NullBCD;
    end;
  end else Result := NullBCD;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TAbstractODBCResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
begin
  Result := nil;
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //loads the lob on demand -> a second call is impossible
    if ColumnType in [stBinaryStream, stAsciiStream, stUnicodeStream]
    then Result := IZBlob(ColumnBuffer)
    else raise CreateCanNotAccessBlobRecordException(ColumnIndex, ColumnType);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TAbstractODBCResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var L: LengthInt;
begin
  Result := False;
  if not IsNull(ColumnIndex) then
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^ <> 0;
      stByte:       Result := PByte(fColDataPtr)^ <> 0;
      stShort:      Result := PShortInt(fColDataPtr)^ <> 0;
      stWord:       Result := PWord(fColDataPtr)^ <> 0;
      stSmall:      Result := PSmallInt(fColDataPtr)^ <> 0;
      stLongWord:   Result := PCardinal(fColDataPtr)^ <> 0;
      stInteger:    Result := PInteger(fColDataPtr)^ <> 0;
      stULong:      Result := PUInt64(fColDataPtr)^ <> 0;
      stLong:       Result := PInt64(fColDataPtr)^ <> 0;
      stFloat:      Result := PSingle(fColDataPtr)^ <> 0;
      stCurrency:   Result := ODBCNumeric2Curr(fColDataPtr) <> 0;
      stDouble,
      stBigDecimal: Result := PDouble(fColDataPtr)^ <> 0;
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2)
                    then Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000) <> 0
                    else Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0) <> 0;
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day) <> 0;
      stTimeStamp:
        Result := (EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day) <> 0) and
          (EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction) <> 0);
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  Result := StrToBoolEx(PWideChar(fColDataPtr), PWideChar(fColDataPtr)+L);
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := StrToBoolEx(PAnsiChar(fColDataPtr), PAnsiChar(fColDataPtr)+L);
                end;
      stAsciiStream, stUnicodeStream:
        Result := StrToBoolEx(IZBlob(ColumnBuffer).{$IFDEF UNICODE}GetUnicodeString{$ELSE}GetString{$ENDIF});
      else raise CreateConversionError(ColumnIndex, ColumnType, stBoolean);
    end;
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
function TAbstractODBCResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
  if IsNull(ColumnIndex) or (fColDataPtr = nil) then begin//Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    Result := nil;
    Len := 0;
  end else begin
    Result := fColDataPtr;
    Len := fStrLen_or_Ind shl Ord((TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType in [stString, stUnicodeString]) and fIsUnicodeDriver);
  end;
end;

function TAbstractODBCResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PCardinal(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stCurrency:   Result := ODBCNumeric2Curr(fColDataPtr);
      stDouble,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000)
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0);
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:
        Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction);
      stString,
      stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  SQLStrToFloatDef(PWideChar(fColDataPtr), 0, Result, L)
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  SQLStrToFloatDef(PAnsiChar(fColDataPtr), 0, Result, L)
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else Result := 0;
end;

procedure TAbstractODBCResultSet.GetDate(ColumnIndex: Integer;
  var Result: TZDate);
var L: LengthInt;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  if not IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case ColumnType of
      stTime:       goto Fill;
      stDate:       begin
                      Result.Year := Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year);
                      Result.Month := PSQL_DATE_STRUCT(fColDataPtr)^.month;
                      Result.Day := PSQL_DATE_STRUCT(fColDataPtr)^.day;
                      Result.IsNegative := PSQL_DATE_STRUCT(fColDataPtr)^.year < 0;
                    end;
      stTimeStamp:  if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIMESTAMPOFFSET) then begin
                      Result.Year := Abs(PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.year);
                      Result.Month := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.month;
                      Result.Day := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.day;
                      Result.IsNegative := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.year < 0;
                    end else begin
                      Result.Year := Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year);
                      Result.Month := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month;
                      Result.Day := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day;
                      Result.IsNegative := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year < 0;
                    end;
      stString,
      stUnicodeString: begin
                if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  LastWasNull := not TryPCharToDate(PWideChar(fColDataPtr), L, ConSettings^.ReadFormatSettings, Result);
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  LastWasNull := not TryPCharToDate(PAnsichar(fColDataPtr), L, ConSettings^.ReadFormatSettings, Result);
                end;
                if LastWasNull then
                  goto Fill;
              end;
      else DecodeDateTimeToDate(GetDouble(ColumnIndex), Result);
    end;
  end else
Fill: PInt64(@Result.Year)^ := 0;
end;

function TAbstractODBCResultSet.GetDouble(ColumnIndex: Integer): Double;
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PCardinal(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stCurrency:   Result := ODBCNumeric2Curr(fColDataPtr);
      stDouble,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000)
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0);
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:
        Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction);
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  SQLStrToFloatDef(PWideChar(fColDataPtr), 0, Result, L)
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  SQLStrToFloatDef(PAnsiChar(fColDataPtr), 0, Result, L)
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else Result := 0;
end;

function TAbstractODBCResultSet.GetFloat(ColumnIndex: Integer): Single;
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PCardinal(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := PSingle(fColDataPtr)^;
      stCurrency:   Result := ODBCNumeric2Curr(fColDataPtr);
      stDouble,
      stBigDecimal: Result := PDouble(fColDataPtr)^;
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                      Result := EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000)
                    else
                      Result := EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0);
      stDate:
        Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:
        Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction);
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  SQLStrToFloatDef(PWideChar(fColDataPtr), 0, Result, L)
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  SQLStrToFloatDef(PAnsiChar(fColDataPtr), 0, Result, L)
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0;
end;

procedure TAbstractODBCResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
var L: LengthInt;
label Fail;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case ColumnType of
      stGUID:     Result := PGUID(fColDataPtr)^;
      stBytes:    if fStrLen_or_Ind = SizeOf(TGUID)
                  then Move(fColDataPtr^, Result.D1, SizeOf(TGUID))
                  else goto fail;
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  if (L = 38) or (L = 36)
                  then ZSysUtils.ValidGUIDToBinary(PWidechar(fColDataPtr), @Result.D1)
                  else goto fail;
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  if (L = 38) or (L = 36)
                  then ZSysUtils.ValidGUIDToBinary(PAnsichar(fColDataPtr), @Result.D1)
                  else goto fail;
                end;
      else
fail:          raise CreateODBCConvertError(ColumnIndex, stGUID);
    end;
  end else
    FillChar(Result, SizeOf(TGUID), #0);
end;

function TAbstractODBCResultSet.GetInt(ColumnIndex: Integer): Integer;
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PCardinal(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stCurrency:   Result := Trunc(ODBCNumeric2Curr(fColDataPtr));
      stDouble,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  Result := UnicodeToIntDef(PWideChar(fColDataPtr), PWideChar(fColDataPtr)+L, 0)
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := RawToIntDef(PAnsiChar(fColDataPtr), PAnsiChar(fColDataPtr)+L, 0);
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0;
end;

function TAbstractODBCResultSet.GetLong(ColumnIndex: Integer): Int64;
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PCardinal(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stCurrency:   Result := Trunc(ODBCNumeric2Curr(fColDataPtr));
      stDouble,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  Result := UnicodeToInt64Def(PWideChar(fColDataPtr), PWideChar(fColDataPtr)+L, 0)
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := RawToInt64Def(PAnsiChar(fColDataPtr), PAnsiChar(fColDataPtr)+L, 0);
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0;
end;

function TAbstractODBCResultSet.GetPAnsiChar(ColumnIndex: Integer;
  out Len: NativeUInt): PAnsiChar;
Label Set_Results;
begin
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    if Boolean(PByte(fColDataPtr)^) then begin
                      Result := Pointer(BoolStrsRaw[True]);
                      Len := 4
                    end else begin
                      Result := Pointer(BoolStrsRaw[False]);
                      Len := 5;
                    end;
      stByte:       begin
                      IntToRaw(Cardinal(PByte(fColDataPtr)^), PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stShort:      begin
                      IntToRaw(Integer(PShortInt(fColDataPtr)^), PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stWord:        begin
                      IntToRaw(Cardinal(PWord(fColDataPtr)^), PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stSmall:      begin
                      IntToRaw(Integer(PSmallInt(fColDataPtr)^), PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stLongWord:   begin
                      IntToRaw(PCardinal(fColDataPtr)^, PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stInteger:    begin
                      IntToRaw(PInteger(fColDataPtr)^, PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stULong:      begin
                      IntToRaw(PUInt64(fColDataPtr)^, PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stLong:       begin
                      IntToRaw(PInt64(fColDataPtr)^, PAnsiChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stFloat:      begin
                      Len := FloatToSqlRaw(PSingle(fColDataPtr)^, PAnsiChar(FByteBuffer));
                      Result := PAnsiChar(FByteBuffer);
                    end;
      stCurrency:   begin
                      CurrToRaw(ODBCNumeric2Curr(fColDataPtr), '.', PAnsiChar(FByteBuffer), @Result);
Set_Results:          Len := Result - PAnsiChar(FByteBuffer);
                      Result := PAnsiChar(FByteBuffer);
                    end;
      stBigDecimal: begin
                      Result := PAnsiChar(FByteBuffer);
                      Len := SQL_MAX_NUMERIC_LEN;
                      SQLNumeric2Raw(fColDataPtr, Result, Len);
                    end;
      stDouble: begin
                      Len := FloatToSqlRaw(PDouble(fColDataPtr)^, PAnsiChar(FByteBuffer));
                      Result := PAnsiChar(FByteBuffer);
                    end;
      stBytes:      begin
                      Result := fColDataPtr;
                      len := fStrLen_or_Ind;
                    end;
      stGUID:       begin
                      GUIDToBuffer(fColDataPtr, PAnsiChar(FByteBuffer), [guidWithBrackets]);
                      Result := PAnsiChar(FByteBuffer);
                      Len := 38;
                    end;
      stTime:       begin
                      Result := PAnsiChar(FByteBuffer);
                      if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2)
                      then Len := TimeToRaw(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second,
                          PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction, Result,
                          ConSettings^.ReadFormatSettings.TimeFormat, False, False)
                      else Len := TimeToRaw(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0, Result,
                          ConSettings^.ReadFormatSettings.TimeFormat, False, False);
                    end;
      stDate:       begin
                      Result := PAnsiChar(FByteBuffer);
                      Len := DateToRaw(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                        PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day, Result,
                          ConSettings^.ReadFormatSettings.DateFormat, False, PSQL_DATE_STRUCT(fColDataPtr)^.year < 0);
                    end;
      stTimeStamp:  begin
                      Result := PAnsiChar(FByteBuffer);
                      Len := DateTimeToRaw(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                        PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction, Result,
                          ConSettings^.ReadFormatSettings.DateTimeFormat,
                          False, PSQL_DATE_STRUCT(fColDataPtr)^.year < 0);
                    end;
      stString, stUnicodeString: begin
                      if fIsUnicodeDriver then begin
                        Len := fStrLen_or_Ind shr 1;
                        if FixedWidth then
                          Len := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), Len);
                        fRawTemp := PUnicodeToRaw(fColDataPtr, Len, FClientCP);
                        Len := Length(fRawTemp);
                        if Len > 0
                        then Result := Pointer(fRawTemp)
                        else Result := pEmptyAnsiString;
                      end else begin
                        Result := fColDataPtr;
                        if FixedWidth
                        then Len := GetAbsorbedTrailingSpacesLen(Result, fStrLen_or_Ind)
                        else Len := fStrLen_or_Ind;
                      end;
                    end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
                    begin
                      if IZBlob(ColumnBuffer).IsCLob
                      then Result := IZBlob(ColumnBuffer).GetPAnsiChar(FClientCP, FRawTemp, Len)
                      else Result := IZBlob(ColumnBuffer).GetBuffer(fRawTemp, Len);
                    end
      else          begin
                      Result := pEmptyAnsiString;
                      Len := 0;
                    end;
    end;
  end else begin
    Result := nil;
    Len := 0;
  end;
end;

function TAbstractODBCResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
Label Set_Results, Set_From_Temp;
begin
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    if Boolean(PByte(fColDataPtr)^) then begin
                      Result := Pointer(BoolStrsW[True]);
                      Len := 4
                    end else begin
                      Result := Pointer(BoolStrsW[False]);
                      Len := 5;
                    end;
      stByte:       begin
                      IntToUnicode(Cardinal(PByte(fColDataPtr)^), PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stShort:      begin
                      IntToUnicode(Integer(PShortInt(fColDataPtr)^), PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stWord:        begin
                      IntToUnicode(Cardinal(PWord(fColDataPtr)^), PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stSmall:      begin
                      IntToUnicode(Integer(PSmallInt(fColDataPtr)^), PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stLongWord:   begin
                      IntToUnicode(PCardinal(fColDataPtr)^, PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stInteger:    begin
                      IntToUnicode(PInteger(fColDataPtr)^, PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stULong:      begin
                      IntToUnicode(PUInt64(fColDataPtr)^, PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stLong:       begin
                      IntToUnicode(PInt64(fColDataPtr)^, PWideChar(FByteBuffer), @Result);
                      goto Set_Results;
                    end;
      stFloat:      begin
                      Len := FloatToSqlUnicode(PSingle(fColDataPtr)^, PWideChar(FByteBuffer));
                      Result := PWideChar(FByteBuffer);
                    end;
      stCurrency:   begin
                      CurrToUnicode(ODBCNumeric2Curr(fColDataPtr), '.', PWideChar(FByteBuffer), @Result);
Set_Results:          Len := Result - PWideChar(FByteBuffer);
                      Result := PWideChar(FByteBuffer);
                    end;

      stBigDecimal: begin
                      Result := PWideChar(FByteBuffer);
                      Len := SQL_MAX_NUMERIC_LEN;
                      SQLNumeric2Uni(fColDataPtr, Result, Len);
                    end;
      stDouble: begin
                      Len := FloatToSqlUnicode(PDouble(fColDataPtr)^, PWideChar(FByteBuffer));
                      Result := PWideChar(FByteBuffer);
                    end;
      stBytes:      begin
                      fUniTemp := Ascii7ToUnicodeString(fColDataPtr, fStrLen_or_Ind);
                      goto Set_From_Temp;
                    end;
      stGUID:       begin
                      GUIDToBuffer(fColDataPtr, PWideChar(FByteBuffer), [guidWithBrackets]);
                      Result := PWideChar(FByteBuffer);
                      Len := 38;
                    end;
      stTime:       begin
                      Result := PWideChar(FByteBuffer);
                      if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2)
                      then Len := TimeToUni(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second,
                          PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction, Result,
                          ConSettings^.ReadFormatSettings.TimeFormat, False, False)
                      else Len := TimeToUni(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0, Result,
                          ConSettings^.ReadFormatSettings.TimeFormat, False, False);
                    end;
      stDate:       begin
                      Result := PWideChar(FByteBuffer);
                      Len := DateTimeToUnicodeSQLDate(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                        PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day), Result,
                          ConSettings^.ReadFormatSettings, False);
                    end;
      stTimeStamp:  begin
                      Result := PWideChar(FByteBuffer);
                      if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIMESTAMPOFFSET)
                      then Len := DateTimeToUni(Abs(PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.year),
                        PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.month, PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.day,
                        PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.second,
                        PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.fraction, Result,
                        ConSettings^.ReadFormatSettings.DateTimeFormat, False,
                        PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.year < 0)
                      else Len := DateTimeToUni(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction, Result,
                        ConSettings^.ReadFormatSettings.DateTimeFormat, False,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year < 0);
                    end;
      stString, stUnicodeString: begin
                      if fIsUnicodeDriver then begin
                        Result := PWideChar(fColDataPtr);
                        Len := fStrLen_or_Ind shr 1;
                        if FixedWidth then
                          Len := GetAbsorbedTrailingSpacesLen(Result, Len);
                      end else begin
                        Len := fStrLen_or_Ind;
                        if FixedWidth then
                          Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), Len);
                        PRawToUnicode(fColDataPtr, Len, FClientCP, FUniTemp);
                        goto Set_From_Temp;
                      end;
                    end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
                    begin
                      if IZBlob(ColumnBuffer).IsCLob
                      then Result := IZBlob(ColumnBuffer).GetPWideChar(fUniTemp, Len)
                      else begin
                        Result := IZBlob(ColumnBuffer).GetBuffer(fRawTemp, Len);
                        fUniTemp := Ascii7ToUnicodeString(PAnsiChar(Result), Len);
Set_From_Temp:          Len := Length(fUniTemp);
                        if Len > 0
                        then Result := Pointer(fUniTemp)
                        else Result := pEmptyUnicodeString;
                      end;
                    end
      else          begin
                      Result := pEmptyUnicodeString;
                      Len := 0;
                    end;
    end;
  end else begin
    Result := nil;
    Len := 0;
  end;
end;

function TAbstractODBCResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PCardinal(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      stULong:      Result := PUInt64(fColDataPtr)^;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stCurrency:   Result := Trunc(ODBCNumeric2Curr(fColDataPtr));
      stDouble,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                  Result := UnicodeToUInt64Def(PWideChar(fColDataPtr), PWideChar(fColDataPtr)+L, 0)
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := RawToUInt64Def(PAnsiChar(fColDataPtr), PAnsiChar(fColDataPtr)+L, 0);
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0;
end;

function TAbstractODBCResultSet.GetRawByteString(
  ColumnIndex: Integer): RawByteString;
var P: Pointer;
  L: NativeUInt;
  CP: Word;
begin
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    if (Ord(ColumnType) < Ord(stString)) or (ColumnType in [stBytes, stBinaryStream]) then begin
      PAnsiChar(P) := GetPAnsiChar(ColumnIndex, L);
      System.SetString(Result, PAnsiChar(P), L);
    end else if IsNull(ColumnIndex) then  //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
      Result := ''
    else case ColumnType of
      stString, stUnicodeString: begin
                        if fIsUnicodeDriver then begin
                          L := fStrLen_or_Ind shr 1;
                          if FixedWidth then
                            L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                          Result := PUnicodeToRaw(fColDataPtr, L, FClientCP);
                        end else begin
                          L := fStrLen_or_Ind;
                          if FixedWidth then
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                          ZSetString(PAnsiChar(fColDataPtr), L, Result);
                        end;
        end;
      stAsciiStream, stUnicodeStream: begin
          if fIsUnicodeDriver
          then CP := GetW2A2WConversionCodePage(ConSettings)
          else CP := FClientCP;
          FTemplob := GetBlob(ColumnIndex);
          if FTemplob <> nil then begin
            Result := FTemplob.GetRawByteString(CP);
            FTemplob := nil;
          end else Result := '';
        end;
      else Result := '';
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZTime</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>zero padded</code>
  @exception SQLException if a database access error occurs
}
procedure TAbstractODBCResultSet.GetTime(ColumnIndex: Integer; var Result: TZTime);
var L: LengthInt;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  if not IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case ColumnType of
      stDate:       goto Fill;
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then begin
                      Result.Hour := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour;
                      Result.Minute := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute;
                      Result.Second := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second;
                      Result.Fractions := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction;
                      Result.IsNegative := False;
                    end else begin
                      Result.Hour := PSQL_TIME_STRUCT(fColDataPtr)^.hour;
                      Result.Minute := PSQL_TIME_STRUCT(fColDataPtr)^.minute;
                      Result.Second := PSQL_TIME_STRUCT(fColDataPtr)^.second;
                      Result.Fractions := 0;
                      Result.IsNegative := False;
                    end;
      stTimeStamp:  if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIMESTAMPOFFSET) then begin
                      Result.Hour := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.hour;
                      Result.Minute := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.minute;
                      Result.Second := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.second;
                      Result.Fractions := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.fraction;
                      Result.IsNegative := False;
                    end else begin
                      Result.Hour := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour;
                      Result.Minute := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute;
                      Result.Second := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second;
                      Result.Fractions := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction;
                      Result.IsNegative := False;
                    end;
      stString, stUnicodeString: begin
                if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  LastWasNull := not TryPCharToTime(PWideChar(fColDataPtr), L, ConSettings^.ReadFormatSettings, Result);
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  LastWasNull := not TryPCharToTime(PAnsiChar(fColDataPtr), L, ConSettings^.ReadFormatSettings, Result);
                end;
                if LastWasNull then
                  goto Fill;
              end;
      else DecodeDateTimeToTime(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
    end;
  end else
Fill: FillChar(Result, SizeOf(TZTime), #0);
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
{$IFDEF FPC} {$PUSH} {$WARN 6018 off : Unreachable code} {$ENDIF} //depents to record size
procedure TAbstractODBCResultSet.GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp);
var L: LengthInt;
label Fill;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  if not IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case ColumnType of
      stDate:       begin
                      Result.Year := Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year);
                      Result.Month := PSQL_DATE_STRUCT(fColDataPtr)^.month;
                      Result.Day := PSQL_DATE_STRUCT(fColDataPtr)^.day;
                      PInt64(@Result.Hour)^ := 0;
                      PInt64(@Result.Fractions)^ := 0;
                      Result.IsNegative := PSQL_DATE_STRUCT(fColDataPtr)^.year < 0;
                    end;
      stTime:     begin
                    PInt64(@Result.Year)^ := PInt64(@cPascalIntegralDatePart.Year)^;
                    if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then begin
                      PInt64(@Result.Year)^ := 0;
                      Result.Hour := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour;
                      Result.Minute := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute;
                      Result.Second := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second;
                      Result.Fractions := PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction;
                    end else begin
                      Result.Hour := PSQL_TIME_STRUCT(fColDataPtr)^.hour;
                      Result.Minute := PSQL_TIME_STRUCT(fColDataPtr)^.minute;
                      Result.Second := PSQL_TIME_STRUCT(fColDataPtr)^.second;
                      Result.Fractions := 0;
                    end;
                    Result.IsNegative := False;
                  end;
      stTimeStamp:  if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIMESTAMPOFFSET) then begin
                      if SizeOf(TSQL_SS_TIMESTAMPOFFSET_STRUCT) = SizeOf(TZTimeStamp)-2 then
                        PSQL_SS_TIMESTAMPOFFSET_STRUCT(@Result.Year)^ := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^
                      else begin
                        Result.Month := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.month;
                        Result.Day := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.day;
                        Result.Hour := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.hour;
                        Result.Minute := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.minute;
                        Result.Second := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.second;
                        Result.Fractions := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.fraction;
                        Result.TimeZoneHour := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.timezone_hour;
                        Result.TimeZoneMinute := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.timezone_minute;
                        Result.IsNegative := False;
                      end;
                      Result.Year := Abs(PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.year);
                      Result.IsNegative := PSQL_SS_TIMESTAMPOFFSET_STRUCT(fColDataPtr)^.year < 0;
                    end else begin
                      if SizeOf(TSQL_TIMESTAMP_STRUCT) = SizeOf(TZTimeStamp)-6 then
                        PSQL_TIMESTAMP_STRUCT(@Result.Year)^ := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^
                      else begin
                        PCardinal(@Result.TimeZoneHour)^ := 0;
                        Result.Month := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month;
                        Result.Day := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day;
                        Result.Hour := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour;
                        Result.Minute := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute;
                        Result.Second := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second;
                        Result.Fractions := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction;
                        Result.IsNegative := False;
                      end;
                      Result.IsNegative := PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year < 0;
                      if Result.IsNegative then
                        Result.Year := Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year);
                      PCardinal(@Result.TimeZoneHour)^ := 0;
                    end;
      stString, stUnicodeString: begin
                if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  LastWasNull := not TryPCharToTimeStamp(PWideChar(fColDataPtr), L, ConSettings^.ReadFormatSettings, Result);
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  LastWasNull := not TryPCharToTimeStamp(PAnsiChar(fColDataPtr), L, ConSettings^.ReadFormatSettings, Result);
                end;
                if LastWasNull then
                  goto Fill;
              end;
      else DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
    end;
  end else
Fill: FillChar(Result, SizeOf(TZTime), #0);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TAbstractODBCResultSet.GetULong(ColumnIndex: Integer): UInt64;
var L: LengthInt;
begin
  if not IsNull(ColumnIndex) then with
    TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    case ColumnType of
      stBoolean:    Result := PByte(fColDataPtr)^;
      stByte:       Result := PByte(fColDataPtr)^;
      stShort:      Result := PShortInt(fColDataPtr)^;
      stWord:       Result := PWord(fColDataPtr)^;
      stSmall:      Result := PSmallInt(fColDataPtr)^;
      stLongWord:   Result := PCardinal(fColDataPtr)^;
      stInteger:    Result := PInteger(fColDataPtr)^;
      stULong:      Result := PUInt64(fColDataPtr)^;
      stLong:       Result := PInt64(fColDataPtr)^;
      stFloat:      Result := Trunc(PSingle(fColDataPtr)^);
      stCurrency:   Result := Trunc(ODBCNumeric2Curr(fColDataPtr));
      stDouble,
      stBigDecimal: Result := Trunc(PDouble(fColDataPtr)^);
      stTime:       if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                      Result := Trunc(EncodeTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000))
                    else
                      Result := Trunc(EncodeTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0));
      stDate:
        Result := Trunc(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day));
      stTimeStamp:
        Result := Trunc(EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day)+
          EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction));
      stString, stUnicodeString: if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  Result := UnicodeToUInt64Def(PWideChar(fColDataPtr), PWideChar(fColDataPtr)+L, 0)
                end else begin
                  L := fStrLen_or_Ind;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := RawToUInt64Def(PAnsiChar(fColDataPtr), PAnsiChar(fColDataPtr)+L, 0);
                end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{$IFNDEF NO_UTF8STRING}
function TAbstractODBCResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var P: Pointer;
  L: NativeUInt;
begin
  Result := '';
  if not IsNull(ColumnIndex) then with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    if (Ord(ColumnType) < Ord(stString)) or (ColumnType in [stBytes, stBinaryStream]) then begin
      P := GetPAnsiChar(ColumnIndex, L);
      System.SetString(Result, PAnsiChar(P), L);
    end else case ColumnType of
      stString, stUnicodeString: begin
                        if fIsUnicodeDriver then begin
                          L := fStrLen_or_Ind shr 1;
                          if FixedWidth then
                            L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                          PUnicodeToRaw(fColDataPtr, L, zCP_UTF8, RawByteString(Result));
                        end else begin
                          L := fStrLen_or_Ind;
                          if FixedWidth then
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                          if FClientCP = zCP_UTF8 then
                            System.SetString(Result, PAnsiChar(fColDataPtr), L)
                          else begin
                            PRawToUnicode(fColDataPtr, l, FClientCP, FUniTemp);
                            PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), zCP_UTF8, RawByteString(Result));
                          end;
                        end;
        end;
      stAsciiStream, stUnicodeStream: begin
          FTempLob := GetBlob(ColumnIndex);
          FTempLob := GetBlob(ColumnIndex);
          if FTempLob <> nil then begin
            Result := FTempLob.GetUTF8String;
            FTempLob := nil;
          end;
        end;
      else raise CreateODBCConvertError(ColumnIndex, stString);
    end;
  end;
end;
{$ENDIF}

function TAbstractODBCResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  Assert((ColumnIndex >= FirstDbcIndex) and (ColumnIndex{$IFDEF GENERIC_INDEX}<{$ELSE}<={$ENDIF} fColumnCount), SColumnIsNotAccessable);
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
  {$R-}
  fStrLen_or_Ind := StrLen_or_IndArray[fCurrentBufRowNo-1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Result := fStrLen_or_Ind = SQL_NULL_DATA;
  fColDataPtr := nil;
    if (Ord(ColumnType) <= Ord(stBytes)) and (not Result) then
      fColDataPtr := { Start entry }            ColumnBuffer+Cardinal(CharOctedLength)*(fCurrentBufRowNo-1)
        { increase by size of indicator array } ;
  end;
  LastWasNull := Result;
end;

procedure TAbstractODBCResultSet.LoadUnBoundColumns;
var
  ColumnIndex: Integer;
  StrLen_or_IndPtr: PSQLLEN;
begin
  for ColumnIndex := fFirstGetDataIndex to fLastGetDataIndex do
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex]) do begin
      {$R-}
      StrLen_or_IndPtr := @StrLen_or_IndArray[fCurrentBufRowNo-1];
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      if not Bound then //some drivers allow GetData in mixed order so check it!
        if Ord(ColumnType) < Ord(stAsciiStream) then //move data to buffers
          CheckStmtError(fPlainDriver.SQLGetData(fPHSTMT^, ColumnIndex+1, ODBC_CType,
            ColumnBuffer, CharOctedLength, StrLen_or_IndPtr))
        else begin
          { check out length of lob }
          CheckStmtError(fPlainDriver.SQLGetData(fPHSTMT^, ColumnIndex+1,
            ODBC_CType, Pointer(1){can not be nil}, 0, StrLen_or_IndPtr));
          if StrLen_or_IndPtr^ = SQL_NULL_DATA then
            PIZlob(@ColumnBuffer)^ := nil
          else if ColumnType = stBinaryStream
            then PIZlob(@ColumnBuffer)^ := TZODBCBlob.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fPlainDriver, FOpenLobStreams)
            else if ColumnType in [stAsciiStream, stUnicodeStream]
              then if ConSettings^.ClientCodePage^.Encoding = ceUTF16
                then PIZlob(@ColumnBuffer)^ := TZODBCClobW.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fPlainDriver, ConSettings, FOpenLobStreams)
                else PIZlob(@ColumnBuffer)^ := TZODBCClobA.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fPlainDriver, ConSettings, FOpenLobStreams);
        end;
  end;
end;

{ TAbstractColumnODBCResultSet }

procedure TAbstractColumnODBCResultSet.BeforeClose;
var STMT: SQLHSTMT;
begin
  inherited BeforeClose;
  if Assigned(fPHSTMT^) and fFreeHandle then begin// from metadata
    STMT := fPHSTMT^;
    fPHSTMT^ := nil;
    CheckStmtError(fPlainDriver.SQLFreeHandle(SQL_HANDLE_STMT, STMT)); //free handle
  end;
  //CheckStmtError(fPlainDriver.SQLFreeStmt(fPHSTMT^,SQL_UNBIND)); //discart col bindings -> commented this kills our memory
end;

constructor TAbstractColumnODBCResultSet.Create(const Statement: IZStatement;
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; const SQL: String;
  const Connection: IZODBCConnection; ZBufferSize: Integer;
  const EnhancedColInfo: Boolean);
var Supported: SQLUSMALLINT;
  Ret: SQLRETURN;
begin
  inherited Create(Statement, SQL, TZODBCResultSetMetadata.Create(Connection.GetMetadata, SQL, Self), Connection.GetConSettings);
  FODBCConnection := Connection;
  fPlainDriver := TZODBC3PlainDriver(FODBCConnection.GetPlainDriver.GetInstance);
  fIsUnicodeDriver := Supports(fPlainDriver, IODBC3UnicodePlainDriver);
  fPHSTMT := @StmtHandle;
  fZBufferSize := ZBufferSize;
  Ret := fPLainDriver.SQLGetFunctions(ConnectionHandle, SQL_API_SQLCOLATTRIBUTE, @Supported);
  if Ret <> SQL_SUCCESS then
    FODBCConnection.HandleErrorOrWarning(Ret, ConnectionHandle, SQL_HANDLE_DBC,
      'SQLGetFunctions', lcOther, Statement);
  fEnhancedColInfo := EnhancedColInfo and (Supported = SQL_TRUE);
  fCurrentBufRowNo := 0;
  fFreeHandle := not Assigned(StmtHandle);
  Ret := fPlainDriver.SQLGetInfo(ConnectionHandle,
    SQL_GETDATA_EXTENSIONS, @fSQL_GETDATA_EXTENSIONS, SizeOf(SQLUINTEGER), nil);
  if Ret <> SQL_SUCCESS then
    FODBCConnection.HandleErrorOrWarning(Ret, ConnectionHandle, SQL_HANDLE_DBC,
      'SQLGetInfo', lcOther, Statement);
  FByteBuffer := FODBCConnection.GetByteBufferAddress;
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;
  fCursorOpened := True;
  FClientCP := ConSettings^.ClientCodePage.CP;
  Open;
end;

constructor TAbstractColumnODBCResultSet.CreateForMetadataCall(
  out StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC;
  {$IFNDEF FPC}const{$ENDIF} Connection: IZODBCConnection);
var Ret: SQLRETURN;
begin
  FIsMetaData := True;
  StmtHandle := nil;
  Create(nil, StmtHandle, ConnectionHandle, '', Connection,
    {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Connection.GetParameters.Values[DSProps_InternalBufSize], 131072), //by default 128KB
    False);
  Ret := fPlainDriver.SQLAllocHandle(SQL_HANDLE_STMT, ConnectionHandle, StmtHandle);
  if Ret <> SQL_SUCCESS then
    Connection.HandleErrorOrWarning(Ret, ConnectionHandle, SQL_HANDLE_DBC,
      'SQLAllocHandle(Stmt)', lcOther, Self);
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
function TAbstractColumnODBCResultSet.Next: Boolean;
//const FetchOrientation: array[Boolean] of SQLSMALLINT = (SQL_FETCH_FIRST, SQL_FETCH_NEXT); //using FetchScroll or ExtendedFetch??
var RETCODE: SQLRETURN;
label Fail, FetchData, cls_crs;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (fPHSTMT^ = nil) or (Closed and not FIsMetaData) then
    Exit;
  if (RowNo = 0) then begin//fetch Iteration count of rows
    if Closed then Open;
FetchData:
    fCursorOpened := True;
    fCurrentBufRowNo := 1;
    RETCODE := fPlainDriver.SQLFetch(fPHSTMT^);
    if fMaxFetchableRows > 1 then //block cursor mode
      case RetCode of
        SQL_NO_DATA: //SQL_NO_DATA is returned too if final block fetch is done too but less rows than demanded are fetched
          if fFetchedRowCount = 0 //so check out the how many rows have been obtained
          then goto Fail
          else goto cls_crs;
        SQL_INVALID_HANDLE: begin
            fPHSTMT^ := nil;
            inherited ResetCursor;
            goto fail;
          end;
        SQL_PARAM_DATA_AVAILABLE: ; //v3.8+
        else begin
            CheckStmtError(RETCODE);
cls_crs:    if fLastGetDataIndex >= fFirstGetDataIndex then
              LoadUnBoundColumns;
            if fFetchedRowCount < fMaxFetchableRows then begin
              CheckStmtError(fPlainDriver.SQLCloseCursor(fPHSTMT^));
              fCursorOpened := False;
            end
          end;
      end
    else if RETCODE = SQL_NO_DATA then //single row fetch -> SQL_NO_DATA = end ow row set
      goto Fail
    else begin
      if (RETCODE <> SQL_PARAM_DATA_AVAILABLE) and (RETCODE <> SQL_SUCCESS) then
        CheckStmtError(RETCODE);
      fFetchedRowCount := 1;
      if fLastGetDataIndex >= fFirstGetDataIndex then
        LoadUnBoundColumns;
    end;
  end else
    if FCurrentBufRowNo < fFetchedRowCount then
      Inc(FCurrentBufRowNo)
    else
      if (fMaxFetchableRows > 1) and (fFetchedRowCount <> fMaxFetchableRows ) then
        goto fail else
        goto FetchData;

  RowNo := RowNo + 1;
  if LastRowNo < RowNo then
    LastRowNo := RowNo;
  Result := True;
  Exit;
Fail:
  if fCursorOpened then begin
    CheckStmtError(fPlainDriver.SQLCloseCursor(fPHSTMT^));
    fCursorOpened := False;
  end;
  if RowNo <= LastRowNo then
    RowNo := LastRowNo + 1;
  if not LastRowFetchLogged and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
end;

{$IFDEF FPC} {$PUSH} {$WARN 4081 off : Converting Operants to "Int64" could prevent overflow errors.} {$ENDIF}
procedure TAbstractColumnODBCResultSet.Open;
var
  bufSQLLEN: SQLLEN;
  DESC_CONCISE_TYPE: SQLLEN absolute bufSQLLEN;
  DESC_NULLABLE: SQLLEN absolute bufSQLLEN;
  ColumnNumber: SQLUSMALLINT;
  ColumnInfo: TZODBCColumnInfo;
  RowSize: NativeUInt;
  LobsInResult: Boolean;
  StrBuf: TByteDynArray;
  Desc: SQLHDESC;
  function NoStreamedColFollows: Boolean;
  var I: Integer;
  begin
    Result := True;
    for i := ColumnsInfo.Count -1 downto 0+ColumnNumber do
      if ord(TZODBCColumnInfo(ColumnsInfo[I]).ColumnType) <= Ord(stUnicodeString) then begin
        Result := False;
        Break;
      end;
  end;
  function IsMoney: Boolean;
  var TypeName: String;
  begin
    TypeName := '';
    ColStrAttribute(ColumnNumber, SQL_DESC_TYPE_NAME, StrBuf, TypeName);
    Result := (ZFastCode.Pos('MONEY', UpperCase(TypeName)) > 0); //handle smallmoney oslt too
  end;
begin
  if Closed and Assigned(fPHSTMT^) then begin
    if fColumnCount = 0 then
      CheckStmtError(fPlainDriver.SQLNumResultCols(fPHSTMT^, @fColumnCount));
    if fColumnCount = 0 then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    RowSize := 0;
    LobsInResult := False;
    {$IFDEF WITH_VAR_INIT_WARNING}StrBuf := nil;{$ENDIF}
    SetLength(StrBuf, (Max(32,
      Max(FODBCConnection.GetMetaData.GetDataBaseInfo.GetMaxTableNameLength,
        Max(FODBCConnection.GetMetaData.GetDataBaseInfo.GetMaxSchemaNameLength,
          Max(FODBCConnection.GetMetaData.GetDataBaseInfo.GetMaxTableNameLength,
            FODBCConnection.GetMetaData.GetDataBaseInfo.GetMaxColumnNameLength))))+1) shl Ord(ConSettings^.ClientCodePage^.Encoding = ceUTF16));

    for ColumnNumber := 1 to fColumnCount do begin
      ColumnInfo := TZODBCColumnInfo.Create;
      ColumnsInfo.Add(ColumnInfo); //add first -> memleaksave
      with ColumnInfo do begin
        if fEnhancedColInfo then begin
          //ColumnInfo.CharOctedLength := ColNumAttribute(ColumnNumber, SQL_DESC_OCTET_LENGTH);
          AutoIncrement := ColNumAttribute(ColumnNumber, SQL_DESC_AUTO_UNIQUE_VALUE) = SQL_TRUE;
          CaseSensitive := ColNumAttribute(ColumnNumber, SQL_DESC_CASE_SENSITIVE) = SQL_TRUE;
          Precision := ColNumAttribute(ColumnNumber, SQL_DESC_DISPLAY_SIZE);
          DESC_NULLABLE := ColNumAttribute(ColumnNumber, SQL_DESC_NULLABLE);
          if DESC_NULLABLE = SQL_NULLABLE then
            Nullable := ntNullable
          else if DESC_NULLABLE = SQL_NO_NULLS then
            Nullable := ntNoNulls
          else
            Nullable := ntNullableUnknown;
          Searchable := ColNumAttribute(ColumnNumber, SQL_DESC_SEARCHABLE) <> SQL_PRED_NONE;
          bufSQLLEN := ColNumAttribute(ColumnNumber, SQL_DESC_UPDATABLE);
          ReadOnly := bufSQLLEN = SQL_ATTR_READONLY;
          Writable := bufSQLLEN <> SQL_ATTR_READONLY;
          DefinitelyWritable := bufSQLLEN = SQL_ATTR_WRITE;

          ColStrAttribute(ColumnNumber, SQL_DESC_LABEL, StrBuf, ColumnLabel);
          ColStrAttribute(ColumnNumber, SQL_DESC_BASE_COLUMN_NAME, StrBuf, ColumnName);
          if ColumnName = '' then
            ColStrAttribute(ColumnNumber, SQL_DESC_NAME, StrBuf, ColumnName);
          if ColumnName <> '' then begin//aggregates like SUM() don't have a columname -> skip processing
            ColStrAttribute(ColumnNumber, SQL_DESC_BASE_TABLE_NAME, StrBuf, TableName);
            if TableName = '' then
              ColStrAttribute(ColumnNumber, SQL_DESC_TABLE_NAME, StrBuf, TableName);
            if TableName <> '' then begin //no table? -> no schema or catalog !
              ColStrAttribute(ColumnNumber, SQL_DESC_SCHEMA_NAME, StrBuf, SchemaName);
              ColStrAttribute(ColumnNumber, SQL_DESC_CATALOG_NAME, StrBuf, CatalogName);
            end;
          end;
          //DefaultValue -> not implemented
          //DefaultExpression -> not implemented
          DESC_CONCISE_TYPE := ColNumAttribute(ColumnNumber, SQL_DESC_CONCISE_TYPE);
          if DESC_CONCISE_TYPE = SQL_TYPE_VARIANT then begin//SQL Server type
            DESC_CONCISE_TYPE := ConvertODBC_CTypeToODBCType(ColNumAttribute(ColumnNumber, SQL_CA_SS_VARIANT_TYPE), Signed);
          end;
          case DESC_CONCISE_TYPE of
            SQL_DATETIME, SQL_TIMESTAMP, SQL_TYPE_TIME, SQL_TYPE_TIMESTAMP,
            SQL_SS_TIME2, SQL_SS_TIMESTAMPOFFSET:
              Scale := ColNumAttribute(ColumnNumber, SQL_DESC_SCALE);
            SQL_NUMERIC, SQL_DECIMAL, SQL_FLOAT, SQL_REAL, SQL_DOUBLE: begin
                Precision := ColNumAttribute(ColumnNumber, SQL_DESC_PRECISION);
                Scale := ColNumAttribute(ColumnNumber, SQL_DESC_SCALE);
                Signed := True;
              end;
            SQL_CHAR, SQL_WCHAR: begin
                FixedWidth := True;
                Scale := Precision;
              end;
            SQL_BINARY: begin
                Precision := ColNumAttribute(ColumnNumber, SQL_DESC_LENGTH);
                Scale := Precision;
                FixedWidth := True;
              end;
            SQL_VARCHAR, SQL_WVARCHAR, SQL_VARBINARY:
                Precision := ColNumAttribute(ColumnNumber, SQL_DESC_LENGTH);
            SQL_TINYINT, SQL_SMALLINT, SQL_INTEGER, SQL_BIGINT:
                Signed := ColNumAttribute(ColumnNumber, SQL_DESC_UNSIGNED) = SQL_FALSE;
          end;

          ColumnType := ConvertODBCTypeToSQLType(DESC_CONCISE_TYPE, Scale, Precision,
              not Signed, ConSettings, @ODBC_CType);
          {numeric data type infos: }
          if (ColumnType in [stDouble, stCurrency]) then
            Currency := IsMoney;
          if ColumnType in [stString, stAsciiStream] then
            ColumnCodePage := FClientCP
          else if ColumnType in [stUnicodeString, stUnicodeStream] then
            ColumnCodePage := zCP_UTF16;

        end else begin
          DescribeColumn(ColumnNumber, StrBuf, ColumnInfo);
          ColumnLabel := ColumnName;
          if (ColumnType in [stString, stUnicodeString, stBytes]) and
             (Precision = 0) then
               ColumnType := TZSQLType(Ord(ColumnType)+3); //switch to streamed mode
        end;
        { calc buf size }
        if Ord(ColumnType) < Ord(stAsciiStream) then begin //streams will be fetched by GetData()
          //if Ord(ColumnType) >= Ord(stString) then
            CharOctedLength := CalcBufSize(Precision, ODBC_CType,
              ColumnType, ConSettings^.ClientCodePage);
          Inc(RowSize, Cardinal(CharOctedLength));
        end else begin
          LobsInResult := True;
          Precision := 0;
          CharOctedLength := 0;
        end;
      end;
    end;
    //GetData don't work with multiple fetched rows for most drivers
    //calculate max count of rows for a single fetch call
    if RowSize > 0 //see https://sourceforge.net/p/zeoslib/tickets/383/
    then fMaxFetchableRows := {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Max(1, (Cardinal(fZBufferSize) div RowSize)*Byte(not LobsInResult))
    else fMaxFetchableRows := 1;
    if fMaxFetchableRows > 1 then begin
      CheckStmtError(fPlainDriver.SQLSetStmtAttr(fPHSTMT^, SQL_ATTR_ROW_ARRAY_SIZE, {%H-}SQLPOINTER(fMaxFetchableRows), 0));
      CheckStmtError(fPlainDriver.SQLSetStmtAttr(fPHSTMT^, SQL_ATTR_ROWS_FETCHED_PTR, @fFetchedRowCount, 0));
    end;
    fFirstGetDataIndex := fColumnCount;
    fLastGetDataIndex := 0;
    for ColumnNumber := 0 to fColumnCount -1 do begin
      ColumnInfo := TZODBCColumnInfo(ColumnsInfo[ColumnNumber]);
      with ColumnInfo do begin
        GetMem(fStrLen_or_IndArray, SizeOf(SQLLEN)*fMaxFetchableRows);
        if CharOctedLength > 0 then //streams will be fetched by GetData()
          GetMem(fColumnBuffer, CharOctedLength*Integer(fMaxFetchableRows));
        Bound := (CharOctedLength > 0) and ((ColumnNumber < fFirstGetDataIndex) or (fSQL_GETDATA_EXTENSIONS and SQL_GD_ANY_COLUMN = SQL_GD_ANY_COLUMN));
        if Bound then
          CheckStmtError(fPlainDriver.SQLBindCol(fPHSTMT^, ColumnNumber+1,
              ODBC_CType, ColumnBuffer, CharOctedLength, PSQLLEN(StrLen_or_IndArray)))
        else begin
          fFirstGetDataIndex := Min(ColumnNumber, fFirstGetDataIndex);
          fLastGetDataIndex := Max(ColumnNumber, fLastGetDataIndex)
        end;
        if (ColumnType in [stBigDecimal, stCurrency]) then begin
          CheckStmtError(FPlainDriver.SQLGetStmtAttr(fPHSTMT^, SQL_ATTR_APP_ROW_DESC, @Desc, 0, nil));
          CheckStmtError(FPlainDriver.SQLSetDescField(Desc, ColumnNumber+1,
            SQL_DESC_CONCISE_TYPE, {%H-}SQLPointer(ODBC_CType), SQL_IS_SMALLINT));
          CheckStmtError(FPlainDriver.SQLSetDescField(Desc, ColumnNumber+1,
            SQL_DESC_PRECISION, {%H-}SQLPointer(Precision), SQL_IS_INTEGER));
          CheckStmtError(FPlainDriver.SQLSetDescField(Desc, ColumnNumber+1,
            SQL_DESC_SCALE, {%H-}SQLPointer(Scale), SQL_IS_SMALLINT));
          if Bound then CheckStmtError(FPlainDriver.SQLSetDescField(Desc, ColumnNumber+1,
            SQL_DESC_DATA_PTR, ColumnBuffer, SQL_IS_POINTER));
        end;
        (*end else begin
          { improve Invalid descriptor index error .. }
          if (fSQL_GETDATA_EXTENSIONS and SQL_GD_BOUND = SQL_GD_BOUND ) then //E: (DM) The specified column was bound.
            if (fSQL_GETDATA_EXTENSIONS and SQL_GD_ANY_COLUMN = SQL_GD_ANY_COLUMN ) //E: (DM) The number of the specified column was less than or equal to the number of the highest bound column
                or NoStreamedColFollows then begin
              CheckStmtError(fPlainDriver.SQLBindCol(fPHSTMT^, ColumnNumber+1,
                ODBC_CType, nil, SQL_DATA_AT_EXEC, PSQLLEN(StrLen_or_IndArray)));
              Bound := True;
            end;
        end;*)
      end;
    end;
    inherited Open;
  end;
  FCursorLocation := rctServer;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TAbstractColumnODBCResultSet.ResetCursor;
begin
  if Assigned(fPHSTMT^) and fCursorOpened then begin
    CheckStmtError(fPlainDriver.SQLCloseCursor(fPHSTMT^)); //close cursor and discard pending result
    fCursorOpened := False;
  end;
  fFetchedRowCount := 0;
  inherited ResetCursor;
end;

{ TODBCResultSetW }

function TODBCResultSetW.ColNumAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT): SQLLEN;
begin
  Result := 0; //init see docs
  CheckStmtError(fPlainW.SQLColAttributeW(fPHSTMT^, ColumnNumber, FieldIdentifier,
      nil, 0, nil, @Result));
end;

procedure TODBCResultSetW.ColStrAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray; var Result: String);
var
  StringLength: SQLSMALLINT;
begin
  StringLength := 0;
  CheckStmtError(fPlainW.SQLColAttributeW(fPHSTMT^, ColumnNumber, FieldIdentifier,
      Pointer(Buf), Length(Buf), @StringLength, nil));
  if StringLength > 0 then
    {$IFDEF UNICODE}
    System.SetString(Result, PWideChar(Pointer(Buf)), StringLength shr 1)
    {$ELSE}
    PUnicodeToRaw(PWideChar(Pointer(Buf)), StringLength shr 1, zCP_UTF8, RawByteString(Result))
    {$ENDIF}
  else Result := '';
end;

constructor TODBCResultSetW.Create(const Statement: IZStatement;
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; const SQL: String;
  const Connection: IZODBCConnection; ZBufferSize: Integer;
  const EnhancedColInfo: Boolean);
begin
  fPlainW := Connection.GetPLainDriver.GetInstance as TODBC3UnicodePlainDriver;
  inherited Create(Statement, StmtHandle, ConnectionHandle, SQL, Connection,
    ZBufferSize, EnhancedColInfo);
end;

procedure TODBCResultSetW.DescribeColumn(ColumnNumber: SQLUSMALLINT;
  const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo);
var
  NameLength, DataType, DecimalDigits, Nullable: SQLSMALLINT;
  ColumnSize: SQLULEN;
  {$IFDEF UNICODE}
  ColName: String;
  {$ENDIF}
begin
  CheckStmtError(fPlainW.SQLDescribeColW(fPHSTMT^, ColumnNumber, Pointer(Buf), Length(Buf),
    @NameLength, @DataType, @ColumnSize, @DecimalDigits, @Nullable));
  if NameLength > 0 then begin
    {$IFDEF UNICODE}
    System.SetString(ColName, PWideChar(Pointer(Buf)), NameLength);
    ColumnInfo.ColumnName := ColName;
    {$ELSE}
    ColumnInfo.ColumnName := PUnicodeToRaw(Pointer(Buf), NameLength, FClientCP);
    {$ENDIF}
  end;
  ColumnInfo.Precision := ColumnSize;
  ColumnInfo.Scale := DecimalDigits;
  if Nullable = SQL_NULLABLE then
    ColumnInfo.Nullable := ntNullable
  else if Nullable = SQL_NO_NULLS then
    ColumnInfo.Nullable := ntNoNulls
  else
    ColumnInfo.Nullable := ntNullableUnknown;
  ColumnInfo.ColumnType := ConvertODBCTypeToSQLType(DataType,
    DecimalDigits, ColumnSize, False, ConSettings, @ColumnInfo.ODBC_CType);
  if ColumnInfo.ColumnType in [stString, stUnicodeString] then
    if Ord(ConSettings^.ClientCodePage^.Encoding) >= Ord(ceUTF16)
    then ColumnInfo.ColumnCodePage := zCP_UTF16
    else ColumnInfo.ColumnCodePage := FClientCP;
end;

{ TODBCResultSetA }

function TODBCResultSetA.ColNumAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT): SQLLEN;
begin
  Result := 0; //init see docs
  CheckStmtError(fPlainA.SQLColAttribute(fPHSTMT^, ColumnNumber, FieldIdentifier,
      nil, 0, nil, @Result));
end;

procedure TODBCResultSetA.ColStrAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray; var Result: String);
var
  StringLength: SQLSMALLINT;
begin
  StringLength := 0;
  CheckStmtError(fPlainA.SQLColAttribute(fPHSTMT^, ColumnNumber, FieldIdentifier,
       Pointer(Buf), Length(Buf), @StringLength, nil));
  if StringLength > 0 then
    {$IFDEF UNICODE}
    PRawToUnicode(PAnsiChar(Pointer(Buf)), StringLength, FClientCP, Result)
    {$ELSE}
    System.SetString(Result, PAnsiChar(Pointer(Buf)), StringLength)
    {$ENDIF}
  else Result := '';
end;

constructor TODBCResultSetA.Create(const Statement: IZStatement;
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; const SQL: String;
  const Connection: IZODBCConnection; ZBufferSize: Integer;
  const EnhancedColInfo: Boolean);
begin
  fPlainA := Connection.GetPLainDriver.GetInstance as TODBC3RawPlainDriver;
  inherited Create(Statement, StmtHandle, ConnectionHandle, SQL, Connection,
    ZBufferSize, EnhancedColInfo);
end;

procedure TODBCResultSetA.DescribeColumn(ColumnNumber: SQLUSMALLINT;
  const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo);
var
  {$IFNDEF UNICODE}
  ColumnName: String;
  {$ENDIF}
  NameLength, DataType, DecimalDigits, Nullable: SQLSMALLINT;
  ColumnSize: SQLULEN;
begin
  CheckStmtError(fPlainA.SQLDescribeCol(fPHSTMT^, ColumnNumber, Pointer(Buf), LEngth(Buf),
    @NameLength, @DataType, @ColumnSize, @DecimalDigits, @Nullable));
  if NameLength > 0 then begin
    {$IFDEF UNICODE}
    PRawToUnicode(Pointer(Buf), NameLength, FClientCP, ColumnInfo.ColumnName);
    {$ELSE}
    SetString(ColumnName, PAnsiChar(Pointer(Buf)), NameLength);
    ColumnInfo.ColumnName := ColumnName;
    {$ENDIF}
  end;
  ColumnInfo.Precision := ColumnSize;
  ColumnInfo.Scale := DecimalDigits;
  if Nullable = SQL_NULLABLE then
    ColumnInfo.Nullable := ntNullable
  else if Nullable = SQL_NO_NULLS then
    ColumnInfo.Nullable := ntNoNulls
  else
    ColumnInfo.Nullable := ntNullableUnknown;
  ColumnInfo.ColumnType := ConvertODBCTypeToSQLType(DataType,
    DecimalDigits, ColumnSize, False, ConSettings, @ColumnInfo.ODBC_CType);
  if ColumnInfo.ColumnType in [stString, stUnicodeString] then
    if Ord(ConSettings^.ClientCodePage^.Encoding) >= Ord(ceUTF16)
    then ColumnInfo.ColumnCodePage := zCP_UTF16
    else ColumnInfo.ColumnCodePage := FClientCP;
end;

{ TZODBCResultSetMetadata }

procedure TZODBCResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  ColumnInfo.ColumnName := '';
//  !!!skip this brings the detailed information through until someone has an idea
//  how to improve the missing UnCachedGetColumns information!
end;

{ TZODBCBlob }

constructor TZODBCBlob.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN;  const PlainDriver: TZODBC3PlainDriver;
  const OpenLobStreams: TZSortedList);
const
  MaxBufSize = $F000;
var
  OffSetPtr: PAnsiChar;
  i: Integer;
  L: Cardinal absolute I;
begin
  inherited Create(OpenLobStreams);
  if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FDataRefAddress.IsNotNull := 0
  else begin
    FDataRefAddress.IsNotNull := 1;
    if StrLen_or_IndPtr^ = 0 then Exit;
    if StrLen_or_IndPtr^ > 0 then begin
      SetCapacity(StrLen_or_IndPtr^);
      OffSetPtr := @FDataRefAddress.VarLenData.Data;
      for i := 1 to StrLen_or_IndPtr^ div MaxBufSize do begin
        Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.SQLGetData(StmtHandle, ColumnNumber,
          SQL_C_BINARY, OffSetPtr, MaxBufSize, StrLen_or_IndPtr));
        Inc(OffSetPtr, MaxBufSize);
      end;
      Assert(PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, MaxBufSize, StrLen_or_IndPtr) = SQL_SUCCESS);
    end else if StrLen_or_IndPtr^ = SQL_NO_TOTAL then begin
      SetCapacity(MaxBufSize);
      OffSetPtr := @FDataRefAddress.VarLenData.Data;
      while (PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, MaxBufSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
        L := FDataRefAddress.VarLenData.Len;
        SetCapacity(L+MaxBufSize);
        OffSetPtr := PAnsiChar(@FDataRefAddress.VarLenData.Data)+L;
      end;
      SetCapacity(FDataRefAddress.VarLenData.Len - MaxBufSize + NativeUInt(StrLen_or_IndPtr^));
    end else
      Assert(StrLen_or_IndPtr^ = 0)
  end;
end;

{ TZODBCClobA }

constructor TZODBCClobA.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN; const PlainDriver: TZODBC3PlainDriver;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList);
const
  MaxBufSize = $F000;
var
  OffSetPtr: PAnsiChar;
  i: Integer;
  L: Cardinal absolute I;
begin
  inherited Create(ConSettings^.ClientCodePage^.CP, ConSettings, OpenLobStreams);
  if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FDataRefAddress.IsNotNull := 0
  else begin
    FDataRefAddress.IsNotNull := 1;
    { truncated string data always have a trailing #0 on top of data }
    if StrLen_or_IndPtr^ = 0 then Exit;
    if StrLen_or_IndPtr^ >= 0 then begin
      SetCapacity(StrLen_or_IndPtr^);
      OffSetPtr := @FDataRefAddress^.VarLenData.Data;
      for i := 1 to StrLen_or_IndPtr^ div MaxBufSize do begin
        Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, MaxBufSize, StrLen_or_IndPtr));
        Inc(OffSetPtr, (MaxBufSize-SizeOf(AnsiChar)));
      end;
      Assert(PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, MaxBufSize, StrLen_or_IndPtr) = SQL_SUCCESS);
    end else begin
      Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
      SetCapacity(MaxBufSize);
      OffSetPtr := @FDataRefAddress.VarLenData.Data;
      while (PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, MaxBufSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
        L := FDataRefAddress.VarLenData.Len -SizeOf(AnsiChar);
        SetCapacity(L+MaxBufSize);
        OffSetPtr := PAnsiChar(@FDataRefAddress.VarLenData.Data)+L;
      end;
      SetCapacity(FDataRefAddress.VarLenData.Len - MaxBufSize + NativeUInt(StrLen_or_IndPtr^));
    end;
    PByte(PAnsiChar(@FDataRefAddress.VarLenData.Data)+FDataRefAddress.VarLenData.Len)^ := 0;
  end;
end;

{ TZODBCClobW }

constructor TZODBCClobW.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN; const PlainDriver: TZODBC3PlainDriver;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList);
const
  MaxBufSize = $F000;
var
  OffSetPtr: PAnsiChar;
  I: Integer;
  L: Cardinal absolute I;
begin
  inherited Create(zCP_UTF16, ConSettings, OpenLobStreams);
  if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FDataRefAddress.IsNotNull := 0
  else begin
    FDataRefAddress.IsNotNull := 1;
    if StrLen_or_IndPtr^ = 0 then Exit;
    { truncated string data always have a trailing #0 on top of data }
    if StrLen_or_IndPtr^ >= 0 then begin
      SetCapacity(StrLen_or_IndPtr^);
      OffSetPtr := @FDataRefAddress^.VarLenData.Data;
      for i := 1 to StrLen_or_IndPtr^ div MaxBufSize do begin
        Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, MaxBufSize, StrLen_or_IndPtr));
        Inc(OffSetPtr, (MaxBufSize-SizeOf(WideChar)));
      end;
      Assert(PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, MaxBufSize, StrLen_or_IndPtr) = SQL_SUCCESS);
    end else begin
      Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
      SetCapacity(MaxBufSize);
      OffSetPtr := @FDataRefAddress.VarLenData.Data;
      while (PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, MaxBufSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
        L := FDataRefAddress.VarLenData.Len -SizeOf(WideChar);
        SetCapacity(L+MaxBufSize);
        OffSetPtr := PAnsiChar(@FDataRefAddress.VarLenData.Data)+L;
      end;
      SetCapacity(FDataRefAddress.VarLenData.Len - MaxBufSize + NativeUInt(StrLen_or_IndPtr^));
    end;
    PWord(PAnsiChar(@FDataRefAddress.VarLenData.Data)+FDataRefAddress.VarLenData.Len)^ := 0;
  end;
end;

{ TZODBCColumnInfo }

destructor TZODBCColumnInfo.Destroy;
begin
  if (fColumnBuffer <> nil) then
    if (CharOctedLength > 0)  then
      FreeMem(fColumnBuffer)
    else IZBLob(fColumnBuffer) := nil;
  if (fStrLen_or_IndArray <> nil) then
    FreeMem(fStrLen_or_IndArray);
  inherited;
end;

{ TZODBCOutParamColumnInfo }

destructor TZODBCOutParamColumnInfo.Destroy;
begin
  fColumnBuffer := nil;
  fStrLen_or_IndArray := nil;
  inherited;
end;

{ TZParamODBCResultSet }

constructor TZParamODBCResultSet.Create(const Statement: IZStatement; const SQL: String;
  BindList: TZBindList);
var I: Integer;
    ColumnInfo: TZODBCOutParamColumnInfo;
    BindValue: PZBindValue;
    Bind: PZODBCBindValue absolute BindValue;
begin
  with Statement.GetConnection do begin
    inherited Create(Statement, SQL, nil, BindList.ConSettings);
    fIsUnicodeDriver := Supports(GetIZPlainDriver, IODBC3UnicodePlainDriver);
  end;
  ColumnsInfo.Clear;
  for i := 0 to BindList.Count -1 do begin
    BindValue := BindList[I];
    if Bind.InputOutputType in [SQL_PARAM_INPUT_OUTPUT, SQL_RESULT_COL,
      SQL_PARAM_OUTPUT, SQL_RETURN_VALUE, SQL_PARAM_INPUT_OUTPUT_STREAM,
      SQL_PARAM_OUTPUT_STREAM] then begin
      ColumnInfo := TZODBCOutParamColumnInfo.Create;
      ColumnInfo.fODBC_CType :=  Bind.ValueType;
      ColumnInfo.fColumnBuffer := Bind.ParameterValuePtr;
      ColumnInfo.fStrLen_or_IndArray := PSQLLENArray(Bind.StrLen_or_IndPtr);
      ColumnInfo.ColumnLabel := Bind.ParamName;
      ColumnInfo.ColumnType := BindValue.SQLType;
      ColumnInfo.Precision := Bind.ColumnSize;
      ColumnInfo.Scale := Bind.DecimalDigits;
      ColumnsInfo.Add(ColumnInfo);
      Inc(fColumnCount);
    end;
  end;
  SetType(rtForwardOnly);
  LastRowNo := 1;
  Inherited Open;
  FCursorLocation := rctClient;
end;

function TZParamODBCResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := not Closed and ((Row = 1) or (Row = 0));
  if (Row >= 0) and (Row <= 2) then
    RowNo := Row;
end;

function TZParamODBCResultSet.Next: Boolean;
begin
  Result := not Closed and (RowNo = 0);
  if RowNo = 0 then
    RowNo := 1
  else if RowNo = 1 then
    RowNo := 2; //set AfterLast
  FCurrentBufRowNo := 1;
end;

{ TZODBCachedResultSetW }

class function TZODBCachedResultSetW.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZODBCRowAccessorW;
end;

{ TZODBCRowAccessorW }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobCacheMode" not used} {$ENDIF}
constructor TZODBCRowAccessorW.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  LobCacheMode: TLobCacheMode);
begin
  inherited Create(ColumnsInfo, ConSettings, OpenLobStreams, lcmOnLoad); //we can not use uncached lobs with ODBC
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ConSettings" not used} {$ENDIF}
class function TZODBCRowAccessorW.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType;
begin
  Result := ColumnInfo.ColumnType;
  if Result in [stString, stAsciiStream] then
    Result := TZSQLType(Byte(Result)+1); // no raw chars in 4 odbc_w
  if Result in [stUnicodeString, stUnicodeStream] then
    ColumnCodePage := zCP_UTF16
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZODBCRowAccessorA }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobCacheMode" not used} {$ENDIF}
constructor TZODBCRowAccessorA.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  LobCacheMode: TLobCacheMode);
begin
  inherited Create(ColumnsInfo, ConSettings, OpenLobStreams, lcmOnLoad); //we can not use uncached lobs with ODBC
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZODBCachedResultSetA }

class function TZODBCachedResultSetA.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZODBCRowAccessorA;
end;

class function TZODBCRowAccessorA.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType;
begin
  Result := ColumnInfo.ColumnType;
  if Result in [stUnicodeString, stUnicodeStream] then
    Result := TZSQLType(Byte(Result)-1); // no national chars in 4 odbc_a
  if Result in [stString, stAsciiStream] then
    ColumnCodePage := Consettings.ClientCodePage.CP
end;

initialization
{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
end.
