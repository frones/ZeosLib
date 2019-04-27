{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           ODBC Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcODBCResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, {$IFDEF BCD_TEST}FmtBCD,{$ENDIF}
  ZSysUtils, ZDbcIntfs,
  ZCompatibility, ZDbcResultSet, ZFastCode, ZDbcResultsetMetadata,
  ZPlainODBCDriver, ZDbcODBCCon;

type
  { eh: improve missing meta informations of SQLColumns}
  TODBCTResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;
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

  TAbstractODBCResultSet = Class(TZAbstractReadOnlyResultSet, IZResultSet)
  private
    fPHSTMT: PSQLHSTMT; //direct reference the handle of the smt/metadata
    fConnection: IZODBCConnection;
    fPlainDriver: TZODBC3PlainDriver;
    fZBufferSize, fChunkSize: Integer;
    fEnhancedColInfo, FIsMetaData: Boolean;
    fColumnCount: SQLSMALLINT;
    fMaxFetchableRows, fFetchedRowCount, fCurrentBufRowNo: SQLULEN;
    fStrLen_or_Ind: SQLLEN;
    fColDataPtr: Pointer;
    //fSQLTypes: TZSQLTypeArray;
    fIsUnicodeDriver: Boolean;
    fFreeHandle, fCursorOpened: Boolean;
    fSQL_GETDATA_EXTENSIONS: SQLUINTEGER;
    fFirstGetDataIndex, fLastGetDataIndex: Integer;
    FTinyBuffer: array[Byte] of Byte;
    FClientCP: Word;
    procedure LoadUnBoundColumns;
  protected
    procedure CheckStmtError(RETCODE: SQLRETURN);
    function ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String; virtual; abstract;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; virtual; abstract;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo); virtual; abstract;
  public
    constructor Create(const Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; const SQL: String; const Connection: IZODBCConnection;
      ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean = True); virtual;
    constructor CreateForMetadataCall(out StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC;
      {$IFNDEF FPC}const{$ENDIF} Connection: IZODBCConnection); virtual; //fpc skope for (GetConneaction as IZODBCConnection) is different to dephi and crashs
    procedure Open; override;
    procedure BeforeClose; override;

    function Next: Boolean; reintroduce;
    procedure ResetCursor; override;

    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    function IsNull(ColumnIndex: Integer): Boolean;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetBoolean(ColumnIndex: Integer): Boolean;
    function GetUInt(ColumnIndex: Integer): LongWord;
    function GetInt(ColumnIndex: Integer): Integer;
    function GetULong(ColumnIndex: Integer): UInt64;
    function GetLong(ColumnIndex: Integer): Int64;
    function GetFloat(ColumnIndex: Integer): Single;
    function GetDouble(ColumnIndex: Integer): Double;
    function GetCurrency(ColumnIndex: Integer): Currency;
    {$IFDEF BCD_TEST}
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    {$ELSE}
    function GetBigDecimal(ColumnIndex: Integer): Extended;
    {$ENDIF}
    function GetBytes(ColumnIndex: Integer): TBytes;
    function GetDate(ColumnIndex: Integer): TDateTime;
    function GetTime(ColumnIndex: Integer): TDateTime;
    function GetTimestamp(ColumnIndex: Integer): TDateTime;
    function GetBlob(ColumnIndex: Integer): IZBlob;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF USE_SYNCOMMONS}
  End;

  TODBCResultSetW = class(TAbstractODBCResultSet)
  private
    fPlainW: TODBC3UnicodePlainDriver;
  protected
    function ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String; override;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; override;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo); override;
  public
    constructor Create(const Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; const SQL: String; const Connection: IZODBCConnection;
      ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean = True); override;
  end;

  TODBCResultSetA = class(TAbstractODBCResultSet)
  private
    fPlainA: TODBC3RawPlainDriver;
  protected
    function ColStrAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String; override;
    function ColNumAttribute(ColumnNumber, FieldIdentifier: SQLUSMALLINT): SQLLEN; override;
    procedure DescribeColumn(ColumnNumber: SQLUSMALLINT; const Buf: TByteDynArray; var ColumnInfo: TZODBCColumnInfo); override;
  public
    constructor Create(const Statement: IZStatement; var StmtHandle: SQLHSTMT;
      ConnectionHandle: SQLHDBC; const SQL: String; const Connection: IZODBCConnection;
      ZBufferSize, ChunkSize: Integer; const EnhancedColInfo: Boolean = True); override;
  end;

  TZODBCBlob = class(TZAbstractBlob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; const PlainDriver: TZODBC3PlainDriver);
  end;

  TZODBCClobA = class(TZAbstractCLob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; const PlainDriver: TZODBC3PlainDriver;
      ConSettings: PZConSettings);
  end;

  TZODBCClobW = class(TZAbstractCLob)
  public
    constructor Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
      StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; const PlainDriver: TZODBC3PlainDriver;
      ConSettings: PZConSettings);
  end;

const
  StringStreamTypes: Array[Boolean] of SQLSMALLINT = (SQL_C_CHAR, SQL_C_WCHAR);

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit

uses Math,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZMessages, ZDbcODBCUtils, ZEncoding, ZDbcProperties,
  ZClasses, ZDbcUtils;

{ TAbstractODBCResultSet }

procedure TAbstractODBCResultSet.CheckStmtError(RETCODE: SQLRETURN);
  procedure HandleError;
  begin
    if Statement <> nil
    then CheckODBCError(RETCODE, fPHSTMT^, SQL_HANDLE_STMT, Statement.GetSQL, Self, fConnection)
    else CheckODBCError(RETCODE, fPHSTMT^, SQL_HANDLE_STMT, 'MetaData-call', Self, fConnection);
  end;
begin
  if RETCODE <> SQL_SUCCESS then
    HandleError;
end;

procedure TAbstractODBCResultSet.BeforeClose;
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

{$IFDEF USE_SYNCOMMONS}
procedure TAbstractODBCResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var C, H, I: Integer;
    P: Pointer;
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
        stCurrency:   JSONWriter.AddCurr64(ODBCNumeric2Curr(fColDataPtr));
        stDouble,
        stBigDecimal: JSONWriter.AddDouble(PDouble(fColDataPtr)^);
        stBytes:      JSONWriter.WrBase64(fColDataPtr,fStrLen_or_Ind,True);
        stGUID:       begin
                        JSONWriter.Add('"');
                        JSONWriter.Add(PGUID(fColDataPtr)^);
                        JSONWriter.Add('"');
                      end;
        stTime:       begin
                        if jcoMongoISODate in JSONComposeOptions then
                          JSONWriter.AddShort('ISODate("0000-00-00')
                        else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                          JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                        end else
                          JSONWriter.Add('"');
                        if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2) then
                          TimeToIso8601PChar(@FTinyBuffer[0], True, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                            PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second,
                            PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000, 'T', jcoMilliseconds in JSONComposeOptions)
                        else
                          TimeToIso8601PChar(@FTinyBuffer[0], True, PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                            PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0, 'T', jcoMilliseconds in JSONComposeOptions);
                        JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],8+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                        if jcoMongoISODate in JSONComposeOptions
                        then JSONWriter.AddShort('Z)"')
                        else JSONWriter.Add('"');
                      end;
        stDate:       begin
                        if jcoMongoISODate in JSONComposeOptions then
                          JSONWriter.AddShort('ISODate("')
                        else if jcoDATETIME_MAGIC in JSONComposeOptions then
                          JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                        else
                          JSONWriter.Add('"');
                        if PSQL_DATE_STRUCT(fColDataPtr)^.year < 0 then
                          JSONWriter.Add('-');
                        DateToIso8601PChar(@FTinyBuffer[0], True, Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                          PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
                        JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],10);
                        if jcoMongoISODate in JSONComposeOptions
                        then JSONWriter.AddShort('Z")')
                        else JSONWriter.Add('"');
                      end;
        stTimeStamp:  begin
                        if jcoMongoISODate in JSONComposeOptions then
                          JSONWriter.AddShort('ISODate("')
                        else if jcoDATETIME_MAGIC in JSONComposeOptions then
                          JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                        else
                          JSONWriter.Add('"');
                        if PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year < 0 then
                          JSONWriter.Add('-');
                        DateToIso8601PChar(@FTinyBuffer[0], True, Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
                          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day);
                        TimeToIso8601PChar(@FTinyBuffer[10], True, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute,
                          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction, 'T', jcoMilliseconds in JSONComposeOptions);
                        JSONWriter.AddNoJSONEscape(@FTinyBuffer[0],19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
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
                FUniTemp := PRawToUnicode(fColDataPtr, fStrLen_or_Ind, ConSettings^.ClientCodePage^.CP);
                JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
              end;
            end;
            JSONWriter.Add('"');
          end;
        stAsciiStream, stUnicodeStream: begin
            JSONWriter.Add('"');
            if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or (ConSettings^.ClientCodePage^.CP <> zCP_UTF8) then begin
              P := IZBlob(fColDataPtr).GetPWideChar;
              JSONWriter.AddJSONEscapeW(P, IZBlob(fColDataPtr).Length shr 1);
            end else begin
              P := IZBlob(fColDataPtr).GetPAnsiChar(zCP_UTF8);
              JSONWriter.AddJSONEscape(P, IZBlob(fColDataPtr).Length);
            end;
            JSONWriter.Add('"');
          end;
        stBinaryStream:
          JSONWriter.WrBase64(IZBlob(fColDataPtr).GetBuffer, IZBlob(fColDataPtr).Length, True);
        else //stArray, stDataSet:
          JSONWriter.AddShort('null,') ;
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

constructor TAbstractODBCResultSet.Create(const Statement: IZStatement;
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; const SQL: String;
  const Connection: IZODBCConnection; ZBufferSize, ChunkSize: Integer;
  const EnhancedColInfo: Boolean = True);
var Supported: SQLUSMALLINT;
begin
  inherited Create(Statement, SQL, TODBCTResultSetMetadata.Create(Connection.GetMetadata, SQL, Self), Connection.GetConSettings);
  fConnection := Connection;
  fPlainDriver := TZODBC3PlainDriver(fConnection.GetPlainDriver.GetInstance);
  fIsUnicodeDriver := Supports(fPlainDriver, IODBC3UnicodePlainDriver);
  fPHSTMT := @StmtHandle;
  fZBufferSize := ZBufferSize;
  fChunkSize := ChunkSize;
  fConnection.CheckDbcError(fPLainDriver.SQLGetFunctions(ConnectionHandle, SQL_API_SQLCOLATTRIBUTE, @Supported));
  fEnhancedColInfo := EnhancedColInfo and (Supported = SQL_TRUE);
  fCurrentBufRowNo := 0;
  fFreeHandle := not Assigned(StmtHandle);
  Connection.CheckDbcError(fPlainDriver.SQLGetInfo(ConnectionHandle,
    SQL_GETDATA_EXTENSIONS, @fSQL_GETDATA_EXTENSIONS, SizeOf(SQLUINTEGER), nil));
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;
  fCursorOpened := True;
  FClientCP := ConSettings^.ClientCodePage.CP;
  Open;
end;

constructor TAbstractODBCResultSet.CreateForMetadataCall(
  out StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; {$IFNDEF FPC}const{$ENDIF} Connection: IZODBCConnection);
begin
  FIsMetaData := True;
  StmtHandle := nil;
  Create(nil, StmtHandle, ConnectionHandle, '', Connection,
    {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Connection.GetParameters.Values[DSProps_InternalBufSize], 131072), //by default 128KB
    {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Connection.GetParameters.Values[DSProps_ChunkSize], 4096), False);
  Connection.CheckDbcError(fPlainDriver.SQLAllocHandle(SQL_HANDLE_STMT, ConnectionHandle, StmtHandle));
end;
{$IFNDEF NO_ANSISTRING}
function TAbstractODBCResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var P: Pointer;
  L: NativeUInt;
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
                          Result := PUnicodeToRaw(fColDataPtr, L, zOSCodePage);
                        end else begin
                          L := fStrLen_or_Ind;
                          if FixedWidth then
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                          if FClientCP = zOSCodePage then
                            System.SetString(Result, PAnsiChar(fColDataPtr), L)
                          else begin
                            FUniTemp := PRawToUnicode(fColDataPtr, l, FClientCP);
                            Result := PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), zOSCodePage);
                          end;
                        end;
        end;
      stAsciiStream, stUnicodeStream:
        Result := GetBlob(ColumnIndex).GetAnsiString;
      else Result := '';
    end;
  end;
end;
{$ENDIF}

{$IFDEF BCD_TEST}
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
      stDouble:     DoubleToBCD(GetDouble(ColumnIndex), Result);
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
{$ELSE}
function TAbstractODBCResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  Result := GetDouble(ColumnIndex);
{$ENDIF}
end;

function TAbstractODBCResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
  Result := nil;
  if not IsNull(ColumnIndex) then
    with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin //loads the lob on demand -> a second call is impossible
    if ColumnType in [stBinaryStream, stAsciiStream, stUnicodeStream]
    then Result := IZBlob(ColumnBuffer)
    else Result := TZAbstractBlob.Create;
  end;
end;

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
    end;
  end;
end;

function TAbstractODBCResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
  if IsNull(ColumnIndex) then //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
    Result := nil
  else if fColDataPtr = nil then //streamed data
    Result := nil
  else begin
    Result := BufferToBytes(
      fColDataPtr,
      fStrLen_or_Ind shl Ord((TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType in [stString, stUnicodeString]) and fIsUnicodeDriver));
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

function TAbstractODBCResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
  L: LengthInt;
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
      stDate:       Result := EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                      PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day);
      stTimeStamp:  Result := EncodeDate(Abs(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.year),
                      PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.day);
      stString,
      stUnicodeString: begin
                if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  Result := UnicodeSQLDateToDateTime(fColDataPtr, L, ConSettings^.ReadFormatSettings, Failed);
                end else begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := RawSQLDateToDateTime(fColDataPtr, L, ConSettings^.ReadFormatSettings, Failed);
                end;
                if Failed then
                  LastWasNull := True;
              end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0;
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

function TAbstractODBCResultSet.GetInt(ColumnIndex: Integer): Integer;
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
                      IntToRaw(Cardinal(PByte(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stShort:      begin
                      IntToRaw(Integer(PShortInt(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stWord:        begin
                      IntToRaw(Cardinal(PWord(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stSmall:      begin
                      IntToRaw(Integer(PSmallInt(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stLongWord:   begin
                      IntToRaw(PCardinal(fColDataPtr)^, @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stInteger:    begin
                      IntToRaw(PInteger(fColDataPtr)^, @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stULong:      begin
                      IntToRaw(PUInt64(fColDataPtr)^, @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stLong:       begin
                      IntToRaw(PInt64(fColDataPtr)^, @FTinyBuffer[0], @Result);
Set_Results:          Len := Result - PAnsiChar(@FTinyBuffer[0]);
                      Result := @FTinyBuffer[0];
                    end;
      stFloat:      begin
                      Len := FloatToSqlRaw(PSingle(fColDataPtr)^, @FTinyBuffer[0]);
                      Result := @FTinyBuffer[0];
                    end;
      stCurrency:   begin
                      Result := @FTinyBuffer[SizeOf(Pointer)];
                      CurrToRaw(ODBCNumeric2Curr(fColDataPtr), Result, @FTinyBuffer[0]);
                      Len := Result-PPAnsiChar(@FTinyBuffer[0])^;
                    end;
      stBigDecimal{$IFDEF BCD_TEST}: begin
                      Result := @FTinyBuffer[0];
                      Len := SQL_MAX_NUMERIC_LEN;
                      SQLNumeric2Raw(fColDataPtr, Result, Len);
                    end;
      {$ELSE},{$ENDIF}
      stDouble: begin
                      Len := FloatToSqlRaw(PDouble(fColDataPtr)^, @FTinyBuffer[0]);
                      Result := @FTinyBuffer[0];
                    end;
      stBytes:      begin
                      Result := fColDataPtr;
                      len := fStrLen_or_Ind;
                    end;
      stGUID:       begin
                      GUIDToBuffer(fColDataPtr, PWideChar(@FTinyBuffer[0]), [guidWithBrackets]);
                      Result := @FTinyBuffer[0];
                      Len := 38;
                    end;
      stTime:       begin
                      Result := @FTinyBuffer[0];
                      if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2)
                      then Len := DateTimeToRawSQLTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second,
                          PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000, Result,
                          ConSettings^.DisplayFormatSettings.TimeFormat, False)
                      else Len := DateTimeToRawSQLTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0, Result,
                          ConSettings^.DisplayFormatSettings.TimeFormat, False);
                    end;
      stDate:       begin
                      DateTimeToRawSQLDate(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                        PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day), @FTinyBuffer[0],
                          ConSettings^.DisplayFormatSettings, False);
                      Result := @FTinyBuffer[0];
                      Len := ConSettings^.DisplayFormatSettings.DateFormatLen;
                    end;
      stTimeStamp:  begin
                      Result := @FTinyBuffer[0];
                      Len := DateTimeToRawSQLTimeStamp(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                        PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction, Result,
                          ConSettings^.DisplayFormatSettings.DateTimeFormat,
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
                      if IZBlob(ColumnBuffer).IsCLob then begin
                        Result := IZBlob(ColumnBuffer).GetPAnsiChar(FClientCP);
                        Len := IZBlob(ColumnBuffer).Length;
                      end else begin
                        Result := IZBlob(ColumnBuffer).GetBuffer;
                        Len := IZBlob(ColumnBuffer).Length;
                      end;
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
                      IntToUnicode(Cardinal(PByte(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stShort:      begin
                      IntToUnicode(Integer(PShortInt(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stWord:        begin
                      IntToUnicode(Cardinal(PWord(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stSmall:      begin
                      IntToUnicode(Integer(PSmallInt(fColDataPtr)^), @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stLongWord:   begin
                      IntToUnicode(PCardinal(fColDataPtr)^, @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stInteger:    begin
                      IntToUnicode(PInteger(fColDataPtr)^, @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stULong:      begin
                      IntToUnicode(PUInt64(fColDataPtr)^, @FTinyBuffer[0], @Result);
                      goto Set_Results;
                    end;
      stLong:       begin
                      IntToUnicode(PInt64(fColDataPtr)^, @FTinyBuffer[0], @Result);
Set_Results:          Len := Result - PWideChar(@FTinyBuffer[0]);
                      Result := @FTinyBuffer[0];
                    end;
      stFloat:      begin
                      Len := FloatToSqlUnicode(PSingle(fColDataPtr)^, @FTinyBuffer[0]);
                      Result := @FTinyBuffer[0];
                    end;
      stCurrency:   begin
                      Result := @FTinyBuffer[SizeOf(Pointer)];
                      CurrToUnicode(ODBCNumeric2Curr(fColDataPtr), Result, @FTinyBuffer[0]);
                      Len := ZPPWideChar(@FTinyBuffer[0])^-Result;
                    end;

      stBigDecimal{$IFDEF BCD_TEST}: begin
                      Result := @FTinyBuffer[0];
                      Len := SQL_MAX_NUMERIC_LEN;
                      SQLNumeric2Uni(fColDataPtr, Result, Len);
                    end;
      {$ELSE},
      {$ENDIF}
      stDouble: begin
                      Len := FloatToSqlUnicode(PDouble(fColDataPtr)^, @FTinyBuffer[0]);
                      Result := @FTinyBuffer[0];
                    end;
      stBytes:      begin
                      fUniTemp := Ascii7ToUnicodeString(fColDataPtr, fStrLen_or_Ind);
                      goto Set_From_Temp;
                    end;
      stGUID:       begin
                      GUIDToBuffer(fColDataPtr, PWideChar(@FTinyBuffer[0]), [guidWithBrackets]);
                      Result := @FTinyBuffer[0];
                      Len := 38;
                    end;
      stTime:       begin
                      Result := @FTinyBuffer[0];
                      if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType = SQL_C_SS_TIME2)
                      then Len := DateTimeToUnicodeSQLTime(PSQL_SS_TIME2_STRUCT(fColDataPtr)^.hour,
                        PSQL_SS_TIME2_STRUCT(fColDataPtr)^.minute, PSQL_SS_TIME2_STRUCT(fColDataPtr)^.second,
                          PSQL_SS_TIME2_STRUCT(fColDataPtr)^.fraction div 1000000, Result,
                          ConSettings^.DisplayFormatSettings.TimeFormat, False)
                      else Len := DateTimeToUnicodeSQLTime(PSQL_TIME_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIME_STRUCT(fColDataPtr)^.minute, PSQL_TIME_STRUCT(fColDataPtr)^.second, 0, Result,
                          ConSettings^.DisplayFormatSettings.TimeFormat, False);
                    end;
      stDate:       begin
                      DateTimeToUnicodeSQLDate(EncodeDate(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                        PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day), @FTinyBuffer[0],
                          ConSettings^.DisplayFormatSettings, False);
                      Result := @FTinyBuffer[0];
                      Len := ConSettings^.DisplayFormatSettings.DateFormatLen;
                    end;
      stTimeStamp:  begin
                      Result := @FTinyBuffer[0];
                      Len := DateTimeToUnicodeSQLTimeStamp(Abs(PSQL_DATE_STRUCT(fColDataPtr)^.year),
                        PSQL_DATE_STRUCT(fColDataPtr)^.month, PSQL_DATE_STRUCT(fColDataPtr)^.day,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
                        PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction, Result,
                        ConSettings^.DisplayFormatSettings.DateTimeFormat, False,
                        PSQL_DATE_STRUCT(fColDataPtr)^.year < 0);
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
                        fUniTemp := PRawToUnicode(fColDataPtr, Len, FClientCP);
                        goto Set_From_Temp;
                      end;
                    end;
      stAsciiStream, stUnicodeStream, stBinaryStream:
                    begin
                      if IZBlob(ColumnBuffer).IsCLob then begin
                        Result := IZBlob(ColumnBuffer).GetPWideChar;
                        Len := IZBlob(ColumnBuffer).Length shr 1;
                      end else begin
                        fUniTemp := Ascii7ToUnicodeString(IZBlob(ColumnBuffer).GetBuffer, IZBlob(ColumnBuffer).Length);
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

function TAbstractODBCResultSet.GetUInt(ColumnIndex: Integer): LongWord;
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
      stAsciiStream, stUnicodeStream:
        Result := GetBlob(ColumnIndex).GetRawByteString;
      else Result := '';
    end;
  end;
end;

function TAbstractODBCResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
  L: LengthInt;
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
      stTimeStamp:
        Result := EncodeTime(PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.hour,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.second,
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction div 1000000);
      stString, stUnicodeString: begin
                if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  Result := UnicodeSQLTimeToDateTime(fColDataPtr, L, ConSettings^.ReadFormatSettings, Failed);
                end else begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := RawSQLTimeToDateTime(fColDataPtr, L, ConSettings^.ReadFormatSettings, Failed);
                end;
                if Failed then
                  LastWasNull := True;
              end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0;
end;

function TAbstractODBCResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var Failed: Boolean;
  L: LengthInt;
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
      stCurrency:   Result := Trunc(ODBCNumeric2Curr(fColDataPtr));
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
          PSQL_TIMESTAMP_STRUCT(fColDataPtr)^.fraction div 1000000);
      stString, stUnicodeString: begin
                if fIsUnicodeDriver then begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                  Result := UnicodeSQLTimeStampToDateTime(fColDataPtr, L, ConSettings^.ReadFormatSettings, Failed);
                end else begin
                  L := fStrLen_or_Ind shr 1;
                  if FixedWidth then
                    L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                  Result := RawSQLTimeStampToDateTime(fColDataPtr, L, ConSettings^.ReadFormatSettings, Failed);
                end;
                if Failed then
                  LastWasNull := True;
              end;
      //stAsciiStream, stUnicodeStream, stBinaryStream:
      else Result := 0;
    end;
  end else
    Result := 0
end;

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

function TAbstractODBCResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var P: Pointer;
  L: NativeUInt;
begin
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    if (Ord(ColumnType) < Ord(stString)) or (ColumnType in [stBytes, stBinaryStream]) then begin
      P := GetPAnsiChar(ColumnIndex, L);
      System.SetString(Result, PAnsiChar(P), L);
    end else if IsNull(ColumnIndex) then  //Sets LastWasNull, fColDataPtr, fStrLen_or_Ind!!
      Result := ''
    else case ColumnType of
      stString, stUnicodeString: begin
                        if fIsUnicodeDriver then begin
                          L := fStrLen_or_Ind shr 1;
                          if FixedWidth then
                            L := GetAbsorbedTrailingSpacesLen(PWideChar(fColDataPtr), L);
                          Result := PUnicodeToRaw(fColDataPtr, L, zCP_UTF8);
                        end else begin
                          L := fStrLen_or_Ind;
                          if FixedWidth then
                            L := GetAbsorbedTrailingSpacesLen(PAnsiChar(fColDataPtr), L);
                          if FClientCP = zCP_UTF8 then
                            System.SetString(Result, PAnsiChar(fColDataPtr), L)
                          else begin
                            FUniTemp := PRawToUnicode(fColDataPtr, l, FClientCP);
                            Result := PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), zCP_UTF8);
                          end;
                        end;
        end;
      stAsciiStream, stUnicodeStream:
        Result := GetBlob(ColumnIndex).GetUTF8String;
      else Result := '';
    end;
  end;
end;

function TAbstractODBCResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  Assert((ColumnIndex >= FirstDbcIndex) and (ColumnIndex{$IFDEF GENERIC_INDEX}<{$ELSE}<={$ENDIF} fColumnCount), SColumnIsNotAccessable);
  with TZODBCColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
  {$R-}
  fStrLen_or_Ind := StrLen_or_IndArray[fCurrentBufRowNo-1];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Result := fStrLen_or_Ind = SQL_NULL_DATA;
  fColDataPtr := nil;
    if (Ord(ColumnType) <= Ord(stUnicodeString)) and (not Result) then
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
            then PIZlob(@ColumnBuffer)^ := TZODBCBlob.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fChunkSize, fPlainDriver)
            else if ColumnType in [stAsciiStream, stUnicodeStream]
              then if ConSettings^.ClientCodePage^.Encoding = ceUTF16
                then PIZlob(@ColumnBuffer)^ := TZODBCClobW.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fChunkSize, fPlainDriver, ConSettings)
                else PIZlob(@ColumnBuffer)^ := TZODBCClobA.Create(ColumnIndex +1, fPHSTMT^, StrLen_or_IndPtr, fChunkSize, fPlainDriver, ConSettings);
        end;
  end;
end;

function TAbstractODBCResultSet.Next: Boolean;
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
end;

procedure TAbstractODBCResultSet.Open;
var
  bufSQLLEN: SQLLEN;
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
  begin
    Result := (ZFastCode.Pos('MONEY', UpperCase(ColStrAttribute(ColumnNumber, SQL_DESC_TYPE_NAME, StrBuf))) > 0); //handle smallmoney oslt too
  end;
begin
  if Closed and Assigned(fPHSTMT^) then begin
    if fColumnCount = 0 then
      CheckStmtError(fPlainDriver.SQLNumResultCols(fPHSTMT^, @fColumnCount));
    if fColumnCount = 0 then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    RowSize := 0;
    LobsInResult := False;
    SetLength(StrBuf, (Max(32,
      Max(fConnection.GetMetaData.GetDataBaseInfo.GetMaxTableNameLength,
        Max(fConnection.GetMetaData.GetDataBaseInfo.GetMaxSchemaNameLength,
          Max(fConnection.GetMetaData.GetDataBaseInfo.GetMaxTableNameLength,
            fConnection.GetMetaData.GetDataBaseInfo.GetMaxColumnNameLength))))+1) shl Ord(ConSettings^.ClientCodePage^.Encoding = ceUTF16));

    for ColumnNumber := 1 to fColumnCount do begin
      ColumnInfo := TZODBCColumnInfo.Create;
      ColumnsInfo.Add(ColumnInfo); //add first -> memleaksave
      with ColumnInfo do begin
        if fEnhancedColInfo then begin
          //ColumnInfo.CharOctedLength := ColNumAttribute(ColumnNumber, SQL_DESC_OCTET_LENGTH);
          AutoIncrement := ColNumAttribute(ColumnNumber, SQL_DESC_AUTO_UNIQUE_VALUE) = SQL_TRUE;
          CaseSensitive := ColNumAttribute(ColumnNumber, SQL_DESC_CASE_SENSITIVE) = SQL_TRUE;
          ColumnDisplaySize := ColNumAttribute(ColumnNumber, SQL_DESC_DISPLAY_SIZE);
          bufSQLLEN := ColNumAttribute(ColumnNumber, SQL_DESC_NULLABLE);
          if bufSQLLEN = SQL_NULLABLE then
            Nullable := ntNullable
          else if bufSQLLEN = SQL_NO_NULLS then
            Nullable := ntNoNulls
          else
            Nullable := ntNullableUnknown;
          Searchable := ColNumAttribute(ColumnNumber, SQL_DESC_SEARCHABLE) <> SQL_PRED_NONE;
          bufSQLLEN := ColNumAttribute(ColumnNumber, SQL_DESC_UPDATABLE);
          ReadOnly := bufSQLLEN = SQL_ATTR_READONLY;
          Writable := bufSQLLEN <> SQL_ATTR_READONLY;
          DefinitelyWritable := bufSQLLEN = SQL_ATTR_WRITE;

          ColumnLabel := ColStrAttribute(ColumnNumber, SQL_DESC_LABEL, StrBuf);
          ColumnName := ColStrAttribute(ColumnNumber, SQL_DESC_BASE_COLUMN_NAME, StrBuf);
          if ColumnName = '' then
            ColumnName := ColStrAttribute(ColumnNumber, SQL_DESC_NAME, StrBuf);
          if ColumnName = '' then //aggregates like SUM() don't have a columname -> skip processing
            ColumnName := 'Col_'+ZFastCode.IntToStr(ColumnNumber)
          else begin
            TableName := ColStrAttribute(ColumnNumber, SQL_DESC_BASE_TABLE_NAME, StrBuf);
            if TableName = '' then
              TableName := ColStrAttribute(ColumnNumber, SQL_DESC_TABLE_NAME, StrBuf);
            if TableName <> '' then begin //no table? -> no schema or catalog !
              SchemaName := ColStrAttribute(ColumnNumber, SQL_DESC_SCHEMA_NAME, StrBuf);
              CatalogName := ColStrAttribute(ColumnNumber, SQL_DESC_CATALOG_NAME, StrBuf);
            end;
          end;
          //DefaultValue -> not implemented
          //DefaultExpression -> not implemented
          bufSQLLEN := ColNumAttribute(ColumnNumber, SQL_DESC_CONCISE_TYPE);
          if bufSQLLEN = SQL_TYPE_VARIANT then begin//SQL Server type
            bufSQLLEN := ConvertODBC_CTypeToODBCType(ColNumAttribute(ColumnNumber, SQL_CA_SS_VARIANT_TYPE), fSigned);
            Signed := fSigned;
          end;
          case bufSQLLEN of
            SQL_NUMERIC, SQL_DECIMAL, SQL_FLOAT, SQL_REAL, SQL_DOUBLE: begin
                Precision := ColNumAttribute(ColumnNumber, SQL_DESC_PRECISION);
                Scale := ColNumAttribute(ColumnNumber, SQL_DESC_SCALE);
                Signed := True;
              end;
            SQL_CHAR, SQL_WCHAR: begin
                Precision := ColumnDisplaySize;
                FixedWidth := True;
              end;
            SQL_BINARY: begin
                Precision := ColNumAttribute(ColumnNumber, SQL_DESC_LENGTH);
                FixedWidth := True;
              end;
            SQL_VARCHAR, SQL_WVARCHAR, SQL_VARBINARY:
                Precision := ColumnDisplaySize;
            SQL_TINYINT, SQL_SMALLINT, SQL_INTEGER, SQL_BIGINT: begin
                Precision := ColumnDisplaySize;
                Signed := ColNumAttribute(ColumnNumber, SQL_DESC_UNSIGNED) = SQL_FALSE;
              end;
            else Precision := ColumnDisplaySize;
          end;

          ColumnType := ConvertODBCTypeToSQLType(bufSQLLEN, Scale, Precision,
              not Signed, ConSettings, @ODBC_CType);
          {numeric data type infos: }
          if (ColumnType in [stDouble, stCurrency]) then
            Currency := IsMoney;
          if ColumnType in [stString, stUnicodeString] then begin
            { character / binary info}
            if ColumnType in [stString, stUnicodeString] then
              if Ord(ConSettings^.ClientCodePage^.Encoding) >= Ord(ceUTF16)
              then ColumnCodePage := zCP_UTF16
              else ColumnCodePage := FClientCP;
          end;
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
    fMaxFetchableRows := {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Max(1, (Cardinal(fZBufferSize) div RowSize)*Byte(Ord(not LobsInResult)));
    if fMaxFetchableRows > 1 then begin
      CheckStmtError(fPlainDriver.SQLSetStmtAttr(fPHSTMT^, SQL_ATTR_ROW_ARRAY_SIZE, SQLPOINTER(fMaxFetchableRows), 0));
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
            SQL_DESC_CONCISE_TYPE, SQLPointer(ODBC_CType), SQL_IS_SMALLINT));
          CheckStmtError(FPlainDriver.SQLSetDescField(Desc, ColumnNumber+1,
            SQL_DESC_PRECISION, SQLPointer(Precision), SQL_IS_INTEGER));
          CheckStmtError(FPlainDriver.SQLSetDescField(Desc, ColumnNumber+1,
            SQL_DESC_SCALE, SQLPointer(Scale), SQL_IS_SMALLINT));
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
end;

procedure TAbstractODBCResultSet.ResetCursor;
begin
  if Assigned(fPHSTMT^) and fCursorOpened then begin
    CheckStmtError(fPlainDriver.SQLCloseCursor(fPHSTMT^)); //close cursor and discard pending result
    fCursorOpened := False;
  end;
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

function TODBCResultSetW.ColStrAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String;
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
    Result := PUnicodeToRaw(PWideChar(Pointer(Buf)), StringLength shr 1, FClientCP)
    {$ENDIF}
  else Result := '';
end;

constructor TODBCResultSetW.Create(const Statement: IZStatement;
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; const SQL: String;
  const Connection: IZODBCConnection; ZBufferSize, ChunkSize: Integer;
  const EnhancedColInfo: Boolean);
begin
  fPlainW := Connection.GetPLainDriver.GetInstance as TODBC3UnicodePlainDriver;
  inherited Create(Statement, StmtHandle, ConnectionHandle, SQL, Connection,
    ZBufferSize, ChunkSize, EnhancedColInfo);
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
  if NameLength = 0 then
    ColumnInfo.ColumnName := 'Col_'+ZFastCode.IntToStr(ColumnNumber)
  else begin
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

function TODBCResultSetA.ColStrAttribute(ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; const Buf: TByteDynArray): String;
var
  StringLength: SQLSMALLINT;
begin
  StringLength := 0;
  CheckStmtError(fPlainA.SQLColAttribute(fPHSTMT^, ColumnNumber, FieldIdentifier,
       Pointer(Buf), Length(Buf), @StringLength, nil));
  if StringLength > 0 then
    {$IFDEF UNICODE}
    Result := PRawToUnicode(PAnsiChar(Pointer(Buf)), StringLength, FClientCP)
    {$ELSE}
    System.SetString(Result, PAnsiChar(Pointer(Buf)), StringLength)
    {$ENDIF}
  else Result := '';
end;

constructor TODBCResultSetA.Create(const Statement: IZStatement;
  var StmtHandle: SQLHSTMT; ConnectionHandle: SQLHDBC; const SQL: String;
  const Connection: IZODBCConnection; ZBufferSize, ChunkSize: Integer;
  const EnhancedColInfo: Boolean);
begin
  fPlainA := Connection.GetPLainDriver.GetInstance as TODBC3RawPlainDriver;
  inherited Create(Statement, StmtHandle, ConnectionHandle, SQL, Connection,
    ZBufferSize, ChunkSize, EnhancedColInfo);
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
  if NameLength = 0 then
    ColumnInfo.ColumnName := 'Col_'+ZFastCode.IntToStr(ColumnNumber)
  else begin
    {$IFDEF UNICODE}
    ColumnInfo.ColumnName := PRawToUnicode(Pointer(Buf), NameLength, FClientCP);
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

{ TODBCTResultSetMetadata }

procedure TODBCTResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
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
  StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer; const PlainDriver: TZODBC3PlainDriver);
var
  OffSetPtr: PAnsiChar;
  i: Integer;
begin
  inherited Create;
  if StrLen_or_IndPtr^ >= 0 then begin
    FBlobSize := StrLen_or_IndPtr^;
    GetMem(FBlobData, FBlobSize);
    OffSetPtr := FBlobData;
    for i := 1 to StrLen_or_IndPtr^ div ChunkSize do begin
      Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, ChunkSize, StrLen_or_IndPtr));
      Inc(OffSetPtr, ChunkSize);
    end;
    Assert(SQL_SUCCEDED(PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, ChunkSize, StrLen_or_IndPtr)));
  end else if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FBlobSize := -1
  else begin
    Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
    GetMem(FBlobData, ChunkSize);
    FBlobSize := ChunkSize;
    OffSetPtr := FBlobData;
    while (PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_BINARY, OffSetPtr, ChunkSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
      ReallocMem(FBlobData, FBlobSize + ChunkSize);
      OffSetPtr := {%H-}Pointer({%H-}NativeUInt(FBlobData)+NativeUInt(FBlobSize));
      FBlobSize := FBlobSize + ChunkSize;
    end;
    FBlobSize := FBlobSize - ChunkSize + StrLen_or_IndPtr^;
    ReallocMem(FBlobData, FBlobSize);
  end;
end;

{ TZODBCClobA }

constructor TZODBCClobA.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer;
  const PlainDriver: TZODBC3PlainDriver; ConSettings: PZConSettings);
var
  OffSetPtr: PAnsiChar;
  i: Integer;
begin
  inherited Create;
  FConSettings := ConSettings;
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FBlobSize := -1
  else begin
    { truncated string data always have a trailing #0 on top of data }
    if StrLen_or_IndPtr^ >= 0 then begin
      FBlobSize := StrLen_or_IndPtr^ +SizeOf(AnsiChar);
      GetMem(FBlobData, FBlobSize);
      OffSetPtr := FBlobData;
      for i := 1 to StrLen_or_IndPtr^ div ChunkSize do begin
        Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr));
        Inc(OffSetPtr, ChunkSize-SizeOf(AnsiChar));
      end;
      Assert(SQL_SUCCEDED(PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr)));
    end else begin
      Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
      GetMem(FBlobData, ChunkSize);
      FBlobSize := ChunkSize;
      OffSetPtr := FBlobData;
      while (PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_CHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
        ReallocMem(FBlobData, FBlobSize + ChunkSize);
        OffSetPtr := PAnsiChar(FBlobData)+FBlobSize-SizeOf(AnsiChar);
        FBlobSize := FBlobSize + ChunkSize-SizeOf(AnsiChar);
      end;
      FBlobSize := FBlobSize - ChunkSize + StrLen_or_IndPtr^ +SizeOf(AnsiChar);
      ReallocMem(FBlobData, FBlobSize);
    end;
    PByte(PAnsiChar(FBlobData)+FBlobSize-SizeOf(AnsiChar))^ := Ord(#0); //set trailing #0
  end;
end;

{ TZODBCClobW }

constructor TZODBCClobW.Create(ColumnNumber: SQLSMALLINT; StmtHandle: SQLHSTMT;
  StrLen_or_IndPtr: PSQLLEN; ChunkSize: Integer;
  const PlainDriver: TZODBC3PlainDriver; ConSettings: PZConSettings);
var
  OffSetPtr: PAnsiChar;
  I: Integer;
begin
  inherited Create;
  FConSettings := ConSettings;
  FCurrentCodePage := zCP_UTF16;
  if StrLen_or_IndPtr^ = SQL_NULL_DATA then
    FBlobSize := -1
  else begin
    { truncated string data always have a trailing #0 on top of data }
    if StrLen_or_IndPtr^ >= 0 then begin
      FBlobSize := StrLen_or_IndPtr^ +SizeOf(WideChar);
      GetMem(FBlobData, FBlobSize);
      OffSetPtr := FBlobData;
      for i := 1 to StrLen_or_IndPtr^ div ChunkSize do begin
        Assert(SQL_SUCCESS_WITH_INFO = PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr));
        Inc(OffSetPtr, ChunkSize-2);
      end;
      Assert(SQL_SUCCEDED(PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr)));
    end else begin
      Assert(StrLen_or_IndPtr^ = SQL_NO_TOTAL);
      GetMem(FBlobData, ChunkSize);
      FBlobSize := ChunkSize;
      OffSetPtr := FBlobData;
      while (PlainDriver.SQLGetData(StmtHandle, ColumnNumber, SQL_C_WCHAR, OffSetPtr, ChunkSize, StrLen_or_IndPtr) = SQL_SUCCESS_WITH_INFO) do begin
        ReallocMem(FBlobData, FBlobSize + ChunkSize);
        OffSetPtr := PAnsiChar(FBlobData)+FBlobSize-SizeOf(WideChar);
        FBlobSize := FBlobSize + ChunkSize-SizeOf(WideChar);
      end;
      FBlobSize := FBlobSize - ChunkSize + StrLen_or_IndPtr^ +SizeOf(WideChar);
      ReallocMem(FBlobData, FBlobSize);
    end;
    PWord(PAnsiChar(FBlobData)+FBlobSize-SizeOf(WideChar))^ := Ord(#0); //set trailing #0
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

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
end.
