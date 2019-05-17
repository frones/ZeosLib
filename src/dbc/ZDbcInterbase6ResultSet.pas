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
  {$IFDEF BCD_TEST}FmtBCD,{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZDbcIntfs, ZDbcResultSet, ZDbcInterbase6, ZPlainFirebirdInterbaseConstants,
  ZPlainFirebirdDriver, ZCompatibility, ZDbcResultSetMetadata, ZMessages, ZPlainDriver,
  ZDbcInterbase6Utils, ZSelectSchema, ZDbcUtils;

type
  {** Implements Interbase ResultSet. }
  TZInterbase6XSQLDAResultSet = class(TZAbstractReadOnlyResultSet_A, IZResultSet)
  private
    FCachedBlob: boolean;
    FStmtHandle: TISC_STMT_HANDLE;
    FStmtHandleAddr: PISC_STMT_HANDLE;
    FWasLastResult: Boolean;
    FXSQLDA: PXSQLDA;
    FIZSQLDA: IZSQLDA;
    FPISC_DB_HANDLE: PISC_DB_HANDLE;
    FBlobTemp: IZBlob;
    FPlainDriver: TZInterbasePlainDriver;
    FDialect: Word;
    FCodePageArray: TWordDynArray;
    FStmtType: TZIbSqlStatementType;
    FGUIDProps: TZInterbase6StatementGUIDProps;
    FISC_TR_HANDLE: TISC_TR_HANDLE;
    FIBConnection: IZInterbase6Connection;
    FClientCP: word;
  protected
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      StmtHandleAddr: PISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
      WasLastResult, CachedBlob: boolean; StmtType: TZIbSqlStatementType);

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
    function Next: Boolean; reintroduce;
  end;

  {** Implements external blob wrapper object for Intebase/Firbird. }
  TZInterbase6UnCachedBlob = Class(TZAbstractUnCachedBlob, IZUnCachedLob)
  private
    FBlobId: TISC_QUAD;
    FPlainDriver: TZInterbasePlainDriver;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const PlainDriver: TZInterbasePlainDriver;
      var BlobId: TISC_QUAD; const Connection: IZInterbase6Connection);
  end;

  TZInterbase6UnCachedClob = Class(TZAbstractUnCachedClob, IZUnCachedLob)
  private
    FBlobId: TISC_QUAD;
    FPlainDriver: TZInterbasePlainDriver;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const PlainDriver: TZInterbasePlainDriver;
      var BlobId: TISC_QUAD; const Connection: IZInterbase6Connection);
  end;

  {** Implements Interbase ResultSetMetadata object. }
  TZInterbaseResultSetMetadata = Class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
    procedure FillColumInfoFromGetColumnsRS(ColumnInfo: TZColumnInfo;
      const TableColumns: IZResultSet; const FieldName: String); override;
  public
    function GetCatalogName({%H-}ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetSchemaName({%H-}ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement({%H-}ColumnIndex: Integer): Boolean; override;
  End;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  ZEncoding, ZFastCode, ZSysUtils, ZDbcMetadata, ZClasses, ZDbcLogging, ZVariant;

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

function GetRawFromTextVar(XSQLVAR: PXSQLVAR): RawByteString; {$IF defined(WITH_INLINE)} inline; {$IFEND}
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
  GetPCharFromTextVar(XSQLVAR, P, Len);
  if Len > 0
  then ZSetString(P, Len, Result{%H-})
  else Result := '';
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
  WasLastResult, CachedBlob: Boolean; StmtType: TZIbSqlStatementType);
begin
  inherited Create(Statement, SQL, TZInterbaseResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  FClientCP := ConSettings.CTRL_CP;
  FIZSQLDA := XSQLDA; //localize the interface to avoid automatic free the object
  FXSQLDA := XSQLDA.GetData; // localize buffer for fast access

  FCachedBlob := CachedBlob;
  FIBConnection := Statement.GetConnection as IZInterbase6Connection;
  FPISC_DB_HANDLE := FIBConnection.GetDBHandle;
  FISC_TR_HANDLE := FIBConnection.GetTrHandle^;
  FPlainDriver := TZInterbasePlainDriver(FIBConnection.GetIZPlainDriver.GetInstance);
  FDialect := FIBConnection.GetDialect;
  FStmtType := StmtType; //required to know how to fetch the columns for ExecProc
  FWasLastResult := WasLastResult;

  FStmtHandleAddr := StmtHandleAddr;
  FStmtHandle := StmtHandleAddr^;
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;

  FCodePageArray := FPlainDriver.GetCodePageArray;
  FCodePageArray[ConSettings^.ClientCodePage^.ID] := ConSettings^.ClientCodePage^.CP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250

  Open;
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
    C, SQLCode: SmallInt;
    TempDate: TZTimeStamp;//TCTimeStructure;
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
    with FXSQLDA.sqlvar[C] do
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
                            if TZColumnInfo(ColumnsInfo[c]).ColumnCodePage = zCP_UTF8 then
                              JSONWriter.AddJSONEscape(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen)
                            else begin
                              FUniTemp := PRawToUnicode(@PISC_VARYING(sqldata).str[0], PISC_VARYING(sqldata).strlen, TZColumnInfo(ColumnsInfo[c]).ColumnCodePage);
                              JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp))
                            end;
                            JSONWriter.Add('"');
                          end;
          SQL_TEXT      : if sqlsubtype = CS_BINARY then
                            JSONWriter.WrBase64(sqldata, sqllen, True)
                          else begin
                            JSONWriter.Add('"');
                            if TZColumnInfo(ColumnsInfo[c]).ColumnCodePage = zCP_UTF8 then
                              JSONWriter.AddJSONEscape(sqldata, L)
                            else begin
                              FUniTemp := PRawToUnicode(sqldata, L, TZColumnInfo(ColumnsInfo[c]).ColumnCodePage);
                              JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp))
                            end;
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
                            P := nil;
                            try
                              if SqlSubType = isc_blob_text then begin
                                JSONWriter.Add('"');
                                ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, FIBConnection.GetTrHandle,
                                    PISC_QUAD(sqldata)^, L, P, False, Self);
                                if ConSettings^.ClientCodePage^.CP = zCP_UTF8 then
                                  JSONWriter.AddJSONEscape(P, L)
                                else begin
                                  fUniTemp := PRawToUnicode(P, L, ConSettings^.ClientCodePage^.CP);
                                  JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
                                end;
                                JSONWriter.Add('"');
                              end else begin
                                ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, FIBConnection.GetTrHandle,
                                  PISC_QUAD(sqldata)^, L, P, true, Self);
                                JSONWriter.WrBase64(P, L, True);
                              end;
                            finally
                              FreeMem(P);
                            end;
                          end;
          SQL_ARRAY     : JSONWriter.AddShort('"Array"');
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
          else
            raise EZIBConvertError.Create(Format(SErrorConvertionField,
              [FIZSQLDA.GetFieldAliasName(C), GetNameSqlType(SQLCode)]));
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
{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFDEF BCD_TEST}
procedure TZInterbase6XSQLDAResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
{$ELSE}
function TZInterbase6XSQLDAResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
{$ENDIF}
var
  TempDate: TZTimeStamp;
  {$IFNDEF BCD_TEST}
  P: PAnsiChar;
  Len: NativeUInt;
  {$ENDIF}
  XSQLVAR: PXSQLVAR;
  dDT, tDT: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF BCD_TEST}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := NullBCD
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_D_FLOAT,
      SQL_DOUBLE    : Double2BCD(PDouble(XSQLVAR.sqldata)^, Result);
      SQL_LONG      : ScaledOrdinal2Bcd(PISC_LONG(XSQLVAR.sqldata)^, Byte(-XSQLVAR.sqlscale), Result);
      SQL_FLOAT     : Double2BCD(PSingle(XSQLVAR.sqldata)^, Result);
      SQL_BOOLEAN   : ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN(XSQLVAR.sqldata)^ <> 0), 0, Result);
      SQL_BOOLEAN_FB: ScaledOrdinal2Bcd(Ord(PISC_BOOLEAN_FB(XSQLVAR.sqldata)^ <> 0), 0, Result);
      SQL_SHORT     : ScaledOrdinal2Bcd(PISC_SHORT(XSQLVAR.sqldata)^, Byte(-XSQLVAR.sqlscale), Result);
      SQL_INT64     : ScaledOrdinal2Bcd(PISC_INT64(XSQLVAR.sqldata)^, Byte(-XSQLVAR.sqlscale), Result);
      SQL_TEXT      : LastWasNull := not TryRawToBCD(XSQLVAR.sqldata, XSQLVAR.sqllen, Result, '.');
      SQL_VARYING   : LastWasNull := not TryRawToBCD(@PISC_VARYING(XSQLVAR.sqldata).str[0], PISC_VARYING(XSQLVAR.sqldata).strlen, Result, '.');
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
                        then dDT := dDT-tDT
                        else dDT := dDT+tDT;
                        Double2BCD(dDT, Result);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Double2BCD(SysUtils.EncodeDate(TempDate.Year,TempDate.Month, TempDate.Day), Result);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Double2BCD(SysUtils.EncodeTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10), Result);
                      end;
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
          [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
    end;
  end;
  {$ELSE}
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
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
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
          [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
    end;
  end;
  {$ENDIF}
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZInterbase6XSQLDAResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  BlobId: TISC_QUAD;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Result := nil;
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := nil;
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_QUAD, SQL_BLOB, SQL_ARRAY: BlobId := PISC_QUAD(XSQLVAR.sqldata)^;
    else
      raise EZIBConvertError.Create(SUnsupportedDataType + ' ' + {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr((XSQLVAR.sqltype and not(1))));
    end;
    case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType of
      stBinaryStream:
        if FCachedBlob then begin
          Result := TZAbstractBlob.Create;
          ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, FIBConnection.GetTrHandle,
            BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, True, Self);
        end else
          Result := TZInterbase6UnCachedBlob.Create(FPlainDriver, BlobId, FIBConnection);
      stAsciiStream, stUnicodeStream:
        if FCachedBlob then begin
          Result := TZAbstractClob.CreateWithData(nil, 0, Consettings^.ClientCodePage^.CP, ConSettings);
          ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, FIBConnection.GetTrHandle,
            BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, False, Self);
        end else
          Result := TZInterbase6UnCachedClob.Create(FPlainDriver, BlobId, FIBConnection);
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
function TZInterbase6XSQLDAResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
                        Result := StrToBoolEx(P, P+Len);
                      end;
      SQL_BLOB      : Result := StrToBoolEx(GetBlob(ColumnIndex).GetString);
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
    end;
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
function TZInterbase6XSQLDAResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  if (XSQLVAR.sqlind <> nil) and (XSQLVAR.sqlind^ = ISC_NULL) then begin
    LastWasNull := True;
    Result := nil
  end else begin
    LastWasNull := False;
    case (XSQLVAR.sqltype and not(1)) of
      SQL_VARYING: Result := BufferToBytes(@PISC_VARYING(XSQLVAR.sqldata).str[0], PISC_VARYING(XSQLVAR.sqldata).strlen);
      SQL_QUAD,
      SQL_BLOB,
      SQL_ARRAY: Result := GetBlob(ColumnIndex).GetBytes;
      else Result := BufferToBytes(XSQLVAR.sqldata, XSQLVAR.sqllen);
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
function TZInterbase6XSQLDAResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  TempDate: TZTimeStamp;//TCTimeStructure;
  Len: NativeUInt;
  P: PAnsiChar;
  Failed: Boolean;
  SQLCode: SmallInt;
  XSQLVAR: PXSQLVAR;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  {$R-}
  XSQLVAR := @FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
    begin
      SQLCode := (sqltype and not(1));
      case SQLCode of
        SQL_TIMESTAMP :
          begin
            isc_decode_date(PISC_TIMESTAMP(sqldata).timestamp_date,
              TempDate.Year, TempDate.Month, Tempdate.Day);
            Result := SysUtils.EncodeDate(TempDate.Year, TempDate.Month, TempDate.Day);
          end;
        SQL_TYPE_DATE :
          begin
            isc_decode_date(PISC_DATE(sqldata)^, TempDate.Year, TempDate.Month, Tempdate.Day);
            Result := SysUtils.EncodeDate(TempDate.Year, TempDate.Month, TempDate.Day);
          end;
        SQL_TYPE_TIME : Result := 0;
        SQL_TEXT,
        SQL_VARYING:
          begin
            GetPCharFromTextVar(XSQLVAR, P, Len);
            if Len = ConSettings^.ReadFormatSettings.DateFormatLen
            then Result := RawSQLDateToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else Result := Int(RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Result = 0;
          end;
        else
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}));
      end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
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
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
          [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
                        ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                      end;
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
          [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType((XSQLVAR.sqltype and not(1)))]));
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
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
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
          [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
                        Result := RawToIntDef(P, P+Len, 0);
                      end;
      SQL_BLOB      : Result := RawToIntDef(GetBlob(ColumnIndex).GetString, 0);
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
                        Result := RawToInt64Def(P, P+Len, 0);
                      end;
      SQL_BLOB      : Result := RawToInt64Def(GetBlob(ColumnIndex).GetString, 0);
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
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
function TZInterbase6XSQLDAResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
  TempDate: TZTimeStamp; //TCTimeStructure;
  Failed: Boolean;
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
      SQL_TIMESTAMP :
        begin
          isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
            TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
          Result := EncodeTime(TempDate.Hour, TempDate.Minute,
            TempDate.Second, TempDate.Fractions div 10);
        end;
      SQL_TYPE_DATE : Result := 0;
      SQL_TYPE_TIME :
        begin
          isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
            TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
          Result := EncodeTime(TempDate.Hour, TempDate.Minute,
            TempDate.Second, TempDate.Fractions div 10);
        end;
      SQL_TEXT, SQL_VARYING:
        begin
          GetPCharFromTextVar(XSQLVAR, P, Len);
          if AnsiChar((P+2)^) = AnsiChar(':') then //possible date if Len = 10 then
            Result := RawSQLTimeToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed)
          else
            Result := Frac(RawSQLTimeStampToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed));
        end;
      else
        Result := Frac(GetDouble(ColumnIndex));
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
function TZInterbase6XSQLDAResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  P: PAnsiChar;
  Len: NativeUInt;
  XSQLVAR: PXSQLVAR;
  TempDate: TZTimeStamp; //TCTimeStructure;
  Failed: Boolean;
  DT: TDateTime;
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
      SQL_TIMESTAMP :
        begin
          isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
            TempDate.Year, TempDate.Month, Tempdate.Day);
          isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
            TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
          if not SysUtils.TryEncodeDate(TempDate.Year, TempDate.Month, Tempdate.Day, Result) then
            Result := 0;
          if not SysUtils.TryEncodeTime(TempDate.Hour, TempDate.Minute,
              TempDate.Second, TempDate.Fractions div 10, DT) then
            DT := 0;
          if Result < 0
          then Result := Result - DT
          else Result := Result + DT;
        end;
      SQL_TYPE_DATE :
        begin
          isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
            TempDate.Year, TempDate.Month, Tempdate.Day);
          if not SysUtils.TryEncodeDate(TempDate.Year,TempDate.Month, TempDate.Day, Result) then
            Result := 0;
        end;
      SQL_TYPE_TIME :
        begin
          isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^,
            TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
          if not SysUtils.TryEncodeTime(TempDate.Hour, TempDate.Minute,
            TempDate.Second, TempDate.Fractions div 10, Result) then
            Result := 0;
        end;
      SQL_TEXT, SQL_VARYING:
        begin
          GetPCharFromTextVar(XSQLVAR, P, Len);
          if AnsiChar((P+2)^) = AnsiChar(':') then
            Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
          else if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4
            then Result := RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
        end;
      else Result := GetDouble(ColumnIndex);
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
      SQL_BLOB      : Begin
                        FBlobTemp := GetBlob(ColumnIndex);  //localize interface to keep pointer alive
                        Result := FBlobTemp.GetBuffer;
                        Len := FBlobTemp.Length;
                      End;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := ZSysUtils.DateTimeToRawSQLTimeStamp(TempDate.Year,
                          TempDate.Month, TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := @FTinyBuffer[0];
                        Len := ZSysUtils.DateTimeToRawSQLDate(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := DateTimeToRawSQLTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False);
                      end;
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
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
  end else begin
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
                        if XSQLVAR.sqlsubtype = CS_BINARY
                        then fUniTemp := Ascii7ToUnicodeString(P, Len)
                        else fUniTemp := PRawToUnicode(P, Len, ConSettings^.ClientCodePage.CP);
                        Len := Length(fUniTemp);
                        if Len <> 0
                        then Result := Pointer(fUniTemp)
                        else Result := PEmptyUnicodeString;
                      end;
      SQL_BLOB      : Begin
                        FBlobTemp := GetBlob(ColumnIndex);  //localize interface to keep pointer alive
                        if FBlobTemp.IsClob then begin
                          Result := FBlobTemp.GetPWideChar;
                          Len := FBlobTemp.Length shr 1;
                        end else begin
                          FUniTemp := Ascii7ToUnicodeString(FBlobTemp.GetBuffer, FBlobTemp.Length);
                          Result := Pointer(FUniTemp);
                          Len := Length(FUniTemp);
                        end;
                      End;
      SQL_TIMESTAMP : begin
                        isc_decode_date(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_date,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        isc_decode_time(PISC_TIMESTAMP(XSQLVAR.sqldata).timestamp_time,
                          TempDate.Hour, TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := ZSysUtils.DateTimeToUnicodeSQLTimeStamp(TempDate.Year,
                          TempDate.Month, TempDate.Day, TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                      end;
      SQL_TYPE_DATE : begin
                        isc_decode_date(PISC_DATE(XSQLVAR.sqldata)^,
                          TempDate.Year, TempDate.Month, Tempdate.Day);
                        Result := @FTinyBuffer[0];
                        Len := ZSysUtils.DateTimeToUnicodeSQLDate(TempDate.Year, TempDate.Month, Tempdate.Day,
                          Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                      end;
      SQL_TYPE_TIME : begin
                        isc_decode_time(PISC_TIME(XSQLVAR.sqldata)^, TempDate.Hour,
                          TempDate.Minute, Tempdate.Second, Tempdate.Fractions);
                        Result := @FTinyBuffer[0];
                        Len := ZSysUtils.DateTimeToUnicodeSQLTime(TempDate.Hour, TempDate.Minute,
                          TempDate.Second, TempDate.Fractions div 10,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False);
                      end;
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
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
  CheckColumnConvertion(ColumnIndex, stLong);
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
      SQL_TEXT,
      SQL_VARYING   : begin
                        GetPCharFromTextVar(XSQLVAR, P, Len);
                        Result := RawToUInt64Def(P, P+Len, 0);
                      end;
      SQL_BLOB      : Result := RawToUInt64Def(GetBlob(ColumnIndex).GetString, 0);
      else raise EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(XSQLVAR.sqltype and not(1))]));
    end;
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

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
  I, OldRow: Integer;
  RS: IZResultSet;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or (RowNo > LastRowNo ) or ((MaxRows > 0) and (LastRowNo >= MaxRows) or (FStmtHandleAddr^ = 0)) then
    Exit;

  { Fetch row. }
  if (FStmtType = stSelect) then begin //AVZ - Test for ExecProc - this is for multiple rows
    { FireBirdAPI:
    Both isc_commit_transaction() and isc_rollback_transaction() close the record streams
    associated with the transaction, reinitialize the transaction name to zero, and release
    system resources allocated for the transaction. Freed system resources are available for
    subsequent use by any application or program. }
    if (FISC_TR_HANDLE <> FIBConnection.GetTrHandle^) and (Statement.GetUpdateCount = 0) then begin //transaction changed and no updates done?
      OldRow := RowNo-1; //safe -> reuse resets the rowno
      //this finally goes to !self! or IZCachedResultSet containing !self! recursive
      //and scrolls to RowNo -1
      if FWasLastResult then begin
        (Statement as IZPreparedStatement).ExecutePrepared;
        RS := Statement.GetResultSet;
      end else
        RS := (Statement as IZPreparedStatement).ExecuteQueryPrepared;
      FISC_TR_HANDLE := FIBConnection.GetTrHandle^;//set current transaction handle
      for i := 1 to OldRow do
        RS.Next; //reload data
    end;
    if (RowNo = 0) and (FStmtHandle = 0) then
      FStmtHandle := FStmtHandleAddr^;
    if FPlainDriver.isc_dsql_fetch(@StatusVector,
      @FStmtHandle, FDialect, FXSQLDA) = 0 then begin
      RowNo := RowNo + 1;
      LastRowNo := RowNo;
      Result := True;
    end else begin
      CheckInterbase6Error(FPlainDriver, StatusVector, Self);
      {no error occoured -> notify IsAfterLast and close the recordset}
      RowNo := RowNo + 1;
      if FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0 then
        CheckInterbase6Error(FPlainDriver, StatusVector, Self);
      FStmtHandle := 0;
    end;
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
  CP: Word;
  XSQLVAR: PXSQLVAR;
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
          stString, stUnicodeString:
            begin
              //see test Bug#886194, we retrieve 565 as CP... the modula get returns the FBID of CP
              CP := XSQLVAR.sqlsubtype and 255;
              //see: http://sourceforge.net/p/zeoslib/tickets/97/
              if (CP = ConSettings^.ClientCodePage^.ID)
              then ZCodePageInfo := ConSettings^.ClientCodePage
              else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CP); //get column CodePage info}
              if ConSettings^.ClientCodePage^.ID = CS_NONE
              then ColumnCodePage := ZCodePageInfo.CP
              else ColumnCodePage := ConSettings^.ClientCodePage^.CP;
              Precision := XSQLVAR.sqllen div ZCodePageInfo^.CharWidth;
              if ColumnType = stString then begin
                CharOctedLength := Precision * ConSettings^.ClientCodePage^.CharWidth;
                ColumnDisplaySize := Precision;
              end else begin
                CharOctedLength := Precision shl 1;
                ColumnDisplaySize := Precision;
              end;
            end;
          stAsciiStream, stUnicodeStream:
            ColumnCodePage := ConSettings^.ClientCodePage^.CP;
          else begin
            ColumnCodePage := zCP_NONE;
            case FieldSqlType of
              stBytes: Precision := XSQLVAR.sqllen;
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
            end;
          end;
        end;
        ReadOnly := (TableName = '') or (ColumnName = '') or
          (ColumnName = 'RDB$DB_KEY') or (FieldSqlType = ZDbcIntfs.stUnknown);

        Nullable := TZColumnNullableType(Ord(FIZSQLDA.IsNullable(I)));
        Scale := FIZSQLDA.GetFieldScale(I);
        CaseSensitive := UpperCase(ColumnName) <> ColumnName; //non quoted fiels are uppercased by default
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;
  inherited Open;
end;

procedure TZInterbase6XSQLDAResultSet.ResetCursor;
var StatusVector: TARRAY_ISC_STATUS;
begin
  if not Closed then begin
    if (FStmtHandle <> 0) then begin
      if (FStmtType <> stExecProc) and (FISC_TR_HANDLE = FIBConnection.GetTrHandle^) and
         (FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0) then
        CheckInterbase6Error(FPlainDriver, StatusVector, Self, lcOther, 'isc_dsql_free_statement');
      FStmtHandle := 0;
    end;
    inherited ResetCursor;
  end;
end;

{ TZInterbase6UnCachedBlob }
{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
constructor TZInterbase6UnCachedBlob.Create(const PlainDriver: TZInterbasePlainDriver;
  var BlobId: TISC_QUAD; const Connection: IZInterbase6Connection);
begin
  FBlobId := BlobId;
  FPlainDriver := PlainDriver;
  FIBConnection := Connection;
end;

procedure TZInterbase6UnCachedBlob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FIBConnection.GetDBHandle, FIBConnection.GetTrHandle,
    FBlobId, Size, Buffer, True, FIBConnection as IImmediatelyReleasable);
  BlobSize := Size;
  BlobData := Buffer;
  inherited ReadLob;
end;

{ TZInterbase6UnCachedClob }

{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
constructor TZInterbase6UnCachedClob.Create(const PlainDriver: TZInterbasePlainDriver;
  var BlobId: TISC_QUAD; const Connection: IZInterbase6Connection);
begin
  inherited CreateWithData(nil, 0, Connection.GetConSettings^.ClientCodePage^.CP,
    Connection.GetConSettings);
  FIBConnection := Connection;
  FBlobId := BlobId;
  FPlainDriver := PlainDriver;
end;

procedure TZInterbase6UnCachedClob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FIBConnection.GetDBHandle, FIBConnection.GetTrHandle,
    FBlobId, Size, Buffer, False, FIBConnection as IImmediatelyReleasable);
  AnsiChar((PAnsiChar(Buffer)+NativeUInt(Size))^) := AnsiChar(#0); //add #0 terminator
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  FBlobSize := Size+1;
  BlobData := Buffer;
  inherited ReadLob;
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

procedure TZInterbaseResultSetMetadata.FillColumInfoFromGetColumnsRS(
  ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet;
  const FieldName: String);
begin
  inherited FillColumInfoFromGetColumnsRS(ColumnInfo, TableColumns, FieldName);
  //FB native rs can't give use users choosen precision
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then
    ColumnInfo.Precision := TableColumns.GetInt(TableColColumnSizeIndex);
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
{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
