{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlResultSet;

interface

{$I ZDbc.inc}

uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZPlainPostgreSqlDriver, ZDbcLogging,
  ZDbcResultSetMetadata, ZCompatibility;

type
  TZPGColumnInfo = class(TZColumnInfo)
  protected
    fTableOID: OID;
    fTableColNo: Integer;
  public
    property TableOID: OID read fTableOID write fTableOID;
    property TableColNo: Integer read fTableColNo write fTableColNo;
  end;

  {** Implements Postgres ResultSet Metadata. }
  TZPostgresResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure LoadColumns; override;
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  {** Implements PostgreSQL ResultSet. }
  TZAbstractPostgreSQLStringResultSet = class(TZAbstractResultSet)
  private
    FUUIDOIDOutBuff: TBytes;
    FconnAddress: PPGconn;
    Fres: TPGresult;
    FresAddress: PPGresult;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
    FIs_bytea_output_hex: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    FCachedLob: boolean;
    FpgOIDTypes: TIntegerDynArray;
    function GetBuffer(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
    procedure ClearPGResult;
  protected
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
    procedure Open; override;
    procedure DefinePostgreSQLToSQLType(ColumnInfo: TZColumnInfo; const TypeOid: Oid);
    function PGRowNo: Integer; virtual; abstract;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      connAddress: PPGconn; resAddress: PPGresult;
      const CachedLob: Boolean; const Chunk_Size, UndefinedVarcharAsStringLength: Integer);

    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
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

    function MoveAbsolute(Row: Integer): Boolean; override;
    {$IFDEF USE_SYNCOMMONS}
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions = [jcoEndJSONObject]); override;
    {$ENDIF USE_SYNCOMMONS}
  end;

  TZClientCursorPostgreSQLStringResultSet = Class(TZAbstractPostgreSQLStringResultSet)
  protected
    procedure Open; override;
    function PGRowNo: Integer; override;
  public
    function MoveAbsolute(Row: Integer): Boolean; override;
  End;

  TZServerCursorPostgreSQLStringResultSet = Class(TZAbstractPostgreSQLStringResultSet)
  protected
    procedure Open; override;
    function PGRowNo: Integer; override;
  public
    function Next: Boolean; override;
  End;

  {** Represents an interface, specific for PostgreSQL blobs. }
  IZPostgreSQLOidBlob = interface(IZBlob)
    ['{BDFB6B80-477D-4CB1-9508-9541FEA6CD72}']
    function GetBlobOid: Oid;
    procedure WriteLob;
    procedure WriteBuffer(const Buffer: Pointer; const Len: integer);
  end;

  {** Implements external blob wrapper object for PostgreSQL. }
  TZPostgreSQLOidBlob = class(TZAbstractUnCachedBlob, IZPostgreSQLOidBlob, IZUnCachedLob)
  private
    FHandle: TPGconn;
    FBlobOid: Oid;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
  public
    constructor Create(const PlainDriver: TZPostgreSQLPlainDriver; const
      Data: Pointer; const Size: Integer; const Handle: TPGconn;
      const BlobOid: Oid; const Chunk_Size: Integer);

    function GetBlobOid: Oid;
    procedure ReadLob; override;
    procedure WriteLob; override;
    procedure WriteBuffer(const Buffer: Pointer; const Len: integer);

    function Clone(Empty: Boolean = False): IZBlob; override;
  end;

  TZPostgreSQLByteaHexBlob = class(TZAbstractBlob)
  public
    constructor Create(Data: PAnsiChar);
  end;

  TZPostgreSQLByteaEscapedBlob = class(TZAbstractBlob)
  public
    constructor Create(const PlainDriver: TZPostgreSQLPlainDriver; Data: PAnsiChar);
  end;


implementation

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} Math,
  ZMessages, ZEncoding, ZFastCode, ZDbcPostgreSqlMetadata, ZDbcMetadata,
  ZDbcPostgreSql, ZDbcPostgreSqlUtils, ZClasses;


// added for suporting Infinity, -Infinity and NaN.
// See https://sourceforge.net/p/zeoslib/tickets/173/
// maybe this should be pushed into ZSysUtils.SQLStrToFloatDef?
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure pgSQLStrToFloatDef(Value: PAnsiChar; const Def: Extended;
  var Result: Extended); overload;
begin
  if Value = 'Infinity' then
    Result := Infinity
  else if Value = '-Infinity' then
    Result := NegInfinity
  else if Value = 'NaN' then
    Result := NaN
  else
    ZSysUtils.SQLStrToFloatDef(Value, Def, Result);
end;
{$IFEND}

procedure pgSQLStrToFloatDef(Value: PAnsiChar; const Def: Single;
  var Result: Single); overload;
begin
  {$IFDEF FPC2_6DOWN}
  {$ifopt R+}
  {$define RangeCheckWasOn}
  {$R-}
  {$endif opt R+}
  {$ifopt Q+}
  {$define OverflowCheckWasOn}
  {$Q-}
  {$endif opt Q+}
  {$ENDIF}
  if Value = 'Infinity' then
    Result := Infinity
  else if Value = '-Infinity' then
    Result := NegInfinity
  else if Value = 'NaN' then
    Result := NaN
  else
    ZSysUtils.SQLStrToFloatDef(Value, Def, Result);
  {$IFDEF FPC2_6DOWN}
  {$ifdef RangeCheckWasOn}
  {$R+}
  {$undef RangeCheckWasOn}
  {$endif}
  {$ifdef OverflowCheckWasOn}
  {$Q+}
  {$undef OverflowCheckWasOn}
  {$endif}
  {$ENDIF}
end;

procedure pgSQLStrToFloatDef(Value: PAnsiChar; const Def: Double;
  var Result: Double); overload;
begin
  {$IFDEF FPC2_6DOWN}
  {$ifopt R+}
  {$define RangeCheckWasOn}
  {$R-}
  {$endif opt R+}
  {$ifopt Q+}
  {$define OverflowCheckWasOn}
  {$Q-}
  {$endif opt Q+}
  {$ENDIF}
  if Value = 'Infinity' then
    Result := Infinity
  else if Value = '-Infinity' then
    Result := NegInfinity
  else if Value = 'NaN' then
    Result := NaN
  else
    ZSysUtils.SQLStrToFloatDef(Value, Def, Result);
  {$IFDEF FPC2_6DOWN}
  {$ifdef RangeCheckWasOn}
  {$R+}
  {$undef RangeCheckWasOn}
  {$endif}
  {$ifdef OverflowCheckWasOn}
  {$Q+}
  {$undef OverflowCheckWasOn}
  {$endif}
  {$ENDIF}
end;

{ TZAbstractPostgreSQLStringResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZAbstractPostgreSQLStringResultSet.ColumnsToJSON(
  JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
var
  C, L: Cardinal;
  P, pgBuff: PAnsiChar;
  RNo, H, I: Integer;
  Blob: IZBlob;
label ProcBts;
begin
  RNo := RowNo - 1;
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    if FPlainDriver.PQgetisnull(Fres, RNo, C) <> 0 then
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
      P := FPlainDriver.PQgetvalue(Fres, RNo, C);
      case TZColumnInfo(ColumnsInfo[c]).ColumnType of
        stUnknown     : JSONWriter.AddShort('null');
        stBoolean     : JSONWriter.AddShort(JSONBool[StrToBoolEx(P, True, (FpgOIDTypes[C] = 18) { char } or (FpgOIDTypes[C] = 1042)  { char/bpchar })]);
        stByte,
        stShort,
        stWord,
        stSmall,
        stLongWord,
        stInteger,
        stULong,
        stLong,
        stFloat,
        stDouble,
        stBigDecimal  : JSONWriter.AddNoJSONEscape(P, ZFastCode.StrLen(P));
        stCurrency    : JSONWriter.AddDouble(ZSysUtils.SQLStrToFloatDef(P, 0, ZFastCode.StrLen(P)));
        stBytes,
        stBinaryStream: if FpgOIDTypes[C] = 17{bytea} then begin
                          pgBuff := nil;
                          if FIs_bytea_output_hex then begin
                            {skip trailing /x}
                            L := (ZFastCode.StrLen(P)-2) shr 1;
                            try
                              GetMem(pgBuff, L);
                              HexToBin(P+2, pgBuff, L);
                              JSONWriter.WrBase64(pgBuff, L, True);
                            finally
                              FreeMem(pgBuff);
                            end;
                          end else if Assigned(FPlainDriver.PQUnescapeBytea) then
                            try
                              pgBuff := FPlainDriver.PQUnescapeBytea(P, @L);
                              JSONWriter.WrBase64(pgBuff, L, True);
                            finally
                              FPlainDriver.PQFreemem(pgBuff);
                            end
                          else
                            JSONWriter.WrBase64(P, FPlainDriver.PQgetlength(Fres, RNo, C), True);
                        end else begin
                          Blob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^, RawToIntDef(P, 0), FChunk_Size);
                          JSONWriter.WrBase64(Blob.GetBuffer, Blob.Length, True);
                        end;
        stGUID        : begin
                          JSONWriter.Add('"');
                          JSONWriter.AddNoJSONEscape(P);//
                          JSONWriter.Add('"');
                        end;
        stDate        : if jcoMongoISODate in JSONComposeOptions then begin
                          JSONWriter.AddShort('ISODate("');
                          JSONWriter.AddNoJSONEscape(P, 10);
                          JSONWriter.AddShort('Z)"');
                        end else begin
                          if jcoDATETIME_MAGIC in JSONComposeOptions
                          then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                          else JSONWriter.Add('"');
                          JSONWriter.AddNoJSONEscape(P, 10);
                          JSONWriter.Add('"');
                        end;
        stTime        : if jcoMongoISODate in JSONComposeOptions then begin
                          JSONWriter.AddShort('ISODate("0000-00-00T');
                          JSONWriter.AddNoJSONEscape(P, 8); //mongo has no milliseconds
                          JSONWriter.AddShort('Z)"');
                        end else begin
                          if jcoDATETIME_MAGIC in JSONComposeOptions
                          then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                          else JSONWriter.Add('"');
                          JSONWriter.AddShort('T');
                          if ((P+8)^ <> '.') or not (jcoMilliseconds in JSONComposeOptions) //time zone ?
                          then JSONWriter.AddNoJSONEscape(P, 8)
                          else JSONWriter.AddNoJSONEscape(P, 12);
                          JSONWriter.Add('"');
                        end;
        stTimestamp   : if jcoMongoISODate in JSONComposeOptions then begin
                          JSONWriter.AddShort('ISODate("');
                          JSONWriter.AddNoJSONEscape(P, 10);
                          JSONWriter.Add('T');
                          JSONWriter.AddNoJSONEscape(P+11, 8);//mongo has no milliseconds
                          JSONWriter.AddShort('Z)"');
                        end else begin
                          if jcoDATETIME_MAGIC in JSONComposeOptions
                          then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                          else JSONWriter.Add('"');
                          JSONWriter.AddNoJSONEscape(P, 10);
                          JSONWriter.Add('T');
                          if ((P+19)^ <> '.') or not (jcoMilliseconds in JSONComposeOptions)
                          then JSONWriter.AddNoJSONEscape(P+11, 8)
                          else JSONWriter.AddNoJSONEscape(P+11, 12);
                          JSONWriter.Add('"');
                        end;
        stString,
        stUnicodeString:begin
                          JSONWriter.Add('"');
                          if (FpgOIDTypes[C] = 18) { char } or (FpgOIDTypes[C] = 1042)  { char/bpchar } then begin
                            L := ZFastCode.StrLen(P);
                            if (L > 0) then while (P+L-1)^ = ' ' do dec(L);
                            JSONWriter.AddJSONEscape(P, L);
                          end else
                            JSONWriter.AddJSONEscape(P);
                          JSONWriter.Add('"');
                        end;
        stAsciiStream,
        stUnicodeStream:begin
                          JSONWriter.Add('"');
                          JSONWriter.AddJSONEscape(P);
                          JSONWriter.Add('"');
                        end;
        //stArray, stDataSet,
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
  @param PlainDriver a PostgreSQL plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a PostgreSQL specific query handle.
}
constructor TZAbstractPostgreSQLStringResultSet.Create(const Statement: IZStatement;
  const SQL: string; connAddress: PPGconn; resAddress: PPGresult; const CachedLob: Boolean;
  const Chunk_Size, UndefinedVarcharAsStringLength: Integer);
begin
  inherited Create(Statement, SQL,
    TZPostgresResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  FconnAddress := connAddress;
  Fres := resAddress^;
  FresAddress := resAddress;
  FPlainDriver := TZPostgreSQLPlainDriver(Statement.GetConnection.GetIZPlainDriver.GetInstance);
  ResultSetConcurrency := rcReadOnly;
  FChunk_Size := Chunk_Size; //size of read/write lob in chunks
  FUndefinedVarcharAsStringLength := UndefinedVarcharAsStringLength;
  FIs_bytea_output_hex := (Statement.GetConnection as IZPostgreSQLConnection).Is_bytea_output_hex;
  FCachedLob := CachedLob;

  Open;
end;

procedure TZAbstractPostgreSQLStringResultSet.ClearPGResult;
begin
  if Fres <> nil then
  begin
    FPlainDriver.PQclear(Fres);
    Fres := nil;
  end;
end;

{**
  Converts a PostgreSQL native types into ZDBC SQL types.
  @param ColumnIndex a column index.
  @param ColumnInfo a column description object.
  @param TypeOid a type oid.
  @return a SQL undepended type.
}
procedure TZAbstractPostgreSQLStringResultSet.DefinePostgreSQLToSQLType(
  ColumnInfo: TZColumnInfo; const TypeOid: Oid);
var
  SQLType: TZSQLType;
  Connection: IZPostgreSQLConnection;
begin
  Connection := Statement.GetConnection as IZPostgreSQLConnection;

  case TypeOid of
    CASHOID: ColumnInfo.Currency := True; { money }
    NAMEOID: if (Connection.GetServerMajorVersion < 7) or
           ((Connection.GetServerMajorVersion = 7) and (Connection.GetServerMinorVersion < 3)) then
          ColumnInfo.Precision := 32
        else
          ColumnInfo.Precision := 64; { name }
    CIDROID: ColumnInfo.Precision := 100; { cidr }
    INETOID: ColumnInfo.Precision := 100; { inet }
    MACADDROID: ColumnInfo.Precision := 17; { macaddr }
    INTERVALOID: ColumnInfo.Precision := 32; { interval }
    REGPROCOID: ColumnInfo.Precision := 64; { regproc } // M.A. was 10
    BYTEAOID:{ bytea }
      if Connection.IsOidAsBlob then
        ColumnInfo.Precision := 256;
  end;

  SQLType := PostgreSQLToSQLType(ConSettings, Connection.IsOidAsBlob, TypeOid);

  if SQLType <> stUnknown then
    ColumnInfo.ColumnType := SQLType
  else
  begin
    ColumnInfo.ColumnType := stString;
    ColumnInfo.Precision := 255;
    ColumnInfo.ReadOnly := True;
  end;
end;

{**
  Opens this recordset.
}
procedure TZAbstractPostgreSQLStringResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZPGColumnInfo;
  FieldMode, FieldSize, FieldType, FieldCount: Integer;
  P: PAnsiChar;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  { Fills the column info. }
  ColumnsInfo.Clear;
  FieldCount := FPlainDriver.PQnfields(Fres);
  SetLength(FpgOIDTypes, FieldCount);
  for I := 0 to FieldCount - 1 do
  begin
    ColumnInfo := TZPGColumnInfo.Create;
    with ColumnInfo do
    begin
      TableOID := FPlainDriver.PQftable(Fres, I);
      TableColNo := FplainDriver.PQftablecol(Fres, I);
      //See: http://zeoslib.sourceforge.net/viewtopic.php?f=38&t=20797
      if TableColNo < 1 then
        // these fields have fixed numbers in the PostgreSQL source code, they seem to not use 0
        case TableColNo of
          0: ColumnName := '';
          -1: ColumnName := 'ctid';
          -2: ColumnName := 'oid';
          -3: ColumnName := 'xmin';
          -4: ColumnName := 'cmin';
          -5: ColumnName := 'xmax';
          -6: ColumnName := 'cmax';
          -7: ColumnName := 'tableoid';
        end;
      P := FPlainDriver.PQfname(Fres, I);
      Precision := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      ColumnLabel := PRawToUnicode(P, Precision, ConSettings^.ClientCodePage^.CP);
      {$ELSE}
      if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
        ColumnLabel := BufferToStr(P, Precision)
      else
        ColumnLabel := ZUnicodeToString(PRawToUnicode(P, Precision, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP);
      {$ENDIF}
      ColumnDisplaySize := 0;
      Scale := 0;
      Precision := 0;

      AutoIncrement := False;
      Signed := False;
      Nullable := ntNullable;

      FieldType := FPlainDriver.PQftype(Fres, I);

      FpgOIDTypes[i] := FieldType;
      DefinePostgreSQLToSQLType(ColumnInfo, FieldType);
      if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
        ColumnCodePage := ConSettings^.ClientCodePage^.CP
      else
        ColumnCodePage := High(Word);

      if Precision = 0 then begin
        FieldMode := FPlainDriver.PQfmod(Fres, I);
        FieldSize := FPlainDriver.PQfsize(Fres, I);
        Precision := Max(Max(FieldMode - 4, FieldSize), 0);

        if ColumnType in [stString, stUnicodeString] then begin
          {begin patch: varchar() is equal to text!}
          if ( FieldMode = -1 ) and ( FieldSize = -1 ) and ( FieldType = 1043) then
            if FUndefinedVarcharAsStringLength > 0 then begin
              Precision := FUndefinedVarcharAsStringLength;
            end else
              DefinePostgreSQLToSQLType(ColumnInfo, 25) //assume text instead!
          else if ( (ColumnLabel = 'expr') or ( Precision = 0 ) ) then
            Precision := 255;
          if ColumnType = stString then begin
            CharOctedLength := Precision * ConSettings^.ClientCodePage^.CharWidth;
            ColumnDisplaySize := Precision;
          end else if ColumnType = stUnicodeString then begin
            CharOctedLength := Precision shl 1;
            ColumnDisplaySize := Precision;
          end;
        end;
      end;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

{**
  Resets cursor position of this recordset and
  reset the prepared handles.
}
procedure TZAbstractPostgreSQLStringResultSet.ResetCursor;
begin
  if not Closed then begin
    ClearPGResult;
    inherited ResetCursor;
  end;
end;
{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractPostgreSQLStringResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  Result := FPlainDriver.PQgetisnull(Fres, RowNo - 1,
    ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}) <> 0;
end;

function TZAbstractPostgreSQLStringResultSet.GetBuffer(ColumnIndex: Integer; out Len: NativeUint): PAnsiChar;
begin
  LastWasNull := FPlainDriver.PQgetisnull(Fres, PGRowNo, ColumnIndex) <> 0;

  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
  begin
    Result := FPlainDriver.PQgetvalue(Fres, PGRowNo, ColumnIndex);
    if (FpgOIDTypes[ColumnIndex] = CHAROID) and not (FIs_bytea_output_hex or Assigned(FPlainDriver.PQUnescapeBytea)) then
      Len := FPlainDriver.PQgetlength(Fres, PGRowNo, ColumnIndex)
    else begin
      {http://www.postgresql.org/docs/9.0/static/libpq-exec.html
      PQgetlength:
       This is the actual data length for the particular data value, that is,
       the size of the object pointed to by PQgetvalue.
       For text data format this is the same as strlen().
       For binary format this is essential information.
       Note that one should not rely on PQfsize to obtain the actual data length.}
      Len := ZFastCode.StrLen(Result);
      if (FpgOIDTypes[ColumnIndex] = CHAROID) { char } or
         (FpgOIDTypes[ColumnIndex] = BPCHAROID)  { char/bpchar } then
        while (Result+Len-1)^ = ' ' do dec(Len); //remove Trailing spaces for fixed character fields
    end;
  end;
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
function TZAbstractPostgreSQLStringResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
  Result := GetBuffer(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractPostgreSQLStringResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>''</code>, the
    value returned is <code>null</code>
}
function TZAbstractPostgreSQLStringResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  P: PAnsiChar;
  L: NativeUInt;
  WS: ZWideString;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  P := GetBuffer(ColumnIndex, L);
  if LastWasNull then
    Result := ''
  else
    if (ConSettings^.ClientCodePage.CP = zCP_UTF8) or (FpgOIDTypes[ColumnIndex] = BYTEAOID) {bytea} then
      ZSetString(P, L, Result)
    else
    begin
      WS := PRawToUnicode(P, L, ConSettings^.ClientCodePage.CP);
      {$IFDEF WITH_RAWBYTESTRING}
      Result := UTF8String(WS);
      {$ELSE}
      Result := ZUnicodeToRaw(WS, zCP_UTF8);
      {$ENDIF}
    end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractPostgreSQLStringResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
  Buffer := GetBuffer(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Len);
  if LastWasNull then
    Result := ''
  else
    ZSetString(Buffer, Len, Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractPostgreSQLStringResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := False
  else
    Result := StrToBoolEx(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), True,
      (FpgOIDTypes[ColumnIndex] = CHAROID) { char } or (FpgOIDTypes[ColumnIndex] = BPCHAROID)  { char/bpchar });
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    Result := RawToIntDef(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    Result := RawToInt64Def(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractPostgreSQLStringResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
   Result := RawToUInt64Def(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0);
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
function TZAbstractPostgreSQLStringResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    pgSQLStrToFloatDef(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0, Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractPostgreSQLStringResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    pgSQLStrToFloatDef(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0, Result);
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
function TZAbstractPostgreSQLStringResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    pgSQLStrToFloatDef(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0, Result);
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
function TZAbstractPostgreSQLStringResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  Buffer, pgBuff: PAnsiChar;
  Len: cardinal;
  TempLob: IZBLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if not LastWasNull then begin
    if FpgOIDTypes[ColumnIndex] = BYTEAOID {bytea} then begin
      Buffer := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
      if FIs_bytea_output_hex then begin
        {skip trailing /x}
        SetLength(Result, (ZFastCode.StrLen(Buffer)-2) shr 1);
        if Assigned(Result) then
          HexToBin(Buffer+2, Pointer(Result), Length(Result));
      end else begin
        if Assigned(FPlainDriver.PQUnescapeBytea) then begin
          pgBuff := FPlainDriver.PQUnescapeBytea(Buffer, @Len);
          Result := BufferToBytes(pgBuff, Len);
          FPlainDriver.PQFreemem(pgBuff);
        end else begin
          Len := FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex);
          Result := BufferToBytes(Buffer, Len);
        end;
      end;
    end else if FpgOIDTypes[ColumnIndex] = UUIDOID { uuid } then begin
      SetLength(FUUIDOIDOutBuff, 16); //take care we've a unique dyn-array if so then this alloc happens once
      Result := FUUIDOIDOutBuff;
      ValidGUIDToBinary(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), Pointer(Result));
    end else if FpgOIDTypes[ColumnIndex] = OIDOID { oid } then begin
      TempLob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^,
        RawToIntDef(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0), FChunk_Size);
      Result := TempLob.GetBytes
    end else
      Result := StrToBytes(DecodeCString(InternalGetString(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))); // Marsupilami79: InternalGetString is doing the same index decrement, as it is done here, so we need to increment it again before we call it here.
  end else Result := nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractPostgreSQLStringResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Buffer := GetBuffer(ColumnIndex, Len);

  if LastWasNull then
    Result := 0
  else
    if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
      Result := RawSQLDateToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed)
    else
      Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
        RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractPostgreSQLStringResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Buffer := GetBuffer(ColumnIndex, Len);

  if LastWasNull then
    Result := 0
  else
    if not (Len > ConSettings^.ReadFormatSettings.TimeFormatLen) and ( ( ConSettings^.ReadFormatSettings.TimeFormatLen - Len) <= 4 )then
      Result := RawSQLTimeToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed)
    else
      Result := Frac(RawSQLTimeStampToDateTime(Buffer,  Len, ConSettings^.ReadFormatSettings, Failed));
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
function TZAbstractPostgreSQLStringResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Result := 0;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  if not LastWasNull then begin
    Buffer := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
    Result := RawSQLTimeStampToDateTime(Buffer, ZFastCode.StrLen(Buffer), ConSettings^.ReadFormatSettings, Failed);
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
function TZAbstractPostgreSQLStringResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  Buffer: PAnsiChar;
  Len: Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.PQgetisnull(Fres, RowNo - 1, ColumnIndex) <> 0;
  Result := nil;

  if (FpgOIDTypes[ColumnIndex] = OIDOID) { oid } and (Statement.GetConnection as IZPostgreSQLConnection).IsOidAsBlob then
    if LastWasNull then
      Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^, 0, FChunk_Size)
    else
      Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^,
        RawToIntDef(FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex), 0), FChunk_Size)
  else if not LastWasNull then
    if FpgOIDTypes[ColumnIndex] = BYTEAOID{bytea} then begin
      Buffer := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
      if FIs_bytea_output_hex then
        Result := TZPostgreSQLByteaHexBlob.Create(Buffer)
      else if Assigned(FPlainDriver.PQUnescapeBytea) then
        Result := TZPostgreSQLByteaEscapedBlob.Create(FPlainDriver, Buffer)
      else
        Result := TZAbstractBlob.CreateWithData(Buffer,
          FPlainDriver.PQgetlength(Fres, RowNo - 1, ColumnIndex));
    end else begin
      Buffer := FPlainDriver.PQgetvalue(Fres, RowNo - 1, ColumnIndex);
      Len := ZFastCode.StrLen(Buffer);
      if (FpgOIDTypes[ColumnIndex] = CHAROID) { char } or
         (FpgOIDTypes[ColumnIndex] = BPCHAROID)  { bpchar } then
        while (Buffer+Len-1)^ = ' ' do dec(Len); //remove Trailing spaces for fixed character fields
      Result := TZAbstractCLob.CreateWithData(Buffer, Len, ConSettings^.ClientCodePage^.CP, ConSettings);
    end;
end;

function TZAbstractPostgreSQLStringResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  if (Fres = nil) and (not Closed) and (RowNo=0)then
  begin
    Fres :=  FresAddress^;
    LastRowNo := FPlainDriver.PQntuples(Fres);
  end;
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
  begin
    if (ResultSetType = rtForwardOnly) then
      ClearPGResult;
    Exit;
  end;

  { Processes negative rows. }
  if Row < 0 then
  begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (ResultSetType <> rtForwardOnly) or (Row >= RowNo) then
  begin
    if (Row >= 0) and (Row <= LastRowNo + 1) then
    begin
      RowNo := Row;
      Result := (Row >= 1) and (Row <= LastRowNo);
    end
    else
      Result := False;
    if not Result and (ResultSetType = rtForwardOnly) then
      ClearPGResult;
  end
  else
    RaiseForwardOnlyException;
end;

{ TZPostgreSQLOidBlob }

{**
  Constructs this class and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
  @param Handle a PostgreSQL connection reference.
}
constructor TZPostgreSQLOidBlob.Create(const PlainDriver: TZPostgreSQLPlainDriver;
  const Data: Pointer; const Size: Integer; const Handle: TPGconn;
  const BlobOid: Oid; const Chunk_Size: Integer);
begin
  inherited CreateWithData(Data, Size);
  FHandle := Handle;
  FBlobOid := BlobOid;
  FPlainDriver := PlainDriver;
  FChunk_Size := Chunk_Size;
end;

{**
  Gets the blob handle oid.
  @return the blob handle oid.
}
function TZPostgreSQLOidBlob.GetBlobOid: Oid;
begin
  Result := FBlobOid;
end;

{**
  Reads the blob by the blob handle.
}
procedure TZPostgreSQLOidBlob.ReadLob;
var
  BlobHandle: Integer;
  Buffer: PAnsiChar;
  ReadNum: Integer;
  OffSet: Integer;
begin
  if not Updated and (FBlobOid > 0) then
  begin
    BlobHandle := FPlainDriver.lo_open(FHandle, FBlobOid, INV_READ);
    if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
      HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Read Large Object',nil);
    if BlobHandle >= 0 then
    begin
      Buffer := AllocMem(FChunk_Size+1);
      OffSet := 0;
      repeat
        ReadNum := FPlainDriver.lo_read(FHandle, BlobHandle,
          Buffer, FChunk_Size);
        Inc(OffSet, ReadNum);
        ReallocMem(FBlobData, OffSet);
        if ReadNum > 0 then
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, {%H-}Pointer({%H-}NativeUInt(FBlobData)+NativeUInt(OffSet-ReadNum))^, ReadNum);
      until ReadNum < FChunk_Size;
      BlobSize := OffSet;
      FPlainDriver.lo_close(FHandle, BlobHandle);
      FreeMem(Buffer, FChunk_Size+1);
    end;
    inherited ReadLob; //don't forget this...
  end;
end;

{**
  Writes the blob by the blob handle.
}
procedure TZPostgreSQLOidBlob.WriteLob;
begin
  WriteBuffer(BlobData, BlobSize);
end;

procedure TZPostgreSQLOidBlob.WriteBuffer(const Buffer: Pointer; const Len: integer);
var
  BlobHandle: Integer;
  Position: Integer;
  Size: Integer;
begin
  { Checks for empty blob. }
  if IsEmpty then
  begin
    FBlobOid := 0;
    Exit;
  end;

  { Creates a new large object. }
  if FBlobOid = 0 then begin
    FBlobOid := FPlainDriver.lo_creat(FHandle, INV_WRITE);
    if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
      HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Create Large Object',nil);
  end;

  { Opens and writes a large object. }
  BlobHandle := FPlainDriver.lo_open(FHandle, FBlobOid, INV_WRITE);
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
    HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Open Large Object',nil);

  Position := 0;
  while Position < Len do
  begin
    if (Len - Position) < FChunk_Size then
      Size := Len - Position
    else
      Size := FChunk_Size;
    FPlainDriver.lo_write(FHandle, BlobHandle,
      {%H-}Pointer({%H-}NativeUInt(Buffer) + NativeUInt(Position)), Size);
      if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
        HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Write Large Object',nil);
    Inc(Position, Size);
  end;

  FPlainDriver.lo_close(FHandle, BlobHandle);
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
    HandlePostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Close Large Object',nil);
end;
{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZPostgreSQLOidBlob.Clone(Empty: Boolean = False): IZBlob;
begin
  if Empty then
    Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
      FHandle, FBlobOid, FChunk_Size)
  else
    Result := TZPostgreSQLOidBlob.Create(FPlainDriver, BlobData, BlobSize,
      FHandle, FBlobOid, FChunk_Size);
end;

{ TZPostgreSQLByteaBlob }

constructor TZPostgreSQLByteaEscapedBlob.Create(const PlainDriver: TZPostgreSQLPlainDriver;
  Data: PAnsiChar);
var
  to_length: LongWord;
  pgBuffer: Pointer;
begin
  inherited CreateWithData(nil, 0);
  pgBuffer := PlainDriver.PQunescapeBytea(Data, @to_length);
  fBlobSize := to_length;
  if fBlobSize > 0 then begin
    System.GetMem(FBlobData, fBlobSize);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(pgBuffer^, FBlobData^, fBlobSize);
  end;
  PlainDriver.PQFreemem(pgBuffer);
end;

{ TZPostgreSQLByteaHexBlob }

constructor TZPostgreSQLByteaHexBlob.Create(Data: PAnsiChar);
begin
  inherited CreateWithData(nil, 0);
  {skip trailing /x}
  fBlobSize := (ZFastCode.StrLen(Data)-2) shr 1;
  if fBlobSize > 0 then begin
    System.GetMem(FBlobData, fBlobSize);
    HexToBin(Data+2, fBlobData, fBlobSize);
  end;
end;

{ TZPostgresResultSetMetadata }

procedure TZPostgresResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  //ColumnInfo.ColumnName := '';
end;

{**
  Initializes columns with additional data.
}
procedure TZPostgresResultSetMetadata.LoadColumns;
{$IFNDEF ZEOS_TEST_ONLY}
var
  Current: TZPGColumnInfo;
  I: Integer;
  PGMetaData: IZPGDatabaseMetadata;
  RS: IZResultSet;
{$ENDIF}
begin
  {$IFDEF ZEOS_TEST_ONLY}
  inherited LoadColumns;
  {$ELSE}
  if Metadata.GetConnection.GetDriver.GetStatementAnalyser.DefineSelectSchemaFromQuery(Metadata.GetConnection.GetDriver.GetTokenizer, SQL) <> nil then
    for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
      Current := TZPGColumnInfo(ResultSet.ColumnsInfo[i]);
      ClearColumn(Current);
      PGMetaData := MetaData as IZPGDatabaseMetadata;
      RS := PGMetaData.GetColumnsByTableOID(Current.TableOID);
      if RS <> nil then begin
        RS.BeforeFirst;
        while RS.Next do
          if RS.GetInt(TableColColumnOrdPosIndex) = Current.TableColNo then begin
            FillColumInfoFromGetColumnsRS(Current, RS, RS.GetString(ColumnNameIndex));
            Break;
          end else
            if RS.GetInt(TableColColumnOrdPosIndex) > Current.TableColNo then
              Break;
      end;
    end;
  Loaded := True;
  {$ENDIF}
end;

{ TZClientCursorPostgreSQLStringResultSet }

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
function TZClientCursorPostgreSQLStringResultSet.MoveAbsolute(
  Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  if (Fres = nil) and (not Closed) and (RowNo=0)then
  begin
    Fres := FresAddress^;
    LastRowNo := FPlainDriver.PQntuples(Fres);
  end;
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
  begin
    if (ResultSetType = rtForwardOnly) then
      ClearPGResult;
    Exit;
  end;

  { Processes negative rows. }
  if Row < 0 then
  begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (ResultSetType <> rtForwardOnly) or (Row >= RowNo) then
  begin
    if (Row >= 0) and (Row <= LastRowNo + 1) then
    begin
      RowNo := Row;
      Result := (Row >= 1) and (Row <= LastRowNo);
    end
    else
      Result := False;
    if not Result and (ResultSetType = rtForwardOnly) then
      ClearPGResult;
  end
  else
    RaiseForwardOnlyException;
end;

{**
  Opens this recordset.
}
procedure TZClientCursorPostgreSQLStringResultSet.Open;
begin
  if not Assigned(Fres) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  LastRowNo := FPlainDriver.PQntuples(Fres);
  inherited open;
end;

function TZClientCursorPostgreSQLStringResultSet.PGRowNo: Integer;
begin
  Result := RowNo-1;
end;

{ TZServerCursorPostgreSQLStringResultSet }

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
function TZServerCursorPostgreSQLStringResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if ((MaxRows > 0) and (LastRowNo >= MaxRows)) or (RowNo > LastRowNo) then
    Exit;
  if RowNo = 0 then
    if not Assigned(Fres) then begin
      Fres := FresAddress^;
      if FPlainDriver.PQsetSingleRowMode(FconnAddress^) <> Ord(PGRES_COMMAND_OK) then
        HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcOther, 'open recordset', Fres);
    end else
      FplainDriver.PQclear(Fres)
end;

{**
  Opens this recordset.
}
procedure TZServerCursorPostgreSQLStringResultSet.Open;
begin
  if ResultSetType <> rtForwardOnly then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);
  if FPlainDriver.PQsetSingleRowMode(FconnAddress^) <> Ord(PGRES_COMMAND_OK) then
    HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcOther, 'open recordset', Fres);
  inherited Open;
end;

function TZServerCursorPostgreSQLStringResultSet.PGRowNo: Integer;
begin
  Result := 0;
end;

end.

