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

uses
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZDbcIntfs, ZDbcResultSet, ZDbcInterbase6, ZPlainFirebirdInterbaseConstants,
  ZPlainFirebirdDriver, ZCompatibility, ZDbcResultSetMetadata, ZMessages,
  ZDbcInterbase6Utils, ZSelectSchema;

type

  {** Implements Interbase ResultSet. }
  TZInterbase6XSQLDAResultSet = class(TZAbstractResultSet)
  private
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FStmtHandle: TISC_STMT_HANDLE;
    fPISC_TR_HANDLE: PISC_TR_HANDLE; //save address
    fTISC_TR_HANDLE: TISC_TR_HANDLE; //save current
    fWasLastResult: Boolean;
    FXSQLDA: PXSQLDA;
    FIZSQLDA: IZSQLDA;
    FPISC_DB_HANDLE: PISC_DB_HANDLE;
    FBlobTemp: IZBlob;
    FPlainDriver: IZInterbasePlainDriver;
    FDialect: Word;
    FCodePageArray: TWordDynArray;
    FStmtType: TZIbSqlStatementType;
    function GetIbSqlSubType(const Index: Word): Smallint; {$IF defined(WITH_INLINE) and not (defined(WITH_URW1135_ISSUE) or defined(WITH_URW1111_ISSUE))} inline; {$IFEND}
    function GetQuad(ColumnIndex: Integer): TISC_QUAD;
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      StatementHandle: TISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
      WasLastResult, CachedBlob: boolean; StmtType: TZIbSqlStatementType);

    procedure Close; override;
    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetCurrency(ColumnIndex: Integer): Currency; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function Next: Boolean; override;
  end;

  {** Implements external blob wrapper object for Intebase/Firbird. }
  TZInterbase6UnCachedBlob = Class(TZAbstractUnCachedBlob, IZUnCachedLob)
  private
    FBlobId: TISC_QUAD;
    FDBHandle: PISC_DB_HANDLE;
    fTISC_TR_HANDLE: PISC_TR_HANDLE;
    FPlainDriver: IZInterbasePlainDriver;
    FConSettings: PZConSettings;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const DBHandle: PISC_DB_HANDLE;
      const TrHandle: PISC_TR_HANDLE; const PlainDriver: IZInterbasePlainDriver;
      var BlobId: TISC_QUAD; Const ConSettings: PZConSettings);
  end;

  TZInterbase6UnCachedClob = Class(TZAbstractUnCachedClob, IZUnCachedLob)
  private
    FBlobId: TISC_QUAD;
    FDBHandle: PISC_DB_HANDLE;
    fTISC_TR_HANDLE: PISC_TR_HANDLE;
    FPlainDriver: IZInterbasePlainDriver;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const DBHandle: PISC_DB_HANDLE;
      const TrHandle: PISC_TR_HANDLE; const PlainDriver: IZInterbasePlainDriver;
      var BlobId: TISC_QUAD; Const ConSettings: PZConSettings);
  end;

  {** Implements Interbase ResultSetMetadata object. }
  TZInterbaseResultSetMetadata = Class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
  public
    function GetCatalogName(ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetSchemaName(ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  End;

implementation

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  ZEncoding, ZFastCode, ZSysUtils, ZDbcMetadata;

procedure GetPCharFromTextVar(SQLCode: SmallInt; sqldata: Pointer; sqllen: Short; out P: PAnsiChar; out Len: NativeUInt); {$IF defined(WITH_INLINE)} inline; {$IFEND}
begin
  case SQLCode of
    SQL_TEXT:
      begin
        P := sqldata;
        // Trim only trailing spaces. TrimRight also removes other characters)
        Len := sqllen;
        if Len > 0 then while (P + Len - 1)^ = ' ' do Dec(Len);
      end;
    SQL_VARYING:
      begin
        P := PISC_VARYING(sqldata).str;
        Len := PISC_VARYING(sqldata).strlen;
      end;
    else // should not happen
      begin
        P := nil;
        Len := 0;
      end;
  end;
end;

function GetRawFromTextVar(SQLCode: SmallInt; sqldata: Pointer; sqllen: Short): RawByteString; {$IF defined(WITH_INLINE)} inline; {$IFEND}
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
  GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
  if Len > 0 then
    ZSetString(P, Len, Result{%H-})
  else
    Result := '';
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
  const SQL: string; StatementHandle: TISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
  WasLastResult, CachedBlob: Boolean; StmtType: TZIbSqlStatementType);
var FIBConnection: IZInterbase6Connection;
begin
  inherited Create(Statement, SQL, TZInterbaseResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);

  FFetchStat := 0;
  FIZSQLDA := XSQLDA; //localize the interface to avoid automatic free the object
  FXSQLDA := XSQLDA.GetData; // localize buffer for fast access

  FCachedBlob := CachedBlob;
  FIBConnection := Statement.GetConnection as IZInterbase6Connection;
  fPISC_TR_HANDLE := FIBConnection.GetTrHandle;
  fTISC_TR_HANDLE := fPISC_TR_HANDLE^;
  FPISC_DB_HANDLE := FIBConnection.GetDBHandle;
  FPlainDriver := FIBConnection.GetPlainDriver;
  FDialect := FIBConnection.GetDialect;
  FStmtType := StmtType; //required to know how to fetch the columns for ExecProc
  fWasLastResult := WasLastResult;

  FStmtHandle := StatementHandle;
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
procedure TZInterbase6XSQLDAResultSet.Close;
begin
  { Free output allocated memory }
  FXSQLDA := nil;
  FIZSQLDA := nil;
  inherited Close; //Calls ResetCursor so FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_CLOSE); is called
  { Free allocate sql statement }
  FStmtHandle := 0; //don't forget!
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
function TZInterbase6XSQLDAResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
function TZInterbase6XSQLDAResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  BlobId: TISC_QUAD;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Result := nil;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then begin
    BlobId := GetQuad(ColumnIndex);
    if FCachedBlob then
      case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType of
        stBinaryStream:
          begin
            Result := TZAbstractBlob.Create;
            ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, fPISC_TR_HANDLE,
              BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, True, ConSettings);
          end;
        stAsciiStream, stUnicodeStream:
          begin
            Result := TZAbstractClob.CreateWithData(nil, 0, Consettings^.ClientCodePage^.CP, ConSettings);
            ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, fPISC_TR_HANDLE,
              BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, False, ConSettings);
          end;
      end
    else
      case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType of
        stBinaryStream:
          Result := TZInterbase6UnCachedBlob.Create(FPISC_DB_HANDLE, fPISC_TR_HANDLE,
            FPlainDriver, BlobId, ConSettings);
        stAsciiStream, stUnicodeStream:
          Result := TZInterbase6UnCachedClob.Create(FPISC_DB_HANDLE, fPISC_TR_HANDLE,
            FPlainDriver, BlobId, ConSettings);
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
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := False
  else
  begin
    {$R-}
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale] <> 0;
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale] <> 0;
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale] <> 0;
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^) > 0;
        else goto Fail;
        end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^) <> 0;
          SQL_LONG      : Result := PInteger(sqldata)^ <> 0;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^) <> 0;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^ <> 0;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^<>0;
          SQL_SHORT     : Result := PSmallint(sqldata)^ <> 0;
          SQL_INT64     : Result := PInt64(sqldata)^ <> 0;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := StrToBoolEx(P);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := StrToBoolEx(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString)
            else
              goto Fail;
        else
          Fail:
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  SQLCode: SmallInt;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := nil
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      case SQLCode of
        SQL_TEXT, SQL_VARYING:
          Result := BufferToBytes(sqldata, sqllen);
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
      end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  TempDate: TCTimeStructure;
  Len: NativeUInt;
  P: PAnsiChar;
  Failed: Boolean;
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
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
            FPlainDriver.isc_decode_timestamp(PISC_TIMESTAMP(sqldata), @TempDate);
            Result := SysUtils.EncodeDate(TempDate.tm_year + 1900,
              TempDate.tm_mon + 1, TempDate.tm_mday);
          end;
        SQL_TYPE_DATE :
          begin
            FPlainDriver.isc_decode_sql_date(PISC_DATE(sqldata), @TempDate);
            Result := SysUtils.EncodeDate(Word(TempDate.tm_year + 1900),
              Word(TempDate.tm_mon + 1), Word(TempDate.tm_mday));
          end;
        SQL_TYPE_TIME : Result := 0;
        SQL_TEXT,
        SQL_VARYING:
          begin
            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
            if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
              Result := RawSQLDateToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Result = 0;
          end;
        else
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}));
      end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$R-}
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result {%H-}:= PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result {%H-}:= PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result {%H-}:= PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else goto Fail;
        end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := RawToIntDef(P, 0);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToIntDef(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else
              goto Fail;
        else
          Fail:
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          goto Fail;
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := RawToInt64Def(P, 0);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToInt64Def(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else
              goto Fail;
        else
          Fail:
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
function TZInterbase6XSQLDAResultSet.GetULong(ColumnIndex: Integer): UInt64;
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    {$IFDEF WITH_UINT64_C1118_ERROR}
    Result := UInt64(0) //need that type cast for D7 else "internal error C1118"
    {$ELSE}
    Result := 0
    {$ENDIF}
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          goto Fail;
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := RawToUInt64Def(P, 0);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToUInt64Def(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else
              goto Fail;
        else
          Fail:
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
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
function TZInterbase6XSQLDAResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  SQLCode: SmallInt;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    Result := '';
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := FloatToRaw(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToRaw(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToRaw(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToRaw(PDouble(sqldata)^);
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := FloatToRaw(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToRaw(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToRaw(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := BoolToRawEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: Result := BoolToRawEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   : Result := GetRawFromTextVar(SQLCode, sqldata, sqllen);
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex);
              if FBlobTemp.IsClob then
                Result := FBlobTemp.GetRawByteString
              else
                Result := FBlobTemp.GetString;
              FBlobTemp := nil;
            End;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  TempDate: TCTimeStructure;
  Failed: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}] do
    begin
      SQLCode := sqltype and not(1);
      case SQLCode of
        SQL_TIMESTAMP :
          begin
            FPlainDriver.isc_decode_timestamp(PISC_TIMESTAMP(sqldata), @TempDate);
            Result := EncodeTime(TempDate.tm_hour, TempDate.tm_min,
              TempDate.tm_sec, Word((PISC_TIMESTAMP(sqldata).timestamp_time mod 10000) div 10));
          end;
        SQL_TYPE_DATE : Result := 0;
        SQL_TYPE_TIME :
          begin
            FPlainDriver.isc_decode_sql_time(PISC_TIME(sqldata), @TempDate);
            Result := SysUtils.EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
              Word(TempDate.tm_sec),  Word((PISC_TIME(sqldata)^ mod 10000) div 10));
          end;
        SQL_TEXT, SQL_VARYING:
          begin
            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
            if (P+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              Result := Frac(RawSQLTimeStampToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed));
          end;
        else
          Result := Frac(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}));
      end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  Failed: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;
  SQLCode: SmallInt;
  TempDate: TCTimeStructure;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}] do
    begin
      SQLCode := sqltype and not(1);
      case SQLCode of
        SQL_TIMESTAMP :
          begin
            FPlainDriver.isc_decode_timestamp(PISC_TIMESTAMP(sqldata), @TempDate);
            Result := SysUtils.EncodeDate(TempDate.tm_year + 1900,
              TempDate.tm_mon + 1, TempDate.tm_mday) + EncodeTime(TempDate.tm_hour,
            TempDate.tm_min, TempDate.tm_sec, Word((PISC_TIMESTAMP(sqldata).timestamp_time mod 10000) div 10));
          end;
        SQL_TYPE_DATE :
          begin
            FPlainDriver.isc_decode_sql_date(PISC_DATE(sqldata), @TempDate);
            Result := SysUtils.EncodeDate(Word(TempDate.tm_year + 1900),
              Word(TempDate.tm_mon + 1), Word(TempDate.tm_mday));
          end;
        SQL_TYPE_TIME :
          begin
            FPlainDriver.isc_decode_sql_time(PISC_TIME(sqldata), @TempDate);
            Result := SysUtils.EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
              Word(TempDate.tm_sec),  Word((PISC_TIME(sqldata)^ mod 10000) div 10));
          end;
        SQL_TEXT, SQL_VARYING:
          begin
            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
            if (P+2)^ = ':' then
              Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4 then
                Result := RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
              else
                Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
          end;
        else
          Result := GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF});
      end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
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
  {$IFOPT D+}
  {$R+}
  {$ENDIF}
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
function TZInterbase6XSQLDAResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  SQLCode: SmallInt;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : FRawTemp := FloatToRaw(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : FRawTemp := FloatToRaw(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : FRawTemp := FloatToRaw(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : FRawTemp := FloatToRaw(PDouble(sqldata)^);
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : FRawTemp := FloatToRaw(PDouble(sqldata)^);
          SQL_LONG      : FRawTemp := IntToRaw(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : FRawTemp := FloatToRaw(PSingle(sqldata)^);
          SQL_BOOLEAN   : FRawTemp := BoolToRawEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: FRawTemp := BoolToRawEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : FRawTemp := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : FRawTemp := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, Result, Len);
              Exit;
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});  //localize interface to keep pointer alive
              if FBlobTemp.IsClob then
              begin
                Result := FBlobTemp.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                Len := FBlobTemp.Length;
              end
              else
              begin
                Result := FBlobTemp.GetBuffer;
                Len := FBlobTemp.Length;
              End;
              Exit;
            End;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    Result := Pointer(FRawTemp);
    Len := NativeUInt({%H-}PLengthInt(NativeUInt(FRawTemp) - StringLenOffSet)^);
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var
  Len: NativeUInt;
  P: PAnsiChar;
begin
  P := GetPAnsiChar(ColumnIndex, Len);
  ZSetString(P, Len, FRawTemp);
  Result := Pointer(FRawTemp);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := FloatToRaw(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToRaw(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToRaw(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToRaw(PDouble(sqldata)^);
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := FloatToRaw(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToRaw(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToRaw(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := BoolToRawEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: Result := BoolToRawEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              if sqlsubtype > High(FCodePageArray) then
                Result := ZConvertPRawToUTF8(P, Len, ConSettings^.ClientCodePage^.cp)
              else
                if (FCodePageArray[sqlsubtype] = zCP_UTF8) then
                  ZSetString(P, Len, Result)
                else
                  Result := ZConvertPRawToUTF8(P, Len, sqlsubtype);
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});  //localize interface to keep pointer alive
              if FBlobTemp.IsClob then
                Result := FBlobTemp.GetUTF8String
              else
                Result := FBlobTemp.GetString;
            End;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
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
function TZInterbase6XSQLDAResultSet.GetString(ColumnIndex: Integer): String;
var
  SubType: SmallInt;
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := FloatToStr(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToStr(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToStr(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToStr(PDouble(sqldata)^);
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_LONG      : Result := ZFastCode.IntToStr(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := {$IFDEF UNICODE}BoolToUnicodeEx{$ELSE}BoolToRawEx{$ENDIF}(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: Result := {$IFDEF UNICODE}BoolToUnicodeEx{$ELSE}BoolToRawEx{$ENDIF}(PByte(sqldata)^ <> 0);
          SQL_SHORT     : Result := ZFastCode.IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := ZFastCode.IntToStr(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);

              SubType := GetIbSqlSubType(ColumnIndex);
              if SubType > High(FCodePageArray) then
                {$IFDEF UNICODE}
                Result := PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP)
                {$ELSE}
                begin
                  ZSetString(P, Len, FRawTemp);
                  Result := ConSettings^.ConvFuncs.ZRawToString(FRawTemp,
                    ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
                end
                {$ENDIF}
              else
                {$IFDEF UNICODE}
                Result := PRawToUnicode(P, Len, FCodePageArray[SubType]);
                {$ELSE}
                begin
                  ZSetString(P, Len, FRawTemp);
                  Result := ConSettings^.ConvFuncs.ZRawToString(FRawTemp,
                    FCodePageArray[SubType], ConSettings^.CTRL_CP);
                end;
                {$ENDIF}
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});  //localize interface to keep pointer alive
              if FBlobTemp.IsClob then
                {$IFDEF UNICODE}
                Result := FBlobTemp.GetUnicodeString
                {$ELSE}
                Result := ConSettings^.ConvFuncs.ZRawToString(FBlobTemp.GetRawByteString,
                  ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)
                {$ENDIF}
              else
                {$IFDEF UNICODE}
                Result := ASCII7ToUnicodeString(FBlobTemp.GetRawByteString);
                {$ELSE}
                Result := FBlobTemp.GetRawByteString;
                {$ENDIF}
            End;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ZWideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
var
  SubType: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    SubType := GetIbSqlSubType(ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF});

    P := GetPAnsiChar(ColumnIndex, Len);
    if SubType > High(FCodePageArray) then
      Result := PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP)
    else
      Result := PRawToUnicode(P, Len, FCodePageArray[SubType]);
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
function TZInterbase6XSQLDAResultSet.Next: Boolean;
var
  StatusVector: TARRAY_ISC_STATUS;
  I, OldRow: Integer;
  RS: IZResultSet;
begin
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (LastRowNo >= MaxRows) or (FStmtHandle = 0) then
    Exit;

  { Fetch row. }
  if (ResultSetType = rtForwardOnly) and (FFetchStat = 0) then
  begin
    if (FStmtType = stSelect) then begin //AVZ - Test for ExecProc - this is for multiple rows
      { FireBirdAPI:
      Both isc_commit_transaction() and isc_rollback_transaction() close the record streams
      associated with the transaction, reinitialize the transaction name to zero, and release
      system resources allocated for the transaction. Freed system resources are available for
      subsequent use by any application or program. }
      if (fPISC_TR_HANDLE^ <> fTISC_TR_HANDLE) and (Statement.GetUpdateCount = 0) then begin //transaction changed and no updates done?
        OldRow := RowNo-1; //safe -> reuse resets the rowno
        //this finally goes to !self! or IZCachedResultSet containing !self! recursive
        //and scrolls to RowNo -1
        if fWasLastResult then begin
          (Statement as IZPreparedStatement).ExecutePrepared;
          RS := Statement.GetResultSet;
        end else
          RS := (Statement as IZPreparedStatement).ExecuteQueryPrepared;
        fTISC_TR_HANDLE := fPISC_TR_HANDLE^;//set current transaction handle
        for i := 1 to OldRow do
          RS.Next; //reload data
      end;
      FFetchStat := FPlainDriver.isc_dsql_fetch(@StatusVector,
        @FStmtHandle, FDialect, FXSQLDA);
      if FFetchStat = 0 then begin
        RowNo := RowNo + 1;
        LastRowNo := RowNo;
        Result := True;
      end else begin
        CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings);
        {no error occoured -> notify IsAfterLast and close the stmt}
        RowNo := RowNo + 1;
        FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE); //close handle but not free it
        CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings);
      end;
    end else begin
      FFetchStat := 1;
      Result := True;
    end;
  end;
end;

{**
   Get Interbase subsql type
   @param Index the index fields
   @return the Interbase subsql
}
function TZInterbase6XSQLDAResultSet.GetIbSqlSubType(const Index: Word): Smallint;
begin
  {$R-}
  result := FXSQLDA.sqlvar[Index].sqlsubtype;
  {$IFOPT D+}
  {$R+}
  {$ENDIF}
end;

{**
   Return Interbase QUAD field value
   @param Index the field index
   @return the field Interbase QUAD value
}
function TZInterbase6XSQLDAResultSet.GetQuad(ColumnIndex: Integer): TISC_QUAD;
begin
  {$R-}
  if not IsNull(ColumnIndex) then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FXSQLDA.sqlvar[ColumnIndex] do
      case (sqltype and not(1)) of
        SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY: result := PISC_QUAD(sqldata)^;
      else
        raise EZIBConvertError.Create(SUnsupportedDataType + ' ' + {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr((sqltype and not(1))));
      end;
  end
  else
    raise EZIBConvertError.Create('Invalid State.');
  {$IFOPT D+}
  {$R+}
  {$ENDIF}
end;

{**
  Opens this recordset.
}
procedure TZInterbase6XSQLDAResultSet.Open;
var
  I: Word;
  DataLen: SmallInt;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
  ZCodePageInfo: PZCodePage;
  CP: Word;
begin
  if FStmtHandle=0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
  if FXSQLDA.sqld > 0 then  //keep track we have a column to avoid range issues see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=10595
    for I := 0 to FXSQLDA.sqld {FieldCount} - 1 do
    begin
      ColumnInfo := TZColumnInfo.Create;
      with ColumnInfo do
      begin
        ColumnName := FIZSQLDA.GetFieldSqlName(I);
        TableName := FIZSQLDA.GetFieldRelationName(I);
        ColumnLabel := FIZSQLDA.GetFieldAliasName(I);
        FieldSqlType := FIZSQLDA.GetFieldSqlType(I);
        DataLen := FIZSQLDA.GetIbSqlLen(I);
        ColumnType := FieldSqlType;

        case FieldSqlType of
          stString, stUnicodeString:
            begin
              CP := GetIbSqlSubType(I);
              if (CP = ConSettings^.ClientCodePage^.ID) or //avoid the loops if we allready have the info's we need
                 (CP > High(FCodePageArray)) then //spezial case for collations like PXW_INTL850 which are nowhere to find in docs
                //see test Bug#886194, we retrieve 565 as CP...
                ZCodePageInfo := ConSettings^.ClientCodePage
              else
                //see: http://sourceforge.net/p/zeoslib/tickets/97/
                ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CP); //get column CodePage info
              ColumnCodePage := ZCodePageInfo^.CP;
              Precision := DataLen div ZCodePageInfo^.CharWidth;
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
          else
            begin
              ColumnCodePage := zCP_NONE;
              case FieldSqlType of
                stBytes:
                  Precision := DataLen;
                stShort, stSmall, stInteger, stLong:
                  Signed := True;
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
begin
  FFetchStat := 0;
  if (FStmtHandle <> 0) and not IsAfterLast{already done} then
    FreeStatement(FPlainDriver, FStmtHandle, DSQL_CLOSE); //close handle but not free it
  inherited ResetCursor;
end;

{ TZInterbase6UnCachedBlob }
{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
constructor TZInterbase6UnCachedBlob.Create(const DBHandle: PISC_DB_HANDLE;
  const TrHandle: PISC_TR_HANDLE; const PlainDriver: IZInterbasePlainDriver;
  var BlobId: TISC_QUAD;Const ConSettings: PZConSettings);
begin
  FBlobId := BlobId;
  FDBHandle := DBHandle;
  fTISC_TR_HANDLE := TrHandle;
  FPlainDriver := PlainDriver;
  FConSettings := ConSettings;
end;

procedure TZInterbase6UnCachedBlob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FDBHandle, fTISC_TR_HANDLE, FBlobId, Size{%H-}, Buffer{%H-}, True, FConSettings);
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
constructor TZInterbase6UnCachedClob.Create(const DBHandle: PISC_DB_HANDLE;
  const TrHandle: PISC_TR_HANDLE; const PlainDriver: IZInterbasePlainDriver;
  var BlobId: TISC_QUAD; const ConSettings: PZConSettings);
begin
  inherited CreateWithData(nil, 0, ConSettings^.ClientCodePage^.CP, ConSettings);
  FBlobId := BlobId;
  FDBHandle := DBHandle;
  fTISC_TR_HANDLE := TrHandle;
  FPlainDriver := PlainDriver;
end;

procedure TZInterbase6UnCachedClob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FDBHandle, fTISC_TR_HANDLE, FBlobId, Size{%H-}, Buffer{%H-}, False, FConSettings);
  (PAnsiChar(Buffer)+NativeUInt(Size))^ := #0; //add #0 terminator
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

end.
