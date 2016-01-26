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
  ZDbcInterbase6Utils;

type

  {** Implements Interbase ResultSet. }
  TZInterbase6XSQLDAResultSet = class(TZAbstractResultSet)
  private
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FStmtHandle: TISC_STMT_HANDLE;
    FXSQLDA: PXSQLDA;
    FIZSQLDA: IZSQLDA;
    FIBConnection: IZInterbase6Connection;
    FBlobTemp: IZBlob;
    FPlainDriver: IZInterbasePlainDriver;
    FDialect: Word;
    FCodePageArray: TWordDynArray;
    FStmtType: TZIbSqlStatementType;
    procedure CheckRange(const Index: Word); {$IFDEF WITH_INLINE} inline; {$ENDIF}
    function GetIbSqlSubType(const Index: Word): Smallint; {$IF defined(WITH_INLINE) and not (defined(WITH_URW1135_ISSUE) or defined(WITH_URW1111_ISSUE))} inline; {$IFEND}
    function DecodeString(const IsText: Boolean; const Index: Word): RawByteString; {$IF defined(WITH_INLINE) and not (defined(WITH_URW1135_ISSUE) or defined(WITH_URW1111_ISSUE))} inline; {$IFEND}
    function GetQuad(const Index: Integer): TISC_QUAD;
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      var StatementHandle: TISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
      const CachedBlob: boolean; const StmtType: TZIbSqlStatementType);

    procedure Close; override;
    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
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
    FTrHandle: PISC_TR_HANDLE;
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
    FTrHandle: PISC_TR_HANDLE;
    FPlainDriver: IZInterbasePlainDriver;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const DBHandle: PISC_DB_HANDLE;
      const TrHandle: PISC_TR_HANDLE; const PlainDriver: IZInterbasePlainDriver;
      var BlobId: TISC_QUAD; Const ConSettings: PZConSettings);
  end;

implementation

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  ZEncoding, ZFastCode, ZSysUtils;

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
  const SQL: string; var StatementHandle: TISC_STMT_HANDLE;
  const XSQLDA: IZSQLDA; const CachedBlob: Boolean;
  const StmtType: TZIbSqlStatementType);
begin
  inherited Create(Statement, SQL, nil,
    Statement.GetConnection.GetConSettings);

  FFetchStat := 0;
  FIZSQLDA := XSQLDA; //localize the interface to avoid automatic free the object
  FXSQLDA := XSQLDA.GetData; // localize buffer for fast access

  FCachedBlob := CachedBlob;
  FIBConnection := Statement.GetConnection as IZInterbase6Connection;
  FPlainDriver := FIBConnection.GetPlainDriver;
  FDialect := FIBConnection.GetDialect;
  FStmtType := StmtType; //required to know how to fetch the columns for ExecProc

  FStmtHandle := StatementHandle;
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;

  FCodePageArray := (Statement.GetConnection.GetIZPlainDriver as IZInterbasePlainDriver).GetCodePageArray;
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
  L: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then begin
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
          SQL_TEXT      : begin
                            l := sqllen; {last char = #0}
                            if L > 0 then
                              while ((sqldata+l-1)^ = ' ') do dec(l); {last char = #0}
                            ZSysUtils.SQLStrToFloatDef(PAnsiChar(sqldata), 0, Result, L)
                          end;
          SQL_VARYING   : ZSysUtils.SQLStrToFloatDef(PAnsiChar(@PISC_VARYING(sqldata).str[0]), 0, Result, PISC_VARYING(sqldata).strlen);
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
{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
function TZInterbase6XSQLDAResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  BlobId: TISC_QUAD;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  if not  LastWasNull then begin
    BlobId := GetQuad(ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF});
    if FCachedBlob then begin
      case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
        stBinaryStream: begin
          Result := TZAbstractBlob.Create;
          with FIBConnection do
            ReadBlobBufer(GetPlainDriver, GetDBHandle, GetTrHandle,
              BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, True, ConSettings);
        end;
        stAsciiStream, stUnicodeStream: begin
          Result := TZAbstractClob.CreateWithData(nil, 0, Consettings^.ClientCodePage^.CP, ConSettings);
          with FIBConnection do
            ReadBlobBufer(GetPlainDriver, GetDBHandle, GetTrHandle,
              BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, False, ConSettings);
        end;
      end;
    end else
      case GetMetaData.GetColumnType(ColumnIndex) of
        stBinaryStream:
          Result := TZInterbase6UnCachedBlob.Create(FIBConnection.GetDBHandle,
            FIBConnection.GetTrHandle, FIBConnection.GetPlainDriver, BlobId,
            ConSettings);
        stAsciiStream, stUnicodeStream:
          Result := TZInterbase6UnCachedClob.Create(FIBConnection.GetDBHandle,
            FIBConnection.GetTrHandle, FIBConnection.GetPlainDriver, BlobId,
            ConSettings);
      end;
  end;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  Result := False;
  if not LastWasNull then
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
          SQL_TEXT      : Result := StrToBoolEx(PAnsiChar(sqldata));
          SQL_VARYING   : Result := StrToBoolEx(PAnsiChar(sqldata)+2{Inc SQLData by sizeof(smallint)});
          SQL_BLOB:
            if sqlsubtype = 1 then
              StrToBoolEx(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString)
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
  Result := nil;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          begin
            SetLength(Result, sqllen);
            System.Move(PAnsiChar(sqldata)^, Pointer(Result)^, sqllen);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
      case (sqltype and not(1)) of
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
        SQL_TEXT, SQL_VARYING:
          begin
            P := GetPAnsiChar(ColumnIndex, Len);
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
   {$IFOPT D+}
  {$R+}
  {$ENDIF}
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
  L: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : begin
                            l := sqllen; {last char = #0}
                            if L > 0 then
                              while ((sqldata+l-1)^ = ' ') do dec(l); {last char = #0}
                            ZSysUtils.SQLStrToFloatDef(PAnsiChar(sqldata), 0, Result, L)
                          end;
          SQL_VARYING   : ZSysUtils.SQLStrToFloatDef(PAnsiChar(@PISC_VARYING(sqldata).str[0]), 0, Result, PISC_VARYING(sqldata).strlen);
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
  L: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : begin
                            l := sqllen; {last char = #0}
                            if L > 0 then
                              while ((sqldata+l-1)^ = ' ') do dec(l); {last char = #0}
                            ZSysUtils.SQLStrToFloatDef(PAnsiChar(sqldata), 0, Result, L)
                          end;
          SQL_VARYING   : ZSysUtils.SQLStrToFloatDef(PAnsiChar(@PISC_VARYING(sqldata).str[0]), 0, Result, PISC_VARYING(sqldata).strlen);
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
  L: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : begin
                            l := sqllen; {last char = #0}
                            if L > 0 then
                              while ((sqldata+l-1)^ = ' ') do dec(l); {last char = #0}
                            ZSysUtils.SQLStrToFloatDef(PAnsiChar(sqldata), 0, Result, L)
                          end;
          SQL_VARYING   : ZSysUtils.SQLStrToFloatDef(PAnsiChar(@PISC_VARYING(sqldata).str[0]), 0, Result, PISC_VARYING(sqldata).strlen);
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
  end;
    {$IFOPT D+}
  {$R+}
  {$ENDIF}
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : Result := RawToIntDef(sqldata, 0);
          SQL_VARYING   : Result := RawToIntDef(PAnsiChar(sqldata)+2{Inc SQLData by sizeof(smallint)}, 0);
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToIntDef(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else goto Fail;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : Result := RawToInt64Def(sqldata, 0);
          SQL_VARYING   : Result := RawToInt64Def(PAnsiChar(sqldata)+2{Inc SQLData by sizeof(smallint)}, 0);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFDEF WITH_UINT64_C1118_ERROR}
  Result := UInt64(0); //need that type cast for D7 else "internal error C1118"
  {$ELSE}
  Result := 0;
  {$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : Result := RawToUInt64Def(DecodeString(True, ColumnIndex), 0);
          SQL_VARYING   : Result := RawToUInt64Def(PAnsiChar(sqldata)+2{Inc SQLData by sizeof(smallint)}, 0);
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
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetShort(ColumnIndex: Integer): ShortInt;
label Fail;
var
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : Result := RawToIntDef(sqldata, 0);
          SQL_VARYING   : Result := RawToIntDef(PAnsiChar(sqldata)+2{Inc SQLData by sizeof(smallint)}, 0);
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
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
label Fail;
var
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := 0;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
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
          SQL_TEXT      : Result := RawToIntDef(sqldata, 0);
          SQL_VARYING   : Result := RawToIntDef(PAnsiChar(sqldata)+2{Inc SQLData by sizeof(smallint)}, 0);
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToIntDef(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else goto Fail;
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
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          SQL_BOOLEAN   :
            if PSmallint(sqldata)^ <> 0 then
              Result := 'YES'
            else
              Result := 'NO';
          SQL_BOOLEAN_FB:
            if PByte(sqldata)^ <> 0 then
              Result := 'YES'
            else
              Result := 'NO';
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT      : Result := DecodeString(True, ColumnIndex);
          SQL_VARYING   : Result := DecodeString(False, ColumnIndex);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}] do
      case (sqltype and not(1)) of
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
            P := GetPAnsiChar(ColumnIndex, Len);
            if (P+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed{%H-})
            else
              Result := Frac(RawSQLTimeStampToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed));
          end;
        else
          Result := Frac(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}));
        end;
   {$IFOPT D+}
  {$R+}
  {$ENDIF}
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
  TempDate: TCTimeStructure;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}] do
      case (sqltype and not(1)) of
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
            P := GetPAnsiChar(ColumnIndex, Len);
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
   {$IFOPT D+}
  {$R+}
  {$ENDIF}
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
  CheckRange(ColumnIndex);
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
          SQL_BOOLEAN   :
            if PSmallint(sqldata)^ <> 0 then
              FRawTemp := 'YES'
            else
              FRawTemp := 'NO';
          SQL_BOOLEAN_FB:
            if PByte(sqldata)^ <> 0 then
              FRawTemp := 'YES'
            else
              FRawTemp := 'NO';
          SQL_SHORT     : FRawTemp := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : FRawTemp := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT      :
            begin
              Result := sqldata;
              // Trim only trailing spaces. TrimRight also removes other characters)
              Len := sqllen;
              if Len > 0 then while (Result+Len-1)^ = ' ' do dec(Len);
              Exit;
            end;
          SQL_VARYING :
            begin
              Result := PISC_VARYING(sqldata).str;
              Len := PISC_VARYING(sqldata).strlen;
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
var Len: NativeUInt;
begin
  Result := GetPAnsiChar(ColumnIndex, Len);
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
label AssignResult;
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
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          SQL_BOOLEAN   :
            if PSmallint(sqldata)^ <> 0 then
              Result := 'YES'
            else
              Result := 'NO';
          SQL_BOOLEAN_FB:
            if PByte(sqldata)^ <> 0 then
              Result := 'YES'
            else
              Result := 'NO';
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT      :
            begin
              // Trim only trailing spaces. TrimRight also removes other characters)
              Len := sqllen;
              P := sqldata;
              if Len > 0 then
                while (P+Len-1)^ = ' ' do dec(Len);
              goto AssignResult;
            end;
          SQL_VARYING :
            begin
              P := PISC_VARYING(sqldata).str;
              Len := PISC_VARYING(sqldata).strlen;
AssignResult: if sqlsubtype > High(FCodePageArray) then
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
  Len: LengthInt;
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
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          SQL_BOOLEAN   :
            if PSmallint(sqldata)^ <> 0 then
              Result := 'YES'
            else
              Result := 'NO';
          SQL_BOOLEAN_FB:
            if PByte(sqldata)^ <> 0 then
              Result := 'YES'
            else
              Result := 'NO';
          SQL_SHORT     : Result := ZFastCode.IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := ZFastCode.IntToStr(PInt64(sqldata)^);
          SQL_TEXT      :
            begin
              P := sqldata;
              // Trim only trailing spaces. TrimRight also removes other characters)
              Len := sqllen;
              if Len > 0 then
                while ((P+Len-1)^ = ' ') do dec(Len);
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
          SQL_VARYING :
            begin
              SubType := GetIbSqlSubType(ColumnIndex);
              if SubType > High(FCodePageArray) then
                {$IFDEF UNICODE}
                Result := PRawToUnicode(PISC_VARYING(sqldata).str,
                  PISC_VARYING(sqldata).strlen, ConSettings^.ClientCodePage^.CP)
                {$ELSE}
                begin
                  ZSetString(PISC_VARYING(sqldata).str, PISC_VARYING(sqldata).strlen, FRawTemp);
                  Result := ConSettings^.ConvFuncs.ZRawToString(FRawTemp,
                    ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
                end
                {$ENDIF}
              else
                {$IFDEF UNICODE}
                Result := PRawToUnicode(PISC_VARYING(sqldata).str,
                  PISC_VARYING(sqldata).strlen, FCodePageArray[SubType]);
                {$ELSE}
                begin
                  ZSetString(PISC_VARYING(sqldata).str, PISC_VARYING(sqldata).strlen, FRawTemp);
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
begin
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (LastRowNo >= MaxRows) or (FStmtHandle = 0) then
    Exit;

  { Fetch row. }
  if (ResultSetType = rtForwardOnly) and (FFetchStat = 0) then
  begin
    if (FStmtType = stSelect) then  //AVZ - Test for ExecProc - this is for multiple rows
    begin
      FFetchStat := FPlainDriver.isc_dsql_fetch(@StatusVector,
        @FStmtHandle, FDialect, FXSQLDA);
      if FFetchStat = 0 then
      begin
        RowNo := RowNo + 1;
        LastRowNo := RowNo;
        Result := True;
      end
      else
        CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings);
    end
    else
    begin
      FFetchStat := 1;
      Result := True;
    end;
  end;
end;

{**
   Chech reange count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZInterbase6XSQLDAResultSet.CheckRange(const Index: Word);
begin
  Assert(Index {$IFNDEF GENERIC_INDEX}<{$ELSE}<={$ENDIF} Word(FXSQLDA.sqln), 'Out of Range.');
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

function TZInterbase6XSQLDAResultSet.DecodeString(const IsText: Boolean;
  const Index: Word): RawByteString;
var
   l: integer;
begin
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  if IsText then
    begin
      l := sqllen; {last char = #0}
      // Trim only spaces. TrimRight also removes other characters)
      if L > 0 then
      begin
        while ((sqldata+l-1)^ = ' ') do dec(l); {last char = #0}
        ZSetString(sqldata, l, Result{%H-});
      end
      else
        Result := '';
    end
  else
    ZSetString(PISC_VARYING(sqldata).str, PISC_VARYING(sqldata).strlen, Result);
  {$IFOPT D+}
{$R+}
{$ENDIF}
end;

{**
   Return Interbase QUAD field value
   @param Index the field index
   @return the field Interbase QUAD value
}
function TZInterbase6XSQLDAResultSet.GetQuad(const Index: Integer): TISC_QUAD;
begin
  CheckRange(Index);
  {$R-}
  with FXSQLDA.sqlvar[Index] do
  if not ((sqlind <> nil) and (sqlind^ = -1)) then
    case (sqltype and not(1)) of
      SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY: result := PISC_QUAD(sqldata)^;
    else
      raise EZIBConvertError.Create(SUnsupportedDataType + ' ' + {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr((sqltype and not(1))));
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
        ColumnType := FieldSqlType;

        if FieldSqlType in [stString, stUnicodeString] then
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
          Precision := FIZSQLDA.GetIbSqlLen(I) div ZCodePageInfo^.CharWidth;
          if ColumnType = stString then begin
            CharOctedLength := Precision * ConSettings^.ClientCodePage^.CharWidth;
            ColumnDisplaySize := Precision;
          end else begin
            CharOctedLength := Precision shl 1;
            ColumnDisplaySize := Precision;
          end;
        end
        else
          if FieldSqlType in [stAsciiStream, stUnicodeStream] then
            ColumnCodePage := ConSettings^.ClientCodePage^.CP
          else
          begin
            ColumnCodePage := zCP_NONE;
            if FieldSQLType = stBytes then
              Precision := FIZSQLDA.GetIbSqlLen(I)
            else
              Signed := FieldSqlType in [stShort, stSmall, stInteger, stLong];
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
  FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_CLOSE); //close handle but not free it
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
  FTrHandle := TrHandle;
  FPlainDriver := PlainDriver;
  FConSettings := ConSettings;
end;

procedure TZInterbase6UnCachedBlob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FDBHandle, FTrHandle, FBlobId, Size{%H-}, Buffer{%H-}, True, FConSettings);
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
  FTrHandle := TrHandle;
  FPlainDriver := PlainDriver;
end;

procedure TZInterbase6UnCachedClob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FDBHandle, FTrHandle, FBlobId, Size{%H-}, Buffer{%H-}, False, FConSettings);
  (PAnsiChar(Buffer)+NativeUInt(Size))^ := #0; //add #0 terminator
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  FBlobSize := Size+1;
  BlobData := Buffer;
  inherited ReadLob;
end;

end.
