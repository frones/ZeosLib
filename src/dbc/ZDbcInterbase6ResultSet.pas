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
    FRawTemp: RawByteString;
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

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetAnsiRec(ColumnIndex: Integer): TZAnsiRec; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
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
  TZInterbase6UnCachedBlob = Class(TZAbstractUnCachedBlob)
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

  TZInterbase6UnCachedClob = Class(TZAbstractUnCachedClob)
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
  ZDbcUtils, ZEncoding, ZFastCode, ZSysUtils, ZDbcInterbase6Statement;

{ TZInterbase6XSQLDAResultSet }

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
var stmt: IZInterbase6PreparedStatement;
begin
  if FStmtHandle <> 0 then
  begin
    { Free output allocated memory }
    FXSQLDA := nil;
    FIZSQLDA := nil;
    { Free allocate sql statement }
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_CLOSE); //close handle but not free it
  end;
  if (Statement <> nil) and Supports(Statement, IZInterbase6PreparedStatement, stmt) then
    stmt.FreeReference;
  inherited Close;
end;

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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    ColumnIndex := ColumnIndex -1;
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToFloat(DecodeString(True, ColumnIndex), '.');
          SQL_VARYING   : Result := RawToFloat(DecodeString(False, ColumnIndex), '.');
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
  Size: Integer;
  Buffer: Pointer;
  BlobId: TISC_QUAD;
begin
  Result := nil;
  CheckBlobColumn(ColumnIndex);

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
      Exit;

  BlobId := GetQuad(ColumnIndex - 1);
  if FCachedBlob then
    try
      with FIBConnection do
        ReadBlobBufer(GetPlainDriver, GetDBHandle, GetTrHandle,
          BlobId, Size, Buffer, Self.GetMetaData.GetColumnType(ColumnIndex) = stBinaryStream,
          ConSettings);
      case GetMetaData.GetColumnType(ColumnIndex) of
        stBinaryStream:
          Result := TZAbstractBlob.CreateWithData(Buffer, Size);
        stAsciiStream, stUnicodeStream:
          Result := TZAbstractClob.CreateWithData(Buffer,
            Size, ConSettings^.ClientCodePage^.CP, ConSettings);
      end;
    finally
      FreeMem(Buffer, Size);
    end
  else
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
var
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex -1] do
    begin
      SQLCode := (sqltype and not(1));

      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale] <> 0;
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale] <> 0;
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale] <> 0;
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^) > 0;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^) <> 0;
          SQL_LONG      : Result := PInteger(sqldata)^ <> 0;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^) <> 0;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^ <> 0;
          SQL_SHORT     : Result := PSmallint(sqldata)^ <> 0;
          SQL_INT64     : Result := PInt64(sqldata)^ <> 0;
          SQL_TEXT      : Result := StrToBoolEx(DecodeString(True, ColumnIndex-1));
          SQL_VARYING   : Result := StrToBoolEx(DecodeString(False, ColumnIndex -1));
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
  {$R+}
  {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetByte(ColumnIndex: Integer): Byte;
var
  SQLCode: SmallInt;
  Index: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    Index := ColumnIndex -1;
    {$R-}
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToInt(DecodeString(True, Index));
          SQL_VARYING   : Result := RawToInt(DecodeString(False, Index));
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
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
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex -1] do
    begin
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
      SQLCode := (sqltype and not(1));

        case SQLCode of
          SQL_TEXT, SQL_VARYING:
            begin
              SetLength(Result, sqllen);
              System.Move(PAnsiChar(sqldata)^, Pointer(Result)^, sqllen);
            end;
          else
            raise EZIBConvertError.Create(Format(SErrorConvertionField,
              [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
  {$R+}
  {$ENDIF}
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex -1] do
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
          else
            Result := Trunc(GetDouble(ColumnIndex -1));
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex -1] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToFloat(DecodeString(True, ColumnIndex -1), '.');
          SQL_VARYING   : Result := RawToFloat(DecodeString(False, ColumnIndex -1), '.');
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
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
function TZInterbase6XSQLDAResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex -1] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToFloat(DecodeString(True, ColumnIndex -1), '.');
          SQL_VARYING   : Result := RawToFloat(DecodeString(False, ColumnIndex -1), '.');
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFOPT D+}
  {$R+}
  {$ENDIF}
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex -1] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToFloat(DecodeString(True, ColumnIndex -1), '.');
          SQL_VARYING   : Result := RawToFloat(DecodeString(False, ColumnIndex -1), '.');
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex -1), GetNameSqlType(SQLCode)]));
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
var
  SQLCode: SmallInt;
  Index: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    Index := ColumnIndex -1;
    {$R-}
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToInt(DecodeString(True, Index));
          SQL_VARYING   : Result := RawToInt(DecodeString(False, Index));
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
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
var
  SQLCode: SmallInt;
  Index: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    Index := ColumnIndex -1;
    {$R-}
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToInt(DecodeString(True, Index));
          SQL_VARYING   : Result := RawToInt(DecodeString(False, Index));
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
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
var
  SQLCode: SmallInt;
  Index: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    Index := ColumnIndex -1;
    {$R-}
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToInt(DecodeString(True, Index));
          SQL_VARYING   : Result := RawToInt(DecodeString(False, Index));
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
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
var
  SQLCode: SmallInt;
  Index: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    Index := ColumnIndex -1;
    {$R-}
    with FXSQLDA.sqlvar[Index] do
    begin
      Result := 0;
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT      : Result := RawToInt(DecodeString(True, Index));
          SQL_VARYING   : Result := RawToInt(DecodeString(False, Index));
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(Index), GetNameSqlType(SQLCode)]));
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
    ColumnIndex := ColumnIndex -1;
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
            if Boolean(PSmallint(sqldata)^) = True then
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex -1] do
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
        else
          Result := Frac(GetDouble(ColumnIndex -1));
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
    with FXSQLDA.sqlvar[ColumnIndex -1] do
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
        else
          Result := GetDouble(ColumnIndex -1);
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

  ColumnIndex := ColumnIndex -1;
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
function TZInterbase6XSQLDAResultSet.GetAnsiRec(ColumnIndex: Integer): TZAnsiRec;
var
  SQLCode: SmallInt;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
  begin
    Result.P := nil;
    Result.Len := 0;
  end
  else
  begin
    ColumnIndex := ColumnIndex -1;
    FRawTemp := '';
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      if (sqlind <> nil) and (sqlind^ = -1) then
           Exit;
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
            if Boolean(PSmallint(sqldata)^) = True then
              FRawTemp := 'YES'
            else
              FRawTemp := 'NO';
          SQL_SHORT     : FRawTemp := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : FRawTemp := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT      :
            begin
              Result.P := sqldata;
              // Trim only trailing spaces. TrimRight also removes other characters)
              Result.Len := sqllen;
              if Result.Len > 0 then
                while ((Result.P+Result.Len-1)^ = ' ') do dec(Result.Len);
              Exit;
            end;
          SQL_VARYING :
            begin
              Result.P := PISC_VARYING(sqldata).str;
              Result.Len := PISC_VARYING(sqldata).strlen;
              Exit;
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex);  //localize interface to keep pointer alive
              if FBlobTemp.IsClob then
              begin
                Result.P := FBlobTemp.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                Result.Len := FBlobTemp.Length;
              end
              else
              begin
                Result.P := FBlobTemp.GetBuffer;
                Result.Len := FBlobTemp.Length;
              End;
              Exit;
            End;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    Result.P := PAnsiChar(FRawTemp);
    Result.Len := Length(FRawTemp);
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
begin
  Result := GetAnsiRec(ColumnIndex).P;
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
  AnsiRec: TZAnsiRec;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    ColumnIndex := ColumnIndex -1;
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
            if Boolean(PSmallint(sqldata)^) = True then
              Result := 'YES'
            else
              Result := 'NO';
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT      :
            begin
              // Trim only trailing spaces. TrimRight also removes other characters)
              AnsiRec.Len := sqllen;
              AnsiRec.P := sqldata;
              if AnsiRec.Len > 0 then
                while (AnsiRec.P+AnsiRec.Len-1)^ = ' ' do dec(AnsiRec.Len);
              if sqlsubtype > High(FCodePageArray) then
                ZConvertAnsiRecToUTF8(AnsiRec, ConSettings^.ClientCodePage^.cp)
              else
                if (FCodePageArray[sqlsubtype] = zCP_UTF8) then
                  ZMoveAnsiRecToUTF8(AnsiRec, zCP_UTF8)
                else
                  ZConvertAnsiRecToUTF8(AnsiRec, sqlsubtype);
              Exit;
            end;
          SQL_VARYING :
            begin
              AnsiRec.P := PISC_VARYING(sqldata).str;
              AnsiRec.Len := PISC_VARYING(sqldata).strlen;
              if sqlsubtype > High(FCodePageArray) then
                ZConvertAnsiRecToUTF8(AnsiRec, ConSettings^.ClientCodePage^.cp)
              else
                if (FCodePageArray[sqlsubtype] = zCP_UTF8) then
                  ZMoveAnsiRecToUTF8(AnsiRec, zCP_UTF8)
                else
                  ZConvertAnsiRecToUTF8(AnsiRec, sqlsubtype);
              Exit;
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex);  //localize interface to keep pointer alive
              if FBlobTemp.IsClob then
                Result := FBlobTemp.GetUTF8String
              else
                Result := FBlobTemp.GetString;
              Exit;
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    SubType := GetIbSqlSubType(ColumnIndex -1);
    if SubType > High(FCodePageArray) then
      {$IFDEF UNICODE}
      Result := ZAnsiRecToUnicode(GetAnsiRec(ColumnIndex),
        ConSettings^.ClientCodePage^.CP)
      {$ELSE}
      Result := ConSettings^.ConvFuncs.ZRawToString(InternalGetString(ColumnIndex),
        ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)
      {$ENDIF}
    else
      {$IFDEF UNICODE}
      Result := ZAnsiRecToUnicode(GetAnsiRec(ColumnIndex),
        FCodePageArray[SubType]);
      {$ELSE}
      Result := ConSettings^.ConvFuncs.ZRawToString(InternalGetString(ColumnIndex),
        FCodePageArray[SubType], ConSettings^.CTRL_CP);
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
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    SubType := GetIbSqlSubType(ColumnIndex -1);
    if SubType > High(FCodePageArray) then
      Result := ZAnsiRecToUnicode(GetAnsiRec(ColumnIndex),
        ConSettings^.ClientCodePage^.CP)
    else
      Result := ZAnsiRecToUnicode(GetAnsiRec(ColumnIndex),
        FCodePageArray[SubType]);
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
  if (MaxRows > 0) and (LastRowNo >= MaxRows) then
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
  Assert(Index < Word(FXSQLDA.sqln), 'Out of Range.');
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
        ZSetString(sqldata, l, Result);
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
  CP: Word;
begin
  if FStmtHandle=0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
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
        if CP > High(FCodePageArray) then //spezial case for collations like PXW_INTL850 which are nowhere to find in docs
          //see test Bug#886194, we retrieve 565 as CP...
          ColumnCodePage := ConSettings^.ClientCodePage^.CP
        else
          ColumnCodePage := FCodePageArray[CP];
      end
      else
        if FieldSqlType in [stAsciiStream, stUnicodeStream] then
          ColumnCodePage := ConSettings^.ClientCodePage^.CP
        else
          ColumnCodePage := zCP_NONE;

      if FieldSqlType in [stBytes, stString, stUnicodeString] then
      begin
        MaxLenghtBytes := FIZSQLDA.GetIbSqlLen(I);
        if (FIZSQLDA.GetIbSqlType(I) = SQL_TEXT) or ( FieldSQLType = stBytes ) then
        begin
          if not ( FieldSQLType = stBytes ) then
            if ConSettings.ClientCodePage^.ID = CS_NONE then
          else
            ColumnDisplaySize := MaxLenghtBytes div ConSettings.ClientCodePage^.CharWidth;
          Precision := MaxLenghtBytes;
        end
        else
          Precision := GetFieldSize(ColumnType, ConSettings, MaxLenghtBytes,
            ConSettings^.ClientCodePage^.CharWidth, @ColumnDisplaySize, True);
      end;

      ReadOnly := (FIZSQLDA.GetFieldRelationName(I) = '') or (FIZSQLDA.GetFieldSqlName(I) = '')
        or (FIZSQLDA.GetFieldSqlName(I) = 'RDB$DB_KEY') or (FieldSqlType = ZDbcIntfs.stUnknown);

      if FIZSQLDA.IsNullable(I) then
        Nullable := ntNullable
      else
        Nullable := ntNoNulls;

      Scale := FIZSQLDA.GetFieldScale(I);
      AutoIncrement := False;
      //Signed := False;
      //CaseSensitive := True;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;
  inherited Open;
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

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
procedure TZInterbase6UnCachedBlob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FDBHandle, FTrHandle, FBlobId, Size, Buffer, True, FConSettings);
  BlobSize := Size;
  BlobData := Buffer;
  inherited ReadLob;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

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

{$IFDEF FPC}
  {$HINTS OFF}
{$ENDIF}
procedure TZInterbase6UnCachedClob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FDBHandle, FTrHandle, FBlobId, Size, Buffer, False, FConSettings);
  (PAnsiChar(Buffer)+NativeUInt(Size))^ := #0; //add #0 terminator
  FBlobSize := Size+1;
  BlobData := Buffer;
  inherited ReadLob;
end;
{$IFDEF FPC}
  {$HINTS ON}
{$ENDIF}

end.
