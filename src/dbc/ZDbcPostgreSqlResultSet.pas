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
  {$IFDEF WITH_TOBJECTLIST_INLINE}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZPlainPostgreSqlDriver, ZDbcLogging,
  ZDbcResultSetMetadata, ZCompatibility;

type
  {** Implements PostgreSQL ResultSet. }
  TZPostgreSQLResultSet = class(TZAbstractResultSet)
  private
    FHandle: PZPostgreSQLConnect;
    FQueryHandle: PZPostgreSQLResult;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
    FIs_bytea_output_hex: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    FCachedLob: boolean;
    FFixedCharFields: TBooleanDynArray;
    FBinaryFields: TBooleanDynArray;
    function GetBuffer(ColumnIndex: Integer; var Len: NativeUInt): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
  protected
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
    procedure Open; override;
    procedure DefinePostgreSQLToSQLType(ColumnInfo: TZColumnInfo; const TypeOid: Oid);
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Statement: IZStatement; const SQL: string; Handle: PZPostgreSQLConnect;
      QueryHandle: PZPostgreSQLResult; const CachedLob: Boolean;
      const Chunk_Size, UndefinedVarcharAsStringLength: Integer);

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
  end;

  {** Represents an interface, specific for PostgreSQL blobs. }
  IZPostgreSQLOidBlob = interface(IZBlob)
    ['{BDFB6B80-477D-4CB1-9508-9541FEA6CD72}']
    function GetBlobOid: Oid;
    procedure WriteLob;
    procedure WriteBuffer(const Buffer: Pointer; const Len: integer);
  end;

  {** Implements external blob wrapper object for PostgreSQL. }
  TZPostgreSQLOidBlob = class(TZAbstractUnCachedBlob, IZPostgreSQLOidBlob)
  private
    FHandle: PZPostgreSQLConnect;
    FBlobOid: Oid;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FChunk_Size: Integer;
  public
    constructor Create(const PlainDriver: IZPostgreSQLPlainDriver; const
      Data: Pointer; const Size: Integer; const Handle: PZPostgreSQLConnect;
      const BlobOid: Oid; const Chunk_Size: Integer);

    function GetBlobOid: Oid;
    procedure ReadLob; override;
    procedure WriteLob; override;
    procedure WriteBuffer(const Buffer: Pointer; const Len: integer);

    function Clone(Empty: Boolean = False): IZBlob; override;
  end;

implementation

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} Math,
  ZMessages, ZDbcUtils, ZEncoding, ZFastCode,
  ZDbcPostgreSql, ZDbcPostgreSqlUtils, ZDbcPostgreSqlStatement;

{ TZPostgreSQLResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a PostgreSQL plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a PostgreSQL specific query handle.
}
constructor TZPostgreSQLResultSet.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Statement: IZStatement; const SQL: string; Handle: PZPostgreSQLConnect;
  QueryHandle: PZPostgreSQLResult; const CachedLob: Boolean;
  const Chunk_Size, UndefinedVarcharAsStringLength: Integer);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);

  FHandle := Handle;
  FQueryHandle := QueryHandle;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FChunk_Size := Chunk_Size; //size of read/write lob in chunks
  FUndefinedVarcharAsStringLength := UndefinedVarcharAsStringLength;
  FIs_bytea_output_hex := (Statement.GetConnection as IZPostgreSQLConnection).Is_bytea_output_hex;
  FCachedLob := CachedLob;

  Open;
end;

{**
  Converts a PostgreSQL native types into ZDBC SQL types.
  @param ColumnIndex a column index.
  @param ColumnInfo a column description object.
  @param TypeOid a type oid.
  @return a SQL undepended type.
}
procedure TZPostgreSQLResultSet.DefinePostgreSQLToSQLType(
  ColumnInfo: TZColumnInfo; const TypeOid: Oid);
var
  SQLType: TZSQLType;
  Connection: IZPostgreSQLConnection;
begin
  Connection := Statement.GetConnection as IZPostgreSQLConnection;

  case TypeOid of
    790: ColumnInfo.Currency := True; { money }
    19: if (Connection.GetServerMajorVersion < 7) or
           ((Connection.GetServerMajorVersion = 7) and (Connection.GetServerMinorVersion < 3)) then
          ColumnInfo.Precision := 32
        else
          ColumnInfo.Precision := 64; { name }
    650: ColumnInfo.Precision := 100; { cidr }
    869: ColumnInfo.Precision := 100; { inet }
    829: ColumnInfo.Precision := 17; { macaddr }
    1186: ColumnInfo.Precision := 32; { interval }
    24: ColumnInfo.Precision := 64; { regproc } // M.A. was 10
    17:{ bytea }
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
procedure TZPostgreSQLResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  FieldMode, FieldSize, FieldType, FieldCount: Integer;
  TableInfo: PZPGTableInfo;
  Connection: IZPostgreSQLConnection;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  if not Assigned(FQueryHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  Connection := Statement.GetConnection as IZPostgreSQLConnection;

  LastRowNo := FPlainDriver.GetRowCount(FQueryHandle);

  { Fills the column info. }
  ColumnsInfo.Clear;
  FieldCount := FPlainDriver.GetFieldCount(FQueryHandle);
  SetLength(FFixedCharFields, FieldCount);
  SetLength(FBinaryFields, FieldCount);
  for I := 0 to FieldCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do
    begin
      if Statement.GetResultSetConcurrency = rcUpdatable then //exclude system-tables and if no updates happen -> useless
        TableInfo := Connection.GetTableInfo(FPlainDriver.GetFieldTableOID(FQueryHandle, I), FieldCount)
      else
        TableInfo := nil;
      if TableInfo = nil then
      begin
        SchemaName := '';
        ColumnName := '';
        TableName := '';
      end
      else
      begin
        SchemaName := TableInfo^.Schema;
        TableName := TableInfo^.Name;
        ColumnName := TableInfo^.ColNames[FplainDriver.GetFieldTableColIdx(FQueryHandle, I) - 1];
      end;
      ColumnLabel := ConSettings^.ConvFuncs.ZRawToString(FPlainDriver.GetFieldName(FQueryHandle, I), ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
      ColumnDisplaySize := 0;
      Scale := 0;
      Precision := 0;

      AutoIncrement := False;
      Signed := False;
      Nullable := ntNullable;

      FieldType := FPlainDriver.GetFieldType(FQueryHandle, I);
      FFixedCharFields[i] := FieldType = 1042;
      DefinePostgreSQLToSQLType(ColumnInfo, FieldType);
      FBinaryFields[i] := ColumnInfo.ColumnType in [stBytes, stBinaryStream];
      if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
        ColumnCodePage := ConSettings^.ClientCodePage^.CP
      else
        ColumnCodePage := High(Word);

      if Precision = 0 then
      begin
        FieldMode := FPlainDriver.GetFieldMode(FQueryHandle, I);
        FieldSize := FPlainDriver.GetFieldSize(FQueryHandle, I);
        Precision := Max(Max(FieldMode - 4, FieldSize), 0);

        if ColumnType in [stString, stUnicodeString] then
          {begin patch: varchar() is equal to text!}
          if ( FieldMode = -1 ) and ( FieldSize = -1 ) and ( FieldType = 1043) then
            if FUndefinedVarcharAsStringLength > 0 then
              Precision := GetFieldSize(ColumnType, ConSettings,
                FUndefinedVarcharAsStringLength,
                ConSettings.ClientCodePage^.CharWidth, nil, False)
            else
              DefinePostgreSQLToSQLType(ColumnInfo, 25) //assume text instead!
          else
            if ( (ColumnLabel = 'expr') or ( Precision = 0 ) ) then
              Precision := GetFieldSize(ColumnType, ConSettings, 255,
                ConSettings.ClientCodePage^.CharWidth, nil, True)
            else
              Precision := GetFieldSize(ColumnType, ConSettings, Precision,
                ConSettings.ClientCodePage^.CharWidth, @ColumnDisplaySize);
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
procedure TZPostgreSQLResultSet.ResetCursor;
begin
  if FQueryHandle <> nil then
  begin
    FPlainDriver.Clear(FQueryHandle);
    FQueryHandle := nil;
  end;
  inherited ResetCursor;
end;
{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZPostgreSQLResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  Result := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1,
    ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}) <> 0;
end;

function TZPostgreSQLResultSet.GetBuffer(ColumnIndex: Integer; var Len: NativeUint): PAnsiChar;
var RNo: Integer;
begin
  RNo := RowNo - 1;
  LastWasNull := FPlainDriver.GetIsNull(FQueryHandle, RNo, ColumnIndex) <> 0;

  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
  begin
    Result := FPlainDriver.GetValue(FQueryHandle, RNo, ColumnIndex);
    if FBinaryFields[ColumnIndex] then
      Len := FPlainDriver.GetLength(FQueryHandle, RNo, ColumnIndex)
    else
    begin
      {http://www.postgresql.org/docs/9.0/static/libpq-exec.html
      PQgetlength:
       This is the actual data length for the particular data value, that is,
       the size of the object pointed to by PQgetvalue.
       For text data format this is the same as strlen().
       For binary format this is essential information.
       Note that one should not rely on PQfsize to obtain the actual data length.}
      Len := ZFastCode.StrLen(Result);
      if FFixedCharFields[ColumnIndex] and (Len > 0) then
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
function TZPostgreSQLResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
  Result := GetBuffer(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Len{%H-});
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZPostgreSQLResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>''</code>, the
    value returned is <code>null</code>
}
function TZPostgreSQLResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
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
    if (ConSettings^.ClientCodePage.CP = zCP_UTF8) or FBinaryFields[ColumnIndex] then
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
function TZPostgreSQLResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
  Buffer := GetBuffer(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Len{%H-});
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
function TZPostgreSQLResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := False
  else
    Result := StrToBoolEx(FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex), True, FFixedCharFields[ColumnIndex]);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    if FFixedCharFields[ColumnIndex] then
      Result := RawToIntDef(InternalGetString(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), 0)
    else
      Result := RawToIntDef(FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    if FFixedCharFields[ColumnIndex] then
      Result := RawToInt64Def(InternalGetString(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), 0)
    else
      Result := RawToInt64Def(FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else
    if FFixedCharFields[ColumnIndex] then
      Result := RawToUInt64Def(InternalGetString(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), 0)
    else
      Result := RawToUInt64Def(FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex), 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Buffer := GetBuffer(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    Result := ZSysUtils.SQLStrToFloatDef(Buffer, 0, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZPostgreSQLResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Buffer := GetBuffer(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    Result := ZSysUtils.SQLStrToFloatDef(Buffer, 0, Len);
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
function TZPostgreSQLResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Buffer := GetBuffer(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    Result := ZSysUtils.SQLStrToFloatDef(Buffer, 0, Len);
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
function TZPostgreSQLResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := StrToBytes(DecodeString(InternalGetString(ColumnIndex)));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZPostgreSQLResultSet.GetDate(ColumnIndex: Integer): TDateTime;
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
  Buffer := GetBuffer(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
  begin
    if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
      Result := RawSQLDateToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
    else
      Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
        RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
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
function TZPostgreSQLResultSet.GetTime(ColumnIndex: Integer): TDateTime;
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
  Buffer := GetBuffer(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    if not (Len > ConSettings^.ReadFormatSettings.TimeFormatLen) and ( ( ConSettings^.ReadFormatSettings.TimeFormatLen - Len) <= 4 )then
      Result := RawSQLTimeToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
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
function TZPostgreSQLResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Buffer := GetBuffer(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    Result := RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed{%H-});
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZPostgreSQLResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  BlobOid: Oid;
  Connection: IZConnection;
  Buffer: PAnsiChar;
  Len: NativeUInt;
  SQLType: TZSQLType;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  SQLType := GetMetadata.GetColumnType(ColumnIndex);
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := FPlainDriver.GetIsNull(FQueryHandle, RowNo - 1, ColumnIndex) <> 0;

  Connection := Statement.GetConnection;
  if (SQLType = stBinaryStream)
    and (Connection as IZPostgreSQLConnection).IsOidAsBlob then
  begin
    if LastWasNull then
      BlobOid := 0
    else
      BlobOid := RawToIntDef(FPlainDriver.GetValue(FQueryHandle, RowNo - 1, ColumnIndex), 0);
    Result := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FHandle, BlobOid, FChunk_Size);
  end
  else
    if not LastWasNull then
      case SQLType of
        stBinaryStream:
          begin
            Len := FPlainDriver.DecodeBYTEA(RowNo-1, ColumnIndex,
              FIs_bytea_output_hex, FHandle, FQueryHandle, Pointer({%H-}Buffer));
            Result := TZAbstractBlob.CreateWithData(Buffer, Len);
            FreeMem(Buffer, Len);
          end;
        stAsciiStream, stUnicodeStream:
          begin
            Buffer := GetBuffer(ColumnIndex, Len);
            Result := TZAbstractCLob.CreateWithData(Buffer, Len, ConSettings^.ClientCodePage^.CP, ConSettings);
          end;
        else
          Result := TZAbstractBlob.CreateWithStream(nil);
      end
    else
      Result := TZAbstractBlob.CreateWithStream(nil);
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
function TZPostgreSQLResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  if (FQueryHandle = nil) and (not Closed) then
  begin
    FQueryHandle := (Statement as IZPGSQLPreparedStatement).GetLastQueryHandle;
    LastRowNo := FPlainDriver.GetRowCount(FQueryHandle);
  end;
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
    Exit;

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
constructor TZPostgreSQLOidBlob.Create(const PlainDriver: IZPostgreSQLPlainDriver;
  const Data: Pointer; const Size: Integer; const Handle: PZPostgreSQLConnect;
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
    BlobHandle := FPlainDriver.OpenLargeObject(FHandle, FBlobOid, INV_READ);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Read Large Object',nil);
    if BlobHandle >= 0 then
    begin
      Buffer := AllocMem(FChunk_Size+1);
      OffSet := 0;
      repeat
        ReadNum := FPlainDriver.ReadLargeObject(FHandle, BlobHandle,
          Buffer, FChunk_Size);
        Inc(OffSet, ReadNum);
        ReallocMem(FBlobData, OffSet);
        if ReadNum > 0 then
          System.Move(Buffer^, {%H-}Pointer({%H-}NativeUInt(FBlobData)+NativeUInt(OffSet-ReadNum))^, ReadNum);
      until ReadNum < FChunk_Size;
      BlobSize := OffSet;
      FPlainDriver.CloseLargeObject(FHandle, BlobHandle);
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
  if FBlobOid = 0 then
  begin
    FBlobOid := FPlainDriver.CreateLargeObject(FHandle, INV_WRITE);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Create Large Object',nil);
  end;

  { Opens and writes a large object. }
  BlobHandle := FPlainDriver.OpenLargeObject(FHandle, FBlobOid, INV_WRITE);
  CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Open Large Object',nil);

  Position := 0;
  while Position < Len do
  begin
    if (Len - Position) < FChunk_Size then
      Size := Len - Position
    else
      Size := FChunk_Size;
    FPlainDriver.WriteLargeObject(FHandle, BlobHandle,
      {%H-}Pointer({%H-}NativeUInt(Buffer) + NativeUInt(Position)), Size);
    CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Write Large Object',nil);
    Inc(Position, Size);
  end;

  FPlainDriver.CloseLargeObject(FHandle, BlobHandle);
  CheckPostgreSQLError(nil, FPlainDriver, FHandle, lcOther, 'Close Large Object',nil);
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

{ TZPostgreSQLUnCachedCLob }
(*procedure TZPostgreSQLUnCachedBLob.ReadLob;
var
  Buffer: Pointer;
begin
  Clear;
  Updated := False;
  BlobSize := FPlainDriver.DecodeBytea(FRowNo, FColumnIndex,
    FIs_bytea_output_hex, FHandle, FQueryHandle, Buffer{%H-});
  BlobData := Buffer;
  inherited ReadLob; //don't forget this!! or lob will read again.. eg. Transaction?
end;

constructor TZPostgreSQLUnCachedBLob.Create(PlainDriver: IZPostgreSQLPlainDriver;
  const QueryHandle: PZPostgreSQLResult; const Handle: PZPostgreSQLConnect;
  Const RowNo, ColumnIndex: Integer; const Is_bytea_output_hex: Boolean);
begin
  FPlainDriver := PlainDriver;
  FQueryHandle := QueryHandle;
  FRowNo := RowNo;
  FColumnIndex := ColumnIndex;
  FHandle := Handle;
  FIs_bytea_output_hex := Is_bytea_output_hex;
end;

{ TZPostgreSQLUnCachedCLob }
procedure TZPostgreSQLUnCachedCLob.ReadLob;
var
  Len: Cardinal;
  Buffer: PAnsichar;
begin
  Len := FPlainDriver.GetLength(FQueryHandle, FRowNo, FColumnIndex);
  Buffer := FPlainDriver.GetValue(FQueryHandle, FRowNo, FColumnIndex);
  if (Len > 0) and (FPlainDriver.GetFieldType(FQueryHandle, FColumnIndex) = 1042) then
    while (Buffer+Len)^ = ' ' do dec(Len); //remove Trailing spaces for fixed character fields
  InternalSetPAnsiChar(Buffer, FConSettings^.ClientCodePage^.CP, Len);
  inherited ReadLob; //don't forget this!! or lob will read again.. eg. Transaction?
end;

constructor TZPostgreSQLUnCachedCLob.Create(PlainDriver: IZPostgreSQLPlainDriver;
  const ConSettings: PZConSettings; const QueryHandle: PZPostgreSQLResult;
  Const RowNo, ColumnIndex: Integer);
begin
  FPlainDriver := PlainDriver;
  FQueryHandle := QueryHandle;
  FRowNo := RowNo;
  FColumnIndex := ColumnIndex;
  FConSettings := ConSettings;
end;*)

end.
