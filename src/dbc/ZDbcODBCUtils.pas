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

unit ZDbcODBCUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit
uses SysUtils,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZCompatibility, ZDbcIntfs, ZPlainODBCDriver, ZFastCode, ZDbcStatement;

type
  PStrLen_or_IndArray = ^TStrLen_or_IndArray;
  TStrLen_or_IndArray = array[0..600] of SQLLEN;

  PZODBCBindValue = ^TZODBCBindValue;
  TZODBCBindValue = record
    /// <summary> the generic bindvalue record<summary>
    BindValue: TZBindValue;
    InputOutputType: SQLSMALLINT; //the InputOutputType of the Parameter
    ValueType: SQLSMALLINT; //the C-DataType
    ParameterType: SQLSMALLINT; //the SQL-DataType
    ColumnSize: SQLULEN;
    DecimalDigits: SQLSMALLINT;
    ParameterValuePtr: SQLPOINTER;
    StrLen_or_IndPtr: PSQLLEN;
    Nullable: SQLSMALLINT;
    BufferLength: SQLLEN;
    ValueCount: Integer;
    Described, ExternalMem: Boolean;
    ParamName: String;
  end;

function ConvertODBCTypeToSQLType(ODBCType, Scale: SQLSMALLINT; Precision: Integer; UnSigned: Boolean;
  ConSettings: PZConSettings; ODBC_CType: PSQLSMALLINT): TZSQLType;

function ConvertODBC_CTypeToODBCType(ODBC_CType: SQLSMALLINT; out Signed: Boolean): SQLSMALLINT;

function ConvertSQLTypeToODBCType(SQLType: TZSQLType; var ODBC_CType: SQLSMALLINT; CharEncoding: TZCharEncoding): SQLSMALLINT;

function CalcBufSize(ColumnSize: Integer; ODBC_CType: SQLSMALLINT; SQLType: TZSQLType;
  ClientCodePage: PZCodePage): SQLSMALLINT;

{ macros of sqlext.h }
function SQL_LEN_DATA_AT_EXEC(Len: SQLLEN): SQLLEN; {$IFDEF WITH_INLINE}inline; {$ENDIF}

function ODBCNumeric2Curr(Src: PSQL_NUMERIC_STRUCT): Currency;
procedure Curr2ODBCNumeric(const Src: Currency; Dest: PSQL_NUMERIC_STRUCT); {$IFDEF WITH_INLINE}inline; {$ENDIF}

function GetConnectionString(WindowHandle: SQLHWND; const InConnectionString, LibraryLocation: String): String;

{$IFDEF ENDIAN_BIG}
procedure Reverse8Bytes(P: Pointer);
{$ENDIF}

const
  LobArrayIndexOffSet = NativeUInt(SizeOf(Pointer));
  LobParameterIndexOffSet = LobArrayIndexOffSet+NativeUInt(SizeOf(Integer));
  SQL_SS_TIME2ScaleFactor: array[0..7] of word = (1,1,1,10,10,10,100,10);
  SQL_SS_TIME2ScaleDevisor: array[0..7] of word = (10,10,10,1,1,1,1,1);

  ODBCInputOutputType: array[Boolean, TZProcedureColumnType] of SQLSMALLINT = (
    (SQL_PARAM_INPUT{pctUnknown}, SQL_PARAM_INPUT{pctIn}, SQL_PARAM_INPUT_OUTPUT{pctInOut},
     SQL_PARAM_OUTPUT{pctOut}, SQL_PARAM_OUTPUT{SQL_RETURN_VALUE does not work with mssql}{pctReturn},
     SQL_PARAM_TYPE_UNKNOWN{pctResultSet}),

    (SQL_PARAM_INPUT{pctUnknown}, SQL_PARAM_INPUT{pctIn}, SQL_PARAM_INPUT_OUTPUT_STREAM{pctInOut},
     SQL_PARAM_OUTPUT_STREAM{pctOut}, SQL_PARAM_OUTPUT{SQL_RETURN_VALUE does not work with mssql}{pctReturn},
     SQL_PARAM_TYPE_UNKNOWN{pctResultSet}));


{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit

uses ZEncoding, ZSysUtils, ZMessages, ZDbcLogging, ZDbcUtils
 {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

function SQL_LEN_DATA_AT_EXEC(Len: SQLLEN): SQLLEN;
begin
  Result := (-(Len)+SQL_LEN_DATA_AT_EXEC_OFFSET)
end;

const ODBCScaleDivisor: array[0..18] of Int64 = (
  1,
  10,
  100,
  1000,
  10000,
  100000,
  1000000,
  10000000,
  100000000,
  1000000000,
  10000000000,
  100000000000,
  1000000000000,
  10000000000000,
  100000000000000,
  1000000000000000,
  10000000000000000,
  100000000000000000,
  1000000000000000000
  );

{$IFDEF ENDIAN_BIG}
procedure Reverse8Bytes(P: Pointer);
{$IFNDEF CPU64}
var C1, C2: Cardinal;
{$ELSE}
var u64: UInt64;
{$ENDIF}
begin
  {$R-}
  {$IFNDEF CPU64}
  C1 := ((PCardinal(          P   )^ shl 8) and $FF00FF00) or ((PCardinal(          P   )^ shr 8) and $00FF00FF);
  C1 :=  (C1 shl 16) or (C1 shr 16);
  C2 := ((PCardinal(PAnsiChar(P)+4)^ shl 8) and $FF00FF00) or ((PCardinal(PAnsiChar(P)+4)^ shr 8) and $00FF00FF);
  C2 :=  (C2 shl 16) or (C2 shr 16);
  PCardinal(          P   )^ := C2;
  PCardinal(PAnsiChar(P)+4)^ := C1;
  {$ELSE}
  u64 := ((PUInt64(p)^ shl 8 ) and $FF00FF00FF00FF00) or ((PUInt64(p)^ shr 8 ) and $00FF00FF00FF00FF);
  u64 := ((u64 shl 16) and $FFFF0000FFFF0000) or ((u64 shr 16) and $0000FFFF0000FFFF);
  u64 :=  (u64 shl 32) or ((u64 shr 32));
  PUInt64(p)^ := u64;
  {$ENDIF}
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;
{$ENDIF}

function ODBCNumeric2Curr(Src: PSQL_NUMERIC_STRUCT): Currency;
var i64: Int64 absolute Result;
begin
  i64 := PInt64(@Src.val[0])^;
  {$IFDEF ENDIAN_BIG}
  Reverse8Bytes(@i64);
  {$ENDIF}
  if i64 = 0 then
    Exit;
  if Src.Scale < 4 then
    i64 := i64 * ODBCScaleDivisor[4 - Src.scale]
  else if Src.Scale > 4 then
    i64 := i64 div ODBCScaleDivisor[Src.scale - 4];
  if Src.Sign = 0 then
    Result := -Result;
end;

procedure Curr2ODBCNumeric(const Src: Currency; Dest: PSQL_NUMERIC_STRUCT);
var i64: Int64 absolute Src;
begin
  Dest.precision := 19;
  Dest.scale := 4;
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
  if Src < 0 then begin
    Dest.sign := 0;
    PUInt64(@Dest.val[0])^ := -i64;
  end else begin
    Dest.sign := 1;
    PUInt64(@Dest.val[0])^ := i64;
  end;
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

  {$IFDEF ENDIAN_BIG}
  Reverse8Bytes(@Dest.val[0]);
  {$ENDIF}
  PInt64(@Dest.val[SizeOf(Currency)])^ := 0;
end;

const ODBC_Str_C_Type: Array[Boolean] of SQLSMALLINT = (SQL_C_CHAR, SQL_C_WCHAR);
function ConvertODBCTypeToSQLType(ODBCType, Scale: SQLSMALLINT; Precision: Integer; UnSigned: Boolean;
  ConSettings: PZConSettings; ODBC_CType: PSQLSMALLINT): TZSQLType;
label Dbl, Sngl;
var ODBCCType: SQLSMALLINT;
begin
  case ODBCType of
    SQL_NUMERIC,
    SQL_DECIMAL:      begin
                        if (Scale <= 4) and (Precision <= sAlignCurrencyScale2Precision[Scale])
                        then Result := stCurrency
                        else Result := stBigDecimal;
                        ODBCCType := SQL_C_NUMERIC;
                      end;
    SQL_INTEGER:      if UnSigned then begin
                        Result := stLongWord;
                        ODBCCType := SQL_C_ULONG;
                      end else begin
                        Result := stInteger;
                        ODBCCType := SQL_C_SLONG;
                      end;
    SQL_SMALLINT:     if UnSigned then begin
                        Result := stWord;
                        ODBCCType := SQL_C_USHORT;
                      end else begin
                        Result := stSmall;
                        ODBCCType := SQL_C_SSHORT;
                      end;
    SQL_REAL:         begin
Sngl:                   Result := stFloat;
                        ODBCCType := SQL_C_FLOAT;
                      end;
    SQL_FLOAT:        if Precision <= 24
                      then goto sngl
                      else goto dbl;

    SQL_DOUBLE:       begin
Dbl:                    Result := stDouble;
                        ODBCCType := SQL_C_DOUBLE;
                      end;
    SQL_CHAR,
    SQL_VARCHAR:      begin
                        if Precision = 0
                        then Result := stAsciiStream
                        else Result := stString;
                        ODBCCType := ODBC_Str_C_Type[ConSettings.ClientCodePage^.Encoding = ceUTF16];
                      end;

    SQL_WCHAR,
    SQL_WVARCHAR:     begin
                        if Precision = 0
                        then Result := stUnicodeStream
                        else Result := stUnicodeString;
                        ODBCCType := ODBC_Str_C_Type[ConSettings.ClientCodePage^.Encoding = ceUTF16];
                      end;
    SQL_LONGVARCHAR:  begin
                        Result := stAsciiStream;
                        ODBCCType := ODBC_Str_C_Type[ConSettings.ClientCodePage^.Encoding = ceUTF16];
                      end;
    SQL_WLONGVARCHAR: begin
                        Result := stUnicodeStream;
                        ODBCCType := ODBC_Str_C_Type[ConSettings.ClientCodePage^.Encoding = ceUTF16];
                      end;
    SQL_SS_TIMESTAMPOFFSET: begin
                        Result := stTimeStamp;
                        ODBCCType := SQL_C_SS_TIMESTAMPOFFSET;
                      end;
    SQL_DATETIME,
    SQL_INTERVAL,
    SQL_TYPE_TIMESTAMP,
    SQL_TIMESTAMP:    begin
                        Result := stTimeStamp;
                        ODBCCType := SQL_C_TYPE_TIMESTAMP;
                      end;
    SQL_BINARY,
    SQL_VARBINARY:    begin
                        if Precision = 0
                        then Result := stBinaryStream
                        else Result := stBytes;
                        ODBCCType := SQL_C_BINARY;
                      end;
    SQL_LONGVARBINARY:begin
                        Result := stBinaryStream;
                        ODBCCType := SQL_C_BINARY;
                      end;
    SQL_BIGINT:       if UnSigned then begin
                        Result := stULong;
                        ODBCCType := SQL_C_UBIGINT;
                      end else begin
                        Result := stLong;
                        ODBCCType := SQL_C_SBIGINT;
                      end;
    SQL_TINYINT:      if UnSigned then begin
                        Result := stByte;
                        ODBCCType := SQL_C_UTINYINT;
                      end else begin
                        Result := stShort;
                        ODBCCType := SQL_C_STINYINT;
                      end;
    SQL_BIT:          begin
                        Result := stBoolean;
                        ODBCCType := SQL_C_BIT;
                      end;
    SQL_GUID:         begin
                        Result := stGUID;
                        ODBCCType := SQL_C_GUID;
                      end;
    SQL_TYPE_DATE:    begin
                        Result := stDate;
                        ODBCCType := SQL_C_TYPE_DATE;
                      end;
    SQL_TYPE_TIME:    begin
                        Result := stTime;
                        ODBCCType := SQL_C_TYPE_TIME;
                      end;
    SQL_SS_TIME2:     begin
                        Result := stTime;
                        ODBCCType := SQL_C_SS_TIME2;
                      end;
    else              begin
                        Result := stString;
                        ODBCCType := ODBC_Str_C_Type[ConSettings.ClientCodePage^.Encoding = ceUTF16];
                      end;
  end;
  if ODBC_CType <> nil then
    ODBC_CType^ := ODBCCType;
end;

function ConvertODBC_CTypeToODBCType(ODBC_CType: SQLSMALLINT; out Signed: Boolean): SQLSMALLINT;
begin
  Signed := False;
  Result := SQL_UNKNOWN_TYPE;
  case ODBC_CType of
    SQL_C_WCHAR:      Result := SQL_WVARCHAR;
    SQL_C_CHAR:       Result := SQL_CHAR;
    SQL_C_LONG,
    SQL_C_SLONG:      begin
                        Result := SQL_INTEGER;
                        Signed := True;
                      end;
    SQL_C_SHORT,
    SQL_C_SSHORT:     begin
                        Result := SQL_SMALLINT;
                        Signed := True;
                      end;
    SQL_C_FLOAT:      Result := SQL_REAL;
    SQL_C_DOUBLE:     Result := SQL_DOUBLE;
    SQL_C_NUMERIC:    Result := SQL_NUMERIC;
    SQL_C_DATE,
    SQL_C_TYPE_DATE:  Result := SQL_DATE;
    SQL_C_TIME,
    SQL_C_TYPE_TIME:  RESULT := SQL_TIME;
    SQL_C_SS_TIME2:   Result := SQL_SS_TIME2;
    SQL_C_TIMESTAMP,
    SQL_C_TYPE_TIMESTAMP,
    SQL_C_INTERVAL_YEAR,
    SQL_C_INTERVAL_MONTH,
    SQL_C_INTERVAL_DAY,
    SQL_C_INTERVAL_HOUR,
    SQL_C_INTERVAL_MINUTE,
    SQL_C_INTERVAL_SECOND,
    SQL_C_INTERVAL_YEAR_TO_MONTH,
    SQL_C_INTERVAL_DAY_TO_HOUR,
    SQL_C_INTERVAL_DAY_TO_MINUTE,
    SQL_C_INTERVAL_DAY_TO_SECOND,
    SQL_C_INTERVAL_HOUR_TO_MINUTE,
    SQL_C_INTERVAL_HOUR_TO_SECOND,
    SQL_C_INTERVAL_MINUTE_TO_SECOND:  Result := SQL_TIMESTAMP;
    SQL_C_SS_TIMESTAMPOFFSET:         Result := SQL_SS_TIMESTAMPOFFSET;
    SQL_C_BINARY:     Result := SQL_BINARY;
    SQL_C_BIT:        Result := SQL_BIT;
    SQL_C_SBIGINT:    begin
                        Result := SQL_BIGINT;
                        Signed := True;
                      end;
    SQL_C_UBIGINT:    Result := SQL_BIGINT;
    SQL_C_TINYINT,
    SQL_C_STINYINT:   begin
                        Result := SQL_TINYINT;
                        Signed := True;
                      end;
    //SQL_C_BOOKMARK,
    SQL_C_ULONG:      Result := SQL_INTEGER;
    SQL_C_USHORT:     Result := SQL_SMALLINT;
    SQL_C_UTINYINT:   Result := SQL_TINYINT;
  { BOOKMARK         }
    SQL_C_GUID:       Result := SQL_GUID;
  end;
end;

function ConvertSQLTypeToODBCType(SQLType: TZSQLType;
  var ODBC_CType: SQLSMALLINT; CharEncoding: TZCharEncoding): SQLSMALLINT;
begin
  case SQLType of
    stBoolean:                          begin
                                          Result := SQL_BIT;
                                          ODBC_CType := SQL_C_BIT;
                                        end;
    stByte:                             begin
                                          Result := SQL_TINYINT;
                                          ODBC_CType := SQL_C_UTINYINT;
                                        end;
    stShort:                            begin
                                          Result := SQL_TINYINT;
                                          ODBC_CType := SQL_C_STINYINT;
                                        end;
    stWord:                             begin
                                          Result := SQL_SMALLINT;
                                          ODBC_CType := SQL_C_USHORT;
                                        end;
    stSmall:                            begin
                                          Result := SQL_SMALLINT;
                                          ODBC_CType := SQL_C_SSHORT;
                                        end;
    stLongWord:                         begin
                                          Result := SQL_INTEGER;
                                          ODBC_CType := SQL_C_ULONG;
                                        end;
    stInteger:                          begin
                                          Result := SQL_INTEGER;
                                          ODBC_CType := SQL_C_SLONG;
                                        end;
    stULong:                            begin
                                          Result := SQL_BIGINT;
                                          ODBC_CType := SQL_C_UBIGINT;
                                        end;
    stLong:                             begin
                                          Result := SQL_BIGINT;
                                          ODBC_CType := SQL_C_SBIGINT;
                                        end;
    stFloat:                            begin
                                          Result := SQL_C_FLOAT;
                                          ODBC_CType := SQL_REAL;
                                        end;
    stCurrency,
    stBigDecimal:                       begin
                                          Result := SQL_C_NUMERIC;
                                          ODBC_CType := SQL_NUMERIC;
                                        end;
    stDouble:                           begin
                                          Result := SQL_DOUBLE;
                                          ODBC_CType := SQL_C_DOUBLE;
                                        end;
    stString, stUnicodeString:          begin
                                          if Ord(CharEncoding) >= Ord(ceUTF16) then begin
                                            ODBC_CType := SQL_C_WCHAR;
                                            Result := SQL_WVARCHAR
                                          end else begin
                                            Result := SQL_VARCHAR;
                                            ODBC_CType := SQL_C_CHAR;
                                          end;
                                        end;
    stBytes:                            begin
                                          Result := SQL_VARBINARY;
                                          ODBC_CType := SQL_C_BINARY;
                                        end;
    stGUID:                             begin
                                          Result := SQL_GUID;
                                          ODBC_CType := SQL_C_GUID;
                                        end;
    stDate:                             begin
                                          Result := SQL_TYPE_DATE;
                                          ODBC_CType := SQL_C_TYPE_DATE;
                                        end;
    stTime:                             begin
                                          Result := SQL_TYPE_TIME;
                                          ODBC_CType := SQL_C_TYPE_TIME;
                                        end;
    stTimestamp:                        begin
                                          Result := SQL_TYPE_TIMESTAMP;
                                          ODBC_CType := SQL_C_TYPE_TIMESTAMP;
                                        end;
    stAsciiStream, stUnicodeStream: if Ord(CharEncoding) >= Ord(ceUTF16) then begin
                                          ODBC_CType := SQL_C_WCHAR;
                                          Result := SQL_WLONGVARCHAR
                                        end else begin
                                          ODBC_CType := SQL_C_CHAR;
                                          Result := SQL_LONGVARCHAR;
                                        end;
    stBinaryStream:                     begin
                                          Result := SQL_LONGVARBINARY;
                                          ODBC_CType := SQL_C_BINARY;
                                        end;
    else                                begin
                                          Result := SQL_UNKNOWN_TYPE;
                                          ODBC_CType := SQL_UNKNOWN_TYPE;
                                        end;
  end;
end;

function CalcBufSize(ColumnSize: Integer; ODBC_CType: SQLSMALLINT; SQLType: TZSQLType;
  ClientCodePage: PZCodePage): SQLSMALLINT;
begin
  case SQLType of
    stBoolean, stByte, stShort:   Result := 1;
    stWord, stSmall:              Result := 2;
    stLongWord, stInteger,stFloat:Result := 4;
    stULong, stLong, stDouble:    Result := 8;
    stCurrency, stBigDecimal:     Result := SizeOf(TSQL_NUMERIC_STRUCT);
    stString,
    stUnicodeString:            if ClientCodePage^.Encoding >= ceUTF16
                                then Result := (ColumnSize +1) shl 1
                                else Result := ColumnSize*ClientCodePage^.CharWidth +1;
    stGUID:                     Result := SizeOf(TGUID);
    stDate:                     Result := SizeOf(TSQL_DATE_STRUCT);
    stTime:                     if (ODBC_CType = SQL_C_BINARY) or (ODBC_CType=SQL_C_SS_TIME2)
                                then Result := SizeOf(TSQL_SS_TIME2_STRUCT)
                                else Result := SizeOf(TSQL_TIME_STRUCT);
    stTimestamp:                if (ODBC_CType = SQL_C_SS_TIMESTAMPOFFSET)
                                then Result := SizeOf(TSQL_SS_TIMESTAMPOFFSET_STRUCT)
                                else Result := SizeOf(TSQL_TIMESTAMP_STRUCT);
    stAsciiStream,
    stUnicodeStream,
    stBinaryStream:             Result := SizeOf(Pointer){we use SQL_DATA_AT_EXEC and this userdefined token points to our lob-interface};
    else Result := ColumnSize;
  end;
end;

function GetConnectionString(WindowHandle: SQLHWND; const InConnectionString, LibraryLocation: String): String;
var
  URL: TZURL;
  PlainDriver: IODBC3BasePlainDriver;
  ODBC3BaseDriver: TZODBC3PlainDriver;
  HENV: SQLHENV;
  HDBC: SQLHDBC;
  aLen: SQLSMALLINT;
begin
  URL := TZURL.Create;
  URL.Protocol := {$IFDEF UNICODE}'odbc_w'{$ELSE}'odbc_a'{$ENDIF};
  URL.Database := InConnectionString;
  URL.LibLocation := LibraryLocation;
  HDBC := nil;
  HENV := nil;
  PlainDriver := DriverManager.GetDriver(URL.URL).GetPlainDriver(URL).Clone as IODBC3BasePlainDriver;
  ODBC3BaseDriver := TZODBC3PlainDriver(PlainDriver.GetInstance);
  try
    PlainDriver.Initialize(LibraryLocation);
    Assert(ODBC3BaseDriver.SQLAllocHandle(SQL_HANDLE_ENV, Pointer(SQL_NULL_HANDLE), HENV) = SQL_SUCCESS, 'Couldn''t allocate an Environment handle');
    //Try to SET Major Version 3 and minior Version 8
    if ODBC3BaseDriver.SQLSetEnvAttr(HENV, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3_80, 0) <> SQL_SUCCESS then
      //set minimum Major Version 3
      Assert(ODBC3BaseDriver.SQLSetEnvAttr(HENV, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, 0) = SQL_SUCCESS, 'Couln''t set minimum ODBC-Version 3.0');
    Assert(ODBC3BaseDriver.SQLAllocHandle(SQL_HANDLE_DBC,HENV,HDBC) = SQL_SUCCESS, 'Couldn''t allocate a DBC handle');
    {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
    SetLength(Result, 1024);
    aLen := 0;
    if ODBC3BaseDriver.SQLDriverConnect(HDBC, WindowHandle,
      Pointer(InConnectionString), Length(InConnectionString), Pointer(Result),
        Length(Result), @aLen, SQL_DRIVER_PROMPT) = SQL_SUCCESS then
      SetLength(Result, aLen)
    else
      Result := InConnectionString;
  finally
    URL.Free;
    if Assigned(HDBC) then
      ODBC3BaseDriver.SQLFreeHandle(SQL_HANDLE_DBC, HDBC);
    if Assigned(HENV) then
      ODBC3BaseDriver.SQLFreeHandle(SQL_HANDLE_ENV, HENV);
    PlainDriver := nil;
  end;
end;

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
end.
