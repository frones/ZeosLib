{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           ODBC Database Connectivity Classes            }
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

unit ZDbcODBCUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit
uses SysUtils,
  ZCompatibility, ZDbcIntfs, ZPlainODBCDriver, ZDbcODBCCon, ZFastCode;

type
  PStrLen_or_IndArray = ^TStrLen_or_IndArray;
  TStrLen_or_IndArray = array[0..600] of SQLLEN;

{ param binding version }
function ConvertODBCTypeToSQLType(ODBCType: SQLSMALLINT; SQLType: TZSQLType;
  CtrlsCPType: TZControlsCodePage): TZSQLType; overload;
{ native Resultset version }
function ConvertODBCTypeToSQLType(ODBCType: SQLSMALLINT; UnSigned: Boolean;
  CtrlsCPType: TZControlsCodePage): TZSQLType; overload;

function ConvertODBC_CTypeToSQLType(ODBC_CType: SQLSMALLINT; CtrlsCPType: TZControlsCodePage): TZSQLType;
function ConvertODBCTypeToODBC_CType(ODBCType: SQLSMALLINT; UnSigned: Boolean; CharEncoding: TZCharEncoding): SQLSMALLINT;

function ConvertSQLTypeToODBCType(SQLType: TZSQLType; DescribedSQLType: SQLSMALLINT; CharEncoding: TZCharEncoding): SQLSMALLINT;

function ParamTypeToODBCParamType(ParamType: TZProcedureColumnType; SQLType: TZSQLType;
  StreamSupport: Boolean): SQLSMALLINT;

function CalcBufSize(ColumnSize, ODBC_CType: SQLSMALLINT; SQLType: TZSQLType;
  ClientCodePage: PZCodePage): SQLSMALLINT;

{ macros of sqlext.h }
function SQL_SUCCEDED(RETCODE: SQLRETURN): Boolean; {$IFDEF WITH_INLINE}inline; {$ENDIF}
function SQL_LEN_DATA_AT_EXEC(Len: SQLLEN): SQLLEN; {$IFDEF WITH_INLINE}inline; {$ENDIF}

procedure CheckODBCError(RETCODE: SQLRETURN; Handle: SQLHANDLE;
  HandleType: SQLSMALLINT; const Connection: IZODBCConnection);

function GetConnectionString(WindowHandle: SQLHWND; const InConnectionString, LibraryLocation: String): String;

const
  LobArrayIndexOffSet = NativeUInt(SizeOf(Pointer));
  LobParameterIndexOffSet = LobArrayIndexOffSet+NativeUInt(SizeOf(Integer));
  SQL_SS_TIME2ScaleFactor: array[0..7] of word = (1,1,1,10,10,10,100,10);
  SQL_SS_TIME2ScaleDevisor: array[0..7] of word = (10,10,10,1,1,1,1,1);

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit

uses ZEncoding, ZSysUtils, ZMessages, ZDbcLogging, ZURL, ZClasses
 {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

function SQL_SUCCEDED(RETCODE: SQLRETURN): Boolean;
begin
  Result := (RETCODE = SQL_SUCCESS);
end;

function SQL_LEN_DATA_AT_EXEC(Len: SQLLEN): SQLLEN;
begin
  Result := (-(Len)+SQL_LEN_DATA_AT_EXEC_OFFSET)
end;

procedure CheckODBCError(RETCODE: SQLRETURN; Handle: SQLHANDLE;
  HandleType: SQLSMALLINT; const Connection: IZODBCConnection);
var
  PlainDriver: TZODBC3PlainDriver;
  SqlstateA: TSQLSTATE;
  SqlstateW: TSQLSTATE_W;
  MessageText: array[0..SQL_MAX_MESSAGE_LENGTH] of WideChar;
  RecNum, FirstNativeError, NativeError: SQLINTEGER;
  TextLength: SQLSMALLINT;
  FirstMsgW, FirstNErrW: ZWideString;
  MsgW, NErrW, ErrorStringW: ZWideString;
  FirstMsgA, FirstNErrA: RawByteString;
  MsgA, NErrA, ErrorStringA: RawByteString;
  aException: EZSQLThrowable;
begin
  if not SQL_SUCCEDED(RETCODE) then begin
    if (Handle=nil) or (RETCODE=SQL_INVALID_HANDLE) then
      aException := EZSQLException.CreateWithCodeAndStatus(SQL_INVALID_HANDLE, 'HY000', 'Invalid handle')
    else begin
      RecNum := 1;
      FirstNativeError := RETCODE;
      PlainDriver := Connection.GetPlainDriver.GetInstance as TZODBC3PlainDriver;
      if PlainDriver is TODBC3UnicodePlainDriver then begin
        ErrorStringW := '';
        while TODBC3UnicodePlainDriver(PlainDriver).SQLGetDiagRecW(HandleType,Handle,RecNum, @SqlstateW[0],
          @NativeError,@MessageText[0],SQL_MAX_MESSAGE_LENGTH,@TextLength) and (not 1)=0 do begin
          while (TextLength>0) and (MessageText[TextLength-1]<=' ') do //trim trailing lineending and spaces
            dec(TextLength);
          if RecNum = 1 then begin
            FirstNativeError := NativeError;
            if TextLength = 0 then
              FirstMsgW := 'Unidentified error'
            else
              SetString(FirstMsgW, PWideChar(@MessageText[0]), TextLength);
            SetString(FirstNErrW, PWideChar(@SqlstateW[0]), 5);
            ErrorStringW := FirstNErrW+'['+IntToUnicode(NativeError)+']:'+FirstMsgW;
          end else begin
            NErrW := IntToUnicode(NativeError);
            SetLength(MsgW, SizeOf(TSQLSTATE)+Length(NErrW)+2+TextLength);
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(SqlstateW, MsgW[1], SQL_SQLSTATE_SIZE  shl 1);
            MsgW[6] := '[';
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(NErrW[1], MsgW[7], Length(NErrW)  shl 1);
            MsgW[7+Length(NErrW)] := ']'; MsgW[8+Length(NErrW)] := ':';
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(MessageText[0], MsgW[9+Length(NErrW)], TextLength shl 1);
            ErrorStringW := ErrorStringW+ZWideString(LineEnding)+MsgW;
          end;
          inc(RecNum);
        end;
        if RecNum = 1 then
          aException := EZSQLException.CreateWithCodeAndStatus(RETCODE, 'HY000', SUnknownError)
        else
          {$IFDEF UNICODE}
          if RETCODE = SQL_SUCCESS_WITH_INFO then
            aException := EZSQLWarning.CreateWithCodeAndStatus(FirstNativeError, FirstNErrW, ErrorStringW)
          else
            aException := EZSQLException.CreateWithCodeAndStatus(FirstNativeError, FirstNErrW, ErrorStringW);
          {$ELSE}
          if RETCODE = SQL_SUCCESS_WITH_INFO then
            aException := EZSQLWarning.CreateWithCodeAndStatus(FirstNativeError, UnicodeStringToASCII7(FirstNErrW),
              PUnicodeToRaw(Pointer(ErrorStringW), Length(ErrorStringW), ZOSCodePage))
          else
            aException := EZSQLException.CreateWithCodeAndStatus(FirstNativeError, UnicodeStringToASCII7(FirstNErrW),
              PUnicodeToRaw(Pointer(ErrorStringW), Length(ErrorStringW), ZOSCodePage))
          {$ENDIF}
      end else begin
        ErrorStringA := '';
        while TODBC3RawPlainDriver(PlainDriver).SQLGetDiagRec(HandleType,Handle,RecNum, @SqlstateA[0],
          @NativeError,@MessageText[0],SQL_MAX_MESSAGE_LENGTH,@TextLength) and (not 1)=0 do begin
          while (TextLength>0) and (PByte(PAnsiChar(@MessageText[0])+TextLength-1)^ <= Ord(' ')) do //trim trailing lineending and spaces
            dec(TextLength);
          if RecNum = 1 then begin
            FirstNativeError := NativeError;
            if TextLength = 0
            then FirstMsgA := 'Unidentified error'
            else ZSetString(PAnsiChar(@MessageText[0]), TextLength, FirstMsgA);
            ZSetString(PAnsiChar(@SqlstateA[0]), 5, FirstNErrA);
            ErrorStringA := FirstNErrA+'['+IntToRaw(NativeError)+']:'+FirstMsgA;
          end else begin
            NErrA := IntToRaw(NativeError);
            SetLength(MsgA, SizeOf(TSQLSTATE)+Length(NErrA)+2+TextLength);
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(SqlstateA, MsgA[1], SQL_SQLSTATE_SIZE);
            MsgA[6] := '[';
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(NErrA[1], MsgA[7], Length(NErrA));
            MsgA[7+Length(NErrA)] := ']'; MsgA[8+Length(NErrA)] := ':';
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(MessageText[0], MsgA[9+Length(NErrA)], TextLength);
            ErrorStringA := ErrorStringA+RawByteString(LineEnding)+MsgA
          end;
          inc(RecNum);
        end;
        if RecNum = 1 then
          aException := EZSQLException.CreateWithCodeAndStatus(RETCODE, 'HY000', SUnknownError)
        else
          {$IFDEF UNICODE}
          if RETCODE = SQL_SUCCESS_WITH_INFO then
            aException := EZSQLWarning.CreateWithCodeAndStatus(FirstNativeError,
              ASCII7ToUnicodeString(FirstNErrA), PRawToUnicode(Pointer(ErrorStringA), Length(ErrorStringA), ZOSCodePage))
          else
            aException := EZSQLException.CreateWithCodeAndStatus(FirstNativeError,
              ASCII7ToUnicodeString(FirstNErrA), PRawToUnicode(Pointer(ErrorStringA), Length(ErrorStringA), ZOSCodePage))
          {$ELSE}
          if RETCODE = SQL_SUCCESS_WITH_INFO then
            aException := EZSQLWarning.CreateWithCodeAndStatus(FirstNativeError, FirstNErrA, ErrorStringA)
          else
            aException := EZSQLException.CreateWithCodeAndStatus(FirstNativeError, FirstNErrA, ErrorStringA);
          {$ENDIF}
      end;
    end;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcOther,
        {$IFDEF UNICODE}
        PUnicodeToRaw(Pointer(aException.Message), Length(aException.Message), ZOSCodePage),
        UnicodeStringToASCII7(Connection.GetIZPlainDriver.GetProtocol)
        {$ELSE}
        aException.Message, Connection.GetIZPlainDriver.GetProtocol
        {$ENDIF});
    if RETCODE = SQL_SUCCESS_WITH_INFO then
      Connection.SetLastWarning(aException as EZSQLWarning)
    else
      raise aException;
  end;
end;

function ConvertODBCTypeToSQLType(ODBCType: SQLSMALLINT; SQLType: TZSQLType;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  Result := SQLType;
  case ODBCType of
    //SQL_UNKNOWN_TYPE: Result := SQLType;
    SQL_CHAR:         Result  := stString;
    SQL_NUMERIC,
    SQL_DECIMAL:      Result := stDouble;
    SQL_INTEGER:      if SQLType = stLongWord then
                        Result := stLongWord else
                        Result := stInteger;
    SQL_SMALLINT:     if SQLType = stWord then
                        Result := stWord else
                        Result := stSmall;
    //SQL_FLOAT,
    SQL_REAL:           Result := stFloat;
    SQL_FLOAT,
    SQL_DOUBLE:         Result := stDouble;
    SQL_VARCHAR,
    SQL_WCHAR,
    SQL_WVARCHAR:       Result := stString;
    SQL_WLONGVARCHAR:   Result := stAsciiStream;
    SQL_DATETIME, SQL_INTERVAL, SQL_TIMESTAMP:
    //SQL_TIME:           same as SQL_INTERVAL
    //SQL_DATE:           same as SQL_DATETIME
        Result := stTimeStamp;
    SQL_LONGVARCHAR:    Result := stAsciiStream;
    SQL_BINARY,
    SQL_VARBINARY:      Result := stBytes;
    SQL_LONGVARBINARY:  Result := stBinaryStream;
    SQL_BIGINT:         if SQLType = stULong then
                          Result := stULong else
                          Result := stLong;
    SQL_TINYINT:        if SQLType = stByte then
                          Result := stByte else
                          Result := stShort;
    SQL_BIT:            Result := stBoolean;
    SQL_GUID:           Result := stGUID;
    SQL_TYPE_DATE:      Result := stDate;
    SQL_TYPE_TIME:      Result := stTime;
    SQL_TYPE_TIMESTAMP: //if not ((SQLType <> stTimeStamp) and (SQLType in [stDate, stTime])) then
                          Result := stTimeStamp;
    //SQL_TYPE_VARIANT:;
    //SQL_SS_UDT:;
    SQL_SS_XML:         Result := stAsciiStream;
    SQL_SS_TABLE:       Result := stDataSet;
    SQL_SS_TIME2:       Result := stTime;
    SQL_SS_TIMESTAMPOFFSET: Result := stTimeStamp;
  end;
  if (Result = stString) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeString
  else if (Result = stAsciiStream) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeStream
end;

function ConvertODBCTypeToSQLType(ODBCType: SQLSMALLINT; UnSigned: Boolean;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  Result := stString;
  case ODBCType of
    //SQL_UNKNOWN_TYPE: Result := SQLType;
    SQL_CHAR:         Result  := stString;
    SQL_NUMERIC,
    SQL_DECIMAL:      Result := stDouble;
    SQL_INTEGER:      if UnSigned then
                        Result := stLongWord else
                        Result := stInteger;
    SQL_SMALLINT:     if UnSigned then
                        Result := stWord else
                        Result := stSmall;
    //SQL_FLOAT,
    SQL_REAL:           Result := stFloat;
    SQL_FLOAT,
    SQL_DOUBLE:         Result := stDouble;
    SQL_VARCHAR,
    SQL_WCHAR,
    SQL_WVARCHAR:       Result := stString;
    SQL_WLONGVARCHAR:   Result := stAsciiStream;
    SQL_DATETIME,
    SQL_INTERVAL,
    SQL_TIMESTAMP:      Result := stTimeStamp;
    SQL_LONGVARCHAR:    Result := stAsciiStream;
    SQL_BINARY,
    SQL_VARBINARY:      Result := stBytes;
    SQL_LONGVARBINARY:  Result := stBinaryStream;
    SQL_BIGINT:         if UnSigned then
                          Result := stULong else
                          Result := stLong;
    SQL_TINYINT:        if UnSigned then
                          Result := stByte else
                          Result := stShort;
    SQL_BIT:            Result := stBoolean;
    SQL_GUID:           Result := stGUID;
    SQL_TYPE_DATE:      Result := stDate;
    SQL_TYPE_TIME,
    SQL_SS_TIME2:       Result := stTime;
    SQL_TYPE_TIMESTAMP: Result := stTimeStamp;
  end;
  if (Result = stString) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeString
  else if (Result = stAsciiStream) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeStream
end;

function ConvertODBC_CTypeToSQLType(ODBC_CType: SQLSMALLINT;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  Result := stString;
  case ODBC_CType of
    SQL_C_WCHAR,
    SQL_C_CHAR:                 Result := stString;
    SQL_C_LONG,
    SQL_C_SLONG:                Result := stInteger;
    SQL_C_SHORT,
    SQL_C_SSHORT:               Result := stSmall;
    SQL_C_FLOAT:                Result := stFloat;
    SQL_C_DOUBLE,
    SQL_C_NUMERIC:              Result := stDouble;
    SQL_C_DATE,
    SQL_C_TYPE_DATE:            Result := stDate;

    SQL_C_TIME,
    SQL_C_TYPE_TIME,
    SQL_C_SS_TIME2:             Result := stTime;
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
    SQL_C_INTERVAL_MINUTE_TO_SECOND,
    SQL_C_SS_TIMESTAMPOFFSET:         Result := stTimeStamp;
    SQL_C_BINARY:                     Result := stBytes;
    SQL_C_BIT:                        Result := stBoolean;
    SQL_C_SBIGINT:                    Result := stLong;
    SQL_C_UBIGINT:                    Result := stULong;
    SQL_C_TINYINT,
    SQL_C_STINYINT:                   Result := stShort;
    //SQL_C_BOOKMARK,
    SQL_C_ULONG:                      Result := stLongWord;
    SQL_C_USHORT:                     Result := stWord;
    SQL_C_UTINYINT:                   Result := stByte;
  { BOOKMARK         }
    SQL_C_GUID:                       Result := stGUID;
    SQL_TYPE_NULL: ;


  end;
  if (Result = stString) and (CtrlsCPType = cCP_UTF16) then
    Result := stUnicodeString
end;

function ConvertODBCTypeToODBC_CType(ODBCType: SQLSMALLINT; UnSigned: Boolean;
  CharEncoding: TZCharEncoding): SQLSMALLINT;
begin
  Result := SQL_TYPE_NULL;
  case ODBCType of
    SQL_NUMERIC,
    SQL_DECIMAL:        REsult := SQL_C_DOUBLE;
    SQL_INTEGER:        if Unsigned then
                          Result := SQL_C_ULONG else
                          Result := SQL_C_LONG;
    SQL_SMALLINT:       if Unsigned then
                          Result := SQL_C_USHORT else
                          Result := SQL_C_SSHORT;
    SQL_FLOAT,
    SQL_DOUBLE:         Result := SQL_C_DOUBLE;
    SQL_REAL:           Result := SQL_C_FLOAT;
    SQL_DATETIME:       Result := SQL_C_TIMESTAMP;
    SQL_CHAR,
    SQL_VARCHAR,
    SQL_WCHAR,
    SQL_WVARCHAR,
    SQL_WLONGVARCHAR,
    SQL_LONGVARCHAR:   if CharEncoding = ceUTF16 then
                          Result := SQL_C_WCHAR else
                          Result := SQL_C_CHAR;
    SQL_TIME:           Result := SQL_C_TYPE_TIME;
    SQL_TIMESTAMP:      Result := SQL_C_TIMESTAMP;
    SQL_BINARY,
    SQL_VARBINARY,
    SQL_LONGVARBINARY:  Result := SQL_C_BINARY;
    SQL_BIGINT:         if Unsigned then
                          Result := SQL_C_UBIGINT else
                          Result := SQL_C_SBIGINT;
    SQL_TINYINT:        if Unsigned then
                          Result := SQL_C_UTINYINT else
                          Result := SQL_C_STINYINT;
    SQL_BIT:            Result := SQL_C_BIT;
    SQL_GUID:           Result := SQL_C_GUID;
    SQL_TYPE_DATE:      Result := SQL_C_TYPE_DATE;
    SQL_TYPE_TIME:      Result := SQL_C_TYPE_TIME;
    SQL_TYPE_TIMESTAMP: Result := SQL_C_TYPE_TIMESTAMP;
    SQL_TYPE_VARIANT,
    SQL_SS_UDT:         Result := SQL_TYPE_NULL;
    SQL_SS_XML:         if CharEncoding = ceUTF16 then
                          Result := SQL_C_WCHAR else
                          Result := SQL_C_CHAR;
    SQL_SS_TABLE:   Result := SQL_TYPE_NULL;
    SQL_SS_TIME2:   Result := SQL_C_BINARY;
    SQL_SS_TIMESTAMPOFFSET: Result := SQL_C_TYPE_TIMESTAMP;
  end;
end;

function ConvertSQLTypeToODBCType(SQLType: TZSQLType;
  DescribedSQLType: SQLSMALLINT; CharEncoding: TZCharEncoding): SQLSMALLINT;
begin
  Result := SQL_UNKNOWN_TYPE; //stUnknwown, stDataSet, stArray
  case SQLType of
    stBoolean:                          Result := SQL_BIT;
    stByte,stShort:                     Result := SQL_TINYINT;
    stWord, stSmall:                    Result := SQL_SMALLINT;
    stLongWord, stInteger:              Result := SQL_INTEGER;
    stULong, stLong:                    Result := SQL_BIGINT;
    stFloat:                            Result := SQL_FLOAT;
    stDouble, stCurrency, stBigDecimal: Result := SQL_DOUBLE;
    stString, stUnicodeString:
      //EH: i've been running into many issues accrding type mapping
      //same issue described: http://blogs.msdn.com/b/psssql/archive/2012/04/24/behavior-change-when-handling-character-conversions-sql-server-s-odbc-driver-sql-2012-version-11-xx.aspx
      //so C data type mapping works like expected but only chunked data is supported
      //whereas the unconverted types should be used as is
      if Ord(CharEncoding) >= Ord(ceUTF16) then
        if DescribedSQLType = SQL_VARCHAR then
          Result := SQL_VARCHAR
        else
          Result := SQL_WVARCHAR
      else
        if DescribedSQLType = SQL_WVARCHAR then
          Result := SQL_WVARCHAR
        else
          Result := SQL_VARCHAR;
    stBytes:                            Result := SQL_VARBINARY;
    stGUID:                             Result := SQL_GUID;
    stDate:                             Result := SQL_TYPE_DATE;
    stTime:                             if DescribedSQLType = SQL_SS_TIME2 then
                                          Result := SQL_SS_TIME2 else
                                          Result := SQL_TYPE_TIME;
    stTimestamp:                        Result := SQL_TYPE_TIMESTAMP;
    stAsciiStream, stUnicodeStream:
      if Ord(CharEncoding) >= Ord(ceUTF16) then
        if DescribedSQLType = SQL_LONGVARCHAR then
          Result := SQL_LONGVARCHAR
        else
          Result := SQL_WLONGVARCHAR
      else
        if DescribedSQLType = SQL_WLONGVARCHAR then
          Result := SQL_WLONGVARCHAR
        else
          Result := SQL_LONGVARCHAR;
    stBinaryStream:                     Result := SQL_LONGVARBINARY;
  end;
end;

function CalcBufSize(ColumnSize, ODBC_CType: SQLSMALLINT; SQLType: TZSQLType;
  ClientCodePage: PZCodePage): SQLSMALLINT;
begin
  Result := ColumnSize;
  case SQLType of
    stBoolean, stByte, stShort:                         Result := 1;
    stWord, stSmall:                                    Result := 2;
    stLongWord, stInteger,stFloat:                      Result := 4;
    stULong, stLong, stDouble, stCurrency, stBigDecimal:Result := 8;
    stString,
    stUnicodeString:
      if ClientCodePage^.Encoding >= ceUTF16 then
        Result := (Result +1) shl 1
      else
        Result := Result*ClientCodePage^.CharWidth +1;
    stGUID:                     Result := SizeOf(TGUID);
    stDate:                     Result := SizeOf(TSQL_DATE_STRUCT);
    stTime:                     if ODBC_CType = SQL_C_BINARY then
                                  Result := SizeOf(TSQL_SS_TIME2_STRUCT) else
                                  Result := SizeOf(TSQL_TIME_STRUCT);
    stTimestamp:                Result := SizeOf(TSQL_TIMESTAMP_STRUCT);
    stAsciiStream,
    stUnicodeStream,
    stBinaryStream:             Result := 0;
  end;
end;

function ParamTypeToODBCParamType(ParamType: TZProcedureColumnType;
  SQLType: TZSQLType; StreamSupport: Boolean): SQLSMALLINT;
begin
  Result := SQL_PARAM_TYPE_UNKNOWN;
  case ParamType of
    pctUnknown, pctInOut: if StreamSupport then
      if (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
        Result := SQL_PARAM_INPUT_OUTPUT_STREAM
      else Result := SQL_PARAM_INPUT_OUTPUT;
    pctIn: Result := SQL_PARAM_INPUT;
    pctOut, pctReturn: if (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
        Result := SQL_PARAM_OUTPUT_STREAM
      else Result := SQL_PARAM_OUTPUT;
    pctResultSet: Result := SQL_PARAM_OUTPUT;
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
    Assert(SQL_SUCCEDED(ODBC3BaseDriver.SQLAllocHandle(SQL_HANDLE_ENV, Pointer(SQL_NULL_HANDLE), HENV)), 'Couldn''t allocate an Environment handle');
    //Try to SET Major Version 3 and minior Version 8
    if not SQL_SUCCEDED(ODBC3BaseDriver.SQLSetEnvAttr(HENV, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3_80, 0)) then
      //set minimum Major Version 3
      Assert(SQL_SUCCEDED(ODBC3BaseDriver.SQLSetEnvAttr(HENV, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, 0)), 'Couln''t set minimum ODBC-Version 3.0');
    Assert(SQL_SUCCEDED(ODBC3BaseDriver.SQLAllocHandle(SQL_HANDLE_DBC,HENV,HDBC)), 'Couldn''t allocate a DBC handle');
    SetLength(Result, 1024);
    aLen := 0;
    if SQL_SUCCEDED(ODBC3BaseDriver.SQLDriverConnect(HDBC, WindowHandle,
      Pointer(InConnectionString), Length(InConnectionString), Pointer(Result),
        Length(Result), @aLen, SQL_DRIVER_PROMPT)) then
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
