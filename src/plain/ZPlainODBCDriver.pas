{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for ODBC               }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZPlainODBCDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ODBC}

uses
  ZCompatibility, ZPlainDriver;

{$IF defined(Unix) or defined (MSWINDOWS)}
const
  ODBC_LOCATION = {$IFDEF MSWINDOWS}'odbc32'{$ELSE}'libodbc'{$ENDIF}+ SharedSuffix;
{$IFEND}

(* maped headers are:
  sql.h
  sqlext.h
  sqlspi.h
  sqltypes.h
  sqlucode.h
*)

type
(* API declaration data types *)
  SQLCHAR = Byte;
  PSQLCHAR = PAnsiChar;
  SQLWCHAR = Word;
  PSQLWCHAR = PWideChar;
  SQLSCHAR = ShortInt;
  SQLDATE = Byte;
  SQLDECIMAL = Byte;
  SQLDOUBLE = Double;
  SQLFLOAT = Double;
  PSQLINTEGER = ^SQLINTEGER;
  SQLINTEGER = Integer;
  SQLUINTEGER = Cardinal;
  PSQLLEN = ^SQLLEN;
  SQLLEN = NativeInt;
  PSQLULEN = ^SQLULEN;
  SQLULEN = NativeUInt;
  SQLSETPOSIROW = {$IFDEF CPU64}UInt64{$ELSE}Word{$ENDIF};
  SQLHWND = Pointer;
  PSQLLENArray = ^TSQLLENArray;
  TSQLLENArray = array[Byte] of SQLLEN;

//For Backward compatibility
  SQLROWCOUNT = SQLULEN;
  SQLROWSETSIZE = SQLULEN;
  SQLTRANSID = SQLULEN;
  SQLROWOFFSET = SQLLEN;

  SQLNUMERIC = Byte;
  PSQLPOINTER = ^SQLPOINTER;
  SQLPOINTER = Pointer;
  SQLREAL = Single;
  PSQLSMALLINT = ^SQLSMALLINT;
  SQLSMALLINT = SmallInt;
  PSQLUSMALLINT = ^SQLUSMALLINT;
  SQLUSMALLINT = Word;
  SQLTIME = Byte;
  SQLTIMESTAMP = Byte;
  SQLVARCHAR = Byte;

(* function return type *)
  SQLRETURN = SQLSMALLINT;

(* generic data structures *)
  SQLHANDLE = Pointer;
  SQLHENV = SQLHANDLE;
  PSQLHDBC = ^SQLHANDLE;
  SQLHDBC = SQLHANDLE;
  PSQLHSTMT = ^SQLHSTMT;
  SQLHSTMT = SQLHANDLE;
  SQLHDESC = SQLHANDLE;

  PRETCODE = ^RETCODE;
  RETCODE = SmallInt;

const
  { size of SQLSTATE  }
  SQL_SQLSTATE_SIZE = 5;
type
  PSQLSTATE = ^TSQLSTATE;
  TSQLSTATE = array[0..SQL_SQLSTATE_SIZE] of SQLCHAR;

  PSQLSTATE_W = ^TSQLSTATE_W;
  TSQLSTATE_W = array[0..SQL_SQLSTATE_SIZE] of SQLWCHAR;

  {.$A-}
//* transfer types for DATE, TIME, TIMESTAMP */
  PSQL_DATE_STRUCT = ^TSQL_DATE_STRUCT;
  TSQL_DATE_STRUCT = packed record
    year:   SQLSMALLINT;
    month:  SQLUSMALLINT;
    day:    SQLUSMALLINT;
  end;

  PSQL_TIME_STRUCT = ^TSQL_TIME_STRUCT;
  TSQL_TIME_STRUCT = packed  record
    hour:   SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
  end;

  PSQL_SS_TIME2_STRUCT = ^TSQL_SS_TIME2_STRUCT;
  TSQL_SS_TIME2_STRUCT = {packet is wrong! size should be 12 else numeric value out of range } record
    hour:     SQLUSMALLINT;
    minute:   SQLUSMALLINT;
    second:   SQLUSMALLINT;
    fraction: SQLUINTEGER;
  end;

  PSQL_TIMESTAMP_STRUCT = ^TSQL_TIMESTAMP_STRUCT;
  TSQL_TIMESTAMP_STRUCT = packed record
    year:     SQLSMALLINT;
    month:    SQLUSMALLINT;
    day:      SQLUSMALLINT;
    hour:     SQLUSMALLINT;
    minute:   SQLUSMALLINT;
    second:   SQLUSMALLINT;
    fraction: SQLUINTEGER;
  end;

// New Structure for TIMESTAMPOFFSET
  PSQL_SS_TIMESTAMPOFFSET_STRUCT = ^TSQL_SS_TIMESTAMPOFFSET_STRUCT;
  TSQL_SS_TIMESTAMPOFFSET_STRUCT = record
    year:           SQLSMALLINT;
    month:          SQLUSMALLINT;
    day:            SQLUSMALLINT;
    hour:           SQLUSMALLINT;
    minute:         SQLUSMALLINT;
    second:         SQLUSMALLINT;
    fraction:       SQLUINTEGER;
    timezone_hour:  SQLSMALLINT;
    timezone_minute:SQLSMALLINT;
  end;

const
  SQL_IS_YEAR             = 1;
  SQL_IS_MONTH            = 2;
  SQL_IS_DAY              = 3;
  SQL_IS_HOUR             = 4;
  SQL_IS_MINUTE           = 5;
  SQL_IS_SECOND           = 6;
  SQL_IS_YEAR_TO_MONTH    = 7;
  SQL_IS_DAY_TO_HOUR      = 8;
  SQL_IS_DAY_TO_MINUTE    = 9;
  SQL_IS_DAY_TO_SECOND    = 10;
  SQL_IS_HOUR_TO_MINUTE   = 11;
  SQL_IS_HOUR_TO_SECOND   = 12;
  SQL_IS_MINUTE_TO_SECOND = 13;

type
  SQL_YEAR_MONTH_STRUCT = packed record
    year:   SQLUINTEGER;
    month:  SQLUINTEGER;
  end;

  SQL_DAY_SECOND_STRUCT = packed record
    day:      SQLUINTEGER;
    hour:     SQLUINTEGER;
    minute:   SQLUINTEGER;
    second:   SQLUINTEGER;
    fraction: SQLUINTEGER;
  end;

  SQL_INTERVAL_STRUCT = packed record
    interval_type: SQLINTEGER;
    interval_sign: SQLSMALLINT;
    case SQLINTEGER of
      SQL_IS_YEAR_TO_MONTH: (year_month: SQL_YEAR_MONTH_STRUCT);
      SQL_IS_DAY_TO_SECOND: (day_second: SQL_DAY_SECOND_STRUCT);
  end;

//https://docs.microsoft.com/de-de/sql/odbc/reference/appendixes/retrieve-numeric-data-sql-numeric-struct-kb222831?view=sql-server-2017
const
  SQL_MAX_NUMERIC_LEN = 16;
type
  PSQL_NUMERIC_STRUCT = ^TSQL_NUMERIC_STRUCT;
  TSQL_NUMERIC_STRUCT = packed record
    precision:  SQLCHAR;
    scale:      SQLSCHAR;
    sign:       SQLCHAR; //* 1 if positive, 0 if negative */
    val:        array[0..SQL_MAX_NUMERIC_LEN-1] of SQLCHAR;
  end;
  {.$A+}

{$ifndef ODBCVER}
  const
    ODBCVER = $0380;
{$endif}

{ special length/indicator values  }
const
  SQL_NULL_DATA = -(1);
  SQL_DATA_AT_EXEC = -(2);

{ return values from functions  }
  SQL_SUCCESS = 0;
  SQL_SUCCESS_WITH_INFO = 1;
  SQL_NO_DATA = 100;
  SQL_PARAM_DATA_AVAILABLE = 101; //(ODBCVER >= 0x0380)

const
  SQL_ERROR = -(1);
  SQL_INVALID_HANDLE = -(2);
  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;

{ flags for null-terminated string  }
const
  SQL_NTS = -(3);
  SQL_NTSL = -(3);

{ maximum message length  }
  SQL_MAX_MESSAGE_LENGTH = 512;
{ date/time length constants  }

const
  SQL_DATE_LEN = 10;
{ add P+1 if precision is nonzero  }
  SQL_TIME_LEN = 8;
{ add P+1 if precision is nonzero  }
  SQL_TIMESTAMP_LEN = 19;

{ handle type identifiers  }
const
  SQL_HANDLE_ENV = 1;
  SQL_HANDLE_DBC = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

{ environment attribute  }
const
  QL_ATTR_OUTPUT_NTSS = 10001;

{ connection attributes  }
const
  SQL_ATTR_AUTO_IPD = 10001;
  SQL_ATTR_METADATA_ID = 10014;

{ statement attributes  }
const
  SQL_ATTR_APP_ROW_DESC = 10010;
  SQL_ATTR_APP_PARAM_DESC = 10011;
  SQL_ATTR_IMP_ROW_DESC = 10012;
  SQL_ATTR_IMP_PARAM_DESC = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE = -(1);
  SQL_ATTR_CURSOR_SENSITIVITY = -(2);

{ SQL_ATTR_CURSOR_SCROLLABLE values  }
const
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE = 1;

{ identifiers of fields in the SQL descriptor  }
const
  SQL_DESC_COUNT = 1001;
  SQL_DESC_TYPE = 1002;
  SQL_DESC_LENGTH = 1003;
  SQL_DESC_OCTET_LENGTH_PTR = 1004;
  SQL_DESC_PRECISION = 1005;
  SQL_DESC_SCALE = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE = 1008;
  SQL_DESC_INDICATOR_PTR = 1009;
  SQL_DESC_DATA_PTR = 1010;
  SQL_DESC_NAME = 1011;
  SQL_DESC_UNNAMED = 1012;
  SQL_DESC_OCTET_LENGTH = 1013;
  SQL_DESC_ALLOC_TYPE = 1099;

{ identifiers of fields in the diagnostics area  }
const
  SQL_DIAG_RETURNCODE = 1;
  SQL_DIAG_NUMBER = 2;
  SQL_DIAG_ROW_COUNT = 3;
  SQL_DIAG_SQLSTATE = 4;
  SQL_DIAG_NATIVE = 5;
  SQL_DIAG_MESSAGE_TEXT = 6;
  SQL_DIAG_DYNAMIC_FUNCTION = 7;
  SQL_DIAG_CLASS_ORIGIN = 8;
  SQL_DIAG_SUBCLASS_ORIGIN = 9;
  SQL_DIAG_CONNECTION_NAME = 10;
  SQL_DIAG_SERVER_NAME = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

{ dynamic function codes  }
const
  SQL_DIAG_ALTER_DOMAIN = 3;
  SQL_DIAG_ALTER_TABLE = 4;
  SQL_DIAG_CALL = 7;
  SQL_DIAG_CREATE_ASSERTION = 6;
  SQL_DIAG_CREATE_CHARACTER_SET = 8;
  SQL_DIAG_CREATE_COLLATION = 10;
  SQL_DIAG_CREATE_DOMAIN = 23;
  SQL_DIAG_CREATE_INDEX = -(1);
  SQL_DIAG_CREATE_SCHEMA = 64;
  SQL_DIAG_CREATE_TABLE = 77;
  SQL_DIAG_CREATE_TRANSLATION = 79;
  SQL_DIAG_CREATE_VIEW = 84;
  SQL_DIAG_DELETE_WHERE = 19;
  SQL_DIAG_DROP_ASSERTION = 24;
  SQL_DIAG_DROP_CHARACTER_SET = 25;
  SQL_DIAG_DROP_COLLATION = 26;
  SQL_DIAG_DROP_DOMAIN = 27;
  SQL_DIAG_DROP_INDEX = -(2);
  SQL_DIAG_DROP_SCHEMA = 31;
  SQL_DIAG_DROP_TABLE = 32;
  SQL_DIAG_DROP_TRANSLATION = 33;
  SQL_DIAG_DROP_VIEW = 36;
  SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
  SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
  SQL_DIAG_GRANT = 48;
  SQL_DIAG_INSERT = 50;
  SQL_DIAG_REVOKE = 59;
  SQL_DIAG_SELECT_CURSOR = 85;
  SQL_DIAG_UNKNOWN_STATEMENT = 0;
  SQL_DIAG_UPDATE_WHERE = 82;

{ SQL data type codes  }
const
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR = 1;
  SQL_NUMERIC = 2;
  SQL_DECIMAL = 3;
  SQL_INTEGER = 4;
  SQL_SMALLINT = 5;
  SQL_FLOAT = 6;
  SQL_REAL = 7;
  SQL_DOUBLE = 8;
  SQL_DATETIME = 9;
  SQL_VARCHAR = 12;
  SQL_WCHAR = (-8);
  SQL_WVARCHAR = (-9);
  SQL_WLONGVARCHAR = (-10);

{ SQL extended datatypes  }
const
  SQL_DATE = 9;
  SQL_INTERVAL = 10;
  SQL_TIME = 10;
  SQL_TIMESTAMP = 11;
  SQL_LONGVARCHAR = -(1);
  SQL_BINARY = -(2);
  SQL_VARBINARY = -(3);
  SQL_LONGVARBINARY = -(4);
  SQL_BIGINT = -(5);
  SQL_TINYINT = -(6);
  SQL_BIT = -(7);
  SQL_GUID = -(11); //(ODBCVER >= 0x0350)

  { One-parameter shortcuts for date/time data types  }
  SQL_TYPE_DATE = 91;
  SQL_TYPE_TIME = 92;
  SQL_TYPE_TIMESTAMP = 93;

// Driver specific SQL data type defines.
// Microsoft has -150 thru -199 reserved for Microsoft SQL Server Native Client driver usage.
  SQL_TYPE_VARIANT = -(150);
  SQL_SS_UDT = (-151);
  SQL_SS_XML = (-152);
  SQL_SS_TABLE = (-153);
  SQL_SS_TIME2 = -(154);
  SQL_SS_TIMESTAMPOFFSET = (-155);

{ interval code  }
const
  SQL_CODE_YEAR = 1;
  SQL_CODE_MONTH = 2;
  SQL_CODE_DAY = 3;
  SQL_CODE_HOUR = 4;
  SQL_CODE_MINUTE = 5;
  SQL_CODE_SECOND = 6;
  SQL_CODE_YEAR_TO_MONTH = 7;
  SQL_CODE_DAY_TO_HOUR = 8;
  SQL_CODE_DAY_TO_MINUTE = 9;
  SQL_CODE_DAY_TO_SECOND = 10;
  SQL_CODE_HOUR_TO_MINUTE = 11;
  SQL_CODE_HOUR_TO_SECOND = 12;
  SQL_CODE_MINUTE_TO_SECOND = 13;
  SQL_INTERVAL_YEAR = 100+SQL_CODE_YEAR;
  SQL_INTERVAL_MONTH = 100+SQL_CODE_MONTH;
  SQL_INTERVAL_DAY = 100+SQL_CODE_DAY;
  SQL_INTERVAL_HOUR = 100+SQL_CODE_HOUR;
  SQL_INTERVAL_MINUTE = 100+SQL_CODE_MINUTE;
  SQL_INTERVAL_SECOND = 100+SQL_CODE_SECOND;
  SQL_INTERVAL_YEAR_TO_MONTH = 100+SQL_CODE_YEAR_TO_MONTH;
  SQL_INTERVAL_DAY_TO_HOUR = 100+SQL_CODE_DAY_TO_HOUR;
  SQL_INTERVAL_DAY_TO_MINUTE = 100+SQL_CODE_DAY_TO_MINUTE;
  SQL_INTERVAL_DAY_TO_SECOND = 100+SQL_CODE_DAY_TO_SECOND;
  SQL_INTERVAL_HOUR_TO_MINUTE = 100+SQL_CODE_HOUR_TO_MINUTE;
  SQL_INTERVAL_HOUR_TO_SECOND = 100+SQL_CODE_HOUR_TO_SECOND;
  SQL_INTERVAL_MINUTE_TO_SECOND = 100+SQL_CODE_MINUTE_TO_SECOND;

  SQL_UNICODE = SQL_WCHAR;
  SQL_UNICODE_VARCHAR = SQL_WVARCHAR;
  SQL_UNICODE_LONGVARCHAR = SQL_WLONGVARCHAR;
  SQL_UNICODE_CHAR = SQL_WCHAR;

  { C datatype to SQL datatype mapping      SQL types
                                             -------------------  }
  { CHAR, VARCHAR, DECIMAL, NUMERIC  }
  const
    SQL_C_WCHAR = SQL_WCHAR;

    SQL_C_CHAR = SQL_CHAR;
  { INTEGER                       }
    SQL_C_LONG = SQL_INTEGER;
  { SMALLINT                      }
    SQL_C_SHORT = SQL_SMALLINT;
  { REAL                          }
    SQL_C_FLOAT = SQL_REAL;
  { FLOAT, DOUBLE                 }
    SQL_C_DOUBLE = SQL_DOUBLE;
    SQL_C_NUMERIC = SQL_NUMERIC;
    SQL_C_DEFAULT = 99;
    SQL_SIGNED_OFFSET = -(20);
    SQL_UNSIGNED_OFFSET = -(22);

  { C datatype to SQL datatype mapping  }
    SQL_C_DATE = SQL_DATE;
    SQL_C_TIME = SQL_TIME;
    SQL_C_TIMESTAMP = SQL_TIMESTAMP;
    SQL_C_TYPE_DATE = SQL_TYPE_DATE;
    SQL_C_TYPE_TIME = SQL_TYPE_TIME;
    SQL_C_TYPE_TIMESTAMP = SQL_TYPE_TIMESTAMP;
    SQL_C_INTERVAL_YEAR = SQL_INTERVAL_YEAR;
    SQL_C_INTERVAL_MONTH = SQL_INTERVAL_MONTH;
    SQL_C_INTERVAL_DAY = SQL_INTERVAL_DAY;
    SQL_C_INTERVAL_HOUR = SQL_INTERVAL_HOUR;
    SQL_C_INTERVAL_MINUTE = SQL_INTERVAL_MINUTE;
    SQL_C_INTERVAL_SECOND = SQL_INTERVAL_SECOND;
    SQL_C_INTERVAL_YEAR_TO_MONTH = SQL_INTERVAL_YEAR_TO_MONTH;
    SQL_C_INTERVAL_DAY_TO_HOUR = SQL_INTERVAL_DAY_TO_HOUR;
    SQL_C_INTERVAL_DAY_TO_MINUTE = SQL_INTERVAL_DAY_TO_MINUTE;
    SQL_C_INTERVAL_DAY_TO_SECOND = SQL_INTERVAL_DAY_TO_SECOND;
    SQL_C_INTERVAL_HOUR_TO_MINUTE = SQL_INTERVAL_HOUR_TO_MINUTE;
    SQL_C_INTERVAL_HOUR_TO_SECOND = SQL_INTERVAL_HOUR_TO_SECOND;
    SQL_C_INTERVAL_MINUTE_TO_SECOND = SQL_INTERVAL_MINUTE_TO_SECOND;
    SQL_C_BINARY = SQL_BINARY;
    SQL_C_BIT = SQL_BIT;
    SQL_C_SBIGINT = SQL_BIGINT+SQL_SIGNED_OFFSET; { SIGNED BIGINT  }
    SQL_C_UBIGINT = SQL_BIGINT+SQL_UNSIGNED_OFFSET; { UNSIGNED BIGINT  }
    SQL_C_TINYINT = SQL_TINYINT;
    SQL_C_SLONG = SQL_C_LONG+SQL_SIGNED_OFFSET; { SIGNED INTEGER }
    SQL_C_SSHORT = SQL_C_SHORT+SQL_SIGNED_OFFSET; { SIGNED SMALLINT }
    SQL_C_STINYINT = SQL_TINYINT+SQL_SIGNED_OFFSET; { SIGNED TINYINT }
    SQL_C_ULONG = SQL_C_LONG+SQL_UNSIGNED_OFFSET; { UNSIGNED INTEGER }
    SQL_C_USHORT = SQL_C_SHORT+SQL_UNSIGNED_OFFSET; { UNSIGNED SMALLINT }
    SQL_C_UTINYINT = SQL_TINYINT+SQL_UNSIGNED_OFFSET; { UNSIGNED TINYINT }
  { BOOKMARK         }
    SQL_C_BOOKMARK = {$ifdef CPU64}SQL_C_UBIGINT{$else}SQL_C_ULONG{$ENDIF};
    SQL_C_GUID = SQL_GUID; //(ODBCVER >= 0x0350)
    SQL_TYPE_NULL = 0;
// Extended C Types range 4000 and above. Range of -100 thru 200 is reserved by Driver Manager.

    SQL_C_TYPES_EXTENDED = $04000;
    SQL_C_SS_TIME2 = (SQL_C_TYPES_EXTENDED+0);
    SQL_C_SS_TIMESTAMPOFFSET = (SQL_C_TYPES_EXTENDED+1);

{ Statement attribute values for cursor sensitivity  }
const
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE = 2;

{ GetTypeInfo() request for all data types  }
const
  SQL_ALL_TYPES = 0;
{ Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()  }
const
  SQL_DEFAULT = 99;

{ SQLSQLLEN GetData() code indicating that the application row descriptor
 * specifies the data type }
const
  SQL_ARD_TYPE = -(99);
  SQL_APD_TYPE = -(100);
{ SQL date/time type subcodes  }
const
  SQL_CODE_DATE = 1;
  SQL_CODE_TIME = 2;
  SQL_CODE_TIMESTAMP = 3;

{ CLI option values  }
const
  SQL_FALSE = 0;
  SQL_TRUE = 1;

{ values of NULLABLE field in descriptor  }
const
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;
{ Value returned by SQLGetTypeInfo() to denote that it is
 * not known whether or not a data type supports null values. }
  SQL_NULLABLE_UNKNOWN = 2;

{ Values returned by SQLGetTypeInfo() to show WHERE clause
 * supported }
const
  SQL_PRED_NONE = 0;
  SQL_PRED_CHAR = 1;
  SQL_PRED_BASIC = 2;

{ values of UNNAMED field in descriptor  }
const
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

{ values of ALLOC_TYPE field in descriptor  }
const
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

{ FreeStmt() options  }
const
  SQL_CLOSE = 0;
  SQL_DROP = 1;
  SQL_UNBIND = 2;
  SQL_RESET_PARAMS = 3;
{ Codes used for FetchOrientation in SQLFetchScroll(),
   and in SQLDataSources() }
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;
{ Other codes used for FetchOrientation in SQLFetchScroll()  }
  SQL_FETCH_LAST = 3;
  SQL_FETCH_PRIOR = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;
{ SQLEndTran() options  }
  SQL_COMMIT = 0;
  SQL_ROLLBACK = 1;
{ null handles returned by SQLAllocHandle()  }
  SQL_NULL_HENV = 0;
  SQL_NULL_HDBC = 0;
  SQL_NULL_HSTMT = 0;
  SQL_NULL_HDESC = 0;
{ null handle used in place of parent handle when allocating HENV  }
  SQL_NULL_HANDLE = 0;

{ Values that may appear in the result set of SQLSpecialColumns()  }
const
  SQL_SCOPE_CURROW = 0;
  SQL_SCOPE_TRANSACTION = 1;
  SQL_SCOPE_SESSION = 2;
  SQL_PC_UNKNOWN = 0;
  SQL_PC_NON_PSEUDO = 1;
  SQL_PC_PSEUDO = 2;

{ Reserved value for the IdentifierType argument of SQLSpecialColumns()  }
  SQL_ROW_IDENTIFIER = 1;

{ Reserved values for UNIQUE argument of SQLStatistics()  }
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL = 1;

{ Values that may appear in the result set of SQLStatistics()  }
  SQL_INDEX_CLUSTERED = 1;
  SQL_INDEX_HASHED = 2;
  SQL_INDEX_OTHER = 3;

{ SQLGetFunctions() values to identify ODBC APIs  }
  SQL_API_SQLALLOCCONNECT = 1;
  SQL_API_SQLALLOCENV = 2;
  SQL_API_SQLALLOCHANDLE = 1001;
  SQL_API_SQLALLOCSTMT = 3;
  SQL_API_SQLBINDCOL = 4;
  SQL_API_SQLBINDPARAM = 1002;
  SQL_API_SQLCANCEL = 5;
  SQL_API_SQLCLOSECURSOR = 1003;
  SQL_API_SQLCOLATTRIBUTE = 6;
  SQL_API_SQLCOLUMNS = 40;
  SQL_API_SQLCONNECT = 7;
  SQL_API_SQLCOPYDESC = 1004;
  SQL_API_SQLDATASOURCES = 57;
  SQL_API_SQLDESCRIBECOL = 8;
  SQL_API_SQLDISCONNECT = 9;
  SQL_API_SQLENDTRAN = 1005;
  SQL_API_SQLERROR = 10;
  SQL_API_SQLEXECDIRECT = 11;
  SQL_API_SQLEXECUTE = 12;
  SQL_API_SQLFETCH = 13;
  SQL_API_SQLFETCHSCROLL = 1021;
  SQL_API_SQLFREECONNECT = 14;
  SQL_API_SQLFREEENV = 15;
  SQL_API_SQLFREEHANDLE = 1006;
  SQL_API_SQLFREESTMT = 16;
  SQL_API_SQLGETCONNECTATTR = 1007;
  SQL_API_SQLGETCONNECTOPTION = 42;
  SQL_API_SQLGETCURSORNAME = 17;
  SQL_API_SQLGETDATA = 43;
  SQL_API_SQLGETDESCFIELD = 1008;
  SQL_API_SQLGETDESCREC = 1009;
  SQL_API_SQLGETDIAGFIELD = 1010;
  SQL_API_SQLGETDIAGREC = 1011;
  SQL_API_SQLGETENVATTR = 1012;
  SQL_API_SQLGETFUNCTIONS = 44;
  SQL_API_SQLGETINFO = 45;
  SQL_API_SQLGETSTMTATTR = 1014;
  SQL_API_SQLGETSTMTOPTION = 46;
  SQL_API_SQLGETTYPEINFO = 47;
  SQL_API_SQLNUMRESULTCOLS = 18;
  SQL_API_SQLPARAMDATA = 48;
  SQL_API_SQLPREPARE = 19;
  SQL_API_SQLPUTDATA = 49;
  SQL_API_SQLROWCOUNT = 20;
  SQL_API_SQLSETCONNECTATTR = 1016;
  SQL_API_SQLSETCONNECTOPTION = 50;
  SQL_API_SQLSETCURSORNAME = 21;
  SQL_API_SQLSETDESCFIELD = 1017;
  SQL_API_SQLSETDESCREC = 1018;
  SQL_API_SQLSETENVATTR = 1019;
  SQL_API_SQLSETPARAM = 22;
  SQL_API_SQLSETSTMTATTR = 1020;
  SQL_API_SQLSETSTMTOPTION = 51;
  SQL_API_SQLSPECIALCOLUMNS = 52;
  SQL_API_SQLSTATISTICS = 53;
  SQL_API_SQLTABLES = 54;
  SQL_API_SQLTRANSACT = 23;
  SQL_API_SQLCANCELHANDLE = 1550; //(ODBCVER >= 0x0380)
  SQL_API_SQLCOMPLETEASYNC = 1551; //(ODBCVER >= 0x0380)

{ Information requested by SQLGetInfo()  }
const
  SQL_MAX_DRIVER_CONNECTIONS = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME = 2;
  SQL_FETCH_DIRECTION = 8;
  SQL_SERVER_NAME = 13;
  SQL_SEARCH_PATTERN_ESCAPE = 14;
  SQL_DBMS_NAME = 17;
  SQL_DBMS_VER = 18;
  SQL_ACCESSIBLE_TABLES = 19;
  SQL_ACCESSIBLE_PROCEDURES = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR = 23;
  SQL_DATA_SOURCE_READ_ONLY = 25;
  SQL_DEFAULT_TXN_ISOLATION = 26;
  SQL_IDENTIFIER_CASE = 28;
  SQL_IDENTIFIER_QUOTE_CHAR = 29;
  SQL_MAX_COLUMN_NAME_LEN = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN = 35;
  SQL_SCROLL_CONCURRENCY = 43;
  SQL_TXN_CAPABLE = 46;
  SQL_TRANSACTION_CAPABLE = SQL_TXN_CAPABLE;
  SQL_USER_NAME = 47;
  SQL_TXN_ISOLATION_OPTION = 72;
  SQL_TRANSACTION_ISOLATION_OPTION = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY = 73;
  SQL_GETDATA_EXTENSIONS = 81;
  SQL_NULL_COLLATION = 85;
  SQL_ALTER_TABLE = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT = 90;
  SQL_SPECIAL_CHARACTERS = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE = 101;
  SQL_MAX_INDEX_SIZE = 102;
  SQL_MAXIMUM_INDEX_SIZE = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE = 104;
  SQL_MAXIMUM_ROW_SIZE = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES = 115;
  SQL_OUTER_JOIN_CAPABILITIES = SQL_OJ_CAPABILITIES;
  SQL_XOPEN_CLI_YEAR = 10000;
  SQL_CURSOR_SENSITIVITY = 10001;
  SQL_DESCRIBE_PARAMETER = 10002;
  SQL_CATALOG_NAME = 10003;
  SQL_COLLATION_SEQ = 10004;
  SQL_MAX_IDENTIFIER_LEN = 10005;
  SQL_MAXIMUM_IDENTIFIER_LENGTH = SQL_MAX_IDENTIFIER_LEN;

{********************************************** }
{ Extended definitions for SQLGetInfo           }
{********************************************** }
{--------------------------------- }
{ Values in ODBC 2.0 that are not  }
{ in the X/Open spec               }
{--------------------------------- }

const
  SQL_INFO_FIRST = 0;
{ MAX_DRIVER_CONNECTIONS  }
  SQL_ACTIVE_CONNECTIONS = 0;
{ MAX_CONCURRENT_ACTIVITIES  }
  SQL_ACTIVE_STATEMENTS = 1;
  SQL_DRIVER_HDBC = 3;
  SQL_DRIVER_HENV = 4;
  SQL_DRIVER_HSTMT = 5;
  SQL_DRIVER_NAME = 6;
  SQL_DRIVER_VER = 7;
  SQL_ODBC_API_CONFORMANCE = 9;
  SQL_ODBC_VER = 10;
  SQL_ROW_UPDATES = 11;
  SQL_ODBC_SAG_CLI_CONFORMANCE = 12;
  SQL_ODBC_SQL_CONFORMANCE = 15;
  SQL_PROCEDURES = 21;
  SQL_CONCAT_NULL_BEHAVIOR = 22;
  SQL_CURSOR_ROLLBACK_BEHAVIOR = 24;
  SQL_EXPRESSIONS_IN_ORDERBY = 27;
{ MAX_SCHEMA_NAME_LEN  }
  SQL_MAX_OWNER_NAME_LEN = 32;
  SQL_MAX_PROCEDURE_NAME_LEN = 33;
{ MAX_CATALOG_NAME_LEN  }
  SQL_MAX_QUALIFIER_NAME_LEN = 34;
  SQL_MULT_RESULT_SETS = 36;
  SQL_MULTIPLE_ACTIVE_TXN = 37;
  SQL_OUTER_JOINS = 38;
  SQL_OWNER_TERM = 39;
  SQL_PROCEDURE_TERM = 40;
  SQL_QUALIFIER_NAME_SEPARATOR = 41;
  SQL_QUALIFIER_TERM = 42;
  SQL_SCROLL_OPTIONS = 44;
  SQL_TABLE_TERM = 45;
  SQL_CONVERT_FUNCTIONS = 48;
  SQL_NUMERIC_FUNCTIONS = 49;
  SQL_STRING_FUNCTIONS = 50;
  SQL_SYSTEM_FUNCTIONS = 51;
  SQL_TIMEDATE_FUNCTIONS = 52;
  SQL_CONVERT_BIGINT = 53;
  SQL_CONVERT_BINARY = 54;
  SQL_CONVERT_BIT = 55;
  SQL_CONVERT_CHAR = 56;
  SQL_CONVERT_DATE = 57;
  SQL_CONVERT_DECIMAL = 58;
  SQL_CONVERT_DOUBLE = 59;
  SQL_CONVERT_FLOAT = 60;
  SQL_CONVERT_INTEGER = 61;
  SQL_CONVERT_LONGVARCHAR = 62;
  SQL_CONVERT_NUMERIC = 63;
  SQL_CONVERT_REAL = 64;
  SQL_CONVERT_SMALLINT = 65;
  SQL_CONVERT_TIME = 66;
  SQL_CONVERT_TIMESTAMP = 67;
  SQL_CONVERT_TINYINT = 68;
  SQL_CONVERT_VARBINARY = 69;
  SQL_CONVERT_VARCHAR = 70;
  SQL_CONVERT_LONGVARBINARY = 71;
{ SQL_INTEGRITY  }
  SQL_ODBC_SQL_OPT_IEF = 73;
  SQL_CORRELATION_NAME = 74;
  SQL_NON_NULLABLE_COLUMNS = 75;
  SQL_DRIVER_HLIB = 76;
  SQL_DRIVER_ODBC_VER = 77;
  SQL_LOCK_TYPES = 78;
  SQL_POS_OPERATIONS = 79;
  SQL_POSITIONED_STATEMENTS = 80;
  SQL_BOOKMARK_PERSISTENCE = 82;
  SQL_STATIC_SENSITIVITY = 83;
  SQL_FILE_USAGE = 84;
  SQL_COLUMN_ALIAS = 87;
  SQL_GROUP_BY = 88;
  SQL_KEYWORDS = 89;
  SQL_OWNER_USAGE = 91;
  SQL_QUALIFIER_USAGE = 92;
  SQL_QUOTED_IDENTIFIER_CASE = 93;
  SQL_SUBQUERIES = 95;
  SQL_UNION = 96;
  SQL_MAX_ROW_SIZE_INCLUDES_LONG = 103;
  SQL_MAX_CHAR_LITERAL_LEN = 108;
  SQL_TIMEDATE_ADD_INTERVALS = 109;
  SQL_TIMEDATE_DIFF_INTERVALS = 110;
  SQL_NEED_LONG_DATA_LEN = 111;
  SQL_MAX_BINARY_LITERAL_LEN = 112;
  SQL_LIKE_ESCAPE_CLAUSE = 113;
  SQL_QUALIFIER_LOCATION = 114;

{----------------------------------------------- }
{ ODBC 3.0 SQLGetInfo values that are not part   }
{ of the X/Open standard at this time.   X/Open  }
{ standard values are in sql.h.                  }
{----------------------------------------------- }
const
  SQL_ACTIVE_ENVIRONMENTS = 116;
  SQL_ALTER_DOMAIN = 117;
  SQL_SQL_CONFORMANCE = 118;
  SQL_DATETIME_LITERALS = 119;
{ new X/Open spec  }
  SQL_ASYNC_MODE = 10021;
  SQL_BATCH_ROW_COUNT = 120;
  SQL_BATCH_SUPPORT = 121;
  SQL_CATALOG_LOCATION = SQL_QUALIFIER_LOCATION;
  SQL_CATALOG_NAME_SEPARATOR = SQL_QUALIFIER_NAME_SEPARATOR;
  SQL_CATALOG_TERM = SQL_QUALIFIER_TERM;
  SQL_CATALOG_USAGE = SQL_QUALIFIER_USAGE;
  SQL_CONVERT_WCHAR = 122;
  SQL_CONVERT_INTERVAL_DAY_TIME = 123;
  SQL_CONVERT_INTERVAL_YEAR_MONTH = 124;
  SQL_CONVERT_WLONGVARCHAR = 125;
  SQL_CONVERT_WVARCHAR = 126;
  SQL_CREATE_ASSERTION = 127;
  SQL_CREATE_CHARACTER_SET = 128;
  SQL_CREATE_COLLATION = 129;
  SQL_CREATE_DOMAIN = 130;
  SQL_CREATE_SCHEMA = 131;
  SQL_CREATE_TABLE = 132;
  SQL_CREATE_TRANSLATION = 133;
  SQL_CREATE_VIEW = 134;
  SQL_DRIVER_HDESC = 135;
  SQL_DROP_ASSERTION = 136;
  SQL_DROP_CHARACTER_SET = 137;
  SQL_DROP_COLLATION = 138;
  SQL_DROP_DOMAIN = 139;
  SQL_DROP_SCHEMA = 140;
  SQL_DROP_TABLE = 141;
  SQL_DROP_TRANSLATION = 142;
  SQL_DROP_VIEW = 143;
  SQL_DYNAMIC_CURSOR_ATTRIBUTES1 = 144;
  SQL_DYNAMIC_CURSOR_ATTRIBUTES2 = 145;
  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 = 146;
  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 = 147;
  SQL_INDEX_KEYWORDS = 148;
  SQL_INFO_SCHEMA_VIEWS = 149;
  SQL_KEYSET_CURSOR_ATTRIBUTES1 = 150;
  SQL_KEYSET_CURSOR_ATTRIBUTES2 = 151;
{ new X/Open spec  }
  SQL_MAX_ASYNC_CONCURRENT_STATEMENTS = 10022;
  SQL_ODBC_INTERFACE_CONFORMANCE = 152;
  SQL_PARAM_ARRAY_ROW_COUNTS = 153;
  SQL_PARAM_ARRAY_SELECTS = 154;
  SQL_SCHEMA_TERM = SQL_OWNER_TERM;
  SQL_SCHEMA_USAGE = SQL_OWNER_USAGE;
  SQL_SQL92_DATETIME_FUNCTIONS = 155;
  SQL_SQL92_FOREIGN_KEY_DELETE_RULE = 156;
  SQL_SQL92_FOREIGN_KEY_UPDATE_RULE = 157;
  SQL_SQL92_GRANT = 158;
  SQL_SQL92_NUMERIC_VALUE_FUNCTIONS = 159;
  SQL_SQL92_PREDICATES = 160;
  SQL_SQL92_RELATIONAL_JOIN_OPERATORS = 161;
  SQL_SQL92_REVOKE = 162;
  SQL_SQL92_ROW_VALUE_CONSTRUCTOR = 163;
  SQL_SQL92_STRING_FUNCTIONS = 164;
  SQL_SQL92_VALUE_EXPRESSIONS = 165;
  SQL_STANDARD_CLI_CONFORMANCE = 166;
  SQL_STATIC_CURSOR_ATTRIBUTES1 = 167;
  SQL_STATIC_CURSOR_ATTRIBUTES2 = 168;
  SQL_AGGREGATE_FUNCTIONS = 169;
  SQL_DDL_INDEX = 170;
  SQL_DM_VER = 171;
  SQL_INSERT_STATEMENT = 172;
  SQL_CONVERT_GUID = 173;
  SQL_UNION_STATEMENT = SQL_UNION;
{ Info Types }
  SQL_ASYNC_DBC_FUNCTIONS = 10023; //(ODBCVER >= 0x0380)
  SQL_DRIVER_AWARE_POOLING_SUPPORTED = 10024;
  SQL_ASYNC_NOTIFICATION = 10025; //(ODBCVER >= 0x0380)
{ Possible values for SQL_ASYNC_NOTIFICATION }
  SQL_ASYNC_NOTIFICATION_NOT_CAPABLE = $00000000; //(ODBCVER >= 0x0380)
  SQL_ASYNC_NOTIFICATION_CAPABLE = $00000001; //(ODBCVER >= 0x0380)
  SQL_DTC_TRANSITION_COST = 1750;

{ SQL_ALTER_TABLE bitmasks  }
const
  SQL_AT_ADD_COLUMN = $00000001;
  SQL_AT_DROP_COLUMN = $00000002;

const
  SQL_AT_ADD_CONSTRAINT = $00000008;
{ The following bitmasks are ODBC extensions and defined in sqlext.h
*#define    SQL_AT_COLUMN_SINGLE                    0x00000020L
*#define    SQL_AT_ADD_COLUMN_DEFAULT               0x00000040L
*#define    SQL_AT_ADD_COLUMN_COLLATION             0x00000080L
*#define    SQL_AT_SET_COLUMN_DEFAULT               0x00000100L
*#define    SQL_AT_DROP_COLUMN_DEFAULT              0x00000200L
*#define    SQL_AT_DROP_COLUMN_CASCADE              0x00000400L
*#define    SQL_AT_DROP_COLUMN_RESTRICT             0x00000800L
*#define SQL_AT_ADD_TABLE_CONSTRAINT                0x00001000L
*#define SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE       0x00002000L
*#define SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT      0x00004000L
*#define SQL_AT_CONSTRAINT_NAME_DEFINITION          0x00008000L
*#define SQL_AT_CONSTRAINT_INITIALLY_DEFERRED       0x00010000L
*#define SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE      0x00020000L
*#define SQL_AT_CONSTRAINT_DEFERRABLE               0x00040000L
*#define SQL_AT_CONSTRAINT_NON_DEFERRABLE           0x00080000L
 }

{ SQL_ASYNC_MODE values  }
const
  SQL_AM_NONE = 0;
  SQL_AM_CONNECTION = 1;
  SQL_AM_STATEMENT = 2;

{ SQL_CURSOR_COMMIT_BEHAVIOR values  }
const
  SQL_CB_DELETE = 0;
  SQL_CB_CLOSE = 1;
  SQL_CB_PRESERVE = 2;

{ SQL_FETCH_DIRECTION bitmasks  }
  SQL_FD_FETCH_NEXT = $00000001;
  SQL_FD_FETCH_FIRST = $00000002;
  SQL_FD_FETCH_LAST = $00000004;
  SQL_FD_FETCH_PRIOR = $00000008;
  SQL_FD_FETCH_ABSOLUTE = $00000010;
  SQL_FD_FETCH_RELATIVE = $00000020;

{ SQL_GETDATA_EXTENSIONS bitmasks  }
  SQL_GD_ANY_COLUMN = $00000001;
  SQL_GD_ANY_ORDER = $00000002;

{ SQL_IDENTIFIER_CASE values  }
  SQL_IC_UPPER = 1;
  SQL_IC_LOWER = 2;
  SQL_IC_SENSITIVE = 3;
  SQL_IC_MIXED = 4;

{ SQL_OJ_CAPABILITIES bitmasks  }
{ NB: this means 'outer join', not what  you may be thinking  }
const
  SQL_OJ_LEFT = $00000001;
  SQL_OJ_RIGHT = $00000002;
  SQL_OJ_FULL = $00000004;
  SQL_OJ_NESTED = $00000008;
  SQL_OJ_NOT_ORDERED = $00000010;
  SQL_OJ_INNER = $00000020;
  SQL_OJ_ALL_COMPARISON_OPS = $00000040;

{ SQL_SCROLL_CONCURRENCY bitmasks  }
const
  SQL_SCCO_READ_ONLY = $00000001;
  SQL_SCCO_LOCK = $00000002;
  SQL_SCCO_OPT_ROWVER = $00000004;
  SQL_SCCO_OPT_VALUES = $00000008;

{ SQL_TXN_CAPABLE values  }
  SQL_TC_NONE = 0;
  SQL_TC_DML = 1;
  SQL_TC_ALL = 2;
  SQL_TC_DDL_COMMIT = 3;
  SQL_TC_DDL_IGNORE = 4;

{ SQL_TXN_ISOLATION_OPTION bitmasks  }
  SQL_TXN_READ_UNCOMMITTED = $00000001;
  SQL_TRANSACTION_READ_UNCOMMITTED = SQL_TXN_READ_UNCOMMITTED;
  SQL_TXN_READ_COMMITTED = $00000002;
  SQL_TRANSACTION_READ_COMMITTED = SQL_TXN_READ_COMMITTED;
  SQL_TXN_REPEATABLE_READ = $00000004;
  SQL_TRANSACTION_REPEATABLE_READ = SQL_TXN_REPEATABLE_READ;
  SQL_TXN_SERIALIZABLE = $00000008;
  SQL_TRANSACTION_SERIALIZABLE = SQL_TXN_SERIALIZABLE;

{ SQL_NULL_COLLATION values  }
  SQL_NC_HIGH = 0;
  SQL_NC_LOW = 1;

//sqlext.h constants
{ maximum data source name size  }
const
  SQL_MAX_DSN_LENGTH = 32;
  SQL_MAX_OPTION_STRING_LENGTH = 256;

{ return code SQL_NO_DATA_FOUND is the same as SQL_NO_DATA  }
const
  SQL_NO_DATA_FOUND = SQL_NO_DATA;

{ an end handle type  }
const
  SQL_HANDLE_SENV = 5;

{ env attribute  }
const
  SQL_ATTR_ODBC_VERSION = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH = 202;

{ values for SQL_ATTR_CONNECTION_POOLING  }
const
  SQL_CP_OFF = 0;
  SQL_CP_ONE_PER_DRIVER = 1;
  SQL_CP_ONE_PER_HENV = 2;
  SQL_CP_DRIVER_AWARE = 3;
  SQL_CP_DEFAULT = SQL_CP_OFF;
{ values for SQL_ATTR_CP_MATCH  }
  SQL_CP_STRICT_MATCH = 0;
  SQL_CP_RELAXED_MATCH = 1;
  SQL_CP_MATCH_DEFAULT = SQL_CP_STRICT_MATCH;
{ values for SQL_ATTR_ODBC_VERSION  }
  SQL_OV_ODBC3 = SQLPOINTER(3);

{ new values for SQL_ATTR_ODBC_VERSION  }
{ From ODBC 3.8 onwards, we should use <major version> * 100 + <minor version> }
const
  SQL_OV_ODBC3_80 = SQLPOINTER(380); //(ODBCVER >= 0x0380)

{ connection attributes with new names  }
const
  SQL_ATTR_ACCESS_MODE = 101;
  SQL_ATTR_AUTOCOMMIT = 102;
  SQL_ATTR_CONNECTION_TIMEOUT = 113;
  SQL_ATTR_CURRENT_CATALOG = 109;
  SQL_ATTR_DISCONNECT_BEHAVIOR = 114;
  SQL_ATTR_ENLIST_IN_DTC = 1207;
  SQL_ATTR_ENLIST_IN_XA = 1208;
  SQL_ATTR_LOGIN_TIMEOUT = 103;
  SQL_ATTR_ODBC_CURSORS = 110;
  SQL_ATTR_PACKET_SIZE = 112;
  SQL_ATTR_QUIET_MODE = 111;
  SQL_ATTR_TRACE = 104;
  SQL_ATTR_TRACEFILE = 105;
  SQL_ATTR_TRANSLATE_LIB = 106;
  SQL_ATTR_TRANSLATE_OPTION = 107;
  SQL_ATTR_TXN_ISOLATION = 108;

{ GetConnectAttr only  }
const
  SQL_ATTR_CONNECTION_DEAD = 1209;
{  ODBC Driver Manager sets this connection attribute to a unicode driver
    (which supports SQLConnectW) when the application is an ANSI application
    (which calls SQLConnect, SQLDriverConnect, or SQLBrowseConnect).
    This is SetConnectAttr only and application does not set this attribute
    This attribute was introduced because some unicode driver's some APIs may
    need to behave differently on ANSI or Unicode applications. A unicode
    driver, which  has same behavior for both ANSI or Unicode applications,
    should return SQL_ERROR when the driver manager sets this connection
    attribute. When a unicode driver returns SQL_SUCCESS on this attribute,
    the driver manager treates ANSI and Unicode connections differently in
    connection pooling.
 }
const
  SQL_ATTR_ANSI_APP = 115; //(ODBCVER >= 0x0351)
  SQL_ATTR_RESET_CONNECTION = 116; //(ODBCVER >= 0x0380)
  SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE = 117; //(ODBCVER >= 0x0380)
  SQL_ATTR_DBC_INFO_TOKEN = 118; // reset the pooled connection in case it is not a perfect match
  SQL_ATTR_ASYNC_DBC_EVENT = 119; //(ODBCVER >= 0x0380)
  { Async Notification }
  SQL_ATTR_ASYNC_DBC_NOTIFICATION_CALLBACK = 120;
  SQL_ATTR_ASYNC_DBC_NOTIFICATION_CONTEXT = 121;


{ SQL_ACCESS_MODE options  }
const
  SQL_MODE_READ_WRITE = SQLPOINTER(0);
  SQL_MODE_READ_ONLY = SQLPOINTER(1);
  SQL_MODE_DEFAULT = SQL_MODE_READ_WRITE;

{ SQL_AUTOCOMMIT options  }
  SQL_AUTOCOMMIT_OFF = SQLPOINTER(0);
  SQL_AUTOCOMMIT_ON = SQLPOINTER(1);
  SQL_AUTOCOMMIT_DEFAULT = SQL_AUTOCOMMIT_ON;

{ SQL_LOGIN_TIMEOUT options  }
  SQL_LOGIN_TIMEOUT_DEFAULT = 15;

{ SQL_OPT_TRACE options  }
  SQL_OPT_TRACE_OFF = 0;
  SQL_OPT_TRACE_ON = 1;
  SQL_OPT_TRACE_DEFAULT = SQL_OPT_TRACE_OFF;
  SQL_OPT_TRACE_FILE_DEFAULT = '\\SQL.LOG';

{ SQL_ODBC_CURSORS options  }
const
  SQL_CUR_USE_DRIVER = 2;
  SQL_CUR_DEFAULT = SQL_CUR_USE_DRIVER;

{ values for SQL_ATTR_DISCONNECT_BEHAVIOR  }
const
  SQL_DB_RETURN_TO_POOL = 0;
  SQL_DB_DISCONNECT = 1;
  SQL_DB_DEFAULT = SQL_DB_RETURN_TO_POOL;
{ values for SQL_ATTR_ENLIST_IN_DTC  }
  SQL_DTC_DONE = 0;

{ values for SQL_ATTR_CONNECTION_DEAD  }
{ Connection is closed/dead  }
const
  SQL_CD_TRUE = 1;
{ Connection is open/available  }
  SQL_CD_FALSE = 0;

{ values for SQL_ATTR_ANSI_APP  }
const
{ the application is an ANSI app  }
  SQL_AA_TRUE = 1; //(ODBCVER >= 0x0351)
{ the application is a Unicode app  }
  SQL_AA_FALSE = 0; //(ODBCVER >= 0x0351)

{ values for SQL_ATTR_RESET_CONNECTION  }
const
  SQL_RESET_CONNECTION_YES = 1; //(ODBCVER >= 0x0380)

{ values for SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE  }
const
  SQL_ASYNC_DBC_ENABLE_ON = 1; //(ODBCVER >= 0x0380)
  SQL_ASYNC_DBC_ENABLE_OFF = 0; //(ODBCVER >= 0x0380)
  SQL_ASYNC_DBC_ENABLE_DEFAULT = SQL_ASYNC_DBC_ENABLE_OFF; //(ODBCVER >= 0x0380)

{ statement attributes  }
const
  SQL_QUERY_TIMEOUT = 0;
  SQL_MAX_ROWS = 1;
  SQL_NOSCAN = 2;
  SQL_MAX_LENGTH = 3;
  SQL_ASYNC_ENABLE = 4;
  SQL_BIND_TYPE = 5;
  SQL_CURSOR_TYPE = 6;
  SQL_CONCURRENCY = 7;
  SQL_KEYSET_SIZE = 8;
  SQL_ROWSET_SIZE = 9;
  SQL_SIMULATE_CURSOR = 10;
  SQL_RETRIEVE_DATA = 11;
  SQL_USE_BOOKMARKS = 12;
{      GetStmtOption Only  }
  SQL_GET_BOOKMARK = 13;
{      GetStmtOption Only  }
  SQL_ROW_NUMBER = 14;

{ statement attributes for ODBC 3.0  }
const
  SQL_ATTR_ASYNC_ENABLE = SQL_ASYNC_ENABLE;
  SQL_ATTR_CONCURRENCY = SQL_CONCURRENCY;
  SQL_ATTR_CURSOR_TYPE = SQL_CURSOR_TYPE;
  SQL_ATTR_ENABLE_AUTO_IPD = 15;
  SQL_ATTR_FETCH_BOOKMARK_PTR = 16;
  SQL_ATTR_KEYSET_SIZE = SQL_KEYSET_SIZE;
  SQL_ATTR_MAX_LENGTH = SQL_MAX_LENGTH;
  SQL_ATTR_MAX_ROWS = SQL_MAX_ROWS;
  SQL_ATTR_NOSCAN = SQL_NOSCAN;
  SQL_ATTR_PARAM_BIND_OFFSET_PTR = 17;
  SQL_ATTR_PARAM_BIND_TYPE = 18;
  SQL_ATTR_PARAM_OPERATION_PTR = 19;
  SQL_ATTR_PARAM_STATUS_PTR = 20;
  SQL_ATTR_PARAMS_PROCESSED_PTR = 21;
  SQL_ATTR_PARAMSET_SIZE = 22;
  SQL_ATTR_QUERY_TIMEOUT = SQL_QUERY_TIMEOUT;
  SQL_ATTR_RETRIEVE_DATA = SQL_RETRIEVE_DATA;
  SQL_ATTR_ROW_BIND_OFFSET_PTR = 23;
  SQL_ATTR_ROW_BIND_TYPE = SQL_BIND_TYPE;
{GetStmtAttr }
  SQL_ATTR_ROW_NUMBER = SQL_ROW_NUMBER;
  SQL_ATTR_ROW_OPERATION_PTR = 24;
  SQL_ATTR_ROW_STATUS_PTR = 25;
  SQL_ATTR_ROWS_FETCHED_PTR = 26;
  SQL_ATTR_ROW_ARRAY_SIZE = 27;
  SQL_ATTR_SIMULATE_CURSOR = SQL_SIMULATE_CURSOR;
  SQL_ATTR_USE_BOOKMARKS = SQL_USE_BOOKMARKS;
  SQL_ATTR_ASYNC_STMT_EVENT = 29; //ODBCVER >= 0x0380

{ define for SQL_DIAG_ROW_NUMBER and SQL_DIAG_COLUMN_NUMBER  }
const
  SQL_NO_ROW_NUMBER = -(1);
  SQL_NO_COLUMN_NUMBER = -(1);
  SQL_ROW_NUMBER_UNKNOWN = -(2);
  SQL_COLUMN_NUMBER_UNKNOWN = -(2);

{ SQLBindParameter extensions  }
const
  SQL_DEFAULT_PARAM = -(5);
  SQL_IGNORE = -(6);
  SQL_COLUMN_IGNORE = SQL_IGNORE;

const
  SQL_LEN_DATA_AT_EXEC_OFFSET = -(100);

{ binary length for driver specific attributes  }
const
  SQL_LEN_BINARY_ATTR_OFFSET = -(100);

const
  SQL_FETCH_BOOKMARK = 8;
{ SQLExtendedFetch "rgfRowStatus" element values  }
  SQL_ROW_SUCCESS = 0;
  SQL_ROW_DELETED = 1;
  SQL_ROW_UPDATED = 2;
  SQL_ROW_NOROW = 3;
  SQL_ROW_ADDED = 4;
  SQL_ROW_ERROR = 5;
  SQL_ROW_SUCCESS_WITH_INFO = 6;
  SQL_ROW_PROCEED = 0;
  SQL_ROW_IGNORE = 1;

{ value for SQL_DESC_ARRAY_STATUS_PTR  }
const
  SQL_PARAM_SUCCESS = 0;
  SQL_PARAM_SUCCESS_WITH_INFO = 6;
  SQL_PARAM_ERROR = 5;
  SQL_PARAM_UNUSED = 7;
  SQL_PARAM_DIAG_UNAVAILABLE = 1;
  SQL_PARAM_PROCEED = 0;
  SQL_PARAM_IGNORE = 1;

{ Defines for SQLForeignKeys (UPDATE_RULE and DELETE_RULE)  }
const
  SQL_CASCADE = 0;
  SQL_RESTRICT = 1;
  SQL_SET_NULL = 2;
  SQL_NO_ACTION = 3;
  SQL_SET_DEFAULT = 4;

{ Note that the following are in a different column of SQLForeignKeys than  }
{ the previous #defines.   These are for DEFERRABILITY.                     }
const
  SQL_INITIALLY_DEFERRED = 5;
  SQL_INITIALLY_IMMEDIATE = 6;
  SQL_NOT_DEFERRABLE = 7;

{ Defines for SQLBindParameter and SQLProcedureColumns (returned in the result set)  }
const
  SQL_PARAM_TYPE_UNKNOWN = 0;
  SQL_PARAM_INPUT = 1;
  SQL_PARAM_INPUT_OUTPUT = 2;
  SQL_RESULT_COL = 3;
  SQL_PARAM_OUTPUT = 4;
  SQL_RETURN_VALUE = 5;
  SQL_PARAM_INPUT_OUTPUT_STREAM = 8; //(ODBCVER >= 0x0380)
  SQL_PARAM_OUTPUT_STREAM = 16; //(ODBCVER >= 0x0380)

{ Defines for SQLProcedures (returned in the result set)  }
const
  SQL_PT_UNKNOWN = 0;
  SQL_PT_PROCEDURE = 1;
  SQL_PT_FUNCTION = 2;

{ Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter }
const
  SQL_PARAM_TYPE_DEFAULT = SQL_PARAM_INPUT_OUTPUT;
  SQL_SETPARAM_VALUE_MAX = -(1);

{ SQLColAttributes defines  }
  SQL_COLUMN_COUNT = 0;
  SQL_COLUMN_NAME = 1;
  SQL_COLUMN_TYPE = 2;
  SQL_COLUMN_LENGTH = 3;
  SQL_COLUMN_PRECISION = 4;
  SQL_COLUMN_SCALE = 5;
  SQL_COLUMN_DISPLAY_SIZE = 6;
  SQL_COLUMN_NULLABLE = 7;
  SQL_COLUMN_UNSIGNED = 8;
  SQL_COLUMN_MONEY = 9;
  SQL_COLUMN_UPDATABLE = 10;
  SQL_COLUMN_AUTO_INCREMENT = 11;
  SQL_COLUMN_CASE_SENSITIVE = 12;
  SQL_COLUMN_SEARCHABLE = 13;
  SQL_COLUMN_TYPE_NAME = 14;
  SQL_COLUMN_TABLE_NAME = 15;
  SQL_COLUMN_OWNER_NAME = 16;
  SQL_COLUMN_QUALIFIER_NAME = 17;
  SQL_COLUMN_LABEL = 18;
  SQL_COLATT_OPT_MAX = SQL_COLUMN_LABEL;
  SQL_COLATT_OPT_MIN = SQL_COLUMN_COUNT;

{ SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE  }
  SQL_ATTR_READONLY = 0;
  SQL_ATTR_WRITE = 1;
  SQL_ATTR_READWRITE_UNKNOWN = 2;

{ SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE  }
{ These are also used by SQLGetInfo                      }
  SQL_UNSEARCHABLE = 0;
  SQL_LIKE_ONLY = 1;
  SQL_ALL_EXCEPT_LIKE = 2;
  SQL_SEARCHABLE = 3;
  SQL_PRED_SEARCHABLE = SQL_SEARCHABLE;

{ Special return values for SQLGetData  }
  SQL_NO_TOTAL = -(4);

/// SQLColAttributes driver specific defines.
// SQLSetDescField/SQLGetDescField driver specific defines.
// Microsoft has 1200 thru 1249 reserved for Microsoft SQL Server Native Client driver usage.
  SQL_CA_SS_BASE = 1200;
  SQL_CA_SS_COLUMN_SSTYPE = (SQL_CA_SS_BASE+0);   //  dbcoltype/dbalttype
  SQL_CA_SS_COLUMN_UTYPE = (SQL_CA_SS_BASE+1);   //  dbcolutype/dbaltutype
  SQL_CA_SS_NUM_ORDERS = (SQL_CA_SS_BASE+2);   //  dbnumorders
  SQL_CA_SS_COLUMN_ORDER = (SQL_CA_SS_BASE+3);   //  dbordercol
  SQL_CA_SS_COLUMN_VARYLEN = (SQL_CA_SS_BASE+4);   //  dbvarylen
  SQL_CA_SS_NUM_COMPUTES = (SQL_CA_SS_BASE+5);   //  dbnumcompute
  SQL_CA_SS_COMPUTE_ID = (SQL_CA_SS_BASE+6);   //  dbnextrow status return
  SQL_CA_SS_COMPUTE_BYLIST = (SQL_CA_SS_BASE+7);   //  dbbylist
  SQL_CA_SS_COLUMN_ID = (SQL_CA_SS_BASE+8);   //  dbaltcolid
  SQL_CA_SS_COLUMN_OP = (SQL_CA_SS_BASE+9);   //  dbaltop
  SQL_CA_SS_COLUMN_SIZE = (SQL_CA_SS_BASE+10);  //  dbcollen
  SQL_CA_SS_COLUMN_HIDDEN = (SQL_CA_SS_BASE+11);  //  Column is hidden (FOR BROWSE)
  SQL_CA_SS_COLUMN_KEY = (SQL_CA_SS_BASE+12);  //  Column is key column (FOR BROWSE)
//  SQL_DESC_BASE_COLUMN_NAME_OLD = (SQL_CA_SS_BASE+13);  //  This is defined at another location.
  SQL_CA_SS_COLUMN_COLLATION = (SQL_CA_SS_BASE+14);  //  Column collation (only for chars)
  SQL_CA_SS_VARIANT_TYPE = (SQL_CA_SS_BASE+15);
  SQL_CA_SS_VARIANT_SQL_TYPE = (SQL_CA_SS_BASE+16);
  SQL_CA_SS_VARIANT_SERVER_TYPE = (SQL_CA_SS_BASE+17);

// XML, CLR UDT, and table valued parameter related metadata
  SQL_CA_SS_UDT_CATALOG_NAME = (SQL_CA_SS_BASE+18); //  UDT catalog name
  SQL_CA_SS_UDT_SCHEMA_NAME = (SQL_CA_SS_BASE+19); //  UDT schema name
  SQL_CA_SS_UDT_TYPE_NAME = (SQL_CA_SS_BASE+20); //  UDT type name
  SQL_CA_SS_UDT_ASSEMBLY_TYPE_NAME = (SQL_CA_SS_BASE+21); //  Qualified name of the assembly containing the UDT class
  SQL_CA_SS_XML_SCHEMACOLLECTION_CATALOG_NAME = (SQL_CA_SS_BASE+22); //  Name of the catalog that contains XML Schema collection
  SQL_CA_SS_XML_SCHEMACOLLECTION_SCHEMA_NAME = (SQL_CA_SS_BASE+23); //  Name of the schema that contains XML Schema collection
  SQL_CA_SS_XML_SCHEMACOLLECTION_NAME = (SQL_CA_SS_BASE+24); //  Name of the XML Schema collection
  SQL_CA_SS_CATALOG_NAME = (SQL_CA_SS_BASE+25); //  Catalog name
  SQL_CA_SS_SCHEMA_NAME = (SQL_CA_SS_BASE+26); //  Schema name
  SQL_CA_SS_TYPE_NAME = (SQL_CA_SS_BASE+27); //  Type name

{ New defines for SEARCHABLE column in SQLGetTypeInfo  }
const
  SQL_COL_PRED_CHAR = SQL_LIKE_ONLY;
  SQL_COL_PRED_BASIC = SQL_ALL_EXCEPT_LIKE;

{ whether an attribute is a pointer or not  }
const
  SQL_IS_POINTER = -(4);
  SQL_IS_UINTEGER = -(5);
  SQL_IS_INTEGER = -(6);
  SQL_IS_USMALLINT = -(7);
  SQL_IS_SMALLINT = -(8);

{ the value of SQL_ATTR_PARAM_BIND_TYPE  }
const
  SQL_PARAM_BIND_BY_COLUMN = 0;
  SQL_PARAM_BIND_TYPE_DEFAULT = SQL_PARAM_BIND_BY_COLUMN;

{ SQL_QUERY_TIMEOUT options  }
const
  SQL_QUERY_TIMEOUT_DEFAULT = 0;
{ SQL_MAX_ROWS options  }
  SQL_MAX_ROWS_DEFAULT = 0;
{ SQL_NOSCAN options  }
{      1.0 FALSE  }
  SQL_NOSCAN_OFF = 0;
{      1.0 TRUE  }
  SQL_NOSCAN_ON = 1;
  SQL_NOSCAN_DEFAULT = SQL_NOSCAN_OFF;
{ SQL_MAX_LENGTH options  }
  SQL_MAX_LENGTH_DEFAULT = 0;
{ values for SQL_ATTR_ASYNC_ENABLE  }
  SQL_ASYNC_ENABLE_OFF = 0;
  SQL_ASYNC_ENABLE_ON = 1;
  SQL_ASYNC_ENABLE_DEFAULT = SQL_ASYNC_ENABLE_OFF;
{ SQL_BIND_TYPE options  }
  SQL_BIND_BY_COLUMN = 0;
{ Default value  }
  SQL_BIND_TYPE_DEFAULT = SQL_BIND_BY_COLUMN;
{ SQL_CONCURRENCY options  }
  SQL_CONCUR_READ_ONLY = 1;
  SQL_CONCUR_LOCK = 2;
  SQL_CONCUR_ROWVER = 3;
  SQL_CONCUR_VALUES = 4;
{ Default value  }
  SQL_CONCUR_DEFAULT = SQL_CONCUR_READ_ONLY;
{ SQL_CURSOR_TYPE options  }
  SQL_CURSOR_FORWARD_ONLY = 0;
  SQL_CURSOR_KEYSET_DRIVEN = 1;
  SQL_CURSOR_DYNAMIC = 2;
  SQL_CURSOR_STATIC = 3;
{ Default value  }
  SQL_CURSOR_TYPE_DEFAULT = SQL_CURSOR_FORWARD_ONLY;
{ SQL_ROWSET_SIZE options  }
  SQL_ROWSET_SIZE_DEFAULT = 1;
{ SQL_KEYSET_SIZE options  }
  SQL_KEYSET_SIZE_DEFAULT = 0;
{ SQL_SIMULATE_CURSOR options  }
  SQL_SC_NON_UNIQUE = 0;
  SQL_SC_TRY_UNIQUE = 1;
  SQL_SC_UNIQUE = 2;
{ SQL_RETRIEVE_DATA options  }
  SQL_RD_OFF = 0;
  SQL_RD_ON = 1;
  SQL_RD_DEFAULT = SQL_RD_ON;
{ SQL_USE_BOOKMARKS options  }
  SQL_UB_OFF = 0;
  SQL_UB_ON = 01;
  SQL_UB_DEFAULT = SQL_UB_OFF;
{ New values for SQL_USE_BOOKMARKS attribute  }
  SQL_UB_FIXED = SQL_UB_ON;
  SQL_UB_VARIABLE = 2;

{ extended descriptor field  }
const
  SQL_DESC_ARRAY_SIZE = 20;
  SQL_DESC_ARRAY_STATUS_PTR = 21;
  SQL_DESC_AUTO_UNIQUE_VALUE = SQL_COLUMN_AUTO_INCREMENT;
  SQL_DESC_BASE_COLUMN_NAME = 22;
  SQL_DESC_BASE_TABLE_NAME = 23;
  SQL_DESC_BIND_OFFSET_PTR = 24;
  SQL_DESC_BIND_TYPE = 25;
  SQL_DESC_CASE_SENSITIVE = SQL_COLUMN_CASE_SENSITIVE;
  SQL_DESC_CATALOG_NAME = SQL_COLUMN_QUALIFIER_NAME;
  SQL_DESC_CONCISE_TYPE = SQL_COLUMN_TYPE;
  SQL_DESC_DATETIME_INTERVAL_PRECISION = 26;
  SQL_DESC_DISPLAY_SIZE = SQL_COLUMN_DISPLAY_SIZE;
  SQL_DESC_FIXED_PREC_SCALE = SQL_COLUMN_MONEY;
  SQL_DESC_LABEL = SQL_COLUMN_LABEL;
  SQL_DESC_LITERAL_PREFIX = 27;
  SQL_DESC_LITERAL_SUFFIX = 28;
  SQL_DESC_LOCAL_TYPE_NAME = 29;
  SQL_DESC_MAXIMUM_SCALE = 30;
  SQL_DESC_MINIMUM_SCALE = 31;
  SQL_DESC_NUM_PREC_RADIX = 32;
  SQL_DESC_PARAMETER_TYPE = 33;
  SQL_DESC_ROWS_PROCESSED_PTR = 34;
  SQL_DESC_ROWVER = 35; //(ODBCVER >= 0x0350)
  SQL_DESC_SCHEMA_NAME = SQL_COLUMN_OWNER_NAME;
  SQL_DESC_SEARCHABLE = SQL_COLUMN_SEARCHABLE;
  SQL_DESC_TYPE_NAME = SQL_COLUMN_TYPE_NAME;
  SQL_DESC_TABLE_NAME = SQL_COLUMN_TABLE_NAME;
  SQL_DESC_UNSIGNED = SQL_COLUMN_UNSIGNED;
  SQL_DESC_UPDATABLE = SQL_COLUMN_UPDATABLE;

{ defines for diagnostics fields  }
const
  SQL_DIAG_CURSOR_ROW_COUNT = -(1249);
  SQL_DIAG_ROW_NUMBER = -(1248);
  SQL_DIAG_COLUMN_NUMBER = -(1247);

{ SQL_ODBC_API_CONFORMANCE values  }
const
  SQL_OAC_NONE = $0000;
  SQL_OAC_LEVEL1 = $0001;
  SQL_OAC_LEVEL2 = $0002;

{ SQL_ODBC_SAG_CLI_CONFORMANCE values  }
  SQL_OSCC_NOT_COMPLIANT = $0000;
  SQL_OSCC_COMPLIANT = $0001;

{ SQL_ODBC_SQL_CONFORMANCE values  }
  SQL_OSC_MINIMUM = $0000;
  SQL_OSC_CORE = $0001;
  SQL_OSC_EXTENDED = $0002;

{ SQL_CONCAT_NULL_BEHAVIOR values  }
  SQL_CB_NULL = $0000;
  SQL_CB_NON_NULL = $0001;

{ SQL_SCROLL_OPTIONS masks  }
  SQL_SO_FORWARD_ONLY = $00000001;
  SQL_SO_KEYSET_DRIVEN = $00000002;
  SQL_SO_DYNAMIC = $00000004;
  SQL_SO_MIXED = $00000008;
  SQL_SO_STATIC = $00000010;

{ SQL_FETCH_DIRECTION masks  }
  SQL_FD_FETCH_BOOKMARK = $00000080;

{ SQL_CORRELATION_NAME values  }
  SQL_CN_NONE = $0000;
  SQL_CN_DIFFERENT = $0001;
  SQL_CN_ANY = $0002;

{ SQL_NON_NULLABLE_COLUMNS values  }
  SQL_NNC_NULL = $0000;
  SQL_NNC_NON_NULL = $0001;

{ SQL_NULL_COLLATION values  }
  SQL_NC_START = $0002;
  SQL_NC_END = $0004;

{ SQL_FILE_USAGE values  }
  SQL_FILE_NOT_SUPPORTED = $0000;
  SQL_FILE_TABLE = $0001;
  SQL_FILE_QUALIFIER = $0002;
  SQL_FILE_CATALOG = SQL_FILE_QUALIFIER;      { ODBC 3.0 }

{ SQL_GETDATA_EXTENSIONS values  }
  SQL_GD_BLOCK = $00000004;
  SQL_GD_BOUND = $00000008;
  SQL_GD_OUTPUT_PARAMS = $00000010; //(ODBCVER >= 0x0380)

{ SQL_POSITIONED_STATEMENTS masks  }
const
  SQL_PS_POSITIONED_DELETE = $00000001;
  SQL_PS_POSITIONED_UPDATE = $00000002;
  SQL_PS_SELECT_FOR_UPDATE = $00000004;

{ SQL_GROUP_BY values  }
  SQL_GB_NOT_SUPPORTED = $0000;
  SQL_GB_GROUP_BY_EQUALS_SELECT = $0001;
  SQL_GB_GROUP_BY_CONTAINS_SELECT = $0002;
  SQL_GB_NO_RELATION = $0003;
  SQL_GB_COLLATE = $0004;

{ SQL_OWNER_USAGE masks  }
const
  SQL_OU_DML_STATEMENTS = $00000001;
  SQL_OU_PROCEDURE_INVOCATION = $00000002;
  SQL_OU_TABLE_DEFINITION = $00000004;
  SQL_OU_INDEX_DEFINITION = $00000008;
  SQL_OU_PRIVILEGE_DEFINITION = $00000010;

{ SQL_SCHEMA_USAGE masks  }
const
  SQL_SU_DML_STATEMENTS = SQL_OU_DML_STATEMENTS;
  SQL_SU_PROCEDURE_INVOCATION = SQL_OU_PROCEDURE_INVOCATION;
  SQL_SU_TABLE_DEFINITION = SQL_OU_TABLE_DEFINITION;
  SQL_SU_INDEX_DEFINITION = SQL_OU_INDEX_DEFINITION;
  SQL_SU_PRIVILEGE_DEFINITION = SQL_OU_PRIVILEGE_DEFINITION;

{ SQL_QUALIFIER_USAGE masks  }
const
  SQL_QU_DML_STATEMENTS = $00000001;
  SQL_QU_PROCEDURE_INVOCATION = $00000002;
  SQL_QU_TABLE_DEFINITION = $00000004;
  SQL_QU_INDEX_DEFINITION = $00000008;
  SQL_QU_PRIVILEGE_DEFINITION = $00000010;

{ SQL_CATALOG_USAGE masks  }
const
  SQL_CU_DML_STATEMENTS = SQL_QU_DML_STATEMENTS;
  SQL_CU_PROCEDURE_INVOCATION = SQL_QU_PROCEDURE_INVOCATION;
  SQL_CU_TABLE_DEFINITION = SQL_QU_TABLE_DEFINITION;
  SQL_CU_INDEX_DEFINITION = SQL_QU_INDEX_DEFINITION;
  SQL_CU_PRIVILEGE_DEFINITION = SQL_QU_PRIVILEGE_DEFINITION;

{ SQL_SUBQUERIES masks  }
const
  SQL_SQ_COMPARISON = $00000001;
  SQL_SQ_EXISTS = $00000002;
  SQL_SQ_IN = $00000004;
  SQL_SQ_QUANTIFIED = $00000008;
  SQL_SQ_CORRELATED_SUBQUERIES = $00000010;
{ SQL_UNION masks  }
  SQL_U_UNION = $00000001;
  SQL_U_UNION_ALL = $00000002;
{ SQL_BOOKMARK_PERSISTENCE values  }
  SQL_BP_CLOSE = $00000001;
  SQL_BP_DELETE = $00000002;
  SQL_BP_DROP = $00000004;
  SQL_BP_TRANSACTION = $00000008;
  SQL_BP_UPDATE = $00000010;
  SQL_BP_OTHER_HSTMT = $00000020;
  SQL_BP_SCROLL = $00000040;
{ SQL_STATIC_SENSITIVITY values  }
  SQL_SS_ADDITIONS = $00000001;
  SQL_SS_DELETIONS = $00000002;
  SQL_SS_UPDATES = $00000004;
{ SQL_VIEW values  }
  SQL_CV_CREATE_VIEW = $00000001;
  SQL_CV_CHECK_OPTION = $00000002;
  SQL_CV_CASCADED = $00000004;
  SQL_CV_LOCAL = $00000008;
{ SQL_LOCK_TYPES masks  }
  SQL_LCK_NO_CHANGE = $00000001;
  SQL_LCK_EXCLUSIVE = $00000002;
  SQL_LCK_UNLOCK = $00000004;
{ SQL_POS_OPERATIONS masks  }
  SQL_POS_POSITION = $00000001;
  SQL_POS_REFRESH = $00000002;
  SQL_POS_UPDATE = $00000004;
  SQL_POS_DELETE = $00000008;
  SQL_POS_ADD = $00000010;
{ SQL_QUALIFIER_LOCATION values  }
  SQL_QL_START = $0001;
  SQL_QL_END = $0002;
{ Here start return values for ODBC 3.0 SQLGetInfo  }

{ SQL_AGGREGATE_FUNCTIONS bitmasks  }
const
  SQL_AF_AVG = $00000001;
  SQL_AF_COUNT = $00000002;
  SQL_AF_MAX = $00000004;
  SQL_AF_MIN = $00000008;
  SQL_AF_SUM = $00000010;
  SQL_AF_DISTINCT = $00000020;
  SQL_AF_ALL = $00000040;
{ SQL_SQL_CONFORMANCE bit masks  }
  SQL_SC_SQL92_ENTRY = $00000001;
  SQL_SC_FIPS127_2_TRANSITIONAL = $00000002;
  SQL_SC_SQL92_INTERMEDIATE = $00000004;
  SQL_SC_SQL92_FULL = $00000008;
{ SQL_DATETIME_LITERALS masks  }
  SQL_DL_SQL92_DATE = $00000001;
  SQL_DL_SQL92_TIME = $00000002;
  SQL_DL_SQL92_TIMESTAMP = $00000004;
  SQL_DL_SQL92_INTERVAL_YEAR = $00000008;
  SQL_DL_SQL92_INTERVAL_MONTH = $00000010;
  SQL_DL_SQL92_INTERVAL_DAY = $00000020;
  SQL_DL_SQL92_INTERVAL_HOUR = $00000040;
  SQL_DL_SQL92_INTERVAL_MINUTE = $00000080;
  SQL_DL_SQL92_INTERVAL_SECOND = $00000100;
  SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH = $00000200;
  SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR = $00000400;
  SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE = $00000800;
  SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND = $00001000;
  SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE = $00002000;
  SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND = $00004000;
  SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND = $00008000;
{ SQL_CATALOG_LOCATION values  }
  SQL_CL_START = SQL_QL_START;
  SQL_CL_END = SQL_QL_END;
{ values for SQL_BATCH_ROW_COUNT  }
  SQL_BRC_PROCEDURES = $0000001;
  SQL_BRC_EXPLICIT = $0000002;
  SQL_BRC_ROLLED_UP = $0000004;
{ bitmasks for SQL_BATCH_SUPPORT  }
  SQL_BS_SELECT_EXPLICIT = $00000001;
  SQL_BS_ROW_COUNT_EXPLICIT = $00000002;
  SQL_BS_SELECT_PROC = $00000004;
  SQL_BS_ROW_COUNT_PROC = $00000008;
{ Values for SQL_PARAM_ARRAY_ROW_COUNTS getinfo  }
  SQL_PARC_BATCH = 1;
  SQL_PARC_NO_BATCH = 2;
{ values for SQL_PARAM_ARRAY_SELECTS  }
  SQL_PAS_BATCH = 1;
  SQL_PAS_NO_BATCH = 2;
  SQL_PAS_NO_SELECT = 3;
{ Bitmasks for SQL_INDEX_KEYWORDS  }
  SQL_IK_NONE = $00000000;
  SQL_IK_ASC = $00000001;
  SQL_IK_DESC = $00000002;
  SQL_IK_ALL = SQL_IK_ASC or SQL_IK_DESC;
{ Bitmasks for SQL_INFO_SCHEMA_VIEWS  }
  SQL_ISV_ASSERTIONS = $00000001;
  SQL_ISV_CHARACTER_SETS = $00000002;
  SQL_ISV_CHECK_CONSTRAINTS = $00000004;
  SQL_ISV_COLLATIONS = $00000008;
  SQL_ISV_COLUMN_DOMAIN_USAGE = $00000010;
  SQL_ISV_COLUMN_PRIVILEGES = $00000020;
  SQL_ISV_COLUMNS = $00000040;
  SQL_ISV_CONSTRAINT_COLUMN_USAGE = $00000080;
  SQL_ISV_CONSTRAINT_TABLE_USAGE = $00000100;
  SQL_ISV_DOMAIN_CONSTRAINTS = $00000200;
  SQL_ISV_DOMAINS = $00000400;
  SQL_ISV_KEY_COLUMN_USAGE = $00000800;
  SQL_ISV_REFERENTIAL_CONSTRAINTS = $00001000;
  SQL_ISV_SCHEMATA = $00002000;
  SQL_ISV_SQL_LANGUAGES = $00004000;
  SQL_ISV_TABLE_CONSTRAINTS = $00008000;
  SQL_ISV_TABLE_PRIVILEGES = $00010000;
  SQL_ISV_TABLES = $00020000;
  SQL_ISV_TRANSLATIONS = $00040000;
  SQL_ISV_USAGE_PRIVILEGES = $00080000;
  SQL_ISV_VIEW_COLUMN_USAGE = $00100000;
  SQL_ISV_VIEW_TABLE_USAGE = $00200000;
  SQL_ISV_VIEWS = $00400000;

{ Bitmasks for SQL_ALTER_DOMAIN  }
  SQL_AD_CONSTRAINT_NAME_DEFINITION = $00000001;
  SQL_AD_ADD_DOMAIN_CONSTRAINT = $00000002;
  SQL_AD_DROP_DOMAIN_CONSTRAINT = $00000004;
  SQL_AD_ADD_DOMAIN_DEFAULT = $00000008;
  SQL_AD_DROP_DOMAIN_DEFAULT = $00000010;
  SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED = $00000020;
  SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_AD_ADD_CONSTRAINT_DEFERRABLE = $00000080;
  SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE = $00000100;
{ SQL_CREATE_SCHEMA bitmasks  }
  SQL_CS_CREATE_SCHEMA = $00000001;
  SQL_CS_AUTHORIZATION = $00000002;
  SQL_CS_DEFAULT_CHARACTER_SET = $00000004;
{ SQL_CREATE_TRANSLATION bitmasks  }
  SQL_CTR_CREATE_TRANSLATION = $00000001;
{ SQL_CREATE_ASSERTION bitmasks  }
  SQL_CA_CREATE_ASSERTION = $00000001;
  SQL_CA_CONSTRAINT_INITIALLY_DEFERRED = $00000010;
  SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE = $00000020;
  SQL_CA_CONSTRAINT_DEFERRABLE = $00000040;
  SQL_CA_CONSTRAINT_NON_DEFERRABLE = $00000080;
{ SQL_CREATE_CHARACTER_SET bitmasks  }
  SQL_CCS_CREATE_CHARACTER_SET = $00000001;
  SQL_CCS_COLLATE_CLAUSE = $00000002;
  SQL_CCS_LIMITED_COLLATION = $00000004;
{ SQL_CREATE_COLLATION bitmasks  }
  SQL_CCOL_CREATE_COLLATION = $00000001;
{ SQL_CREATE_DOMAIN bitmasks  }
  SQL_CDO_CREATE_DOMAIN = $00000001;
  SQL_CDO_DEFAULT = $00000002;
  SQL_CDO_CONSTRAINT = $00000004;
  SQL_CDO_COLLATION = $00000008;
  SQL_CDO_CONSTRAINT_NAME_DEFINITION = $00000010;
  SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED = $00000020;
  SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_CDO_CONSTRAINT_DEFERRABLE = $00000080;
  SQL_CDO_CONSTRAINT_NON_DEFERRABLE = $00000100;
{ SQL_CREATE_TABLE bitmasks  }
  SQL_CT_CREATE_TABLE = $00000001;
  SQL_CT_COMMIT_PRESERVE = $00000002;
  SQL_CT_COMMIT_DELETE = $00000004;
  SQL_CT_GLOBAL_TEMPORARY = $00000008;
  SQL_CT_LOCAL_TEMPORARY = $00000010;
  SQL_CT_CONSTRAINT_INITIALLY_DEFERRED = $00000020;
  SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_CT_CONSTRAINT_DEFERRABLE = $00000080;
  SQL_CT_CONSTRAINT_NON_DEFERRABLE = $00000100;
  SQL_CT_COLUMN_CONSTRAINT = $00000200;
  SQL_CT_COLUMN_DEFAULT = $00000400;
  SQL_CT_COLUMN_COLLATION = $00000800;
  SQL_CT_TABLE_CONSTRAINT = $00001000;
  SQL_CT_CONSTRAINT_NAME_DEFINITION = $00002000;
{ SQL_DDL_INDEX bitmasks  }
  SQL_DI_CREATE_INDEX = $00000001;
  SQL_DI_DROP_INDEX = $00000002;
{ SQL_DROP_COLLATION bitmasks  }
  SQL_DC_DROP_COLLATION = $00000001;
{ SQL_DROP_DOMAIN bitmasks  }
  SQL_DD_DROP_DOMAIN = $00000001;
  SQL_DD_RESTRICT = $00000002;
  SQL_DD_CASCADE = $00000004;
{ SQL_DROP_SCHEMA bitmasks  }
  SQL_DS_DROP_SCHEMA = $00000001;
  SQL_DS_RESTRICT = $00000002;
  SQL_DS_CASCADE = $00000004;
{ SQL_DROP_CHARACTER_SET bitmasks  }
  SQL_DCS_DROP_CHARACTER_SET = $00000001;
{ SQL_DROP_ASSERTION bitmasks  }
  SQL_DA_DROP_ASSERTION = $00000001;
{ SQL_DROP_TABLE bitmasks  }
  SQL_DT_DROP_TABLE = $00000001;
  SQL_DT_RESTRICT = $00000002;
  SQL_DT_CASCADE = $00000004;
{ SQL_DROP_TRANSLATION bitmasks  }
  SQL_DTR_DROP_TRANSLATION = $00000001;
{ SQL_DROP_VIEW bitmasks  }
  SQL_DV_DROP_VIEW = $00000001;
  SQL_DV_RESTRICT = $00000002;
  SQL_DV_CASCADE = $00000004;
{ SQL_INSERT_STATEMENT bitmasks  }
  SQL_IS_INSERT_LITERALS = $00000001;
  SQL_IS_INSERT_SEARCHED = $00000002;
  SQL_IS_SELECT_INTO = $00000004;
{ SQL_ODBC_INTERFACE_CONFORMANCE values  }
  SQL_OIC_CORE = 1;
  SQL_OIC_LEVEL1 = 2;
  SQL_OIC_LEVEL2 = 3;
{ SQL_SQL92_FOREIGN_KEY_DELETE_RULE bitmasks  }
  SQL_SFKD_CASCADE = $00000001;
  SQL_SFKD_NO_ACTION = $00000002;
  SQL_SFKD_SET_DEFAULT = $00000004;
  SQL_SFKD_SET_NULL = $00000008;
{ SQL_SQL92_FOREIGN_KEY_UPDATE_RULE bitmasks  }
  SQL_SFKU_CASCADE = $00000001;
  SQL_SFKU_NO_ACTION = $00000002;
  SQL_SFKU_SET_DEFAULT = $00000004;
  SQL_SFKU_SET_NULL = $00000008;
{ SQL_SQL92_GRANT  bitmasks  }
  SQL_SG_USAGE_ON_DOMAIN = $00000001;
  SQL_SG_USAGE_ON_CHARACTER_SET = $00000002;
  SQL_SG_USAGE_ON_COLLATION = $00000004;
  SQL_SG_USAGE_ON_TRANSLATION = $00000008;
  SQL_SG_WITH_GRANT_OPTION = $00000010;
  SQL_SG_DELETE_TABLE = $00000020;
  SQL_SG_INSERT_TABLE = $00000040;
  SQL_SG_INSERT_COLUMN = $00000080;
  SQL_SG_REFERENCES_TABLE = $00000100;
  SQL_SG_REFERENCES_COLUMN = $00000200;
  SQL_SG_SELECT_TABLE = $00000400;
  SQL_SG_UPDATE_TABLE = $00000800;
  SQL_SG_UPDATE_COLUMN = $00001000;
{ SQL_SQL92_PREDICATES bitmasks  }
  SQL_SP_EXISTS = $00000001;
  SQL_SP_ISNOTNULL = $00000002;
  SQL_SP_ISNULL = $00000004;
  SQL_SP_MATCH_FULL = $00000008;
  SQL_SP_MATCH_PARTIAL = $00000010;
  SQL_SP_MATCH_UNIQUE_FULL = $00000020;
  SQL_SP_MATCH_UNIQUE_PARTIAL = $00000040;
  SQL_SP_OVERLAPS = $00000080;
  SQL_SP_UNIQUE = $00000100;
  SQL_SP_LIKE = $00000200;
  SQL_SP_IN = $00000400;
  SQL_SP_BETWEEN = $00000800;
  SQL_SP_COMPARISON = $00001000;
  SQL_SP_QUANTIFIED_COMPARISON = $00002000;
{ SQL_SQL92_RELATIONAL_JOIN_OPERATORS bitmasks  }
  SQL_SRJO_CORRESPONDING_CLAUSE = $00000001;
  SQL_SRJO_CROSS_JOIN = $00000002;
  SQL_SRJO_EXCEPT_JOIN = $00000004;
  SQL_SRJO_FULL_OUTER_JOIN = $00000008;
  SQL_SRJO_INNER_JOIN = $00000010;
  SQL_SRJO_INTERSECT_JOIN = $00000020;
  SQL_SRJO_LEFT_OUTER_JOIN = $00000040;
  SQL_SRJO_NATURAL_JOIN = $00000080;
  SQL_SRJO_RIGHT_OUTER_JOIN = $00000100;
  SQL_SRJO_UNION_JOIN = $00000200;
{ SQL_SQL92_REVOKE bitmasks  }
  SQL_SR_USAGE_ON_DOMAIN = $00000001;
  SQL_SR_USAGE_ON_CHARACTER_SET = $00000002;
  SQL_SR_USAGE_ON_COLLATION = $00000004;
  SQL_SR_USAGE_ON_TRANSLATION = $00000008;
  SQL_SR_GRANT_OPTION_FOR = $00000010;
  SQL_SR_CASCADE = $00000020;
  SQL_SR_RESTRICT = $00000040;
  SQL_SR_DELETE_TABLE = $00000080;
  SQL_SR_INSERT_TABLE = $00000100;
  SQL_SR_INSERT_COLUMN = $00000200;
  SQL_SR_REFERENCES_TABLE = $00000400;
  SQL_SR_REFERENCES_COLUMN = $00000800;
  SQL_SR_SELECT_TABLE = $00001000;
  SQL_SR_UPDATE_TABLE = $00002000;
  SQL_SR_UPDATE_COLUMN = $00004000;
{ SQL_SQL92_ROW_VALUE_CONSTRUCTOR bitmasks  }
  SQL_SRVC_VALUE_EXPRESSION = $00000001;
  SQL_SRVC_NULL = $00000002;
  SQL_SRVC_DEFAULT = $00000004;
  SQL_SRVC_ROW_SUBQUERY = $00000008;
{ SQL_SQL92_VALUE_EXPRESSIONS bitmasks  }
  SQL_SVE_CASE = $00000001;
  SQL_SVE_CAST = $00000002;
  SQL_SVE_COALESCE = $00000004;
  SQL_SVE_NULLIF = $00000008;
{ SQL_STANDARD_CLI_CONFORMANCE bitmasks  }
  SQL_SCC_XOPEN_CLI_VERSION1 = $00000001;
  SQL_SCC_ISO92_CLI = $00000002;
{ SQL_UNION_STATEMENT bitmasks  }
  SQL_US_UNION = SQL_U_UNION;
  SQL_US_UNION_ALL = SQL_U_UNION_ALL;
{ values for SQL_DRIVER_AWARE_POOLING_SUPPORTED  }
  SQL_DRIVER_AWARE_POOLING_NOT_CAPABLE = $00000000;
  SQL_DRIVER_AWARE_POOLING_CAPABLE = $00000001;

{ SQL_DTC_TRANSITION_COST bitmasks  }
const
  SQL_DTC_ENLIST_EXPENSIVE = $00000001;
  SQL_DTC_UNENLIST_EXPENSIVE = $00000002;

{ possible values for SQL_ASYNC_DBC_FUNCTIONS  }
const
  SQL_ASYNC_DBC_NOT_CAPABLE = $00000000; //(ODBCVER >= 0x0380)
  SQL_ASYNC_DBC_CAPABLE = $00000001; //(ODBCVER >= 0x0380)

{ additional SQLDataSources fetch directions  }
const
  SQL_FETCH_FIRST_USER = 31;
  SQL_FETCH_FIRST_SYSTEM = 32;

{ Defines for SQLSetPos  }
const
  SQL_ENTIRE_ROWSET = 0;
{ Operations in SQLSetPos  }
{      1.0 FALSE  }
  SQL_POSITION = 0;
{      1.0 TRUE  }
  SQL_REFRESH = 1;
  SQL_UPDATE = 2;
  SQL_DELETE = 3;
{ Operations in SQLBulkOperations  }
  SQL_ADD = 4;
  SQL_SETPOS_MAX_OPTION_VALUE = SQL_ADD;
  SQL_UPDATE_BY_BOOKMARK = 5;
  SQL_DELETE_BY_BOOKMARK = 6;
  SQL_FETCH_BY_BOOKMARK = 7;

{ Lock options in SQLSetPos  }
{      1.0 FALSE  }

const
  SQL_LOCK_NO_CHANGE = 0;
{      1.0 TRUE  }
  SQL_LOCK_EXCLUSIVE = 1;
  SQL_LOCK_UNLOCK = 2;
  SQL_SETPOS_MAX_LOCK_VALUE = SQL_LOCK_UNLOCK;

{ Options for SQLDriverConnect  }
const
  SQL_DRIVER_NOPROMPT = 0;
  SQL_DRIVER_COMPLETE = 1;
  SQL_DRIVER_PROMPT = 2;
  SQL_DRIVER_COMPLETE_REQUIRED = 3;

{ supported SQLFetchScroll FetchOrientation's  }
const
  SQL_CA1_NEXT = $00000001;
  SQL_CA1_ABSOLUTE = $00000002;
  SQL_CA1_RELATIVE = $00000004;
  SQL_CA1_BOOKMARK = $00000008;
{ supported SQLSetPos LockType's  }
  SQL_CA1_LOCK_NO_CHANGE = $00000040;
  SQL_CA1_LOCK_EXCLUSIVE = $00000080;
  SQL_CA1_LOCK_UNLOCK = $00000100;
{ supported SQLSetPos Operations  }
  SQL_CA1_POS_POSITION = $00000200;
  SQL_CA1_POS_UPDATE = $00000400;
  SQL_CA1_POS_DELETE = $00000800;
  SQL_CA1_POS_REFRESH = $00001000;
{ positioned updates and deletes  }
  SQL_CA1_POSITIONED_UPDATE = $00002000;
  SQL_CA1_POSITIONED_DELETE = $00004000;
  SQL_CA1_SELECT_FOR_UPDATE = $00008000;
{ supported SQLBulkOperations operations  }
  SQL_CA1_BULK_ADD = $00010000;
  SQL_CA1_BULK_UPDATE_BY_BOOKMARK = $00020000;
  SQL_CA1_BULK_DELETE_BY_BOOKMARK = $00040000;
  SQL_CA1_BULK_FETCH_BY_BOOKMARK = $00080000;

{ bitmasks for SQL_DYNAMIC_CURSOR_ATTRIBUTES2,
 * SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2,
 * SQL_KEYSET_CURSOR_ATTRIBUTES2, and SQL_STATIC_CURSOR_ATTRIBUTES2
  }
{ supported values for SQL_ATTR_SCROLL_CONCURRENCY  }
const
  SQL_CA2_READ_ONLY_CONCURRENCY = $00000001;
  SQL_CA2_LOCK_CONCURRENCY = $00000002;
  SQL_CA2_OPT_ROWVER_CONCURRENCY = $00000004;
  SQL_CA2_OPT_VALUES_CONCURRENCY = $00000008;
{ sensitivity of the cursor to its own inserts, deletes, and updates  }
  SQL_CA2_SENSITIVITY_ADDITIONS = $00000010;
  SQL_CA2_SENSITIVITY_DELETIONS = $00000020;
  SQL_CA2_SENSITIVITY_UPDATES = $00000040;
{ semantics of SQL_ATTR_MAX_ROWS  }
  SQL_CA2_MAX_ROWS_SELECT = $00000080;
  SQL_CA2_MAX_ROWS_INSERT = $00000100;
  SQL_CA2_MAX_ROWS_DELETE = $00000200;
  SQL_CA2_MAX_ROWS_UPDATE = $00000400;
  SQL_CA2_MAX_ROWS_CATALOG = $00000800;
  SQL_CA2_MAX_ROWS_AFFECTS_ALL = (((SQL_CA2_MAX_ROWS_SELECT or SQL_CA2_MAX_ROWS_INSERT) or SQL_CA2_MAX_ROWS_DELETE) or SQL_CA2_MAX_ROWS_UPDATE) or SQL_CA2_MAX_ROWS_CATALOG;
{ semantics of SQL_DIAG_CURSOR_ROW_COUNT  }
  SQL_CA2_CRC_EXACT = $00001000;
  SQL_CA2_CRC_APPROXIMATE = $00002000;
{ the kinds of positioned statements that can be simulated  }
  SQL_CA2_SIMULATE_NON_UNIQUE = $00004000;
  SQL_CA2_SIMULATE_TRY_UNIQUE = $00008000;
  SQL_CA2_SIMULATE_UNIQUE = $00010000;

//sqlncli.h
// SQLSetStmtAttr SQL Server Native Client driver specific defines.
// Statement attributes
  SQL_SOPT_SS_BASE                            = 1225;
  SQL_SOPT_SS_TEXTPTR_LOGGING                 = (SQL_SOPT_SS_BASE+0); // Text pointer logging
  SQL_SOPT_SS_CURRENT_COMMAND                 = (SQL_SOPT_SS_BASE+1); // dbcurcmd SQLGetStmtOption only
  SQL_SOPT_SS_HIDDEN_COLUMNS                  = (SQL_SOPT_SS_BASE+2); // Expose FOR BROWSE hidden columns
  SQL_SOPT_SS_NOBROWSETABLE                   = (SQL_SOPT_SS_BASE+3); // Set NOBROWSETABLE option
  SQL_SOPT_SS_REGIONALIZE                     = (SQL_SOPT_SS_BASE+4); // Regionalize output character conversions
  SQL_SOPT_SS_CURSOR_OPTIONS                  = (SQL_SOPT_SS_BASE+5); // Server cursor options
  SQL_SOPT_SS_NOCOUNT_STATUS                  = (SQL_SOPT_SS_BASE+6); // Real vs. Not Real row count indicator
  SQL_SOPT_SS_DEFER_PREPARE                   = (SQL_SOPT_SS_BASE+7); // Defer prepare until necessary
  SQL_SOPT_SS_QUERYNOTIFICATION_TIMEOUT       = (SQL_SOPT_SS_BASE+8); // Notification timeout
  SQL_SOPT_SS_QUERYNOTIFICATION_MSGTEXT       = (SQL_SOPT_SS_BASE+9); // Notification message text
  SQL_SOPT_SS_QUERYNOTIFICATION_OPTIONS       = (SQL_SOPT_SS_BASE+10);// SQL service broker name
  SQL_SOPT_SS_PARAM_FOCUS                     = (SQL_SOPT_SS_BASE+11);// Direct subsequent calls to parameter related methods to set properties on constituent columns/parameters of container types
  SQL_SOPT_SS_NAME_SCOPE                      = (SQL_SOPT_SS_BASE+12);// Sets name scope for subsequent catalog function calls
  SQL_SOPT_SS_MAX_USED                        = SQL_SOPT_SS_NAME_SCOPE;

// Defines for use with SQL_SOPT_SS_CURSOR_OPTIONS
  SQL_CO_OFF                          = 0;           //  Clear all cursor options
  SQL_CO_FFO                          = 1;           //  Fast-forward cursor will be used
  SQL_CO_AF                           = 2;           //  Autofetch on cursor open
  SQL_CO_FFO_AF                       = (SQL_CO_FFO or SQL_CO_AF);  //  Fast-forward cursor with autofetch
  SQL_CO_FIREHOSE_AF                  = 4;           //  Auto fetch on fire-hose cursors
  SQL_CO_DEFAULT                      = SQL_CO_OFF;

{ Defines for SQLTables  }
const
  SQL_ALL_CATALOGS = '%';
  SQL_ALL_SCHEMAS = '%';
  SQL_ALL_TABLE_TYPES = '%';

type
  IODBC3BasePlainDriver = interface(IZPlainDriver)
    ['{1B4A3A81-78DF-4097-B715-1D692D35565A}']
  end;

  IODBC3UnicodePlainDriver = interface(IODBC3BasePlainDriver)
    ['{40D3EDEF-B1A8-4151-A406-8E8B8F4BD1E4}']
  end;

  IODBC3RawPlainDriver = interface(IODBC3BasePlainDriver)
    ['{6CE34B8A-CBC9-411F-8FC9-87E519762C95}']
  end;

  TZODBC3PlainDriver = class(TZAbstractPlainDriver, IODBC3BasePlainDriver)
  public
    SQLConnect: function (ConnectionHandle: SQLHDBC;
      ServerName: {$IFDEF UNICODE}PSQLWCHAR{$ELSE}PSQLCHAR{$ENDIF}; NameLength1: SQLSMALLINT;
      UserName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLAllocHandle: function(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE;
      var OutputHandle: SQLHANDLE): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLBindCol: function(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_Ind: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLBindParameter: function(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      InputOutputType: SQLSMALLINT; ValueType: SQLSMALLINT; ParameterType: SQLSMALLINT;
      ColumnSize: SQLULEN; DecimalDigits: SQLSMALLINT; ParameterValuePtr: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_IndPtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLBulkOperations: function(StatementHandle: SQLHSTMT; Operation: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCancel: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCancelHandle: function(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCloseCursor: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCompleteAsync: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      var AsyncRetCodePtr: PRETCODE): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCopyDesc: function (SourceDescHandle: SQLHDESC; TargetDescHandle: SQLHDESC): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeParam: function(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      DataTypePtr: PSQLSMALLINT; ParameterSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDisconnect: function(ConnectionHandle: SQLHDBC): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDriverConnect: function(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: {$IFDEF UNICODE}PSQLWCHAR{$ELSE}PSQLCHAR{$ENDIF}; StringLength1: SQLSMALLINT;
      OutConnectionString: {$IFDEF UNICODE}PSQLWCHAR{$ELSE}PSQLCHAR{$ENDIF}; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLEndTran: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      CompletionType: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLExecute: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFetch: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFetchScroll: function(StatementHandle: SQLHSTMT; FetchOrientation: SQLSMALLINT;
      FetchOffset: SQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFreeHandle: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFreeStmt: function(StatementHandle: SQLHSTMT; Option: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetData: function(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER; BufferLength: SQLLEN;
      StrLen_or_IndPtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetEnvAttr: function(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetFunctions: function(ConnectionHandle: SQLHDBC; FunctionId: SQLUSMALLINT;
      SupportedPtr: PSQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetTypeInfo: function(StatementHandle: SQLHSTMT; DataType: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLMoreResults: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNumParams: function(StatementHandle: SQLHSTMT; ParameterCountPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNumResultCols: function(StatementHandle: SQLHSTMT; ColumnCountPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLParamData: function(StatementHandle: SQLHSTMT; ValuePtrPtr: PSQLPOINTER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPutData: function(StatementHandle: SQLHSTMT; DataPtr: SQLPOINTER;
      StrLen_or_Ind: SQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLRowCount: function(StatementHandle: SQLHSTMT; RowCountPtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetDescRec: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLLEN; Precision: SQLSMALLINT;
      Scale: SQLSMALLINT; DataPtr: SQLPOINTER; StringLengthPtr: PSQLLEN;
      IndicatorPtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetEnvAttr: function(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetPos: function(StatementHandle: SQLHSTMT; RowNumber: SQLSETPOSIROW;
      Operation: SQLUSMALLINT; LockType: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  public //next fields are assigned by descendants:
    SQLGetInfo: function(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetStmtAttr: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetConnectAttr: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetDescField: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetStmtAttr: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  private
    FDriverName: String;
  protected
    procedure LoadApi; override;
  public
    function GetProtocol: string; override;
    constructor Create(const DriverName: String);
  end;

  TODBC3UnicodePlainDriver = class(TZODBC3PlainDriver, IODBC3UnicodePlainDriver)
  public
    SQLBrowseConnectW: function(ConnectionHandle: SQLHDBC; InConnectionString: PSQLWCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColAttributeW: function(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumnPrivilegesW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumnsW: function (StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDataSourcesW: function (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLWCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeColW: function(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDriversW: function(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLWCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLWCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLExecDirectW: function(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLForeignKeysW: function(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLWCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLWCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLWCHAR; NameLength6: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetConnectAttrW: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescFieldW: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagFieldW: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetCursorNameW: function(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescRecW: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLWCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagRecW: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLWCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNativeSqlW: function(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrepareW: function(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrimaryKeysW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProcedureColumnsW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProceduresW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetCursorNameW: function(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      NameLength: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSpecialColumnsW: function(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLStatisticsW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTablePrivilegesW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTablesW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    function GetDescription: string; override;
    procedure LoadCodePages; override;
    constructor Create(const DriverName: String = '_w');
  end;

  TODBC3RawPlainDriver = class(TZODBC3PlainDriver, IODBC3RawPlainDriver)
  public
    SQLBrowseConnect: function(ConnectionHandle: SQLHDBC; InConnectionString: PSQLCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColAttribute: function(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumnPrivileges: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumns: function (StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDataSources: function (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeCol: function(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDrivers: function(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLExecDirect: function(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLForeignKeys: function(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLCHAR; NameLength6: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetConnectAttr: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescField: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagField: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetCursorName: function(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescRec: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagRec: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNativeSql: function(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrepare: function(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrimaryKeys: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProcedureColumns: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProcedures: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetCursorName: function(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      NameLength: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSpecialColumns: function(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLStatistics: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTablePrivileges: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTables: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    function GetDescription: string; override;
    procedure LoadCodePages; override;
    constructor Create(const DriverName: String = '_a');
  end;

{$ENDIF ZEOS_DISABLE_ODBC}

implementation

{$IFNDEF ZEOS_DISABLE_ODBC}

uses ZPlainLoader;

{ TZODBC3PlainDriver }

constructor TZODBC3PlainDriver.Create(const DriverName: String);
begin
  inherited Create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IF defined(Unix) or defined (MSWINDOWS)}
  FLoader.AddLocation(ODBC_LOCATION);
  {$IFEND}
  FDriverName := 'odbc'+DriverName;
  LoadCodePages;
end;

function TZODBC3PlainDriver.GetProtocol: string;
begin
  Result := FDriverName;
end;

procedure TZODBC3PlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  @SQLAllocHandle           := GetAddress('SQLAllocHandle');
  @SQLBindCol               := GetAddress('SQLBindCol');
  @SQLBindParameter         := GetAddress('SQLBindParameter');
  @SQLBulkOperations        := GetAddress('SQLBulkOperations');
  @SQLCancel                := GetAddress('SQLCancel');
  @SQLCancelHandle          := GetAddress('SQLCancelHandle');
  @SQLCloseCursor           := GetAddress('SQLCloseCursor');
  @SQLCompleteAsync         := GetAddress('SQLCompleteAsync');
  @SQLConnect               := GetAddress({$IFDEF UNICODE}'SQLConnectW'{$ELSE}'SQLConnect'{$ENDIF});
  @SQLCopyDesc              := GetAddress('SQLCopyDesc');
  @SQLDescribeParam         := GetAddress('SQLDescribeParam');
  @SQLDisconnect            := GetAddress('SQLDisconnect');
  @SQLDriverConnect         := GetAddress({$IFDEF UNICODE}'SQLDriverConnectW'{$ELSE}'SQLDriverConnect'{$ENDIF});
  @SQLEndTran               := GetAddress('SQLEndTran');
  @SQLExecute               := GetAddress('SQLExecute');
  @SQLFetch                 := GetAddress('SQLFetch');
  @SQLFetchScroll           := GetAddress('SQLFetchScroll');
  @SQLFreeHandle            := GetAddress('SQLFreeHandle');
  @SQLFreeStmt              := GetAddress('SQLFreeStmt');
  @SQLGetData               := GetAddress('SQLGetData');
  @SQLGetEnvAttr            := GetAddress('SQLGetEnvAttr');
  @SQLGetFunctions          := GetAddress('SQLGetFunctions');
  @SQLGetTypeInfo           := GetAddress('SQLGetTypeInfo');
  @SQLMoreResults           := GetAddress('SQLMoreResults');
  @SQLNumParams             := GetAddress('SQLNumParams');
  @SQLNumResultCols         := GetAddress('SQLNumResultCols');
  @SQLParamData             := GetAddress('SQLParamData');
  @SQLPutData               := GetAddress('SQLPutData');
  @SQLRowCount              := GetAddress('SQLRowCount');
  @SQLSetDescRec            := GetAddress('SQLSetDescRec');
  @SQLSetEnvAttr            := GetAddress('SQLSetEnvAttr');
  @SQLSetPos                := GetAddress('SQLSetPos');
  end;
end;

{ TODBC3UnicodePlainDriver }

function TODBC3UnicodePlainDriver.Clone: IZPlainDriver;
begin
  Result := TODBC3UnicodePlainDriver.Create;
end;

constructor TODBC3UnicodePlainDriver.Create(const DriverName: String);
begin
  inherited Create(DriverName)
end;

function TODBC3UnicodePlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ODBC Unicode';
end;

procedure TODBC3UnicodePlainDriver.LoadApi;
begin
  inherited LoadApi;
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  @SQLBrowseConnectW        := GetAddress('SQLBrowseConnectW');
  @SQLColAttributeW         := GetAddress('SQLColAttributeW');
  @SQLColumnPrivilegesW     := GetAddress('SQLColumnPrivilegesW');
  @SQLColumnsW              := GetAddress('SQLColumnsW');
  @SQLDataSourcesW          := GetAddress('SQLDataSourcesW');
  @SQLDescribeColW          := GetAddress('SQLDescribeColW');
  @SQLDriversW              := GetAddress('SQLDriversW');
  @SQLExecDirectW           := GetAddress('SQLExecDirectW');
  @SQLForeignKeysW          := GetAddress('SQLForeignKeysW');
  @SQLGetConnectAttrW       := GetAddress('SQLGetConnectAttrW');
  @SQLGetCursorNameW        := GetAddress('SQLGetCursorNameW');
  @SQLGetDescFieldW         := GetAddress('SQLGetDescFieldW');
  @SQLGetDescRecW           := GetAddress('SQLGetDescRecW');
  @SQLGetDiagFieldW         := GetAddress('SQLGetDiagFieldW');
  @SQLGetDiagRecW           := GetAddress('SQLGetDiagRecW');
  @SQLGetInfo               := GetAddress('SQLGetInfoW');
  @SQLGetStmtAttr           := GetAddress('SQLGetStmtAttrW');
  @SQLNativeSqlW            := GetAddress('SQLNativeSqlW');
  @SQLPrepareW              := GetAddress('SQLPrepareW');
  @SQLPrimaryKeysW          := GetAddress('SQLPrimaryKeysW');
  @SQLProcedureColumnsW     := GetAddress('SQLProcedureColumnsW');
  @SQLProceduresW           := GetAddress('SQLProceduresW');
  @SQLSetConnectAttr        := GetAddress('SQLSetConnectAttrW');
  @SQLSetCursorNameW        := GetAddress('SQLSetCursorNameW');
  @SQLSetDescField          := GetAddress('SQLSetDescFieldW');
  @SQLSetStmtAttr           := GetAddress('SQLSetStmtAttrW');
  @SQLSpecialColumnsW       := GetAddress('SQLSpecialColumnsW');
  @SQLStatisticsW           := GetAddress('SQLStatisticsW');
  @SQLTablePrivilegesW      := GetAddress('SQLTablePrivilegesW');
  @SQLTablesW               := GetAddress('SQLTablesW');
  end;
end;

procedure TODBC3UnicodePlainDriver.LoadCodePages;
begin
  AddCodePage('CP_UTF16', 0, ceUTF16, ZOSCodePage, '', 1, False);
end;

{ TODBC3RawPlainDriver }

function TODBC3RawPlainDriver.Clone: IZPlainDriver;
begin
  Result := TODBC3RawPlainDriver.Create;
end;

constructor TODBC3RawPlainDriver.Create(const DriverName: String);
begin
  inherited Create(DriverName);
end;

function TODBC3RawPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ODBC raw';
end;

procedure TODBC3RawPlainDriver.LoadApi;
begin
  inherited LoadApi;
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  @SQLBrowseConnect         := GetAddress('SQLBrowseConnect');
  @SQLColAttribute          := GetAddress('SQLColAttribute');
  @SQLColumnPrivileges      := GetAddress('SQLColumnPrivileges');
  @SQLColumns               := GetAddress('SQLColumns');
  @SQLDataSources           := GetAddress('SQLDataSources');
  @SQLDescribeCol           := GetAddress('SQLDescribeCol');
  @SQLDrivers               := GetAddress('SQLDrivers');
  @SQLExecDirect            := GetAddress('SQLExecDirect');
  @SQLForeignKeys           := GetAddress('SQLForeignKeys');
  @SQLGetConnectAttr        := GetAddress('SQLGetConnectAttr');
  @SQLGetCursorName         := GetAddress('SQLGetCursorName');
  @SQLGetDescRec            := GetAddress('SQLGetDescRec');
  @SQLGetDescField          := GetAddress('SQLGetDescField');
  @SQLGetDiagField          := GetAddress('SQLGetDiagField');
  @SQLGetDiagRec            := GetAddress('SQLGetDiagRec');
  @SQLGetInfo               := GetAddress('SQLGetInfo');
  @SQLGetStmtAttr           := GetAddress('SQLGetStmtAttr');
  @SQLNativeSql             := GetAddress('SQLNativeSql');
  @SQLPrepare               := GetAddress('SQLPrepare');
  @SQLPrimaryKeys           := GetAddress('SQLPrimaryKeys');
  @SQLProcedureColumns      := GetAddress('SQLProcedureColumns');
  @SQLProcedures            := GetAddress('SQLProcedures');
  @SQLSetConnectAttr        := GetAddress('SQLSetConnectAttr');
  @SQLSetCursorName         := GetAddress('SQLSetCursorName');
  @SQLSetDescField          := GetAddress('SQLSetDescField');
  @SQLSetStmtAttr           := GetAddress('SQLSetStmtAttr');
  @SQLSpecialColumns        := GetAddress('SQLSpecialColumns');
  @SQLStatistics            := GetAddress('SQLStatistics');
  @SQLTablePrivileges       := GetAddress('SQLTablePrivileges');
  @SQLTables                := GetAddress('SQLTables');
  end;
end;

procedure TODBC3RawPlainDriver.LoadCodePages;
begin
  AddCodePage('CP_ACP', 0, ceAnsi, ZOSCodePage, '', 1, True);
end;

{$ENDIF ZEOS_DISABLE_ODBC}

end.
