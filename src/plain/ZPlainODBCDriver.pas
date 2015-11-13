{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for ODBC               }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZPlainODBCDriver;

interface

{$I ZPlain.inc}

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
  SQLHDBC = SQLHANDLE;
  SQLHSTMT = SQLHANDLE;
  SQLHDESC = SQLHANDLE;

  PRETCODE = ^RETCODE;
  RETCODE = SmallInt;

const
  { size of SQLSTATE  }
  SQL_SQLSTATE_SIZE = 5;
  { size of SQLSTATE for unicode }
  SQL_SQLSTATE_SIZEW = 10;
type
  PSQLSTATE = ^TSQLSTATE;
  TSQLSTATE = array[0..SQL_SQLSTATE_SIZE] of SQLCHAR;

  PSQLSTATE_W = ^TSQLSTATE_W;
  TSQLSTATE_W = array[0..SQL_SQLSTATE_SIZEW] of SQLCHAR;

  {$A-}
//* transfer types for DATE, TIME, TIMESTAMP */
  SQL_DATE_STRUCT = record
    year:   SQLSMALLINT;
    month:  SQLUSMALLINT;
    day:    SQLUSMALLINT;
  end;

  SQL_TIME_STRUCT = record
    hour:   SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
  end;

  SQL_TIMESTAMP_STRUCT = record
    year:     SQLSMALLINT;
    month:    SQLUSMALLINT;
    day:      SQLUSMALLINT;
    hour:     SQLUSMALLINT;
    minute:   SQLUSMALLINT;
    second:   SQLUSMALLINT;
    fraction: SQLUINTEGER;
  end;

  SQLINTERVAL = (
    SQL_IS_YEAR             = 1,
    SQL_IS_MONTH            = 2,
    SQL_IS_DAY              = 3,
    SQL_IS_HOUR             = 4,
    SQL_IS_MINUTE           = 5,
    SQL_IS_SECOND           = 6,
    SQL_IS_YEAR_TO_MONTH    = 7,
    SQL_IS_DAY_TO_HOUR      = 8,
    SQL_IS_DAY_TO_MINUTE    = 9,
    SQL_IS_DAY_TO_SECOND    = 10,
    SQL_IS_HOUR_TO_MINUTE   = 11,
    SQL_IS_HOUR_TO_SECOND   = 12,
    SQL_IS_MINUTE_TO_SECOND = 13);

  SQL_YEAR_MONTH_STRUCT = record
    year:   SQLUINTEGER;
    month:  SQLUINTEGER;
  end;

  SQL_DAY_SECOND_STRUCT = record
    day:      SQLUINTEGER;
    hour:     SQLUINTEGER;
    minute:   SQLUINTEGER;
    second:   SQLUINTEGER;
    fraction: SQLUINTEGER;
  end;

  SQL_INTERVAL_STRUCT = record
    interval_type: SQLINTERVAL;
    interval_sign: SQLSMALLINT;
    case SQLINTERVAL of
      SQL_IS_YEAR_TO_MONTH: (year_month: SQL_YEAR_MONTH_STRUCT);
      SQL_IS_DAY_TO_HOUR,
      SQL_IS_DAY_TO_MINUTE,
      SQL_IS_DAY_TO_SECOND,
      SQL_IS_HOUR_TO_MINUTE,
      SQL_IS_HOUR_TO_SECOND,
      SQL_IS_MINUTE_TO_SECOND: (day_second: SQL_DAY_SECOND_STRUCT);
  end;

const
  SQL_MAX_NUMERIC_LEN = 16;

type
  SQL_NUMERIC_STRUCT = record
    precision:  SQLCHAR;
    scale:      SQLSCHAR;
    sign:       SQLCHAR; //* 1 if positive, 0 if negative */
    val:        array[0..SQL_MAX_NUMERIC_LEN-1] of SQLCHAR;
  end;
  {$A+}

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
  SQL_ATTR_OUTPUT_NTS = 10001;

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
  SQL_OV_ODBC3 = Pointer(3);
  SQL_OV_ODBC351 = Pointer(351);

{ new values for SQL_ATTR_ODBC_VERSION  }
{ From ODBC 3.8 onwards, we should use <major version> * 100 + <minor version> }
const
  SQL_OV_ODBC3_80 = Pointer(380); //(ODBCVER >= 0x0380)

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
  SQL_MODE_READ_WRITE = 0;
  SQL_MODE_READ_ONLY = 1;
  SQL_MODE_DEFAULT = SQL_MODE_READ_WRITE;

{ SQL_AUTOCOMMIT options  }
  SQL_AUTOCOMMIT_OFF = 0;
  SQL_AUTOCOMMIT_ON = 1;
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

type
  IODBC3BasePlainDriver = interface(IZPlainDriver)
    ['{1B4A3A81-78DF-4097-B715-1D692D35565A}']
    function AllocHandle(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE;
      var OutputHandle: SQLHANDLE): SQLRETURN;
    function BindCol(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_Ind: PSQLLEN): SQLRETURN;
    function BindParameter(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      InputOutputType: SQLSMALLINT; ValueType: SQLSMALLINT; ParameterType: SQLSMALLINT;
      ColumnSize: SQLULEN; DecimalDigits: SQLSMALLINT; ParameterValuePtr: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
    function BulkOperations(StatementHandle: SQLHSTMT; Operation: SQLUSMALLINT): SQLRETURN;
    function Cancel(StatementHandle: SQLHSTMT): SQLRETURN;
    function CancelHandle(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE): SQLRETURN;
    function CloseCursor(StatementHandle: SQLHSTMT): SQLRETURN;
    function CompleteAsync(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      var AsyncRetCodePtr: PRETCODE): SQLRETURN;
    function CopyDesc(SourceDescHandle: SQLHDESC; TargetDescHandle: SQLHDESC): SQLRETURN;
    function DescribeParam(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      DataTypePtr: PSQLSMALLINT; ParameterSizePtr: PSQLULEN;
      DecimalDigitsPtr: SQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function Disconnect(ConnectionHandle: SQLHDBC): SQLRETURN;
    function EndTran(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      CompletionType: SQLSMALLINT): SQLRETURN;
    function GetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function Execute(StatementHandle: SQLHSTMT): SQLRETURN;
    function Fetch(StatementHandle: SQLHSTMT): SQLRETURN;
    function FetchScroll(StatementHandle: SQLHSTMT; FetchOrientation: SQLSMALLINT;
      FetchOffset: SQLLEN): SQLRETURN;
    function FreeHandle(HandleType: SQLSMALLINT; Handle: SQLHANDLE): SQLRETURN;
    function FreeStmt(StatementHandle: SQLHSTMT; Option: SQLUSMALLINT): SQLRETURN;
    function GetData(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER; BufferLength: SQLLEN;
      StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
    function GetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN;
    function GetDiagField(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN;
    function GetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN;
    function GetFunctions(ConnectionHandle: SQLHDBC; FunctionId: SQLUSMALLINT;
      SupportedPtr: PSQLUSMALLINT): SQLRETURN;
    function GetInfo(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetTypeInfo(StatementHandle: SQLHSTMT; DataType: SQLSMALLINT): SQLRETURN;
    function GetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function MoreResults(StatementHandle: SQLHSTMT): SQLRETURN;
    function NumParams(StatementHandle: SQLHSTMT; ParameterCountPtr: PSQLSMALLINT): SQLRETURN;
    function NumResultCols(StatementHandle: SQLHSTMT; ColumnCountPtr: PSQLSMALLINT): SQLRETURN;
    function ParamData(StatementHandle: SQLHSTMT; ValuePtrPtr: PSQLPOINTER): SQLRETURN;
    function PutData(StatementHandle: SQLHSTMT; DataPtr: SQLPOINTER;
      StrLen_or_Ind: SQLLEN): SQLRETURN;
    function RowCount(StatementHandle: SQLHSTMT; RowCountPtr: PSQLLEN): SQLRETURN;
    function SetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN;
    function SetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLLEN; Precision: SQLSMALLINT;
      Scale: SQLSMALLINT; DataPtr: SQLPOINTER; StringLengthPtr: PSQLLEN;
      IndicatorPtr: PSQLLEN): SQLRETURN;
    function SetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetPos(StatementHandle: SQLHSTMT; RowNumber: SQLSETPOSIROW;
      Operation: SQLUSMALLINT; LockType: SQLUSMALLINT): SQLRETURN;
  end;

  IODBC3UnicodePlainDriver = interface(IODBC3BasePlainDriver)
    ['{40D3EDEF-B1A8-4151-A406-8E8B8F4BD1E4}']
    function BrowseConnect(ConnectionHandle: SQLHDBC; InConnectionString: PSQLWCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function ColAttribute(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColumnPrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Columns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Connect(ConnectionHandle: SQLHDBC;
      ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function DataSources(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLWCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DescribeCol(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DriverConnect(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function Drivers(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLWCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLWCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function ExecDirect(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function ForeignKeys(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLWCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLWCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLWCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function GetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN;
    function GetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      Name: PSQLWCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
      TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
      PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDiagRec(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLWCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function NativeSql(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function Prepare(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrimaryKeys(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProcedureColumns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Procedures(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function SetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SpecialColumns(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function Statistics(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function TablePrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function Tables(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
  end;

  IODBC3RawPlainDriver = interface(IODBC3BasePlainDriver)
    ['{6CE34B8A-CBC9-411F-8FC9-87E519762C95}']
    function BrowseConnect(ConnectionHandle: SQLHDBC; InConnectionString: PSQLCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function ColAttribute(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColumnPrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Columns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Connect(ConnectionHandle: SQLHDBC;
      ServerName: PSQLCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function DataSources(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DescribeCol(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DriverConnect(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function Drivers(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function ExecDirect(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function ForeignKeys(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function GetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDiagRec(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function NativeSql(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function Prepare(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrimaryKeys(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProcedureColumns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Procedures(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function SetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SpecialColumns(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function Statistics(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function TablePrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function Tables(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
  end;

  TODBC3BaseDriver = class(TZAbstractPlainDriver)
  private
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
      DecimalDigitsPtr: SQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDisconnect: function(ConnectionHandle: SQLHDBC): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
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
  private
    FDriverName: String;
  protected
    procedure LoadApi; override;
  public
    function GetProtocol: string; override;
    constructor Create(DriverName: String);
  public
    function AllocHandle(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE;
      var OutputHandle: SQLHANDLE): SQLRETURN;
    function BindCol(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_Ind: PSQLLEN): SQLRETURN;
    function BindParameter(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      InputOutputType: SQLSMALLINT; ValueType: SQLSMALLINT; ParameterType: SQLSMALLINT;
      ColumnSize: SQLULEN; DecimalDigits: SQLSMALLINT; ParameterValuePtr: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
    function BulkOperations(StatementHandle: SQLHSTMT; Operation: SQLUSMALLINT): SQLRETURN;
    function Cancel(StatementHandle: SQLHSTMT): SQLRETURN;
    function CancelHandle(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE): SQLRETURN;
    function CloseCursor(StatementHandle: SQLHSTMT): SQLRETURN;
    function CompleteAsync(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      var AsyncRetCodePtr: PRETCODE): SQLRETURN;
    function CopyDesc(SourceDescHandle: SQLHDESC; TargetDescHandle: SQLHDESC): SQLRETURN;
    function DescribeParam(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      DataTypePtr: PSQLSMALLINT; ParameterSizePtr: PSQLULEN;
      DecimalDigitsPtr: SQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function Disconnect(ConnectionHandle: SQLHDBC): SQLRETURN;
    function EndTran(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      CompletionType: SQLSMALLINT): SQLRETURN;
    function GetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN; virtual; abstract;
    function Execute(StatementHandle: SQLHSTMT): SQLRETURN;
    function Fetch(StatementHandle: SQLHSTMT): SQLRETURN;
    function FetchScroll(StatementHandle: SQLHSTMT; FetchOrientation: SQLSMALLINT;
      FetchOffset: SQLLEN): SQLRETURN;
    function FreeHandle(HandleType: SQLSMALLINT; Handle: SQLHANDLE): SQLRETURN;
    function FreeStmt(StatementHandle: SQLHSTMT; Option: SQLUSMALLINT): SQLRETURN;
    function GetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; virtual; abstract;
    function GetData(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER; BufferLength: SQLLEN;
      StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
    function GetDiagField(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; virtual; abstract;
    function GetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN;
    function GetFunctions(ConnectionHandle: SQLHDBC; FunctionId: SQLUSMALLINT;
      SupportedPtr: PSQLUSMALLINT): SQLRETURN;
    function GetInfo(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN;  virtual; abstract;
    function GetTypeInfo(StatementHandle: SQLHSTMT; DataType: SQLSMALLINT): SQLRETURN;
    function GetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; virtual; abstract;
    function MoreResults(StatementHandle: SQLHSTMT): SQLRETURN;
    function NumParams(StatementHandle: SQLHSTMT; ParameterCountPtr: PSQLSMALLINT): SQLRETURN;
    function NumResultCols(StatementHandle: SQLHSTMT; ColumnCountPtr: PSQLSMALLINT): SQLRETURN;
    function ParamData(StatementHandle: SQLHSTMT; ValuePtrPtr: PSQLPOINTER): SQLRETURN;
    function PutData(StatementHandle: SQLHSTMT; DataPtr: SQLPOINTER;
      StrLen_or_Ind: SQLLEN): SQLRETURN;
    function RowCount(StatementHandle: SQLHSTMT; RowCountPtr: PSQLLEN): SQLRETURN;
    function SetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;  virtual; abstract;
    function SetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN;  virtual; abstract;
    function SetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLLEN; Precision: SQLSMALLINT;
      Scale: SQLSMALLINT; DataPtr: SQLPOINTER; StringLengthPtr: PSQLLEN;
      IndicatorPtr: PSQLLEN): SQLRETURN;
    function SetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; virtual; abstract;
    function SetPos(StatementHandle: SQLHSTMT; RowNumber: SQLSETPOSIROW;
      Operation: SQLUSMALLINT; LockType: SQLUSMALLINT): SQLRETURN;
  end;

  TODBC3UnicodePlainDriver = class(TODBC3BaseDriver, IODBC3UnicodePlainDriver)
  private
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
    SQLConnectW: function (ConnectionHandle: SQLHDBC;
      ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDataSourcesW: function (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLWCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeColW: function(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDriverConnectW: function(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
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
    SQLGetCursorNameW: function(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescFieldW: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescRecW: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLWCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagFieldW: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagRecW: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLWCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetInfoW: function(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetStmtAttrW: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
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
    SQLSetConnectAttrW: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetCursorNameW: function(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      NameLength: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetDescFieldW: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetStmtAttrW: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
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
    constructor Create(DriverName: String = '_W');
  public
    function BrowseConnect(ConnectionHandle: SQLHDBC; InConnectionString: PSQLWCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function ColAttribute(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColumnPrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Columns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Connect(ConnectionHandle: SQLHDBC;
      ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function DataSources(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLWCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DescribeCol(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DriverConnect(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function Drivers(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLWCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLWCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function ExecDirect(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function ForeignKeys(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLWCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLWCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLWCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function GetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN; override;
    function GetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; override;
    function GetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLWCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDiagField(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; override;
    function GetDiagRec(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLWCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function GetInfo(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN; override;
    function GetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; override;
    function NativeSql(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function Prepare(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrimaryKeys(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProcedureColumns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Procedures(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function SetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; override;
    function SetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN; override;
    function SetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; override;
    function SpecialColumns(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function Statistics(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function TablePrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function Tables(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
   end;

  TODBC3RawPlainDriver = class(TODBC3BaseDriver, IODBC3RawPlainDriver)
  private
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
    SQLConnect: function (ConnectionHandle: SQLHDBC;
      ServerName: PSQLCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDataSources: function (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeCol: function(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDriverConnect: function(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
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
    SQLGetCursorName: function(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescField: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescRec: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagField: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagRec: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetInfo: function(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetStmtAttr: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
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
    SQLSetConnectAttr: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetCursorName: function(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      NameLength: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetDescField: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetStmtAttr: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
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
    constructor Create(DriverName: String = '_A');
  public
    function BrowseConnect(ConnectionHandle: SQLHDBC; InConnectionString: PSQLCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function ColAttribute(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColumnPrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Columns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Connect(ConnectionHandle: SQLHDBC;
      ServerName: PSQLCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function DataSources(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DescribeCol(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DriverConnect(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function Drivers(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function ExecDirect(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function ForeignKeys(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function GetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN; override;
    function GetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; override;
    function GetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDiagField(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; override;
    function GetDiagRec(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function GetInfo(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN; override;
    function GetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; override;
    function NativeSql(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function Prepare(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrimaryKeys(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProcedureColumns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Procedures(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function SetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; override;
    function SetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN; override;
    function SetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; override;
    function SpecialColumns(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function Statistics(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function TablePrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function Tables(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
   end;

implementation

uses ZPlainLoader;

{ TODBC3BaseDriver }

function TODBC3BaseDriver.AllocHandle(HandleType: SQLSMALLINT;
  InputHandle: SQLHANDLE; var OutputHandle: SQLHANDLE): SQLRETURN;
begin
  if not Assigned(SQLAllocHandle) then
    Result := SQL_ERROR
  else
    Result := SQLAllocHandle(HandleType, InputHandle, OutputHandle);
end;

function TODBC3BaseDriver.BindCol(StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT; TargetType: SQLSMALLINT; TargetValue: SQLPOINTER;
  BufferLength: SQLLEN; StrLen_or_Ind: PSQLLEN): SQLRETURN;
begin
  Result := SQLBindCol(StatementHandle, ColumnNumber, TargetType, TargetValue,
    BufferLength, StrLen_or_Ind);
end;

function TODBC3BaseDriver.BindParameter(StatementHandle: SQLHSTMT;
  ParameterNumber: SQLUSMALLINT; InputOutputType, ValueType,
  ParameterType: SQLSMALLINT; ColumnSize: SQLULEN; DecimalDigits: SQLSMALLINT;
  ParameterValuePtr: SQLPOINTER; BufferLength: SQLLEN;
  StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLBindParameter(StatementHandle, ParameterNumber, InputOutputType,
    ValueType, ParameterType, ColumnSize, DecimalDigits, ParameterValuePtr,
    BufferLength, StrLen_or_IndPtr);
end;

function TODBC3BaseDriver.BulkOperations(StatementHandle: SQLHSTMT;
  Operation: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLBulkOperations(StatementHandle, Operation);
end;

function TODBC3BaseDriver.Cancel(StatementHandle: SQLHSTMT): SQLRETURN;
begin
  Result := SQLCancel(StatementHandle);
end;

function TODBC3BaseDriver.CancelHandle(HandleType: SQLSMALLINT;
  InputHandle: SQLHANDLE): SQLRETURN;
begin
  Result := SQLCancelHandle(HandleType, InputHandle);
end;

function TODBC3BaseDriver.CloseCursor(StatementHandle: SQLHSTMT): SQLRETURN;
begin
  Result := SQLCloseCursor(StatementHandle);
end;

function TODBC3BaseDriver.CompleteAsync(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; var AsyncRetCodePtr: PRETCODE): SQLRETURN;
begin
  Result := SQLCompleteAsync(HandleType, Handle, AsyncRetCodePtr);
end;

function TODBC3BaseDriver.CopyDesc(SourceDescHandle,
  TargetDescHandle: SQLHDESC): SQLRETURN;
begin
  Result := SQLCopyDesc(SourceDescHandle, TargetDescHandle);
end;

constructor TODBC3BaseDriver.Create(DriverName: String);
begin
  inherited Create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IF defined(Unix) or defined (MSWINDOWS)}
  FLoader.AddLocation(ODBC_LOCATION);
  {$IFEND}
  FDriverName := 'ODBC'+DriverName
end;

function TODBC3BaseDriver.DescribeParam(StatementHandle: SQLHSTMT;
  ParameterNumber: SQLUSMALLINT; DataTypePtr: PSQLSMALLINT;
  ParameterSizePtr: PSQLULEN; DecimalDigitsPtr: SQLSMALLINT;
  NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDescribeParam(StatementHandle, ParameterNumber, DataTypePtr,
    ParameterSizePtr, DecimalDigitsPtr, NullablePtr)
end;

function TODBC3BaseDriver.Disconnect(ConnectionHandle: SQLHDBC): SQLRETURN;
begin
  Result := SQLDisconnect(ConnectionHandle);
end;

function TODBC3BaseDriver.EndTran(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
  CompletionType: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLEndTran(HandleType, Handle, CompletionType);
end;

function TODBC3BaseDriver.Execute(StatementHandle: SQLHSTMT): SQLRETURN;
begin
  Result := SQLExecute(StatementHandle);
end;

function TODBC3BaseDriver.Fetch(StatementHandle: SQLHSTMT): SQLRETURN;
begin
  Result := SQLFetch(StatementHandle);
end;

function TODBC3BaseDriver.FetchScroll(StatementHandle: SQLHSTMT;
  FetchOrientation: SQLSMALLINT; FetchOffset: SQLLEN): SQLRETURN;
begin
  Result := SQLFetchScroll(StatementHandle, FetchOrientation, FetchOffset);
end;

function TODBC3BaseDriver.FreeHandle(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE): SQLRETURN;
begin
  Result := SQLFreeHandle(HandleType, Handle);
end;

function TODBC3BaseDriver.FreeStmt(StatementHandle: SQLHSTMT;
  Option: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLFreeStmt(StatementHandle, Option);
end;

function TODBC3BaseDriver.GetData(StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT; TargetType: SQLSMALLINT; TargetValue: SQLPOINTER;
  BufferLength: SQLLEN; StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLGetData(StatementHandle, ColumnNumber, TargetType, TargetValue,
    BufferLength, StrLen_or_IndPtr);
end;

function TODBC3BaseDriver.GetEnvAttr(EnvironmentHandle: SQLHENV;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetEnvAttr(EnvironmentHandle, Attribute, ValuePtr, BufferLength,
    StringLength);
end;

function TODBC3BaseDriver.GetFunctions(ConnectionHandle: SQLHDBC;
  FunctionId: SQLUSMALLINT; SupportedPtr: PSQLUSMALLINT): SQLRETURN;
begin
  Result := SQLGetFunctions(ConnectionHandle, FunctionId, SupportedPtr);
end;

function TODBC3BaseDriver.GetProtocol: string;
begin
  Result := FDriverName;
end;

function TODBC3BaseDriver.GetTypeInfo(StatementHandle: SQLHSTMT;
  DataType: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetTypeInfo(StatementHandle, DataType);
end;

procedure TODBC3BaseDriver.LoadApi;
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
  @SQLCopyDesc              := GetAddress('SQLCopyDesc');
  @SQLDescribeParam         := GetAddress('SQLDescribeParam');
  @SQLDisconnect            := GetAddress('SQLDisconnect');
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

function TODBC3BaseDriver.MoreResults(StatementHandle: SQLHSTMT): SQLRETURN;
begin
  Result := SQLMoreResults(StatementHandle);
end;

function TODBC3BaseDriver.NumParams(StatementHandle: SQLHSTMT;
  ParameterCountPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLNumParams(StatementHandle, ParameterCountPtr);
end;

function TODBC3BaseDriver.NumResultCols(StatementHandle: SQLHSTMT;
  ColumnCountPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLNumResultCols(StatementHandle, ColumnCountPtr);
end;

function TODBC3BaseDriver.ParamData(StatementHandle: SQLHSTMT;
  ValuePtrPtr: PSQLPOINTER): SQLRETURN;
begin
  Result := SQLParamData(StatementHandle, ValuePtrPtr);
end;

function TODBC3BaseDriver.PutData(StatementHandle: SQLHSTMT;
  DataPtr: SQLPOINTER; StrLen_or_Ind: SQLLEN): SQLRETURN;
begin
  Result := SQLPutData(StatementHandle, DataPtr, StrLen_or_Ind);
end;

function TODBC3BaseDriver.RowCount(StatementHandle: SQLHSTMT;
  RowCountPtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLRowCount(StatementHandle, RowCountPtr);
end;

function TODBC3BaseDriver.SetDescRec(DescriptorHandle: SQLHDESC; RecNumber,
  _Type, SubType: SQLSMALLINT; Length: SQLLEN; Precision, Scale: SQLSMALLINT;
  DataPtr: SQLPOINTER; StringLengthPtr, IndicatorPtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLSetDescRec(DescriptorHandle, RecNumber, _Type, SubType, Length,
    Precision, Scale, DataPtr, StringLengthPtr, IndicatorPtr);
end;

function TODBC3BaseDriver.SetEnvAttr(EnvironmentHandle: SQLHENV;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetEnvAttr(EnvironmentHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3BaseDriver.SetPos(StatementHandle: SQLHSTMT;
  RowNumber: SQLSETPOSIROW; Operation, LockType: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLSetPos(StatementHandle, RowNumber, Operation, LockType);
end;

{ TODBC3UnicodePlainDriver }

function TODBC3UnicodePlainDriver.BrowseConnect(ConnectionHandle: SQLHDBC;
  InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
  OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
  StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLBrowseConnectW(ConnectionHandle, InConnectionString, StringLength1,
    OutConnectionString, BufferLength, StringLength2Ptr);
end;

function TODBC3UnicodePlainDriver.Clone: IZPlainDriver;
begin
  Result := TODBC3UnicodePlainDriver.Create;
end;

function TODBC3UnicodePlainDriver.ColAttribute(StatementHandle: SQLHSTMT; ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
  BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
  NumericAttributePtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLColAttributeW(StatementHandle, ColumnNumber, FieldIdentifier,
    CharacterAttributePtr, BufferLength, StringLengthPtr, NumericAttributePtr);
end;

function TODBC3UnicodePlainDriver.ColumnPrivileges(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumnPrivilegesW(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3UnicodePlainDriver.Columns(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumnsW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;
function TODBC3UnicodePlainDriver.Connect(ConnectionHandle: SQLHDBC;
  ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT; UserName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; Authentication: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLConnectW(ConnectionHandle, ServerName, NameLength1, UserName,
    NameLength2, Authentication, NameLength3);
end;

constructor TODBC3UnicodePlainDriver.Create(DriverName: String = '_W');
begin
  inherited Create(DriverName)
end;

function TODBC3UnicodePlainDriver.DataSources(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT;
  NameLength1Ptr: PSQLSMALLINT; Description: PSQLWCHAR;
  BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDataSourcesW(EnvironmentHandle, Direction, ServerName,
    BufferLength1, NameLength1Ptr, Description, BufferLength2, NameLength2Ptr);
end;

function TODBC3UnicodePlainDriver.DescribeCol(StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr, DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
  DecimalDigitsPtr, NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDescribeColW(StatementHandle, ColumnNumber, ColumnName, BufferLength,
    NameLengthPtr, DataTypePtr, ColumnSizePtr, DecimalDigitsPtr, NullablePtr);
end;

function TODBC3UnicodePlainDriver.DriverConnect(ConnectionHandle: SQLHDBC;
  WindowHandle: SQLHWND; InConnectionString: PSQLWCHAR;
  StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
  BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT;
  DriverCompletion: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLDriverConnectW(ConnectionHandle, WindowHandle, InConnectionString,
    StringLength1, OutConnectionString, BufferLength, StringLength2Ptr, DriverCompletion);
end;

function TODBC3UnicodePlainDriver.Drivers(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; DriverDescription: PSQLWCHAR;
  BufferLength1: SQLSMALLINT; DescriptionLengthPtr: PSQLSMALLINT;
  DriverAttributes: PSQLWCHAR; BufferLength2: SQLSMALLINT;
  AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDriversW(EnvironmentHandle, Direction, DriverDescription,
    BufferLength1, DescriptionLengthPtr, DriverAttributes, BufferLength2,
    AttributesLengthPtr);
end;

function TODBC3UnicodePlainDriver.ExecDirect(StatementHandle: SQLHSTMT;
  StatementText: PSQLWCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLExecDirectW(StatementHandle, StatementText, TextLength);
end;

function TODBC3UnicodePlainDriver.ForeignKeys(StatementHandle: SQLHSTMT;
  PKCatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; PKSchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; PKTableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  FKCatalogName: PSQLWCHAR; NameLength4: SQLSMALLINT; FKSchemaName: PSQLWCHAR;
  NameLength5: SQLSMALLINT; FKTableName: PSQLWCHAR;
  NameLength6: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLForeignKeysW(StatementHandle, PKCatalogName, NameLength1,
    PKSchemaName, NameLength2, PKTableName, NameLength3, FKCatalogName,
    NameLength4, FKSchemaName, NameLength5, FKTableName, NameLength6);
end;

function TODBC3UnicodePlainDriver.GetConnectAttr(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; Value: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetConnectAttrW(ConnectionHandle, Attribute, Value, BufferLength,
    StringLengthPtr);
end;

function TODBC3UnicodePlainDriver.GetCursorName(StatementHandle: SQLHSTMT;
  CursorName: PSQLWCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetCursorNameW(StatementHandle, CursorName, BufferLength,
    NameLengthPtr);
end;

function TODBC3UnicodePlainDriver.GetDescField(DescriptorHandle: SQLHDESC;
  RecNumber, FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
  BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetDescFieldW(DescriptorHandle, RecNumber, FieldIdentifier,
    Value, BufferLength, StringLength);
end;

function TODBC3UnicodePlainDriver.GetDescRec(DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT; Name: PSQLWCHAR; BufferLength: SQLSMALLINT;
  StringLengthPtr, TypePtr, SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
  PrecisionPtr, ScalePtr, NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDescRecW(DescriptorHandle, RecNumber, Name, BufferLength,
    StringLengthPtr, TypePtr, SubTypePtr, LengthPtr, PrecisionPtr, ScalePtr, NullablePtr)
end;

function TODBC3UnicodePlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ODBC Unicode';
end;

function TODBC3UnicodePlainDriver.GetDiagField(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; RecNumber, DiagIdentifier: SQLSMALLINT;
  DiagInfo: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagFieldW(HandleType, Handle, RecNumber, DiagIdentifier,
    DiagInfo, BufferLength, StringLength);
end;

function TODBC3UnicodePlainDriver.GetDiagRec(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR;
  NativeErrorPtr: PSQLINTEGER; MessageText: PSQLWCHAR;
  BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagRecW(HandleType, Handle, RecNumber, Sqlstate, NativeErrorPtr,
    MessageText, BufferLength, TextLength)
end;

function TODBC3UnicodePlainDriver.GetInfo(ConnectionHandle: SQLHDBC;
  InfoType: SQLUSMALLINT; InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetInfoW(ConnectionHandle, InfoType, InfoValuePtr, BufferLength,
    StringLengthPtr);
end;

function TODBC3UnicodePlainDriver.GetStmtAttr(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetStmtAttrW(StatementHandle, Attribute, ValuePtr, BufferLength,
    StringLengthPtr);
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
  @SQLConnectW              := GetAddress('SQLConnectW');
  @SQLDataSourcesW          := GetAddress('SQLDataSourcesW');
  @SQLDescribeColW          := GetAddress('SQLDescribeColW');
  @SQLDriverConnectW        := GetAddress('SQLDriverConnectW');
  @SQLDriversW              := GetAddress('SQLDriversW');
  @SQLExecDirectW           := GetAddress('SQLExecDirectW');
  @SQLForeignKeysW          := GetAddress('SQLForeignKeysW');
  @SQLGetConnectAttrW       := GetAddress('SQLGetConnectAttrW');
  @SQLGetCursorNameW        := GetAddress('SQLGetCursorNameW');
  @SQLGetDescFieldW         := GetAddress('SQLGetDescFieldW');
  @SQLGetDescRecW           := GetAddress('SQLGetDescRecW');
  @SQLGetDiagRecW           := GetAddress('SQLGetDiagRecW');
  @SQLGetInfoW              := GetAddress('SQLGetInfoW');
  @SQLGetStmtAttrW          := GetAddress('SQLGetStmtAttrW');
  @SQLNativeSqlW            := GetAddress('SQLNativeSqlW');
  @SQLPrepareW              := GetAddress('SQLPrepareW');
  @SQLPrimaryKeysW          := GetAddress('SQLPrimaryKeysW');
  @SQLProcedureColumnsW     := GetAddress('SQLProcedureColumnsW');
  @SQLProceduresW           := GetAddress('SQLProceduresW');
  @SQLSetConnectAttrW       := GetAddress('SQLSetConnectAttrW');
  @SQLSetCursorNameW        := GetAddress('SQLSetCursorNameW');
  @SQLSetDescFieldW         := GetAddress('SQLSetDescFieldW');
  @SQLSetStmtAttrW          := GetAddress('SQLSetStmtAttrW');
  @SQLSpecialColumnsW       := GetAddress('SQLSpecialColumnsW');
  @SQLStatisticsW           := GetAddress('SQLStatisticsW');
  @SQLTablePrivilegesW      := GetAddress('SQLTablePrivilegesW');
  @SQLTablesW               := GetAddress('SQLTablesW');
  end;
end;

procedure TODBC3UnicodePlainDriver.LoadCodePages;
begin
  AddCodePage('CP_UTF16', 0, ceUTF16, ZDefaultSystemCodePage , '', 1, False);
end;

function TODBC3UnicodePlainDriver.NativeSql(ConnectionHandle: SQLHDBC;
  InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
  OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
  TextLength2Ptr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLNativeSqlW(ConnectionHandle, InStatementText, TextLength1,
    OutStatementText, BufferLength, TextLength2Ptr);
end;

function TODBC3UnicodePlainDriver.Prepare(StatementHandle: SQLHSTMT;
  StatementText: PSQLWCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLPrepareW(StatementHandle, StatementText, TextLength);
end;

function TODBC3UnicodePlainDriver.PrimaryKeys(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLPrimaryKeysW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3);
end;

function TODBC3UnicodePlainDriver.ProcedureColumns(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProcedureColumnsW(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, ProcName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3UnicodePlainDriver.Procedures(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength1: SQLSMALLINT; ProcName: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProceduresW(StatementHandle, CatalogName, NameLength2, SchemaName,
    NameLength1, ProcName, NameLength3);
end;

function TODBC3UnicodePlainDriver.SetConnectAttr(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetConnectAttrW(ConnectionHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3UnicodePlainDriver.SetCursorName(StatementHandle: SQLHSTMT;
  CursorName: PSQLWCHAR; NameLength: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSetCursorNameW(StatementHandle, CursorName, NameLength);
end;

function TODBC3UnicodePlainDriver.SetDescField(DescriptorHandle: SQLHDESC; RecNumber,
  FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetDescFieldW(DescriptorHandle, RecNumber, FieldIdentifier,
    ValuePtr, BufferLength);
end;

function TODBC3UnicodePlainDriver.SetStmtAttr(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetStmtAttrW(StatementHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3UnicodePlainDriver.SpecialColumns(StatementHandle: SQLHSTMT;
  IdentifierType: SQLSMALLINT; CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
  SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT; TableName: PSQLWCHAR;
  NameLength3, Scope, Nullable: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSpecialColumnsW(StatementHandle, IdentifierType, CatalogName,
    NameLength1, SchemaName, NameLength2, TableName, NameLength3, Scope, Nullable);
end;

function TODBC3UnicodePlainDriver.Statistics(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  Unique, Reserved: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLStatisticsW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, Unique, Reserved);
end;

function TODBC3UnicodePlainDriver.TablePrivileges(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTablePrivilegesW(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3);
end;

function TODBC3UnicodePlainDriver.Tables(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTablesW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, TableType, NameLength4);
end;

{ TODBC3RawPlainDriver }

function TODBC3RawPlainDriver.BrowseConnect(ConnectionHandle: SQLHDBC;
  InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
  OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
  StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLBrowseConnect(ConnectionHandle, InConnectionString, StringLength1,
    OutConnectionString, BufferLength, StringLength2Ptr);
end;

function TODBC3RawPlainDriver.Clone: IZPlainDriver;
begin
  Result := TODBC3RawPlainDriver.Create;
end;

function TODBC3RawPlainDriver.ColAttribute(StatementHandle: SQLHSTMT; ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
  BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
  NumericAttributePtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLColAttribute(StatementHandle, ColumnNumber, FieldIdentifier,
    CharacterAttributePtr, BufferLength, StringLengthPtr, NumericAttributePtr);
end;

function TODBC3RawPlainDriver.ColumnPrivileges(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumnPrivileges(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3RawPlainDriver.Columns(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumns(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;
function TODBC3RawPlainDriver.Connect(ConnectionHandle: SQLHDBC;
  ServerName: PSQLCHAR; NameLength1: SQLSMALLINT; UserName: PSQLCHAR;
  NameLength2: SQLSMALLINT; Authentication: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLConnect(ConnectionHandle, ServerName, NameLength1, UserName,
    NameLength2, Authentication, NameLength3);
end;

constructor TODBC3RawPlainDriver.Create(DriverName: String = '_A');
begin
  inherited Create(DriverName);
end;

function TODBC3RawPlainDriver.DataSources(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT;
  NameLength1Ptr: PSQLSMALLINT; Description: PSQLCHAR;
  BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDataSources(EnvironmentHandle, Direction, ServerName,
    BufferLength1, NameLength1Ptr, Description, BufferLength2, NameLength2Ptr);
end;

function TODBC3RawPlainDriver.DescribeCol(StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr, DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
  DecimalDigitsPtr, NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDescribeCol(StatementHandle, ColumnNumber, ColumnName, BufferLength,
    NameLengthPtr, DataTypePtr, ColumnSizePtr, DecimalDigitsPtr, NullablePtr);
end;

function TODBC3RawPlainDriver.DriverConnect(ConnectionHandle: SQLHDBC;
  WindowHandle: SQLHWND; InConnectionString: PSQLCHAR;
  StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
  BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT;
  DriverCompletion: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLDriverConnect(ConnectionHandle, WindowHandle, InConnectionString,
    StringLength1, OutConnectionString, BufferLength, StringLength2Ptr, DriverCompletion);
end;

function TODBC3RawPlainDriver.Drivers(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; DriverDescription: PSQLCHAR;
  BufferLength1: SQLSMALLINT; DescriptionLengthPtr: PSQLSMALLINT;
  DriverAttributes: PSQLCHAR; BufferLength2: SQLSMALLINT;
  AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDrivers(EnvironmentHandle, Direction, DriverDescription,
    BufferLength1, DescriptionLengthPtr, DriverAttributes, BufferLength2,
    AttributesLengthPtr);
end;

function TODBC3RawPlainDriver.ExecDirect(StatementHandle: SQLHSTMT;
  StatementText: PSQLCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLExecDirect(StatementHandle, StatementText, TextLength);
end;

function TODBC3RawPlainDriver.ForeignKeys(StatementHandle: SQLHSTMT;
  PKCatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; PKSchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; PKTableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  FKCatalogName: PSQLCHAR; NameLength4: SQLSMALLINT; FKSchemaName: PSQLCHAR;
  NameLength5: SQLSMALLINT; FKTableName: PSQLCHAR;
  NameLength6: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLForeignKeys(StatementHandle, PKCatalogName, NameLength1,
    PKSchemaName, NameLength2, PKTableName, NameLength3, FKCatalogName,
    NameLength4, FKSchemaName, NameLength5, FKTableName, NameLength6);
end;

function TODBC3RawPlainDriver.GetConnectAttr(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; Value: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetConnectAttr(ConnectionHandle, Attribute, Value, BufferLength,
    StringLengthPtr);
end;

function TODBC3RawPlainDriver.GetCursorName(StatementHandle: SQLHSTMT;
  CursorName: PSQLCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetCursorName(StatementHandle, CursorName, BufferLength,
    NameLengthPtr);
end;

function TODBC3RawPlainDriver.GetDescField(DescriptorHandle: SQLHDESC;
  RecNumber, FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
  BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetDescField(DescriptorHandle, RecNumber, FieldIdentifier, Value,
    BufferLength, StringLength);
end;

function TODBC3RawPlainDriver.GetDescRec(DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT; Name: PSQLCHAR; BufferLength: SQLSMALLINT;
  StringLengthPtr, TypePtr, SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
  PrecisionPtr, ScalePtr, NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDescRec(DescriptorHandle, RecNumber, Name, BufferLength,
    StringLengthPtr, TypePtr, SubTypePtr, LengthPtr, PrecisionPtr, ScalePtr, NullablePtr)
end;

function TODBC3RawPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ODBC raw';
end;

function TODBC3RawPlainDriver.GetDiagField(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; RecNumber, DiagIdentifier: SQLSMALLINT;
  DiagInfo: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagField(HandleType, Handle, RecNumber, DiagIdentifier,
    DiagInfo, BufferLength, StringLength);
end;

function TODBC3RawPlainDriver.GetDiagRec(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR;
  NativeErrorPtr: PSQLINTEGER; MessageText: PSQLCHAR;
  BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagRec(HandleType, Handle, RecNumber, Sqlstate, NativeErrorPtr,
    MessageText, BufferLength, TextLength)
end;

function TODBC3RawPlainDriver.GetInfo(ConnectionHandle: SQLHDBC;
  InfoType: SQLUSMALLINT; InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetInfo(ConnectionHandle, InfoType, InfoValuePtr, BufferLength,
    StringLengthPtr);
end;

function TODBC3RawPlainDriver.GetStmtAttr(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetStmtAttr(StatementHandle, Attribute, ValuePtr, BufferLength,
    StringLengthPtr);
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
  @SQLConnect               := GetAddress('SQLConnect');
  @SQLDataSources           := GetAddress('SQLDataSources');
  @SQLDescribeCol           := GetAddress('SQLDescribeCol');
  @SQLDriverConnect         := GetAddress('SQLDriverConnect');
  @SQLDrivers               := GetAddress('SQLDrivers');
  @SQLExecDirect            := GetAddress('SQLExecDirect');
  @SQLForeignKeys           := GetAddress('SQLForeignKeys');
  @SQLGetConnectAttr        := GetAddress('SQLGetConnectAttr');
  @SQLGetCursorName         := GetAddress('SQLGetCursorName');
  @SQLGetDescRec            := GetAddress('SQLGetDescRec');
  @SQLGetDescField          := GetAddress('SQLGetDescField');
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
  AddCodePage('CP_ACP', 0, ceAnsi, ZDefaultSystemCodePage , '', 1, False);
end;

function TODBC3RawPlainDriver.NativeSql(ConnectionHandle: SQLHDBC;
  InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
  OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
  TextLength2Ptr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLNativeSql(ConnectionHandle, InStatementText, TextLength1,
    OutStatementText, BufferLength, TextLength2Ptr);
end;

function TODBC3RawPlainDriver.Prepare(StatementHandle: SQLHSTMT;
  StatementText: PSQLCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLPrepare(StatementHandle, StatementText, TextLength);
end;

function TODBC3RawPlainDriver.PrimaryKeys(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLPrimaryKeys(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3);
end;

function TODBC3RawPlainDriver.ProcedureColumns(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProcedureColumns(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, ProcName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3RawPlainDriver.Procedures(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength1: SQLSMALLINT; ProcName: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProcedures(StatementHandle, CatalogName, NameLength2, SchemaName,
    NameLength1, ProcName, NameLength3);
end;

function TODBC3RawPlainDriver.SetConnectAttr(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetConnectAttr(ConnectionHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3RawPlainDriver.SetCursorName(StatementHandle: SQLHSTMT;
  CursorName: PSQLCHAR; NameLength: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSetCursorName(StatementHandle, CursorName, NameLength);
end;

function TODBC3RawPlainDriver.SetDescField(DescriptorHandle: SQLHDESC; RecNumber,
  FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetDescField(DescriptorHandle, RecNumber, FieldIdentifier,
    ValuePtr, BufferLength);
end;

function TODBC3RawPlainDriver.SetStmtAttr(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetStmtAttr(StatementHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3RawPlainDriver.SpecialColumns(StatementHandle: SQLHSTMT;
  IdentifierType: SQLSMALLINT; CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
  SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT; TableName: PSQLCHAR;
  NameLength3, Scope, Nullable: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSpecialColumns(StatementHandle, IdentifierType, CatalogName,
    NameLength1, SchemaName, NameLength2, TableName, NameLength3, Scope, Nullable);
end;

function TODBC3RawPlainDriver.Statistics(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  Unique, Reserved: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLStatistics(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, Unique, Reserved);
end;

function TODBC3RawPlainDriver.TablePrivileges(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTablePrivileges(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3);
end;

function TODBC3RawPlainDriver.Tables(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTables(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, TableType, NameLength4);
end;

end.
