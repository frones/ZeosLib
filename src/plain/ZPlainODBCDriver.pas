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
//{$if (ODBCVER >= 0x0380)}
  SQL_PARAM_DATA_AVAILABLE = 101;
//{$ifend}

const
  SQL_ERROR = -(1);
  SQL_INVALID_HANDLE = -(2);
  SQL_STILL_EXECUTING = 2;
  SQL_NEED_DATA = 99;
{ test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }

//#define SQL_SUCCEEDED(rc)  (((rc)&(~1))==0)
//function SQL_SUCCEEDED(rc : longint) : longint;

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
//{$if (ODBCVER >= 0x0380)}
const
  SQL_API_SQLCANCELHANDLE = 1550;
  SQL_API_SQLCOMPLETEASYNC = 1551;
//{$ifend}

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
  SQL_MAX_DSN_LENGTH = 32;  // maximum data source name size

  SQL_MAX_OPTION_STRING_LENGTH  =  256;


type
  IODBC3BaseDriver = interface(IZPlainDriver)
    ['{40D3EDEF-B1A8-4151-A406-8E8B8F4BD1E4}']
    function AllocHandle(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE;
      var OutputHandle: SQLHANDLE): SQLRETURN;
    function BindCol(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_Ind: PSQLLEN): SQLRETURN;
    function BindParameter(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      InputOutputType: SQLSMALLINT; ValueType: SQLSMALLINT; ParameterType: SQLSMALLINT;
      ColumnSize: SQLULEN; DecimalDigits: SQLSMALLINT; ParameterValuePtr: SQLPOINTER;
      BufferLength: SQLLEN; StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
    function BrowseConnect(ConnectionHandle: SQLHDBC; InConnectionString: PSQLCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function BrowseConnectW(ConnectionHandle: SQLHDBC; InConnectionString: PSQLWCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function BulkOperations(StatementHandle: SQLHSTMT; Operation: SQLUSMALLINT): SQLRETURN;
    function Cancel(StatementHandle: SQLHSTMT): SQLRETURN;
    function CancelHandle(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE): SQLRETURN;
    function CloseCursor(StatementHandle: SQLHSTMT): SQLRETURN;
    function ColAttribute(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColAttributeW(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColumnPrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function ColumnPrivilegesW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Columns (StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function ColumnsW (StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function CompleteAsync(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      var AsyncRetCodePtr: PRETCODE): SQLRETURN;
    function Connect (ConnectionHandle: SQLHDBC;
      ServerName: PSQLCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ConnectW (ConnectionHandle: SQLHDBC;
      ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function CopyDesc (SourceDescHandle: SQLHDESC; TargetDescHandle: SQLHDESC): SQLRETURN;
    function DataSources (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DataSourcesW (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLWCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DescribeCol(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DescribeColW(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DescribeParam(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      DataTypePtr: PSQLSMALLINT; ParameterSizePtr: PSQLULEN;
      DecimalDigitsPtr: SQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function Disconnect(ConnectionHandle: SQLHDBC): SQLRETURN;
    function DriverConnect(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function DriverConnectW(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function Drivers(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function DriversW(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLWCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLWCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function EndTran(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      CompletionType: SQLSMALLINT): SQLRETURN;
    function ExecDirect(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function ExecDirectW(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function Execute(StatementHandle: SQLHSTMT): SQLRETURN;
    function Fetch(StatementHandle: SQLHSTMT): SQLRETURN;
    function FetchScroll(StatementHandle: SQLHSTMT; FetchOrientation: SQLSMALLINT;
      FetchOffset: SQLLEN): SQLRETURN;
    function ForeignKeys(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function ForeignKeysW(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLWCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLWCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLWCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function FreeHandle(HandleType: SQLSMALLINT; Handle: SQLHANDLE): SQLRETURN;
    function FreeStmt(StatementHandle: SQLHSTMT; Option: SQLUSMALLINT): SQLRETURN;
    function GetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetConnectAttrW(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetCursorNameW(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetData(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER; BufferLength: SQLLEN;
      StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
    function GetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN;
    function GetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDescRecW(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLWCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDiagField(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN;
    function GetDiagFieldW(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN;
    function GetDiagRec(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function GetDiagRecW(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLWCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function GetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN;
    function GetFunctions(ConnectionHandle: SQLHDBC; FunctionId: SQLUSMALLINT;
      SupportedPtr: PSQLUSMALLINT): SQLRETURN;
    function GetInfo(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetInfoW(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetStmtAttrW(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetTypeInfo(StatementHandle: SQLHSTMT; DataType: SQLSMALLINT): SQLRETURN;
    function MoreResults(StatementHandle: SQLHSTMT): SQLRETURN;
    function NativeSql(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function NativeSqlW(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function NumParams(StatementHandle: SQLHSTMT; ParameterCountPtr: PSQLSMALLINT): SQLRETURN;
    function NumResultCols(StatementHandle: SQLHSTMT; ColumnCountPtr: PSQLSMALLINT): SQLRETURN;
    function ParamData(StatementHandle: SQLHSTMT; ValuePtrPtr: PSQLPOINTER): SQLRETURN;
    function Prepare(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrepareW(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrimaryKeys(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function PrimaryKeysW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProcedureColumns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function ProcedureColumnsW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Procedures(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProceduresW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function PutData(StatementHandle: SQLHSTMT; DataPtr: SQLPOINTER;
      StrLen_or_Ind: SQLLEN): SQLRETURN;
    function RowCount(StatementHandle: SQLHSTMT; RowCountPtr: PSQLLEN): SQLRETURN;
    function SetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetConnectAttrW(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SetCursorNameW(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN;
    function SetDescFieldW(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN;
    function SetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLLEN; Precision: SQLSMALLINT;
      Scale: SQLSMALLINT; DataPtr: SQLPOINTER; StringLengthPtr: PSQLLEN;
      IndicatorPtr: PSQLLEN): SQLRETURN;
    function SetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetPos(StatementHandle: SQLHSTMT; RowNumber: SQLSETPOSIROW;
      Operation: SQLUSMALLINT; LockType: SQLUSMALLINT): SQLRETURN;
    function SetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetStmtAttrW(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SpecialColumns(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function SpecialColumnsW(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function Statistics(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function StatisticsW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function TablePrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function TablePrivilegesW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function Tables(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function TablesW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
  end;

  TODBC3BaseDriver = class (TZAbstractPlainDriver, IODBC3BaseDriver)
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
    SQLBrowseConnect: function(ConnectionHandle: SQLHDBC; InConnectionString: PSQLCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLBrowseConnectW: function(ConnectionHandle: SQLHDBC; InConnectionString: PSQLWCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLBulkOperations: function(StatementHandle: SQLHSTMT; Operation: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCancel: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCancelHandle: function(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCloseCursor: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColAttribute: function(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColAttributeW: function(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumnPrivileges: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumnPrivilegesW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumns: function (StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLColumnsW: function (StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCompleteAsync: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      var AsyncRetCodePtr: PRETCODE): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLConnect: function (ConnectionHandle: SQLHDBC;
      ServerName: PSQLCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLConnectW: function (ConnectionHandle: SQLHDBC;
      ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLCopyDesc: function (SourceDescHandle: SQLHDESC; TargetDescHandle: SQLHDESC): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDataSources: function (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDataSourcesW: function (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLWCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeCol: function(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeColW: function(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDescribeParam: function(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      DataTypePtr: PSQLSMALLINT; ParameterSizePtr: PSQLULEN;
      DecimalDigitsPtr: SQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDisconnect: function(ConnectionHandle: SQLHDBC): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDriverConnect: function(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDriverConnectW: function(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDrivers: function(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLDriversW: function(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLWCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLWCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLEndTran: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      CompletionType: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLExecDirect: function(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLExecDirectW: function(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLExecute: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFetch: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFetchScroll: function(StatementHandle: SQLHSTMT; FetchOrientation: SQLSMALLINT;
      FetchOffset: SQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLForeignKeys: function(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLCHAR; NameLength6: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLForeignKeysW: function(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLWCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLWCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLWCHAR; NameLength6: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFreeHandle: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLFreeStmt: function(StatementHandle: SQLHSTMT; Option: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetConnectAttr: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetConnectAttrW: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetCursorName: function(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetCursorNameW: function(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetData: function(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER; BufferLength: SQLLEN;
      StrLen_or_IndPtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescField: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescRec: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDescRecW: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLWCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagField: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagFieldW: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagRec: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetDiagRecW: function(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLWCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetEnvAttr: function(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetFunctions: function(ConnectionHandle: SQLHDBC; FunctionId: SQLUSMALLINT;
      SupportedPtr: PSQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetInfo: function(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetInfoW: function(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetStmtAttr: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetStmtAttrW: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLGetTypeInfo: function(StatementHandle: SQLHSTMT; DataType: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLMoreResults: function(StatementHandle: SQLHSTMT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNativeSql: function(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNativeSqlW: function(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNumParams: function(StatementHandle: SQLHSTMT; ParameterCountPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLNumResultCols: function(StatementHandle: SQLHSTMT; ColumnCountPtr: PSQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLParamData: function(StatementHandle: SQLHSTMT; ValuePtrPtr: PSQLPOINTER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrepare: function(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrepareW: function(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrimaryKeys: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPrimaryKeysW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProcedureColumns: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProcedureColumnsW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProcedures: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLProceduresW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLPutData: function(StatementHandle: SQLHSTMT; DataPtr: SQLPOINTER;
      StrLen_or_Ind: SQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLRowCount: function(StatementHandle: SQLHSTMT; RowCountPtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetConnectAttr: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetConnectAttrW: function(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetCursorName: function(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      NameLength: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetCursorNameW: function(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      NameLength: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetDescField: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetDescFieldW: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetDescRec: function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLLEN; Precision: SQLSMALLINT;
      Scale: SQLSMALLINT; DataPtr: SQLPOINTER; StringLengthPtr: PSQLLEN;
      IndicatorPtr: PSQLLEN): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetEnvAttr: function(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetPos: function(StatementHandle: SQLHSTMT; RowNumber: SQLSETPOSIROW;
      Operation: SQLUSMALLINT; LockType: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetStmtAttr: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSetStmtAttrW: function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSpecialColumns: function(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLSpecialColumnsW: function(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLStatistics: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLStatisticsW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTablePrivileges: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTablePrivilegesW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTables: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    SQLTablesW: function(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure LoadCodePages; override;
    constructor Create;
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
    function BrowseConnect(ConnectionHandle: SQLHDBC; InConnectionString: PSQLCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function BrowseConnectW(ConnectionHandle: SQLHDBC; InConnectionString: PSQLWCHAR;
      StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function BulkOperations(StatementHandle: SQLHSTMT; Operation: SQLUSMALLINT): SQLRETURN;
    function Cancel(StatementHandle: SQLHSTMT): SQLRETURN;
    function CancelHandle(HandleType: SQLSMALLINT; InputHandle: SQLHANDLE): SQLRETURN;
    function CloseCursor(StatementHandle: SQLHSTMT): SQLRETURN;
    function ColAttribute(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColAttributeW(StatementHandle: SQLHSTMT;
      ColumnNumber, FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
      BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT; NumericAttributePtr: PSQLLEN): SQLRETURN;
    function ColumnPrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function ColumnPrivilegesW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Columns (StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function ColumnsW (StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function CompleteAsync(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      var AsyncRetCodePtr: PRETCODE): SQLRETURN;
    function Connect (ConnectionHandle: SQLHDBC;
      ServerName: PSQLCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ConnectW (ConnectionHandle: SQLHDBC;
      ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      UserName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      Authentication: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function CopyDesc (SourceDescHandle: SQLHDESC; TargetDescHandle: SQLHDESC): SQLRETURN;
    function DataSources (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DataSourcesW (EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT; NameLength1Ptr: PSQLSMALLINT;
      Description: PSQLWCHAR; BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
    function DescribeCol(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DescribeColW(StatementHandle: SQLHSTMT;
      ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
      NameLengthPtr: PSQLSMALLINT; DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
      DecimalDigitsPtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function DescribeParam(StatementHandle: SQLHSTMT; ParameterNumber: SQLUSMALLINT;
      DataTypePtr: PSQLSMALLINT; ParameterSizePtr: PSQLULEN;
      DecimalDigitsPtr: SQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function Disconnect(ConnectionHandle: SQLHDBC): SQLRETURN;
    function DriverConnect(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function DriverConnectW(ConnectionHandle: SQLHDBC; WindowHandle: SQLHWND;
      InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
      OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
      StringLength2Ptr: PSQLSMALLINT; DriverCompletion: SQLUSMALLINT): SQLRETURN;
    function Drivers(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function DriversW(EnvironmentHandle: SQLHENV; Direction: SQLUSMALLINT;
      DriverDescription: PSQLWCHAR; BufferLength1: SQLSMALLINT;
      DescriptionLengthPtr: PSQLSMALLINT; DriverAttributes: PSQLWCHAR;
      BufferLength2: SQLSMALLINT; AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
    function EndTran(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      CompletionType: SQLSMALLINT): SQLRETURN;
    function ExecDirect(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function ExecDirectW(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function Execute(StatementHandle: SQLHSTMT): SQLRETURN;
    function Fetch(StatementHandle: SQLHSTMT): SQLRETURN;
    function FetchScroll(StatementHandle: SQLHSTMT; FetchOrientation: SQLSMALLINT;
      FetchOffset: SQLLEN): SQLRETURN;
    function ForeignKeys(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function ForeignKeysW(StatementHandle: SQLHSTMT;
      PKCatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      PKSchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      PKTableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      FKCatalogName: PSQLWCHAR; NameLength4: SQLSMALLINT;
      FKSchemaName: PSQLWCHAR; NameLength5: SQLSMALLINT;
      FKTableName: PSQLWCHAR; NameLength6: SQLSMALLINT): SQLRETURN;
    function FreeHandle(HandleType: SQLSMALLINT; Handle: SQLHANDLE): SQLRETURN;
    function FreeStmt(StatementHandle: SQLHSTMT; Option: SQLUSMALLINT): SQLRETURN;
    function GetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetConnectAttrW(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
       Value: SQLPOINTER; BufferLength: SQLINTEGER; StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetCursorNameW(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      BufferLength: SQLSMALLINT; NameLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetData(StatementHandle: SQLHSTMT; ColumnNumber: SQLUSMALLINT;
      TargetType: SQLSMALLINT; TargetValue: SQLPOINTER; BufferLength: SQLLEN;
      StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
    function GetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: PSQLINTEGER): SQLRETURN;
    function GetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDescRecW(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
       Name: PSQLWCHAR; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
       TypePtr: PSQLSMALLINT; SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
       PrecisionPtr: PSQLSMALLINT; ScalePtr: PSQLSMALLINT; NullablePtr: PSQLSMALLINT): SQLRETURN;
    function GetDiagField(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN;
    function GetDiagFieldW(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; DiagIdentifier: SQLSMALLINT; DiagInfo: SQLPOINTER;
      BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT): SQLRETURN;
    function GetDiagRec(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function GetDiagRecW(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
      RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR; NativeErrorPtr: PSQLINTEGER;
      MessageText: PSQLWCHAR; BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
    function GetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN;
    function GetFunctions(ConnectionHandle: SQLHDBC; FunctionId: SQLUSMALLINT;
      SupportedPtr: PSQLUSMALLINT): SQLRETURN;
    function GetInfo(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetInfoW(ConnectionHandle: SQLHDBC; InfoType: SQLUSMALLINT;
      InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT): SQLRETURN;
    function GetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetStmtAttrW(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLengthPtr: PSQLINTEGER): SQLRETURN;
    function GetTypeInfo(StatementHandle: SQLHSTMT; DataType: SQLSMALLINT): SQLRETURN;
    function MoreResults(StatementHandle: SQLHSTMT): SQLRETURN;
    function NativeSql(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function NativeSqlW(ConnectionHandle: SQLHDBC;
      InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
      OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
      TextLength2Ptr: PSQLINTEGER): SQLRETURN;
    function NumParams(StatementHandle: SQLHSTMT; ParameterCountPtr: PSQLSMALLINT): SQLRETURN;
    function NumResultCols(StatementHandle: SQLHSTMT; ColumnCountPtr: PSQLSMALLINT): SQLRETURN;
    function ParamData(StatementHandle: SQLHSTMT; ValuePtrPtr: PSQLPOINTER): SQLRETURN;
    function Prepare(StatementHandle: SQLHSTMT; StatementText: PSQLCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrepareW(StatementHandle: SQLHSTMT; StatementText: PSQLWCHAR;
      TextLength: SQLINTEGER): SQLRETURN;
    function PrimaryKeys(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function PrimaryKeysW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProcedureColumns(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function ProcedureColumnsW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function Procedures(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function ProceduresW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function PutData(StatementHandle: SQLHSTMT; DataPtr: SQLPOINTER;
      StrLen_or_Ind: SQLLEN): SQLRETURN;
    function RowCount(StatementHandle: SQLHSTMT; RowCountPtr: PSQLLEN): SQLRETURN;
    function SetConnectAttr(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetConnectAttrW(ConnectionHandle: SQLHDBC; Attribute: SQLINTEGER;
     ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetCursorName(StatementHandle: SQLHSTMT; CursorName: PSQLCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SetCursorNameW(StatementHandle: SQLHSTMT; CursorName: PSQLWCHAR;
      NameLength: SQLSMALLINT): SQLRETURN;
    function SetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN;
    function SetDescFieldW(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
      BufferLength: SQLINTEGER): SQLRETURN;
    function SetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLLEN; Precision: SQLSMALLINT;
      Scale: SQLSMALLINT; DataPtr: SQLPOINTER; StringLengthPtr: PSQLLEN;
      IndicatorPtr: PSQLLEN): SQLRETURN;
    function SetEnvAttr(EnvironmentHandle: SQLHENV; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetPos(StatementHandle: SQLHSTMT; RowNumber: SQLSETPOSIROW;
      Operation: SQLUSMALLINT; LockType: SQLUSMALLINT): SQLRETURN;
    function SetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SetStmtAttrW(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      ValuePtr: SQLPOINTER; StringLength: SQLINTEGER): SQLRETURN;
    function SpecialColumns(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function SpecialColumnsW(StatementHandle: SQLHSTMT; IdentifierType: SQLSMALLINT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Scope: SQLSMALLINT; Nullable: SQLSMALLINT): SQLRETURN;
    function Statistics(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function StatisticsW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      Unique: SQLUSMALLINT; Reserved: SQLUSMALLINT): SQLRETURN;
    function TablePrivileges(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function TablePrivilegesW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT): SQLRETURN;
    function Tables(StatementHandle: SQLHSTMT;
      CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
    function TablesW(StatementHandle: SQLHSTMT;
      CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
      SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT;
      TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
      TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
   end;

implementation

uses ZPlainLoader;

{ TODBC3BaseDriver }

function TODBC3BaseDriver.AllocHandle(HandleType: SQLSMALLINT;
  InputHandle: SQLHANDLE; var OutputHandle: SQLHANDLE): SQLRETURN;
begin
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

function TODBC3BaseDriver.BrowseConnect(ConnectionHandle: SQLHDBC;
  InConnectionString: PSQLCHAR; StringLength1: SQLSMALLINT;
  OutConnectionString: PSQLCHAR; BufferLength: SQLSMALLINT;
  StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLBrowseConnect(ConnectionHandle, InConnectionString, StringLength1,
    OutConnectionString, BufferLength, StringLength2Ptr);
end;

function TODBC3BaseDriver.BrowseConnectW(ConnectionHandle: SQLHDBC;
  InConnectionString: PSQLWCHAR; StringLength1: SQLSMALLINT;
  OutConnectionString: PSQLWCHAR; BufferLength: SQLSMALLINT;
  StringLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLBrowseConnectW(ConnectionHandle, InConnectionString, StringLength1,
    OutConnectionString, BufferLength, StringLength2Ptr);
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

function TODBC3BaseDriver.Clone: IZPlainDriver;
begin
  Result := TODBC3BaseDriver.Create;
end;

function TODBC3BaseDriver.CloseCursor(StatementHandle: SQLHSTMT): SQLRETURN;
begin
  Result := SQLCloseCursor(StatementHandle);
end;

function TODBC3BaseDriver.ColAttribute(StatementHandle: SQLHSTMT; ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLCHAR;
  BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
  NumericAttributePtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLColAttribute(StatementHandle, ColumnNumber, FieldIdentifier,
    CharacterAttributePtr, BufferLength, StringLengthPtr, NumericAttributePtr);
end;

function TODBC3BaseDriver.ColAttributeW(StatementHandle: SQLHSTMT; ColumnNumber,
  FieldIdentifier: SQLUSMALLINT; CharacterAttributePtr: PSQLWCHAR;
  BufferLength: SQLSMALLINT; StringLengthPtr: PSQLSMALLINT;
  NumericAttributePtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLColAttributeW(StatementHandle, ColumnNumber, FieldIdentifier,
    CharacterAttributePtr, BufferLength, StringLengthPtr, NumericAttributePtr);
end;

function TODBC3BaseDriver.ColumnPrivileges(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumnPrivileges(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3BaseDriver.ColumnPrivilegesW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumnPrivilegesW(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3BaseDriver.Columns(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumns(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3BaseDriver.ColumnsW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLColumnsW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3BaseDriver.CompleteAsync(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; var AsyncRetCodePtr: PRETCODE): SQLRETURN;
begin
  Result := SQLCompleteAsync(HandleType, Handle, AsyncRetCodePtr);
end;

function TODBC3BaseDriver.Connect(ConnectionHandle: SQLHDBC;
  ServerName: PSQLCHAR; NameLength1: SQLSMALLINT; UserName: PSQLCHAR;
  NameLength2: SQLSMALLINT; Authentication: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLConnect(ConnectionHandle, ServerName, NameLength1, UserName,
    NameLength2, Authentication, NameLength3);
end;

function TODBC3BaseDriver.ConnectW(ConnectionHandle: SQLHDBC;
  ServerName: PSQLWCHAR; NameLength1: SQLSMALLINT; UserName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; Authentication: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLConnectW(ConnectionHandle, ServerName, NameLength1, UserName,
    NameLength2, Authentication, NameLength3);
end;

function TODBC3BaseDriver.CopyDesc(SourceDescHandle,
  TargetDescHandle: SQLHDESC): SQLRETURN;
begin
  Result := SQLCopyDesc(SourceDescHandle, TargetDescHandle);
end;

constructor TODBC3BaseDriver.Create;
begin
  inherited Create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IF defined(Unix) or defined (MSWINDOWS)}
  FLoader.AddLocation(ODBC_LOCATION);
  {$IFEND}
end;

function TODBC3BaseDriver.DataSources(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; ServerName: PSQLCHAR; BufferLength1: SQLSMALLINT;
  NameLength1Ptr: PSQLSMALLINT; Description: PSQLCHAR;
  BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDataSources(EnvironmentHandle, Direction, ServerName,
    BufferLength1, NameLength1Ptr, Description, BufferLength2, NameLength2Ptr);
end;

function TODBC3BaseDriver.DataSourcesW(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; ServerName: PSQLWCHAR; BufferLength1: SQLSMALLINT;
  NameLength1Ptr: PSQLSMALLINT; Description: PSQLWCHAR;
  BufferLength2: SQLSMALLINT; NameLength2Ptr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDataSourcesW(EnvironmentHandle, Direction, ServerName,
    BufferLength1, NameLength1Ptr, Description, BufferLength2, NameLength2Ptr);
end;

function TODBC3BaseDriver.DescribeCol(StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT; ColumnName: PSQLCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr, DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN; DecimalDigitsPtr,
  NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDescribeCol(StatementHandle, ColumnNumber, ColumnName, BufferLength,
    NameLengthPtr, DataTypePtr, ColumnSizePtr, DecimalDigitsPtr, NullablePtr);
end;

function TODBC3BaseDriver.DescribeColW(StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT; ColumnName: PSQLWCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr, DataTypePtr: PSQLSMALLINT; ColumnSizePtr: PSQLULEN;
  DecimalDigitsPtr, NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDescribeColW(StatementHandle, ColumnNumber, ColumnName, BufferLength,
    NameLengthPtr, DataTypePtr, ColumnSizePtr, DecimalDigitsPtr, NullablePtr);
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

function TODBC3BaseDriver.DriverConnect(ConnectionHandle: SQLHDBC;
  WindowHandle: SQLHWND; InConnectionString: PSQLCHAR;
  StringLength1: SQLSMALLINT; OutConnectionString: PSQLCHAR;
  BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT;
  DriverCompletion: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLDriverConnect(ConnectionHandle, WindowHandle, InConnectionString,
    StringLength1, OutConnectionString, BufferLength, StringLength2Ptr, DriverCompletion);
end;

function TODBC3BaseDriver.DriverConnectW(ConnectionHandle: SQLHDBC;
  WindowHandle: SQLHWND; InConnectionString: PSQLWCHAR;
  StringLength1: SQLSMALLINT; OutConnectionString: PSQLWCHAR;
  BufferLength: SQLSMALLINT; StringLength2Ptr: PSQLSMALLINT;
  DriverCompletion: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLDriverConnectW(ConnectionHandle, WindowHandle, InConnectionString,
    StringLength1, OutConnectionString, BufferLength, StringLength2Ptr, DriverCompletion);
end;

function TODBC3BaseDriver.Drivers(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; DriverDescription: PSQLCHAR;
  BufferLength1: SQLSMALLINT; DescriptionLengthPtr: PSQLSMALLINT;
  DriverAttributes: PSQLCHAR; BufferLength2: SQLSMALLINT;
  AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDrivers(EnvironmentHandle, Direction, DriverDescription,
    BufferLength1, DescriptionLengthPtr, DriverAttributes, BufferLength2,
    AttributesLengthPtr);
end;

function TODBC3BaseDriver.DriversW(EnvironmentHandle: SQLHENV;
  Direction: SQLUSMALLINT; DriverDescription: PSQLWCHAR;
  BufferLength1: SQLSMALLINT; DescriptionLengthPtr: PSQLSMALLINT;
  DriverAttributes: PSQLWCHAR; BufferLength2: SQLSMALLINT;
  AttributesLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLDriversW(EnvironmentHandle, Direction, DriverDescription,
    BufferLength1, DescriptionLengthPtr, DriverAttributes, BufferLength2,
    AttributesLengthPtr);
end;

function TODBC3BaseDriver.EndTran(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
  CompletionType: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLEndTran(HandleType, Handle, CompletionType);
end;

function TODBC3BaseDriver.ExecDirect(StatementHandle: SQLHSTMT;
  StatementText: PSQLCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLExecDirect(StatementHandle, StatementText, TextLength);
end;

function TODBC3BaseDriver.ExecDirectW(StatementHandle: SQLHSTMT;
  StatementText: PSQLWCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLExecDirectW(StatementHandle, StatementText, TextLength);
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

function TODBC3BaseDriver.ForeignKeys(StatementHandle: SQLHSTMT;
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

function TODBC3BaseDriver.ForeignKeysW(StatementHandle: SQLHSTMT;
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

function TODBC3BaseDriver.GetConnectAttr(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; Value: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetConnectAttr(ConnectionHandle, Attribute, Value, BufferLength,
    StringLengthPtr);
end;

function TODBC3BaseDriver.GetConnectAttrW(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; Value: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetConnectAttrW(ConnectionHandle, Attribute, Value, BufferLength,
    StringLengthPtr);
end;

function TODBC3BaseDriver.GetCursorName(StatementHandle: SQLHSTMT;
  CursorName: PSQLCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetCursorName(StatementHandle, CursorName, BufferLength,
    NameLengthPtr);
end;

function TODBC3BaseDriver.GetCursorNameW(StatementHandle: SQLHSTMT;
  CursorName: PSQLWCHAR; BufferLength: SQLSMALLINT;
  NameLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetCursorNameW(StatementHandle, CursorName, BufferLength,
    NameLengthPtr);
end;

function TODBC3BaseDriver.GetData(StatementHandle: SQLHSTMT;
  ColumnNumber: SQLUSMALLINT; TargetType: SQLSMALLINT; TargetValue: SQLPOINTER;
  BufferLength: SQLLEN; StrLen_or_IndPtr: PSQLLEN): SQLRETURN;
begin
  Result := SQLGetData(StatementHandle, ColumnNumber, TargetType, TargetValue,
    BufferLength, StrLen_or_IndPtr);
end;

function TODBC3BaseDriver.GetDescField(DescriptorHandle: SQLHDESC; RecNumber,
  FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLength: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetDescField(DescriptorHandle, RecNumber, FieldIdentifier,
    Value, BufferLength, StringLength);
end;

function TODBC3BaseDriver.GetDescRec(DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT; Name: PSQLCHAR; BufferLength: SQLSMALLINT;
  StringLengthPtr, TypePtr, SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
  PrecisionPtr, ScalePtr, NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDescRec(DescriptorHandle, RecNumber, Name, BufferLength,
    StringLengthPtr, TypePtr, SubTypePtr, LengthPtr, PrecisionPtr, ScalePtr, NullablePtr)
end;

function TODBC3BaseDriver.GetDescRecW(DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT; Name: PSQLWCHAR; BufferLength: SQLSMALLINT;
  StringLengthPtr, TypePtr, SubTypePtr: PSQLSMALLINT; LengthPtr: PSQLLEN;
  PrecisionPtr, ScalePtr, NullablePtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDescRecW(DescriptorHandle, RecNumber, Name, BufferLength,
    StringLengthPtr, TypePtr, SubTypePtr, LengthPtr, PrecisionPtr, ScalePtr, NullablePtr)
end;

function TODBC3BaseDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for ODBC3+';
end;

function TODBC3BaseDriver.GetDiagField(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; RecNumber, DiagIdentifier: SQLSMALLINT;
  DiagInfo: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagField(HandleType, Handle, RecNumber, DiagIdentifier,
    DiagInfo, BufferLength, StringLength);
end;

function TODBC3BaseDriver.GetDiagFieldW(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; RecNumber, DiagIdentifier: SQLSMALLINT;
  DiagInfo: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagFieldW(HandleType, Handle, RecNumber, DiagIdentifier,
    DiagInfo, BufferLength, StringLength);
end;

function TODBC3BaseDriver.GetDiagRec(HandleType: SQLSMALLINT; Handle: SQLHANDLE;
  RecNumber: SQLSMALLINT; Sqlstate: PSQLCHAR; NativeErrorPtr: PSQLINTEGER;
  MessageText: PSQLCHAR; BufferLength: SQLSMALLINT;
  TextLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagRec(HandleType, Handle, RecNumber, Sqlstate, NativeErrorPtr,
    MessageText, BufferLength, TextLength)
end;

function TODBC3BaseDriver.GetDiagRecW(HandleType: SQLSMALLINT;
  Handle: SQLHANDLE; RecNumber: SQLSMALLINT; Sqlstate: PSQLWCHAR;
  NativeErrorPtr: PSQLINTEGER; MessageText: PSQLWCHAR;
  BufferLength: SQLSMALLINT; TextLength: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetDiagRecW(HandleType, Handle, RecNumber, Sqlstate, NativeErrorPtr,
    MessageText, BufferLength, TextLength)
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

function TODBC3BaseDriver.GetInfo(ConnectionHandle: SQLHDBC;
  InfoType: SQLUSMALLINT; InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetInfo(ConnectionHandle, InfoType, InfoValuePtr, BufferLength,
    StringLengthPtr);
end;

function TODBC3BaseDriver.GetInfoW(ConnectionHandle: SQLHDBC;
  InfoType: SQLUSMALLINT; InfoValuePtr: SQLPOINTER; BufferLength: SQLSMALLINT;
  StringLengthPtr: PSQLSMALLINT): SQLRETURN;
begin
  Result := SQLGetInfoW(ConnectionHandle, InfoType, InfoValuePtr, BufferLength,
    StringLengthPtr);
end;

function TODBC3BaseDriver.GetProtocol: string;
begin
  Result := 'odbc';
end;

function TODBC3BaseDriver.GetStmtAttr(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetStmtAttr(StatementHandle, Attribute, ValuePtr, BufferLength,
    StringLengthPtr);
end;

function TODBC3BaseDriver.GetStmtAttrW(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER; BufferLength: SQLINTEGER;
  StringLengthPtr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLGetStmtAttrW(StatementHandle, Attribute, ValuePtr, BufferLength,
    StringLengthPtr);
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
  @SQLBrowseConnect         := GetAddress('SQLBrowseConnect');
  @SQLBrowseConnectW        := GetAddress('SQLBrowseConnectW');
  @SQLBulkOperations        := GetAddress('SQLBulkOperations');
  @SQLCancel                := GetAddress('SQLCancel');
  @SQLCancelHandle          := GetAddress('SQLCancelHandle');
  @SQLCloseCursor           := GetAddress('SQLCloseCursor');
  @SQLColAttribute          := GetAddress('SQLColAttribute');
  @SQLColAttributeW         := GetAddress('SQLColAttributeW');
  @SQLColumnPrivileges      := GetAddress('SQLColumnPrivileges');
  @SQLColumnPrivilegesW     := GetAddress('SQLColumnPrivilegesW');
  @SQLColumns               := GetAddress('SQLColumns');
  @SQLColumnsW              := GetAddress('SQLColumnsW');
  @SQLCompleteAsync         := GetAddress('SQLCompleteAsync');
  @SQLConnect               := GetAddress('SQLConnect');
  @SQLConnectW              := GetAddress('SQLConnectW');
  @SQLCopyDesc              := GetAddress('SQLCopyDesc');
  @SQLDataSources           := GetAddress('SQLDataSources');
  @SQLDataSourcesW          := GetAddress('SQLDataSourcesW');
  @SQLDescribeCol           := GetAddress('SQLDescribeCol');
  @SQLDescribeColW          := GetAddress('SQLDescribeColW');
  @SQLDescribeParam         := GetAddress('SQLDescribeParam');
  @SQLDisconnect            := GetAddress('SQLDisconnect');
  @SQLDriverConnect         := GetAddress('SQLDriverConnect');
  @SQLDriverConnectW        := GetAddress('SQLDriverConnectW');
  @SQLDrivers               := GetAddress('SQLDrivers');
  @SQLDriversW              := GetAddress('SQLDriversW');
  @SQLEndTran               := GetAddress('SQLEndTran');
  @SQLExecDirect            := GetAddress('SQLExecDirect');
  @SQLExecDirectW           := GetAddress('SQLExecDirectW');
  @SQLExecute               := GetAddress('SQLExecute');
  @SQLFetch                 := GetAddress('SQLFetch');
  @SQLFetchScroll           := GetAddress('SQLFetchScroll');
  @SQLForeignKeys           := GetAddress('SQLForeignKeys');
  @SQLForeignKeysW          := GetAddress('SQLForeignKeysW');
  @SQLFreeHandle            := GetAddress('SQLFreeHandle');
  @SQLFreeStmt              := GetAddress('SQLFreeStmt');
  @SQLGetConnectAttr        := GetAddress('SQLGetConnectAttr');
  @SQLGetConnectAttrW       := GetAddress('SQLGetConnectAttrW');
  @SQLGetCursorName         := GetAddress('SQLGetCursorName');
  @SQLGetCursorNameW        := GetAddress('SQLGetCursorNameW');
  @SQLGetData               := GetAddress('SQLGetData');
  @SQLGetDescField          := GetAddress('SQLGetDescField');
  @SQLGetDescRec            := GetAddress('SQLGetDescRec');
  @SQLGetDescRecW           := GetAddress('SQLGetDescRecW');
  @SQLGetDiagField          := GetAddress('SQLGetDiagField');
  @SQLGetDiagRec            := GetAddress('SQLGetDiagRec');
  @SQLGetDiagRecW           := GetAddress('SQLGetDiagRecW');
  @SQLGetEnvAttr            := GetAddress('SQLGetEnvAttr');
  @SQLGetFunctions          := GetAddress('SQLGetFunctions');
  @SQLGetInfo               := GetAddress('SQLGetInfo');
  @SQLGetInfoW              := GetAddress('SQLGetInfoW');
  @SQLGetStmtAttr           := GetAddress('SQLGetStmtAttr');
  @SQLGetStmtAttrW          := GetAddress('SQLGetStmtAttrW');
  @SQLGetTypeInfo           := GetAddress('SQLGetTypeInfo');
  @SQLMoreResults           := GetAddress('SQLMoreResults');
  @SQLNativeSql             := GetAddress('SQLNativeSql');
  @SQLNativeSqlW            := GetAddress('SQLNativeSqlW');
  @SQLNumParams             := GetAddress('SQLNumParams');
  @SQLNumResultCols         := GetAddress('SQLNumResultCols');
  @SQLParamData             := GetAddress('SQLParamData');
  @SQLPrepare               := GetAddress('SQLPrepare');
  @SQLPrepareW              := GetAddress('SQLPrepareW');
  @SQLPrimaryKeys           := GetAddress('SQLPrimaryKeys');
  @SQLPrimaryKeysW          := GetAddress('SQLPrimaryKeysW');
  @SQLProcedureColumns      := GetAddress('SQLProcedureColumns');
  @SQLProcedureColumnsW     := GetAddress('SQLProcedureColumnsW');
  @SQLProcedures            := GetAddress('SQLProcedures');
  @SQLProceduresW           := GetAddress('SQLProceduresW');
  @SQLPutData               := GetAddress('SQLPutData');
  @SQLRowCount              := GetAddress('SQLRowCount');
  @SQLSetConnectAttr        := GetAddress('SQLSetConnectAttr');
  @SQLSetConnectAttrW       := GetAddress('SQLSetConnectAttrW');
  @SQLSetCursorName         := GetAddress('SQLSetCursorName');
  @SQLSetCursorNameW        := GetAddress('SQLSetCursorNameW');
  @SQLSetDescField          := GetAddress('SQLSetDescField');
  @SQLSetDescFieldW         := GetAddress('SQLSetDescFieldW');
  @SQLSetDescRec            := GetAddress('SQLSetDescRec');
  @SQLSetEnvAttr            := GetAddress('SQLSetEnvAttr');
  @SQLSetPos                := GetAddress('SQLSetPos');
  @SQLSetStmtAttr           := GetAddress('SQLSetStmtAttr');
  @SQLSetStmtAttrW          := GetAddress('SQLSetStmtAttrW');
  @SQLSpecialColumns        := GetAddress('SQLSpecialColumns');
  @SQLSpecialColumnsW       := GetAddress('SQLSpecialColumnsW');
  @SQLStatistics            := GetAddress('SQLStatistics');
  @SQLStatisticsW           := GetAddress('SQLStatisticsW');
  @SQLTablePrivileges       := GetAddress('SQLTablePrivileges');
  @SQLTablePrivilegesW      := GetAddress('SQLTablePrivilegesW');
  @SQLTables                := GetAddress('SQLTables');
  @SQLTablesW               := GetAddress('SQLTablesW');
  end;
end;

procedure TODBC3BaseDriver.LoadCodePages;
begin
  AddCodePage('CP_UTF16', 0, ceUTF16, ZDefaultSystemCodePage , '', 1, False);
end;

function TODBC3BaseDriver.MoreResults(StatementHandle: SQLHSTMT): SQLRETURN;
begin
  Result := SQLMoreResults(StatementHandle);
end;

function TODBC3BaseDriver.NativeSql(ConnectionHandle: SQLHDBC;
  InStatementText: PSQLCHAR; TextLength1: SQLINTEGER;
  OutStatementText: PSQLCHAR; BufferLength: SQLINTEGER;
  TextLength2Ptr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLNativeSql(ConnectionHandle, InStatementText, TextLength1,
    OutStatementText, BufferLength, TextLength2Ptr);
end;

function TODBC3BaseDriver.NativeSqlW(ConnectionHandle: SQLHDBC;
  InStatementText: PSQLWCHAR; TextLength1: SQLINTEGER;
  OutStatementText: PSQLWCHAR; BufferLength: SQLINTEGER;
  TextLength2Ptr: PSQLINTEGER): SQLRETURN;
begin
  Result := SQLNativeSqlW(ConnectionHandle, InStatementText, TextLength1,
    OutStatementText, BufferLength, TextLength2Ptr);
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

function TODBC3BaseDriver.Prepare(StatementHandle: SQLHSTMT;
  StatementText: PSQLCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLPrepare(StatementHandle, StatementText, TextLength);
end;

function TODBC3BaseDriver.PrepareW(StatementHandle: SQLHSTMT;
  StatementText: PSQLWCHAR; TextLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLPrepareW(StatementHandle, StatementText, TextLength);
end;

function TODBC3BaseDriver.PrimaryKeys(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLPrimaryKeys(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3);
end;

function TODBC3BaseDriver.PrimaryKeysW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLPrimaryKeysW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3);
end;

function TODBC3BaseDriver.ProcedureColumns(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; ProcName: PSQLCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProcedureColumns(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, ProcName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3BaseDriver.ProcedureColumnsW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; ProcName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  ColumnName: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProcedureColumnsW(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, ProcName, NameLength3, ColumnName, NameLength4);
end;

function TODBC3BaseDriver.Procedures(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength2: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength1: SQLSMALLINT; ProcName: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProcedures(StatementHandle, CatalogName, NameLength2, SchemaName,
    NameLength1, ProcName, NameLength3);
end;

function TODBC3BaseDriver.ProceduresW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength2: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength1: SQLSMALLINT; ProcName: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLProceduresW(StatementHandle, CatalogName, NameLength2, SchemaName,
    NameLength1, ProcName, NameLength3);
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

function TODBC3BaseDriver.SetConnectAttr(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetConnectAttr(ConnectionHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3BaseDriver.SetConnectAttrW(ConnectionHandle: SQLHDBC;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetConnectAttrW(ConnectionHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3BaseDriver.SetCursorName(StatementHandle: SQLHSTMT;
  CursorName: PSQLCHAR; NameLength: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSetCursorName(StatementHandle, CursorName, NameLength);
end;

function TODBC3BaseDriver.SetCursorNameW(StatementHandle: SQLHSTMT;
  CursorName: PSQLWCHAR; NameLength: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSetCursorNameW(StatementHandle, CursorName, NameLength);
end;

function TODBC3BaseDriver.SetDescField(DescriptorHandle: SQLHDESC; RecNumber,
  FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetDescField(DescriptorHandle, RecNumber, FieldIdentifier,
    ValuePtr, BufferLength);
end;

function TODBC3BaseDriver.SetDescFieldW(DescriptorHandle: SQLHDESC; RecNumber,
  FieldIdentifier: SQLSMALLINT; ValuePtr: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetDescFieldW(DescriptorHandle, RecNumber, FieldIdentifier,
    ValuePtr, BufferLength);
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

function TODBC3BaseDriver.SetStmtAttr(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetStmtAttr(StatementHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3BaseDriver.SetStmtAttrW(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; ValuePtr: SQLPOINTER;
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := SQLSetStmtAttrW(StatementHandle, Attribute, ValuePtr, StringLength);
end;

function TODBC3BaseDriver.SpecialColumns(StatementHandle: SQLHSTMT;
  IdentifierType: SQLSMALLINT; CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT;
  SchemaName: PSQLCHAR; NameLength2: SQLSMALLINT; TableName: PSQLCHAR;
  NameLength3, Scope, Nullable: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSpecialColumns(StatementHandle, IdentifierType, CatalogName,
    NameLength1, SchemaName, NameLength2, TableName, NameLength3, Scope, Nullable);
end;

function TODBC3BaseDriver.SpecialColumnsW(StatementHandle: SQLHSTMT;
  IdentifierType: SQLSMALLINT; CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT;
  SchemaName: PSQLWCHAR; NameLength2: SQLSMALLINT; TableName: PSQLWCHAR;
  NameLength3, Scope, Nullable: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLSpecialColumnsW(StatementHandle, IdentifierType, CatalogName,
    NameLength1, SchemaName, NameLength2, TableName, NameLength3, Scope, Nullable);
end;

function TODBC3BaseDriver.Statistics(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  Unique, Reserved: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLStatistics(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, Unique, Reserved);
end;

function TODBC3BaseDriver.StatisticsW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  Unique, Reserved: SQLUSMALLINT): SQLRETURN;
begin
  Result := SQLStatisticsW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, Unique, Reserved);
end;

function TODBC3BaseDriver.TablePrivileges(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTablePrivileges(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3);
end;

function TODBC3BaseDriver.TablePrivilegesW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR;
  NameLength3: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTablePrivilegesW(StatementHandle, CatalogName, NameLength1,
    SchemaName, NameLength2, TableName, NameLength3);
end;

function TODBC3BaseDriver.Tables(StatementHandle: SQLHSTMT;
  CatalogName: PSQLCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLCHAR; NameLength3: SQLSMALLINT;
  TableType: PSQLCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTables(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, TableType, NameLength4);
end;

function TODBC3BaseDriver.TablesW(StatementHandle: SQLHSTMT;
  CatalogName: PSQLWCHAR; NameLength1: SQLSMALLINT; SchemaName: PSQLWCHAR;
  NameLength2: SQLSMALLINT; TableName: PSQLWCHAR; NameLength3: SQLSMALLINT;
  TableType: PSQLWCHAR; NameLength4: SQLSMALLINT): SQLRETURN;
begin
  Result := SQLTablesW(StatementHandle, CatalogName, NameLength1, SchemaName,
    NameLength2, TableName, NameLength3, TableType, NameLength4);
end;

end.
