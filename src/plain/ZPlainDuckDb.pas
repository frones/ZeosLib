{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for DuckDB             }
{                                                         }
{         Originally written by Jan Baumgarten            }
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

unit ZPlainDuckDb;

interface

{$I ZPlain.inc}

uses
  ctypes, SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZCompatibility, ZPlainDriver;

{$IFNDEF ZEOS_DISABLE_DUCKDB}

const
    WINDOWS_VS_DLL_LOCATION = 'duckdb.dll';
    WINDOWS_MINGW_DLL_LOCATION = 'libduckdb.dll';
    LINUX_DLL_LOCATION = 'libduckdb'+SharedSuffix;

    //! An enum over DuckDB's internal types.
    //enum DUCKDB_TYPE
    DUCKDB_TYPE_INVALID      = 0;
    DUCKDB_TYPE_BOOLEAN      = 1; // bool
    DUCKDB_TYPE_TINYINT      = 2; // int8_t
    DUCKDB_TYPE_SMALLINT     = 3; // int16_t
    DUCKDB_TYPE_INTEGER      = 4; // int32_t
    DUCKDB_TYPE_BIGINT       = 5; // int64_t
    DUCKDB_TYPE_UTINYINT     = 6; // uint8_t
    DUCKDB_TYPE_USMALLINT    = 7; // uint16_t
    DUCKDB_TYPE_UINTEGER     = 8; // uint32_t
    DUCKDB_TYPE_UBIGINT      = 9; // uint64_t
    DUCKDB_TYPE_FLOAT        = 10; // float
    DUCKDB_TYPE_DOUBLE       = 11; // double
    DUCKDB_TYPE_TIMESTAMP    = 12; // duckdb_timestamp, in microseconds
    DUCKDB_TYPE_DATE         = 13; // duckdb_date
    DUCKDB_TYPE_TIME         = 14; // duckdb_time
    DUCKDB_TYPE_INTERVAL     = 15; // duckdb_interval
    DUCKDB_TYPE_HUGEINT      = 16; // duckdb_hugeint
    DUCKDB_TYPE_UHUGEINT     = 32; // duckdb_uhugeint
    DUCKDB_TYPE_VARCHAR      = 17; // const char*
    DUCKDB_TYPE_BLOB         = 18; // duckdb_blob
    DUCKDB_TYPE_DECIMAL      = 19; // decimal
    DUCKDB_TYPE_TIMESTAMP_S  = 20; // duckdb_timestamp, in seconds
    DUCKDB_TYPE_TIMESTAMP_MS = 21; // duckdb_timestamp, in milliseconds
    DUCKDB_TYPE_TIMESTAMP_NS = 22; // duckdb_timestamp, in nanoseconds
    DUCKDB_TYPE_ENUM         = 23; // enum type, only useful as logical type
    DUCKDB_TYPE_LIST         = 24; // list type, only useful as logical type
    DUCKDB_TYPE_STRUCT       = 25; // struct type, only useful as logical type
    DUCKDB_TYPE_MAP          = 26; // map type, only useful as logical type
    DUCKDB_TYPE_ARRAY        = 33; // duckdb_array, only useful as logical type
    DUCKDB_TYPE_UUID         = 27; // duckdb_hugeint
    DUCKDB_TYPE_UNION        = 28; // union type, only useful as logical type
    DUCKDB_TYPE_BIT          = 29; // duckdb_bit
    DUCKDB_TYPE_TIME_TZ      = 30; // duckdb_time_tz
    DUCKDB_TYPE_TIMESTAMP_TZ = 31; // duckdb_timestamp
    DUCKDB_TYPE_ANY          = 34; // ANY type
    DUCKDB_TYPE_VARINT       = 35; // duckdb_varint
    DUCKDB_TYPE_SQLNULL      = 36; // SQLNULL type

    //! An enum over the returned state of different functions.
    //enum duckdb_state
    DuckDBSuccess = 0;
    DuckDBError   = 1;
	
    //! An enum over the pending state of a pending query result.
    //enum duckdb_pending_state
    DUCKDB_PENDING_RESULT_READY = 0;
    DUCKDB_PENDING_RESULT_NOT_READY = 1;
    DUCKDB_PENDING_ERROR = 2;
    DUCKDB_PENDING_NO_TASKS_AVAILABLE = 3;

    //! An enum over DuckDB's different result types.
    //enum duckdb_result_type
    DUCKDB_RESULT_TYPE_INVALID = 0;
    DUCKDB_RESULT_TYPE_CHANGED_ROWS = 1;
    DUCKDB_RESULT_TYPE_NOTHING = 2;
    DUCKDB_RESULT_TYPE_QUERY_RESULT = 3;

    //! An enum over DuckDB's different statement types.
    //enum duckdb_statement_type
    DUCKDB_STATEMENT_TYPE_INVALID = 0;
    DUCKDB_STATEMENT_TYPE_SELECT = 1;
    DUCKDB_STATEMENT_TYPE_INSERT = 2;
    DUCKDB_STATEMENT_TYPE_UPDATE = 3;
    DUCKDB_STATEMENT_TYPE_EXPLAIN = 4;
    DUCKDB_STATEMENT_TYPE_DELETE = 5;
    DUCKDB_STATEMENT_TYPE_PREPARE = 6;
    DUCKDB_STATEMENT_TYPE_CREATE = 7;
    DUCKDB_STATEMENT_TYPE_EXECUTE = 8;
    DUCKDB_STATEMENT_TYPE_ALTER = 9;
    DUCKDB_STATEMENT_TYPE_TRANSACTION = 10;
    DUCKDB_STATEMENT_TYPE_COPY = 11;
    DUCKDB_STATEMENT_TYPE_ANALYZE = 12;
    DUCKDB_STATEMENT_TYPE_VARIABLE_SET = 13;
    DUCKDB_STATEMENT_TYPE_CREATE_FUNC = 14;
    DUCKDB_STATEMENT_TYPE_DROP = 15;
    DUCKDB_STATEMENT_TYPE_EXPORT = 16;
    DUCKDB_STATEMENT_TYPE_PRAGMA = 17;
    DUCKDB_STATEMENT_TYPE_VACUUM = 18;
    DUCKDB_STATEMENT_TYPE_CALL = 19;
    DUCKDB_STATEMENT_TYPE_SET = 20;
    DUCKDB_STATEMENT_TYPE_LOAD = 21;
    DUCKDB_STATEMENT_TYPE_RELATION = 22;
    DUCKDB_STATEMENT_TYPE_EXTENSION = 23;
    DUCKDB_STATEMENT_TYPE_LOGICAL_PLAN = 24;
    DUCKDB_STATEMENT_TYPE_ATTACH = 25;
    DUCKDB_STATEMENT_TYPE_DETACH = 26;
    DUCKDB_STATEMENT_TYPE_MULTI = 27;

    //! An enum over DuckDB's different result types.
    //enum duckdb_error_type
    DUCKDB_ERROR_INVALID = 0;
    DUCKDB_ERROR_OUT_OF_RANGE = 1;
    DUCKDB_ERROR_CONVERSION = 2;
    DUCKDB_ERROR_UNKNOWN_TYPE = 3;
    DUCKDB_ERROR_DECIMAL = 4;
    DUCKDB_ERROR_MISMATCH_TYPE = 5;
    DUCKDB_ERROR_DIVIDE_BY_ZERO = 6;
    DUCKDB_ERROR_OBJECT_SIZE = 7;
    DUCKDB_ERROR_INVALID_TYPE = 8;
    DUCKDB_ERROR_SERIALIZATION = 9;
    DUCKDB_ERROR_TRANSACTION = 10;
    DUCKDB_ERROR_NOT_IMPLEMENTED = 11;
    DUCKDB_ERROR_EXPRESSION = 12;
    DUCKDB_ERROR_CATALOG = 13;
    DUCKDB_ERROR_PARSER = 14;
    DUCKDB_ERROR_PLANNER = 15;
    DUCKDB_ERROR_SCHEDULER = 16;
    DUCKDB_ERROR_EXECUTOR = 17;
    DUCKDB_ERROR_CONSTRAINT = 18;
    DUCKDB_ERROR_INDEX = 19;
    DUCKDB_ERROR_STAT = 20;
    DUCKDB_ERROR_CONNECTION = 21;
    DUCKDB_ERROR_SYNTAX = 22;
    DUCKDB_ERROR_SETTINGS = 23;
    DUCKDB_ERROR_BINDER = 24;
    DUCKDB_ERROR_NETWORK = 25;
    DUCKDB_ERROR_OPTIMIZER = 26;
    DUCKDB_ERROR_NULL_POINTER = 27;
    DUCKDB_ERROR_IO = 28;
    DUCKDB_ERROR_INTERRUPT = 29;
    DUCKDB_ERROR_FATAL = 30;
    DUCKDB_ERROR_INTERNAL = 31;
    DUCKDB_ERROR_INVALID_INPUT = 32;
    DUCKDB_ERROR_OUT_OF_MEMORY = 33;
    DUCKDB_ERROR_PERMISSION = 34;
    DUCKDB_ERROR_PARAMETER_NOT_RESOLVED = 35;
    DUCKDB_ERROR_PARAMETER_NOT_ALLOWED = 36;
    DUCKDB_ERROR_DEPENDENCY = 37;
    DUCKDB_ERROR_HTTP = 38;
    DUCKDB_ERROR_MISSING_EXTENSION = 39;
    DUCKDB_ERROR_AUTOLOAD = 40;
    DUCKDB_ERROR_SEQUENCE = 41;
    DUCKDB_INVALID_CONFIGURATION = 42;

    //! An enum over DuckDB's different cast modes.
    //enum duckdb_cast_mode
    DUCKDB_CAST_NORMAL = 0;
    DUCKDB_CAST_TRY = 1;
	
type
   // Inserted for the enums:
   TDuckDB_State = cint32;
   TDuckDB_Type = cint32;
   TDuckDB_Pending_State = cint32;
   TDuckDB_Result_Type = cint32;
   TDuckDB_Statement_Type = cint32;
   TDuckDB_Error_Type = cint32;
   TDuckDB_Cast_Mode = cint32;

    //! DuckDB's index type.
    idx_t = cuint64;
    Pidx_t = ^idx_t;
	
    //! The callback that will be called to destroy data, e.g.,
    //! bind data (if any), init data (if any), extra data for replacement scans (if any)
    //typedef void (*duckdb_delete_callback_t)(void *data);
    TDuckDb_Delete_Callback = procedure(data: Pointer); stdcall;
	
    //! Used for threading, contains a task state. Must be destroyed with `duckdb_destroy_state`.
    //typedef void *duckdb_task_state;
    TDuckDb_Task_State = Pointer;
	
    //===--------------------------------------------------------------------===//
    // Types (no explicit freeing)
     //===--------------------------------------------------------------------===//

    //! Days are stored as days since 1970-01-01
    //! Use the duckdb_from_date/duckdb_to_date function to extract individual information
    TDuckDb_Date = packed record 
        days: cint32;
    end;

    TDuckDb_Date_Struct = packed record
        year: cint32;
	month: cint8;
	day: cint8;
    end;

    //! Time is stored as microseconds since 00:00:00
    //! Use the duckdb_from_time/duckdb_to_time function to extract individual information
    TDuckDb_Time = packed record
        micros: cint64;
    end;
		
    TDuckDb_Time_Struct = packed record
        hour: cint8;
	min:  cint8;
	sec:  cint8;
	micros: cint32;
    end;

    //! TIME_TZ is stored as 40 bits for int64_t micros, and 24 bits for int32_t offset
    TDuckDb_Time_TZ = packed record
        bits: cuint64;
    end;

    TDuckDb_Time_TZ_Struct = packed record
        time: TDuckDb_Time_Struct;
	offset: cint32;
    end;
	
    //! Timestamps are stored as microseconds since 1970-01-01
    //! Use the duckdb_from_timestamp/duckdb_to_timestamp function to extract individual information
    TDuckDb_TimeStamp = packed record
        micros: cint64;
    end;
	
    TDuckDB_TimeStamp_Struct = packed record
        date: TDuckDb_Date_Struct;
        time: TDuckDb_Time_Struct;
    end;

    TDuckDb_Interval = packed record
        months: cint32;
	days: cint32;
	micros: cint64;
    end;
	
    //! Hugeints are composed of a (lower, upper) component
    //! The value of the hugeint is upper * 2^64 + lower
    //! For easy usage, the functions duckdb_hugeint_to_double/duckdb_double_to_hugeint are recommended
    TDuckDB_HugeInt = packed record
        lower: cuint64;
	upper: cint64;
    end;
	
    TDuckDB_UHugeInt = packed record
        lower: cuint64;
        upper: cuint64;
    end;

    //! Decimals are composed of a width and a scale, and are stored in a hugeint
    TDuckDB_Decimal = packed record
        width: cuint8;
	scale: cuint8;
	value: TDuckDB_HugeInt;
    end;
	
    //! A type holding information about the query execution progress
    TDuckDB_Query_Progress_Type = packed record
        percentage: double;
	rows_processed: cuint64;
	total_rows_to_process: cuint64;
    end;
	

    TDuckDB_String_Pointer = packed record
        prefix: array [0..3] of AnsiChar;
        ptr: PAnsiChar;
    end;

    //! The internal representation of a VARCHAR (string_t). If the VARCHAR does not
    //! exceed 12 characters, then we inline it. Otherwise, we inline a prefix for faster
    //! string comparisons and store a pointer to the remaining characters. This is a non-
    //! owning structure, i.e., it does not have to be freed.
    TDuckDB_String_T = packed record
        length: cuint32;
        case Boolean of
          true: (Pointer: TDuckDB_String_Pointer);
          false: (Inlined: Array[0..11] of AnsiChar);
    end;
    PDuckDB_String_T = ^TDuckDB_String_T;

    //! The internal representation of a list metadata entry contains the list's offset in
    //! the child vector, and its length. The parent vector holds these metadata entries,
    //! whereas the child vector holds the data
    TDuckDB_List_Entry = packed record
        offset: cuint64;
        length: cuint64;
    end;

    //! A column consists of a pointer to its internal data. Don't operate on this type directly.
    //! Instead, use functions such as duckdb_column_data, duckdb_nullmask_data,
    //! duckdb_column_type, and duckdb_column_name, which take the result and the column index
    //! as their parameters
    TDuckDB_Column = packed record
	// deprecated, use duckdb_column_data
	deprecated_data: Pointer;
	// deprecated, use duckdb_nullmask_data
	deprecated_nullmask: pcbool;
	// deprecated, use duckdb_column_type
	deprecated_type: cint32;
	// deprecated, use duckdb_column_name
	deprecated_name: PAnsiChar;
	internal_data:Pointer;
    end;
    PDuckDB_Column = ^TDuckDB_Column;

    //! A vector to a specified column in a data chunk. Lives as long as the
    //! data chunk lives, i.e., must not be destroyed.
    SDuckDB_Vector = packed record
	internal_ptr: Pointer;
    end;
    TDuckDB_Vector = ^SDuckDB_Vector; // Note translated as T... because it is more consistent with the other DuckDB types

    //===--------------------------------------------------------------------===//
    // Types (explicit freeing/destroying)
    //===--------------------------------------------------------------------===//

    //! Strings are composed of a char pointer and a size. You must free string.data
    //! with `duckdb_free`.
    TDuckDB_String = packed record
	data: PAnsiChar;
	size: idx_t;
    end;

    //! BLOBs are composed of a byte pointer and a size. You must free blob.data
    //! with `duckdb_free`.
    TDuckDB_Blob = packed record
	data: Pointer;
	size: idx_t;
    end;

    //! A query result consists of a pointer to its internal data.
    //! Must be freed with 'duckdb_destroy_result'.
    TDuckDB_Result = packed record
	// deprecated, use duckdb_column_count
	deprecated_column_count: idx_t;
	// deprecated, use duckdb_row_count
	deprecated_row_count: idx_t;
	// deprecated, use duckdb_rows_changed
	deprecated_rows_changed: idx_t;
	// deprecated, use duckdb_column_*-family of functions
	deprecated_columns: PDuckDB_Column;
	// deprecated, use duckdb_result_error
	deprecated_error_message: PAnsiChar;
	internal_data: Pointer;
    end;
    PDuckDB_Result = ^TDuckDB_Result;

    //! A database object. Should be closed with `duckdb_close`.
    SDuckDB_Database = packed record
	internal_ptr: Pointer;
    end;
    TDuckDB_Database = ^SDuckDB_Database;
    PDuckDB_Database = ^TDuckDB_Database;

    //! A connection to a duckdb database. Must be closed with `duckdb_disconnect`.
    SDuckDB_Connection = packed record
	internal_ptr: Pointer;
    end;
    TDuckDB_Connection = ^SDuckDB_Connection;
    PDuckDB_Connection = ^TDuckDB_Connection;

    //! A prepared statement is a parameterized query that allows you to bind parameters to it.
    //! Must be destroyed with `duckdb_destroy_prepare`.
    SDuckDB_Prepared_Statement = packed record
	internal_ptr: Pointer;
    end;
    TDuckDB_Prepared_Statement = ^SDuckDB_Prepared_Statement;
    PDuckDB_Prepared_Statement = ^TDuckDB_Prepared_Statement;

    //! Extracted statements. Must be destroyed with `duckdb_destroy_extracted`.
    SDuckDB_Extracted_Statements = packed record
    	internal_ptr: Pointer;
    end;
    TDuckDB_Extracted_Statements = ^SDuckDB_Extracted_Statements;

    //! The pending result represents an intermediate structure for a query that is not yet fully executed.
    //! Must be destroyed with `duckdb_destroy_pending`.
    SDuckdb_Pending_Result = packed record
    	internal_ptr: Pointer;
    end;
    TDuckdb_Pending_Result = ^SDuckdb_Pending_Result;

    //! The appender enables fast data loading into DuckDB.
    //! Must be destroyed with `duckdb_appender_destroy`.
    SDuckDB_Appender = packed record
    	internal_ptr: Pointer;
    end;
    TDuckdb_Appender = ^SDuckDB_Appender;

    //! The table description allows querying info about the table.
    //! Must be destroyed with `duckdb_table_description_destroy`.
    SDuckDB_Table_Description = packed record
    	internal_ptr: Pointer;
    end;
    TDuckdb_Table_Description = ^SDuckDB_Table_Description;

    //! Can be used to provide start-up options for the DuckDB instance.
    //! Must be destroyed with `duckdb_destroy_config`.
    SDuckDB_Config = packed record
    	internal_ptr: Pointer;
    end;
    TDuckDB_Config = ^SDuckDB_Config;
    PDuckDB_Config = ^TDuckDB_Config;

    //! Holds an internal logical type.
    //! Must be destroyed with `duckdb_destroy_logical_type`.
    SDuckDB_Logical_Type = packed record
    	internal_ptr: Pointer;
    end;
    TDuckDB_Logical_Type = ^SDuckDB_Logical_Type;

    //! Holds extra information used when registering a custom logical type.
    //! Reserved for future use.
    SDuckDB_Create_Type_Info = packed record
    	internal_ptr: Pointer;
    end;
    TDuckDB_Create_Type_Info = ^SDuckDB_Create_Type_Info;

    //! Contains a data chunk from a duckdb_result.
    //! Must be destroyed with `duckdb_destroy_data_chunk`.
    SDuckDB_Data_Chunk = packed record
    	internal_ptr: Pointer;
    end;
    TDuckDB_Data_Chunk = ^SDuckDB_Data_Chunk;

    //! Holds a DuckDB value, which wraps a type.
    //! Must be destroyed with `duckdb_destroy_value`.
    SDuckDB_Value = packed record
    	internal_ptr: Pointer;
    end;
    TDuckdb_Value = ^SDuckDB_Value;

    //! Holds a recursive tree that matches the query plan.
    SDuckdb_Profiling_Info = packed record
    	internal_ptr: Pointer;
    end;
    TDuckDB_Profiling_Info = ^SDuckdb_Profiling_Info;

    // The following functions have been left out intentionally, because I think
    // they are not necessary for a first driver implementation.
    (*
    //===--------------------------------------------------------------------===//
    // C API Extension info
    //===--------------------------------------------------------------------===//
    //! Holds state during the C API extension intialization process
    typedef struct _duckdb_extension_info {
	    void *internal_ptr;
    } * duckdb_extension_info;
    *)

    //===--------------------------------------------------------------------===//
    // Function types
    //===--------------------------------------------------------------------===//

    // ...

    //===--------------------------------------------------------------------===//
    // Scalar function types
    //===--------------------------------------------------------------------===//

    // ...

    //===--------------------------------------------------------------------===//
    // Aggregate function types
    //===--------------------------------------------------------------------===//

    // ...

    //===--------------------------------------------------------------------===//
    // Table function types
    //===--------------------------------------------------------------------===//

    // ...

    //===--------------------------------------------------------------------===//
    // Cast types
    //===--------------------------------------------------------------------===//

    // ...

    //===--------------------------------------------------------------------===//
    // Replacement scan types
    //===--------------------------------------------------------------------===//

    // ...

    //===--------------------------------------------------------------------===//
    // Arrow-related types
    //===--------------------------------------------------------------------===//

    // ...

    //===--------------------------------------------------------------------===//
    // DuckDB extension access
    //===--------------------------------------------------------------------===//

    /// ...


    {** Represents a generic interface to SQLite native API. }
    IZDuckDBPlainDriver = interface (IZPlainDriver)
      ['{BE9C27ED-A75E-46BE-9219-87A88E4E03E1}']
    end;

  TZDuckDBPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZDuckDBPlainDriver)
  public
    //===--------------------------------------------------------------------===//
    // Functions
    //===--------------------------------------------------------------------===//

    //===--------------------------------------------------------------------===//
    // Open Connect
    //===--------------------------------------------------------------------===//

    (*!
    Creates a new database or opens an existing database file stored at the given path.
    If no path is given a new in-memory database is created instead.
    The instantiated database should be closed with 'duckdb_close'.

    * @param path Path to the database file on disk, or `nullptr` or `:memory:` to open an in-memory database.
    * @param out_database The result database object.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Open: function(const path: PAnsiChar; out_database: PDuckDB_Database): TDuckDB_State; stdcall;

    (*!
    Extended version of duckdb_open. Creates a new database or opens an existing database file stored at the given path.
    The instantiated database should be closed with 'duckdb_close'.

    * @param path Path to the database file on disk, or `nullptr` or `:memory:` to open an in-memory database.
    * @param out_database The result database object.
    * @param config (Optional) configuration used to start up the database system.
    * @param out_error If set and the function returns DuckDBError, this will contain the reason why the start-up failed.
    Note that the error must be freed using `duckdb_free`.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Open_Ext: function(const path: PAnsiChar; out_database: PDuckDB_Database; config: TDuckdb_Config;
                                            out_error: PPAnsiChar): TDuckDB_State; stdcall;

    (*!
    Closes the specified database and de-allocates all memory allocated for that database.
    This should be called after you are done with any database allocated through `duckdb_open` or `duckdb_open_ext`.
    Note that failing to call `duckdb_close` (in case of e.g. a program crash) will not cause data corruption.
    Still, it is recommended to always correctly close a database object after you are done with it.

    * @param database The database object to shut down.
    *)
    DuckDB_Close: procedure(database: PDuckDB_Database); stdcall;

    (*!
    Opens a connection to a database. Connections are required to query the database, and store transactional state
    associated with the connection.
    The instantiated connection should be closed using 'duckdb_disconnect'.

    * @param database The database file to connect to.
    * @param out_connection The result connection object.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    Duckdb_Connect: function(database: TDuckDB_Database; out_connection: PDuckDB_Connection): TDuckDB_State; stdcall;

    (*!
    Interrupt running query

    * @param connection The connection to interrupt
    *)
    DuckDB_Interrupt: procedure(connection: TDuckDB_Connection); stdcall;

    (*!
    Get progress of the running query

    * @param connection The working connection
    * @return -1 if no progress or a percentage of the progress
    *)
    DuckDB_Query_Progress: function(connection: TDuckDB_Connection): TDuckDB_Query_Progress_Type; stdcall;

    (*!
    Closes the specified connection and de-allocates all memory allocated for that connection.

    * @param connection The connection to close.
    *)
    DuckDB_Disconnect: procedure(connection: PDuckDB_Connection); stdcall;

    (*!
    Returns the version of the linked DuckDB, with a version postfix for dev versions

    Usually used for developing C extensions that must return this for a compatibility check.
    *)
    DuckDB_Library_Version: function(): PAnsiChar; stdcall;

    //===--------------------------------------------------------------------===//
    // Configuration
    //===--------------------------------------------------------------------===//

    (*!
    Initializes an empty configuration object that can be used to provide start-up options for the DuckDB instance
    through `duckdb_open_ext`.
    The duckdb_config must be destroyed using 'duckdb_destroy_config'

    This will always succeed unless there is a malloc failure.

    Note that `duckdb_destroy_config` should always be called on the resulting config, even if the function returns
    `DuckDBError`.

    * @param out_config The result configuration object.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Create_Config: function(out_config: PDuckDB_Config): TDuckDB_State; stdcall;

    (*!
    This returns the total amount of configuration options available for usage with `duckdb_get_config_flag`.

    This should not be called in a loop as it internally loops over all the options.

    * @return The amount of config options available.
    *)
    DuckDB_Config_Count: function(): size_t; stdcall;

    (*!
    Obtains a human-readable name and description of a specific configuration option. This can be used to e.g.
    display configuration options. This will succeed unless `index` is out of range (i.e. `>= duckdb_config_count`).

    The result name or description MUST NOT be freed.

    * @param index The index of the configuration option (between 0 and `duckdb_config_count`)
    * @param out_name A name of the configuration flag.
    * @param out_description A description of the configuration flag.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Get_Config_Flag: function(index: size_t; out_name: PPAnsiChar; out_description: PPAnsiChar): TDuckDB_State; stdcall;

    (*!
    Sets the specified option for the specified configuration. The configuration option is indicated by name.
    To obtain a list of config options, see `duckdb_get_config_flag`.

    In the source code, configuration options are defined in `config.cpp`.

    This can fail if either the name is invalid, or if the value provided for the option is invalid.

    * @param config The configuration object to set the option on.
    * @param name The name of the configuration flag to set.
    * @param option The value to set the configuration flag to.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Set_Config: function(config: TDuckDB_Config; const name: PAnsiChar; const option: PAnsiChar): TDuckDB_State; stdcall;

    (*!
    Destroys the specified configuration object and de-allocates all memory allocated for the object.

    * @param config The configuration object to destroy.
    *)
    DuckDB_Destroy_Config: procedure(config: PDuckDB_Config); stdcall;

    //===--------------------------------------------------------------------===//
    // Query Execution
    //===--------------------------------------------------------------------===//

    (*!
    Executes a SQL query within a connection and stores the full (materialized) result in the out_result pointer.
    If the query fails to execute, DuckDBError is returned and the error message can be retrieved by calling
    `duckdb_result_error`.

    Note that after running `duckdb_query`, `duckdb_destroy_result` must be called on the result object even if the
    query fails, otherwise the error stored within the result will not be freed correctly.

    * @param connection The connection to perform the query in.
    * @param query The SQL query to run.
    * @param out_result The query result.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Query: function(connection: TDuckDB_Connection; const query: PAnsiChar; out_result: PDuckDB_Result): TDuckDB_State; stdcall;

    (*!
    Closes the result and de-allocates all memory allocated for that connection.

    * @param result The result to destroy.
    *)
    DuckDB_Destroy_Result: procedure(result: PDuckDB_Result); stdcall;

    (*!
    Returns the column name of the specified column. The result should not need to be freed; the column names will
    automatically be destroyed when the result is destroyed.

    Returns `NULL` if the column is out of range.

    * @param result The result object to fetch the column name from.
    * @param col The column index.
    * @return The column name of the specified column.
    *)
    DuckDB_Column_Name: function(result: PDuckDB_Result; col: idx_t): PAnsiChar; stdcall;

    (*!
    Returns the column type of the specified column.

    Returns `DUCKDB_TYPE_INVALID` if the column is out of range.

    * @param result The result object to fetch the column type from.
    * @param col The column index.
    * @return The column type of the specified column.
    *)
    DuckDB_Column_Type: function(result: PDuckDB_Result; col: idx_t): TDuckDB_Type; stdcall;

    (*!
    Returns the statement type of the statement that was executed

    * @param result The result object to fetch the statement type from.
    * @return duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID
    *)
    DuckDB_Result_Statement_Type: function(result: TDuckDB_Result): TDuckDB_Statement_Type; stdcall;

    (*!
    Returns the logical column type of the specified column.

    The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

    Returns `NULL` if the column is out of range.

    * @param result The result object to fetch the column type from.
    * @param col The column index.
    * @return The logical column type of the specified column.
    *)
    DuckDB_Column_Logical_Type: function(result: PDuckDB_Result; col: idx_t): TDuckDB_Logical_Type; stdcall;

    (*!
    Returns the number of columns present in a the result object.

    * @param result The result object.
    * @return The number of columns present in the result object.
    *)
    DuckDB_Column_Count: function(result: PDuckDB_Result): idx_t; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    Returns the number of rows present in the result object.

    * @param result The result object.
    * @return The number of rows present in the result object.
    *)
    DuckDB_Row_Count: function(result: PDuckDB_Result): idx_t; stdcall;

    (*!
    Returns the number of rows changed by the query stored in the result. This is relevant only for INSERT/UPDATE/DELETE
    queries. For other queries the rows_changed will be 0.

    * @param result The result object.
    * @return The number of rows changed.
    *)
    DuckDB_Rows_Changed: function(result: PDuckDB_Result): idx_t; stdcall;

    (*!
    **DEPRECATED**: Prefer using `duckdb_result_get_chunk` instead.

    Returns the data of a specific column of a result in columnar format.

    The function returns a dense array which contains the result data. The exact type stored in the array depends on the
    corresponding duckdb_type (as provided by `duckdb_column_type`). For the exact type by which the data should be
    accessed, see the comments in [the types section](types) or the `DUCKDB_TYPE` enum.

    For example, for a column of type `DUCKDB_TYPE_INTEGER`, rows can be accessed in the following manner:
    ```c
    int32_t *data = (int32_t * ) duckdb_column_data(&result, 0);
    printf("Data for row %d: %d\n", row, data[row]);
    ```

    * @param result The result object to fetch the column data from.
    * @param col The column index.
    * @return The column data of the specified column.
    *)
    DuckDB_Column_Data: function(result: PDuckDB_Result; col: idx_t): Pointer; stdcall;

    (*!
    **DEPRECATED**: Prefer using `duckdb_result_get_chunk` instead.

    Returns the nullmask of a specific column of a result in columnar format. The nullmask indicates for every row
    whether or not the corresponding row is `NULL`. If a row is `NULL`, the values present in the array provided
    by `duckdb_column_data` are undefined.

    ```c
    int32_t *data = (int32_t * ) duckdb_column_data(&result, 0);
    bool *nullmask = duckdb_nullmask_data(&result, 0);
    if (nullmask[row]) {
        printf("Data for row %d: NULL\n", row);
    } else {
        printf("Data for row %d: %d\n", row, data[row]);
    }
    ```

    * @param result The result object to fetch the nullmask from.
    * @param col The column index.
    * @return The nullmask of the specified column.
    *)
    DuckDB_Nullmask_Data: function(result: PDuckDB_Result; col: idx_t): pcbool; stdcall;

    (*!
    Returns the error message contained within the result. The error is only set if `duckdb_query` returns `DuckDBError`.

    The result of this function must not be freed. It will be cleaned up when `duckdb_destroy_result` is called.

    * @param result The result object to fetch the error from.
    * @return The error of the result.
    *)
    DuckDB_Result_Error: function(result: PDuckDB_Result): PAnsiChar; stdcall;

    (*!
    Returns the result error type contained within the result. The error is only set if `duckdb_query` returns
    `DuckDBError`.

    * @param result The result object to fetch the error from.
    * @return The error type of the result.
    *)
    DuckDB_Result_Error_Type: function(result: PDuckDB_Result): TDuckDB_Error_Type; stdcall;

    //===--------------------------------------------------------------------===//
    // Result Functions
    //===--------------------------------------------------------------------===//

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    Fetches a data chunk from the duckdb_result. This function should be called repeatedly until the result is exhausted.

    The result must be destroyed with `duckdb_destroy_data_chunk`.

    This function supersedes all `duckdb_value` functions, as well as the `duckdb_column_data` and `duckdb_nullmask_data`
    functions. It results in significantly better performance, and should be preferred in newer code-bases.

    If this function is used, none of the other result functions can be used and vice versa (i.e. this function cannot be
    mixed with the legacy result functions).

    Use `duckdb_result_chunk_count` to figure out how many chunks there are in the result.

    * @param result The result object to fetch the data chunk from.
    * @param chunk_index The chunk index to fetch from.
    * @return The resulting data chunk. Returns `NULL` if the chunk index is out of bounds.
    *)
    DuckDB_Result_Get_Chunk: function(result: TDuckDB_Result; chunk_index: idx_t): TDuckDB_Data_Chunk; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    Checks if the type of the internal result is StreamQueryResult.

    * @param result The result object to check.
    * @return Whether or not the result object is of the type StreamQueryResult
    *)
    DuckDB_Result_Is_Streaming: function(result: TDuckDB_Result): cbool; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    Returns the number of data chunks present in the result.

    * @param result The result object
    * @return Number of data chunks present in the result.
    *)
    DuckDB_Result_Chunk_Count: function(result: TDuckDB_Result): idx_t; stdcall;

    (*!
    Returns the return_type of the given result, or DUCKDB_RETURN_TYPE_INVALID on error

    * @param result The result object
    * @return The return_type
    *)
    DuckDB_Result_Return_Type: function(result: TDuckDB_Result): TDuckDB_Result_Type; stdcall;

    //===--------------------------------------------------------------------===//
    // Safe Fetch Functions
    //===--------------------------------------------------------------------===//

    // These functions will perform conversions if necessary.
    // On failure (e.g. if conversion cannot be performed or if the value is NULL) a default value is returned.
    // Note that these functions are slow since they perform bounds checking and conversion
    // For fast access of values prefer using `duckdb_result_get_chunk`

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The boolean value at the specified location, or false if the value cannot be converted.
    *)
    DuckDB_Value_Boolean: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cbool; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The int8_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Int8: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cint8; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The int16_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Int16: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cint16; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The int32_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Int32: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cint32; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The int64_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Int64: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cint64; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_hugeint value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Hugeint: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDB_HugeInt; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_uhugeint value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_UHugeint: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDB_UHugeInt; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_decimal value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Decimal: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDB_Decimal; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The uint8_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_UInt8: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cuint8; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The uint16_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_UInt16: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cuint16; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The uint32_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_UInt32: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cuint32; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The uint64_t value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_UInt64: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cuint64; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The float value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Float: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cfloat; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The double value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Double: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cdouble; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_date value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Date: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDb_Date; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_time value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Time: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDb_Time; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_timestamp value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Timestamp: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDb_TimeStamp; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_interval value at the specified location, or 0 if the value cannot be converted.
    *)
    DuckDB_Value_Interval: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDb_Interval; stdcall;

    (*!
    **DEPRECATED**: Use duckdb_value_string instead. This function does not work correctly if the string contains null
    bytes.

    * @return The text value at the specified location as a null-terminated string, or nullptr if the value cannot be
    converted. The result must be freed with `duckdb_free`.
    *)
    DuckDB_Value_Varchar: function(result: PDuckDB_Result; col: idx_t; row: idx_t): PAnsiChar; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    No support for nested types, and for other complex types.
    The resulting field "string.data" must be freed with `duckdb_free.`

    * @return The string value at the specified location. Attempts to cast the result value to string.
    *)
    DuckDB_Value_String: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDB_String; stdcall;

    (*!
    **DEPRECATED**: Use duckdb_value_string_internal instead. This function does not work correctly if the string contains
    null bytes.

    * @return The char* value at the specified location. ONLY works on VARCHAR columns and does not auto-cast.
    If the column is NOT a VARCHAR column this function will return NULL.

    The result must NOT be freed.
    *)
    DuckDB_Value_Varchar_Internal: function(result: PDuckDB_Result; col: idx_t; row: idx_t): PAnsiChar; stdcall;

    (*!
    **DEPRECATED**: Use duckdb_value_string_internal instead. This function does not work correctly if the string contains
    null bytes.
    * @return The char* value at the specified location. ONLY works on VARCHAR columns and does not auto-cast.
    If the column is NOT a VARCHAR column this function will return NULL.

    The result must NOT be freed.
    *)
    DuckDB_Value_String_Internal: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDB_String; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return The duckdb_blob value at the specified location. Returns a blob with blob.data set to nullptr if the
    value cannot be converted. The resulting field "blob.data" must be freed with `duckdb_free.`
    *)
    DuckDB_Value_Blob: function(result: PDuckDB_Result; col: idx_t; row: idx_t): TDuckDB_Blob; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    * @return Returns true if the value at the specified index is NULL, and false otherwise.
    *)
    DuckDB_Value_Is_Null: function(result: PDuckDB_Result; col: idx_t; row: idx_t): cbool; stdcall;

    //===--------------------------------------------------------------------===//
    // Helpers
    //===--------------------------------------------------------------------===//

    (*!
    Allocate `size` bytes of memory using the duckdb internal malloc function. Any memory allocated in this manner
    should be freed using `duckdb_free`.

    * @param size The number of bytes to allocate.
    * @return A pointer to the allocated memory region.
    *)
    DuckDB_Malloc: function(size: size_t): Pointer; stdcall;

    (*!
    Free a value returned from `duckdb_malloc`, `duckdb_value_varchar`, `duckdb_value_blob`, or
    `duckdb_value_string`.

    * @param ptr The memory region to de-allocate.
    *)
    DuckDB_free: procedure(ptr: Pointer); stdcall;

    (*!
    The internal vector size used by DuckDB.
    This is the amount of tuples that will fit into a data chunk created by `duckdb_create_data_chunk`.

    * @return The vector size.
    *)
    DuckDB_Vector_Size: function(): idx_t; stdcall;

    (*!
    Whether or not the duckdb_string_t value is inlined.
    This means that the data of the string does not have a separate allocation.

    *)
    DuckDB_String_Is_Inlined: function(AString: TDuckDB_String_T): cbool; stdcall;

    (*!
    Get the string length of a string_t

    * @param string The string to get the length of.
    * @return The length.
    *)
    DuckDB_String_T_Length: function(AString: TDuckDB_String_T): cuint32; stdcall;

    (*!
    Get a pointer to the string data of a string_t

    * @param string The string to get the pointer to.
    * @return The pointer.
    *)
    DuckDB_String_T_Data: function(AString: PDuckDB_String_T): PAnsiChar; stdcall;

    //===--------------------------------------------------------------------===//
    // Date Time Timestamp Helpers
    //===--------------------------------------------------------------------===//

    (*!
    Decompose a `duckdb_date` object into year, month and date (stored as `duckdb_date_struct`).

    * @param date The date object, as obtained from a `DUCKDB_TYPE_DATE` column.
    * @return The `duckdb_date_struct` with the decomposed elements.
    *)
    DuckDB_From_Date: function(ADate: TDuckDb_Date): TDuckDb_Date_Struct; stdcall;

    (*!
    Re-compose a `duckdb_date` from year, month and date (`duckdb_date_struct`).

    * @param date The year, month and date stored in a `duckdb_date_struct`.
    * @return The `duckdb_date` element.
    *)
    DuckDB_To_Date: function(date: TDuckDb_Date_Struct): TDuckDb_Date; stdcall;

    (*!
    Test a `duckdb_date` to see if it is a finite value.

    * @param date The date object, as obtained from a `DUCKDB_TYPE_DATE` column.
    * @return True if the date is finite, false if it is ±infinity.
    *)
    DuckDB_Is_Finite_Date: function(date: TDuckDb_Date): cbool; stdcall;

    (*!
    Decompose a `duckdb_time` object into hour, minute, second and microsecond (stored as `duckdb_time_struct`).

    * @param time The time object, as obtained from a `DUCKDB_TYPE_TIME` column.
    * @return The `duckdb_time_struct` with the decomposed elements.
    *)
    DuckDB_From_Time: function(ATime: TDuckDb_Time): TDuckDb_Time_Struct; stdcall;

    (*!
    Create a `duckdb_time_tz` object from micros and a timezone offset.

    * @param micros The microsecond component of the time.
    * @param offset The timezone offset component of the time.
    * @return The `duckdb_time_tz` element.
    *)
    DuckDB_Create_Time_TZ: function(micros: cint64; offset: cint32): TDuckDb_Time_TZ; stdcall;

    (*!
    Decompose a TIME_TZ objects into micros and a timezone offset.

    Use `duckdb_from_time` to further decompose the micros into hour, minute, second and microsecond.

    * @param micros The time object, as obtained from a `DUCKDB_TYPE_TIME_TZ` column.
    *)
    DuckDB_From_Time_TZ: function(micros: TDuckDb_Time_TZ): TDuckDb_Time_TZ_Struct; stdcall;

    (*!
    Re-compose a `duckdb_time` from hour, minute, second and microsecond (`duckdb_time_struct`).

    * @param time The hour, minute, second and microsecond in a `duckdb_time_struct`.
    * @return The `duckdb_time` element.
    *)
    DuckDB_To_Time: function(ATime: TDuckDb_Time_Struct): TDuckDb_Time; stdcall;

    (*!
    Decompose a `duckdb_timestamp` object into a `duckdb_timestamp_struct`.

    * @param ts The ts object, as obtained from a `DUCKDB_TYPE_TIMESTAMP` column.
    * @return The `duckdb_timestamp_struct` with the decomposed elements.
    *)
    DuckDB_From_Timestamp: function(ts: TDuckDb_TimeStamp): TDuckDB_TimeStamp_Struct; stdcall;

    (*!
    Re-compose a `duckdb_timestamp` from a duckdb_timestamp_struct.

    * @param ts The de-composed elements in a `duckdb_timestamp_struct`.
    * @return The `duckdb_timestamp` element.
    *)
    DuckDB_To_Timestamp: function(ts: TDuckDB_TimeStamp_Struct): TDuckDb_TimeStamp; stdcall;

    (*!
    Test a `duckdb_timestamp` to see if it is a finite value.

    * @param ts The timestamp object, as obtained from a `DUCKDB_TYPE_TIMESTAMP` column.
    * @return True if the timestamp is finite, false if it is ±infinity.
    *)
    DuckDB_Is_Finite_Timestamp: function(ts: TDuckDb_TimeStamp): cbool; stdcall;

    //===--------------------------------------------------------------------===//
    // Hugeint Helpers
    //===--------------------------------------------------------------------===//

    (*!
    Converts a duckdb_hugeint object (as obtained from a `DUCKDB_TYPE_HUGEINT` column) into a double.

    * @param val The hugeint value.
    * @return The converted `double` element.
    *)
    DuckDB_Hugeint_To_Double: function(val: TDuckDB_HugeInt): cdouble; stdcall;

    (*!
    Converts a double value to a duckdb_hugeint object.

    If the conversion fails because the double value is too big the result will be 0.

    * @param val The double value.
    * @return The converted `duckdb_hugeint` element.
    *)
    DuckDB_Double_To_Hugeint: function(val: cdouble): TDuckDB_HugeInt; stdcall;

    //===--------------------------------------------------------------------===//
    // Unsigned Hugeint Helpers
    //===--------------------------------------------------------------------===//

    (*!
    Converts a duckdb_uhugeint object (as obtained from a `DUCKDB_TYPE_UHUGEINT` column) into a double.

    * @param val The uhugeint value.
    * @return The converted `double` element.
    *)
    DuckDB_UHugeint_To_Double: function(val: TDuckDB_UHugeInt): cdouble; stdcall;

    (*!
    Converts a double value to a duckdb_uhugeint object.

    If the conversion fails because the double value is too big the result will be 0.

    * @param val The double value.
    * @return The converted `duckdb_uhugeint` element.
    *)
    DuckDB_Double_To_UHugeint: function(val: cdouble): TDuckDB_UHugeInt; stdcall;

    //===--------------------------------------------------------------------===//
    // Decimal Helpers
    //===--------------------------------------------------------------------===//

    (*!
    Converts a double value to a duckdb_decimal object.

    If the conversion fails because the double value is too big, or the width/scale are invalid the result will be 0.

    * @param val The double value.
    * @return The converted `duckdb_decimal` element.
    *)
    DuckDB_Double_To_Decimal: function(val: cdouble; width: cuint8; scale: cuint8): TDuckDB_Decimal; stdcall;

    (*!
    Converts a duckdb_decimal object (as obtained from a `DUCKDB_TYPE_DECIMAL` column) into a double.

    * @param val The decimal value.
    * @return The converted `double` element.
    *)
    DuckDB_Decimal_To_Double: function(val: TDuckDB_Decimal): cdouble; stdcall;

    //===--------------------------------------------------------------------===//
    // Prepared Statements
    //===--------------------------------------------------------------------===//

    // A prepared statement is a parameterized query that allows you to bind parameters to it.
    // * This is useful to easily supply parameters to functions and avoid SQL injection attacks.
    // * This is useful to speed up queries that you will execute several times with different parameters.
    // Because the query will only be parsed, bound, optimized and planned once during the prepare stage,
    // rather than once per execution.
    // For example:
    //   SELECT * FROM tbl WHERE id=?
    // Or a query with multiple parameters:
    //   SELECT * FROM tbl WHERE id=$1 OR name=$2

    (*!
    Create a prepared statement object from a query.

    Note that after calling `duckdb_prepare`, the prepared statement should always be destroyed using
    `duckdb_destroy_prepare`, even if the prepare fails.

    If the prepare fails, `duckdb_prepare_error` can be called to obtain the reason why the prepare failed.

    * @param connection The connection object
    * @param query The SQL query to prepare
    * @param out_prepared_statement The resulting prepared statement object
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Prepare: function(connection: TDuckDB_Connection; const query: PAnsiChar; out_prepared_statement: TDuckDB_Prepared_Statement): TDuckDB_State; stdcall;

    (*!
    Closes the prepared statement and de-allocates all memory allocated for the statement.

    * @param prepared_statement The prepared statement to destroy.
    *)
    DuckDB_Destroy_Prepare: procedure(prepared_statement: PDuckDB_Prepared_Statement); stdcall;

    (*!
    Returns the error message associated with the given prepared statement.
    If the prepared statement has no error message, this returns `nullptr` instead.

    The error message should not be freed. It will be de-allocated when `duckdb_destroy_prepare` is called.

    * @param prepared_statement The prepared statement to obtain the error from.
    * @return The error message, or `nullptr` if there is none.
    *)
    DuckDB_Prepare_Error: function(prepared_statement: TDuckDB_Prepared_Statement): PAnsiChar; stdcall;

    (*!
    Returns the number of parameters that can be provided to the given prepared statement.

    Returns 0 if the query was not successfully prepared.

    * @param prepared_statement The prepared statement to obtain the number of parameters for.
    *)
    DuckDB_NParams: function(prepared_statement: TDuckDB_Prepared_Statement): idx_t; stdcall;

    (*!
    Returns the name used to identify the parameter
    The returned string should be freed using `duckdb_free`.

    Returns NULL if the index is out of range for the provided prepared statement.

    * @param prepared_statement The prepared statement for which to get the parameter name from.
    *)
    DuckDB_Parameter_Name: function(prepared_statement: TDuckDB_Prepared_Statement; index: idx_t): PAnsiChar; stdcall;

    (*!
    Returns the parameter type for the parameter at the given index.

    Returns `DUCKDB_TYPE_INVALID` if the parameter index is out of range or the statement was not successfully prepared.

    * @param prepared_statement The prepared statement.
    * @param param_idx The parameter index.
    * @return The parameter type
    *)
    DuckDB_Param_Type: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t): TDuckDB_Type; stdcall;

    (*!
    Clear the params bind to the prepared statement.
    *)
    DuckDB_Clear_Bindings: function(prepared_statement: TDuckDB_Prepared_Statement): TDuckDB_State; stdcall;

    (*!
    Returns the statement type of the statement to be executed

    * @param statement The prepared statement.
    * @return duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID
    *)
    DuckDB_Prepared_Statement_Type: function(statement: TDuckDB_Prepared_Statement): TDuckDB_Statement_Type; stdcall;

    //===--------------------------------------------------------------------===//
    // Bind Values To Prepared Statements
    //===--------------------------------------------------------------------===//

    (*!
    Binds a value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Value: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckdb_Value): TDuckDB_State; stdcall;

    (*!
    Retrieve the index of the parameter for the prepared statement, identified by name
    *)
    DuckDB_Bind_Parameter_Index: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx_out: Pidx_t; name: PAnsiChar): TDuckDB_State; stdcall;

    (*!
    Binds a bool value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Boolean: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cbool): TDuckDB_State; stdcall;

    (*!
    Binds an int8_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Int8: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cint8): TDuckDB_State; stdcall;

    (*!
    Binds an int16_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Int16: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cint16): TDuckDB_State; stdcall;

    (*!
    Binds an int32_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Int32: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cint32): TDuckDB_State; stdcall;

    (*!
    Binds an int64_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Int64: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cint64): TDuckDB_State; stdcall;

    (*!
    Binds a duckdb_hugeint value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Hugeint: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDB_HugeInt): TDuckDB_State; stdcall;

    (*!
    Binds an duckdb_uhugeint value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_UHugeint: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDB_UHugeInt): TDuckDB_State; stdcall;

    (*!
    Binds a duckdb_decimal value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Decimal: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDB_Decimal): TDuckDB_State; stdcall;

    (*!
    Binds an uint8_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_UInt8: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cuint8): TDuckDB_State; stdcall;

    (*!
    Binds an uint16_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_UInt16: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cuint16): TDuckDB_State; stdcall;

    (*!
    Binds an uint32_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_UInt32: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cuint32): TDuckDB_State; stdcall;

    (*!
    Binds an uint64_t value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Uint64: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cuint64): TDuckDB_State; stdcall;

    (*!
    Binds a float value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Float: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cfloat): TDuckDB_State; stdcall;

    (*!
    Binds a double value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Double: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: cdouble): TDuckDB_State; stdcall;

    (*!
    Binds a duckdb_date value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Date: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDb_Date): TDuckDB_State; stdcall;

    (*!
    Binds a duckdb_time value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Time: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDb_Time): TDuckDB_State; stdcall;

    (*!
    Binds a duckdb_timestamp value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Timestamp: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDb_TimeStamp): TDuckDB_State; stdcall;

    (*!
    Binds a duckdb_timestamp value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Timestamp_TZ: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDb_TimeStamp): TDuckDB_State; stdcall;

    (*!
    Binds a duckdb_interval value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Interval: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: TDuckDb_Interval): TDuckDB_State; stdcall;

    (*!
    Binds a null-terminated varchar value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Varchar: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: PAnsiChar): TDuckDB_State; stdcall;

    (*!
    Binds a varchar value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Varchar_Length: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; val: PAnsiChar; length: idx_t): TDuckDB_State; stdcall;

    (*!
    Binds a blob value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Blob: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t; data: Pointer; length: idx_t): TDuckDB_State; stdcall;

    (*!
    Binds a NULL value to the prepared statement at the specified index.
    *)
    DuckDB_Bind_Null: function(prepared_statement: TDuckDB_Prepared_Statement; param_idx: idx_t): TDuckDB_State; stdcall;

    //===--------------------------------------------------------------------===//
    // Execute Prepared Statements
    //===--------------------------------------------------------------------===//

    (*!
    Executes the prepared statement with the given bound parameters, and returns a materialized query result.

    This method can be called multiple times for each prepared statement, and the parameters can be modified
    between calls to this function.

    Note that the result must be freed with `duckdb_destroy_result`.

    * @param prepared_statement The prepared statement to execute.
    * @param out_result The query result.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Execute_Prepared: function(prepared_statement: TDuckDB_Prepared_Statement; out_result: PDuckDB_Result): TDuckDB_State; stdcall;

    (*!
    **DEPRECATION NOTICE**: This method is scheduled for removal in a future release.

    Executes the prepared statement with the given bound parameters, and returns an optionally-streaming query result.
    To determine if the resulting query was in fact streamed, use `duckdb_result_is_streaming`

    This method can be called multiple times for each prepared statement, and the parameters can be modified
    between calls to this function.

    Note that the result must be freed with `duckdb_destroy_result`.

    * @param prepared_statement The prepared statement to execute.
    * @param out_result The query result.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    DuckDB_Execute_Prepared_Streaming: function(prepared_statement: TDuckDB_Prepared_Statement; out_result: PDuckDB_Result): TDuckDB_State; stdcall;
  protected
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_DUCKDB}

implementation

{$IFNDEF ZEOS_DISABLE_DUCKDB}

uses ZPlainLoader, ZEncoding{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZSQLitePlainDriver }

function TZDuckDBPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF8'
end;

procedure TZDuckDBPlainDriver.LoadCodePages;
begin
  { MultiByte }
  AddCodePage('UTF8', 1, ceUTF8, zCP_UTF8, 'URF-8', 4);
end;

constructor TZDuckDBPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IFDEF MSWINDOWS}
  FLoader.AddLocation(WINDOWS_VS_DLL_LOCATION);
  FLoader.AddLocation(WINDOWS_MINGW_DLL_LOCATION);
  {$ELSE}
  FLoader.AddLocation(LINUX_DLL_LOCATION);
  FLoader.AddLocation(LINUX_DLL_LOCATION+'.0');
  {$ENDIF}
  LoadCodePages;
end;

function TZDuckDBPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZDuckDBPlainDriver.Create;
end;

procedure TZDuckDBPlainDriver.LoadApi;
begin
  { ************** Load adresses of API Functions ************* }
  with Loader do begin
    @DuckDB_Open := GetAddress('duckdb_open');
    @DuckDB_Open_Ext := GetAddress('duckdb_open_ext');
    @DuckDB_Close := GetAddress('duckdb_close');
    @DuckDB_Connect := GetAddress('duckdb_connect');
    @DuckDB_Interrupt := GetAddress('duckdb_interrupt');
    @DuckDB_Query_Progress := GetAddress('duckdb_query_progress');
    @DuckDB_Disconnect := GetAddress('duckdb_disconnect');
    @DuckDB_Library_Version := GetAddress('duckdb_library_version');
    @DuckDB_Create_Config := GetAddress('duckdb_create_config');
    @DuckDB_Config_Count := GetAddress('duckdb_config_count');
    @DuckDB_Get_Config_Flag := GetAddress('duckdb_get_config_flag');
    @DuckDB_Set_Config := GetAddress('duckdb_set_config');
    @DuckDB_Destroy_Config := GetAddress('duckdb_destroy_config');
    @DuckDB_Query := GetAddress('duckdb_query');
    @DuckDB_Destroy_Result := GetAddress('duckdb_destroy_result');
    @DuckDB_Column_Name := GetAddress('duckdb_column_name');
    @DuckDB_Column_Type := GetAddress('duckdb_column_type');
    @DuckDB_Result_Statement_Type := GetAddress('duckdb_result_statement_type');
    @DuckDB_Column_Logical_Type := GetAddress('duckdb_column_logical_type');
    @DuckDB_Column_Count := GetAddress('duckdb_column_count');
    @DuckDB_Row_Count := GetAddress('duckdb_row_count');
    @DuckDB_Rows_Changed := GetAddress('duckdb_rows_changed');
    @DuckDB_Column_Data := GetAddress('duckdb_column_data');
    @DuckDB_Nullmask_Data := GetAddress('duckdb_nullmask_data');
    @DuckDB_Result_Error := GetAddress('duckdb_result_error');
    @DuckDB_Result_Error_Type := GetAddress('duckdb_result_error_type');
    @DuckDB_Result_Get_Chunk := GetAddress('duckdb_result_get_chunk');
    @DuckDB_Result_Is_Streaming := GetAddress('duckdb_result_is_streaming');
    @DuckDB_Result_Chunk_Count := GetAddress('duckdb_result_chunk_count');
    @DuckDB_Result_Return_Type := GetAddress('duckdb_result_return_type');
    @DuckDB_Value_Boolean := GetAddress('duckdb_value_boolean');
    @DuckDB_Value_Int8 := GetAddress('duckdb_value_int8');
    @DuckDB_Value_Int16 := GetAddress('duckdb_value_int16');
    @DuckDB_Value_Int32 := GetAddress('duckdb_value_int32');
    @DuckDB_Value_Int64 := GetAddress('duckdb_value_int64');
    @DuckDB_Value_Hugeint := GetAddress('duckdb_value_hugeint');
    @DuckDB_Value_UHugeint := GetAddress('duckdb_value_uhugeint');
    @DuckDB_Value_Decimal := GetAddress('duckdb_value_decimal');
    @DuckDB_Value_UInt8 := GetAddress('duckdb_value_uint8');
    @DuckDB_Value_UInt16 := GetAddress('duckdb_value_uint16');
    @DuckDB_Value_UInt32 := GetAddress('duckdb_value_uint32');
    @DuckDB_Value_UInt64 := GetAddress('duckdb_value_uint64');
    @DuckDB_Value_Float := GetAddress('duckdb_value_float');
    @DuckDB_Value_Double := GetAddress('duckdb_value_double');
    @DuckDB_Value_Date := GetAddress('duckdb_value_date');
    @DuckDB_Value_Time := GetAddress('duckdb_value_time');
    @DuckDB_Value_Timestamp := GetAddress('duckdb_value_timestamp');
    @DuckDB_Value_Interval := GetAddress('duckdb_value_interval');
    @DuckDB_Value_Varchar := GetAddress('duckdb_value_varchar');
    @DuckDB_Value_String := GetAddress('duckdb_value_string');
    @DuckDB_Value_Varchar_Internal := GetAddress('duckdb_value_varchar_internal');
    @DuckDB_Value_String_Internal := GetAddress('duckdb_value_string_internal');
    @DuckDB_Value_Blob := GetAddress('duckdb_value_blob');
    @DuckDB_Value_Is_Null := GetAddress('duckdb_value_is_null');
    @DuckDB_Malloc := GetAddress('duckdb_malloc');
    @DuckDB_free := GetAddress('duckdb_free');
    @DuckDB_Vector_Size := GetAddress('duckdb_vector_size');
    @DuckDB_String_Is_Inlined := GetAddress('duckdb_string_is_inlined');
    @DuckDB_String_T_Length := GetAddress('duckdb_string_t_length');
    @DuckDB_String_T_Data := GetAddress('duckdb_string_t_data');
    @DuckDB_From_Date := GetAddress('duckdb_from_date');
    @DuckDB_To_Date := GetAddress('duckdb_to_date');
    @DuckDB_Is_Finite_Date := GetAddress('duckdb_is_finite_date');
    @DuckDB_From_Time := GetAddress('duckdb_from_time');
    @DuckDB_Create_Time_TZ := GetAddress('duckdb_create_time_tz');
    @DuckDB_From_Time_TZ := GetAddress('duckdb_from_time_tz');
    @DuckDB_To_Time := GetAddress('duckdb_to_time');
    @DuckDB_From_Timestamp := GetAddress('duckdb_from_timestamp');
    @DuckDB_To_Timestamp := GetAddress('duckdb_to_timestamp');
    @DuckDB_Is_Finite_Timestamp := GetAddress('duckdb_is_finite_timestamp');
    @DuckDB_Hugeint_To_Double := GetAddress('duckdb_hugeint_to_double');
    @DuckDB_Double_To_Hugeint := GetAddress('duckdb_double_to_hugeint');
    @DuckDB_UHugeint_To_Double := GetAddress('duckdb_uhugeint_to_double');
    @DuckDB_Double_To_UHugeint := GetAddress('duckdb_double_to_uhugeint');
    @DuckDB_Double_To_Decimal := GetAddress('duckdb_double_to_decimal');
    @DuckDB_Decimal_To_Double := GetAddress('duckdb_decimal_do_double');
    @DuckDB_Prepare := GetAddress('duckdb_prepare');
    @DuckDB_Destroy_Prepare := GetAddress('duckdb_destroy_prepare');
    @DuckDB_Prepare_Error := GetAddress('duckdb_prepare_error');
    @DuckDB_NParams := GetAddress('duckdb_nparams');
    @DuckDB_Parameter_Name := GetAddress('duckdb_parameter_name');
    @DuckDB_Param_Type := GetAddress('duckdb_param_type');
    @DuckDB_Clear_Bindings := GetAddress('duckdb_clear_bindings');
    @DuckDB_Prepared_Statement_Type := GetAddress('duckdb_prepared_statement_type');
    @DuckDB_Bind_Value := GetAddress('duckdb_bind_value');
    @DuckDB_Bind_Parameter_Index := GetAddress('duckdb_bind_parameter_index');
    @DuckDB_Bind_Boolean := GetAddress('duckdb_bind_boolean');
    @DuckDB_Bind_Int8 := GetAddress('duckdb_bind_int8');
    @DuckDB_Bind_Int16 := GetAddress('duckdb_bind_int16');
    @DuckDB_Bind_Int32 := GetAddress('duckdb_bind_int32');
    @DuckDB_Bind_Int64 := GetAddress('duckdb_bind_int64');
    @DuckDB_Bind_Hugeint := GetAddress('duckdb_bind_hugeint');
    @DuckDB_Bind_UHugeint := GetAddress('duckdb_bind_uhugeint');
    @DuckDB_Bind_Decimal := GetAddress('duckdb_bind_decimal');
    @DuckDB_Bind_UInt8 := GetAddress('duckdb_bind_uint8');
    @DuckDB_Bind_UInt16 := GetAddress('duckdb_bind_uint16');
    @DuckDB_Bind_UInt32 := GetAddress('duckdb_bind_uint32');
    @DuckDB_Bind_UInt64 := GetAddress('duckdb_bind_uint64');
    @DuckDB_Bind_Float := GetAddress('duckdb_bind_float');
    @DuckDB_Bind_Double := GetAddress('duckdb_bind_double');
    @DuckDB_Bind_Date := GetAddress('duckdb_bind_date');
    @DuckDB_Bind_Time := GetAddress('duckdb_bind_time');
    @DuckDB_Bind_Timestamp := GetAddress('duckdb_bind_timestamp');
    @DuckDB_Bind_Timestamp_TZ := GetAddress('duckdb_bind_timestamp_tz');
    @DuckDB_Bind_Interval := GetAddress('duckdb_bind_interval');
    @DuckDB_Bind_Varchar := GetAddress('duckdb_bind_varchar');
    @DuckDB_Bind_Varchar_Length := GetAddress('duckdb_bind_varchar_length');
    @DuckDB_Bind_Blob := GetAddress('duckdb_bind_blob');
    @DuckDB_Bind_Null := GetAddress('duckdb_bind_null');
    @DuckDB_Execute_Prepared := GetAddress('duckdb_execute_prepared');
    @DuckDB_Execute_Prepared_Streaming := GetAddress('duckdb_execute_prepared_streaming');
  end;
end;

function TZDuckDBPlainDriver.GetProtocol: string;
begin
  Result := 'DuckDB';
end;

function TZDuckDBPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for DuckDB';
end;

{$ENDIF ZEOS_DISABLE_DUCKDB}

end.
