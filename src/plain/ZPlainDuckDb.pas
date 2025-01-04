unit ZPlainDuckDb;

interface

uses
    ctypes;

const
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
    TDuckDB_Prepared_Statement = ^ SDuckDB_Prepared_Statement;

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

    //===--------------------------------------------------------------------===//
    // Function types
    //===--------------------------------------------------------------------===//
    //! Additional function info. When setting this info, it is necessary to pass a destroy-callback function.
    typedef struct _duckdb_function_info {
	    void *internal_ptr;
    } * duckdb_function_info;

    //===--------------------------------------------------------------------===//
    // Scalar function types
    //===--------------------------------------------------------------------===//
    //! A scalar function. Must be destroyed with `duckdb_destroy_scalar_function`.
    typedef struct _duckdb_scalar_function {
	    void *internal_ptr;
    } * duckdb_scalar_function;

    //! A scalar function set. Must be destroyed with `duckdb_destroy_scalar_function_set`.
    typedef struct _duckdb_scalar_function_set {
	    void *internal_ptr;
    } * duckdb_scalar_function_set;

    //! The main function of the scalar function.
    typedef void (*duckdb_scalar_function_t)(duckdb_function_info info, duckdb_data_chunk input, duckdb_vector output);

    //===--------------------------------------------------------------------===//
    // Aggregate function types
    //===--------------------------------------------------------------------===//
    //! An aggregate function. Must be destroyed with `duckdb_destroy_aggregate_function`.
    typedef struct _duckdb_aggregate_function {
	    void *internal_ptr;
    } * duckdb_aggregate_function;

    //! A aggregate function set. Must be destroyed with `duckdb_destroy_aggregate_function_set`.
    typedef struct _duckdb_aggregate_function_set {
	    void *internal_ptr;
    } * duckdb_aggregate_function_set;

    //! Aggregate state
    typedef struct _duckdb_aggregate_state {
	    void *internal_ptr;
    } * duckdb_aggregate_state;

    //! Returns the aggregate state size
    typedef idx_t (*duckdb_aggregate_state_size)(duckdb_function_info info);
    //! Initialize the aggregate state
    typedef void (*duckdb_aggregate_init_t)(duckdb_function_info info, duckdb_aggregate_state state);
    //! Destroy aggregate state (optional)
    typedef void (*duckdb_aggregate_destroy_t)(duckdb_aggregate_state *states, idx_t count);
    //! Update a set of aggregate states with new values
    typedef void (*duckdb_aggregate_update_t)(duckdb_function_info info, duckdb_data_chunk input,
                                              duckdb_aggregate_state *states);
    //! Combine aggregate states
    typedef void (*duckdb_aggregate_combine_t)(duckdb_function_info info, duckdb_aggregate_state *source,
                                               duckdb_aggregate_state *target, idx_t count);
    //! Finalize aggregate states into a result vector
    typedef void (*duckdb_aggregate_finalize_t)(duckdb_function_info info, duckdb_aggregate_state *source,
                                                duckdb_vector result, idx_t count, idx_t offset);

    //===--------------------------------------------------------------------===//
    // Table function types
    //===--------------------------------------------------------------------===//

    //! A table function. Must be destroyed with `duckdb_destroy_table_function`.
    typedef struct _duckdb_table_function {
	    void *internal_ptr;
    } * duckdb_table_function;

    //! The bind info of the function. When setting this info, it is necessary to pass a destroy-callback function.
    typedef struct _duckdb_bind_info {
	    void *internal_ptr;
    } * duckdb_bind_info;

    //! Additional function init info. When setting this info, it is necessary to pass a destroy-callback function.
    typedef struct _duckdb_init_info {
	    void *internal_ptr;
    } * duckdb_init_info;

    //! The bind function of the table function.
    typedef void (*duckdb_table_function_bind_t)(duckdb_bind_info info);

    //! The (possibly thread-local) init function of the table function.
    typedef void (*duckdb_table_function_init_t)(duckdb_init_info info);

    //! The main function of the table function.
    typedef void (*duckdb_table_function_t)(duckdb_function_info info, duckdb_data_chunk output);

    //===--------------------------------------------------------------------===//
    // Cast types
    //===--------------------------------------------------------------------===//

    //! A cast function. Must be destroyed with `duckdb_destroy_cast_function`.
    typedef struct _duckdb_cast_function {
	    void *internal_ptr;
    } * duckdb_cast_function;

    typedef bool (*duckdb_cast_function_t)(duckdb_function_info info, idx_t count, duckdb_vector input,
                                           duckdb_vector output);

    //===--------------------------------------------------------------------===//
    // Replacement scan types
    //===--------------------------------------------------------------------===//

    //! Additional replacement scan info. When setting this info, it is necessary to pass a destroy-callback function.
    typedef struct _duckdb_replacement_scan_info {
	    void *internal_ptr;
    } * duckdb_replacement_scan_info;

    //! A replacement scan function that can be added to a database.
    typedef void (*duckdb_replacement_callback_t)(duckdb_replacement_scan_info info, const char *table_name, void *data);

    //===--------------------------------------------------------------------===//
    // Arrow-related types
    //===--------------------------------------------------------------------===//

    //! Holds an arrow query result. Must be destroyed with `duckdb_destroy_arrow`.
    typedef struct _duckdb_arrow {
	    void *internal_ptr;
    } * duckdb_arrow;

    //! Holds an arrow array stream. Must be destroyed with `duckdb_destroy_arrow_stream`.
    typedef struct _duckdb_arrow_stream {
	    void *internal_ptr;
    } * duckdb_arrow_stream;

    //! Holds an arrow schema. Remember to release the respective ArrowSchema object.
    typedef struct _duckdb_arrow_schema {
	    void *internal_ptr;
    } * duckdb_arrow_schema;

    //! Holds an arrow array. Remember to release the respective ArrowArray object.
    typedef struct _duckdb_arrow_array {
	    void *internal_ptr;
    } * duckdb_arrow_array;

    //===--------------------------------------------------------------------===//
    // DuckDB extension access
    //===--------------------------------------------------------------------===//
    //! Passed to C API extension as parameter to the entrypoint
    struct duckdb_extension_access {
	    //! Indicate that an error has occurred
	    void (*set_error)(duckdb_extension_info info, const char *error);
	    //! Fetch the database from duckdb to register extensions to
	    duckdb_database *(*get_database)(duckdb_extension_info info);
	    //! Fetch the API
	    const void *(*get_api)(duckdb_extension_info info, const char *version);
    };
    *)*)*)*)*)*)*)*)*)*)*)*)*)*)*)*)

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
    TDuckDB_Open = function(const path: PAnsiChar; out_database: PDuckDB_Database): TDuckDB_State; stdcall;

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
    TDuckDB_Open_Ext = function(const path: PAnsiChar; out_database: PDuckDB_Database; config: TDuckdb_Config;
                                            out_error: PPAnsiChar): TDuckDB_State; stdcall;

    (*!
    Closes the specified database and de-allocates all memory allocated for that database.
    This should be called after you are done with any database allocated through `duckdb_open` or `duckdb_open_ext`.
    Note that failing to call `duckdb_close` (in case of e.g. a program crash) will not cause data corruption.
    Still, it is recommended to always correctly close a database object after you are done with it.

    * @param database The database object to shut down.
    *)
    TDuckDB_Close = procedure(database: PDuckDB_Database); stdcall;

    (*!
    Opens a connection to a database. Connections are required to query the database, and store transactional state
    associated with the connection.
    The instantiated connection should be closed using 'duckdb_disconnect'.

    * @param database The database file to connect to.
    * @param out_connection The result connection object.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    TDuckdb_Connect = function(database: TDuckDB_Database; out_connection: PDuckDB_Connection): TDuckDB_State; stdcall;

    (*!
    Interrupt running query

    * @param connection The connection to interrupt
    *)
    TDuckDB_Interrupt = procedure(connection: TDuckDB_Connection); stdcall;

    (*!
    Get progress of the running query

    * @param connection The working connection
    * @return -1 if no progress or a percentage of the progress
    *)
    TDuckDB_Query_Progress = function(connection: TDuckDB_Connection): TDuckDB_Query_Progress_Type; stdcall;

    (*!
    Closes the specified connection and de-allocates all memory allocated for that connection.

    * @param connection The connection to close.
    *)
    TDuckDB_Disconnect = procedure(connection: PDuckDB_Connection); stdcall;

    (*!
    Returns the version of the linked DuckDB, with a version postfix for dev versions

    Usually used for developing C extensions that must return this for a compatibility check.
    *)
    TDuckDB_Library_Version = function(): PAnsiChar; stdcall;

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
    TDuckDB_Create_Config = function(out_config: PDuckDB_Config): TDuckDB_State; stdcall;

    (*!
    This returns the total amount of configuration options available for usage with `duckdb_get_config_flag`.

    This should not be called in a loop as it internally loops over all the options.

    * @return The amount of config options available.
    *)
    TDuckDB_Config_Count = function(): size_t; stdcall;

    (*!
    Obtains a human-readable name and description of a specific configuration option. This can be used to e.g.
    display configuration options. This will succeed unless `index` is out of range (i.e. `>= duckdb_config_count`).

    The result name or description MUST NOT be freed.

    * @param index The index of the configuration option (between 0 and `duckdb_config_count`)
    * @param out_name A name of the configuration flag.
    * @param out_description A description of the configuration flag.
    * @return `DuckDBSuccess` on success or `DuckDBError` on failure.
    *)
    TDuckDB_Get_Config_Flag = function(index: size_t; out_name: PPAnsiChar; out_description: PPAnsiChar): TDuckDB_State; stdcall;

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
    TDuckDB_Set_Config = function(config: TDuckDB_Config; const name: PAnsiChar; const option: PAnsiChar): TDuckDB_State; stdcall;

    (*!
    Destroys the specified configuration object and de-allocates all memory allocated for the object.

    * @param config The configuration object to destroy.
    *)
    TDuckDB_Destroy_Config = procedure(config: PDuckDB_Config); stdcall;

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
    TDuckDB_Query = function(connection: TDuckDB_Connection; const query: PAnsiChar; out_result: PDuckDB_Result): TDuckDB_State; stdcall;

    (*!
    Closes the result and de-allocates all memory allocated for that connection.

    * @param result The result to destroy.
    *)
    TDuckDB_Destroy_Result = procedure(result: PDuckDB_Result); stdcall;

    (*!
    Returns the column name of the specified column. The result should not need to be freed; the column names will
    automatically be destroyed when the result is destroyed.

    Returns `NULL` if the column is out of range.

    * @param result The result object to fetch the column name from.
    * @param col The column index.
    * @return The column name of the specified column.
    *)
    TDuckDB_Column_Name = function(result: PDuckDB_Result; col: idx_t): PAnsiChar; stdcall;

    (*!
    Returns the column type of the specified column.

    Returns `DUCKDB_TYPE_INVALID` if the column is out of range.

    * @param result The result object to fetch the column type from.
    * @param col The column index.
    * @return The column type of the specified column.
    *)
    TDuckDB_Column_Type = function(result: PDuckDB_Result; col: idx_t): TDuckDB_Type; stdcall;

    (*!
    Returns the statement type of the statement that was executed

    * @param result The result object to fetch the statement type from.
    * @return duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID
    *)
    TDuckDB_Result_Statement_Type = function(result: TDuckDB_Result): TDuckDB_Statement_Type; stdcall;

    (*!
    Returns the logical column type of the specified column.

    The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

    Returns `NULL` if the column is out of range.

    * @param result The result object to fetch the column type from.
    * @param col The column index.
    * @return The logical column type of the specified column.
    *)
    TDuckDB_Column_Logical_Type = function(result: PDuckDB_Result; col: idx_t): TDuckDB_Logical_Type; stdcall;

    (*!
    Returns the number of columns present in a the result object.

    * @param result The result object.
    * @return The number of columns present in the result object.
    *)
    TDuckDB_Column_Count = function(result: PDuckDB_Result): idx_t; stdcall;

    (*!
    Returns the number of rows changed by the query stored in the result. This is relevant only for INSERT/UPDATE/DELETE
    queries. For other queries the rows_changed will be 0.

    * @param result The result object.
    * @return The number of rows changed.
    *)
    TDuckDB_Rows_Changed = function(result: PDuckDB_Result): idx_t; stdcall;

    (*!
    Returns the error message contained within the result. The error is only set if `duckdb_query` returns `DuckDBError`.

    The result of this function must not be freed. It will be cleaned up when `duckdb_destroy_result` is called.

    * @param result The result object to fetch the error from.
    * @return The error of the result.
    *)
    TDuckDB_Result_Error = function(result: PDuckDB_Result): PAnsiChar; stdcall;

    (*!
    Returns the result error type contained within the result. The error is only set if `duckdb_query` returns
    `DuckDBError`.

    * @param result The result object to fetch the error from.
    * @return The error type of the result.
    *)
    TDuckDB_Result_Error_Type = function(result: PDuckDB_Result): TDuckDB_Error_Type; stdcall;

    //===--------------------------------------------------------------------===//
    // Result Functions
    //===--------------------------------------------------------------------===//

    (*!
    Returns the return_type of the given result, or DUCKDB_RETURN_TYPE_INVALID on error

    * @param result The result object
    * @return The return_type
    *)
    TDuckDB_Result_Return_Type = function(result: TDuckDB_Result): TDuckDB_Result_Type; stdcall;

    //===--------------------------------------------------------------------===//
    // Safe Fetch Functions
    //===--------------------------------------------------------------------===//

    // These functions will perform conversions if necessary.
    // On failure (e.g. if conversion cannot be performed or if the value is NULL) a default value is returned.
    // Note that these functions are slow since they perform bounds checking and conversion
    // For fast access of values prefer using `duckdb_result_get_chunk`

    // -> These functions were not translated because thy are all marked as deprecated

    //===--------------------------------------------------------------------===//
    // Helpers
    //===--------------------------------------------------------------------===//








    TDuckDB_API = record
      DuckDB_Open: TDuckDB_Open;
	  DuckDB_Open_Ext: TDuckDB_Open_Ext;
	  DuckDB_Close: TDuckDB_Close;
	  DuckDB_State: TDuckDB_State;
	  DuckDB_Interrupt: TDuckDB_Interrupt;
	  DuckDB_Query_Progress: TDuckDB_Query_Progress;
	  DuckDB_Disconnect: TDuckDB_Disconnect;
	  DuckDB_Library_Version: TDuckDB_Library_Version;
	  DuckDB_Create_Config: TDuckDB_Create_Config;
	  DuckDB_Config_Count: TDuckDB_Config_Count;
	  DuckDB_Get_Config_Flag: TDuckDB_Get_Config_Flag;
	  DuckDB_Set_Config: TDuckDB_Set_Config;
	  DuckDB_Destroy_Config: TDuckDB_Destroy_Config;
	  DuckDB_Query: TDuckDB_Query;
	  DuckDB_Destroy_Result: TDuckDB_Destroy_Result;
	  DuckDB_Column_Name: TDuckDB_Column_Name;
	  DuckDB_Column_Type: TDuckDB_Column_Type;
	  DuckDB_Result_Statement_Type: TDuckDB_Result_Statement_Type;
	  DuckDB_Column_Logical_Type: TDuckDB_Column_Logical_Type;
	  DuckDB_Column_Count: TDuckDB_Column_Count;
	  DuckDB_Rows_Changed: TDuckDB_Rows_Changed;
	  DuckDB_Result_Error: TDuckDB_Result_Error;
	  DuckDB_Result_Error_Type: TDuckDB_Result_Error_Type;
	  DuckDB_Result_Return_Type: TDuckDB_Result_Return_Type;

    end;

implementation

end.
