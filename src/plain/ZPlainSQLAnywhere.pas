{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZPlainSQLAnywhere;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ASA}

uses Classes, ZCompatibility, ZPlainDriver;

const
  SQLAnyLibrary = {$IFDEF MSWINDOWS}'dbcapi'{$ELSE}'libdbcapi'{$ENDIF}+SharedSuffix;

  /// <summary>
  ///  Version 1 was the initial version of the C/C++ API.
  /// </summary>
  SQLANY_API_VERSION_1 = 1;
  /// <summary>
  ///  Version 2 introduced the "_ex" functions and the ability to cancel requests.
  /// </summary>
  SQLANY_API_VERSION_2 = 2;
  /// <summary>
  ///  Version 3 introduced the "callback" function.
  /// </summary>
  SQLANY_API_VERSION_3 = 3;
  /// <summary>
  ///  Version 4 introduced NCHAR support and wide inserts.
  /// </summary>
  SQLANY_API_VERSION_4 = 4;
  /// <summary>
  ///  Version 5 introduced a way to reset sent data through sqlany_send_param_data() * and the A_FLOAT data type
  /// </summary>
  SQLANY_API_VERSION_5 = 5;

  SACAPI_VERSION = SQLANY_API_VERSION_5; //switch to latest version

  /// <summary>
  ///  Returns the minimal error buffer size.
  /// </summary>
  SACAPI_ERROR_SIZE = 256;

type
  /// <summary>
  ///  A handle to an interface context
  /// </summary>
  Pa_sqlany_interface_context = ^Ta_sqlany_interface_context;
  Ta_sqlany_interface_context = record end;

  /// <summary>
  ///  A handle to a connection object
  /// </summary>
  Pa_sqlany_connection = ^Ta_sqlany_connection;
  Ta_sqlany_connection = record end;

  /// <summary>
  ///  An address of address to a handle of a statement object
  /// </summary>
  PPa_sqlany_stmt = ^Pa_sqlany_stmt;
  /// <summary>
  ///  An address to a handle of a statement object
  /// </summary>
  Pa_sqlany_stmt = ^Ta_sqlany_stmt;
  /// <summary>
  ///  A handle to a statement object
  /// </summary>
  Ta_sqlany_stmt = record end;

  /// <summary>
  ///  A portable 32-bit signed value
  /// </summary>
  Tsacapi_i32 = type Integer;
  /// <summary>
  ///  A pointer to a portable 32-bit signed value
  /// </summary>
  Psacapi_i32 = ^Tsacapi_i32;
  /// <summary>
  ///  A portable 32-bit unsigned value
  /// </summary>
  Tsacapi_u32 = type Cardinal;
  /// <summary>
  ///  A pointer to a portable 32-bit unsigned value
  /// </summary>
  Psacapi_u32 = ^Tsacapi_u32;
  /// <summary>
  ///  A portable boolean value
  /// </summary>
  Tsacapi_bool = type Tsacapi_i32;

  Psacapi_i32Array = ^Tsacapi_i32Array;
  Tsacapi_i32Array = array[Byte] of Tsacapi_i32;

  TSQLANY_CALLBACK = procedure() {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

{$Z+} //delphi byte enum size to dword size
  /// <summary>
  ///  Specifies the data type being passed in or retrieved.
  /// </summary>
  Ta_sqlany_data_type = (
    /// <summary>
    ///  Invalid data type.
    /// </summary>
    A_INVALID_TYPE,
    /// <summary>
    /// Binary data.  Binary data is treated as-is and no character set conversion is performed.
    /// </summary>
    A_BINARY,
    /// <summary>
    /// String data.  The data where character set conversion is performed.
    /// </summary>
    A_STRING,
    /// <summary>
    /// Double data.  Includes float values.
    /// </summary>
    A_DOUBLE,
    /// <summary>
    /// 64-bit signed integer
    /// </summary>
    A_VAL64,
    /// <summary>
    /// 64-bit unsigned integer
    /// </summary>
    A_UVAL64,
    /// <summary>
    /// 32-bit signed integer
    /// </summary>
    A_VAL32,
    /// <summary>
    /// 32-bit unsigned integer
    /// </summary>
    A_UVAL32,
    /// <summary>
    /// 16-bit signed integer
    /// </summary>
    A_VAL16,
    /// <summary>
    /// 16-bit unsigned integer
    /// </summary>
    A_UVAL16,
    /// <summary>
    /// 8-bit signed integer
    /// </summary>
    A_VAL8,
    /// <summary>
    /// 8-bit unsigned integer
    /// </summary>
    A_UVAL8,
    /// <summary>
    /// Float precision data. available since version 5
    /// </summary>
    A_FLOAT
  );

  PSize_t = ^Tsize_t;
  Tsize_t = NativeUint;

  Psize_tArray = ^Tsize_tArray;
  Tsize_tArray = Array[Byte] of Tsize_t;
  /// <summary>
  /// Returns a description of the attributes of a data value.
  /// </summary>
  Ta_sqlany_data_value = record
    /// <summary>
    /// A pointer to user supplied buffer of data.
    /// </summary>
    buffer: PAnsiChar;
    /// <summary>
    /// The size of the buffer.
    /// </summary>
    buffer_size: Tsize_t;
    /// <summary>
    /// A pointer to the number of valid bytes in the buffer. This value must be less than buffer_size.
    /// </summary>
    length: Psize_t;
    /// <summary>
    /// The type of the data.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    /// A pointer to indicate whether the last fetched data is NULL.
    /// </summary>
    is_null: Psacapi_i32;
  end;
  Pa_sqlany_data_value = ^Ta_sqlany_data_value;

  Pa_sqlany_data_valueArray = ^Ta_sqlany_data_valueArray;
  Ta_sqlany_data_valueArray = array[Byte] of Ta_sqlany_data_value;

  /// <summary>
  /// Returns a description of the attributes of a data value. min version is V4
  /// </summary>
  Ta_sqlany_data_valueV4up = packed record
    /// <summary>
    /// A pointer to user supplied buffer of data.
    /// </summary>
    buffer: PAnsiChar;
    /// <summary>
    /// The size of the buffer.
    /// </summary>
    buffer_size: Tsize_t;
    /// <summary>
    /// A pointer to the number of valid bytes in the buffer. This value must be less than buffer_size.
    /// </summary>
    length: Psize_tArray;
    /// <summary>
    /// The type of the data.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    /// A pointer to indicate whether the last fetched data is NULL.
    /// </summary>
    is_null: Psacapi_i32Array;
    /// <summary>
    /// Indicates whether the buffer value is an pointer to the actual value.
    /// </summary>
    is_address: Tsacapi_i32;
  end;

  Pa_sqlany_data_valueV4up = ^Ta_sqlany_data_valueV4up;

  Pa_sqlany_data_valueV4upArray = ^Ta_sqlany_data_valueV4upArray;
  Ta_sqlany_data_valueV4upArray = array[Byte] of Ta_sqlany_data_valueV4up;

  /// <summary>
  ///  A data direction enumeration.
  /// </summary>
  Ta_sqlany_data_direction = (
    /// <summary>
    ///  Invalid data direction.
    /// </summary>
    DD_INVALID = $0000,
    /// <summary>
    /// Input-only host variables.
    /// </summary>
    DD_INPUT = $0001,
    /// <summary>
    /// Output-only host variables.
    /// </summary>
    DD_OUTPUT = $0002,
    /// <summary>
    /// Input and output host variables.
    /// </summary>
    DD_INPUT_OUTPUT = $0003
  );

  /// <summary>
  /// A bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Ta_sqlany_bind_param = record
    /// <summary>
    /// The direction of the data. (input, output, input_output)
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    /// The actual value of the data.
    /// </summary>
    value: Ta_sqlany_data_value;
    /// <summary>
    /// Name of the bind parameter. This is only used by sqlany_describe_bind_param()
    /// </summary>
    name: PAnsiChar;
  end;
  /// <summary>
  /// A pointer to a bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Pa_sqlany_bind_param = ^Ta_sqlany_bind_param;

  Pa_sqlany_bind_paramArray = ^Ta_sqlany_bind_paramArray;
  Ta_sqlany_bind_paramArray = array[Byte] of Ta_sqlany_bind_param;

  /// <summary>
  /// A bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Ta_sqlany_bind_paramV4Up = record
    /// <summary>
    /// The direction of the data. (input, output, input_output)
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    /// The actual value of the data.
    /// </summary>
    value: Ta_sqlany_data_valueV4Up;
    /// <summary>
    /// Name of the bind parameter. This is only used by sqlany_describe_bind_param()
    /// </summary>
    name: PAnsiChar;
  end;
  /// <summary>
  /// A pointer to a bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Pa_sqlany_bind_paramV4Up = ^Ta_sqlany_bind_paramV4Up;

  Pa_sqlany_bind_paramV4UpArray = ^Ta_sqlany_bind_paramV4UpArray;
  Ta_sqlany_bind_paramV4UpArray = array[Byte] of Ta_sqlany_bind_paramV4up;


  /// <summary>
  /// An enumeration of the native types of values as described by the server.
  /// The value types correspond to the embedded SQL data types.
  /// </summary>
  Ta_sqlany_native_type = (
    /// <summary>
    /// No data type.
    /// </summary>
    DT_NOTYPE = $0000,
    /// <summary>
    /// Null-terminated character string that is a valid date.
    /// </summary>
    DT_DATE = 384,
    /// <summary>
    /// Null-terminated character string that is a valid time.
    /// </summary>
    DT_TIME = 388,
    /// <summary>
    /// a timestamp struct type (not implemented in sacapi.h just known from our old ASA driver and the docs).
    ///  see: http://dcx.sybase.com/sa160/en/dbprogramming/pg-ruby-native-types.html
    /// </summary>
    DT_TIMESTAMP_STRUCT = 390,
    /// <summary>
    /// Null-terminated character string that is a valid timestamp.
    /// </summary>
    DT_TIMESTAMP = 392,
    /// <summary>
    /// Varying length character string, in the CHAR character set, with a two-byte length field.
    /// The maximum length is 32765 bytes. When sending data, you must set the length field.
    /// When fetching data, the database server sets the length field.
    /// The data is not null-terminated or blank-padded.
    /// </summary>
    DT_VARCHAR = 448,
    /// <summary>
    /// Fixed-length blank-padded character string, in the CHAR character set.
    /// The maximum length, specified in bytes, is 32767. The data is not null-terminated.
    /// </summary>
    DT_FIXCHAR = 452,
    /// <summary>
    /// Long varying length character string, in the CHAR character set.
    /// </summary>
    DT_LONGVARCHAR = 456,
    /// <summary>
    /// Null-terminated character string, in the CHAR character set.
    /// The string is blank-padded if the database is initialized with blank-padded strings.
    /// </summary>
    DT_STRING = 460,
    /// <summary>
    /// 8-byte floating-point number.
    /// </summary>
    DT_DOUBLE = 480,
    /// <summary>
    /// 4-byte floating-point number.
    /// </summary>
    DT_FLOAT = 482,
    /// <summary>
    /// Packed decimal number (proprietary format).
    /// </summary>
    DT_DECIMAL = 484,
    /// <summary>
    /// 32-bit signed integer.
    /// </summary>
    DT_INT = 496,
    /// <summary>
    /// 16-bit signed integer.
    /// </summary>
    DT_SMALLINT = 500,
    /// <summary>
    /// Varying length binary data with a two-byte length field.
    /// The maximum length is 32765 bytes. When supplying information to the database server,
    /// you must set the length field. When fetching information from the database server, the server sets the length field.
    /// </summary>
    DT_BINARY = 524,
    /// <summary>
    /// Long binary data.
    /// </summary>
    DT_LONGBINARY = 528,
    /// <summary>
    ///  a variable datatype (not implemented in sacapi.h just known from our old ASA driver and the docs).
    ///  see: http://dcx.sybase.com/sa160/en/dbprogramming/pg-ruby-native-types.html
    /// </summary>
    DT_VARIABLE = 600,
    /// <summary>
    /// 8-bit signed integer.
    /// </summary>
    DT_TINYINT = 604,
    /// <summary>
    /// 64-bit signed integer.
    /// </summary>
    DT_BIGINT = 608,
    /// <summary>
    /// 32-bit unsigned integer.
    /// </summary>
    DT_UNSINT = 612,
    /// <summary>
    /// 16-bit unsigned integer.
    /// </summary>
    DT_UNSSMALLINT = 616,
    /// <summary>
    /// 64-bit unsigned integer.
    /// </summary>
    DT_UNSBIGINT = 620,
    /// <summary>
    /// 8-bit signed integer.
    /// </summary>
    DT_BIT = 624,
    /// <summary>
    /// Null-terminated character string, in the NCHAR character set.
    /// The string is blank-padded if the database is initialized with blank-padded strings.
    /// </summary>
    DT_NSTRING = 628,
    /// <summary>
    /// Fixed-length blank-padded character string, in the NCHAR character set.
    /// The maximum length, specified in bytes, is 32767. The data is not null-terminated.
    /// </summary>
    DT_NFIXCHAR = 632,
    /// <summary>
    /// Varying length character string, in the NCHAR character set, with a two-byte length field.
    /// The maximum length is 32765 bytes. When sending data, you must set the length field.
    /// When fetching data, the database server sets the length field. The data is not null-terminated or blank-padded.
    /// </summary>
    DT_NVARCHAR = 636,
    /// <summary>
    /// Long varying length character string, in the NCHAR character set.
    /// </summary>
    DT_LONGNVARCHAR = 640
  );

  /// <summary>
  ///  Returns column metadata information.
  ///  sqlany_get_column_info() can be used to populate this structure.
  /// </summary>
  Ta_sqlany_column_info = packed record
    /// <summary>
    ///  The name of the column (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The column data type.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    ///  The native type of the column in the database.
    /// </summary>
    native_type: Ta_sqlany_native_type;
    /// <summary>
    ///  The precision.
    /// </summary>
    precision: word;
    /// <summary>
    ///  The scale.
    /// </summary>
    scale: word;
    /// <summary>
    ///  The maximum size a data value in this column can take.
    /// </summary>
    max_size: Tsize_t;
    /// <summary>
    ///  Indicates whether a value in the column can be null.
    /// </summary>
    nullable: Tsacapi_bool;
  end;
  /// <summary>
  ///  A pointer to the column metadata information.
  /// </summary>
  Pa_sqlany_column_info = ^Ta_sqlany_column_info;

  /// <summary>
  ///  Returns column metadata information. min Version is V4
  ///  sqlany_get_column_info() can be used to populate this structure.
  /// </summary>
  Ta_sqlany_column_infoV4up = packed record
    /// <summary>
    ///  The name of the column (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The column data type.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    ///  The native type of the column in the database.
    /// </summary>
    native_type: Ta_sqlany_native_type;
    /// <summary>
    ///  The precision.
    /// </summary>
    precision: word;
    /// <summary>
    ///  The scale.
    /// </summary>
    scale: word;
    /// <summary>
    ///  The maximum size a data value in this column can take.
    /// </summary>
    max_size: Tsize_t;
    /// <summary>
    ///  Indicates whether a value in the column can be null.
    /// </summary>
    nullable: Tsacapi_bool;
    /// <summary>
    ///  The name of the table (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    table_name: PAnsiChar;
    /// <summary>
    ///  The name of the owner (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    owner_name: PAnsiChar;
    /// <summary>
    ///  Indicates whether the column is bound to a user buffer.
    /// </summary>
    is_bound: Tsacapi_bool;
    /// <summary>
    ///  Information about the bound column.
    /// </summary>
    binding: Ta_sqlany_data_valueV4up;
  end;
  /// <summary>
  ///  A pointer to the column metadata information of version 4 and up.
  /// </summary>
  Pa_sqlany_column_infoV4up = ^Ta_sqlany_column_infoV4up;

  /// <summary>
  ///  Gets information about the currently bound parameters.
  ///  sqlany_get_bind_param_info() can be used to populate this structure.
  /// </summary>
  Ta_sqlany_bind_param_info = packed record
    /// <summary>
    ///  A pointer to the name of the parameter.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The direction of the parameter.
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    ///  Information about the bound input value.
    /// </summary>
    input_value: Ta_sqlany_data_value;
    /// <summary>
    ///  Information about the bound output value.
    /// </summary>
    output_value: Ta_sqlany_data_value;
  end;
  Pa_sqlany_bind_param_info = ^Ta_sqlany_bind_param_info;

  Ta_sqlany_bind_param_infoV4up = packed record
    /// <summary>
    ///  A pointer to the name of the parameter.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The direction of the parameter.
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    ///  Information about the bound input value.
    /// </summary>
    input_value: Ta_sqlany_data_value;
    /// <summary>
    ///  Information about the bound output value.
    /// </summary>
    output_value: Ta_sqlany_data_value;
    /// <summary>
    ///  The native type of the column in the database.
    /// </summary>
    native_type: Ta_sqlany_native_type;
    /// <summary>
    ///  The precision.
    /// </summary>
    precision: word;
    /// <summary>
    ///  The scale.
    /// </summary>
    scale: word;
    /// <summary>
    ///  The maximum size a data value in this column can take.
    /// </summary>
    max_size: Tsize_t;
  end;
  Pa_sqlany_bind_param_infoV4up = ^Ta_sqlany_bind_param_infoV4up;

  /// <summary>
  ///  Returns metadata information about a column value in a result set.
  ///  sqlany_get_data_info() can be used to populate this structure with
  ///  information about what was last retrieved by a fetch operation.
  /// </summary>
  Ta_sqlany_data_info = packed record
    /// <summary>
    ///  The type of the data in the column.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    ///  Indicates whether the last fetched data is NULL.
    ///  This field is only valid after a successful fetch operation.
    /// </summary>
    is_null: Tsacapi_bool;
    /// <summary>
    ///  The total number of bytes available to be fetched.
    ///  This field is only valid after a successful fetch operation.
    /// </summary>
    data_size: Tsize_t;
  end;
  Pa_sqlany_data_info = ^Ta_sqlany_data_info;

  /// <summary>
  ///  An enumeration of the callback types.
  /// </summary>
  Ta_sqlany_callback_type = (
    /// <summary>
    ///  This function is called just before a database request is sent to the server.
    ///  CALLBACK_START is used only on Windows operating systems.
    /// </summary>
    CALLBACK_START = 0,
    /// <summary>
    ///  This function is called repeatedly by the interface library while the database server or client library is busy processing your database request.
    /// </summary>
    CALLBACK_WAIT,
    /// <summary>
    ///  This function is called after the response to a database request has been received by the DBLIB interface DLL.
    ///  CALLBACK_FINISH is used only on Windows operating systems.
    /// </summary>
    CALLBACK_FINISH,
    /// <summary>
    ///  This function is called when messages are received from the server during the processing of a request.
    ///  Messages can be sent to the client application from the database server using the SQL MESSAGE statement.
    ///  Messages can also be generated by long running database server statements
    /// </summary>
    DB_CALLBACK_MESSAGE = 7,
    /// <summary>
    ///  This function is called when the database server is about to drop a connection because of a liveness timeout,
    ///  through a DROP CONNECTION statement, or because the database server is being shut down.
    ///  The connection name conn_name is passed in to allow you to distinguish between connections.
    ///  If the connection was not named, it has a value of NULL.
    /// </summary>
    CALLBACK_CONN_DROPPED,
    /// <summary>
    ///  This function is called once for each debug message and is passed a null-terminated string containing the text of the debug message.
    ///  A debug message is a message that is logged to the LogFile file. In order for a debug message to be passed to this callback, the LogFile
    ///  connection parameter must be used. The string normally has a newline character (\n) immediately before the terminating null character.
    /// </summary>
    CALLBACK_DEBUG_MESSAGE,
    /// <summary>
    ///  This function is called when a file transfer requires validation.
    ///  If the client data transfer is being requested during the execution of indirect statements such as from within a stored procedure,
    ///  the client library will not allow a transfer unless the client application has registered a validation callback and the response from
    ///  the callback indicates that the transfer may take place.
    /// </summary>
    CALLBACK_VALIDATE_FILE_TRANSFER
    );

  /// <summary>
  ///  An enumeration of the message types for the MESSAGE callback.
  /// </summary>
  Ta_sqlany_message_type = (
    /// <summary>
    ///  The message type was INFO.
    /// </summary>
    MESSAGE_TYPE_INFO = 0,
    /// <summary>
    ///  The message type was WARNING.
    /// </summary>
    MESSAGE_TYPE_WARNING,
    /// <summary>
    ///  The message type was ACTION.
    /// </summary>
    MESSAGE_TYPE_ACTION,
    /// <summary>
    ///  The message type was STATUS.
    /// </summary>
    MESSAGE_TYPE_STATUS,
    /// <summary>
    ///  Initializes the interface.
    ///  This type of message is generated by long running database server statements such as BACKUP DATABASE and LOAD TABLE.
    /// </summary>
    MESSAGE_TYPE_PROGRESS
  );

  PZSQLAnyDateTime = ^TZSQLAnyDateTime;
  TZSQLAnyDateTime = packed record
    Year             : SmallInt;  //* e.g. 1992
    Month            : Byte;      //* 0-11
    Day_of_Week      : Byte;      //* 0-6  0=Sunday, 1=Monday, ...
    Day_of_Year      : SmallInt;  //* 0-365
    Day              : Byte;      //* 1-31
    Hour             : Byte;      //* 0-23
    Minute           : Byte;      //* 0-59
    Second           : Byte;      //* 0-59
    MicroSecond      : LongInt;   //* 0-999999
  end;

  TZSQLAnywherePlainDriver = class(TZAbstractPlainDriver)
  public
    /// <summary>
    ///  Initializes the interface.
    /// </summary>
    /// <param name="app_name">
    ///  A string that names the application that is using the API.  For example, "PHP", "PERL", or "RUBY".
    /// </param>
    /// <param name="api_version">
    ///  The version of the compiled application.
    /// </param>
    /// <param name="version_available">
    ///  An optional argument to return the maximum supported API version.
    /// </param>
    /// <returns>
    ///  1 on success, 0 otherwise.
    /// </returns>
    sqlany_init: function(app_name: PAnsiChar; api_version: Tsacapi_u32;
      version_available: Psacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Initializes the interface.
    /// </summary>
    /// <param name="app_name">
    ///  A string that names the application that is using the API.  For example, "PHP", "PERL", or "RUBY".
    /// </param>
    /// <param name="api_version">
    ///  The version of the compiled application.
    ///  This should normally be one of the SQLANY_API_VERSION_* macros
    /// </param>
    /// <param name="version_available">
    ///  An optional argument to return the maximum supported API version.
    /// </param>
    /// <returns>
    ///  a context object on success and NULL on failure.
    /// </returns>
    sqlany_init_ex: function(app_name: PAnsiChar; api_version: Tsacapi_u32;
      version_available: Psacapi_u32): Pa_sqlany_interface_context; cdecl;
    /// <summary>
    ///  Finalizes the interface.
    ///  Frees any resources allocated by the API.
    /// </summary>
    sqlany_fini: procedure(); cdecl;
    /// <summary>
    ///  Finalize the interface that was created using the specified context.
    ///  Frees any resources allocated by the API.
    /// </summary>
    /// <param name="context">
    ///  A context object that was returned from sqlany_init_ex().
    /// </param>
    sqlany_fini_ex: procedure(context: Pa_sqlany_interface_context); cdecl;
    /// <summary>
    ///  Creates a connection object.
    ///  You must create an API connection object before establishing a database connection. Errors can be retrieved
    ///  from the connection object. Only one request can be processed on a connection at a time. In addition,
    ///  not more than one thread is allowed to access a connection object at a time. Undefined behavior or a failure
    ///  occurs when multiple threads attempt to access a connection object simultaneously.
    /// </summary>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_new_connection: function: Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Creates a connection object using a context.
    ///  An API connection object needs to be created before a database connection is established. Errors can be retrieved
    ///  from the connection object. Only one request can be processed on a connection at a time. In addition,
    ///  not more than one thread is allowed to access a connection object at a time. If multiple threads attempt
    ///  to access a connection object simultaneously, then undefined behavior/crashes will occur.
    /// </summary>
    /// <param name="context">
    ///  A context object that was returned from sqlany_init_ex().
    /// </param>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_new_connection_ex: function(context: Pa_sqlany_interface_context): Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Frees the resources associated with a connection object.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object created with sqlany_new_connection().
    /// </param>
    sqlany_free_connection: procedure(sqlany_conn: Pa_sqlany_connection); cdecl;
    /// <summary>
    ///  Creates a connection object based on a supplied DBLIB SQLCA pointer.
    /// </summary>
    /// <param name="arg">
    ///  A void * pointer to a DBLIB SQLCA object.
    /// </param>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_make_connection: function(arg: Pointer): Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Creates a connection object based on a supplied DBLIB SQLCA pointer.
    /// </summary>
    /// <param name="context">
    ///  A valid context object that was created by sqlany_init_ex().
    /// </param>
    /// <param name="arg">
    ///  A void * pointer to a DBLIB SQLCA object.
    /// </param>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_make_connection_ex: function(constext: Pa_sqlany_interface_context;
      arg: Pointer): Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Creates a connection to a SQL Anywhere database server using the supplied connection object and connection string.
    ///  The supplied connection object must first be allocated using sqlany_new_connection().
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object created by sqlany_new_connection().
    /// </param>
    /// <param name="str">
    ///  A SQL Anywhere connection string.
    /// </param>
    /// <returns>
    ///  1 if the connection is established successfully or 0 when the connection fails.
    ///  Use sqlany_error() to retrieve the error code and message.
    /// </returns>
    sqlany_connect: function(sqlany_conn: Pa_sqlany_connection; str: PAnsichar): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Disconnects an already established SQL Anywhere connection.
    ///  All uncommitted transactions are rolled back.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    /// <returns>
    ///  1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_disconnect: function(sqlany_conn: Pa_sqlany_connection): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Cancel an outstanding request on a connection.
    ///  This function can be used to cancel an outstanding request on a specific connection.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    sqlany_cancel: procedure(sqlany_conn: Pa_sqlany_connection); cdecl;
    /// <summary>
    ///  Executes the supplied SQL statement immediately without returning a result set.
    ///  This function is useful for SQL statements that do not return a result set.
    /// </summary>
    /// </param>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// <param name="sql">
    ///  A string representing the SQL statement to be executed.
    /// </param>
    /// <returns>
    /// 1 on success or 0 on failure.
    /// </returns>
    sqlany_execute_immediate: function(sqlany_conn: Pa_sqlany_connection;
      sql: PAnsiChar): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Prepares a supplied SQL string
    ///  Execution does not happen until sqlany_execute() is
    ///  called. The returned statement object should be freed using sqlany_free_stmt()
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// <param name="sql">
    ///  The SQL statement to be prepared.
    /// </param>
    /// <returns>
    ///  A handle to a SQL Anywhere statement object or NULL if an error occours.
    ///  The statement object can be used by sqlany_execute() to execute the statement.
    /// </returns>
    sqlany_prepare: function(sqlany_conn: Pa_sqlany_connection;
      sql: PAnsiChar): Pa_sqlany_stmt; cdecl;
    /// <summary>
    ///  Frees resources associated with a prepared statement object
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object returned by the successful execution of sqlany_prepare() or sqlany_execute_direct().
    /// </param>
    sqlany_free_stmt: procedure(sqlany_stmt: Pa_sqlany_stmt); cdecl;
    /// <summary>
    ///  Returns the number of parameters expected for a prepared statement
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object returned by the successful execution of sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The expected number of parameters, or -1 if the statement object is not valid.
    /// </returns>
    sqlany_num_params: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///   Describes the bind parameters of a prepared statement
    ///  This function allows the caller to determine information about prepared statement parameters.  The type of prepared
    ///  statement, stored procedured or a DML, determines the amount of information provided.  The direction of the parameters
    ///  (input, output, or input-output) are always provided.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the parameter. This number must be between 0 and sqlany_num_params() - 1.
    /// <param name="param">
    ///  An a_sqlany_bind_param structure that is populated with information.
    /// </param>
    /// <returns>
    ///  1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_describe_bind_param: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; param: Pa_sqlany_bind_param): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Bind a user-supplied buffer as a parameter to the prepared statement.
    /// </summary>
    /// </param>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// <param name="index">
    ///  The index of the parameter. This number must be between 0 and sqlany_num_params() - 1.
    /// <param name="param">
    ///  An a_sqlany_bind_param structure description of the parameter to be bound.
    /// </param>
    /// <returns>
    ///  1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_bind_param: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; param: Pa_sqlany_bind_param): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Sends data as part of a bound parameter.
    ///  This method can be used to send a large amount of data for a bound parameter in chunks^.
    ///  This method can be used only when the batch size is 1.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the parameter. This number must be between 0 and sqlany_num_params() - 1.
    /// </param>
    /// <param name="buffer">
    ///  The data to be sent.
    /// </param>
    /// <param name="size">
    ///  The number of bytes to send.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_send_param_data: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; buffer: PAnsiChar; size: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Clears param data that was previously been set using \sa sqlany_send_param_data().
    ///  This method can be used to clear data that was previously been sent using sqlany_send_param_data()
    ///  If no param data was previously sent, nothing is changed.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  param index The index of the parameter. This should be a number between 0 and sqlany_num_params() - 1.
    /// </param>
    /// <param name="buffer">
    ///  The data to be sent.
    /// </param>
    /// <param name="size">
    ///  The number of bytes to send.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_reset_param_data: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the length of the last error message stored in the connection object
    ///  including the NULL terminator. If there is no error, 0 is returned.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    /// <returns>
    ///  The length of the last error message including the NULL terminator.
    /// </returns>
    sqlany_error_length: function(sqlany_conn: Pa_sqlany_connection): Tsize_t; cdecl;
    /// <summary>
    ///  Sets the size of the row array for a batch execute
    ///  The batch size is used only for an INSERT statement. The default batch size is 1.
    ///  A value greater than 1 indicates a wide insert.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="num_rows">
    ///  The number of rows for batch execution. The value must be 1 or greater.
    /// </param>
    /// <returns>
    ///  return 1 on success or 0 on failure.
    /// </returns>
    sqlany_set_batch_size: function(sqlany_stmt: Pa_sqlany_stmt;
      num_rows: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Sets the bind type of parameters.
    ///  The default value is 0, which indicates column-wise binding. A non-zero value indicates
    ///  row-wise binding and specifies the byte size of the data structure that stores the row.
    ///  The parameter is bound to the first element in a contiguous array of values. The address
    ///  offset to the next element is computed based on the bind type:
    ///
    ///  Column-wise binding - the byte size of the parameter type
    ///  Row-wise binding - the row_size
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="row_size">
    ///  The byte size of the row. A value of 0 indicates column-wise binding and a positive value indicates row-wise binding.
    /// </param>
    /// <returns>
    ///  return 1 on success or 0 on failure.
    /// </returns>
    sqlany_set_param_bind_type: function(sqlany_stmt: Pa_sqlany_stmt; row_size: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the size of the row array for a batch execute.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The size of the row array.
    /// </returns>
    sqlany_get_batch_size: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_u32; cdecl;
    /// <summary>
    ///  Sets the size of the row set to be fetched by the sqlany_fetch_absolute() and sqlany_fetch_next() functions.
    ///  The default size of the row set is 1. Specifying num_rows to be a value greater than 1 indicates a wide fetch.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="num_rows">
    ///  The size of the row set. The value must be 1 or greater.
    /// </param>
    /// <returns>
    ///  return 1 on success or 0 on failure.
    /// </returns>
    sqlany_set_rowset_size: function(sqlany_stmt: Pa_sqlany_stmt;
      num_rows: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the size of the row set to be fetched by the sqlany_fetch_absolute() and sqlany_fetch_next() functions.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The size of the row set, or 0 if the statement does not return a result set.
    /// </returns>
    sqlany_get_rowset_size: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_u32; cdecl;
    /// <summary>
    ///  Sets the bind type of columns.
    ///  The default value is 0, which indicates column-wise binding. A non-zero value indicates
    ///  row-wise binding and specifies the byte size of the data structure that stores the row.
    ///  The column is bound to the first element in a contiguous array of values. The address
    ///  offset to the next element is computed based on the bind type:
    ///
    ///  Column-wise binding - the byte size of the column type
    ///  Row-wise binding - the row_size
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="row_size">
    ///  The byte size of the row. A value of 0 indicates column-wise binding and a positive value indicates row-wise binding.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_set_column_bind_type: function(sqlany_stmt: Pa_sqlany_stmt; row_size: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Binds a user-supplied buffer as a result set column to the prepared statement.
    ///  If the size of the fetched row set is greater than 1, the buffer must be large enough to
    ///  hold the data of all of the rows in the row set. This function can also be used to clear the
    ///  binding of a column by specifying value to be NULL.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the column. This number must be between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="value">
    ///  An a_sqlany_data_value structure describing the bound buffers, or NULL to clear previous binding information.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on unsuccessful.
    /// </returns>
    sqlany_bind_column: function(sqlany_stmt: Pa_sqlany_stmt; index: Tsacapi_u32;
      value: Pa_sqlany_data_valueV4up): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Removes all column bindings defined using sqlany_bind_column().
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_clear_column_bindings: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the number of rows fetched.
    ///  In general, the number of rows fetched is equal to the size specified by the sqlany_set_rowset_size() function. The
    ///  exception is when there are fewer rows from the fetch position to the end of the result set than specified, in which
    ///  case the number of rows fetched is smaller than the specified row set size. The function returns -1 if the last fetch
    ///  was unsuccessful or if the statement has not been executed. The function returns 0 if the statement has been executed
    ///  but no fetching has been done.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The number of rows fetched or -1 on failure
    /// </returns>
    sqlany_fetched_rows: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Sets the current row in the fetched row set.
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The functions
    ///  sqlany_get_column(), sqlany_get_data(), sqlany_get_data_info() are used to retrieve data
    ///  at the current row.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="row_num">
    ///  The row number within the row set. The valid values are from 0 to sqlany_fetched_rows() - 1.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure
    /// </returns>
    sqlany_set_rowset_pos: function(sqlany_stmt: Pa_sqlany_stmt; row_num: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Resets a statement to its prepared state condition.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure
    /// </returns>
    sqlany_reset: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves information about the parameters that were bound using sqlany_bind_param().
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the parameter. This number should be between 0 and sqlany_num_params() - 1.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure
    /// </returns>
    sqlany_get_bind_param_info: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; info: Pa_sqlany_bind_param_info): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Executes a prepared statement.
    ///  You can use sqlany_num_cols() to verify if the executed statement returned a result set.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///   1 if the statement is executed successfully or 0 on failure.
    /// </returns>
    sqlany_execute: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Executes the SQL statement specified by the string argument and possibly returns a result set.
    ///  Use this method to prepare and execute a statement.
    ///  or instead of calling sqlany_prepare() followed by sqlany_execute()
    ///  This function cannot be used for executing a SQL statement with parameters.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    /// <param name="sql_str">
    ///  A SQL string. The SQL string should not have parameters such as ?.
    /// </param>
    /// <returns>
    ///   A statement handle if the function executes successfully, NULL when the function executes unsuccessfully.
    /// </returns>
    sqlany_execute_direct: function(sqlany_conn: Pa_sqlany_connection;
      sql_str: PAnsiChar): Pa_sqlany_stmt; cdecl;
    /// <summary>
    ///  Moves the current row in the result set to the specified row number and then fetches
    ///  rows of data starting from the current row.
    ///  The number of rows fetched is set using the sqlany_set_rowset_size() function. By default, one row is returned.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="row_num">
    ///  The row number to be fetched. The first row is 1, the last row is -1.
    /// </param>
    /// <returns>
    ///   1 if the fetch was successfully, 0 when the fetch is unsuccessful.
    /// </returns>
    sqlany_fetch_absolute: function(sqlany_stmt: Pa_sqlany_stmt;
      row_num: Tsacapi_i32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the next set of rows from the result set.
    ///  When the result object is first created, the current row
    ///  pointer is set to before the first row, that is, row 0.
    ///  This function first advances the row pointer to the next
    ///  unfetched row and then fetches rows of data starting from
    ///  that row. The number of rows fetched is set by the
    ///  sqlany_set_rowset_size() function. By default, one row is returned.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///   1 if the fetch was successfully, 0 when the fetch is unsuccessful.
    /// </returns>
    sqlany_fetch_next: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Advances to the next result set in a multiple result set query.
    ///  If a query (such as a call to a stored procedure) returns multiple result sets, then this function
    ///  advances from the current result set to the next.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///   1 if the statement successfully advances to the next result set, 0 otherwise.
    /// </returns>
    sqlany_get_next_result: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the number of rows affected by execution of the prepared statement.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///   The number of rows affected or -1 on failure.
    /// </returns>
    sqlany_affected_rows: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Returns number of columns in the result set.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///  The number of columns in the result set or -1 on a failure.
    /// </returns>
    sqlany_num_cols: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  By default this function only returns an estimate. To return an exact count, set the row_counts option
    ///  on the connection.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///  The number rows in the result set. If the number of rows is an estimate, the number returned is
    ///  negative and the estimate is the absolute value of the returned integer. The value returned is positive
    ///  if the number of rows is exact.
    /// </returns>
    sqlany_num_rows: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The current
    ///  row is set using the sqlany_set_rowset_pos() function.
    ///  For A_BINARY and A_STRING * data types,
    ///  value->buffer points to an internal buffer associated with the result set.
    ///  Do not rely upon or alter the content of the pointer buffer as it changes when a
    ///  new row is fetched or when the result set object is freed.  Users should copy the
    ///  data out of those pointers into their own buffers.
    ///  The value->length field indicates the number of valid characters that
    ///  value->buffer points to. The data returned in value->buffer is not
    ///  null-terminated. This function fetches all the returned values from the SQL
    ///  Anywhere database server.  For example, if the column contains
    ///  a blob, this function attempts to allocate enough memory to hold that value.
    ///  If you do not want to allocate memory, use sqlany_get_data() instead.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="buffer">
    ///  An a_sqlany_data_value object to be filled with the data fetched for column col_index at the current row in the row set.
    /// </param>
    /// <returns>
    ///  1 on success or 0 for failure. A failure can happen if any of the parameters are invalid or if there is
    ///  not enough memory to retrieve the full value from the SQL Anywhere database server.
    ///  if the number of rows is exact.
    /// </returns>
    sqlany_get_column: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      buffer: Pa_sqlany_data_value): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the data fetched for the specified column at the current row into the supplied buffer memory
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The current
    ///  row is set using the sqlany_set_rowset_pos() function.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="offset">
    ///  The starting offset of the data to get.
    /// </param>
    /// <param name="buffer">
    ///  A buffer to be filled with the contents of the column at the current row in the row set.
    ///  The buffer pointer must be aligned correctly for the data type copied into it.
    /// </param>
    /// <param name="size">
    ///  The size of the buffer in bytes. The function fails. The function fails
    ///   if you specify a size greater than 2^31 - 1
    /// </param>
    /// <returns>
    ///  The number of bytes successfully copied into the supplied buffer.
    ///  This number must not exceed 2^31 - 1. 0 indicates that no data remains to be copied. -1 indicates a failure.
    ///  if the number of rows is exact.
    /// </returns>
    sqlany_get_data: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      offset: Tsize_t; Buffer: Pointer; size: Tsize_t): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Retrieves information about the fetched data at the current row
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The current
    ///  row is set using the sqlany_set_rowset_pos() function.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="buffer">
    ///  A data info buffer to be filled with the metadata about the data at the current row in the row set.
    /// </param>
    /// <returns>
    ///   1 on success, and 0 on failure. Failure is returned when any of the supplied parameters are invalid.
    /// </returns>
    sqlany_get_data_info: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      buffer: Pa_sqlany_data_info): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves column metadata information and fills the a_sqlany_column_info structure with information about the column.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="buffer">
    ///  A column info structure to be filled with column information.
    /// </param>
    /// <returns>
    ///   1 on success or 0 if the column index is out of range, or if the statement does not return a result set.
    /// </returns>
    sqlany_get_column_info: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      buffer: Pa_sqlany_column_info): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Commits the current transaction.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  The connection object on which the commit operation is performed.
    /// <returns>
    ///   return 1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_commit: function(sqlany_conn: Pa_sqlany_connection): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Rolls back the current transaction.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  The connection object on which the rollback operation is to be performed.
    /// </param>
    /// <returns>
    ///   return 1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_rollback: function(sqlany_conn: Pa_sqlany_connection): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the current client version.
    ///  This method fills the buffer passed with the major, minor, patch, and build number of the client library.
    ///  The buffer will be null-terminated.
    /// </summary>
    /// <param name="buffer">
    ///  The buffer to be filled with the client version string.
    /// </param>
    /// <returns>
    /// <param name="len">
    ///  The length of the buffer supplied.
    /// <returns>
    ///   1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_client_version: function(buffer: PAnsiChar; len: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the current client version.
    ///  This method fills the buffer passed with the major, minor, patch, and build number of the client library.
    ///  The buffer will be null-terminated.
    /// </summary>
    /// <param name="context">
    ///  The object that was created with sqlany_init_ex().
    /// </param>
    /// <param name="buffer">
    ///  The buffer to be filled with the client version string.
    /// </param>
    /// <param name="len">
    ///  The length of the buffer supplied.
    /// </param>
    /// <returns>
    ///   1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_client_version_ex: function(context: Pa_sqlany_interface_context;
      buffer: PAnsiChar; len: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the last error code and message stored in the connection object.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    /// <param name="buffer">
    ///  A buffer to be filled with the error message.
    /// </param>
    /// <param name="size">
    ///  The size of the supplied buffer.
    /// </param>
    /// <returns>
    ///   The last error code. Positive values are warnings, negative values are errors, and 0 indicates success.
    /// </returns>
    sqlany_error: function(sqlany_conn: Pa_sqlany_connection;
      buffer: PAnsiChar; size: Tsize_t): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Retrieves the current SQLSTATE.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    /// <param name="buffer">
    ///  A buffer to be filled with the current 5-character SQLSTATE.
    /// </param>
    /// <param name="size">
    ///  The buffer size.
    /// </param>
    /// <returns>
    ///   The number of bytes copied into the buffer.
    /// </returns>
    sqlany_sqlstate: function(sqlany_conn: Pa_sqlany_connection;
      buffer: PAnsiChar; size: Tsize_t): Tsize_t; cdecl;
    /// <summary>
    ///   Clears the last stored error code
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    sqlany_clear_error: procedure(sqlany_conn: Pa_sqlany_connection); cdecl;
    /// <summary>
    ///  Register a callback routine.
    ///  This function can be used to register callback functions.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    /// <param name="index">
    ///  Any of the callback types listed below.
    /// </param>
    /// <param name="callback">
    ///  Address of the callback routine.
    /// </param>
    /// <returns>
    ///   1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_register_callback: function(sqlany_conn: Pa_sqlany_connection;
      index: Ta_sqlany_callback_type; callback: TSQLANY_CALLBACK): Tsacapi_bool; cdecl;
  protected
    procedure LoadApi; override;
  public
    procedure LoadCodePages; override;
    constructor Create;

    function Clone: IZPlainDriver; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;


{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses ZPlainLoader, ZEncoding;

{ TZSQLAnywherePlainDriver }

function TZSQLAnywherePlainDriver.Clone: IZPlainDriver;
begin
  Result := TZSQLAnywherePlainDriver.Create;
end;

constructor TZSQLAnywherePlainDriver.Create;
begin
  FLoader := TZNativeLibraryLoader.Create([]);
  FLoader.AddLocation(SQLAnyLibrary);
  LoadCodePages;
end;

function TZSQLAnywherePlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Sybase SQL Anywhere';
end;

function TZSQLAnywherePlainDriver.GetProtocol: string;
begin
  Result := 'sqlany';
end;

procedure TZSQLAnywherePlainDriver.LoadApi;
begin
  with FLoader do
  begin
    @sqlany_init                  := GetAddress('sqlany_init');
    @sqlany_init_ex               := GetAddress('sqlany_init_ex');
    @sqlany_fini                  := GetAddress('sqlany_fini', true);
    @sqlany_fini_ex               := GetAddress('sqlany_fini_ex');
    @sqlany_new_connection        := GetAddress('sqlany_new_connection');
    @sqlany_new_connection_ex     := GetAddress('sqlany_new_connection_ex');
    @sqlany_free_connection       := GetAddress('sqlany_free_connection');
    @sqlany_make_connection       := GetAddress('sqlany_make_connection');
    @sqlany_make_connection_ex    := GetAddress('sqlany_make_connection_ex');
    @sqlany_connect               := GetAddress('sqlany_connect');
    @sqlany_disconnect            := GetAddress('sqlany_disconnect');
    @sqlany_cancel                := GetAddress('sqlany_cancel');
    @sqlany_execute_immediate     := GetAddress('sqlany_execute_immediate');
    @sqlany_prepare               := GetAddress('sqlany_prepare');
    @sqlany_free_stmt             := GetAddress('sqlany_free_stmt');
    @sqlany_num_params            := GetAddress('sqlany_num_params');
    @sqlany_describe_bind_param   := GetAddress('sqlany_describe_bind_param');
    @sqlany_bind_param            := GetAddress('sqlany_bind_param');
    @sqlany_send_param_data       := GetAddress('sqlany_send_param_data');
    @sqlany_reset_param_data      := GetAddress('sqlany_reset_param_data');
    @sqlany_error_length          := GetAddress('sqlany_error_length');
    @sqlany_set_batch_size        := GetAddress('sqlany_set_batch_size');
    @sqlany_set_param_bind_type   := GetAddress('sqlany_set_param_bind_type');
    @sqlany_get_batch_size        := GetAddress('sqlany_get_batch_size');
    @sqlany_set_rowset_size       := GetAddress('sqlany_set_rowset_size');
    @sqlany_get_rowset_size       := GetAddress('sqlany_get_rowset_size');
    @sqlany_set_column_bind_type  := GetAddress('sqlany_set_column_bind_type');
    @sqlany_bind_column           := GetAddress('sqlany_bind_column');
    @sqlany_clear_column_bindings := GetAddress('sqlany_clear_column_bindings');
    @sqlany_fetched_rows          := GetAddress('sqlany_fetched_rows');
    @sqlany_set_rowset_pos        := GetAddress('sqlany_set_rowset_pos');
    @sqlany_reset                 := GetAddress('sqlany_reset');
    @sqlany_get_bind_param_info   := GetAddress('sqlany_get_bind_param_info');
    @sqlany_execute               := GetAddress('sqlany_execute');
    @sqlany_fetch_absolute        := GetAddress('sqlany_fetch_absolute');
    @sqlany_fetch_next            := GetAddress('sqlany_fetch_next');
    @sqlany_get_next_result       := GetAddress('sqlany_get_next_result');
    @sqlany_affected_rows         := GetAddress('sqlany_affected_rows');
    @sqlany_num_cols              := GetAddress('sqlany_num_cols', True);
    @sqlany_num_rows              := GetAddress('sqlany_num_rows');
    @sqlany_get_column            := GetAddress('sqlany_get_column');
    @sqlany_get_data              := GetAddress('sqlany_get_data');
    @sqlany_get_data_info         := GetAddress('sqlany_get_data_info');
    @sqlany_get_column_info       := GetAddress('sqlany_get_column_info');
    @sqlany_commit                := GetAddress('sqlany_commit');
    @sqlany_rollback              := GetAddress('sqlany_rollback');
    @sqlany_client_version        := GetAddress('sqlany_client_version');
    @sqlany_client_version_ex     := GetAddress('sqlany_client_version_ex');
    @sqlany_error                 := GetAddress('sqlany_error');
    @sqlany_sqlstate              := GetAddress('sqlany_sqlstate');
    @sqlany_clear_error           := GetAddress('sqlany_clear_error');
    @sqlany_register_callback     := GetAddress('sqlany_register_callback');
  end;
end;

procedure TZSQLAnywherePlainDriver.LoadCodePages;
begin
  { MultiByte }
  AddCodePage('TIS-620', 1, ceAnsi, 874);
  AddCodePage('Windows-31J', 2, ceAnsi, 932);
  AddCodePage('GBK', 3, ceAnsi, 936);
  AddCodePage('IBM949', 4, ceAnsi, 949);
  AddCodePage('BIG5', 5, ceAnsi, 950);
  AddCodePage('EUC_CHINA', 6, ceAnsi, zCP_GB2312);
  AddCodePage('UTF-8', 7, ceUTF8, zCP_UTF8, '', 3);

  { SingleByte }
  AddCodePage('Windows-1250', 8, ceAnsi, 1250);
  AddCodePage('Windows-1251', 9, ceAnsi, 1251);
  AddCodePage('Windows-1252', 10, ceAnsi, 1252);
  AddCodePage('Windows-1253', 11, ceAnsi, 1253);
  AddCodePage('Windows-1254', 12, ceAnsi, 1254);
  AddCodePage('Windows-1255', 13, ceAnsi, 1255);
  AddCodePage('Windows-1256', 14, ceAnsi, 1256);
  AddCodePage('Windows-1257', 15, ceAnsi, 1257);
  AddCodePage('Windows-1258', 16, ceAnsi, 1258);
  {*nix}
  AddCodePage('ISO_8859-6:1987', 17, ceAnsi, 1256);
  AddCodePage('ISO_8859-2:1987', 18, ceAnsi, 1251);
  AddCodePage('GB2312', 19, ceAnsi, zCP_GB2312);
  AddCodePage('Big5-HKSCS', 20, ceAnsi, 950);
  AddCodePage('ISO_8859-9:1989', 21, ceAnsi, 920);
end;

initialization
{$ENDIF ZEOS_DISABLE_ASA}
end.
