{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcMySqlStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types, FmtBCD,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZClasses, ZDbcIntfs, ZDbcStatement, ZDbcMySql, ZVariant, ZPlainMySqlDriver,
  ZCompatibility, ZDbcLogging, ZDbcUtils, ZDbcMySqlUtils, ZCollections, ZExceptions;

type
  TMySQLPreparable = (myDelete, myInsert, myUpdate, mySelect, mySet, myCall);
  TOpenCursorCallback = procedure of Object;
  TMyStmtHandleStatus = (myhsUnknown, myhsAllocated, myhsPrepared, myhsExecutedPrepared, myhsExecutedOnce);

  /// <author>EgonHugeist</author>
  /// <summary>Defines a reference of the TZMySQLBindValue record</summary>
  PZMySQLBindValue = ^TZMySQLBindValue;
  /// <author>EgonHugeist</author>
  /// <summary>Defines a BindValue record which widened the TZBindValue by a
  ///  question mark position indicator</summary>
  TZMySQLBindValue = record
    /// <summary>the TZQMarkPosBindValue record</summary>
    BindValue:  TZQMarkPosBindValue;
    /// <summary>Represents the value as a raw string</summary>
    EmulatedValue: RawByteString;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a MySQL Bindlist object</summary>
  TZMySQLBindList = class(TZQuestionMarkBindList)
  protected
    /// <summary>Get the size of the custom element of this class.</summary>
    /// <returns>the size of the custom element.</returns>
    class function GetElementSize: Integer; override;
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TZMySQLPreparedStatement = class;

  TZAbstractMySQLPreparedStatement = class(TZRawParamDetectPreparedStatement)
  private
    FPMYSQL: PPMYSQL; //the connection handle
    FMySQLConnection: IZMySQLConnection;
    FMYSQL_STMT: PMYSQL_STMT; //a allocated stmt handle
    FPlainDriver: TZMySQLPlainDriver;
    FUseResult, //single row fetches with tabular foward only streaming
    FUseDefaults, //prozess default values -> EH: this should be handled higher up (my POV)
    FMySQL_FieldType_Bit_1_IsBoolean, //self-descriptive isn't it?
    FInitial_emulate_prepare, //the user given mode
    FBindSingleRowBatches,
    FBindAgain, //if types or pointer locations do change(realloc f.e.) we need to bind again -> this is dead slow with mysql
    FChunkedData: Boolean; //just skip the binding loop for sending long data
    FMyHandleStatus: TMyStmtHandleStatus;
    FPreparablePrefixTokens: TPreparablePrefixTokens;
    FBindOffset: PMYSQL_BINDOFFSETS;
    FPrefetchRows: Ulong; //Number of rows to fetch from server at a time when using a cursor.
    FHasMoreResults: Boolean;
    FClientVersion: Integer; //just a local variable
    FMYSQL_BINDs: Pointer; //a buffer for N-params * mysql_bind-record size which are changing from version to version
    FMYSQL_aligned_BINDs: PMYSQL_aligned_BINDs; //offset structure to set all the mysql info's aligned to it's field-structures
    FOpenCursorCallback: TOpenCursorCallback;
    FEmulatedParams: Boolean; //just use emulated String params?
    FLastWasOutParams: Boolean;
    FMinExecCount2Prepare: Integer; //how many executions must be done to fall into a real prepared mode?
    FExecCount: Integer; //How often did we execute the stmt until we reached MinExecCount2Prepare?
    FMYSQL_ColumnsBindingArray: PMYSQL_ColumnsBindingArray;
    FResultSetIndex: Integer; //index of current ColumnsBindingArray
    FResultSetBuffCnt: Integer; //count of allocated Buffers in ColumnsBindingArray
    FIsCallPreparable: Boolean; //are callable statements "real" preparable?
    FChunkSize: Integer;
    FCallResultCache: TZCollection;
    FByteBuffer: PByteBuffer;
    FEmulatedArrayDMLStatement: TZMySQLPreparedStatement;
    /// <summary>Creates a result set based on the current settings.</summary>
    /// <param>"BufferIndex" the index of the buffer hold by this object.
    ///  Reason is we've noticed mysql continues to write into the buffer
    ///  even if the handle get's closed, MariaDB doesn't behave equal</param>
    /// <param>"FieldCount" the count of fields obtained in curren resultset.</param>
    /// <returns>a created result set object interface.</returns>
    function CreateResultSet(const SQL: string; BufferIndex: Integer; FieldCount: UInt): IZResultSet;
    procedure InitBuffer(SQLType: TZSQLType; Index: Integer; Bind: PMYSQL_aligned_BIND; ActualLength: LengthInt = 0);
    procedure FlushPendingResults;
    procedure InternalRealPrepare;
    function CheckPrepareSwitchMode: Boolean;
    ///<summary>Composes an sql query with emultated parameters.</summary>
    ///<returns>the generated sql.</summary>
    function ComposeRawSQLQuery: RawByteString;
    function IsOutParamResult: Boolean;
    procedure ClearCallResultCache;
    procedure FetchCallResults(FieldCount: UInt; UpdateCount: Integer);
    function GetLastResultSet: IZResultSet;
    function GetFirstResultSet: IZResultSet;
    procedure FlushEmulatedArrayDMLStatement;
  protected
    /// <summary>Prepares eventual structures for binding input parameters.</summary>
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
    procedure InternalSetInParamCount(NewParamCount: Integer);
    procedure CheckParameterIndex(var Value: Integer); override;
    procedure SetBindCapacity(Capacity: Integer); override;
    procedure SetParamCount(NewParamCount: Integer); override;
    /// <summary>Removes the current connection reference from this object.</summary>
    /// <remarks>This method will be called only if the object is garbage.</remarks>
    procedure ReleaseConnection; override;
  public
    /// <summary>Constructs this object and assignes the main properties.</summary>
    /// <param>"Connection" a mysql database connection object.</param>
    /// <param>"SQL" a command to execute.</param>
    /// <param>"Info" a list of statement parameters.</param>
    constructor Create(const Connection: IZMySQLConnection;
      const SQL: string; Info: TStrings);
  public
    /// <summary>Releases all driver handles and set the object in a closed
    ///  Zombi mode waiting for destruction. Each known supplementary object,
    ///  supporting this interface, gets called too. This may be a recursive
    ///  call from parant to childs or vice vera. So finally all resources
    ///  to the servers are released. This method is triggered by a connecton
    ///  loss. Don't use it by hand except you know what you are doing.</summary>
    /// <param>"Sender" the object that did notice the connection lost.</param>
    /// <param>"AError" a reference to an EZSQLConnectionLost error.
    ///  You may free and nil the error object so no Error is thrown by the
    ///  generating method. So we start from the premisse you have your own
    ///  error handling in any kind.</param>
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); override;
    /// <summary>Register the parameter properties. This method is required for
    ///  all InOut, Out or Result parameters to access them afterwards. It's not
    ///  requiered to register In params.</summary>
    /// <param>"ParameterIndex" the first parameter is 0, the second is 1, ...
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the parameters SQLType.</param>
    /// <param>"ParamType" the TZProcedureColumnType of the parameter.</param>
    /// <param>"PrecisionOrSize" either the Precision for Numeric types or the
    ///  Length for strings or bytes. The value is ignored for all other types.</param>
    /// <param>"Scale" the numeric or second-fraction scale of the parameter.</param>
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
    /// <summary>prepares the statement on the server if minimum execution
    ///  count have been reached</summary>
    procedure Prepare; override;
    /// <summary>Unprepares the statement, deallocates all bindings and handles</summary>
    procedure Unprepare; override;
    /// <summary>Executes the SQL query in this <c>PreparedStatement</c> object
    ///  and returns the result set generated by the query.</summary>
    /// <returns>a <c>IZResultSet</c> interface that contains the data produced
    ///  by the query; never <c>nil</c></returns>
    function ExecuteQueryPrepared: IZResultSet; override;
    /// <summary>Executes the SQL INSERT, UPDATE or DELETE statement in this
    ///  <c>PreparedStatement</c> object. In addition, SQL statements that
    ///  return nothing, such as SQL DDL statements, can be executed.</summary>
    /// <returns>either the row count for INSERT, UPDATE or DELETE statements;
    ///  or -1 for SQL statements that return nothing</returns>
    function ExecuteUpdatePrepared: Integer; override;
    /// <summary>Executes any kind of SQL statement. Some prepared statements
    ///  return multiple results; the <c>ExecutePrepared</c> method handles these
    ///  complex statements as well as the simpler form of statements handled
    ///  by the methods <c>ExecuteQuery</c> and <c>ExecuteUpdate</c>.
    ///  see IStatement.execute</summary>
    /// <returns>True if a ResultSet is available otherwise false.</returns>
    function ExecutePrepared: Boolean; override;
    /// <summary>Moves to a <c>Statement</c> object's next result.  It returns
    ///  <c>true</c> if this result is a <c>ResultSet</c> object.
    ///  This method also implicitly closes any current <c>ResultSet</c>
    ///  object obtained with the method <c>getResultSet</c>.
    ///
    ///  There are no more results when the following is true:
    ///  <code>(not GetMoreResults and (GetUpdateCount = -1)</code>
    /// </summary>
    /// <returns><c>true</c> if the next result is a <c>ResultSet</c> object;
    ///  <c>false</c> if it is an update count or there are no more results
    /// </returns>
    /// <seealso cref="Execute">Execute</seealso>
    function GetMoreResults: Boolean; override;
    /// <summary>Returns the current result as an update count;
    ///  if the result is a <c>ResultSet</c> object or there are no more results, -1
    ///  is returned. This method should be called only once per result.
    /// </summary>
    /// <returns>the current result as an update count; -1 if the current result is a
    ///  <c>ResultSet</c> object or there are no more results</returns>
    /// <seealso cref="Execute">Execute</seealso>
    function GetUpdateCount: Integer; override;
    /// <summary>Returns the current result as a <c>ResultSet</c> object.
    ///  This method should be called only once per result. The last obtained
    ///  resultset get's flushed. So this method can be called only once
    ///  per result.</summary>
    /// <returns>the current result as a <c>ResultSet</c> object; <c>nil</c> if
    ///  the result is an update count or there are no more results</returns>
    /// <remarks>see execute</remarks>
    function GetResultSet: IZResultSet; override;
  end;

  TZMySQLPreparedStatement = class(TZAbstractMySQLPreparedStatement, IZPreparedStatement)
  private
    procedure BindSInteger(Index: Integer; SQLType: TZSQLType; Value: NativeInt);
    procedure BindUInteger(Index: Integer; SQLType: TZSQLType; Value: NativeUInt);
    procedure InternalBindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double);
  protected
    class function GetBindListClass: TZBindListClass; override;
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString); override;
    procedure BindInParameters; override;
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZSQLStringWriter; Var Result: SQLString); override;
  public
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual
  public
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(Index: Integer; Value: Boolean);
    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(ParameterIndex: Integer; Value: Byte);
    /// <summary>Sets the designated parameter to a <c>ShortInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt);
    /// <summary>Sets the designated parameter to a <c>Word</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetWord(ParameterIndex: Integer; Value: Word);
    /// <summary>Sets the designated parameter to a <c>SmallInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt);
    /// <summary>Sets the designated parameter to a <c>Cardinal</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal);
    /// <summary>Sets the designated parameter to a <c>Integer</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetInt(ParameterIndex: Integer; Value: Integer);
    /// <summary>Sets the designated parameter to a <c>UInt64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64);
    /// <summary>Sets the designated parameter to a <c>Int64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetLong(ParameterIndex: Integer; const Value: Int64);
    /// <summary>Sets the designated parameter to a <c>Single</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetFloat(Index: Integer; Value: Single);
    /// <summary>Sets the designated parameter to a <c>Double</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDouble(Index: Integer; const Value: Double);
    /// <summary>Sets the designated parameter to a <c>Currency</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetCurrency(Index: Integer; const Value: Currency);
    /// <summary>Sets the designated parameter to a <c>BigDecimal(TBCD)</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBigDecimal(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
    /// <summary>Sets the designated parameter to a <c>TZDate</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDate(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate); overload;
    /// <summary>Sets the designated parameter to a <c>TZTime</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTime(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime); overload;
    /// <summary>Sets the designated parameter to a <c>TZTimestamp</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTimestamp(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp); overload;
    /// <summary>Sets the designated parameter to a <c>ByteArray reference</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value reference.</param>
    /// <param>"Len" the Length of the bytes buffer.</param>
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
  end;

  TZMariaDBBatchDMLPreparedStatement = class(TZMySQLPreparedStatement)
  public
    /// <summary>Clears the current parameter values immediately.
    ///  In general, parameter values remain in force for repeated use of a
    ///  statement. Setting a parameter value automatically clears its
    ///  previous value.  However, in some cases it is useful to immediately
    ///  release the resources used by the current parameter values; this can
    ///  be done by calling the method <c>ClearParameters</c>.</summary>
    procedure ClearParameters; override;
    /// <summary>Sets the designated parameter to a data array value. This
    ///  method usually initializes the BatchArray DML mode unless the parameter
    ///  was registered as a PLSQLTable ( in (?) )before.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter array value to be set. Note we just
    ///  reference the array address. We do not increment the Array-Refcount.
    ///  Means you need to keep the arrays alive until the statement has been
    ///  excuted.</param>
    /// <param>"SQLType" the SQLType of the array</param>
    /// <param>"VariantType" the VariantType of the array. It is used as a
    ///  subtype like:
    ///  (SQLType = stString, VariantType = vtUTF8String) or
    ///  (SQLType = stDate, VariantType = vtDate or vtDateTime) </param>
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
    /// <summary>Sets the designated parameter to a null array value. A null
    ///  array can not be bound if not data array has been bound before. So
    ///  SetDataArray() needs to be called first.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQLType of the array. Valid value is stBoolean.</param>
    /// <param>"Value" the parameter null array value to be set. Note we just
    ///  reference the array address. We do not increment the Array-Refcount.
    ///  Means you need to keep the arrays alive until the statement has been
    ///  excuted.</param>
    /// <param>"VariantType" the VariantType of the array. Valid value is vtNull.</param>
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); override;
  end;

  TZMySQLEmulatedBatchPreparedStatement = class(TZMySQLPreparedStatement)
  protected
    function CreateEmulatedArrayDMLStatement: TZMySQLPreparedStatement;
  public
    /// <summary>Clears the current parameter values immediately.
    ///  In general, parameter values remain in force for repeated use of a
    ///  statement. Setting a parameter value automatically clears its
    ///  previous value.  However, in some cases it is useful to immediately
    ///  release the resources used by the current parameter values; this can
    ///  be done by calling the method <c>ClearParameters</c>.</summary>
    procedure ClearParameters; override;
    /// <summary>Sets the designated parameter to a data array value. This
    ///  method usually initializes the BatchArray DML mode unless the parameter
    ///  was registered as a PLSQLTable ( in (?) )before.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter array value to be set. Note we just
    ///  reference the array address. We do not increment the Array-Refcount.
    ///  Means you need to keep the arrays alive until the statement has been
    ///  excuted.</param>
    /// <param>"SQLType" the SQLType of the array</param>
    /// <param>"VariantType" the VariantType of the array. It is used as a
    ///  subtype like:
    ///  (SQLType = stString, VariantType = vtUTF8String) or
    ///  (SQLType = stDate, VariantType = vtDate or vtDateTime) </param>
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
    /// <summary>Sets the designated parameter to a null array value. A null
    ///  array can not be bound if not data array has been bound before. So
    ///  SetDataArray() needs to be called first.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQLType of the array. Valid value is stBoolean.</param>
    /// <param>"Value" the parameter null array value to be set. Note we just
    ///  reference the array address. We do not increment the Array-Refcount.
    ///  Means you need to keep the arrays alive until the statement has been
    ///  excuted.</param>
    /// <param>"VariantType" the VariantType of the array. Valid value is vtNull.</param>
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); override;
  end;

  TZMySQLStatement = class(TZAbstractMySQLPreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZMySQLConnection; Info: TStrings);
  end;

  TZMySQLCallableStatement56up = class(TZAbstractCallableStatement_A,
    IZCallableStatement)
  protected
    /// <summary>creates an exceution Statement. Which wraps the call.</summary>
    /// <param>"StoredProcName" the name of the stored procedure or function to
    ///  be called.</param>
    /// <returns>a TZAbstractPreparedStatement object.</returns>
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Defines a reference of the TZMySQLBindValue record</summary>
  PZMySQLBindValue56down = ^TZMySQLBindValue56down;
  /// <author>EgonHugeist</author>
  /// <summary>Defines a BindValue record which widened the TZMySQLBindValue56
  ///  by a parameter name</summary>
  TZMySQLBindValue56down = record
    /// <summary>the TZMySQLBindValue record</summary>
    BindValue:  TZMySQLBindValue;
    /// <summary>The Parametername</summary>
    ParamName: String;
    PrecisionOrLen: Integer;
    Scale: Integer;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a MySQL Bindlist object</summary>
  TZMySQLBindList56down = class(TZMySQLBindList)
  protected
    /// <summary>Get the size of the custom element of this class.</summary>
    /// <returns>the size of the custom element.</returns>
    class function GetElementSize: Integer; override;
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  TZMySQLCallableStatement56down = class(TZAbstractCallableStatement_A,
    IZCallableStatement)
  private
    FPlainDriver: TZMySQLPLainDriver;
    FStmt, FGetOutParmStmt: TZMySQLPreparedStatement;
    procedure CreateOutParamResultSet;
  protected
    class function GetBindListClass: TZBindListClass; override;
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
    procedure BindInParameters; override;
  public
    procedure AfterConstruction; override;
  public
    procedure Unprepare; override;
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
        Scale: LengthInt = 0); override;
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit

uses
  Math, DateUtils,
  ZFastCode, ZDbcMySqlResultSet, ZDbcProperties, ZDbcMetadata, ZSysUtils,
  ZMessages, ZDbcCachedResultSet, ZEncoding, ZDbcResultSet, ZDbcResultSetMetadata
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF}
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND};

var
  MySQL568PreparableTokens: TPreparablePrefixTokens;

const EnumBool: array[Boolean] of {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = ('N','Y');
const MySQLNullIndicatorMatrix: array[Boolean, Boolean] of TIndicator = (
  (STMT_INDICATOR_NONE, STMT_INDICATOR_NONE), //not null
  (STMT_INDICATOR_NULL, STMT_INDICATOR_DEFAULT));


{ TZAbstractMySQLPreparedStatement }

procedure TZAbstractMySQLPreparedStatement.CheckParameterIndex(var Value: Integer);
begin
  if ((FMYSQL_STMT <> nil) and (BindList.Count < Value+1)) or
     ((FMYSQL_STMT =  nil) and (BindList.Capacity < Value+1))
  then raise CreateBindVarOutOfRangeException(Value)
  else inherited CheckParameterIndex(Value);
end;

function TZAbstractMySQLPreparedStatement.CheckPrepareSwitchMode: Boolean;
begin
  Result := ((not FInitial_emulate_prepare) or (BatchDMLArrayCount > 0 )) and
     (FMYSQL_STMT = nil) and (TokenMatchIndex <> -1) and
     ((BatchDMLArrayCount > 0 ) or (FExecCount = FMinExecCount2Prepare) or
     ((TokenMatchIndex = Ord(myCall)) and ((FPLainDriver.IsMariaDBDriver and ((FClientVersion  >= 100000) or (FClientVersion  < 50000))) or
     (not FPLainDriver.IsMariaDBDriver and (FClientVersion >= 50608)))));
  if Result then begin
    FEmulatedParams := False;
    if (BindList.Count > 0) then
      InternalSetInParamCount(BindList.Count);
  end;
end;

procedure TZAbstractMySQLPreparedStatement.ClearCallResultCache;
var I: Integer;
  RS: IZResultSet;
begin
  for I := 0 to FCallResultCache.Count -1 do
    if Supports(FCallResultCache[i], IZResultSet, RS) then
      RS.Close;
  FreeAndNil(FCallResultCache);
end;

function TZAbstractMySQLPreparedStatement.ComposeRawSQLQuery: RawByteString;
var
  I, LastPos, L: Cardinal;
  SQLWriter: TZRawSQLStringWriter;
  P: PAnsiChar;
  BindValue: PZBindValue;
  QMarkBindValue: PZQMarkPosBindValue absolute BindValue;
  MySQLBindValue: PZMySQLBindValue absolute BindValue;
begin
  if BindList.Count = 0
  then Result := FASQL
  else begin
    Result := EmptyRaw;
    P := Pointer(FASQL);
    L := Length(FASQL);
    LastPos := 0;
    I := L + Cardinal(BindList.Count) shl 5; //add 32 bytes/param by default
    SQLWriter := TZRawSQLStringWriter.Create(I);
    try
      for I := 0 to BindList.Count -1 do begin
        BindValue := BindList[I];
        if QMarkBindValue.QMarkPosition > 0 then begin
          SQLWriter.AddText(P+LastPos, QMarkBindValue.QMarkPosition - LastPos, Result);
          LastPos := QMarkBindValue.QMarkPosition + 1;
        end;
        if BindValue.ParamType <> pctReturn then
          if BindValue.BindType = zbtNull
          then SQLWriter.AddText('null', Result)
          else SQLWriter.AddText(MySQLBindValue.EmulatedValue, Result);
      end;
      SQLWriter.AddText(P+LastPos, L - LastPos, Result);
      SQLWriter.Finalize(Result);
    finally
      FreeAndNil(SQLWriter);
    end;
  end;
end;

constructor TZAbstractMySQLPreparedStatement.Create(
  const Connection: IZMySQLConnection;
  const SQL: string; Info: TStrings);
begin
  FPlainDriver := TZMySQLPlainDriver(Connection.GetIZPlainDriver.GetInstance);
  FClientVersion := FPLainDriver.mysql_get_client_version;
  FBindOffset := GetBindOffsets(FPlainDriver.IsMariaDBDriver, FClientVersion);

  FIsCallPreparable := (FPLainDriver.IsMariaDBDriver and ((FClientVersion >= 100000) or (FClientVersion < 50000))) or
     (not FPLainDriver.IsMariaDBDriver and (FClientVersion >= 50608));
  FPreparablePrefixTokens := MySQL568PreparableTokens;
  FMySQLConnection := Connection;
  FPMYSQL := Connection.GetConnectionHandleAddress;
  FByteBuffer := Connection.GetByteBufferAddress;
  inherited Create(Connection, SQL, Info);

  FUseResult := StrToBoolEx(DefineStatementParameter(Self, DSProps_UseResult, 'false'));
  if not FUseResult then
    ResultSetType := rtScrollInsensitive;
  FUseDefaults := StrToBoolEx(DefineStatementParameter(Self, DSProps_MySQLUseDefaults, 'true'));
  FPrefetchRows := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, DSProps_PrefetchRows, '1'),1);
  //JDBC prepares after 4th execution
  FMinExecCount2Prepare := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, DSProps_MinExecCntBeforePrepare, '2'), 2);

  FInitial_emulate_prepare := (FBindOffset.buffer_type=0) or (FMinExecCount2Prepare < 0) or
    StrToBoolEx(DefineStatementParameter(Self, DSProps_EmulatePrepares, 'false')) {
    and not StrToBoolEx(DefineStatementParameter(Self, DSProps_PreferPrepared, 'False'))};
  FEmulatedParams := True;
  FMySQL_FieldType_Bit_1_IsBoolean := FMySQLConnection.MySQL_FieldType_Bit_1_IsBoolean;
  FGUIDAsString := True;
  FResultSetIndex := -1;

  FChunkSize := StrToInt(DefineStatementParameter(Self, DSProps_ChunkSize, '4096'));
end;

procedure TZAbstractMySQLPreparedStatement.Prepare;
begin
  FlushPendingResults;
  if not Prepared then
    inherited Prepare;
  if CheckPrepareSwitchMode then
    InternalRealPrepare;
end;

procedure TZAbstractMySQLPreparedStatement.Unprepare;
var //status: Integer;
  ParamCount: Integer;
begin
  if FCallResultCache <> nil then
    ClearCallResultCache;
  if FEmulatedArrayDMLStatement <> nil then
    FlushEmulatedArrayDMLStatement;

  ParamCount := BindList.Count;
  inherited Unprepare;
  FExecCount := 0;
  //FlushPendingResults; //EH: if we receive a "Malformed communication packet" error
  //i.e. old lib for new servers, we leak mem everywhere
  //thus i commented it...
  try
    if not FEmulatedParams and (FMYSQL_STMT <> nil) then begin
      //cancel all pending results:
      //https://mariadb.com/kb/en/library/mysql_stmt_close/
      {status := EH: Commented out, this leads to some memleaks if libmariadb is vinny nilly with its own handles}
        FPlainDriver.mysql_stmt_close(FMYSQL_STMT);
      {try
        if status <> 0 then
          FMySQLConnection.HandleErrorOrWarning(lcUnprepStmt, FMYSQL_STMT,
            'mysql_stmt_close', IImmediatelyReleasable(FWeakImmediatRelPtr));
      finally}
        FMYSQL_STMT := nil;
        FMyHandleStatus := myhsUnknown;
        if ParamCount > 0 then
          ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
            ParamCount*Ord(FMYSQL_aligned_BINDs<>nil), 0, 1);
      //end;
    end else if (ParamCount > 0) and (FMYSQL_BINDs <> nil) then //switch mode did alloc mem
      ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
        ParamCount*Ord(FMYSQL_aligned_BINDs<>nil), 0, 1);
  finally
    if FResultSetBuffCnt > 0 then begin
      ReAllocMySQLColumnBuffer(FResultSetBuffCnt,0, FMYSQL_ColumnsBindingArray, FBindOffset);
      FHasMoreResults := False;
      FResultSetBuffCnt := 0;
      FResultSetIndex := -1;
    end;
    FEmulatedParams := FInitial_emulate_prepare;
    FLastWasOutParams := False;
  end;
end;

function TZAbstractMySQLPreparedStatement.GetMoreResults: Boolean;
var status: Integer;
  FieldCount: UInt;
  RS: IZResultSet;
  AnyValue: IZAnyValue;
begin
  if FEmulatedArrayDMLStatement <> nil
  then Result := FEmulatedArrayDMLStatement.GetMoreResults
  else begin
    if (FOpenResultSet <> nil)
    then IZResultSet(FOpenResultSet).Close;
    if FCallResultCache <> nil then begin
      Result := FCallResultCache.Count > 0;
      if Result then begin
        if Supports(FCallResultCache[0], IZResultSet, RS) then begin
          LastResultSet := RS;
          LastUpdateCount := -1;
        end else begin
          FCallResultCache[0].QueryInterface(IZAnyValue, AnyValue);
          LastUpdateCount := AnyValue.GetInteger;
        end;
        FCallResultCache.Delete(0);
      end;
    end else begin
      Result := False;
      LastResultSet := nil;
      LastUpdateCount := -1;
      if FEmulatedParams then begin
        if Assigned(FPlainDriver.mysql_next_result) and Assigned(FPMYSQL^) then begin
          //if FPlainDriver.mysql_more_results(FPMYSQL^) = 0 then Exit;
          Status := FPlainDriver.mysql_next_result(FPMYSQL^);
          if Status > 0 //if status is -1 then there are no more resuls
          then FMySQLConnection.HandleErrorOrWarning(lcExecute, nil, SQL,
            IImmediatelyReleasable(FWeakImmediatRelPtr))
          else if Status = 0 then begin //results are in queue
            Result := True;
            FHasMoreResults := True;
            FieldCount := FPlainDriver.mysql_field_count(FPMYSQL^);
            if (FieldCount > 0)
            then LastResultSet := CreateResultSet(SQL, FResultSetIndex+1, FieldCount)
            else LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^);
          end;
        end;
      end else if Assigned(FPlainDriver.mysql_stmt_next_result) and Assigned(FMYSQL_STMT) then begin
        //see: https://bugs.mysql.com/bug.php?id=43608
        {if Assigned(FPlainDriver.mysql_stmt_more_results) then begin
          if FPlainDriver.mysql_stmt_more_results(FMYSQL_STMT) = 0 then Exit;
        end else if FPlainDriver.mysql_more_results(FPMYSQL^) = 0 then Exit;}
        Status := FPlainDriver.mysql_stmt_next_result(FMYSQL_STMT);
        if Status > 0 //if status is -1 then there are no more resuls
        then FMySQLConnection.HandleErrorOrWarning(lcExecute, FMYSQL_STMT, SQL,
          IImmediatelyReleasable(FWeakImmediatRelPtr))
        else if Status = 0 then begin //results are in queue
          Result := True;
          FHasMoreResults := True;
          FieldCount := FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT);
          if (FieldCount > 0)
          then LastResultSet := CreateResultSet(SQL, FResultSetIndex+1, FieldCount)
          else LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT);
        end;
      end;
      //handle multiple statements which can not be prepared
      if FHasMoreResults and (TokenMatchIndex < Ord(myCall)) then
        FTokenMatchIndex := -1; //indicate we'll never fall into a prepared mode
    end;
  end;
end;

function TZAbstractMySQLPreparedStatement.GetResultSet: IZResultSet;
begin
  if FEmulatedArrayDMLStatement <> nil
  then Result := FEmulatedArrayDMLStatement.GetResultSet
  else Result := inherited GetResultSet;
end;

function TZAbstractMySQLPreparedStatement.CreateResultSet(const SQL: string;
  BufferIndex: Integer; FieldCount: UInt): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZAbstractMySQLResultSet;
  CachedResultSet: TZCachedResultSet;
  MYSQL_ColumnsBinding: PMYSQL_ColumnsBinding;
begin
  FLastWasOutParams := IsOutParamResult;
  if FLastWasOutParams and (FOpenResultSet <> nil) then
    IZResultSet(FOpenResultSet).Close;
  if BufferIndex >= FResultSetBuffCnt then begin
    ReAllocMySQLColumnBuffer(FResultSetBuffCnt, BufferIndex+1, FMYSQL_ColumnsBindingArray, FBindOffset);
    FResultSetBuffCnt := BufferIndex +1;
  end;
  {$R-}
  MYSQL_ColumnsBinding := @FMYSQL_ColumnsBindingArray[BufferIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF};
  FResultSetIndex := BufferIndex;
  if MYSQL_ColumnsBinding.FieldCount <> FieldCount then begin
    ReallocBindBuffer(MYSQL_ColumnsBinding.MYSQL_Col_BINDs,
      MYSQL_ColumnsBinding.MYSQL_aligned_BINDs, FBindOffset, MYSQL_ColumnsBinding.FieldCount, FieldCount, 1);
    MYSQL_ColumnsBinding.FieldCount := FieldCount;
  end;

  if (not FHasMoreResults) and (FOpenResultSet <> nil) then begin
    Result := IZResultSet(FOpenResultSet);
    FOpenCursorCallback;
    if (fUseResult or (FCursorLocation = rctServer)) and ((GetResultSetConcurrency = rcUpdatable) or
       (GetResultSetType = rtScrollInsensitive)) then begin
      Result.Last; //invoke fetch all -> note this is done on msql_strore_result too
      Result.BeforeFirst;
    end;
  end else begin
    if FUseResult or (FCursorLocation = rctServer)//server cursor?
    then NativeResultSet := TZMySQL_Use_ResultSet.Create(Self, SQL, FMySQLConnection,
      False, @FMYSQL_STMT, MYSQL_ColumnsBinding , nil, FOpenCursorCallback)
    else NativeResultSet := TZMySQL_Store_ResultSet.Create(Self, SQL, FMySQLConnection,
      FLastWasOutParams, @FMYSQL_STMT, MYSQL_ColumnsBinding, nil, FOpenCursorCallback);
    if TokenMatchIndex = Ord(myCall) then begin
      Result := NativeResultSet; //inc the refcount
      Result := Connection.GetMetadata.CloneCachedResultSet(Result); //replace the result
    end else if (GetResultSetConcurrency = rcUpdatable) or
       ((GetResultSetType <> rtForwardOnly) and FUseResult) then begin
      if (GetResultSetConcurrency = rcUpdatable) then
        if FEmulatedParams
        then CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver,
          FPMYSQL, nil, Self, NativeResultSet.GetMetaData)
        else CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver,
          FPMYSQL, FMYSQL_STMT, Self, NativeResultSet.GetMetaData)
      else CachedResolver := nil;
      if FUseResult then
        if FEmulatedParams
        then CachedResultSet := TZMySQLUseResultsCachedResultSet.CreateWithColumns(
          NativeResultSet.ColumnsInfo, NativeResultSet, SQL, CachedResolver, ConSettings)
        else CachedResultSet := TZMySQLPreparedUseResultsCachedResultSet.CreateWithColumns(
          NativeResultSet.ColumnsInfo, NativeResultSet, SQL, CachedResolver, ConSettings)
      else if (LobCacheMode = lcmOnLoad) and not FEmulatedParams
        then CachedResultSet := TZMySQLPreparedStoreResultsCachedLobsResultSet.CreateWithColumns(
          NativeResultSet.ColumnsInfo, NativeResultSet, SQL, CachedResolver, ConSettings)
        else CachedResultSet := TZMySQLUseResultsCachedResultSet.CreateWithColumns(
          NativeResultSet.ColumnsInfo, NativeResultSet, SQL, CachedResolver, ConSettings);
      if fUseResult then begin
        CachedResultSet.Last; //invoke fetch all -> note this is done on msql_strore_result in the lib too
        CachedResultSet.BeforeFirst;
      end;
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
      Result.GetMetadata.IsWritable(FirstDbcIndex); //force metadata loading
    end else
      Result := NativeResultSet;
    FOpenResultSet := Pointer(Result);
  end;
  //note to myselve: OutParams are always the last resultset see:
  //https://dev.mysql.com/doc/refman/5.7/en/c-api-prepared-call-statements.html
(*  if FLastWasOutParams or (BindList.HasOutOrInOutOrResultParam and (BufferIndex = 0)) then
    FOutParamResultSet := Result; *)
end;

procedure TZAbstractMySQLPreparedStatement.PrepareInParameters;
begin
  if not FEmulatedParams and (FMYSQL_STMT<> nil)
  then SetParamCount(FPlainDriver.mysql_stmt_param_count(FMYSQL_STMT))
  else InternalSetInParamCount(BindList.Capacity);
end;

procedure TZAbstractMySQLPreparedStatement.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZProcedureColumnType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
var OldCount: Integer;
begin
  OldCount := BindList.Count;
  if SQLType in [stUnicodeString, stUnicodeStream] then
    SQLType := TZSQLType(Ord(SQLType)-1);
  inherited RegisterParameter(ParameterIndex, SQLType, ParamType, Name, PrecisionOrSize, Scale);
  CheckParameterIndex(ParameterIndex);
  if not FEmulatedParams then begin
    if OldCount <> BindList.Count then begin
      ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
        OldCount, BindList.Count, 1);
      {$R-}
      FMYSQL_aligned_BINDs[ParameterIndex].is_null_address^ := 1;
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    end;
    if ParamType = pctOut then
      if SQLType in [stString, stUnicodestring] then
        PrecisionOrSize := PrecisionOrSize * ConSettings.ClientCodePage.CharWidth;
      {$R-}
      InitBuffer(SQLType, ParameterIndex, @FMYSQL_aligned_BINDs[ParameterIndex], PrecisionOrSize);
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  end;
end;

procedure TZAbstractMySQLPreparedStatement.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FMySQLConnection := nil;
end;

procedure TZAbstractMySQLPreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FPMYSQL^ := nil;
  FMYSQL_STMT := nil;
  FBindAgain := True;
  FMyHandleStatus := myhsUnknown;
  inherited ReleaseImmediat(Sender, AError);
end;

procedure TZAbstractMySQLPreparedStatement.SetBindCapacity(Capacity: Integer);
var OldCapacity: Integer;
begin
  OldCapacity := Bindlist.Capacity;
  inherited SetBindCapacity(Capacity);
  if OldCapacity <> BindList.Capacity then
    if (FMYSQL_STMT <> nil) then
      ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
        OldCapacity, BindList.Capacity, 1);
end;

procedure TZAbstractMySQLPreparedStatement.SetParamCount(
  NewParamCount: Integer);
begin
  if (BindList.Capacity > 0) and (NewParamCount = BindList.Capacity +1)
  then TZMySQLBindList(BindList).Insert(0)
  else inherited SetParamCount(NewParamCount);
end;

procedure TZAbstractMySQLPreparedStatement.UnPrepareInParameters;
begin
  SetBindCapacity(0);
  FBindAgain := True;
  FChunkedData := False;
end;

function TZAbstractMySQLPreparedStatement.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := @FPreparablePrefixTokens;
end;

function TZAbstractMySQLPreparedStatement.GetFirstResultSet: IZResultSet;
var I: Integer;
begin
  Result := nil;
  if FCallResultCache <> nil then
    for I := 0 to FCallResultCache.Count -1 do
      if Supports(FCallResultCache[i], IZResultSet, Result) then begin
        FCallResultCache.Delete(I);
        Break;
      end;
end;

function TZAbstractMySQLPreparedStatement.GetLastResultSet: IZResultSet;
var I: Integer;
begin
  Result := nil;
  if FCallResultCache <> nil then
    for I := FCallResultCache.Count -1 downto 0 do
      if Supports(FCallResultCache[i], IZResultSet, Result) then begin
        FCallResultCache.Delete(I);
        Break;
      end;
end;

function TZAbstractMySQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  FieldCount: UInt;
  function ExecuteEmulated: IZResultSet; //use a own method to suppress any LStrClear calls for non emulation
  var
    RSQL: RawByteString;
  begin
    RSQL := ComposeRawSQLQuery;
    if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(RSQL), Length(RSQL)) = 0 then begin
      FieldCount := FPlainDriver.mysql_field_count(FPMYSQL^);
      if FieldCount = 0
      then LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^)
      else LastUpdateCount := -1;
      { Logging Execution }
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecute,Self);
      if (TokenMatchIndex = Ord(myCall)) or BindList.HasOutParam or BindList.HasInOutParam then begin
        FetchCallResults(FieldCount,LastUpdateCount);
        Result := GetFirstResultSet;
        if BindList.HasOutParam or BindList.HasInOutParam then
          FOutParamResultSet := GetLastResultSet;
      end else if FieldCount <> 0 then
        Result := CreateResultSet(SQL, 0, FieldCount)
      else while GetMoreResults do
        if LastResultSet <> nil then begin
          Result := LastResultSet;
          FLastResultSet := nil;
          FOpenResultSet := Pointer(Result);
          Break;
        end;
      FOpenResultSet := Pointer(Result);
    end else
      FMySQLConnection.HandleErrorOrWarning(lcExecute, nil, SQL,
        IImmediatelyReleasable(FWeakImmediatRelPtr));
    Inc(FExecCount, Ord((FMinExecCount2Prepare >= 0) and (FExecCount < FMinExecCount2Prepare)));
    CheckPrepareSwitchMode;
  end;
begin
  if FEmulatedArrayDMLStatement <> nil
  then Result := FEmulatedArrayDMLStatement.ExecuteQueryPrepared
  else begin
    PrepareOpenResultSetForReUse;
    Prepare;
    BindInParameters;
    RestartTimer;
    if FEmulatedParams or (FMYSQL_STMT = nil)
    then Result := ExecuteEmulated
    else if (FPlainDriver.mysql_stmt_execute(FMYSQL_STMT) = 0) then begin
      if (FMyHandleStatus = myhsPrepared)
      then FMyHandleStatus := myhsExecutedPrepared
      else FMyHandleStatus := myhsExecutedOnce;
      FieldCount := FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT);
      if FieldCount = 0
      then LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT)
      else LastUpdateCount := -1;
      { Logging Execution }
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecPrepStmt,Self);
      if (TokenMatchIndex = Ord(myCall)) or BindList.HasOutParam or BindList.HasInOutParam then begin
        FetchCallResults(FieldCount,LastUpdateCount);
        Result := GetFirstResultSet;
        if BindList.HasOutParam or BindList.HasInOutParam then
          FOutParamResultSet := GetLastResultSet;
      end else if FieldCount = 0 then begin
        while GetMoreResults do
          if FLastResultSet <> nil then begin
            Result := FLastResultSet;
            FLastResultSet := nil;
            FOpenResultset := Pointer(Result);
          end
      end else
        Result := CreateResultSet(SQL, 0, FieldCount);
      FOpenResultSet := Pointer(Result);
    end else begin
      Result := nil;
      if not FMySQLConnection.IsSilentError then
        FMySQLConnection.HandleErrorOrWarning(lcExecPrepStmt, FMYSQL_STMT,
          SQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
    if (Result = nil) and not FMySQLConnection.IsSilentError then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    if (FTokenMatchIndex = Ord(myCall)) or BindList.HasReturnParam then
      FOutParamResultSet := Result;
  end;
end;

function TZAbstractMySQLPreparedStatement.ExecuteUpdatePrepared: Integer;
  procedure ExecEmulated;
  var FieldCount: ULong;
      RSQL: RawByteString;
  begin
    RSQL := ComposeRawSQLQuery;
    if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(RSQL), Length(RSQL)) = 0 then begin
      FieldCount := FplainDriver.mysql_field_count(FPMYSQL^);
      if FieldCount = 0
      then LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^)
      else LastUpdateCount := -1;
      { Logging Execution }
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecute,Self);
      if (TokenMatchIndex = Ord(myCall)) or BindList.HasOutParam or BindList.HasInOutParam then
        FetchCallResults(FieldCount,Result)
      else if FieldCount > 0 then
        if BindList.HasReturnParam //retrieve outparam
        then FOutParamResultSet := CreateResultSet(SQL, 0, FieldCount)
        else LastResultSet := CreateResultSet(SQL, 0, FieldCount);
    end else
      FMySQLConnection.HandleErrorOrWarning(lcExecute, nil, SQL,
        IImmediatelyReleasable(FWeakImmediatRelPtr));
    Inc(FExecCount, Ord((FMinExecCount2Prepare >= 0) and (FExecCount < FMinExecCount2Prepare)));
    CheckPrepareSwitchMode;
  end;
var FieldCount: ULong;
begin
  if FEmulatedArrayDMLStatement <> nil
  then Result := FEmulatedArrayDMLStatement.ExecuteUpdatePrepared
  else begin
    Prepare;
    BindInParameters;
    RestartTimer;
    if FEmulatedParams or (FMYSQL_STMT = nil)
    then ExecEmulated
    else if (FPlainDriver.mysql_stmt_execute(FMYSQL_STMT) = 0) then begin
      if FMyHandleStatus = myhsPrepared
      then FMyHandleStatus := myhsExecutedPrepared
      else FMyHandleStatus := myhsExecutedOnce;

      FieldCount := FplainDriver.mysql_stmt_field_count(FMYSQL_STMT);
      if FieldCount = 0
      then LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT)
      else LastUpdateCount := -1;
      { Logging Execution }
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecPrepStmt,Self);
      if (TokenMatchIndex = Ord(myCall)) or BindList.HasOutParam or BindList.HasInOutParam then begin
        FetchCallResults(FieldCount,LastUpdateCount);
        if BindList.HasOutParam or BindList.HasInOutParam then
          FOutParamResultSet := GetLastResultSet;
      end else if FieldCount > 0 then
        if BindList.HasReturnParam //retrieve outparam
        then FOutParamResultSet := CreateResultSet(SQL, 0, FieldCount)
        else LastResultSet := CreateResultSet(SQL, 0, FieldCount);
    end else
      FMySQLConnection.HandleErrorOrWarning(lcExecPrepStmt, FMYSQL_STMT,
        SQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
    Result := LastUpdateCount;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.FetchCallResults(
  FieldCount: UInt; UpdateCount: Integer);
var CallResultCache: TZCollection;
begin
  CallResultCache := TZCollection.Create;
  if FieldCount > 0 then begin
    CallResultCache.Add(CreateResultSet(SQL, 0, FieldCount));
    FOpenResultSet := nil; //invoke closing the result
  end else CallResultCache.Add(TZAnyValue.CreateWithInteger(UpdateCount));
  while GetMoreresults do
    if LastResultSet <> nil then begin
      CallResultCache.Add(LastResultSet);
      FLastResultSet := nil;
      FOpenResultSet := nil;
    end else
      CallResultCache.Add(TZAnyValue.CreateWithInteger(LastUpdateCount));
  FCallResultCache := CallResultCache;
end;

procedure TZAbstractMySQLPreparedStatement.FlushEmulatedArrayDMLStatement;
begin
  try
    FEmulatedArrayDMLStatement.Close;
  finally
    FEmulatedArrayDMLStatement._Release;
    FEmulatedArrayDMLStatement := nil;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.FlushPendingResults;
var
  FQueryHandle: PZMySQLResult;
  Status: Integer;
label jmpCheckErr;
begin
  if Assigned(FCallResultCache) then
    ClearCallResultCache;
  if FLastWasOutParams and Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close;
  if Assigned(FOutParamResultSet) then
    FOutParamResultSet.ResetCursor;
  if (FEmulatedParams) then
    //old lib's do not have mysql_next_result method
    while Assigned(FPlainDriver.mysql_next_result) do begin
      Status := FPlainDriver.mysql_next_result(FPMYSQL^);
      if Status = -1 then
        Break
      else if (Status = 0) then begin
        FHasMoreResults := True;
        if FPlainDriver.mysql_field_count(FPMYSQL^) > 0 then begin
          FQueryHandle := FPlainDriver.mysql_store_result(FPMYSQL^);
          FPlainDriver.mysql_free_result(FQueryHandle);
        end;
      end else if (Status > 0) then begin
        FMySQLConnection.HandleErrorOrWarning(lcExecute, nil, SQL,
          IImmediatelyReleasable(FWeakImmediatRelPtr));
        Break;
      end;
    end
  else if (FMYSQL_STMT <> nil) and (Ord(FMyHandleStatus) >= Ord(myhsExecutedPrepared)) then
    while Assigned(FPlainDriver.mysql_stmt_next_result) do begin
      Status := FPlainDriver.mysql_stmt_next_result(FMYSQL_STMT);
      if Status = -1 then
        Break
      else if (Status = 0) then begin
        FHasMoreResults := True;
        if (FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) > 0) then
          if FPlainDriver.mysql_stmt_free_result(FMYSQL_STMT) <> 0 then
            goto jmpCheckErr
      end else if (Status > 0) then begin
jmpCheckErr:
        FMySQLConnection.HandleErrorOrWarning(lcExecPrepStmt, FMYSQL_STMT,
          SQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
        Break;
      end;
    end;
end;

function TZAbstractMySQLPreparedStatement.ExecutePrepared: Boolean;
var
  FieldCount: UInt;
  procedure ExecuteEmulated;
  var RSQL: RawByteString;
  begin
    RSQL := ComposeRawSQLQuery;
    if FPlainDriver.mysql_real_query(FPMYSQL^, Pointer(RSQL), Length(RSQL)) = 0 then begin
      FieldCount := FPlainDriver.mysql_field_count(FPMYSQL^);
      if FieldCount = 0
      then LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^)
      else LastUpdateCount := -1;
      if (TokenMatchIndex = Ord(myCall)) or BindList.HasOutParam or BindList.HasInOutParam then begin
        FetchCallResults(FieldCount,LastUpdateCount);
        if FieldCount > 0
        then LastResultSet := GetFirstResultSet
        else LastResultSet := nil;
      end else if FieldCount > 0
        then LastResultSet := CreateResultSet(SQL, 0, FieldCount)
        else LastResultSet := nil;
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecute,Self);
    end else FMySQLConnection.HandleErrorOrWarning(lcExecute, nil, SQL,
      IImmediatelyReleasable(FWeakImmediatRelPtr));
    Inc(FExecCount, Ord((FMinExecCount2Prepare >= 0) and (FExecCount < FMinExecCount2Prepare)));
    CheckPrepareSwitchMode;
  end;
begin
  if FEmulatedArrayDMLStatement <> nil
  then Result := FEmulatedArrayDMLStatement.ExecutePrepared
  else begin
    PrepareLastResultSetForReUse;
    Prepare;
    BindInParameters;
    RestartTimer;
    if FEmulatedParams or (FMYSQL_STMT = nil)
    then ExecuteEmulated
    else if FPlainDriver.mysql_stmt_execute(FMYSQL_STMT) = 0 then begin
      if FMyHandleStatus = myhsPrepared
      then FMyHandleStatus := myhsExecutedPrepared
      else FMyHandleStatus := myhsExecutedOnce;
      FieldCount := FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT);
      if FieldCount = 0 // we can call this function if fielcount is zero only
      then LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT)
      else LastUpdateCount := -1;
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcExecPrepStmt,Self);
      if (TokenMatchIndex = Ord(myCall)) or BindList.HasOutParam or BindList.HasInOutParam then begin
        FetchCallResults(FieldCount,LastUpdateCount);
        if BindList.HasOutParam or BindList.HasInOutParam then
          FOutParamResultSet := GetLastResultSet;
        if FieldCount > 0
        then LastResultSet := GetFirstResultSet
        else LastResultSet := nil;
      end else begin if FieldCount > 0
        then LastResultSet := CreateResultSet(SQL, 0, FieldCount)
        else LastResultSet := nil;
      end;
    end else FMySQLConnection.HandleErrorOrWarning(lcExecPrepStmt, FMYSQL_STMT,
      SQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
    if BindList.HasReturnParam then begin
      FOutParamResultset := Connection.GetMetadata.CloneCachedResultSet(FLastResultSet);
      if FLastResultSet.GetType = rtForwardOnly then
        FLastResultSet := FOutParamResultset;
    end;
    Result := Assigned(LastResultSet);
  end;
end;

function TZAbstractMySQLPreparedStatement.GetUpdateCount: Integer;
begin
  if FEmulatedArrayDMLStatement <> nil
  then Result := FEmulatedArrayDMLStatement.GetUpdateCount
  else begin
    Result := LastUpdateCount;
    if FEmulatedParams then begin
      if (Result = -1) and Assigned(FPMYSQL^) and (FPlainDriver.mysql_field_count(FPMYSQL^) = 0) then begin
        LastUpdateCount := FPlainDriver.mysql_affected_rows(FPMYSQL^);
        Result := LastUpdateCount;
      end
    end else begin
      if (Result = -1) and Assigned(FMYSQL_STMT) and (FPlainDriver.mysql_stmt_field_count(FMYSQL_STMT) = 0) then begin
        LastUpdateCount := FPlainDriver.mysql_stmt_affected_rows(FMYSQL_STMT);
        Result := LastUpdateCount;
      end;
    end;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.InitBuffer(SQLType: TZSQLType;
  Index: Integer; Bind: PMYSQL_aligned_BIND; ActualLength: LengthInt = 0);
var BuffSize: Integer;
begin
  case SQLType of
    stBoolean:      begin
                      BuffSize := 1;
                      if fMySQL_FieldType_Bit_1_IsBoolean then begin
                        Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
                        Bind^.is_unsigned_address^ := 1;
                      end else Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stByte,
    stShort:        begin
                      BuffSize := SizeOf(Byte);
                      Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stByte)
                    end;
    stWord,
    stSmall:        begin
                      BuffSize := SizeOf(Word);
                      Bind^.buffer_type_address^ := FIELD_TYPE_SHORT;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stWord)
                    end;
    stLongWord,
    stInteger:      begin
                      BuffSize := SizeOf(Cardinal);
                      Bind^.buffer_type_address^ := FIELD_TYPE_LONG;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stLongWord)
                    end;
    stULong,
    stLong:         begin
                      BuffSize := SizeOf(Int64);
                      Bind^.buffer_type_address^ := FIELD_TYPE_LONGLONG;
                      Bind^.is_unsigned_address^ := Ord(SQLType = stULong)
                    end;

    stCurrency:     begin
                      BuffSize := 21;  //f.e: -922337203685477.5808
                      Bind^.buffer_type_address^ := FIELD_TYPE_NEWDECIMAL; //EH: mysql binds the high precision types as strings.. using Tdecimal_t is worth in vain                    end;
                    end;
    stBigDecimal: begin
                      BuffSize := MaxFMTBcdFractionSize+2{dot, sign};
                      Bind^.buffer_type_address^ := FIELD_TYPE_NEWDECIMAL; //EH: mysql binds the high precision types as strings.. using Tdecimal_t is worth in vain
                    end;
    stFloat,
    stDouble:       begin
                      BuffSize := SizeOf(Double);
                      Bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
                    end;
    stDate:         begin
                      BuffSize := SizeOf(TMYSQL_TIME);
                      Bind^.buffer_type_address^ := FIELD_TYPE_DATE;
                    end;
    stTime:         if true or FPlainDriver.IsMariaDBDriver then begin
                    //https://dev.mysql.com/doc/refman/5.7/en/c-api-prepared-statement-problems.html
                      BuffSize := SizeOf(TMYSQL_TIME);
                      Bind^.buffer_type_address^ := FIELD_TYPE_TIME;
                    end else begin //milli/micro-second fractions are not supported
                      BuffSize := ConSettings^.WriteFormatSettings.TimeFormatLen;
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stTimeStamp:    if true or FPlainDriver.IsMariaDBDriver then begin
                    //https://dev.mysql.com/doc/refman/5.7/en/c-api-prepared-statement-problems.html
                      BuffSize := SizeOf(TMYSQL_TIME);
                      Bind^.buffer_type_address^ := FIELD_TYPE_DATETIME;
                    end else begin //milli/micro-second fractions are not supported
                      BuffSize := ConSettings^.WriteFormatSettings.DateTimeFormatLen;
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stGUID:         begin  //EH: binary(16) or char(38/36/34) ?
                      BuffSize := 38;
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stString,
    stUnicodeString,
    stBytes:        begin
                      if ActualLength = 0 then
                        ActualLength := 8;
                      //ludob: mysql adds terminating #0 on top of data. Avoid buffer overrun.
                      BuffSize := Max(8, (((ActualLength-1) shr 3)+1) shl 3); //8 byte aligned including space for trailing #0
                      if SQLType <> stBytes
                      then Bind^.buffer_type_address^ := FIELD_TYPE_STRING
                      else Bind^.buffer_type_address^ := FIELD_TYPE_TINY_BLOB;
                    end;
    stAsciiStream,
    stUnicodeStream:begin
                      BuffSize := 0; //chunked
                      Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
                    end;
    stBinaryStream: begin
                      BuffSize := 0; //chunked
                      Bind^.buffer_type_address^ := FIELD_TYPE_BLOB;
                    end;
    else raise EZUnsupportedException.Create(sUnsupportedOperation);
  end;
  if BuffSize > 0 then
    ReAllocMem(Bind.buffer, BuffSize+Ord(Bind^.buffer_type_address^ in [FIELD_TYPE_STRING, FIELD_TYPE_NEWDECIMAL, FIELD_TYPE_BLOB,FIELD_TYPE_TINY_BLOB] ))
  else if Bind.buffer <> nil then begin
    FreeMem(Bind.buffer);
    Bind.buffer := nil;
  end;
  Bind^.buffer_address^ := Bind.buffer;
  Bind^.buffer_length_address^ := BuffSize;
  Bind^.Iterations := 1;
  BindList[Index].SQLType := SQLType;
  fBindAgain := True;
  if (SQLType in [stDate, stTime, stTimeStamp]) and not (Bind^.buffer_type_address^ = FIELD_TYPE_STRING) then begin
    FillChar(Bind^.buffer^, SizeOf(TMYSQL_TIME), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
    if SQLType = stTime then
      PMYSQL_TIME(Bind^.buffer)^.time_type := MYSQL_TIMESTAMP_TIME
    else if SQLType = stTimeStamp then
      PMYSQL_TIME(Bind^.buffer)^.time_type := MYSQL_TIMESTAMP_DATETIME;
  end;
end;

procedure TZAbstractMySQLPreparedStatement.InternalRealPrepare;
var I: Integer;
begin
  if (FMYSQL_STMT = nil) then
    FMYSQL_STMT := FPlainDriver.mysql_stmt_init(FPMYSQL^);
  FBindAgain := True;
  FMyHandleStatus := myhsAllocated;
  if (FPlainDriver.mysql_stmt_prepare(FMYSQL_STMT, Pointer(FASQL), length(FASQL)) <> 0) then
    FMySQLConnection.HandleErrorOrWarning(lcPrepStmt, FMYSQL_STMT, SQL,
      IImmediatelyReleasable(FWeakImmediatRelPtr));
  FMyHandleStatus := myhsPrepared;
  //see user comment: http://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-fetch.html
  //"If you want work with more than one statement simultaneously, anidated select,
  //for example, you must declare CURSOR_TYPE_READ_ONLY the statement after just prepared this.!"
  if FUseResult and ((TokenMatchIndex = Ord(mySelect)) or (TokenMatchIndex = Ord(myCall)) ) then
    //EH: This can be set only if results are expected else server is hanging on execute
    if (FClientVersion >= 50020 ) then //supported since 5.0.2
      if Assigned(FPlainDriver.mysql_stmt_attr_set517UP) //we need this to be able to use more than !one! stmt -> keep cached
      then FPlainDriver.mysql_stmt_attr_set517UP(FMYSQL_STMT, STMT_ATTR_CURSOR_TYPE, @CURSOR_TYPE_READ_ONLY)
      else FPlainDriver.mysql_stmt_attr_set(FMYSQL_STMT, STMT_ATTR_CURSOR_TYPE, @CURSOR_TYPE_READ_ONLY);
  if (FClientVersion >= 50060) and (FPrefetchRows <> 1) and ((TokenMatchIndex = Ord(mySelect)) or (TokenMatchIndex = Ord(myCall))) then //supported since 5.0.6
    //try achieve best performnce. No idea how to calculate it
    if Assigned(FPlainDriver.mysql_stmt_attr_set517UP) and (FPrefetchRows <> 1)
    then FPlainDriver.mysql_stmt_attr_set517UP(FMYSQL_STMT, STMT_ATTR_PREFETCH_ROWS, @FPrefetchRows)
    else FPlainDriver.mysql_stmt_attr_set(FMYSQL_STMT, STMT_ATTR_PREFETCH_ROWS, @FPrefetchRows);
  FEmulatedParams := False;
  for i := 0 to BindList.Count -1 do
    PZMySQLBindValue(BindList[i]).EmulatedValue := '';
  if (BindList.Capacity > 0) and (FMYSQL_BINDs = nil) then
    InternalSetInParamCount(BindList.Capacity);
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcPrepStmt,Self);
end;

procedure TZAbstractMySQLPreparedStatement.InternalSetInParamCount(NewParamCount: Integer);
var I: Integer;
begin
  if not FEmulatedParams then
    if (FMYSQL_BINDs <> nil) and (NewParamCount <> BindList.Count) or ((NewParamCount > 0) and (FMYSQL_aligned_BINDs = nil)) then begin
      ReallocBindBuffer(FMYSQL_BINDs, FMYSQL_aligned_BINDs, FBindOffset,
        BindList.Count*Ord(FMYSQL_aligned_BINDs<>nil), NewParamCount, 1);
      if NewParamCount > 0 then begin
        //init types, buffers and move data to buffer
        BindList.BindValuesToStatement(Self);
        //releas duplicate data now
        for i := 0 to BindList.Count -1 do
          if not (BindList[i].BindType in [zbtArray, zbtRefArray]) then
            if BindList[i].BindType <> zbtLob then
              BindList.ClearValue(I)
            {$R-}
            else InitBuffer(BindList[i].SQLType, i, @FMYSQL_aligned_BINDs[I]);
            {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      end;
    end;
  inherited SetParamCount(NewParamCount);
end;

function TZAbstractMySQLPreparedStatement.IsOutParamResult: Boolean;
begin
  Result := False;
  if FPMYSQL^ <> nil then
    Result := PLongWord(PAnsiChar(FPMYSQL^)+GetServerStatusOffset(FClientVersion))^ and SERVER_PS_OUT_PARAMS <> 0;
end;

{ TZMySQLStatement }

constructor TZMySQLStatement.Create(const Connection: IZMySQLConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
  FEmulatedParams := True;
  FMinExecCount2Prepare := -1;
  FInitial_emulate_prepare := True;
end;

{ TZMySQLPreparedStatement }

procedure TZMySQLPreparedStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var
  Bind: PMYSQL_aligned_BIND;
begin
  if (SQLType = stGUID) and FGUIDAsString then
    BindRawStr(Index, GUIDToRaw(Buf, Len, False))
  else if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then inherited BindBinary(Index, SQLType, Buf, Len)
    else CheckParameterIndex(Index);
    PZMySQLBindValue(BindList[Index]).EmulatedValue := ZDbcUtils.GetSQLHexAnsiString(PAnsiChar(Buf), Len, False);
  end else begin
    CheckParameterIndex(Index);
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> SQLType) or (Bind^.buffer_length_address^ < Cardinal(Len+1)*Byte(Ord(SQLType <> stBinaryStream))) then begin
      InitBuffer(SQLType, Index, Bind, Len);
      BindList[Index].SQLType := SQLType;
    end;
    if SQLType <> stBinaryStream then begin
      if Len = 0
      then PByte(Bind^.buffer)^ := Ord(#0)
      else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, Pointer(Bind^.buffer)^, Len);
      Bind^.Length[0] := Len;
    end else begin
      FChunkedData := True;
      Bind^.Length[0] := 0;
    end;
    Bind^.is_null_address^ := STMT_INDICATOR_NONE;
  end;
end;

procedure TZMySQLPreparedStatement.BindUInteger(Index: Integer; SQLType: TZSQLType;
  Value: NativeUInt);
var
  Bind: PMYSQL_aligned_BIND;
  BindValue: PZBindValue;
  MySQLBindValue: PZMySQLBindValue absolute BindValue;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure EmulatedAsRaw; begin MySQLBindValue.EmulatedValue  := IntToRaw(Value) end;
begin
  CheckParameterIndex(Index);
  BindValue := BindList[Index];
  if ((BindValue.ParamType <> pctUnknown) and (BindValue.SQLType <> stUnknown)) then
    SQLType := BindValue.SQLType;
  if FEmulatedParams then begin
    case SQLType of
      stBoolean: begin
          BindList.Put(Index, Value <> 0);
          if FMySQL_FieldType_Bit_1_IsBoolean
          then EmulatedAsRaw
          else MySQLBindValue.EmulatedValue  := EnumBool[Value <> 0]
        end;
      stShort, stSmall, stInteger{$IFDEF CPU64},stLong{$ENDIF}:
        BindSInteger(Index, BindValue.SQLType, Value);
      stByte, stWord, stLongWord{$IFDEF CPU64},stULong{$ENDIF}, stArray{overwrite}: begin
          BindList.Put(Index, SQLType, {$IFNDEF CPU64}P4Bytes{$ELSE}P8Bytes{$ENDIF}(@Value));
          EmulatedAsRaw;
        end;
      {$IFNDEF CPU64}
      stLong: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      stULong: SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      {$ENDIF}
      stFloat, stDouble, stTime, stDate, stTimeStamp:
          InternalBindDouble(Index, SQLType, Value);
      stCurrency: SetCurrency(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stBigDecimal: begin
          ZSysUtils.ScaledOrdinal2Bcd(Value, 0, PBCD(FByteBuffer)^, False);
          BindList.Put(Index, PBCD(FByteBuffer)^);
        end;
      else begin
        EmulatedAsRaw;
        SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZMySQLBindValue(BindList[Index]).EmulatedValue);
      end;
    end;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindValue.SQLType <> SQLType) or (Bind^.buffer = nil) then
      InitBuffer(SQLType, Index, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:      if Bind^.is_unsigned_address^ = 0
                            then PShortInt(Bind^.buffer)^ := ShortInt(Value)
                            else PByte(Bind^.buffer)^ := Byte(Value);
      FIELD_TYPE_SHORT:     if Bind^.is_unsigned_address^ = 0
                            then PSmallInt(Bind^.buffer)^ := SmallInt(Value)
                            else PWord(Bind^.buffer)^ := Word(Value);
      FIELD_TYPE_LONG:      if Bind^.is_unsigned_address^ = 0
                            then PInteger(Bind^.buffer)^ := Integer(Value)
                            else PCardinal(Bind^.buffer)^ := Cardinal(Value);
      FIELD_TYPE_LONGLONG:  if Bind^.is_unsigned_address^ = 0
                            then PInt64(Bind^.buffer)^ := Value
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                            else PUInt64(Bind^.buffer)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      FIELD_TYPE_STRING:  begin //can happen only if stBoolean and not MySQL_FieldType_Bit_1_IsBoolean
                            Bind^.Length[0] := 1;
                            PWord(Bind^.buffer)^ := PWord(EnumBool[Value <> 0])^;
                          end;
      else raise CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stLongWord, SQLType);
    end;
    Bind^.is_null_address^ := STMT_INDICATOR_NONE;
  end;
end;

class function TZMySQLPreparedStatement.GetBindListClass: TZBindListClass;
begin
  Result := TZMySQLBindList;
end;

procedure TZMySQLPreparedStatement.BindSInteger(Index: Integer; SQLType: TZSQLType;
  Value: NativeInt);
var
  Bind: PMYSQL_aligned_BIND;
  BindValue: PZBindValue;
  MySQLBindValue: PZMySQLBindValue absolute BindValue;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure EmulatedAsRaw; begin MySQLBindValue.EmulatedValue := IntToRaw(Value) end;
begin
  CheckParameterIndex(Index);
  BindValue := BindList[Index];
  if ((BindValue.ParamType <> pctUnknown) and (BindValue.SQLType <> stUnknown)) then
    SQLType := BindValue.SQLType;
  if FEmulatedParams then begin
    case SQLType of
      stBoolean: begin
          BindList.Put(Index, Value <> 0);
          if FMySQL_FieldType_Bit_1_IsBoolean
          then EmulatedAsRaw
          else MySQLBindValue.EmulatedValue := EnumBool[Value <> 0]
        end;
      stShort, stSmall, stInteger{$IFDEF CPU64},stLong{$ENDIF}, stArray{overwrite}: begin
          BindList.Put(Index, SQLType, {$IFNDEF CPU64}P4Bytes{$ELSE}P8Bytes{$ENDIF}(@Value));
          EmulatedAsRaw;
        end;
      stByte, stWord, stLongWord{$IFDEF CPU64},stULong{$ENDIF}:
        BindUInteger(Index, SQLType, Value);
      {$IFNDEF CPU64}
      stLong: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      stULong: SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      {$ENDIF}
      stFloat, stDouble, stTime, stDate, stTimeStamp:
          InternalBindDouble(Index, BindValue.SQLType, Value);
      stCurrency: SetCurrency(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stBigDecimal: begin
          ZSysUtils.ScaledOrdinal2Bcd(Value, 0, PBCD(FByteBuffer)^);
          BindList.Put(Index, PBCD(FByteBuffer)^);
        end;
      else begin
        EmulatedAsRaw;
        SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, MySQLBindValue.EmulatedValue);
      end;
    end;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindValue.SQLType <> SQLType) or (Bind^.buffer = nil) then
      InitBuffer(SQLType, Index, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:      if Bind^.is_unsigned_address^ = 0
                            then PShortInt(Bind^.buffer)^ := ShortInt(Value)
                            else PByte(Bind^.buffer)^ := Byte(Value);
      FIELD_TYPE_SHORT:     if Bind^.is_unsigned_address^ = 0
                            then PSmallInt(Bind^.buffer)^ := SmallInt(Value)
                            else PWord(Bind^.buffer)^ := Word(Value);
      FIELD_TYPE_LONG:      if Bind^.is_unsigned_address^ = 0
                            then PInteger(Bind^.buffer)^ := Integer(Value)
                            else PCardinal(Bind^.buffer)^ := Cardinal(Value);
      FIELD_TYPE_LONGLONG:  if Bind^.is_unsigned_address^ = 0
                            then PInt64(Bind^.buffer)^ := Value
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                            else PUInt64(Bind^.buffer)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      FIELD_TYPE_STRING:  begin //can happen only if stBoolean and not MySQL_FieldType_Bit_1_IsBoolean
                            Bind^.Length[0] := 1;
                            PWord(Bind^.buffer)^ := PWord(EnumBool[Value <> 0])^;
                          end;
      else raise CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stInteger, SQLType);
    end;
    Bind^.is_null_address^ := STMT_INDICATOR_NONE;
  end;
end;

procedure TZMySQLPreparedStatement.BindInParameters;
var
  P: PAnsiChar;
  Len: NativeUInt;
  I: Integer;
  bind: PMYSQL_aligned_BIND;
  OffSet, PieceSize: Cardinal;
  array_size: UInt;
begin
  if not FEmulatedParams and (FBindAgain or (FTokenMatchIndex = Ord(myCall)))//handle a MySQL bug:
     // EH: on sp's we always need to rebind else we get an invalid input parameter error
     // to veryfy just execute mysql_stmt_execute twice and the error pop's up!
     and (BindList.Count > 0) and (FMYSQL_STMT <> nil) then begin
    if (BatchDMLArrayCount > 0) then begin
      //set array_size first: https://mariadb.com/kb/en/library/bulk-insert-column-wise-binding/
      array_size := BatchDMLArrayCount;
      if FPlainDriver.mysql_stmt_attr_set517up(FMYSQL_STMT, STMT_ATTR_ARRAY_SIZE, @array_size) <> 0 then begin
        FMySQLConnection.HandleErrorOrWarning(lcBindPrepStmt, FMYSQL_STMT,
          {$IFDEF ZEOSDEBUG}'mysql_stmt_attr_set'{$ELSE}''{$ENDIF}, IImmediatelyReleasable(FWeakImmediatRelPtr));
      end;
    end;
    if (FPlainDriver.mysql_stmt_bind_param(FMYSQL_STMT, PAnsichar(FMYSQL_BINDs)+(Ord(BindList.HasReturnParam)*FBindOffset.size)) <> 0) then
      FMySQLConnection.HandleErrorOrWarning(lcBindPrepStmt, FMYSQL_STMT,
        SBindingFailure, IImmediatelyReleasable(FWeakImmediatRelPtr));
      FBindAgain := False;
  end;
  inherited BindInParameters;
  { now finlize chunked data }
  if not FEmulatedParams and FChunkedData then
    // Send large data chunked
    for I := 0 to BindList.Count - 1 do begin
      {$R-}
      Bind := @FMYSQL_aligned_BINDs[I];
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      if (Bind^.is_null_address^ = 0) and (Bind^.buffer = nil) and (BindList[i].BindType = zbtLob) then begin
        P := IZBlob(BindList[I].Value).GetBuffer(FRawTemp, Len);
        OffSet := 0;
        PieceSize := fChunkSize;
        while (OffSet < Len) or (Len = 0) do begin
          if OffSet+PieceSize > Len then
            PieceSize := Len - OffSet;
          if (FPlainDriver.mysql_stmt_send_long_data(FMYSQL_STMT, I, P, PieceSize) <> 0) then begin
            FMySQLConnection.HandleErrorOrWarning(lcBindPrepStmt, FMYSQL_STMT,
              SBindingFailure, IImmediatelyReleasable(FWeakImmediatRelPtr));
            exit;
          end else Inc(P, PieceSize);
          Inc(OffSet, PieceSize);
          if Len = 0 then Break;
        end;
      end;
    end;
end;

procedure TZMySQLPreparedStatement.BindLob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
var
  Bind: PMYSQL_aligned_BIND;
  P: Pointer;
  L: NativeUInt;
begin
  inherited BindLob(Index, SQLType, Value); //refcounts
  if (Value = nil) or (Value.IsEmpty) then
    SetNull(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType)
  else if FEmulatedParams then begin
    P := Value.GetBuffer(FRawTemp, L);
    if SQLType = stBinaryStream
    then PZMySQLBindValue(BindList[Index]).EmulatedValue := ZDbcUtils.GetSQLHexAnsiString(P, L, False)
    else FMySQLConnection.GetEscapeString(P, L, PZMySQLBindValue(BindList[Index]).EmulatedValue)
  end else begin
    FChunkedData := True;
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (Bind^.buffer <> nil) or
      ((Bind^.buffer_type_address^ <> FIELD_TYPE_BLOB) and (Bind^.buffer_type_address^ <> FIELD_TYPE_STRING)) then
      InitBuffer(SQLType, Index, Bind, 0);
    Bind^.is_null_address^ := STMT_INDICATOR_NONE;
  end;
end;

procedure TZMySQLPreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
var
  Bind: PMYSQL_aligned_BIND;
  Len: LengthInt;
  BindValue: PZBindValue;
  SQLType: TZSQLType;
begin
  CheckParameterIndex(Index);
  Len := Length(Value){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
  BindValue := BindList[Index];
  if (BindValue.SQLType = stUnknown) or (BindValue.SQLType = stArray)
  then SQLType := stString
  else SQLType := BindValue.SQLType;
  if FEmulatedParams then begin
    BindList.Put(Index, SQLType, Value, FClientCP); //localize
    FMySQLConnection.GetEscapeString(Pointer(Value), Len, PZMySQLBindValue(BindList[Index]).EmulatedValue);
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindValue.ParamType = pctUnknown) or (BindValue.SQLType in [stBigDecimal, stCurrency, stString, stGUID]) then begin
      if (BindValue.SQLType <> SQLType) or (Bind.buffer_length_address^ < Cardinal(Len+1)) then
        InitBuffer(SQLType, Index, Bind, Len);
      if Len > 0 then
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Bind^.buffer)^, Len);
      PByte(PAnsiChar(Bind^.buffer)+Len)^ := Ord(#0);
      Bind^.Length[0] := Len;
      Bind^.is_null_address^ := STMT_INDICATOR_NONE;
    end else
      BindRawStr(Index, Pointer(Value), Len);
  end;
end;

procedure TZMySQLPreparedStatement.InternalBindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var
  Bind: PMYSQL_aligned_BIND;
  P: PMYSQL_TIME;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure BindEmulated;
  var Len: LengthInt;
  begin
    case SQLType of
      stDate: Len := DateTimeToRawSQLDate(Value, PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings, True);
      stTime: Len := DateTimeToRawSQLTime(Value, PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings, True);
      stTimestamp: Len := DateTimeToRawSQLTimeStamp(Value, PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings, True)
      else Len := FloatToSQLRaw(Value, PAnsiChar(FByteBuffer));
    end;
    ZSetString(PAnsiChar(FByteBuffer), Len, PZMySQLBindValue(BindList[Index]).EmulatedValue);
  end;
begin
  CheckParameterIndex(Index);
  if FEmulatedParams then begin
    BindList.Put(Index, SQLType, P8Bytes(@Value));
    BindEmulated;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> SQLType) or (Bind^.buffer = nil)  then
      InitBuffer(SQLType, Index, Bind);
    if Bind^.buffer_type_address^ = FIELD_TYPE_DOUBLE then
      PDouble(bind^.buffer)^ := Value
    else begin
      P := Pointer(bind^.buffer);
      P^.neg := Ord(Value < 0);
      if P^.time_type = MYSQL_TIMESTAMP_DATE then
        DecodeDate(Value, PWord(@P^.Year)^, PWord(@P^.Month)^, PWord(@P^.Day)^)
      else begin
        P^.second_part := 0;
        if P^.time_type = MYSQL_TIMESTAMP_TIME
        then DecodeTime(Value, PWord(@P^.hour)^, PWord(@P^.minute)^, PWord(@P^.second)^, PWord(@P^.second_part)^)
        else DecodeDateTime(Value, PWord(@P^.Year)^, PWord(@P^.Month)^, PWord(@P^.Day)^,
          PWord(@P^.hour)^, PWord(@P^.minute)^, PWord(@P^.second)^, PWord(@P^.second_part)^);
        P^.second_part := P^.second_part*1000;
      end;
    end;
    Bind^.is_null_address^ := 0;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZMySQLPreparedStatement.BindRawStr(Index: Integer; Buf: PAnsiChar;
  Len: LengthInt);
var
  Bind: PMYSQL_aligned_BIND;
  BindValue: PZBindValue;
  SQLType: TZSQLType;
  TS: TZTimeStamp;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
  procedure BindAsLob;
  var Lob: IZBlob;
    P: PAnsiChar;
  begin
    if SQLType = stBinaryStream then
      Lob := TZLocalMemBLob.CreateWithData(Buf, Len)
    else begin
      if Len = 0
      then P := PEmptyAnsiString
      else P := Buf;
      Lob := TZLocalMemCLob.CreateWithData(P, Len, FClientCP, ConSettings);
      BindLob(Index, SQLType, Lob);
    end;
  end;
begin
  CheckParameterIndex(Index);
  BindValue := BindList[Index];
  if (BindValue.SQLType = stUnknown) or (BindValue.SQLType = stArray)
  then SQLType := stString
  else SQLType := BindValue.SQLType;
  if FEmulatedParams then begin
    BindList.Put(Index, SQLType, Buf, Len, FClientCP); //localize
    FMySQLConnection.GetEscapeString(Buf, Len, PZMySQLBindValue(BindList[Index]).EmulatedValue);
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindValue.ParamType = pctUnknown) or (BindValue.SQLType in [stBigDecimal, stCurrency, stString, stGUID]) then begin
      if (BindList.SQLTypes[Index] <> stString) or (Bind.buffer_length_address^ < Cardinal(Len+1)) then
        InitBuffer(stString, Index, Bind, Len);
      if Len = 0
      then PByte(Bind^.buffer)^ := Ord(#0)
      else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, Pointer(Bind^.buffer)^, Len+1);
      Bind^.Length[0] := Len;
      Bind^.is_null_address^ := 0;
    end else case BindValue.SQLType of
      stBoolean: BindSInteger(Index, stBoolean, Ord(StrToBoolEx(Buf, Buf+Len)));
      stShort, stSmall, stInteger: BindSInteger(Index, stBoolean, RawToInt(Buf));
      stLong: {$IFDEF CPU64}
              BindSInteger(Index, stBoolean, RawToInt64Def(Buf, -1));
              {$ELSE}
              SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RawToInt64Def(Buf, -1));
              {$ENDIF}
      stByte, stWord: BindUInteger(Index, stBoolean, RawToInt(Buf));
      stLongWord, stULong: {$IFDEF CPU64}
              BindUInteger(Index, stBoolean, RawToUInt64Def(Buf, 0));
              {$ELSE}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
              SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RawToUInt64Def(Buf, 0));
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
              {$ENDIF}
      stFloat, stDouble: begin
          if Bind^.buffer_type_address^ = FIELD_TYPE_DOUBLE
          then RawToFloat(Buf, AnsiChar('.'), PDouble(bind^.buffer)^)
          else RawToFloat(Buf, AnsiChar('.'), PSingle(bind^.buffer)^);
          Bind^.is_null_address^ := 0;
        end;
      stTime:     if TryPCharToTime(Buf, Len, ConSettings.WriteFormatSettings, T)
                  then SetTime(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, T);
      stDate:     if TryPCharToDate(Buf, Len, ConSettings.WriteFormatSettings, D)
                  then SetDate(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, D);
      stTimeStamp: if TryPCharToTimeStamp(Buf, Len, ConSettings.WriteFormatSettings, TS)
                  then SetTimeStamp(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TS);
      stAsciiStream..stBinaryStream: BindAsLob;
      else raise CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stString, BindValue.SQLType);
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZMySQLPreparedStatement.SetBigDecimal(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
var
  Bind: PMYSQL_aligned_BIND;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure EmulatedAsRaw; begin PZMySQLBindValue(BindList[Index]).EmulatedValue := BcdToSQLRaw(Value) end;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  if FEmulatedParams then begin
    BindList.Put(Index, Value);
    EmulatedAsRaw;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stBigDecimal) or (Bind^.buffer = nil) then
      InitBuffer(stBigDecimal, Index, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_DOUBLE:  PDouble(Bind^.buffer)^ := BCDToDouble(Value);
      FIELD_TYPE_NEWDECIMAL,
      FIELD_TYPE_STRING:  begin
                            Bind^.Length[0] := BcdToRaw(Value, Bind.buffer, '.');
                            PByte(PAnsiChar(Bind.buffer)+Bind^.Length[0])^ := Ord(#0);
                          end;
      else raise CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBigDecimal, BindList.SQLTypes[Index]);
    end;
    Bind^.is_null_address^ := 0;
  end;
end;

procedure TZMySQLPreparedStatement.SetBoolean(Index: Integer;
  Value: Boolean);
begin
  if FMySQL_FieldType_Bit_1_IsBoolean
  then BindSInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBoolean, Ord(Value))
  else SetRawByteString(Index, EnumBool[Value]);
end;

procedure TZMySQLPreparedStatement.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  BindUInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stByte, Value);
end;

procedure TZMySQLPreparedStatement.SetBytes(Index: Integer; Value: PByte;
  Len: NativeUInt);
var
  Bind: PMYSQL_aligned_BIND;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then BindList.Put(Index, stBytes, Value, Len)
    else CheckParameterIndex(Index);
    PZMySQLBindValue(BindList[Index]).EmulatedValue := ZDbcUtils.GetSQLHexAnsiString(PAnsiChar(Value), Len, False);
  end else begin
    CheckParameterIndex(Index);
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stBytes) or (Bind^.buffer_length_address^ < (Len+1)) then begin
      InitBuffer(stBytes, Index, Bind, Len);
      BindList[Index].SQLType := stBytes;
    end;
    if Len = 0
    then PByte(Bind^.buffer)^ := Ord(#0)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, Pointer(Bind^.buffer)^, Len);
    Bind^.Length[0] := Len;
    if Value = nil then
      if FUseDefaults
      then Bind^.is_null_address^ := STMT_INDICATOR_DEFAULT
      else Bind^.is_null_address^ := STMT_INDICATOR_NULL
    else Bind^.is_null_address^ := 0;
  end;
end;

procedure TZMySQLPreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var
  Bind: PMYSQL_aligned_BIND;
  PEnd: PAnsiChar;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure EmulatedAsRaw; begin PZMySQLBindValue(BindList[Index]).EmulatedValue := CurrToRaw(Value, '.') end;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  if FEmulatedParams then begin
    BindList.Put(Index, stCurrency, P8Bytes(@Value));
    EmulatedAsRaw;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stCurrency) or (Bind^.buffer = nil) then
      InitBuffer(stCurrency, Index, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:      if Bind^.is_unsigned_address^ = 0
                            then PShortInt(Bind^.buffer)^ := PInt64(@Value)^ div 10000
                            else PByte(Bind^.buffer)^ := PInt64(@Value)^ div 10000;
      FIELD_TYPE_SHORT:     if Bind^.is_unsigned_address^ = 0
                            then PSmallInt(Bind^.buffer)^ := PInt64(@Value)^ div 10000
                            else PWord(Bind^.buffer)^ := PInt64(@Value)^ div 10000;
      FIELD_TYPE_LONG:      if Bind^.is_unsigned_address^ = 0
                            then PInteger(Bind^.buffer)^ := PInt64(@Value)^ div 10000
                            else PCardinal(Bind^.buffer)^ := PInt64(@Value)^ div 10000;
      FIELD_TYPE_LONGLONG:  if Bind^.is_unsigned_address^ = 0
                            then PInt64(Bind^.buffer)^ := PInt64(@Value)^ div 10000
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                            else PUInt64(Bind^.buffer)^ := PInt64(@Value)^ div 10000;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      FIELD_TYPE_DOUBLE:    PDouble(Bind^.buffer)^ := Value;
      FIELD_TYPE_NEWDECIMAL,
      FIELD_TYPE_STRING:  begin
                            CurrToRaw(Value, '.', Bind.buffer, @PEnd);
                            Bind^.Length[0] := PEnd-PAnsiChar(Bind.buffer);
                            PByte(PEnd)^ := 0;
                          end;
      else raise CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stCurrency, BindList.SQLTypes[Index]);
    end;
    Bind^.is_null_address^ := 0;
  end;
end;

procedure TZMySQLPreparedStatement.SetDate(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate);
var
  Bind: PMYSQL_aligned_BIND;
  P: PMYSQL_TIME;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure BindEmulated;
  var L: LengthInt;
  begin
    L := DateToRaw(Value.Year, Value.Month, Value.Day,
      PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateFormat, True, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), L, PZMySQLBindValue(BindList[Index]).EmulatedValue);
  end;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  if FEmulatedParams then begin
    BindList.Put(Index, Value);
    BindEmulated;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stDate) or (Bind^.buffer = nil)  then
      InitBuffer(stDate, Index, Bind);
    P := Pointer(bind^.buffer);
    P^.neg := Ord(Value.IsNegative);
    P^.year := Value.Year;
    P^.month := Value.Month;
    P^.day := Value.Day;
    Bind^.is_null_address^ := STMT_INDICATOR_NONE;
  end;
end;

procedure TZMySQLPreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
begin
  InternalBindDouble(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stDouble, Value);
end;

procedure TZMySQLPreparedStatement.SetFloat(Index: Integer; Value: Single);
begin
  InternalBindDouble(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stFloat, Value);
end;

procedure TZMySQLPreparedStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZSQLStringWriter; var Result: SQLString);
var
  Bind: PMYSQL_aligned_BIND;
  BindValue: PZBindValue;
  MySQLBindValue: PZMySQLBindValue absolute BindValue;
  TmpDateTime, TmpDateTime2: TDateTime;
begin
  CheckParameterIndex(ParamIndex);
  if FEmulatedParams then begin
    BindValue := BindList[ParamIndex];
    if BindValue.SQLType in [stAsciiStream, stUnicodeStream]
    then SQLWriter.AddText('(CLOB)', Result)
    else if BindValue.SQLType = stBinaryStream then
      SQLWriter.AddText('(BLOB)', Result)
    else if BindValue.SQLType = stAsciiStream then
      SQLWriter.AddText('(CLOB)', Result)
    {$IFDEF UNICODE}
    else if Ord(BindValue.SQLType) < Ord(stString) then
      SQLWriter.AddAscii7Text(Pointer(MySQLBindValue.EmulatedValue), Length(MySQLBindValue.EmulatedValue), Result)
    else begin
      FUniTemp := ZRawToUnicode(MySQLBindValue.EmulatedValue, FClientCP);
      SQLWriter.AddText(FUniTemp, Result);
      FUniTemp := '';
    end
    {$ELSE}
    else SQLWriter.AddText(MySQLBindValue.EmulatedValue, Result)
    {$ENDIF}
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[ParamIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:
        if Bind^.is_unsigned_address^ = 0
        then SQLWriter.AddOrd(PShortInt(Bind^.buffer_address^)^, Result)
        else SQLWriter.AddOrd(PByte(Bind^.buffer_address^)^, Result);
      FIELD_TYPE_SHORT:
        if Bind^.is_unsigned_address^ = 0
        then SQLWriter.AddOrd(PSmallInt(Bind^.buffer_address^)^, Result)
        else SQLWriter.AddOrd(PWord(Bind^.buffer_address^)^, Result);
      FIELD_TYPE_LONG:
        if Bind^.is_unsigned_address^ = 0
        then SQLWriter.AddOrd(PInteger(Bind^.buffer_address^)^, Result)
        else SQLWriter.AddOrd(PCardinal(Bind^.buffer_address^)^, Result);
      FIELD_TYPE_FLOAT:
        SQLWriter.AddFloat(PSingle(Bind^.buffer_address^)^, Result);
      FIELD_TYPE_DOUBLE:
        SQLWriter.AddFloat(PDouble(Bind^.buffer_address^)^, Result);
      FIELD_TYPE_NULL:
        SQLWriter.AddText('(NULL)', Result);
      FIELD_TYPE_TIMESTAMP:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(Bind^.buffer_address^)^.Year,
            PMYSQL_TIME(Bind^.buffer_address^)^.Month,
            PMYSQL_TIME(Bind^.buffer_address^)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(Bind^.buffer_address^)^.Hour,
            PMYSQL_TIME(Bind^.buffer_address^)^.Minute,
            PMYSQL_TIME(Bind^.buffer_address^)^.Second,
            0{PMYSQL_TIME(Bind^.buffer_address^)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          if TmpDateTime < 0
          then TmpDateTime := TmpDateTime - TmpDateTime2
          else TmpDateTime := TmpDateTime + TmpDateTime2;
          SQLWriter.AddDateTime(TmpDateTime, ConSettings^.WriteFormatSettings.DateTimeFormat, Result);
        end;
      FIELD_TYPE_LONGLONG:
        if Bind^.is_unsigned_address^ = 0
        then SQLWriter.AddOrd(PInt64(Bind^.buffer_address^)^, Result)
        else SQLWriter.AddOrd(PUInt64(Bind^.buffer_address^)^, Result);
      FIELD_TYPE_DATE: begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(Bind^.buffer_address^)^.Year,
            PMYSQL_TIME(Bind^.buffer_address^)^.Month,
            PMYSQL_TIME(Bind^.buffer_address^)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          SQLWriter.AddDate(TmpDateTime, ConSettings^.WriteFormatSettings.DateTimeFormat, Result);
        end;
      FIELD_TYPE_TIME: begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(Bind^.buffer_address^)^.Hour,
            PMYSQL_TIME(Bind^.buffer_address^)^.Minute,
            PMYSQL_TIME(Bind^.buffer_address^)^.Second,
            0{PMYSQL_TIME(Bind^.buffer_address^)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          SQLWriter.AddTime(TmpDateTime, ConSettings^.WriteFormatSettings.DateTimeFormat, Result);
        end;
      FIELD_TYPE_YEAR:
        SQLWriter.AddOrd(PWord(Bind^.buffer_address^)^, Result);
      FIELD_TYPE_NEWDECIMAL:
        {$IFDEF UNICODE}
        SQLWriter.AddAscii7Text(PAnsiChar(Bind^.buffer), Bind^.length[0], Result);
        {$ELSE}
        SQLWriter.AddText(PAnsiChar(Bind^.buffer), Bind^.length[0], Result);
        {$ENDIF}
      FIELD_TYPE_STRING: if BindList[ParamIndex].SQLType in [stAsciiStream, stUnicodeStream]
          then SQLWriter.AddText('(CLOB)', Result)
          {$IFDEF UNICODE}
          else begin
            FUniTemp := PRawToUnicode(PAnsiChar(Bind^.buffer), Bind^.length[0], FClientCP);
            SQLWriter.AddTextQuoted(FUniTemp, #39, Result);
            FUniTemp := '';
          end;
          {$ELSE}
          else SQLWriter.AddTextQuoted(PAnsiChar(Bind^.buffer), Bind^.length[0], AnsiChar(#39), Result);
          {$ENDIF}
      FIELD_TYPE_TINY_BLOB: SQLWriter.AddHexBinary(Bind^.buffer, Bind^.length[0], False, Result);
      FIELD_TYPE_BLOB: SQLWriter.AddText('(BLOB)', Result);
      else SQLWriter.AddText('(UNKNOWN)', Result);
    end;
  end;
end;

procedure TZMySQLPreparedStatement.SetInt(ParameterIndex, Value: Integer);
begin
  BindSInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stInteger, Value);
end;

procedure TZMySQLPreparedStatement.SetLong(ParameterIndex: Integer;
  const Value: Int64);
{$IFDEF CPU64}
begin
  BindSInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLong, Value);
{$ELSE}
var
  Bind: PMYSQL_aligned_BIND;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure EmulatedAsRaw; begin PZMySQLBindValue(BindList[ParameterIndex]).EmulatedValue := IntToRaw(Value) end;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FEmulatedParams then begin
    BindList.Put(ParameterIndex, stLong, P8Bytes(@Value));
    EmulatedAsRaw;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[ParameterIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[ParameterIndex] <> stLong) or (Bind^.buffer = nil) then
      InitBuffer(stLong, ParameterIndex, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:      if Bind^.is_unsigned_address^ = 0
                            then PShortInt(Bind^.buffer)^ := ShortInt(Value)
                            else PByte(Bind^.buffer)^ := Byte(Value);
      FIELD_TYPE_SHORT:     if Bind^.is_unsigned_address^ = 0
                            then PSmallInt(Bind^.buffer)^ := SmallInt(Value)
                            else PWord(Bind^.buffer)^ := Word(Value);
      FIELD_TYPE_LONG:      if Bind^.is_unsigned_address^ = 0
                            then PInteger(Bind^.buffer)^ := Integer(Value)
                            else PCardinal(Bind^.buffer)^ := Cardinal(Value);
      FIELD_TYPE_LONGLONG:  if Bind^.is_unsigned_address^ = 0
                            then PInt64(Bind^.buffer)^ := Value
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                            else PUInt64(Bind^.buffer)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      FIELD_TYPE_STRING:  begin //can happen only if stBoolean and not MySQL_FieldType_Bit_1_IsBoolean
                            Bind^.Length[0] := 1;
                            PWord(Bind^.buffer)^ := PWord(EnumBool[Value <> 0])^;
                          end;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //impossible
    end;
    Bind^.is_null_address^ := 0;
  end;
{$ENDIF}
end;

procedure TZMySQLPreparedStatement.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
var
  Bind: PMYSQL_aligned_BIND;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if Boolean(BindList[ParameterIndex].ParamType) and Boolean(BindList[ParameterIndex].SQLType) then
    SQLType := BindList[ParameterIndex].SQLType;
  if FEmulatedParams then begin
    if FTokenMatchIndex <> -1
    then BindList.SetNull(ParameterIndex, SQLType);
    //we always need a new copy of the defult values else we'll write into the metadata default values for the non unicode compilers
    PZMySQLBindValue(BindList[ParameterIndex]).EmulatedValue := 'null'
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[ParameterIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[ParameterIndex] <> SQLType) then
      InitBuffer(SQLType, ParameterIndex, Bind, 0);
    if FUseDefaults
    then Bind^.is_null_address^ := STMT_INDICATOR_DEFAULT
    else Bind^.is_null_address^ := STMT_INDICATOR_NULL;
  end;
end;

procedure TZMySQLPreparedStatement.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  BindSInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stShort, Value);
end;

procedure TZMySQLPreparedStatement.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  BindSInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stSmall, Value);
end;

procedure TZMySQLPreparedStatement.SetTime(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime);
var
  Bind: PMYSQL_aligned_BIND;
  P: PMYSQL_TIME;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  if FEmulatedParams then begin
    BindList.Put(Index, Value);
    L := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
      PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.TimeFormat, True, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), L, PZMySQLBindValue(BindList[Index]).EmulatedValue);
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stTime) or (Bind^.buffer = nil) then
      InitBuffer(stTime, Index, Bind);
    P := Pointer(bind^.buffer);
    P^.neg := Ord(Value.IsNegative);
    P^.hour := Value.Hour;
    P^.minute := Value.Minute;
    P^.second := Value.Second;
    P^.second_part := Value.Fractions div 1000;
    Bind^.is_null_address^ := STMT_INDICATOR_NONE;
  end;
end;

procedure TZMySQLPreparedStatement.SetTimestamp(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp);
var
  Bind: PMYSQL_aligned_BIND;
  P: PMYSQL_TIME;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  if FEmulatedParams then begin
    BindList.Put(Index, Value);
    L := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
      Value.Hour, Value.Minute, Value.Second, Value.Fractions,
      PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateTimeFormat, True, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), L, PZMySQLBindValue(BindList[Index]).EmulatedValue);
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[Index] <> stTimeStamp) or (Bind^.buffer = nil)  then
      InitBuffer(stTimeStamp, Index, Bind);
    P := Pointer(bind^.buffer);
    P^.neg := Ord(Value.IsNegative);
    P^.year := Value.Year;
    P^.month := Value.Month;
    P^.day := Value.Day;
    P^.hour := Value.Hour;
    P^.minute := Value.Minute;
    P^.second := Value.Second;
    P^.second_part := Value.Fractions div 1000;
    Bind^.is_null_address^ := STMT_INDICATOR_NONE;
  end;
end;

procedure TZMySQLPreparedStatement.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  BindUInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLongWord, Value);
end;

procedure TZMySQLPreparedStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
{$IFDEF CPU64}
begin
  BindUInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stULong, Value);
{$ELSE}
var
  Bind: PMYSQL_aligned_BIND;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure EmulatedAsRaw; begin PZMySQLBindValue(BindList[ParameterIndex]).EmulatedValue := IntToRaw(Value) end;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FEmulatedParams then begin
    BindList.Put(ParameterIndex, stULong, P8Bytes(@Value));
    EmulatedAsRaw;
  end else begin
    {$R-}
    Bind := @FMYSQL_aligned_BINDs[ParameterIndex];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList.SQLTypes[ParameterIndex] <> stULong) or (Bind^.buffer = nil) then
      InitBuffer(stULong, ParameterIndex, Bind);
    case Bind^.buffer_type_address^ of
      FIELD_TYPE_TINY:      if Bind^.is_unsigned_address^ = 0
                            then PShortInt(Bind^.buffer)^ := ShortInt(Value)
                            else PByte(Bind^.buffer)^ := Byte(Value);
      FIELD_TYPE_SHORT:     if Bind^.is_unsigned_address^ = 0
                            then PSmallInt(Bind^.buffer)^ := SmallInt(Value)
                            else PWord(Bind^.buffer)^ := Word(Value);
      FIELD_TYPE_LONG:      if Bind^.is_unsigned_address^ = 0
                            then PInteger(Bind^.buffer)^ := Integer(Value)
                            else PCardinal(Bind^.buffer)^ := Cardinal(Value);
      FIELD_TYPE_LONGLONG:  if Bind^.is_unsigned_address^ = 0
                            then PInt64(Bind^.buffer)^ := Value
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                            else PUInt64(Bind^.buffer)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      FIELD_TYPE_STRING:  begin //can happen only if stBoolean and not MySQL_FieldType_Bit_1_IsBoolean
                            Bind^.Length[0] := 1;
                            PWord(Bind^.buffer)^ := PWord(EnumBool[Value <> 0])^;
                          end;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //impossible
    end;
    Bind^.is_null_address^ := 0;
  end;
{$ENDIF}
end;

procedure TZMySQLPreparedStatement.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  BindUInteger(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stWord, Value);
end;

{ TZMySQLCallableStatement56up }

function TZMySQLCallableStatement56up.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  SQLWriter: TZSQLStringWriter;
begin
  SQL := '';
  I := Length(StoredProcName);
  i := I + 20+BindList.Count shl 1;
  SQLWriter := TZSQLStringWriter.Create(I);
  if IsFunction //see http://ftp.nchu.edu.tw/MySQL/doc/refman/4.1/en/sql-syntax-prepared-statements.html
  then SQLWriter.AddText('SELECT ', SQL) //EH: How todo a SET ? = function ??
  else SQLWriter.AddText('CALL ', SQL);
  SQLWriter.AddText(StoredProcName, SQL);
  if BindList.Count > 0 then
    SQLWriter.AddChar('(', SQL);
  for i := 0 to BindList.Count-1 do
    if BindList.ParamTypes[i] <> pctReturn then
      SQLWriter.AddText('?,', SQL);
  if BindList.Count > 0 then begin
    SQLWriter.CancelLastComma(SQL);
    SQLWriter.AddChar(')', SQL);
  end;
  if IsFunction then
    SQLWriter.AddText(' as ReturnValue', SQL);;
  SQLWriter.Finalize(SQL);
  FreeAndNil(SQLWriter);
  Result := TZMySQLPreparedStatement.Create(Connection as IZMySQLConnection, SQL, Info);
  {TZMySQLPreparedStatement(Result).FMinExecCount2Prepare := 0; //prepare immediately
  TZMySQLPreparedStatement(Result).InternalRealPrepare;}
  TZMySQLPreparedStatement(Result).Prepare;
  FSupportsBidirectionalParamIO := True;
end;

{ TZMySQLCallableStatement56down }

procedure TZMySQLCallableStatement56down.AfterConstruction;
begin
  inherited AfterConstruction;
  FPlainDriver := TZMySQLPLainDriver(Connection.GetIZPlainDriver.GetInstance);
end;

procedure TZMySQLCallableStatement56down.BindInParameters;
var SQL: RawByteString;
  I: Integer;
  Stmt: TZMySQLPreparedStatement;
  SQLWriter: TZRawSQLStringWriter;
  BindValue: PZBindValue;
  MySQLBindValue: PZMySQLBindValue absolute BindValue;
  MySQLBindValue56Down: PZMySQLBindValue56down absolute BindValue;
begin
  inherited BindInParameters;
  if (BindList.Count = 0) then
    Exit;
  SQL := 'SET ';
  SQLWriter := TZRawSQLStringWriter.Create(4+BindList.Count shl 5);
  Stmt := TZMySQLPreparedStatement(FExecStatement);
  for I := 0 to BindList.Count -1 do begin
    BindValue := BindList[i];
    if Ord(BindValue.ParamType) < Ord(pctOut) then begin
      SQLWriter.AddChar(AnsiChar('@'), SQL);
      {$IFDEF UNICODE}
      SQLWriter.AddText(ZUnicodeToRaw(MySQLBindValue56Down.ParamName, FClientCP), SQL);
      {$ELSE}
      SQLWriter.AddText(MySQLBindValue56Down.ParamName, SQL);
      {$ENDIF}
      SQLWriter.AddChar(AnsiChar('='), SQL);
      SQLWriter.AddText(MySQLBindValue.EmulatedValue, SQL);
      SQLWriter.AddChar(AnsiChar(','), SQL);
    end;
  end;
  SQLWriter.Finalize(SQL);
  I := Length(SQL);
  if i = 4 then //no inparams ?
    Exit;
  if FplainDriver.mysql_real_query(Stmt.FPMYSQL^, Pointer(SQL), I-1) <> 0 then begin
    {$IFDEF UNICODE}
    FUniTemp := ZRawToUnicode(SQL, FClientCP);
    Stmt.FMySQLConnection.HandleErrorOrWarning(lcExecute, nil, FUniTemp,
      IImmediatelyReleasable(FWeakImmediatRelPtr));
    {$ELSE}
    Stmt.FMySQLConnection.HandleErrorOrWarning(lcExecute, nil, SQL,
      IImmediatelyReleasable(FWeakImmediatRelPtr));
    {$ENDIF}
  end;
end;

function TZMySQLCallableStatement56down.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  SQLWriter: TZSQLStringWriter;
  BindValue: PZMySQLBindValue56down;
begin
  I := Length(StoredProcName);
  i := I + 20+BindList.Count shl 1;
  SQLWriter := TZSQLStringWriter.Create(I);
  try
    if BindList.HasReturnParam //see http://ftp.nchu.edu.tw/MySQL/doc/refman/4.1/en/sql-syntax-prepared-statements.html
    then SQL := 'SELECT ' //EH: How todo a SET ? = function ??
    else SQL := 'CALL ';
    SQLWriter.AddText(StoredProcName, SQL);
    if BindList.Count-Ord(BindList.HasReturnParam) > 0 then
      SQLWriter.AddChar('(', SQL);
    for i := Ord(BindList.HasReturnParam) to BindList.Count-1 do begin
      BindValue := PZMySQLBindValue56down(BindList[i]);
      SQLWriter.AddChar('@', SQL);
      SQLWriter.AddText(BindValue.ParamName, SQL);
      SQLWriter.AddChar(',', SQL);
    end;
    if BindList.Count-Ord(BindList.HasReturnParam) > 0 then begin
      SQLWriter.CancelLastComma(SQL);
      SQLWriter.AddChar(')', SQL);
    end;
    if IsFunction then
      SQLWriter.AddText(' as ReturnValue', SQL);
    SQLWriter.Finalize(SQL);

    FStmt := TZMySQLPreparedStatement.Create(Connection as IZMySQLConnection, SQL, Info);
    Result := FStmt;
    FStmt.FMinExecCount2Prepare := -1; //prepare never
    FStmt.FEmulatedParams := True;
    FStmt.FInitial_emulate_prepare := True;
    FStmt.FUseDefaults := False;
    FStmt.Prepare;
    SQL := 'SELECT ';
    for i := Ord(BindList.HasReturnParam) to BindList.Count-1 do
      if Ord(BindList[I].ParamType) >= Ord(pctInOut) then begin
        BindValue := PZMySQLBindValue56down(BindList[i]);
        SQLWriter.AddChar('@', SQL);
        SQLWriter.AddText(BindValue.ParamName, SQL);
        SQLWriter.AddChar(',', SQL);
      end;
    SQLWriter.CancelLastComma(SQL);
    SQLWriter.Finalize(SQL);
    if SQL <> 'SELECT ' then begin
      FGetOutParmStmt := TZMySQLPreparedStatement.Create(Connection as IZMySQLConnection, SQL, Info);
      FGetOutParmStmt.FMinExecCount2Prepare := -1; //prepare never
      FGetOutParmStmt.FEmulatedParams := True;
      FGetOutParmStmt.FInitial_emulate_prepare := True;
      FGetOutParmStmt._AddRef;
    end;
  finally
    FreeAndNil(SQLWriter);
  end;
end;

procedure TZMySQLCallableStatement56down.CreateOutParamResultSet;
var
  I, j: Integer;
  ColumnInfo: TZColumnInfo;
  ColumnsInfo: TObjectList;
  RS: TZVirtualResultSet;
  BCD: TBCD;
  L: NativeUInt;
  Bind: PZBindValue;
  BindValue: PZMySQLBindValue56down absolute Bind;
  Result: IZResultSet;
begin
  if BindList.HasInOutParam or BindList.HasOutParam then begin
    ColumnsInfo := TObjectList.Create;
    Result := FGetOutParmStmt.ExecuteQueryPrepared; //refcounting..
    try
      for I := 0 to BindList.Count -1 do begin
        Bind := BindList[I];
        if Ord(Bind.ParamType) >= Ord(pctInOut) then begin
          ColumnInfo := TZColumnInfo.Create;
          with ColumnInfo do begin
            ColumnLabel := BindValue.ParamName;
            ColumnType := Bind.SQLType;
            Precision := BindValue.PrecisionOrLen;
            Scale := BindValue.Scale;
          end;
          ColumnsInfo.Add(ColumnInfo);
        end;
      end;
      RS := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '', ConSettings);
      RS.MoveToInsertRow;
      RS.SetType(rtScrollInsensitive);
      RS.SetConcurrency(rcReadOnly);
      J := FirstDbcIndex;
      if Result.IsBeforeFirst then
        Result.Next;
      for I := 0 to BindList.Count -1 do begin
        Bind := BindList[I];
        if (Ord(Bind.ParamType) >= Ord(pctInOut)) then begin
          if not Result.IsNull(J) then
            case Bind.SQLType of
              stBoolean: RS.UpdateBoolean(J, Result.GetBoolean(J));
              stByte, stWord, stLongWord: RS.UpdateUInt(J, Result.GetUInt(J));
              stShort, stSmall, stInteger: RS.UpdateInt(J, Result.GetInt(J));
              stLong: RS.UpdateLong(J, Result.GetLong(J));
              stULong: RS.UpdateULong(J, Result.GetULong(J));
              stFloat, stDouble: RS.UpdateDouble(J, Result.GetDouble(J));
              stCurrency: RS.UpdateCurrency(J, Result.GetCurrency(J));
              stBigDecimal: begin
                  Result.GetBigDecimal(J, BCD{%H-});
                  RS.UpdateBigDecimal(J, BCD);
                end;
              stTime: RS.UpdateTime(J, Result.GetTime(J));
              stDate: RS.UpdateDate(J, Result.GetDate(J));
              stTimeStamp: RS.UpdateTimeStamp(J, Result.GetTimeStamp(J));
              stString, stUnicodeString: RS.UpdatePAnsiChar(J, Result.GetPAnsiChar(J, L), L);
              stBytes, stGUID: RS.UpdateBytes(J, Result.GetBytes(J));
              stAsciiStream..stBinaryStream: RS.UpdateLob(J, Result.GetBlob(J));
              {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //impossible
            end;
          Inc(J);
        end;
      end;
      RS.InsertRow;
      Result := RS; //now we free the native result
      FStmt.FOutParamResultSet := Result;
    finally
      ColumnsInfo.Free;
    end;
  end else if (FStmt.FOutParamResultSet <> nil) and not (BindList.HasReturnParam) then begin
    FStmt.FOutParamResultSet.Close;
    FStmt.FOutParamResultSet := nil;
  end;
end;

function TZMySQLCallableStatement56down.ExecutePrepared: Boolean;
begin
  Result := Inherited ExecutePrepared;
  CreateOutParamResultSet;
end;

function TZMySQLCallableStatement56down.ExecuteQueryPrepared: IZResultSet;
begin
  if (FStmt<> nil) and (FStmt.FOutParamResultSet <> nil) then begin
    FStmt.FOutParamResultSet.Close;
    FStmt.FOutParamResultSet := nil;
  end;
  Result := Inherited ExecuteQueryPrepared;
  CreateOutParamResultSet;
  if (Result = nil) and (FStmt.FOutParamResultSet <> nil) then
    Result := FStmt.FOutParamResultSet;
end;

function TZMySQLCallableStatement56down.ExecuteUpdatePrepared: Integer;
begin
  Result := Inherited ExecuteUpdatePrepared;
  CreateOutParamResultSet;
end;

class function TZMySQLCallableStatement56down.GetBindListClass: TZBindListClass;
begin
  Result := TZMySQLBindList56down;
end;

procedure TZMySQLCallableStatement56down.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZProcedureColumnType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
var BindValue: PZMySQLBindValue56down ;
begin
  CheckParameterIndex(ParameterIndex);
  if not FParamsRegistered or FRegisteringParamFromMetadata then begin
    inherited RegisterParameter(ParameterIndex, SQLType, ParamType);
    BindValue := PZMySQLBindValue56down(BindList[ParameterIndex]);
    BindValue.ParamName := Name;
    BindValue.PrecisionOrLen := PrecisionOrSize;
    BindValue.Scale := Scale;
  end;
end;

procedure TZMySQLCallableStatement56down.Unprepare;
begin
  if Assigned(FGetOutParmStmt) then begin
    FGetOutParmStmt.Close;
    FGetOutParmStmt._Release;
    FGetOutParmStmt := nil
  end;
  FStmt := nil;
  inherited Unprepare;
end;

{ TZMySQLBindList }

class function TZMySQLBindList.GetElementSize: Integer;
begin
  Result := SizeOf(TZMySQLBindValue);
end;

procedure TZMySQLBindList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) then
    PZMySQLBindValue(Ptr).EmulatedValue := '';
  inherited Notify(Ptr, Action);
end;

{ TZMySQLBindList56down }

class function TZMySQLBindList56down.GetElementSize: Integer;
begin
  Result := SizeOf(TZMySQLBindValue56down);
end;

procedure TZMySQLBindList56down.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) then
    PZMySQLBindValue56down(Ptr).ParamName := '';
  inherited Notify(Ptr, Action);
end;

{ TZMariaDBBatchDMLPreparedStatement }

{$IFDEF FPC} {$PUSH}
  {$WARN 5057 off : Local variable "TS" does not seem to be initialized}
  {$IFDEF WITH_NOT_INLINED_WARNING}
    {$WARN 6058 off : Call to subroutine "procedure GUIDToBuffer(const Source:Pointer;Dest:PChar;const Options:TGUIDConvOptions);" marked as inline is not inlined}
  {$ENDIF}
{$ENDIF}
procedure TZMariaDBBatchDMLPreparedStatement.ClearParameters;
var
  array_size: UInt;
  I: Integer;
  Bind: PMYSQL_aligned_BIND;
begin
  if BatchDMLArrayCount > 0 then begin
    array_size := 0;
    for i := 0 to BindList.Count -1 do begin
      {$R-}
      Bind := @FMYSQL_aligned_BINDs^[i];
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      FreeMem(Bind.indicators, BatchDMLArrayCount);
      Bind.indicators := nil;
      Bind.indicator_address^ := nil;
      if (Bind^.buffer_address^ = Bind.buffer) and (Bind.buffer <> nil) then begin
        FreeMem(Bind.buffer);
        Bind.buffer := nil;
        if (PZArray(BindList[i].Value).VArrayType >= Byte(stGUID)) or
          ((TZSqlType(PZArray(BindList[i].Value).VArrayType) = stBoolean) and not FMySQL_FieldType_Bit_1_IsBoolean) then begin
          FreeMem(Bind^.length, SizeOf(ULong)*BatchDMLArrayCount);
          GetMem(Bind^.length, SizeOf(ULong));
          Bind.length_address^ := Bind^.length;
          Bind.buffer_length_address^ := 0;
        end;
      end;
      Bind^.buffer_address^ := Bind.buffer;
    end;
    if FPlainDriver.mysql_stmt_attr_set517up(FMYSQL_STMT, STMT_ATTR_ARRAY_SIZE, @array_size) <> 0 then begin
      FMySQLConnection.HandleErrorOrWarning(lcOther, FMYSQL_STMT, SBindingFailure,
        IImmediatelyReleasable(FWeakImmediatRelPtr));
    end;
  end;
  inherited ClearParameters;
end;

procedure TZMariaDBBatchDMLPreparedStatement.SetDataArray(
  ParameterIndex: Integer; const Value; const SQLType: TZSQLType;
  const VariantType: TZVariantType);
var
  Bind: PMYSQL_aligned_BIND;
  I: Integer;
  ClientCP: Word;
  MySQLTime: PMYSQL_TIME;
  P, PEnd: PAnsiChar;
  PD: PZDate absolute PEnd;
  PT: PZTime absolute PEnd;
  PTS: PZTimeStamp absolute PEnd;
  TS: TZtimeStamp;
  D: TZDate absolute TS;
  T: TZTime absolute TS;
  procedure BindLobs;
  var Lob: IZBLob;
    CLob: IZCLob;
    I: Integer;
    P: Pointer;
    L: NativeUint;
  begin
    ReAllocMem(Bind^.length, BatchDMLArrayCount*SizeOf(ULong));
    Bind^.length_address^ := Bind^.length;
    Bind^.buffer_type_address^ := FIELD_TYPE_BLOB;
    ReAllocMem(Bind^.Buffer, SizeOf(Pointer)*BatchDMLArrayCount);
    for I := 0 to BatchDMLArrayCount -1 do begin
      if (TInterfaceDynArray(Value)[i] = nil) or not Supports(TInterfaceDynArray(Value)[i], IZBlob, Lob) or Lob.IsEmpty then
        {$R-}Bind^.indicators[i] := Ord(STMT_INDICATOR_NULL){$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      else begin
        if (SQLType <> stBinaryStream) then begin
          if not Supports(Lob, IZCLob, CLob)
          then raise CreateConversionError(ParameterIndex, stBinaryStream, stAsciiStream)
          else CLob.SetCodePageTo(ClientCP);
        end;
        P := Lob.GetBuffer(FRawTemp, L);
        PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := P;
        {$R-}Bind^.length[i] := L;{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      end;
    end;
    Bind^.buffer_address^ := Pointer(Bind^.Buffer);
  end;
  procedure BindRaw;
  var I: Integer;
  begin
    ReAllocMem(Bind^.Buffer, SizeOf(Pointer)*BatchDMLArrayCount);
    for I := 0 to BatchDMLArrayCount -1 do begin
      {$R-}
      Bind^.length[i] := Length(TRawByteStringDynArray(Value)[i]);
      if Bind^.length[i] > 0
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      then PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := Pointer(TRawByteStringDynArray(Value)[i]) //write address
      else PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := PEmptyAnsiString;
    end;
    Bind^.buffer_address^ := Pointer(Bind^.buffer);
  end;
  procedure BindRawFromConvertion;
  var I: Integer;
    ClientStrings: TRawByteStringDynArray;
    BufferSize: ULong;
  label move_from_temp;
  begin
    BufferSize := 0;
    {$IFDEF WITH_VAR_INIT_WARNING}ClientStrings := nil;{$ENDIF}
    SetLength(ClientStrings, BatchDMLArrayCount);
    case VariantType of
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString: begin
          for I := 0 to BatchDMLArrayCount -1 do begin
            fUniTemp := ZRawToUnicode(TRawByteStringDynArray(Value)[i], ZOSCodePage);
            ClientStrings[i] := ZUnicodeToRaw(fUniTemp, ClientCP);
            BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
          end;
          goto move_from_temp;
        end;
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String: begin
          for I := 0 to BatchDMLArrayCount -1 do begin
            fUniTemp := ZRawToUnicode(TRawByteStringDynArray(Value)[i], zCP_UTF8);
            ClientStrings[i] := ZUnicodeToRaw(fUniTemp, ClientCP);
            BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
          end;
          goto move_from_temp;
        end;
      {$ENDIF}
      vtUnicodeString
      {$IFDEF UNICODE}
      ,vtString
      {$ENDIF}:       begin
          for I := 0 to BatchDMLArrayCount -1 do begin
            ClientStrings[i] := ZUnicodeToRaw(TUnicodeStringDynArray(Value)[i], ClientCP);
            BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
          end;
move_from_temp:
          ReAllocMem(Bind^.Buffer, Cardinal(SizeOf(Pointer)*BatchDMLArrayCount)+BufferSize);
          P := PAnsichar(Bind^.Buffer)+ SizeOf(Pointer)*BatchDMLArrayCount;
          for I := 0 to BatchDMLArrayCount -1 do begin
            {$R-}Bind^.length[i] := Length(ClientStrings[i]);
            if Bind^.length[i] > 0
            then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(ClientStrings[i])^, P^, Bind^.length[i]+1)  //write buffer
            else Byte(P^) := Ord(#0);
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := P;
            Inc(P, Bind^.length[i]+1);
            {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
          end;
          Bind^.buffer_address^ := Pointer(Bind^.buffer);
        end;
      vtCharRec:      begin
          ReAllocMem(Bind^.Buffer, SizeOf(Pointer)*BatchDMLArrayCount); //minumum size
          for I := 0 to BatchDMLArrayCount -1 do
            if (TZCharRecDynArray(Value)[i].CP = ClientCP) or (TZCharRecDynArray(Value)[i].Len = 0) then begin
              {$R-}Bind^.length[i] := TZCharRecDynArray(Value)[i].Len;{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
              PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := TZCharRecDynArray(Value)[i].P; //wite address
            end else if (TZCharRecDynArray(Value)[i].CP = zCP_UTF16) then begin
              ClientStrings[i] := PUnicodeToRaw(TZCharRecDynArray(Value)[i].P, TZCharRecDynArray(Value)[i].Len, ClientCP);
              BufferSize := BufferSize + Cardinal(Length(ClientStrings[i])) +1;
              {$R-}Bind^.length[i] := Length(ClientStrings[i]);{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
            end else begin
              fUniTemp := PRawToUnicode(TZCharRecDynArray(Value)[i].P, TZCharRecDynArray(Value)[i].Len, TZCharRecDynArray(Value)[i].CP);
              ClientStrings[i] := ZUnicodeToRaw(fUniTemp, ClientCP);
              BufferSize := BufferSize + Cardinal(Length(ClientStrings[i]))+1;
              {$R-}Bind^.length[i] := Length(ClientStrings[i]);{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
            end;
          if BufferSize > 0 then begin
            ReAllocMem(Bind^.buffer, Cardinal(BatchDMLArrayCount*SizeOf(Pointer))+BufferSize);
            P := PAnsichar(Bind^.Buffer)+ SizeOf(Pointer)*BatchDMLArrayCount;
            for I := 0 to BatchDMLArrayCount -1 do
              if Pointer(ClientStrings[i]) <> nil then begin
                {$R-}
                {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(ClientStrings[i])^, P^, Bind^.length[i]); //write buffer
                PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := P;
                Inc(P, Bind^.length[i]+1);
                {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
              end;
            end;
          Bind^.buffer_address^ := Pointer(Bind^.buffer);
        end;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //tested alread in inherited call
    end;
    SetLength(ClientStrings, 0);
  end;
begin
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  if (FMYSQL_STMT = nil) then begin
    InternalRealPrepare;
    Exit;
  end;
  if (FMYSQL_STMT = nil) then
    raise EZSQLException.Create(SFailedtoPrepareStmt);
  {$R-}
  Bind := @FMYSQL_aligned_BINDs[ParameterIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  ClientCP := ConSettings^.ClientCodePage.CP;
  FBindAgain := True;
  ReAllocMem(Bind^.indicators, BatchDMLArrayCount);
  Bind^.indicator_address^ := Pointer(Bind^.indicators);
  FillChar(Pointer(Bind^.indicators)^, BatchDMLArrayCount, Char(STMT_INDICATOR_NONE));
  Bind^.Iterations := BatchDMLArrayCount;
  case SQLType of
    stBoolean:
      if FMySQL_FieldType_Bit_1_IsBoolean then begin
        Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
        Bind^.buffer_address^ := Pointer(Value); //no move
      end else begin
        ReAllocMem(Bind^.length, BatchDMLArrayCount*SizeOf(ULong));
        Bind^.length_address^ := Bind^.length;
        ReAllocMem(Bind^.buffer, SizeOf(Pointer)*BatchDMLArrayCount + (BatchDMLArrayCount shl 1));
        Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
        for i := 0 to BatchDMLArrayCount -1 do begin
          PWord(PAnsiChar(Bind^.buffer)+BatchDMLArrayCount*SizeOf(Pointer)+(i shl 1))^ := PWord(EnumBool[TBooleanDynArray(Value)[i]])^; //write data
          PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := PAnsiChar(Bind^.buffer)+BatchDMLArrayCount*SizeOf(Pointer)+(i shl 1); //write address
          {$R-}
          Bind^.length[i] := 1;
          {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
        end;
      end;
    stByte, stShort: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_TINY;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stByte));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stWord, stSmall: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_SHORT;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stWord));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stInteger, stLongWord: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_LONG;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stLongWord));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stLong, stULong: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_LONGLONG;
        Bind^.is_unsigned_address^ := Byte(Ord(SQLType = stULong));
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stFloat: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_FLOAT;
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stDouble: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_DOUBLE;
        Bind^.buffer_address^ := Pointer(Value); //no move
      end;
    stCurrency: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_NEWDECIMAL;
        ReAllocMem(Bind^.length, BatchDMLArrayCount*SizeOf(Ulong));
        Bind^.length_address^ := Bind^.length;
        ReAllocMem(Bind^.buffer, (SizeOf(Pointer)+22{19Dig, Sign, Dot, #0}) *BatchDMLArrayCount);
        Bind^.buffer_address^ := Bind^.buffer;
        if (VariantType = vtNull) or (VariantType = vtCurrency) then
          P := PAnsiChar(Bind^.buffer)+(SizeOf(Pointer)*BatchDMLArrayCount);
          for i := 0 to BatchDMLArrayCount -1 do begin
            PPointer(PAnsiChar(Bind.Buffer)+i*SizeOf(Pointer))^ := P;
            CurrToRaw(TCurrencyDynArray(Value)[i], '.', P, @PEnd);
            Bind^.length[i] := PEnd-P;
            PByte(PEnd)^ := 0;
            Inc(P, Bind^.length[i]+1);
          end;
      end;
    stBigDecimal: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_NEWDECIMAL;
        ReAllocMem(Bind^.length, BatchDMLArrayCount*SizeOf(Ulong));
        Bind^.length_address^ := Bind^.length;
        ReAllocMem(Bind^.buffer, (SizeOf(Pointer)+FmtBcd.MaxFMTBcdFractionSize+3{Sign, Dot, #0}) *BatchDMLArrayCount);
        Bind^.buffer_address^:= Bind.buffer;
        if (VariantType = vtNull) or (VariantType = vtBigDecimal) then
          P := PAnsiChar(Bind^.buffer)+(SizeOf(Pointer)*BatchDMLArrayCount);
          for i := 0 to BatchDMLArrayCount -1 do begin
            FRawTemp := BcdToSQLRaw(TBCDDynArray(Value)[i]);
            PPointer(PAnsiChar(Bind.Buffer)+i*SizeOf(Pointer))^ := P;
            Bind^.length[i] := Length(FRawTemp);
            Move(Pointer(FRawTemp)^, P^, Bind^.length[i]+1);
            Inc(P, Bind^.length[i]+1);
          end;
      end;
     stDate, stTime, stTimeStamp: begin
        ReAllocMem(Bind^.buffer, (SizeOf(TMYSQL_TIME)+SizeOf(Pointer))*BatchDMLArrayCount);
        Bind^.buffer_address^ := Pointer(Bind^.buffer);
        P := PAnsiChar(Bind^.buffer)+(BatchDMLArrayCount*SizeOf(Pointer));
        FillChar(P^, BatchDMLArrayCount*SizeOf(TMYSQL_TIME), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
        if SQLType = stDate then begin
          Bind^.buffer_type_address^ := FIELD_TYPE_DATE;
          for i := 0 to BatchDMLArrayCount -1 do begin
            MySQLTime := PMYSQL_TIME(P+(I*SizeOf(TMYSQL_TIME)));
            if VariantType in [vtNull, vtDateTime] then begin
              DecodeDateTimeToDate(TDateTimeDynArray(Value)[i], D);
              PD :=  @D;
            end else if VariantType = vtDate
              then PD :=  @TZDateDynArray(Value)[i]
              else raise CreateUnsupportedParameterTypeException(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stDate);
            MySQLTime^.year := PD^.Year;
            MySQLTime^.month := PD^.Month;
            MySQLTime^.day := PD^.Day;
            MySQLTime^.neg := Byte(Ord(PD^.IsNegative));
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := MySQLTime; //write address
            //MySQLTime.time_type := MYSQL_TIMESTAMP_DATE; //done by fillchar
          end;
        end else if SQLType = stTime then begin
          Bind^.buffer_type_address^ := FIELD_TYPE_TIME;
          for i := 0 to BatchDMLArrayCount -1 do begin
            MySQLTime := PMYSQL_TIME(P+(I*SizeOf(TMYSQL_TIME)));
            if VariantType in [vtNull, vtDateTime] then begin
              ZSysUtils.DecodeDateTimeToTime(TDateTimeDynArray(Value)[i], T);
              PT :=  @T;
            end else if VariantType = vtTime
              then PT :=  @TZTimeDynArray(Value)[i]
              else raise CreateUnsupportedParameterTypeException(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stTime);
            MySQLTime^.hour := PT.Hour;
            MySQLTime^.minute := PT.Minute;
            MySQLTime^.second := PT.Second;
            MySQLTime^.second_part := PT.Fractions div 1000;
            MySQLTime^.neg := Byte(Ord(PT^.IsNegative));
            MySQLTime^.time_type := MYSQL_TIMESTAMP_TIME;
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := MySQLTime; //write address
          end
        end else begin
          Bind^.buffer_type_address^ := FIELD_TYPE_DATETIME;
          for i := 0 to BatchDMLArrayCount -1 do begin
            MySQLTime := PMYSQL_TIME(P+(I*SizeOf(TMYSQL_TIME)));
            if VariantType in [vtNull, vtDateTime] then begin
              ZSysUtils.DecodeDateTimeToTimeStamp(TDateTimeDynArray(Value)[i], TS);
              PTS :=  @TS;
            end else if VariantType = vtTimeStamp
              then PTS :=  @TZTimeStampDynArray(Value)[i]
              else raise CreateUnsupportedParameterTypeException(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stTimeStamp);
            MySQLTime^.year := PTS^.Year;
            MySQLTime^.month := PTS^.Month;
            MySQLTime^.day := PTS^.Day;
            MySQLTime^.hour := PTS^.Hour;
            MySQLTime^.minute := PTS^.Minute;
            MySQLTime^.second := PTS^.Second;
            MySQLTime^.second_part := PTS^.Fractions div 1000;
            MySQLTime^.neg := Ord(PTS^.IsNegative);
            MySQLTime^.time_type := MYSQL_TIMESTAMP_DATETIME;
            PPointer(PAnsiChar(Bind^.buffer)+(I*SizeOf(Pointer)))^ := MySQLTime; //write address
          end;
        end;
      end;
    stBytes: begin
        ReAllocMem(Bind^.buffer, SizeOf(Pointer)*BatchDMLArrayCount);
        Bind^.buffer_type_address^ := FIELD_TYPE_TINY_BLOB;
        ReAllocMem(Bind^.length, BatchDMLArrayCount*SizeOf(ULong));
        Bind^.length_address^ := Bind^.length;
        for i := 0 to BatchDMLArrayCount -1 do begin
          {$R-}
          Bind^.length[i] := Length(TBytesDynArray(Value)[i]);
          if Bind^.length[i] > 0
          {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
          then PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := Pointer(TBytesDynArray(Value)[i]) //write address
          else PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := PEmptyAnsiString;
        end;
        Bind^.buffer_address^ := Pointer(Bind^.buffer);
      end;
    stGUID: begin
        ReAllocMem(Bind^.length, BatchDMLArrayCount*SizeOf(ULong));
        Bind^.length_address^ := Bind^.length;
        if FGUIDAsString then begin
          ReAllocMem(Bind^.buffer, SizeOf(Pointer)*BatchDMLArrayCount + (37*BatchDMLArrayCount));
          Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
          P := PAnsiChar(Bind^.buffer)+ SizeOf(Pointer)*BatchDMLArrayCount;
          for i := 0 to BatchDMLArrayCount -1 do begin
            {$R-}Bind^.length[i] := 36; {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
            GUIDToBuffer(@TGUIDDynArray(Value)[i].D1, P, False, True);
            PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := P; //write address
            Inc(P, 37);
          end;
        end else begin
          ReAllocMem(Bind^.buffer, SizeOf(Pointer)*BatchDMLArrayCount);
          Bind^.buffer_type_address^ := FIELD_TYPE_TINY_BLOB;
          for i := 0 to BatchDMLArrayCount -1 do begin
            PPointer(PAnsiChar(Bind^.buffer)+I*SizeOf(Pointer))^ := @TGUIDDynArray(Value)[i].D1; //write address
            {$R-}Bind^.length[i] := SizeOf(TGUID);{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
          end;
        end;
        Bind^.buffer_address^ := Pointer(Bind^.buffer);
      end;
    stString, stUnicodeString: begin
        Bind^.buffer_type_address^ := FIELD_TYPE_STRING;
        ReAllocMem(Bind^.length, BatchDMLArrayCount*SizeOf(Ulong));
        Bind^.length_address^ := Bind^.length;
        case VariantType of
          {$IFNDEF UNICODE}
          vtString:
            BindRaw;
          {$ENDIF}
          {$IFNDEF NO_ANSISTRING}
          vtAnsiString: if (ZOSCodePage = ClientCP)
            then BindRaw
            else BindRawFromConvertion;
          {$ENDIF}
          {$IFNDEF NO_UTF8STRING}
          vtUTF8String: if (zCP_UTF8 = ClientCP)
            then BindRaw
            else BindRawFromConvertion;
          {$ENDIF}
          vtRawByteString: BindRaw;
          vtUnicodeString
          {$IFDEF UNICODE}
          ,vtString
          {$ENDIF}: BindRawFromConvertion;
          vtCharRec: BindRawFromConvertion;
          else raise EZSQLException.Create(SUnsupportedParameterType);
        end;
      end;
    stAsciiStream, stUnicodeStream, stBinaryStream: BindLobs;
    else raise CreateUnsupportedParameterTypeException(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType);
  end;
  Bind^.Iterations := BatchDMLArrayCount;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZMariaDBBatchDMLPreparedStatement.SetNullArray(
  ParameterIndex: Integer; const SQLType: TZSQLType; const Value;
  const VariantType: TZVariantType);
var
  Bind: PMYSQL_aligned_BIND;
  aArray: PZArray;
  I: Integer;
begin
  inherited SetNullArray(ParameterIndex, SQLType, Value, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  {$R-}
  Bind := @FMYSQL_aligned_BINDs[ParameterIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (FMYSQL_STMT = nil) then
    InternalRealPrepare;
  if (FMYSQL_STMT = nil) then
    raise EZSQLException.Create(SFailedtoPrepareStmt);
  aArray := BindList[ParameterIndex].Value;
  if Pointer(Value) = nil
  then FillChar(Bind^.indicators^, BatchDMLArrayCount, Char(MySQLNullIndicatorMatrix[False, FUseDefaults]))
  else for i := 0 to BatchDMLArrayCount -1 do
    {$R-}
    Bind^.indicators[I] :=  MySQLNullIndicatorMatrix[(Bind^.indicators[i] = Ord(STMT_INDICATOR_NULL)) or IsNullFromArray(aArray, I), FUseDefaults];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

{ TZMySQLEmulatedBatchPreparedStatement }

procedure TZMySQLEmulatedBatchPreparedStatement.ClearParameters;
begin
  if FEmulatedArrayDMLStatement <> nil then
    FlushEmulatedArrayDMLStatement;
  inherited ClearParameters;
end;

function TZMySQLEmulatedBatchPreparedStatement.CreateEmulatedArrayDMLStatement: TZMySQLPreparedStatement;
var PStart: PChar;
  SQLWriter: TZSQLStringWriter;
  I, L, BracketDiff, AfterBracketClosePos, BracketOpenPos: NativeUInt;
begin
  if FEmulatedArrayDMLStatement <> nil then
    FlushEmulatedArrayDMLStatement;
  if (FTokenMatchIndex >= Ord(myDelete)) and (FTokenMatchIndex <= Ord(myUpdate)) then begin
    if ((FTokenMatchIndex = Ord(myInsert)) and (FBracketClosePos > 0)) or
       ((FTokenMatchIndex = Ord(myDelete)) and (BindList.Capacity = 1)) then begin
      PStart := Pointer({$IFDEF UNICODE}fWSQL{$ELSE}fASQL{$ENDIF});
      L := Length({$IFDEF UNICODE}fWSQL{$ELSE}fASQL{$ENDIF});
      if FBracketClosePos = 0 then begin//delete whithout "in (?)"
        BracketDiff := 1;
        AfterBracketClosePos := FFirstQuestionMark +1;
        BracketOpenPos := FFirstQuestionMark;
      end else begin
        AfterBracketClosePos := FBracketClosePos +1;
        BracketDiff := AfterBracketClosePos-FBracketOpenPos;
        BracketOpenPos := FBracketOpenPos;
      end;
      SQLWriter := TZSQLStringWriter.Create(L+(BracketDiff*(Cardinal(BatchDMLArrayCount-1)){cloned questionmarks with brackets})+Cardinal(BatchDMLArrayCount){commas});
      try
        if FBracketClosePos = 0 then begin
          SQLWriter.AddText(PStart, FFirstQuestionMark-1, {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
          SQLWriter.AddText(' in (?', {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
        end else SQLWriter.AddText(PStart, AfterBracketClosePos, {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
        for I := 2 to BatchDMLArrayCount do begin
          SQLWriter.AddChar(',', {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
          SQLWriter.AddText(PStart+BracketOpenPos, BracketDiff, {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
        end;
        if FBracketClosePos = 0 then
          SQLWriter.AddChar(')', {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
        if (L > AfterBracketClosePos) then
          SQLWriter.AddText(PStart+AfterBracketClosePos, (L -AfterBracketClosePos), {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
        SQLWriter.Finalize({$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF});
      finally
        FreeAndNil(SQLWriter);
      end;
      if (FTokenMatchIndex <> Ord(myInsert)) then begin//we can use emulated values only var an in (?) statement
        Result := TZMySQLPreparedStatement.Create(FMySQLConnection, {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF}, Info);
        if (FTokenMatchIndex = Ord(myUpdate))
        then Result.FMinExecCount2Prepare := 0 //emmidiate prepare
        else Result.FMinExecCount2Prepare := -1; //never prepare -> excute as sql string or with mariadb_execute direct
      end else begin
        Result := TZMySQLPreparedStatement.Create(FMySQLConnection, '', Info);
        Result.{$IFDEF UNICODE}fWSQL{$ELSE}FASQL{$ENDIF} := {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF};
        {$IFDEF UNICODE}
        PUnicodeToRaw(Pointer(Result.FWSQL), Length(Result.FWSQL), FClientCP, Result.FASQL);
        {$ENDIF}
        Result.BindList.Count := BindList.Capacity * BatchDMLArrayCount;
        Result._AddRef;
        Result.FTokenMatchIndex := FTokenMatchIndex;
        Result.InternalRealPrepare;
      end;
      {$IFDEF UNICODE}FUniTemp{$ELSE}fRawTemp{$ENDIF} := '';
      FBindSingleRowBatches := False;
    end else begin
      Result := TZMySQLPreparedStatement.Create(FMySQLConnection, {$IFDEF UNICODE}fWSQL{$ELSE}fASQL{$ENDIF}, Info);
      FBindSingleRowBatches := True;
    end;
    Result._AddRef;
  end else
    raise EZSQLException.Create('No valid statement found for ArrayDML bindings.');
end;

procedure TZMySQLEmulatedBatchPreparedStatement.SetDataArray(
  ParameterIndex: Integer; const Value; const SQLType: TZSQLType;
  const VariantType: TZVariantType);
var OldBatchCount: Integer;
begin
  OldBatchCount := BatchDMLArrayCount;
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
  if ((FEmulatedArrayDMLStatement = nil) or ((OldBatchCount <> BatchDMLArrayCount) and (FTokenMatchIndex <> Ord(myUpdate)))) and (BatchDMLArrayCount > 0) then
    FEmulatedArrayDMLStatement := CreateEmulatedArrayDMLStatement;
  if (not FBindSingleRowBatches) then
    BindDataArrayAsParameters(BindList[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value, FEmulatedArrayDMLStatement,
      ParameterIndex, BindList.Capacity, BatchDMLArrayCount);
end;

procedure TZMySQLEmulatedBatchPreparedStatement.SetNullArray(
  ParameterIndex: Integer; const SQLType: TZSQLType; const Value;
  const VariantType: TZVariantType);
begin
  inherited SetNullArray(ParameterIndex, SQLType, Value, VariantType);
  BindNullArrayAsParameters(BindList[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value,
    FEmulatedArrayDMLStatement, ParameterIndex, BindList.Capacity, BatchDMLArrayCount);
end;

initialization

{ preparable statements: }
{$IFDEF WITH_VAR_INIT_WARNING}MySQL568PreparableTokens := nil;{$ENDIF}
SetLength(MySQL568PreparableTokens, Ord(myCall)+1);
MySQL568PreparableTokens[Ord(myDelete)].MatchingGroup := 'DELETE';
MySQL568PreparableTokens[Ord(myInsert)].MatchingGroup := 'INSERT';
MySQL568PreparableTokens[Ord(myUpdate)].MatchingGroup := 'UPDATE';
MySQL568PreparableTokens[Ord(mySelect)].MatchingGroup := 'SELECT';
MySQL568PreparableTokens[Ord(mySet)].MatchingGroup := 'SET';
MySQL568PreparableTokens[Ord(myCall)].MatchingGroup := 'CALL'; //for non realpreparable api we're emultating it..
{EH: all others i do ignore -> they are ususall send once }
{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
end.

