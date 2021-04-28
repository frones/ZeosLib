{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
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

{constributors:
  Mark Ford
}

unit ZDbcOleDBStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
{$IFDEF WIN64}
{$ALIGN 8}
{$ELSE}
{$ALIGN 2}
{$ENDIF}
{$MINENUMSIZE 4}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX, FmtBCD,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZCompatibility, ZSysUtils, ZPlainOleDBDriver, ZCollections, ZClasses,
  ZDbcLogging, ZDbcStatement, ZDbcIntfs, ZVariant, ZDbcProperties, ZDbcUtils,
  ZDbcOleDB, ZDbcOleDBUtils;

type
  /// <summary>Defines an OleDB specific statement interface</summary>
  IZOleDBPreparedStatement = Interface(IZStatement)
    ['{42A4A633-C63D-4EFA-A8BC-CF755237D0AD}']
    function GetInternalBufferSize: Integer;
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
  End;

  {** Implements Prepared ADO Statement. }
  TZAbstractOleDBStatement = class(TZUTF16ParamCountPreparedStatement,
    IZOleDBPreparedStatement)
  private
    FMultipleResults: IMultipleResults;
    FRowSet: IRowSet;
    FZBufferSize, fStmtTimeOut: Integer;
    FCommand: ICommandText;
    FRowSize: NativeUInt;
    FDBParams: TDBParams;
    FRowsAffected: DBROWCOUNT;
    fMoreResultsIndicator: TZMoreResultsIndicator;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FSupportsMultipleResultSets: Boolean;
    FOutParameterAvailibility: TOleEnum;
    FCallResultCache: TZCollection;
    FByteBuffer: PByteBuffer;
    FOleDBConnection: IZOleDBConnection;
    fDEFERPREPARE: Boolean; //ole: if not set the stmt will be prepared immediatelly and we'll try to decribe params
    procedure PrepareOpenedResultSetsForReusing;
    function FetchCallResults(var RowSet: IRowSet): Boolean;
    function GetFirstResultSet: IZResultSet;
    procedure ClearCallResultCache;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);
    destructor Destroy; override;
    /// <summary>Do tasks after the statement was closed. For example
    ///  dispose statement handles.</summary>
    procedure AfterClose; override;

    procedure Prepare; override;
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
    ///  <code>(not getMoreResults and (getUpdateCount = -1)</code>
    /// </summary>
    /// <returns><c>true</c> if the next result is a <c>ResultSet</c> object;
    ///  <c>false</c> if it is an update count or there are no more results
    /// </returns>
    /// <seealso cref="Execute">Execute</seealso>
    function GetMoreResults: Boolean; override;
    /// <summary>
    ///  Cancels this <c>Statement</c> object if both the DBMS and
    ///  driver support aborting an SQL statement.
    ///  This method can be used by one thread to cancel a statement that
    ///  is being executed by another thread.
    /// </summary>
    procedure Cancel; override;
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  protected
    function CreateResultSet: IZResultSet;
    function CreateOutParamResultSet: IZResultSet; virtual;
  public { implement IZOleDBPreparedStatement }
    function GetInternalBufferSize: Integer;
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
  end;

  EZOleDBConvertError = class(EZSQLException);

  TZOleDBPreparedStatement = class(TZAbstractOleDBStatement, IZPreparedStatement)
  private
    FDBBindingArray: TDBBindingDynArray;
    FParamNamesArray: TStringDynArray;
    FDBUPARAMS: DB_UPARAMS;
    fBindImmediat, //the param describe did fail! we'll try to bind the params with describe emulation
    fBindAgain: Boolean; //param type or sizes have been changed need to create a new accessor handle
    //fSupportsByRef: Boolean; //are by REF bound values supported by provider?
    FParamsBuffer: TByteDynArray; //our value buffer
    FParameterAccessor: IAccessor;
    FClientCP: Word;
    procedure CalcParamSetsAndBufferSize;
    procedure SetPWideChar(Index: Word; Value: PWideChar; Len: Cardinal);
    procedure SetPAnsiChar(Index: Word; Value: PAnsiChar; Len: Cardinal);
    procedure BindBatchDMLArrays;
    procedure BindRaw(Index: Integer; const Value: RawByteString; CP: Word);
    procedure Dyn_W_Convert(Index, Len: Integer; var Arr: PZArray);
    procedure SetOleCommandProperties;
    procedure InitVaryBind(Index: Integer; Len: Cardinal; _Type: DBTYPE);
    procedure InitFixedBind(Index: Integer; Size: Cardinal; _Type: DBTYPE);
    procedure InitDateBind(Index: Integer; SQLType: TZSQLType);
    procedure InitLongBind(Index: Integer; _Type: DBTYPE);
    procedure InternalBindSInt(Index: Integer; SQLType: TZSQLType; Value: NativeInt);
    procedure InternalBindUInt(Index: Integer; SQLType: TZSQLType; Value: NativeUInt);
    procedure InternalBindDbl(Index: Integer; SQLType: TZSQLType; const Value: Double);
    procedure SetBindOffsets;
  protected
    /// <summary>Prepares eventual structures for binding input parameters.</summary>
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure CheckParameterIndex(var Value: Integer); override;
    procedure SetParamCount(NewParamCount: Integer); override;
    procedure SetBindCapacity(Capacity: Integer); override;
    function CreateOleDBConvertErrror(Index: Integer; WType: Word; SQLType: TZSQLType): EZOleDBConvertError;
    procedure RaiseExceeded(Index: Integer);
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZSQLStringWriter; Var Result: SQLString); override;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
    function CreateOutParamResultSet: IZResultSet; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);

    procedure Prepare; override;
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  public //setters
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual

    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(Index: Integer; {%H-}SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(Index: Integer; Value: Boolean);
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(Index: Integer; Value: Byte);
    /// <summary>Sets the designated parameter to a <c>ShortInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetShort(Index: Integer; Value: ShortInt);
    /// <summary>Sets the designated parameter to a <c>Word</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetWord(Index: Integer; Value: Word);
    /// <summary>Sets the designated parameter to a <c>SmallInt</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetSmall(Index: Integer; Value: SmallInt);
    /// <summary>Sets the designated parameter to a <c>Cardinal</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUInt(Index: Integer; Value: Cardinal);
    /// <summary>Sets the designated parameter to a <c>Integer</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetInt(Index: Integer; Value: Integer);
    /// <summary>Sets the designated parameter to a <c>UInt64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetULong(Index: Integer; const Value: UInt64);
    /// <summary>Sets the designated parameter to a <c>Int64</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetLong(Index: Integer; const Value: Int64);
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
    /// <summary>Sets the designated parameter to a <c>TZCharRec</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetCharRec(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZCharRec); reintroduce;
    /// <summary>Sets the designated parameter to a <c>String</c> value.
    ///  This method equals to SetUnicodeString on Unicode-Compilers. For
    ///  Raw-String compilers the encoding is defined by W2A2WEncodingSource of
    ///  the ConnectionSettings record. The driver will convert the string to
    ///  the Client-Characterset.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetString(Index: Integer; const Value: String); reintroduce;
    {$IFNDEF NO_UTF8STRING}
    /// <summary>Sets the designated parameter to a <c>RawByteString</c> value.
    ///  The string must be UTF8 encoded. The driver will convert the value
    ///  if the driver uses an different encoding.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUTF8String(Index: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Sets the designated parameter to a <c>AnsiString</c> value.
    ///  The string must be GET_ACP encoded. The driver will convert the value
    ///  if the driver uses an different encoding.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetAnsiString(Index: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(Index: Integer; const Value: RawByteString); reintroduce;
    /// <summary>Sets the designated parameter to a <c>UnicodeString</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetUnicodeString(Index: Integer; const Value: UnicodeString); reintroduce;
    /// <summary>Sets the designated parameter to a <c>TZDate</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetDate(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>Time(TDateTime)</c> value.
    ///  This method is obsolate and left for compatibility. The method always
    ///  decodes the value and calls the <c>SetTime(Index: Integer; Value: TZtime)</c>
    ///  overload.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTime(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>TDateTime</c> value.
    ///  This method is obsolate and left for compatibility. The method always
    ///  decodes the value and calls the
    ///  <c>SetTimestamp(Index: Integer; Value: TZTimestamp)</c>overload.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetTimestamp(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>byte array</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBytes(Index: Integer; const Value: TBytes); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>ByteArray reference</c> value.
    ///  The references need to be valid until the statement is executed.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value reference.</param>
    /// <param>"Len" the Length of the bytes buffer.</param>
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    /// <summary>Sets the designated parameter to a <c>TGUID</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetGUID(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TGUID); reintroduce;
    /// <summary>Sets the designated parameter to the given blob wrapper object.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" defines the lob constent. Valid values are:
    ///  stAsciiStream(raw encoded text), stUnicodeStream(UTF16 encoded text)
    ///  and stBinaryStream(binary data), stJSON, stXML</param>
    /// <param>"Value" the parameter blob wrapper object to be set.</param>
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
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
    procedure SetDataArray(ParameterIndex: Integer; const Value;
      const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
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
    procedure RegisterParameter(Index: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
  end;

  TZOleDBStatement = class(TZAbstractOleDBStatement, IZStatement)
  public
    constructor Create(const Connection: IZConnection; const Info: TStrings);
  end;

  TZOleDBCallableStatementMSSQL = class(TZAbstractCallableStatement_W,
    IZCallableStatement)
  protected
    /// <summary>creates an exceution Statement. Which wraps the call.</summary>
    /// <param>"StoredProcName" the name of the stored procedure or function to
    ///  be called.</param>
    /// <returns>a TZAbstractPreparedStatement object.</returns>
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit

uses
  Variants, Math,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF}, TypInfo,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} DateUtils,
  ZDbcOleDBResultSet, ZEncoding, ZDbcOleDBMetadata,
  ZFastCode, ZDbcMetadata, ZMessages, ZDbcResultSet,
  ZDbcCachedResultSet, ZDbcGenericResolver;

var DefaultPreparableTokens: TPreparablePrefixTokens;
const
  LogExecType: array[Boolean] of TZLoggingCategory = (lcExecPrepStmt, lcExecute);

{ TZAbstractOleDBStatement }

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZAbstractOleDBStatement.Cancel;
var Status: HResult;
begin
  if FCommand <> nil then begin
    Status := FCommand.Cancel;
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
        {$IFDEF DEBUG}'ICommand.Cancel'{$ELSE}''{$ENDIF},
        IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
  end else
    inherited Cancel;
end;

procedure TZAbstractOleDBStatement.ClearCallResultCache;
var I: Integer;
  RS: IZResultSet;
begin
  for I := 0 to FCallResultCache.Count -1 do
    if Supports(FCallResultCache[i], IZResultSet, RS) then
      RS.Close;
  FreeAndNil(FCallResultCache);
end;

constructor TZAbstractOleDBStatement.Create(const Connection: IZConnection;
  const SQL: string; const Info: TStrings);
var DatabaseInfo: IZDataBaseInfo;
begin
  inherited Create(Connection, SQL, Info);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_InternalBufSize, ''), 131072); //by default 128KB
  fStmtTimeOut := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_StatementTimeOut, ''), 60); //execution timeout in seconds by default 1 min
  DatabaseInfo := Connection.GetMetadata.GetDatabaseInfo;
  FSupportsMultipleResultSets := DatabaseInfo.SupportsMultipleResultSets;
  FOutParameterAvailibility := (DatabaseInfo as IZOleDBDatabaseInfo).GetOutParameterAvailability;
  FOleDBConnection := Connection as IZOleDBConnection;
  FByteBuffer := FOleDBConnection.GetByteBufferAddress;
  DatabaseInfo := nil;
end;

function TZAbstractOleDBStatement.CreateOutParamResultSet: IZResultSet;
begin
  Result := nil;
end;

function TZAbstractOleDBStatement.CreateResultSet: IZResultSet;
var
  CachedResolver: IZCachedResolver;
  NativeResultSet: TZOleDBResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  Result := nil;
  if Assigned(FRowSet) then begin
    NativeResultSet := TZOleDBResultSet.Create(Self, SQL, @FRowSet, FZBufferSize);
    if (ResultSetConcurrency = rcUpdatable) or (ResultSetType <> rtForwardOnly) then begin
      if (Connection.GetServerProvider = spMSSQL) and (Self.GetResultSetConcurrency = rcUpdatable)
      then CachedResolver := TZOleDBMSSQLCachedResolver.Create(Self, NativeResultSet.GetMetaData)
      else CachedResolver := TZGenerateSQLCachedResolver.Create(Self, NativeResultSet.GetMetaData);
      CachedResultSet := TZOleDBCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(ResultSetConcurrency);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
  end;
  FOpenResultSet := Pointer(Result);
end;

destructor TZAbstractOleDBStatement.Destroy;
begin
  inherited Destroy;
  FCommand := nil;
end;

function TZAbstractOleDBStatement.GetFirstResultSet: IZResultSet;
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

function TZAbstractOleDBStatement.GetInternalBufferSize: Integer;
begin
  Result := FZBufferSize;
end;

procedure TZAbstractOleDBStatement.AfterClose;
begin
  FCommand := nil;
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractOleDBStatement.Prepare;
var Status: HResult;
begin
  if FCommand = nil then begin
    FCommand := Self.FOleDBConnection.CreateCommand;
    Status := FCommand.SetCommandText(DBGUID_DEFAULT, Pointer(WSQL));
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
        {$IFDEF DEBUG}'ICommand.SetCommandText'{$ELSE}''{$ENDIF},
        IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
  end;
  if FCallResultCache <> nil then
    ClearCallResultCache;
  inherited Prepare;
end;

procedure TZAbstractOleDBStatement.PrepareOpenedResultSetsForReusing;
  procedure SetMoreResInd;
  begin
    if (fMoreResultsIndicator = mriUnknown) and Assigned(FMultipleResults) then begin
      if GetMoreResults and (Assigned(LastResultSet) or (FRowsAffected <> -1)) then
        fMoreResultsIndicator := mriHasMoreResults
      else
        fMoreResultsIndicator := mriHasNoMoreResults;
    end;
    if Assigned(LastResultSet) then begin
      LastResultSet.Close;
      LastResultSet := nil;
    end;
  end;
begin
  if Assigned(FOpenResultSet) then
    if fMoreResultsIndicator <> mriHasNoMoreResults then begin
      if (Pointer(LastResultSet) = FOpenResultSet) then begin
        LastResultSet.Close;
        LastResultSet := nil;
      end else begin
        IZResultSet(FOpenResultSet).Close;
        FOpenResultSet := nil;
      end;
      SetMoreResInd;
    end else
      IZResultSet(FOpenResultSet).ResetCursor;
  if Assigned(LastResultSet) then begin
    if (fMoreResultsIndicator <> mriHasNoMoreResults) then begin
      LastResultSet.Close;
      LastResultSet := nil;
      SetMoreResInd;
    end else
      LastResultSet.ResetCursor;
  end;
end;

procedure TZAbstractOleDBStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  inherited ReleaseImmediat(Sender, AError);
  FMultipleResults := nil;
  FCommand := nil;
  ClearCallResultCache;
  SetLength(FDBBINDSTATUSArray, 0);
end;

function TZAbstractOleDBStatement.ExecuteQueryPrepared: IZResultSet;
var Status: HResult;
begin
  PrepareOpenedResultSetsForReusing;
  Prepare;
  BindInParameters;
  RestartTimer;
  if Assigned(FOpenResultSet) then begin
    Result := IZResultSet(FOpenResultSet);
    Status := FCommand.Execute(nil, IID_IRowset, FDBParams, @FRowsAffected, @FRowSet);
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
        SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
  end else begin
    if FSupportsMultipleResultSets then begin
      Status := FCommand.Execute(nil, IID_IMultipleResults, FDBParams,
        @FRowsAffected,@FMultipleResults);
      if Failed(Status) then
        FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
          SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), fDBBINDSTATUSArray);
      if Assigned(FMultipleResults) then begin
        //don't use DBRESULTFLAG_ROWSET see: https://zeoslib.sourceforge.io/viewtopic.php?f=50&t=129214
        Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_DEFAULT),
          IID_IRowset, @FRowsAffected, @FRowSet);
        if Failed(Status) then
          FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
            SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
      end;
    end else begin
      Status := FCommand.Execute(nil, IID_IRowset, FDBParams,@FRowsAffected,@FRowSet);
      if Failed(Status) then
        FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
          SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), fDBBINDSTATUSArray);
    end;
    if DriverManager.HasLoggingListener then
       DriverManager.LogMessage(LogExecType[fDEFERPREPARE],Self);
    if BindList.HasOutOrInOutOrResultParam then begin
      FetchCallResults(FRowSet);
      Result := GetFirstResultSet;
    end else if FRowSet <> nil
      then Result := CreateResultSet
      else Result := nil;
    LastUpdateCount := FRowsAffected;
    if not Assigned(Result) then
      while GetMoreResults do
        if (LastResultSet <> nil) then begin
          Result := LastResultSet;
          FLastResultSet := nil;
          Break;
        end;
  end;
end;

function TZAbstractOleDBStatement.ExecuteUpdatePrepared: Integer;
var Status: HResult;
begin
  Prepare;
  BindInParameters;
  RestartTimer;
  FRowsAffected := DB_COUNTUNAVAILABLE; //init
  if FSupportsMultipleResultSets then begin
    Status := FCommand.Execute(nil, IID_IMultipleResults, FDBParams,
      @FRowsAffected,@FMultipleResults);
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
        SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), fDBBINDSTATUSArray);
    if Assigned(FMultipleResults) then begin
      Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_DEFAULT),
        DB_NULLGUID, @FRowsAffected, nil);
      if Failed(Status) then
        FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
          {$IFDEF DEBUG}'IMultipleResults.GetResult'{$ELSE}''{$ENDIF},
          IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
    end;
  end else begin
    Status := FCommand.Execute(nil, DB_NULLGUID,FDBParams,@FRowsAffected,nil);
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
        SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), fDBBINDSTATUSArray);
  end;
  if DriverManager.HasLoggingListener then
     DriverManager.LogMessage(LogExecType[fDEFERPREPARE],Self);
  if BindList.HasOutOrInOutOrResultParam then
    FOutParamResultSet := CreateOutParamResultSet;
  LastUpdateCount := FRowsAffected;
  Result := LastUpdateCount;
end;

function TZAbstractOleDBStatement.FetchCallResults(var RowSet: IRowSet): Boolean;
var CallResultCache: TZCollection;
begin
  Result := RowSet <> nil;
  if (FOutParameterAvailibility = DBPROPVAL_OA_ATEXECUTE) then
    FOutParamResultSet := CreateOutParamResultSet;
  CallResultCache := TZCollection.Create;
  if RowSet <> nil then begin
    FLastResultSet := CreateResultSet;
    CallResultCache.Add(Connection.GetMetadata.CloneCachedResultSet(FlastResultSet));
    FLastResultSet.Close;
    RowSet := nil;
  end else CallResultCache.Add(TZAnyValue.CreateWithInteger(LastUpdateCount));
  while GetMoreresults do
    if LastResultSet <> nil then begin
      CallResultCache.Add(Connection.GetMetadata.CloneCachedResultSet(FLastResultSet));
      FLastResultSet.Close;
      FLastResultSet := nil;
      FOpenResultSet := nil;
    end else
      CallResultCache.Add(TZAnyValue.CreateWithInteger(LastUpdateCount));
  if (FOutParameterAvailibility = DBPROPVAL_OA_ATROWRELEASE) then
    FOutParamResultSet := CreateOutParamResultSet;
  FCallResultCache := CallResultCache;
end;

function TZAbstractOleDBStatement.ExecutePrepared: Boolean;
var Status: HResult;
begin
  PrepareOpenedResultSetsForReusing;
  LastUpdateCount := -1;
  Prepare;
  RestartTimer;
  FRowsAffected := DB_COUNTUNAVAILABLE;
  if FSupportsMultipleResultSets then begin
    Status := FCommand.Execute(nil, IID_IMultipleResults,
      FDBParams,@FRowsAffected,@FMultipleResults);
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
        SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), fDBBINDSTATUSArray);
    if Assigned(FMultipleResults) then begin
      Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_DEFAULT),
        IID_IRowset, @FRowsAffected, @FRowSet);
      if Failed(Status) then
        FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
          {$IFDEF DEBUG}'IMultipleResults.GetResult'{$ELSE}''{$ENDIF},
          IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
    end
  end else begin
    Status := FCommand.Execute(nil, IID_IRowset, FDBParams,@FRowsAffected,@FRowSet);
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, LogExecType[fDEFERPREPARE],
        SQL, IImmediatelyReleasable(FWeakImmediatRelPtr), fDBBINDSTATUSArray);
  end;
  if DriverManager.HasLoggingListener then
     DriverManager.LogMessage(LogExecType[fDEFERPREPARE],Self);
  if BindList.HasOutOrInOutOrResultParam then
    if FetchCallResults(FRowSet)
    then LastResultSet := GetFirstResultSet
    else LastResultSet := nil
  else if FRowSet <> nil
    then LastResultSet := CreateResultSet
    else LastResultSet := nil;
  LastUpdateCount := FRowsAffected;
  Result := Assigned(LastResultSet);
end;

function TZAbstractOleDBStatement.GetMoreResults: Boolean;
var Status: HResult;
    RS: IZResultSet;
    AnyValue: IZAnyValue;
begin
  if (FOpenResultSet <> nil) and (FOpenResultSet <> Pointer(FOutParamResultSet))
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
        LastResultSet := nil;
      end;
      FCallResultCache.Delete(0);
    end;
  end else begin
    Result := False;
    LastResultSet := nil;
    LastUpdateCount := -1;
    if Assigned(FMultipleResults) then begin
      Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
        IID_IRowset, @FRowsAffected, @FRowSet);
      Result := Status = S_OK;
      if Result then begin
        if Assigned(FRowSet)
        then LastResultSet := CreateResultSet
        else LastUpdateCount := FRowsAffected;
      end {else if Status <> DB_S_NORESULT then
        CheckError(Status, lcOther)};
    end;
  end;
end;

function TZAbstractOleDBStatement.GetMoreResultsIndicator: TZMoreResultsIndicator;
begin
  Result := fMoreResultsIndicator;
end;

procedure TZAbstractOleDBStatement.Unprepare;
var
  Status: HRESULT;
  FRowSet: IRowSet;
  CommandPrepare: ICommandPrepare;
begin
  if Prepared then try
    inherited Unprepare;
    FRowSet := nil;
    if FCallResultCache <> nil then
      ClearCallResultCache;
    if FMultipleResults <> nil then begin
      repeat
        FRowSet := nil;
        Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_DEFAULT),
          IID_IRowset, @FRowsAffected, @FRowSet);
      until Failed(Status) or (Status = DB_S_NORESULT);
      FMultipleResults := nil;
    end;
    if (FCommand.QueryInterface(ICommandPrepare, CommandPrepare) = S_OK) and
       not fDEFERPREPARE then begin
      try
        Status := CommandPrepare.UnPrepare;
        if Failed(Status) then
          FOleDBConnection.HandleErrorOrWarning(Status, lcUnprepStmt, SQL,
            IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
        if DriverManager.HasLoggingListener then
          LogPrepStmtMessage(lcUnprepStmt);
      finally
        CommandPrepare := nil;
      end;
    end;
  finally
    FCommand := nil;
    FMultipleResults := nil;
  end;
end;

procedure TZAbstractOleDBStatement.SetMoreResultsIndicator(
  Value: TZMoreResultsIndicator);
begin
  fMoreResultsIndicator := Value;
end;

{ TZOleDBPreparedStatement }

//const OleDbNotNullTable: array[Boolean] of DBSTATUS = (DBSTATUS_S_ISNULL, DBSTATUS_S_OK);
{$IFDEF FPC}
  {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 5057 off : Local variable "BCD" does not seem to be initialized}
{$ENDIF} // uses pointer maths
procedure TZOleDBPreparedStatement.BindBatchDMLArrays;
var
  ZData, Data, P: Pointer;
  PLen: PDBLENGTH;
  PD: PZDate absolute PLen;
  PT: PZTime absolute PLen;
  PTS: PZTimeStamp absolute PLen;
  P_BCD: PBCD absolute PLen;
  MaxL, CPL: DBLENGTH;
  ZArray: PZArray;
  I, j: Integer;
  BuffOffSet: NativeUInt;
  SQLType: TZSQLType;
  DateTimeTemp: TDateTime;
  W1, WType: Word;
  Native: Boolean;
  BCD: TBCD;
  TS: TZTimeStamp absolute BCD;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
  DBDate: PDBDate absolute Data;
  DBTime: PDBTime absolute Data;
  DBTIME2: PDBTIME2 absolute Data;
  DBTimeStamp: PDBTimeStamp absolute Data;
  DBTIMESTAMPOFFSET: PDBTIMESTAMPOFFSET absolute Data;
  DB_NUMERIC: PDB_NUMERIC absolute Data;

  procedure Bind_DBTYPE_BYTES(ByRef: Boolean);
  var TempLob: IZBlob;
    P: Pointer;
  begin
    case SQLType of
      stBinaryStream: begin
                TempLob := TInterfaceDynArray(ZData)[J] as IZBLob;
                P := TempLob.GetBuffer(FRawTemp, PLen^);
                TempLob := nil;
              end;
      stBytes: begin
                PLen^ := Length(TBytesDynArray(ZData)[J]);
                P := Pointer(TBytesDynArray(ZData)[J]);
              end;
      stGUID: begin
                PLen^ := SizeOf(TGUID);
                P := @TGUIDDynArray(ZData)[J].D1;
              end;
      else
        raise EZSQLException.Create('Unsupported Byte-Array Variant');
    end;
    if ByRef then PPointer(Data)^:= P
    else if (PLen^ > 0) and (PLen^ <= MaxL)
      then Move(P^, Pointer(Data)^, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(MaxL, PLen^))
      else RaiseExceeded(I);
  end;
  procedure Bind_Long_DBTYPE_WSTR_BY_REF;
  var TempLob: IZBlob;
    Len: NativeUInt;
  begin
    TempLob := TInterfaceDynArray(ZData)[J] as IZBLob;
    TempLob.SetCodePageTo(zCP_UTF16);
    PPointer(Data)^:= TempLob.GetPWideChar(fUniTemp, Len);
    PLen^ := Len shl 1;
  end;
label W_Len, WStr;
begin
  {.$R-}
  MaxL := 0; CPL := 0; W1 := 0; Native := False;//satisfy the compiler
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for i := 0 to BindList.Count -1 do begin
    if not (BindList[I].BindType in [zbtRefArray, zbtArray]) then
      Continue;
    ZArray := BindList[I].Value;
    ZData := ZArray.VArray;
    SQLType := TZSQLType(ZArray.VArrayType);
    BuffOffSet := 0;
    WType := fDBBindingArray[i].wType;
    if (Wtype = DBTYPE_WSTR) then begin
      MaxL := fDBBindingArray[i].cbMaxLen -2; //omit trailing zero
      CPL := MaxL shr 1;  //need codepoint len
      if (SQLType in [stString, stUnicodeString]) then
      case ZArray.VArrayVariantType of
        {$IFNDEF UNICODE}
        vtString: W1 := GetW2A2WConversionCodePage(ConSettings);
        {$ENDIF}
        vtAnsiString: W1 := ZOSCodePage;
        vtUTF8String: W1 := zCP_UTF8;
        vtRawByteString: W1 := FClientCP;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
    end else if (wType = DBTYPE_BYTES) or (wType = DBTYPE_STR)
    then MaxL := fDBBindingArray[i].cbMaxLen - Byte(Ord(wType = DBTYPE_STR))
    else Native := (SQLType2OleDBTypeEnum[SQLType] = wType) and (ZArray.VArrayVariantType = vtNull);
    for J := 0 to fDBParams.cParamSets-1 do begin
      if IsNullFromArray(ZArray, J) {or (wType = DBTYPE_NULL)} then begin
        PDBSTATUS(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL;
        Inc(BuffOffSet, fRowSize);
        Continue;
      end else
        PDBSTATUS(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_OK;
      Data := Pointer(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obValue + BuffOffSet));
      //note PLen is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
      PLen := PDBLENGTH(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obLength + BuffOffSet));
      case wType of
        DBTYPE_I1:    if Native
                      then PShortInt(Data)^   := TShortIntDynArray(ZData)[j]
                      else PShortInt(Data)^   := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI1:   if Native
                      then PByte(Data)^       := TByteDynArray(ZData)[j]
                      else PByte(Data)^       := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I2:    if Native
                      then PSmallInt(Data)^   := TSmallIntDynArray(ZData)[j]
                      else PSmallInt(Data)^   := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI2:   if Native
                      then PWord(Data)^       := TWordDynArray(ZData)[j]
                      else PWord(Data)^       := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I4:    if Native
                      then PInteger(Data)^    := TIntegerDynArray(ZData)[j]
                      else PInteger(Data)^    := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI4:   if Native
                      then PCardinal(Data)^   := TCardinalDynArray(ZData)[j]
                      else PCardinal(Data)^   := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I8:    if Native
                      then PInt64(Data)^      := TInt64DynArray(ZData)[j]
                      else PInt64(Data)^      := ArrayValueToInt64(ZArray, j);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        DBTYPE_UI8:   if Native
                      then PUInt64(Data)^     := TUInt64DynArray(ZData)[j]
                      else PUInt(Data)^       := ArrayValueToUInt64(ZArray, j);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
        DBTYPE_R4:    if Native
                      then PSingle(Data)^     := TSingleDynArray(ZData)[j]
                      else PSingle(Data)^     := ArrayValueToDouble(ZArray, j);
        DBTYPE_R8:    if Native
                      then PDouble(Data)^     := TDoubleDynArray(ZData)[j]
                      else PDouble(Data)^     := ArrayValueToDouble(ZArray, j);
        DBTYPE_CY:    if Native
                      then PCurrency(Data)^   := TCurrencyDynArray(ZData)[j]
                      else PCurrency(Data)^   := ArrayValueToCurrency(ZArray, j);
        DBType_BOOL:  if Native
                      then PWordBool(Data)^   := TBooleanDynArray(ZData)[j]
                      else PWordBool(Data)^   := ArrayValueToBoolean(ZArray, j);
        DBTYPE_NUMERIC: begin
                        if Native
                        then P_BCD := @TBCDDynArray(ZData)[j]
                        else begin
                          ArrayValueToBCD(ZArray, J, BCD);
                          P_BCD := @BCD;
                        end;
                        BCD2SQLNumeric(P_BCD^, DB_NUMERIC)
                      end;
        DBTYPE_DATE:  PDateTime(Data)^ := ArrayValueToDateTime(ZArray, j, ConSettings.WriteFormatSettings);
        DBTYPE_DBDATE: begin
                        if ZArray.VArrayVariantType = vtDate then
                          PD := @TZDateDynArray(ZData)[J]
                        else begin
                          PD := @D;
                          if (ZArray.VArrayVariantType in [vtNull, vtDateTime])
                          then DecodeDateTimeToDate(TDateTimeDynArray(ZData)[J], D)
                          else begin
                            DateTimeTemp := ArrayValueToDate(ZArray, J, ConSettings^.WriteFormatSettings);
                            DecodeDateTimeToDate(DateTimeTemp, D);
                          end;
                        end;
                        DBDate^.year := PD^.Year;
                        if PD^.IsNegative then
                          DBDate^.year := -DBDate^.year;
                        DBDate^.month := PD^.Month;
                        DBDate^.day := PD^.Day;
                      end;

        DBTYPE_DBTIME, DBTYPE_DBTIME2: begin
                        if ZArray.VArrayVariantType = vtTime then
                          PT := @TZTimeDynArray(ZData)[J]
                        else begin
                          PT := @T;
                          if (ZArray.VArrayVariantType in [vtNull, vtDateTime])
                          then DecodeDateTimeToTime(TDateTimeDynArray(ZData)[J], T)
                          else begin
                            DateTimeTemp := ArrayValueToTime(ZArray, J, ConSettings^.WriteFormatSettings);
                            DecodeDateTimeToTime(DateTimeTemp, T);
                          end;
                        end;
                        if wType = DBTYPE_DBTIME then begin
                          DBTime.hour := PT^.Hour;
                          DBTime.minute := PT^.Minute;
                          DBTime.second := PT^.Second;
                        end else begin
                          DBTIME2.hour := PT^.Hour;
                          DBTIME2.minute := PT^.Minute;
                          DBTIME2.second := PT^.Second;
                          DBTIME2.fraction := PT^.Fractions;
                        end;
                      end;
        DBTYPE_DBTIMESTAMP, DBTYPE_DBTIMESTAMPOFFSET: begin
                        if ZArray.VArrayVariantType = vtTimeStamp then
                          PTS := @TZTimeStampDynArray(ZData)[J]
                        else begin
                          PTS := @TS;
                          if (ZArray.VArrayVariantType in [vtNull, vtDateTime])
                          then DecodeDateTimeToTimeStamp(TDateTimeDynArray(ZData)[J], TS)
                          else begin
                            if (ZArray.VArrayVariantType = vtDate) then
                              TimeStampFromDate(TZDateDynArray(ZData)[J], TS)
                            else if (ZArray.VArrayVariantType = vtTime) then
                              TimeStampFromTime(TZTimeDynArray(ZData)[J], TS)
                            else begin
                              DateTimeTemp := ArrayValueToDatetime(ZArray, J, ConSettings^.WriteFormatSettings);
                              DecodeDateTimeToTimeStamp(DateTimeTemp, TS);
                            end;
                          end;
                        end;
                        if wType = DBTYPE_DBTIMESTAMP then begin
                          DBTimeStamp.year := PTS^.Year;
                          if PTS^.IsNegative then
                            DBTimeStamp^.year := -DBTimeStamp^.year;
                          DBTimeStamp^.month := PTS^.Month;
                          DBTimeStamp^.day := PTS^.Day;
                          DBTimeStamp.hour := PTS^.Hour;
                          DBTimeStamp.minute := PTS^.Minute;
                          DBTimeStamp.second := PTS^.Second;
                          DBTimeStamp.fraction := PTS^.Fractions;
                        end else begin
                          DBTIMESTAMPOFFSET.year := PTS^.Year;
                          if PTS^.IsNegative then
                            DBTIMESTAMPOFFSET^.year := -DBTIMESTAMPOFFSET^.year;
                          DBTIMESTAMPOFFSET^.month := PTS^.Month;
                          DBTIMESTAMPOFFSET^.day := PTS^.Day;
                          DBTIMESTAMPOFFSET.hour := PTS^.Hour;
                          DBTIMESTAMPOFFSET.minute := PTS^.Minute;
                          DBTIMESTAMPOFFSET.second := PTS^.Second;
                          DBTIMESTAMPOFFSET.fraction := PTS^.Fractions;
                          DBTIMESTAMPOFFSET.timezone_hour := PTS^.TimeZoneHour;
                          DBTIMESTAMPOFFSET.timezone_minute := PTS^.TimeZoneMinute;
                        end;
                      end;
        { next types are automatically prepared on binding the arrays }
        DBTYPE_GUID: ArrayValueToGUID(ZArray, j, PGUID(Data));
        DBTYPE_GUID or DBTYPE_BYREF: ArrayValueToGUID(ZArray, j, PGUID(PPointer(Data)^));
        DBTYPE_BYTES: Bind_DBTYPE_BYTES(False);
        DBTYPE_BYTES or DBTYPE_BYREF: Bind_DBTYPE_BYTES(True);
        DBTYPE_WSTR, DBTYPE_WSTR or DBTYPE_BYREF: begin
            case SQLType of
             { stBoolean:      FUniTemp := BoolToUnicodeEx(TBooleanDynArray(ZData)[J]);
              stByte:         FUniTemp := IntToUnicode(TByteDynArray(ZData)[J]);
              stShort:        FUniTemp := IntToUnicode(TShortIntDynArray(ZData)[J]);
              stWord:         FUniTemp := IntToUnicode(TWordDynArray(ZData)[J]);
              stSmall:        FUniTemp := IntToUnicode(TSmallIntDynArray(ZData)[J]);
              stLongWord:     FUniTemp := IntToUnicode(TCardinalDynArray(ZData)[J]);
              stInteger:      FUniTemp := IntToUnicode(TIntegerDynArray(ZData)[J]);
              stULong:        FUniTemp := IntToUnicode(TUInt64DynArray(ZData)[J]);
              stLong:         FUniTemp := IntToUnicode(TInt64DynArray(ZData)[J]);
              stFloat:        FUniTemp := FloatToUnicode(TSingleDynArray(ZData)[J]);
              stDouble:       FUniTemp := FloatToUnicode(TDoubleDynArray(ZData)[J]);
              stCurrency:     FUniTemp := FloatToUnicode(TCurrencyDynArray(ZData)[J]);
              stBigDecimal:   FUniTemp := FloatToUnicode(TExtendedDynArray(ZData)[J]);
              stTime:         FUniTemp := DateTimeToUnicodeSQLTime(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);}
              stDate:         if (WType = DBTYPE_WSTR) and (ConSettings.WriteFormatSettings.DateFormatLen <= MaxL)
                              then DateTimeToUnicodeSQLDate(TDateTimeDynArray(ZData)[J], PWideChar(Data), ConSettings.WriteFormatSettings, False)
                              else RaiseExceeded(I);
              (*stTimeStamp:    FUniTemp := DateTimeToUnicodeSQLTimeStamp(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);}*)
              stString, stUnicodeString: begin
                case ZArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString, {$ENDIF}
                  vtAnsiString,vtUTF8String,vtRawByteString:
                      if wType = DBTYPE_WSTR then begin
                        P := Pointer(TRawByteStringDynArray(ZData)[J]);
                        PLen^ := PRaw2PUnicode(PAnsiChar(P), PWideChar(Data), W1, LengthInt(Length(TRawByteStringDynArray(ZData)[J])), LengthInt(CPL)) shl 1;
                        goto W_Len;
                      end else begin
                        Dyn_W_Convert(I, Length(TRawByteStringDynArray(ZData)), ZArray);
                        ZData := ZArray.VArray;
                        goto WStr;
                      end;
                  {$IFDEF UNICODE}vtString,{$ENDIF} vtUnicodeString: begin
WStr:                 PLen^ := Length(TUnicodeStringDynArray(ZData)[J]) shl 1;
                      if PLen^ > 0 then
                        if wType = DBTYPE_WSTR then begin
                          Move(Pointer(TUnicodeStringDynArray(ZData)[J])^, PWideChar(Data)^, ({$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(PLen^, MaxL)+2));
                          goto W_Len
                        end else
                          PPointer(Data)^ := Pointer(TUnicodeStringDynArray(ZData)[J])
                      else if wType = DBTYPE_WSTR
                        then PWord(Data)^ := 0
                        else PPointer(Data)^ := PEmptyUnicodeString;
                    end;
                  vtCharRec: begin
                      if TZCharRecDynArray(ZData)[J].CP = zCP_UTF16 then begin
                        PLen^ := TZCharRecDynArray(ZData)[J].Len shl 1;
                        if wType = DBTYPE_WSTR
                        then Move(PWideChar(TZCharRecDynArray(ZData)[J].P)^, PWideChar(Data)^, ({$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(PLen^, MaxL)+2))
                        else PPointer(Data)^ := TZCharRecDynArray(ZData)[J].P;
                      end else begin
                        if wType = DBTYPE_WSTR
                        then PLen^ := PRaw2PUnicode(PAnsiChar(TZCharRecDynArray(ZData)[J].P), PWideChar(Data), TZCharRecDynArray(ZData)[J].CP, LengthInt(TZCharRecDynArray(ZData)[J].Len), LengthInt(MaxL))
                        else begin
                          Dyn_W_Convert(I, Length(TZCharRecDynArray(ZData)), ZArray);
                          ZData := ZArray.VArray;
                          goto WStr;
                        end;
                      end;
W_Len:                if PLen^ > MaxL then
                        RaiseExceeded(I);
                    end;
                  else
                    raise EZSQLException.Create('Unsupported String Variant');
                end;
              end;
              stAsciiStream, stUnicodeStream: Bind_Long_DBTYPE_WSTR_BY_REF;
              else
                raise EZSQLException.Create('Unsupported AnsiString-Array Variant');
            end;
          end;
        else raise CreateOleDBConvertErrror(I, WType, SQLType);
        //DBTYPE_UDT: ;
        //DBTYPE_HCHAPTER:;
        //DBTYPE_PROPVARIANT:;
        //DBTYPE_VARNUMERIC:;
      end;
      Inc(BuffOffSet, fRowSize);
    end;  //*)
  end;
  {$IF defined (RangeCheckEnabled)}{$R+}{$IFEND}
end;
{$IFDEF FPC} {$POP} {$ENDIF} // uses pointer maths

procedure TZOleDBPreparedStatement.BindInParameters;
begin
  if BindList.Count = 0 then
    Exit;
  if not fBindImmediat or (BatchDMLArrayCount > 0) then
    try
      fBindImmediat := True;
      if fBindAgain or (FDBParams.hAccessor = 0) then
        PrepareInParameters;
      if BatchDMLArrayCount = 0
      then BindList.BindValuesToStatement(Self)
      else BindBatchDMLArrays;
      fBindAgain := False;
    finally
      fBindImmediat := not fDEFERPREPARE;
    end;
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$ENDIF} // uses pointer maths
procedure TZOleDBPreparedStatement.BindRaw(Index: Integer;
  const Value: RawByteString; CP: Word);
var L: Cardinal;
  PLen: PDBLENGTH;
  Bind: PDBBINDING;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat and (FDBBindingArray[Index].wType = DBTYPE_WSTR) then begin
    Bind := @FDBBindingArray[Index];
    PLen := PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength);
    L := Bind.cbMaxLen-2;
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    PLen^ := PRaw2PUnicode(Pointer(Value), Pointer(NativeUInt(fDBParams.pData)+Bind.obValue), CP, LengthInt(Length(Value)), LengthInt(L shr 1)) shl 1;
    if PLen^ > L then
      RaiseExceeded(Index)
  end else begin
    FUniTemp := PRawToUnicode(Pointer(Value), Length(Value), CP);
    BindList.Put(Index, stUnicodeString, FUniTemp);
    L := Length(FUniTemp);
    if fBindImmediat then
      if Value <> ''
      then SetPWideChar(Index, Pointer(FUniTemp), L)
      else SetPWideChar(Index, PEmptyUnicodeString, 0)
    else InitVaryBind(Index, (L+1) shl 1, DBTYPE_WSTR);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF} // uses pointer maths

procedure TZOleDBPreparedStatement.CalcParamSetsAndBufferSize;
var FAccessorRefCount: DBREFCOUNT;
    Status: HResult;
begin
  FDBParams.cParamSets := Max(1, BatchDMLArrayCount); //indicate rows for single executions
  if (FDBParams.hAccessor <> 0) and fBindAgain then begin
    FParameterAccessor.ReleaseAccessor(FDBParams.hAccessor, @FAccessorRefCount);
    FDBParams.hAccessor := 0;
  end;
  SetLength(FParamsBuffer, FDBParams.cParamSets * FRowSize);
  FDBParams.pData := Pointer(FParamsBuffer); //set entry pointer
  if (FDBParams.hAccessor = 0) then begin
    Status := FParameterAccessor.CreateAccessor(DBACCESSOR_PARAMETERDATA,
      FDBUPARAMS, Pointer(FDBBindingArray), FRowSize, @FDBParams.hAccessor,
      Pointer(FDBBINDSTATUSArray));
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
        {$IFDEF DEBUG}'IParameterAccessor.CreateAccessor'{$ELSE}''{$ENDIF},
        IImmediatelyReleasable(FWeakImmediatRelPtr), FDBBINDSTATUSArray);
  end;
end;

procedure TZOleDBPreparedStatement.CheckParameterIndex(var Value: Integer);
begin
  if not Prepared then
    Prepare;
  if (BindList.Count < Value+1) then
    if fBindImmediat then begin
      {$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF} := Format(SBindVarOutOfRange, [Value]);
      raise EZSQLException.Create({$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF});
    end else inherited CheckParameterIndex(Value);
end;

constructor TZOleDBPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FClientCP := ConSettings^.ClientCodePage.CP;
end;

function TZOleDBPreparedStatement.CreateOleDBConvertErrror(Index: Integer;
  WType: Word; SQLType: TZSQLType): EZOleDBConvertError;
begin
  Result := EZOleDBConvertError.Create('Index: '+ZFastCode.IntToStr(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF})+
    ', OleType: '+ZFastCode.IntToStr(wType)+', SQLType: '+GetEnumName(TypeInfo(TZSQLType), Ord(SQLType))+
    LineEnding+SUnsupportedParameterType+LineEnding+ 'Stmt: '+GetSQL);
end;

function TZOleDBPreparedStatement.CreateOutParamResultSet: IZResultSet;
var NativeResultSet: TZOleDBParamResultSet;
    CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOleDBParamResultSet.Create(Self, FParamsBuffer,
    FDBBindingArray, FParamNamesArray);
  if (ResultSetConcurrency = rcUpdatable) or (ResultSetType <> rtForwardOnly) then begin
    CachedResultSet := TZOleDBCachedResultSet.Create(NativeResultSet, SQL,
      TZGenerateSQLCachedResolver.Create(Self, NativeResultSet.GetMetaData), ConSettings);
    CachedResultSet.SetConcurrency(ResultSetConcurrency);
    Result := CachedResultSet;
  end else
    Result := NativeResultSet;
  FOpenResultSet := Pointer(Result);
end;

procedure TZOleDBPreparedStatement.Dyn_W_Convert(Index, Len: Integer; var Arr: PZArray);
var
  W_Dyn: TUnicodeStringDynArray;
  CP: Word;
  I: Integer;
  NewArr: TZArray;
label SetUniArray;
begin
  {$IFDEF WITH_VAR_INIT_WARNING}W_Dyn := nil;{$ENDIF}
  SetLength(W_Dyn, Len);
  CP := zCP_NONE;
  case TZSQLType(Arr.VArrayType) of
    stString, stUnicodeString:
      case Arr.VArrayVariantType of
        {$IFNDEF UNICODE}
        vtString:   CP := GetW2A2WConversionCodePage(ConSettings);
        {$ENDIF}
        vtUTF8String: CP := zCP_UTF8;
        vtAnsiString: CP := ZOSCodePage;
        vtRawByteString: CP := FClientCP;
        vtCharRec: begin
                    W_Dyn := CharRecArray2UnicodeStrArray(TZCharRecDynArray(Arr.VArray));
                    goto SetUniArray;
                   end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
    {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
  end;
  for I := 0 to High(W_Dyn) do
    W_Dyn[i] := ZRawToUnicode(TRawByteStringDynArray(Arr.VArray)[i], CP);
SetUniArray:
  NewArr := Arr^; //localize
  NewArr.VArrayType := Ord(stUnicodeString);
  NewArr.VArrayVariantType := vtUnicodeString;
  NewArr.VArray := Pointer(W_Dyn);
  BindList.Put(Index, NewArr, True);
  Arr := BindList[Index].Value;
end;

function TZOleDBPreparedStatement.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := @DefaultPreparableTokens;
end;

procedure TZOleDBPreparedStatement.InitDateBind(Index: Integer;
  SQLType: TZSQLType);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and (Bind.wType <> SQLType2OleDBTypeEnum[SQLType]));
  if fBindagain then begin
    Bind.wType := SQLType2OleDBTypeEnum[SQLType];
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and not DBPARAMFLAGS_ISLONG;
    case SQLType of
      stDate: Bind.cbMaxLen := SizeOf(TDBDate);
      stTime: Bind.cbMaxLen := SizeOf(TDBTime2);
      else    Bind.cbMaxLen := SizeOf(Double); //DBTYPE_DATE
    end;
    Bind.dwPart := DBPART_VALUE or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.InitFixedBind(Index: Integer; Size: Cardinal;
  _Type: DBTYPE);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and ((Bind.wType <> _Type) or (Bind.cbMaxLen <> Size)));
  if fBindagain then begin
    Bind.wType := _Type;
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and not DBPARAMFLAGS_ISLONG;
    Bind.cbMaxLen := Size;
    Bind.dwPart := DBPART_VALUE or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.InitLongBind(Index: Integer; _Type: DBTYPE);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and (Bind.wType <> _Type or DBTYPE_BYREF));
  if fBindagain then begin
    Bind.wType := _Type or DBTYPE_BYREF;
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and DBPARAMFLAGS_ISLONG;
    Bind.cbMaxLen := SizeOf(Pointer);
    Bind.dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.InitVaryBind(Index: Integer; Len: Cardinal;
  _Type: DBTYPE);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and ((Bind.wType <> _Type) or (Bind.cbMaxLen < Len)));
  if fBindagain then begin
    Bind.wType := _Type;
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and not DBPARAMFLAGS_ISLONG;
    Bind.cbMaxLen := {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Max(512,
      {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Max(Bind.cbMaxLen, Len));
    Bind.dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.InternalBindDbl(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var Bind: PDBBINDING;
  Data: PAnsichar;
  L: Cardinal;
  MS: Word;
label DWConv, TWConv, TSWConv;
begin
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_I8,
      DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4, DBTYPE_UI8:
                        SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Trunc(Value));
      DBTYPE_DATE:        PDateTime(Data)^ := Value;
      DBTYPE_DBDATE:      DecodeDate(Value, PWord(@PDBDate(Data).year)^, PDBDate(Data).month, PDBDate(Data).day);
      DBTYPE_DBTIME:      DecodeTime(Value, PDBTIME(Data)^.hour,
                              PDBTIME(Data)^.minute, PDBTIME(Data)^.second, MS);
      DBTYPE_DBTIME2:     begin
                            DecodeTime(Value, PDBTIME2(Data)^.hour,
                              PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second, MS);
                            PDBTIME2(Data)^.fraction := MS * 1000000;
                          end;
      DBTYPE_DBTIMESTAMP: begin
          DecodeDateTime(Value, PWord(@PDBTimeStamp(Data)^.year)^, PDBTimeStamp(Data).month, PDBTimeStamp(Data).day,
            PDBTimeStamp(Data).hour, PDBTimeStamp(Data)^.minute, PDBTimeStamp(Data)^.second, MS);
          PDBTimeStamp(Data)^.fraction := MS * 1000000;
        end;
      DBTYPE_WSTR: case SQLType of
            stFloat, stDouble: if Bind.cbMaxLen < 128 then begin
                  L := FloatToUnicode(Value, PWideChar(fByteBuffer)) shl 1;
                  if L < Bind.cbMaxLen
                  then Move(PWideChar(fByteBuffer)^, Data^, L)
                  else RaiseExceeded(Index);
                  PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L;
                end else
                  PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := FloatToUnicode(Value, PWideChar(Data)) shl 1;
            stDate: if Bind.cbMaxLen >= 22 then
DWConv:               PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                        DateTimeToUnicodeSQLDate(Value, PWideChar(Data), ConSettings.WriteFormatSettings, False) shl 1
                    else RaiseExceeded(Index);
            stTime: if (Bind.cbMaxLen >= 26 ){00.00.00.000#0} or ((Bind.cbMaxLen-2) shr 1 = DBLENGTH(ConSettings.WriteFormatSettings.TimeFormatLen)) then
TWConv:               PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                        DateTimeToUnicodeSQLTime(Value, PWideChar(Data), ConSettings.WriteFormatSettings, False) shl 1
                      else RaiseExceeded(Index);
            stTimeStamp: if (Bind.cbMaxLen >= 48){0000-00-00T00.00.00.000#0}  or ((Bind.cbMaxLen-2) shr 1 = DBLENGTH(ConSettings.WriteFormatSettings.DateTimeFormatLen)) then
TSWConv:              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                        DateTimeToUnicodeSQLTime(Value, PWideChar(Data), ConSettings.WriteFormatSettings, False) shl 1
                    else RaiseExceeded(Index);
            else raise CreateOleDBConvertErrror(Index, Bind.wType, SQLType);
          end;
      (DBTYPE_WSTR or DBTYPE_BYREF): case SQLType of
            stFloat, stDouble: begin
                PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 128);
                PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := FloatToUnicode(Value, ZPPWideChar(Data)^) shl 1;
              end;
            stDate: begin
                      PPointer(Data)^ := BindList.AcquireCustomValue(Index, stUnicodeString, 24);
                      Data := PPointer(Data)^;
                      goto DWConv;
                    end;
            stTime: begin
                      PPointer(Data)^ := BindList.AcquireCustomValue(Index, stUnicodeString, 26);
                      Data := PPointer(Data)^;
                      goto TWConv;
                    end;
            stTimeStamp: begin
                      PPointer(Data)^ := BindList.AcquireCustomValue(Index, stUnicodeString, 48);
                      Data := PPointer(Data)^;
                      goto TSWConv;
                    end;
            else raise CreateOleDBConvertErrror(Index, Bind.wType, SQLType);
        end;
      DBTYPE_NUMERIC: begin
                        Double2BCD(Value, PBCD(fByteBuffer)^);
                        BCD2SQLNumeric(PBCD(fByteBuffer)^, PDB_NUMERIC(Data));
                      end;
      //DBTYPE_VARNUMERIC:;
      else raise CreateOleDBConvertErrror(Index, Bind.wType, SQLType);
    end;
  end else begin//Late binding
    if SQLtype in [stDate, stTime, stTimeStamp]
    then InitDateBind(Index, SQLType)
    else InitFixedBind(Index, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType]);
    BindList.Put(Index, SQLType, P8Bytes(@Value));
  end;
end;

procedure TZOleDBPreparedStatement.InternalBindSInt(Index: Integer;
  SQLType: TZSQLType; Value: NativeInt);
var Bind: PDBBINDING;
  Data: Pointer;
  C: NativeUInt; //some delphis can't determine the overload of GetOrdinalDigits if a NativeUInt is uses
  L: Cardinal;
  Negative: Boolean;
begin
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Value;
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Value;
      DBTYPE_I1:        PShortInt(Data)^ := Value;
      DBTYPE_UI2:       PWord(Data)^ := Value;
      DBTYPE_UI4:       PCardinal(Data)^ := Value;
      DBTYPE_I8:        PInt64(Data)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value, C, Negative);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 24);
            Data := PPointer(Data)^;
          end else if (Bind.cbMaxLen <= (L +Byte(Ord(Negative))) shl 1) then
            RaiseExceeded(Index);
          if Negative then
            PWord(Data)^ := Ord('-');
          IntToUnicode(C, PWideChar(Data)+Ord(Negative), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1 + Byte(Ord(Negative));
        end;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_NUMERIC: begin
                        PDB_NUMERIC(Data)^.precision := GetOrdinalDigits(Value, {$IFDEF CPU64}PUInt64{$ELSE}PCardinal{$ENDIF}(@PDB_NUMERIC(Data).val[0])^, Negative);
                        PDB_NUMERIC(Data)^.scale := 0;
                        PDB_NUMERIC(Data)^.sign := Ord(not Negative);
                        FillChar(PDB_NUMERIC(Data)^.val[SizeOf(NativeUInt)], SQL_MAX_NUMERIC_LEN-SizeOf(NativeUInt), #0);
                      end;
      //DBTYPE_VARNUMERIC:;
      else raise CreateOleDBConvertErrror(Index, Bind.wType, SQLType);
    end;
  end else begin//Late binding
    InitFixedBind(Index, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType]);
    BindList.Put(Index, SQLType, {$IFDEF CPU64}P8Bytes{$ELSE}P4Bytes{$ENDIF}(@Value));
  end;
end;

procedure TZOleDBPreparedStatement.InternalBindUInt(Index: Integer;
  SQLType: TZSQLType; Value: NativeUInt);
var Bind: PDBBINDING;
  Data: PAnsichar;
  L: Cardinal;
begin
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := SmallInt(Value);
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Byte(Value);
      DBTYPE_I1:        PShortInt(Data)^ := ShortInt(Value);
      DBTYPE_UI2:       PWord(Data)^ := Word(Value);
      DBTYPE_UI4:       PCardinal(Data)^ := Cardinal(Value);
      DBTYPE_I8:        PInt64(Data)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 24);
            Data := PPointer(Data)^;
          end else if (L shl 1 >= Bind.cbMaxLen) then
            RaiseExceeded(Index);
          PWord(PWideChar(Data)+ L)^ := Ord(#0);
          IntToUnicode(Value, PWideChar(Data), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1;
        end;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      DBTYPE_NUMERIC: begin
                        PDB_NUMERIC(Data)^.precision := GetOrdinalDigits(Value);
                        PDB_NUMERIC(Data)^.scale := 0;
                        PDB_NUMERIC(Data)^.sign := 0;
                        FillChar(PDB_NUMERIC(Data)^.val[0], SQL_MAX_NUMERIC_LEN, #0);
                        PNativeUInt(@PDB_NUMERIC(Data)^.val[0])^ := Value;
                      end;
      //DBTYPE_VARNUMERIC:;
      else raise CreateOleDBConvertErrror(Index, Bind.wType, SQLType);
    end;
  end else begin//Late binding
    InitFixedBind(Index, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType]);
    BindList.Put(Index, SQLType, {$IFDEF CPU64}P8Bytes{$ELSE}P4Bytes{$ENDIF}(@Value));
  end;
end;

procedure TZOleDBPreparedStatement.Prepare;
var
  //DBInfo: IZDataBaseInfo;
  CommandPrepare: ICommandPrepare;
  S: String;
  Status: HResult;
label jmpRecreate;
begin
  if Not Prepared then begin//prevent PrepareInParameters
    S := GetParameters.Values[DSProps_DeferPrepare];
    if S = '' then
      S := Connection.GetParameters.Values[DSProps_DeferPrepare];
    if S = '' then begin
      S := DefineStatementParameter(Self, DSProps_PreferPrepared, StrTrue);
      fDEFERPREPARE := not StrToBoolEx(S);
    end else
      fDEFERPREPARE := StrToBoolEx(S);
    fDEFERPREPARE := fDEFERPREPARE or (FTokenMatchIndex = -1);
jmpRecreate:
    fBindImmediat := False;
    FCommand := FOleDBConnection.CreateCommand;
    if FOleDBConnection.GetServerProvider <> spMSJet then
      SetOleCommandProperties;
    Status := fCommand.SetCommandText(DBGUID_DEFAULT, Pointer(WSQL));
    if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
        {$IFDEF DEBUG}'ICommand.SetCommandText'{$ELSE}''{$ENDIF},
        IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
    if not fDEFERPREPARE and (fCommand.QueryInterface(IID_ICommandPrepare, CommandPrepare) = S_OK) then begin
      Status := CommandPrepare.Prepare(0);
      if Succeeded(Status) then begin
        fBindImmediat := True;
        if DriverManager.HasLoggingListener then
          DriverManager.LogMessage(lcPrepStmt,Self);
      end else if Status = DTS_E_OLEDBERROR then begin
        fDEFERPREPARE := True;
        goto jmpRecreate;
      end else FOleDBConnection.HandleErrorOrWarning(Status, lcPrepStmt, SQL,
        IImmediatelyReleasable(FWeakImmediatRelPtr), nil);
    end;
    //DBInfo := Connection.GetMetadata.GetDatabaseInfo;
    if FSupportsMultipleResultSets
    then fMoreResultsIndicator := mriUnknown
    else fMoreResultsIndicator := mriHasNoMoreResults;
    //fSupportsByRef := (DBInfo as IZOleDBDatabaseInfo).SupportsByRefAccessors;
    //DBInfo := nil;
    inherited Prepare;
  end else begin
    if FCallResultCache <> nil then
      ClearCallResultCache;
    FRowSet := nil; //release this interface! else we can't free the command in some tests
    FMultipleResults := nil; //release this interface! else we can't free the command in some tests
    if Assigned(FParameterAccessor) and ((BatchDMLArrayCount > 0) and
       (FDBParams.cParamSets = 0)) or //new arrays have been set
       ((BatchDMLArrayCount = 0) and (FDBParams.cParamSets > 1)) then //or single exec follows
      CalcParamSetsAndBufferSize;
  end;
end;

procedure TZOleDBPreparedStatement.PrepareInParameters;
var
  FNamesBuffer: PPOleStr; //we don't need this here except as param!
  FParamInfoArray: PDBParamInfoArray;
  FCommandWithParameters: ICommandWithParameters;
  DescripedDBPARAMINFO: TDBParamInfoDynArray;
  Status: HResult;
  Malloc: IMalloc;
begin
  if not fBindImmediat then
    Exit;
  if not Prepared then begin
    {check out the parameter informations }
    FParamInfoArray := nil; FNamesBuffer := nil; DescripedDBPARAMINFO := nil;
    OleCheck(fcommand.QueryInterface(IID_ICommandWithParameters, FCommandWithParameters));
    Status := FCommandWithParameters.GetParameterInfo(FDBUPARAMS,PDBPARAMINFO(FParamInfoArray), FNamesBuffer);
    if Status = DB_E_PARAMUNAVAILABLE then begin
      fDEFERPREPARE := true;
      fBindImmediat := False;
      Exit;
    end else if Failed(Status) then
      FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
        {$IFDEF DEBUG}'ICommandWithParameters.GetParameterInfo'{$ELSE}''{$ENDIF},
        IImmediatelyReleasable(FWeakImmediatRelPtr), FDBBINDSTATUSArray);
    try
      SetParamCount(FDBUPARAMS);
      if FDBUPARAMS > 0 then begin
        OleCheck(FCommand.QueryInterface(IID_IAccessor, FParameterAccessor));
        FRowSize := PrepareOleParamDBBindings(FDBUPARAMS, FDBBindingArray,
          FParamInfoArray);
        CalcParamSetsAndBufferSize;
        if not (FDBParams.hAccessor = 1) then
          raise EZSQLException.Create('Accessor handle should be unique!');
      end else begin
        { init ! }
        FDBParams.pData := nil;
        FDBParams.cParamSets := 0;
        FDBParams.hAccessor := 0;
      end;
    finally
      Malloc := FOleDBConnection.GetMalloc;
      try
        if Assigned(FParamInfoArray) and (Pointer(FParamInfoArray) <> Pointer(DescripedDBPARAMINFO)) then
          Malloc.Free(FParamInfoArray);
        if Assigned(FNamesBuffer) then
          Malloc.Free(FNamesBuffer);
      finally
        Malloc := nil;
      end;
      FCommandWithParameters := nil;
    end;
  end else begin
    FDBUPARAMS := BindList.Count;
    SetBindOffsets;
    if FParameterAccessor = nil then
      OleCheck(FCommand.QueryInterface(IID_IAccessor, FParameterAccessor));
    CalcParamSetsAndBufferSize;
  end;
end;

procedure TZOleDBPreparedStatement.RaiseExceeded(Index: Integer);
begin
  raise EZSQLException.Create(Format(SParamValueExceeded, [Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}])+LineEnding+
    'Stmt: '+GetSQL);
end;

procedure TZOleDBPreparedStatement.RegisterParameter(Index: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
var Bind: PDBBINDING;
begin
  CheckParameterIndex(Index);
  if (Name <> '') then begin
    if (High(FParamNamesArray) < Index) then
      SetLength(FParamNamesArray, Index+1);
    FParamNamesArray[Index] := Name;
  end;
  Bind := @FDBBindingArray[Index];
  if not fDEFERPREPARE then begin
    case ParamType of
      pctReturn, pctOut: if Bind.dwFlags and DBPARAMFLAGS_ISINPUT <> 0 then
                           Bind.dwFlags := (Bind.dwFlags and not DBPARAMFLAGS_ISINPUT) or DBPARAMFLAGS_ISOUTPUT;
      pctIn, pctInOut: if Bind.dwFlags and DBPARAMFLAGS_ISINPUT = 0 then
                           Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISINPUT;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
  end else begin
    Bind.wType := SQLType2OleDBTypeEnum[SQLType];
    if (Ord(SQLType) < Ord(stBigDecimal)) or (SQLtype = stGUID) then
      InitFixedBind(Index, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType])
    else if Ord(SQLType) <= Ord(stTimestamp) then
       InitDateBind(Index, SQLType)
    else if Ord(SQLType) < Ord(stAsciiStream) then
      InitVaryBind(Index, Max(512, PrecisionOrSize), SQLType2OleDBTypeEnum[SQLType])
    else InitLongBind(Index, SQLType2OleDBTypeEnum[SQLType]);
  end;
  Bind.eParamIO :=  ParamType2OleIO[ParamType];
  inherited RegisterParameter(Index, SQLType, ParamType, Name, PrecisionOrSize, Scale);
end;

procedure TZOleDBPreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FParameterAccessor := nil;
  SetLength(FDBBindingArray, 0);
  SetLength(FParamsBuffer, 0);
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZOleDBPreparedStatement.SetAnsiString(Index: Integer;
  const Value: AnsiString);
begin
  BindRaw(Index, Value, zOSCodePage);
end;
{$ENDIF}

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF NO_CONST_ZEROBCD}
const ZeroBCDFraction: packed array [0..31] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
{$ENDIF}
procedure TZOleDBPreparedStatement.SetBigDecimal(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
var Bind: PDBBINDING;
  Data: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NUMERIC:   BCD2SQLNumeric(Value, PDB_Numeric(Data));
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      {$IFDEF CPU64}
      DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_I8: InternalBindSInt(Index, stLong, BCD2Int64(Value));
      DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4, DBTYPE_UI8: InternalBindUInt(Index, stULong, BCD2UInt64(Value));
      {$ELSE}
      DBTYPE_I1, DBTYPE_I2, DBTYPE_I4: InternalBindSInt(Index, stInteger, Integer(BCD2Int64(Value)));
      DBTYPE_I8:        PInt64(Data)^ := BCD2Int64(Value);
      DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4: InternalBindUInt(Index, stLongWord, Cardinal(BCD2UInt64(Value)));
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := BCD2UInt64(Value);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      {$ENDIF}
      DBTYPE_R4:        PSingle(Data)^ := BCDToDouble(Value);
      DBTYPE_R8:        PDouble(Data)^ := BCDToDouble(Value);
      DBTYPE_CY:        BCDToCurr(Value, PCurrency(Data)^);
      DBTYPE_BOOL:      PWordBool(Data)^ := not CompareMem(@Value.Fraction[0], @{$IFDEF NO_CONST_ZEROBCD}ZeroBCDFraction[0]{$ELSE}NullBcd.Fraction[0]{$ENDIF}, MaxFMTBcdDigits);
      DBTYPE_VARIANT:   POleVariant(Data)^ := BcdToSQLUni(Value);
      DBTYPE_WSTR: if Bind.cbMaxLen < {$IFDEF FPC}Byte{$ENDIF}(Value.Precision+2) shl 1 then begin //(64nibbles+dot+neg sign) -> test final length
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := BcdToUni(Value, PWideChar(fByteBuffer), '.') shl 1;
                    if PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ < Bind.cbMaxLen
                    then Move(PWideChar(fByteBuffer)^, Data^, PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^)
                    else RaiseExceeded(Index);
                  end else begin
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := BcdToUni(Value, PWideChar(Data), '.') shl 1;
                  end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                   PPointer(Data)^ := BindList.AcquireCustomValue(Index, stUnicodeString, 68); //8Byte align
                   PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := BcdToUni(Value, ZPPWideChar(Data)^, '.') shl 1;;
                 end;
      //DBTYPE_VARNUMERIC:;
     else raise CreateOleDBConvertErrror(Index, Bind.wType, stBigDecimal);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(TDB_NUMERIC), DBTYPE_NUMERIC);
    BindList.Put(Index, Value);
  end;
end;

procedure TZOleDBPreparedStatement.SetBindCapacity(Capacity: Integer);
begin
  inherited SetBindCapacity(Capacity);
  if not fBindImmediat and fDEFERPREPARE and (Bindlist.Count < Capacity) then
    SetParamCount(Capacity);
end;

procedure TZOleDBPreparedStatement.SetBindOffsets;
var I: Integer;
  Bind: PDBBINDING;
begin
  FRowSize := 0;
  for I := 0 to BindList.Count -1 do begin
    Bind := @FDBBindingArray[I];
    Bind.iOrdinal := I +1;
    Bind.obStatus := FRowSize;
    Inc(FRowSize, SizeOf(DBSTATUS));
    Bind.obLength := FRowSize;
    if Bind.dwPart and DBPART_LENGTH <> 0 then
      Inc(FRowSize, SizeOf(DBLENGTH));
    Bind.obValue := FRowSize;
    Inc(FRowSize, Bind.cbMaxLen);
    Bind.eParamIO := ParamType2OleIO[BindList.ParamTypes[I]];
    if Ord(BindList.ParamTypes[I]) >= Ord(pctInOut) then begin
      if BindList.ParamTypes[I] = pctInOut then
        Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISINPUT or DBPARAMFLAGS_ISOUTPUT
      else
        Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISOUTPUT;
    end else
      Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISINPUT;
  end;
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large binary value is input to a <code>LONGVARBINARY</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the binary parameter value
}
procedure TZOleDBPreparedStatement.SetBlob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
var Bind: PDBBINDING;
  Data: PAnsichar;
  DBStatus: PDBSTATUS;
  DBLENGTH: PDBLENGTH;
  Len: NativeUInt;
  PA: PAnsiChar;
  PW: PWideChar absolute PA;
label Fix_CLob;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  BindList.Put(Index, SQLType, Value);//keep alive
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    DBStatus := PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus);
    if (Value = nil) or Value.IsEmpty then begin
      DBSTATUS^ := DBSTATUS_S_ISNULL;
      Exit;
    end;
    DBSTATUS^ := DBSTATUS_S_OK;
    DBLENGTH := PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength);
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      (DBTYPE_STR or DBTYPE_BYREF):
        if Value.IsClob then begin
          Value.SetCodePageTo(FClientCP);
          PPointer(Data)^ := Value.GetPAnsiChar(FClientCP, FRawTemp, Len);
          DBLENGTH^ := Len;
        end else
Fix_CLob: raise CreateConversionError(Index, stBinaryStream, stAsciiStream);
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
              Value.SetCodePageTo(zCP_UTF16);
              PPointer(Data)^ := Value.GetPWideChar(fUniTemp, Len);
              DBLENGTH^ := Len shl 1;
            end;
      (DBTYPE_GUID or DBTYPE_BYREF):;
      (DBTYPE_BYTES or DBTYPE_BYREF): begin
          FRawTemp := '';
          PPointer(Data)^ := Value.GetBuffer(FrawTemp, DBLENGTH^);
          if Pointer(FRawTemp) <> nil then
             SetBlob(Index, stBinaryStream, TZLocalMemBLob.CreateWithData(PPointer(Data)^, DBLENGTH^, FOpenLobStreams));
        end;
      DBTYPE_BYTES: begin
              PA := Value.GetBuffer(FRawTemp, DBLENGTH^);
              if DBLENGTH^ < Bind.cbMaxLen
              then Move(PA^, Data^, DBLENGTH^)
              else RaiseExceeded(Index);
            end;
      DBTYPE_STR: if Value.IsClob then begin
                Value.SetCodePageTo(FClientCP);
                PA := Value.GetPAnsiChar(FClientCP, FRawTemp, Len);
                DBLENGTH^ := Len;
                if Len < Bind.cbMaxLen
                then Move(PA^, Data^, DBLENGTH^)
                else RaiseExceeded(Index);
              end else
                goto Fix_CLob;
      DBTYPE_WSTR: begin
              Value.SetCodePageTo(FClientCP);
              PW := Value.GetPWideChar(FUniTemp, Len);
              DBLENGTH^ := Len shl 1;
              if DBLENGTH^ < Bind.cbMaxLen
              then Move(PW^, Data^, DBLENGTH^)
              else RaiseExceeded(Index);
            end;
      else raise CreateOleDBConvertErrror(Index, Bind.wType, SQLType);
    end;
  end else
    InitLongBind(Index, SQLType2OleDBTypeEnum[SQLType]);
end;

procedure TZOleDBPreparedStatement.SetBoolean(Index: Integer; Value: Boolean);
begin
  InternalBindUInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBoolean, Ord(Value));
end;

procedure TZOleDBPreparedStatement.SetByte(Index: Integer; Value: Byte);
begin
  InternalBindUInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stByte, Value);
end;

{**
  Sets the designated parameter to a Java array of bytes by reference.
  The driver converts this to an SQL <code>VARBINARY</code> or
  <code>LONGVARBINARY</code> (depending on the argument's size relative to
  the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the parameter value address
  @param Len the length of the addressed value
}
procedure TZOleDBPreparedStatement.SetBytes(Index: Integer;
  Value: PByte; Len: NativeUInt);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  BindList.Put(Index, stBytes, Value, Len); //localize
  if fBindImmediat
  then SetPAnsiChar(Index, PAnsiChar(Value), Len)
  else InitVaryBind(Index, Len, DBTYPE_BYTES);
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetBytes(Index: Integer;
  const Value: TBytes);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  BindList.Put(Index, stBytes, Value); //localize
  if fBindImmediat
  then SetPAnsiChar(Index, Pointer(Value), Length(Value))
  else InitVaryBind(Index, Length(Value), DBTYPE_BYTES);
end;

procedure TZOleDBPreparedStatement.SetCharRec(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZCharRec);
label set_from_tmp;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then
    if Value.CP = zCP_UTF16 then
      case FDBBindingArray[Index].wType of
        DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF):
          SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUnicodeToRaw(Value.P, Value.Len, FClientCP));
        else SetPWideChar(Index, Value.P, Value.Len)
      end
    else case FDBBindingArray[Index].wType of
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF):
        goto set_from_tmp;
      DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF):
        if FClientCP = Value.CP then
          SetPAnsiChar(Index, Value.P, Value.Len);
        else begin
set_from_tmp:
          FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
          SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp));
        end;
    end
  else begin
    InitVaryBind(Index, (Value.Len+1) shl 1, DBTYPE_WSTR);
    BindList.Put(Index, stString, Value.P, Value.Len, Value.CP);
  end;
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var Bind: PDBBINDING;
  Data, PEnd: PAnsiChar;
  Negative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_I4:        PInteger(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_I1:        PShortInt(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_UI2:       PWord(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_UI4:       PCardinal(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_I8:        PInt64(Data)^ := PInt64(@Value)^ div 10000;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := PInt64(@Value)^ div 10000;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      DBTYPE_WSTR: if Bind.cbMaxLen < 44 then begin //(19digits+dot+neg sign) -> test final length
                    CurrToUnicode(Value, '.', PWideChar(fByteBuffer), @PEnd);
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-PAnsiChar(fByteBuffer);//size in bytes
                    if PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ < Bind.cbMaxLen
                    then Move(PWideChar(fByteBuffer)^, Data^, PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^)
                    else RaiseExceeded(Index);
                  end else begin
                    CurrToUnicode(Value, '.', PWideChar(Data), @PEnd);
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-Data;
                  end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                   PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 48); //8Byte align
                   CurrToUnicode(Value, '.', ZPPWideChar(Data)^, @PEnd);
                   PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-PPAnsiChar(Data)^;
                 end;
      DBTYPE_NUMERIC: begin
                        PDB_NUMERIC(Data)^.precision := GetOrdinalDigits(PInt64(@Value)^, PUInt64(@PDB_NUMERIC(Data)^.val[0])^, Negative);
                        PDB_NUMERIC(Data)^.scale := 4;
                        PDB_NUMERIC(Data)^.sign := Ord(not Negative);
                        FillChar(PDB_NUMERIC(Data)^.val[SizeOf(Currency)], SQL_MAX_NUMERIC_LEN-SizeOf(Currency), #0);
                      end;
      //DBTYPE_VARNUMERIC:;
     else raise CreateOleDBConvertErrror(Index, Bind.wType, stCurrency);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(Currency), DBTYPE_CY);
    BindList.Put(Index, stCurrency, P8Bytes(@Value));
  end;
end;

procedure TZOleDBPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType);
var arr: TZArray;
  GUID_Dyn: TGUIDDynArray;
  i,L: LengthInt;
  P: Pointer;
begin
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
  if (ParameterIndex = FirstDbcIndex) and (BindList.ParamTypes[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] <> pctResultSet) then
    FDBParams.cParamSets := 0;
  if (SQLType = stGUID) and not (VariantType in [vtNull, vtBytes]) then begin
    {$IFDEF WITH_VAR_INIT_WARNING}GUID_Dyn := nil;{$ENDIF}
    SetLength(GUID_Dyn, Length(TRawByteStringDynArray(Value)));
    Arr := PZArray(BindList[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value)^;
    for I := 0 to High(GUID_Dyn) do
      ArrayValueToGUID(@Arr, i, @GUID_Dyn[i]);
    Arr.VArrayType := Ord(stGUID);
    Arr.VArrayVariantType := vtNull;
    BindList.Put(ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Arr, True);
  end;
  if fDEFERPREPARE then begin
    {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
    case SQLtype of
      stBigDecimal: InitFixedBind(ParameterIndex, SizeOf(Double), DBTYPE_R8);
      stDate: InitFixedBind(ParameterIndex, SizeOf(TDBDate), DBTYPE_DBDATE);
      stTime: InitFixedBind(ParameterIndex, SizeOf(TDBTIME2), DBTYPE_DBTIME2);
      stTimestamp: InitFixedBind(ParameterIndex, SizeOf(Double), DBTYPE_DATE);
      stString,
      stUnicodeString: begin
         P := PZArray(BindList[ParameterIndex].Value).VArray;
         L := 0;
         for I := 0 to {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFNDEF FPC}-1{$ENDIF} do
           case PZArray(BindList[ParameterIndex].Value).VArrayVariantType of
              {$IFNDEF UNICODE}vtString,{$ENDIF}
              vtAnsiString, vtUTF8String, VtRawByteString:  L := Max(L, Length(TRawByteStringDynArray(P)[I]));
              vtCharRec:                                    L := Max(L, TZCharRecDynArray(P)[I].Len);
              {$IFDEF UNICODE}vtString,{$ENDIF}
              vtUnicodeString:                              L := Max(L, Length(TUnicodeStringDynArray(P)[I]));
              {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
            end;
          InitVaryBind(ParameterIndex, (L+1) shl 1, DBTYPE_WSTR);
        end;
      stBytes: begin
          L := 0;
          for I := 0 to High(TBytesDynArray(Value)) do
            L := Max(L, Length(TBytesDynArray(Value)[I]));
          InitVaryBind(ParameterIndex, L, DBTYPE_BYTES);
        end;
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream: InitLongBind(ParameterIndex, SQLType2OleDBTypeEnum[SQLType]);
      stUnknown, stArray, stResultSet: raise CreateOleDBConvertErrror(ParameterIndex, DBTYPE_WSTR, SQLType);
      else InitFixedBind(ParameterIndex, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType]);
    end;
  end;
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZOleDBPreparedStatement.SetDate(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate);
var Bind: PDBBINDING;
  Data: PAnsichar;
  DT: TDateTime;
label DWConv;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:        PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_DATE:        if not TryDateToDateTime(Value, PDateTime(Data)^) then
                            raise CreateOleDBConvertErrror(Index, Bind.wType, stDate);
      DBTYPE_DBDATE:      begin
                            PDBDate(Data).year := Value.Year;
                            if Value.IsNegative then
                              PDBDate(Data).year := -PDBDate(Data).year;
                            PDBDate(Data).month := Value.Month;
                            PDBDate(Data).day := Value.Day;
                          end;
      DBTYPE_DBTIME:      FillChar(Data^, SizeOf(TDBTIME), #0);
      DBTYPE_DBTIME2:     FillChar(Data^, SizeOf(TDBTIME2), #0);
      DBTYPE_DBTIMESTAMP: begin
                            Fillchar(Data^, SizeOf(TDBTimeStamp), #0);
                            PDBTimeStamp(Data)^.year := Value.Year;
                            if Value.IsNegative then
                              PDBTimeStamp(Data)^.year := -PDBTimeStamp(Data)^.year;
                            PDBTimeStamp(Data)^.month := Value.Month;
                            PDBTimeStamp(Data)^.day := Value.Day;
                          end;
      DBTYPE_DBTIMESTAMPOFFSET: begin
                            Fillchar(Data^, SizeOf(TDBTIMESTAMPOFFSET), #0);
                            PDBTIMESTAMPOFFSET(Data)^.year := Value.Year;
                            if Value.IsNegative then
                              PDBTIMESTAMPOFFSET(Data)^.year := -PDBTimeStamp(Data)^.year;
                            PDBTIMESTAMPOFFSET(Data)^.month := Value.Month;
                            PDBTIMESTAMPOFFSET(Data)^.day := Value.Day;
                          end;
      DBTYPE_WSTR:  if Bind.cbMaxLen >= 22 then
DWConv:               PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                        DateToUni(Value.Year, Value.Month, Value.Day,
                          PWideChar(Data), ConSettings.WriteFormatSettings.DateFormat,
                          False, Value.IsNegative) shl 1
                    else RaiseExceeded(Index);
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                      PPointer(Data)^ := BindList.AcquireCustomValue(Index, stUnicodeString, 24);
                      Data := PPointer(Data)^;
                      goto DWConv;
                    end;
      else          if TryDateToDateTime(Value, DT)
                    then InternalBindDbl(Index, stDate, DT)
                    else InternalBindSInt(Index, stDate, 1);
    end;
  end else begin//Late binding
    InitDateBind(Index, stDate);
    BindList.Put(Index, Value);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
begin
  InternalBindDbl(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stDouble, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetFloat(Index: Integer; Value: Single);
begin
  InternalBindDbl(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stFloat, Value);
end;

procedure TZOleDBPreparedStatement.SetGUID(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TGUID);
var Bind: PDBBINDING;
  Data: Pointer;
label set_uni_len, set_uid_len;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_GUID:  PGUID(Data)^ := Value;
      (DBTYPE_GUID or DBTYPE_BYREF): begin
                        BindList.Put(Index, Value); //localize
                        PPointer(Data)^ := BindList[Index].Value;
                      end;
      DBTYPE_BYTES: if Bind.cbMaxLen < SizeOf(TGUID) then
                      RaiseExceeded(Index)
                    else begin
                      PGUID(Data)^ := Value;
                      goto set_uid_len;
                    end;
      DBTYPE_BYTES or DBTYPE_BYREF: begin
                        BindList.Put(Index, Value); //localize
                        PPointer(Data)^ := BindList[Index].Value;
set_uid_len:            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=  SizeOf(TGUID);
                      end;
(*      DBTYPE_STR: if Bind.cbMaxLen < 37 then
                    RaiseExceeded(Index)
                  else begin
                    GUIDToBuffer(@Value.D1, PAnsiChar(Data), [guidSet0Term]);
                    goto set_raw_len;
                  end;
      (DBTYPE_STR or DBTYPE_BYREF): begin
                    PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 37);
                    GUIDToBuffer(@Value.D1, PPAnsiChar(Data)^, [guidSet0Term]);
set_raw_len:        PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 36;
                  end; *)
      DBTYPE_WSTR:if Bind.cbMaxLen < 74 then
                    RaiseExceeded(Index)
                  else begin
                    GUIDToBuffer(@Value.D1, PWideChar(Data), [guidSet0Term]);
                    goto set_uni_len;
                  end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                    PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 74);
                    GUIDToBuffer(@Value.D1, ZPPWideChar(Data)^, [guidSet0Term]);
set_uni_len:        PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 72;
                  end;
      else raise CreateOleDBConvertErrror(Index, Bind.wType, stGUID);
    end;
  end else begin
    InitFixedBind(Index, SizeOf(TGUID), DBTYPE_GUID);
    BindList.Put(Index, Value);
  end;
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC}
  {$PUSH}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 5057 off : Local variable "Len" does not seem to be initialized}
{$ENDIF} // uses pointer maths
procedure TZOleDBPreparedStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZSQLStringWriter; var Result: SQLString);
var Bind: PDBBINDING;
  Data: Pointer;
  Len: NativeUInt;
begin
  case BindList.ParamTypes[ParamIndex] of
    pctReturn: SQLWriter.AddText('(RETURN_VALUE)', Result);
    pctOut: SQLWriter.AddText('(OUT_PARAM)', Result);
    else begin
      Bind := @FDBBindingArray[ParamIndex];
      if PDBSTATUS(NativeUInt(FDBParams.pData)+Bind.obStatus)^ = DBSTATUS_S_ISNULL then
        SQLWriter.AddText('(NULL)', Result)
      else begin
        Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
        case Bind.wType of
          DBTYPE_NULL:  SQLWriter.AddText('(NULL)', Result);
          (DBTYPE_STR   or DBTYPE_BYREF): SQLWriter.AddText('(CLOB/VARCHAR(MAX))', Result);
          (DBTYPE_WSTR  or DBTYPE_BYREF): SQLWriter.AddText('(NCLOB/NVARCHAR(MAX))', Result);
          (DBTYPE_BYTES or DBTYPE_BYREF): SQLWriter.AddText('(BLOB/VARBINARY(MAX))', Result);
          DBTYPE_BOOL:  if PWordBool(Data)^
                        then SQLWriter.AddText('(TRUE)', Result)
                        else SQLWriter.AddText('(FALSE)', Result);
          DBTYPE_I1:    SQLWriter.AddOrd(PShortInt(Data)^, Result);
          DBTYPE_UI1:   SQLWriter.AddOrd(PByte(Data)^, Result);
          DBTYPE_I2:    SQLWriter.AddOrd(PSmallInt(Data)^, Result);
          DBTYPE_UI2:   SQLWriter.AddOrd(PWord(Data)^, Result);
          DBTYPE_I4:    SQLWriter.AddOrd(PInteger(Data)^, Result);
          DBTYPE_UI4:   SQLWriter.AddOrd(PCardinal(Data)^, Result);
          DBTYPE_I8:    SQLWriter.AddOrd(PInt64(Data)^, Result);
          DBTYPE_UI8:   SQLWriter.AddOrd(PUInt64(Data)^, Result);
          DBTYPE_R4:    SQLWriter.AddFloat(PSingle(Data)^, Result);
          DBTYPE_R8:    SQLWriter.AddFloat(PDouble(Data)^, Result);
          DBTYPE_CY:    SQLWriter.AddDecimal(PCurrency(Data)^, Result);
          DBTYPE_GUID:  SQLWriter.AddGUID(PGUID(Data)^, [guidWithBrackets, guidQuoted], Result);
          DBTYPE_NUMERIC: begin
                        Len := SQL_MAX_NUMERIC_LEN;
                        {$IFDEF UNICODE}
                        SQLNumeric2Uni(PDB_Numeric(Data), PWideChar(FByteBuffer), Len);
                        SQLWriter.AddText(PWideChar(FByteBuffer), Len, Result);
                        {$ELSE}
                        SQLNumeric2Raw(PDB_Numeric(Data), PAnsiChar(FByteBuffer), Len);
                        SQLWriter.AddText(PAnsiChar(FByteBuffer), Len, Result);
                        {$ENDIF}
                      end;
          DBTYPE_BYTES: SQLWriter.AddHexBinary(Data, PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^, True, Result);
          DBTYPE_WSTR:  {$IFDEF UNICODE}
                        SQLWriter.AddTextQuoted(Data, PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ shr 1, #39, Result);
                        {$ELSE}
                        begin
                          FRawTemp := PUnicodeToRaw(Data, PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ shr 1, zCP_UTF8);
                          SQLWriter.AddTextQuoted(FRawTemp, AnsiChar(#39), Result);
                        end;
                        {$ENDIF}
          DBTYPE_DBDATE:begin
                        Len := {$IFDEF UNICODE}DateToUni{$ELSE}DateToRaw{$ENDIF}(
                          Abs(PDBDATE(Data)^.year), PDBDATE(Data)^.month,
                          PDBDATE(Data)^.day, {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer),
                          ConSettings.WriteFormatSettings.DateFormat, True, PDBDATE(Data)^.year <0);
                        SQLWriter.AddText({$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer), Len, Result);
                      end;
          DBTYPE_DATE:  SQLWriter.AddDate(PDateTime(Data)^, ConSettings.WriteFormatSettings.DateFormat, Result);
          DBTYPE_DBTIME: begin
                        Len := {$IFDEF UNICODE}TimeToUni{$ELSE}TimeToRaw{$ENDIF}(
                          PDBTIME(Data)^.hour, PDBTIME(Data)^.minute,
                          PDBTIME(Data)^.second, 0, {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer),
                          ConSettings.WriteFormatSettings.TimeFormat, True, False);
                        SQLWriter.AddText({$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer), Len, Result);
                      end;
          DBTYPE_DBTIME2: begin
                        Len := {$IFDEF UNICODE}TimeToUni{$ELSE}TimeToRaw{$ENDIF}(
                          PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute,
                          PDBTIME2(Data)^.second, PDBTIME2(Data)^.fraction,
                          {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer),
                          ConSettings.WriteFormatSettings.DateTimeFormat, True, False);
                        SQLWriter.AddText({$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer), Len, Result);
                      end;
          DBTYPE_DBTIMESTAMP: begin
                        Len := {$IFDEF UNICODE}DateTimeToUni{$ELSE}DateTimeToRaw{$ENDIF}(
                          Abs(PDBTimeStamp(Data)^.year), PDBTimeStamp(Data).month,
                          PDBTimeStamp(Data).day, PDBTimeStamp(Data).hour, PDBTimeStamp(Data)^.minute,
                          PDBTimeStamp(Data)^.second, PDBTimeStamp(Data)^.fraction,
                          {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer),
                          ConSettings.WriteFormatSettings.DateTimeFormat, True, PDBTimeStamp(Data)^.year < 0);
                        SQLWriter.AddText({$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer), Len, Result);
                      end;
          DBTYPE_DBTIMESTAMPOFFSET: begin
                        Len := {$IFDEF UNICODE}DateTimeToUni{$ELSE}DateTimeToRaw{$ENDIF}(
                          Abs(PDBTIMESTAMPOFFSET(Data)^.year), PDBTIMESTAMPOFFSET(Data).month,
                          PDBTIMESTAMPOFFSET(Data).day, PDBTIMESTAMPOFFSET(Data).hour, PDBTIMESTAMPOFFSET(Data)^.minute,
                          PDBTIMESTAMPOFFSET(Data)^.second, PDBTIMESTAMPOFFSET(Data)^.fraction,
                          {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer),
                          ConSettings.WriteFormatSettings.DateTimeFormat, True, PDBTimeStamp(Data)^.year < 0);
                          SQLWriter.AddText({$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(fByteBuffer), Len, Result);
                        end;
          else SQLWriter.AddText('(unknown)', Result);
        end;
      end;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZOleDBPreparedStatement.SetInt(Index, Value: Integer);
begin
  InternalBindSInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stInteger, Value);
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetLong(Index: Integer; const Value: Int64);
{$IFDEF CPU64}
begin
  InternalBindSInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLong, Value);
{$ELSE}
var Bind: PDBBINDING;
  Data: PAnsichar;
  u64: UInt64;
  L: Cardinal;
  Negative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Value;
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Value;
      DBTYPE_I1:        PShortInt(Data)^ := Value;
      DBTYPE_UI2:       PWord(Data)^ := Value;
      DBTYPE_UI4:       PCardinal(Data)^ := Value;
      DBTYPE_I8:        PInt64(Data)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value, u64, Negative);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 24); //8Byte align
            Data := PPointer(Data)^; //-9.223.372.036.854.775.808
          end else if (Bind.cbMaxLen <= (L +Byte(Ord(Negative))) shl 1) then
            RaiseExceeded(Index);
          if Negative then
            PWord(PPointer(Data)^)^ := Ord('-');
          IntToUnicode(u64, PWideChar(Data)+Ord(Negative), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1 + Byte(Ord(Negative));
        end;
      DBTYPE_NUMERIC: begin
                        PDB_NUMERIC(Data)^.precision := GetOrdinalDigits(PInt64(@Value)^, PUInt64(@PDB_NUMERIC(Data)^.val[0])^, Negative);
                        PDB_NUMERIC(Data)^.scale := 0;
                        PDB_NUMERIC(Data)^.sign := Ord(not Negative);
                        FillChar(PDB_NUMERIC(Data)^.val[SizeOf(UInt64)], SQL_MAX_NUMERIC_LEN-SizeOf(UInt64), #0);
                      end;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      //DBTYPE_VARNUMERIC:;
      else raise CreateOleDBConvertErrror(Index, Bind.wType, stLong);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(Int64), DBTYPE_I8);
    BindList.Put(Index, stLong, P8Bytes(@Value));
  end;
  {$ENDIF}
end;

{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF} // uses pointer maths
procedure TZOleDBPreparedStatement.SetNull(Index: Integer; SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then
    PDBSTATUS(NativeUInt(FDBParams.pData)+FDBBindingArray[Index].obStatus)^ := DBSTATUS_S_ISNULL
  else begin
    if SQLType = stUnknown then
      SQLtype := BindList.SQLTypes[Index];
    BindList.SetNull(Index, SQLType);
    if Ord(SQLType) < Ord(stString) then
      if SQLType in [stDate, stTime, stTimeStamp]
      then InitDateBind(Index, SQLType)
      else InitFixedBind(Index, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType])
    else if Ord(SQLType) < Ord(stAsciiStream) then
      InitFixedBind(Index, 512, SQLType2OleDBTypeEnum[SQLType])
    else InitLongBind(Index, SQLType2OleDBTypeEnum[SQLType])
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZOleDBPreparedStatement.SetOleCommandProperties;
var
  FCmdProps: ICommandProperties;
  rgCommonProperties: array[0..20] of TDBProp;
  rgProviderProperties: TDBProp;
  rgPropertySets: array[0..1] of TDBPROPSET;
  Provider: TZServerProvider;
  Status: HResult;

  procedure SetProp(var PropSet: TDBPROPSET; PropertyID: DBPROPID; Value: SmallInt);
  begin
    //initialize common property options
    //VariantInit(PropSet.rgProperties^[PropSet.cProperties].vValue);
    PropSet.rgProperties^[PropSet.cProperties].dwPropertyID := PropertyID;
    PropSet.rgProperties^[PropSet.cProperties].dwOptions    := DBPROPOPTIONS_REQUIRED;
    PropSet.rgProperties^[PropSet.cProperties].dwStatus     := 0;
    PropSet.rgProperties^[PropSet.cProperties].colid        := DB_NULLID;
    PropSet.rgProperties^[PropSet.cProperties].vValue       := Value;
    Inc(PropSet.cProperties);
  end;
begin
  FCmdProps := nil; //init
  if Succeeded(fCommand.QueryInterface(IID_ICommandProperties, FCmdProps)) then begin
    Provider := Connection.GetServerProvider;
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms723066%28v=vs.85%29.aspx
    rgPropertySets[0].cProperties     := 0; //init
    rgPropertySets[0].guidPropertySet := DBPROPSET_ROWSET;
    rgPropertySets[0].rgProperties    := @rgCommonProperties[0];
    rgPropertySets[1].cProperties     := 0;
    case Provider of
      spMSSQL: rgPropertySets[1].guidPropertySet := DBPROPSET_SQLSERVERROWSET
      else rgPropertySets[1].guidPropertySet := DBPROPSET_ROWSET;
    end;
    rgPropertySets[1].rgProperties    := @rgProviderProperties;

    SetProp(rgPropertySets[0], DBPROP_COMMANDTIMEOUT,    Max(0, fStmtTimeOut)); //Set command time_out static!
    SetProp(rgPropertySets[0], DBPROP_SERVERCURSOR,      ZVARIANT_TRUE); //force a server side cursor
    if (Provider = spMSSQL) then begin
      //turn off deferred prepare -> raise exception on Prepare if command can't be executed!
      //http://msdn.microsoft.com/de-de/library/ms130779.aspx
      if fDEFERPREPARE
      then SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, ZVARIANT_TRUE)
      else SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, ZVARIANT_FALSE);
    end else begin
      //to avoid http://support.microsoft.com/kb/272358/de we need a
      //FAST_FORWARD(RO) server cursor
      {common sets which are NOT default: according the cursor models of
      http://msdn.microsoft.com/de-de/library/ms130840.aspx }
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,        ZVARIANT_FALSE);
      if (Connection as IZOleDBConnection).SupportsMARSConnection then begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         ZVARIANT_FALSE);
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   ZVARIANT_FALSE);
      end else begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         ZVARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   ZVARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
      end;
      SetProp(rgPropertySets[0], DBPROP_OTHERINSERT,       ZVARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_OTHERUPDATEDELETE, ZVARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,         ZVARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANFETCHBACKWARDS,  ZVARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANSCROLLBACKWARDS, ZVARIANT_FALSE);
    end;
    try
      Status := FCmdProps.SetProperties(2,@rgPropertySets[0]);
      if Failed(Status) then
        FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
          'ICommandProperties.SetProperties', FOleDBConnection);
    finally
      FCmdProps := nil;
    end;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF}
procedure TZOleDBPreparedStatement.SetPAnsiChar(Index: Word; Value: PAnsiChar;
  Len: Cardinal);
var Bind: PDBBINDING;
  Data: PAnsichar;
  TS: TZTimeStamp;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
label Fail;
begin
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := RawToIntDef(Value, Value+Len, 0);
      DBTYPE_I4:        PInteger(Data)^  := RawToIntDef(Value, Value+Len, 0);
      DBTYPE_R4:        SQLStrToFloatDef(Value, 0, PSingle(Data)^, Len);
      DBTYPE_R8:        SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
      DBTYPE_CY:        SQLStrToFloatDef(Value, 0, PCurrency(Data)^, Len);
      DBTYPE_DATE:      if not TryPCharToDateTime(Value, Len, ConSettings^.WriteFormatSettings, PDateTime(Data)^) then
                          goto Fail;
      //DBTYPE_IDISPATCH	= 9;
      //DBTYPE_ERROR	= 10;
      DBTYPE_BOOL:      PWordBool(Data)^ := StrToBoolEx(Value, Value+Len, True, False);
      //DBTYPE_VARIANT	= 12;
      //DBTYPE_IUNKNOWN	= 13;
      DBTYPE_UI1:       PByte(Data)^    := RawToIntDef(Value, Value+Len, 0);
      DBTYPE_I1:        PShortInt(Data)^:= RawToIntDef(Value, Value+Len, 0);
      DBTYPE_UI2:       PWord(Data)^    := RawToIntDef(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI4:       PCardinal(Data)^:= RawToUInt64Def(Value, Value+Len, 0);
      DBTYPE_UI8:       PUInt64(Data)^  := RawToUInt64Def(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      DBTYPE_GUID:      if Len = SizeOf(TGUID)
                        then Move(Value^, Data^, SizeOf(TGUID))
                        else ValidGUIDToBinary(Value, Data);
      (DBTYPE_GUID or DBTYPE_BYREF): if Len = SizeOf(TGUID) then
                          PPointer(Data)^ := Value
                        else begin
                          ValidGUIDToBinary(Value, PAnsiChar(fByteBuffer));
                          BindList.Put(Index, PGUID(fByteBuffer)^);
                          PPointer(Data)^ := BindList[Index].Value;
                        end;
      DBTYPE_BYTES,
      DBTYPE_STR:       if Bind.cbMaxLen < Len+Byte(Ord(Bind.wType = DBTYPE_STR)) then
                          RaiseExceeded(Index)
                        else begin
                          Move(Value^, Data^, Len+Byte(Ord(Bind.wType = DBTYPE_STR)));
                          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
                        end;
      (DBTYPE_BYTES or DBTYPE_BYREF),
      (DBTYPE_STR or DBTYPE_BYREF): begin
              PPointer(Data)^ := Value;
              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
            end;
      DBTYPE_DBDATE:  if TryPCharToDate(Value, Len, ConSettings^.WriteFormatSettings, D) then begin
                        PDBDate(Data)^.year := D.Year;
                        if D.IsNegative then
                          PDBDate(Data)^.year := -PDBDate(Data)^.year;
                        PDBDate(Data)^.month := D.Month;
                        PDBDate(Data)^.day := D.Day;
                      end else goto Fail;
      DBTYPE_DBTIME:  if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T) then begin
                        PDBTime(Data)^.hour := T.Hour;
                        PDBTime(Data)^.minute := T.Minute;
                        PDBTime(Data)^.second := t.Second;
                      end else goto Fail;
      DBTYPE_DBTIME2: if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T) then begin
                        PDBTIME2(Data)^.hour := T.Hour;
                        PDBTIME2(Data)^.minute := T.Minute;
                        PDBTIME2(Data)^.second := T.Second;
                        PDBTIME2(Data)^.fraction := T.Fractions;
                      end else goto Fail;
      DBTYPE_DBTIMESTAMP:if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                        PDBTimeStamp(Data)^.year := TS.Year;
                        if Ts.IsNegative then
                          PDBTimeStamp(Data)^.year := -PDBTimeStamp(Data)^.year;
                        PDBTimeStamp(Data)^.month := TS.Month;
                        PDBTimeStamp(Data)^.day := TS.Day;
                        PDBTimeStamp(Data)^.hour := TS.Hour;
                        PDBTimeStamp(Data)^.minute := TS.Minute;
                        PDBTimeStamp(Data)^.second := TS.Second;
                        PDBTimeStamp(Data)^.fraction := TS.Fractions;
                      end else goto Fail;
      DBTYPE_DBTIMESTAMPOFFSET:if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                        PDBTimeStamp(Data)^.year := TS.Year;
                        if Ts.IsNegative then
                          PDBTIMESTAMPOFFSET(Data)^.year := -PDBTIMESTAMPOFFSET(Data)^.year;
                        PDBTIMESTAMPOFFSET(Data)^.month := TS.Month;
                        PDBTIMESTAMPOFFSET(Data)^.day := TS.Day;
                        PDBTIMESTAMPOFFSET(Data)^.hour := TS.Hour;
                        PDBTIMESTAMPOFFSET(Data)^.minute := TS.Minute;
                        PDBTIMESTAMPOFFSET(Data)^.second := TS.Second;
                        PDBTIMESTAMPOFFSET(Data)^.fraction := TS.Fractions;
                        PDBTIMESTAMPOFFSET(Data)^.timezone_hour := TS.TimeZoneHour;
                        PDBTIMESTAMPOFFSET(Data)^.timezone_minute := TS.TimeZoneMinute;
                      end else goto Fail;
     else
Fail:    raise CreateOleDBConvertErrror(Index, Bind.wType, stString);
      //DBTYPE_UDT: ;
      //DBTYPE_HCHAPTER:;
      //DBTYPE_PROPVARIANT:;
      //DBTYPE_VARNUMERIC:;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZOleDBPreparedStatement.SetParamCount(NewParamCount: Integer);
var OldParamCount: Integer;
begin
  OldParamCount := BindList.Count;
  if OldParamCount <> NewParamCount then begin
    inherited SetParamCount(NewParamCount);
    SetLength(FDBBindingArray, NewParamCount);
    SetLength(FDBBINDSTATUSArray, NewParamCount);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS" does not seem to be initialized} {$ENDIF} //rolling eyes
procedure TZOleDBPreparedStatement.SetPWideChar(Index: Word; Value: PWideChar;
  Len: Cardinal);
var Bind: PDBBINDING;
  Data: PAnsichar;
  TS: TZTimeStamp;
  T: TZTime absolute TS;
  D: TZDate absolute TS;
label Fail, set_Raw;
begin
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_I4:        PInteger(Data)^  := UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_R4:        SQLStrToFloatDef(Value, 0, PSingle(Data)^, Len);
      DBTYPE_R8:        SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
      DBTYPE_CY:        SQLStrToFloatDef(Value, 0, PCurrency(Data)^, Len);
      DBTYPE_DATE:      if not TryPCharToDateTime(Value, Len, ConSettings^.WriteFormatSettings, PDateTime(Data)^) then
                          goto Fail;
      //DBTYPE_IDISPATCH	= 9;
      //DBTYPE_ERROR	= 10;
      DBTYPE_BOOL:      PWordBool(Data)^ := StrToBoolEx(Value, Value+Len, True, False);
      //DBTYPE_VARIANT	= 12;
      //DBTYPE_IUNKNOWN	= 13;
      DBTYPE_UI1:       PByte(Data)^    := UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_I1:        PShortInt(Data)^:= UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_UI2:       PWord(Data)^    := UnicodeToIntDef(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI4:       PCardinal(Data)^:= UnicodeToUInt64Def(Value, Value+Len, 0);
      DBTYPE_UI8:       PUInt64(Data)^  := UnicodeToUInt64Def(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      DBTYPE_I8:        PInt64(Data)^   := UnicodeToInt64Def(Value, Value+Len, 0);
      DBTYPE_GUID:      ValidGUIDToBinary(Value, Data);
      DBTYPE_GUID or DBTYPE_BYREF: begin
                          ValidGUIDToBinary(Value, PAnsiChar(fByteBuffer));
                          BindList.Put(Index, PGUID(fByteBuffer)^);
                          PPointer(Data)^ := BindList[Index].Value;
                        end;
      DBTYPE_BYTES, (DBTYPE_BYTES or DBTYPE_BYREF): begin
            FRawTemp := UnicodeStringToAscii7(Value, Len);
            goto set_Raw;
          end;
      DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF): begin
            FRawTemp := PUnicodeToRaw(Value, Len, FClientCP);
            Len := Length(FRawTemp);
set_Raw:    if Bind.wType and DBTYPE_BYREF <> 0 then begin
              BindList.Put(Index, stString, FRawTemp, FClientCP); //keep alive
              PPointer(Data)^ := Pointer(FRawTemp);
              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
            end else if Bind.cbMaxLen < Len then
              RaiseExceeded(Index)
            else begin
              Move(Pointer(FRawTemp)^, Data^, Len);
              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
            end;
          end;
      DBTYPE_WSTR:  if Bind.cbMaxLen < Len*2 then
                      RaiseExceeded(Index)
                    else begin
                      Move(Value^, Data^, Len shl 1);
                      PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len shl 1;
                    end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                  PPointer(Data)^ := Value;
                  PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len shl 1;
                end;
      DBTYPE_DBDATE:  if TryPCharToDate(Value, Len, ConSettings^.WriteFormatSettings, D) then begin
                        PDBDate(Data)^.year := D.Year;
                        if D.IsNegative then
                          PDBDate(Data)^.year := -PDBDate(Data)^.year;
                        PDBDate(Data)^.month := D.Month;
                        PDBDate(Data)^.day := D.Day;
                      end else goto Fail;
      DBTYPE_DBTIME:  if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T) then begin
                        PDBTime(Data)^.hour := T.Hour;
                        PDBTime(Data)^.minute := T.Minute;
                        PDBTime(Data)^.second := t.Second;
                      end else goto Fail;
      DBTYPE_DBTIME2: if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T) then begin
                        PDBTIME2(Data)^.hour := T.Hour;
                        PDBTIME2(Data)^.minute := T.Minute;
                        PDBTIME2(Data)^.second := T.Second;
                        PDBTIME2(Data)^.fraction := T.Fractions;
                      end else goto Fail;
      DBTYPE_DBTIMESTAMP:if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                        PDBTimeStamp(Data)^.year := TS.Year;
                        if Ts.IsNegative then
                          PDBTimeStamp(Data)^.year := -PDBTimeStamp(Data)^.year;
                        PDBTimeStamp(Data)^.month := TS.Month;
                        PDBTimeStamp(Data)^.day := TS.Day;
                        PDBTimeStamp(Data)^.hour := TS.Hour;
                        PDBTimeStamp(Data)^.minute := TS.Minute;
                        PDBTimeStamp(Data)^.second := TS.Second;
                        PDBTimeStamp(Data)^.fraction := TS.Fractions;
                      end else goto Fail;
      DBTYPE_DBTIMESTAMPOFFSET:if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                        PDBTimeStamp(Data)^.year := TS.Year;
                        if Ts.IsNegative then
                          PDBTIMESTAMPOFFSET(Data)^.year := -PDBTIMESTAMPOFFSET(Data)^.year;
                        PDBTIMESTAMPOFFSET(Data)^.month := TS.Month;
                        PDBTIMESTAMPOFFSET(Data)^.day := TS.Day;
                        PDBTIMESTAMPOFFSET(Data)^.hour := TS.Hour;
                        PDBTIMESTAMPOFFSET(Data)^.minute := TS.Minute;
                        PDBTIMESTAMPOFFSET(Data)^.second := TS.Second;
                        PDBTIMESTAMPOFFSET(Data)^.fraction := TS.Fractions;
                        PDBTIMESTAMPOFFSET(Data)^.timezone_hour := TS.TimeZoneHour;
                        PDBTIMESTAMPOFFSET(Data)^.timezone_minute := TS.TimeZoneMinute;
                      end else goto Fail;
      else
Fail:     raise CreateOleDBConvertErrror(Index, Bind.wType, stUnicodeString);
      //DBTYPE_UDT: ;
      //DBTYPE_HCHAPTER:;
      //DBTYPE_PROPVARIANT:;
      //DBTYPE_VARNUMERIC:;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a Java <code>raw encoded string</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetRawByteString(Index: Integer;
  const Value: RawByteString);
begin
  BindRaw(Index, Value, FClientCP);
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetShort(Index: Integer; Value: ShortInt);
begin
  InternalBindSInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stShort, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetSmall(Index: Integer; Value: SmallInt);
begin
  InternalBindSInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stSmall, Value);
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetString(Index: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  SetUnicodeString(Index, Value);
  {$ELSE}
  BindRaw(Index, Value, GetW2A2WConversionCodePage(ConSettings));
  {$ENDIF}
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZOleDBPreparedStatement.SetTime(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime);
var Bind: PDBBINDING;
  Data: PAnsichar;
  DT: TDateTime;
label TWConv;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:        PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_DATE:        if not TryTimeToDateTime(Value, PDateTime(Data)^) then
                            raise CreateOleDBConvertErrror(Index, Bind.wType, stTime);
      DBTYPE_DBDATE:      FillChar(Data^, SizeOf(TDBDate), #0);
      DBTYPE_DBTIME:      begin
                            PDBTIME(Data)^.hour := Value.Hour;
                            PDBTIME(Data)^.minute := Value.Minute;
                            PDBTIME(Data)^.second := Value.Second;
                          end;
      DBTYPE_DBTIME2:     begin
                            PDBTIME2(Data)^.hour := Value.Hour;
                            PDBTIME2(Data)^.minute := Value.Minute;
                            PDBTIME2(Data)^.second := Value.Second;
                            PDBTIME2(Data)^.fraction := Value.Fractions;
                          end;
      DBTYPE_DBTIMESTAMP: begin
                            PDBTimeStamp(Data)^.year := cPascalIntegralDatePart.Year;
                            PDBTimeStamp(Data)^.month := cPascalIntegralDatePart.Month;
                            PDBTimeStamp(Data)^.day := cPascalIntegralDatePart.Day;
                            PDBTimeStamp(Data)^.hour := Value.Hour;
                            PDBTimeStamp(Data)^.minute := Value.Minute;
                            PDBTimeStamp(Data)^.second := Value.Second;
                            PDBTimeStamp(Data)^.fraction := Value.Fractions;
                          end;
      DBTYPE_DBTIMESTAMPOFFSET: begin
                            PDBTIMESTAMPOFFSET(Data)^.year := cPascalIntegralDatePart.Year;
                            PDBTIMESTAMPOFFSET(Data)^.month := cPascalIntegralDatePart.Month;
                            PDBTIMESTAMPOFFSET(Data)^.day := cPascalIntegralDatePart.Day;
                            PDBTIMESTAMPOFFSET(Data)^.hour := Value.Hour;
                            PDBTIMESTAMPOFFSET(Data)^.minute := Value.Minute;
                            PDBTIMESTAMPOFFSET(Data)^.second := Value.Second;
                            PDBTIMESTAMPOFFSET(Data)^.fraction := Value.Fractions;
                            PDBTIMESTAMPOFFSET(Data)^.timezone_hour := 0;
                            PDBTIMESTAMPOFFSET(Data)^.timezone_minute := 0;
                          end;
      DBTYPE_WSTR:  if (Bind.cbMaxLen >= 26 ){00.00.00.000#0} or ((Bind.cbMaxLen-2) shr 1 = DBLENGTH(ConSettings.WriteFormatSettings.TimeFormatLen)) then
TWConv:               PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                        TimeToUni(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
                        PWideChar(Data), ConSettings.WriteFormatSettings.TimeFormat, False, Value.IsNegative) shl 1
                      else RaiseExceeded(Index);
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                      PPointer(Data)^ := BindList.AcquireCustomValue(Index, stUnicodeString, 24);
                      Data := PPointer(Data)^;
                      goto TWConv;
                    end;
      else          if TryTimeToDateTime(Value, DT)
                    then InternalBindDbl(Index, stTime, DT)
                    else InternalBindSint(Index, stTime, 1);
    end;
  end else begin//Late binding
    InitDateBind(Index, stTime);
    BindList.Put(Index, Value);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZOleDBPreparedStatement.SetTimestamp(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp);
var Bind: PDBBINDING;
  Data: PAnsichar;
  DT: TDateTime;
label TSWConv;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:        PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_DATE:        if not TryTimeStampToDateTime(Value, PDateTime(Data)^) then
                            raise CreateOleDBConvertErrror(Index, Bind.wType, stTimeStamp);
      DBTYPE_DBDATE:      begin
                            PDBDate(Data).year := Value.Year;
                            if Value.IsNegative then
                              PDBDate(Data).year := -PDBDate(Data).year;
                            PDBDate(Data).month := Value.Month;
                            PDBDate(Data).day := Value.Day;
                          end;
      DBTYPE_DBTIME:      begin
                            PDBTIME(Data)^.hour := Value.Hour;
                            PDBTIME(Data)^.minute := Value.Minute;
                            PDBTIME(Data)^.second := Value.Second;
                          end;
      DBTYPE_DBTIME2:     begin
                            PDBTIME2(Data)^.hour := Value.Hour;
                            PDBTIME2(Data)^.minute := Value.Minute;
                            PDBTIME2(Data)^.second := Value.Second;
                            PDBTIME2(Data)^.fraction := Value.Fractions;
                          end;
      DBTYPE_DBTIMESTAMP: begin
                            PDBTimeStamp(Data)^.year := Value.Year;
                            if Value.IsNegative then
                              PDBTimeStamp(Data)^.year := -PDBTimeStamp(Data)^.year;
                            PDBTimeStamp(Data)^.month := Value.Month;
                            PDBTimeStamp(Data)^.day := Value.Day;
                            PDBTimeStamp(Data)^.hour := Value.Hour;
                            PDBTimeStamp(Data)^.minute := Value.Minute;
                            PDBTimeStamp(Data)^.second := Value.Second;
                            PDBTimeStamp(Data)^.fraction := Value.Fractions;
                          end;
      DBTYPE_DBTIMESTAMPOFFSET: begin
                            PDBTIMESTAMPOFFSET(Data)^.year := Value.Year;
                            if Value.IsNegative then
                              PDBTIMESTAMPOFFSET(Data)^.year := -PDBTIMESTAMPOFFSET(Data)^.year;
                            PDBTIMESTAMPOFFSET(Data).month := Value.Month;
                            PDBTIMESTAMPOFFSET(Data).day := Value.Day;
                            PDBTIMESTAMPOFFSET(Data)^.hour := Value.Hour;
                            PDBTIMESTAMPOFFSET(Data)^.minute := Value.Minute;
                            PDBTIMESTAMPOFFSET(Data)^.second := Value.Second;
                            PDBTIMESTAMPOFFSET(Data)^.fraction := Value.Fractions;
                            PDBTIMESTAMPOFFSET(Data)^.timezone_hour := Value.TimeZoneHour;
                            PDBTIMESTAMPOFFSET(Data)^.timezone_minute := Value.TimeZoneMinute;
                          end;
      DBTYPE_WSTR:  if (Bind.cbMaxLen >= 48){0000-00-00T00.00.00.000#0}  or ((Bind.cbMaxLen-2) shr 1 = DBLENGTH(ConSettings.WriteFormatSettings.DateTimeFormatLen)) then
TSWConv:              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                        DateTimeToUni(Value.Year, Value.Month, Value.Day,
                          Value.Hour, Value.Minute, Value.Second, Value.Fractions, PWideChar(Data),
                          ConSettings.WriteFormatSettings.DateTimeFormat, False, Value.IsNegative) shl 1
                    else RaiseExceeded(Index);
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                      PPointer(Data)^ := BindList.AcquireCustomValue(Index, stUnicodeString, 24);
                      Data := PPointer(Data)^;
                      goto TSWConv;
                    end;
      else          if ZSysUtils.TryTimeStampToDateTime(Value, DT)
                    then InternalBindDbl(Index, stTimeStamp, DT)
                    else InternalBindSInt(Index, stTimeStamp, 1);
    end;
  end else begin//Late binding
    InitDateBind(Index, stTime);
    BindList.Put(Index, Value);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetUInt(Index: Integer; Value: Cardinal);
begin
  InternalBindUInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLongWord, Value);
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZOleDBPreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
{$IFDEF CPU64}
begin
  InternalBindUInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stULong, Value);
{$ELSE}
var Bind: PDBBINDING;
  Data: PAnsichar;
  L: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Value;
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Value;
      DBTYPE_I1:        PShortInt(Data)^ := Value;
      DBTYPE_UI2:       PWord(Data)^ := Value;
      DBTYPE_UI4:       PCardinal(Data)^ := Value;
      DBTYPE_I8:        PInt64(Data)^ := Value;
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AcquireCustomValue(Index, stString, 48); //8Byte align
            Data := PPointer(Data)^; //18.446.744.073.709.551.615
          end else if (Bind.cbMaxLen <= L shl 1) then
            RaiseExceeded(Index);
          IntToUnicode(Value, PWideChar(Data), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1;
        end;
      DBTYPE_NUMERIC: begin
                        PDB_NUMERIC(Data)^.precision := GetOrdinalDigits(Value);
                        PDB_NUMERIC(Data)^.scale := 0;
                        PDB_NUMERIC(Data)^.sign := 1;
                        PUInt64(@PDB_NUMERIC(Data)^.val[0])^ := Value;
                        FillChar(PDB_NUMERIC(Data)^.val[SizeOf(UInt64)], SQL_MAX_NUMERIC_LEN-SizeOf(UInt64), #0);
                      end;
      //DBTYPE_VARNUMERIC:;
      else raise CreateOleDBConvertErrror(Index, Bind.wType, stULong);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(UInt64), DBTYPE_UI8);
    BindList.Put(Index, stULong, P8Bytes(@Value));
  end;
  {$ENDIF}
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Sets the designated parameter to a Object Pascal <code>WideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetUnicodeString(Index: Integer;
  const Value: UnicodeString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then
    if Value <> ''
    then SetPWideChar(Index, Pointer(Value), Length(Value))
    else SetPWideChar(Index, PEmptyUnicodeString, 0)
  else begin
    BindList.Put(Index, stUnicodeString, Value);
    InitVaryBind(Index, (Length(Value)+1) shl 1, DBTYPE_WSTR);
  end;
end;

{**
  Sets the designated parameter to a Java <code>UTF8String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_UTF8STRING}
procedure TZOleDBPreparedStatement.SetUTF8String(Index: Integer;
  const Value: UTF8String);
begin
  BindRaw(Index, Value, zCP_UTF8);
end;
{$ENDIF}

procedure TZOleDBPreparedStatement.SetWord(Index: Integer; Value: Word);
begin
  InternalBindUInt(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stWord, Value);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOleDBPreparedStatement.UnPrepareInParameters;
var FAccessorRefCount: DBREFCOUNT;
begin
  if Assigned(FParameterAccessor) then begin
    //don't forgett to release the Accessor else we're leaking mem on Server!
    FParameterAccessor.ReleaseAccessor(FDBParams.hAccessor, @FAccessorRefCount);
    FDBParams.hAccessor := 0;
    FParameterAccessor := nil;
  end;
  inherited UnPrepareInParameters;
end;

{ TZOleDBStatement }

constructor TZOleDBStatement.Create(const Connection: IZConnection;
  const Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZOleDBCallableStatementMSSQL }

function TZOleDBCallableStatementMSSQL.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  SQLWriter: TZSQLStringWriter;
begin
  //https://docs.microsoft.com/en-us/sql/relational-databases/native-client-ole-db-how-to/results/execute-stored-procedure-with-rpc-and-process-output?view=sql-server-2017
  SQL := '{? = CALL ';
  SQLWriter := TZSQLStringWriter.Create(Length(StoredProcName)+BindList.Count shl 2);
  SQLWriter.AddText(StoredProcName, SQL);
  if BindList.Count > 1 then
    SQLWriter.AddChar(Char('('), SQL);
  for i := 1 to BindList.Count-1 do
    SQLWriter.AddText('?,', SQL);
  if BindList.Count > 1 then
    SQLWriter.ReplaceOrAddLastChar(',', ')', SQL);
  SQLWriter.AddChar('}', SQL);
  SQLWriter.Finalize(SQL);
  FreeAndNil(SQLWriter);
  Result := TZOleDBPreparedStatement.Create(Connection, SQL, Info);
  TZOleDBPreparedStatement(Result).Prepare;
end;

initialization

SetLength(DefaultPreparableTokens, 6);
DefaultPreparableTokens[0].MatchingGroup := 'DELETE';
DefaultPreparableTokens[1].MatchingGroup := 'INSERT';
DefaultPreparableTokens[2].MatchingGroup := 'UPDATE';
DefaultPreparableTokens[3].MatchingGroup := 'SELECT';
DefaultPreparableTokens[4].MatchingGroup := 'CALL';
DefaultPreparableTokens[5].MatchingGroup := 'SET';

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
end.
