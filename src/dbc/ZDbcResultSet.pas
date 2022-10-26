{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

unit ZDbcResultSet;

interface

{$I ZDbc.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBcd,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZDbcIntfs, ZDbcLogging, ZSysUtils, ZCompatibility, ZVariant, ZClasses, ZExceptions;

type
  {** Implements Abstract ResultSet. }
  TZAbstractResultSet = class(TZImmediatelyReleasableObject,
    IImmediatelyReleasable)
  private
    FLastRowNo: Integer;
    FMaxRows: Integer;
    FClosed: Boolean;
    FFetchDirection: TZFetchDirection;
    FFetchSize: Integer;
    FResultSetType: TZResultSetType;
    FResultSetConcurrency: TZResultSetConcurrency;
    FPostUpdates: TZPostUpdatesMode;
    FLocateUpdates: TZLocateUpdatesMode;
    FColumnsInfo: TObjectList;
    FMetadata: TContainedObject;
    FStatement: IZStatement;
    FLastRowFetchLogged: Boolean;
  protected
    FWeakIZResultSetPtr: Pointer; //EH: Remainder for dereferencing on stmt
    //note: while in destruction IZResultSet(Self) has no longer the same pointer address!
    //so we mark the address in constructor
    FRowNo: Integer;
    FRawTemp: RawByteString;
    FUniTemp: UnicodeString;
    LastWasNull: Boolean;
    FOpenLobStreams: TZSortedList;
    FCursorLocation: TZCursorLocation;

    function CreateForwardOnlyException: EZSQLException;
    procedure CheckClosed;
    procedure CheckColumnConvertion(ColumnIndex: Integer; ResultType: TZSQLType);
    procedure CheckBlobColumn(ColumnIndex: Integer);
    procedure Open; virtual;

    procedure AfterClose; virtual;
    /// <summary>Maps the given <c>Metadata</c> column name to its
    ///  <c>Metadata</c> column index. First searches with case-sensivity then,
    ///  if nothing matches, a case.insensitive search is performed.
    /// <param>"ColumnName" the name of the column</param>
    /// <returns>the column index of the given column name. If the ColumnName
    ///  was not found an EZSQLException is thrown.</returns>
    function GetColumnIndex(const ColumnName: string): Integer;
    property RowNo: Integer read FRowNo write FRowNo;
    property LastRowNo: Integer read FLastRowNo write FLastRowNo;
    property MaxRows: Integer read FMaxRows write FMaxRows;
    property Closed: Boolean read FClosed write FClosed;
    property FetchDirection: TZFetchDirection
      read FFetchDirection write FFetchDirection;
    property FetchSize: Integer read FFetchSize write FFetchSize;
    property ResultSetType: TZResultSetType
      read FResultSetType write FResultSetType;
    property ResultSetConcurrency: TZResultSetConcurrency
      read FResultSetConcurrency write FResultSetConcurrency;
    property Statement: IZStatement read FStatement;
    property Metadata: TContainedObject read FMetadata write FMetadata;
    property LastRowFetchLogged: Boolean read FLastRowFetchLogged;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      Metadata: TContainedObject; ConSettings: PZConSettings);
    destructor Destroy; override;
  public
    procedure SetType(Value: TZResultSetType);
    procedure SetConcurrency(Value: TZResultSetConcurrency);

    procedure BeforeClose; virtual;
    /// <summary>Releases this <c>ResultSet</c> object's database and resources
    ///  immediately instead of waiting for this to happen when it is
    ///  automatically closed. Note: A <c>ResultSet</c> object is automatically
    ///  closed by the <c>Statement</c> object that generated it when that
    ///  <c>Statement</c> object is closed, or is used to retrieve the next
    ///  result from a sequence of multiple results. A <c>ResultSet</c> object
    ///  is also automatically closed when it is garbage collected.</summary>
    procedure Close; virtual;
    /// <summary>Resets the Cursor position to beforeFirst, releases server and
    ///  client resources but keeps buffers or Column-Informations alive.</summary>
    procedure ResetCursor; virtual;
    /// <summary>Reports whether the last column read had a value of SQL
    ///  <c>NULL</c>. Note that you must first call one of the <c>getXXX</c>
    ///  methods on a column to try to read its value and then call the method
    ///  <c>wasNull</c> to see if the value read was SQL <c>NULL</c>.</summary>
    /// <returns><c>true</c> if the last column value read was SQL <c>NULL</c>
    ///  and <c>false</c> otherwise.</returns>
    function WasNull: Boolean;
    /// <summary>Indicates whether the this <c>ResultSet</c> is closed.</summary>
    /// <returns><c>true</c> if closed; <c>false</c> otherwise.</returns>
    function IsClosed: Boolean;
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); virtual;
    /// <summary>get the number of columns in this <c>ResultSet</c> interface.</summary>
    /// <returns>the number of columns</returns>
    function GetColumnCount: Integer;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Byte</c> value.The driver will
    ///  try to convert the value if it's not a Byte value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetByte(ColumnIndex: Integer): Byte;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>ShortInt</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetShort(ColumnIndex: Integer): ShortInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Word</c> value.The driver will
    ///  try to convert the value if it's not a Word value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetWord(ColumnIndex: Integer): Word;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>SmallInt</c> value.The driver will
    ///  try to convert the value if it's not a SmallInt value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetSmall(ColumnIndex: Integer): SmallInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBytes</c> value.The driver will
    ///  try to convert the value if it's not a TBytes value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>nil</c>. The value otherwise.</returns>
    function GetBytes(ColumnIndex: Integer): TBytes; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into ASCII.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAsciiStream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  UTF8 encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAnsiStream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of UTF16 characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGNVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into UTF16
    ///  encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUnicodeStream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  UTF8 encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUTF8Stream(ColumnIndex: Integer): TStream;
    /// <summary>Gets the value of a column in the current row as a stream of
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a binary stream of uninterpreted bytes. The
    ///  value can then be read in chunks from the stream. This method is
    ///  particularly suitable for retrieving large <c>LONGVARBINARY</c> values.
    /// </summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetBinaryStream(ColumnIndex: Integer): TStream;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZResultSet</c> object.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>ResultSet</c> object representing the SQL
    ///  <c>ResultSet</c> value in the specified column otherwise</returns>
    function GetResultSet(ColumnIndex: Integer): IZResultSet; virtual;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a TZVariant record.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-Variant</c>. The variable value otherwise.</returns>
    function GetValue(ColumnIndex: Integer): TZVariant;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value. Note this method
    ///  is obsolate. It always calls the GetTime using the TZTime overload.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTime(ColumnIndex: Integer): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Date value. Note this method
    ///  is obsolate. It always calls the GetDate using the TZDate overload.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDate(ColumnIndex: Integer): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Timestamp value. Note this method
    ///  is obsolate. It always calls the GetTimestamp using the TZTimestamp
    ///  overload.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTimestamp(ColumnIndex: Integer): TDateTime; overload;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    /// <summary>Indicates if the value of the designated column in the current
    ///  row of this <c>ResultSet</c> object is Null.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>true</c>. <c>false</c> otherwise.</returns>
    function IsNullByName(const ColumnName: string): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PAnsiChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver try convert the value if it's not a
    ///  raw text value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPAnsiCharByName(const ColumnName: string; out Len: NativeUInt): PAnsiChar;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PWideChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver will try to convert the value if it's
    ///  not a UTF16 text value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in words.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPWideCharByName(const ColumnName: string; out Len: NativeUInt): PWideChar;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>String</c>. This method equals to
    ///  GetUnicodeString on Unicode-Compilers. For Raw-String compilers the
    ///  string encoding is defined by W2A2WEncodingSource of the
    ///  ConnectionSettings record. The driver will try to convert the
    ///  value if it's necessary.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetStringByName(const ColumnName: string): String;
    {$IFNDEF NO_ANSISTRING}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>AnsiString</c>. The driver will
    ///  try to convert the value if it's not a raw value in operating system
    ///  encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetAnsiStringByName(const ColumnName: string): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UTF8String</c>. The driver will
    ///  try to convert the value if it's not a raw value in UTF8 encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetUTF8StringByName(const ColumnName: string): UTF8String;
    {$ENDIF}
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>RawByteString</c>.
    ///  The driver will try to convert the value if it's not a raw value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetRawByteStringByName(const ColumnName: string): RawByteString;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UnicodeString</c> in
    ///  the pascal programming language. The driver will try to convert the
    ///  value if it's not a value in UTF16 encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetUnicodeStringByName(const ColumnName: string): UnicodeString;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Boolean</c> value.The driver will
    ///  try to convert the value if it's not a Boolean value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>False</c>. The value otherwise.</returns>
    function GetBooleanByName(const ColumnName: string): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Byte</c> value.The driver will
    ///  try to convert the value if it's not a Byte value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetByteByName(const ColumnName: string): Byte;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>ShortInt</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetShortByName(const ColumnName: string): ShortInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Word</c> value.The driver will
    ///  try to convert the value if it's not a Byte value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetWordByName(const ColumnName: string): Word;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>SmallInt</c> value.The driver will
    ///  try to convert the value if it's not a SmallInt value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetSmallByName(const ColumnName: string): SmallInt;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Cardinal</c> value.The driver will
    ///  try to convert the value if it's not a Cardinal value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetUIntByName(const ColumnName: string): Cardinal;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Integer</c> value.The driver will
    ///  try to convert the value if it's not a Integer value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetIntByName(const ColumnName: string): Integer;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UInt64</c> value.The driver will
    ///  try to convert the value if it's not a UInt64 value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetULongByName(const ColumnName: string): UInt64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Int64</c> value.The driver will
    ///  try to convert the value if it's not a Int64 value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetLongByName(const ColumnName: string): Int64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Single</c> value.The driver will
    ///  try to convert the value if it's not a Single value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetFloatByName(const ColumnName: string): Single;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Double</c> value.The driver will
    ///  try to convert the value if it's not a Double value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDoubleByName(const ColumnName: string): Double;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Currency</c> value.The driver will
    ///  try to convert the value if it's not a Currency value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetCurrencyByName(const ColumnName: string): Currency;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBCD</c> value.The driver will
    ///  try to convert the value if it's not a TBCD value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Resuls" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-BCD</c>. The value otherwise.</param>
    procedure GetBigDecimalByName(const ColumnName: string; var Result: TBCD);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TGUID</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-UID</c>. The value otherwise.</param>
    procedure GetGUIDByName(const ColumnName: string; var Result: TGUID);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBytes</c> value.The driver will
    ///  try to convert the value if it's not a TBytes value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>nil</c>. The value otherwise.</returns>
    function GetBytesByName(const ColumnName: string): TBytes; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PByte</c> binary reference.
    ///  Live time is per call. It's not guaranteed the address is valid after
    ///  the row position changed, or another column of same row has been
    ///  accessed. It is an error to write into the buffer. The driver will try
    ///  to convert the value if it's not a binary value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetBytesByName(const ColumnName: string; out Len: NativeUInt): PByte; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PByte</c> binary reference.
    ///  Live time is per call. It's not guaranteed the address is valid after
    ///  the row position changed, or another column of same row has been
    ///  accessed. It is an error to write into the buffer. The driver will try
    ///  to convert the value if it's not a binary value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetDateByName(const ColumnName: string): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Date value. Note this method
    ///  is obsolate. It always calls the GetDate using the TZDate overload.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    procedure GetDateByName(const ColumnName: string; var Result: TZDate); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value. Note this method
    ///  is obsolate. It always calls the GetTime using the TZTime overload.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTimeByName(const ColumnName: string): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTime</c>. The value otherwise.</returns>
    procedure GetTimeByName(const ColumnName: string; Var Result: TZTime); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TDateTime</c> value. The driver will
    ///  try to convert the value if it's not a Timestamp value. Note this method
    ///  is obsolate. It always calls the GetTimestamp using the TZTimestamp
    ///  overload.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetTimestampByName(const ColumnName: string): TDateTime; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTimestamp</c> value. The driver
    ///  will try to convert the value if it's not a Timestamp value.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTimestamp</c>. The value otherwise.</param>
    procedure GetTimeStampByName(const ColumnName: string; var Result: TZTimeStamp); overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into ASCII.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAsciiStreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  operating system encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetAnsiStreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of raw characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into raw
    ///  UTF8 encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUTF8StreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a stream of UTF16 characters. The value
    ///  can then be read in chunks from the stream. This method is particularly
    ///  suitable for retrieving large <c>LONGNVARCHAR</c> values. The driver
    ///  will do any necessary conversion from the database format into UTF16
    ///  encoding.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetUnicodeStreamByName(const ColumnName: string): TStream;
    /// <summary>Gets the value of a column in the current row as a stream of
    ///  Gets the value of the designated column in the current row of this
    ///  <c>ResultSet</c> object as a binary stream of uninterpreted bytes. The
    ///  value can then be read in chunks from the stream. This method is
    ///  particularly suitable for retrieving large <c>LONGVARBINARY</c> values.
    /// </summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The stream value otherwise.</returns>
    function GetBinaryStreamByName(const ColumnName: string): TStream;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZBlob</c> object.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>Blob</c> object representing the SQL <c>BLOB</c> value in
    ///  the specified column otherwise</returns>
    function GetBlobByName(const ColumnName: string; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZResultSet</c> object.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>ResultSet</c> object representing the SQL
    ///  <c>ResultSet</c> value in the specified column otherwise</returns>
    function GetResultSetByName(const ColumnName: String): IZResultSet;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a TZVariant record.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-Variant</c>. The variable value otherwise.</returns>
    function GetValueByName(const ColumnName: string): TZVariant;
    /// <summary>Gets the DefaultExpression value of the designated column in
    /// the current row of this <c>ResultSet</c> object as a <c>String</c>.</summary>
    /// <param>"ColumnName" the SQL name of the column. <c>Note</c> the cursor
    ///  must be on a valid position and the Name must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the DefaultExpression value</returns>
    function GetDefaultExpressionByName(const ColumnName: string): string;

    //=====================================================================
    // Advanced features:
    //=====================================================================

    function GetWarnings: EZSQLWarning; virtual;

    procedure ClearWarnings; virtual;
    /// <summary>Not yet implpemented. Gets the name of the SQL cursor used by
    ///  this <c>ResultSet</c> object. In SQL, a result table is retrieved
    ///  through a cursor that is named. The current row of a result set can be
    ///  updated or deleted using a positioned update/delete statement that
    ///  references the cursor name. To insure that the cursor has the proper
    ///  isolation level to support update, the cursor's <c>select</c> statement
    ///  should be of the form 'select for update'. If the 'for update' clause
    ///  is omitted, the positioned updates may fail.
    ///  The ZDBC API supports this SQL feature by providing the name of the
    ///  SQL cursor used by a <c>ResultSet</c> object. The current row of a
    ///  <c>ResultSet</c> object is also the current row of this SQL cursor.
    ///  <B>Note:</B> If positioned update is not supported, a
    ///  <c>EZSQLException</c> is thrown.</summary>
    /// <returns>the SQL name for this <c>ResultSet</c> object's cursor</returns>
    function GetCursorName: String; virtual;
    /// <summary>Retrieves the IZResultSetMetadata interface containing all
    ///  Informations of the <c>ResultSet</c> object's columns.</summary>
    /// <returns>the description interface of this <c>ResultSet</c> object's
    /// columns.</returns>
    function GetMetaData: IZResultSetMetaData; virtual;
    /// <summary>Maps the given <c>Metadata</c> column name to its
    ///  <c>Metadata</c> column index. First searches with case-sensivity then,
    ///  if nothing matches, a case.insensitive search is performed.
    /// <param>"ColumnName" the name of the column</param>
    /// <returns>the column index of the given column name or an
    ///  InvalidDbcIndex if nothing was found</returns>
    function FindColumn(const ColumnName: string): Integer;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; virtual;
    /// <summary>Indicates whether the cursor is before the first row in this
    ///  <c>ResultSet</c> object.</summary>
    /// <returns><c>true</c> if the cursor is before the first row; <c>false</c>
    ///  if the cursor is at any other position or the result set contains no
    ///  rows</returns>
    function IsBeforeFirst: Boolean; virtual;
    /// <summary>Indicates whether the cursor is after the last row in this
    ///  <c>ResultSet</c> object.</summary>
    /// <returns><c>true</c> if the cursor is after the last row; <c>false</c>
    ///  if the cursor is at any other position or the result set contains no
    ///  rows</returns>
    function IsAfterLast: Boolean; virtual;
    /// <summary>Indicates whether the cursor is on the first row of this
    ///  <c>ResultSet</c> object.<summary>
    /// <returns><c>true</c> if the cursor is on the first row;
    ///  <c>false</c> otherwise.</returns>
    function IsFirst: Boolean; virtual;
    /// <summary>Indicates whether the cursor is on the last row of this
    ///  <c>ResultSet</c> object. Note: Calling the method <c>isLast</c> may be
    ///  expensive because the driver might need to fetch ahead one row in order
    ///  to determine whether the current row is the last row in the result set.
    /// </summary>
    /// <returns><c>true</c> if the cursor is on the last row;
    ///  <c>false</c> otherwise.</returns>
    function IsLast: Boolean; virtual;
    /// <summary>Moves the cursor to the top of this <c>ResultSet</c> interface,
    ///  just before the first row.</summary>
    procedure BeforeFirst; virtual;
    /// <summary>Moves the cursor to the end of this <c>ResultSet</c> interface,
    ///  just after the last row. This method has no effect if the result set
    ///  contains no rows.</summary>
    procedure AfterLast; virtual;
    /// <summary>Moves the cursor to the first row in this <c>ResultSet</c>
    ///  object.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if
    ///  there are no rows in the resultset</returns>
    function First: Boolean; virtual;
    /// <summary>Moves the cursor to the last row in this <c>ResultSet</c>
    ///  object.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if
    ///  there are no rows in the result set </returns>
    function Last: Boolean; virtual;
    /// <summary>Retrieves the current row number. The first row is number 1,
    ///  the second number 2, and so on.
    /// <returns>the current row number; <c>0</c> if there is no current row
    /// <returns>
    function GetRow: NativeInt; virtual;
    /// <summary>Moves the cursor to the given row number in
    ///  this <c>ResultSet</c> object. If the row number is positive, the cursor
    ///  moves to the given row number with respect to the beginning of the
    ///  result set. The first row is row 1, the second is row 2, and so on.
    ///  If the given row number is negative, the cursor moves to
    ///  an absolute row position with respect to the end of the result set.
    ///  For example, calling the method <c>absolute(-1)</c> positions the
    ///  cursor on the last row; calling the method <c>absolute(-2)</c>
    ///  moves the cursor to the next-to-last row, and so on. An attempt to
    ///  position the cursor beyond the first/last row in the result set leaves
    ///  the cursor before the first row or after the last row.
    ///  <B>Note:</B> Calling <c>absolute(1)</c> is the same
    ///  as calling <c>first()</c>. Calling <c>absolute(-1)</c>
    ///  is the same as calling <c>last()</c>.</summary>
    /// <param>"Row" the absolute position to be moved.</param>
    /// <returns><c>true</c> if the cursor is on the result set;<c>false</c>
    ///  otherwise</returns>
    function MoveAbsolute(Row: Integer): Boolean; virtual;
    /// <summary>Moves the cursor a relative number of rows, either positive
    ///  or negative. Attempting to move beyond the first/last row in the
    ///  result set positions the cursor before/after the the first/last row.
    ///  Calling <c>relative(0)</c> is valid, but does not change the cursor
    ///  position. Note: Calling the method <c>relative(1)</c> is different
    ///  from calling the method <c>next()</c> because is makes sense to call
    ///  <c>next()</c> when there is no current row, for example, when the
    ///  cursor is positioned before the first row or after the last row of the
    ///  result set. </summary>
    /// <param>"Rows" the relative number of rows to move the cursor.</param>
    /// <returns><c>true</c> if the cursor is on a row;<c>false</c> otherwise
    /// </returns>
    function MoveRelative(Rows: Integer): Boolean; virtual;
    /// <summary>Moves the cursor to the previous row in this <c>ResultSet</c>
    ///  interface. Note: Calling the method <c>previous()</c> is not the same
    ///  as calling the method <c>relative(-1)</c> because it makes sense to
    ///  call<c>previous()</c> when there is no current row.</summary>
    /// <returns><c>true</c> if the cursor is on a valid row; <c>false</c> if it
    ///  is off the result set</returns>
    function Previous: Boolean; virtual;

    //---------------------------------------------------------------------
    // Properties
    //---------------------------------------------------------------------

    /// <summary>Gives a hint as to the direction in which the rows in this
    ///  <c>ResultSet</c> object will be processed. Default is fdForward.
    ///  The initial value is determined by the
    ///  <c>Statement</c> object
    ///  that produced this <c>ResultSet</c> object.
    ///  The fetch direction may be changed at any time.</summary>
    /// <param>"Value" one of <c>fdForward, fdReverse, fdUnknown</c>.</param>
    procedure SetFetchDirection(Direction: TZFetchDirection); virtual;
    function GetFetchDirection: TZFetchDirection; virtual;

    procedure SetFetchSize(Rows: Integer); virtual;
    function GetFetchSize: Integer; virtual;

    function GetType: TZResultSetType; virtual;
    function GetConcurrency: TZResultSetConcurrency; virtual;
    /// <author>EgonHugeist</author>
    /// <summary>Get the cursor type of this resultset</summary>
    /// <returns>the cursortype of this resultset</returns>
    function GetCursorLocation: TZCursorLocation;

    function GetPostUpdates: TZPostUpdatesMode;
    function GetLocateUpdates: TZLocateUpdatesMode;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean; virtual;
    function RowInserted: Boolean; virtual;
    function RowDeleted: Boolean; virtual;

    procedure UpdateDate(ColumnIndex: Integer; const Value: TDateTime); overload;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TDateTime); overload;
    procedure UpdateTimeStamp(ColumnIndex: Integer; const Value: TDateTime); overload;
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TBytes); overload;

    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant);

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    procedure UpdateNullByName(const ColumnName: string);
    procedure UpdateBooleanByName(const ColumnName: string; Value: Boolean);
    procedure UpdateByteByName(const ColumnName: string; Value: Byte);
    procedure UpdateShortByName(const ColumnName: string; Value: ShortInt);
    procedure UpdateWordByName(const ColumnName: string; Value: Word);
    procedure UpdateSmallByName(const ColumnName: string; Value: SmallInt);
    procedure UpdateUIntByName(const ColumnName: string; Value: Cardinal);
    procedure UpdateIntByName(const ColumnName: string; Value: Integer);
    procedure UpdateULongByName(const ColumnName: string; const Value: UInt64);
    procedure UpdateLongByName(const ColumnName: string; const Value: Int64);
    procedure UpdateFloatByName(const ColumnName: string; Value: Single);
    procedure UpdateDoubleByName(const ColumnName: string; const Value: Double);
    procedure UpdateCurrencyByName(const ColumnName: string; const Value: Currency);
    procedure UpdateBigDecimalByName(const ColumnName: string; const Value: TBCD);
    procedure UpdateGUIDByName(const ColumnName: string; const Value: TGUID);
    procedure UpdatePAnsiCharByName(const ColumnName: string; Value: PAnsiChar; var Len: NativeUInt);
    procedure UpdatePWideCharByName(const ColumnName: string; Value: PWideChar; var Len: NativeUInt);
    procedure UpdateStringByName(const ColumnName: string; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiStringByName(const ColumnName: string; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8StringByName(const ColumnName: string; const Value: UTF8String);
    {$ENDIF}
    procedure UpdateRawByteStringByName(const ColumnName: string; const Value: RawByteString);
    procedure UpdateUnicodeStringByName(const ColumnName: string; const Value: UnicodeString);
    procedure UpdateBytesByName(const ColumnName: string; const Value: TBytes);
    procedure UpdateDateByName(const ColumnName: string; const Value: TDateTime); overload;
    procedure UpdateDateByName(const ColumnName: string; const Value: TZDate); overload;
    procedure UpdateTimeByName(const ColumnName: string; const Value: TDateTime); overload;
    procedure UpdateTimeByName(const ColumnName: string; const Value: TZTime); overload;
    procedure UpdateTimestampByName(const ColumnName: string; const Value: TDateTime); overload;
    procedure UpdateTimestampByName(const ColumnName: string; const Value: TZTimeStamp); overload;
    procedure UpdateAsciiStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateUnicodeStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateBinaryStreamByName(const ColumnName: string; const Value: TStream);
    procedure UpdateValueByName(const ColumnName: string; const Value: TZVariant);

    procedure InsertRow; virtual;
    procedure UpdateRow; virtual;
    procedure DeleteRow; virtual;
    procedure RefreshRow; virtual;
    procedure CancelRowUpdates; virtual;
    procedure MoveToInsertRow; virtual;
    procedure MoveToCurrentRow; virtual;

    function CompareRows(Row1, Row2: NativeInt; const ColumnIndices: TIntegerDynArray;
      const CompareFuncs: TCompareFuncs): Integer; virtual;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs; virtual;

    function GetStatement: IZStatement; virtual;

    property ColumnsInfo: TObjectList read FColumnsInfo write FColumnsInfo;
  end;

  TZAbstractReadOnlyResultSet = class(TZAbstractResultSet, IZLoggingObject)
  protected
    FWeakIZLoggingObjectPtr: Pointer; //weak reference to IZLoggingObject intf of Self
  public //getter
    function GetUnicodeString(ColumnIndex: Integer): UnicodeString;
    function GetString(ColumnIndex: Integer): String;
    function GetRawByteString(ColumnIndex: Integer): RawByteString;
    function GetDefaultExpression(ColumnIndex: Integer): string;
  public //setter
    procedure UpdateNull(ColumnIndex: Integer);
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure UpdateByte(ColumnIndex: Integer; Value: Byte);
    procedure UpdateShort(ColumnIndex: Integer; Value: ShortInt);
    procedure UpdateWord(ColumnIndex: Integer; Value: Word);
    procedure UpdateSmall(ColumnIndex: Integer; Value: SmallInt);
    procedure UpdateUInt(ColumnIndex: Integer; Value: Cardinal);
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer);
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64);
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64);
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single);
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double);
    procedure UpdateCurrency(ColumnIndex: Integer; const Value: Currency);
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: TBCD);
    procedure UpdateGUID(ColumnIndex: Integer; const Value: TGUID);
    /// <summary>Updates the designated column with a <c>PAnsiChar</c> buffer
    ///  value. The <c>updateXXX</c> methods are used to update column values in
    ///  the current row or the insert row.  The <c>updateXXX</c> methods do not
    ///  update the underlying database; instead the <c>updateRow</c> or
    ///  <c>insertRow</c> methods are called to update the database.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" an address of the value buffer</param>
    /// <param>"Len" a reference of the buffer Length variable in bytes.</param>
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; var Len: NativeUInt); overload;
    /// <summary>Updates the designated column with a <c>PWideChar</c> buffer
    ///  value. The <c>updateXXX</c> methods are used to update column values in
    ///  the current row or the insert row.  The <c>updateXXX</c> methods do not
    ///  update the underlying database; instead the <c>updateRow</c> or
    ///  <c>insertRow</c> methods are called to update the database.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" an address of the value buffer</param>
    /// <param>"Len" a reference of the buffer Length variable in words.</param>
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar; var Len: NativeUInt); overload;
    procedure UpdateString(ColumnIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiString(ColumnIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8String(ColumnIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure UpdateRawByteString(ColumnIndex: Integer; const Value: RawByteString);
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: UnicodeString);
    procedure UpdateBytes(ColumnIndex: Integer; Value: PByte; var Len: NativeUInt); overload;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TZDate); overload;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TZTime); overload;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TZTimeStamp); overload;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream);
    /// <summary>Updates the designated column with a binary stream value.
    ///  The <c>updateXXX</c> methods are used to update column values in the
    ///  current row or the insert row. The <c>updateXXX</c> methods do not
    ///  update the underlying database; instead the <c>updateRow</c> or
    ///  <c>insertRow</c> methods are called to update the database. </summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the new column value</param>
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream);
    procedure UpdateLob(ColumnIndex: Integer; const Value: IZBlob);
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant);
    procedure UpdateDefaultExpression(ColumnIndex: Integer; const Value: string);
  public
    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent;
  public
    procedure AfterConstruction; override;
  end;

  //EH: sequential stream with faket interface implementationm, not refcounted
  //aim is to have a object which can handle connection loss
  TZImmediatelyReleasableLobStream = class(TStream, IImmediatelyReleasable)
  protected
    FCodePage: Word;  //in case of binary strem the CP is zero
    FReleased: Boolean;
    FOwner: IImmediatelyReleasable;
    FOwnerLob: IZLob; //keep lob alive while stream is underway
    FOpenLobStreams: TZSortedList;
  public
    constructor Create(const OwnerLob: IZLob; const Owner: IImmediatelyReleasable; const OpenLobStreams: TZSortedList);
    destructor Destroy; override;
  public //IImmediatelyReleasable
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); virtual;
    function GetConSettings: PZConSettings;
  protected //implement fakes IInterface
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF not defined(MSWINDOWS) and defined(FPC)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF not defined(MSWINDOWS) and defined(FPC)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF not defined(MSWINDOWS) and defined(FPC)}cdecl{$ELSE}stdcall{$IFEND};
  end;

  { TZAbstractLob }

  {** EH: implements an abstract large object descriptor }
  TZAbstractLob = class(TInterfacedObject{, IZLob})
  protected
    FWeakRefOfBlob, FWeakRefOfClob: Pointer;
    FColumnCodePage, RawControlsCP: Word;
    FIsUpdated, FReleased: Boolean;
    FLobStreamMode: TZLobStreamMode;
    FConSettings: PZConSettings;
    FOnUpdateHandler: TOnLobUpdate;
    FField: NativeInt;
    FOpenLobStreams: TZSortedList;
    //FRawTemp: RawByteString; //place holder for PAnsiChar
    //FUniTemp: UnicodeString; //place holder for PWideChar
    function CreateBinaryException: EZSQLException;
    /// <summary>Creates a lob stream</summary>
    /// <param>"CodePage" the lob codepage. 0 means it's a binary stream</param>
    /// <param>"LobStreamMode" the stream mode on open the lob. It's one of
    ///  <c>lsmRead, lsmWrite, lsmReadWrite</c></param>
    /// <returns>a TStream object</returns>
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; virtual; abstract;
  public //the string implementation
    function GetRawByteString(CodePage: Word): RawByteString;
    procedure SetRawByteString(Const Value: RawByteString; const CodePage: Word);
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString;
    procedure SetAnsiString(Const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String;
    procedure SetUTF8String(Const Value: UTF8String);
    {$ENDIF}
    procedure SetUnicodeString(const Value: UnicodeString);
    function GetUnicodeString: UnicodeString;
  public //the buffer implementations
    procedure SetPAnsiChar(Buffer: PAnsiChar; CodePage: Word; Len: NativeUInt); virtual; abstract;
    procedure SetPWideChar(Buffer: PWideChar; Len: NativeUInt); virtual; abstract;
  public
    function GetStream(CodePage: Word): TStream; overload;
  public //the streamed clob implementation
    procedure SetStream(const Value: TStream; CodePage: Word); overload; virtual; abstract;
    function GetStream: TStream; overload;
    function GetRawByteStream: TStream;
    function GetAnsiStream: TStream;
    function GetUTF8Stream: TStream;
    function GetUnicodeStream: TStream;
  public //bin implementation
    procedure SetStream(const Value: TStream); overload;
    procedure SetString(const Value: RawByteString);
    procedure SetBytes(const Value: TBytes);
  public
    constructor Create(ColumnCodePage: Word; const OpenLobStreams: TZSortedList);
    procedure AfterConstruction; override;
  public //might be obsolete in future
    function IsClob: Boolean;
    function IsCached:  Boolean;   // mjf:
    function Length: Integer; virtual; abstract;
  public
    function IsEmpty: Boolean; virtual; abstract;
    function IsUpdated: Boolean; virtual;
    procedure SetUpdated(Value: Boolean); virtual;
    procedure Clear; virtual; abstract;
    procedure Open(LobStreamMode: TZLobStreamMode); virtual;
  public //add an update notify handle for the ResultSets oslt
    procedure SetOnUpdateHandler(Handler: TOnLobUpdate; AField: NativeInt);
  public //bin implementation
    function GetString: RawByteString;
  end;

  {** implements a Lob descriptor object for streamed lobs }
  TZAbstractStreamedLob = class(TZAbstractLob{, IZLob clear is not introduced})
  public //the buffer implementations
    function GetPAnsiChar(CodePage: Word; var ConversionBuf: RawByteString; out Len: NativeUInt): PAnsiChar;
    procedure SetPAnsiChar(Buffer: PAnsiChar; CodePage: Word; Len: NativeUInt); override;
    function GetPWideChar(var ConversionBuf: UnicodeString; Out Len: NativeUint): PWideChar;
    procedure SetPWideChar(Buffer: PWideChar; Len: NativeUInt); override;
    procedure SetCodePageTo(Value: Word);
  public //the streamed implementation
    procedure SetStream(const Value: TStream; CodePage: Word); overload; override;
  public //bin implementation
    function GetBytes: TBytes;
    function GetBuffer(var LocalBuffer: RawByteString; out Len: NativeUInt): Pointer;
    procedure SetBuffer(Buffer: Pointer; Len: NativeUInt);
  end;


  PZVarLenData = ^TZVarLenData;
  TZVarLenData = packed record //that's how the memref works in TZRowAccessor
    Len: Cardinal;
    Data: array[0..0] of Byte;
  end;

  PZVarLenDataRef = ^TZVarLenDataRef;
  TZVarLenDataRef = packed record //that's the local field representation in the rowbuffer
    IsNotNull: Byte;
    VarLenData: PZVarLenData;  //if is null or len is zero the ptr^ is nil!
  end;

  { implements a Memory Referenced lob descriptor }

  { TZVarLenDataRefLob }

  TZVarLenDataRefLob = class(TZAbstractLob, IZLob)
  protected
    FDataRefAddress: PZVarLenDataRef;
    FLobStreamMode: TZLobStreamMode;
    FCapacity: NativeUint;
    procedure InternalClear;
    procedure SetCapacity(Value: NativeUint);
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public //the string implementation
    procedure SetCodePageTo(Value: Word);
  public //the buffer implementations
    function GetPAnsiChar(CodePage: Word; var Buffer: RawByteString; out Len: NativeUInt): PAnsiChar;
    procedure SetPAnsiChar(Buffer: PAnsiChar; CodePage: Word; Len: NativeUInt); override;
    function GetPWideChar(var Buffer: UnicodeString; out Len: NativeUInt): PWideChar;
    procedure SetPWideChar(Buffer: PWideChar; Len: NativeUInt); override;
  public
    constructor Create(CodePage: Word; DataRefAddress: PZVarLenDataRef; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList);
  public //the streamed implementation
    procedure SetStream(const Value: TStream; CodePage: Word); override;
  public //bin implementation
    function GetBytes: TBytes;
    function GetBuffer(var LocalBuffer: RawByteString; Out Len: NativeUInt): Pointer;
    procedure SetBuffer(Buffer: Pointer; Len: NativeUInt);
  public //might be obsolete
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function Length: Integer; override;
  end;

  TZCodePageFixedVarLenDataRefLob = class(TZVarLenDataRefLob)
  public
    procedure SetCodePageTo(Value: Word);
  end;

  { TZLocalMemBLob }

  {** EH: implements an inmemory binary large object descriptor }
  TZLocalMemBLob = class(TZCodePageFixedVarLenDataRefLob, IZBlob)
  private
    FDataRef: TZVarLenDataRef;
  public
    function Clone(LobStreamMode: TZLobStreamMode): IZBlob;
  public
    constructor Create(const OpenLobStreams: TZSortedList = nil);
    constructor CreateWithData(Data: Pointer; BytesCount: NativeUInt; const OpenLobStreams: TZSortedList = nil);
    constructor CreateWithStream(Value: TZImmediatelyReleasableLobStream;
      FreeStream: Boolean; const OpenLobStreams: TZSortedList = nil); overload;
    constructor CreateWithStream(Value: TStream; const OpenLobStreams: TZSortedList = nil); overload;
    constructor CreateFromBlob(const Value: IZBlob; const OpenLobStreams: TZSortedList = nil);
    destructor Destroy; override;
  end;

  TZAbstractBLob = TZLocalMemBLob;

  TZCodePageVariableVarLenDataRefLob = class(TZVarLenDataRefLob)
  public
    procedure SetCodePageTo(Value: Word);
  end;

  { TZLocalMemCLob }

  {** EH: implements an inmemory character large object descriptor }
  TZLocalMemCLob = class(TZCodePageVariableVarLenDataRefLob, IZBlob, IZClob)
  private
    FDataRef: TZVarLenDataRef;
  public
    function Clone(LobStreamMode: TZLobStreamMode): IZBlob;
  public
    constructor Create(CodePage: Word; ConSettings: PZConSettings; const OpenLobStreams: TZSortedList = nil);
    constructor CreateWithData(Data: Pointer; BytesCount: NativeUInt; CodePage: Word; ConSettings: PZConSettings; const OpenLobStreams: TZSortedList = nil); overload;
    constructor CreateWithData(Data: PWideChar; WordCount: NativeUint; ConSettings: PZConSettings; const OpenLobStreams: TZSortedList = nil); overload;
    constructor CreateWithStream(Value: TZImmediatelyReleasableLobStream;
      ColumnCodePage: Word; FreeStream: Boolean; const OpenLobStreams: TZSortedList = nil); overload;
    constructor CreateWithStream(Value: TStream; ColumnCodePage: Word; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList = nil); overload;
    constructor CreateFromClob(const Value: IZClob; ColumnCodePage: Word; ConSettings: PZConSettings; const OpenLobStreams: TZSortedList = nil);
    destructor Destroy; override;
  end;
  TZAbstractCLob = TZLocalMemCLob;

  {** EH: implements a readonly resultset for raw encoded protocols }
  TZAbstractReadOnlyResultSet_A = class(TZAbstractReadOnlyResultSet)
  public
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    {$ENDIF}
  end;

  {** EH: implements a codepage conversion stream helper}
  TZCodePageConversionStream = class(TMemoryStream)
  private
    FNativeCodePage, FCurrentCodePage: Word;
    FOwnerStream: TStream;
    FConSettings: PZConSettings;
    FUpdated: Boolean;
    FInConstructionState: Boolean;
    FOpenLobStreams: TZSortedList;
  protected
    function Realloc(var NewCapacity: {$IFDEF FPC}PtrInt{$ELSE}{$IFDEF MEMORYSTREAM_REALLOC_NATIVEINT}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF}): Pointer; override;
  protected
    procedure FlushMemToStream(Buf: Pointer; Len: NativeUInt; Stream: TStream); virtual;
    procedure ReadStreamToMem(var Buf: Pointer; var Len: NativeUint; Stream: TStream); virtual;
  public
    constructor Create(const Owner: TStream; SourceCodePage, DestCodePage: Word;
      ConSettings: PZConSettings; const OpenLobStreams: TZSortedList);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  {** EH: implements a codepage conversion stream helper to read data only}
  TZCodePageConversionROStream = class(TZCodePageConversionStream)
  protected
    function Realloc(var NewCapacity: {$IFDEF FPC}PtrInt{$ELSE}{$IFDEF MEMORYSTREAM_REALLOC_NATIVEINT}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF}): Pointer; override;
  end;

  TZVarVarLenDataRefStream = class(TMemoryStream)
  private
    FVarLenDataRef: PZVarLenDataRef;
    FColumnCodePage: Word;
    FLobStreamMode: TZLobStreamMode;
    FUpdated: Boolean;
    FOwner: IZLob; //this keeps data alive while the stream is underway
    FOpenLobStreams: TZSortedList;
  protected
    function Realloc(var NewCapacity: {$IFDEF FPC}PtrInt{$ELSE}{$IFDEF MEMORYSTREAM_REALLOC_NATIVEINT}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF}): Pointer; override;
  public
    Constructor Create(const Owner: IZLob; CodePage: Word;
      VarLenDataRef: PZVarLenDataRef; LobStreamMode: TZLobStreamMode;
      const OpenLobStreams: TZSortedList);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  {** EH: implements a readonly stream
    use it for MySQL(store_result) or Postgres <> non single_row_mode
    in non updatable mode only }
  TZReadOnlyDataRefStream = class(TMemoryStream)
  private
    FOwner: IZLob; //this keeps data alive while the stream is underway
    FOpenLobStreams: TZSortedList;
  public
    Constructor Create(const Owner: IZLob; Data: Pointer; DataLen: NativeUInt;
      const OpenLobStreams: TZSortedList);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize: Longint); override;
  end;

  TZMemoryReferencedLob = class(TZAbstractLob, IZLob)
  private
    FMemoryReference: Pointer;
    FDataLen: NativeUInt;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public
    constructor CreateWithData(Buffer: Pointer; DataLen: NativeUInt;
      ColumnCodePage: Word; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList);
  public
    function GetUnicodeString: UnicodeString;
  public //the buffer implementations
    function GetPAnsiChar(CodePage: Word; var Buffer: RawByteString; out Len: NativeUInt): PAnsiChar;
    procedure SetPAnsiChar(Buffer: PAnsiChar; CodePage: Word; Len: NativeUInt); override;
    function GetPWideChar(var Buffer: UnicodeString; out Len: NativeUInt): PWideChar;
    procedure SetPWideChar(Buffer: PWideChar; Len: NativeUInt); override;
  public //bin implementation
    function GetBytes: TBytes;
    function GetBuffer(var LocalBuffer: RawByteString; Out Len: NativeUInt): Pointer;
    procedure SetBuffer(Buffer: Pointer; Len: NativeUInt);
  public //the streamed clob implementation
    procedure SetStream(const Value: TStream; CodePage: Word); overload; override;
  public //might be obsolete in future
    function Length: Integer; override;
  public
    function IsEmpty: Boolean; override;
    procedure Clear; override;
    procedure Open(LobStreamMode: TZLobStreamMode); override;
    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
    procedure SetCodePageTo(Value: Word);
  end;

  TZMemoryReferencedBLob = class(TZMemoryReferencedLob, IZBlob)
  public
    constructor CreateWithData(Buffer: Pointer; DataLen: NativeUInt; const OpenLobStreams: TZSortedList);
  end;

  TZMemoryReferencedCLob = class(TZMemoryReferencedLob, IZBlob, IZClob);


{$IF defined(USE_SYNCOMMONS) or defined(MORMOT2)}
const
  JSONBool: array[Boolean] of ShortString = ('false', 'true');
{$IFEND}

implementation

uses ZMessages, ZDbcUtils, ZDbcResultSetMetadata, ZEncoding, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}, Math;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // parameters not used intentionally
function CompareNothing(const Null1, Null2: Boolean; const V1, V2): Integer; //emergency exit for complex types we can't sort quickly like arrays, ResultSet ...
begin
  Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function CompareBoolean_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VBoolean)-Ord(TZVariant(V2).VBoolean);
end;

function CompareBoolean_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBoolean_Asc(Null1, Null2, V1, V2);
end;

function CompareInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VInteger > TZVariant(V2).VInteger)-Ord(TZVariant(V1).VInteger < TZVariant(V2).VInteger);
end;

function CompareInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareUInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VUInteger > TZVariant(V2).VUInteger)-Ord(TZVariant(V1).VUInteger < TZVariant(V2).VUInteger);
end;

function CompareUInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareDouble_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(CompareValue(TZVariant(V1).VDouble, TZVariant(V2).VDouble));
end;

function CompareDouble_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDouble_Asc(Null1, Null2, V1, V2);
end;

function CompareCurrency_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VCurrency > TZVariant(V2).VCurrency)-Ord(TZVariant(V1).VCurrency < TZVariant(V2).VCurrency);
end;

function CompareCurrency_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareCurrency_Asc(Null1, Null2, V1, V2);
end;

function CompareBigDecimal_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := ZBCDCompare(PBCD(@TZVariant(V1).VBigDecimal.Precision)^, PBCD(@TZVariant(V2).VBigDecimal.Precision)^);
end;

function CompareBigDecimal_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBigDecimal_Asc(Null1, Null2, V1, V2);
end;

function CompareDateTime_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := ZCompareDateTime(TZVariant(V1).VDateTime, TZVariant(V2).VDateTime);
end;

function CompareTimeStamp_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := ZCompareTimeStamp(TZVariant(V1).VTimeStamp, TZVariant(V2).VTimeStamp);
end;

function CompareDateTime_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDateTime_Asc(Null1, Null2, V1, V2);
end;

function CompareTimeStamp_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareTimeStamp_Asc(Null1, Null2, V1, V2);
end;

function CompareDate_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := ZCompareDate(TZVariant(V1).VDate, TZVariant(V2).VDate);
end;

function CompareDate_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDate_Asc(Null1, Null2, V1, V2);
end;

function CompareTime_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := ZCompareTime(TZVariant(V1).VTime, TZVariant(V2).VTime);
end;

function CompareTime_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDateTime_Asc(Null1, Null2, V1, V2);
end;

function CompareBytes_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1 else
  begin
    Result := Length(TZVariant(V1).VRawByteString) - Length(TZVariant(V2).VRawByteString); //overflow save!
    if Result = 0 then
      Result := ZMemLComp(Pointer(TZVariant(V1).VRawByteString), Pointer(TZVariant(V2).VRawByteString),
        Length(TZVariant(V1).VRawByteString));
  end;
end;

function CompareBytes_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBytes_Asc(Null1, Null2, V1, V2);
end;

{$IFNDEF WITH_USC2_ANSICOMPARESTR_ONLY}
{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "ReadInterbase6Number" marked as inline is not inlined}{$ENDIF}
function CompareRawByteString_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}
    AnsiStrComp(PAnsiChar(TZVariant(V1).VRawByteString), PAnsiChar(TZVariant(V2).VRawByteString));
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}

function CompareRawByteString_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareRawByteString_Asc(Null1, Null2, V1, V2);
end;
{$ENDIF}

function CompareUnicodeString_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  {$IFDEF UNICODE}
  else Result := AnsiCompareStr(TZVariant(V1).VUnicodeString, TZVariant(V2).VUnicodeString);
  {$ELSE}
  else Result := WideCompareStr(TZVariant(V1).VUnicodeString, TZVariant(V2).VUnicodeString);
  {$ENDIF}
end;

function CompareUnicodeString_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUnicodeString_Asc(Null1, Null2, V1, V2);
end;

{ TZLocalMemCLob }

function TZLocalMemCLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var P: Pointer;
  L: NativeUInt;
  R: RawByteString;
begin
  R := '';
  P := GetBuffer(R, L);
  Result := TZLocalMemCLob.CreateWithData(P, L, FColumnCodePage, FConSettings, FOpenLobStreams);
  Result.Open(LobStreamMode)
end;

constructor TZLocalMemCLob.Create(CodePage: Word; ConSettings: PZConSettings;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(CodePage, @FDataRef, ConSettings, OpenLobStreams);
end;

constructor TZLocalMemCLob.CreateFromClob(const Value: IZClob;
  ColumnCodePage: Word; ConSettings: PZConSettings; const OpenLobStreams: TZSortedList);
var Stream: TStream;
begin
  Create(ColumnCodePage, ConSettings, OpenLobStreams);
  FColumnCodePage := ColumnCodePage;
  FConSettings := ConSettings;
  Stream := Value.GetStream(ColumnCodePage);
  if Stream = nil then
    Exit;
  FDataRef.IsNotNull := 1;
  try
    SetCapacity(Stream.Size);
    Stream.Read(FDataRef.VarLenData.Data, Stream.Size);
  finally
    Stream.Free;
  end;
end;

constructor TZLocalMemCLob.CreateWithData(Data: PWideChar;
  WordCount: NativeUint; ConSettings: PZConSettings; const OpenLobStreams: TZSortedList);
begin
  CreateWithData(Data, WordCount shl 1, zCP_UTF16, ConSettings, OpenLobStreams);
end;

constructor TZLocalMemCLob.CreateWithData(Data: Pointer;
  BytesCount: NativeUInt; CodePage: Word; ConSettings: PZConSettings;
  const OpenLobStreams: TZSortedList);
begin
  Create(CodePage, ConSettings, OpenLobStreams);
  if Data <> nil then begin
     FDataRef.IsNotNull := 1;
     GetMem(FDataRef.VarLenData, SizeOf(Cardinal)+BytesCount+1+Byte(CodePage = zCP_UTF16));
     Move(Data^, FDataRef.VarLenData.Data, BytesCount);
     if CodePage = zCP_UTF16
     then PWord(PAnsiChar(@FDataRef.VarLenData.Data)+BytesCount)^ := 0
     else PByte(PAnsiChar(@FDataRef.VarLenData.Data)+BytesCount)^ := 0;
     FDataRef.VarLenData.Len := BytesCount;
  end;
end;

constructor TZLocalMemCLob.CreateWithStream(Value: TStream;
  ColumnCodePage: Word; ConSettings: PZConSettings;
  const OpenLobStreams: TZSortedList);
const
  MaxBufSize = $F000;
var Count, BufSize, ReadBytes: LongInt;
  Buf: PAnsichar;
begin
  Create(ColumnCodePage, ConSettings, OpenLobStreams);
  if Value = nil then Exit;
  Count := Value.Size;
  FDataRef.IsNotNull := 1;
  SetCapacity(Count);
  if Count > MaxBufSize
  then BufSize := MaxBufSize
  else BufSize := Count;
  Buf := PAnsiChar(@FDataRef.VarLenData.Data);
  while Count <> 0 do begin
    ReadBytes := Value.Read(Buf^, BufSize);
    Dec(Count, ReadBytes);
    Inc(Buf, ReadBytes);
    if Count < BufSize then
      BufSize := Count;
  end;
  { set trailing terms for the postgres server only (uses StrLen() & ignores the length attibutes)}
  if ColumnCodePage = zCP_UTF16
  then PWord(Buf)^ := 0
  else PByte(Buf)^ := 0;
end;

destructor TZLocalMemCLob.Destroy;
begin
  if FDataRef.VarLenData <> nil then begin
    FreeMem(FDataRef.VarLenData);
    FDataRef.VarLenData := nil;
  end;
  inherited;
end;

constructor TZLocalMemCLob.CreateWithStream(
  Value: TZImmediatelyReleasableLobStream; ColumnCodePage: Word;
  FreeStream: Boolean; const OpenLobStreams: TZSortedList);
begin
  CreateWithStream(Value, ColumnCodePage, Value.FOwner.GetConSettings, OpenLobStreams);
  if FreeStream then Value.Free;
end;

{ TZLocalMemBLob }

function TZLocalMemBLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var P: Pointer;
  L: NativeUInt;
  R: RawByteString;
begin
  if LobStreamMode = lsmWrite then begin
    P := nil;
    L := 0;
  end else begin
    R := '';
    P := GetBuffer(R, L);
  end;
  Result := TZLocalMemBLob.CreateWithData(P, L, FOpenLobStreams);
end;

{ TZAbstractResultSet }

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param Metadata a resultset metadata object.
  @param ConSettings the pointer to Connection Settings record
}
constructor TZAbstractResultSet.Create(const Statement: IZStatement; const SQL: string;
  Metadata: TContainedObject; ConSettings: PZConSettings);
var
  DatabaseMetadata: IZDatabaseMetadata;
  RS: IZResultSet;
begin
  Self.ConSettings := ConSettings;
  LastWasNull := True;
  FRowNo := 0;
  FLastRowNo := 0;
  FClosed := True;
  FOpenLobStreams := TZSortedList.Create;

  { the constructor keeps the refcount to 1}
  QueryInterface(IZResultSet, RS);
  FWeakIZResultSetPtr := Pointer(RS); //Remainder for unregister on stmt!
  RS := nil;
  if Statement = nil then begin
    FResultSetType := rtForwardOnly;
    FResultSetConcurrency := rcReadOnly;
    FPostUpdates := poColumnsAll;
    FLocateUpdates := loWhereAll;
    FMaxRows := 0;
  end else begin
    FFetchDirection := Statement.GetFetchDirection;
    FFetchSize := Statement.GetFetchSize;
    FResultSetType := Statement.GetResultSetType;
    FResultSetConcurrency := Statement.GetResultSetConcurrency;
    FPostUpdates := Statement.GetPostUpdates;
    FLocateUpdates := Statement.GetLocateUpdates;
    FStatement := Statement;
    FMaxRows := Statement.GetMaxRows;
  end;

  if Metadata = nil then begin
    if Statement <> nil
    then DatabaseMetadata := GetStatement.GetConnection.GetMetadata
    else DatabaseMetadata := nil;
    FMetadata := TZAbstractResultSetMetadata.Create(DatabaseMetadata, SQL, Self);
   end else
    FMetadata := Metadata;

  FColumnsInfo := TObjectList.Create(True); //Free the MemoryLeaks of TZColumnInfo
end;

{**
  Creates an operation is not allowed in FORWARD ONLY mode exception.
}
function TZAbstractResultSet.CreateForwardOnlyException: EZSQLException;
begin
  Result := EZSQLException.Create(SOperationIsNotAllowed1);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractResultSet.Destroy;
begin
  if not FClosed then
    Close;
  FreeAndNil(FMetadata);
  FreeAndNil(FColumnsInfo);
  FreeAndNil(FOpenLobStreams);
  inherited Destroy;
end;

{**
  Checks if result set is open and operation is allowed.
}
procedure TZAbstractResultSet.CheckClosed;
begin
  if FClosed then
    raise EZSQLException.Create(SOperationIsNotAllowed4);
end;

{**
  Checks is the column convertion from one type to another type allowed.
  @param ColumnIndex an index of column.
  @param ResultType a requested data type.
}
procedure TZAbstractResultSet.CheckColumnConvertion(ColumnIndex: Integer;
  ResultType: TZSQLType);
var
  InitialType: TZSQLType;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  if (Metadata = nil) or (ColumnIndex < FirstDbcIndex) or
     (ColumnIndex > Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  InitialType := Metadata.GetColumnType(ColumnIndex);
  if not CheckConvertion(InitialType, ResultType) then
    raise EZSQLException.Create(Format(SConvertionIsNotPossible, [ColumnIndex,
       DefineColumnTypeName(InitialType), DefineColumnTypeName(ResultType)]));
end;

{**
  Checks for blob expected column.
  @param ColumnIndex an index of column.
}
procedure TZAbstractResultSet.CheckBlobColumn(ColumnIndex: Integer);
var
  InitialType: TZSQLType;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  if (Metadata = nil) or (ColumnIndex < FirstDbcIndex) or
     (ColumnIndex > Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  InitialType := Metadata.GetColumnType(ColumnIndex);
  if not (InitialType in [stAsciiStream, stBinaryStream, stUnicodeStream]) then
    raise EZSQLException.Create(Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(InitialType)]));
end;

{**
  Set the concurrency mode of this <code>ResultSet</code> object.
  The concurrency used is determined by the
  <code>Statement</code> object that created the result set.

  @param the concurrency type, either <code>CONCUR_READ_ONLY</code>
    or <code>CONCUR_UPDATABLE</code>
}
procedure TZAbstractResultSet.SetConcurrency(Value: TZResultSetConcurrency);
begin
  ResultSetConcurrency := Value;
end;

{**
  Set the type of this <code>ResultSet</code> object.
  The type is determined by the <code>Statement</code> object
  that created the result set.

  @param <code>TYPE_FORWARD_ONLY</code>,
    <code>TYPE_SCROLL_INSENSITIVE</code>,
    or <code>TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractResultSet.SetType(Value: TZResultSetType);
begin
  ResultSetType := Value;
end;

{**
  Opens this recordset.
}
procedure TZAbstractResultSet.Open;
begin
  FClosed := False;
end;

procedure TZAbstractResultSet.ResetCursor;
begin
  if not FClosed then begin
    if Assigned(Statement){virtual RS ! }  then begin
      FFetchSize := Statement.GetFetchSize;
      FPostUpdates := Statement.GetPostUpdates;
      FLocateUpdates := Statement.GetLocateUpdates;
      FMaxRows := Statement.GetMaxRows;
    end;
    FRowNo := 0;
    FLastRowNo := 0;
    LastWasNull := True;
    FLastRowFetchLogged := False;
  end;
end;

procedure TZAbstractResultSet.Close;
var RefCountAdded: Boolean;
begin
  if FOpenLobStreams.Count > 0 then
    raise EZSQLException.Create('close all lob streams before closing the resultset');
  if not Closed then begin
    BeforeClose;
    FClosed := True;
    RefCountAdded := False;
    try
      if (FStatement <> nil) then begin
        if (RefCount = 1) then begin
          _AddRef;
          RefCountAdded := True;
        end;
        FStatement.FreeOpenResultSetReference(IZResultSet(FWeakIZResultSetPtr));
        FStatement := nil;
      end;
      AfterClose;
    finally
      if RefCountAdded then begin
        if (RefCount = 1) then
          DriverManager.AddGarbage(Self);
        _Release;
      end;
    end;
  end;
end;

function TZAbstractResultSet.WasNull: Boolean;
begin
  Result := LastWasNull;
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

function TZAbstractResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := IZResultSet(FWeakIZResultSetPtr).GetUInt(ColumnIndex);
end;

function TZAbstractResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := IZResultSet(FWeakIZResultSetPtr).GetInt(ColumnIndex);
end;

function TZAbstractResultSet.GetWord(ColumnIndex: Integer): Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  Result := Word(IZResultSet(FWeakIZResultSetPtr).GetInt(ColumnIndex));
end;

function TZAbstractResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  Result := IZResultSet(FWeakIZResultSetPtr).GetInt(ColumnIndex);
end;

function TZAbstractResultSet.GetAsciiStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
    Clob: IZCLob;
    CP: Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  Result := nil;
  if IZResultSet(FWeakIZResultSetPtr).IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) then
      Blob.QueryInterface(IZCLob, Clob);
      if Clob = nil then
        Result := (Blob).GetStream
      else begin
        CP := TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF _GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage;
        if CP = zCP_UTF16 then
          CP := GetW2A2WConversionCodePage(ConSettings);
        Result := Clob.GetStream(CP)
      end;
    LastWasNull := (Result = nil);
  end;
end;

function TZAbstractResultSet.GetUnicodeStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
    CLob: IZCLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  Result := nil;
  if IZResultSet(FWeakIZResultSetPtr).IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) and Supports(Blob, IZClob, CLob)
    then Result := Clob.GetStream(zCP_UTF16)
    else Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

function TZAbstractResultSet.GetBinaryStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  Result := nil;
  if IZResultSet(FWeakIZResultSetPtr).IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) then
      Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // base class - parameter not used intentionally
function TZAbstractResultSet.GetResultSet(ColumnIndex: Integer): IZResultSet;
begin
  Result := nil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractResultSet.GetValue(ColumnIndex: Integer): TZVariant;
var
  Metadata: TZAbstractResultSetMetadata;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  Metadata := TZAbstractResultSetMetadata(FMetadata);
{$IFNDEF DISABLE_CHECKING}
  if (Metadata = nil) or (ColumnIndex < FirstDbcIndex)
    or (ColumnIndex > Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
{$ENDIF}

  case Metadata.GetColumnType(ColumnIndex) of
    stBoolean:
      Result := EncodeBoolean(IZResultSet(FWeakIZResultSetPtr).GetBoolean(ColumnIndex));
    stShort, stSmall, stInteger, stLong:
      Result := EncodeInteger(IZResultSet(FWeakIZResultSetPtr).GetLong(ColumnIndex));
    stByte, stWord, stLongWord, stULong:
      Result := EncodeUInteger(IZResultSet(FWeakIZResultSetPtr).GetULong(ColumnIndex));
    stFloat, stDouble:
      Result := EncodeDouble(IZResultSet(FWeakIZResultSetPtr).GetDouble(ColumnIndex));
    stCurrency:
      Result := EncodeCurrency(IZResultSet(FWeakIZResultSetPtr).GetCurrency(ColumnIndex));
    stBigDecimal: begin
                    InitializeVariant(Result, vtBigDecimal);
                    IZResultSet(FWeakIZResultSetPtr).GetBigDecimal(ColumnIndex, Result.VBigDecimal);
                  end;
    stDate:   begin
                InitializeVariant(Result, vtDate);
                IZResultSet(FWeakIZResultSetPtr).GetDate(ColumnIndex, Result.VDate);
              end;
    stTime:   begin
                InitializeVariant(Result, vtTime);
                IZResultSet(FWeakIZResultSetPtr).GetTime(ColumnIndex, Result.VTime);
              end;
    stTimestamp:begin
                InitializeVariant(Result, vtTimeStamp);
                IZResultSet(FWeakIZResultSetPtr).GetTimeStamp(ColumnIndex, Result.VTimeStamp);
              end;
    stGUID: begin
              InitializeVariant(Result, vtGUID);
              IZResultSet(FWeakIZResultSetPtr).GetGUID(ColumnIndex, Result.VGUID);
            end;
    stBytes, stBinaryStream:
      Result := EncodeBytes(IZResultSet(FWeakIZResultSetPtr).GetBytes(ColumnIndex));
    stString, stAsciiStream, stUnicodeString, stUnicodeStream:
      {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
      Result := EncodeUnicodeString(IZResultSet(FWeakIZResultSetPtr).GetUnicodeString(ColumnIndex));
      {$ELSE}
      if (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) or
         (ConSettings^.ClientCodePage^.Encoding in [ceUTf8, ceUTF16]) then
        Result := EncodeUnicodeString(IZResultSet(FWeakIZResultSetPtr).GetUnicodeString(ColumnIndex))
      else
        Result := EncodeRawByteString(IZResultSet(FWeakIZResultSetPtr).GetRawByteString(ColumnIndex));
      {$ENDIF}
    else
      Result.VType := vtNull;
  end;

  if WasNull then
    Result.VType := vtNull;
end;

function TZAbstractResultSet.GetDefaultExpressionByName(
  const ColumnName: string): string;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetDefaultExpression(GetColumnIndex(ColumnName));
end;


//======================================================================
// Methods for accessing results by column name
//======================================================================

function TZAbstractResultSet.IsNullByName(const ColumnName: string): Boolean;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).IsNull(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetPAnsiCharByName(const ColumnName: string;
  out Len: NativeUInt): PAnsiChar;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetPAnsiChar(GetColumnIndex(ColumnName), Len);
end;

function TZAbstractResultSet.GetPWideCharByName(const ColumnName: string;
  out Len: NativeUInt): PWideChar;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetPWideChar(GetColumnIndex(ColumnName), Len);
end;

function TZAbstractResultSet.GetStringByName(const ColumnName: string): String;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetString(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetAnsiStream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
  CLob: IZCLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  Result := nil;
  if IZResultSet(FWeakIZResultSetPtr).IsNull(ColumnIndex) then
    LastWasNull := True
  else begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) and Supports(Blob, IZClob, CLob)
    then Result := Clob.GetStream(zOSCodePage)
    else Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

function TZAbstractResultSet.GetAnsiStreamByName(
  const ColumnName: string): TStream;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetAnsiStream(GetColumnIndex(ColumnName));
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractResultSet.GetAnsiStringByName(const ColumnName: string): AnsiString;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetAnsiString(GetColumnIndex(ColumnName));
end;
{$ENDIF}

function TZAbstractResultSet.GetUTF8Stream(ColumnIndex: Integer): TStream;
var Blob: IZBlob;
    CLob: IZCLob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  Result := nil;
  if IZResultSet(FWeakIZResultSetPtr).IsNull(ColumnIndex)
  then LastWasNull := True
  else begin
    Blob := IZResultSet(FWeakIZResultSetPtr).GetBlob(ColumnIndex);
    if not LastWasNull and (Blob <> nil) and Supports(Blob, IZClob, CLob)
    then Result := Clob.GetStream(zCP_UTF8)
    else Result := Blob.GetStream;
    LastWasNull := (Result = nil);
  end;
end;

function TZAbstractResultSet.GetUTF8StreamByName(
  const ColumnName: string): TStream;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetUTF8Stream(GetColumnIndex(ColumnName));
end;

{$IFNDEF NO_UTF8STRING}
function TZAbstractResultSet.GetUTF8StringByName(const ColumnName: string): UTF8String;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetUTF8String(GetColumnIndex(ColumnName));
end;
{$ENDIF}

function TZAbstractResultSet.GetRawByteStringByName(const ColumnName: string): RawByteString;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetRawByteString(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetUnicodeStringByName(const ColumnName: string):
  UnicodeString;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetUnicodeString(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetBooleanByName(const ColumnName: string): Boolean;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetBoolean(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetByteByName(const ColumnName: string): Byte;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetUInt(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetShortByName(const ColumnName: string): ShortInt;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetInt(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetWordByName(const ColumnName: string): Word;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetUInt(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetSmallByName(const ColumnName: string): SmallInt;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetInt(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetUIntByName(const ColumnName: string): Cardinal;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetUInt(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetIntByName(const ColumnName: string): Integer;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetInt(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetULongByName(const ColumnName: string): UInt64;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetULong(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetLongByName(const ColumnName: string): Int64;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetLong(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetFloatByName(const ColumnName: string): Single;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetFloat(GetColumnIndex(ColumnName));
end;

procedure TZAbstractResultSet.GetGUIDByName(const ColumnName: string;
  var Result: TGUID);
begin
  IZResultSet(FWeakIZResultSetPtr).GetGUID(GetColumnIndex(ColumnName), Result);
end;

function TZAbstractResultSet.GetDoubleByName(const ColumnName: string): Double;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetDouble(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetCurrencyByName(const ColumnName: string): Currency;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetCurrency(GetColumnIndex(ColumnName));
end;

procedure TZAbstractResultSet.GetBigDecimalByName(const ColumnName: string; var Result: TBCD);
begin
  IZResultSet(FWeakIZResultSetPtr).GetBigDecimal(GetColumnIndex(ColumnName), Result);
end;

function TZAbstractResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var P: PByte;
  L: NativeUInt;
begin
  P := IZResultSet(FWeakIZResultSetPtr).GetBytes(ColumnIndex, L);
  if (P <> nil) and (L > 0) then begin
    {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
    SetLength(Result, L);
    Move(P^, Pointer(Result)^, L);
  end else
    Result := nil;
end;

function TZAbstractResultSet.GetBytesByName(const ColumnName: string;
  out Len: NativeUInt): PByte;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetBytes(GetColumnIndex(ColumnName), Len);
end;

function TZAbstractResultSet.GetBytesByName(const ColumnName: string): TBytes;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetBytes(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var D: TZDate;
begin
  IZResultSet(FWeakIZResultSetPtr).GetDate(ColumnIndex, D{%H-});
  if not LastWasNull then
    LastWasNull := not TryDateToDateTime(D, Result{%H-});
  if LastWasNull then
    Result := 0;
end;

procedure TZAbstractResultSet.GetDateByName(const ColumnName: string;
  var Result: TZDate);
begin
  IZResultSet(FWeakIZResultSetPtr).GetDate(GetColumnIndex(ColumnName), Result);
end;

function TZAbstractResultSet.GetDateByName(const ColumnName: string): TDateTime;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetDate(GetColumnIndex(ColumnName));
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "T" does not seem to be initialized}
  {$WARN 5060 off : Function result variable does not seem to be initialized}
{$ENDIF}
function TZAbstractResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var T: TZTime;
begin
  IZResultSet(FWeakIZResultSetPtr).GetTime(columnIndex, T);
  if not LastWasNull then
    LastWasNull := not TryTimeToDateTime(T, Result);
  if LastWasNull then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractResultSet.GetTimeByName(const ColumnName: string;
  var Result: TZTime);
begin
  IZResultSet(FWeakIZResultSetPtr).GetTime(GetColumnIndex(ColumnName), Result);
end;

function TZAbstractResultSet.GetTimeByName(const ColumnName: string): TDateTime;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetTime(GetColumnIndex(ColumnName));
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "TS" does not seem to be initialized}
  {$WARN 5060 off : Function result variable does not seem to be initialized}
{$ENDIF}
function TZAbstractResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var TS: TZTimeStamp;
begin
  IZResultSet(FWeakIZResultSetPtr).GetTimeStamp(ColumnIndex, TS);
  if not LastWasNull then
    LastWasNull := not TryTimeStampToDateTime(TS, Result);
  if LastWasNull then
    Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractResultSet.GetTimestampByName(const ColumnName: string;
  var Result: TZTimeStamp);
begin
  IZResultSet(FWeakIZResultSetPtr).GetTimestamp(GetColumnIndex(ColumnName), Result);
end;

function TZAbstractResultSet.GetTimestampByName(const ColumnName: string): TDateTime;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetTimestamp(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetAsciiStreamByName(const ColumnName: string): TStream;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetAsciiStream(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetUnicodeStreamByName(const ColumnName: string): TStream;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetUnicodeStream(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetBinaryStreamByName(const ColumnName: string): TStream;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetBinaryStream(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetBlobByName(const ColumnName: string;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetBlob(GetColumnIndex(ColumnName), LobStreamMode);
end;

function TZAbstractResultSet.GetResultSetByName(const ColumnName: string): IZResultSet;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetResultSet(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetValueByName(const ColumnName: string): TZVariant;
begin
  Result := IZResultSet(FWeakIZResultSetPtr).GetValue(GetColumnIndex(ColumnName));
end;

//=====================================================================
// Advanced features:
//=====================================================================

{**
  Returns the first warning reported by calls on this
  <code>ResultSet</code> object.
  Subsequent warnings on this <code>ResultSet</code> object
  will be chained to the <code>SQLWarning</code> object that
  this method returns.

  <P>The warning chain is automatically cleared each time a new
  row is read.

  <P><B>Note:</B> This warning chain only covers warnings caused
  by <code>ResultSet</code> methods.  Any warning caused by
  <code>Statement</code> methods
  (such as reading OUT parameters) will be chained on the
  <code>Statement</code> object.

  @return the first <code>SQLWarning</code> object reported or <code>null</code>
}
function TZAbstractResultSet.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported on this <code>ResultSet</code> object.
  After this method is called, the method <code>getWarnings</code>
  returns <code>null</code> until a new warning is
  reported for this <code>ResultSet</code> object.
}
procedure TZAbstractResultSet.ClearWarnings;
begin
end;

function TZAbstractResultSet.GetCursorName: String;
begin
  Result := '';
end;

function TZAbstractResultSet.GetCursorLocation: TZCursorLocation;
begin
  Result := FCursorLocation;
end;

function TZAbstractResultSet.GetMetaData: IZResultSetMetaData;
begin
  Result := TZAbstractResultSetMetadata(FMetadata);
end;

function TZAbstractResultSet.GetColumnCount: Integer;
begin
  CheckClosed;
  Result := TZAbstractResultSetMetadata(FMetadata).GetColumnCount;
end;

function TZAbstractResultSet.GetColumnIndex(const ColumnName: string): Integer;
begin
  Result := FindColumn(ColumnName);

  if Result = InvalidDbcIndex then
    raise EZSQLException.Create(Format(SColumnWasNotFound, [ColumnName]));
end;

function TZAbstractResultSet.FindColumn(const ColumnName: string): Integer;
begin
  CheckClosed;
  Result := TZAbstractResultSetMetadata(FMetadata).FindColumn(ColumnName);
end;

//---------------------------------------------------------------------
// Traversal/Positioning
//---------------------------------------------------------------------

function TZAbstractResultSet.IsBeforeFirst: Boolean;
begin
  Result := (FRowNo = 0);
end;

function TZAbstractResultSet.IsClosed: Boolean;
begin
  Result := fClosed;
end;

function TZAbstractResultSet.IsAfterLast: Boolean;
begin
  Result := {(FLastRowNo > 0) and} (FRowNo > FLastRowNo);
end;

function TZAbstractResultSet.IsFirst: Boolean;
begin
  Result := (FRowNo = 1);
end;

function TZAbstractResultSet.IsLast: Boolean;
begin
  Result := {(FLastRowNo > 0) and} (FRowNo = FLastRowNo);
end;

procedure TZAbstractResultSet.BeforeClose;
begin
  ResetCursor;
end;

procedure TZAbstractResultSet.BeforeFirst;
begin
  MoveAbsolute(0);
end;

procedure TZAbstractResultSet.AfterClose;
begin
  FColumnsInfo.Clear;
end;

procedure TZAbstractResultSet.AfterLast;
begin
  Last;
  Next;
end;

function TZAbstractResultSet.First: Boolean;
begin
  Result := MoveAbsolute(1);
end;

function TZAbstractResultSet.Last: Boolean;
begin
  Result := MoveAbsolute(FLastRowNo);
end;

function TZAbstractResultSet.GetRow: NativeInt;
begin
  Result := FRowNo;
end;

{$IFDEF FPC} {$PUSH}
  {$WARN 5024 off : Parameter "Row" not used}
  {$WARN 5033 off : Function result does not seem to be set}
{$ENDIF}
function TZAbstractResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  raise CreateForwardOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := MoveAbsolute(FRowNo + Rows);
end;

function TZAbstractResultSet.Previous: Boolean;
begin
  Result := MoveAbsolute(FRowNo - 1);
end;

function TZAbstractResultSet.Next: Boolean;
begin
  Result := MoveAbsolute(FRowNo + 1);
end;

//---------------------------------------------------------------------
// Properties
//---------------------------------------------------------------------

{**
  Returns the fetch direction for this
  <code>ResultSet</code> object.
  @return the current fetch direction for this <code>ResultSet</code> object
}
function TZAbstractResultSet.GetFetchDirection: TZFetchDirection;
begin
  Result := FFetchDirection;
end;

procedure TZAbstractResultSet.SetFetchDirection(Direction: TZFetchDirection);
begin
  if Direction <> fdForward then
    Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

{**
  Returns the fetch size for this
  <code>ResultSet</code> object.
  @return the current fetch size for this <code>ResultSet</code> object
}
function TZAbstractResultSet.GetFetchSize: Integer;
begin
  Result := FFetchSize;
end;

{**
  Gives the JDBC driver a hint as to the number of rows that should
  be fetched from the database when more rows are needed for this
  <code>ResultSet</code> object.
  If the fetch size specified is zero, the JDBC driver
  ignores the value and is free to make its own best guess as to what
  the fetch size should be.  The default value is set by the
  <code>Statement</code> object
  that created the result set.  The fetch size may be changed at any time.

  @param rows the number of rows to fetch
}
procedure TZAbstractResultSet.SetFetchSize(Rows: Integer);
begin
  FFetchSize := Rows;
end;

{**
  Returns the type of this <code>ResultSet</code> object.
  The type is determined by the <code>Statement</code> object
  that created the result set.

  @return <code>TYPE_FORWARD_ONLY</code>,
    <code>TYPE_SCROLL_INSENSITIVE</code>,
    or <code>TYPE_SCROLL_SENSITIVE</code>
}
function TZAbstractResultSet.GetType: TZResultSetType;
begin
  Result := FResultSetType;
end;

{**
  Returns the concurrency mode of this <code>ResultSet</code> object.
  The concurrency used is determined by the
  <code>Statement</code> object that created the result set.

  @return the concurrency type, either <code>CONCUR_READ_ONLY</code>
    or <code>CONCUR_UPDATABLE</code>
}
function TZAbstractResultSet.GetConcurrency: TZResultSetConcurrency;
begin
  Result := FResultSetConcurrency;
end;

{**
  Gets an assigned post locate mode.
  @param the assigned post locate mode.
}
function TZAbstractResultSet.GetLocateUpdates: TZLocateUpdatesMode;
begin
  Result := FLocateUpdates;
end;

function TZAbstractResultSet.GetPostUpdates: TZPostUpdatesMode;
begin
  Result := FPostUpdates;
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Indicates whether the current row has been updated.  The value returned
  depends on whether or not the result set can detect updates.

  @return <code>true</code> if the row has been visibly updated
    by the owner or another, and updates are detected
}
function TZAbstractResultSet.RowUpdated: Boolean;
begin
  Result := False;
end;

{**
  Indicates whether the current row has had an insertion.
  The value returned depends on whether or not this
  <code>ResultSet</code> object can detect visible inserts.

  @return <code>true</code> if a row has had an insertion
    and insertions are detected; <code>false</code> otherwise
}
function TZAbstractResultSet.RowInserted: Boolean;
begin
  Result := False;
end;

{**
  Indicates whether a row has been deleted.  A deleted row may leave
  a visible "hole" in a result set.  This method can be used to
  detect holes in a result set.  The value returned depends on whether
  or not this <code>ResultSet</code> object can detect deletions.

  @return <code>true</code> if a row was deleted and deletions are detected;
    <code>false</code> otherwise
}
function TZAbstractResultSet.RowDeleted: Boolean;
begin
  Result := False;
end;

{**
  Updates the designated column with a variant value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateValue(ColumnIndex: Integer;
  const Value: TZVariant);
var Lob: IZBLob;
  Len: NativeUInt;
begin
  case Value.VType of
    vtBoolean: IZResultSet(FWeakIZResultSetPtr).UpdateBoolean(ColumnIndex, Value.VBoolean);
    vtInteger: IZResultSet(FWeakIZResultSetPtr).UpdateLong(ColumnIndex, Value.VInteger);
    vtUInteger: IZResultSet(FWeakIZResultSetPtr).UpdateULong(ColumnIndex, Value.VUInteger);
    vtDouble: IZResultSet(FWeakIZResultSetPtr).UpdateDouble(ColumnIndex, Value.VDouble);
    vtCurrency: IZResultSet(FWeakIZResultSetPtr).UpdateCurrency(ColumnIndex, Value.VCurrency);
    vtBigDecimal: IZResultSet(FWeakIZResultSetPtr).UpdateBigDecimal(ColumnIndex, Value.VBigDecimal);
    vtGUID:    IZResultSet(FWeakIZResultSetPtr).UpdateGUID(ColumnIndex, Value.VGUID);
    vtString: IZResultSet(FWeakIZResultSetPtr).UpdateString(ColumnIndex, Value.{$IFDEF UNICODE}VUnicodeString{$ELSE}VRawByteString{$ENDIF});
{$IFNDEF NO_ANSISTRING}
    vtAnsiString: IZResultSet(FWeakIZResultSetPtr).UpdateAnsiString(ColumnIndex, Value.VRawByteString);
{$ENDIF}
{$IFNDEF NO_UTF8STRING}
    vtUTF8String: IZResultSet(FWeakIZResultSetPtr).UpdateUTF8String(ColumnIndex, Value.VRawByteString);
{$ENDIF}
    vtRawByteString: IZResultSet(FWeakIZResultSetPtr).UpdateRawByteString(ColumnIndex, Value.VRawByteString);
    vtBytes: begin
              Len := Length(Value.VRawByteString);
              IZResultSet(FWeakIZResultSetPtr).UpdateBytes(ColumnIndex, Pointer(Value.VRawByteString), Len);
            end;
    vtDateTime: IZResultSet(FWeakIZResultSetPtr).UpdateTimestamp(ColumnIndex, Value.VDateTime);
    vtDate: IZResultSet(FWeakIZResultSetPtr).UpdateDate(ColumnIndex, Value.VDate);
    vtTime: IZResultSet(FWeakIZResultSetPtr).UpdateTime(ColumnIndex, Value.VTime);
    vtTimeStamp: IZResultSet(FWeakIZResultSetPtr).UpdateTimeStamp(ColumnIndex, Value.VTimeStamp);
    vtUnicodeString: IZResultSet(FWeakIZResultSetPtr).UpdateUnicodeString(ColumnIndex, Value.VUnicodeString);
    vtInterface: begin
      if (Value.vInterface <> nil) and Supports(Value.vInterface, IZBLob, Lob)
      then IZResultSet(FWeakIZResultSetPtr).UpdateLob(ColumnIndex, Lob)
      else IZResultSet(FWeakIZResultSetPtr).UpdateNull(ColumnIndex);
    end
  else
    IZResultSet(FWeakIZResultSetPtr).UpdateNull(ColumnIndex);
  end;
end;

{**
  Updates the designated column with a <code>null</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
}
procedure TZAbstractResultSet.UpdateNullByName(const ColumnName: string);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateNull(GetColumnIndex(ColumnName));
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBooleanByName(const ColumnName: string;
  Value: Boolean);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateBoolean(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateByteByName(const ColumnName: string;
  Value: Byte);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateByte(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>ShortInt</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateShortByName(const ColumnName: string;
  Value: ShortInt);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateShort(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>Word</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateWordByName(const ColumnName: string;
  Value: Word);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateWord(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>SmallInt</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateSmallByName(const ColumnName: string;
  Value: SmallInt);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateSmall(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an <code>usigned long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUIntByName(const ColumnName: string;
  Value: Cardinal);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateUInt(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateIntByName(const ColumnName: string;
  Value: Integer);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateInt(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateULongByName(const ColumnName: string;
  const Value: UInt64);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateULong(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>signed longlong</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateLongByName(const ColumnName: string;
  const Value: Int64);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateLong(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>float	</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateFloatByName(const ColumnName: string;
  Value: Single);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateFloat(GetColumnIndex(ColumnName), Value);
end;

procedure TZAbstractResultSet.UpdateGUIDByName(const ColumnName: string;
  const Value: TGUID);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateGUID(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDoubleByName(const ColumnName: string;
  const Value: Double);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateDouble(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>currency</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateCurrencyByName(const ColumnName: string;
  const Value: Currency);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateCurrency(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBigDecimalByName(const ColumnName: string;
  const Value: TBCD);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateBigDecimal(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>PAnsiChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param Len the pointer to the length in bytes
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePAnsiCharByName(const ColumnName: string;
  Value: PAnsiChar; var Len: NativeUInt);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdatePAnsiChar(GetColumnIndex(ColumnName), Value, Len);
end;

{**
  Updates the designated column with a <code>PWideChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param Len the pointer to the length of the string in codepopints
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePWideCharByName(const ColumnName: string;
  Value: PWideChar; var Len: NativeUInt);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdatePWideChar(GetColumnIndex(ColumnName), Value, Len);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateStringByName(const ColumnName: string;
   const Value: String);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>AnsiString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractResultSet.UpdateAnsiStringByName(const ColumnName: string;
   const Value: AnsiString);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateAnsiString(GetColumnIndex(ColumnName), Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>UTF8String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractResultSet.UpdateUTF8StringByName(const ColumnName: string;
   const Value: UTF8String);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateUTF8String(GetColumnIndex(ColumnName), Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>RawByteString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateRawByteStringByName(const ColumnName: string;
   const Value: RawByteString);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateRawByteString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>WideString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStringByName(const ColumnName: string;
  const Value: UnicodeString);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateUnicodeString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>byte</code> array value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBytes(ColumnIndex: Integer;
  const Value: TBytes);
var Len: NativeUint;
begin
  Len := Length(Value);
  IZResultSet(FWeakIZResultSetPtr).UpdateBytes(ColumnIndex, Pointer(Value), Len);
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  JDBC 2.0

  Updates a column with a byte array value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row, or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or <code>insertRow</code>
  methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}

procedure TZAbstractResultSet.UpdateBytesByName(const ColumnName: string;
  const Value: TBytes);
var Len: NativeUint;
begin
  Len := Length(Value);
  IZResultSet(FWeakIZResultSetPtr).UpdateBytes(GetColumnIndex(ColumnName), Pointer(Value), Len);
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDateByName(const ColumnName: string;
  const Value: TDateTime);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateDate(GetColumnIndex(ColumnName), Value);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "D" does not seem to be initialized} {$ENDIF}
procedure TZAbstractResultSet.UpdateDate(ColumnIndex: Integer;
  const Value: TDateTime);
var D: TZDate;
begin
  DecodeDateTimeToDate(Value, D);
  IZResultSet(FWeakIZResultSetPtr).UpdateDate(ColumnIndex, D);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDateByName(const ColumnName: string;
  const Value: TZDate);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateDate(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimeByName(const ColumnName: string;
  const Value: TDateTime);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateTime(GetColumnIndex(ColumnName), Value);
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5060 off : Function result variable does not seem to be initialized}
{$ENDIF}
procedure TZAbstractResultSet.UpdateTime(ColumnIndex: Integer;
  const Value: TDateTime);
var T: TZTime;
begin
  DecodeDateTimeToTime(Value, T);
  IZResultSet(FWeakIZResultSetPtr).UpdateTime(ColumnIndex, T);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimeByName(const ColumnName: string;
  const Value: TZTime);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateTime(GetColumnIndex(ColumnName), Value);
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
  {$WARN 5060 off : Function result variable does not seem to be initialized}
{$ENDIF}
procedure TZAbstractResultSet.UpdateTimeStamp(ColumnIndex: Integer;
  const Value: TDateTime);
var TS: TZTimeStamp;
begin
  DecodeDateTimeToTimeStamp(Value, TS);
  IZResultSet(FWeakIZResultSetPtr).UpdateTimeStamp(ColumnIndex, TS);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimestampByName(const ColumnName: string;
  const Value: TDateTime);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateTimestamp(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimestampByName(const ColumnName: string;
  const Value: TZTimeStamp);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateTimestamp(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateAsciiStreamByName(const ColumnName: string;
  const Value: TStream);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateAsciiStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBinaryStreamByName(const ColumnName: string;
  const Value: TStream);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateBinaryStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStreamByName(const ColumnName: string;
  const Value: TStream);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateUnicodeStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>Variant</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateValueByName(const ColumnName: string;
  const Value: TZVariant);
begin
  IZResultSet(FWeakIZResultSetPtr).UpdateValue(GetColumnIndex(ColumnName), Value);
end;

{**
  Inserts the contents of the insert row into this
  <code>ResultSet</code> objaect and into the database.
  The cursor must be on the insert row when this method is called.
}
procedure TZAbstractResultSet.InsertRow;
begin
  raise CreateReadOnlyException;
end;

{**
  Updates the underlying database with the new contents of the
  current row of this <code>ResultSet</code> object.
  This method cannot be called when the cursor is on the insert row.
}
procedure TZAbstractResultSet.UpdateRow;
begin
  raise CreateReadOnlyException;
end;

{**
  Deletes the current row from this <code>ResultSet</code> object
  and from the underlying database.  This method cannot be called when
  the cursor is on the insert row.
}
procedure TZAbstractResultSet.DeleteRow;
begin
  raise CreateReadOnlyException;
end;

{**
  Refreshes the current row with its most recent value in
  the database.  This method cannot be called when
  the cursor is on the insert row.

  <P>The <code>refreshRow</code> method provides a way for an
  application to
  explicitly tell the JDBC driver to refetch a row(s) from the
  database.  An application may want to call <code>refreshRow</code> when
  caching or prefetching is being done by the JDBC driver to
  fetch the latest value of a row from the database.  The JDBC driver
  may actually refresh multiple rows at once if the fetch size is
  greater than one.

  <P> All values are refetched subject to the transaction isolation
  level and cursor sensitivity.  If <code>refreshRow</code> is called after
  calling an <code>updateXXX</code> method, but before calling
  the method <code>updateRow</code>, then the
  updates made to the row are lost.  Calling the method
  <code>refreshRow</code> frequently will likely slow performance.
}
procedure TZAbstractResultSet.RefreshRow;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

procedure TZAbstractResultSet.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var ImmediatelyReleasable: IImmediatelyReleasable;
  i: Integer;
begin
  if not FClosed and Assigned(Statement){virtual RS ! } then begin
    FClosed := True;
    FRowNo := 0;
    FLastRowNo := 0;
    LastWasNull := True;
    for I := FOpenLobStreams.Count -1 downto 0 do
      if (FOpenLobStreams[0] <> nil) and TObject(FOpenLobStreams[i]).GetInterface(IImmediatelyReleasable, ImmediatelyReleasable)
        and (Sender <> ImmediatelyReleasable) then
          ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
    FOpenLobStreams.Clear;
    if Supports(Statement, IImmediatelyReleasable, ImmediatelyReleasable) and
       (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
    AfterClose;
  end;
end;

{**
  Cancels the updates made to the current row in this
  <code>ResultSet</code> object.
  This method may be called after calling an
  <code>updateXXX</code> method(s) and before calling
  the method <code>updateRow</code> to roll back
  the updates made to a row.  If no updates have been made or
  <code>updateRow</code> has already been called, this method has no
  effect.
}
procedure TZAbstractResultSet.CancelRowUpdates;
begin
  raise CreateReadOnlyException;
end;

{**
  Moves the cursor to the insert row.  The current cursor position is
  remembered while the cursor is positioned on the insert row.

  The insert row is a special row associated with an updatable
  result set.  It is essentially a buffer where a new row may
  be constructed by calling the <code>updateXXX</code> methods prior to
  inserting the row into the result set.

  Only the <code>updateXXX</code>, <code>getXXX</code>,
  and <code>insertRow</code> methods may be
  called when the cursor is on the insert row.  All of the columns in
  a result set must be given a value each time this method is
  called before calling <code>insertRow</code>.
  An <code>updateXXX</code> method must be called before a
  <code>getXXX</code> method can be called on a column value.
}
procedure TZAbstractResultSet.MoveToInsertRow;
begin
  raise CreateReadOnlyException;
end;

{**
  Moves the cursor to the remembered cursor position, usually the
  current row.  This method has no effect if the cursor is not on
  the insert row.
}
procedure TZAbstractResultSet.MoveToCurrentRow;
begin
end;

{**
  Compares fields from two row buffers.
  @param Row1 the first row buffer to compare.
  @param Row2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZAbstractResultSet.CompareRows(Row1, Row2: NativeInt;
  const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
var
  I: Integer;
  ColumnIndex: Integer;
  SaveRowNo: Integer;
  Value1, Value2: TZVariant;
begin
  Result := 0;
  SaveRowNo := RowNo;
  try
    for I := Low(ColumnIndices) to High(ColumnIndices) do
    begin
      ColumnIndex := ColumnIndices[I];

      MoveAbsolute(Row1);
      Value1 := GetValue(ColumnIndex);
      MoveAbsolute(Row2);
      Value2 := GetValue(ColumnIndex);
      Result := CompareFuncs[i]((Value1.VType = vtNull), (Value2.VType = vtNull), Value1, Value2);
      if Result <> 0 then Break;
    end;
  finally
    MoveAbsolute(SaveRowNo);
  end;
end;

function TZAbstractResultSet.GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
  const CompareKinds: TComparisonKindArray): TCompareFuncs;
var I: Integer;
begin
  {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
  SetLength(Result, Length(ColumnIndices));
  for i := low(ColumnIndices) to high(ColumnIndices) do
    case CompareKinds[i] of
      ckAscending:
        case TZAbstractResultSetMetadata(FMetadata).GetColumnType(ColumnIndices[i]) of
          stBoolean:
            Result[i] := CompareBoolean_Asc;
          stShort, stSmall, stInteger, stLong:
            Result[i] := CompareInt64_Asc;
          stByte, stWord, stLongWord, stULong:
            Result[i] := CompareUInt64_Asc;
          stFloat, stDouble:
            Result[i] := CompareDouble_Asc;
          stCurrency:
            Result[i] := CompareCurrency_Asc;
          stBigDecimal:
            Result[i] := CompareBigDecimal_Asc;
          stDate:
            Result[i] := CompareDate_Asc;
          stTime:
            Result[i] := CompareTime_Asc;
          stTimestamp:
            Result[i] := CompareTimeStamp_Asc;
          stBytes, stBinaryStream, stGUID:
            Result[i] := CompareBytes_Asc;
          stString, stAsciiStream, stUnicodeString, stUnicodeStream:
            {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
            Result[i] := CompareUnicodeString_Asc;
            {$ELSE}
            if (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) or
                (ConSettings^.ClientCodePage^.Encoding in [ceUTf8, ceUTF16]) then
              Result[i] := CompareUnicodeString_Asc
            else
              Result[I] := CompareRawByteString_Asc
            {$ENDIF}
          else
            Result[i] := CompareNothing;
        end;
      ckDescending:
        case TZAbstractResultSetMetadata(FMetadata).GetColumnType(ColumnIndices[i]) of
          stBoolean:
            Result[i] := CompareBoolean_Desc;
          stShort, stSmall, stInteger, stLong:
            Result[i] := CompareInt64_Desc;
          stByte, stWord, stLongWord, stULong:
            Result[i] := CompareUInt64_Desc;
          stFloat, stDouble:
            Result[i] := CompareDouble_Desc;
          stCurrency:
            Result[i] := CompareCurrency_Desc;
          stBigDecimal:
            Result[i] := CompareBigDecimal_Desc;
          stDate:
            Result[i] := CompareDate_Desc;
          stTime:
            Result[i] := CompareTime_Desc;
          stTimestamp:
            Result[i] := CompareTimeStamp_Desc;
          stBytes, stBinaryStream, stGUID:
            Result[i] := CompareBytes_Desc;
          stString, stAsciiStream, stUnicodeString, stUnicodeStream:
            {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
            Result[i] := CompareUnicodeString_Desc;
            {$ELSE}
            if (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) or
                (ConSettings^.ClientCodePage^.Encoding in [ceUTf8, ceUTF16]) then
              Result[i] := CompareUnicodeString_Desc
            else
              Result[I] := CompareRawByteString_Desc
            {$ENDIF}
          else
            Result[i] := CompareNothing;
        end;
      ckEquals: raise EZSQLException.Create('Compare Equals is not allowed here!');
    end;
end;

{**
  Returns the <code>Statement</code> object that produced this
  <code>ResultSet</code> object.
  If the result set was generated some other way, such as by a
  <code>DatabaseMetaData</code> method, this method returns
  <code>null</code>.

  @return the <code>Statment</code> object that produced
    this <code>ResultSet</code> object or <code>null</code>
    if the result set was produced some other way
}
function TZAbstractResultSet.GetStatement: IZStatement;
begin
  Result := FStatement;
end;

{ TZAbstractReadOnlyResultSet }

procedure TZAbstractReadOnlyResultSet.AfterConstruction;
var LogObj: IZLoggingObject;
begin
  LogObj := nil;
  QueryInterface(IZLoggingObject, LogObj);
  FWeakIZLoggingObjectPtr := Pointer(LogObj);
  LogObj := nil;
  inherited AfterConstruction;
end;

function TZAbstractReadOnlyResultSet.CreateLogEvent(
  const Category: TZLoggingCategory): TZLoggingEvent;
var Stmt: IZStatement;
  LogObj: IZLoggingObject;
begin
  Stmt := GetStatement;
  if (Category = lcFetchDone) and (Stmt <> nil) and (Stmt.QueryInterface(IZLoggingObject, LogObj) = S_OK) then begin
    Result := LogObj.CreateLogEvent(lcFetchDone);
    if Result <> nil then
      Result.ErrorCodeOrAffectedRows := LastRowNo;
    FLastRowFetchLogged := True;
  end else result := nil;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // readonly ResultSet - parameter not used intentionally
function TZAbstractReadOnlyResultSet.GetDefaultExpression(
  ColumnIndex: Integer): string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractReadOnlyResultSet.GetRawByteString(
  ColumnIndex: Integer): RawByteString;
var P: PAnsiChar;
  L: NativeUInt;
begin
  P := IZResultSet(FWeakIZResultSetPtr).GetPAnsiChar(ColumnIndex, L);
  if (P <> nil) and (L > 0) then
    if P = Pointer(FRawTemp)
    then Result := FRawTemp
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    else ZSetString(P, L, Result)
    {$ELSE}
    else System.SetString(Result, P, L)
    {$ENDIF}
  else Result := EmptyRaw;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractReadOnlyResultSet.GetString(ColumnIndex: Integer): String;
var P: Pointer;
  L: NativeUInt;
begin
  {$IFDEF UNICODE}
  P := IZResultSet(FWeakIZResultSetPtr).GetPWideChar(ColumnIndex, L);
  if (P <> nil) and (L > 0) then
    if P = Pointer(FUniTemp)
    then Result := FUniTemp
    else begin
      Result := '';
      System.SetString(Result, PWideChar(P), L)
    end
  else Result := '';
  {$ELSE}
  if (ConSettings.ClientCodePage.Encoding = ceUTF16) or (TZColumnInfo(FColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType in [stUnicodeString, stUnicodeStream]) then begin
    P := IZResultSet(FWeakIZResultSetPtr).GetPWideChar(ColumnIndex, L);
    if (P <> nil) and (L > 0)
    then Result := PUnicodeToRaw(P, L, GetW2A2WConversionCodePage(ConSettings))
    else Result := '';
  end else begin
    P := IZResultSet(FWeakIZResultSetPtr).GetPAnsiChar(ColumnIndex, L);
    if (P <> nil) and (L > 0) then
      if P = Pointer(FRawTemp)
      then Result := FRawTemp
      else System.SetString(Result, PAnsiChar(P), L)
    else Result := '';
  end;
  {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UnicodeString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractReadOnlyResultSet.GetUnicodeString(
  ColumnIndex: Integer): UnicodeString;
var P: PWideChar;
  L: NativeUInt;
begin
  P := IZResultSet(FWeakIZResultSetPtr).GetPWideChar(ColumnIndex, L);
  if LastWasNull or (L = 0) then
    Result := ''
  else if P = Pointer(FUniTemp)
    then Result := FUniTemp
    else begin
      Result := '';
      System.SetString(Result, P, L);
    end;
end;

{**
  Updates the designated column with a <code>AnsiString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // parameters not used intentionally
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractReadOnlyResultSet.UpdateAnsiString(ColumnIndex: Integer;
  const Value: AnsiString);
begin
  raise CreateReadOnlyException;
end;
{$ENDIF}


{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateAsciiStream(ColumnIndex: Integer;
  const Value: TStream);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateBigDecimal(ColumnIndex: Integer;
  const Value: TBCD);
begin
  raise CreateReadOnlyException;;
end;

procedure TZAbstractReadOnlyResultSet.UpdateBinaryStream(ColumnIndex: Integer;
  const Value: TStream);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateBoolean(ColumnIndex: Integer;
  Value: Boolean);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateByte(ColumnIndex: Integer;
  Value: Byte);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>byte</code> array value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Value the address of new column value
  @param Len the length of the addressed value
}
procedure TZAbstractReadOnlyResultSet.UpdateBytes(ColumnIndex: Integer;
  Value: PByte; var Len: NativeUInt);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>currency</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateCurrency(ColumnIndex: Integer;
  const Value: Currency);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateDate(ColumnIndex: Integer;
  const Value: TZDate);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the DefaultExpression of the designated column with a <code>String</code> value.
  This changes the behaviour of the RowAccessor used by the Resultset
  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new DefaultExpression value for the column
}
procedure TZAbstractReadOnlyResultSet.UpdateDefaultExpression(
  ColumnIndex: Integer; const Value: string);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateDouble(ColumnIndex: Integer;
  const Value: Double);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>float</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateFloat(ColumnIndex: Integer;
  Value: Single);
begin
  raise CreateReadOnlyException;;
end;

procedure TZAbstractReadOnlyResultSet.UpdateGUID(ColumnIndex: Integer;
  const Value: TGUID);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with an <code>signed long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateInt(ColumnIndex: Integer;
  Value: Integer);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>signed longlong</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateLob(ColumnIndex: Integer;
  const Value: IZBlob);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>signed longlong</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param value the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateLong(ColumnIndex: Integer;
  const Value: Int64);
begin
  raise CreateReadOnlyException;;
end;

{**
  Gives a nullable column a null value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code>
  or <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZAbstractReadOnlyResultSet.UpdateNull(ColumnIndex: Integer);
begin
  raise CreateReadOnlyException;;
end;

procedure TZAbstractReadOnlyResultSet.UpdatePAnsiChar(ColumnIndex: Integer;
  Value: PAnsiChar; var Len: NativeUInt);
begin
  raise CreateReadOnlyException;;
end;

procedure TZAbstractReadOnlyResultSet.UpdatePWideChar(ColumnIndex: Integer;
  Value: PWideChar; var Len: NativeUInt);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>RawByteString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateRawByteString(ColumnIndex: Integer;
  const Value: RawByteString);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>short</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateShort(ColumnIndex: Integer;
  Value: ShortInt);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>small</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateSmall(ColumnIndex: Integer;
  Value: SmallInt);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateString(ColumnIndex: Integer;
  const Value: String);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateTime(ColumnIndex: Integer;
  const Value: TZTime);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateTimestamp(ColumnIndex: Integer;
  const Value: TZTimeStamp);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with an <code>uint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateUInt(ColumnIndex: Integer;
  Value: Cardinal);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>ulong</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateULong(ColumnIndex: Integer;
  const Value: UInt64);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateUnicodeStream(ColumnIndex: Integer;
  const Value: TStream);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>WideString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateUnicodeString(ColumnIndex: Integer;
  const Value: UnicodeString);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>UTF8String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractReadOnlyResultSet.UpdateUTF8String(ColumnIndex: Integer;
  const Value: UTF8String);
begin
  raise CreateReadOnlyException;;
end;
{$ENDIF}

procedure TZAbstractReadOnlyResultSet.UpdateValue(ColumnIndex: Integer;
  const Value: TZVariant);
begin
  raise CreateReadOnlyException;;
end;

{**
  Updates the designated column with a <code>word</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractReadOnlyResultSet.UpdateWord(ColumnIndex: Integer;
  Value: Word);
begin
  raise CreateReadOnlyException;;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZAbstractReadOnlyResultSet_A }

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.
  the encoding is the encoding of the OS

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractReadOnlyResultSet_A.GetAnsiString(
  ColumnIndex: Integer): AnsiString;
var P: PAnsichar;
    L: NativeUInt;
    RBS: RawByteString absolute Result;
label jmpA, jmpSet;
begin
  with TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    case ColumnType of
      stString,stAsciiStream: begin
jmpA:     P := IZResultSet(FWeakIZResultSetPtr).GetPAnsiChar(ColumnIndex, L);
          RBS := '';
          if (P <> nil) and (ColumnCodePage <> ZOSCodePage)
          then PRawToRawConvert(P, L, ColumnCodePage, ZOSCodePage, RBS)
          else goto jmpSet;
        end;
      stUnicodeString, stUnicodeStream: {some drivers just tag N-Columns but are raw encoded}
        if ColumnCodePage = zCP_UTF16 then begin
          P := Pointer(IZResultSet(FWeakIZResultSetPtr).GetPWideChar(ColumnIndex, L));
          Result := PUnicodeToRaw(PWideChar(P), L, ZOSCodePage);
        end else goto jmpA
      else begin
          P := IZResultSet(FWeakIZResultSetPtr).GetPAnsiChar(ColumnIndex, L);
jmpSet:   System.SetString(Result, P, L)
        end;
    end;
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractReadOnlyResultSet_A.GetUTF8String(
  ColumnIndex: Integer): UTF8String;
var P: PAnsichar;
    L: NativeUInt;
    RBS: RawByteString absolute Result;
label jmpA, jmpSet;
begin
  with TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    case ColumnType of
      stString,stAsciiStream: begin
jmpA:     P := IZResultSet(FWeakIZResultSetPtr).GetPAnsiChar(ColumnIndex, L);
          RBS := '';
          if (P <> nil) and (ColumnCodePage <> zCP_UTF8)
          then PRawToRawConvert(P, L, ColumnCodePage, zCP_UTF8, RBS)
          else goto jmpSet;
        end;
      stUnicodeString, stUnicodeStream: {some drivers just tag N-Columns but are raw encoded}
        if ColumnCodePage = zCP_UTF16 then begin
          P := Pointer(IZResultSet(FWeakIZResultSetPtr).GetPWideChar(ColumnIndex, L));
          Result := PUnicodeToRaw(PWideChar(P), L, zCP_UTF8);
        end else goto jmpA
      else begin
          P := IZResultSet(FWeakIZResultSetPtr).GetPAnsiChar(ColumnIndex, L);
jmpSet:   {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
          ZSetString(P, L, result)
          {$ELSE}
          System.SetString(Result, P, L)
          {$ENDIF}
        end;
    end;
end;
{$ENDIF}

constructor TZLocalMemBLob.Create(const OpenLobStreams: TZSortedList);
begin
  inherited Create(zCP_Binary, @FDataRef, nil, OpenLobStreams);
end;

constructor TZLocalMemBLob.CreateFromBlob(const Value: IZBlob;
  const OpenLobStreams: TZSortedList);
var Stream: TStream;
begin
  Create(OpenLobStreams);
  Stream := Value.GetStream;
  if Stream = nil then
    Exit;
  FDataRef.IsNotNull := 1;
  try
    SetCapacity(Stream.Size);
    Stream.Read(FDataRef.VarLenData.Data, Stream.Size);
  finally
    Stream.Free;
  end;
end;

constructor TZLocalMemBLob.CreateWithData(Data: Pointer; BytesCount: NativeUInt;
  const OpenLobStreams: TZSortedList);
begin
  Create(OpenLobStreams);
  if Data <> nil then begin
     FDataRef.IsNotNull := 1;
     SetCapacity(BytesCount);
     Move(Data^, FDataRef.VarLenData.Data, BytesCount);
  end;
end;

constructor TZLocalMemBLob.CreateWithStream(
  Value: TZImmediatelyReleasableLobStream;
  FreeStream: Boolean; const OpenLobStreams: TZSortedList);
begin
  CreateWithStream(Value, OpenLobStreams);
  if FreeStream then Value.Free;
end;

constructor TZLocalMemBLob.CreateWithStream(Value: TStream;
  const OpenLobStreams: TZSortedList);
const
  MaxBufSize = $F000;
var Count, BufSize, ReadBytes: LongInt;
  Buf: PAnsichar;
begin
  Create(OpenLobStreams);
  if Value = nil then Exit;
  Count := Value.Size;
  FDataRef.IsNotNull := 1;
  SetCapacity(Count);
  if Count > MaxBufSize
  then BufSize := MaxBufSize
  else BufSize := Count;
  Buf := @FDataRef.VarLenData.Data;
  while Count <> 0 do begin
    ReadBytes := Value.Read(Buf^, BufSize);
    Dec(Count, ReadBytes);
    Inc(Buf, readBytes);
    if Count < BufSize then
      BufSize := Count;
  end;
end;

destructor TZLocalMemBLob.Destroy;
begin
  if FDataRef.VarLenData <> nil then begin
    FreeMem(FDataRef.VarLenData);
    FDataRef.VarLenData := nil;
  end;
  inherited Destroy;
end;

{ TZImmediatelyReleasableLobStream }

constructor TZImmediatelyReleasableLobStream.Create(const OwnerLob: IZLob;
  const Owner: IImmediatelyReleasable; const OpenLobStreams: TZSortedList);
begin
  inherited Create;
  FOwnerLob := OwnerLob;
  FOwner := Owner;
  FOpenLobStreams := OpenLobStreams;
  FOpenLobStreams.Add(Pointer(Self));
end;

destructor TZImmediatelyReleasableLobStream.Destroy;
var Idx: Integer;
begin
  Idx := FOpenLobStreams.IndexOf(Pointer(Self));
  if idx >= 0 then
    FOpenLobStreams.Delete(Idx);
  inherited;
end;

function TZImmediatelyReleasableLobStream.GetConSettings: PZConSettings;
begin
  Result := FOwner.GetConSettings;
end;

function TZImmediatelyReleasableLobStream.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj)
  then Result := S_OK
  else Result := E_NOINTERFACE;
end;

procedure TZImmediatelyReleasableLobStream.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var idx: Integer;
  imm: IImmediatelyReleasable;
begin
  if FReleased then Exit;
  FReleased := True;
  Idx := FOpenLobStreams.IndexOf(Pointer(Self));
  if idx >= 0 then
    FOpenLobStreams.Delete(Idx);
  if (FOwnerLob <> nil) and (FOwnerLob.QueryInterface(IImmediatelyReleasable, Imm) = S_OK) and
     (imm <> Sender) then
    Imm.ReleaseImmediat(Sender, AError);
  if (FOwner <> Sender) and (FOwner <> nil) then
     FOwner.ReleaseImmediat(Sender, AError);
end;

function TZImmediatelyReleasableLobStream._AddRef: Integer;
begin
  Result := -1;
end;

function TZImmediatelyReleasableLobStream._Release: Integer;
begin
  Result := -1;
end;

{ TZCodePageConversionStream }

constructor TZCodePageConversionStream.Create(const Owner: TStream;
  SourceCodePage, DestCodePage: Word; ConSettings: PZConSettings;
  const OpenLobStreams: TZSortedList);
var Src, Dst: Pointer;
  L: NativeUint;
label W2A;
begin
  FNativeCodePage := SourceCodePage;
  FCurrentCodePage := DestCodePage;
  FOwnerStream := Owner;
  FConSettings := ConSettings;
  FOpenLobStreams := OpenLobStreams;
  if FOpenLobStreams <> nil then
    FOpenLobStreams.Add(Pointer(Self));

  Src := nil;
  L := Owner.Size;
  if L = 0 then Exit;
  if (SourceCodePage = DestCodePage) then begin
    ReadStreamToMem(Src, L, Owner);
    SetPointer(Src, L);
    FInConstructionState := True;
    Capacity := L; //realloc mem
    FInConstructionState := False;
  end else begin
    ReadStreamToMem(Src, L, Owner);
    FInConstructionState := True;
    try
W2A:  GetMem(Dst, (L+1) shl 1);
      if SourceCodePage = zCP_UTF16
      then L := PUnicode2PRawBuf(Src, Dst, L shr 1, L shl 1, DestCodePage)
      else begin
        L := PRaw2PUnicodeBuf(Src, Dst, L, SourceCodePage);
        L := L shl 1; //word to bytelen
        if DestCodePage <> zCP_UTF16 then begin  //A2A convert?
          FreeMem(Src);
          Src := Dst;
          SourceCodePage := zCP_UTF16;
          goto W2A; //get new mem + convert from W to A
        end;
      end;
      SetPointer(Dst, L);
      Capacity := L; //realloc mem
    finally
      FreeMem(Src);
      FInConstructionState := False;
    end;
  end;
end;

destructor TZCodePageConversionStream.Destroy;
var Src, Dst: Pointer;
    L, idx: Integer;
label jmpW2A;
begin
  if FOpenLobStreams <> nil then begin
    Idx := FOpenLobStreams.IndexOf(Pointer(Self));
    FOpenLobStreams.Delete(Idx);
  end;
  try
    if FUpdated then begin
      L := Size;
      Src := Memory;
      SetPointer(nil, 0); //the destructor should not kill our mem
      if (FCurrentCodePage = FNativeCodePage)
      then Dst := Src
      else begin
jmpW2A: GetMem(Dst, (L+1) shl 1);
        if (FCurrentCodePage = zCP_UTF16) then begin
          L := PUnicode2PRawBuf(Src, Dst, l shr 1, l shl 1, FNativeCodePage);
          FreeMem(Src);
        end else begin
          L := PRaw2PUnicodeBuf(Src, Dst, L, FCurrentCodePage);
          FreeMem(Src);
          L := L shl 1;
          if FNativeCodePage <> zCP_UTF16 then begin
            FCurrentCodePage := zCP_UTF16;
            Src := Dst;
            goto jmpW2A;
          end;
        end;
      end;
      try
        FlushMemToStream(Dst,L,FOwnerStream);
      finally
        FreeMem(Dst);
      end;
    end;
  finally
    FInConstructionState := True;
    inherited Destroy;
    {$IFNDEF AUTOREFCOUNT}
    FreeAndNil(FOwnerStream);
    {$ELSE}
    FOwnerStream.Free;
    FOwnerStream := nil;
    {$ENDIF}
  end;
end;

procedure TZCodePageConversionStream.FlushMemToStream(Buf: Pointer;
  Len: NativeUInt; Stream: TStream);
begin
  Stream.Size := 0;
  Stream.Position := 0;
  Stream.Write(Buf^, Len); //notify updated
end;

procedure TZCodePageConversionStream.ReadStreamToMem(var Buf: Pointer;
  var Len: NativeUint; Stream: TStream);
begin
  if Buf <> nil then
    FreeMem(Buf);
  Len := Stream.Size;
  GetMem(Buf, Len);
  Stream.Read(Buf^, Len); //Move data
end;

function TZCodePageConversionStream.Realloc(var NewCapacity: {$IFDEF FPC}PtrInt{$ELSE}{$IFDEF MEMORYSTREAM_REALLOC_NATIVEINT}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF}): Pointer;
begin
  Result := Memory;
  if FInConstructionState then begin
    if NewCapacity <> Capacity then
      ReallocMem(Result, NewCapacity);
  end else begin
    FUpdated := True;
    ReallocMem(Result, NewCapacity); //d7 uses GlobalAllocPtr we can't free this ptr so we do the job here
  end;
end;

function TZCodePageConversionStream.Write(const Buffer;
  Count: Longint): Longint;
begin
  if (Position = 0) and (Size > 0) then
    Clear;
  Result := inherited Write(Buffer, Count);
  FUpdated := True;
end;

{ TZAbstractLob }

procedure TZAbstractLob.AfterConstruction;
var Clob: IZCLob;
    Blob: IZBlob;
begin
  QueryInterface(IZCLob, Clob);
  FWeakRefOfClob := Pointer(Clob);
  CLob := nil;
  QueryInterface(IZBLob, Blob);
  FWeakRefOfBlob := Pointer(Blob);
  BLob := nil;
  inherited;
end;

constructor TZAbstractLob.Create(ColumnCodePage: Word;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create;
  FColumnCodePage := ColumnCodePage;
  FOpenLobStreams := OpenLobStreams;
end;

function TZAbstractLob.CreateBinaryException: EZSQLException;
begin
  Result := EZSQLException.Create(Format(SOperationIsNotAllowed3, ['binary']));
end;

procedure TZAbstractLob.SetRawByteString(const Value: RawByteString;
  const CodePage: Word);
var P: PAnsiChar;
    L: NativeUInt;
begin
  L := System.Length(Value);
  P := Pointer(Value);
  if P = nil then
    P := PEmptyAnsiString;
  SetPAnsiChar(P, CodePage, L)
end;

{$IFNDEF NO_ANSISTRING}
procedure TZAbstractLob.SetAnsiString(const Value: AnsiString);
begin
  SetRawByteString(Value, ZOSCodePage);
end;
{$ENDIF NO_ANSISTRING}

procedure TZAbstractLob.SetBytes(const Value: TBytes);
begin
  SetPAnsiChar(Pointer(Value), zCP_Binary, System.Length(Value));
end;

procedure TZAbstractLob.SetOnUpdateHandler(Handler: TOnLobUpdate;
  AField: NativeInt);
begin
  FOnUpdateHandler := Handler;
  FField := AField;
end;

{$IFNDEF NO_UTF8STRING}
procedure TZAbstractLob.SetUTF8String(const Value: UTF8String);
begin
  SetRawByteString(Value, zCP_UTF8);
end;
{$ENDIF NO_UTF8STRING}

procedure TZAbstractLob.SetUnicodeString(const Value: UnicodeString);
var P: PWideChar;
    L: NativeUInt;
begin
  L := System.Length(Value);
  P := Pointer(Value);
  if P = nil then
    P := PEmptyUnicodeString;
  SetPWideChar(P, L)
end;

procedure TZAbstractLob.SetUpdated(Value: Boolean);
begin
  if (FLobStreamMode = lsmRead) and Value then
    raise CreateReadOnlyException;
  FIsUpdated := Value;
  if Value and Assigned(FOnUpdateHandler) then
    FOnUpdateHandler(FField);
end;

function TZAbstractLob.GetAnsiStream: TStream;
begin
  {$IFDEF FPC}Result := nil;{$ENDIF}
  if FWeakRefOfClob <> nil
  then Result := GetStream(ZOSCodePage)
  else raise CreateBinaryException;
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractLob.GetAnsiString: AnsiString;
begin
  Result := GetRawByteString(ZOSCodePage);
end;
{$ENDIF NO_ANSISTRING}

function TZAbstractLob.GetRawByteStream: TStream;
var CP: Word;
begin
  {$IFDEF FPC}Result := nil;{$ENDIF}
  if FWeakRefOfClob <> nil
  then if FColumnCodePage = zCP_UTF16
    then CP := RawControlsCP
    else CP := FColumnCodePage
  else CP := zCP_Binary;
  Result := GetStream(CP)
end;

function TZAbstractLob.GetRawByteString(CodePage: Word): RawByteString;
var P: PAnsiChar;
    Len: NativeUint;
begin
  Result := '';
  if (FWeakRefOfClob = nil) and (CodePage <> zCP_Binary) then
    raise CreateBinaryException;
  if (FWeakRefOfClob <> nil)
  then P := IZClob(FWeakRefOfClob).GetPAnsiChar(CodePage, Result, Len)
  else P := IZBlob(FWeakRefOfBlob).GetBuffer(Result, Len);
  if P <> Pointer(Result) then
    ZSetString(P, Len, Result{$IFDEF WITH_RAWBYTESTRING}, CodePage{$ENDIF});
end;

function TZAbstractLob.GetStream(CodePage: Word): TStream;
begin
  Result := CreateLobStream(CodePage, FLobStreamMode);
end;

function TZAbstractLob.GetStream: TStream;
begin
  Result := GetStream(FColumnCodePage);
end;

function TZAbstractLob.GetString: RawByteString;
begin
  if IsClob and (FColumnCodePage = zCP_UTF16)
  then Result := GetRawByteString(GetW2A2WConversionCodePage(FConSettings))
  else Result := GetRawByteString(FColumnCodePage);
end;

function TZAbstractLob.GetUnicodeStream: TStream;
begin
  {$IFDEF FPC}Result := nil;{$ENDIF}
  if FWeakRefOfClob <> nil
  then Result := GetStream(zCP_UTF16)
  else raise CreateBinaryException;
end;

function TZAbstractLob.GetUnicodeString: UnicodeString;
var P: PWideChar;
    Len: NativeUInt;
begin
  if FWeakRefOfClob <> nil then begin
    Result := '';
    P := IZClob(FWeakRefOfClob).GetPWideChar(Result, Len);
    if P <> Pointer(Result) then
      System.SetString(Result, P, Len);
  end else raise CreateBinaryException;
end;

function TZAbstractLob.GetUTF8Stream: TStream;
begin
  {$IFDEF FPC}Result := nil;{$ENDIF}
  if FWeakRefOfClob <> nil
  then Result := GetStream(zCP_UTF8)
  else raise CreateBinaryException;
end;

{$IFNDEF NO_UTF8STRING}
function TZAbstractLob.GetUTF8String: UTF8String;
begin
  Result := GetRawByteString(zCP_UTF8);
end;
{$ENDIF NO_UTF8STRING}

function TZAbstractLob.IsCached: Boolean;
begin
  Result := (Self is TZVarLenDataRefLob);  // True if cached.
end;

function TZAbstractLob.IsClob: Boolean;
begin
  Result := FWeakRefOfClob <> nil
end;

function TZAbstractLob.IsUpdated: Boolean;
begin
  Result := FIsUpdated;
end;

procedure TZAbstractLob.Open(LobStreamMode: TZLobStreamMode);
begin
  FLobStreamMode := LobStreamMode;
end;

procedure TZAbstractLob.SetStream(const Value: TStream);
var CP: Word;
begin
  if IsCLob
  then CP := zCP_None
  else CP := zCP_Binary;
  SetStream(Value, CP);
end;

procedure TZAbstractLob.SetString(const Value: RawByteString);
var CP: Word;
begin
  if FWeakRefOfClob <> nil
  then if FColumnCodePage = zCP_UTF16
    then CP := RawControlsCP
    else CP := FColumnCodePage
  else CP := zCP_Binary;
  SetRawByteString(Value, CP);
end;

{ TZAbstractStreamedLob }

function TZAbstractStreamedLob.GetBuffer(var LocalBuffer: RawByteString;
  out Len: NativeUInt): Pointer;
var Stream: TStream;
begin
  LocalBuffer := '';
  if IsEmpty then
    Result := nil
  else begin
    Stream := CreateLobStream(FColumnCodePage, lsmRead);
    Len := Stream.Size;
    try
      SetLength(LocalBuffer, Len);
      Stream.Read(Pointer(LocalBuffer)^, Len);
    finally
      Stream.Free;
    end;
    Result := Pointer(LocalBuffer);
    if (Result = nil) and (FColumnCodePage <> zCP_Binary) then
      Result := PEmptyUnicodeString;
  end;
end;

function TZAbstractStreamedLob.GetBytes: TBytes;
var Stream: TStream;
begin
  {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
  Stream := CreateLobStream(FColumnCodePage, lsmRead);
  try
    if Stream.Size > 0 then begin
      SetLength(Result, Stream.Size);
      Stream.Read(Pointer(Result)^, Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;


function TZAbstractStreamedLob.GetPAnsiChar(CodePage: Word;
  var ConversionBuf: RawByteString; out Len: NativeUInt): PAnsiChar;
var Stream: TStream;
begin
  if IsEmpty then begin
    Result := nil;
    Len := 0;
  end else begin
    Stream := CreateLobStream(CodePage, lsmRead);
    Len := Stream.Size;
    try
      ZSetString(nil, Len, ConversionBuf{$IFDEF WITH_RAWBYTESTRING}, CodePage{$ENDIF});
      Stream.Read(Pointer(ConversionBuf)^, Len);
    finally
      Stream.Free;
    end;
    Result := Pointer(ConversionBuf);
    if Result = nil then
      Result := PEmptyAnsiString;
  end;
end;

function TZAbstractStreamedLob.GetPWideChar(var ConversionBuf: UnicodeString;
  Out Len: NativeUint): PWideChar;
var Stream: TStream;
begin
  if IsEmpty then begin
    Result := nil;
    Len := 0;
  end else begin
    Stream := CreateLobStream(zCP_UTF16, lsmRead);
    try
      Len := Stream.Size shr 1;
      SetLength(ConversionBuf, Len);
      Stream.Read(Pointer(ConversionBuf)^, Stream.Size);
    finally
      Stream.Free;
    end;
    Result := Pointer(ConversionBuf);
    if Result = nil then
      Result := PEmptyUnicodeString;
  end;
end;

procedure TZAbstractStreamedLob.SetBuffer(Buffer: Pointer;
  Len: NativeUInt);
var Stream: TStream;
begin
  Stream := CreateLobStream(FColumnCodePage, lsmWrite);
  try
    Stream.Write(Buffer, Len)
  finally
    Stream.Free;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractStreamedLob.SetCodePageTo(Value: Word);
begin
  //{$Message 'todo remove SetCodePageTo. code page conversion should happen by recreating the lob or we use behavior injection technics!'}
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractStreamedLob.SetPAnsiChar(Buffer: PAnsiChar;
  CodePage: Word; Len: NativeUint);
var Stream: TStream;
begin
  if (Buffer = nil) or ((CodePage = zCP_Binary) and (Len = 0)) then
    Clear
  else begin
    Stream := CreateLobStream(CodePage, lsmWrite);
    try
      Stream.Write(Buffer^, Len)
    finally
      Stream.Free;
    end;
  end;
end;

procedure TZAbstractStreamedLob.SetPWideChar(Buffer: PWideChar;
  Len: NativeUInt);
var Stream: TStream;
begin
  if (Buffer = nil) then
    Clear
  else begin
    Stream := CreateLobStream(zCP_UTF16, lsmWrite);
    try
      Stream.Write(Buffer^, Len shl 1)
    finally
      Stream.Free;
    end;
  end;
end;

procedure TZAbstractStreamedLob.SetStream(const Value: TStream;
  CodePage: Word);
var Stream: TStream;
begin
  if Value = nil then
    Clear
  else begin
    Stream := GetStream(CodePage);
    try
      if Stream = Value then
        raise EZSQLException.Create('Source- and Deststream are equal');
      Stream.Size := 0;
      Stream.CopyFrom(Value, Value.Size);
      Value.Position := 0;
      FIsUpdated := True;
    finally
      Stream.Free;
    end;
  end;
end;

{ TZVarLenDataRefLob }

procedure TZVarLenDataRefLob.Clear;
begin
  InternalClear;
end;

constructor TZVarLenDataRefLob.Create(CodePage: Word;
  DataRefAddress: PZVarLenDataRef; ConSettings: PZConSettings;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(CodePage, OpenLobStreams);
  FDataRefAddress := DataRefAddress;
  FColumnCodePage := CodePage;
  FConSettings := ConSettings;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LocalBuffer" not used} {$ENDIF}
function TZVarLenDataRefLob.GetBuffer(var LocalBuffer: RawByteString;
  out Len: NativeUInt): Pointer;
label SetNil;
begin
  if FDataRefAddress.IsNotNull = 1 then
    if FDataRefAddress.VarLenData = nil then begin
      if FColumnCodePage <> zCP_Binary
      then Result := PEmptyUnicodeString else
SetNil: Result := nil;
      Len := 0;
    end else begin
      Result := @FDataRefAddress.VarLenData.Data;
      Len := FDataRefAddress.VarLenData.Len;
    end
  else goto SetNil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


function TZVarLenDataRefLob.GetBytes: TBytes;
var P: PAnsiChar;
    Len: NativeUInt;
    R: RawByteString;
begin
  R := EmptyRaw;
  P := GetPAnsiChar(zCP_Binary, R, Len);
  Result := BufferToBytes(P, Len);
end;

function TZVarLenDataRefLob.GetPAnsiChar(CodePage: Word;
  var Buffer: RawByteString; out Len: NativeUInt): PAnsiChar;
var P: Pointer;
label FromA;
begin
  if (FWeakRefOfClob = nil) and (CodePage <> zCP_Binary) then
    raise CreateBinaryException;
  if FLobStreamMode = lsmWrite then
    raise CreateWriteOnlyException;

  if FDataRefAddress.IsNotNull = 0 then begin
    Result := nil;
    Len := 0;
  end else if FDataRefAddress.VarLenData = nil then begin
    if CodePage = zCP_Binary
    then Result := nil
    else Result := PEmptyAnsiString;
    Len := 0;
  end else begin
    Result := @FDataRefAddress.VarLenData.Data;
    Len := FDataRefAddress.VarLenData.Len;
    if FColumnCodePage = CodePage then Exit;
    if FColumnCodePage = zCP_UTF16 then begin
      Buffer := PUnicodeToRaw(PWideChar(Result), Len shr 1, CodePage);
FromA:Len := System.Length(Buffer);
      if Len = 0
      then Result := PEmptyAnsiString
      else Result := Pointer(Buffer)
    end else begin
      GetMem(P, (Len+1) shl 1);
      Len := ZEncoding.PRaw2PUnicodeBuf(Result, P, Len, FColumnCodePage);
      Buffer := PUnicodeToRaw(P, Len, CodePage);
      FreeMem(P);
      goto FromA;
    end;
  end;
end;

function TZVarLenDataRefLob.GetPWideChar(
  var Buffer: UnicodeString; out Len: NativeUInt): PWideChar;
begin
  if FWeakRefOfClob = nil then
    raise CreateBinaryException;
  if FLobStreamMode = lsmWrite then
    raise CreateWriteOnlyException;
  if FDataRefAddress.IsNotNull = 0 then begin
    Result := nil;
    Len := 0;
  end else if FDataRefAddress.VarLenData = nil then begin
    Result := PEmptyUnicodeString;
    Len := 0;
  end else begin
    Result := @FDataRefAddress.VarLenData.Data;
    Len := FDataRefAddress.VarLenData.Len;
    if FColumnCodePage = zCP_UTF16 then begin
      Len := Len shr 1;
      Exit;
    end;
    Buffer := PRawToUnicode(PAnsiChar(Result), Len, FColumnCodePage);
    Len := System.Length(Buffer);
    if Len = 0
    then Result := PEmptyUnicodeString
    else Result := Pointer(Buffer)
  end;
end;

function TZVarLenDataRefLob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  Result := TZVarVarLenDataRefStream.Create(Self, FColumnCodePage, FDataRefAddress, LobStreamMode, FOpenLobStreams);
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;

procedure TZVarLenDataRefLob.InternalClear;
begin
  if FDataRefAddress.VarLenData <> nil then begin
    FreeMem(FDataRefAddress.VarLenData);
    FDataRefAddress.VarLenData := nil;
  end;
  FDataRefAddress.IsNotNull := 0;
  FIsUpdated := True;
end;

function TZVarLenDataRefLob.IsEmpty: Boolean;
begin
  Result := FDataRefAddress.IsNotNull = 0
end;

function TZVarLenDataRefLob.Length: Integer;
begin
  if (FDataRefAddress.IsNotNull = 0)
  then Result := -1
  else if (FDataRefAddress.VarLenData = nil)
    then Result := 0
    else Result := FDataRefAddress.VarLenData.Len;
end;

procedure TZVarLenDataRefLob.SetBuffer(Buffer: Pointer;
  Len: NativeUInt);
begin
  if FColumnCodePage <> zCP_Binary then
    raise CreateNonBinaryException;
  InternalClear;
  if (Buffer <> nil) and (Len >0) then begin
    SetCapacity(Len);
    Move(Buffer^, FDataRefAddress.VarLenData.Data, Len);
  end;
end;

procedure TZVarLenDataRefLob.SetCapacity(Value: NativeUint);
var Current: Cardinal;
begin
  if (FDataRefAddress.VarLenData = nil)
  then Current := 0
  else Current := FDataRefAddress.VarLenData.Len;
  if Value <> Current then
    if Value = 0 then begin
       FreeMem(FDataRefAddress.VarLenData);
       FDataRefAddress.VarLenData := nil;
    end else begin
      ReallocMem(FDataRefAddress.VarLenData, Value+SizeOf(Cardinal)+Byte(FColumnCodePage <> zCP_Binary)+Byte(FColumnCodePage = zCP_UTF16));
      FDataRefAddress.VarLenData.Len := Value;
    end;
end;

procedure TZVarLenDataRefLob.SetCodePageTo(Value: Word);
begin
  if FColumnCodePage <> Value then
    raise EZSQLException.Create('codepage conversion for this class is not possible');
end;

procedure TZVarLenDataRefLob.SetPAnsiChar(Buffer: PAnsiChar;
  CodePage: Word; Len: NativeUInt);
  procedure SetW;
  var P: Pointer;
    WL: LengthInt;
  begin
    GetMem(P, (Len+1) shl 1);
    try
      WL := ZEncoding.PRaw2PUnicodeBuf(Buffer, P, Len, CodePage);
      SetPWidechar(P, WL);
    finally
      FreeMem(P);
    end;
  end;
begin
  if (FColumnCodePage = zCP_Binary) and (CodePage <> zCP_Binary) then
    raise CreateBinaryException;
  InternalClear;
  if (Buffer <> nil) then begin
    FDataRefAddress.IsNotNull := 1;
    if Len > 0 then
      if (CodePage = FColumnCodePage) or (FColumnCodePage = zCP_None) or (FColumnCodePage = zCP_Binary) then begin
        FColumnCodePage := CodePage;
        GetMem(FDataRefAddress.VarLenData, SizeOf(Cardinal)+Len+Byte(CodePage <> zCP_Binary));
        Move(Buffer^, FDataRefAddress.VarLenData.Data, Len);
        if CodePage <> zCP_Binary then
          PByte(PAnsiChar(@FDataRefAddress.VarLenData.Data)+Len)^ := 0;
        FDataRefAddress.VarLenData.Len := Len;
      end else SetW;
  end;
end;

procedure TZVarLenDataRefLob.SetPWideChar(Buffer: PWideChar;
  Len: NativeUInt);
  procedure SetA;
  var P: Pointer;
    AL: LengthInt;
  begin
    AL := Len shl 2;
    GetMem(P, AL+1);
    try
      AL := ZEncoding.PUnicode2PRawBuf(Buffer, P, Len, Al, FColumnCodePage);
      SetPAnsiChar(P, FColumnCodePage, AL);
    finally
      FreeMem(P);
    end;
  end;
begin
  if (FColumnCodePage = zCP_Binary) then
    raise CreateBinaryException;
  InternalClear;
  if (Buffer <> nil) then begin
    FDataRefAddress.IsNotNull := 1;
    if Len > 0 then
      if (FColumnCodePage = zCP_UTF16) or (FColumnCodePage = zCP_None) then begin
        FColumnCodePage := zCP_UTF16;
        Len := Len shl 1;
        GetMem(FDataRefAddress.VarLenData, (SizeOf(Cardinal)+Len+2));
        Move(Buffer^, FDataRefAddress.VarLenData.Data, Len);
        PWord(PAnsiChar(@FDataRefAddress.VarLenData.Data)+Len)^ := 0;
        FDataRefAddress.VarLenData.Len := Len;
      end else SetA;
  end;
end;

procedure TZVarLenDataRefLob.SetStream(const Value: TStream;
  CodePage: Word);
var P: Pointer;
  L: NativeUInt;
begin
  InternalClear;
  if Value = nil then Exit;
  L := Value.Size;
  Value.Position := 0;
  if (L = 0) then begin
    if (FColumnCodePage <> zCP_Binary) then
      FDataRefAddress.IsNotNull := 1;
    Exit;
  end;
  FDataRefAddress.IsNotNull := 1;
  if (CodePage = FColumnCodePage) or (FColumnCodePage = zCP_Binary) then begin
    GetMem(FDataRefAddress.VarLenData, SizeOf(Cardinal)+L+Byte(FColumnCodePage <> zCP_Binary)+Byte(FColumnCodePage=zCP_UTF16));
    Value.Read(FDataRefAddress.VarLenData.Data, L);
    if FColumnCodePage = zCP_UTF16
    then PWord(PAnsiChar(@FDataRefAddress.VarLenData.Data)+L)^ := 0
    else if FColumnCodePage <> zCP_Binary then
      PByte(PAnsiChar(@FDataRefAddress.VarLenData.Data)+L)^ := 0;
    FDataRefAddress.VarLenData.Len := L;
  end else begin
    GetMem(P, L);
    try
      Value.Read(P^, L);
      if CodePage = zCP_UTF16
        then SetPWideChar(P, L)
        else SetPAnsiChar(P, CodePage, L);
    finally
      FreeMem(P);
    end;
  end;
end;

{ TZVarVarLenDataRefStream }

constructor TZVarVarLenDataRefStream.Create(const Owner: IZLob; CodePage:Word;
  VarLenDataRef: PZVarLenDataRef; LobStreamMode: TZLobStreamMode;
  const OpenLobStreams: TZSortedList);
var P: Pointer;
  L: NativeUInt;
begin
  FOwner := Owner;
  FVarLenDataRef := VarLenDataRef;
  Assert(VarLenDataRef <> nil); //Debug only
  FColumnCodePage := CodePage;
  fLobStreamMode := LobStreamMode;
  FOpenLobStreams := OpenLobStreams;
  if FOpenLobStreams <> nil then
    FOpenLobStreams.Add(Pointer(Self));
  if FVarLenDataRef.VarLenData = nil then begin
    L := 0;
    P := nil;
  end else begin
    L := FVarLenDataRef.VarLenData.Len;
    P := @FVarLenDataRef.VarLenData.Data;
  end;
  SetPointer(P, L);
end;

destructor TZVarVarLenDataRefStream.Destroy;
var P: Pointer;
  PA: PAnsiChar;
  Idx: Integer;
begin
  P := FVarLenDataRef.VarLenData;
  FVarLenDataRef.VarLenData := nil; //do not relloc memory on destruction
  if FUpdated then
    FOwner.SetUpdated(True);
  inherited; //calls realloc
  FVarLenDataRef.VarLenData := P; //the memory is owned by lob or reference
  { now add a null term for Postgres f.e.}
  if (P <> nil) and (FColumnCodePage <> zCP_Binary) then begin
    PA := @FVarLenDataRef.VarLenData.Data;
    Inc(PA, FVarLenDataRef.VarLenData.Len);
    if FColumnCodePage = zCP_UTF16
    then PWord(PA)^ := 0
    else PByte(PA)^ := 0;
  end;
  if FOpenLobStreams <> nil then begin
    idx := FOpenLobStreams.IndexOf(Pointer(Self));
    FOpenLobStreams.Delete(idx);
  end;
  FOwner := nil;
end;

function TZVarVarLenDataRefStream.Realloc(var NewCapacity: {$IFDEF FPC}PtrInt{$ELSE}{$IFDEF MEMORYSTREAM_REALLOC_NATIVEINT}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF}): Pointer;
begin
  if (NewCapacity = 0) then begin
    if (FVarLenDataRef.VarLenData <> nil) then begin
      FreeMem(FVarLenDataRef.VarLenData);
      FVarLenDataRef.VarLenData := nil;
      FVarLenDataRef.IsNotNull := 0;
    end;
    Result := nil;
  end else begin
    ReallocMem(FVarLenDataRef.VarLenData, NewCapacity+SizeOf(Cardinal)+Byte(FColumnCodePage<>zCP_Binary)+Byte(FColumnCodePage=zCP_UTF16));
    FVarLenDataRef.VarLenData.Len := NewCapacity;
    Result := @FVarLenDataRef.VarLenData.Data;
    FVarLenDataRef.IsNotNull := 1;
  end;
  FUpdated := True;
end;

function TZVarVarLenDataRefStream.Write(const Buffer; Count: Longint): Longint;
begin
  FUpdated := True;
  Result := inherited Write(Buffer, Count);
end;

{ TZCodePageFixedVarLenDataRefLob }

procedure TZCodePageFixedVarLenDataRefLob.SetCodePageTo(Value: Word);
begin
  if FColumnCodePage <> Value then
    raise EZSQLException.Create('codepage conversion for this class is not possible');
end;

{ TZCodePageVariableVarLenDataRefLob }

procedure TZCodePageVariableVarLenDataRefLob.SetCodePageTo(Value: Word);
var P: Pointer;
  L: NativeUInt;
label W2A;
begin
  if (FColumnCodePage <> Value) and (FDataRefAddress.VarLenData <> nil) then begin
    if Value = zCP_Binary then
      raise CreateNonBinaryException;
    if Value = zCP_None then
      raise EZSQLException.Create('codepage invalid');
    if FColumnCodePage = zCP_UTF16 then begin
W2A:  L := FDataRefAddress.VarLenData.Len shl 1;
      GetMem(P, L+1);
      try
        L := ZEncoding.PUnicode2PRawBuf(@FDataRefAddress.VarLenData.Data, P,
          FDataRefAddress.VarLenData.Len shr 1, L, Value);
        FreeMem(FDataRefAddress.VarLenData);
        GetMem(FDataRefAddress.VarLenData, L+SizeOf(Cardinal)+1);
        Move(P^, FDataRefAddress.VarLenData.Data, L);
        PByte(PAnsiChar(@FDataRefAddress.VarLenData.Data)+L)^ := 0;
        FDataRefAddress.VarLenData.Len := L;
      finally
        FreeMem(P);
      end;
      FColumnCodePage := Value;
    end else begin
      L := FDataRefAddress.VarLenData.Len shl 1;
      GetMem(P, L+2);
      try
        L := ZEncoding.PRaw2PUnicodeBuf(@FDataRefAddress.VarLenData.Data, P,
          FDataRefAddress.VarLenData.Len, FColumnCodePage);
        FreeMem(FDataRefAddress.VarLenData);
        L := L shl 1;
        GetMem(FDataRefAddress.VarLenData, SizeOf(Cardinal)+L+2);
        Move(P^, FDataRefAddress.VarLenData.Data, L);
        PWord(PAnsiChar(@FDataRefAddress.VarLenData.Data)+L)^ := 0;
        FDataRefAddress.VarLenData.Len := L;
      finally
        FreeMem(P);
      end;
      FColumnCodePage := zCP_UTF16;
      if Value <> zCP_UTF16 then
        goto W2A;
    end;
  end;
end;

{ TZReadOnlyDataRefStream }

constructor TZReadOnlyDataRefStream.Create(const Owner: IZLob; Data: Pointer;
  DataLen: NativeUInt; const OpenLobStreams: TZSortedList);
begin
  inherited Create;
  FOwner := Owner;
  FOpenLobStreams := OpenLobStreams;
  FOpenLobStreams.Add(Pointer(Self));
  SetPointer(Data, DataLen);
end;

destructor TZReadOnlyDataRefStream.Destroy;
var idx: Integer;
begin
  idx := FOpenLobStreams.IndexOf(Pointer(Self));
  FOpenLobStreams.Delete(Idx);
  inherited;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "NewSize" not used} {$ENDIF}
procedure TZReadOnlyDataRefStream.SetSize(NewSize: Longint);
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "NewSize,Count" not used}
{$ENDIF}
function TZReadOnlyDataRefStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZCodePageConversionROStream }

function TZCodePageConversionROStream.Realloc(var NewCapacity: {$IFDEF FPC}PtrInt{$ELSE}{$IFDEF MEMORYSTREAM_REALLOC_NATIVEINT}NativeInt{$ELSE}Longint{$ENDIF}{$ENDIF}): Pointer;
begin
  if FInConstructionState
  then Result := inherited Realloc(NewCapacity)
  else raise CreateReadOnlyException;
end;

{ TZMemoryReferencedLob }

procedure TZMemoryReferencedLob.Clear;
begin
  raise CreateReadOnlyException;
end;

function TZMemoryReferencedLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
begin
  if FWeakRefOfClob <> nil
  then Result := TZLocalMemCLob.CreateWithData(FMemoryReference, FDataLen, FColumnCodePage, FConSettings, FOpenLobStreams)
  else Result := TZLocalMemBLob.CreateWithData(FMemoryReference, FDataLen, FOpenLobStreams);
  Result.Open(LobStreamMode);
end;

function TZMemoryReferencedLob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  Result := TZReadOnlyDataRefStream.Create(Self, FMemoryReference, FDataLen, FOpenLobStreams);
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionROStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;

constructor TZMemoryReferencedLob.CreateWithData(Buffer: Pointer;
  DataLen: NativeUInt; ColumnCodePage: Word; ConSettings: PZConSettings;
  const OpenLobStreams: TZSortedList);
begin
  inherited Create(ColumnCodePage, OpenLobStreams);
  FColumnCodePage := ColumnCodePage;
  FMemoryReference := Buffer;
  FDataLen := DataLen;
  FConSettings := ConSettings;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LocalBuffer" not used} {$ENDIF}
function TZMemoryReferencedLob.GetBuffer(var LocalBuffer: RawByteString;
  out Len: NativeUInt): Pointer;
begin
  Result := FMemoryReference;
  Len := FDataLen;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZMemoryReferencedLob.GetBytes: TBytes;
begin
  Result := BufferToBytes(FMemoryReference, FDataLen);
end;

function TZMemoryReferencedLob.GetPAnsiChar(CodePage: Word;
  var Buffer: RawByteString; out Len: NativeUInt): PAnsiChar;
var P: Pointer;
label FromA;
begin
  if (FWeakRefOfClob = nil) and (CodePage <> zCP_Binary) then
    raise CreateBinaryException;
  if FLobStreamMode = lsmWrite then
    raise CreateWriteOnlyException;

  if FMemoryReference = nil then begin
    Result := nil;
    Len := 0;
  end else if FDataLen = 0 then begin
    if CodePage = zCP_Binary
    then Result := nil
    else Result := PEmptyAnsiString;
    Len := 0;
  end else begin
    Result := FMemoryReference;
    Len := FDataLen;
    if FColumnCodePage = CodePage then Exit;
    if FColumnCodePage = zCP_UTF16 then begin
      Buffer := PUnicodeToRaw(PWideChar(Result), Len, CodePage);
FromA:Len := System.Length(Buffer);
      if Len = 0
      then Result := PEmptyAnsiString
      else Result := Pointer(Buffer)
    end else begin
      GetMem(P, (Len+1) shl 1);
      Len := ZEncoding.PRaw2PUnicodeBuf(Result, P, Len, FColumnCodePage);
      Buffer := PUnicodeToRaw(P, Len, CodePage);
      FreeMem(P);
      goto FromA;
    end;
  end;
end;

function TZMemoryReferencedLob.GetPWideChar(var Buffer: UnicodeString;
  out Len: NativeUInt): PWideChar;
begin
  if FWeakRefOfClob = nil then
    raise CreateBinaryException;
  if FLobStreamMode = lsmWrite then
    raise CreateWriteOnlyException;
  if FMemoryReference = nil then begin
    Result := nil;
    Len := 0;
  end else if FDataLen = 0 then begin
    Result := PEmptyUnicodeString;
    Len := 0;
  end else begin
    Result := FMemoryReference;
    Len := FDataLen;
    if FColumnCodePage = zCP_UTF16 then Exit;
    Buffer := PRawToUnicode(PAnsiChar(Result), Len, FColumnCodePage);
    Len := System.Length(Buffer);
    if Len = 0
    then Result := PEmptyUnicodeString
    else Result := Pointer(Buffer)
  end;
end;

function TZMemoryReferencedLob.GetUnicodeString: UnicodeString;
var P: PWideChar;
    Len: NativeUInt;
begin
  Result := '';
  P := GetPWideChar(Result, Len);
  if P <> Pointer(Result) then
    System.SetString(Result, P, Len);
end;

function TZMemoryReferencedLob.IsEmpty: Boolean;
begin
  Result := FMemoryReference = nil;
end;

function TZMemoryReferencedLob.Length: Integer;
begin
  Result := FDataLen
end;

procedure TZMemoryReferencedLob.Open(LobStreamMode: TZLobStreamMode);
begin
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LocalBuffer/Len" not used} {$ENDIF}
procedure TZMemoryReferencedLob.SetBuffer(Buffer: Pointer; Len: NativeUInt);
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZMemoryReferencedLob.SetCodePageTo(Value: Word);
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Buffer/CodePage" not used} {$ENDIF}
procedure TZMemoryReferencedLob.SetPAnsiChar(Buffer: PAnsiChar; CodePage: Word;
  Len: NativeUInt);
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Buffer/Len" not used} {$ENDIF}
procedure TZMemoryReferencedLob.SetPWideChar(Buffer: PWideChar;
  Len: NativeUInt);
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value/CodePage" not used} {$ENDIF}
procedure TZMemoryReferencedLob.SetStream(const Value: TStream; CodePage: Word);
begin
  raise CreateReadOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZMemoryReferencedBLob }

constructor TZMemoryReferencedBLob.CreateWithData(Buffer: Pointer;
  DataLen: NativeUInt; const OpenLobStreams: TZSortedList);
begin
  inherited CreateWithData(Buffer, DataLen, zCP_Binary, nil, OpenLobStreams);
end;

end.
