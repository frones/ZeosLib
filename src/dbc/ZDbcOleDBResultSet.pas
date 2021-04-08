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

unit ZDbcOleDBResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
uses
  {$IFDEF MORMOT2}
  mormot.db.core, mormot.core.datetime, mormot.core.text, mormot.core.base,
  {$ELSE MORMOT2} {$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
  {$ENDIF USE_SYNCOMMONS} {$ENDIF MORMOT2}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Contnrs, Types{$ENDIF},
  Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX, FmtBCD,
  ZSysUtils, ZDbcIntfs, ZDbcGenericResolver, ZPlainOleDBDriver, ZDbcOleDBUtils,
  ZDbcCache, ZDbcCachedResultSet, ZDbcResultSet, ZDbcResultsetMetadata,
  ZCompatibility, ZClasses, ZDbcOleDB;

{$IFDEF WITH_NOT_INLINED_WARNING}{$WARN 6058 off : Call to subroutine "operator:=(const sourc:OleVariant):" marked as inline is not inlined}{$ENDIF}
type
  /// <summary>Defines a IRowSet reference</summary>
  PRowSet = ^IRowSet;

  /// <summary> Implements an abstract OleDB ResultSet.</summary>
  TZAbstractOleDBResultSet = class(TZAbstractReadOnlyResultSet, IZResultSet)
  private
    FRowSetAddr: PRowSet;
    FZBufferSize: Integer;
    FDBBindingArray: TDBBindingDynArray;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FRowSize: NativeInt;
    FAccessor:HACCESSOR;
    FRowCount: DBROWCOUNT;
    FCurrentBufRowNo: DBROWOFFSET;
    FRowsObtained: DBCOUNTITEM;
    FHROWS: PHROWS_Array;
    FColBuffer: TByteDynArray;
    FRowStates: TDBROWSTATUSDynArray;
    fpcColumns: DBORDINAL;
    fTempBlob: IZBlob;
    fClientCP: Word;
    FOleDBConnection: IZOleDBConnection;
    FByteBuffer: PByteBuffer;
    FGetNextRowsStatus: HResult;
  private
    FData: Pointer;
    FLength: DBLENGTH;
    FwType: DBTYPE;
    FColBind: PDBBINDING;
    /// <summary>Release the fetched rows of the IAccessor object</summary>
    procedure ReleaseFetchedRows;
    /// <summary>Creates the Accessor object</summary>
    procedure CreateAccessor;
    /// <summary>Creates a conversion error.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"SQLType" the expected SQLType</param>
    /// <returns>an conversion error EZSQLException.</returns>
    function CreateOleDbConvertError(ColumnIndex: Integer; SQLType: TZSQLType): EZSQLException;
  public
    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    /// <summary>Indicates if the value of the designated column in the current
    ///  row of this <c>ResultSet</c> object is Null.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>true</c>. <c>false</c> otherwise.</returns>
    function IsNull(ColumnIndex: Integer): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>AnsiString</c>. The driver will
    ///  try to convert the value if it's not a raw value in operating system
    ///  encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetAnsiString(ColumnIndex: Integer): AnsiString;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UTF8String</c>. The driver will
    ///  try to convert the value if it's not a raw value in UTF8 encoding.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The value otherwise.</returns>
    function GetUTF8String(ColumnIndex: Integer): UTF8String;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PAnsiChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver try convert the value if it's not a
    ///  raw text value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PWideChar</c> text reference in
    ///  the pascal programming language. Live time is per call. It's not
    ///  guaranteed the address is valid after the row position changed,
    ///  or another column of same row has been accessed. It is an error to
    ///  write into the buffer. The driver will try to convert the value if it's
    ///  not a UTF16 text value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in words.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Boolean</c> value.The driver will
    ///  try to convert the value if it's not a Boolean value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>False</c>. The value otherwise.</returns>
    function GetBoolean(ColumnIndex: Integer): Boolean;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Integer</c> value.The driver will
    ///  try to convert the value if it's not a Integer value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetInt(ColumnIndex: Integer): Integer;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Cardinal</c> value.The driver will
    ///  try to convert the value if it's not a Cardinal value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetUInt(ColumnIndex: Integer): Cardinal;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Int64</c> value.The driver will
    ///  try to convert the value if it's not a Int64 value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetLong(ColumnIndex: Integer): Int64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>UInt64</c> value.The driver will
    ///  try to convert the value if it's not a UInt64 value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetULong(ColumnIndex: Integer): UInt64;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Single</c> value.The driver will
    ///  try to convert the value if it's not a Single value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetFloat(ColumnIndex: Integer): Single;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Double</c> value.The driver will
    ///  try to convert the value if it's not a Double value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetDouble(ColumnIndex: Integer): Double;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>Currency</c> value.The driver will
    ///  try to convert the value if it's not a Currency value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>0</c>. The value otherwise.</returns>
    function GetCurrency(ColumnIndex: Integer): Currency;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TBCD</c> value.The driver will
    ///  try to convert the value if it's not a TBCD value. The value will be
    ///  filled with the minimum of digits and precision.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-BCD</c>. The value otherwise.</param>
    procedure GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TGUID</c> value.The driver will
    ///  try to convert the value if it's not a ShortInt value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-UID</c>. The value otherwise.</param>
    procedure GetGUID(ColumnIndex: Integer; var Result: TGUID);
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>PByte</c> binary reference.
    ///  Live time is per call. It's not guaranteed the address is valid after
    ///  the row position changed, or another column of same row has been
    ///  accessed. It is an error to write into the buffer. The driver will try
    ///  to convert the value if it's not a binary value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Len" returns the length of the buffer value in bytes.</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. The buffer address otherwise.</returns>
    function GetBytes(ColumnIndex: Integer; out Len: NativeUInt): PByte; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZDate</c> value. The driver will
    ///  try to convert the value if it's not a Date value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZDATE</c>. The value otherwise.</param>
    procedure GetDate(ColumnIndex: Integer; var Result: TZDate); reintroduce; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTime</c> value. The driver will
    ///  try to convert the value if it's not a Time value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTime</c>. The value otherwise.</returns>
    procedure GetTime(ColumnIndex: Integer; var Result: TZTime); reintroduce; overload;
    /// <summary>Gets the value of the designated column in the current row of
    ///  this <c>ResultSet</c> object as a <c>TZTimestamp</c> value. The driver
    ///  will try to convert the value if it's not a Timestamp value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Result" if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NULL-TZTimestamp</c>. The value otherwise.</param>
    procedure GetTimeStamp(ColumnIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;
    /// <summary>Returns the value of the designated column in the current row
    ///  of this <c>ResultSet</c> object as a <c>IZBlob</c> object.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>if the value is SQL <c>NULL</c>, the value returned is
    ///  <c>NIL</c>. A <c>Blob</c> object representing the SQL <c>BLOB</c> value in
    ///  the specified column otherwise</returns>
    function GetBlob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;

    {$IFDEF WITH_COLUMNS_TO_JSON}
    /// <summary>Fill the JSONWriter with column data</summary>
    /// <param>"JSONComposeOptions" the TZJSONComposeOptions used for composing
    ///  the JSON contents</param>
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
    {$ENDIF WITH_COLUMNS_TO_JSON}
  end;

  /// <summary>Implements an OleDB readonly ResultSet.</summary>
  TZOleDBResultSet = class(TZAbstractOleDBResultSet, IZResultSet)
  protected
    /// <summary>Opens this recordset and initializes the Column information.</summary>
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      const RowSet: PRowSet; ZBufferSize: Integer);
    /// <summary>Resets the Cursor position to beforeFirst, releases server and
    ///  client resources.</summary>
    procedure ResetCursor; override;
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; override;
  end;

  /// <summary>Implements an OleDB metadata ResultSet.</summary>
  TZOleDBMetadataResultSet = Class(TZOleDBResultSet)
  private
    FRowSet: IRowSet;
  public
    /// <summary>Creates a this object and assignes the main properties.</summary>
    /// <param>"Statement" the related Statement object.</param>
    /// <param>"RowSet" the related IRowSet interface we read from.</param>
    /// <param>"ZBufferSize" the allowed block read buffersize.</param>
    constructor Create(const Statement: IZStatement; const RowSet: IRowSet;
      ZBufferSize: Integer);
  End;

  /// <summary>Implements an OleDB parameter ResultSet.</summary>
  TZOleDBParamResultSet = class(TZAbstractOleDBResultSet, IZResultSet)
  public
    /// <summary>Creates a this object and assignes the main properties.</summary>
    /// <param>"Statement" the related Statement object.</param>
    /// <param>"ParamBuffer" the Statement parameter buffer containing the data.</param>
    /// <param>"ParamBindings" the Parameter bindings containing addresses and offsets.</param>
    /// <param>"ParamNameArray" an array of parameter names.</param>
    constructor Create(const Statement: IZStatement; const ParamBuffer: TByteDynArray;
      const ParamBindings: TDBBindingDynArray; const ParamNameArray: TStringDynArray);
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; override;
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
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

  /// <summary>Implements an OleDB resultset metadate object.</summary>
  TZOleDBMSSQLResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    /// <summary>Clears specified column information.</summary>
    /// <param>"ColumnInfo" a column information object.</param>
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    /// <summary>Initializes columns with additional data.</summary>
    procedure LoadColumns; override;
  public
    /// <summary>Gets the designated column's catalog name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>catalog name or "" if not applicable.</returns>
    function GetCatalogName(ColumnIndex: Integer): string; override;
    /// <summary>Get the designated column's name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>return column name or "" if not applicable</returns>
    function GetColumnName(ColumnIndex: Integer): string; override;
    /// <summary>Retrieves the designated column's SQL type.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the ZDBC SQL type</returns>
    function GetColumnType(ColumnIndex: Integer): TZSQLType; override;
    /// <summary>Get the designated column's table's schema.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>schema name or "" if not applicable</returns>
    function GetSchemaName(ColumnIndex: Integer): string; override;
    /// <summary>Gets the designated column's table name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>table name or "" if not applicable.</returns>
    function GetTableName(ColumnIndex: Integer): string; override;
    /// <summary>Indicates whether the designated column is automatically
    ///  numbered, thus read-only.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  end;

  /// <summary>Implements a cached resolver with MSSQL specific functionality.</summary>
  TZOleDBMSSQLCachedResolver = class (TZGenerateSQLCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
    FResultSet: IZResultSet;
    fStmt: IZPreparedStatement;
  public
    /// <summary>Creates a cached resolver and assignes the main properties.</summary>
    /// <param>"Statement" the related Statement object.</param>
    /// <param>"Metadata" the related ResultSet metadata object.</param>
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    /// <summary>Destroys this object.</summary>
    destructor Destroy; override;
    /// <summary>Posts updates to database.</summary>
    /// <param>"Sender" a cached result set inteface.</param>
    /// <param>"UpdateType" a type of updates.</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

  /// <summary>Implements an OleDB specific LOB descriptor object.</summary>
  TZOleDBLob = Class(TZAbstractStreamedLob, IZLob)
  private
    FRowSet: IRowSet;
    FAccessor: HACCESSOR;
    FwType: DBTYPE;
    FCurrentRow: HROW;
    FLength: DBLENGTH;
    FOleConnection: IZOleDBConnection;
  protected
    /// <summary>Creates a lob stream</summary>
    /// <param>"CodePage" the lob codepage. 0 means it's a binary stream</param>
    /// <param>"LobStreamMode" the stream mode on open the lob. It's one of
    ///  <c>lsmRead, lsmWrite, lsmReadWrite</c></param>
    /// <returns>a TStream object</returns>
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public
    /// <summary>Creates this object and assigns main properties.</summary>
    /// <param>"RowSet" the OleDB IRowSet interface we read from.</param>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"wType" the OleDB type of the column.</param>
    /// <param>"CurrentRow" the OleDB row number.</param>
    /// <param>"Connection" the OleDB specific connection.</param>
    /// <param>"ALength" the Length of the lob.</param>
    /// <param>"OpenLobStreams" a list we register the streams if they are open.</param>
    constructor Create(const RowSet: IRowSet; ColumnIndex: Integer; wType: DBTYPE;
      CurrentRow: HROW; const Connection: IZOleDBConnection;
      ALength: DBLENGTH; const OpenLobStreams: TZSortedList);
    /// <summary>Destroys this object.</summary>
    destructor Destroy; override;
  public
    /// <summary>Get the length of the lob content</summary>
    /// <returns>the length of the lob content.</returns>
    function Length: Integer; override;
    /// <summary>Clear/NULL the lob content</summary>
    function IsEmpty: Boolean; override;
    /// <summary>Clears or NULLs the lob content</summary>
    procedure Clear; override;
  end;

  /// <summary>Implements an OleDB specific Lob-stream.</summary>
  TZOleLobStream = class(TZImmediatelyReleasableLobStream)
  private
    FSequentialStream: ISequentialStream;
    FDescriptor: TZOleDBLob;
    FPosition: Int64;
  protected
    /// <summary>Get the size of the stream</summary>
    /// <returns>the size of the stream</returns>
    function GetSize: Int64; override;
  public
    /// <summary>Create this object.</summary>
    /// <param>"Descriptor" the owner descriptor object which constructs this object.</param>
    /// <param>"OpenLobStreams" the List to register the streams if they are
    ///  open and unregister if the stream is closing.</param>
    constructor Create(const Descriptor: TZOleDBLob; const OpenLobStreams: TZSortedList);
  public // TStream overrides
    /// <summary>Attempts to read Count bytes from the stream to Buffer,
    ///  starting at the current position.</summary>
    /// <remarks>The method advances the current position in the stream by the
    ///  number of bytes actually transferred.</remarks>
    /// <param>"Buffer" a reference to a buffer the stream writes into.</param>
    /// <param>"Count" the number of bytes the steam should read from.</param>
    /// <returns>the number of bytes actually read, which may be less than Count.</returns>
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    /// <summary>Attempts to write Count bytes from buffer to the stream,
    ///  starting at the current position.</summary>
    /// <remarks>The method advances the current position in the stream by the
    ///  number of bytes actually transferred.</remarks>
    /// <param>"Buffer" a reference to a buffer the stream reads from.</param>
    /// <param>"Count" the number of bytes the steam should write from.</param>
    /// <returns>the number of bytes actually written, which may be less than Count.</returns>
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    /// <summary>Seek sets the position of the stream to Offset bytes from Origin.</summary>
    /// <param>"Offset" should be negative when the origin is soFromEnd. It
    ///  should be positive for soFromBeginning and can have both signs for
    ///  soFromCurrent origin.</param>
    /// <param>"Origin" is one of:
    ///  <c>soFromBeginning</c> Set the position relative to the start of the stream.
    ///  <c>soFromCurrent</c> Set the position relative to the current position in the stream.
    ///  <c>soFromEnd</c> Set the position relative to the end of the stream.</param>
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
  end;

  /// <summary>Implements an OleDb BLOB object</summary>
  TZOleDBBLob = Class(TZOleDBLob, IZBlob)
  public
    /// <summary>Clones the Blob</summary>
    /// <param>"LobStreamMode" the open mode of the lob. Default mode is lsmRead.</param>
    /// <returns>a memory cached Blob describtor object.</returns>
    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
  End;

  /// <summary>Implements an OleDb CLOB object</summary>
  TZOleDBCLob = Class(TZOleDBLob, IZBlob, IZCLob)
  public
    /// <summary>Clones the Clob</summary>
    /// <param>"LobStreamMode" the open mode of the lob. Default mode is lsmRead.</param>
    /// <returns>a memory cached Clob describtor object.</returns>
    function Clone(LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
  End;

  /// <summary>Implements an OleDb specific cached resultset</summary>
  TZOleDBCachedResultSet = class(TZCachedResultSet)
  private
    FResultSet: TZAbstractOleDBResultSet;
  protected
    /// <summary>Fetches one row from the wrapped result set object.</summary>
    /// <returns><c>True</c> if row was successfuly fetched or <c>False</c>
    ///  otherwise.</returns>
    function Fetch: Boolean; override;
    /// <summary>Get a provider specififc class of a TZRowAccesssor</summary>
    /// <returns>the rowaccessor class</returns>
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    /// <summary>Create this object.</summary>
    /// <param>"ResultSet" the underlaying resultset the cached resultset
    ///  clones from.</param>
    /// <param>"SQL" the query that did produce this resultset</param>
    /// <param>"Resolver" the CachedResolver object used to execute DML's or
    ///  refresh current row.</param>
    /// <param>"ConSettings" a reference to the connection settings.</param>
    constructor Create(ResultSet: TZAbstractOleDBResultSet; const SQL: string;
      const Resolver: IZCachedResolver; ConSettings: PZConSettings);
  end;

  /// <summary>Implements an OleDb specific RowAccessor object.</summary>
  TZOleDbRowAccessor = class(TZRowAccessor)
  protected
    /// <summary>convert the metadata retrieved type to a rowaccessors stored
    ///  type</summary>
    /// <param>"ColumnInfo" a columninfo object containing the metadata column
    ///  informations</param>
    /// <param>"ConSettings" a reference to the connection settings.</param>
    /// <param>"ColumnCodePage" a reference to columncodepage.</param>
    /// <returns>the RowAccessor used SQLType</returns>
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
  public
    /// <summary>Create this object.</summary>
    /// <param>"ColumnsInfo" a list of TZColumnInfo objects.</param>
    /// <param>"ConSettings" a reference to the connection settings.</param>
    /// <param>"OpenLobStreams" the List to register the streams if they are
    ///  open and unregister if the stream is closing.</param>
    /// <param>"LobCacheMode" indicates how the Lobs retrieved from the native
    ///  resultset should be memory cached.</param>
    constructor Create(ColumnsInfo: TObjectList; ConSettings: PZConSettings;
      const OpenLobStreams: TZSortedList; LobCacheMode: TLobCacheMode); override;
  end;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit

uses
  Variants, Math, DateUtils,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZMessages, ZEncoding, ZFastCode, ZTokenizer,
  ZSelectSchema, ZGenericSqlAnalyser,
  ZDbcOleDBStatement, ZDbcMetaData, ZDbcUtils, ZDbcLogging;

var
  LobReadObj: TDBObject;
  LobDBBinding: TDBBinding;

{ TZOleDBLob }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZOleDBLob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  Result := TZOleLobStream.Create(Self, FOpenLobStreams);
  if (FColumnCodePage <> zCP_Binary) and (CodePage <> FColumnCodePage) then
    Result := TZCodePageConversionStream.Create(Result, FColumnCodePage, CodePage, FConSettings, FOpenLobStreams);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

destructor TZOleDBLob.Destroy;
var
  FAccessorRefCount: DBREFCOUNT;
  Status: HResult;
begin
  if FAccessor > 0 then begin
    Status := (fRowSet As IAccessor).ReleaseAccessor(FAccessor, @FAccessorRefCount);
    if Status <> S_OK then
      FOleConnection.HandleErrorOrWarning(Status, lcOther, 'IAccessor.ReleaseAccessor',
        FOleConnection);
  end;
  inherited Destroy;
end;

function TZOleDBLob.IsEmpty: Boolean;
begin
  Result := False;
end;

function TZOleDBLob.Length: Integer;
begin
  Result := FLength;
end;

procedure TZOleDBLob.Clear;
begin
  raise CreateReadOnlyException;
end;

constructor TZOleDBLob.Create(const RowSet: IRowSet; ColumnIndex: Integer;
  wType: DBTYPE; CurrentRow: HROW; const Connection: IZOleDBConnection;
  ALength: DBLENGTH; const OpenLobStreams: TZSortedList);
var ColumnCodePage: Word;
  Status: HResult;
begin
  if wType = DBTYPE_BYTES then
    ColumnCodePage := zCP_Binary
  else if wType = DBTYPE_STR then
    ColumnCodePage := Connection.GetConSettings.ClientCodePage.CP
  else ColumnCodePage := zCP_UTF16;
  inherited Create(ColumnCodePage, OpenLobStreams);
  FConSettings := Connection.GetConSettings;
  FRowSet := RowSet;
  fWType := wType;
  FCurrentRow := CurrentRow;
  FLength := ALength;
  LobDBBinding.iOrdinal := ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF};
  FOleConnection := Connection;
  Status := (FRowSet as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA, 1,
    @LobDBBinding, 0, @FAccessor, nil);
  if Status <> S_OK then
    FOleConnection.HandleErrorOrWarning(Status, lcOther,
      'IAccessor.CreateAccessor', FOleConnection);
end;

{ TZOleLobStream }

constructor TZOleLobStream.Create(const Descriptor: TZOleDBLob;
  const OpenLobStreams: TZSortedList);
var Status: HResult;
begin
  inherited Create(Descriptor, Descriptor.FOleConnection, OpenLobStreams);
  FDescriptor := Descriptor;
  Status := Descriptor.FRowSet.GetData(Descriptor.FCurrentRow, Descriptor.FAccessor, @FSequentialStream);
  if Status <> S_OK then
    Descriptor.FOleConnection.HandleErrorOrWarning(Status, lcOther,
      'IRowSet.GetData', Descriptor.FOleConnection);
end;

function TZOleLobStream.GetSize: Int64;
begin
  Result := FDescriptor.FLength;
end;

function TZOleLobStream.Read(var Buffer; Count: Longint): Longint;
var pcbRead: ULong;
    Status: HResult;
begin
  if FSequentialStream = nil then begin
    Status := FDescriptor.FRowSet.GetData(FDescriptor.FCurrentRow, FDescriptor.FAccessor, @FSequentialStream);
    if Status <> S_OK then
      FDescriptor.FOleConnection.HandleErrorOrWarning(Status, lcOther,
        'IRowSet.GetData', Self);
  end;
  FSequentialStream.Read(@Buffer, Count, @pcbRead);
  Result := pcbRead;
  Inc(FPosition, Result);
end;

function TZOleLobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if Origin = soFromEnd then
    Result := FPosition - OffSet
  else if Origin = soFromCurrent then
    Result := FPosition + OffSet
  else begin
    Result := OffSet;
    if Offset = 0 then begin
      FSequentialStream := nil;
      FPosition := 0;
    end;
  end;
  if Result <> FPosition then
    raise EZSQLException.Create(SOperationIsNotAllowed1);
end;

function TZOleLobStream.Write(const Buffer; Count: Longint): Longint;
var pcbWritten: ULong;
    Status: HResult;
begin
  if FSequentialStream = nil then begin
    Status := FDescriptor.FRowSet.GetData(FDescriptor.FCurrentRow, FDescriptor.FAccessor, @FSequentialStream);
    if Status <> S_OK then
      FDescriptor.FOleConnection.HandleErrorOrWarning(Status, lcOther,
        'IRowSet.GetData', Self);
  end;
  FSequentialStream.Write(@Buffer, Count, @pcbWritten);
  Result := pcbWritten;
  Inc(FPosition, Result)
end;

{$IFDEF WITH_COLUMNS_TO_JSON}
procedure TZAbstractOleDBResultSet.ColumnsToJSON(JSONWriter: TJSONWriter;
  JSONComposeOptions: TZJSONComposeOptions);
var I, C, H: Integer;
    P: PAnsiChar;
    L: NativeUint;
    MS: Word;
label jmpCLob;
begin
  //init
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    if IsNull(C+FirstDbcIndex) then
      if JSONWriter.Expand then begin
        if not (jcsSkipNulls in JSONComposeOptions) then begin
          JSONWriter.AddString(JSONWriter.ColNames[I]);
          JSONWriter.AddShort('null,')
        end;
      end else
        JSONWriter.AddShort('null,')
    else begin
      if JSONWriter.Expand then
        JSONWriter.AddString(JSONWriter.ColNames[i]);
      case FwType of
        DBTYPE_EMPTY,
        DBTYPE_IDISPATCH,
        DBTYPE_IUNKNOWN:  JSONWriter.AddShort('""');
        DBTYPE_NULL:      JSONWriter.AddShort('null');
        DBTYPE_I2:        JSONWriter.Add(PSmallInt(FData)^);
        DBTYPE_I4,
        DBTYPE_ERROR:     JSONWriter.Add(PInteger(FData)^);
        DBTYPE_R4:        JSONWriter.AddSingle(PSingle(FData)^);
        DBTYPE_R8:        JSONWriter.AddDouble(PDouble(FData)^);
        DBTYPE_CY:        JSONWriter.AddCurr64(PInt64(FData){$IFNDEF MORMOT2}^{$ENDIF});
        DBTYPE_DATE:      JSONWriter.AddDateTime(PDateTime(FData), 'T', '"');
        DBTYPE_BOOL:      JSONWriter.AddShort(JSONBool[PWord(FData)^ <> 0]);
        DBTYPE_VARIANT: begin
            JSONWriter.Add('"');
            FUniTemp := POleVariant(FData)^;
            JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
            JSONWriter.Add('"');
          end;
        //DBTYPE_DECIMAL = 14;
        DBTYPE_UI1:       JSONWriter.AddU(PByte(FData)^);
        DBTYPE_I1:        JSONWriter.Add(PShortInt(FData)^);
        DBTYPE_UI2:       JSONWriter.AddU(PWord(FData)^);
        DBTYPE_UI4:       JSONWriter.AddU(PCardinal(FData)^);
        DBTYPE_I8:        JSONWriter.Add(PInt64(FData)^);
        DBTYPE_UI8:       JSONWriter.AddQ(PUInt64(FData)^);
        DBTYPE_GUID:      begin
                            {$IFNDEF MORMOT2}
                            JSONWriter.Add('"');
                            JSONWriter.Add(PGUID(FData)^);
                            JSONWriter.Add('"');
                            {$ELSE}
                            JSONWriter.Add(PGUID(FData),'"');
                            {$ENDIF}
                          end;
        DBTYPE_BYTES:
          if FColBind.cbMaxLen = 0 then begin //streamed
            fTempBlob := TZOleDBBLOB.Create(FRowSetAddr^, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
              DBTYPE_BYTES, FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams);
            P := fTempBlob.GetBuffer(fRawTemp, L);
            JSONWriter.WrBase64(P, L, true); // withMagic=true
            fTempBlob := nil;
          end else
            JSONWriter.WrBase64(FData,FLength, True);
        DBTYPE_STR: begin
            JSONWriter.Add('"');
            if FColBind.cbMaxLen = 0
            then goto jmpCLob
            else begin
              if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                FLength := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData), FLength);
              FUniTemp := PRawToUnicode(PAnsiChar(FData), FLength, fClientCP);
              JSONWriter.AddJSONEscapeW(Pointer(FUniTemp), Length(FUniTemp));
            end;
            JSONWriter.Add('"');
          end;
        DBTYPE_WSTR, DBTYPE_XML: begin
            JSONWriter.Add('"');
            if FColBind.cbMaxLen = 0 then begin
jmpCLob:      fTempBlob := TZOleDBCLOB.Create(FRowSetAddr^, C{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
                FwType, FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams);
              P := Pointer(fTempBlob.GetPWideChar(FUniTemp, L));
              JSONWriter.AddJSONEscapeW(Pointer(P), L);
              fTempBlob := nil;
            end else begin
              FLength := FLength shr 1;
              if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                FLength := GetAbsorbedTrailingSpacesLen(PWideChar(FData), FLength);
              JSONWriter.AddJSONEscapeW(FData, FLength);
            end;
            JSONWriter.Add('"');
          end;
        DBTYPE_NUMERIC: begin
                          FLength := SQL_MAX_NUMERIC_LEN;
                          SQLNumeric2Raw(FData, PAnsiChar(FByteBuffer), FLength);
                          JSONWriter.AddJSONEscape(PAnsiChar(FByteBuffer), FLength);
                        end;
        //DBTYPE_UDT = 132;
        DBTYPE_DBDATE:    begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              {$IFDEF MORMOT2}
                              JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                              {$ELSE}
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              {$ENDIF}
                            else
                              JSONWriter.Add('"');
                            if PDBDate(FData)^.year < 0 then
                              JSONWriter.Add('-');
                            DateToIso8601PChar(Pointer(FByteBuffer), True, Abs(PDBDate(FData)^.year),
                              PDBDate(FData)^.month, PDBDate(FData)^.day);
                            JSONWriter.AddNoJSONEscape(Pointer(FByteBuffer),10);
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('T00:00:00Z")')
                            else JSONWriter.Add('"');
                          end;
        DBTYPE_DBTIME:    begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("0000-00-00')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then begin
                              {$IFDEF MORMOT2}
                              JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                              {$ELSE}
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              {$ENDIF}
                            end else
                              JSONWriter.Add('"');
                            TimeToIso8601PChar(Pointer(FByteBuffer), True, PDBTime(FData)^.hour,
                              PDBTime(FData)^.minute, PDBTime(FData)^.second, 0, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(Pointer(FByteBuffer),9+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z)"')
                            else JSONWriter.Add('"');
                          end;
        DBTYPE_DBTIMESTAMP: begin
                            if jcoMongoISODate in JSONComposeOptions then
                              JSONWriter.AddShort('ISODate("')
                            else if jcoDATETIME_MAGIC in JSONComposeOptions then
                              {$IFDEF MORMOT2}
                              JSONWriter.AddShorter(JSON_SQLDATE_MAGIC_QUOTE_STR)
                              {$ELSE}
                              JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              {$ENDIF}
                            else
                              JSONWriter.Add('"');
                            if PDBTimeStamp(FData)^.year < 0 then
                              JSONWriter.Add('-');
                            DateToIso8601PChar(Pointer(FByteBuffer), True, Abs(PDBTimeStamp(FData)^.Year),
                               PDBTimeStamp(FData)^.Month, PDBTimeStamp(FData)^.Day);
                            MS := (PDBTimeStamp(FData)^.fraction * Byte(ord(jcoMilliseconds in JSONComposeOptions))) div 1000000;
                            TimeToIso8601PChar(Pointer(PAnsiChar(FByteBuffer)+10), True, PDBTimeStamp(FData)^.Hour,
                              PDBTimeStamp(FData)^.Minute, PDBTimeStamp(FData)^.Second, MS, 'T', jcoMilliseconds in JSONComposeOptions);
                            JSONWriter.AddNoJSONEscape(Pointer(FByteBuffer),19+(4*Ord(jcoMilliseconds in JSONComposeOptions)));
                            if jcoMongoISODate in JSONComposeOptions
                            then JSONWriter.AddShort('Z")')
                            else JSONWriter.Add('"');
                          end;
        DBTYPE_HCHAPTER:  JSONWriter.{$IFDEF CPU64}AddQ{$ELSE}AddU{$ENDIF}(PCHAPTER(FData)^);
        //DBTYPE_FILETIME = 64;
        //DBTYPE_PROPVARIANT = 138;
        DBTYPE_VARNUMERIC: begin
                            SQLNumeric2Raw(FData, PAnsiChar(FByteBuffer), FLength);
                            JSONWriter.AddJSONEscape(Pointer(FByteBuffer), FLength);
                          end;
      end;
      JSONWriter.Add(',');
    end;
  end;
  if jcoEndJSONObject in JSONComposeOptions then begin
    JSONWriter.CancelLastComma; // cancel last ','
    if JSONWriter.Expand then
      JSONWriter.Add('}');
  end;
end;
{$ENDIF WITH_COLUMNS_TO_JSON}

procedure TZAbstractOleDBResultSet.CreateAccessor;
var Status: HResult;
begin
  Status := (FRowSetAddr^ as IAccessor).CreateAccessor(DBACCESSOR_ROWDATA,
    fpcColumns, Pointer(FDBBindingArray), FRowSize, @FAccessor,
    Pointer(FDBBINDSTATUSArray));
  if Status <> S_OK then
    FOleDBConnection.HandleErrorOrWarning(Status, lcOther,
      'CreateAccessor', Self);
end;

function TZAbstractOleDBResultSet.CreateOleDbConvertError(ColumnIndex: Integer;
  SQLType: TZSQLType): EZSQLException;
begin
  Result := CreateConversionError(ColumnIndex, SQLType, TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType)
end;

procedure TZAbstractOleDBResultSet.ReleaseFetchedRows;
var Status: HResult;
begin
  if (FRowsObtained > 0) then begin
    Status := fRowSetAddr.ReleaseRows(FRowsObtained,FHROWS,nil,nil,Pointer(FRowStates));
    if Status <> S_OK then
      FOleDBConnection.HandleErrorOrWarning(Status, lcOther, 'IRowSet.ReleaseRows', Self);
    FOleDBConnection.GetMalloc.Free(FHROWS);
    FHROWS := nil;
    FRowsObtained := 0;
  end;
end;

function TZAbstractOleDBResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {.$R-}
  FColBind := @FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  fwType := FColBind.wType;
  Result := PDBSTATUS(@FColBuffer[FColBind.obStatus+NativeUInt(FRowSize*FCurrentBufRowNo)])^ = DBSTATUS_S_ISNULL;
  LastWasNull := Result;
  if Result then begin
    FData := nil;
    FLength := 0;
  end else begin
    //note FData is valid only if no Lobs DBPART_VALUE was set on binding!!!
    if FColBind.dwPart and DBPART_VALUE <> 0 //test if lob's are bound
    then FData := @FColBuffer[FColBind.obValue+NativeUInt(FRowSize*FCurrentBufRowNo)]
    else FData := nil;
    if (FColBind.wType and DBTYPE_BYREF = DBTYPE_BYREF) then begin
      if (FData <> nil) then
        FData := PPointer(FData)^;
      FwType := fWType and not DBTYPE_BYREF;
    end;
    //note FLength is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
    if FColBind.dwPart and DBPART_LENGTH <> 0 //fixed types don't have a length indicator
    then FLength := PDBLENGTH(@FColBuffer[FColBind.obLength+NativeUInt(FRowSize*FCurrentBufRowNo)])^
    else FLength := 0;
  end;
  //{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZAbstractOleDBResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var P: Pointer;
  L: NativeUInt;
begin
  Result := '';
  case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
    DBTYPE_VARIANT,
    DBTYPE_STR, DBTYPE_STR or DBTYPE_BYREF,
    DBTYPE_WSTR, DBTYPE_WSTR or DBTYPE_BYREF: begin
        P := GetPWideChar(ColumnIndex, L);
        PUnicodeToRaw(P, L, zOSCodePage, RawByteString(Result));
      end
    else begin
        P := GetPAnsiChar(ColumnIndex, L);
        System.SetString(Result, PAnsichar(P), L);
      end;
  end;
end;

function TZAbstractOleDBResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var P: Pointer;
  L: NativeUInt;
begin
  Result := '';
  case FDBBindingArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].wType of
    DBTYPE_VARIANT,
    DBTYPE_STR, DBTYPE_STR or DBTYPE_BYREF,
    DBTYPE_WSTR, DBTYPE_WSTR or DBTYPE_BYREF: begin
        P := GetPWideChar(ColumnIndex, L);
        PUnicodeToRaw(P, L, zCP_UTF8, RawByteString(Result));
      end
    else begin
        P := GetPAnsiChar(ColumnIndex, L);
        ZSetString(PAnsiChar(P), L, Result);
      end;
  end;
end;

function TZAbstractOleDBResultSet.GetPAnsiChar(ColumnIndex: Integer;
  out Len: NativeUInt): PAnsiChar;
label set_from_tmp, set_from_buf, set_from_num;
begin
  if IsNull(ColumnIndex) then begin //Sets LastWasNull, FData, FLength!!
    Result := nil;
    Len := 0;
  end else case FwType of
    DBTYPE_BOOL:      if PWordBool(FData)^ then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
    DBTYPE_I1:        begin
                        IntToRaw(Integer(PShortInt(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I2:        begin
                        IntToRaw(Integer(PSmallInt(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I4,
    DBTYPE_ERROR:     begin
                        IntToRaw(PInteger(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I8:        begin
                        IntToRaw(PInt64(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI1:       begin
                        IntToRaw(Cardinal(PByte(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI2:       begin
                        IntToRaw(Cardinal(PWord(FData)^), PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFNDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI4:       begin
                        IntToRaw(PCardinal(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI8:       begin
                        IntToRaw(PUInt64(FData)^, PAnsiChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_R4:        begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PSingle(FData)^, Result);
                      end;
    DBTYPE_R8:        begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := FloatToSQLRaw(PDouble(FData)^, Result);
                      end;
    DBTYPE_CY:        begin
                        CurrToRaw(PCurrency(FData)^, '.', PAnsiChar(fByteBuffer), @Result);
set_from_buf:           Len := Result - PAnsiChar(fByteBuffer);
                        Result := PAnsiChar(fByteBuffer);
                      end;
    DBTYPE_DATE:      begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := DateTimeToRawSQLTimeStamp(PDateTime(FData)^,
                          Result, ConSettings.ReadFormatSettings, False);
                      end;
    DBTYPE_DBDATE:    begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := DateToRaw(Abs(PDBDate(FData)^.year),
                          PDBDate(FData)^.month, PDBDate(FData)^.day,
                          Result, ConSettings.ReadFormatSettings.DateFormat,
                          False, PDBDate(FData)^.year < 0);
                      end;
    DBTYPE_DBTIME:    begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := TimeToRaw(PDBTime(FData)^.hour,
                          PDBTime(FData)^.minute, PDBTime(FData)^.second,0,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
    DBTYPE_DBTIMESTAMP: begin
                        Result := PAnsiChar(fByteBuffer);
                        Len := DateTimeToRaw(Word(Abs(PDBTimeStamp(FData)^.year)),
                          PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day,
                          PDBTimeStamp(FData)^.hour, PDBTimeStamp(FData)^.minute,
                          PDBTimeStamp(FData)^.second, PDBTimeStamp(FData)^.fraction,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat,
                          False, PDBTimeStamp(FData)^.year < 0);
                      end;
    DBTYPE_VARIANT:   begin
                        FUniTemp := POleVariant(FData)^;
                        PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), GetW2A2WConversionCodePage(ConSettings), FRawTemp);
                        goto set_from_tmp;
                      end;
    DBTYPE_GUID:      begin
                        GUIDToBuffer(FData, PAnsiChar(fByteBuffer), []);
                        Len := 36;
                        Result := PAnsiChar(fByteBuffer);
                      end;
    DBTYPE_STR:       if FColBind.cbMaxLen = 0 then begin
                        fTempBlob := GetBlob(ColumnIndex);
                        Result := fTempBlob.GetPAnsiChar(fClientCP, fRawTemp, Len);
                        fTempBlob := nil;
                      end else begin
                        Result := FData;
                        if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0
                        then Len := GetAbsorbedTrailingSpacesLen(Result, FLength)
                        else Len := FLength;
                      end;
    DBTYPE_WSTR:    if FColBind.cbMaxLen = 0 then begin
                      fTempBlob := GetBlob(ColumnIndex);
                      Result := fTempBlob.GetPAnsiChar(GetW2A2WConversionCodePage(ConSettings), fRawTemp, Len);
                      fTempBlob := nil;
                    end else begin
                      Result := FData;
                      FLength := Flength shr 1;
                      if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0
                      then Len := GetAbsorbedTrailingSpacesLen(PWideChar(Result), FLength)
                      else Len := FLength;
                      PUnicodeToRaw(PWideChar(Result), Len, GetW2A2WConversionCodePage(ConSettings), FRawTemp);
set_from_tmp:         Len := Length(FRawTemp);
                      if Len = 0
                      then Result := PEmptyAnsiString
                      else Result := Pointer(FRawTemp);
                    end;
    DBTYPE_NUMERIC: begin
                      Len := SQL_MAX_NUMERIC_LEN;
                      goto set_from_num;
                    end;
    DBTYPE_VARNUMERIC: begin
                      Len := FLength;
set_from_num:         Result := PAnsiChar(fByteBuffer);
                      SQLNumeric2Raw(fData, Result, Len);
                    end;
    //DBTYPE_UDT	= 132;
    //DBTYPE_FILETIME	= 64;
    //DBTYPE_PROPVARIANT	= 138;
    else raise CreateOleDbConvertError(ColumnIndex, stString);
  end;
end;

function TZAbstractOleDBResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
label set_from_tmp, set_from_buf, set_from_clob, set_from_num;
begin
  if IsNull(ColumnIndex) then begin //Sets LastWasNull, FData, FLength!!
    Result := nil;
    Len := 0;
  end else case FwType of
    DBTYPE_BOOL:      if PWordBool(FData)^ then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
    DBTYPE_I1:        begin
                        IntToUnicode(Integer(PShortInt(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I2:        begin
                        IntToUnicode(Integer(PSmallInt(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I4,
    DBTYPE_ERROR:     begin
                        IntToUnicode(PInteger(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_I8:        begin
                        IntToUnicode(PInt64(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI1:       begin
                        IntToUnicode(Cardinal(PByte(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_UI2:       begin
                        IntToUnicode(Cardinal(PWord(FData)^), PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFNDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI4:       begin
                        IntToUnicode(PCardinal(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    {$IFDEF CPUX64}DBTYPE_HCHAPTER,{$ENDIF} //NativeUnit
    DBTYPE_UI8:       begin
                        IntToUnicode(PUInt64(FData)^, PWideChar(fByteBuffer), @Result);
                        goto set_from_buf;
                      end;
    DBTYPE_R4:        begin
                        Result := PWideChar(fByteBuffer);
                        Len := FloatToSQLUnicode(PSingle(FData)^, Result);
                      end;
    DBTYPE_R8:        begin
                        Result := PWideChar(fByteBuffer);
                        Len := FloatToSQLUnicode(PDouble(FData)^, Result);
                      end;
    DBTYPE_CY:        begin
                        CurrToUnicode(PCurrency(FData)^, '.', PWideChar(fByteBuffer), @Result);
set_from_buf:           Len := Result - PWideChar(fByteBuffer);
                        Result := PWideChar(fByteBuffer);
                      end;
    DBTYPE_DATE:      begin
                        Result := PWideChar(fByteBuffer);
                        Len := DateTimeToUnicodeSQLTimeStamp(PDateTime(FData)^,
                          Result, ConSettings.ReadFormatSettings, False);
                      end;
    DBTYPE_DBDATE:    begin
                        Result := PWideChar(fByteBuffer);
                        Len := DateToUni(Abs(PDBDate(FData)^.year),
                          PDBDate(FData)^.month, PDBDate(FData)^.day,
                          Result, ConSettings.ReadFormatSettings.DateFormat,
                          False, PDBDate(FData)^.year < 0);
                      end;
    DBTYPE_DBTIME:    begin
                        Result := PWideChar(fByteBuffer);
                        Len := TimeToUni(PDBTime(FData)^.hour,
                          PDBTime(FData)^.minute, PDBTime(FData)^.second,0,
                          Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                      end;
    DBTYPE_DBTIMESTAMP: begin
                        Result := PWideChar(fByteBuffer);
                        Len := DateTimeToUni(Abs(PDBTimeStamp(FData)^.year),
                          PDBTimeStamp(FData)^.month, PDBTimeStamp(FData)^.day,
                          PDBTimeStamp(FData)^.hour, PDBTimeStamp(FData)^.minute,
                          PDBTimeStamp(FData)^.second, PDBTimeStamp(FData)^.fraction,
                          Result, ConSettings.ReadFormatSettings.DateTimeFormat,
                          False, PDBTimeStamp(FData)^.year < 0);
                      end;
    DBTYPE_VARIANT:   begin
                        FUniTemp := POleVariant(FData)^;
                        goto set_from_tmp;
                      end;
    DBTYPE_GUID:      begin
                        GUIDToBuffer(FData, PWideChar(fByteBuffer), []);
                        Len := 36;
                        Result := PWideChar(fByteBuffer);
                      end;
    DBTYPE_STR: begin
                  if FColBind.cbMaxLen = 0 then
                    goto set_from_clob
                  else if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH = 0 then
                    FUniTemp := PRawToUnicode(PAnsiChar(FData), FLength, fClientCP)
                  else begin
                    Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData), FLength);
                    FUniTemp := PRawToUnicode(PAnsiChar(FData), Len, fClientCP);
                  end;
set_from_tmp:     Len := Length(FUniTemp);
                  if Len > 0
                  then Result := Pointer(FUniTemp)
                  else Result := PEmptyUnicodeString;
                end;
    DBTYPE_WSTR: if FColBind.cbMaxLen = 0 then begin
set_from_clob:    fTempBlob := GetBlob(ColumnIndex); //localize
                  Result := fTempBlob.GetPWideChar(fUniTemp, Len);
                  fTempBlob := nil;
                end else begin
                  Result := PWideChar(FData);
                  Len := FLength shr 1;
                  if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                    Len := GetAbsorbedTrailingSpacesLen(Result, Len);
                end;
    DBTYPE_NUMERIC: begin
                      Len := SQL_MAX_NUMERIC_LEN;
                      goto set_from_num;
                    end;
    DBTYPE_VARNUMERIC: begin
                      Len := FLength;
set_from_num:         Result := PWideChar(fByteBuffer);
                      SQLNumeric2Uni(fData, Result, Len);
                    end;
    //DBTYPE_UDT	= 132;
    //DBTYPE_FILETIME	= 64;
    //DBTYPE_PROPVARIANT	= 138;
    else raise CreateOleDbConvertError(ColumnIndex, stUnicodeString);
  end;
end;

function TZAbstractOleDBResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  Result := False;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength, fwType!!
    case FwType of
      DBTYPE_I2:        Result := PSmallInt(FData)^ <> 0;
      DBTYPE_I4:        Result := PInteger(FData)^ <> 0;
      DBTYPE_R4:        Result := PSingle(FData)^  <> 0;
      DBTYPE_R8:        Result := PDouble(FData)^ <> 0;
      DBTYPE_CY:        Result := PCurrency(FData)^ <> 0;
      DBTYPE_DATE:      Result := PDateTime(FData)^ <> 0;
      DBTYPE_ERROR:     Result := PInteger(FData)^ <> 0;
      DBTYPE_BOOL:      Result := PWordBool(FData)^;
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^ <> 0;
      DBTYPE_I1:        Result := PShortInt(FData)^ <> 0;
      DBTYPE_UI2:       Result := PWord(FData)^ <> 0;
      DBTYPE_UI4:       Result := PCardinal(FData)^ <> 0;
      DBTYPE_I8:        Result := PInt64(FData)^ <> 0;
      DBTYPE_UI8:       Result := PUInt64(FData)^ <> 0;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := StrToBoolEx(PA, PA+Len);
          FTempBlob := nil;
        end else
          Result := StrToBoolEx(PAnsiChar(FData),
            True, FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := StrToBoolEx(PW, PW+Len);
          FTempBlob := nil;
        end else
          Result := StrToBoolEx(PWideChar(FData),
            True, FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0);
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^ <> 0;
      else raise CreateOleDbConvertError(ColumnIndex, stBoolean);
    end;
end;

function TZAbstractOleDBResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
begin
  Result := nil;
  Len := 0;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_GUID:
        begin
          SetLength(FRawTemp, SizeOf(TGUID));
          Result := Pointer(FRawTemp);
          Len := SizeOf(TGUID);
          PGUID(Result)^ := PGUID(FData)^;
        end;
      DBTYPE_BYTES:
        begin
          Result := FData;
          Len := FLength;
        end;
      else LastWasNull := True;
    end;
end;

function TZAbstractOleDBResultSet.GetInt(ColumnIndex: Integer): Integer;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToIntDef(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToIntDef(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToIntDef(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToIntDef(PWideChar(FData),0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stInteger);
    end
  else Result := 0;
end;

function TZAbstractOleDBResultSet.GetLong(ColumnIndex: Integer): Int64;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_ERROR,
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToInt64Def(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToInt64Def(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToInt64Def(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToInt64Def(PWideChar(FData),0);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stLong);
    end
  else Result := 0;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractOleDBResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToUInt32Def(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToUInt64Def(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToUInt32Def(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToUInt64Def(PWideChar(FData),0);
      //DBTYPE_NUMERIC = 131;
      //DBTYPE_UDT = 132;
      //DBTYPE_DBDATE = 133;
      //DBTYPE_DBTIME = 134;
      //DBTYPE_DBTIMESTAMP = 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT = 138;
      //DBTYPE_VARNUMERIC = 139;
      else raise CreateOleDbConvertError(ColumnIndex, stLongWord);
    end
  else Result := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}

function TZAbstractOleDBResultSet.GetULong(ColumnIndex: Integer): UInt64;
var PA: PAnsiChar;
    PW: PWideChar absolute PA;
    Len: NativeUint;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := Trunc(PSingle(FData)^);
      DBTYPE_R8:        Result := Trunc(PDouble(FData)^);
      DBTYPE_CY:        Result := Trunc(PCurrency(FData)^);
      DBTYPE_DATE:      Result := Trunc(PDateTime(FData)^);
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL = 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PA := FTempBlob.GetPAnsiChar(FClientCP, FRawTemp, Len);
          Result := RawToUInt64Def(PA, PA+Len, 0);
          FTempBlob := nil;
        end else
          Result := RawToUInt64Def(PAnsiChar(FData),0);
      DBTYPE_WSTR:
        if FColBind.cbMaxLen = 0 then begin
          FTempBlob := GetBlob(ColumnIndex); //localize
          PW := FTempBlob.GetPWideChar(FUniTemp, Len);
          Result := UnicodeToUInt64Def(PW, PW+Len, 0);
          FTempBlob := nil;
        end else
          Result := UnicodeToUInt64Def(PWideChar(FData),0);
      //DBTYPE_NUMERIC = 131;
      //DBTYPE_UDT = 132;
      //DBTYPE_DBDATE = 133;
      //DBTYPE_DBTIME = 134;
      //DBTYPE_DBTIMESTAMP = 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME = 64;
      //DBTYPE_PROPVARIANT = 138;
      //DBTYPE_VARNUMERIC = 139;
      else raise CreateOleDbConvertError(ColumnIndex, stULong);
    end
  else Result := 0;
end;

function TZAbstractOleDBResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_WSTR:
                        SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stFloat);
    end
  else Result := 0;
end;

procedure TZAbstractOleDBResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
label Fail;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_GUID: Result := PGUID(FData)^;
      DBTYPE_STR: if FColBind.cbMaxLen = 0 then
                    goto Fail
                  else begin
                    if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                      FLength := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData), FLength);
                    ValidGUIDToBinary(PAnsiChar(FData), @Result.D1);
                  end;
      DBTYPE_WSTR: if FColBind.cbMaxLen = 0 then
                    goto Fail
                  else begin
                    FLength := FLength shr 1;
                    if FColBind.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                      FLength := GetAbsorbedTrailingSpacesLen(PWideChar(FData), FLength);
                    ValidGUIDToBinary(PWideChar(FData), @Result.D1);
                  end;
      else
Fail:        raise CreateOleDbConvertError(ColumnIndex, stGUID);
    end
  else FillChar(Result, SizeOf(TGUID), #0);
end;

procedure TZAbstractOleDBResultSet.GetDate(ColumnIndex: Integer;
  var Result: TZDate);
label Fill;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_DATE: DecodeDateTimeToDate(PDateTime(FData)^, Result);
      DBTYPE_WSTR: LastWasNull := not TryPCharToDate(PWideChar(FData), FLength shr 1,
        ConSettings.ReadFormatSettings, Result);
      DBTYPE_DBDATE: begin
              Result.Year := Abs(PDBDate(FData)^.year);
              Result.Month := PDBDate(FData)^.month;
              Result.Day := PDBDate(FData)^.day;
              Result.IsNegative := PDBDate(FData)^.year < 0;
            end;
      DBTYPE_DBTIME: goto Fill;
      DBTYPE_DBTIMESTAMP: begin
              Result.Year := Abs(PDBTimeStamp(FData)^.year);
              Result.Month := PDBTimeStamp(FData)^.month;
              Result.Day := PDBTimeStamp(FData)^.day;
              Result.IsNegative := PDBTimeStamp(FData)^.year < 0;
            end;
      DBTYPE_DBTIMESTAMPOFFSET: begin
              Result.Year := Abs(PDBTIMESTAMPOFFSET(FData)^.year);
              Result.Month := PDBTIMESTAMPOFFSET(FData)^.month;
              Result.Day := PDBTIMESTAMPOFFSET(FData)^.day;
              Result.IsNegative := PDBTIMESTAMPOFFSET(FData)^.year < 0;
            end;
      else DecodeDateTimeToDate(GetDouble(ColumnIndex), Result);
    end
  else
Fill: PInt64(@Result.Year)^ := 0;
end;

function TZAbstractOleDBResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_WSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      //DBTYPE_NUMERIC	= 131;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      //DBTYPE_VARNUMERIC	= 139;
      else raise CreateOleDbConvertError(ColumnIndex, stDouble);
    end
  else Result := 0;
end;

procedure TZAbstractOleDBResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var P: Pointer;
  L: NativeUInt;
label Fail;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_I2:        ScaledOrdinal2Bcd(PSmallInt(FData)^, 0, Result);
      DBTYPE_I4:        ScaledOrdinal2Bcd(PInteger(FData)^, 0, Result);
      DBTYPE_R4:        Result := DoubleToBCD(PSingle(FData)^);
      DBTYPE_R8:        Result := DoubleToBCD(PDouble(FData)^);
      DBTYPE_CY:        Currency2Bcd(PCurrency(FData)^, Result);
      DBTYPE_DATE:      Result := DoubleToBCD(PDateTime(FData)^);
      DBTYPE_STR: begin
          P := GetPAnsichar(ColumnIndex, L);
          if not ZSysUtils.TryRawToBcd(P, L, Result, '.') then
            goto fail;
        end;
      DBTYPE_WSTR: begin
          P := GetPWidechar(ColumnIndex, L);
          if not ZSysUtils.TryUniToBcd(P, L, Result, '.') then
            goto fail;
        end;
      DBTYPE_VARIANT:   Result := VarToBCD(POleVariant(FData)^);
      DBTYPE_ERROR:     ScaledOrdinal2Bcd(PInteger(FData)^, 0, Result);
      DBTYPE_BOOL:      ScaledOrdinal2Bcd(Word(Ord(PWord(FData)^ <> 0)), 0, Result);
      DBTYPE_UI1:       ScaledOrdinal2Bcd(Word(PByte(FData)^), 0, Result, False);
      DBTYPE_I1:        ScaledOrdinal2Bcd(SmallInt(PShortInt(FData)^), 0, Result);
      DBTYPE_UI2:       ScaledOrdinal2Bcd(PWord(FData)^, 0, Result);
      DBTYPE_UI4:       ScaledOrdinal2Bcd(PCardinal(FData)^, 0, Result, False);
      DBTYPE_I8:        ScaledOrdinal2Bcd(PInt64(FData)^, 0, Result);
      DBTYPE_UI8:       ScaledOrdinal2Bcd(PUInt64(FData)^, 0, Result, False);
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  ScaledOrdinal2Bcd(PCHAPTER(FData)^, 0, Result, False);
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      DBTYPE_NUMERIC:   SQLNumeric2BCD(FData, Result, SQL_MAX_NUMERIC_LEN);
      DBTYPE_VARNUMERIC:SQLNumeric2BCD(FData, Result, FLength);
      else
Fail:    raise CreateOleDbConvertError(ColumnIndex, stBigDecimal);
    end
  else
    FillChar(Result, SizeOf(TBCD), #0)
end;

function TZAbstractOleDBResultSet.GetCurrency(ColumnIndex: Integer): Currency;
label jmpFail;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_CY:        Result := PCurrency(FData)^;
      DBTYPE_I2:        Result := PSmallInt(FData)^;
      DBTYPE_I4:        Result := PInteger(FData)^;
      DBTYPE_R4:        Result := PSingle(FData)^;
      DBTYPE_R8:        Result := PDouble(FData)^;
      DBTYPE_DATE:      Result := PDateTime(FData)^;
      DBTYPE_ERROR:     Result := PInteger(FData)^;
      DBTYPE_BOOL:      Result := Ord(PWord(FData)^ <> 0);
      DBTYPE_VARIANT:   Result := POleVariant(FData)^;
      //DBTYPE_DECIMAL	= 14;
      DBTYPE_UI1:       Result := PByte(FData)^;
      DBTYPE_I1:        Result := PShortInt(FData)^;
      DBTYPE_UI2:       Result := PWord(FData)^;
      DBTYPE_UI4:       Result := PCardinal(FData)^;
      DBTYPE_I8:        Result := PInt64(FData)^;
      DBTYPE_UI8:       Result := PUInt64(FData)^;
      DBTYPE_STR:       SQLStrToFloatDef(PAnsiChar(FData), 0, Result, FLength);
      DBTYPE_WSTR:      SQLStrToFloatDef(PWideChar(FData), 0, Result, FLength shr 1);
      DBTYPE_NUMERIC,
      DBTYPE_VARNUMERIC:if PDB_NUMERIC(FData).precision < 19
                        then Result := DBNumeric2Curr_LE(FData, 0)
                        else goto jmpFail;
      //DBTYPE_UDT	= 132;
      //DBTYPE_DBDATE	= 133;
      //DBTYPE_DBTIME	= 134;
      //DBTYPE_DBTIMESTAMP	= 135;
      DBTYPE_HCHAPTER:  Result := PCHAPTER(FData)^;
      //DBTYPE_FILETIME	= 64;
      //DBTYPE_PROPVARIANT	= 138;
      else
jmpFail:raise CreateOleDbConvertError(ColumnIndex, stCurrency);
    end
  else Result := 0;
end;

procedure TZAbstractOleDBResultSet.GetTime(ColumnIndex: Integer;
  var Result: TZTime);
label Fill;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_DATE:    DecodeDateTimeToTime(PDateTime(FData)^, Result);
      DBTYPE_WSTR:    LastWasNull := not TryPCharToTime(PWideChar(FData), FLength shr 1,
                        ConSettings^.ReadFormatSettings, Result);
      DBTYPE_DBDATE:  goto fill;
      DBTYPE_DBTIME: begin
                      Result.Hour := PDBTime(FData)^.hour;
                      Result.Minute := PDBTime(FData)^.minute;
                      Result.Second := PDBTime(FData)^.second;
                      Result.Fractions := 0;
                      Result.IsNegative := False;
                    end;
      DBTYPE_DBTIME2: begin
                      Result.Hour := PDBTime2(FData)^.hour;
                      Result.Minute := PDBTime2(FData)^.minute;
                      Result.Second := PDBTime2(FData)^.second;
                      Result.Fractions := PDBTime2(FData)^.fraction;
                      Result.IsNegative := False;
                    end;
      DBTYPE_DBTIMESTAMP: begin
                      Result.Hour := PDBTimeStamp(FData)^.hour;
                      Result.Minute := PDBTimeStamp(FData)^.minute;
                      Result.Second := PDBTimeStamp(FData)^.second;
                      Result.Fractions := PDBTimeStamp(FData)^.fraction;
                      Result.IsNegative := False;
                    end;
      else DecodeDateTimeToTime(GetDouble(ColumnIndex), Result);
    end
  else
Fill: FillChar(Result, SizeOf(TZTimeStamp), #0);
end;

{$IFDEF FPC} {$PUSH} {$WARN 6018 off : Warning unreachable code} {$ENDIF}
procedure TZAbstractOleDBResultSet.GetTimestamp(ColumnIndex: Integer;
  var Result: TZTimeStamp);
label Fill;
begin
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_DBTIMESTAMP: begin
            if SizeOf(TDBTimeStamp) = SizeOf(TZTimeStamp)-6 then
              PDBTimeStamp(@Result.Year)^ := PDBTimeStamp(FData)^
            else begin
              Result.Month := PDBTimeStamp(FData)^.month;
              Result.Day := PDBTimeStamp(FData)^.day;
              Result.Hour := PDBTimeStamp(FData)^.hour;
              Result.Minute := PDBTimeStamp(FData)^.minute;
              Result.Second := PDBTimeStamp(FData)^.second;
              Result.Fractions := PDBTimeStamp(FData)^.fraction;
            end;
            Result.Year := Abs(PDBTimeStamp(FData)^.year);
            PCardinal(@Result.TimeZoneHour)^ := 0;
            Result.IsNegative := PDBTimeStamp(FData)^.year < 0;
          end;
      DBTYPE_DBTIMESTAMPOFFSET: begin
            if SizeOf(TDBTIMESTAMPOFFSET) = SizeOf(TZTimeStamp)-2 then
              PDBTIMESTAMPOFFSET(@Result.Year)^ := PDBTIMESTAMPOFFSET(FData)^
            else begin
              Result.Month := PDBTIMESTAMPOFFSET(FData)^.month;
              Result.Day := PDBTIMESTAMPOFFSET(FData)^.day;
              Result.Hour := PDBTIMESTAMPOFFSET(FData)^.hour;
              Result.Minute := PDBTIMESTAMPOFFSET(FData)^.minute;
              Result.Second := PDBTIMESTAMPOFFSET(FData)^.second;
              Result.Fractions := PDBTIMESTAMPOFFSET(FData)^.fraction;
              Result.TimeZoneHour := PDBTIMESTAMPOFFSET(FData)^.timezone_hour;
              Result.TimeZoneMinute := PDBTIMESTAMPOFFSET(FData)^.timezone_minute;
            end;
            Result.Year := Abs(PDBTIMESTAMPOFFSET(FData)^.year);
            Result.IsNegative := PDBTIMESTAMPOFFSET(FData)^.year < 0;
          end;
      DBTYPE_DBDATE: begin
              Result.Year := Abs(PDBDate(FData)^.year);
              Result.Month := PDBDate(FData)^.month;
              Result.Day := PDBDate(FData)^.day;
              PInt64(@Result.Hour)^ := 0;
              PInt64(@Result.Fractions)^ := 0;
              Result.IsNegative := PDBDate(FData)^.year < 0;
            end;
      DBTYPE_DBTIME: begin
              PInt64(@Result.Year)^ := 0;
              Result.Hour := PDBTime(FData)^.hour;
              Result.Minute := PDBTime(FData)^.minute;
              Result.Second := PDBTime(FData)^.second;
              PInt64(@Result.Fractions)^ := 0;
              Result.IsNegative := False;
            end;
      DBTYPE_DBTIME2: begin
              PInt64(@Result.Year)^ := 0;
              Result.Hour := PDBTime2(FData)^.hour;
              Result.Minute := PDBTime2(FData)^.minute;
              Result.Second := PDBTime2(FData)^.second;
              Result.Fractions := PDBTime2(FData)^.fraction;
              PCardinal(@Result.TimeZoneHour)^ := 0;
              Result.IsNegative := False;
            end;
      DBTYPE_DATE: DecodeDateTimeToTimeStamp(PDateTime(FData)^,Result);
      DBTYPE_WSTR: begin
          LastWasNull := not TryPCharToTimeStamp(PWideChar(FData), FLength shr 1,
                ConSettings^.ReadFormatSettings, Result);
          if LastWasNull then
            goto Fill;
        end;
      else DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex),Result);
    end
  else
Fill: FillChar(Result, SizeOf(TZTimeStamp), #0);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractOleDBResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
begin
  Result := nil;
  if LobStreamMode <> lsmRead then
    raise CreateReadOnlyException;
  if not IsNull(ColumnIndex) then //Sets LastWasNull, FData, FLength!!
    case FwType of
      DBTYPE_GUID:
        Result := TZLocalMemBLob.CreateWithData(Pointer(FData), 16, FOpenLobStreams);
      DBTYPE_BYTES:
        if FColBind.cbMaxLen = 0 then
          Result := TZOleDBBLOB.Create(FRowSetAddr^, ColumnIndex, DBTYPE_BYTES,
            FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams)
        else
          Result := TZLocalMemBLob.CreateWithData(Pointer(FData), FLength, FOpenLobStreams);
      DBTYPE_STR:
        if FColBind.cbMaxLen = 0 then
          Result := TZOleDBCLOB.Create(FRowSetAddr^, ColumnIndex, DBTYPE_STR,
            FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams)
        else
          Result := TZLocalMemCLob.CreateWithData(PAnsiChar(FData),
            FLength, FClientCP, ConSettings, FOpenLobStreams);
      DBTYPE_WSTR, DBTYPE_XML:
        if FColBind.cbMaxLen = 0 then
          Result := TZOleDBCLOB.Create(FRowSetAddr^, ColumnIndex, FwType,
            FHROWS^[FCurrentBufRowNo], FOleDBConnection, FLength, FOpenLobStreams)
        else
          Result := TZLocalMemCLob.CreateWithData(PWideChar(FData), FLength shr 1, ConSettings, FOpenLobStreams);
      else raise CreateCanNotAccessBlobRecordException(ColumnIndex, TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType);
    end;
end;


{ TZOleDBMSSQLCachedResolver }

constructor TZOleDBMSSQLCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte,stShort,stWord,stSmall,stLongWord,stInteger,stULong,stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
  fStmt := TZOleDBPreparedStatement.Create(Statement.GetConnection, 'SELECT SCOPE_IDENTITY()', nil);
end;

destructor TZOleDBMSSQLCachedResolver.Destroy;
begin
  if fStmt <> nil then
    fStmt.Close;
  inherited Destroy;
end;

procedure TZOleDBMSSQLCachedResolver.PostUpdates(const Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; const OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex) and
      OldRowAccessor.IsNull(FAutoColumnIndex) then begin
    FResultSet := fStmt.ExecuteQueryPrepared;
    if Assigned(FResultSet) and FResultSet.Next then
      NewRowAccessor.SetLong(FAutoColumnIndex, FResultSet.GetLong(FAutoColumnIndex));
  end;
end;

{ TZOleDBCachedResultSet }

constructor TZOleDBCachedResultSet.Create(ResultSet: TZAbstractOleDBResultSet;
  const SQL: string; const Resolver: IZCachedResolver; ConSettings: PZConSettings);
begin
  inherited Create(ResultSet, SQL, Resolver, ConSettings);
  FResultSet := ResultSet;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "BCD" does not seem to be initialized} {$ENDIF}
function TZOleDBCachedResultSet.Fetch: Boolean;
var
  I: Integer;
  TempRow: PZRowBuffer;
  DBBINDING: PDBBINDING;
  FData: PPointer;
  FLength: PDBLENGTH;
  Len: NativeUInt;
  BCD: TBCD; //one val on stack 4 all
  TS: TZTimeStamp absolute BCD;
  D: TZDate absolute BCD;
  T: TZTime absolute BCD;
  procedure MoveAsLocalLob(wType: DBTYPE; ColumnIndex: Integer);
  var OleDBBLob, LocalLob: IZBlob;
    OleDBCLob: IZCLob;
  begin
    case wType of
      DBTYPE_BYTES: begin
          OleDBBLob := TZOleDBBLOB.Create(FResultSet.FRowSetAddr^,
            I, DBTYPE_BYTES, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
            FResultSet.FOleDBConnection, Flength^, FOpenLobStreams);
          LocalLob := TZLocalMemBLob.CreateFromBlob(OleDBBLob, FOpenLobStreams);
        end;
      DBTYPE_STR: begin
          OleDBCLob := TZOleDBCLOB.Create(FResultSet.FRowSetAddr^,
            I, DBTYPE_STR, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
            FResultSet.FOleDBConnection, Flength^, FOpenLobStreams);
          LocalLob := TZLocalMemCLob.CreateFromClob(OleDBCLob, ConSettings.ClientCodePage.CP, ConSettings, FOpenLobStreams);
        end;
      else begin
          OleDBCLob := TZOleDBCLOB.Create(FResultSet.FRowSetAddr^,
            I, DBTYPE_WSTR, FResultSet.FHROWS^[FResultSet.FCurrentBufRowNo],
            FResultSet.FOleDBConnection, Flength^, FOpenLobStreams);
          LocalLob := TZLocalMemCLob.CreateFromClob(OleDBCLob, zCP_UTF16, ConSettings, FOpenLobStreams);
        end;
    end;
    RowAccessor.SetBlob(ColumnIndex, LocalLob);
  end;
begin
  if Assigned(FResultSet)
  then Result := FResultSet.Next
  else Result := False;
  if not Result or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then begin
    if (FResultSet <> nil) and not FLastRowFetched then
      FResultSet.ResetCursor; //EH: clear library mem or release servercursor
    FLastRowFetched := True;
    Exit;
  end;

  TempRow := RowAccessor.RowBuffer;
  FData := @FResultSet.FData;
  FLength := @FResultSet.FLength;
  RowAccessor.Alloc;
  RowAccessor.RowBuffer.Index := GetNextRowIndex;
  RowAccessor.RowBuffer.UpdateType := utUnmodified;
  try
    for I := FirstDbcIndex to {$IFDEF GENERIC_INDEX}High{$ELSE}Length{$ENDIF}(FResultSet.FDBBindingArray) do
      if FResultSet.IsNull(I) then
        RowAccessor.SetNull(I)
      else begin
        DBBINDING := @FResultSet.FDBBindingArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
        case DBBINDING.wType of
          DBTYPE_EMPTY,
          DBTYPE_NULL                 : RowAccessor.SetNull(I);
          DBTYPE_I2                   : RowAccessor.SetSmall(I, PSmallInt(FData^)^);
          DBTYPE_I4,
          DBTYPE_ERROR,
          DBTYPE_HCHAPTER             : RowAccessor.SetInt(I, PInteger(FData^)^);
          DBTYPE_R4	                  : RowAccessor.SetFloat(I, PSingle(FData^)^);
          DBTYPE_R8                   : RowAccessor.SetDouble(I, PDouble(FData^)^);
          DBTYPE_CY                   : RowAccessor.SetCurrency(I, PCurrency(FData^)^);
          DBTYPE_DATE                 : begin
                                          DecodeDateTimeToTimeStamp(PDateTime(FData^)^, TS);
                                          RowAccessor.SetTimeStamp(I, TS);
                                        end;
          DBTYPE_WSTR                 : if DBBINDING.cbMaxLen = 0
                                        then MoveAsLocalLob(DBTYPE_WSTR, I)
                                        else begin
                                          Len := FLength^ shr 1;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            Len := GetAbsorbedTrailingSpacesLen(PWideChar(FData^), Len);
                                          RowAccessor.SetPWideChar(I, FData^, Len);
                                        end;
          //DBTYPE_IDISPATCH = 9;
          DBTYPE_BOOL                 : RowAccessor.SetBoolean(I, PWordBool(FData^)^);
          DBTYPE_VARIANT              : case RowAccessor.GetColumnType(I) of
                                          stBoolean: RowAccessor.SetBoolean(I, POleVariant(FData^)^);
                                          stByte        : RowAccessor.SetByte(I, POleVariant(FData^)^);
                                          stShort       : RowAccessor.SetShort(I, POleVariant(FData^)^);
                                          stWord        : RowAccessor.SetWord(I, POleVariant(FData^)^);
                                          stSmall       : RowAccessor.SetSmall(I, POleVariant(FData^)^);
                                          stLongWord    : RowAccessor.SetUInt(I, POleVariant(FData^)^);
                                          stInteger     : RowAccessor.SetInt(I, POleVariant(FData^)^);
                                          stULong       : RowAccessor.SetULong(I, POleVariant(FData^)^);
                                          stLong        : RowAccessor.SetLong(I, POleVariant(FData^)^);
                                          stFloat       : RowAccessor.SetFloat(I, POleVariant(FData^)^);
                                          stDouble      : RowAccessor.SetDouble(I, POleVariant(FData^)^);
                                          stCurrency    : RowAccessor.SetCurrency(I, POleVariant(FData^)^);
                                          stBigDecimal  : RowAccessor.SetBigDecimal(I, DoubleToBCD(POleVariant(FData^)^));
                                          {stDate, stTime, stTimestamp,
                                          stGUID,
                                          //now varying size types in equal order
                                          stString, stUnicodeString, stBytes,
                                          stAsciiStream, stUnicodeStream, stBinaryStream,
                                          //finally the object types
                                          stArray, stDataSet}
                                          {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
                                        end;
          //DBTYPE_IUNKNOWN = 13;
          //DBTYPE_DECIMAL = 14;
          DBTYPE_UI1                  : RowAccessor.SetByte(I, PByte(FData^)^);
          DBTYPE_I1                   : RowAccessor.SetShort(I, PShortInt(FData^)^);
          DBTYPE_UI2                  : RowAccessor.SetWord(I, PWord(FData^)^);
          DBTYPE_UI4                  : RowAccessor.SetUInt(I, PCardinal(FData^)^);
          DBTYPE_I8                   : RowAccessor.SetLong(I, PInt64(FData^)^);
          DBTYPE_UI8                  : RowAccessor.SetULong(I, PInt64(FData^)^);
          DBTYPE_GUID                 : RowAccessor.SetGUID(I, PGUID(FData^)^);
          DBTYPE_BYTES                : if DBBINDING.cbMaxLen = 0
                                        then MoveAsLocalLob(DBTYPE_BYTES, I)
                                        else RowAccessor.SetBytes(I, FData^, FLength^);
          DBTYPE_STR                  : if DBBINDING.cbMaxLen = 0
                                        then MoveAsLocalLob(DBTYPE_STR, I)
                                        else begin
                                          Len := FLength^;
                                          if DBBINDING.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH <> 0 then
                                            Len := GetAbsorbedTrailingSpacesLen(PAnsiChar(FData^), Len);
                                          RowAccessor.SetPAnsiChar(I, FData^, Len);
                                        end;
          DBTYPE_NUMERIC              : begin
                                          SQLNumeric2BCD(PDB_NUMERIC(FData^), BCD, SQL_MAX_NUMERIC_LEN);
                                          RowAccessor.SetBigDecimal(I, BCD);
                                        end;
          //DBTYPE_UDT = 132;
          DBTYPE_DBDATE               : begin
                                          D.Year := Abs(PDBDate(FData^)^.year);
                                          D.Month := PDBDate(FData^)^.month;
                                          D.Day := PDBDate(FData^)^.day;
                                          D.IsNegative := PDBDate(FData^)^.year < 0;
                                          RowAccessor.SetDate(I, D);
                                        end;
          DBTYPE_DBTIME               : begin
                                          T.Hour := PDBTime(FData^)^.hour;
                                          T.Minute := PDBTime(FData^)^.minute;
                                          T.Second := PDBTime(FData^)^.second;
                                          T.Fractions := 0;
                                          T.IsNegative := False;
                                          RowAccessor.SetTime(I, T);
                                        end;
          DBTYPE_DBTIMESTAMP          : begin
                                          TS.Year := Abs(PDBTimeStamp(FData^)^.year);
                                          Ts.Month := PDBTimeStamp(FData^)^.month;
                                          TS.Day := PDBTimeStamp(FData^)^.day;
                                          TS.Hour := PDBTimeStamp(FData^)^.hour;
                                          TS.Minute := PDBTimeStamp(FData^)^.minute;
                                          TS.Second := PDBTimeStamp(FData^)^.second;
                                          TS.Fractions := PDBTimeStamp(FData^)^.fraction;
                                          Ts.TimeZoneHour := 0;
                                          TS.TimeZoneMinute := 0;
                                          TS.IsNegative := PDBTimeStamp(FData^)^.year < 0;
                                          RowAccessor.SetTimestamp(I, TS);
                                        end;
          {SQL Server types only }
          DBTYPE_XML                  : MoveAsLocalLob(DBTYPE_WSTR, I);
          //DBTYPE_TABLE = 143; // introduced in SQL 2008
          DBTYPE_DBTIME2              : begin
                                          T.Hour := PDBTime2(FData^)^.hour;
                                          T.Minute := PDBTime2(FData^)^.minute;
                                          T.Second := PDBTime2(FData^)^.second;
                                          T.Fractions := PDBTime2(FData^)^.fraction;
                                          T.IsNegative := False;
                                          RowAccessor.SetTime(I, T);
                                        end;
          //DBTYPE_DBTIMESTAMPOFFSET = 146; // introduced in SQL 2008
          //DBTYPE_FILETIME = 64;
          //DBTYPE_PROPVARIANT = 138;
          DBTYPE_VARNUMERIC           : begin
                                          SQLNumeric2BCD(PDB_NUMERIC(FData^), BCD, FLength^);
                                          RowAccessor.SetBigDecimal(I, BCD);
                                        end;
        end;
      end;
      RowsList.Add(RowAccessor.RowBuffer);
      LastRowNo := RowsList.Count;
    finally
      RowAccessor.RowBuffer := TempRow;
    end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

class function TZOleDBCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZOleDbRowAccessor;
end;

{ TZOleDBMSSQLResultSetMetadata }

procedure TZOleDBMSSQLResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

function TZOleDBMSSQLResultSetMetadata.GetCatalogName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).CatalogName;
end;

function TZOleDBMSSQLResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnName;
end;

function TZOleDBMSSQLResultSetMetadata.GetColumnType(
  ColumnIndex: Integer): TZSQLType;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType;
end;

function TZOleDBMSSQLResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).SchemaName;
end;

function TZOleDBMSSQLResultSetMetadata.GetTableName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).TableName;
end;

function TZOleDBMSSQLResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).AutoIncrement;
end;

procedure TZOleDBMSSQLResultSetMetadata.LoadColumns;
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
  Connection: IZConnection;
  IdentifierConverter: IZIdentifierConverter;
  Analyser: IZStatementAnalyser;
  Tokenizer: IZTokenizer;
begin
  Connection := Metadata.GetConnection;
  Analyser := Connection.GetStatementAnalyser;
  Tokenizer := Connection.GetTokenizer;
  IdentifierConverter := Metadata.GetIdentifierConverter;
  try
    if Analyser.DefineSelectSchemaFromQuery(Tokenizer, SQL) <> nil then
      for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
        Current := TZColumnInfo(ResultSet.ColumnsInfo[i]);
        ClearColumn(Current);
        if Current.TableName = '' then
          continue;
        TableColumns := Metadata.GetColumns(Current.CatalogName, Current.SchemaName, Metadata.AddEscapeCharToWildcards(IdentifierConverter.Quote(Current.TableName, iqTable)),'');
        if TableColumns <> nil then begin
          TableColumns.BeforeFirst;
          while TableColumns.Next do
            if TableColumns.GetString(ColumnNameIndex) = Current.ColumnName then begin
              FillColumInfoFromGetColumnsRS(Current, TableColumns, Current.ColumnName);
              Break;
            end;
        end;
      end;
  finally
    Connection := nil;
    Analyser := nil;
    Tokenizer := nil;
    IdentifierConverter := nil;
  end;
  Loaded := True;
end;

{ TZOleDBParamResultSet }

constructor TZOleDBParamResultSet.Create(const Statement: IZStatement;
  const ParamBuffer: TByteDynArray; const ParamBindings: TDBBindingDynArray;
  const ParamNameArray: TStringDynArray);
var
  i, j: Integer;
  ColumnInfo: TZColumnInfo;
begin
  FOleDBConnection := (Statement.GetConnection as IZOleDBConnection);
  FByteBuffer := FOleDBConnection.GetByteBufferAddress;
  inherited Create(Statement, Statement.GetSQL, nil, FOleDBConnection.GetConSettings);
  fClientCP := ConSettings.ClientCodePage.CP;
  FColBuffer := ParamBuffer;
  J := 0;
  for I := Low(ParamBindings) to High(ParamBindings) do
    if ParamBindings[i].eParamIO <> DBPARAMIO_INPUT then
      Inc(J);
  FRowSize := Length(ParamBuffer);
  SetLength(FDBBindingArray,J);
  SetLength(FDBBINDSTATUSArray, J);
  FRowSize := Length(ParamBuffer);
  J := 0;
  for I := Low(ParamBindings) to High(ParamBindings) do
    if ParamBindings[i].eParamIO <> DBPARAMIO_INPUT then begin
      Move(ParamBindings[i], FDBBindingArray[j], SizeOf(TDBBinding)); //copy all offsets
      ColumnInfo := TZColumnInfo.Create;
      if I<=High(ParamNameArray) then
        ColumnInfo.ColumnLabel := ParamNameArray[i];
      ColumnInfo.ColumnType := ConvertOleDBTypeToSQLType(
        ParamBindings[i].wType and not DBTYPE_BYREF,
        ParamBindings[i].dwFlags and DBCOLUMNFLAGS_ISLONG <> 0,
        ParamBindings[i].bPrecision, ParamBindings[i].bScale);
      ColumnInfo.Scale := ParamBindings[i].bScale;
      ColumnInfo.Precision := ParamBindings[i].bPrecision;
      ColumnInfo.CharOctedLength := ParamBindings[i].cbMaxLen;
      ColumnsInfo.Add(ColumnInfo);
      Inc(J);
    end;
  Open;
  LastRowNo := 1;
  FCursorLocation := rctClient;
end;

function TZOleDBParamResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := not Closed and ((Row = 1) or (Row = 0));
  if (Row >= 0) and (Row <= 2) then
    RowNo := Row;
end;

function TZOleDBParamResultSet.Next: Boolean;
begin
  Result := not Closed and (RowNo = 0);
  if RowNo = 0 then
    RowNo := 1
  else if RowNo = 1 then
    RowNo := 2; //set AfterLast
end;

{ TZOleDBResultSet }

constructor TZOleDBResultSet.Create(const Statement: IZStatement;
  const SQL: string; const RowSet: PRowSet; ZBufferSize: Integer);
begin
  FOleDBConnection := Statement.GetConnection as IZOleDBConnection;
  FByteBuffer := FOleDBConnection.GetByteBufferAddress;
  FRowSetAddr := RowSet;
  inherited Create(Statement, SQL, nil, FOleDBConnection.GetConSettings);
  FZBufferSize := ZBufferSize;
  fClientCP := ConSettings.ClientCodePage.CP;
  Open;
end;

function TZOleDBResultSet.Next: Boolean;
var I: NativeInt;
    Status: HResult;
label NoSuccess;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or (FRowSetAddr^ = nil) or
    ((RowNo = LastRowNo) and (FGetNextRowsStatus = DB_S_ENDOFROWSET)) or
    ((MaxRows > 0) and (RowNo >= MaxRows)) then
    goto NoSuccess;

  if (FRowsObtained > 0) and (FCurrentBufRowNo < DBROWCOUNT(FRowsObtained)-1)
  then Inc(FCurrentBufRowNo)
  else begin
    {release old rows}
    if (FAccessor =  0) then begin
      CreateAccessor;
      RowNo := 0;
    end else ReleaseFetchedRows;

    FGetNextRowsStatus := fRowSetAddr^.GetNextRows(DB_NULL_HCHAPTER,0,FRowCount, FRowsObtained, FHROWS);
    if Failed(FGetNextRowsStatus) then try
      FOleDBConnection.HandleErrorOrWarning(FGetNextRowsStatus, lcOther, 'IRowSet.GetNextRows', Self);
    finally
      ResetCursor;
    end;
    if (RowNo = 0) then begin
      if (FGetNextRowsStatus = DB_S_ROWLIMITEXCEEDED) or (FGetNextRowsStatus = DB_S_STOPLIMITREACHED) then begin
        FRowCount := FRowsObtained;
        FGetNextRowsStatus := S_OK; //indicate success
      end;
      if (FGetNextRowsStatus = DB_S_ENDOFROWSET)
      then I := FRowsObtained
      else I := FRowCount;
      SetLength(FColBuffer, I * FRowSize);
      SetLength(FRowStates, I);
    end;
    if (FGetNextRowsStatus = S_OK) or (FGetNextRowsStatus = DB_S_ENDOFROWSET) then
      LastRowNo := RowNo + NativeInt(FRowsObtained);
    FCurrentBufRowNo := 0; //reset Buffer offsett
    if FRowsObtained > 0 then begin
      {fetch data into the buffer}
      for i := 0 to FRowsObtained -1 do begin
        Status := fRowSetAddr^.GetData(FHROWS[i], FAccessor, @FColBuffer[I*FRowSize]);
        if Status <> S_OK then
          FOleDBConnection.HandleErrorOrWarning(Status, lcOther, 'IRowSet.GetData', Self);
      end;
    end else goto NoSuccess;
  end;

  RowNo := RowNo + 1;
  Result := True;
  Exit;
NoSuccess:
  if (RowNo = LastRowNo) then begin
    if not LastRowFetchLogged and DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    I := LastRowNo; // remainder
    ResetCursor; //free resources
    LastRowNo := I; //apply again
  end;
  if RowNo <= LastRowNo then
    RowNo := LastRowNo + 1;
end;

procedure TZOleDBResultSet.Open;
var
  OleDBColumnsInfo: IColumnsInfo;
  prgInfo, OriginalprgInfo: PDBColumnInfo;
  ppStringsBuffer: PWideChar;
  I: Integer;
  FieldSize: Integer;
  ColumnInfo: TZColumnInfo;
label jmpFixedAndSize;
begin
  if not Assigned(FRowSetAddr^) or
     Failed(FRowSetAddr^.QueryInterface(IID_IColumnsInfo, OleDBColumnsInfo)) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  OleDBColumnsInfo.GetColumnInfo(fpcColumns{%H-}, prgInfo, ppStringsBuffer);
  OriginalprgInfo := prgInfo; //save pointer for Malloc.Free
  try
    { Fills the column info }
    ColumnsInfo.Clear;
    if Assigned(prgInfo) then
      if prgInfo.iOrdinal = 0 then begin// skip possible bookmark column
        Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));
        Dec(fpcColumns);
      end;
    SetLength(FDBBINDSTATUSArray, fpcColumns);
    FRowSize := PrepareOleColumnDBBindings(fpcColumns, FDBBindingArray, prgInfo);
    FRowCount := Max(1, FZBufferSize div NativeInt(FRowSize));
    if (MaxRows > 0) and (FRowCount > MaxRows) then
      FRowCount := MaxRows; //fetch only wanted count of rows

    for I := prgInfo.iOrdinal-1 to fpcColumns-1 do
    begin
      ColumnInfo := TZColumnInfo.Create;
      if (prgInfo.pwszName<>nil) and (prgInfo.pwszName^<>#0) then
        {$IFDEF UNICODE}
        System.SetString(ColumnInfo.ColumnLabel, prgInfo^.pwszName, {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(prgInfo^.pwszName));
        {$ELSE}
        ColumnInfo.ColumnLabel := PUnicodeToRaw(prgInfo^.pwszName, {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(prgInfo^.pwszName), zCP_UTF8);
        {$ENDIF}
      ColumnInfo.ColumnType := ConvertOleDBTypeToSQLType(prgInfo^.wType,
        prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG <> 0,
        prgInfo.bScale, prgInfo.bPrecision);

      if prgInfo^.ulColumnSize >= Cardinal(MaxInt) then
        FieldSize := 0
      else
        FieldSize := prgInfo^.ulColumnSize;
      case ColumnInfo.ColumnType of
        stGUID:         ColumnInfo.Precision := 38;
        stString:       begin
                          ColumnInfo.ColumnCodePage := ConSettings.ClientCodePage.CP;
                          goto jmpFixedAndSize;
                        end;
        stUnicodeString:begin
                          ColumnInfo.ColumnCodePage := zCP_UTF16;
                          goto jmpFixedAndSize;
                        end;
        stBytes:        begin
jmpFixedAndSize:          ColumnInfo.Precision := FieldSize;
                          if (prgInfo.dwFlags and DBCOLUMNFLAGS_ISFIXEDLENGTH) <> 0 then
                            ColumnInfo.Scale := ColumnInfo.Precision;
                        end;
        stCurrency,
        stBigDecimal:   begin
                          ColumnInfo.Precision := prgInfo.bPrecision;
                          if (prgInfo^.wType = DBTYPE_CY)
                          then ColumnInfo.Scale := 4
                          else ColumnInfo.Scale := prgInfo.bScale;

                        end;
        stAsciiStream:  begin
                          ColumnInfo.Precision := -1;
                          ColumnInfo.ColumnCodePage := ConSettings.ClientCodePage.CP
                        end;
        stUnicodeStream:begin
                          ColumnInfo.Precision := -1;
                          ColumnInfo.ColumnCodePage := zCP_UTF16
                        end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
      ColumnInfo.Currency := prgInfo.wType = DBTYPE_CY;
      ColumnInfo.AutoIncrement := prgInfo.dwFlags and DBCOLUMNFLAGS_ISROWID = DBCOLUMNFLAGS_ISROWID;
      if (ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong, stFloat, stDouble, stCurrency, stBigDecimal]) then
        ColumnInfo.Signed := True;
      ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0);
      ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0);
      ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
      ColumnsInfo.Add(ColumnInfo);
      Inc({%H-}NativeUInt(prgInfo), SizeOf(TDBColumnInfo));  //M.A. Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));
    end;
  finally
    if Assigned(ppStringsBuffer) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(ppStringsBuffer);
    if Assigned(OriginalprgInfo) then (Statement.GetConnection as IZOleDBConnection).GetMalloc.Free(OriginalprgInfo);
  end;
  inherited Open;
  FCursorLocation := rctServer;
end;

procedure TZOleDBResultSet.ResetCursor;
var
  FAccessorRefCount: DBREFCOUNT;
  Status: HResult;
begin
  if not Closed then begin
    try
      fTempBlob := nil;
      ReleaseFetchedRows;
      if FAccessor > 0 then begin
        Status := (fRowSetAddr^ As IAccessor).ReleaseAccessor(FAccessor, @FAccessorRefCount);
        if Status <> S_OK then
          FOleDBConnection.HandleErrorOrWarning(Status, lcOther, 'IAccessor.ReleaseAccessor', Self);
      end;
    finally
      FRowSetAddr^ := nil;
      FAccessor := 0;
      FCurrentBufRowNo := 0;
      FRowsObtained := 0;
      FGetNextRowsStatus := S_OK;
      inherited ResetCursor;
    end;
  end;
end;

{ TZOleDBBLob }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZOleDBBLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var P: Pointer;
  R: RawByteString;
  L: NativeUint;
begin
  R := '';
  P := GetBuffer(R, L);
  Result := TZLocalMemBLob.CreateWithData(P, L, FOpenLobStreams);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZOleDBCLob }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobStreamMode" not used} {$ENDIF}
function TZOleDBCLob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var P: Pointer;
  R: RawByteString;
  U: UnicodeString;
  L: NativeUint;
  CP: Word;
  ConSettings: PZConSettings;
begin
  ConSettings := FOleConnection.GetConSettings;
  if FwType <> DBTYPE_STR then begin
    U := '';
    P := GetPWideChar(U, L);
    Result := TZLocalMemCLob.CreateWithData(PWideChar(P), L, ConSettings, FOpenLobStreams);
  end else begin
    CP := ConSettings.ClientCodePage.CP;
    R := '';
    P := GetPAnsiChar(CP, R, L);
    Result := TZLocalMemCLob.CreateWithData(P, L, CP, ConSettings, FOpenLobStreams);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZOleDbRowAccessor }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "LobCacheMode" not used} {$ENDIF}
constructor TZOleDbRowAccessor.Create(ColumnsInfo: TObjectList;
  ConSettings: PZConSettings; const OpenLobStreams: TZSortedList;
  LobCacheMode: TLobCacheMode);
begin
  inherited Create(ColumnsInfo, ConSettings, OpenLobStreams, lcmOnLoad); //we can not use uncached lobs with OleDB
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "ConSettings" not used} {$ENDIF}
class function TZOleDbRowAccessor.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType;
begin
  Result := ColumnInfo.ColumnType;
  if Result in [stString, stAsciiStream] then
    Result := TZSQLType(Byte(Result)+1);  // no raw chars in 4 OleDB
  if Result in [stUnicodeString, stUnicodeStream] then
    ColumnCodePage := zCP_UTF16;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZOleDBMetadataResultSet }

constructor TZOleDBMetadataResultSet.Create(const Statement: IZStatement;
  const RowSet: IRowSet; ZBufferSize: Integer);
begin
  FRowSet := RowSet;
  inherited Create(Statement, '', @FRowSet, ZBufferSize);
end;

initialization
  {init some reusable records (: }
  LobReadObj.dwFlags := STGM_READ;
  LobReadObj.iid := IID_ISequentialStream; //i also testet it with IStream but this does not mork on GetData

  LobDBBinding.obValue := 0;
  LobDBBinding.pTypeInfo := nil;
  LobDBBinding.pObject := @LobReadObj;
  LobDBBinding.pBindExt := nil;
  LobDBBinding.dwPart := DBPART_VALUE;
  LobDBBinding.dwMemOwner := DBMEMOWNER_CLIENTOWNED;
  LobDBBinding.eParamIO := DBPARAMIO_NOTPARAM;
  LobDBBinding.cbMaxLen := 0;
  LobDBBinding.dwFlags := 0;// DBCOLUMNFLAGS_ISLONG;  {}
  LobDBBinding.wType := DBTYPE_IUNKNOWN;
  LobDBBinding.bPrecision := 0;
  LobDBBinding.bScale := 0;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
end.
