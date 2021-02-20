{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types{$ELSE}Types{$ENDIF},
  FmtBCD, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZPlainPostgreSqlDriver, ZDbcLogging,
  ZDbcResultSetMetadata, ZCompatibility, ZDbcCache, ZDbcGenericResolver,
  ZClasses, ZDbcCachedResultSet, ZDbcPostgreSql;

type
  /// <summary>Implements Postgres column information object.</summary>
  TZPGColumnInfo = class(TZColumnInfo)
  public
    TableOID: OID;
    TableColNo: Integer;
    TypeOID: OID;
  end;

  /// <summary>Implements Postgres ResultSet Metadata.</summary>
  TZPostgresResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    /// <summary>Initializes columns with additional data.</summary>
    procedure LoadColumns; override;
    /// <summary>Clears specified column information.</summary>
    /// <param>"ColumnInfo" a column information object.</param>
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  { Postgres Error Class}
  EZPGConvertError = class(EZSQLException);

  /// <summary>Defines a Postgres ResultSet interface.</summary>
  IZPostgresResultSet = interface(IZResultSet)
    ['{FFCDB099-053C-45B1-99D3-2099AEFBEBC8}']
    /// <summary>Assigns the Column-Information to a Destination object list.
    /// </summary>
    /// <param>"Dest" a column information list.</param>
    procedure AssignColumnsInfo(const Dest: TObjectList);
    /// <summary>Get the IZPostgreSQLConnection of the current object.</summary>
    /// <returns>The postgres connection interface.</returns>
    function GetConnection: IZPostgreSQLConnection;
  end;

  {** Implements PostgreSQL ResultSet. }
  TZPostgreSQLResultSet = class(TZAbstractReadOnlyResultSet_A, IZResultSet,
    IZPostgresResultSet)
  private
    FFirstRow: Boolean;
    FconnAddress: PPGconn;
    Fres: TPGresult;
    FresAddress: PPGresult;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FSingleRowMode, FIs_bytea_output_hex: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    FClientCP: Word;
    FResultFormat: PInteger;
    FBinaryValues, Finteger_datetimes, FIsOidAsBlob: Boolean;
    FDecimalSeps: array[Boolean] of Char;
    FPGConnection: IZPostgreSQLConnection;
    FByteBuffer: PByteBuffer;
    function CreatePGConvertError(ColumnIndex: Integer; DataType: OID): EZPGConvertError;
  protected
    /// <summary>Opens this recordset.</summary>
    procedure Open; override;
    /// <summary>Converts a PostgreSQL native types into ZDBC SQL types.</summary>
    /// <param>"ColumnInfo" the postgres column information object to be filled.</param>
    /// <param>"TypeOid" a type oid.</param>
    /// <param>"TypeModifier" a type modifier.</param>
    procedure DefinePostgreSQLToSQLType({$IFDEF AUTOREFCOUNT}var{$ENDIF}ColumnInfo: TZPGColumnInfo;
      TypeOid: Oid; TypeModifier: Integer);
    function PGRowNo: Integer;
  public //implement IZPostgresResultSet
    /// <summary>Get the IZPostgreSQLConnection of the current object.</summary>
    /// <returns>The postgres connection interface.</returns>
    function GetConnection: IZPostgreSQLConnection;
    /// <summary>Assigns the Column-Information to a Destination object list.
    /// </summary>
    /// <param>"Dest" a column information list.</param>
    procedure AssignColumnsInfo(const Dest: TObjectList);
  public
    /// <summary>Constructs this object, assignes main properties and
    ///  opens the record set.</summary>
    /// <param>"Statement" a related SQL statement interface.</param>
    /// <param>"SQL" a SQL statement.</param>
    /// <param>"Connection" a postgres connection interface.</param>
    /// <param>"resAddress" a reference to the PGResult pointer.</param>
    /// <param>"ResultFormat" a reference to the posgres result indicator.</param>
    /// <param>"SingleRowMode" is the SingleRowMode turned on?.</param>
    /// <param>"UndefinedVarcharAsStringLength" obsolate, sets the varchar()
    ///  columns to a stringtype if the values is greater than zero. Note this
    ///  leads to string truncations.</param>
    constructor Create(const Statement: IZStatement; const SQL: string;
      const Connection: IZPostgreSQLConnection; resAddress: PPGresult;
      ResultFormat: PInteger; SingleRowMode: Boolean;
      const UndefinedVarcharAsStringLength: Integer);

    /// <summary>Resets the Cursor position to beforeFirst, releases server and
    ///  client resources but keeps buffers or Column-Informations alive.</summary>
    procedure ResetCursor; override;

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
    function GetLong(ColumnIndex: Integer): Int64;
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
    procedure GetTime(ColumnIndex: Integer; Var Result: TZTime); reintroduce; overload;
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
    procedure GetTimestamp(ColumnIndex: Integer; Var Result: TZTimeStamp); reintroduce; overload;
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

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

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
    /// <summary>Moves the cursor down one row from its current position. A
    ///  <c>ResultSet</c> cursor is initially positioned before the first row;
    ///  the first call to the method <c>next</c> makes the first row the
    ///  current row; the second call makes the second row the current row, and
    ///  so on. If an input stream is open for the current row, a call to the
    ///  method <c>next</c> will implicitly close it. A <c>ResultSet</c>
    ///  object's warning chain is cleared when a new row is read.</summary>
    /// <returns><c>true</c> if the new current row is valid; <c>false</c> if
    ///  there are no more rows</returns>
    function Next: Boolean; reintroduce;
    {$IFDEF USE_SYNCOMMONS}
  public
    procedure ColumnsToJSON(JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions = [jcoEndJSONObject]);
    {$ENDIF USE_SYNCOMMONS}
  end;

  /// <summary>Defines a Postgres OID Blob interface</summary>
  IZPostgreSQLOidBlob = interface(IZBlob)
    ['{BDFB6B80-477D-4CB1-9508-9541FEA6CD72}']
    /// <summary>Get the OID of the BLOB. A zero value represents an
    ///  invalid OID.</summary>
    /// <returns>the BLOB OID</returns>
    function GetBlobOid: Oid;
  end;

  /// <summary>Implements a Postgres OID Blob object</summary>
  TZPostgreSQLOidBlob = class(TZAbstractStreamedLob, IZLob, IZBlob,
    IZPostgreSQLOidBlob, IImmediatelyReleasable)
  private
    FBlobOid: Oid;
    FPlainDriver: TZPostgreSQLPlainDriver;
    FOwner: IZPostgreSQLConnection;
  protected
    function CreateLobStream(CodePage: Word; LobStreamMode: TZLobStreamMode): TStream; override;
  public
    constructor Create(const Connection: IZPostgreSQLConnection; BlobOid: Oid; LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
    constructor CreateFromBlob(const Value: IZBlob; const Connection: IZPostgreSQLConnection; const OpenLobStreams: TZSortedList);
  public //implement IZPostgreSQLOidBlob
    /// <summary>Get the OID of the BLOB. A zero value represents an
    ///  invalid OID.</summary>
    /// <returns>the BLOB OID</returns>
    function GetBlobOid: Oid;
  public // implement IZBlob
    /// <summary>Clones this blob object.</summary>
    /// <param>"LobStreamMode" the mode the cloned object is used for is one of:
    ///  <c>lsmRead, lsmWrite, lsmReadWrite</c></param>
    /// <returns> a cloned blob object.</returns>
    function Clone(LobStreamMode: TZLobStreamMode): IZBlob;
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
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
    function GetConSettings: PZConSettings;
  public
    function IsEmpty: Boolean; override;
    procedure Clear; override;
  public //obsolete
    function Length: Integer; override;
  end;

  TZPostgreSQLOidBlobStream = class(TZImmediatelyReleasableLobStream)
  private
    FPlainDriver: TZPostgreSQLPlainDriver;
    FBlobHandle: Integer;
    FLobStreamMode: TZLobStreamMode;
    FHandle: TPGconn;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FOwnerLob: TZPostgreSQLOidBlob;
    FLobIsOpen: Boolean;
    procedure BeforeWrite;
    procedure BeforeRead;
  protected
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(const OwnerLob: TZPostgreSQLOidBlob;
      LobStreamMode: TZLobStreamMode);
    destructor Destroy; override;
  public
    Updated: Boolean;
  public
    procedure OpenLob;
    procedure CloseLob;
    procedure CreateLob;
  public //TStream  overrides
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

  TZPostgreSQLByteaHexBlob = class(TZLocalMemBLob)
  public
    constructor Create(Data: PAnsiChar);
  end;

  TZPostgreSQLByteaEscapedBlob = class(TZLocalMemBLob)
  public
    constructor Create(const PlainDriver: TZPostgreSQLPlainDriver;
      Data: PAnsiChar);
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenerateSQLCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL version 7.4 and up. }
  TZPostgreSQLCachedResolverV74up = class(TZPostgreSQLCachedResolver)
  public
    /// <summary>Forms a where clause for UPDATE or DELETE DML statements.</summary>
    /// <param>"SQLWriter" a TZSQLStringWriter object used for buffered writes</param>
    /// <param>"OldRowAccessor" an accessor object to old column values.</param>
    /// <param>"Result" a reference to the Result String the SQLWriter uses
    ///  for the buffered writes.</param>
    procedure FormWhereClause(const SQLWriter: TZSQLStringWriter;
      const OldRowAccessor: TZRowAccessor; var Result: SQLString); override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL version 8.0 and up. }
  TZPostgreSQLCachedResolverV8up = class(TZPostgreSQLCachedResolverV74up)
  protected
    procedure SetResolverStatementParamters(const Statement: IZStatement;
      {$IFDEF AUTOREFCOUNT}const {$ENDIF} Params: TStrings); override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL version 10.0 and up. }
  TZPostgreSQLCachedResolverV10up = class(TZPostgreSQLCachedResolverV8up)
  protected
    FInsertReturningFields: TStrings;
    FHasAutoIncrementColumns, FHasWritableAutoIncrementColumns: Boolean;
    FReturningPairs: TZIndexPairList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    /// <summary>Forms an INSERT statement.</summary>
    /// <param>"NewRowAccessor" an accessor object to new column values.</param>
    /// <returns>the composed insert SQL.</returns>
    function FormInsertStatement(NewRowAccessor: TZRowAccessor): SQLString; override;
    procedure UpdateAutoIncrementFields(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor; const Resolver: IZCachedResolver); override;
    procedure PostUpdates(const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      const OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

  { TZPostgresCachedResultSet }

  TZPostgresCachedResultSet = class(TZCachedResultSet)
  protected
    procedure FillColumnsInfo(const ColumnsInfo: TObjectList); override;
    class function GetRowAccessorClass: TZRowAccessorClass; override;
  public
    function CreateLob(ColumnIndex: Integer; LobStreamMode: TZLobStreamMode): IZBlob{IZLob}; override;
  end;

  { TZPostgreSQLRowAccessor }

  TZPostgreSQLRowAccessor = class(TZRowAccessor)
  protected
    class function MetadataToAccessorType(ColumnInfo: TZColumnInfo;
      ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType; override;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} Math, SysConst, TypInfo,
  ZMessages, ZEncoding, ZFastCode, ZVariant, ZTokenizer,
  ZGenericSqlAnalyser, ZSelectSchema,
  ZDbcPostgreSqlMetadata, ZDbcMetadata, ZDbcPostgreSqlUtils, ZDbcUtils,
  ZDbcProperties;

{ TZPostgreSQLResultSet }

{$IFDEF USE_SYNCOMMONS}
procedure TZPostgreSQLResultSet.ColumnsToJSON(
  JSONWriter: TJSONWriter; JSONComposeOptions: TZJSONComposeOptions);
var
  C: Cardinal;
  L: NativeUInt;
  P, pgBuff: PAnsiChar;
  RNo, H, I: Integer;
  TS: TZTimeStamp;
  Months, Days: Integer absolute TS;
label ProcBts, jmpTime, jmpTS, jmpOIDBLob;
begin
  RNo := PGRowNo;
  if JSONWriter.Expand then
    JSONWriter.Add('{');
  if Assigned(JSONWriter.Fields) then
    H := High(JSONWriter.Fields) else
    H := High(JSONWriter.ColNames);
  for I := 0 to H do begin
    if Pointer(JSONWriter.Fields) = nil then
      C := I else
      C := JSONWriter.Fields[i];
    if FPlainDriver.PQgetisnull(Fres, RNo, C) <> 0 then
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
      P := FPlainDriver.PQgetvalue(Fres, RNo, C);
      with TZPGColumnInfo(ColumnsInfo[C]) do begin
        if FBinaryValues then
          case ColumnType of
            stUnknown     : JSONWriter.AddShort('null');
            stBoolean     : JSONWriter.AddShort(JSONBool[PByte(P)^<>0]);
            stSmall       : JSONWriter.Add(PG2SmallInt(P));
            stInteger     : JSONWriter.Add(PG2Integer(P));
            stLong        : JSONWriter.Add(PG2Int64(P));
            stCurrency    : if TypeOID = CASHOID
                            then JSONWriter.AddCurr64(PGCash2Currency(P))
                            else JSONWriter.AddCurr64(PGNumeric2Currency(P));
            stFloat       : JSONWriter.AddSingle(PG2Single(P));
            stDouble      : JSONWriter.AddDouble(PG2Double(P));
            stBigDecimal  : begin
                              pgBuff := PAnsiChar(FByteBuffer)+SizeOf(TBCD);
                              PGNumeric2BCD(P, PBCD(FByteBuffer)^);
                              JSONWriter.AddNoJSONEscape(pgBuff,
                                BCDToRaw(PBCD(FByteBuffer)^, pgBuff, '.'));
                            end;
            stBytes,
            stBinaryStream: if TypeOID = BYTEAOID then
                              JSONWriter.WrBase64(P, FPlainDriver.PQgetlength(Fres, RNo, C), True)
                            else begin
                              PPointer(FByteBuffer)^ := nil; //init avoid gpf
                              L := PG2Cardinal(P);
jmpOIDBLob:                   PIZlob(FByteBuffer)^ := TZPostgreSQLOidBlob.Create(FPGConnection, L , lsmRead, FOpenLobStreams);
                              P := PIZlob(FByteBuffer)^.GetBuffer(fRawTemp, L);
                              JSONWriter.WrBase64(P, L, True);
                              PIZlob(FByteBuffer)^ := nil;
                              fRawTemp := '';
                            end;
            stGUID        : begin
                              JSONWriter.Add('"');
                              {$IFNDEF ENDIAN_BIG} {$Q-} {$R-}
                              PGUID(fByteBuffer)^.D1 := PG2Cardinal(@PGUID(P).D1); //what a *beep* swapped digits! but only on reading
                              PGUID(fByteBuffer)^.D2 := (PGUID(P).D2 and $00FF shl 8) or (PGUID(P).D2 and $FF00 shr 8);
                              PGUID(fByteBuffer)^.D3 := (PGUID(P).D3 and $00FF shl 8) or (PGUID(P).D3 and $FF00 shr 8);
                              PInt64(@PGUID(fByteBuffer)^.D4)^ := PInt64(@PGUID(P).D4)^;
                              JSONWriter.Add(PGUID(fByteBuffer)^);
                              {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
                              {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
                              {$ELSE}
                              JSONWriter.Add(PGUID(P)^);
                              {$ENDIF}
                              JSONWriter.Add('"');
                            end;
            stDate        : begin
                              if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('ISODate("')
                              else if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                              PG2Date(PInteger(P)^, TS.Year, TS.Month, TS.Day);
                              DateToIso8601PChar(PUTF8Char(FByteBuffer), True, TS.Year, TS.Month, TS.Day);
                              JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer), 10);
                              if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('Z)"')
                              else JSONWriter.Add('"');
                            end;
            stTime        : begin
                              if Finteger_datetimes
                              then dt2Time(PG2Int64(P), TS.Hour, TS.Minute, TS.Second, Ts.Fractions)
                              else dt2Time(PG2Double(P), TS.Hour, TS.Minute, TS.Second, Ts.Fractions);
jmpTime:                      if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('ISODate("0000-00-00')
                              else if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                              TimeToIso8601PChar(PUTF8Char(FByteBuffer), True, TS.Hour, TS.Minute, TS.Second, TS.Fractions div NanoSecsPerMSec, 'T', jcoMilliseconds in JSONComposeOptions);
                              JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer), 9+4*Ord(not (jcoMongoISODate in JSONComposeOptions) and (jcoMilliseconds in JSONComposeOptions)));
                              if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('Z)"')
                              else JSONWriter.Add('"');
                            end;
            stTimestamp   : begin
                              if Finteger_datetimes
                              then PG2DateTime(PInt64(P)^, TS.Year, TS.Month, TS.Day, Ts.Hour, TS.Minute, TS.Second, TS.Fractions)
                              else PG2DateTime(PDouble(P)^, TS.Year, TS.Month, TS.Day, Ts.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTS:                        if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('ISODate("')
                              else if jcoDATETIME_MAGIC in JSONComposeOptions
                                then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                                else JSONWriter.Add('"');
                              DateToIso8601PChar(PUTF8Char(FByteBuffer), True, TS.Year, TS.Month, TS.Day);
                              TimeToIso8601PChar(PUTF8Char(FByteBuffer)+10, True, TS.Hour, TS.Minute, TS.Second, TS.Fractions div NanoSecsPerMSec, 'T', jcoMilliseconds in JSONComposeOptions);
                              JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer),19+(4*Ord(not (jcoMongoISODate in JSONComposeOptions) and (jcoMilliseconds in JSONComposeOptions))));
                              if jcoMongoISODate in JSONComposeOptions
                              then JSONWriter.AddShort('Z)"')
                              else JSONWriter.Add('"');
                            end;
            stString,
            stUnicodeString:if (TypeOID = MACADDROID) then begin
                              JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer), PGMacAddr2Raw(P, PAnsiChar(FByteBuffer)));
                              JSONWriter.Add('"');
                            end else if (TypeOID = INETOID) or (TypeOID = CIDROID) then begin
                              JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(PUTF8Char(FByteBuffer), PGInetAddr2Raw(P, PAnsiChar(FByteBuffer)));
                              JSONWriter.Add('"');
                            end else if TypeOID = INTERVALOID then begin
                              if Finteger_datetimes
                              then PG2Time(PInt64(P)^, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                              else PG2Time(PDouble(P)^, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
                              Months := PG2Integer(P+12);
                              TS.IsNegative := Months < 0;
                              if Months < 0 then
                                Months := -Months;
                              if Months > 12 then begin
                                TS.Year := Months div 12;
                                TS.Month := Months mod 12;
                              end else begin
                                TS.Year := 0;
                                TS.Month := Months;
                              end;
                              Days := PG2Integer(P+8);
                              if Days < 0 then begin
                                TS.IsNegative := True;
                                Days := -Days;
                              end;
                              TS.Day := Days;
                              if (TS.Day > 0) or (TS.Month > 0)
                              then goto jmpTS
                              else goto jmpTime;
                            end else begin
                              JSONWriter.Add('"');
                              if (TypeOID = CHAROID) or (TypeOID = BPCHAROID)
                              then JSONWriter.AddJSONEscape(P, ZDbcUtils.GetAbsorbedTrailingSpacesLen(P, SynCommons.StrLen(P)))
                              else JSONWriter.AddJSONEscape(P{, SynCommons.StrLen(P)});
                              JSONWriter.Add('"');
                            end;
            stAsciiStream,
            stUnicodeStream:if (TypeOID = JSONOID) or (TypeOID = JSONBOID) then
                              JSONWriter.AddNoJSONEscape(P{, SynCommons.StrLen(P)})
                            else begin
                              JSONWriter.Add('"');
                              JSONWriter.AddJSONEscape(P{, SynCommons.StrLen(P)});
                              JSONWriter.Add('"');
                            end;
            //stArray, stDataSet,
          end
        else
          case ColumnType of
            stUnknown     : JSONWriter.AddShort('null');
            stBoolean     : JSONWriter.AddShort(JSONBool[StrToBoolEx(P, True, (TypeOID = CHAROID) or (TypeOID = BPCHAROID))]);
            stByte..stDouble, stBigDecimal  : JSONWriter.AddNoJSONEscape(P, ZFastCode.StrLen(P));
            stCurrency    : JSONWriter.AddDouble(ZSysUtils.SQLStrToFloatDef(P, 0, ZFastCode.StrLen(P)));
            stBytes,
            stBinaryStream: if TypeOID = BYTEAOID then begin
                              pgBuff := nil;
                              if FIs_bytea_output_hex then begin
                                {skip trailing /x}
                                L := (ZFastCode.StrLen(P)-2) shr 1;
                                try
                                  GetMem(pgBuff, L);
                                  HexToBin(P+2, pgBuff, L);
                                  JSONWriter.WrBase64(pgBuff, L, True);
                                finally
                                  FreeMem(pgBuff);
                                end;
                              end else if Assigned(FPlainDriver.PQUnescapeBytea) then
                                try
                                  pgBuff := FPlainDriver.PQUnescapeBytea(P, @L);
                                  JSONWriter.WrBase64(pgBuff, L, True);
                                finally
                                  FPlainDriver.PQFreemem(pgBuff);
                                end
                              else
                                JSONWriter.WrBase64(P, FPlainDriver.PQgetlength(Fres, RNo, C), True);
                            end else begin
                              L := RawToUint64(P);
                              goto jmpOIDBLob;
                            end;
            stGUID        : begin
                              JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(P);//
                              JSONWriter.Add('"');
                            end;
            stDate        : if jcoMongoISODate in JSONComposeOptions then begin
                              JSONWriter.AddShort('ISODate("');
                              JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.AddShort('Z)"');
                            end else begin
                              if jcoDATETIME_MAGIC in JSONComposeOptions
                              then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              else JSONWriter.Add('"');
                              JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.Add('"');
                            end;
            stTime        : if jcoMongoISODate in JSONComposeOptions then begin
                              JSONWriter.AddShort('ISODate("0000-00-00T');
                              JSONWriter.AddNoJSONEscape(P, 8); //mongo has no milliseconds
                              JSONWriter.AddShort('Z)"');
                            end else begin
                              if jcoDATETIME_MAGIC in JSONComposeOptions
                              then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              else JSONWriter.Add('"');
                              JSONWriter.Add('T');
                              if ((P+8)^ <> '.') or not (jcoMilliseconds in JSONComposeOptions) //time zone ?
                              then JSONWriter.AddNoJSONEscape(P, 8)
                              else JSONWriter.AddNoJSONEscape(P, 12);
                              JSONWriter.Add('"');
                            end;
            stTimestamp   : if jcoMongoISODate in JSONComposeOptions then begin
                              JSONWriter.AddShort('ISODate("');
                              JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.Add('T');
                              JSONWriter.AddNoJSONEscape(P+11, 8);//mongo has no milliseconds
                              JSONWriter.AddShort('Z)"');
                            end else begin
                              if jcoDATETIME_MAGIC in JSONComposeOptions
                              then JSONWriter.AddNoJSONEscape(@JSON_SQLDATE_MAGIC_QUOTE_VAR,4)
                              else JSONWriter.Add('"');
                              (P+10)^ := 'T';
                              {JSONWriter.AddNoJSONEscape(P, 10);
                              JSONWriter.Add('T');
                              if ((P+19)^ <> '.') or not (jcoMilliseconds in JSONComposeOptions)
                              then JSONWriter.AddNoJSONEscape(P+11, 8)
                              else} JSONWriter.AddNoJSONEscape(P{+11, 12});
                              JSONWriter.Add('"');
                            end;
            stString,
            stUnicodeString:begin
                              JSONWriter.Add('"');
                              if (TypeOID = CHAROID) or (TypeOID = BPCHAROID)
                              then JSONWriter.AddJSONEscape(P, ZDbcUtils.GetAbsorbedTrailingSpacesLen(P, SynCommons.StrLen(P)))
                              else JSONWriter.AddJSONEscape(P{, SynCommons.StrLen(P)});
                              JSONWriter.Add('"');
                            end;
            stAsciiStream,
            stUnicodeStream:if (TypeOID = JSONOID) or (TypeOID = JSONBOID) then
                              JSONWriter.AddNoJSONEscape(P{, SynCommons.StrLen(P)})
                            else begin
                              JSONWriter.Add('"');
                              JSONWriter.AddJSONEscape(P{, SynCommons.StrLen(P)});
                              JSONWriter.Add('"');
                            end;
            //stArray, stDataSet,
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
{$ENDIF USE_SYNCOMMONS}

constructor TZPostgreSQLResultSet.Create(const Statement: IZStatement;
  const SQL: string; const Connection: IZPostgreSQLConnection; resAddress: PPGresult;
  ResultFormat: PInteger; SingleRowMode: Boolean;
  const UndefinedVarcharAsStringLength: Integer);
var PGCon: IZPostgreSQLConnection;
begin
  inherited Create(Statement, SQL,
    TZPostgresResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  FPGConnection := Connection;
  FconnAddress := FPGConnection.GetPGconnAddress;
  FByteBuffer := FPGConnection.GetByteBufferAddress;
  Fres := resAddress^;
  FresAddress := resAddress;
  FPlainDriver := Connection.GetPlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FUndefinedVarcharAsStringLength := UndefinedVarcharAsStringLength;
  FIsOidAsBlob := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Statement, DSProps_OidAsBlob, 'False'));
  Statement.GetConnection.QueryInterface(IZPostgreSQLConnection, PGCon);
  FIs_bytea_output_hex := PGCon.Is_bytea_output_hex;
  Finteger_datetimes := PGCon.integer_datetimes;
  PGCon := nil;
  FResultFormat := ResultFormat;
  FBinaryValues := FResultFormat^ = ParamFormatBin;
  FClientCP := ConSettings.ClientCodePage.CP;
  FSingleRowMode := SingleRowMode;
  if FSingleRowMode
  then SetType(rtForwardOnly)
  else SetType(rtScrollInsensitive);
  Open;
end;

function TZPostgreSQLResultSet.CreatePGConvertError(
  ColumnIndex: Integer; DataType: OID): EZPGConvertError;
begin
  Result := EZPGConvertError.Create(Format(SErrorConvertionField,
        [TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnLabel,
          TypInfo.GetEnumName(TypeInfo(TZSQLType),
          Ord(TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType))])+
          '; OID: '+IntToStr(DataType));
end;

procedure TZPostgreSQLResultSet.AssignColumnsInfo(
  const Dest: TObjectList);
var
  I: Integer;
  Current: TZPGColumnInfo;
  ColumnInfo: TZPGColumnInfo;
begin
  for I := 0 to ColumnsInfo.Count - 1 do
  begin
    Current := TZPGColumnInfo(ColumnsInfo[I]);
    ColumnInfo := TZPGColumnInfo.Create;

    ColumnInfo.AutoIncrement := Current.AutoIncrement;
    ColumnInfo.CaseSensitive := Current.CaseSensitive;
    ColumnInfo.Searchable := Current.Searchable;
    ColumnInfo.Currency := Current.Currency;
    ColumnInfo.Nullable := Current.Nullable;
    ColumnInfo.Signed := Current.Signed;
    ColumnInfo.ColumnLabel := Current.ColumnLabel;
    ColumnInfo.ColumnName := Current.ColumnName;
    ColumnInfo.SchemaName := Current.SchemaName;
    ColumnInfo.Precision := Current.Precision;
    ColumnInfo.Scale := Current.Scale;
    ColumnInfo.TableName := Current.TableName;
    ColumnInfo.CatalogName := Current.CatalogName;
    ColumnInfo.ColumnType := Current.ColumnType;
    ColumnInfo.ReadOnly := Current.ReadOnly;
    ColumnInfo.Writable := Current.Writable;
    ColumnInfo.DefinitelyWritable := Current.DefinitelyWritable;
    ColumnInfo.ColumnCodePage := Current.ColumnCodePage;

    ColumnInfo.TableOID := Current.TableOID;
    ColumnInfo.TableColNo := Current.TableColNo;
    ColumnInfo.TypeOID := Current.TypeOID;
    Dest.Add(ColumnInfo);
  end;
end;

procedure TZPostgreSQLResultSet.DefinePostgreSQLToSQLType(
  {$IFDEF AUTOREFCOUNT}var{$ENDIF}ColumnInfo: TZPGColumnInfo; TypeOid: Oid;
  TypeModifier: Integer);
var
  SQLType: TZSQLType;
  Connection: IZPostgreSQLConnection;
label asignTScaleAndPrec;
begin
  Connection := Statement.GetConnection as IZPostgreSQLConnection;

  case TypeOid of
    CASHOID: begin
        ColumnInfo.Currency := True; { money }
        ColumnInfo.Precision := 22;
        ColumnInfo.Scale := 2;
        ColumnInfo.ColumnType := stCurrency;
        ColumnInfo.Signed := True;
        ColumnInfo.Currency := True;
        Exit;
      end;
    NAMEOID: if (Connection.GetServerMajorVersion < 7) or
           ((Connection.GetServerMajorVersion = 7) and (Connection.GetServerMinorVersion < 3))
        then ColumnInfo.Precision := 32
        else ColumnInfo.Precision := 64; { name }
    CIDROID: ColumnInfo.Precision := 100; { cidr }
    INETOID: ColumnInfo.Precision := 100{39}; { inet }
    MACADDROID: ColumnInfo.Precision := 17; { macaddr }
    INTERVALOID: ColumnInfo.Precision := 32; { interval }
    REGPROCOID: ColumnInfo.Precision := 64; { regproc } // M.A. was 10
    BYTEAOID: begin{ bytea }
        if TypeModifier >= VARHDRSZ then begin
          ColumnInfo.Precision := TypeModifier - VARHDRSZ;
          ColumnInfo.ColumnType := stBytes;
        end else begin
          ColumnInfo.Precision := -1;
          ColumnInfo.ColumnType := stBinaryStream;
        end;
        Exit;
      end;
    //see: https://www.postgresql.org/message-id/slrnd6hnhn.27a.andrew%2Bnonews%40trinity.supernews.net
    //macro:
    //numeric: this is ugly, the typmod is ((prec << 16) | scale) + VARHDRSZ,
    //i.e. numeric(10,2) is ((10 << 16) | 2) + 4
    NUMERICOID: if TypeModifier <> -1 then begin
        ColumnInfo.Precision := (TypeModifier - VARHDRSZ) shr 16 and $FFFF;
        ColumnInfo.Scale     := (TypeModifier - VARHDRSZ)        and $FFFF;
        if (ColumnInfo.Scale <= 4) and (ColumnInfo.Precision <= sAlignCurrencyScale2Precision[ColumnInfo.Scale])
        then ColumnInfo.ColumnType := stCurrency
        else ColumnInfo.ColumnType := stBigDecimal;
        Exit;
      end;
    TIMESTAMPOID, TIMESTAMPTZOID, ABSTIMEOID: begin
      ColumnInfo.ColumnType := stTimestamp; { timestamp,timestamptz/abstime. no 'datetime' any more}
      goto asignTScaleAndPrec;
    end;
    TIMEOID, TIMETZOID: begin
      ColumnInfo.ColumnType := stTime;
asignTScaleAndPrec:
      if TypeModifier = -1 //variable precision of second fractions
      then ColumnInfo.Scale := {-}6 //tag variable
      else ColumnInfo.Scale := TypeModifier; //fixed second fractions..
      Exit;
    end;
  end;

  SQLType := PostgreSQLToSQLType(Connection.IsOidAsBlob, TypeOid, TypeModifier);

  if SQLType <> stUnknown then
    ColumnInfo.ColumnType := SQLType
  else begin
    ColumnInfo.ColumnType := stString;
    ColumnInfo.Precision := 255;
    ColumnInfo.ReadOnly := True;
  end;
end;

procedure TZPostgreSQLResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZPGColumnInfo;
  FieldMode, FieldSize, FieldType, FieldCount: Integer;
  P: PAnsiChar;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);
  if not Assigned(Fres) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  FFirstRow := True;

  { Fills the column info. }
  ColumnsInfo.Clear;
  FieldCount := FPlainDriver.PQnfields(Fres);
  for I := 0 to FieldCount - 1 do
  begin
    ColumnInfo := TZPGColumnInfo.Create;
    with ColumnInfo do
    begin
      TableOID := FPlainDriver.PQftable(Fres, I);
      TableColNo := FplainDriver.PQftablecol(Fres, I);
      //See: http://zeoslib.sourceforge.net/viewtopic.php?f=38&t=20797
      if TableColNo < 1 then
        // these fields have fixed numbers in the PostgreSQL source code, they seem to not use 0
        case TableColNo of
          0: ColumnName := '';
          -1: ColumnName := 'ctid';
          -2: ColumnName := 'oid';
          -3: ColumnName := 'xmin';
          -4: ColumnName := 'cmin';
          -5: ColumnName := 'xmax';
          -6: ColumnName := 'cmax';
          -7: ColumnName := 'tableoid';
        end;
      P := FPlainDriver.PQfname(Fres, I);
      Precision := ZFastCode.StrLen(P);
      {$IFDEF UNICODE}
      ColumnLabel := PRawToUnicode(P, Precision, FClientCP);
      {$ELSE}
      ColumnLabel := BufferToStr(P, Precision);
      {$ENDIF}
      Nullable := ntNullableUnknown; //there is NO information about nullable
      Precision := 0;

      FieldType := FPlainDriver.PQftype(Fres, I);

      TypeOID := FieldType;
      FieldMode := FPlainDriver.PQfmod(Fres, I);
      DefinePostgreSQLToSQLType(ColumnInfo, FieldType, FieldMode);
      if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]
      then ColumnCodePage := FClientCP
      else if (ColumnInfo.ColumnType in [stBytes, stBinaryStream])
        then ColumnCodePage := zCP_Binary
        else ColumnCodePage := High(Word);

      if Precision = 0 then begin
        FieldSize := FPlainDriver.PQfsize(Fres, I);
        Precision := Max(Max(FieldMode - 4, FieldSize), 0);

        if ColumnType in [stString, stUnicodeString] then begin
          {begin patch: varchar() is equal to text!}
          if ( FieldMode = -1 ) and ( FieldSize = -1 ) and ( FieldType = 1043) then
            if FUndefinedVarcharAsStringLength > 0
            then Precision := FUndefinedVarcharAsStringLength
            else DefinePostgreSQLToSQLType(ColumnInfo, 25, FieldMode) //assume text instead!
          else if ( (ColumnLabel = 'expr') or ( Precision = 0 ) ) then
            Precision := 255;
          if ColumnType = stString then
            CharOctedLength := Precision * ConSettings^.ClientCodePage^.CharWidth
          else if ColumnType = stUnicodeString then
            CharOctedLength := Precision shl 1;
        end;
      end;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

function TZPostgreSQLResultSet.PGRowNo: Integer;
begin
  if FSingleRowMode
  then Result := 0
  else Result := RowNo-1;
end;

procedure TZPostgreSQLResultSet.ResetCursor;
begin
  if not Closed then begin
    if FSingleRowMode and not IsAfterLast then begin
      while next do ;
    end else if Fres <> nil then begin
      FPlainDriver.PQclear(Fres);
      Fres := nil;
    end;
    FFirstRow := True;
    inherited ResetCursor;
  end;
end;

function TZPostgreSQLResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  Result := FPlainDriver.PQgetisnull(Fres, PGRowNo,
    ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}) <> 0;
end;

function TZPostgreSQLResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var L: LongWord;
  PEnd: PAnsiChar;
  BCD: TBCD;
  TS: TZTimeStamp absolute BCD;
  {$IFNDEF ENDIAN_BIG}UUID: TGUID absolute BCD;{$ENDIF}
  ROW_IDX: Integer;
  Days: Integer absolute ROW_IDX;
  Months: Integer absolute ROW_IDX;
  function FromOIDLob(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
  begin
    FRawTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetString;
    Result := Pointer(FRawTemp);
    Len := Length(FRawTemp);
  end;
label JmpPEndTinyBuf, JmpStr, jmpTime, jmpTS;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    Result := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:  if PByte(Result)^ = 0 then begin
                      Result := Pointer(BoolStrsRaw[True]);
                      Len := 4
                    end else begin
                      Result := Pointer(BoolStrsRaw[False]);
                      Len := 5;
                    end;
        stSmall:    begin
                      IntToRaw(PG2SmallInt(Result), PAnsiChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLongWord: begin
                      IntToRaw(PG2Cardinal(Result), PAnsiChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stInteger:  begin
                      IntToRaw(PG2Integer(Result), PAnsiChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLong:     begin
                      IntToRaw(PG2int64(Result), PAnsiChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stFloat:    begin
                      Len := FloatToSqlRaw(PG2Single(Result), PAnsiChar(fByteBuffer));
                      Result := PAnsiChar(fByteBuffer);
                    end;
        stDouble:   begin
                      Len := FloatToSqlRaw(PG2Double(Result), PAnsiChar(fByteBuffer));
                      Result := PAnsiChar(fByteBuffer);
                    end;
        stCurrency: begin
                      CurrToRaw(GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), '.', PAnsiChar(fByteBuffer), @PEnd);
JmpPEndTinyBuf:       Result := PAnsiChar(fByteBuffer);
                      Len := PEnd - Result;
                    end;
        stBigDecimal: begin
                      PGNumeric2BCD(Result, BCD{%H-});
                      Result := PAnsiChar(fByteBuffer);
                      Len := BCDToRaw(BCD, PAnsiChar(fByteBuffer), '.');
                    end;
        stDate:     begin
                      PG2Date(PInteger(Result)^, TS.Year, TS.Month, TS.Day);
                      Result := PAnsiChar(fByteBuffer);
                      Len := DateToRaw(TS.Year, TS.Month, TS.Day, Result,
                        ConSettings.ReadFormatSettings.DateFormat, False, False);
                    end;
        stTime:     begin
                      if Finteger_datetimes
                      then dt2time(PG2Int64(Result), TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else dt2time(PG2Double(Result), TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTime:              Result := PAnsiChar(fByteBuffer);
                      Len := TimeToRaw(TS.Hour, TS.Minute, TS.Second, TS.Fractions,
                        Result, ConSettings.ReadFormatSettings.TimeFormat, False, False);
                    end;
        stTimestamp:begin
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(Result)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else PG2DateTime(PDouble(Result)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTS:                Result := PAnsiChar(fByteBuffer);
                      Len := ZSysUtils.DateTimeToRaw(TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute,
                        TS.Second, TS.Fractions, Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                    end;
        stGUID:     begin
                      {$IFNDEF ENDIAN_BIG} {$Q-} {$R-}
                      UUID.D1 := PG2Cardinal(@PGUID(Result).D1); //what a *beep* swapped digits! but only on reading
                      UUID.D2 := (PGUID(Result).D2 and $00FF shl 8) or (PGUID(Result).D2 and $FF00 shr 8);
                      UUID.D3 := (PGUID(Result).D3 and $00FF shl 8) or (PGUID(Result).D3 and $FF00 shr 8);
                      PInt64(@UUID.D4)^ := PInt64(@PGUID(Result).D4)^;
                      ZSysUtils.GUIDToBuffer(@UUID.D1, PAnsiChar(fByteBuffer), []); //pg does not Return brackets adopt behavior
                      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
                      {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
                      {$ELSE}
                      ZSysUtils.GUIDToBuffer(Result, PAnsiChar(fByteBuffer), []); //pg does not Return brackets adopt behavior
                      {$ENDIF}
                      for ColumnIndex := 0 to 8 do
                        PCardinal(PAnsiChar(fByteBuffer)+(4*ColumnIndex))^ := PCardinal(PAnsiChar(fByteBuffer)+(4*ColumnIndex))^ or $20202020;
                      Result := PAnsiChar(fByteBuffer);
                      Len := 36;
                    end;
        stString,
        stUnicodeString: if (TypeOID = MACADDROID) then begin
                      Len := PGMacAddr2Raw(Result, PAnsiChar(fByteBuffer));
                      Result := PAnsiChar(fByteBuffer);
                    end else if (TypeOID = INETOID) or (TypeOID = CIDROID) then begin
                      Len := PGInetAddr2Raw(Result, PAnsiChar(fByteBuffer));
                      Result := PAnsiChar(fByteBuffer);
                    end else if TypeOID = INTERVALOID then begin
                      if Finteger_datetimes
                      then PG2Time(PInt64(Result)^, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else PG2Time(PDouble(Result)^, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
                      Months := PG2Integer(Result+12);
                      TS.IsNegative := Months < 0;
                      if Months < 0 then
                        Months := -Months;
                      if Months > 12 then begin
                        TS.Year := Months div 12;
                        TS.Month := Months mod 12;
                      end else begin
                        TS.Year := 0;
                        TS.Month := Months;
                      end;
                      Days := PG2Integer(Result+8);
                      if Days < 0 then begin
                        TS.IsNegative := True;
                        Days := -Days;
                      end;
                      TS.Day := Days;
                      if (TS.Day > 0) or (TS.Month > 0)
                      then goto jmpTS
                      else goto jmpTime;
                    end else goto jmpStr;
        stAsciiStream,
        stUnicodeStream:Len := ZFastCode.StrLen(Result);
        stBytes:        Len := FPlainDriver.PQgetlength(Fres, ROW_IDX, ColumnIndex);
        stBinaryStream: if TypeOID = OIDOID
                        then Result := FromOIDLob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Len)
                        else Len := FPlainDriver.PQgetlength(Fres, ROW_IDX, ColumnIndex);
        else            begin
                          Result := PEmptyAnsiString;
                          Len := 0;
                        end;
      end
    else case TypeOID of
      BYTEAOID: if FIs_bytea_output_hex then begin
                  {skip trailing /x}
                  SetLength(FRawTemp, (ZFastCode.StrLen(Result)-2) shr 1);
                  Len := Length(FRawTemp);
                  HexToBin(Result+2, Pointer(FRawTemp), Len);
                  Result := Pointer(FRawTemp);
                end else if Assigned(FPlainDriver.PQUnescapeBytea) then begin
                  Result := FPlainDriver.PQUnescapeBytea(Result, @L);
                  ZSetString(Result, L, FRawTemp);
                  Len := Length(FRawTemp);
                  FPlainDriver.PQFreemem(Result);
                  Result := Pointer(FRawTemp);
                end else
                  Len := FPlainDriver.PQgetlength(Fres, ROW_IDX, ColumnIndex);
      {OIDOID:   if TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType = stBinaryStream then begin
                  FTempLob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0, FconnAddress^,
                    RawToInt64Def(Result, 0), FChunk_Size); //Localize it
                  Result := FTempLob.GetBuffer;
                  Len := FTempLob.Length;
                end else
                  Len := ZFastCode.StrLen(Result); }
      else      begin
                  {http://www.postgresql.org/docs/9.0/static/libpq-exec.html
                  PQgetlength:
                   This is the actual data length for the particular data value, that is,
                   the size of the object pointed to by PQgetvalue.
                   For text data format this is the same as strlen().
                   For binary format this is essential information.
                   Note that one should not rely on PQfsize to obtain the actual data length.}
JmpStr:           Len := ZFastCode.StrLen(Result);
                  if (TypeOID = CHAROID) or (TypeOID = BPCHAROID) then
                    Len := GetAbsorbedTrailingSpacesLen(Result, Len);
                end;
    end;
  end;
end;

function TZPostgreSQLResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
var P: PAnsiChar;
  PEnd: PWideChar;
  BCD: TBCD;
  TS: TZTimeStamp absolute BCD;
  UUID: TGUID absolute BCD;
  C: Currency absolute BCD;
  ROW_IDX: Integer;
  Months: Integer absolute ROW_IDX;
  Days: Integer absolute ROW_IDX;
  procedure FromOIDLob(ColumnIndex: Integer);
  var Lob: IZBlob;
  begin
    Lob := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
    P := Lob.GetBuffer(fRawTemp, Len);
    FUniTemp := Ascii7ToUnicodeString(P, len);
  end;
label JmpPEndTinyBuf, JmpUni, jmpStr, jmpTxt, jmpRaw, jmpBin, jmpLen, jmpTime, jmpTS;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:  if PByte(P)^ = 0 then begin
                      Result := Pointer(BoolStrsW[True]);
                      Len := 4
                    end else begin
                      Result := Pointer(BoolStrsW[False]);
                      Len := 5;
                    end;
        stSmall:    begin
                      IntToUnicode(PG2SmallInt(P), PWideChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLongWord: begin
                      IntToUnicode(PG2Cardinal(P), PWideChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stInteger:  begin
                      IntToUnicode(PG2Integer(P), PWideChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stLong:     begin
                      IntToUnicode(PG2int64(P), PWideChar(fByteBuffer), @PEnd);
                      goto JmpPEndTinyBuf;
                    end;
        stFloat:    begin
                      Len := FloatToSqlUnicode(PG2Single(P), PWideChar(fByteBuffer));
                      Result := PWideChar(fByteBuffer);
                    end;
        stDouble:   begin
                      Len := FloatToSqlUnicode(PG2Double(P), PWideChar(fByteBuffer));
                      Result := PWideChar(fByteBuffer);
                    end;
        stCurrency: begin
                      C := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                      CurrToUnicode(C, '.', PWideChar(fByteBuffer), @PEnd);
JmpPEndTinyBuf:       Result := PWideChar(fByteBuffer);
                      Len := PEnd - Result;
                    end;
        stBigDecimal: begin
                      PGNumeric2BCD(P, BCD{%H-});
                      Result := PWideChar(fByteBuffer);
                      Len := BCDToUni(BCD, Result, '.');
                    end;
        stDate:     begin
                      PG2Date(PInteger(P)^, TS.Year, TS.Month, TS.Day);
                      Result := PWideChar(fByteBuffer);
                      Len := DateToUni(TS.Year, TS.Month, TS.Day,
                        Result, ConSettings.ReadFormatSettings.DateFormat, False, False);
                    end;
        stTime:     begin
                      if Finteger_datetimes
                      then dt2time(PG2Int64(P), TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else dt2time(PG2Double(P), TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTime:              Result := PWideChar(fByteBuffer);
                      Len := TimeToUni(TS.Hour, TS.Minute, TS.Second, TS.Fractions,
                        Result, ConSettings.ReadFormatSettings.TimeFormat, False, tS.IsNegative);
                    end;
        stTimestamp:begin
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(P)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else PG2DateTime(PDouble(P)^, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
jmpTS:                Result := PWideChar(fByteBuffer);
                      Len := ZSysUtils.DateTimeToUni(TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute,
                        TS.Second, TS.Fractions, Result, ConSettings.ReadFormatSettings.DateTimeFormat, False, False);
                    end;
        stGUID:     begin
                      {$IFNDEF ENDIAN_BIG} {$Q-} {$R-}
                      UUID.D1 := PG2Cardinal(@PGUID(P).D1); //what a *beep* swapped digits! but only on reading
                      UUID.D2 := (PGUID(P).D2 and $00FF shl 8) or (PGUID(P).D2 and $FF00 shr 8);
                      UUID.D3 := (PGUID(P).D3 and $00FF shl 8) or (PGUID(P).D3 and $FF00 shr 8);
                      PInt64(@UUID.D4)^ := PInt64(@PGUID(P).D4)^;
                      ZSysUtils.GUIDToBuffer(@UUID.D1, PWideChar(fByteBuffer), []); //pg does not Return brackets adopt behavior
                      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
                      {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
                      {$ELSE}
                      ZSysUtils.GUIDToBuffer(P, PWideChar(fByteBuffer), []); //pg does not Return brackets adopt behavior
                      {$ENDIF}
                      for ColumnIndex := 0 to 35 do //to lowercase
                        PWord(PWideChar(fByteBuffer)+ColumnIndex)^ := PWord(PWideChar(fByteBuffer)+ColumnIndex)^ or $0020;
                      Result := PWideChar(fByteBuffer);
                      Len := 36;
                    end;
        stUnicodeString,
        stString:   if (TypeOID = MACADDROID) then begin
                      Len := PGMacAddr2Uni(P, PWideChar(fByteBuffer));
                      Result := PWideChar(fByteBuffer);
                    end else if (TypeOID = INETOID) or (TypeOID = CIDROID) then begin
                      Len := PGInetAddr2Uni(P, PWideChar(fByteBuffer));
                      Result := PWideChar(fByteBuffer);
                    end else if TypeOID = INTERVALOID then begin
                      if Finteger_datetimes
                      then PG2Time(PInt64(P)^, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                      else PG2Time(PDouble(P)^, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
                      Months := PG2Integer(P+12);
                      TS.IsNegative := Months < 0;
                      if Months < 0 then
                        Months := -Months;
                      if Months > 12 then begin
                        TS.Year := Months div 12;
                        TS.Month := Months mod 12;
                      end else begin
                        TS.Year := 0;
                        TS.Month := Months;
                      end;
                      Days := PG2Integer(P+8);
                      if Days < 0 then begin
                        TS.IsNegative := True;
                        Days := -Days;
                      end;
                      TS.Day := Days;
                      if (TS.Day > 0) or (TS.Month > 0)
                      then goto jmpTS
                      else goto jmpTime;
                    end else goto jmpStr;
        stUnicodeStream,
        stAsciiStream: goto JmpTxt;
        stBytes:        begin
jmpBin:                   Len := FPlainDriver.PQgetlength(Fres, ROW_IDX, ColumnIndex);
                          goto jmpRaw;
                        end;
        stBinaryStream: if TypeOID = OIDOID then begin
                          FromOIDLob(ColumnIndex);
                          goto jmpLen;
                        end else
                          goto jmpBin;
        else            begin
                          Result := PEmptyUnicodeString;
                          Len := 0;
                        end;
      end
    else begin
      if ColumnType in [stString,stUnicodeString] then
jmpStr: if (TypeOID = CHAROID) or (TypeOID = BPCHAROID) then begin
          Len := GetAbsorbedTrailingSpacesLen(P, ZFastCode.StrLen(P));
          goto JmpUni;
        end else
          goto jmpTxt
      else if (ColumnType in [stAsciiStream,stUnicodeStream]) then begin
jmpTxt: Len := ZFastCode.StrLen(P);
JmpUni: FUniTemp := PRawToUnicode(P, Len, FClientCP);
      end else begin
         Len := ZFastCode.StrLen(P);
jmpRaw:  FUniTemp := Ascii7ToUnicodeString(P,Len);
      end;
jmpLen:
      Len := Length(FUniTemp);
      if (Len > 0)
      then Result := Pointer(FUniTemp)
      else Result := PEmptyUnicodeString;
    end;
  end;
end;

function TZPostgreSQLResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull then
    Result := False
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^ <> 0;
        stSmall:                      Result := PWord(P)^ <> 0;
        stLongWord, stInteger, stDate:Result := PCardinal(P)^ <> 0;
        stULong, stLong:              Result := PUint64(P)^ <> 0;
        stFloat:                      Result := PSingle(P)^ <> 0;
        stDouble:                     Result := PDouble(P)^ <> 0;
        stCurrency:                   if TypeOID = CASHOID
                                      then Result := PInt64(P)^ <> 0
                                      else Result := (PWord(P)^ <> 0);//read nbasedigit count
        stBigDecimal:                 Result := PWord(P)^ <> 0;//read nbasedigit count
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PInt64(P)^ <> 0
                                      else Result := PDouble(P)^ <> 0;
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := StrToBoolEx(P, True, (TypeOID = CHAROID) or (TypeOID = BPCHAROID));
        //stBytes: ;
        //stBinaryStream: ;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else
      Result := StrToBoolEx(P, True, (TypeOID = CHAROID) or (TypeOID = BPCHAROID));
  end;
end;

function TZPostgreSQLResultSet.GetBytes(ColumnIndex: Integer;
  out Len: NativeUInt): PByte;
var
  pgBuff: PAnsiChar;
  to_lenght: LongWord;
  TempLob: IZBLob;
  ResUUID: PGUID absolute Result;
  SrcUUID: PGUID absolute pgBuff;
  ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if not LastWasNull then with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    Result := PByte(FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex));
    if TypeOID = BYTEAOID {bytea} then begin
      if FBinaryValues then
        Len := FPlainDriver.PQgetlength(Fres, ROW_IDX, ColumnIndex)
      else if FIs_bytea_output_hex then begin
        {skip trailing /x}
        Len := (ZFastCode.StrLen(PAnsichar(Result))-2) shr 1;
        SetLength(FRawTemp, Len);
        if Len > 0 then begin
          HexToBin(PAnsichar(Result)+2, Pointer(FRawTemp), Len);
          Result := Pointer(FRawTemp);
        end;
      end else if Assigned(FPlainDriver.PQUnescapeBytea) then begin
        pgBuff := FPlainDriver.PQUnescapeBytea(PAnsiChar(Result), @to_lenght);
        ZSetString(pgBuff, to_lenght, FRawTemp);
        FPlainDriver.PQFreemem(pgBuff);
        Result := Pointer(FRawTemp);
        Len := to_lenght;
      end else begin
        pgBuff := PAnsiChar(Result);
        to_lenght := ZFastCode.StrLen(pgBuff);
        SetLength(FRawTemp, to_lenght);
        Result := Pointer(FRawTemp);
        Len := DecodeCString(to_lenght, pgBuff, PAnsichar(Result));
      end;
    end else if TypeOID = UUIDOID { uuid } then begin
      SetLength(FRawTemp, SizeOf(TGUID)); //take care we've a unique dyn-array if so then this alloc happens once
      pgBuff := PAnsiChar(Result);
      Result := Pointer(FRawTemp);
      Len := SizeOf(TGUID);
      if FBinaryValues then begin
        {$IFNDEF ENDIAN_BIG} {$Q-} {$R-}
        ResUUID.D1 := PG2Cardinal(@SrcUUID.D1); //what a *beep* swapped digits! but only on reading
        ResUUID.D2 := (SrcUUID.D2 and $00FF shl 8) or (SrcUUID.D2 and $FF00 shr 8);
        ResUUID.D3 := (SrcUUID.D3 and $00FF shl 8) or (SrcUUID.D3 and $FF00 shr 8);
        PInt64(@ResUUID.D4)^ := PInt64(@SrcUUID.D4)^;
        {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
        {$ELSE}
        ResUUID^ := SrcUUID^;
        {$ENDIF}
      end else ValidGUIDToBinary(pgBuff, Pointer(Result));
    end else if TypeOID = OIDOID { oid } then begin
      if FBinaryValues
      then Len := PG2Cardinal(Result)
      else Len := RawToIntDef(PAnsiChar(Result), 0);
      TempLob := TZPostgreSQLOidBlob.Create(FPGConnection, Len, lsmRead, FOpenLobStreams);
      FRawTemp := TempLob.GetString;
      TempLob := nil;
      Result := Pointer(FRawTemp);
      Len := Length(FRawTemp);
    end else begin
      Result := nil;
      Len := 0;
    end;
  end else begin
    Result := nil;
    Len := 0;
  end;
end;

function TZPostgreSQLResultSet.GetInt(ColumnIndex: Integer): Integer;
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := Trunc(PG2Single(P));
        stDouble:                     Result := Trunc(PG2Double(P));
        stCurrency:                   begin
                                        PCurrency(fByteBuffer)^ := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                                        Result := PInt64(fByteBuffer)^ div 10000;
                                      end;
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(fByteBuffer)^);
                                        Result := BCD2Int64(PBCD(fByteBuffer)^);
                                      end;
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PG2Int64(P)
                                      else Result := Trunc(PG2Double(P));
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := RawToIntDef(P, 0);
        //stBytes: ;
        stBinaryStream: if TypeOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else Result := RawToIntDef(P, 0);
  end;
end;

function TZPostgreSQLResultSet.GetLong(ColumnIndex: Integer): Int64;
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := Trunc(PG2Single(P));
        stDouble:                     Result := Trunc(PG2Double(P));
        stCurrency:                   begin
                                        PCurrency(@Result)^ := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                                        Result := Result div 10000;
                                      end;
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(fByteBuffer)^);
                                        BCD2Int64(PBCD(fByteBuffer)^, Result);
                                      end;
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PG2Int64(P)
                                      else Result := Trunc(PG2Double(P));
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := RawToInt64Def(P, 0);
        //stBytes: ;
        stBinaryStream: if TypeOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else Result := RawToInt64Def(P, 0);
  end;
end;

function TZPostgreSQLResultSet.GetUInt(
  ColumnIndex: Integer): Cardinal;
begin
  Result := GetLong(ColumnIndex);
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZPostgreSQLResultSet.GetULong(ColumnIndex: Integer): UInt64;
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := Trunc(PG2Single(P));
        stDouble:                     Result := Trunc(PG2Double(P));
        stCurrency:                   begin
                                        PCurrency(@Result)^ := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
                                        Result := Result div 10000;
                                      end;
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(fByteBuffer)^);
                                        BCD2UInt64(PBCD(fByteBuffer)^, Result);
                                      end;
        stTime, stTimestamp:          if Finteger_datetimes
                                      then Result := PG2Int64(P)
                                      else Result := Trunc(PG2Double(P));
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    Result := RawToUInt64Def(P, 0);
        //stBytes: ;
        stBinaryStream: if TypeOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else Result := RawToUInt64Def(P, 0);
  end;

end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

function TZPostgreSQLResultSet.GetFloat(ColumnIndex: Integer): Single;
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger, stDate:            Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := PG2Single(P);
        stDouble:                     Result := PG2Double(P);
        stCurrency:                   Result := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(fByteBuffer)^);
                                        Result := BCDToDouble(PBCD(fByteBuffer)^);
                                      end;
        stTime:                       if Finteger_datetimes
                                      then Result := PG2Time(PInt64(P)^)
                                      else Result := PG2Time(PDouble(P)^);
         stTimestamp:                 if Finteger_datetimes
                                      then Result := PG2DateTime(PInt64(P)^)
                                      else Result := PG2DateTime(PDouble(P)^);
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    SQLStrToFloatDef(P, Result, 0);
        //stBytes: ;
        stBinaryStream: if TypeOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else SQLStrToFloatDef(P, 0, Result);
  end;
end;

procedure TZPostgreSQLResultSet.GetGUID(ColumnIndex: Integer;
  var Result: TGUID);
var
  Buffer, pgBuff: PAnsiChar;
  Len: cardinal;
  SrcUUID: PGUID absolute Buffer;
  ROW_IDX: Integer;
label Fail;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stGUID);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if not LastWasNull then with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    Buffer := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if TypeOID = BYTEAOID {bytea} then begin
      if FBinaryValues then begin
        Len := FPlainDriver.PQgetlength(Fres, ROW_IDX, ColumnIndex);
        if Len = SizeOf(TGUID)
        then Move(Buffer^, Result.D1, SizeOf(TGUID))
        else goto Fail;
      end else if FIs_bytea_output_hex then begin
        {skip trailing /x}
        Len := (ZFastCode.StrLen(Buffer)-2) shr 1;
        if Len = SizeOf(TGUID)
        then {$IFDEF USE_SYNCOMMONS}SynCommons.{$ENDIF}HexToBin(Buffer+2, @Result.D1, SizeOf(TGUID))
        else goto Fail;
      end else if Assigned(FPlainDriver.PQUnescapeBytea) then begin
        pgBuff := FPlainDriver.PQUnescapeBytea(Buffer, @Len);
        if Len = SizeOf(TGUID) then begin
          Move(pgBuff^, Result.D1, SizeOf(TGUID));
          FPlainDriver.PQFreemem(pgBuff);
        end else begin
          FPlainDriver.PQFreemem(pgBuff);
          goto Fail;
        end;
      end else begin
        Len := ZFastCode.StrLen(Buffer);
        getMem(pgBuff, Len);
        Len := DecodeCString(Len, Buffer, pgBuff);
        if Len = SizeOf(TGUID) then begin
          Move(pgBuff^, Result.D1, SizeOf(TGUID));
          FreeMem(pgBuff);
        end else begin
          FreeMem(pgBuff);
          goto Fail;
        end;
      end;
    end else if TypeOID = UUIDOID { uuid } then begin
      if FBinaryValues then begin
        {$IFNDEF ENDIAN_BIG} {$Q-} {$R-}
        Result.D1 := PG2Cardinal(@SrcUUID.D1); //what a *beep* swapped digits! but only on reading
        Result.D2 := (SrcUUID.D2 and $00FF shl 8) or (SrcUUID.D2 and $FF00 shr 8);
        Result.D3 := (SrcUUID.D3 and $00FF shl 8) or (SrcUUID.D3 and $FF00 shr 8);
        PInt64(@Result.D4)^ := PInt64(@SrcUUID.D4)^;
        {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
        {$ELSE}
        Result := SrcUUID^;
        {$ENDIF ENDIAN_BIG}
      end else ValidGUIDToBinary(Buffer, @Result.D1);
    end else case ColumnType of
      stString,
      stUnicodeString: if (TypeOID = MACADDROID) or (TypeOID = INETOID) or (TypeOID = CIDROID) or (TypeOID = INTERVALOID)
                      then goto Fail
                      else begin
                        Len := ZFastCode.StrLen(Buffer);
                        if (TypeOID = CHAROID) or (TypeOID = BPCHAROID) then
                          Len := GetAbsorbedTrailingSpacesLen(Buffer, Len);
                        if (Len = 36) or (Len = 38)
                        then ValidGUIDToBinary(Buffer, @Result.D1)
                        else goto Fail;
                      end;
      else
Fail:       raise CreatePGConvertError(ColumnIndex, TypeOID);
    end;
  end else FillChar(Result, SizeOf(TGUID), #0);
end;

function TZPostgreSQLResultSet.GetDouble(ColumnIndex: Integer): Double;
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull then
    Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean:                    Result := PByte(P)^;
        stSmall:                      Result := PG2SmallInt(P);
        stInteger:                    Result := PG2Integer(P);
        stLongWord:                   Result := PG2Cardinal(P);
        stLong:                       Result := PG2Int64(P);
        stFloat:                      Result := PG2Single(P);
        stDouble:                     Result := PG2Double(P);
        stCurrency:                   Result := GetCurrency(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(fByteBuffer)^);
                                        Result := BCDToDouble(PBCD(fByteBuffer)^);
                                      end;
        stDate:                       Result := PG2Date(Pinteger(P)^);
        stTime:                       if Finteger_datetimes
                                      then Result := PG2Time(PInt64(P)^)
                                      else Result := PG2Time(PDouble(P)^);
        stTimestamp:                  if Finteger_datetimes
                                      then Result := PG2DateTime(PInt64(P)^)
                                      else Result := PG2DateTime(PDouble(P)^);
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    SQLStrToFloatDef(P, Result, 0);
        //stBytes: ;
        stBinaryStream: if TypeOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else SQLStrToFloatDef(P, 0, Result);
  end;
end;

procedure TZPostgreSQLResultSet.GetBigDecimal(ColumnIndex: Integer; var Result: TBCD);
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then FillChar(Result, SizeOf(TBCD), #0)
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean, stSmall,
        stInteger, stLong, stLongWord: ScaledOrdinal2BCD(GetLong(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), 0, Result);
        stFloat, stDouble,
        stDate, stTime, stTimeStamp:  Double2Bcd(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
        stCurrency:                   if TypeOID = NUMERICOID
                                      then PGNumeric2BCD(P, Result)
                                      else ScaledOrdinal2BCD(PG2Int64(P), 2, Result);
        stBigDecimal:                 PGNumeric2BCD(P, Result);
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    LastWasNull := not TryRawToBcd(P, StrLen(P), Result, '.');
        //stBytes: ;
        stBinaryStream: if TypeOID = OIDOID
                        then ScaledOrdinal2BCD(PG2Cardinal(P), 0, Result, False)
                        else Result := NullBCD;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else LastWasNull := not TryRawToBcd(P, StrLen(P), Result, '.');
  end;
end;

function TZPostgreSQLResultSet.GetConnection: IZPostgreSQLConnection;
begin
  Result := FPGConnection;
end;

function TZPostgreSQLResultSet.GetCurrency(
  ColumnIndex: Integer): Currency;
var P: PAnsiChar;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then Result := 0
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    if FBinaryValues then
      case ColumnType of
        stBoolean, stSmall,
        stInteger, stLong, stLongWord:Result := GetLong(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stFloat, stDouble,
        stDate, stTime, stTimeStamp:  Result := GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        stCurrency:                   if TypeOID = NUMERICOID
                                      then Result := PGNumeric2Currency(P)
                                      else Result := PGCash2Currency(P);
        stBigDecimal:                 begin
                                        PGNumeric2BCD(P, PBCD(fByteBuffer)^);
                                        BCDToCurr(PBCD(fByteBuffer)^, Result);
                                      end;
        //stGUID: ;
        stAsciiStream, stUnicodeStream,
        stString, stUnicodeString:    SQLStrToFloatDef(P, Result, 0);
        //stBytes: ;
        stBinaryStream: if TypeOID = OIDOID
                        then Result := PG2Cardinal(P)
                        else Result := 0;
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
      end
    else SQLStrToFloatDef(P, 0, FDecimalSeps[TypeOID = CASHOID], Result);
  end;
end;

procedure TZPostgreSQLResultSet.GetDate(ColumnIndex: Integer;
  var Result: TZDate);
var Len: NativeUInt;
    P: PAnsiChar;
    ROW_IDX: Integer;
    Months: Integer absolute ROW_IDX;
    Days: Integer absolute ROW_IDX;
label from_str, jmpZero;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then goto jmpZero
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    case ColumnType of
      stDate:       if FBinaryValues then begin
                      PG2Date(Pinteger(P)^, Result.Year, Result.Month, Result.Day);
                      Result.IsNegative := False;
                    end else begin
from_str:             Len := ZFastCode.StrLen(P);
                      LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, Result);
                      if LastWasNull then
                        goto jmpZero;
                    end;
      stTime:       goto jmpZero;
      stTimestamp:  if FBinaryValues then begin
                      Result.IsNegative := False;
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(P)^, Result.Year, Result.Month, Result.Day,
                        PZTime(fByteBuffer)^.Hour, PZTime(fByteBuffer)^.Minute,
                        PZTime(fByteBuffer)^.Second, PZTime(fByteBuffer)^.Fractions)
                      else PG2DateTime(PDouble(P)^, Result.Year, Result.Month, Result.Day,
                        PZTime(fByteBuffer)^.Hour, PZTime(fByteBuffer)^.Minute,
                        PZTime(fByteBuffer)^.Second, PZTime(fByteBuffer)^.Fractions);
                    end else begin
                      Len := ZFastCode.StrLen(P);
                      LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings,
                        PZTimeStamp(fByteBuffer)^);
                      if LastWasNull then
jmpZero:                PInt64(@Result.Year)^ := 0
                      else begin
                        Result := PZDate(fByteBuffer)^;
                        Result.IsNegative := PZTimeStamp(fByteBuffer)^.IsNegative;
                      end;
                    end;
      stBoolean, stSmall,
      stInteger, stLong, stLongWord,
      stFloat, stDouble,
      stCurrency, stBigDecimal:  DecodeDateTimeToDate(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
      stAsciiStream, stUnicodeStream: goto from_str;
      stString, stUnicodeString: if FBinaryValues and (TypeOID = INTERVALOID) then begin
                    Months := PG2Integer(P+12);
                    Result.IsNegative := Months < 0;
                    if Months < 0 then
                      Months := -Months;
                    if Months > 12 then begin
                      Result.Year := Cardinal(Months) div 12;
                      Result.Month := Cardinal(Months) mod 12;
                    end else begin
                      Result.Year := 0;
                      Result.Month := Months;
                    end;
                    Days := PG2Integer(P+8);
                    if Days < 0 then begin
                      Result.IsNegative := True;
                      Days := -Days;
                    end;
                    Result.Day := Days;
                  end else goto from_str;
      else raise CreatePGConvertError(ColumnIndex, TypeOID);
    end;
  end;
end;

procedure TZPostgreSQLResultSet.GetTime(ColumnIndex: Integer;
  var Result: TZTime);
var Len: NativeUInt;
    P: PAnsiChar;
    ROW_IDX: Integer;
label from_str, jmpZero;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then goto jmpZero
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    case ColumnType of
      stDate:     goto jmpZero;
      stTime:     if FBinaryValues then begin
                    Result.IsNegative := False;
                    if Finteger_datetimes
                    then PG2Time(PInt64(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                    else PG2Time(PDouble(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions);
                  end else begin
from_str:           Len := StrLen(P);
                    LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, Result);
                    if LastWasNull then goto jmpZero;
                  end;
      stTimestamp:if FBinaryValues then begin
                    Result.IsNegative := False;
                    if Finteger_datetimes
                    then PG2DateTime(PInt64(P)^, PZDate(fByteBuffer)^.Year,
                      PZDate(fByteBuffer)^.Month, PZDate(fByteBuffer)^.Day,
                      Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                    else PG2DateTime(PDouble(P)^, PZDate(fByteBuffer)^.Year,
                      PZDate(fByteBuffer)^.Month, PZDate(fByteBuffer)^.Day,
                      Result.Hour, Result.Minute, Result.Second, Result.Fractions);
                  end else begin
                    Len := StrLen(P);
                    LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings,
                      PZTimeStamp(fByteBuffer)^);
                    if LastWasNull then begin
jmpZero:              PCardinal(@Result.Hour)^ := 0;
                      PInt64(@Result.Second)^ := 0;
                    end else begin
                      Result := PZTime(@PZTimeStamp(fByteBuffer).Hour)^;
                      Result.IsNegative := False;
                    end;
                  end;
      stBoolean, stSmall,
      stInteger, stLong, stLongWord,
      stFloat, stDouble,
      stCurrency, stBigDecimal: DecodeDateTimeToTime(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
      stAsciiStream, stUnicodeStream: goto from_str;
      stString, stUnicodeString: if FBinaryValues and (TypeOID = INTERVALOID) then
                    if Finteger_datetimes
                    then PG2Time(PInt64(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                    else PG2Time(PDouble(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                  else goto from_str;
      else raise CreatePGConvertError(ColumnIndex, TypeOID);
    end
  end;
end;

procedure TZPostgreSQLResultSet.GetTimestamp(ColumnIndex: Integer;
  var Result: TZTimeStamp);
var Len: NativeUInt;
    P: PAnsiChar;
    ROW_IDX: Integer;
    Months: Integer absolute ROW_IDX;
    Days: Integer absolute ROW_IDX;
label from_str, jmpZero;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  if LastWasNull
  then goto jmpZero
  else with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    case ColumnType of
      stDate:     begin
                    if FBinaryValues
                    then PG2Date(Pinteger(P)^, Result.Year, Result.Month, Result.Day)
                    else begin
                      Len := StrLen(P);
                      LastWasNull := not TryPCharToDate(P, Len, ConSettings^.ReadFormatSettings, PZDate(@Result)^);
                      if LastWasNull then
                        goto jmpZero
                    end;
                    PInt64(@Result.Hour)^ := 0;
                    PInt64(PAnsiChar(@Result.Fractions)-2)^ := 0;;
                  end;
      stTime:     begin
                    PInt64(@Result.Year)^ := 0;
                    if FBinaryValues then
                      if Finteger_datetimes
                      then PG2Time(PInt64(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                      else PG2Time(PDouble(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                    else begin
                      Len := StrLen(P);
                      LastWasNull := not TryPCharToTime(P, Len, ConSettings^.ReadFormatSettings, PZTime(@Result.Hour)^);
                      if LastWasNull then goto jmpZero
                    end;
                    PCardinal(@Result.TimeZoneHour)^ := 0;
                    Result.IsNegative := False;
                  end;
      stTimestamp:begin
                  if FBinaryValues then begin
                    if Finteger_datetimes
                    then PG2DateTime(PInt64(P)^, Result.Year,
                      Result.Month, Result.Day, Result.Hour, Result.Minute,
                      Result.Second, Result.Fractions)
                    else PG2DateTime(PDouble(P)^, Result.Year,
                      Result.Month, Result.Day, Result.Hour, Result.Minute,
                      Result.Second, Result.Fractions);
                    PCardinal(@Result.TimeZoneHour)^ := 0;
                    Result.IsNegative := False;
                  end else
from_str:           if TypeOID = INTERVALOID then begin
                    if Finteger_datetimes
                    then PG2Time(PInt64(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions)
                    else PG2Time(PDouble(P)^, Result.Hour, Result.Minute, Result.Second, Result.Fractions);
                    Months := PG2Integer(P+12);
                    Result.IsNegative := Months < 0;
                    if Months < 0 then
                      Months := -Months;
                    if Months > 12 then begin
                      Result.Year := Months div 12;
                      Result.Month := Months mod 12;
                    end else begin
                      Result.Year := 0;
                      Result.Month := Months;
                    end;
                    Days := PG2Integer(P+8);
                    if Days < 0 then begin
                      Result.IsNegative := True;
                      Days := -Days;
                    end;
                    Result.Day := Days;
                  end else begin
                    Len := StrLen(P);
                    LastWasNull := not TryPCharToTimeStamp(P, Len, ConSettings^.ReadFormatSettings, Result);
                    if LastWasNull then begin
jmpZero:              PInt64(@Result.Year)^ := 0;
                      PInt64(@Result.Minute)^ := 0;
                    end;
                  end;
                end;
      stBoolean, stSmall,
      stInteger, stLong, stLongWord,
      stFloat, stDouble,
      stCurrency, stBigDecimal: DecodeDateTimeToTimeStamp(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}), Result);
      stAsciiStream, stUnicodeStream,
      stString, stUnicodeString:    goto from_str;
      else raise CreatePGConvertError(ColumnIndex, TypeOID);
    end
  end;
end;

function TZPostgreSQLResultSet.GetBlob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode = lsmRead): IZBlob;
var P: PAnsiChar;
    Len: NativeUint;
    ROW_IDX: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}

  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  ROW_IDX := PGRowNo;
  LastWasNull := FPlainDriver.PQgetisnull(Fres, ROW_IDX, ColumnIndex) <> 0;
  Result := nil;
  with TZPGColumnInfo(ColumnsInfo[ColumnIndex]) do begin
    P := FPlainDriver.PQgetvalue(Fres, ROW_IDX, ColumnIndex);
    case ColumnType of
      //stGUID:
      stString,
      stUnicodeString,
      stAsciiStream,
      stUnicodeStream:  begin
          Len := ZFastCode.StrLen(P);
          Result := TZAbstractCLob.CreateWithData(P, Len, FClientCP, ConSettings);
        end;
      stBytes,
      stBinaryStream: if (TypeOID = OIDOID) and FIsOidAsBlob then begin
                        if FBinaryValues
                        then Len := PG2Cardinal(P)
                        else Len := RawToUInt64(P);
                        Result := TZPostgreSQLOidBlob.Create(FPGConnection, Len, LobStreamMode, FOpenLobStreams);
                      end else if FBinaryValues then begin
                        Len := FPlainDriver.PQgetlength(Fres, ROW_IDX, ColumnIndex);
                        Result := TZLocalMemBlob.CreateWithData(P, Len )
                      end else if FIs_bytea_output_hex
                      then Result := TZPostgreSQLByteaHexBlob.Create(P)
                      else Result := TZPostgreSQLByteaEscapedBlob.Create(FPlainDriver, P)
        else raise CreatePGConvertError(ColumnIndex, TypeOID);
    end;
  end;
end;

function TZPostgreSQLResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  if FFirstRow then begin
    Fres := FresAddress^; //first row is obtained already
    FFirstRow := False;
    if not FSingleRowMode then begin
      LastRowNo := FPlainDriver.PQntuples(Fres);
      if not LastRowFetchLogged and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
    end;
  end;
  { Checks for maximum row. }
  Result := False;
  if Closed then Exit;

  if (MaxRows > 0) and (Row > MaxRows) then begin
    if (ResultSetType = rtForwardOnly) then
      ResetCursor;
    Exit;
  end;

  { Processes negative rows. }
  if Row < 0 then begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if not FSingleRowMode or (Row = RowNo) then begin
    if (Row >= 0) and (Row <= LastRowNo + 1) then begin
      RowNo := Row;
      Result := (Row >= 1) and (Row <= LastRowNo);
    end else
      Result := False;
    if not Result and FSingleRowMode then
      if Fres <> nil then begin
        FPlainDriver.PQclear(Fres);
        Fres := nil;
      end;
  end else
    raise CreateForwardOnlyException;
end;

function TZPostgreSQLResultSet.Next: Boolean;
var Status: TZPostgreSQLExecStatusType;
label jmpRes;
begin
  Result := False;
  if Closed or ((MaxRows > 0) and (RowNo >= MaxRows)) or (RowNo > LastRowNo) then //previously set by stmt or Next
    Exit;
  if FFirstRow then begin
    Fres := FresAddress^; //first row is obtained already
    FFirstRow := False;
    if FSingleRowMode
    then goto jmpRes
    else LastRowNo := FPlainDriver.PQntuples(Fres);
  end;
  if FSingleRowMode then begin
    if Fres <> nil then begin
      FplainDriver.PQclear(Fres);
      Fres := FPlainDriver.PQgetResult(FconnAddress^);
      FresAddress^ := Fres;
jmpRes:
      Status := FPlainDriver.PQresultStatus(Fres)
    end else Exit;
    if (Status = PGRES_SINGLE_TUPLE) then begin
      RowNo := RowNo + 1;
      LastRowNo := RowNo;
      Result := True;
    end else if Status = PGRES_TUPLES_OK then begin //end of stream
      LastRowNo := RowNo;
      RowNo := RowNo +1;
      FplainDriver.PQclear(Fres);
      Fres := nil;
      FresAddress^ := nil;
    end else
      FPGConnection.HandleErrorOrWarning(Status, lcOther, 'PQgetResult', Self, nil);
  end else begin
    RowNo := RowNo + 1;
    Result := (RowNo <= LastRowNo);
  end;
  if not Result and not LastRowFetchLogged and DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcFetchDone, IZLoggingObject(FWeakIZLoggingObjectPtr));
end;

{ TZPostgreSQLOidBlob }

{**
  Constructs this class and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
  @param Handle a PostgreSQL connection reference.
}
constructor TZPostgreSQLOidBlob.Create(const Connection: IZPostgreSQLConnection;
  BlobOid: Oid; LobStreamMode: TZLobStreamMode; const OpenLobStreams: TZSortedList);
begin
  inherited Create(zCP_Binary, OpenLobStreams);
  FBlobOid := BlobOid;
  FPlainDriver := Connection.GetPlainDriver;
  FOwner := Connection;
  FlobStreamMode := LobStreamMode;
end;

constructor TZPostgreSQLOidBlob.CreateFromBlob(const Value: IZBlob;
  const Connection: IZPostgreSQLConnection; const OpenLobStreams: TZSortedList);
var Stream: TStream;
  P: Pointer;
  L: NativeUint;
  R: RawByteString;
begin
  Create(Connection, 0, lsmWrite, OpenLobStreams);
  Stream := CreateLobStream(zCP_Binary, lsmWrite);
  try
    R := '';
    P := Value.GetBuffer(R, L);
    if (P <> nil) and (L > 0)
    then Stream.Write(P^, L)
    else Stream.Size := 0;
  finally
    Stream.Free;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "CodePage" not used} {$ENDIF}
function TZPostgreSQLOidBlob.CreateLobStream(CodePage: Word;
  LobStreamMode: TZLobStreamMode): TStream;
begin
  FLobStreamMode := LobStreamMode;
  Result := TZPostgreSQLOidBlobStream.Create(Self, LobStreamMode);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the blob handle oid.
  @return the blob handle oid.
}
function TZPostgreSQLOidBlob.GetBlobOid: Oid;
begin
  Result := FBlobOid;
end;

function TZPostgreSQLOidBlob.GetConSettings: PZConSettings;
begin
  Result := FOwner.GetConSettings;
end;

function TZPostgreSQLOidBlob.IsEmpty: Boolean;
begin
  Result := FBlobOid = 0;
end;

function TZPostgreSQLOidBlob.Length: Integer;
var Stream: TStream;
begin
  Result := -1;
  if not IsEmpty then begin
    Stream := GetStream(zCP_Binary);
    try
      Result := Stream.Size;
    finally
      FreeAndNil(Stream);
    end;
  end;
end;

procedure TZPostgreSQLOidBlob.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FBlobOid := 0;
  FOwner.ReleaseImmediat(Sender, AError);
end;

procedure TZPostgreSQLOidBlob.Clear;
begin
  FBlobOid := 0;
end;

function TZPostgreSQLOidBlob.Clone(LobStreamMode: TZLobStreamMode): IZBlob;
var aOID: OID;
begin
  if LobStreamMode = lsmWrite
  then aOID := 0
  else aOID := FBlobOid;
  Self.FLobStreamMode := LobStreamMode;
  Result := TZPostgreSQLOidBlob.Create(FOwner, aOID, LobStreamMode, FOpenLobStreams);
end;

{ TZPostgreSQLByteaBlob }

constructor TZPostgreSQLByteaEscapedBlob.Create(const PlainDriver: TZPostgreSQLPlainDriver;
  Data: PAnsiChar);
var
  to_length: LongWord;
  pgBuffer: Pointer;
begin
  inherited Create;
  pgBuffer := PlainDriver.PQunescapeBytea(Data, @to_length);
  if to_length > 0 then begin
    SetCapacity(to_length);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(pgBuffer^, FDataRefAddress.VarLenData.Data, to_length);
    FDataRefAddress.IsNotNull := 1;
    FDataRefAddress.VarLenData.Len := to_length;
  end;
  PlainDriver.PQFreemem(pgBuffer);
end;

{ TZPostgreSQLByteaHexBlob }

constructor TZPostgreSQLByteaHexBlob.Create(Data: PAnsiChar);
var BinSize: Integer;
begin
  inherited Create;
  {skip trailing /x}
  Inc(Data, 2);
  BinSize := ZFastCode.StrLen(Data) shr 1;
  if BinSize > 0 then begin
    SetCapacity(BinSize);
    HexToBin(Data, {$IFDEF USE_SYNCOMMONS}PAnsiChar{$ENDIF}(@FDataRefAddress.VarLenData.Data), BinSize);
    FDataRefAddress.IsNotNull := 1;
    FDataRefAddress.VarLenData.Len := BinSize;
  end;
end;

{ TZPostgresResultSetMetadata }

procedure TZPostgresResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  //ColumnInfo.ColumnName := '';
end;

{**
  Initializes columns with additional data.
}
procedure TZPostgresResultSetMetadata.LoadColumns;
var
  Current: TZPGColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
  Connection: IZConnection;
  Driver: IZDriver;
  Analyser: IZStatementAnalyser;
  Tokenizer: IZTokenizer;
  PGMetaData: IZPGDatabaseMetadata;
begin
  Connection := Metadata.GetConnection;
  Driver := Connection.GetDriver;
  Analyser := Driver.GetStatementAnalyser;
  Tokenizer := Driver.GetTokenizer;
  PGMetaData := MetaData as IZPGDatabaseMetadata;
  try
    if Analyser.DefineSelectSchemaFromQuery(Tokenizer, SQL) <> nil then
      for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
        Current := TZPGColumnInfo(ResultSet.ColumnsInfo[i]);
        ClearColumn(Current);
        TableColumns := PGMetaData.GetColumnsByTableOID(Current.TableOID);
        if TableColumns <> nil then begin
          TableColumns.BeforeFirst;
          while TableColumns.Next do
            if TableColumns.GetInt(TableColColumnOrdPosIndex) = Current.TableColNo then begin
              FillColumInfoFromGetColumnsRS(Current, TableColumns, TableColumns.GetString(ColumnNameIndex));
              Break;
            end else if TableColumns.GetInt(TableColColumnOrdPosIndex) > Current.TableColNo then
              Break;
        end;
      end;
  finally
    Driver := nil;
    Connection := nil;
    Analyser := nil;
    Tokenizer := nil;
    IdentifierConverter := nil;
    PGMetaData := nil;
  end;
  Loaded := True;
end;

{ TZPostgreSQLCachedResolver }

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZPostgreSQLCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex) in [stUnknown, stBinaryStream]);
end;

{ TZPostgreSQLCachedResolverV74up }

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "OldRowAccessor" not used} {$ENDIF}
procedure TZPostgreSQLCachedResolverV74up.FormWhereClause(
  const SQLWriter: TZSQLStringWriter; const OldRowAccessor: TZRowAccessor;
  var Result: SQLString);
var
  I, Idx: Integer;
  Tmp, S: SQLString;
begin
  if WhereColumns.Count > 0 then
    SQLWriter.AddText(' WHERE ', Result);
  for I := 0 to WhereColumns.Count - 1 do begin
    idx := PZIndexPair(WhereColumns[I]).ColumnIndex;
    if I > 0 then
      SQLWriter.AddText(' AND ', Result);
    S := MetaData.GetColumnName(idx);
    Tmp := IdentifierConverter.Quote(S, iqColumn);
    SQLWriter.AddText(Tmp, Result);
    if (Metadata.IsNullable(idx) = ntNullable)
    then SQLWriter.AddText(' IS NOT DISTINCT FROM ?', Result)
    else SQLWriter.AddText('=?', Result);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZPostgreSQLCachedResolverV8up }

procedure TZPostgreSQLCachedResolverV8up.SetResolverStatementParamters(
  const Statement: IZStatement;
  {$IFDEF AUTOREFCOUNT}const {$ENDIF}Params: TStrings);
begin
  inherited SetResolverStatementParamters(Statement, Params);
  Params.Values[ConnProps_BindDoublesAsString] := 'false';
  Params.Values[DSProps_EmulatePrepares] := 'false';
end;


{ TZPostgresCachedResultSet }

function TZPostgresCachedResultSet.CreateLob(ColumnIndex: Integer;
  LobStreamMode: TZLobStreamMode): IZBlob;
var PostgresResultSet: IZPostgresResultSet;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  with TZPGColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    //EH now we can also create an OIDLobs because we know the col oid !
    //so we can support byteea and oid-lobs here
    //see TZPGColumnInfo().TypeOID
    //if ColumnType is stBinaryStream and TypeOID is OIDOID we assume the streamed lobs
    if (ColumnType = stBinaryStream) and (TypeOID = OIDOID) and
      (ResultSet.QueryInterface(IZPostgresResultSet, PostgresResultSet) = S_OK) then begin
      Result := TZPostgreSQLOidBlob.Create(PostgresResultSet.GetConnection, 0, LobStreamMode, FOpenLobStreams);
      UpdateLob(ColumnIndex, Result);
    end else Result := inherited CreateLob(ColumnIndex, LobStreamMode)
end;

procedure TZPostgresCachedResultSet.FillColumnsInfo(
  const ColumnsInfo: TObjectList);
var PostgresResultSet: IZPostgresResultSet;
begin
  if Supports(ResultSet, IZPostgresResultSet, PostgresResultSet)
  then PostgresResultSet.AssignColumnsInfo(ColumnsInfo)
  else inherited FillColumnsInfo(ColumnsInfo);
end;

class function TZPostgresCachedResultSet.GetRowAccessorClass: TZRowAccessorClass;
begin
  Result := TZPostgreSQLRowAccessor;
end;

{ TZPostgreSQLRowAccessor }

class function TZPostgreSQLRowAccessor.MetadataToAccessorType(
  ColumnInfo: TZColumnInfo; ConSettings: PZConSettings; Var ColumnCodePage: Word): TZSQLType;
begin
  {EH: usuall this code is NOT nessecary if we would handle the types as the
  providers are able to. But in current state we just copy all the incompatibilities
  from the DataSets into dbc... grumble. the only streamed data pg supports are
  oid-lobs. Those we leave as streams, doesn't matter if cached or not, just
  identify purpose}
  Result := ColumnInfo.ColumnType;
  if Result in [stUnicodeString, stUnicodeStream] then begin
    Result := TZSQLType(Byte(Result)-1); // no national chars 4
  end;
  if (Result = stAsciiStream) or ((Result = stBinaryStream) and (TZPGColumnInfo(ColumnInfo).TypeOID <> OIDOID)) then
    Result := TZSQLType(Byte(Result)-3); // no lob streams 4 posgres except OID-BLobs
  if Result = stString then
    ColumnCodePage := ConSettings.ClientCodePage.CP;
end;


{ TZPostgreSQLOidBlobStream }

//https://www.postgresql.org/docs/9.4/lo-examplesect.html

procedure TZPostgreSQLOidBlobStream.BeforeRead;
begin
  FHandle := FOwnerLob.FOwner.GetPGconnAddress^;
  if (FOwnerLob.FBlobOid <> 0) and not FLobIsOpen then
    OpenLob;
end;

procedure TZPostgreSQLOidBlobStream.BeforeWrite;
begin
  FHandle := FOwnerLob.FOwner.GetPGconnAddress^;
  if FLobStreamMode = lsmRead then
    raise EZSQLException.Create(SOperationIsNotAllowed2);
  if FOwnerLob.FBlobOid = 0 then
    CreateLob;
  if not FLobIsOpen then
    OpenLob;
end;

procedure TZPostgreSQLOidBlobStream.CloseLob;
begin
  if not FReleased and (FOwnerLob.FOwner.GetPGconnAddress^ <> nil) and FLobIsOpen then begin
    FLobIsOpen := False;
    if FPlainDriver.lo_close( FHandle, FBlobHandle) <> 0 then
      FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Close Large Object', Self, nil);
  end;
end;

constructor TZPostgreSQLOidBlobStream.Create(const OwnerLob: TZPostgreSQLOidBlob;
  LobStreamMode: TZLobStreamMode);
begin
  inherited Create(OwnerLob, OwnerLob.FOwner, Ownerlob.FOpenLobStreams);
  FplainDriver := OwnerLob.FPlainDriver;
  FLobStreamMode := LobStreamMode;
  FOwnerLob := OwnerLob;
end;


const PGOidLopOpenMode: Array[TZLobStreamMode] of Integer = (INV_READ or INV_WRITE, INV_WRITE, INV_READ or INV_WRITE);
procedure TZPostgreSQLOidBlobStream.CreateLob;
begin
  { Creates a new large object. }
  FOwnerLob.FBlobOid := FPlainDriver.lo_creat(FHandle, PGOidLopOpenMode[FLobStreamMode]);
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
    FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Create Large Object', Self, nil);
end;

destructor TZPostgreSQLOidBlobStream.Destroy;
begin
  if not FReleased and FLobIsOpen then
    CloseLob;
  inherited Destroy;
end;

procedure TZPostgreSQLOidBlobStream.OpenLob;
begin
  { Opens a large object. }
  FBlobHandle := FPlainDriver.lo_open(FHandle, FOwnerLob.FBlobOid, PGOidLopOpenMode[FLobStreamMode]);
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FHandle)) then
    FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Open Large Object', Self, nil);
  FLobIsOpen := True;
end;

function TZPostgreSQLOidBlobStream.Read(var Buffer; Count: Longint): Longint;
var Buf: PAnsiChar;
begin
  if (Count < 0) then
    raise ERangeError.CreateRes(@SRangeError);
  if not FReleased then begin
    BeforeRead;
    if FLobStreamMode = lsmWrite then
      raise CreateWriteOnlyException;
    Buf := @Buffer;
    Result := FPlainDriver.lo_read(FHandle, FBlobHandle, Buf, Count);
    if Result = -1 then
      FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Read Large Object', Self, nil);
  end else Result := 0;
end;

//compiler should optimize it since values are equal but we use them from docs
const PgSeekLocation: array[TSeekOrigin] of integer = (BLOB_SEEK_SET, BLOB_SEEK_CUR, BLOB_SEEK_END);
function TZPostgreSQLOidBlobStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if FReleased or (FOwnerLob.FBlobOid = 0)
  then Result := 0
  else begin
    BeforeRead;
    if Assigned(FPlainDriver.lo_lseek64) then begin
      Result := FPlainDriver.lo_lseek64(FHandle, FBlobHandle, Offset, PgSeekLocation[Origin]);  //since pg9.3
      if Result = -1 then
        FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Seek Large Object', Self, nil);
    end else Result := inherited Seek(OffSet, Origin); //including rangechecks
  end;
end;

procedure TZPostgreSQLOidBlobStream.SetSize(const NewSize: Int64);
var Res: Integer;
begin
  if not FReleased then begin
    BeforeWrite;
    if Size = 0 then begin
     { UnLinks(drops) a large object. }
      if FPlainDriver.lo_unlink(FHandle, FOwnerLob.FBlobOid) <> 0 then
      FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Unlink Large Object', Self, nil);
      FOwnerLob.FBlobOid := 0;
    end else begin
      if Assigned(FPlainDriver.lo_truncate64)
      then Res := FPlainDriver.lo_truncate64(FHandle, FBlobHandle, NewSize)  //sinc pg9.3
      else Res := FPlainDriver.lo_truncate(FHandle, FBlobHandle, NewSize);
      if Res = -1 then
      FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Truncate Large Object', Self, nil);
    end;
    Updated := True;
  end;
end;

function TZPostgreSQLOidBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if FReleased or (FOwnerLob.FBlobOid = 0)
  then Result := 0
  else begin
    BeforeRead;
    Result := FPlainDriver.lo_lseek(FHandle, FBlobHandle, Offset, Origin);
    if Result = -1 then
      FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Seek Large Object', Self, nil);
  end;
end;

function TZPostgreSQLOidBlobStream.Write(const Buffer; Count: Longint): Longint;
var Buf: PAnsiChar;
begin
  if (Count < 0) then
    raise ERangeError.CreateRes(@SRangeError);
  if FLobStreamMode = lsmRead then
    raise EZSQLException.Create(SOperationIsNotAllowed2);
  Buf := @Buffer;
  if not FReleased then begin
    BeforeWrite;
    Result := FPlainDriver.lo_write(FHandle, FBlobHandle, Buf, Count);
    if Result = -1 then
      FOwnerLob.FOwner.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcOther, 'Write Large Object', Self, nil);
    Updated := True;
  end else Result := 0;
end;

{ TZPostgreSQLCachedResolverV10up }

procedure TZPostgreSQLCachedResolverV10up.AfterConstruction;
var
  Fields: string;
begin
  inherited;
  FReturningPairs := TZIndexPairList.Create;
  Fields := Statement.GetParameters.Values[DSProps_InsertReturningFields];
  if Fields <> '' then
    FInsertReturningFields := ExtractFields(Fields, [';', ',']);
end;

procedure TZPostgreSQLCachedResolverV10up.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FReturningPairs);
  FreeAndNil(FInsertReturningFields);
end;

function TZPostgreSQLCachedResolverV10up.FormInsertStatement(
  NewRowAccessor: TZRowAccessor): SQLString;
var
  I, ColumnIndex: Integer;
  Tmp: SQLString;
  SQLWriter: TZSQLStringWriter;
  {$IF DECLARED(DSProps_InsertReturningFields)}
  Fields: TStrings;
  {$IFEND}
begin
  I := MetaData.GetColumnCount;
  SQLWriter := TZSQLStringWriter.Create(512+(I shl 5));
  {$IF DECLARED(DSProps_InsertReturningFields)}
  if FInsertReturningFields <> nil then begin
    Fields := TStringList.Create;
    Fields.Assign(FInsertReturningFields);
  end else Fields := nil;
  {$IFEND}
  Result := 'INSERT INTO ';
  try
    Tmp := DefineTableName;
    SQLWriter.AddText(Tmp, Result);
    SQLWriter.AddChar(' ', Result);
    SQLWriter.AddChar('(', Result);
    if (FInsertStatements.Count > 0) and FHasWritableAutoIncrementColumns then
      FInsertColumns.Clear;
    if (FInsertColumns.Count = 0) then
      FillInsertColumnsPairList(NewRowAccessor);
    if (FInsertColumns.Count = 0) and not
       {test for generated always cols }
       ((Metadata.GetColumnCount > 0) and Metadata.IsAutoIncrement(FirstDbcIndex)) then begin
      Result := '';
      Exit;
    end;
    for I := 0 to FInsertColumns.Count-1 do begin
      ColumnIndex := PZIndexPair(FInsertColumns[i])^.ColumnIndex;
      Tmp := Metadata.GetColumnName(ColumnIndex);
      Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
      SQLWriter.AddText(Tmp, Result);
      SQLWriter.AddChar(',', Result);
    end;
    SQLWriter.ReplaceOrAddLastChar(',', ')', Result);
    SQLWriter.AddText(' VALUES (', Result);
    for I := 0 to FInsertColumns.Count - 1 do begin
      SQLWriter.AddChar('?', Result);
      SQLWriter.AddChar(',', Result);
    end;
    SQLWriter.ReplaceOrAddLastChar(',', ')', Result);
    FReturningPairs.Clear;
    for i := FirstDbcIndex to MetaData.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if Metadata.IsAutoIncrement(I) then begin
        FHasAutoIncrementColumns := True;
        if Metadata.IsWritable(I) then begin
          FHasWritableAutoIncrementColumns := True;
          if not NewRowAccessor.IsNull(I) then
            Continue;
        end;
        if FReturningPairs.Count = 0 then
          SQLWriter.AddText(' RETURNING ', Result);
        FReturningPairs.Add(I, FReturningPairs.Count{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        Tmp := Metadata.GetColumnName(I);
        {$IF DECLARED(DSProps_InsertReturningFields)}
        if (Fields <> nil) then begin
          ColumnIndex := Fields.IndexOf(Tmp);
          if ColumnIndex > -1 then
            Fields.Delete(ColumnIndex); { avoid duplicates }
        end;
        {$IFEND}
        Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
        SQLWriter.AddText(Tmp, Result);
        SQLWriter.AddChar(',', Result);
      end;
    {$IF DECLARED(DSProps_InsertReturningFields)}
    if (Fields <> nil) and (Fields.Count > 0) then begin
      if FReturningPairs.Count = 0 then
        SQLWriter.AddText(' RETURNING ', Result);
      for I := 0 to Fields.Count - 1 do begin
        Tmp := Fields[I];
        ColumnIndex := MetaData.FindColumn(Tmp);
        if ColumnIndex = InvalidDbcIndex then
          raise CreateColumnWasNotFoundException(Tmp);
        FReturningPairs.Add(ColumnIndex, FReturningPairs.Count{$IFNDEF GENERIC_INDEX}+1{$ENDIF});
        Tmp := IdentifierConverter.Quote(Tmp, iqColumn);
        SQLWriter.AddText(Tmp, Result);
        SQLWriter.AddChar(',', Result);
      end;
    end;
    {$IFEND}
    SQLWriter.CancelLastComma(Result);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
    {$IF DECLARED(DSProps_InsertReturningFields)}
    FreeAndNil(Fields);
    {$IFEND}
  end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZPostgreSQLCachedResolverV10up.PostUpdates(
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
  const OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);
  if (UpdateType = utInserted) then begin
    if (FReturningPairs.Count >0) then
      UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
    if FHasWritableAutoIncrementColumns then
      InsertStatement := nil;
  end;
end;

{**
 Do Tasks after Post updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
procedure TZPostgreSQLCachedResolverV10up.UpdateAutoIncrementFields(
  const Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
  const OldRowAccessor, NewRowAccessor: TZRowAccessor;
  const Resolver: IZCachedResolver);
var
  I: Integer;
  RS: IZResultSet;
begin
  RS := InsertStatement.GetResultSet;
  if (RS <> nil) then try
    if RS.Next then
      for i := 0 to FReturningPairs.Count -1 do with PZIndexPair(FReturningPairs[I])^ do
        NewRowAccessor.SetValue(SrcOrDestIndex, RS.GetValue(ColumnIndex));
  finally
    RS.Close; { Without Close RS keeps circular ref to Statement causing mem leak }
    RS := nil;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

initialization
{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.

