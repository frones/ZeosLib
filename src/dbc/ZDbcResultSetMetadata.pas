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

unit ZDbcResultSetMetadata;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, {$IFNDEF NO_UNIT_CONTNRS}Contnrs, {$ENDIF}
  ZDbcIntfs, ZCollections,
  {$IF defined(OLDFPC) or defined (NO_UNIT_CONTNRS)}ZClasses,{$IFEND}
  ZGenericSqlAnalyser,
  ZTokenizer, ZSelectSchema, ZCompatibility, ZDbcResultSet;

type

  {** Implements a column information structure. }
  PZColumnInfo = ^TZColumnInfo;
  TZColumnInfo = class(TObject)
  public
    AutoIncrement: Boolean;
    CaseSensitive: Boolean;
    Searchable, SearchableDisabled: Boolean;
    Currency: Boolean; //note we'll map all fixed numbers to stCurrency(ftBCD)
                        //if Scale&Precision allows it. But if a field is a true
                        //currency field like MS/PG-Money should be indicated here
    Nullable: TZColumnNullableType;
    Signed: Boolean; //signed ordinals or fixed with datatype?
    CharOctedLength: Integer;
    ColumnLabel: string;
    ColumnName: string;
    SchemaName: string;
    Precision: Integer;
    Scale: Integer;
    TableName: string;
    CatalogName: string;
    ColumnType: TZSQLType;
    ReadOnly: Boolean;
    Writable: Boolean;
    DefinitelyWritable: Boolean;
    DefaultValue: string;
    DefaultExpression : string;
    ColumnCodePage: Word;
    constructor Create;
    function GetColumnTypeName: string;
  end;

  {** Implements Abstract ResultSet Metadata. }
  TZAbstractResultSetMetadata = class(TContainedObject, IZResultSetMetaData)
  private
    FLoaded: Boolean;
    FMetadata: IZDatabaseMetadata;
    FColumnsLabelsCS, //a case sensitive list of unique column labels
    FColumnsLabelsCI: TStrings;  //a lower case list of unique column labels if still duplicate values exist
    FSQL: string;
    FTableColumns: TZHashMap;
    FIdentifierConverter: IZIdentifierConverter;
    FResultSet: TZAbstractResultSet;
  protected
    FConSettings: PZConSettings;
    procedure SetAutoIncrementFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetCaseSensitiveFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnCodePageFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnNullableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetDefaultValueFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetDefinitelyWritableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetIsSearchableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnPrecisionFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetReadOnlyFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnScaleFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnTypeFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetWritableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure LoadColumn(ColumnIndex: Integer; ColumnInfo: TZColumnInfo;
      const SelectSchema: IZSelectSchema); virtual;
  protected
    procedure FillColumInfoFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet; const FieldName: String);
    function GetTableColumns(TableRef: TZTableRef): IZResultSet;
    function ReadColumnByRef(FieldRef: TZFieldRef; ColumnInfo: TZColumnInfo): Boolean;
    function ReadColumnByName(const FieldName: string; TableRef: TZTableRef;
      ColumnInfo: TZColumnInfo): Boolean;
    /// <summary>Clears specified column information.</summary>
    /// <param>"ColumnInfo" a column information object.</param>
    procedure ClearColumn(ColumnInfo: TZColumnInfo); virtual;
    /// <summary>Initializes columns with additional data.</summary>
    procedure LoadColumns; virtual;
    procedure ReplaceStarColumns(const SelectSchema: IZSelectSchema);
  public
    /// <summary>Sets the databasemetadata to this object.</summary>
    /// <param>"Value" a new IZDatabaseMetadata interface</param>
    procedure SetMetadata(const Value: IZDatabaseMetadata);
  protected
    property MetaData: IZDatabaseMetadata read FMetadata write SetMetadata;
    property ColumnsLabels: TStrings read FColumnsLabelsCS write FColumnsLabelsCS;
    property SQL: string read FSQL write FSQL;
    property IdentifierConverter: IZIdentifierConverter
      read FIdentifierConverter write FIdentifierConverter;
    property Loaded: Boolean read FLoaded write FLoaded;
    property ResultSet: TZAbstractResultSet read FResultSet write FResultSet;
  public
    constructor Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
    destructor Destroy; override;
    /// <summary>Maps the given <c>Metadata</c> column name to its
    ///  <c>Metadata</c> column index. First searches with case-sensivity then,
    ///  if nothing matches, a case.insensitive search is performed.
    /// <param>"ColumnName" the name of the column</param>
    /// <returns>the column index of the given column name or an
    ///  InvalidDbcIndex if nothing was found</returns>
    function FindColumn(const ColumnName: string): Integer;
    /// <summary>get the number of columns in this <c>ResultSet</c> interface.</summary>
    /// <returns>the number of columns</returns>
    function GetColumnCount: Integer;
    /// <author>EgonHugeist</author>
    /// <summary>Indicates whether the metainformations are loaded.</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsMetadataLoaded: Boolean;
    /// <author>EgonHugeist</author>
    procedure AssignColumnInfosTo(Dest: TObjectList);
    /// <author>EgonHugeist</author>
    /// <summary>Sets the metainformations are loaded by value.</summary>
    /// <param><c>true</c> if indicate they are loaded; <c>false</c> otherwise</param>
    procedure SetMetadataLoaded(Value: Boolean);
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
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Indicates whether a column's case matters.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsCaseSensitive(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Indicates whether the designated column can be used in a where
    ///  clause.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsSearchable(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Set if the column can be used in a where
    ///  clause.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Value" <c>true</c> if the column is searchable;
    ///  <c>False</c> otherwise.</param>
    procedure SetSearchable(ColumnIndex: Integer; Value: Boolean);
    /// <summary>Indicates whether the designated column is a cash value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsCurrency(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Indicates the nullability of values in the designated column.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the nullability status of the given column; one of
    ///  <c>ntNoNulls</c>, <c>ntNullable</c> or <c>ntNullableUnknown</c></returns>
    function IsNullable(ColumnIndex: Integer): TZColumnNullableType; virtual;
    /// <summary>Indicates whether values in the designated column are signed
    ///  numbers.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsSigned(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Gets the designated column's suggested title for use in
    ///  printouts and displays.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>a case sensitive unique column title.</returns>
    function GetColumnLabel(ColumnIndex: Integer): string; virtual;
    /// <summary>Gets the designated column's original title for use in
    ///  printouts and displays returned by the server.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the server given column title.</returns>
    function GetOrgColumnLabel(ColumnIndex: Integer): string; virtual;
    /// <summary>Get the designated column's name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the column name.</returns>
    function GetColumnName(ColumnIndex: Integer): string; virtual;
    /// <summary>Get the designated column's codepage.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the column codepage.</returns>
    /// <remarks>If the column is a [var,long,fixed]binary the returned value is
    ///  zero. If the column is a text/character column the returned value is
    ///  depends to the connection characterset or if the Charset is vairable
    ///  like IB/FB characterset "NONE" it's the column-characterset. Otherwise
    ///  the value is High(Word) and indicates a zCP_NONE codepage. See
    ///  ZEncoding.pas.</remarks>
    function GetColumnCodePage(ColumnIndex: Integer): Word;
    /// <summary>Get the designated column's table's schema.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>schema name or "" if not applicable</returns>
    function GetSchemaName(ColumnIndex: Integer): string; virtual;
    /// <summary>Get the designated column's number of decimal digits for
    ///  numeric or decimal types or or the number of bytes for binary columns
    ///  or the number of display characters any other column type</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>precision/bytes/visible characters</returns>
    function GetPrecision(ColumnIndex: Integer): Integer; virtual;
    /// <summary>Gets the designated column's table's character octed length.
    ///  This is count of bytes for a buffer to store the data. This may depend
    ///  to DB's character set or true UFT16 vs Raw encoded strings</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>char octed length or 0 if not applicable.</returns>
    function GetCharOctedLength(ColumnIndex: Integer): Integer; virtual;
    /// <summary>Gets the designated column's number of digits to right of the
    ///  decimal point for Numeric or Decimal types or the second fractions for
    ///  time/timestamp types or the minimum chars/bytes for fixed
    ///  binary/char/nchar columns, zero otherwise.
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>scale</returns>
    function GetScale(ColumnIndex: Integer): Integer; virtual;
    /// <summary>Gets the designated column's table name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>table name or "" if not applicable.</returns>
    function GetTableName(ColumnIndex: Integer): string; virtual;
    /// <summary>Gets the designated column's catalog name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>catalog name or "" if not applicable.</returns>
    function GetCatalogName(ColumnIndex: Integer): string; virtual;
    /// <summary>Retrieves the designated column's SQL type.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>the ZDBC SQL type</returns>
    function GetColumnType(ColumnIndex: Integer): TZSQLType; virtual;
    /// <summary>Retrieves the designated column's database-specific type name.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>type name used by the database or "" if not applicable. If the
    ///  column type is a user-defined type, then a fully-qualified type name is
    ///  returned.</returns>
    function GetColumnTypeName(ColumnIndex: Integer): string; virtual;
    /// <summary>Indicates whether the designated column is definitely not
    ///  writable.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsReadOnly(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Set the readonly state of a field. The value will be ignored
    ///  if the field is not writable.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <param>"Value" if <c>true</c> then the field will be ignored on
    ///  generating the dml's.</param>
    procedure SetReadOnly(ColumnIndex: Integer; Value: Boolean); virtual;
    /// <summary>Indicates whether it is possible for a write on the designated
    ///  column to succeed.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsWritable(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Indicates whether a write on the designated column will
    ///  definitely succeed.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function IsDefinitelyWritable(ColumnIndex: Integer): Boolean; virtual;
    /// <summary>Gets a default value for this field.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns>a default value for this field.</returns>
    function GetDefaultValue(ColumnIndex: Integer): string; virtual;
    /// <summary>Finds whether this field has a default value.</summary>
    /// <param>"ColumnIndex" the first Column is 1, the second is 2, ... unless
    ///  <c>GENERIC_INDEX</c> is defined. Then the first column is 0, the second
    ///  is 1. This will change in future to a zero based index. It's recommented
    ///  to use an incrementation of FirstDbcIndex. <c>Note</c> the cursor must
    ///  be on a valid position and the Index must be valid. Otherwise the
    ///  results may be unexpected. See traversal/positioning method's like
    ///  <c>IsBeforeFirst</c>,<c>Next()</c>,<c>IsAfterLast</c>...</param>
    /// <returns><c>true</c> if this field has a default value; <c>false</c>
    ///  otherwise.</returns>
    function HasDefaultValue(ColumnIndex: Integer): Boolean; virtual;
  end;

implementation

uses ZFastCode, ZVariant, ZDbcUtils, ZDbcMetadata, ZSysUtils, ZEncoding;

{ TZColumnInfo }

{**
  Constructs this object and assigns main properties.
}
constructor TZColumnInfo.Create;
begin
  AutoIncrement := False;
  Nullable := ntNullableUnknown;
  ReadOnly := True;
  ColumnCodePage := zCP_NONE;
end;

{**
  Retrieves the designated column's database-specific type name.
  @return type name used by the database. If the column type is
    a user-defined type, then a fully-qualified type name is returned.
}
function TZColumnInfo.GetColumnTypeName: string;
begin
  Result := DefineColumnTypeName(ColumnType);
end;

{ TZAbstractResultSetMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Metadata a database metadata object.
  @param SQL an SQL query statement.
  @param ColumnsInfo a collection of columns info.
}
constructor TZAbstractResultSetMetadata.Create(const Metadata: IZDatabaseMetadata;
  const SQL: string; ParentResultSet: TZAbstractResultSet);
begin
  inherited Create(ParentResultSet);

  SetMetadata(Metadata);
  FSQL := SQL;
  FLoaded := not ((FMetadata <> nil) and FMetadata.GetConnection.UseMetadata);
  FTableColumns := TZHashMap.Create;
  FResultSet := ParentResultSet;

  FConSettings := FResultSet.GetConSettings;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractResultSetMetadata.Destroy;
begin
  FIdentifierConverter := nil;
  FMetadata := nil;
  FreeAndNil(FTableColumns);
  FreeAndNil(FColumnsLabelsCS);
  FreeAndNil(FColumnsLabelsCI);
  inherited Destroy;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "FieldName" not used}
{$ENDIF}
procedure TZAbstractResultSetMetadata.FillColumInfoFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo;
  const TableColumns: IZResultSet; const FieldName: String);
begin
  ColumnInfo.CatalogName := TableColumns.GetString(CatalogNameIndex);
  ColumnInfo.SchemaName := TableColumns.GetString(SchemaNameIndex);
  ColumnInfo.TableName := TableColumns.GetString(TableNameIndex);
  ColumnInfo.ColumnName := TableColumns.GetString(ColumnNameIndex);;

  if FConSettings = nil then //fix if on creation nil was assigned
    FConSettings := ResultSet.GetStatement.GetConnection.GetConSettings;
  { Overwrite ColumnTypes if necessary }
  SetColumnTypeFromGetColumnsRS(ColumnInfo, TableColumns);
  {Assign ColumnCodePages if necessary }
  SetColumnCodePageFromGetColumnsRS(ColumnInfo, TableColumns);
  {Precision or column-size}
  SetColumnPrecisionFromGetColumnsRS(ColumnInfo, TableColumns);
  {Scale}
  SetColumnScaleFromGetColumnsRS(ColumnInfo, TableColumns);
  {nullable}
  SetColumnNullableFromGetColumnsRS(ColumnInfo, TableColumns);
  {auto increment field}
  SetAutoIncrementFromGetColumnsRS(ColumnInfo, TableColumns);
  {Case sensitive}
  SetCaseSensitiveFromGetColumnsRS(ColumnInfo, TableColumns);
  {Is searchable}
  SetIsSearchableFromGetColumnsRS(ColumnInfo, TableColumns);
  {Writable}
  SetWritableFromGetColumnsRS(ColumnInfo, TableColumns);
  {DefinitelyWritable}
  SetDefinitelyWritableFromGetColumnsRS(ColumnInfo, TableColumns);
  {readonly}
  SetReadOnlyFromGetColumnsRS(ColumnInfo, TableColumns);
  {default value}
  SetDefaultValueFromGetColumnsRS(ColumnInfo, TableColumns);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractResultSetMetadata.FindColumn(const ColumnName: string): Integer;
var
  I: Integer;
  ColumnNameLower: string;
begin
  { Search for case sensitive columns. }
  for I := FirstDbcIndex to GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if GetColumnLabel(I) = ColumnName then begin
      Result := I;
      Exit;
    end;
  { Search for case insensitive columns. }
  ColumnNameLower := AnsiLowerCase(ColumnName);
  if FColumnsLabelsCI <> nil then begin//alive only if caseinsensitive duplicates exist (to find fields of DataSetLogic)
    Result := FColumnsLabelsCI.IndexOf(ColumnNameLower);
    if Result > -1 then begin
      {$IFNDEF GENERIC_INDEX}Inc(Result);{$ENDIF}
      Exit;
    end;
  end else for I := FirstDbcIndex to GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if AnsiLowerCase(GetColumnLabel(I)) = ColumnNameLower then
    begin
      Result := I;
      Exit;
    end;

  Result := InvalidDbcIndex;
end;

{**
  Returns the number of columns in this <code>ResultSet</code> object.
  @return the number of columns
}
function TZAbstractResultSetMetadata.GetColumnCount: Integer;
begin
  Result := FResultSet.ColumnsInfo.Count;
end;

function TZAbstractResultSetMetadata.IsAutoIncrement(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).AutoIncrement;
end;

function TZAbstractResultSetMetadata.IsCaseSensitive(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).CaseSensitive;
end;

function TZAbstractResultSetMetadata.IsSearchable(ColumnIndex: Integer): Boolean;
begin
  with TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    if SearchableDisabled then
      Result := False
    else begin
      if not Loaded then
        LoadColumns;
      Result := Searchable;
    end;
end;

function TZAbstractResultSetMetadata.IsCurrency(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Currency;
end;

function TZAbstractResultSetMetadata.IsNullable(
  ColumnIndex: Integer): TZColumnNullableType;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Nullable;
end;

function TZAbstractResultSetMetadata.IsSigned(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Signed;
end;

function TZAbstractResultSetMetadata.GetColumnLabel(ColumnIndex: Integer): string;
  procedure FillListAndMakeUnique;
  var
    I, J, N: Integer;
    ColumnName, OrgLabel: string;
    ColumnsInfo: TObjectList;
    B, HasLowerLabelDuplicates: Boolean;
  begin
    ColumnsInfo := FResultSet.ColumnsInfo;
    FColumnsLabelsCS := TStringList.Create;
    FColumnsLabelsCI := TStringList.Create;
    HasLowerLabelDuplicates := False;
    { fills a case sensitive unique list }
    for I := 0 to ColumnsInfo.Count - 1 do begin
      N := 0;
      ColumnName := TZColumnInfo(ColumnsInfo[I]).ColumnLabel;
      if ColumnName = '' then
        ColumnName := 'Column';
      OrgLabel := ColumnName;
      Repeat
        //see test TestDuplicateColumnNames or
        //https://zeoslib.sourceforge.io/viewtopic.php?f=50&t=120692
        b := False;
        for J := 0 to I - 1 do
          if TZColumnInfo(ColumnsInfo[J]).ColumnLabel = ColumnName then
            Begin
             Inc(N);
             b := True;
            End;
        if N > 0 then
          ColumnName := OrgLabel + '_' + ZFastCode.IntToStr(N);
      Until Not b;
      FColumnsLabelsCS.Add(ColumnName);
    end;
    { fills a case insensitive unique list }
    for I := 0 to FColumnsLabelsCS.Count - 1 do begin
      N := 0;
      OrgLabel := FColumnsLabelsCS[I];
      ColumnName := AnsiLowerCase(OrgLabel);
      while FColumnsLabelsCI.IndexOf(ColumnName) > -1 do begin
        Inc(N);
        ColumnName := OrgLabel + '_' + ZFastCode.IntToStr(N);
        HasLowerLabelDuplicates := True;
      end;
      FColumnsLabelsCI.Add(ColumnName);
    end;
    if not HasLowerLabelDuplicates then
      FreeAndNil(FColumnsLabelsCI);
  end;
begin
  { Prepare unique column labels. }
  if FColumnsLabelsCS = nil then
    FillListAndMakeUnique;
  Result := ColumnsLabels[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}];
end;

function TZAbstractResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnName;
end;

function TZAbstractResultSetMetadata.GetColumnCodePage(ColumnIndex: Integer): Word;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage;
end;

function TZAbstractResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).SchemaName;
end;

function TZAbstractResultSetMetadata.GetPrecision(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Precision;
end;

function TZAbstractResultSetMetadata.GetScale(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Scale;
end;

function TZAbstractResultSetMetadata.GetTableName(ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

function TZAbstractResultSetMetadata.GetCatalogName(ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).CatalogName;
end;

function TZAbstractResultSetMetadata.GetCharOctedLength(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).CharOctedLength;
end;

function TZAbstractResultSetMetadata.GetColumnType(ColumnIndex: Integer): TZSQLType;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType;
end;

function TZAbstractResultSetMetadata.GetColumnTypeName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).GetColumnTypeName;
end;

function TZAbstractResultSetMetadata.IsReadOnly(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ReadOnly;
end;

function TZAbstractResultSetMetadata.IsWritable(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Writable;
end;

function TZAbstractResultSetMetadata.IsDefinitelyWritable(
  ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).DefinitelyWritable;
end;

function TZAbstractResultSetMetadata.IsMetadataLoaded: Boolean;
begin
  Result := Loaded;
end;

function TZAbstractResultSetMetadata.GetDefaultValue(
  ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).DefaultValue;
end;

function TZAbstractResultSetMetadata.GetOrgColumnLabel(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnLabel;
end;

function TZAbstractResultSetMetadata.HasDefaultValue(
  ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  // '' = NULL / no default value, '''''' = empty string (''), etc.
  Result := not(TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).DefaultValue = '');
end;

{**
  Gets a table description result set.
  @param TableRef a table reference object.
  @return a result set with table columns from database metadata.
}
function TZAbstractResultSetMetadata.GetTableColumns(
  TableRef: TZTableRef): IZResultSet;
var
  TableKey: IZAnyValue;
begin
  TableKey := TZAnyValue.CreateWithString(TableRef.FullName);
  if FTableColumns.Get(TableKey) = nil then
  begin
    Result := Metadata.GetColumns(TableRef.Catalog,
      TableRef.Schema, FResultSet.GetStatement.GetConnection.GetMetadata.AddEscapeCharToWildcards(TableRef.Table), '');
    FTableColumns.Put(TableKey, Result);
  end else
    Result := FTableColumns.Get(TableKey) as IZResultSet;
end;

procedure TZAbstractResultSetMetadata.AssignColumnInfosTo(
  Dest: TObjectList);
var I: Integer;
  ACopy: TZColumnInfo;
begin
  Dest.Clear;
  Dest.Capacity := FResultSet.ColumnsInfo.Count;
  for i := 0 to FResultSet.ColumnsInfo.Count -1 do begin
    ACopy := TZColumnInfo.Create;
    Dest.Add(ACopy);
    ACopy.AutoIncrement := TZColumnInfo(FResultSet.ColumnsInfo[i]).AutoIncrement;
    ACopy.CaseSensitive := TZColumnInfo(FResultSet.ColumnsInfo[i]).CaseSensitive;
    ACopy.Searchable := TZColumnInfo(FResultSet.ColumnsInfo[i]).Searchable;
    ACopy.SearchableDisabled := TZColumnInfo(FResultSet.ColumnsInfo[i]).SearchableDisabled;
    ACopy.Currency := TZColumnInfo(FResultSet.ColumnsInfo[i]).Currency;
    ACopy.Nullable := TZColumnInfo(FResultSet.ColumnsInfo[i]).Nullable;
    ACopy.Signed := TZColumnInfo(FResultSet.ColumnsInfo[i]).Signed;
    ACopy.CharOctedLength := TZColumnInfo(FResultSet.ColumnsInfo[i]).CharOctedLength;
    ACopy.ColumnLabel := TZColumnInfo(FResultSet.ColumnsInfo[i]).ColumnLabel;
    ACopy.ColumnName := TZColumnInfo(FResultSet.ColumnsInfo[i]).ColumnName;
    ACopy.SchemaName := TZColumnInfo(FResultSet.ColumnsInfo[i]).SchemaName;
    ACopy.Precision := TZColumnInfo(FResultSet.ColumnsInfo[i]).Precision;
    ACopy.Scale := TZColumnInfo(FResultSet.ColumnsInfo[i]).Scale;
    ACopy.TableName := TZColumnInfo(FResultSet.ColumnsInfo[i]).TableName;
    ACopy.CatalogName := TZColumnInfo(FResultSet.ColumnsInfo[i]).CatalogName;
    ACopy.ColumnType := TZColumnInfo(FResultSet.ColumnsInfo[i]).ColumnType;
    ACopy.ReadOnly := TZColumnInfo(FResultSet.ColumnsInfo[i]).ReadOnly;
    ACopy.Writable := TZColumnInfo(FResultSet.ColumnsInfo[i]).Writable;
    ACopy.DefinitelyWritable := TZColumnInfo(FResultSet.ColumnsInfo[i]).DefinitelyWritable;
    ACopy.DefaultValue := TZColumnInfo(FResultSet.ColumnsInfo[i]).DefaultValue;
    ACopy.DefaultExpression := TZColumnInfo(FResultSet.ColumnsInfo[i]).DefaultExpression;
    ACopy.ColumnCodePage := TZColumnInfo(FResultSet.ColumnsInfo[i]).ColumnCodePage;
  end;
end;

procedure TZAbstractResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  ColumnInfo.ColumnName := '';
end;

{**
  Reads a column information from table metadata.
  @param FieldName a name of the field.
  @param TableRef a table reference object.
  @param ColumnInfo a column information object.
  @return <code>True</code> is column was found and read.
}
function TZAbstractResultSetMetadata.ReadColumnByName(const FieldName: string;
  TableRef: TZTableRef; ColumnInfo: TZColumnInfo): Boolean;
var
  TableColumns: IZResultSet;
  Catalog, UpCatalog, Schema, UpSchema, UpField: String;
begin
  Result := False;
  if (FieldName = '') then
    Exit;
  TableColumns := GetTableColumns(TableRef);
  { Checks for unexisted table. }
  if not Assigned(TableColumns) then
    Exit;
  Catalog := TableRef.Catalog;
  Schema  := TableRef.Schema;
  with FMetadata.GetDatabaseInfo do
    if SupportsCatalogsInDataManipulation and (TableRef.Catalog = '') then
      Catalog := FMetadata.GetConnection.GetCatalog
    else if SupportsSchemasInDataManipulation and (TableRef.Schema = '') and (TableRef.Catalog = '') then
      Schema := FMetadata.GetConnection.GetCatalog;

  { Locates a column row casesensitive. }
  TableColumns.BeforeFirst;
  while TableColumns.Next do
    if TableColumns.GetString(ColumnNameIndex) = FieldName then begin
      if (Catalog = '') or (TableColumns.GetString(CatalogNameIndex) = Catalog) then
        if (Schema = '') or (TableColumns.GetString(SchemaNameIndex) = Schema) then
          Break;
    end;
  if TableColumns.IsAfterLast then begin
    UpField := AnsiUpperCase(FieldName);
    UpCatalog := AnsiUpperCase(Catalog);
    UpSchema := AnsiUpperCase(Schema);
    { Locates a column row with case insensitivity. }
    TableColumns.BeforeFirst;
    while TableColumns.Next do
      if AnsiUpperCase(TableColumns.GetString(ColumnNameIndex)) = UpField then begin
        if (Catalog = '') or (TableColumns.GetString(CatalogNameIndex) = Catalog) or
           (AnsiUpperCase(TableColumns.GetString(CatalogNameIndex)) = UpCatalog) then
          if (Schema = '') or (TableColumns.GetString(SchemaNameIndex) = Schema) or
             (AnsiUpperCase(TableColumns.GetString(SchemaNameIndex)) = UpSchema) then
            Break;
    end;
    if TableColumns.IsAfterLast then
      Exit;
  end;

  { Reads a column information. }
  Result := True;
  FillColumInfoFromGetColumnsRS(ColumnInfo, TableColumns, FieldName);
end;

{**
  Reads a column information from table metadata.
  @param FieldRef a field reference object.
  @param ColumnInfo a column information object.
  @return <code>True</code> if column was found and read.
}
function TZAbstractResultSetMetadata.ReadColumnByRef(
  FieldRef: TZFieldRef; ColumnInfo: TZColumnInfo): Boolean;
begin
  Result := False;
  ClearColumn(ColumnInfo);
  { Checks for uncompleted field reference. }
  if not Assigned(FieldRef) or not Assigned(FieldRef.TableRef) then
    Exit;
  if not FieldRef.IsField then
    Exit;

  Result := ReadColumnByName(FieldRef.Field, FieldRef.TableRef, ColumnInfo);
end;

{**
  Initializes on single column of the result set.
  @param ColumnIndex a column index in the query.
  @param ColumnInfo a column information object to be initialized.
  @param SelectSchema a schema of the select statement.
}
procedure TZAbstractResultSetMetadata.LoadColumn(ColumnIndex: Integer;
  ColumnInfo: TZColumnInfo; const SelectSchema: IZSelectSchema);
var
  I: Integer;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  Found: Boolean;
  AName: String;
begin
  { Initializes single columns with specified table. }
  FieldRef := SelectSchema.LinkFieldByIndexAndShortName(ColumnIndex,
    ColumnInfo.ColumnLabel, IdentifierConverter);
  if ReadColumnByRef(FieldRef, ColumnInfo) then //else double processing down below
    Exit;
 //EH commented: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=71516&start=15
 // if ColumnInfo.ColumnName <> '' then
   // Exit;
  if Assigned(FieldRef) and not FieldRef.IsField then
    Exit;
  { Initializes single columns without specified table. }
  I := 0;
  Found := False;
  while {(ColumnInfo.ColumnName = '') and }(I < SelectSchema.TableCount) and not Found do begin
    TableRef := SelectSchema.Tables[I];
    if Assigned(FieldRef)
    then AName := IdentifierConverter.ExtractQuote(FieldRef.Field)
    else AName := IdentifierConverter.ExtractQuote(ColumnInfo.ColumnLabel);
    Found := ReadColumnByName(AName, TableRef, ColumnInfo);
    Inc(I);
  end;
end;

{**
  Replaces '*' columns in the select schema.
  @param SelectSchema a query select schema.
}
procedure TZAbstractResultSetMetadata.ReplaceStarColumns(
  const SelectSchema: IZSelectSchema);
var
  I: Integer;
  Current: TZFieldRef;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  ResultSet: IZResultSet;
begin
  I := 0;
  while I < SelectSchema.FieldCount do begin
    Current := SelectSchema.Fields[I];
    if (Current.Field = '*') and (Current.TableRef <> nil) then begin
      TableRef := Current.TableRef;
      ResultSet := GetTableColumns(TableRef);
      if ResultSet <> nil then begin
        ResultSet.BeforeFirst;
        while ResultSet.Next do begin
          FieldRef := TZFieldRef.Create(True, TableRef.Catalog, TableRef.Schema,
            TableRef.Table, ResultSet.GetString(ColumnNameIndex), '', TableRef);
          SelectSchema.InsertField(I, FieldRef);
          Inc(I);
        end;
      end;
      SelectSchema.DeleteField(Current);
      Dec(I);
    end;
    Inc(I);
  end;
end;

procedure TZAbstractResultSetMetadata.SetAutoIncrementFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnAutoIncIndex) then begin
    ColumnInfo.AutoIncrement := TableColumns.GetBoolean(TableColColumnAutoIncIndex);
  end;
end;

procedure TZAbstractResultSetMetadata.SetCaseSensitiveFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnCaseSensitiveIndex) then
    ColumnInfo.CaseSensitive := TableColumns.GetBoolean(TableColColumnCaseSensitiveIndex);
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "TableColumns" not used}
{$ENDIF}
procedure TZAbstractResultSetMetadata.SetColumnCodePageFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}


procedure TZAbstractResultSetMetadata.SetColumnNullableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnNullableIndex) then
    ColumnInfo.Nullable := TZColumnNullableType(TableColumns.GetInt(TableColColumnNullableIndex));
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "ColumnIndex" not used}
{$ENDIF}
procedure TZAbstractResultSetMetadata.SetColumnPrecisionFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  //it's a nop -> use a Override if necessary
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "ColumnIndex/TableColumns" not used}
{$ENDIF}
procedure TZAbstractResultSetMetadata.SetColumnScaleFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF} ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  //it's a nop -> use a Override if necessary
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractResultSetMetadata.SetColumnTypeFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo;
  const TableColumns: IZResultSet);
var tempColType: TZSQLType;
begin
//If the returned column information is null or is unknown then the value assigned during
//the resultset.open will be kept
  if not TableColumns.IsNull(TableColColumnTypeIndex) and (TableColumns.GetSmall(TableColColumnTypeIndex) > Ord(stUnknown)) then begin
    //since Pointer referencing by RowAccessor we've a pointer and GetBlob
    //raises an exception if the pointer is a reference to PPAnsiChar or
    //ZPPWideChar. if we execute a cast of a lob field the database meta-informtions
    //assume a IZLob-Pointer. So let's prevent this case and check for
    //stByte, stString, stUnicoeString first. If this type is returned from the
    //ResultSet-Metadata we do NOT overwrite the column-type
    //f.e. select cast( name as varchar(100)), cast(setting as varchar(100)) from pg_settings

    //or the same vice versa:
    //(CASE WHEN (Ticket51_B."Name" IS NOT NULL) THEN Ticket51_B."Name" ELSE 'Empty' END) As "Name"
    //we've NO fixed length for a case(postgres and FB2.5up f.e.) select
    tempColType := TZSQLType(TableColumns.GetSmall(TableColColumnTypeIndex));
    if not (tempColType in [stBinaryStream, stAsciiStream,
        stUnicodeStream, stBytes, stString, stUnicodeString]) or (ColumnInfo.ColumnType = stUnknown) then
      ColumnInfo.ColumnType := tempColType;
  end;
end;

procedure TZAbstractResultSetMetadata.SetDefaultValueFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnColDefIndex) then
    ColumnInfo.DefaultValue := TableColumns.GetString(TableColColumnColDefIndex);
end;

procedure TZAbstractResultSetMetadata.SetDefinitelyWritableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnDefinitelyWritableIndex) then
    if ColumnInfo.AutoIncrement and Assigned(FMetadata) then {improve ADO where the metainformations do not bring autoincremental fields through}
      if FMetadata.GetDatabaseInfo.SupportsUpdateAutoIncrementFields then
        ColumnInfo.DefinitelyWritable := TableColumns.GetBoolean(TableColColumnDefinitelyWritableIndex)
      else
        ColumnInfo.DefinitelyWritable := False
    else
      ColumnInfo.DefinitelyWritable := TableColumns.GetBoolean(TableColColumnDefinitelyWritableIndex);
end;

procedure TZAbstractResultSetMetadata.SetIsSearchableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnSearchableIndex) then
    ColumnInfo.Searchable := TableColumns.GetBoolean(TableColColumnSearchableIndex);
end;

procedure TZAbstractResultSetMetadata.SetMetadata(
  const Value: IZDatabaseMetadata);
begin
  FMetadata := Value;
  if Value<>nil then
    FIdentifierConverter := Value.GetIdentifierConverter
  else
    FIdentifierConverter := TZDefaultIdentifierConverter.Create(FMetadata);
end;

procedure TZAbstractResultSetMetadata.SetMetadataLoaded(Value: Boolean);
begin
  Loaded := Value;
end;

procedure TZAbstractResultSetMetadata.SetSearchable(ColumnIndex: Integer;
  Value: Boolean);
begin
  with TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    SearchableDisabled := not Value;
end;

procedure TZAbstractResultSetMetadata.SetReadOnly(ColumnIndex: Integer;
  Value: Boolean);
begin
  with TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do
    if Value <> ReadOnly then
      if Value
      then ReadOnly := True
      else ReadOnly := IsWritable(ColumnIndex);
end;

procedure TZAbstractResultSetMetadata.SetReadOnlyFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnReadonlyIndex) then
    if ColumnInfo.AutoIncrement and Assigned(FMetadata) then {improve ADO where the metainformations do not bring autoincremental fields through}
      if FMetadata.GetDatabaseInfo.SupportsUpdateAutoIncrementFields then
        ColumnInfo.ReadOnly := TableColumns.GetBoolean(TableColColumnReadonlyIndex)
      else
        ColumnInfo.ReadOnly := True
    else
      ColumnInfo.ReadOnly := TableColumns.GetBoolean(TableColColumnReadonlyIndex);
end;

procedure TZAbstractResultSetMetadata.SetWritableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnWritableIndex) then
    if ColumnInfo.AutoIncrement and Assigned(FMetadata) then {improve ADO where the metainformations do not bring autoincremental fields through}
      if FMetadata.GetDatabaseInfo.SupportsUpdateAutoIncrementFields
      then ColumnInfo.Writable := TableColumns.GetBoolean(TableColColumnWritableIndex)
      else ColumnInfo.Writable := False
    else ColumnInfo.Writable := TableColumns.GetBoolean(TableColColumnWritableIndex);
end;

procedure TZAbstractResultSetMetadata.LoadColumns;
var
  I, j: Integer;
  Connection: IZConnection;
  Tokenizer: IZTokenizer;
  StatementAnalyser: IZStatementAnalyser;
  SelectSchema: IZSelectSchema;
  FillByIndices: Boolean;
begin
  { Parses the Select statement and retrieves a schema object. }
  Connection := Metadata.GetConnection;
  Tokenizer := Connection.GetTokenizer;
  StatementAnalyser := Connection.GetStatementAnalyser;
  SelectSchema := StatementAnalyser.DefineSelectSchemaFromQuery(Tokenizer, SQL);
  if Assigned(SelectSchema) then begin
    SelectSchema.LinkReferences(IdentifierConverter);
    ReplaceStarColumns(SelectSchema);
    FillByIndices := SelectSchema.FieldCount = FResultSet.ColumnsInfo.Count;
    J := -1;
    for I := 0 to FResultSet.ColumnsInfo.Count - 1 do begin
      if FillByIndices then
        J := I{$IFNDEF GENERIC_INDEX}+1{$ENDIF};
      LoadColumn(J, TZColumnInfo(FResultSet.ColumnsInfo[I]), SelectSchema);
    end;
  end;
  Loaded := True;
end;

end.

