{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Abstract Database Connectivity Classes        }
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

unit ZDbcMetadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}FmtBCD,
  ZSysUtils, ZClasses, ZDbcIntfs, ZDbcResultSetMetadata, ZDbcCachedResultSet,
  ZCompatibility, ZSelectSchema, ZDbcConnection;

type
  TZWildcardsSet= {$IFDEF UNICODE}
                    {$IFNDEF TSYSCHARSET_IS_DEPRECATED}
                    TSysCharSet
                    {$ELSE}
                    array of Char
                    {$ENDIF}
                  {$ELSE} set of Char {$ENDIF};

  {** Defines a metadata resultset column definition. }
  TZMetadataColumnDef = record
    Name: string;
    SQLType: TZSQLType;
    Length: Integer
  end;

  {** Defines a dynamic array of metadata column definitions. }
  TZMetadataColumnDefs = array of TZMetadataColumnDef;

  {** Implements Uncloseable ResultSet which frees all memory if it's not referenced anymore. }
  TZUnCloseableResultSet = class(TZVirtualResultSet)
  private
    fDoClose: Boolean;
  public
    procedure Close; override;
    destructor Destroy; override;
    procedure ResetCursor; override;
  end;

  PZKeyAndResultSetPair = ^TZKeyAndResultSetPair;
  TZKeyAndResultSetPair = record
    Key: String;
    ResultSet: IZResultSet;
  end;

  TZKeyAndResultSetPairList = Class(TZCustomUniqueElementBinarySearchList)
  protected
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(ElementSize: Cardinal; ElementNeedsFinalize: Boolean);
  End;

  {** Implements Abstract Database Metadata. }

  { TZAbstractDatabaseMetadata }
  TZAbstractDatabaseMetadata = class(TContainedObject, IZDatabaseMetadata)
  private
    FConnection: Pointer;
    FUrl: TZURL;
    FCachedResultSets: TZKeyAndResultSetPairList;
    FDatabaseInfo: IZDatabaseInfo;
    function GetInfo: TStrings;
    function GetURLString: String;
  private
    fCurrentBufIndex: Byte;
    fBuf: Array[Byte] of Char;
    fKeyAndResultSetValue: TZKeyAndResultSetPair;
  protected
    FIC: IZIdentifierConverter;
    FConSettings: PZConSettings;
    procedure InitBuf(FirstChar: Char); {$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure ClearBuf; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure FlushBuf(var Value: String); {$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure ToBuf(C: Char; var Value: String); {$IFDEF WITH_INLINE}inline;{$ENDIF}
  protected //EH: normalize the qualifier names before testing against the cache
    procedure NormalizeCatalogName(var Value: String); virtual;
    procedure NormalizeSchemaName(var Value: String; IsPattern: Boolean); virtual;
    procedure NormalizeTableName(var Value: String; IsPattern: Boolean); virtual;
    procedure NormalizeProcedureName(var Value: String; IsPattern: Boolean); virtual;
    procedure NormalizeColumnNamePattern(var Value: String); virtual;
    procedure NormalizeParameterNamePattern(var Value: String); virtual;
    procedure NormalizeSequenceNamePattern(var Value: String); virtual;
    procedure NormalizeTriggerNamePattern(var Value: String); virtual;
  protected
    FDatabase: String;
    WildcardsArray: {$IFDEF TSYSCHARSET_IS_DEPRECATED}TZWildcardsSet{$ELSE}array of char{$ENDIF}; //Added by Cipto
    function StripEscape(const Pattern: string): string;
    function HasNoWildcards(const Pattern: string): boolean;
    function EscapeString(const S: string): string; virtual;
    function DecomposeObjectString(const S: String): String; virtual;
    function CreateDatabaseInfo: IZDatabaseInfo; virtual; // technobot 2008-06-24
    function GetStatement: IZSTatement; // technobot 2008-06-28 - moved from descendants

    { Metadata ResultSets Caching. }
    procedure AddResultSetToCache(const Key: string; const ResultSet: IZResultSet);
    function GetResultSetFromCache(const Key: string): IZResultSet;
    function HasKey(const Key: String): Boolean;
    function ConstructVirtualResultSet(ColumnsDefs: TZMetadataColumnDefs):
      IZVirtualResultSet; virtual;
    function CopyToVirtualResultSet(const SrcResultSet: IZResultSet;
      const DestResultSet: IZVirtualResultSet): IZVirtualResultSet;
    function CloneCachedResultSet(const ResultSet: IZResultSet): IZResultSet;
    function ConstructNameCondition(const Pattern: string; const Column: string): string; virtual;
    function AddEscapeCharToWildcards(const Pattern: string): string;
    function GetWildcardsSet:TZWildcardsSet;
    procedure FillWildcards; virtual;
    function NormalizePatternCase(const Pattern: String): string;
    property Url: string read GetURLString;
    property Info: TStrings read GetInfo;
    property CachedResultSets: TZKeyAndResultSetPairList read FCachedResultSets;
    property IC: IZIdentifierConverter read FIC;
  protected
    /// <summary>Gets a description of tables available in a catalog.
    ///  Only table descriptions matching the catalog, schema, table
    ///  name and type criteria are returned. They are ordered by
    ///  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.
    ///  Each table description has the following columns:
 	  ///  <c>TABLE_CAT</c> String => table catalog (may be null)
 	  ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
 	  ///  <c>TABLE_NAME</c> String => table name
 	  ///  <c>TABLE_TYPE</c> String => table type.  Typical types are "TABLE",
 	  ///  		"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 	  ///  		"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	  ///  <c>REMARKS</c> String => explanatory comment on the table</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema
    ///  pattern from the selection criteria</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <param>"Types" an array of table types to include; nil returns all
    ///  types.</param>
    /// <returns><c>ResultSet</c> - each row is a table description</returns>
    /// <remarks>Some databases may not return information for
    ///  all tables. Additional columns beyond REMARKS can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function UncachedGetTables(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}TableNamePattern: string; const {%H-}Types: TStringDynArray): IZResultSet; virtual;
    function UncachedGetSchemas: IZResultSet; virtual;
    function UncachedGetCatalogs: IZResultSet; virtual;
    /// <summary>Gets the table types available in this database. The results
    ///  are ordered by table type.
    ///  The table type is:
    ///  <c>TABLE_TYPE</c> String => table type. Typical types are "TABLE",
    ///  "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY","LOCAL TEMPORARY", "ALIAS",
    ///  "SYNONYM".</summary>
    /// <returns><c>ResultSet</c> - each row has a single String column that is
    ///  a table type</returns>
    function UncachedGetTableTypes: IZResultSet; virtual;
    /// <summary>Gets a description of table columns available in
    ///  the specified catalog.
    ///  Only column descriptions matching the catalog, schema pattern, table
    ///  name pattern and column name criteria are returned. They are ordered by
    ///  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.
    ///  Each column description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>COLUMN_NAME</c> String => column name
    ///  <c>DATA_TYPE</c> short => SQL type from java.sql.Types
    ///  <c>TYPE_NAME</c> String => Data source dependent type name,
    ///   for a UDT the type name is fully qualified
    ///  <c>COLUMN_SIZE</c> int => column size.  For char or date
    ///      types this is the maximum number of characters, for numeric or
    ///      decimal types this is precision.
    ///  <c>BUFFER_LENGTH</c> is not used.
    ///  <c>DECIMAL_DIGITS</c> int => the number of fractional digits
    ///  <c>NUM_PREC_RADIX</c> int => Radix (typically either 10 or 2)
    ///  <c>NULLABLE</c> int => is NULL allowed?
    ///     Ord(ntNoNulls) - does not allow NULL values
    ///     Ord(ntNullable) - allows NULL values
    ///     Ord(ntNullableUnknown) - nullability unknown
    ///  <c>REMARKS</c> String => comment describing column (may be null)
    ///  <c>COLUMN_DEF</c> String => default value (may be null)
    ///  <c>SQL_DATA_TYPE</c> int => unused
    ///  <c>SQL_DATETIME_SUB</c> int => unused
    ///  <c>CHAR_OCTET_LENGTH</c> int => for char types the
    ///        maximum number of bytes in the column
    ///  <c>ORDINAL_POSITION</c> int	=> index of column in table
    ///       (starting at 1)
    ///  <c>IS_NULLABLE</c> String => "NO" means column definitely
    ///       does not allow NULL values; "YES" means the column might
    ///       allow NULL values. An empty string means nobody knows.</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" retrieves those
    ///  without a schema</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <param>"ColumnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a column description</returns>
    /// <remarks>Some databases may not return information for
    ///  all tables. Additional columns beyond IS_NULLABLE can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function UncachedGetColumns(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}TableNamePattern: string; const {%H-}ColumnNamePattern: string): IZResultSet; virtual;
    /// <summary>Gets a description of the access rights for each table
    ///  available in a catalog. Note that a table privilege applies to one or
    ///  more columns in the table. It would be wrong to assume that
    ///  this priviledge applies to all columns (this may be true for
    ///  some systems but is not true for all.)
    ///
    ///  Only privileges matching the schema and table name
    ///  criteria are returned. They are ordered by TABLE_SCHEM,
    ///  TABLE_NAME, and PRIVILEGE.
    ///
    ///  Each privilige description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>GRANTOR</c> => grantor of access (may be null)
    ///  <c>GRANTEE</c> String => grantee of access
    ///  <c>PRIVILEGE</c> String => name of access (SELECT,
    ///      INSERT, UPDATE, REFRENCES, ...)
    ///  <c>IS_GRANTABLE</c> String => "YES" if grantee is permitted
    ///   to grant to others; "NO" if not; null if unknown</summary>
    ///
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema from
    ///  the selection criteria</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a table privilege description</returns>
    /// <remarks>see GetSearchStringEscape</remarks>
    function UncachedGetTablePrivileges(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}TableNamePattern: string): IZResultSet; virtual;
    /// <summary>Gets a description of the access rights for a table's columns.
    ///
    ///  Only privileges matching the column name criteria are
    ///  returned. They are ordered by COLUMN_NAME and PRIVILEGE.
    ///
    ///  Each privilige description has the following columns:
 	  ///  <c>TABLE_CAT</c> String => table catalog (may be null)
 	  ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
 	  ///  <c>TABLE_NAME</c> String => table name
 	  ///  <c>COLUMN_NAME</c> String => column name
 	  ///  <c>GRANTOR</c> => grantor of access (may be null)
 	  ///  <c>GRANTEE</c> String => grantee of access
 	  ///  <c>PRIVILEGE</c> String => name of access (SELECT,
    ///     INSERT, UPDATE, REFRENCES, ...)
 	  ///  <c>IS_GRANTABLE</c> String => "YES" if grantee is permitted
    ///   to grant to others; "NO" if not; null if unknown</summary>
    /// <param>"Catalog" a catalog name; An empty catalog means drop catalog
    ///  name from the selection criteria</param>
    /// <param>"schema" a schema name; An empty schema means drop schema
    ///  name from the selection criteria</param>
    /// <param>"table" a table name; An empty table means drop table
    ///  name from the selection criteria</param>
    /// <param>"ColumnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a privilege description</returns>
    /// <remarks>see GetSearchStringEscape</remarks>
    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; virtual;
    /// <summary>Gets a description of a table's primary key columns. They
    ///  are ordered by COLUMN_NAME.
    ///  Each primary key column description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>COLUMN_NAME</c> String => column name
    ///  <c>KEY_SEQ</c> short => sequence number within primary key
    ///  <c>PK_NAME</c> String => primary key name (may be null)</summary>
    /// <param>"Catalog" a catalog name; An empty catalog means drop catalog
    ///  name from the selection criteria</param>
    /// <param>"schema" a schema name; An empty schema means drop schema
    ///  name from the selection criteria</param>
    /// <param>"table" a table name; An empty table means drop table
    ///  name from the selection criteria</param>
    /// <returns><c>ResultSet</c> - each row is a primary key column description</returns>
    /// <remarks>see GetSearchStringEscape</remarks>
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; virtual;
    /// <summary>Gets a description of the primary key columns that are
    ///  referenced by a table's foreign key columns (the primary keys
    ///  imported by a table).  They are ordered by PKTABLE_CAT,
    ///  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.
    ///  Each primary key column description has the following columns:
    ///  <c>PKTABLE_CAT</c> String => primary key table catalog
    ///       being imported (may be null)
    ///  <c>PKTABLE_SCHEM</c> String => primary key table schema
    ///       being imported (may be null)
    ///  <c>PKTABLE_NAME</c> String => primary key table name
    ///       being imported
    ///  <c>PKCOLUMN_NAME</c> String => primary key column name
    ///       being imported
    ///  <c>FKTABLE_CAT</c> String => foreign key table catalog (may be null)
    ///  <c>FKTABLE_SCHEM</c> String => foreign key table schema (may be null)
    ///  <c>FKTABLE_NAME</c> String => foreign key table name
    ///  <c>FKCOLUMN_NAME</c> String => foreign key column name
    ///  <c>KEY_SEQ</c> short => sequence number within foreign key
    ///  <c>UPDATE_RULE</c> short => What happens to
    ///        foreign key when primary is updated:
    ///        importedNoAction - do not allow update of primary
    ///                key if it has been imported
    ///        importedKeyCascade - change imported key to agree
    ///                with primary key update
    ///        importedKeySetNull - change imported key to NULL if
    ///                its primary key has been updated
    ///        importedKeySetDefault - change imported key to default values
    ///                if its primary key has been updated
    ///        importedKeyRestrict - same as importedKeyNoAction
    ///                                  (for ODBC 2.x compatibility)
    ///  <c>DELETE_RULE</c> short => What happens to
    ///       the foreign key when primary is deleted.
    ///        importedKeyNoAction - do not allow delete of primary
    ///                key if it has been imported
    ///        importedKeyCascade - delete rows that import a deleted key
    ///       importedKeySetNull - change imported key to NULL if
    ///                its primary key has been deleted
    ///        importedKeyRestrict - same as importedKeyNoAction
    ///                                  (for ODBC 2.x compatibility)
    ///        importedKeySetDefault - change imported key to default if
    ///                its primary key has been deleted
    ///  <c>FK_NAME</c> String => foreign key name (may be null)
    ///  <c>PK_NAME</c> String => primary key name (may be null)
    ///  <c>DEFERRABILITY</c> short => can the evaluation of foreign key
    ///       constraints be deferred until commit
    ///        importedKeyInitiallyDeferred - see SQL92 for definition
    ///        importedKeyInitiallyImmediate - see SQL92 for definition
    ///        importedKeyNotDeferrable - see SQL92 for definition</summary>
    /// <param>"Catalog" a catalog name; An empty catalog means drop catalog
    ///  name from the selection criteria</param>
    /// <param>"schema" a schema name; An empty schema means drop schema
    ///  name from the selection criteria</param>
    /// <param>"table" a table name; An empty table means drop table
    ///  name from the selection criteria</param>
    /// <returns><c>ResultSet</c> - each row is imported key column description</returns>
    /// <remarks>see GetSearchStringEscape;GetExportedKeys</remarks>
    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; virtual;
    function UncachedGetExportedKeys(const {%H-}Catalog: string; const {%H-}Schema: string;
      const {%H-}Table: string): IZResultSet; virtual;
    function UncachedGetCrossReference(const {%H-}PrimaryCatalog: string; const {%H-}PrimarySchema: string;
      const {%H-}PrimaryTable: string; const {%H-}ForeignCatalog: string; const {%H-}ForeignSchema: string;
      const {%H-}ForeignTable: string): IZResultSet; virtual;
    function UncachedGetIndexInfo(const {%H-}Catalog: string; const {%H-}Schema: string; const {%H-}Table: string;
      {%H-}Unique: Boolean; {%H-}Approximate: Boolean): IZResultSet; virtual;
    function UncachedGetSequences(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}SequenceNamePattern: string): IZResultSet; virtual;
    function UncachedGetTriggers(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}TableNamePattern: string; const {%H-}TriggerNamePattern: string): IZResultSet; virtual; //EgonHugeist
    function UncachedGetCollationAndCharSet(const {%H-}Catalog, {%H-}SchemaPattern,
      {%H-}TableNamePattern, {%H-}ColumnNamePattern: string): IZResultSet; virtual; //EgonHugeist
    function UncachedGetCharacterSets: IZResultSet; virtual; //EgonHugeist
    /// <summary>Gets a description of the stored procedures available in a
    ///  catalog. This method needs to be implemented per driver.
    ///  Only procedure descriptions matching the schema and procedure name
    ///  criteria are returned. They are ordered by
    ///  PROCEDURE_SCHEM, and PROCEDURE_NAME.
    ///  Each procedure description has the the following columns:
    ///  <c>PROCEDURE_CAT</c> String => procedure catalog (may be null)
    ///  <c>PROCEDURE_SCHEM</c> String => procedure schema (may be null)
    ///  <c>PROCEDURE_NAME</c> String => procedure name
    ///  <c>PROCEDURE_OVERLOAD</c> => a overload indicator (may be null)
    ///  <c>RESERVED1</c> => for future use
    ///  <c>RESERVED2</c> => for future use
    ///  <c>REMARKS</c> String => explanatory comment on the procedure
    ///  <c>PROCEDURE_TYPE</c> short => kind of procedure:
    ///   procedureResultUnknown - May return a result
    ///   procedureNoResult - Does not return a result
    ///   procedureReturnsResult - Returns a result</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema
    ///  pattern from the selection criteria</param>
    /// <param>"ProcedureNamePattern" a procedure name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a procedure description.</returns>
    /// <remarks>see getSearchStringEscape</remarks>
    function UncachedGetProcedures(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}ProcedureNamePattern: string): IZResultSet; virtual;
    /// <summary>Gets a description of a catalog's stored procedure parameters
    ///  and result columns. This method needs to be implemented per driver.
    ///  Only descriptions matching the schema, procedure and
    ///  parameter name criteria are returned.  They are ordered by
    ///  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
    ///  if any, is first. Next are the parameter descriptions in call
    ///  order. The column descriptions follow in column number order.
    ///  Each row in the <c>ResultSet</c> is a parameter description or
    ///  column description with the following fields:
 	  ///  <c>PROCEDURE_CAT</c> String => procedure catalog (may be null)
 	  ///  <c>PROCEDURE_SCHEM</c> String => procedure schema (may be null)
 	  ///  <c>PROCEDURE_NAME</c> String => procedure name
 	  ///  <c>COLUMN_NAME</c> String => column/parameter name
 	  ///  <c>COLUMN_TYPE</c> Short => kind of column/parameter:
    ///      Ord(pctUnknown) - nobody knows
    ///      Ord(pctIn) - IN parameter
    ///      Ord(pctInOut) - INOUT parameter
    ///      Ord(pctOut) - OUT parameter
    ///      Ord(pctReturn) - procedure return value
    ///      Ord(pctResultSet) - result column in <c>ResultSet</c>
    ///  <c>DATA_TYPE</c> short => ZDBC SQL type
 	  ///  <c>TYPE_NAME</c> String => SQL type name, for a UDT type the
    ///   type name is fully qualified
 	  ///  <c>PRECISION</c> int => precision
 	  ///  <c>LENGTH</c> int => length in bytes of data
 	  ///  <c>SCALE</c> short => scale, second fractions
 	  ///  <c>RADIX</c> short => radix
 	  ///  <c>NULLABLE</c> short => can it contain NULL?
    ///     Ord(ntNoNulls) - does not allow NULL values
    ///     Ord(ntNullable) - allows NULL values
    ///     Ord(ntNullableUnknown) - nullability unknown
 	  ///  <c>REMARKS</c> String => comment describing parameter/column</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop
    ///  schema pattern from the selection criteria</param>
    /// <param>"ProcedureNamePattern" a procedure name pattern</param>
    /// <param>"columnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row describes a stored procedure
    ///  parameter or column</returns>
    /// <remarks>Some databases may not return the column descriptions for a
    ///  procedure. Additional columns beyond REMARKS can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function UncachedGetProcedureColumns(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}ProcedureNamePattern: string; const {%H-}ColumnNamePattern: string):
      IZResultSet; virtual;
    function UncachedGetBestRowIdentifier(const Catalog: string; const Schema: string;
      const Table: string; {%H-}Scope: Integer; {%H-}Nullable: Boolean): IZResultSet; virtual;
    function UncachedGetVersionColumns(const {%H-}Catalog: string; const {%H-}Schema: string;
      const {%H-}Table: string): IZResultSet; virtual;
    function UncachedGetTypeInfo: IZResultSet; virtual;
    function UncachedGetUDTs(const {%H-}Catalog: string; const {%H-}SchemaPattern: string;
      const {%H-}TypeNamePattern: string; const {%H-}Types: TIntegerDynArray): IZResultSet; virtual;
  public
    /// <summary>Constructs this object and assignes the main properties.</summary>
    /// <param>"Connection" a database connection object.</param>
    /// <param>"Url" a database connection url class.</param>
    constructor Create(Connection: TZAbstractDbcConnection; const Url: TZURL); virtual;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
    /// <summary>What's the url for this database?</summary>
    /// <returns>the url or null if it cannot be generated</returns>
    function GetURL: string; virtual;
    /// <summary>What's our user name as known to the database?</summary>
    /// <returns>our database user name</returns>
    function GetUserName: string; virtual;
    /// <author>technobot</author>
    /// <summary>Returns general information about the database (version,
    ///  capabilities,  policies, etc).</summary>
    /// <returns>the database information object as interface.</returns>
    function GetDatabaseInfo: IZDatabaseInfo;
    /// <summary>Gets a description of tables available in a catalog from a
    ///  cache. If the cache doesn't have a entry for the matching criteria
    ///  UncachedGetTables is called and the result will be added to the cache.
    ///  Only table descriptions matching the catalog, schema, table
    ///  name and type criteria are returned. They are ordered by
    ///  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.
    ///  Each table description has the following columns:
 	  ///  <c>TABLE_CAT</c> String => table catalog (may be null)
 	  ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
 	  ///  <c>TABLE_NAME</c> String => table name
 	  ///  <c>TABLE_TYPE</c> String => table type.  Typical types are "TABLE",
 	  ///  		"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 	  ///  		"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	  ///  <c>REMARKS</c> String => explanatory comment on the table</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema
    ///  pattern from the selection criteria</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <param>"Types" an array of table types to include; nil returns all
    ///  types.</param>
    /// <returns><c>ResultSet</c> - each row is a table description</returns>
    /// <remarks>Some databases may not return information for
    ///  all tables. Additional columns beyond REMARKS can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function GetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet;
    /// <summary>Gets the schema names available in this database. The results
    ///  are ordered by schema name.
    ///  The schema column is:
    ///  <C>TABLE_SCHEM</C> String => schema name</summary>
    /// <returns><c>ResultSet</c> - each row has a single String column that is
    ///  a schema name</returns>
    function GetSchemas: IZResultSet;
    /// <summary>Gets the catalog names available in this database. The results
    ///  are ordered by catalog name.
    ///  The catalog column is:
    ///  <C>TABLE_CAT</C> String => catalog name</summary>
    /// <returns><c>ResultSet</c> - each row has a single String column that is
    ///  a catalog name</returns>
    function GetCatalogs: IZResultSet;
    /// <summary>Gets the table types available in this database. The results
    ///  are ordered by table type.
    ///  The table type is:
    ///  <c>TABLE_TYPE</c> String => table type. Typical types are "TABLE",
    ///  "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY","LOCAL TEMPORARY", "ALIAS",
    ///  "SYNONYM".</summary>
    /// <returns><c>ResultSet</c> - each row has a single String column that is
    ///  a table type</returns>
    function GetTableTypes: IZResultSet;
    /// <summary>Gets a description of table columns available in
    ///  the specified catalog from a cache. If the cache doesn't have a entry
    ///  for the matching criteria UncachedGetTables is called and the result
    ///  will be added to the cache.
    ///  Only column descriptions matching the catalog, schema pattern, table
    ///  name pattern and column name criteria are returned. They are ordered by
    ///  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.
    ///  Each column description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>COLUMN_NAME</c> String => column name
    ///  <c>DATA_TYPE</c> short => SQL type from java.sql.Types
    ///  <c>TYPE_NAME</c> String => Data source dependent type name,
    ///   for a UDT the type name is fully qualified
    ///  <c>COLUMN_SIZE</c> int => column size. For char or date
    ///      types this is the maximum number of characters, for numeric or
    ///      decimal types this is precision.
    ///  <c>BUFFER_LENGTH</c> is not used.
    ///  <c>DECIMAL_DIGITS</c> int => the number of fractional digits
    ///  <c>NUM_PREC_RADIX</c> int => Radix (typically either 10 or 2)
    ///  <c>NULLABLE</c> int => is NULL allowed?
    ///     Ord(ntNoNulls) - does not allow NULL values
    ///     Ord(ntNullable) - allows NULL values
    ///     Ord(ntNullableUnknown) - nullability unknown
    ///  <c>REMARKS</c> String => comment describing column (may be null)
    ///  <c>COLUMN_DEF</c> String => default value (may be null)
    ///  <c>SQL_DATA_TYPE</c> int => unused
    ///  <c>SQL_DATETIME_SUB</c> int => unused
    ///  <c>CHAR_OCTET_LENGTH</c> int => for char types the
    ///        maximum number of bytes in the column
    ///  <c>ORDINAL_POSITION</c> int	=> index of column in table
    ///       (starting at 1)
    ///  <c>IS_NULLABLE</c> String => "NO" means column definitely
    ///       does not allow NULL values; "YES" means the column might
    ///       allow NULL values. An empty string means nobody knows.</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" retrieves those
    ///  without a schema</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <param>"ColumnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a column description</returns>
    /// <remarks>Some databases may not return information for
    ///  all tables. Additional columns beyond IS_NULLABLE can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function GetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet;
    /// <summary>Gets a description of the access rights for each table
    ///  available in a catalog from a cache. Note that a table privilege
    ///  applies to one or more columns in the table. It would be wrong to
    ///  assume that this priviledge applies to all columns (this may be true
    ///  for some systems but is not true for all.)
    ///
    ///  Only privileges matching the schema and table name
    ///  criteria are returned. They are ordered by TABLE_SCHEM,
    ///  TABLE_NAME, and PRIVILEGE.
    ///
    ///  Each privilige description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>GRANTOR</c> => grantor of access (may be null)
    ///  <c>GRANTEE</c> String => grantee of access
    ///  <c>PRIVILEGE</c> String => name of access (SELECT,
    ///      INSERT, UPDATE, REFRENCES, ...)
    ///  <c>IS_GRANTABLE</c> String => "YES" if grantee is permitted
    ///   to grant to others; "NO" if not; null if unknown</summary>
    ///
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema from
    ///  the selection criteria</param>
    /// <param>"TableNamePattern" a table name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a table privilege description</returns>
    /// <remarks>see GetSearchStringEscape</remarks>
    function GetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet;
    /// <summary>Gets a description of the access rights for a table's columns
    ///  from a cache.
    ///  Only privileges matching the column name criteria are
    ///  returned. They are ordered by COLUMN_NAME and PRIVILEGE.
    ///
    ///  Each privilige description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>COLUMN_NAME</c> String => column name
    ///  <c>GRANTOR</c> => grantor of access (may be null)
    ///  <c>GRANTEE</c> String => grantee of access
    ///  <c>PRIVILEGE</c> String => name of access (SELECT,
    ///     INSERT, UPDATE, REFRENCES, ...)
    ///  <c>IS_GRANTABLE</c> String => "YES" if grantee is permitted
    ///   to grant to others; "NO" if not; null if unknown</summary>
    /// <param>"Catalog" a catalog name; An empty catalog means drop catalog
    ///  name from the selection criteria</param>
    /// <param>"schema" a schema name; An empty schema means drop schema
    ///  name from the selection criteria</param>
    /// <param>"table" a table name; An empty table means drop table
    ///  name from the selection criteria</param>
    /// <param>"ColumnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a privilege description</returns>
    /// <remarks>see GetSearchStringEscape</remarks>
    function GetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet;
    /// <summary>Gets a description of a table's primary key columns from a
    ///  cache. They are ordered by COLUMN_NAME.
    ///  Each primary key column description has the following columns:
    ///  <c>TABLE_CAT</c> String => table catalog (may be null)
    ///  <c>TABLE_SCHEM</c> String => table schema (may be null)
    ///  <c>TABLE_NAME</c> String => table name
    ///  <c>COLUMN_NAME</c> String => column name
    ///  <c>KEY_SEQ</c> short => sequence number within primary key
    ///  <c>PK_NAME</c> String => primary key name (may be null)</summary>
    /// <param>"Catalog" a catalog name; An empty catalog means drop catalog
    ///  name from the selection criteria</param>
    /// <param>"schema" a schema name; An empty schema means drop schema
    ///  name from the selection criteria</param>
    /// <param>"table" a table name; An empty table means drop table
    ///  name from the selection criteria</param>
    /// <returns><c>ResultSet</c> - each row is a primary key column description</returns>
    /// <remarks>see GetSearchStringEscape</remarks>
    function GetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    /// <summary>Gets a description of the primary key columns that are
    ///  referenced by a table's foreign key columns (the primary keys
    ///  imported by a table) from a cache.  They are ordered by PKTABLE_CAT,
    ///  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.
    ///  Each primary key column description has the following columns:
    ///  <c>PKTABLE_CAT</c> String => primary key table catalog
    ///       being imported (may be null)
    ///  <c>PKTABLE_SCHEM</c> String => primary key table schema
    ///       being imported (may be null)
    ///  <c>PKTABLE_NAME</c> String => primary key table name
    ///       being imported
    ///  <c>PKCOLUMN_NAME</c> String => primary key column name
    ///       being imported
    ///  <c>FKTABLE_CAT</c> String => foreign key table catalog (may be null)
    ///  <c>FKTABLE_SCHEM</c> String => foreign key table schema (may be null)
    ///  <c>FKTABLE_NAME</c> String => foreign key table name
    ///  <c>FKCOLUMN_NAME</c> String => foreign key column name
    ///  <c>KEY_SEQ</c> short => sequence number within foreign key
    ///  <c>UPDATE_RULE</c> short => What happens to
    ///        foreign key when primary is updated:
    ///        importedNoAction - do not allow update of primary
    ///                key if it has been imported
    ///        importedKeyCascade - change imported key to agree
    ///                with primary key update
    ///        importedKeySetNull - change imported key to NULL if
    ///                its primary key has been updated
    ///        importedKeySetDefault - change imported key to default values
    ///                if its primary key has been updated
    ///        importedKeyRestrict - same as importedKeyNoAction
    ///                                  (for ODBC 2.x compatibility)
    ///  <c>DELETE_RULE</c> short => What happens to
    ///       the foreign key when primary is deleted.
    ///        importedKeyNoAction - do not allow delete of primary
    ///                key if it has been imported
    ///        importedKeyCascade - delete rows that import a deleted key
    ///       importedKeySetNull - change imported key to NULL if
    ///                its primary key has been deleted
    ///        importedKeyRestrict - same as importedKeyNoAction
    ///                                  (for ODBC 2.x compatibility)
    ///        importedKeySetDefault - change imported key to default if
    ///                its primary key has been deleted
    ///  <c>FK_NAME</c> String => foreign key name (may be null)
    ///  <c>PK_NAME</c> String => primary key name (may be null)
    ///  <c>DEFERRABILITY</c> short => can the evaluation of foreign key
    ///       constraints be deferred until commit
    ///        importedKeyInitiallyDeferred - see SQL92 for definition
    ///        importedKeyInitiallyImmediate - see SQL92 for definition
    ///        importedKeyNotDeferrable - see SQL92 for definition</summary>
    /// <param>"Catalog" a catalog name; An empty catalog means drop catalog
    ///  name from the selection criteria</param>
    /// <param>"schema" a schema name; An empty schema means drop schema
    ///  name from the selection criteria</param>
    /// <param>"table" a table name; An empty table means drop table
    ///  name from the selection criteria</param>
    /// <returns><c>ResultSet</c> - each row is imported key column description</returns>
    /// <remarks>see GetSearchStringEscape;GetExportedKeys</remarks>
    function GetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet;
    function GetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet;
    function GetCollationAndCharSet(const Catalog, Schema, TableName, ColumnName: String): IZResultSet; //EgonHugeist
    function GetCharacterSets: IZResultSet; //EgonHugeist
    function GetTriggers(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet; //EgonHugesit
    function GetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet;
    /// <summary>Gets a description of the stored procedures available in a
    ///  catalog from a cache. If the cache doesn't have a entry for the
    ///  matching criteria UncachedGetProcedures is called and the result will
    ///  be added to the cache.
    ///  Only procedure descriptions matching the schema and procedure name
    ///  criteria are returned. They are ordered by
    ///  PROCEDURE_SCHEM, and PROCEDURE_NAME.
    ///  Each procedure description has the the following columns:
    ///  <c>PROCEDURE_CAT</c> String => procedure catalog (may be null)
    ///  <c>PROCEDURE_SCHEM</c> String => procedure schema (may be null)
    ///  <c>PROCEDURE_NAME</c> String => procedure name
    ///  <c>PROCEDURE_OVERLOAD</c> => a overload indicator (may be null)
    ///  <c>RESERVED1</c> => for future use
    ///  <c>RESERVED2</c> => for future use
    ///  <c>REMARKS</c> String => explanatory comment on the procedure
    ///  <c>PROCEDURE_TYPE</c> short => kind of procedure:
    ///   procedureResultUnknown - May return a result
    ///   procedureNoResult - Does not return a result
    ///   procedureReturnsResult - Returns a result</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop schema
    ///  pattern from the selection criteria</param>
    /// <param>"ProcedureNamePattern" a procedure name pattern</param>
    /// <returns><c>ResultSet</c> - each row is a procedure description.</returns>
    /// <remarks>see getSearchStringEscape</remarks>
    function GetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet;
    /// <summary>Gets a description of a catalog's stored procedure parameters
    ///  and result columns from a cache. If the cache doesn't have a entry for
    ///  the matching criteria UncachedGetProcedureColumns is called and the
    ///  result will be added to the cache.
    ///  Only descriptions matching the schema, procedure and
    ///  parameter name criteria are returned.  They are ordered by
    ///  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
    ///  if any, is first. Next are the parameter descriptions in call
    ///  order. The column descriptions follow in column number order.
    ///  Each row in the <c>ResultSet</c> is a parameter description or
    ///  column description with the following fields:
    ///  <c>PROCEDURE_CAT</c> String => procedure catalog (may be null)
    ///  <c>PROCEDURE_SCHEM</c> String => procedure schema (may be null)
    ///  <c>PROCEDURE_NAME</c> String => procedure name
    ///  <c>COLUMN_NAME</c> String => column/parameter name
    ///  <c>COLUMN_TYPE</c> Short => kind of column/parameter:
    ///      Ord(pctUnknown) - nobody knows
    ///      Ord(pctIn) - IN parameter
    ///      Ord(pctInOut) - INOUT parameter
    ///      Ord(pctOut) - OUT parameter
    ///      Ord(pctReturn) - procedure return value
    ///      Ord(pctResultSet) - result column in <c>ResultSet</c>
    ///  <c>DATA_TYPE</c> short => ZDBC SQL type
    ///  <c>TYPE_NAME</c> String => SQL type name, for a UDT type the
    ///   type name is fully qualified
    ///  <c>PRECISION</c> int => precision
    ///  <c>LENGTH</c> int => length in bytes of data
    ///  <c>SCALE</c> short => scale, second fractions
    ///  <c>RADIX</c> short => radix
    ///  <c>NULLABLE</c> short => can it contain NULL?
    ///     Ord(ntNoNulls) - does not allow NULL values
    ///     Ord(ntNullable) - allows NULL values
    ///     Ord(ntNullableUnknown) - nullability unknown
 	  ///  <c>REMARKS</c> String => comment describing parameter/column</summary>
    /// <param>"Catalog" a catalog name; "" means drop catalog name from the
    ///  selection criteria</param>
    /// <param>"SchemaPattern" a schema name pattern; "" means drop
    ///  schema pattern from the selection criteria</param>
    /// <param>"ProcedureNamePattern" a procedure name pattern</param>
    /// <param>"columnNamePattern" a column name pattern</param>
    /// <returns><c>ResultSet</c> - each row describes a stored procedure
    ///  parameter or column</returns>
    /// <remarks>Some databases may not return the column descriptions for a
    ///  procedure. Additional columns beyond REMARKS can be defined by the
    ///  database.
    /// see GetSearchStringEscape</remarks>
    function GetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet;
    function GetBestRowIdentifier(const Catalog: string; const Schema: string;
      const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet;
    function GetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet;
    function GetTypeInfo: IZResultSet;
    function GetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet;

    function GetConnection: IZConnection; virtual;

    function GetIdentifierConvertor: IZIdentifierConverter; //EH: left for compatibility
    function GetIdentifierConverter: IZIdentifierConverter; virtual;
    /// <summary>Clears all cached metadata.</summary>
    procedure ClearCache; overload;virtual;
    /// <summary>Clears specific cached metadata by a key.</summary>
    /// <param>"Key" a resultset unique key value.</summary>
    procedure ClearCache(const Key: string); overload; virtual;

    // --> technobot 2008-06-14:  metadata cache key retrieval API:
    function GetTablesCacheKey(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): string;
    function GetSchemasCacheKey: string;
    function GetCatalogsCacheKey: string;
    function GetTableTypesCacheKey: string;
    function GetColumnsCacheKey(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): string;
    function GetColumnPrivilegesCacheKey(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): string;
    function GetTablePrivilegesCacheKey(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): string;
    function GetPrimaryKeysCacheKey(const Catalog: string; const Schema: string;
      const Table: string): string;
    function GetImportedKeysCacheKey(const Catalog: string; const Schema: string;
      const Table: string): string;
    function GetExportedKeysCacheKey(const Catalog: string; const Schema: string;
      const Table: string): string;
    function GetCrossReferenceCacheKey(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): string;
    function GetIndexInfoCacheKey(const Catalog: string; const Schema: string; const Table: string;
      const Unique: Boolean; const Approximate: Boolean): string;
    function GetSequencesCacheKey(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): string;
    function GetCollationAndCharSetCacheKey(const Catalog, SchemaPattern,
      TableNamePattern, ColumnPattern: String): string; //EgonHugeist
    function GetCharacterSetsCacheKey: String; //EgonHugeist
    function GetTriggersCacheKey(const Catalog: string;
      const SchemaPattern: string; const TableNamePattern: string;
      const TriggerNamePattern: string): string; //EgonHugeist
    function GetProceduresCacheKey(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): string;
    function GetProcedureColumnsCacheKey(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string): string;
    function GetBestRowIdentifierCacheKey(const Catalog: string; const Schema: string;
      const Table: string; const Scope: Integer; const Nullable: Boolean): string;
    function GetVersionColumnsCacheKey(const Catalog: string; const Schema: string;
      const Table: string): string;
    function GetTypeInfoCacheKey: string;
    function GetUDTsCacheKey(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): string;

    procedure GetCacheKeys(List: TStrings);
    property ConSettings: PZConSettings Read FConSettings write FConSettings;
    // <-- technobot 2008-06-14
  end;

  // technobot 2008-06-24 - methods moved as is from TZAbstractDatabaseMetadata:
  {** Implements Abstract Database Information. }
  TZAbstractDatabaseInfo = class(TInterfacedObject, IZDatabaseInfo)
  private
    FMetadata: TZAbstractDatabaseMetadata;
    FIdentifierQuoteKeywords: TStringList;
  protected
    FIdentifierQuotes: String;
    property Metadata: TZAbstractDatabaseMetadata read FMetadata;
  public
    constructor Create(const Metadata: TZAbstractDatabaseMetadata); overload;
    constructor Create(const Metadata: TZAbstractDatabaseMetadata;
      const IdentifierQuotes: String); overload;
    destructor Destroy; override;

    // database/driver/server info:
    /// <summary>What's the name of this database product?</summary>
    /// <returns>database product name</returns>
    function GetDatabaseProductName: string; virtual;
    /// <summary>What's the version of this database product?</summary>
    /// <returns>database version</returns>
    function GetDatabaseProductVersion: string; virtual;
    /// <summary>What's the name of this ZDBC driver?
    /// <returns>ZDBC driver name</returns>
    function GetDriverName: string; virtual;
    /// <summary>What's the version of this ZDBC driver?</summary>
    /// <returns>the ZDBC driver version as string.</returns>
    function GetDriverVersion: string; virtual;
    /// <summary>What's this ZDBC driver's major version number?</summary>
    /// <returns>ZDBC driver major version</returns>
    function GetDriverMajorVersion: Integer; virtual;
    /// <summary>What's this ZDBC driver's minor version number?</summary>
    /// <returns>The ZDBC driver minor version number as Integer.</returns>
    function GetDriverMinorVersion: Integer; virtual;
    /// <summary>What's the server version?</summary>
    /// <returns>The server version string.</returns>
    function GetServerVersion: string; virtual;

    // capabilities (what it can/cannot do):

    /// <summary>Can all the procedures returned by getProcedures be called by
    ///  the current user?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function AllProceduresAreCallable: Boolean; virtual;
    /// <summary>Can all the tables returned by getTable be SELECTed by the
    ///  current user?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function AllTablesAreSelectable: Boolean; virtual;
    /// <summary>Does the database treat mixed case unquoted SQL identifiers as
    ///  case sensitive and as a result store them in mixed case?
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsMixedCaseIdentifiers: Boolean; virtual;
    /// <summary>Does the database treat mixed case quoted SQL identifiers as
    ///  case sensitive and as a result store them in mixed case?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsMixedCaseQuotedIdentifiers: Boolean; virtual;
    /// <summary>Is "ALTER TABLE" with add column supported?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsAlterTableWithAddColumn: Boolean; virtual;
    /// <summary>Is "ALTER TABLE" with drop column supported?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsAlterTableWithDropColumn: Boolean; virtual;
    /// <summary>Is column aliasing supported? If so, the SQL AS clause can be
    ///  used to provide names for computed columns or to provide alias names
    ///  for columns as required.<summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsColumnAliasing: Boolean; virtual;
    /// <summary>Is the CONVERT function between SQL types supported?</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsConvert: Boolean; virtual;
    /// <summary>Not Yet implemented. Is CONVERT between the given SQL types
    ///  supported?</summary>
    /// <param><c>"FromType"</c> the type to convert from</param>
    /// <param><c>"ToType"</c> the type to convert to</param>
    /// <returns><c>true</c> if so; <c>false</c> otherwise.</returns>
    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
      Boolean; virtual;
    /// <summary>Are table correlation names supported?
    /// A Zeos Compliant <c>TM</c> driver always returns true.</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function SupportsTableCorrelationNames: Boolean; virtual;
    function SupportsDifferentTableCorrelationNames: Boolean; virtual;
    function SupportsExpressionsInOrderBy: Boolean; virtual;
    function SupportsOrderByUnrelated: Boolean; virtual;
    function SupportsGroupBy: Boolean; virtual;
    function SupportsGroupByUnrelated: Boolean; virtual;
    function SupportsGroupByBeyondSelect: Boolean; virtual;
    function SupportsLikeEscapeClause: Boolean; virtual;
    function SupportsMultipleResultSets: Boolean; virtual;
    function SupportsMultipleTransactions: Boolean; virtual;
    function SupportsNonNullableColumns: Boolean; virtual;
    function SupportsMinimumSQLGrammar: Boolean; virtual;
    function SupportsCoreSQLGrammar: Boolean; virtual;
    function SupportsExtendedSQLGrammar: Boolean; virtual;
    function SupportsANSI92EntryLevelSQL: Boolean; virtual;
    function SupportsANSI92IntermediateSQL: Boolean; virtual;
    function SupportsANSI92FullSQL: Boolean; virtual;
    function SupportsIntegrityEnhancementFacility: Boolean; virtual;
    function SupportsOuterJoins: Boolean; virtual;
    function SupportsFullOuterJoins: Boolean; virtual;
    function SupportsLimitedOuterJoins: Boolean; virtual;
    function SupportsSchemasInDataManipulation: Boolean; virtual;
    function SupportsSchemasInProcedureCalls: Boolean; virtual;
    function SupportsSchemasInTableDefinitions: Boolean; virtual;
    function SupportsSchemasInIndexDefinitions: Boolean; virtual;
    function SupportsSchemasInPrivilegeDefinitions: Boolean; virtual;
    function SupportsCatalogsInDataManipulation: Boolean; virtual;
    function SupportsCatalogsInProcedureCalls: Boolean; virtual;
    function SupportsCatalogsInTableDefinitions: Boolean; virtual;
    function SupportsCatalogsInIndexDefinitions: Boolean; virtual;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean; virtual;
    function SupportsOverloadPrefixInStoredProcedureName: Boolean; virtual;
    function SupportsParameterBinding: Boolean; virtual;
    function SupportsPositionedDelete: Boolean; virtual;
    function SupportsPositionedUpdate: Boolean; virtual;
    function SupportsSelectForUpdate: Boolean; virtual;
    function SupportsStoredProcedures: Boolean; virtual;
    function SupportsSubqueriesInComparisons: Boolean; virtual;
    function SupportsSubqueriesInExists: Boolean; virtual;
    function SupportsSubqueriesInIns: Boolean; virtual;
    function SupportsSubqueriesInQuantifieds: Boolean; virtual;
    function SupportsCorrelatedSubqueries: Boolean; virtual;
    function SupportsUnion: Boolean; virtual;
    function SupportsUnionAll: Boolean; virtual;
    function SupportsOpenCursorsAcrossCommit: Boolean; virtual;
    function SupportsOpenCursorsAcrossRollback: Boolean; virtual;
    function SupportsOpenStatementsAcrossCommit: Boolean; virtual;
    function SupportsOpenStatementsAcrossRollback: Boolean; virtual;
    function SupportsTransactions: Boolean; virtual;
    function SupportsTransactionIsolationLevel(const {%H-}Level: TZTransactIsolationLevel):
      Boolean; virtual;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; virtual;
    function SupportsDataManipulationTransactionsOnly: Boolean; virtual;
    function SupportsResultSetType(const {%H-}_Type: TZResultSetType): Boolean; virtual;
    function SupportsResultSetConcurrency(const {%H-}_Type: TZResultSetType;
      const {%H-}Concurrency: TZResultSetConcurrency): Boolean; virtual;
    function SupportsBatchUpdates: Boolean; virtual;
    function SupportsNonEscapedSearchStrings: Boolean; virtual;
    function SupportsMilliSeconds: Boolean; virtual;
    function SupportsUpdateAutoIncrementFields: Boolean; virtual;
    function SupportsArrayBindings: Boolean; virtual;

    // maxima:
    function GetMaxBinaryLiteralLength: Integer; virtual;
    function GetMaxCharLiteralLength: Integer; virtual;
    function GetMaxColumnNameLength: Integer; virtual;
    function GetMaxColumnsInGroupBy: Integer; virtual;
    function GetMaxColumnsInIndex: Integer; virtual;
    function GetMaxColumnsInOrderBy: Integer; virtual;
    function GetMaxColumnsInSelect: Integer; virtual;
    function GetMaxColumnsInTable: Integer; virtual;
    function GetMaxConnections: Integer; virtual;
    function GetMaxCursorNameLength: Integer; virtual;
    function GetMaxIndexLength: Integer; virtual;
    function GetMaxSchemaNameLength: Integer; virtual;
    function GetMaxProcedureNameLength: Integer; virtual;
    function GetMaxCatalogNameLength: Integer; virtual;
    function GetMaxRowSize: Integer; virtual;
    function GetMaxStatementLength: Integer; virtual;
    function GetMaxStatements: Integer; virtual;
    function GetMaxTableNameLength: Integer; virtual;
    function GetMaxTablesInSelect: Integer; virtual;
    function GetMaxUserNameLength: Integer; virtual;

    // policies (how are various data and operations handled):
    function IsReadOnly: Boolean; virtual;
    function IsCatalogAtStart: Boolean; virtual;
    function DoesMaxRowSizeIncludeBlobs: Boolean; virtual;
    function NullsAreSortedHigh: Boolean; virtual;
    function NullsAreSortedLow: Boolean; virtual;
    function NullsAreSortedAtStart: Boolean; virtual;
    function NullsAreSortedAtEnd: Boolean; virtual;
    function NullPlusNonNullIsNull: Boolean; virtual;
    function UsesLocalFiles: Boolean; virtual;
    function UsesLocalFilePerTable: Boolean; virtual;
    function StoresUpperCaseIdentifiers: Boolean; virtual;
    function StoresLowerCaseIdentifiers: Boolean; virtual;
    function StoresMixedCaseIdentifiers: Boolean; virtual;
    function StoresUpperCaseQuotedIdentifiers: Boolean; virtual;
    function StoresLowerCaseQuotedIdentifiers: Boolean; virtual;
    function StoresMixedCaseQuotedIdentifiers: Boolean; virtual;
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; virtual;
    function DataDefinitionCausesTransactionCommit: Boolean; virtual;
    function DataDefinitionIgnoredInTransactions: Boolean; virtual;

    // interface details (terms, keywords, etc):
    function GetIdentifierQuoteString: string;
    function GetSchemaTerm: string; virtual;
    function GetProcedureTerm: string; virtual;
    function GetCatalogTerm: string; virtual;
    function GetCatalogSeparator: string; virtual;
    function GetSQLKeywords: string; virtual;
    function GetIdentifierQuoteKeywordsSorted: TStringList;
    function GetNumericFunctions: string; virtual;
    function GetStringFunctions: string; virtual;
    function GetSystemFunctions: string; virtual;
    function GetTimeDateFunctions: string; virtual;
    function GetSearchStringEscape: string; virtual;
    function GetExtraNameCharacters: string; virtual;
  end;

  {** Implements a default Case Sensitive/Unsensitive identifier Converter. }
  TZDefaultIdentifierConverter = class (TZAbstractObject,
    IZIdentifierConverter)
  private
    FMetadata: Pointer;
    /// <summary>Get the underlaying IZMetadata interface.</summary>
    /// <returns>the owner IZMetadata interface.</returns>
    function GetMetaData: IZDatabaseMetadata;
  protected
    property Metadata: IZDatabaseMetadata read GetMetaData;
    /// <summary>Checks is the specified string in lower case.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <returns><c>True</c> if the identifier string is lower case;<c>False</c>
    ///  otherwise.</returns>
    function IsLowerCase(const Value: string): Boolean;
    /// <summary>Checks is the specified string in upper case.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <returns><c>True</c> if the identifier string is upper case;<c>False</c>
    ///  otherwise.</returns>
    function IsUpperCase(const Value: string): Boolean;
    /// <summary>Checks is the specified string in special case.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <returns><c>True</c> if the identifier string is special case;
    ///  <c>False</c> otherwise.</returns>
    function IsSpecialCase(const Value: string): Boolean; virtual;
  public
    /// <summary>Constructs this default identifier Converter object.</summary>
    /// <param>"Metadata" the owner database metadata interface.</param>
    constructor Create(const Metadata: IZDatabaseMetadata);

    /// <author>FrOsT</author>
    /// <summary>Get the indentifier case.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <param>"TestKeyWords" indicate if reserved Keywords should be compared.</param>
    /// <returns>on of the following:
    ///  icNone - just numbers starting with a underscore found,
    ///  icLower - the indentifier is lower case,
    ///  icUpper - the indentifier is upper case,
    ///  icMixed - the indentifier is mixed case,
    ///  icSpecial - the identifier is a reserved keyword or contains a
    ///    character not matching '0'..'9', 'a'..'z' and 'A'..'Z'.</returns>
    function GetIdentifierCase(const Value: String; TestKeyWords: Boolean): TZIdentifierCase;
    /// <summary>Checks is the string case sensitive.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <returns><c>True</c> if the identifier string is case sensitive;
    ///  <c>False</c> otherwise.</returns>
    function IsCaseSensitive(const Value: string): Boolean;
    /// <summary>Checks is the string quoted.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <return><c>True</c> if the identifier string is case quoted;
    ///  <c>False</c> otherwise.</returns>
    function IsQuoted(const Value: string): Boolean; virtual;
    /// <summary>Quotes the identifier string.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <param>"Qualifier" an identifier qualifier. Default is <c>iqUnspecified</c>.</param>
    /// <returns>a quoted string.</returns>
    function Quote(const Value: string; Qualifier: TZIdentifierQualifier = iqUnspecified): string; virtual;
    /// <summary>Extracts the quote from the idenfitier string.</summary>
    /// <param>"Value" an identifier string.</param>
    /// <returns>an extracted and processed string.</returns>
    function ExtractQuote(const Value: string): string; virtual;
  end;
  TZDefaultIdentifierConvertor = TZDefaultIdentifierConverter; //keep that alias for compatibility

  function GetTablesMetaDataCacheKey(Const Catalog:String;
      Const SchemaPattern:String;Const TableNamePattern:String;const Types: TStringDynArray):String;
      deprecated; // (technobot) use TZAbstractDatabaseMetadata.GetTablesCacheKey instead

const
  CatalogNameIndex = FirstDbcIndex + 0;
  SchemaNameIndex  = FirstDbcIndex + 1;
  TableNameIndex   = FirstDbcIndex + 2;
  ColumnNameIndex  = FirstDbcIndex + 3;

const
  CollationAndCharSetNameIndex = FirstDbcIndex + 5;
var
  CharacterSetsColumnsDynArray: TZMetadataColumnDefs;
const
  CharacterSetsNameIndex = FirstDbcIndex + 0;
  CharacterSetsIDIndex   = FirstDbcIndex + 1;
var
  CollationCharSetColumnsDynArray: TZMetadataColumnDefs;
const
  CollationNameIndex    = FirstDbcIndex + 4;
  CharacterSetNameIndex = FirstDbcIndex + 5;
  CharacterSetIDIndex   = FirstDbcIndex + 6;
  CharacterSetSizeIndex = FirstDbcIndex + 7;
var
  TriggersColumnsDynArray: TZMetadataColumnDefs;
const
  TrgColTriggerNameIndex     = FirstDbcIndex + 2;
  TrgColRelationNameIndex    = FirstDbcIndex + 3;
  TrgColTriggerTypeIndex     = FirstDbcIndex + 4;
  TrgColTriggerInactiveIndex = FirstDbcIndex + 5;
  TrgColTriggerSourceIndex   = FirstDbcIndex + 6;
  TrgColDescriptionIndex     = FirstDbcIndex + 7;
const
  ProcedureNameIndex       = FirstDbcIndex + 2;
  ProcedureOverloadIndex   = FirstDbcIndex + 3;
  ProcedureReserved1Index  = FirstDbcIndex + 4;
  ProcedureReserved2Index  = FirstDbcIndex + 5;
  ProcedureRemarksIndex    = FirstDbcIndex + 6;
  ProcedureTypeIndex       = FirstDbcIndex + 7;
type
  TProcedureMap = record
    Initilized: Boolean;
    ColIndices: array[CatalogNameIndex..ProcedureTypeIndex] of ShortInt
  end;
var
  ProceduresColumnsDynArray: TZMetadataColumnDefs;
const
  ProcColProcedureNameIndex = FirstDbcIndex + 2;
  ProcColColumnNameIndex    = FirstDbcIndex + 3;
  ProcColColumnTypeIndex    = FirstDbcIndex + 4;
  ProcColDataTypeIndex      = FirstDbcIndex + 5;
  ProcColTypeNameIndex      = FirstDbcIndex + 6;
  ProcColPrecisionIndex     = FirstDbcIndex + 7;
  ProcColLengthIndex        = FirstDbcIndex + 8;
  ProcColScaleIndex         = FirstDbcIndex + 9;
  ProcColRadixIndex         = FirstDbcIndex + 10;
  ProcColNullableIndex      = FirstDbcIndex + 11;
  ProcColRemarksIndex       = FirstDbcIndex + 12;
type
  TProcedureColumnsColMap = record
    Initilized: Boolean;
    ColIndices: array[CatalogNameIndex..ProcColRemarksIndex] of ShortInt;
  end;
var
  ProceduresColColumnsDynArray: TZMetadataColumnDefs;
const
  TableColumnsSQLType = FirstDbcIndex + 3;
  TableColumnsRemarks = FirstDbcIndex + 4;
var
  TableColumnsDynArray: TZMetadataColumnDefs;
const
  SchemaColumnsTableSchemaIndex = FirstDbcIndex;
var
  SchemaColumnsDynArray: TZMetadataColumnDefs;
  CatalogColumnsDynArray: TZMetadataColumnDefs;
const
  TableTypeColumnTableTypeIndex = FirstDbcIndex;
var
  TableTypeColumnsDynArray: TZMetadataColumnDefs;
const
  TableColColumnTypeIndex               = FirstDbcIndex + 4;
  TableColColumnTypeNameIndex           = FirstDbcIndex + 5;
  TableColColumnSizeIndex               = FirstDbcIndex + 6;
  TableColColumnBufLengthIndex          = FirstDbcIndex + 7; //unused as documented
  TableColColumnDecimalDigitsIndex      = FirstDbcIndex + 8;
  TableColColumnNumPrecRadixIndex       = FirstDbcIndex + 9;
  TableColColumnNullableIndex           = FirstDbcIndex + 10;
  TableColColumnRemarksIndex            = FirstDbcIndex + 11;
  TableColColumnColDefIndex             = FirstDbcIndex + 12;
  TableColColumnSQLDataTypeIndex        = FirstDbcIndex + 13;
  TableColColumnSQLDateTimeSubIndex     = FirstDbcIndex + 14;
  TableColColumnCharOctetLengthIndex    = FirstDbcIndex + 15;
  TableColColumnOrdPosIndex             = FirstDbcIndex + 16;
  TableColColumnIsNullableIndex         = FirstDbcIndex + 17;
  TableColColumnAutoIncIndex            = FirstDbcIndex + 18;
  TableColColumnCaseSensitiveIndex      = FirstDbcIndex + 19;
  TableColColumnSearchableIndex         = FirstDbcIndex + 20;
  TableColColumnWritableIndex           = FirstDbcIndex + 21;
  TableColColumnDefinitelyWritableIndex = FirstDbcIndex + 22;
  TableColColumnReadonlyIndex           = FirstDbcIndex + 23;
type
  TTableColColumnMap = record
    Initilized: Boolean;
    ColIndices: array[CatalogNameIndex..TableColColumnReadonlyIndex] of ShortInt;
  end;
var
  TableColColumnsDynArray: TZMetadataColumnDefs;
const
  TableColPrivGrantorIndex     = FirstDbcIndex + 4;
  TableColPrivGranteeIndex     = FirstDbcIndex + 5;
  TableColPrivPrivilegeIndex   = FirstDbcIndex + 6;
  TableColPrivIsGrantableIndex = FirstDbcIndex + 7;
type
  TTableColPrivMap = record
    Initilized: Boolean;
    ColIndices: array[CatalogNameIndex..TableColPrivIsGrantableIndex] of ShortInt;
  end;
var
  TableColPrivColumnsDynArray: TZMetadataColumnDefs;
const
  TablePrivGrantorIndex     = FirstDbcIndex + 3;
  TablePrivGranteeIndex     = FirstDbcIndex + 4;
  TablePrivPrivilegeIndex   = FirstDbcIndex + 5;
  TablePrivIsGrantableIndex = FirstDbcIndex + 6;
type
  TTablePrivMap = record
    Initilized: Boolean;
    ColIndices: array[CatalogNameIndex..TablePrivIsGrantableIndex] of ShortInt;
  end;
var
  TablePrivColumnsDynArray: TZMetadataColumnDefs;
const
  BestRowIdentScopeIndex         = FirstDbcIndex + 0;
  BestRowIdentColNameIndex       = FirstDbcIndex + 1;
  BestRowIdentDataTypeIndex      = FirstDbcIndex + 2;
  BestRowIdentTypeNameIndex      = FirstDbcIndex + 3;
  BestRowIdentColSizeIndex       = FirstDbcIndex + 4;
  BestRowIdentBufLengthIndex     = FirstDbcIndex + 5;
  BestRowIdentDecimalDigitsIndex = FirstDbcIndex + 6;
  BestRowIdentPseudoColumnIndex  = FirstDbcIndex + 7;
var
  BestRowIdentColumnsDynArray: TZMetadataColumnDefs;
const
  TableColVerScopeIndex         = FirstDbcIndex + 0;
  TableColVerColNameIndex       = FirstDbcIndex + 1;
  TableColVerDataTypeIndex      = FirstDbcIndex + 2;
  TableColVerTypeNameIndex      = FirstDbcIndex + 3;
  TableColVerColSizeIndex       = FirstDbcIndex + 4;
  TableColVerBufLengthIndex     = FirstDbcIndex + 5;
  TableColVerDecimalDigitsIndex = FirstDbcIndex + 6;
  TableColVerPseudoColumnIndex  = FirstDbcIndex + 7;
var
  TableColVerColumnsDynArray: TZMetadataColumnDefs;
const
  PrimaryKeyColumnNameIndex = FirstDbcIndex + 3;
  PrimaryKeyKeySeqIndex     = FirstDbcIndex + 4;
  PrimaryKeyPKNameIndex     = FirstDbcIndex + 5;
var
  PrimaryKeyColumnsDynArray: TZMetadataColumnDefs;
const
  ImportedKeyColPKTableCatalogIndex = FirstDbcIndex + 0;
  ImportedKeyColPKTableSchemaIndex  = FirstDbcIndex + 1;
  ImportedKeyColPKTableNameIndex    = FirstDbcIndex + 2;
  ImportedKeyColPKColumnNameIndex   = FirstDbcIndex + 3;
  ImportedKeyColFKTableCatalogIndex = FirstDbcIndex + 4;
  ImportedKeyColFKTableSchemaIndex  = FirstDbcIndex + 5;
  ImportedKeyColFKTableNameIndex    = FirstDbcIndex + 6;
  ImportedKeyColFKColumnNameIndex   = FirstDbcIndex + 7;
  ImportedKeyColKeySeqIndex         = FirstDbcIndex + 8;
  ImportedKeyColUpdateRuleIndex     = FirstDbcIndex + 9;
  ImportedKeyColDeleteRuleIndex     = FirstDbcIndex + 10;
  ImportedKeyColFKNameIndex         = FirstDbcIndex + 11;
  ImportedKeyColPKNameIndex         = FirstDbcIndex + 12;
  ImportedKeyColDeferrabilityIndex  = FirstDbcIndex + 13;
var
  ImportedKeyColumnsDynArray: TZMetadataColumnDefs;
const
  ExportedKeyColPKTableCatalogIndex = FirstDbcIndex + 0;
  ExportedKeyColPKTableSchemaIndex  = FirstDbcIndex + 1;
  ExportedKeyColPKTableNameIndex    = FirstDbcIndex + 2;
  ExportedKeyColPKColumnNameIndex   = FirstDbcIndex + 3;
  ExportedKeyColFKTableCatalogIndex = FirstDbcIndex + 4;
  ExportedKeyColFKTableSchemaIndex  = FirstDbcIndex + 5;
  ExportedKeyColFKTableNameIndex    = FirstDbcIndex + 6;
  ExportedKeyColFKColumnNameIndex   = FirstDbcIndex + 7;
  ExportedKeyColKeySeqIndex         = FirstDbcIndex + 8;
  ExportedKeyColUpdateRuleIndex     = FirstDbcIndex + 9;
  ExportedKeyColDeleteRuleIndex     = FirstDbcIndex + 10;
  ExportedKeyColFKNameIndex         = FirstDbcIndex + 11;
  ExportedKeyColPKNameIndex         = FirstDbcIndex + 12;
  ExportedKeyColDeferrabilityIndex  = FirstDbcIndex + 13;
var
  ExportedKeyColumnsDynArray: TZMetadataColumnDefs;
const
  CrossRefKeyColPKTableCatalogIndex = FirstDbcIndex + 0;
  CrossRefKeyColPKTableSchemaIndex  = FirstDbcIndex + 1;
  CrossRefKeyColPKTableNameIndex    = FirstDbcIndex + 2;
  CrossRefKeyColPKColumnNameIndex   = FirstDbcIndex + 3;
  CrossRefKeyColFKTableCatalogIndex = FirstDbcIndex + 4;
  CrossRefKeyColFKTableSchemaIndex  = FirstDbcIndex + 5;
  CrossRefKeyColFKTableNameIndex    = FirstDbcIndex + 6;
  CrossRefKeyColFKColumnNameIndex   = FirstDbcIndex + 7;
  CrossRefKeyColKeySeqIndex         = FirstDbcIndex + 8;
  CrossRefKeyColUpdateRuleIndex     = FirstDbcIndex + 9;
  CrossRefKeyColDeleteRuleIndex     = FirstDbcIndex + 10;
  CrossRefKeyColFKNameIndex         = FirstDbcIndex + 11;
  CrossRefKeyColPKNameIndex         = FirstDbcIndex + 12;
  CrossRefKeyColDeferrabilityIndex  = FirstDbcIndex + 13;
type
  TCrossRefKeyCol = record
    Initilized: Boolean;
    ColIndices: array[CrossRefKeyColPKTableCatalogIndex..CrossRefKeyColDeferrabilityIndex] of ShortInt;
  end;
var
  CrossRefColumnsDynArray: TZMetadataColumnDefs;
const
  TypeInfoTypeNameIndex          = FirstDbcIndex + 0;
  TypeInfoDataTypeIndex          = FirstDbcIndex + 1;
  TypeInfoPecisionIndex          = FirstDbcIndex + 2;
  TypeInfoLiteralPrefixIndex     = FirstDbcIndex + 3;
  TypeInfoLiteralSuffixIndex     = FirstDbcIndex + 4;
  TypeInfoCreateParamsIndex      = FirstDbcIndex + 5;
  TypeInfoNullAbleIndex          = FirstDbcIndex + 6;
  TypeInfoCaseSensitiveIndex     = FirstDbcIndex + 7;
  TypeInfoSearchableIndex        = FirstDbcIndex + 8;
  TypeInfoUnsignedAttributeIndex = FirstDbcIndex + 9;
  TypeInfoFixedPrecScaleIndex    = FirstDbcIndex + 10;
  TypeInfoAutoIncrementIndex     = FirstDbcIndex + 11;
  TypeInfoLocaleTypeNameIndex    = FirstDbcIndex + 12;
  TypeInfoMinimumScaleIndex      = FirstDbcIndex + 13;
  TypeInfoMaximumScaleIndex      = FirstDbcIndex + 14;
  TypeInfoSQLDataTypeIndex       = FirstDbcIndex + 15;
  TypeInfoSQLDateTimeSubIndex    = FirstDbcIndex + 16;
  TypeInfoNumPrecRadix           = FirstDbcIndex + 17;
var
  TypeInfoColumnsDynArray: TZMetadataColumnDefs;
const
  IndexInfoColNonUniqueIndex       = FirstDbcIndex + 3;
  IndexInfoColIndexQualifierIndex  = FirstDbcIndex + 4;
  IndexInfoColIndexNameIndex       = FirstDbcIndex + 5;
  IndexInfoColTypeIndex            = FirstDbcIndex + 6;
  IndexInfoColOrdPositionIndex     = FirstDbcIndex + 7;
  IndexInfoColColumnNameIndex      = FirstDbcIndex + 8;
  IndexInfoColAscOrDescIndex       = FirstDbcIndex + 9;
  IndexInfoColCardinalityIndex     = FirstDbcIndex + 10;
  IndexInfoColPagesIndex           = FirstDbcIndex + 11;
  IndexInfoColFilterConditionIndex = FirstDbcIndex + 12;
type
  TIndexInfoMap = record
    Initilized: Boolean;
    ColIndices: array[CatalogNameIndex..IndexInfoColFilterConditionIndex] of ShortInt;
  end;
var
  IndexInfoColumnsDynArray: TZMetadataColumnDefs;
const
  SequenceNameIndex = FirstDbcIndex + 2;
var
  SequenceColumnsDynArray: TZMetadataColumnDefs;
const
  UDTColTypeNameIndex  = FirstDbcIndex + 2;
  UDTColClassNameIndex = FirstDbcIndex + 3;
  UDTColDataTypeIndex  = FirstDbcIndex + 4;
  UDTColRemarksIndex   = FirstDbcIndex + 5;
var
  UDTColumnsDynArray: TZMetadataColumnDefs;

implementation

uses ZFastCode, ZVariant, ZMessages, ZEncoding,
  ZDbcProperties, ZDbcUtils, ZExceptions;

{ TZAbstractDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZAbstractDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  Create(MetaData, '"');
end;

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
  @param IdentifierQuotes
    What's the string used to quote SQL identifiers?
    This returns a space " " if identifier quoting isn't supported.
    A JDBC Compliant<sup><font size=-2>TM</font></sup>
    driver always uses a double quote character.
}
constructor TZAbstractDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata;
  const IdentifierQuotes: String);
begin
  inherited Create;
  FMetadata := Metadata;
  if FMetaData.FUrl.Properties.IndexOfName(ConnProps_IdentifierQuotes) > -1 then //prevent to loose emty quotes '' !!!
    FIdentifierQuotes := FMetaData.FUrl.Properties.Values[ConnProps_IdentifierQuotes]
  else
    if IdentifierQuotes = '' then
      FIdentifierQuotes := '"'
    else
      FIdentifierQuotes := IdentifierQuotes;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractDatabaseInfo.Destroy;
begin
  FMetadata := nil;
  FreeAndNil(FIdentifierQuoteKeywords);
  inherited;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

function TZAbstractDatabaseInfo.AllProceduresAreCallable: Boolean;
begin
  Result := True;
end;

function TZAbstractDatabaseInfo.AllTablesAreSelectable: Boolean;
begin
  Result := True;
end;

{**
  Is the database in read-only mode?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.IsReadOnly: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted high?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.NullsAreSortedHigh: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted low?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.NullsAreSortedLow: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted at the start regardless of sort order?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.NullsAreSortedAtStart: Boolean;
begin
  Result := False;
end;

{**
  Are NULL values sorted at the end regardless of sort order?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.NullsAreSortedAtEnd: Boolean;
begin
  Result := False;
end;

function TZAbstractDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := '';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZAbstractDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

function TZAbstractDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver';
end;

{**
  What's the version of this JDBC driver?
  @return JDBC driver version
}
function TZAbstractDatabaseInfo.GetDriverVersion: string;
begin
  Result := Format('%d.%d', [GetDriverMajorVersion, GetDriverMinorVersion]);
end;

function TZAbstractDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

function TZAbstractDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

function TZAbstractDatabaseInfo.GetServerVersion: string;
begin
  Result := '';
end;

{**
  Does the database store tables in a local file?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.UsesLocalFiles: Boolean;
begin
  Result := True;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZAbstractDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

function TZAbstractDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

function TZAbstractDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

function TZAbstractDatabaseInfo.GetIdentifierQuoteKeywordsSorted: TStringList;
const
  SQL92Keywords = 'insert,update,delete,select,drop,create,for,from,set,values,'
    + 'where,order,group,by,having,into,as,table,index,primary,key,on,is,null,'
    + 'char,varchar,integer,number,alter,column,value,values,'
    + 'current,top,login,status,version';

  procedure Append(const Values: String; Dest: TStrings);
  begin
    if StoresUpperCaseIdentifiers
      then ZSysUtils.AppendSplitString(Dest, UpperCase(Values), ',')
      else ZSysUtils.AppendSplitString(Dest, LowerCase(Values), ',');
  end;

begin
  if FIdentifierQuoteKeywords = nil then
  begin
    FIdentifierQuoteKeywords := TStringList.Create;
    FIdentifierQuoteKeywords.Sorted := True;
    FIdentifierQuoteKeywords.Duplicates := dupIgnore;
    Append(SQL92Keywords, FIdentifierQuoteKeywords);
    Append(GetSQLKeyWords, FIdentifierQuoteKeywords);
    Append(GetNumericFunctions, FIdentifierQuoteKeywords);
    Append(GetStringFunctions, FIdentifierQuoteKeywords);
    Append(GetSystemFunctions, FIdentifierQuoteKeywords);
    Append(GetTimeDateFunctions, FIdentifierQuoteKeywords);
  end;
  Result := FIdentifierQuoteKeywords;
end;

{**
  What's the string used to quote SQL identifiers?
  This returns a space " " if identifier quoting isn't supported.
  A JDBC Compliant<sup><font size=-2>TM</font></sup>
  driver always uses a double quote character.
  @return the quoting string
}
function TZAbstractDatabaseInfo.GetIdentifierQuoteString: string;
begin
  Result := FIdentifierQuotes;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZAbstractDatabaseInfo.GetSQLKeywords: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractDatabaseInfo.GetNumericFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractDatabaseInfo.GetStringFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractDatabaseInfo.GetSystemFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZAbstractDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := '';
end;

{**
  Gets the string that can be used to escape wildcard characters.
  This is the string that can be used to escape '_' or '%' in
  the string pattern style catalog search parameters.

  <P>The '_' character represents any single character.
  <P>The '%' character represents any sequence of zero or
  more characters.

  @return the string used to escape wildcard characters
}
function TZAbstractDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '%';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZAbstractDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

function TZAbstractDatabaseInfo.SupportsAlterTableWithAddColumn: Boolean;
begin
  Result := True;
end;

function TZAbstractDatabaseInfo.SupportsAlterTableWithDropColumn: Boolean;
begin
  Result := True;
end;

function TZAbstractDatabaseInfo.SupportsColumnAliasing: Boolean;
begin
  Result := True;
end;

{**
  Are concatenations between NULL and non-NULL values NULL?
  For SQL-92 compliance, a JDBC technology-enabled driver will
  return <code>true</code>.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.NullPlusNonNullIsNull: Boolean;
begin
  Result := True;
end;

function TZAbstractDatabaseInfo.SupportsConvert: Boolean;
begin
  Result := False;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF}
function TZAbstractDatabaseInfo.SupportsConvertForTypes(
  FromType: TZSQLType; ToType: TZSQLType): Boolean;
begin
  Result := False;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractDatabaseInfo.SupportsTableCorrelationNames: Boolean;
begin
  Result := True;
end;

{**
  If table correlation names are supported, are they restricted
  to be different from the names of the tables?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsDifferentTableCorrelationNames: Boolean;
begin
  Result := False;
end;

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := False;
end;

{**
  Is the escape character in "LIKE" clauses supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsLikeEscapeClause: Boolean;
begin
  Result := True;
end;

{**
  Are multiple <code>ResultSet</code> from a single execute supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsMultipleResultSets: Boolean;
begin
  Result := True;
end;

{**
  Can we have multiple transactions open at once (on different
  connections)?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsMultipleTransactions: Boolean;
begin
  Result := True;
end;

{**
  Can columns be defined as non-nullable?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsNonNullableColumns: Boolean;
begin
  Result := True;
end;

{**
  Is the ODBC Minimum SQL grammar supported?
  All JDBC Compliant<sup><font size=-2>TM</font></sup> drivers must return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsMinimumSQLGrammar: Boolean;
begin
  Result := True;
end;

{**
  Is the ODBC Core SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsCoreSQLGrammar: Boolean;
begin
  Result := True;
end;

{**
  Is the ODBC Extended SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsExtendedSQLGrammar: Boolean;
begin
  Result := True;
end;

{**
  Is the ANSI92 entry level SQL grammar supported?
  All JDBC Compliant<sup><font size=-2>TM</font></sup> drivers must return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsANSI92EntryLevelSQL: Boolean;
begin
  Result := True;
end;

{**
  Is the ANSI92 intermediate SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsANSI92IntermediateSQL: Boolean;
begin
  Result := True;
end;

{**
  Is the ANSI92 full SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsANSI92FullSQL: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  Is some form of outer join supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsOuterJoins: Boolean;
begin
  Result := True;
end;

{**
  Are full nested outer joins supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsFullOuterJoins: Boolean;
begin
  Result := True;
end;

{**
  Is there limited support for outer joins?  (This will be true
  if supportFullOuterJoins is true.)
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsLimitedOuterJoins: Boolean;
begin
  Result := True;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZAbstractDatabaseInfo.GetSchemaTerm: string;
begin
  Result := 'Schema';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZAbstractDatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'Procedure';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZAbstractDatabaseInfo.GetCatalogTerm: string;
begin
  Result := 'Catalog';
end;

{**
  Does a catalog appear at the start of a qualified table name?
  (Otherwise it appears at the end)
  @return true if it appears at the start
}
function TZAbstractDatabaseInfo.IsCatalogAtStart: Boolean;
begin
  Result := False;
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZAbstractDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a stored procedure have an additional overload suffix?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsOverloadPrefixInStoredProcedureName: Boolean;
begin
  Result := False;
end;

{**
  Is parameter bindings supported by Provider?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsParameterBinding: Boolean;
begin
  Result := True;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := False;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := False;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := False;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := False;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := False;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := False;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := True;
end;

//----------------------------------------------------------------------
// The following group of methods exposes various limitations
// based on the target database with the current driver.
// Unless otherwise specified, a result of zero means there is no
// limit, or the limit is not known.

{**
  How many hex characters can you have in an inline binary literal?
  @return max binary literal length in hex characters;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 0;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 0;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 0;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 0;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 0;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZAbstractDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZAbstractDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  Result := True;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsResultSetType(
  const _Type: TZResultSetType): Boolean;
begin
  Result := True;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsResultSetConcurrency(
  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := True;
end;

{**
  Indicates whether the driver supports batch updates.
  @return true if the driver supports batch updates; false otherwise
}
function TZAbstractDatabaseInfo.SupportsBatchUpdates: Boolean;
begin
  Result := True;
end;

{**
  Does the Database or Actual Version understand non escaped search strings?
  @return <code>true</code> if the DataBase does understand non escaped
  search strings
}
function TZAbstractDatabaseInfo.SupportsNonEscapedSearchStrings: Boolean;
begin
  Result := False;
end;

{**
  Does the database driver supports milliseconds?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractDatabaseInfo.SupportsMilliSeconds: Boolean;
begin
  Result := True;
end;

{**
  Does the Database support updating auto incremental fields?
  @return <code>true</code> if the DataBase allows it.
}
function TZAbstractDatabaseInfo.SupportsUpdateAutoIncrementFields: Boolean;
begin
  Result := True;
end;

{**
  Does the Database support binding arrays? Is the ZDbc ready for this?
  @return <code>true</code> if the DataBase allows it.
}
function TZAbstractDatabaseInfo.SupportsArrayBindings: Boolean;
begin
  Result := False;
end;

{ TZAbstractDatabaseMetadata }

constructor TZAbstractDatabaseMetadata.Create(Connection: TZAbstractDbcConnection;
  const Url: TZURL);
begin
  inherited Create(Connection as IZConnection);
  FIC := Self.GetIdentifierConverter;
  FConnection := Pointer(Connection as IZConnection);
  FUrl := Url;
  FCachedResultSets := TZKeyAndResultSetPairList.Create(SizeOf(TZKeyAndResultSetPair), True);
  FDatabaseInfo := nil;
  FDatabase := Url.Database;
  FConSettings := IZConnection(FConnection).GetConSettings;
  FillWildcards;
end;

function TZAbstractDatabaseMetadata.GetInfo: TStrings;
begin
  Result := FURL.Properties;
end;

function TZAbstractDatabaseMetadata.GetURLString: String;
begin
  Result := FURL.URL;
end;

{**
   Remove escapes from pattren string
   @param Pattern a sql pattern
   @return string without escapes
}
function TZAbstractDatabaseMetadata.StripEscape(const Pattern: string): string;
var
  L: Integer;
  PreviousChar, EscapeChar: Char;
  pPat, pPatEnd, pRes: PChar;
begin
  PreviousChar := #0;
  L := Length(Pattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Result := '';{$ENDIF}
  SetLength(Result, L);
  pRes := Pointer(GetDatabaseInfo.GetSearchStringEscape);
  if (L = 0) or (pRes = nil) then Exit;
  EscapeChar := pRes^;
  pRes := Pointer(Result);
  pPat := Pointer(Pattern);
  pPatEnd := pPat+L;
  while pPat < pPatEnd do begin
    if (pPat^ <> EscapeChar) then begin
      PreviousChar := pPat^;
      pRes^ := PreviousChar;
      Inc(pRes);
    end else if (PreviousChar = EscapeChar) then begin
      pRes^ := PreviousChar;
      PreviousChar := #0;
      Inc(pRes);
    end else
      PreviousChar := pPat^;
    Inc(pPat);
  end;
  pPat := Pointer(Result);
  if (pRes-pPat) <> L then
    SetLength(Result, (pRes-pPat));
end;

procedure TZAbstractDatabaseMetadata.ToBuf(C: Char; var Value: String);
begin
  if fCurrentBufIndex < High(Byte) then begin
    fBuf[fCurrentBufIndex] := C;
    Inc(fCurrentBufIndex);
  end else begin
    FlushBuf(Value);
    InitBuf(C);
  end;
end;

function TZAbstractDatabaseMetadata.HasKey(const Key: String): Boolean;
var I: Integer;
  KeyAndResultSetPair: PZKeyAndResultSetPair;
begin
  for i := 0 to FCachedResultSets.Count -1 do begin
    KeyAndResultSetPair := FCachedResultSets.Get(i);
    if KeyAndResultSetPair.Key = Key then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

{**
   Check if pattern does not contain wildcards
   @param Pattern a sql pattern
   @return if pattern contain wildcards return true otherwise false
}
function TZAbstractDatabaseMetadata.HasNoWildcards(const Pattern: string): boolean;
var
  PreviousCharWasEscape: Boolean;
  EscapeChar,PreviousChar: Char;
  WildcardsSet: TZWildcardsSet;
  P, PEnd: PChar;
  {$IFDEF TSYSCHARSET_IS_DEPRECATED}
  function CharInSet(C: Char; const CharSet: TZWildcardsSet): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := Low(CharSet) to High(CharSet) do
      if CharSet[i] = C then begin
        Result := True;
        Break;
      end;
  end;
  {$ENDIF}
begin
  Result := False;
  PreviousChar := #0;
  PreviousCharWasEscape := False;
  P := Pointer(GetDatabaseInfo.GetSearchStringEscape);
  EscapeChar := P^;
  WildcardsSet := GetWildcardsSet;
  P := Pointer(Pattern);
  PEnd := P+Length(Pattern);
  while P<PEnd do begin
    if (not PreviousCharWasEscape) and CharInset(P^, WildcardsSet) then
     Exit;
    PreviousCharWasEscape := (P^ = EscapeChar) and (PreviousChar <> EscapeChar);
    if (PreviousCharWasEscape) and (P^ = EscapeChar)
    then PreviousChar := #0
    else PreviousChar := P^;
    Inc(P);
  end;
  Result := True;
end;

procedure TZAbstractDatabaseMetadata.InitBuf(FirstChar: Char);
begin
  fBuf[0] := FirstChar;
  fCurrentBufIndex := 1;
end;

function TZAbstractDatabaseMetadata.EscapeString(const S: string): string;
begin
  Result := '''' + S + '''';
end;

{**
  Decomposes a object name, QuotedStr or NullText
  @param S the object string
  @return a non-quoted string
}
function TZAbstractDatabaseMetadata.DecomposeObjectString(const S: String): String;
begin
  if S = ''
  then Result := S
  else if IC.IsQuoted(S)
    then Result := IC.ExtractQuote(S)
    else Result := S;
end;

destructor TZAbstractDatabaseMetadata.Destroy;
begin
  FIC := nil;
  FUrl := nil;
  FreeAndNil(FCachedResultSets);
  FDatabaseInfo := nil;

  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZAbstractDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZAbstractDatabaseInfo.Create(Self);
end;

{**
  Creates and returns a statement object.
  @return the statement object
}
function TZAbstractDatabaseMetadata.GetStatement: IZSTatement;
begin
  Result := GetConnection.CreateStatement;
end;

{**
  Retrieves the connection that produced this metadata object.
  @return the connection that produced this metadata object
}
function TZAbstractDatabaseMetadata.GetConnection: IZConnection;
begin
  Result := IZConnection(FConnection);
end;

{**
  Constructs a virtual result set object.
  @param ColumnsDefs an array of column definition objects.
  @return a created result set.
}
function TZAbstractDatabaseMetadata.ConstructVirtualResultSet(
  ColumnsDefs: TZMetadataColumnDefs): IZVirtualResultSet;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  ColumnsInfo: TObjectList;
begin
  ColumnsInfo := TObjectList.Create(True);
  try
    for I := 0 to High(ColumnsDefs) do
    begin
      ColumnInfo := TZColumnInfo.Create;
      with ColumnInfo do begin
        ColumnLabel := ColumnsDefs[I].Name;
        ColumnType := ColumnsDefs[I].SQLType;
        if ColumnType in [stString, stUnicodeString] then
          if (FConSettings.ClientCodePage.Encoding = ceUTF16) then begin
            ColumnType := stUnicodeString;
            ColumnCodePage := zCP_UTF16;
          end else if FConSettings.ClientCodePage.Encoding = ceUTF8 then
            ColumnCodePage := zCP_UTF8
          else ColumnCodePage := FConSettings.ClientCodePage.CP;
        Precision := ColumnsDefs[I].Length;
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;

    Result := TZUnCloseableResultSet.CreateWithColumns(ColumnsInfo, '',
      FConSettings);
    with Result do begin
      SetType(rtScrollInsensitive);
      SetConcurrency(rcUpdatable);
    end;
  finally
    ColumnsInfo.Free;
  end;
end;

procedure TZAbstractDatabaseMetadata.ClearCache;
begin
  FCachedResultSets.Clear;
end;

procedure TZAbstractDatabaseMetadata.ClearBuf;
begin
  fCurrentBufIndex := 0;
end;

procedure TZAbstractDatabaseMetadata.ClearCache(const Key: string);
var I: Integer;
  KeyAndResultSetPair: PZKeyAndResultSetPair;
begin
  for i := FCachedResultSets.Count -1 downto 0 do begin
    KeyAndResultSetPair := FCachedResultSets.Get(i);
    if KeyAndResultSetPair.Key = Key then begin
      FCachedResultSets.Delete(I);
      Break;
    end;
  end;
end;

{**
  Adds resultset to the internal cache.
  @param Key a resultset unique key value.
  @param ResultSet a resultset interface.
}
procedure TZAbstractDatabaseMetadata.AddResultSetToCache(const Key: string;
  const ResultSet: IZResultSet);
var I: NativeInt;
  PKeyAndResultSetPair: PZKeyAndResultSetPair;
begin
  fKeyAndResultSetValue.Key := Key;
  if not FCachedResultSets.Find(@fKeyAndResultSetValue, i) then begin
    PKeyAndResultSetPair := FCachedResultSets.Insert(i);
    PKeyAndResultSetPair^.ResultSet := ResultSet;
    PKeyAndResultSetPair^.Key := Key;
  end;
  if ResultSet <> nil then
    ResultSet.BeforeFirst;
end;

{**
  Gets a resultset interface from the internal cache by key.
  @param Key a resultset unique key value.
  @returns a cached resultset interface or <code>nil</code> otherwise.
}
function TZAbstractDatabaseMetadata.GetResultSetFromCache(
  const Key: string): IZResultSet;
var I: NativeInt;//Integer;
  PKeyAndResultSetPair: PZKeyAndResultSetPair;
begin
  Result := nil;
  fKeyAndResultSetValue.Key := Key;
  if FCachedResultSets.Find(@fKeyAndResultSetValue, i) then begin
    PKeyAndResultSetPair := FCachedResultSets.Get(i);
    Result := PKeyAndResultSetPair^.ResultSet;
  end;
  if Result <> nil then
    Result.BeforeFirst;
end;

{**
  Copies on result set to another one from the current position.
  @param SrcResultSet a source result set.
  @param DestResultSet a destination result set.
  @returns a destination result set.
}
function TZAbstractDatabaseMetadata.CopyToVirtualResultSet(
  const SrcResultSet: IZResultSet; const DestResultSet: IZVirtualResultSet):
  IZVirtualResultSet;
begin
  DestResultSet.SetType(rtScrollInsensitive);
  DestResultSet.SetConcurrency(rcUpdatable);
  if SrcResultSet.GetColumnCount <> DestResultSet.GetColumnCount then
    raise EZSQLException.Create('Column count missmatch.'+LineEnding+
      'Wrong method for copy to virtual resultset.');
  DestResultset.CopyFrom(SrcResultSet, nil, nil);
  DestResultSet.SetConcurrency(rcReadOnly);
  Result := DestResultSet;
end;

{**
  Clones the cached resultset.
  @param ResultSet the resultset to be cloned.
  @returns the clone of the specified resultset.
}
function TZAbstractDatabaseMetadata.CloneCachedResultSet(
  const ResultSet: IZResultSet): IZResultSet;
var RS: TZVirtualResultSet;
begin
  RS := TZVirtualResultSet.CreateCloneFrom(ResultSet);
  Result := CopyToVirtualResultSet(ResultSet, RS);
end;

{**
   Takes a name patternand column name and retuen an appropriate SQL clause
    @param Pattern a sql pattren
    @parma Column a sql column name
    @return processed string for query
}
function TZAbstractDatabaseMetadata.ConstructNameCondition(const Pattern: string;
  const Column: string): string;
var
  WorkPattern: string;
begin
  Result := '';
  if (Length(Pattern) > 2 * 31) then
    raise EZSQLException.Create(SPattern2Long);

  if (Pattern = '%') or (Pattern = '') then
     Exit;
  WorkPattern := NormalizePatternCase(Pattern);
  if HasNoWildcards(WorkPattern) then
  begin
    WorkPattern := StripEscape(WorkPattern);
    Result := Column+' = '+EscapeString(WorkPattern);
  end
  else
    Result := Column+' like '+EscapeString(WorkPattern);
end;

function TZAbstractDatabaseMetadata.GetURL: string;
begin
  Result := GetURLString;
end;

function TZAbstractDatabaseMetadata.GetUserName: string;
begin
  Result := FURL.UserName;
end;

function TZAbstractDatabaseMetadata.GetDatabaseInfo: IZDatabaseInfo;
begin
  if not Assigned(FDatabaseInfo) then
    FDatabaseInfo := CreateDatabaseInfo;
  Result := FDatabaseInfo;
end;

function TZAbstractDatabaseMetadata.GetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(ProceduresColumnsDynArray);
    exit;
  end;

  Key := GetProceduresCacheKey(Catalog, SchemaPattern, ProcedureNamePattern);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
begin
  Result := ConstructVirtualResultSet(ProceduresColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(ProceduresColColumnsDynArray);
    exit;
  end;

  Key := GetProcedureColumnsCacheKey(Catalog, SchemaPattern, ProcedureNamePattern,
    ColumnNamePattern);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(ProceduresColColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetTriggers(const Catalog: string; const SchemaPattern: string;
  const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TriggersColumnsDynArray);
    exit;
  end;

  Key := GetTriggersCacheKey(Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetTriggers(Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetTriggers(const Catalog: string; const SchemaPattern: string;
  const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet;
begin
  Result := ConstructVirtualResultSet(TriggersColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetCollationAndCharSet(const Catalog, Schema,
  TableName, ColumnName: String): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(CollationCharSetColumnsDynArray);
    exit;
  end;

  Key := GetCollationAndCharSetCacheKey(Catalog, Schema, TableName, ColumnName);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetCollationAndCharSet(Catalog, Schema, TableName, ColumnName);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.GetCharacterSets: IZResultSet; //EgonHugeist
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(CharacterSetsColumnsDynArray);
    exit;
  end;

  Key := GetCharacterSetsCacheKey;
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetCharacterSets;
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetCollationAndCharSet(const Catalog, SchemaPattern,
  TableNamePattern, ColumnNamePattern: string): IZResultSet;
begin
  Result := ConstructVirtualResultSet(CollationCharSetColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.UncachedGetCharacterSets: IZResultSet; //EgonHugeist
begin
  Result := ConstructVirtualResultSet(CharacterSetsColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetCollationAndCharSetCacheKey(const Catalog, SchemaPattern,
  TableNamePattern, ColumnPattern: String): string;
begin
  Result := Format('get-CollationAndCharSet:%s:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern, ColumnPattern]);
end;

function TZAbstractDatabaseMetadata.GetCharacterSetsCacheKey: String; //EgonHugeist
begin
  Result := 'get-charactersets';
end;

function TZAbstractDatabaseMetadata.GetTriggersCacheKey(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const TriggerNamePattern: string): string;
begin
  Result := Format('get-trigger:%s:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern]);
end;

function TZAbstractDatabaseMetadata.GetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TableColumnsDynArray);
    exit;
  end;

  Key := GetTablesCacheKey(Catalog, SchemaPattern, TableNamePattern, Types);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
begin
  Result := ConstructVirtualResultSet(TableColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetSchemas: IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(SchemaColumnsDynArray);
    exit;
  end;

  Key := GetSchemasCacheKey;
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetSchemas;
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets the schema names available in this database.  The results
  are ordered by schema name.

  <P>The schema column is:
   <OL>
 	<LI><B>TABLE_SCHEM</B> String => schema name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  schema name
}
function TZAbstractDatabaseMetadata.UncachedGetSchemas: IZResultSet;
begin
    Result := ConstructVirtualResultSet(SchemaColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetCatalogs: IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(CatalogColumnsDynArray);
    exit;
  end;

  Key := GetCatalogsCacheKey;
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetCatalogs;
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets the catalog names available in this database.  The results
  are ordered by catalog name.

  <P>The catalog column is:
   <OL>
 	<LI><B>TABLE_CAT</B> String => catalog name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  catalog name
}
function TZAbstractDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
begin
    Result := ConstructVirtualResultSet(CatalogColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetTableTypes: IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);
    exit;
  end;

  Key := GetTableTypesCacheKey;
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetTableTypes;
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets the table types available in this database.  The results
  are ordered by table type.

  <P>The table type is:
   <OL>
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZAbstractDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);
end;

{**
  Gets a description of table columns available in
  the specified catalog.

  <P>Only column descriptions matching the catalog, schema, table
  and column name criteria are returned.  They are ordered by
  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
 	<LI><B>COLUMN_SIZE</B> int => column size.  For char or date
 	    types this is the maximum number of characters, for numeric or
 	    decimal types this is precision.
 	<LI><B>BUFFER_LENGTH</B> is not used.
 	<LI><B>DECIMAL_DIGITS</B> int => the number of fractional digits
 	<LI><B>NUM_PREC_RADIX</B> int => Radix (typically either 10 or 2)
 	<LI><B>NULLABLE</B> int => is NULL allowed?
       <UL>
       <LI> columnNoNulls - might not allow NULL values
       <LI> columnNullable - definitely allows NULL values
       <LI> columnNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing column (may be null)
  	<LI><B>COLUMN_DEF</B> String => default value (may be null)
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>CHAR_OCTET_LENGTH</B> int => for char types the
        maximum number of bytes in the column
 	<LI><B>ORDINAL_POSITION</B> int	=> index of column in table
       (starting at 1)
 	<LI><B>IS_NULLABLE</B> String => "NO" means column definitely
       does not allow NULL values; "YES" means the column might
       allow NULL values.  An empty string means nobody knows.
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column description
  @see #getSearchStringEscape
}
function TZAbstractDatabaseMetadata.GetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TableColColumnsDynArray);
    exit;
  end;

  Key := GetColumnsCacheKey(Catalog, SchemaPattern, TableNamePattern,
    ColumnNamePattern);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
begin
  Result := ConstructVirtualResultSet(TableColColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);
    exit;
  end;

  Key := GetColumnPrivilegesCacheKey(Catalog, Schema, Table,
    ColumnNamePattern);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);
    AddResultSetToCache(Key, Result);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Catalog/Schema/Table/ColumnNamePattern" not used} {$ENDIF}
function TZAbstractDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
begin
  Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);
end;
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Catalog/Schema" not used} {$ENDIF}

function TZAbstractDatabaseMetadata.GetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);
    exit;
  end;

  Key := GetTablePrivilegesCacheKey(Catalog, SchemaPattern,
    TableNamePattern);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);
end;

{**
  Gets a description of a table's optimal set of columns that
  uniquely identifies a row. They are ordered by SCOPE.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => actual scope of result
       <UL>
       <LI> bestRowTemporary - very temporary, while using row
       <LI> bestRowTransaction - valid for remainder of current transaction
       <LI> bestRowSession - valid for remainder of current session
       </UL>
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => not used
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> bestRowUnknown - may or may not be pseudo column
       <LI> bestRowNotPseudo - is NOT a pseudo column
       <LI> bestRowPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param scope the scope of interest; use same values as SCOPE
  @param nullable include columns that are nullable?
  @return <code>ResultSet</code> - each row is a column description
}
function TZAbstractDatabaseMetadata.GetBestRowIdentifier(const Catalog: string;
  const Schema: string; const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(BestRowIdentColumnsDynArray);
    exit;
  end;

  Key := GetBestRowIdentifierCacheKey(Catalog, Schema, Table, Scope,
    Nullable);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetBestRowIdentifier(Catalog, Schema, Table, Scope, Nullable);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a table's optimal set of columns that
  uniquely identifies a row. They are ordered by SCOPE.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => actual scope of result
       <UL>
       <LI> bestRowTemporary - very temporary, while using row
       <LI> bestRowTransaction - valid for remainder of current transaction
       <LI> bestRowSession - valid for remainder of current session
       </UL>
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => not used
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> bestRowUnknown - may or may not be pseudo column
       <LI> bestRowNotPseudo - is NOT a pseudo column
       <LI> bestRowPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param scope the scope of interest; use same values as SCOPE
  @param nullable include columns that are nullable?
  @return <code>ResultSet</code> - each row is a column description
}
function TZAbstractDatabaseMetadata.UncachedGetBestRowIdentifier(const Catalog: string;
  const Schema: string; const Table: string; Scope: Integer; Nullable: Boolean): IZResultSet;
var
  IndexName: string;
  ColumnNames: TStrings;
begin
  Result := ConstructVirtualResultSet(BestRowIdentColumnsDynArray);
  ColumnNames := TStringList.Create;
  try
    { Tries primary keys. }
    with GetPrimaryKeys(Catalog, Schema, Table) do begin
      while Next do
        ColumnNames.Add(GetString(PrimaryKeyColumnNameIndex));
      Close;
    end;
    { Tries unique indices. }
    if ColumnNames.Count = 0 then
    begin
      with GetIndexInfo(Catalog, Schema, Table, True, False) do begin
        IndexName := '';
        while Next do begin
          if IndexName = '' then
            IndexName := GetString(IndexInfoColIndexNameIndex);
          if GetString(IndexInfoColIndexNameIndex) = IndexName then
            ColumnNames.Add(GetString(IndexInfoColColumnNameIndex));
        end;
        Close;
      end;
    end;

    with GetColumns(Catalog, AddEscapeCharToWildcards(Schema), AddEscapeCharToWildcards(Table), '') do begin
      while Next do begin
        if (ColumnNames.Count <> 0) and (ColumnNames.IndexOf(
          GetString(ColumnNameIndex)) < 0) then
          Continue;
        if (ColumnNames.Count = 0)
          and (TZSQLType(GetSmall(TableColColumnTypeIndex)) in
          [stBytes, stBinaryStream, stAsciiStream, stUnicodeStream]) then
          Continue;

        Result.MoveToInsertRow;
        Result.UpdateInt(BestRowIdentScopeIndex, Ord(sbrSession));
        Result.UpdateString(BestRowIdentColNameIndex, GetString(ColumnNameIndex));
        Result.UpdateSmall(BestRowIdentDataTypeIndex, GetSmall(TableColColumnTypeIndex));
        Result.UpdateString(BestRowIdentTypeNameIndex, GetString(TableColColumnTypeNameIndex));
        Result.UpdateInt(BestRowIdentColSizeIndex, GetInt(TableColColumnSizeIndex));
        Result.UpdateInt(BestRowIdentBufLengthIndex, GetInt(TableColColumnBufLengthIndex));
        Result.UpdateInt(BestRowIdentDecimalDigitsIndex, GetInt(TableColColumnDecimalDigitsIndex));
        Result.UpdateInt(BestRowIdentPseudoColumnIndex, Ord(brNotPseudo));
        Result.InsertRow;
      end;
      Close;
    end;
  finally
    ColumnNames.Free;
  end;
end;

{**
  Gets a description of a table's columns that are automatically
  updated when any value in a row is updated.  They are
  unordered.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => is not used
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => length of column value in bytes
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> versionColumnUnknown - may or may not be pseudo column
       <LI> versionColumnNotPseudo - is NOT a pseudo column
       <LI> versionColumnPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a column description
  @exception SQLException if a database access error occurs
}

function TZAbstractDatabaseMetadata.GetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TableColVerColumnsDynArray);
    exit;
  end;

  Key := GetVersionColumnsCacheKey(Catalog, Schema, Table);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetVersionColumns(Catalog, Schema, Table);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a table's columns that are automatically
  updated when any value in a row is updated.  They are
  unordered.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => is not used
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => length of column value in bytes
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> versionColumnUnknown - may or may not be pseudo column
       <LI> versionColumnNotPseudo - is NOT a pseudo column
       <LI> versionColumnPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a column description
  @exception SQLException if a database access error occurs
}
function TZAbstractDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableColVerColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(PrimaryKeyColumnsDynArray);
    exit;
  end;

  Key := GetPrimaryKeysCacheKey(Catalog, Schema, Table);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetPrimaryKeys(Catalog, Schema, Table);
    AddResultSetToCache(Key, Result);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Catalog/Schema/Table" not used} {$ENDIF}
function TZAbstractDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := ConstructVirtualResultSet(PrimaryKeyColumnsDynArray);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractDatabaseMetadata.GetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(ImportedKeyColumnsDynArray);
    exit;
  end;

  Key := GetImportedKeysCacheKey(Catalog, Schema, Table);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetImportedKeys(Catalog, Schema, Table);
    AddResultSetToCache(Key, Result);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Catalog/Schema/Table" not used} {$ENDIF}
function TZAbstractDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := ConstructVirtualResultSet(ImportedKeyColumnsDynArray);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets a description of the foreign key columns that reference a
  table's primary key columns (the foreign keys exported by a
  table).  They are ordered by FKTABLE_CAT, FKTABLE_SCHEM,
  FKTABLE_NAME, and KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZAbstractDatabaseMetadata.GetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(ExportedKeyColumnsDynArray);
    exit;
  end;

  Key := GetExportedKeysCacheKey(Catalog, Schema, Table);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetExportedKeys(Catalog, Schema, Table);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of the foreign key columns that reference a
  table's primary key columns (the foreign keys exported by a
  table).  They are ordered by FKTABLE_CAT, FKTABLE_SCHEM,
  FKTABLE_NAME, and KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZAbstractDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(ExportedKeyColumnsDynArray);
end;

{**
  Gets a description of the foreign key columns in the foreign key
  table that reference the primary key columns of the primary key
  table (describe how one table imports another's key.) This
  should normally return a single foreign key/primary key pair
  (most tables only import a foreign key from a table once.)  They
  are ordered by FKTABLE_CAT, FKTABLE_SCHEM, FKTABLE_NAME, and
  KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param primaryCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param primarySchema a schema name; "" retrieves those
  without a schema
  @param primaryTable the table name that exports the key
  @param foreignCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param foreignSchema a schema name; "" retrieves those
  without a schema
  @param foreignTable the table name that imports the key
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZAbstractDatabaseMetadata.GetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(CrossRefColumnsDynArray);
    exit;
  end;

  Key := GetCrossReferenceCacheKey(PrimaryCatalog, PrimarySchema, PrimaryTable,
    ForeignCatalog, ForeignSchema, ForeignTable);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
    ForeignCatalog, ForeignSchema, ForeignTable);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of the foreign key columns in the foreign key
  table that reference the primary key columns of the primary key
  table (describe how one table imports another's key.) This
  should normally return a single foreign key/primary key pair
  (most tables only import a foreign key from a table once.)  They
  are ordered by FKTABLE_CAT, FKTABLE_SCHEM, FKTABLE_NAME, and
  KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param primaryCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param primarySchema a schema name; "" retrieves those
  without a schema
  @param primaryTable the table name that exports the key
  @param foreignCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param foreignSchema a schema name; "" retrieves those
  without a schema
  @param foreignTable the table name that imports the key
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZAbstractDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(CrossRefColumnsDynArray);
end;

{**
  Gets a description of all the standard SQL types supported by
  this database. They are ordered by DATA_TYPE and then by how
  closely the data type maps to the corresponding JDBC SQL type.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_NAME</B> String => Type name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>PRECISION</B> int => maximum precision
 	<LI><B>LITERAL_PREFIX</B> String => prefix used to quote a literal
       (may be null)
 	<LI><B>LITERAL_SUFFIX</B> String => suffix used to quote a literal
        (may be null)
 	<LI><B>CREATE_PARAMS</B> String => parameters used in creating
       the type (may be null)
 	<LI><B>NULLABLE</B> short => can you use NULL for this type?
       <UL>
       <LI> typeNoNulls - does not allow NULL values
       <LI> typeNullable - allows NULL values
       <LI> typeNullableUnknown - nullability unknown
       </UL>
 	<LI><B>CASE_SENSITIVE</B> boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> boolean => can it be used for an
       auto-increment value?
 	<LI><B>LOCAL_TYPE_NAME</B> String => localized version of type name
       (may be null)
 	<LI><B>MINIMUM_SCALE</B> short => minimum scale supported
 	<LI><B>MAXIMUM_SCALE</B> short => maximum scale supported
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>NUM_PREC_RADIX</B> int => usually 2 or 10
   </OL>

  @return <code>ResultSet</code> - each row is an SQL type description
}
function TZAbstractDatabaseMetadata.GetTypeInfo: IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);
    exit;
  end;

  Key := GetTypeInfoCacheKey;
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetTypeInfo;
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of all the standard SQL types supported by
  this database. They are ordered by DATA_TYPE and then by how
  closely the data type maps to the corresponding JDBC SQL type.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_NAME</B> String => Type name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>PRECISION</B> int => maximum precision
 	<LI><B>LITERAL_PREFIX</B> String => prefix used to quote a literal
       (may be null)
 	<LI><B>LITERAL_SUFFIX</B> String => suffix used to quote a literal
        (may be null)
 	<LI><B>CREATE_PARAMS</B> String => parameters used in creating
       the type (may be null)
 	<LI><B>NULLABLE</B> short => can you use NULL for this type?
       <UL>
       <LI> typeNoNulls - does not allow NULL values
       <LI> typeNullable - allows NULL values
       <LI> typeNullableUnknown - nullability unknown
       </UL>
 	<LI><B>CASE_SENSITIVE</B> boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> boolean => can it be used for an
       auto-increment value?
 	<LI><B>LOCAL_TYPE_NAME</B> String => localized version of type name
       (may be null)
 	<LI><B>MINIMUM_SCALE</B> short => minimum scale supported
 	<LI><B>MAXIMUM_SCALE</B> short => maximum scale supported
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>NUM_PREC_RADIX</B> int => usually 2 or 10
   </OL>

  @return <code>ResultSet</code> - each row is an SQL type description
}
function TZAbstractDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>NON_UNIQUE</B> boolean => Can index values be non-unique?
       false when TYPE is tableIndexStatistic
 	<LI><B>INDEX_QUALIFIER</B> String => index catalog (may be null);
       null when TYPE is tableIndexStatistic
 	<LI><B>INDEX_NAME</B> String => index name; null when TYPE is
       tableIndexStatistic
 	<LI><B>TYPE</B> short => index type:
       <UL>
       <LI> tableIndexStatistic - this identifies table statistics that are
            returned in conjuction with a table's index descriptions
       <LI> tableIndexClustered - this is a clustered index
       <LI> tableIndexHashed - this is a hashed index
       <LI> tableIndexOther - this is some other style of index
       </UL>
 	<LI><B>ORDINAL_POSITION</B> short => column sequence number
       within index; zero when TYPE is tableIndexStatistic
 	<LI><B>COLUMN_NAME</B> String => column name; null when TYPE is
       tableIndexStatistic
 	<LI><B>ASC_OR_DESC</B> String => column sort sequence, "A" => ascending,
       "D" => descending, may be null if sort sequence is not supported;
       null when TYPE is tableIndexStatistic
 	<LI><B>CARDINALITY</B> int => When TYPE is tableIndexStatistic, then
       this is the number of rows in the table; otherwise, it is the
       number of unique values in the index.
 	<LI><B>PAGES</B> int => When TYPE is  tableIndexStatisic then
       this is the number of pages used for the table, otherwise it
       is the number of pages used for the current index.
 	<LI><B>FILTER_CONDITION</B> String => Filter condition, if any.
       (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param unique when true, return only indices for unique values;
      when false, return indices regardless of whether unique or not
  @param approximate when true, result is allowed to reflect approximate
      or out of data values; when false, results are requested to be
      accurate
  @return <code>ResultSet</code> - each row is an index column description
}
function TZAbstractDatabaseMetadata.GetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);
    exit;
  end;

  Key := GetIndexInfoCacheKey(Catalog, Schema, Table, Unique, Approximate);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);
    AddResultSetToCache(Key, Result);
  end;
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>NON_UNIQUE</B> boolean => Can index values be non-unique?
       false when TYPE is tableIndexStatistic
 	<LI><B>INDEX_QUALIFIER</B> String => index catalog (may be null);
       null when TYPE is tableIndexStatistic
 	<LI><B>INDEX_NAME</B> String => index name; null when TYPE is
       tableIndexStatistic
 	<LI><B>TYPE</B> short => index type:
       <UL>
       <LI> tableIndexStatistic - this identifies table statistics that are
            returned in conjuction with a table's index descriptions
       <LI> tableIndexClustered - this is a clustered index
       <LI> tableIndexHashed - this is a hashed index
       <LI> tableIndexOther - this is some other style of index
       </UL>
 	<LI><B>ORDINAL_POSITION</B> short => column sequence number
       within index; zero when TYPE is tableIndexStatistic
 	<LI><B>COLUMN_NAME</B> String => column name; null when TYPE is
       tableIndexStatistic
 	<LI><B>ASC_OR_DESC</B> String => column sort sequence, "A" => ascending,
       "D" => descending, may be null if sort sequence is not supported;
       null when TYPE is tableIndexStatistic
 	<LI><B>CARDINALITY</B> int => When TYPE is tableIndexStatistic, then
       this is the number of rows in the table; otherwise, it is the
       number of unique values in the index.
 	<LI><B>PAGES</B> int => When TYPE is  tableIndexStatisic then
       this is the number of pages used for the table, otherwise it
       is the number of pages used for the current index.
 	<LI><B>FILTER_CONDITION</B> String => Filter condition, if any.
       (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param unique when true, return only indices for unique values;
      when false, return indices regardless of whether unique or not
  @param approximate when true, result is allowed to reflect approximate
      or out of data values; when false, results are requested to be
      accurate
  @return <code>ResultSet</code> - each row is an index column description
}
function TZAbstractDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
begin
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);
end;

function TZAbstractDatabaseMetadata.GetSequences(const Catalog: string;
  const SchemaPattern: string; const SequenceNamePattern: string): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(SequenceColumnsDynArray);
    exit;
  end;

  Key := GetSequencesCacheKey(Catalog, SchemaPattern, SequenceNamePattern);
  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetSequences(Catalog, SchemaPattern, SequenceNamePattern);
    AddResultSetToCache(Key, Result);
  end;
end;

function TZAbstractDatabaseMetadata.UncachedGetSequences(const Catalog: string;
  const SchemaPattern: string; const SequenceNamePattern: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(SequenceColumnsDynArray);
end;

{**

  Gets a description of the user-defined types defined in a particular
  schema.  Schema-specific UDTs may have type JAVA_OBJECT, STRUCT,
  or DISTINCT.

  <P>Only types matching the catalog, schema, type name and type
  criteria are returned.  They are ordered by DATA_TYPE, TYPE_SCHEM
  and TYPE_NAME.  The type name parameter may be a fully-qualified
  name.  In this case, the catalog and schemaPattern parameters are
  ignored.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_CAT</B> String => the type's catalog (may be null)
 	<LI><B>TYPE_SCHEM</B> String => type's schema (may be null)
 	<LI><B>TYPE_NAME</B> String => type name
   <LI><B>CLASS_NAME</B> String => Java class name
 	<LI><B>DATA_TYPE</B> String => type value defined in java.sql.Types.
   One of JAVA_OBJECT, STRUCT, or DISTINCT
 	<LI><B>REMARKS</B> String => explanatory comment on the type
   </OL>

  <P><B>Note:</B> If the driver does not support UDTs, an empty
  result set is returned.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param typeNamePattern a type name pattern; may be a fully-qualified name
  @param types a list of user-named types to include (JAVA_OBJECT,
  STRUCT, or DISTINCT); null returns all types
  @return <code>ResultSet</code> - each row is a type description
}
function TZAbstractDatabaseMetadata.GetUDTs(const Catalog: string;
  const SchemaPattern: string; const TypeNamePattern: string;
  const Types: TIntegerDynArray): IZResultSet;
var
  Key: string;
begin
  if not GetConnection.UseMetadata then
  begin
    Result := ConstructVirtualResultSet(UDTColumnsDynArray);
    exit;
  end;

  Key := GetUDTsCacheKey(Catalog, SchemaPattern, TypeNamePattern,
    Types);

  Result := GetResultSetFromCache(Key);
  if Result = nil then
  begin
    Result := UncachedGetUDTs(Catalog, SchemaPattern, TypeNamePattern, Types);
    AddResultSetToCache(Key, Result);
  end;
end;

{**

  Gets a description of the user-defined types defined in a particular
  schema.  Schema-specific UDTs may have type JAVA_OBJECT, STRUCT,
  or DISTINCT.

  <P>Only types matching the catalog, schema, type name and type
  criteria are returned.  They are ordered by DATA_TYPE, TYPE_SCHEM
  and TYPE_NAME.  The type name parameter may be a fully-qualified
  name.  In this case, the catalog and schemaPattern parameters are
  ignored.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_CAT</B> String => the type's catalog (may be null)
 	<LI><B>TYPE_SCHEM</B> String => type's schema (may be null)
 	<LI><B>TYPE_NAME</B> String => type name
   <LI><B>CLASS_NAME</B> String => Java class name
 	<LI><B>DATA_TYPE</B> String => type value defined in java.sql.Types.
   One of JAVA_OBJECT, STRUCT, or DISTINCT
 	<LI><B>REMARKS</B> String => explanatory comment on the type
   </OL>

  <P><B>Note:</B> If the driver does not support UDTs, an empty
  result set is returned.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param typeNamePattern a type name pattern; may be a fully-qualified name
  @param types a list of user-named types to include (JAVA_OBJECT,
  STRUCT, or DISTINCT); null returns all types
  @return <code>ResultSet</code> - each row is a type description
}
function TZAbstractDatabaseMetadata.UncachedGetUDTs(const Catalog: string;
  const SchemaPattern: string; const TypeNamePattern: string;
  const Types: TIntegerDynArray): IZResultSet;
begin
    Result := ConstructVirtualResultSet(UDTColumnsDynArray);
end;

{**
  Creates ab identifier converter object.
  @returns an identifier converter object.
}
function TZAbstractDatabaseMetadata.GetIdentifierConverter: IZIdentifierConverter;
begin
  Result := TZDefaultIdentifierConverter.Create(Self);
end;

{**
  Creates ab identifier converter object.
  @returns an identifier converter object.
}
function TZAbstractDatabaseMetadata.GetIdentifierConvertor:
  IZIdentifierConverter;
begin
  Result := GetIdentifierConverter;
end;

{**
  Add escape character in the pattern that has wildcards character
  @param Pattern The pattern that would be escaped
  @return Escaped Pattern
}
function TZAbstractDatabaseMetadata.AddEscapeCharToWildcards(
  const Pattern: string): string;
var
  i:Integer;
  EscapeChar : string;
begin
  if GetDatabaseInfo.SupportsNonEscapedSearchStrings then
    Result := Pattern
  else
  begin
    EscapeChar := GetDatabaseInfo.GetSearchStringEscape;
    if WildcardsArray<>nil then
    begin
      Result:=StringReplace(Pattern,EscapeChar,EscapeChar+EscapeChar,[rfReplaceAll]);
      for i:=0 to High(WildcardsArray) do
        Result:=StringReplace(Result,WildcardsArray[i],EscapeChar+WildcardsArray[i],[rfReplaceAll]);
    end;
  end;
end;

{**
  Set the Wildcards character for WildcardsArray variable.
  Overrride this method if the wildcards character is different in other database
}
procedure TZAbstractDatabaseMetadata.FillWildcards;
begin
  SetLength(WildcardsArray,2);
  WildcardsArray[0]:='_';  //<---- seems to be a trublemaker, no idea how to test it with our tests. See http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=13184
  WildcardsArray[1]:='%';
end;

procedure TZAbstractDatabaseMetadata.FlushBuf(var Value: String);
var I: Integer;
begin
  if fCurrentBufIndex > 0 then begin
    I := Length(Value);
    SetLength(Value, i+fCurrentBufIndex);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(fBuf[0], Value[I+1], fCurrentBufIndex * SizeOf(Char));
    fCurrentBufIndex := 0;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeCatalogName(var Value: String);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeColumnNamePattern(var Value: String);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeParameterNamePattern(var Value: String);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractDatabaseMetadata.NormalizePatternCase(const Pattern: String): string;
begin
  with FIC do
    if not IsQuoted(Pattern) then begin
      //EH: if this is not made test spaced name will fail
      Result := Pattern;
      case GetIdentifierCase(Pattern, False) of
        icLower: if FDatabaseInfo.StoresUpperCaseIdentifiers then
                  Result := UpperCase(Pattern);
        icUpper: if FDatabaseInfo.StoresLowerCaseIdentifiers then
                   Result := LowerCase(Pattern);
        icMixed: if FDatabaseInfo.StoresUpperCaseIdentifiers then
                   Result := UpperCase(Pattern)
                 else if FDatabaseInfo.StoresLowerCaseIdentifiers then
                   Result := LowerCase(Pattern);
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end
    end else
      Result := ExtractQuote(Pattern);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeProcedureName(var Value: String;
  IsPattern: Boolean);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeSchemaName(var Value: String;
  IsPattern: Boolean);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeSequenceNamePattern(var Value: String);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeTableName(var Value: String;
  IsPattern: Boolean);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Value" not used} {$ENDIF}
procedure TZAbstractDatabaseMetadata.NormalizeTriggerNamePattern(var Value: String);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Get the Wildscards in set of char type
  @return TZWildcardsSet type
}
function TZAbstractDatabaseMetadata.GetWildcardsSet:TZWildcardsSet;
{$IFNDEF TSYSCHARSET_IS_DEPRECATED}
var i:Integer;
{$ENDIF}
begin
  {$IFDEF TSYSCHARSET_IS_DEPRECATED}
  Result := WildcardsArray;
  {$ELSE}
  Result:=[];
  for i:=0 to High(WildcardsArray) do
    Result:=Result+[WildcardsArray[i]];
  {$ENDIF}
end;

//----------------------------------------------------------------------
// Metadata cache key retrieval API (technobot 2008-06-14):

{**
  returns cache key for GetProcedures metadata entry
  @param Catalog a catalog name
  @param SchemaPattern a schema name pattern
  @param ProcedureNamePattern a procedure name pattern
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetProceduresCacheKey(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): string;
begin
  Result := 'get-procedures:'+Catalog+':'+SchemaPattern+':'+ProcedureNamePattern;
end;

{**
  returns cache key for GetProcedureColumns metadata entry
  @param Catalog a catalog name
  @param SchemaPattern a schema name pattern
  @param ProcedureNamePattern a procedure name pattern
  @param ColumnNamePattern a column name pattern
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetProcedureColumnsCacheKey(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): string;
begin
  Result := 'get-procedure-columns:'+Catalog+':'+SchemaPattern+':'+
    ProcedureNamePattern+':'+ColumnNamePattern;
end;

{**
  returns cache key for GetTables metadata entry
  @param Catalog a catalog name
  @param SchemaPattern a schema name pattern
  @param TableNamePattern a table name pattern
  @param Types table types list
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetTablesCacheKey(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): string;
var
  I: Integer;
  Key: string;
begin
  Key := '';
  for I := Low(Types) to High(Types) do
    Key := Key + ':' + Types[I];

  Result := 'get-tables:'+Catalog+':'+SchemaPattern+':'+TableNamePattern+':'+Key;
end;

{**
  returns cache key for GetSchemas metadata entry
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetSchemasCacheKey: string;
begin
  Result := 'get-schemas';
end;

{**
  returns cache key for GetCatalogs metadata entry
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetCatalogsCacheKey: string;
begin
  Result := 'get-catalogs';
end;

{**
  returns cache key for GetTableTypes metadata entry
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetTableTypesCacheKey: string;
begin
  Result := 'get-table-types';
end;

{**
  returns cache key for GetColumns metadata entry
  @param Catalog a catalog name
  @param SchemaPattern a schema name pattern
  @param TableNamePattern a table name pattern
  @param ColumnNamePattern a column name pattern
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetColumnsCacheKey(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): string;
begin
  Result := 'get-columns:'+Catalog+':'+SchemaPattern+':'+TableNamePattern+':'+
    ColumnNamePattern;
end;

{**
  returns cache key for GetColumnPrivileges metadata entry
  @param Catalog a catalog name
  @param Schema a schema name
  @param Table a table name
  @param ColumnNamePattern a column name pattern
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetColumnPrivilegesCacheKey(
  const Catalog: string; const Schema: string; const Table: string;
  const ColumnNamePattern: string): string;
begin
  Result := 'get-column-privileges:'+Catalog+':'+Schema+':'+Table+':'+
    ColumnNamePattern;
end;

{**
  returns cache key for GetTablePrivileges metadata entry
  @param Catalog a catalog name
  @param SchemaPattern a schema name pattern
  @param TableNamePattern a table name pattern
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetTablePrivilegesCacheKey(
  const Catalog: string; const SchemaPattern: string;
  const TableNamePattern: string): string;
begin
  Result := 'get-table-privileges:'+Catalog+':'+SchemaPattern+':'+
    TableNamePattern;
end;

{**
  returns cache key for GetBestRowIdentifier metadata entry
  @param Catalog a catalog name
  @param Schema a schema name
  @param Table a table name
  @param Scope the scope of interest
  @param Nullable include columns that are nullable?
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetBestRowIdentifierCacheKey(
  const Catalog: string; const Schema: string; const Table: string;
  const Scope: Integer; const Nullable: Boolean): string;
begin
  Result := 'get-best-row-identifier:'+Catalog+':'+Schema+':'+Table+':'+
    ZFastCode.IntToStr(Scope)+':'+BoolToStr(Nullable);
end;

{**
  returns cache key for GetVersionColumns metadata entry
  @param Catalog a catalog name
  @param Schema a schema name
  @param Table a table name
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetVersionColumnsCacheKey(
  const Catalog: string; const Schema: string; const Table: string): string;
begin
  Result := 'get-version-columns:'+Catalog+':'+Schema+':'+Table;
end;

{**
  returns cache key for GetPrimaryKeys metadata entry
  @param Catalog a catalog name
  @param Schema a schema name
  @param Table a table name
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetPrimaryKeysCacheKey(const Catalog: string;
  const Schema: string; const Table: string): string;
begin
  Result := 'get-primary-keys:'+Catalog+':'+Schema+':'+Table;
end;

{**
  returns cache key for GetImportedKeys metadata entry
  @param Catalog a catalog name
  @param Schema a schema name
  @param Table a table name
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetImportedKeysCacheKey(const Catalog: string;
  const Schema: string; const Table: string): string;
begin
  Result := 'get-imported-keys:'+Catalog+':'+Schema+':'+Table;
end;

{**
  returns cache key for GetExportedKeys metadata entry
  @param Catalog a catalog name
  @param Schema a schema name
  @param Table a table name
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetExportedKeysCacheKey(const Catalog: string;
  const Schema: string; const Table: string): string;
begin
  Result := 'get-exported-keys:'+Catalog+':'+Schema+':'+Table;
end;

{**
  returns cache key for GetCrossReference metadata entry
  @param PrimaryCatalog a catalog name for the primary table
  @param PrimarySchema a schema name for the primary table
  @param PrimaryTable the table name that exports the key
  @param ForeignCatalog a catalog name for the foreign table
  @param ForeignSchema a schema name for the foreign table
  @param ForeignTable the table name that imports the key
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetCrossReferenceCacheKey(
  const PrimaryCatalog: string; const PrimarySchema: string;
  const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): string;
begin
  Result := 'get-cross-reference:'+PrimaryCatalog+':'+PrimarySchema+':'+
    PrimaryTable+':'+ForeignCatalog+':'+ForeignSchema+':'+ForeignTable;
end;

{**
  returns cache key for GetTypeInfo metadata entry
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetTypeInfoCacheKey: string;
begin
  Result := 'get-type-info';
end;

{**
  returns cache key for GetIndexInfo metadata entry
  @param Catalog a catalog name
  @param Schema a schema name
  @param Table a table name
  @param Unique when true, return key for a metadata entry that should contain
      only indices for unique values; when false, return key for a metadata
      entry that may contain indices to non-unique values
  @param Approximate when true, return key for a metadata entry that may include
      approximate or out of data values; when false, return key for a metadata
      entry that should contain only accurate results
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetIndexInfoCacheKey(const Catalog: string;
  const Schema: string; const Table: string; const Unique: Boolean;
  const Approximate: Boolean): string;
begin
  Result := 'get-index-info:'+Catalog+':'+Schema+':'+Table+':'+
    BoolToStr(Unique)+':'+BoolToStr(Approximate);
end;

{**
  returns cache key for GetSequences metadata entry
  @param Catalog a catalog name
  @param SchemaPattern a schema name pattern
  @param SequenceNamePattern a sequence name pattern
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetSequencesCacheKey(const Catalog: string;
  const SchemaPattern: string; const SequenceNamePattern: string): string;
begin
  Result := 'get-sequences:'+Catalog+':'+SchemaPattern+':'+SequenceNamePattern;
end;

{**
  returns cache key for GetUDTs metadata entry
  @param Catalog a catalog name
  @param SchemaPattern a schema name pattern
  @param TypeNamePattern a type name pattern
  @param Types a list of user-named types to include
  @return the cache key string
}
function TZAbstractDatabaseMetadata.GetUDTsCacheKey(const Catalog: string;
  const SchemaPattern: string; const TypeNamePattern: string;
  const Types: TIntegerDynArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Types) to High(Types) do
    Result := Result + ':' + ZFastCode.IntToStr(Types[I]);
  Result := 'get-udts:'+Catalog+':'+SchemaPattern+':'+TypeNamePattern+':'+Result;
end;

{**
  fills string list with the keys for the currently cached metadata entries
  @param List a string list to fill out
}
procedure TZAbstractDatabaseMetadata.GetCacheKeys(List: TStrings);
var I: Integer;
  KeyAndResultSetPair: PZKeyAndResultSetPair;
begin
  List.Clear;
  for i := FCachedResultSets.Count -1 downto 0 do begin
    KeyAndResultSetPair := FCachedResultSets.Get(i);
    List.Add(KeyAndResultSetPair.Key)
  end;
end;

// End of metadata cache key retrieval API (technobot 2008-06-14)
//----------------------------------------------------------------------


{ TZDefaultIdentifierConverter }

constructor TZDefaultIdentifierConverter.Create(
  const Metadata: IZDatabaseMetadata);
begin
  inherited Create;
  FMetadata := Pointer(Metadata);
end;

function TZDefaultIdentifierConverter.GetIdentifierCase(
  const Value: String; TestKeyWords: Boolean): TZIdentifierCase;
var
  P1: PChar;
  UpCnt, LoCnt: Integer;
  S: String;
begin
  Result := icNone;
  if Value = '' then Exit;
  P1 := Pointer(Value);
  case P1^ of //don't use damn slow charInSet
    '0'..'9': begin
      Result := icSpecial;
      Exit;
    end;
  end;
  UpCnt := 0; LoCnt := 0;
  while P1^<> #0 do begin
    case P1^ of
      'A'..'Z': Inc(UpCnt);
      'a'..'z': Inc(LoCnt);
      '0'..'9','_': ;
      else begin //stop loop
        Result := icSpecial; //Exit(caseSpec) is supported since XE2 all older do not compile this
        Exit;
      end;
    end;
    Inc(P1);
  end;
  if (UpCnt > 0) then
    if (LoCnt = 0)
    then Result := icUpper
    else Result := icMixed
  else if (LoCnt > 0)
  then Result := icLower
  else Result := icNone; //this could happen only if table starts with '_' and possible numbers follow

  if TestKeyWords and (Result <> icNone){not (Result in [icNone, icSpecial])} then begin
    { Checks for reserved keywords. }
    if Metadata.GetDatabaseInfo.StoresUpperCaseIdentifiers and (Result <> icUpper) then
      S := UpperCase(Value)
    else if not Metadata.GetDatabaseInfo.StoresLowerCaseIdentifiers and (Result <> icLower) then
      s := LowerCase(Value)
    else S := Value;
    // With sorted list fast binary search is performed
    if Metadata.GetDatabaseInfo.GetIdentifierQuoteKeywordsSorted.IndexOf(S) <> -1 then
      Result := icSpecial;
  end;
end;

function TZDefaultIdentifierConverter.GetMetaData;
begin
  if Assigned(FMetadata)
  then Result := IZDatabaseMetadata(FMetadata)
  else Result := nil;
end;

function TZDefaultIdentifierConverter.IsLowerCase(const Value: string): Boolean;
begin
  Result := GetIdentifierCase(Value, False) = icLower;
end;

function TZDefaultIdentifierConverter.IsUpperCase(const Value: string): Boolean;
begin
  Result := GetIdentifierCase(Value, False) = icUpper;
end;

function TZDefaultIdentifierConverter.IsSpecialCase(const Value: string): Boolean;
begin
  Result := GetIdentifierCase(Value, True) = icSpecial;
end;

function TZDefaultIdentifierConverter.IsCaseSensitive(const Value: string): Boolean;
var DataBaseInfo: IZDataBaseInfo;
begin
  DataBaseInfo := Metadata.GetDatabaseInfo;
  case GetIdentifierCase(Value, True) of
    icLower:   Result := not DatabaseInfo.StoresMixedCaseIdentifiers and DatabaseInfo.StoresUpperCaseIdentifiers;
    icUpper:   Result := not DatabaseInfo.StoresMixedCaseIdentifiers and DatabaseInfo.StoresLowerCaseIdentifiers;
    icSpecial: Result := True;
    icMixed:   Result := not DatabaseInfo.StoresMixedCaseIdentifiers;
    else Result := False;
  end;
end;

function TZDefaultIdentifierConverter.IsQuoted(const Value: string): Boolean;
var
  QuoteDelim: string;
  PQ: PChar absolute QuoteDelim;
  PV: PChar absolute Value;
begin
  QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
  Result := (PV <> nil) and (PQ <> nil) and (PV^ = PQ^) and
            ((PV+Length(Value)-1)^ = (PQ+Length(QuoteDelim)-1)^);
end;

function TZDefaultIdentifierConverter.ExtractQuote(const Value: string): string;
var
  QuoteDelim: string;
  PQ: PChar absolute QuoteDelim;
begin
  if IsQuoted(Value) then begin
    QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
    case Length(QuoteDelim) of
      1: Result := SQLDequotedStr(Value, PQ^);
      2: Result := SQLDequotedStr(Value, PQ^, (PQ+1)^);
      else Result := Value;
    end;
  end else begin
    Result := Value;
    case GetIdentifierCase(Value,True) of
      icMixed:
          if Metadata.GetDatabaseInfo.StoresUpperCaseIdentifiers
          then Result := UpperCase(Result)
          else if Metadata.GetDatabaseInfo.StoresLowerCaseIdentifiers then
            Result := LowerCase(Result);
      icLower: if Metadata.GetDatabaseInfo.StoresUpperCaseIdentifiers then
        Result := UpperCase(Result);
      icUpper: if Metadata.GetDatabaseInfo.StoresLowerCaseIdentifiers then
        Result := LowerCase(Result);
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "Qualifier" not used} {$ENDIF}
function TZDefaultIdentifierConverter.Quote(const Value: string;
  Qualifier: TZIdentifierQualifier = iqUnspecified): string;
var
  QuoteDelim: string;
  PQ: PChar absolute QuoteDelim;
begin
  Result := Value;
  if IsCaseSensitive(Value) then begin
    QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
    case Length(QuoteDelim) of
      1: Result := SQLQuotedStr(Value, PQ^);
      2: Result := SQLQuotedStr(Value, PQ^, (PQ+1)^);
      else Result := Value;
    end;
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ TZUnCloseableResultSet }

procedure TZUnCloseableResultSet.Close;
begin
  if fDoClose then
    inherited Close;
end;

destructor TZUnCloseableResultSet.Destroy;
begin
  fDoClose := True;
  inherited Destroy;
end;

procedure TZUnCloseableResultSet.ResetCursor;
begin
  if not fDoClose then
    BeforeFirst;
end;

{ TZKeyAndResultSetPairList }

function MetadataKeyCompare(Item1, Item2: Pointer): Integer;
var P1, P2: Pointer;
begin
  P1 := Pointer(PZKeyAndResultSetPair(Item1).Key);
  if P1 = nil then
    P1 := {$IFDEF UNICODE}PEmptyUnicodeString{$ELSE}PEmptyAnsiString{$ENDIF};
  P2 := Pointer(PZKeyAndResultSetPair(Item2).Key);
  if P2 = nil then
    P2 := {$IFDEF UNICODE}PEmptyUnicodeString{$ELSE}PEmptyAnsiString{$ENDIF};
  Result := StrComp(PChar(P1), PChar(P2));
end;

constructor TZKeyAndResultSetPairList.Create(ElementSize: Cardinal; ElementNeedsFinalize: Boolean);
begin
  inherited Create(@MetadataKeyCompare, ElementSize, ElementNeedsFinalize);
end;

procedure TZKeyAndResultSetPairList.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if Action = lnDeleted then begin
    PZKeyAndResultSetPair(Ptr).Key := '';
    PZKeyAndResultSetPair(Ptr).ResultSet := nil;
  end;
end;

{**
  rerurns cache key for get tables metadata entry
  @param Catalog catalog name
  @param SchemaPattern schema pattern
  @param TableNamePattern table name pattern
  @param Types table types
  @return the cache key string
  @deprecated use TZAbstractDatabaseMetadata.GetTablesCacheKey instead
}
function GetTablesMetaDataCacheKey(Const Catalog:String;
      Const SchemaPattern:String;	Const TableNamePattern:String;const Types: TStringDynArray):String;
Var I : Integer;
    Key :  String;
begin
  Key := '';
  for I := Low(Types) to High(Types) do
    Key := Key + ':' + Types[I];

  Result:= Format('get-tables:%s:%s:%s:%s',
    [Catalog, SchemaPattern, TableNamePattern, Key]);
end;

const
  CharacterSetsColumnsCount = 2;
  CharacterSetsColumns: array[FirstDbcIndex..CharacterSetsColumnsCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'CHARACTER_SET_NAME'; SQLType: stString; Length: 35),
    (Name: 'CHARACTER_SET_ID'; SQLType: stSmall; Length: 0)
  );

  CollationCharSetColumnsCount = 8; //EgonHugeist
  CollationCharSetColumns: array[FirstDbcIndex..CollationCharSetColumnsCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'COLLATION_CATALOG'; SQLType: stString; Length: 35),
    (Name: 'COLLATION_SCHEMA'; SQLType: stString; Length: 35),
    (Name: 'COLLATION_TABLE'; SQLType: stString; Length: 35),
    (Name: 'COLLATION_COLUMN'; SQLType: stString; Length: 35),
    (Name: 'COLLATION_NAME'; SQLType: stString; Length: 35),
    (Name: 'CHARACTER_SET_NAME'; SQLType: stString; Length: 35),
    (Name: 'CHARACTER_SET_ID'; SQLType: stSmall; Length: 0),
    (Name: 'CHARACTER_SET_SIZE'; SQLType: stSmall; Length: 0)
  );

  TriggersColumnCount = 8;
  TriggersColumns: array[FirstDbcIndex..TriggersColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TRIGGER_CAT'; SQLType: stString; Length: 255),
    (Name: 'TRIGGER_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TRIGGER_NAME'; SQLType: stString; Length: 255), //RDB$TRIGGER_NAME
    (Name: 'TRIGGER_RELATION'; SQLType: stString; Length: 255), //RDB$RELATION_NAME
    (Name: 'TRIGGER_TYPE'; SQLType: stSmall; Length: 0),     //RDB$TRIGGER_TYPE
    (Name: 'TRIGGER_INACTIVE'; SQLType: stSmall; Length: 0),     //RDB$TRIGGER_INACTIVE
    (Name: 'TRIGGER_SOURCE'; SQLType: stString; Length: 3000),     //RDB$TRIGGER_SOURCE
    (Name: 'TRIGGER_DESCRIPTION'; SQLType: stString; Length: 255)     //RDB$DESCRIPTION
  );

  ProceduresColumnCount = 8;
  ProceduresColumns: array[FirstDbcIndex..ProceduresColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'PROCEDURE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_OVERLOAD'; SQLType: stString; Length: 255),
    (Name: 'RESERVED1'; SQLType: stString; Length: 255),
    (Name: 'RESERVED2'; SQLType: stString; Length: 255),
    (Name: 'REMARKS'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_TYPE'; SQLType: stSmall; Length: 0)
  );

  ProceduresColColumnCount = 13;
  ProceduresColColumns: array[FirstDbcIndex..ProceduresColColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'PROCEDURE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PROCEDURE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'DATA_TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PRECISION'; SQLType: stInteger; Length: 0),
    (Name: 'LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'SCALE'; SQLType: stSmall; Length: 0),
    (Name: 'RADIX'; SQLType: stSmall; Length: 0),
    (Name: 'NULLABLE'; SQLType: stSmall; Length: 0),
    (Name: 'REMARKS'; SQLType: stString; Length: 255)
  );

  TableColumnCount = 5;
  TableColumns: array[FirstDbcIndex..TableColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'TABLE_TYPE'; SQLType: stString; Length: 255),
    (Name: 'REMARKS'; SQLType: stString; Length: 255)
  );

  SchemaColumnCount = 1;
  SchemaColumns: array[FirstDbcIndex..SchemaColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255)
  );

  CatalogColumnCount = 1;
  CatalogColumns: array[FirstDbcIndex..CatalogColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255)
  );

  TableTypeColumnCount = 1;
  TableTypeColumns: array[FirstDbcIndex..TableTypeColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_TYPE'; SQLType: stString; Length: 255)
  );

  TableColColumnCount = 24;
  TableColColumns: array[FirstDbcIndex..TableColColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_SIZE'; SQLType: stInteger; Length: 0),
    (Name: 'BUFFER_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'DECIMAL_DIGITS'; SQLType: stInteger; Length: 0),
    (Name: 'NUM_PREC_RADIX'; SQLType: stInteger; Length: 0),
    (Name: 'NULLABLE'; SQLType: stSmall; Length: 0),
    (Name: 'REMARKS'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_DEF'; SQLType: stString; Length: 255),
    (Name: 'SQL_DATA_TYPE'; SQLType: stInteger; Length: 0),
    (Name: 'SQL_DATETIME_SUB'; SQLType: stInteger; Length: 0),
    (Name: 'CHAR_OCTET_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'ORDINAL_POSITION'; SQLType: stInteger; Length: 0),
    (Name: 'IS_NULLABLE'; SQLType: stString; Length: 255),
    (Name: 'AUTO_INCREMENT'; SQLType: stBoolean; Length: 0),
    (Name: 'CASE_SENSITIVE'; SQLType: stBoolean; Length: 0),
    (Name: 'SEARCHABLE'; SQLType: stBoolean; Length: 0),
    (Name: 'WRITABLE'; SQLType: stBoolean; Length: 0),
    (Name: 'DEFINITELYWRITABLE'; SQLType: stBoolean; Length: 0),
    (Name: 'READONLY'; SQLType: stBoolean; Length: 0)
  );

  TableColPrivColumnCount = 8;
  TableColPrivColumns: array[FirstDbcIndex..TableColPrivColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'GRANTOR'; SQLType: stString; Length: 255),
    (Name: 'GRANTEE'; SQLType: stString; Length: 255),
    (Name: 'PRIVILEGE'; SQLType: stString; Length: 255),
    (Name: 'IS_GRANTABLE'; SQLType: stString; Length: 255)
  );

  TablePrivColumnCount = 7;
  TablePrivColumns: array[FirstDbcIndex..TablePrivColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'GRANTOR'; SQLType: stString; Length: 255),
    (Name: 'GRANTEE'; SQLType: stString; Length: 255),
    (Name: 'PRIVILEGE'; SQLType: stString; Length: 255),
    (Name: 'IS_GRANTABLE'; SQLType: stString; Length: 255)
  );

  BestRowIdentColumnCount = 8;
  BestRowIdentColumns: array[FirstDbcIndex..BestRowIdentColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'SCOPE'; SQLType: stSmall; Length: 0),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_SIZE'; SQLType: stInteger; Length: 0),
    (Name: 'BUFFER_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'DECIMAL_DIGITS'; SQLType: stSmall; Length: 0),
    (Name: 'PSEUDO_COLUMN'; SQLType: stSmall; Length: 0)
  );

  TableColVerColumnCount = 8;
  TableColVerColumns: array[FirstDbcIndex..TableColVerColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'SCOPE'; SQLType: stSmall; Length: 0),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_SIZE'; SQLType: stInteger; Length: 0),
    (Name: 'BUFFER_LENGTH'; SQLType: stInteger; Length: 0),
    (Name: 'DECIMAL_DIGITS'; SQLType: stSmall; Length: 0),
    (Name: 'PSEUDO_COLUMN'; SQLType: stSmall; Length: 0)
  );

  PrimaryKeyColumnCount = 6;
  PrimaryKeyColumns: array[FirstDbcIndex..PrimaryKeyColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stSmall; Length: 0),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255)
  );

  ImportedKeyColumnCount = 14;
  ImportedKeyColumns: array[FirstDbcIndex..ImportedKeyColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'PKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stSmall; Length: 0),
    (Name: 'UPDATE_RULE'; SQLType: stSmall; Length: 0),
    (Name: 'DELETE_RULE'; SQLType: stSmall; Length: 0),
    (Name: 'FK_NAME'; SQLType: stString; Length: 255),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255),
    (Name: 'DEFERRABILITY'; SQLType: stSmall; Length: 0)
  );

  ExportedKeyColumnCount = 14;
  ExportedKeyColumns: array[FirstDbcIndex..ExportedKeyColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'PKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stSmall; Length: 0),
    (Name: 'UPDATE_RULE'; SQLType: stSmall; Length: 0),
    (Name: 'DELETE_RULE'; SQLType: stSmall; Length: 0),
    (Name: 'FK_NAME'; SQLType: stString; Length: 255),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255),
    (Name: 'DEFERRABILITY'; SQLType: stSmall; Length: 0)
  );

  CrossRefColumnCount = 14;
  CrossRefColumns: array[FirstDbcIndex..CrossRefColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'PKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'PKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'PKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'FKTABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'FKCOLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'KEY_SEQ'; SQLType: stSmall; Length: 0),
    (Name: 'UPDATE_RULE'; SQLType: stSmall; Length: 0),
    (Name: 'DELETE_RULE'; SQLType: stSmall; Length: 0),
    (Name: 'FK_NAME'; SQLType: stString; Length: 255),
    (Name: 'PK_NAME'; SQLType: stString; Length: 255),
    (Name: 'DEFERRABILITY'; SQLType: stSmall; Length: 0)
  );

  TypeInfoColumnCount = 18;
  TypeInfoColumns: array[FirstDbcIndex..TypeInfoColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'PRECISION'; SQLType: stInteger; Length: 0),
    (Name: 'LITERAL_PREFIX'; SQLType: stString; Length: 255),
    (Name: 'LITERAL_SUFFIX'; SQLType: stString; Length: 255),
    (Name: 'CREATE_PARAMS'; SQLType: stString; Length: 255),
    (Name: 'NULLABLE'; SQLType: stSmall; Length: 0),
    (Name: 'CASE_SENSITIVE'; SQLType: stBoolean; Length: 0),
    (Name: 'SEARCHABLE'; SQLType: stSmall; Length: 0),
    (Name: 'UNSIGNED_ATTRIBUTE'; SQLType: stBoolean; Length: 0),
    (Name: 'FIXED_PREC_SCALE'; SQLType: stBoolean; Length: 0),
    (Name: 'AUTO_INCREMENT'; SQLType: stBoolean; Length: 0),
    (Name: 'LOCAL_TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'MINIMUM_SCALE'; SQLType: stSmall; Length: 0),
    (Name: 'MAXIMUM_SCALE'; SQLType: stSmall; Length: 0),
    (Name: 'SQL_DATA_TYPE'; SQLType: stInteger; Length: 0),
    (Name: 'SQL_DATETIME_SUB'; SQLType: stInteger; Length: 0),
    (Name: 'NUM_PREC_RADIX'; SQLType: stInteger; Length: 0)
  );

  IndexInfoColumnCount = 13;
  IndexInfoColumns: array[FirstDbcIndex..IndexInfoColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TABLE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TABLE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TABLE_NAME'; SQLType: stString; Length: 255),
    (Name: 'NON_UNIQUE'; SQLType: stBoolean; Length: 0),
    (Name: 'INDEX_QUALIFIER'; SQLType: stString; Length: 255),
    (Name: 'INDEX_NAME'; SQLType: stString; Length: 255),
    (Name: 'TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'ORDINAL_POSITION'; SQLType: stSmall; Length: 0),
    (Name: 'COLUMN_NAME'; SQLType: stString; Length: 255),
    (Name: 'ASC_OR_DESC'; SQLType: stString; Length: 255),
    (Name: 'CARDINALITY'; SQLType: stInteger; Length: 0),
    (Name: 'PAGES'; SQLType: stInteger; Length: 0),
    (Name: 'FILTER_CONDITION'; SQLType: stString; Length: 255)
  );

  SequenceColumnCount = 3;
  SequenceColumns: array[FirstDbcIndex..SequenceColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef = (
    (Name: 'SEQUENCE_CAT'; SQLType: stString; Length: 255),
    (Name: 'SEQUENCE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'SEQUENCE_NAME'; SQLType: stString; Length: 255)
  );

  UDTColumnCount = 6;
  UDTColumns: array[FirstDbcIndex..UDTColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF}]
    of TZMetadataColumnDef =(
    (Name: 'TYPE_CAT'; SQLType: stString; Length: 255),
    (Name: 'TYPE_SCHEM'; SQLType: stString; Length: 255),
    (Name: 'TYPE_NAME'; SQLType: stString; Length: 255),
    (Name: 'CLASS_NAME'; SQLType: stString; Length: 255),
    (Name: 'DATA_TYPE'; SQLType: stSmall; Length: 0),
    (Name: 'REMARKS'; SQLType: stString; Length: 255)
  );

var
  I: Integer;

initialization
  SetLength(CharacterSetsColumnsDynArray, CharacterSetsColumnsCount);
  for I := FirstDbcIndex to CharacterSetsColumnsCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    CharacterSetsColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := CharacterSetsColumns[I];

  SetLength(CollationCharSetColumnsDynArray, CollationCharSetColumnsCount);
  for I := FirstDbcIndex to CollationCharSetColumnsCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    CollationCharSetColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := CollationCharSetColumns[I];

  SetLength(TriggersColumnsDynArray, TriggersColumnCount);
  for I := FirstDbcIndex to TriggersColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TriggersColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TriggersColumns[I];

  SetLength(ProceduresColumnsDynArray, ProceduresColumnCount);
  for I := FirstDbcIndex to ProceduresColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    ProceduresColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := ProceduresColumns[I];

  SetLength(ProceduresColColumnsDynArray, ProceduresColColumnCount);
  for I := FirstDbcIndex to ProceduresColColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    ProceduresColColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := ProceduresColColumns[I];

  SetLength(TableColumnsDynArray, TableColumnCount);
  for I := FirstDbcIndex to TableColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TableColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TableColumns[I];

  SetLength(SchemaColumnsDynArray, SchemaColumnCount);
  for I := FirstDbcIndex to SchemaColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    SchemaColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := SchemaColumns[I];

  SetLength(CatalogColumnsDynArray, CatalogColumnCount);
  for I := FirstDbcIndex to CatalogColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    CatalogColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := CatalogColumns[I];

  SetLength(TableTypeColumnsDynArray, TableTypeColumnCount);
  for I := FirstDbcIndex to TableTypeColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TableTypeColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TableTypeColumns[I];

  SetLength(TableColColumnsDynArray, TableColColumnCount);
  for I := FirstDbcIndex to TableColColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TableColColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TableColColumns[I];

  SetLength(TableColPrivColumnsDynArray, TableColPrivColumnCount);
  for I := FirstDbcIndex to TableColPrivColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TableColPrivColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TableColPrivColumns[I];

  SetLength(TablePrivColumnsDynArray, TablePrivColumnCount);
  for I := FirstDbcIndex to TablePrivColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TablePrivColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TablePrivColumns[I];

  SetLength(BestRowIdentColumnsDynArray, BestRowIdentColumnCount);
  for I := FirstDbcIndex to BestRowIdentColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    BestRowIdentColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := BestRowIdentColumns[I];

  SetLength(TableColVerColumnsDynArray, TableColVerColumnCount);
  for I := FirstDbcIndex to TableColVerColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TableColVerColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TableColVerColumns[I];

  SetLength(PrimaryKeyColumnsDynArray, PrimaryKeyColumnCount);
  for I := FirstDbcIndex to PrimaryKeyColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    PrimaryKeyColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := PrimaryKeyColumns[I];

  SetLength(ImportedKeyColumnsDynArray, ImportedKeyColumnCount);
  for I := FirstDbcIndex to ImportedKeyColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    ImportedKeyColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := ImportedKeyColumns[I];

  SetLength(ExportedKeyColumnsDynArray, ExportedKeyColumnCount);
  for I := FirstDbcIndex to ExportedKeyColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    ExportedKeyColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := ExportedKeyColumns[I];

  SetLength(CrossRefColumnsDynArray, CrossRefColumnCount);
  for I := FirstDbcIndex to CrossRefColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    CrossRefColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := CrossRefColumns[I];

  SetLength(TypeInfoColumnsDynArray, TypeInfoColumnCount);
  for I := FirstDbcIndex to TypeInfoColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    TypeInfoColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TypeInfoColumns[I];

  SetLength(IndexInfoColumnsDynArray, IndexInfoColumnCount);
  for I := FirstDbcIndex to IndexInfoColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    IndexInfoColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := IndexInfoColumns[I];

  SetLength(SequenceColumnsDynArray, SequenceColumnCount);
  for I := FirstDbcIndex to SequenceColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    SequenceColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := SequenceColumns[I];

  SetLength(UDTColumnsDynArray, UDTColumnCount);
  for I := FirstDbcIndex to UDTColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    UDTColumnsDynArray[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := UDTColumns[I];
end.
