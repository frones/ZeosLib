{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              ODBC metadata information                 }
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

unit ZDbcODBCMetadata;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcMetadata,
  ZCompatibility, ZDbcConnection, ZPlainODBCDriver, ZDbcODBCCon;

type
  {** Implements ODBC Database Information. }
  TZAbstractODBCDatabaseInfo = class(TZAbstractDatabaseInfo)
  protected
    fPHDBC: PSQLHDBC;
    fSQLKeyWords: String;
    function GetODBCConnection: IZODBCConnection;
    function GetBoolDbcInfo(InfoType: SQLUSMALLINT): Boolean;
    function GetUIntDbcInfo(InfoType: SQLUSMALLINT): SQLUINTEGER;
    function GetUSmallDbcInfo(InfoType: SQLUSMALLINT): SQLUSMALLINT;
    function GetStringDbcInfo(InfoType: SQLUSMALLINT): String; virtual; abstract;
  public
    constructor Create(const Metadata: TZAbstractDatabaseMetadata; ConnectionHandleRef: PSQLHDBC);
    // database/driver/server info:
    /// <summary>What's the name of this database product?</summary>
    /// <returns>database product name</returns>
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    /// <summary>What's the name of this ZDBC driver?
    /// <returns>ZDBC driver name</returns>
    function GetDriverName: string; override;
//    function GetDriverVersion: string; override; -> Same as parent
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
    function GetServerVersion: string; override;

    // capabilities (what it can/cannot do):
    function AllProceduresAreCallable: Boolean; override;
    function AllTablesAreSelectable: Boolean; override;
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
//    function SupportsAlterTableWithAddColumn: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithDropColumn: Boolean; override; -> Not implemented
    function SupportsColumnAliasing: Boolean; override;
//    function SupportsConvert: Boolean; override; -> Not implemented
//    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
//      Boolean; override; -> Not implemented

    /// <summary>Are table correlation names supported?
    /// A Zeos Compliant <c>TM</c> driver always returns true.</summary>
    /// <returns><c>true</c> if so; <c>false</c> otherwise</returns>
    function SupportsTableCorrelationNames: Boolean; override;
    function SupportsDifferentTableCorrelationNames: Boolean; override;
    function SupportsExpressionsInOrderBy: Boolean; override;
    function SupportsOrderByUnrelated: Boolean; override;
    function SupportsGroupBy: Boolean; override;
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
    function SupportsLikeEscapeClause: Boolean; override;
    function SupportsMultipleResultSets: Boolean; override;
    function SupportsMultipleTransactions: Boolean; override;
    function SupportsNonNullableColumns: Boolean; override;
    function SupportsMinimumSQLGrammar: Boolean; override;
//    function SupportsCoreSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsExtendedSQLGrammar: Boolean; override; -> Not implemented
    function SupportsANSI92EntryLevelSQL: Boolean; override;
    function SupportsANSI92IntermediateSQL: Boolean; override;
    function SupportsANSI92FullSQL: Boolean; override;
    function SupportsIntegrityEnhancementFacility: Boolean; override;
    function SupportsOuterJoins: Boolean; override;
    function SupportsFullOuterJoins: Boolean; override;
    function SupportsLimitedOuterJoins: Boolean; override;
    function SupportsSchemasInDataManipulation: Boolean; override;
    function SupportsSchemasInProcedureCalls: Boolean; override;
    function SupportsSchemasInTableDefinitions: Boolean; override;
    function SupportsSchemasInIndexDefinitions: Boolean; override;
    function SupportsSchemasInPrivilegeDefinitions: Boolean; override;
    function SupportsCatalogsInDataManipulation: Boolean; override;
    function SupportsCatalogsInProcedureCalls: Boolean; override;
    function SupportsCatalogsInTableDefinitions: Boolean; override;
    function SupportsCatalogsInIndexDefinitions: Boolean; override;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean; override;
    function SupportsOverloadPrefixInStoredProcedureName: Boolean; override;
    function SupportsPositionedDelete: Boolean; override;
    function SupportsPositionedUpdate: Boolean; override;
    function SupportsSelectForUpdate: Boolean; override;
    function SupportsStoredProcedures: Boolean; override;
    function SupportsSubqueriesInComparisons: Boolean; override;
    function SupportsSubqueriesInExists: Boolean; override;
    function SupportsSubqueriesInIns: Boolean; override;
    function SupportsSubqueriesInQuantifieds: Boolean; override;
    function SupportsCorrelatedSubqueries: Boolean; override;
    function SupportsUnion: Boolean; override;
    function SupportsUnionAll: Boolean; override;
    function SupportsOpenCursorsAcrossCommit: Boolean; override;
    function SupportsOpenCursorsAcrossRollback: Boolean; override;
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;
    function SupportsTransactions: Boolean; override;
    function SupportsTransactionIsolationLevel(const Level: TZTransactIsolationLevel): Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function SupportsResultSetType(const {%H-}_Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(const {%H-}_Type: TZResultSetType;
      const {%H-}Concurrency: TZResultSetConcurrency): Boolean; override;
    function SupportsBatchUpdates: Boolean; override;
    function SupportsNonEscapedSearchStrings: Boolean; override;
    function SupportsUpdateAutoIncrementFields: Boolean; override;
    function SupportsMilliSeconds: Boolean; override;
    function SupportsArrayBindings: Boolean; override;

    // maxima:
    function GetMaxBinaryLiteralLength: Integer; override;
    function GetMaxCharLiteralLength: Integer; override;
    function GetMaxColumnNameLength: Integer; override;
    function GetMaxColumnsInGroupBy: Integer; override;
    function GetMaxColumnsInIndex: Integer; override;
    function GetMaxColumnsInOrderBy: Integer; override;
    function GetMaxColumnsInSelect: Integer; override;
    function GetMaxColumnsInTable: Integer; override;
    function GetMaxConnections: Integer; override;
    function GetMaxCursorNameLength: Integer; override;
    function GetMaxIndexLength: Integer; override;
    function GetMaxSchemaNameLength: Integer; override;
    function GetMaxProcedureNameLength: Integer; override;
    function GetMaxCatalogNameLength: Integer; override;
    function GetMaxRowSize: Integer; override;
    function GetMaxStatementLength: Integer; override;
    function GetMaxStatements: Integer; override;
    function GetMaxTableNameLength: Integer; override;
    function GetMaxTablesInSelect: Integer; override;
    function GetMaxUserNameLength: Integer; override;

    // policies (how are various data and operations handled):
    function IsReadOnly: Boolean; override;
    function IsCatalogAtStart: Boolean; override;
    function DoesMaxRowSizeIncludeBlobs: Boolean; override;
    function NullsAreSortedHigh: Boolean; override;
    function NullsAreSortedLow: Boolean; override;
    function NullsAreSortedAtStart: Boolean; override;
    function NullsAreSortedAtEnd: Boolean; override;
//    function NullPlusNonNullIsNull: Boolean; override; -> Not implemented
    function UsesLocalFiles: Boolean; override;
    function UsesLocalFilePerTable: Boolean; override;
    function StoresUpperCaseIdentifiers: Boolean; override;
    function StoresLowerCaseIdentifiers: Boolean; override;
    function StoresMixedCaseIdentifiers: Boolean; override;
    function StoresUpperCaseQuotedIdentifiers: Boolean; override;
    function StoresLowerCaseQuotedIdentifiers: Boolean; override;
    function StoresMixedCaseQuotedIdentifiers: Boolean; override;
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
    function DataDefinitionCausesTransactionCommit: Boolean; override;
    function DataDefinitionIgnoredInTransactions: Boolean; override;

    // interface details (terms, keywords, etc):
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
    function GetCatalogSeparator: string; override;
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;
  end;

  TZODBCDatabaseInfoW = class(TZAbstractODBCDatabaseInfo)
  protected
    function GetStringDbcInfo(InfoType: SQLUSMALLINT): String; override;
  end;

  TZODBCDatabaseInfoA = class(TZAbstractODBCDatabaseInfo)
  protected
    function GetStringDbcInfo(InfoType: SQLUSMALLINT): String; override;
  end;

  TAbstractODBCDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    fPHDBC: PSQLHDBC;
    fTableColColumnMap: TTableColColumnMap;
    fProcedureMap: TProcedureMap;
    fProcedureColumnsColMap: TProcedureColumnsColMap;
  protected
    procedure CheckStmtError(RETCODE: SQLRETURN; StmtHandle: SQLHSTMT;
      const Connection: IZODBCConnection);
    procedure IntializeTableColColumnMap(const RS: IZResultSet);
    procedure IntializeProcedureMap(const RS: IZResultSet);
    procedure IntializeProceduresProcedureColumnsColMap(const RS: IZResultSet);
  protected
    function UncachedGetTableTypes: IZResultSet; override;
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
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
  public
    constructor Create(Connection: TZAbstractDbcConnection; const Url: TZURL; var ConnectionHandle: SQLHDBC); reintroduce; virtual;
  end;

  {** Implements ODBC Metadata. }
  TODBCDatabaseMetadataW = class(TAbstractODBCDatabaseMetadata)
  private
    fPlainW: TODBC3UnicodePlainDriver;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-27
    function DecomposeObjectString(const S: String): UnicodeString; reintroduce;
  protected
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
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
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
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
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
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
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string;
      const Table: string; Unique: Boolean; Approximate: Boolean): IZResultSet; override;
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
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
  public
    constructor Create(Connection: TZAbstractDbcConnection; const Url: TZURL; var ConnectionHandle: SQLHDBC); override;
  end;

  {** Implements ODBC Metadata. }
  TODBCDatabaseMetadataA = class(TAbstractODBCDatabaseMetadata)
  private
    fPlainA: TODBC3RawPlainDriver;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-27
    function DecomposeObjectString(const S: String): RawByteString; reintroduce;
  protected
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
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
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
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
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
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
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string;
      const Table: string; Unique: Boolean; Approximate: Boolean): IZResultSet; override;
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
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
  public
    constructor Create(Connection: TZAbstractDbcConnection; const Url: TZURL; var ConnectionHandle: SQLHDBC); override;
  end;
{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_ODBC} //if set we have an empty unit

uses
  ZGenericSqlToken, ZDbcODBCUtils, ZDbcODBCResultSet, ZDbcLogging,
  ZEncoding, ZSysUtils, ZFastCode {$IFNDEF Unicode},ZDbcUtils{$ENDIF}
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

{ TZAbstractODBCDatabaseInfo }

{**
  Can all the procedures returned by getProcedures be called by the
  current user?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.AllProceduresAreCallable: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_ACCESSIBLE_PROCEDURES);
end;

{**
  Can all the tables returned by getTable be SELECTed by the
  current user?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.AllTablesAreSelectable: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_ACCESSIBLE_TABLES);
end;

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
  @param IdentifierQuotes the default Quotes for Identifiers used by the driver
}
constructor TZAbstractODBCDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata; ConnectionHandleRef: PSQLHDBC);
begin
  inherited Create(MetaData, '[]');
  fPHDBC := ConnectionHandleRef;
  fSQLKeyWords := '';
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

function TZAbstractODBCDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := GetStringDbcInfo(SQL_DBMS_NAME);
end;

{**
  What's the version of this database product?
  @return database version
}
function TZAbstractODBCDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := GetStringDbcInfo(SQL_DBMS_VER);
end;

function TZAbstractODBCDatabaseInfo.GetDriverName: string;
begin
  Result := GetStringDbcInfo(SQL_DRIVER_NAME);
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZAbstractODBCDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

function TZAbstractODBCDatabaseInfo.GetUIntDbcInfo(InfoType: SQLUSMALLINT): SQLUINTEGER;
var
  ODBCConnection: IZODBCConnection;
  Ret: SQLRETURN;
begin
  Result := 0; //satisfy compiler
  ODBCConnection := GetODBCConnection;
  Ret := TZODBC3PlainDriver(ODBCConnection.GetPlainDriver.GetInstance).SQLGetInfo(fPHDBC^,
    InfoType, @Result, SizeOf(SQLUINTEGER), nil);
  if Ret <> SQL_SUCCESS then
    ODBCConnection.HandleErrorOrWarning(ret, fPHDBC^, SQL_HANDLE_DBC,
      'SQLGetInfo', lcOther, ODBCConnection);
end;

function TZAbstractODBCDatabaseInfo.GetUSmallDbcInfo(
  InfoType: SQLUSMALLINT): SQLUSMALLINT;
var
  ODBCConnection: IZODBCConnection;
  Ret: SQLRETURN;
begin
  Result := 0; //satisfy compiler
  ODBCConnection := GetODBCConnection;
  Ret := TZODBC3PlainDriver(ODBCConnection.GetPlainDriver.GetInstance).SQLGetInfo(fPHDBC^,
    InfoType, @Result, SizeOf(SQLUSMALLINT), nil);
  if Ret <> SQL_SUCCESS then
    ODBCConnection.HandleErrorOrWarning(ret, fPHDBC^, SQL_HANDLE_DBC,
      'SQLGetInfo', lcOther, ODBCConnection);
end;

{**
  Does a catalog appear at the start of a qualified table name?
  (Otherwise it appears at the end)
  @return true if it appears at the start
}
function TZAbstractODBCDatabaseInfo.IsCatalogAtStart: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_CATALOG_LOCATION) and SQL_CL_START = SQL_CL_START;
end;

{**
  Is the database in read-only mode?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.IsReadOnly: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_DATA_SOURCE_READ_ONLY);
end;

{**
  Are NULL values sorted at the end regardless of sort order?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.NullsAreSortedAtEnd: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_NULL_COLLATION) and SQL_NC_END = SQL_NC_END;
end;

{**
  Are NULL values sorted at the start regardless of sort order?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.NullsAreSortedAtStart: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_NULL_COLLATION) and SQL_NC_START = SQL_NC_START;
end;

{**
  Are NULL values sorted high?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.NullsAreSortedHigh: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_NULL_COLLATION) and SQL_NC_HIGH = SQL_NC_HIGH;
end;

{**
  Are NULL values sorted low?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.NullsAreSortedLow: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_NULL_COLLATION) and SQL_NC_LOW = SQL_NC_LOW;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZAbstractODBCDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZAbstractODBCDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_FILE_USAGE) and SQL_FILE_TABLE = SQL_FILE_TABLE;
end;

{**
  Does the database store tables in a local file?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.UsesLocalFiles: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_FILE_TABLE) and SQL_FILE_CATALOG = SQL_FILE_CATALOG;
end;

{**
  Does the database driver supports milliseconds?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsMilliSeconds: Boolean;
begin
  //https://social.msdn.microsoft.com/Forums/sqlserver/en-US/ac1b5a6d-5e64-4603-9c92-b75ba4e51bf2/error-22008-datetime-field-overflow-when-inserting-a-record-with-datetime2-field-via-odbc?forum=sqldataaccess
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will
  always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsMinimumSQLGrammar: Boolean;
begin
  Result := True;
end;

function TZAbstractODBCDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_IDENTIFIER_CASE) in [SQL_IC_SENSITIVE, SQL_IC_MIXED];
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_IDENTIFIER_CASE) and SQL_IC_UPPER = SQL_IC_UPPER;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_IDENTIFIER_CASE) and SQL_IC_LOWER = SQL_IC_LOWER;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
var Value: SQLUSMALLINT;
begin
  Value := GetUSmallDbcInfo(SQL_IDENTIFIER_CASE);
  Result := (Value and SQL_IC_SENSITIVE = SQL_IC_SENSITIVE) or
            (Value and SQL_IC_MIXED = SQL_IC_MIXED)
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
var Value: SQLUSMALLINT;
begin
  Value := GetUSmallDbcInfo(SQL_IDENTIFIER_CASE);
  Result := (Value and SQL_IC_SENSITIVE = SQL_IC_SENSITIVE) or
            (Value and SQL_IC_MIXED = SQL_IC_MIXED)
end;

{**
  Are multiple <code>ResultSet</code> from a single execute supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsMultipleResultSets: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_MULT_RESULT_SETS);
end;

{**
  Can we have multiple transactions open at once (on different
  connections)?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsMultipleTransactions: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_MULTIPLE_ACTIVE_TXN);
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_QUOTED_IDENTIFIER_CASE) and SQL_IC_UPPER = SQL_IC_UPPER;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_QUOTED_IDENTIFIER_CASE) and SQL_IC_LOWER = SQL_IC_LOWER;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
var value: SQLUSMALLINT;
begin
  Value := GetUSmallDbcInfo(SQL_QUOTED_IDENTIFIER_CASE);
  Result := (Value and SQL_IC_SENSITIVE = SQL_IC_SENSITIVE) or
            (Value and SQL_IC_MIXED = SQL_IC_MIXED);
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZAbstractODBCDatabaseInfo.GetSQLKeywords: string;
begin
  if fSQLKeyWords = '' then
    fSQLKeyWords := GetStringDbcInfo(SQL_KEYWORDS);
  Result := fSQLKeyWords;
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractODBCDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATN2,CEILING,COS,COT,DEGREES,EXP,FLOOR,LOG,LOG10,'+
            'PI,POWER,RADIANS,RAND,ROUND,SIGN,SIN,SQUARE,SQRT,TAN';
end;

function TZAbstractODBCDatabaseInfo.GetODBCConnection: IZODBCConnection;
begin
  MetaData.GetConnection.QueryInterface(IZODBCConnection, Result);
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractODBCDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'ASCII,CHAR,CHARINDEX,DIFFERENCE,LEFT,LEN,LOWER,LTRIM,NCHAR,PATINDEX,'+
            'REPLACE,QUOTENAME,REPLICATE,REVERSE,RIGHT,RTRIM,SOUNDEX,SPACE,STR,'+
            'STUFF,SUBSTRING,UNICODE,UPPER';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAbstractODBCDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'APP_NAME,CASE,CAST,CONVERT,COALESCE,CURRENT_TIMESTAMP,CURRENT_USER,'+
            'DATALENGTH,@@ERROR,FORMATMESSAGE,GETANSINULL,HOST_ID,HOST_NAME,'+
            'IDENT_INCR,IDENT_SEED,@@IDENTITY,IDENTITY,ISDATE,ISNULL,ISNUMERIC,'+
            'NEWID,NULLIF,PARSENAME,PERMISSIONS,@@ROWCOUNT,SESSION_USER,STATS_DATE,'+
            'SYSTEM_USER,@@TRANCOUNT,USER_NAME';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZAbstractODBCDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := 'DATEADD,DATEDIFF,DATENAME,DATEPART,DAY,GETDATE,MONTH,YEAR';
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
function TZAbstractODBCDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := GetStringDbcInfo(SQL_SEARCH_PATTERN_ESCAPE);
end;

{**
  Returns the server version
  @return the server version string
}
function TZAbstractODBCDatabaseInfo.GetServerVersion: string;
begin
  Result := GetStringDbcInfo(SQL_DBMS_VER);
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZAbstractODBCDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := GetStringDbcInfo(SQL_SPECIAL_CHARACTERS);
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_EXPRESSIONS_IN_ORDERBY);
end;

{**
  Are full nested outer joins supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsFullOuterJoins: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_OJ_CAPABILITIES) and SQL_OJ_FULL = SQL_OJ_FULL;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := not GetBoolDbcInfo(SQL_ORDER_BY_COLUMNS_IN_SELECT);
end;

{**
  Is some form of outer join supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsOuterJoins: Boolean;
var
  Value: SQLUINTEGER;
begin
  Value := GetUIntDbcInfo(SQL_OJ_CAPABILITIES);
  Result := (Value and SQL_OJ_LEFT = SQL_OJ_LEFT) or
            (Value and SQL_OJ_RIGHT = SQL_OJ_RIGHT) or
            (Value and SQL_OJ_FULL = SQL_OJ_FULL) or
            (Value and SQL_OJ_NESTED = SQL_OJ_NESTED) or
            (Value and SQL_OJ_NOT_ORDERED = SQL_OJ_NOT_ORDERED) or
            (Value and SQL_OJ_INNER = SQL_OJ_INNER) or
            (Value and SQL_OJ_ALL_COMPARISON_OPS = SQL_OJ_ALL_COMPARISON_OPS);
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_GROUP_BY) and SQL_GB_NOT_SUPPORTED <> SQL_GB_NOT_SUPPORTED;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_GROUP_BY) and SQL_GB_NO_RELATION = SQL_GB_NO_RELATION;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_GROUP_BY) and SQL_GB_GROUP_BY_CONTAINS_SELECT = SQL_GB_GROUP_BY_CONTAINS_SELECT;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_INTEGRITY);
end;

function TZAbstractODBCDatabaseInfo.SupportsLikeEscapeClause: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_LIKE_ESCAPE_CLAUSE);
end;

{**
  Is there limited support for outer joins?  (This will be true
  if supportFullOuterJoins is false.)
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsLimitedOuterJoins: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_OJ_CAPABILITIES) and SQL_OJ_FULL <> SQL_OJ_FULL;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZAbstractODBCDatabaseInfo.GetSchemaTerm: string;
begin
  Result := GetStringDbcInfo(SQL_SCHEMA_TERM);
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZAbstractODBCDatabaseInfo.GetProcedureTerm: string;
begin
  Result := GetStringDbcInfo(SQL_PROCEDURE_TERM);
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZAbstractODBCDatabaseInfo.GetCatalogTerm: string;
begin
  Result := GetStringDbcInfo(SQL_QUALIFIER_TERM);
end;

function TZAbstractODBCDatabaseInfo.GetBoolDbcInfo(InfoType: SQLUSMALLINT): Boolean;
var
  Buf: array[0..1] of WideChar;
  PropLength: SQLSMALLINT;
  PlainW: IODBC3UnicodePlainDriver;
  PlainA: IODBC3RawPlainDriver;
  Plain: TZODBC3PlainDriver;
  ODBCConnection: IZODBCConnection;
  Ret: SQLRETURN;
begin
  Result := False; //satisfy compiler
  ODBCConnection := GetODBCConnection;
  plain := ODBCConnection.GetPlainDriver;
  if Plain.GetInterface(IODBC3UnicodePlainDriver, PlainW) then begin
    Ret := Plain.SQLGetInfo(fPHDBC^, InfoType, @Buf[0], SizeOf(Buf), @PropLength);
    Result := ZSysUtils.StrToBoolEx(PWideChar(@Buf[0]), False)
  end else if Plain.GetInterface(IODBC3RawPlainDriver, PlainA) then begin
    Ret := Plain.SQLGetInfo(fPHDBC^, InfoType, @Buf[0], SizeOf(Buf), @PropLength);
    Result := ZSysUtils.StrToBoolEx(PAnsiChar(@Buf[0]), False)
  end else Ret := SQL_SUCCESS;
  if Ret <> SQL_SUCCESS then
    ODBCConnection.HandleErrorOrWarning(ret, fPHDBC^, SQL_HANDLE_DBC,
      'SQLGetInfo', lcOther, ODBCConnection);
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZAbstractODBCDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := GetStringDbcInfo(SQL_CATALOG_NAME_SEPARATOR);
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SCHEMA_USAGE)and SQL_SU_DML_STATEMENTS = SQL_SU_DML_STATEMENTS
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SCHEMA_USAGE) and SQL_SU_DML_STATEMENTS = SQL_SU_DML_STATEMENTS;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SCHEMA_USAGE) and SQL_SU_TABLE_DEFINITION = SQL_SU_TABLE_DEFINITION;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SCHEMA_USAGE) and SQL_SU_INDEX_DEFINITION = SQL_SU_INDEX_DEFINITION;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SCHEMA_USAGE) and SQL_SU_PRIVILEGE_DEFINITION = SQL_SU_PRIVILEGE_DEFINITION;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_CATALOG_USAGE) and SQL_CU_DML_STATEMENTS = SQL_CU_DML_STATEMENTS;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_CATALOG_USAGE) and SQL_CU_PROCEDURE_INVOCATION = SQL_CU_PROCEDURE_INVOCATION;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_CATALOG_USAGE) and SQL_CU_TABLE_DEFINITION = SQL_CU_TABLE_DEFINITION;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_CATALOG_USAGE) and SQL_CU_INDEX_DEFINITION = SQL_CU_INDEX_DEFINITION;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_CATALOG_USAGE) and SQL_CU_PRIVILEGE_DEFINITION = SQL_CU_PRIVILEGE_DEFINITION;
end;

{**
  Can a stored procedure have an additional overload suffix?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsOverloadPrefixInStoredProcedureName: Boolean;
begin
  Result := True;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_DYNAMIC_CURSOR_ATTRIBUTES1) and SQL_CA1_POS_DELETE = SQL_CA1_POS_DELETE;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_DYNAMIC_CURSOR_ATTRIBUTES1) and SQL_CA1_POS_UPDATE = SQL_CA1_POS_UPDATE;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_DYNAMIC_CURSOR_ATTRIBUTES1) and SQL_CA1_SELECT_FOR_UPDATE = SQL_CA1_SELECT_FOR_UPDATE;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_PROCEDURES)
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SUBQUERIES) and SQL_SQ_COMPARISON = SQL_SQ_COMPARISON;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SUBQUERIES) and SQL_SQ_EXISTS = SQL_SQ_EXISTS;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SUBQUERIES) and SQL_SQ_IN = SQL_SQ_IN;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SUBQUERIES) and SQL_SQ_QUANTIFIED = SQL_SQ_QUANTIFIED;
end;

{**
  Is column aliasing supported?

  <P>If so, the SQL AS clause can be used to provide names for
  computed columns or to provide alias names for columns as
  required.
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsColumnAliasing: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_COLUMN_ALIAS);
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SUBQUERIES) and SQL_SQ_CORRELATED_SUBQUERIES = SQL_SQ_CORRELATED_SUBQUERIES;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_UNION) and SQL_U_UNION = SQL_U_UNION;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_UNION) and SQL_U_UNION_ALL = SQL_U_UNION_ALL;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractODBCDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_CURSOR_COMMIT_BEHAVIOR) = SQL_CB_PRESERVE;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractODBCDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_CURSOR_ROLLBACK_BEHAVIOR) = SQL_CB_PRESERVE;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractODBCDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_CURSOR_ROLLBACK_BEHAVIOR) <> SQL_CB_DELETE ;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAbstractODBCDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_CURSOR_COMMIT_BEHAVIOR) <> SQL_CB_DELETE ;
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
function TZAbstractODBCDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_BINARY_LITERAL_LEN);
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_CHAR_LITERAL_LEN);
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_COLUMN_NAME_LEN);
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAXIMUM_COLUMNS_IN_GROUP_BY);
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_COLUMNS_IN_INDEX);
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_COLUMNS_IN_ORDER_BY);
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_COLUMNS_IN_SELECT);
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_COLUMNS_IN_TABLE);
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_DRIVER_CONNECTIONS);
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_CURSOR_NAME_LEN);
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_INDEX_SIZE);
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_SCHEMA_NAME_LEN);
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_PROCEDURE_NAME_LEN);
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := GetUSmallDbcInfo(SQL_MAX_QUALIFIER_NAME_LEN);
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_ROW_SIZE);
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := GetBoolDbcInfo(SQL_MAX_ROW_SIZE_INCLUDES_LONG);
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_STATEMENT_LEN);
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := GetUIntDbcInfo(SQL_ACTIVE_STATEMENTS);
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_TABLE_NAME_LEN);
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_TABLES_IN_SELECT);
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAbstractODBCDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := GetUIntDbcInfo(SQL_MAX_USER_NAME_LEN);
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZAbstractODBCDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
var Value: SQLUINTEGER;
begin
  Value := GetUIntDbcInfo(SQL_DEFAULT_TXN_ISOLATION);
  case Value of
    SQL_TRANSACTION_READ_UNCOMMITTED: Result := tiReadUncommitted;
    SQL_TRANSACTION_REPEATABLE_READ:  Result := tiRepeatableRead;
    SQL_TRANSACTION_SERIALIZABLE:     Result := tiSerializable;
    else
      Result := tiReadCommitted;
  end;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_TXN_CAPABLE) and SQL_TC_NONE <> SQL_TC_NONE;
end;

function TZAbstractODBCDatabaseInfo.SupportsTableCorrelationNames: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_CORRELATION_NAME) and SQL_CN_NONE <> SQL_CN_NONE;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZAbstractODBCDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  case Level of
    tiReadUncommitted: Result := GetUSmallDbcInfo(SQL_TXN_ISOLATION_OPTION) and SQL_TXN_READ_UNCOMMITTED = SQL_TXN_READ_UNCOMMITTED;
    tiReadCommitted: Result := GetUSmallDbcInfo(SQL_TXN_ISOLATION_OPTION) and SQL_TXN_READ_COMMITTED = SQL_TXN_READ_COMMITTED;
    tiRepeatableRead: Result := GetUSmallDbcInfo(SQL_TXN_ISOLATION_OPTION) and SQL_TXN_REPEATABLE_READ = SQL_TXN_REPEATABLE_READ;
    tiSerializable: Result := GetUSmallDbcInfo(SQL_TXN_ISOLATION_OPTION) and SQL_TXN_SERIALIZABLE = SQL_TXN_SERIALIZABLE;
    else Result := False;
  end;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_TXN_CAPABLE) and SQL_TC_ALL = SQL_TC_ALL;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsDataManipulationTransactionsOnly: Boolean;
var Value: SQLUSMALLINT;
begin
  Value := GetUSmallDbcInfo(SQL_TXN_CAPABLE);
  Result := (Value and SQL_TC_DML = SQL_TC_DML) and
            (Value and SQL_TC_ALL <> SQL_TC_ALL);
end;

{**
  If table correlation names are supported, are they restricted
  to be different from the names of the tables?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsDifferentTableCorrelationNames: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_CORRELATION_NAME) and SQL_CN_NONE <> SQL_CN_NONE;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_TXN_CAPABLE) and SQL_TC_DDL_COMMIT = SQL_TC_DDL_COMMIT;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_TXN_CAPABLE) and SQL_TC_DDL_IGNORE = SQL_TC_DDL_IGNORE;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsResultSetType(
  const _Type: TZResultSetType): Boolean;
begin
  case _Type of
    rtForwardOnly: Result := GetUIntDbcInfo(SQL_SCROLL_OPTIONS) and SQL_SO_FORWARD_ONLY = SQL_SO_FORWARD_ONLY;
    {rtScrollInsensitive: ; yub they are but not implemented
    rtScrollSensitive: ; }
    else Result := False;
  end;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsResultSetConcurrency(const _Type: TZResultSetType;
  const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := True;
end;

{**
  Does the Database or Actual Version understand non escaped search strings?
  @return <code>true</code> if the DataBase does understand non escaped
  search strings
}
function TZAbstractODBCDatabaseInfo.SupportsNonEscapedSearchStrings: Boolean;
begin
  Result := True;
end;

{**
  Can columns be defined as non-nullable?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsNonNullableColumns: Boolean;
begin
  Result := GetUSmallDbcInfo(SQL_NON_NULLABLE_COLUMNS) and SQL_NNC_NON_NULL = SQL_NNC_NON_NULL;
end;

{**
  Does the Database support updating auto incremental fields?
  @return <code>true</code> if the DataBase allows it.
}
function TZAbstractODBCDatabaseInfo.SupportsUpdateAutoIncrementFields: Boolean;
begin
  Result := False;
end;

{**
  Is the ANSI92 entry level SQL grammar supported?
  All JDBC Compliant<sup><font size=-2>TM</font></sup> drivers must return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsANSI92EntryLevelSQL: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SQL_CONFORMANCE) and SQL_SC_SQL92_ENTRY = SQL_SC_SQL92_ENTRY;
end;

{**
  Is the ANSI92 full SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsANSI92FullSQL: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SQL_CONFORMANCE) and SQL_SC_SQL92_FULL = SQL_SC_SQL92_FULL;
end;

{**
  Is the ANSI92 intermediate SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsANSI92IntermediateSQL: Boolean;
begin
  Result := GetUIntDbcInfo(SQL_SQL_CONFORMANCE) and SQL_SC_SQL92_INTERMEDIATE = SQL_SC_SQL92_INTERMEDIATE;
end;

{**
  Does the Database support binding arrays? Is the ZDbc ready for this?
  @return <code>true</code> if the DataBase allows it.
}
function TZAbstractODBCDatabaseInfo.SupportsArrayBindings: Boolean;
begin
  Result := GetODBCConnection.GetArrayRowSupported or GetODBCConnection.GetArraySelectSupported;
end;


{**
  Indicates whether the driver supports batch updates.
  @return true if the driver supports batch updates; false otherwise
}
function TZAbstractODBCDatabaseInfo.SupportsBatchUpdates: Boolean;
begin
  Result := GetODBCConnection.GetArrayRowSupported;
end;

{ TODBCDatabaseMetadataW }

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
constructor TODBCDatabaseMetadataW.Create(Connection: TZAbstractDbcConnection;
  const Url: TZURL; var ConnectionHandle: SQLHDBC);
begin
  inherited Create(Connection, URL, ConnectionHandle);
  fPlainW := Connection.GetIZPlainDriver.GetInstance as TODBC3UnicodePlainDriver;
end;

function TODBCDatabaseMetadataW.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZODBCDatabaseInfoW.Create(Self, fPHDBC);
end;

function TODBCDatabaseMetadataW.DecomposeObjectString(const S: String): UnicodeString;
{$IFNDEF UNICODE}
var tmp: String;
    CP: Word;
{$ENDIF}
begin
  {$IFNDEF UNICODE}
  CP := GetW2A2WConversionCodePage(ConSettings);
  {$ENDIF}
  if S = ''
  then Result := ''
  else if IC.IsQuoted(S) then begin
    {$IFDEF UNICODE}
    Result := IC.ExtractQuote(S)
    {$ELSE}
    tmp := IC.ExtractQuote(S);
    Result := ZRawToUnicode(tmp, CP);
    {$ENDIF}
  end else {$IFDEF UNICODE}
    Result := S;
    {$ELSE}
    Result := ZRawToUnicode(S, CP);
    {$ENDIF}
end;

function TODBCDatabaseMetadataW.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Proc: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Proc := DecomposeObjectString(ProcedureNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLProceduresW(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Proc), Length(Proc)), HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        IntializeProcedureMap(RS);
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(fProcedureMap.ColIndices[CatalogNameIndex], Len), Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(fProcedureMap.ColIndices[SchemaNameIndex], Len), Len);
        Result.UpdatePWideChar(ProcedureNameIndex, GetPWideChar(fProcedureMap.ColIndices[ProcedureNameIndex], Len), Len);
        //overload?
        Result.UpdatePWideChar(ProcedureRemarksIndex, GetPWideChar(fProcedureMap.ColIndices[ProcedureRemarksIndex], Len), Len);
        Result.UpdateSmall(ProcedureTypeIndex, GetSmall(fProcedureMap.ColIndices[ProcedureTypeIndex]) - 1);
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of a catalog's stored procedure parameters
  and result columns.

  <P>Only descriptions matching the schema, procedure and
  parameter name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
  if any, is first. Next are the parameter descriptions in call
  order. The column descriptions follow in column number order.

  <P>Each row in the <code>ResultSet</code> is a parameter description or
  column description with the following fields:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
 	<LI><B>COLUMN_NAME</B> String => column/parameter name
 	<LI><B>COLUMN_TYPE</B> Short => kind of column/parameter:
       <UL>
       <LI> procedureColumnUnknown - nobody knows
       <LI> procedureColumnIn - IN parameter
       <LI> procedureColumnInOut - INOUT parameter
       <LI> procedureColumnOut - OUT parameter
       <LI> procedureColumnReturn - procedure return value
       <LI> procedureColumnResult - result column in <code>ResultSet</code>
       </UL>
   <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => SQL type name, for a UDT type the
   type name is fully qualified
 	<LI><B>PRECISION</B> int => precision
 	<LI><B>LENGTH</B> int => length in bytes of data
 	<LI><B>SCALE</B> short => scale
 	<LI><B>RADIX</B> short => radix
 	<LI><B>NULLABLE</B> short => can it contain NULL?
       <UL>
       <LI> procedureNoNulls - does not allow NULL values
       <LI> procedureNullable - allows NULL values
       <LI> procedureNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing parameter/column
   </OL>

  <P><B>Note:</B> Some databases may not return the column
  descriptions for a procedure. Additional columns beyond
  REMARKS can be defined by the database.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row describes a stored procedure parameter or
       column
  @see #getSearchStringEscape
}
function TODBCDatabaseMetadataW.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  SQLType: TZSQLType;
  Cat, Schem, Proc, Col: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Proc := DecomposeObjectString(ProcedureNamePattern);
  Col := DecomposeObjectString(ColumnNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLProcedureColumnsW(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Proc), Length(Proc), Pointer(Col), Length(Col)), HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        IntializeProceduresProcedureColumnsColMap(RS);
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(fProcedureColumnsColMap.ColIndices[CatalogNameIndex], Len), Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(fProcedureColumnsColMap.ColIndices[SchemaNameIndex], Len), Len);
        Result.UpdatePWideChar(ProcColProcedureNameIndex, GetPWideChar(fProcedureColumnsColMap.ColIndices[ProcColProcedureNameIndex], Len), Len);
        Result.UpdatePWideChar(ProcColColumnNameIndex, GetPWideChar(fProcedureColumnsColMap.ColIndices[ProcColColumnNameIndex], Len), Len);
        case GetSmall(fProcedureColumnsColMap.ColIndices[ProcColColumnTypeIndex]) of
          SQL_PARAM_INPUT:        Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctIn));
          SQL_PARAM_INPUT_OUTPUT: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
          SQL_RESULT_COL:         Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctResultSet));
          SQL_PARAM_OUTPUT:       Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctOut));
          SQL_RETURN_VALUE:       Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctReturn));
          else                    Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
        end;
        SQLType := ConvertODBCTypeToSQLType(GetSmall(fProcedureColumnsColMap.ColIndices[ProcColDataTypeIndex]),
          GetSmall(fProcedureColumnsColMap.ColIndices[ProcColScaleIndex]),
          GetInt(fProcedureColumnsColMap.ColIndices[ProcColPrecisionIndex]), False, ConSettings, nil);
        if (Ord(SQLType) < Ord(stFloat)) and (Ord(SQLType) > Ord(stBoolean)) then
          if SQLType = stShort then //spezial case: MSSQL should map stByte / MySQL should use stShort
            SQLType := stByte
          else //test unsigned
            SQLType := TZSQLType(Ord(SQLType)-Ord(ZFastCode.Pos('U', UpperCase(GetString(fProcedureColumnsColMap.ColIndices[TableColColumnTypeNameIndex]))) > 0));
        Result.UpdateSmall(ProcColDataTypeIndex, Ord(SQLType));
        Result.UpdatePWideChar(ProcColTypeNameIndex, GetPWideChar(fProcedureColumnsColMap.ColIndices[ProcColTypeNameIndex], Len), Len);
        Result.UpdateInt(ProcColPrecisionIndex, GetInt(fProcedureColumnsColMap.ColIndices[ProcColPrecisionIndex]));
        Result.UpdateInt(ProcColLengthIndex, GetInt(fProcedureColumnsColMap.ColIndices[ProcColLengthIndex]));
        Result.UpdateSmall(ProcColScaleIndex, GetSmall(fProcedureColumnsColMap.ColIndices[ProcColScaleIndex]));
        Result.UpdateSmall(ProcColRadixIndex, GetSmall(fProcedureColumnsColMap.ColIndices[ProcColRadixIndex]));
        Result.UpdateSmall(ProcColNullableIndex, GetSmall(fProcedureColumnsColMap.ColIndices[ProcColNullableIndex]));
        Result.UpdatePWideChar(ProcColRemarksIndex, GetPWideChar(fProcedureColumnsColMap.ColIndices[ProcColRemarksIndex], Len), Len);
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of tables available in a catalog.

  <P>Only table descriptions matching the catalog, schema, table
  name and type criteria are returned.  They are ordered by
  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.

  <P>Each table description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	<LI><B>REMARKS</B> String => explanatory comment on the table
   </OL>

  <P><B>Note:</B> Some databases may not return information for
  all tables.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param types a list of table types to include; null returns all types
  @return <code>ResultSet</code> - each row is a table description
  @see #getSearchStringEscape
}
function TODBCDatabaseMetadataW.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Table, TableTypes: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Table := DecomposeObjectString(TableNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  TableTypes := '';
  for I := Low(Types) to High(Types) do begin
    if Length(TableTypes) > 0 then
      TableTypes := TableTypes + ',';
    {$IFDEF UNICODE}
    TableTypes := TableTypes + Types[I];
    {$ELSE}
    TableTypes := TableTypes + ZRawToUnicode(Types[I], GetW2A2WConversionCodePage(ConSettings));
    {$ENDIF}
  end;
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLTablesW(HSTMT, Pointer(Cat), Length(Cat), Pointer(Schem), Length(Schem),
    Pointer(Table), Length(Table), Pointer(TableTypes), Length(TableTypes)), HSTMT, ODBCConnection);
  if Assigned(RS) then with RS do begin
    while Next do begin
      Result.MoveToInsertRow;
      Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(CatalogNameIndex, Len), Len);
      Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(SchemaNameIndex, Len), Len);
      Result.UpdatePWideChar(TableNameIndex, GetPWideChar(TableNameIndex, Len), Len);
      Result.UpdatePWideChar(TableColumnsSQLType, GetPWideChar(TableColumnsSQLType, Len), Len);
      Result.UpdatePWideChar(TableColumnsRemarks, GetPWideChar(TableColumnsRemarks, Len), Len);
      Result.InsertRow;
    end;
    Close;
  end;
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
function TODBCDatabaseMetadataW.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  SQLType: TZSQLType;
  Cat, Schem, Table, Column: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Table := DecomposeObjectString(TableNamePattern);
  Column := DecomposeObjectString(ColumnNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLColumnsW(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Table), Length(Table),
    Pointer(Column), Length(Column)), HSTMT, ODBCConnection);
  if Assigned(RS) then begin
    with RS do begin
      while Next do begin
        IntializeTableColColumnMap(RS);
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(fTableColColumnMap.ColIndices[CatalogNameIndex], Len), Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(fTableColColumnMap.ColIndices[SchemaNameIndex], Len), Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideChar(fTableColColumnMap.ColIndices[TableNameIndex], Len), Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideChar(fTableColColumnMap.ColIndices[ColumnNameIndex], Len), Len);
        SQLType := ConvertODBCTypeToSQLType(GetSmall(fTableColColumnMap.ColIndices[TableColColumnTypeIndex]),
          GetInt(fTableColColumnMap.ColIndices[TableColColumnDecimalDigitsIndex]),
          SmallInt(GetInt(fTableColColumnMap.ColIndices[TableColColumnSizeIndex])), False, ConSettings, nil);
        if (Ord(SQLType) < Ord(stFloat)) and (Ord(SQLType) > Ord(stBoolean)) then
          if SQLType = stShort then //spezial case: MSSQL should map stByte / MySQL should use stShort
            SQLType := stByte
          else //test unsigned
            SQLType := TZSQLType(Ord(SQLType)-Ord(ZFastCode.Pos('U', UpperCase(GetString(fTableColColumnMap.ColIndices[TableColColumnTypeNameIndex]))) > 0));
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(SQLType));
        Result.UpdatePWideChar(TableColColumnTypeNameIndex, GetPWideChar(fTableColColumnMap.ColIndices[TableColColumnTypeNameIndex], Len), Len);
        Result.UpdateInt(TableColColumnSizeIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnSizeIndex]));
        Result.UpdateInt(TableColColumnBufLengthIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnBufLengthIndex]));
        Result.UpdateInt(TableColColumnDecimalDigitsIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnDecimalDigitsIndex]));
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnNumPrecRadixIndex]));
        Result.UpdateSmall(TableColColumnNullableIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnNullableIndex]));
        Result.UpdatePWideChar(TableColColumnRemarksIndex, GetPWideChar(fTableColColumnMap.ColIndices[TableColColumnRemarksIndex], Len), Len);
        Result.UpdatePWideChar(TableColColumnColDefIndex, GetPWideChar(fTableColColumnMap.ColIndices[TableColColumnColDefIndex], Len), Len);
        Result.UpdateSmall(TableColColumnSQLDataTypeIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnSQLDataTypeIndex]));
        Result.UpdateSmall(TableColColumnSQLDateTimeSubIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnSQLDateTimeSubIndex]));
        Result.UpdateInt(TableColColumnCharOctetLengthIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnCharOctetLengthIndex]));
        Result.UpdateInt(TableColColumnOrdPosIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnOrdPosIndex]));
        Result.UpdatePWideChar(TableColColumnIsNullableIndex, GetPWideChar(fTableColColumnMap.ColIndices[TableColColumnIsNullableIndex], Len), Len);
        { here equals do end just test if we know the column then do something }
        if fTableColColumnMap.ColIndices[TableColColumnAutoIncIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnAutoIncIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnAutoIncIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnCaseSensitiveIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnCaseSensitiveIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnSearchableIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnSearchableIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnSearchableIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnWritableIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnWritableIndex, not GetBoolean(fTableColColumnMap.ColIndices[TableColColumnWritableIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnDefinitelyWritableIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, not GetBoolean(fTableColColumnMap.ColIndices[TableColColumnDefinitelyWritableIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnReadonlyIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnReadonlyIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnReadonlyIndex]));
        Result.InsertRow;
      end;
      Close;
    end;
  end;
end;

function TODBCDatabaseMetadataW.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl, Col: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(Schema);
  Tabl := DecomposeObjectString(Table);
  Col := DecomposeObjectString(ColumnNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLColumnPrivilegesW(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Tabl), Length(Tabl), Pointer(Col), Length(Col)), HSTMT, ODBCConnection);
  if Assigned(RS) then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(CatalogNameIndex, Len), Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(SchemaNameIndex, Len), Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideChar(TableNameIndex, Len), Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideChar(ColumnNameIndex, Len), Len);
        Result.UpdatePWideChar(TableColPrivGrantorIndex, GetPWideChar(TableColPrivGrantorIndex, Len), Len);
        Result.UpdatePWideChar(TableColPrivGranteeIndex, GetPWideChar(TableColPrivGranteeIndex, Len), Len);
        Result.UpdatePWideChar(TableColPrivPrivilegeIndex, GetPWideChar(TableColPrivPrivilegeIndex, Len), Len);
        Result.UpdatePWideChar(TableColPrivIsGrantableIndex, GetPWideChar(TableColPrivIsGrantableIndex, Len), Len);
        Result.InsertRow;
      end;
      Close;
    end;
end;

function TODBCDatabaseMetadataW.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Table: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Table := DecomposeObjectString(TableNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLTablePrivilegesW(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Table), Length(Table)), HSTMT, ODBCConnection);
  if Assigned(RS) then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(CatalogNameIndex, Len), Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(SchemaNameIndex, Len), Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideChar(TableNameIndex, Len), Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideChar(ColumnNameIndex, Len), Len);
        Result.UpdatePWideChar(TablePrivGrantorIndex, GetPWideChar(TablePrivGrantorIndex, Len), Len);
        Result.UpdatePWideChar(TablePrivGranteeIndex, GetPWideChar(TablePrivGranteeIndex, Len), Len);
        Result.UpdatePWideChar(TablePrivPrivilegeIndex, GetPWideChar(TablePrivPrivilegeIndex, Len), Len);
        Result.UpdatePWideChar(TablePrivIsGrantableIndex, GetPWideChar(TablePrivIsGrantableIndex, Len), Len);
        Result.InsertRow;
      end;
      Close;
    end;
end;

function TODBCDatabaseMetadataW.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(Schema);
  Tabl := DecomposeObjectString(Table);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLPrimaryKeysW(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Tabl), Length(Tabl)), HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(CatalogNameIndex, Len), Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(SchemaNameIndex, Len), Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideChar(TableNameIndex, Len), Len);
        Result.UpdatePWideChar(PrimaryKeyColumnNameIndex, GetPWideChar(PrimaryKeyColumnNameIndex, Len), Len);
        Result.UpdateSmall(PrimaryKeyKeySeqIndex, GetSmall(PrimaryKeyKeySeqIndex));
        Result.UpdatePWideChar(PrimaryKeyPKNameIndex, GetPWideChar(PrimaryKeyPKNameIndex, Len), Len);
        Result.InsertRow;
      end;
      Close;
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
function TODBCDatabaseMetadataW.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  PKCat, PKSchem, PKTabl, FKCat, FKSchem, FKTabl: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                              ForeignCatalog, ForeignSchema, ForeignTable);

  PKCat := DecomposeObjectString(PrimaryCatalog);
  PKSchem := DecomposeObjectString(PrimarySchema);
  PKTabl := DecomposeObjectString(PrimaryTable);
  FKCat := DecomposeObjectString(ForeignCatalog);
  FKSchem := DecomposeObjectString(ForeignSchema);
  FKTabl := DecomposeObjectString(ForeignTable);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLForeignKeysW(HSTMT, Pointer(PKCat), Length(PKCat),
    Pointer(PKSchem), Length(PKSchem), Pointer(PKTabl), Length(PKTabl),
    Pointer(FKCat), Length(FKCat),
    Pointer(FKSchem), Length(FKSchem), Pointer(FKTabl), Length(FKTabl)), HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CrossRefKeyColPKTableCatalogIndex, GetPWideChar(CrossRefKeyColPKTableCatalogIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColPKTableSchemaIndex, GetPWideChar(CrossRefKeyColPKTableSchemaIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColPKTableNameIndex, GetPWideChar(CrossRefKeyColPKTableNameIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColPKColumnNameIndex, GetPWideChar(CrossRefKeyColPKColumnNameIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableCatalogIndex, GetPWideChar(CrossRefKeyColFKTableCatalogIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableSchemaIndex, GetPWideChar(CrossRefKeyColFKTableSchemaIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableNameIndex, GetPWideChar(CrossRefKeyColFKTableNameIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColFKColumnNameIndex, GetPWideChar(CrossRefKeyColFKColumnNameIndex, Len), Len);
        Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmall(CrossRefKeyColKeySeqIndex));
        if not IsNull(CrossRefKeyColUpdateRuleIndex) then
          Result.UpdateSmall(CrossRefKeyColUpdateRuleIndex, GetSmall(CrossRefKeyColUpdateRuleIndex));
        if not IsNull(CrossRefKeyColDeleteRuleIndex) then
          Result.UpdateSmall(CrossRefKeyColDeleteRuleIndex, GetSmall(CrossRefKeyColDeleteRuleIndex));;
        Result.UpdatePWideChar(CrossRefKeyColFKNameIndex, GetPWideChar(CrossRefKeyColFKNameIndex, Len), Len);
        Result.UpdatePWideChar(CrossRefKeyColPKNameIndex, GetPWideChar(CrossRefKeyColPKNameIndex, Len), Len);
        Result.UpdateInt(CrossRefKeyColDeferrabilityIndex, GetSmall(CrossRefKeyColDeferrabilityIndex));
        Result.InsertRow;
      end;
      Close;
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
function TODBCDatabaseMetadataW.UncachedGetIndexInfo(const Catalog, Schema,
  Table: string; Unique, Approximate: Boolean): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl: UnicodeString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(Schema);
  Tabl := DecomposeObjectString(Table);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLStatisticsW(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Tabl), Length(Table),
    Ord(Unique), Ord(Approximate)), HSTMT, ODBCConnection);
  with RS do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar(CatalogNameIndex, Len), Len);
      Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(SchemaNameIndex, Len), Len);
      Result.UpdatePWideChar(TableNameIndex, GetPWideChar(TableNameIndex, Len), Len);
      Result.UpdateBoolean(IndexInfoColNonUniqueIndex, GetBoolean(IndexInfoColNonUniqueIndex));
      Result.UpdatePWideChar(IndexInfoColIndexQualifierIndex, GetPWideChar(IndexInfoColIndexQualifierIndex, Len), Len);
      Result.UpdatePWideChar(IndexInfoColIndexNameIndex, GetPWideChar(IndexInfoColIndexNameIndex, Len), Len);
      Result.UpdateSmall(IndexInfoColTypeIndex, GetSmall(IndexInfoColTypeIndex));
      Result.UpdateSmall(IndexInfoColOrdPositionIndex, GetSmall(IndexInfoColOrdPositionIndex));
      Result.UpdatePWideChar(IndexInfoColColumnNameIndex, GetPWideChar(IndexInfoColColumnNameIndex, Len), Len);
      Result.UpdatePWideChar(IndexInfoColAscOrDescIndex, GetPWideChar(IndexInfoColAscOrDescIndex, Len), Len);
      Result.UpdateInt(IndexInfoColCardinalityIndex, GetInt(IndexInfoColCardinalityIndex));
      Result.UpdateInt(IndexInfoColPagesIndex, GetInt(IndexInfoColPagesIndex));
      Result.UpdatePWideChar(IndexInfoColFilterConditionIndex, GetPWideChar(IndexInfoColFilterConditionIndex, Len), Len);
      Result.InsertRow;
    end;
    Close;
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
function TODBCDatabaseMetadataW.UncachedGetTypeInfo: IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetTypeInfo;
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetW.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainW.SQLGetTypeInfo(HSTMT, SQL_ALL_TYPES), HSTMT, ODBCConnection);
  with RS do begin
    while Next do begin
      Result.MoveToInsertRow;
      Result.UpdatePWideChar(TypeInfoTypeNameIndex, GetPWideChar(TypeInfoTypeNameIndex, Len), Len);
      Result.UpdateSmall(TypeInfoDataTypeIndex, Ord(ConvertODBCTypeToSQLType(
         GetSmall(TypeInfoDataTypeIndex), 0,0,GetBoolean(TypeInfoUnsignedAttributeIndex), ConSettings, nil)));
      Result.UpdateInt(TypeInfoPecisionIndex, GetInt(TypeInfoPecisionIndex));
      Result.UpdatePWideChar(TypeInfoLiteralPrefixIndex, GetPWideChar(TypeInfoLiteralPrefixIndex, Len), Len);
      Result.UpdatePWideChar(TypeInfoLiteralSuffixIndex, GetPWideChar(TypeInfoLiteralSuffixIndex, Len), Len);
      Result.UpdatePWideChar(TypeInfoCreateParamsIndex, GetPWideChar(TypeInfoCreateParamsIndex, Len), Len);
      Result.UpdateSmall(TypeInfoNullAbleIndex, Ord(GetBoolean(TypeInfoNullAbleIndex)));
      Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, GetBoolean(TypeInfoCaseSensitiveIndex));
      Result.UpdateSmall(TypeInfoSearchableIndex, GetSmall(TypeInfoSearchableIndex));
      Result.UpdateBoolean(TypeInfoUnsignedAttributeIndex, GetBoolean(TypeInfoUnsignedAttributeIndex));
      Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, GetBoolean(TypeInfoFixedPrecScaleIndex));
      Result.UpdateBoolean(TypeInfoAutoIncrementIndex, GetBoolean(TypeInfoAutoIncrementIndex));
      Result.UpdatePWideChar(TypeInfoLocaleTypeNameIndex, GetPWideChar(TypeInfoLocaleTypeNameIndex, Len), Len);
      Result.UpdateSmall(TypeInfoMinimumScaleIndex, GetSmall(TypeInfoMinimumScaleIndex));
      Result.UpdateSmall(TypeInfoMaximumScaleIndex, GetSmall(TypeInfoMaximumScaleIndex));
      Result.UpdateSmall(TypeInfoSQLDataTypeIndex, GetSmall(TypeInfoSQLDataTypeIndex));
      Result.UpdateSmall(TypeInfoSQLDateTimeSubIndex, GetSmall(TypeInfoSQLDateTimeSubIndex));
      Result.UpdateSmall(TypeInfoNumPrecRadix, GetSmall(TypeInfoNumPrecRadix));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{ TODBCDatabaseMetadataA }

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
constructor TODBCDatabaseMetadataA.Create(Connection: TZAbstractDbcConnection;
  const Url: TZURL; var ConnectionHandle: SQLHDBC);
begin
  inherited Create(Connection, URL, ConnectionHandle);
  fPlainA := Connection.GetIZPlainDriver.GetInstance as TODBC3RawPlainDriver;
end;

function TODBCDatabaseMetadataA.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZODBCDatabaseInfoA.Create(Self, fPHDBC);
end;

function TODBCDatabaseMetadataA.DecomposeObjectString(const S: String): RawByteString;
{$IFDEF UNICODE}
var Tmp: String;
{$ENDIF}
begin
  if S = ''
  then Result := ''
  {$IFDEF UNICODE}
  else begin
    if IC.IsQuoted(S)
    then Tmp := IC.ExtractQuote(S)
    else Tmp := S;
    Result := ZUnicodeToRaw(Tmp, ConSettings.ClientCodePage.CP);
  end;
  {$ELSE}
  else if IC.IsQuoted(S)
    then Result := IC.ExtractQuote(S)
    else Result := S;
  {$ENDIF}
end;

function TODBCDatabaseMetadataA.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Proc: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Proc := DecomposeObjectString(ProcedureNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLProcedures(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Proc), Length(Proc)), HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        IntializeProcedureMap(RS);
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(fProcedureMap.ColIndices[CatalogNameIndex], Len), Len);
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(fProcedureMap.ColIndices[SchemaNameIndex], Len), Len);
        Result.UpdatePAnsiChar(ProcedureNameIndex, GetPAnsiChar(fProcedureMap.ColIndices[ProcedureNameIndex], Len), Len);
        //overload?
        Result.UpdatePAnsiChar(ProcedureRemarksIndex, GetPAnsiChar(fProcedureMap.ColIndices[ProcedureRemarksIndex], Len), Len);
        Result.UpdateSmall(ProcedureTypeIndex, GetSmall(fProcedureMap.ColIndices[ProcedureTypeIndex]) - 1);
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of a catalog's stored procedure parameters
  and result columns.

  <P>Only descriptions matching the schema, procedure and
  parameter name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
  if any, is first. Next are the parameter descriptions in call
  order. The column descriptions follow in column number order.

  <P>Each row in the <code>ResultSet</code> is a parameter description or
  column description with the following fields:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
 	<LI><B>COLUMN_NAME</B> String => column/parameter name
 	<LI><B>COLUMN_TYPE</B> Short => kind of column/parameter:
       <UL>
       <LI> procedureColumnUnknown - nobody knows
       <LI> procedureColumnIn - IN parameter
       <LI> procedureColumnInOut - INOUT parameter
       <LI> procedureColumnOut - OUT parameter
       <LI> procedureColumnReturn - procedure return value
       <LI> procedureColumnResult - result column in <code>ResultSet</code>
       </UL>
   <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => SQL type name, for a UDT type the
   type name is fully qualified
 	<LI><B>PRECISION</B> int => precision
 	<LI><B>LENGTH</B> int => length in bytes of data
 	<LI><B>SCALE</B> short => scale
 	<LI><B>RADIX</B> short => radix
 	<LI><B>NULLABLE</B> short => can it contain NULL?
       <UL>
       <LI> procedureNoNulls - does not allow NULL values
       <LI> procedureNullable - allows NULL values
       <LI> procedureNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing parameter/column
   </OL>

  <P><B>Note:</B> Some databases may not return the column
  descriptions for a procedure. Additional columns beyond
  REMARKS can be defined by the database.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row describes a stored procedure parameter or
       column
  @see #getSearchStringEscape
}
function TODBCDatabaseMetadataA.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  SQLType: TZSQLType;
  Cat, Schem, Proc, Col: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Proc := DecomposeObjectString(ProcedureNamePattern);
  Col := DecomposeObjectString(ColumnNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLProcedureColumns(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Proc), Length(Proc), Pointer(Col), Length(Col)),
    HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        IntializeProceduresProcedureColumnsColMap(RS);
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(fProcedureColumnsColMap.ColIndices[CatalogNameIndex], Len), Len);
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(fProcedureColumnsColMap.ColIndices[SchemaNameIndex], Len), Len);
        Result.UpdatePAnsiChar(ProcColProcedureNameIndex, GetPAnsiChar(fProcedureColumnsColMap.ColIndices[ProcColProcedureNameIndex], Len), Len);
        Result.UpdatePAnsiChar(ProcColColumnNameIndex, GetPAnsiChar(fProcedureColumnsColMap.ColIndices[ProcColColumnNameIndex], Len), Len);
        case GetSmall(fProcedureColumnsColMap.ColIndices[ProcColColumnTypeIndex]) of
          SQL_PARAM_INPUT:        Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctIn));
          SQL_PARAM_INPUT_OUTPUT: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
          SQL_RESULT_COL:         Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctResultSet));
          SQL_PARAM_OUTPUT:       Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctOut));
          SQL_RETURN_VALUE:       Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctReturn));
          else                    Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
        end;
        SQLType := ConvertODBCTypeToSQLType(GetSmall(fProcedureColumnsColMap.ColIndices[ProcColDataTypeIndex]),
          GetSmall(fProcedureColumnsColMap.ColIndices[ProcColScaleIndex]),
          GetInt(fProcedureColumnsColMap.ColIndices[ProcColPrecisionIndex]), False, ConSettings, nil);
        if (Ord(SQLType) < Ord(stFloat)) and (Ord(SQLType) > Ord(stBoolean)) then
          if SQLType = stShort then //spezial case: MSSQL should map stByte / MySQL should use stShort
            SQLType := stByte
          else //test unsigned
            SQLType := TZSQLType(Ord(SQLType)-Ord(ZFastCode.Pos('U', UpperCase(GetString(fProcedureColumnsColMap.ColIndices[TableColColumnTypeNameIndex]))) > 0));
        Result.UpdateSmall(ProcColDataTypeIndex, Ord(SQLType));
        Result.UpdatePAnsiChar(ProcColTypeNameIndex, GetPAnsiChar(fProcedureColumnsColMap.ColIndices[ProcColTypeNameIndex], Len), Len);
        Result.UpdateInt(ProcColPrecisionIndex, GetInt(fProcedureColumnsColMap.ColIndices[ProcColPrecisionIndex]));
        Result.UpdateInt(ProcColLengthIndex, GetInt(fProcedureColumnsColMap.ColIndices[ProcColLengthIndex]));
        Result.UpdateSmall(ProcColScaleIndex, GetSmall(fProcedureColumnsColMap.ColIndices[ProcColScaleIndex]));
        Result.UpdateSmall(ProcColRadixIndex, GetSmall(fProcedureColumnsColMap.ColIndices[ProcColRadixIndex]));
        Result.UpdateSmall(ProcColNullableIndex, GetSmall(fProcedureColumnsColMap.ColIndices[ProcColNullableIndex]));
        Result.UpdatePAnsiChar(ProcColRemarksIndex, GetPAnsiChar(fProcedureColumnsColMap.ColIndices[ProcColRemarksIndex], Len), Len);
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of tables available in a catalog.

  <P>Only table descriptions matching the catalog, schema, table
  name and type criteria are returned.  They are ordered by
  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.

  <P>Each table description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	<LI><B>REMARKS</B> String => explanatory comment on the table
   </OL>

  <P><B>Note:</B> Some databases may not return information for
  all tables.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param types a list of table types to include; null returns all types
  @return <code>ResultSet</code> - each row is a table description
  @see #getSearchStringEscape
}
function TODBCDatabaseMetadataA.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl, TableTypes: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Tabl := DecomposeObjectString(TableNamePattern);

  TableTypes := '';
  for I := Low(Types) to High(Types) do begin
    if Length(TableTypes) > 0 then
      TableTypes := TableTypes + ',';
    {$IFDEF UNICODE}
    TableTypes := TableTypes + ZUnicodeToRaw(Types[I], ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    TableTypes := TableTypes + Types[I];
    {$ENDIF}
  end;
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLTables(HSTMT, Pointer(Cat), Length(Cat), Pointer(Schem), Length(Schem),
    Pointer(Tabl), Length(Tabl), Pointer(TableTypes), Length(TableTypes)), HSTMT, ODBCConnection);
  if Assigned(RS) then with RS do begin
    while Next do begin
      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(CatalogNameIndex, Len), Len);
      Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(SchemaNameIndex, Len), Len);
      Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(TableNameIndex, Len), Len);
      Result.UpdatePAnsiChar(TableColumnsSQLType, GetPAnsiChar(TableColumnsSQLType, Len), Len);
      Result.UpdatePAnsiChar(TableColumnsRemarks, GetPAnsiChar(TableColumnsRemarks, Len), Len);
      Result.InsertRow;
    end;
    Close;
  end;
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
function TODBCDatabaseMetadataA.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  SQLType: TZSQLType;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl, Col: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Tabl := DecomposeObjectString(TableNamePattern);
  Col := DecomposeObjectString(ColumnNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLColumns(HSTMT, Pointer(Cat), Length(Cat), Pointer(Schem),
    Length(Schem), Pointer(Tabl), Length(Tabl), Pointer(Col), Length(Col)), HSTMT, ODBCConnection);
  if Assigned(RS) then begin
    with RS do begin
      while Next do begin
        IntializeTableColColumnMap(RS);
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[CatalogNameIndex], Len), Len);
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[SchemaNameIndex], Len), Len);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[TableNameIndex], Len), Len);
        Result.UpdatePAnsiChar(ColumnNameIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[ColumnNameIndex], Len), Len);
        SQLType := ConvertODBCTypeToSQLType(GetSmall(fTableColColumnMap.ColIndices[TableColColumnTypeIndex]),
          GetInt(fTableColColumnMap.ColIndices[TableColColumnDecimalDigitsIndex]),
          GetInt(fTableColColumnMap.ColIndices[TableColColumnSizeIndex]), False, ConSettings, nil);
        if (Ord(SQLType) < Ord(stFloat)) and (Ord(SQLType) > Ord(stBoolean)) then
          if SQLType = stShort then //spezial case: MSSQL should map stByte / MySQL should use stShort
            SQLType := stByte
          else //test unsigned
            SQLType := TZSQLType(Ord(SQLType)-Ord(ZFastCode.Pos('U', UpperCase(GetString(fTableColColumnMap.ColIndices[TableColColumnTypeNameIndex]))) > 0));
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(SQLType));
        Result.UpdatePAnsiChar(TableColColumnTypeNameIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[TableColColumnTypeNameIndex], Len), Len);
        Result.UpdateInt(TableColColumnSizeIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnSizeIndex]));
        Result.UpdateInt(TableColColumnBufLengthIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnBufLengthIndex]));
        Result.UpdateInt(TableColColumnDecimalDigitsIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnDecimalDigitsIndex]));
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnNumPrecRadixIndex]));
        Result.UpdateSmall(TableColColumnNullableIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnNullableIndex]));
        Result.UpdatePAnsiChar(TableColColumnRemarksIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[TableColColumnRemarksIndex], Len), Len);
        Result.UpdatePAnsiChar(TableColColumnColDefIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[TableColColumnColDefIndex], Len), Len);
        Result.UpdateSmall(TableColColumnSQLDataTypeIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnSQLDataTypeIndex]));
        Result.UpdateSmall(TableColColumnSQLDateTimeSubIndex, GetSmall(fTableColColumnMap.ColIndices[TableColColumnSQLDateTimeSubIndex]));
        Result.UpdateInt(TableColColumnCharOctetLengthIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnCharOctetLengthIndex]));
        Result.UpdateInt(TableColColumnOrdPosIndex, GetInt(fTableColColumnMap.ColIndices[TableColColumnOrdPosIndex]));
        Result.UpdatePAnsiChar(TableColColumnIsNullableIndex, GetPAnsiChar(fTableColColumnMap.ColIndices[TableColColumnIsNullableIndex], Len), Len);
        if fTableColColumnMap.ColIndices[TableColColumnAutoIncIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnAutoIncIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnAutoIncIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnCaseSensitiveIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnCaseSensitiveIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnSearchableIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnSearchableIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnSearchableIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnWritableIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnWritableIndex, not GetBoolean(fTableColColumnMap.ColIndices[TableColColumnWritableIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnDefinitelyWritableIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, not GetBoolean(fTableColColumnMap.ColIndices[TableColColumnDefinitelyWritableIndex]));
        if fTableColColumnMap.ColIndices[TableColColumnReadonlyIndex] <> InvalidDbcIndex then
          Result.UpdateBoolean(TableColColumnReadonlyIndex, GetBoolean(fTableColColumnMap.ColIndices[TableColColumnReadonlyIndex]));
        Result.InsertRow;
      end;
      Close;
    end;
  end;
end;

function TODBCDatabaseMetadataA.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl,Col: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(Schema);
  Tabl := DecomposeObjectString(Table);
  Col := DecomposeObjectString(ColumnNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLColumnPrivileges(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Tabl), Length(Tabl), Pointer(Col), Length(Col)),
    HSTMT, ODBCConnection);
  if Assigned(RS) then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(CatalogNameIndex, Len), Len);
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(SchemaNameIndex, Len), Len);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(TableNameIndex, Len), Len);
        Result.UpdatePAnsiChar(ColumnNameIndex, GetPAnsiChar(ColumnNameIndex, Len), Len);
        Result.UpdatePAnsiChar(TableColPrivGrantorIndex, GetPAnsiChar(TableColPrivGrantorIndex, Len), Len);
        Result.UpdatePAnsiChar(TableColPrivGranteeIndex, GetPAnsiChar(TableColPrivGranteeIndex, Len), Len);
        Result.UpdatePAnsiChar(TableColPrivPrivilegeIndex, GetPAnsiChar(TableColPrivPrivilegeIndex, Len), Len);
        Result.UpdatePAnsiChar(TablePrivIsGrantableIndex, GetPAnsiChar(TablePrivIsGrantableIndex, Len), Len);
        Result.InsertRow;
      end;
      Close;
    end;
end;

function TODBCDatabaseMetadataA.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(SchemaPattern);
  Tabl := DecomposeObjectString(TableNamePattern);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLTablePrivileges(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Tabl), Length(Tabl)), HSTMT, ODBCConnection);
  if Assigned(RS) then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(CatalogNameIndex, Len), Len);
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(SchemaNameIndex, Len), Len);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(TableNameIndex, Len), Len);
        Result.UpdatePAnsiChar(ColumnNameIndex, GetPAnsiChar(ColumnNameIndex, Len), Len);
        Result.UpdatePAnsiChar(TablePrivGrantorIndex, GetPAnsiChar(TablePrivGrantorIndex, Len), Len);
        Result.UpdatePAnsiChar(TablePrivGranteeIndex, GetPAnsiChar(TablePrivGranteeIndex, Len), Len);
        Result.UpdatePAnsiChar(TablePrivPrivilegeIndex, GetPAnsiChar(TablePrivPrivilegeIndex, Len), Len);
        Result.UpdatePAnsiChar(TablePrivIsGrantableIndex, GetPAnsiChar(TablePrivIsGrantableIndex, Len), Len);
        Result.InsertRow;
      end;
      Close;
    end;
end;

function TODBCDatabaseMetadataA.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(Schema);
  Tabl := DecomposeObjectString(Table);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLPrimaryKeys(HSTMT, Pointer(Cat), Length(Cat), Pointer(Schem),
    Length(Schem), Pointer(Tabl), Length(Tabl)), HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(CatalogNameIndex, Len), Len);
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(SchemaNameIndex, Len), Len);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(TableNameIndex, Len), Len);
        Result.UpdatePAnsiChar(PrimaryKeyColumnNameIndex, GetPAnsiChar(PrimaryKeyColumnNameIndex, Len), Len);
        Result.UpdateSmall(PrimaryKeyKeySeqIndex, GetSmall(PrimaryKeyKeySeqIndex));
        Result.UpdatePAnsiChar(PrimaryKeyPKNameIndex, GetPAnsiChar(PrimaryKeyPKNameIndex, Len), Len);
        Result.InsertRow;
      end;
      Close;
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
function TODBCDatabaseMetadataA.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  PKCat, PKSchem, PKTabl, FKCat, FKSchem, FKTabl: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                              ForeignCatalog, ForeignSchema, ForeignTable);

  PKCat := DecomposeObjectString(PrimaryCatalog);
  PKSchem := DecomposeObjectString(PrimarySchema);
  PKTabl := DecomposeObjectString(PrimaryTable);
  FKCat := DecomposeObjectString(ForeignCatalog);
  FKSchem := DecomposeObjectString(ForeignSchema);
  FKTabl := DecomposeObjectString(ForeignTable);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLForeignKeys(HSTMT, Pointer(PKCat), Length(PKCat),
    Pointer(PKSchem), Length(PKSchem), Pointer(PKTabl), Length(PKTabl),
    Pointer(FKCat), Length(FKCat), Pointer(FKSchem), Length(FKSchem), Pointer(FKTabl), Length(FKTabl)),
    HSTMT, ODBCConnection);
  if RS <> nil then
    with RS do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(CrossRefKeyColPKTableCatalogIndex, GetPAnsiChar(CrossRefKeyColPKTableCatalogIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKTableSchemaIndex, GetPAnsiChar(CrossRefKeyColPKTableSchemaIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKTableNameIndex, GetPAnsiChar(CrossRefKeyColPKTableNameIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKColumnNameIndex, GetPAnsiChar(CrossRefKeyColPKColumnNameIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColFKTableCatalogIndex, GetPAnsiChar(CrossRefKeyColFKTableCatalogIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColFKTableSchemaIndex, GetPAnsiChar(CrossRefKeyColFKTableSchemaIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColFKTableNameIndex, GetPAnsiChar(CrossRefKeyColFKTableNameIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColFKColumnNameIndex, GetPAnsiChar(CrossRefKeyColFKColumnNameIndex, Len), Len);
        Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmall(CrossRefKeyColKeySeqIndex));
        if not IsNull(CrossRefKeyColUpdateRuleIndex) then
          Result.UpdateSmall(CrossRefKeyColUpdateRuleIndex, GetSmall(CrossRefKeyColUpdateRuleIndex));
        if not IsNull(CrossRefKeyColDeleteRuleIndex) then
          Result.UpdateSmall(CrossRefKeyColDeleteRuleIndex, GetSmall(CrossRefKeyColDeleteRuleIndex));;
        Result.UpdatePAnsiChar(CrossRefKeyColFKNameIndex, GetPAnsiChar(CrossRefKeyColFKNameIndex, Len), Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKNameIndex, GetPAnsiChar(CrossRefKeyColPKNameIndex, Len), Len);
        Result.UpdateInt(CrossRefKeyColDeferrabilityIndex, GetSmall(CrossRefKeyColDeferrabilityIndex));
        Result.InsertRow;
      end;
      Close;
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
function TODBCDatabaseMetadataA.UncachedGetIndexInfo(const Catalog, Schema,
  Table: string; Unique, Approximate: Boolean): IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  Cat, Schem, Tabl: RawByteString;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  Cat := DecomposeObjectString(Catalog);
  Schem := DecomposeObjectString(Schema);
  Tabl := DecomposeObjectString(Table);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLStatistics(HSTMT, Pointer(Cat), Length(Cat),
    Pointer(Schem), Length(Schem), Pointer(Tabl), Length(Table),
    Ord(Unique), Ord(Approximate)), HSTMT, ODBCConnection);
  with RS do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(CatalogNameIndex, Len), Len);
      Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(SchemaNameIndex, Len), Len);
      Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(TableNameIndex, Len), Len);
      Result.UpdateBoolean(IndexInfoColNonUniqueIndex, GetBoolean(IndexInfoColNonUniqueIndex));
      Result.UpdatePAnsiChar(IndexInfoColIndexQualifierIndex, GetPAnsiChar(IndexInfoColIndexQualifierIndex, Len), Len);
      Result.UpdatePAnsiChar(IndexInfoColIndexNameIndex, GetPAnsiChar(IndexInfoColIndexNameIndex, Len), Len);
      Result.UpdateSmall(IndexInfoColTypeIndex, GetSmall(IndexInfoColTypeIndex));
      Result.UpdateSmall(IndexInfoColOrdPositionIndex, GetSmall(IndexInfoColOrdPositionIndex));
      Result.UpdatePAnsiChar(IndexInfoColColumnNameIndex, GetPAnsiChar(IndexInfoColColumnNameIndex, Len), Len);
      Result.UpdatePAnsiChar(IndexInfoColAscOrDescIndex, GetPAnsiChar(IndexInfoColAscOrDescIndex, Len), Len);
      Result.UpdateInt(IndexInfoColCardinalityIndex, GetInt(IndexInfoColCardinalityIndex));
      Result.UpdateInt(IndexInfoColPagesIndex, GetInt(IndexInfoColPagesIndex));
      Result.UpdatePAnsiChar(IndexInfoColFilterConditionIndex, GetPAnsiChar(IndexInfoColFilterConditionIndex, Len), Len);
      Result.InsertRow;
    end;
    Close;
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
function TODBCDatabaseMetadataA.UncachedGetTypeInfo: IZResultSet;
var
  RS: IZResultSet;
  Len: NativeUInt;
  HSTMT: SQLHSTMT;
  ODBCConnection: IZODBCConnection;
begin
  Result:=inherited UncachedGetTypeInfo;
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  //skope of FPC !const! Connection: IZODBCConnection in methods is different to Delphi
  //we need to localize the connection
  ODBCConnection := GetConnection as IZODBCConnection;
  RS := TODBCResultSetA.CreateForMetadataCall(HSTMT, fPHDBC^, ODBCConnection);
  CheckStmtError(fPLainA.SQLGetTypeInfo(HSTMT, SQL_ALL_TYPES), HSTMT, ODBCConnection);
  with RS do begin
    while Next do begin
      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(TypeInfoTypeNameIndex, GetPAnsiChar(TypeInfoTypeNameIndex, Len), Len);
      Result.UpdateSmall(TypeInfoDataTypeIndex, Ord(ConvertODBCTypeToSQLType(
         GetSmall(TypeInfoDataTypeIndex), 0, 0, GetBoolean(TypeInfoUnsignedAttributeIndex), ConSettings, nil)));
      Result.UpdateInt(TypeInfoPecisionIndex, GetInt(TypeInfoPecisionIndex));
      Result.UpdatePAnsiChar(TypeInfoLiteralPrefixIndex, GetPAnsiChar(TypeInfoLiteralPrefixIndex, Len), Len);
      Result.UpdatePAnsiChar(TypeInfoLiteralSuffixIndex, GetPAnsiChar(TypeInfoLiteralSuffixIndex, Len), Len);
      Result.UpdatePAnsiChar(TypeInfoCreateParamsIndex, GetPAnsiChar(TypeInfoCreateParamsIndex, Len), Len);
      Result.UpdateSmall(TypeInfoNullAbleIndex, Ord(GetBoolean(TypeInfoNullAbleIndex)));
      Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, GetBoolean(TypeInfoCaseSensitiveIndex));
      Result.UpdateSmall(TypeInfoSearchableIndex, GetSmall(TypeInfoSearchableIndex));
      Result.UpdateBoolean(TypeInfoUnsignedAttributeIndex, GetBoolean(TypeInfoUnsignedAttributeIndex));
      Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, GetBoolean(TypeInfoFixedPrecScaleIndex));
      Result.UpdateBoolean(TypeInfoAutoIncrementIndex, GetBoolean(TypeInfoAutoIncrementIndex));
      Result.UpdatePAnsiChar(TypeInfoLocaleTypeNameIndex, GetPAnsiChar(TypeInfoLocaleTypeNameIndex, Len), Len);
      Result.UpdateSmall(TypeInfoMinimumScaleIndex, GetSmall(TypeInfoMinimumScaleIndex));
      Result.UpdateSmall(TypeInfoMaximumScaleIndex, GetSmall(TypeInfoMaximumScaleIndex));
      Result.UpdateSmall(TypeInfoSQLDataTypeIndex, GetSmall(TypeInfoSQLDataTypeIndex));
      Result.UpdateSmall(TypeInfoSQLDateTimeSubIndex, GetSmall(TypeInfoSQLDateTimeSubIndex));
      Result.UpdateSmall(TypeInfoNumPrecRadix, GetSmall(TypeInfoNumPrecRadix));
      Result.InsertRow;
    end;
    Close;
  end;
end;

{ TAbstractODBCDatabaseMetadata }

procedure TAbstractODBCDatabaseMetadata.CheckStmtError(RETCODE: SQLRETURN;
  StmtHandle: SQLHSTMT; const Connection: IZODBCConnection);
begin
  if RetCode <> SQL_SUCCESS then
     Connection.HandleErrorOrWarning(RETCODE, StmtHandle, SQL_HANDLE_STMT, '', lcExecute, Connection);
end;

{**
  Gets a description of the primary key columns that are
  referenced by a table's foreign key columns (the primary keys
  imported by a table).  They are ordered by PKTABLE_CAT,
  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog
       being imported (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema
       being imported (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
       being imported
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
       being imported
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
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
  @return <code>ResultSet</code> - each row is a primary key column description
  @see #getExportedKeys
}
constructor TAbstractODBCDatabaseMetadata.Create(
  Connection: TZAbstractDbcConnection; const Url: TZURL; var ConnectionHandle: SQLHDBC);
begin
  fPHDBC := @ConnectionHandle;
  inherited Create(Connection, URL);
  fTableColColumnMap.Initilized := False;
  fProcedureMap.Initilized := False;
  fProcedureColumnsColMap.Initilized := False;
end;

procedure TAbstractODBCDatabaseMetadata.IntializeProcedureMap(
  const RS: IZResultSet);
begin
  if not fProcedureMap.Initilized then
  begin
    fProcedureMap.ColIndices[CatalogNameIndex] := CatalogNameIndex;
    fProcedureMap.ColIndices[SchemaNameIndex] := SchemaNameIndex;
    fProcedureMap.ColIndices[ProcedureNameIndex] := ProcedureNameIndex;
    fProcedureMap.ColIndices[ProcedureRemarksIndex] := RS.FindColumn('REMARKS');
    fProcedureMap.ColIndices[ProcedureOverloadIndex] := -1;
    fProcedureMap.ColIndices[ProcedureTypeIndex] := RS.FindColumn('PROCEDURE_TYPE');
    fProcedureMap.Initilized := True;
  end;
end;

procedure TAbstractODBCDatabaseMetadata.IntializeProceduresProcedureColumnsColMap(
  const RS: IZResultSet);
begin
 if not fProcedureColumnsColMap.Initilized then begin
   fProcedureColumnsColMap.ColIndices[CatalogNameIndex] := CatalogNameIndex;
   fProcedureColumnsColMap.ColIndices[SchemaNameIndex] := SchemaNameIndex;
   fProcedureColumnsColMap.ColIndices[ProcColProcedureNameIndex] := ProcColProcedureNameIndex;
   fProcedureColumnsColMap.ColIndices[ProcColColumnNameIndex] := ProcColColumnNameIndex;
   fProcedureColumnsColMap.ColIndices[ProcColColumnTypeIndex] := ProcColColumnTypeIndex;
   fProcedureColumnsColMap.ColIndices[ProcColDataTypeIndex] := ProcColDataTypeIndex;
   fProcedureColumnsColMap.ColIndices[ProcColTypeNameIndex] := ProcColTypeNameIndex;
   fProcedureColumnsColMap.ColIndices[ProcColPrecisionIndex] := ProcColPrecisionIndex;
   fProcedureColumnsColMap.ColIndices[ProcColLengthIndex] := RS.FindColumn('CHAR_OCTET_LENGTH');
   fProcedureColumnsColMap.ColIndices[ProcColScaleIndex] := RS.FindColumn('DECIMAL_DIGITS');
   fProcedureColumnsColMap.ColIndices[ProcColRadixIndex] := RS.FindColumn('NUM_PREC_RADIX');
   fProcedureColumnsColMap.ColIndices[ProcColNullableIndex] := RS.FindColumn('NULLABLE');
   fProcedureColumnsColMap.ColIndices[ProcColRemarksIndex] := RS.FindColumn('REMARKS');
   fProcedureColumnsColMap.Initilized := True;
 end;
end;

procedure TAbstractODBCDatabaseMetadata.IntializeTableColColumnMap(
  const RS: IZResultSet);
var I: ShortInt;
begin
  if not fTableColColumnMap.Initilized then begin
    for i := CatalogNameIndex to TableColColumnIsNullableIndex do
      fTableColColumnMap.ColIndices[I] := I;
    fTableColColumnMap.ColIndices[TableColColumnAutoIncIndex] := RS.FindColumn('SS_IS_IDENTITY');
    fTableColColumnMap.ColIndices[TableColColumnCaseSensitiveIndex] := InvalidDbcIndex;
    fTableColColumnMap.ColIndices[TableColColumnSearchableIndex] := InvalidDbcIndex;
    fTableColColumnMap.ColIndices[TableColColumnCaseSensitiveIndex] := InvalidDbcIndex;
    fTableColColumnMap.ColIndices[TableColColumnWritableIndex] := RS.FindColumn('SS_IS_COMPUTED');
    fTableColColumnMap.ColIndices[TableColColumnDefinitelyWritableIndex] := fTableColColumnMap.ColIndices[TableColColumnWritableIndex];
    fTableColColumnMap.ColIndices[TableColColumnReadonlyIndex] := fTableColColumnMap.ColIndices[TableColColumnDefinitelyWritableIndex];
    fTableColColumnMap.Initilized := true;
  end;
end;

function TAbstractODBCDatabaseMetadata.UncachedGetExportedKeys(const Catalog,
  Schema, Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference(Catalog, Schema, Table, '', '', '');
end;

function TAbstractODBCDatabaseMetadata.UncachedGetImportedKeys(const Catalog,
  Schema, Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference('', '', '', Catalog, Schema, Table);
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
function TAbstractODBCDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypes: array[0..7] of UnicodeString = (
    'ALIAS', 'TABLE', 'SYNONYM', 'SYSTEM TABLE', 'VIEW',
    'GLOBAL TEMPORARY', 'LOCAL TEMPORARY', 'SYSTEM VIEW'
  );
var
  I: Integer;
begin
  Result:=inherited UncachedGetTableTypes;

  for I := 0 to 7 do
  begin
    Result.MoveToInsertRow;
    Result.UpdateUnicodeString(TableTypeColumnTableTypeIndex, TableTypes[I]);
    Result.InsertRow;
  end;
end;

{ TZODBCDatabaseInfoW }

function TZODBCDatabaseInfoW.GetStringDbcInfo(InfoType: SQLUSMALLINT): String;
var
  Buf: array[0..SQL_MAX_MESSAGE_LENGTH shl 1] of WideChar;
  PropLength: SQLSMALLINT;
  ODBCConnection: IZODBCConnection;
  Ret: SQLRETURN;
begin
  ODBCConnection := GetODBCConnection;
  Ret := ODBCConnection.GetPlainDriver.SQLGetInfo(fPHDBC^, InfoType, @Buf[0],
    SizeOf(Buf), @PropLength);
  if Ret <> SQL_SUCCESS then
    ODBCConnection.HandleErrorOrWarning(ret, fPHDBC^, SQL_HANDLE_DBC,
      'SQLGetInfo', lcOther, ODBCConnection);
  {$IFDEF UNICODE}
  SetString(Result, PWideChar(@Buf[0]), PropLength shr 1);
  {$ELSE}
  Result := PUnicodeToRaw(PWideChar(@Buf[0]),PropLength shr 1,ZOSCodePage);
  {$ENDIF}
end;

{ TZODBCDatabaseInfoA }

function TZODBCDatabaseInfoA.GetStringDbcInfo(InfoType: SQLUSMALLINT): String;
var
  Buf: array[0..SQL_MAX_MESSAGE_LENGTH shl 1] of AnsiChar;
  PropLength: SQLSMALLINT;
  ODBCConnection: IZODBCConnection;
  Ret: SQLRETURN;
begin
  ODBCConnection := GetODBCConnection;
  Ret := ODBCConnection.GetPlainDriver.SQLGetInfo(fPHDBC^, InfoType, @Buf[0],
    SizeOf(Buf), @PropLength);
  if Ret <> SQL_SUCCESS then
    ODBCConnection.HandleErrorOrWarning(ret,fPHDBC^, SQL_HANDLE_DBC,
      'SQLGetInfo', lcConnect, ODBCConnection);
  {$IFDEF UNICODE}
  Result := PRawToUnicode(PAnsiChar(@Buf[0]),PropLength,ZOSCodePage);
  {$ELSE}
  SetString(Result, PAnsiChar(@Buf[0]), PropLength);
  {$ENDIF}
end;

{$ENDIF ZEOS_DISABLE_ODBC} //if set we have an empty unit
end.
