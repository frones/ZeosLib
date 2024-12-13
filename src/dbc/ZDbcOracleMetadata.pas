{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Oracle Database Connectivity Classes           }
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

unit ZDbcOracleMetadata;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Types, Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata,
  ZCompatibility, ZDbcOracleUtils;

type

  // technobot 2008-06-28 - methods moved as is from TZOracleDatabaseMetadata:
  {** Implements Oracle Database Information. }
  TZOracleDatabaseInfo = class(TZAbstractDatabaseInfo)
//    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
//      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
  public
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
//    function GetServerVersion: string; -> Not implemented

    // capabilities (what it can/cannot do):
//    function AllProceduresAreCallable: Boolean; override; -> Not implemented
//    function AllTablesAreSelectable: Boolean; override; -> Not implemented
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
//    function SupportsAlterTableWithAddColumn: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithDropColumn: Boolean; override; -> Not implemented
//    function SupportsColumnAliasing: Boolean; override; -> Not implemented
//    function SupportsConvert: Boolean; override; -> Not implemented
//    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
//      Boolean; override; -> Not implemented
//    function SupportsDifferentTableCorrelationNames: Boolean; override; -> Not implemented
    function SupportsExpressionsInOrderBy: Boolean; override;
    function SupportsOrderByUnrelated: Boolean; override;
    function SupportsGroupBy: Boolean; override;
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
//    function SupportsLikeEscapeClause: Boolean; override; -> Not implemented
//    function SupportsMultipleResultSets: Boolean; override; -> Not implemented
//    function SupportsMultipleTransactions: Boolean; override; -> Not implemented
//    function SupportsNonNullableColumns: Boolean; override; -> Not implemented
//    function SupportsMinimumSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsCoreSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsExtendedSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsANSI92EntryLevelSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92IntermediateSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92FullSQL: Boolean; override; -> Not implemented
    function SupportsIntegrityEnhancementFacility: Boolean; override;
//    function SupportsOuterJoins: Boolean; override; -> Not implemented
//    function SupportsFullOuterJoins: Boolean; override; -> Not implemented
//    function SupportsLimitedOuterJoins: Boolean; override; -> Not implemented
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
    function SupportsTransactionIsolationLevel(const {%H-}Level: TZTransactIsolationLevel):
      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function SupportsResultSetType(const _Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(const _Type: TZResultSetType;
      const Concurrency: TZResultSetConcurrency): Boolean; override;
//    function SupportsBatchUpdates: Boolean; override; -> Not implemented
    function SupportsNonEscapedSearchStrings: Boolean; override;
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
//    function IsReadOnly: Boolean; override; -> Not implemented
//    function IsCatalogAtStart: Boolean; override; -> Not implemented
    function DoesMaxRowSizeIncludeBlobs: Boolean; override;
//    function NullsAreSortedHigh: Boolean; override; -> Not implemented
//    function NullsAreSortedLow: Boolean; override; -> Not implemented
//    function NullsAreSortedAtStart: Boolean; override; -> Not implemented
//    function NullsAreSortedAtEnd: Boolean; override; -> Not implemented
//    function NullPlusNonNullIsNull: Boolean; override; -> Not implemented
//    function UsesLocalFiles: Boolean; override; -> Not implemented
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

  {** Implements Oracle Database Metadata. }
  TZOracleDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    function InternalGetCrossReference(const PrimarySchema, PrimaryTable,
      ForeignSchema, ForeignTable, OrderBy: string;
      const ColumnsDefs: TZMetadataColumnDefs): IZResultSet;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-28

    function UncachedGetTables(const {%H-}Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetSchemas: IZResultSet; override;
//    function UncachedGetCatalogs: IZResultSet; override; -> Not implemented
    function UncachedGetTableTypes: IZResultSet; override;
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
    function UncachedGetTablePrivileges(const {%H-}Catalog: string; const SchemaPattern: string;
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
    function UncachedGetColumnPrivileges(const {%H-}Catalog: string; const Schema: string;
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
    function UncachedGetPrimaryKeys(const {%H-}Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
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
    function UncachedGetImportedKeys(const {%H-}Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const {%H-}Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const {%H-}PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const {%H-}ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
    function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet; override;
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
    function UncachedGetProcedures(const Catalog, SchemaPattern,
      ProcedureNamePattern: string): IZResultSet;override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
//    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
//    function UncachedGetTypeInfo: IZResultSet; override;
  public
  end;

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  ZFastCode, ZDbcUtils, ZSelectSchema, ZClasses, ZEncoding,
  ZPlainOracleDriver {$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF},
  ZDbcOracle;

{ TZOracleDatabaseInfo }

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

function TZOracleDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'Oracle';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZOracleDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

function TZOracleDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Oracle';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZOracleDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZOracleDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZOracleDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZOracleDatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'ACCESS,ADD,AUDIT,CLUSTER,COMMENT,COMPRESS,CONNECT,'
    + 'DATE,EXCLUSIVE,FILE,IDENTIFIED,IMMEDIATE,INCREMENT,INITIAL,'
    + 'INTERSECT,LEVEL,LOCK,LONG,MAXEXTENTS,MINUS,MODE,NOAUDIT,NOCOMPRESS,'
    + 'NOWAIT,OFFLINE,ONLINE,PCTFREE,PRIOR';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZOracleDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATAN2,CEILING,COS,EXP,FLOOR,LOG,LOG10,MOD,PI,'
    + 'POWER,ROUND,SIGN,SIN,SQRT,TAN,TRUNCATE';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZOracleDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'ASCII,CHAR,CONCAT,LCASE,LENGTH,LTRIM,REPLACE,RTRIM,SOUNDEX,'
    + 'SUBSTRING,UCASE';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZOracleDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'USER';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZOracleDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := 'CURDATE,CURTIME,DAYOFMONTH,HOUR,MINUTE,MONTH,NOW,SECOND,YEAR';
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
function TZOracleDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '//';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZOracleDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '$#';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := True;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZOracleDatabaseInfo.GetSchemaTerm: string;
begin
  Result := 'schema';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZOracleDatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'procedure';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZOracleDatabaseInfo.GetCatalogTerm: string;
begin
  Result := '';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZOracleDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := False;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := False;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZOracleDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := False;
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
function TZOracleDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 1000;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 2000;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 32;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 1000;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxCursorNameLength: Integer;
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
function TZOracleDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 2000;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 65535;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 30;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZOracleDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 30;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZOracleDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZOracleDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  Result := Level in [tiReadCommitted, tiSerializable];
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := True;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsResultSetType(
  const _Type: TZResultSetType): Boolean;
begin
  Result := _Type = rtForwardOnly;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZOracleDatabaseInfo.SupportsResultSetConcurrency(
  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtForwardOnly) and (Concurrency = rcReadOnly);
end;

{**
  Does the Database or Actual Version understand non escaped search strings?
  @return <code>true</code> if the DataBase does understand non escaped
  search strings
}
function TZOracleDatabaseInfo.SupportsNonEscapedSearchStrings: Boolean;
begin
  Result := MetaData.GetConnection.GetClientVersion > 10000000;
end;

{**
  Does the Database support binding arrays? Is the ZDbc ready for this?
  @return <code>true</code> if the DataBase allows it.
}
function TZOracleDatabaseInfo.SupportsArrayBindings: Boolean;
begin
  Result := True;
end;

{ TZOracleDatabaseMetadata }

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZOracleDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZOracleDatabaseInfo.Create(Self);
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
function TZOracleDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  NameCondition, OwnerCondition, PartSQL, SQL: string;

  function IncludedType(const TypeName: string): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := Low(Types) to High(Types) do
      Result := Result or (UpperCase(Types[I]) = TypeName);
    Result := Result or (Length(Types) = 0);
  end;

  function CreateWhere: String;
  begin
    Result := '';
    If OwnerCondition <> '' then
      Result := OwnerCondition;
    If NameCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + NameCondition
      Else
        Result := NameCondition;
    If Result <> '' then
      Result := ' Where ' + Result;
  end;

begin
  OwnerCondition := ConstructNameCondition(SchemaPattern,'OWNER');

  if IncludedType('TABLE') then
  begin
    NameCondition := ConstructNameCondition(TableNamePattern,'TABLE_NAME');
    SQL := 'SELECT NULL AS TABLE_CAT, OWNER AS TABLE_SCHEM, TABLE_NAME,'
      + ' ''TABLE'' AS TABLE_TYPE, NULL AS REMARKS FROM SYS.ALL_TABLES'
      + CreateWhere;
  end else
    SQL := '';

  if IncludedType('SYNONYM') then
  begin
    NameCondition := ConstructNameCondition(TableNamePattern,'SYNONYM_NAME');
    PartSQL := 'SELECT NULL AS TABLE_CAT, OWNER AS TABLE_SCHEM,'
      + ' SYNONYM_NAME AS TABLE_NAME, ''SYNONYM'' AS TABLE_TYPE,'
      + ' NULL AS REMARKS FROM SYS.ALL_SYNONYMS'
      + CreateWhere;
    if SQL <> '' then
      SQL := SQL + ' UNION ';
    SQL := SQL + PartSQL;
  end;

  if IncludedType('VIEW') then
  begin
    NameCondition := ConstructNameCondition(TableNamePattern,'VIEW_NAME');
    PartSQL := 'SELECT NULL AS TABLE_CAT, OWNER AS TABLE_SCHEM,'
      + ' VIEW_NAME AS TABLE_NAME, ''VIEW'' AS TABLE_TYPE,'
      + ' NULL AS REMARKS FROM SYS.ALL_VIEWS'
      + CreateWhere;
    if SQL <> '' then
      SQL := SQL + ' UNION ';
    SQL := SQL + PartSQL;
  end;

  if IncludedType('SEQUENCE') then
  begin
    OwnerCondition := ConstructNameCondition(SchemaPattern,'SEQUENCE_OWNER');
    NameCondition := ConstructNameCondition(TableNamePattern,'SEQUENCE_NAME');
    PartSQL := 'SELECT NULL AS TABLE_CAT, SEQUENCE_OWNER AS TABLE_SCHEM,'
      + ' SEQUENCE_NAME AS TABLE_NAME, ''SEQUENCE'' AS TABLE_TYPE,'
      + ' NULL AS REMARKS FROM SYS.ALL_SEQUENCES'
      + CreateWhere;
    if SQL <> '' then
      SQL := SQL + ' UNION ';
    SQL := SQL + PartSQL;
  end;

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(TableColumnsDynArray));
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
function TZOracleDatabaseMetadata.UncachedGetProcedureColumns(const Catalog,
  SchemaPattern, ProcedureNamePattern, ColumnNamePattern: string): IZResultSet;
var
  TempProcedureNamePattern, TmpSchemaPattern: SQLString;
  RS: IZResultSet;
  DescriptorA: TZOraProcDescriptor_A;
  DescriptorW: TZOraProcDescriptor_W;
  Connection: IZOracleConnection;
  SQLWriter: TZSQLStringWriter;
  {$IFDEF UNICODE}
  SQLWriterA: TZRawSQLStringWriter;
  {$ELSE}
  SQLWriterW: TZUnicodeSQLStringWriter;
  {$ENDIF}
  SL: TStrings;
  i: Integer;
  SQLType: TZSQLType;
  S: {$IFDEF UNICODE}RawByteString{$ELSE}UnicodeString{$ENDIF};

  function CheckOwner: Boolean;
  begin
    if TmpSchemaPattern = '' then
      Result := False
    else with GetConnection.CreateStatement.ExecuteQuery('SELECT COUNT( * ) FROM ALL_USERS WHERE '+ConstructNameCondition(TmpSchemaPattern,'username')) do begin
      Next;
      Result := GetInt(FirstDbcIndex) > 0;
      Close;
    end;
  end;
  procedure AddArgsA({$IFDEF AUTOREFCOUNT}const {$ENDIF}Descriptor: TZOraProcDescriptor_A;
    {$IFDEF AUTOREFCOUNT}const{$ENDIF} SQLWriter: TZRawSQLStringWriter);
  var I: Integer;
    ProcName, ParamName: RawByteString;
    Arg: TZOraProcDescriptor_A;
  begin
    ProcName := '';
    Descriptor.ConcatParentName(True, SQLWriter, ProcName, IC);
    SQLWriter.AddText(Descriptor.AttributeName, ProcName);
    SQLWriter.Finalize(ProcName);
    for I := 0 to Descriptor.Args.Count-1 do begin
      Result.MoveToInsertRow;
      Result.UpdateRawByteString(SchemaNameIndex, Descriptor.SchemaName);
      Result.UpdateRawByteString(ProcColProcedureNameIndex, Descriptor.AttributeName);
      ParamName := '';
      Arg := TZOraProcDescriptor_A(Descriptor.Args[i]);
      Arg.ConcatParentName(False, SQLWriter, ParamName, IC);
      SQLWriter.AddText(Arg.AttributeName, ParamName);
      SQLWriter.Finalize(ParamName);
      Result.UpdateRawByteString(ProcColColumnNameIndex, ParamName);
      Result.UpdateRawByteString(ProcColTypeNameIndex, Arg.TypeName);
      if Arg.OrdPos = 0
      then Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctReturn))
      else case Arg.IODirection of
        OCI_TYPEPARAM_IN    : Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctIn));
        OCI_TYPEPARAM_OUT   : Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctOut));
        OCI_TYPEPARAM_INOUT : Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctInOut));
      end;
      SQLType := NormalizeOracleTypeToSQLType(Arg.DataType,
        Arg.DataSize, Arg.DescriptorType, Arg.Precision, Arg.Scale, ConSettings);
      if (Ord(SQLType) >= Ord(stString)) and (Ord(SQLType) <= Ord(stBytes))
      then Result.UpdateInt(ProcColPrecisionIndex, Arg.DataSize)
      else Result.UpdateInt(ProcColPrecisionIndex, Arg.Precision);
      Result.UpdateInt(ProcColLengthIndex, Arg.DataSize);
      Result.UpdateInt(ProcColDataTypeIndex, Ord(SQLType));

      Result.UpdateInt(ProcColScaleIndex, Arg.Scale);
      Result.UpdateInt(ProcColRadixIndex, Arg.Radix);
      Result.UpdateInt(ProcColNullableIndex, Ord(ntNullableUnknown));
      //ProcColRemarksIndex       = FirstDbcIndex + 12;
      Result.InsertRow;
    end;
  end;

  procedure AddPackageArgsA({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_A;
    {$IFDEF AUTOREFCOUNT}const{$ENDIF} SQLWriter: TZRawSQLStringWriter);
  var I: Integer;
  begin
    for I := 0 to Descriptor.Args.Count -1 do begin
      if TZOraProcDescriptor_A(Descriptor.Args[I]).ObjType = OCI_PTYPE_PKG
      then AddPackageArgsA(TZOraProcDescriptor_A(Descriptor.Args[I]), SQLWriter)
      else AddArgsA(TZOraProcDescriptor_A(Descriptor.Args[i]), SQLWriter);
    end;
  end;

  procedure AddArgsW({$IFDEF AUTOREFCOUNT}const {$ENDIF}Descriptor: TZOraProcDescriptor_W;
    {$IFDEF AUTOREFCOUNT}const{$ENDIF} SQLWriter: TZUnicodeSQLStringWriter);
  var I: Integer;
    ProcName, ParamName: UnicodeString;
    Arg: TZOraProcDescriptor_W;
  begin
    ProcName := '';
    Descriptor.ConcatParentName(True, SQLWriter, ProcName, IC);
    SQLWriter.AddText(Descriptor.AttributeName, ProcName);
    SQLWriter.Finalize(ProcName);
    for I := 0 to Descriptor.Args.Count-1 do begin
      Result.MoveToInsertRow;
      Result.UpdateUnicodeString(SchemaNameIndex, Descriptor.SchemaName);
      Result.UpdateUnicodeString(ProcColProcedureNameIndex, Descriptor.AttributeName);
      ParamName := '';
      Arg := TZOraProcDescriptor_W(Descriptor.Args[i]);
      Arg.ConcatParentName(False, SQLWriter, ParamName, IC);
      SQLWriter.AddText(Arg.AttributeName, ParamName);
      SQLWriter.Finalize(ParamName);
      Result.UpdateUnicodeString(ProcColColumnNameIndex, ParamName);
      Result.UpdateUnicodeString(ProcColTypeNameIndex, Arg.TypeName);
      if Arg.OrdPos = 0
      then Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctReturn))
      else case Arg.IODirection of
        OCI_TYPEPARAM_IN    : Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctIn));
        OCI_TYPEPARAM_OUT   : Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctOut));
        OCI_TYPEPARAM_INOUT : Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctInOut));
      end;
      SQLType := NormalizeOracleTypeToSQLType(Arg.DataType,
        Arg.DataSize, Arg.DescriptorType, Arg.Precision, Arg.Scale, ConSettings);
      if (Ord(SQLType) >= Ord(stString)) and (Ord(SQLType) <= Ord(stBytes))
      then Result.UpdateInt(ProcColPrecisionIndex, Arg.DataSize)
      else Result.UpdateInt(ProcColPrecisionIndex, Arg.Precision);
      Result.UpdateInt(ProcColLengthIndex, Arg.DataSize);
      Result.UpdateInt(ProcColDataTypeIndex, Ord(SQLType));

      Result.UpdateInt(ProcColScaleIndex, Arg.Scale);
      Result.UpdateInt(ProcColRadixIndex, Arg.Radix);
      Result.UpdateInt(ProcColNullableIndex, Ord(ntNullableUnknown));
      //ProcColRemarksIndex       = FirstDbcIndex + 12;
      Result.InsertRow;
    end;
  end;

  procedure AddPackageArgsW({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_W;
    {$IFDEF AUTOREFCOUNT}const{$ENDIF} SQLWriter: TZUnicodeSQLStringWriter);
  var I: Integer;
  begin
    for I := 0 to Descriptor.Args.Count -1 do begin
      if TZOraProcDescriptor_W(Descriptor.Args[I]).ObjType = OCI_PTYPE_PKG
      then AddPackageArgsW(TZOraProcDescriptor_W(Descriptor.Args[I]), SQLWriter)
      else AddArgsW(TZOraProcDescriptor_W(Descriptor.Args[i]), SQLWriter);
    end;
  end;
begin
  Result := inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);
  SQLWriter := TZSQLStringWriter.Create(1024);
  if Catalog = ''
  then TmpSchemaPattern := SchemaPattern
  else TmpSchemaPattern := Catalog;
  TempProcedureNamePattern := ProcedureNamePattern;

  if (TmpSchemaPattern <> '') and (not CheckOwner) then begin
    TempProcedureNamePattern  := IC.ExtractQuote(TempProcedureNamePattern);
    TmpSchemaPattern          := IC.ExtractQuote(TmpSchemaPattern);
    TempProcedureNamePattern  := TmpSchemaPattern+'.'+TempProcedureNamePattern; //no Schema so it's a PackageName
    TmpSchemaPattern          := '';
  end else if ZFastCode.Pos('.', TempProcedureNamePattern) > 0 then begin
    SL := SplitString(TempProcedureNamePattern, '.');
    TempProcedureNamePattern := '';
    if (SL.Count > 1) then begin
      TmpSchemaPattern := SL[0];
      if CheckOwner
      then I := 1
      else I := 0;
      TmpSchemaPattern := '';
    end else
      I := 0;
    for I := I to SL.Count -1 do begin
      SQLWriter.AddText(IC.ExtractQuote(SL[i]), TempProcedureNamePattern);
      SQLWriter.AddChar('.', TempProcedureNamePattern);
    end;
    SQLWriter.CancelLastCharIfExists('.', TempProcedureNamePattern);
    SQLWriter.Finalize(TempProcedureNamePattern);
    FreeAndNil(SL);
  end;
  Connection := GetConnection as IZOracleConnection;
  try
    RS := GetProcedures('', TmpSchemaPattern, TempProcedureNamePattern);
    while RS.Next do begin
      TempProcedureNamePattern := '';
      SL := SplitString(RS.GetString(ProcedureNameIndex), '.');
      for I := 0 to SL.Count -1 do
        if not IC.IsQuoted(SL[i]) then begin
          SQLWriter.AddChar('"', TempProcedureNamePattern);
          SQLWriter.AddText(SL[i], TempProcedureNamePattern);
          SQLWriter.AddChar('"', TempProcedureNamePattern);
          SQLWriter.AddChar('.', TempProcedureNamePattern);
        end else begin
          SQLWriter.AddText(SL[i], TempProcedureNamePattern);
          SQLWriter.AddChar('.', TempProcedureNamePattern);
        end;
      SQLWriter.CancelLastCharIfExists('.', TempProcedureNamePattern);
      SQLWriter.Finalize(TempProcedureNamePattern);
      if ConSettings.ClientCodePage.Encoding = ceUTF16 then begin
        DescriptorW := TZOraProcDescriptor_W.Create(nil, Connection{$IFNDEF UNICODE}, GetW2A2WConversionCodePage(ConSettings){$ENDIF});
        {$IFNDEF UNICODE}
        SQLWriterW := TZUnicodeSQLStringWriter.Create(1024);
        S := ZRawToUnicode(TempProcedureNamePattern, GetW2A2WConversionCodePage(ConSettings));
        {$ENDIF}
        try
          DescriptorW.Describe(OCI_PTYPE_UNK, {$IFNDEF UNICODE}S{$ELSE}TempProcedureNamePattern{$ENDIF});
          if DescriptorW.ObjType = OCI_PTYPE_PKG
          then AddPackageArgsW(DescriptorW, {$IFNDEF UNICODE}SQLWriterW{$ELSE}SQLWriter{$ENDIF})
          else AddArgsW(DescriptorW, {$IFNDEF UNICODE}SQLWriterW{$ELSE}SQLWriter{$ENDIF});
        finally
          SL.Free;
          FreeAndNil(DescriptorW);
          {$IFNDEF UNICODE}
          SQLWriterW.Free;
          {$ENDIF}
        end;
      end else begin
        DescriptorA := TZOraProcDescriptor_A.Create(nil, Connection{$IFDEF UNICODE}, ConSettings.ClientCodePage.CP{$ENDIF});
        {$IFDEF UNICODE}
        SQLWriterA := TZRawSQLStringWriter.Create(1024);
        S := ZUnicodeToRaw(TempProcedureNamePattern, ConSettings.ClientCodePage.CP);
        {$ENDIF}
        try
          DescriptorA.Describe(OCI_PTYPE_UNK, {$IFDEF UNICODE}S{$ELSE}TempProcedureNamePattern{$ENDIF});
          if DescriptorA.ObjType = OCI_PTYPE_PKG
          then AddPackageArgsA(DescriptorA, {$IFDEF UNICODE}SQLWriterA{$ELSE}SQLWriter{$ENDIF})
          else AddArgsA(DescriptorA, {$IFDEF UNICODE}SQLWriterA{$ELSE}SQLWriter{$ENDIF});
        finally
          SL.Free;
          FreeAndNil(DescriptorA);
          {$IFDEF UNICODE}
          SQLWriterA.Free;
          {$ENDIF}
        end;
      end;
    end;
    RS.Close;
  finally
    FreeAndNil(SQLWriter);
  end;
end;

function TZOracleDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
const
  //PROCEDURE_CAT_Index      = FirstDbcIndex + 0; unused
  PROCEDURE_SCHEM_Index    = FirstDbcIndex + 1;
  OBJECT_NAME_Index        = FirstDbcIndex + 2;
  PROCEDURE_NAME_Index     = FirstDbcIndex + 3;
  PROCEDURE_OVERLOAD_Index = FirstDbcIndex + 4;
  PROCEDURE_TYPE_Index     = FirstDbcIndex + 5;
var
  Len: NativeUInt;
  SQL: string;
  LProcedureNamePattern, LSchemaNamePattern,
  sName:string;
begin
  Result:=inherited UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);

  LProcedureNamePattern := ConstructNameCondition(ProcedureNamePattern,'decode(procedure_name,null,object_name,object_name||''.''||procedure_name)');
  LSchemaNamePattern := ConstructNameCondition(SchemaPattern,'owner');
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  SQL := 'select NULL AS PROCEDURE_CAT, OWNER AS PROCEDURE_SCHEM, '+
    'OBJECT_NAME, PROCEDURE_NAME AS PROCEDURE_NAME, '+
    'OVERLOAD AS PROCEDURE_OVERLOAD, OBJECT_TYPE AS PROCEDURE_TYPE FROM '+
    'ALL_PROCEDURES WHERE OBJECT_TYPE in (''FUNCTION'',''PROCEDURE'',''PACKAGE'')';
  if LProcedureNamePattern <> '' then
    SQL := SQL + ' AND ' + LProcedureNamePattern;
  if LSchemaNamePattern <> '' then
    SQL := SQL + ' AND ' + LSchemaNamePattern;
  SQL := SQL + ' ORDER BY decode(owner,user,0,1),owner,object_name,procedure_name,overload';

  with GetConnection.CreateStatement.ExecuteQuery(SQL) do begin
    while Next do begin
      sName := IC.Quote(GetString(OBJECT_NAME_Index));
      if GetString(PROCEDURE_NAME_Index) <> '' then
        sName :=  sName+'.'+IC.Quote(GetString(PROCEDURE_NAME_Index), iqStoredProcedure);
      Result.MoveToInsertRow;
      //Result.UpdateNull(CatalogNameIndex);
      if FConSettings.ClientCodePage.Encoding = ceUTF16 then begin
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(PROCEDURE_SCHEM_Index, Len), Len);
        Result.UpdatePWideChar(ProcedureOverloadIndex, GetPWideChar(PROCEDURE_OVERLOAD_Index, Len), Len);
      end else begin
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(PROCEDURE_SCHEM_Index, Len), Len);
        Result.UpdatePAnsiChar(ProcedureOverloadIndex, GetPAnsiChar(PROCEDURE_OVERLOAD_Index, Len), Len);
      end;
      Result.UpdateString(ProcedureNameIndex, sName);
      if GetString(PROCEDURE_TYPE_Index) = 'FUNCTION' then
        Result.UpdateByte(ProcedureTypeIndex, Ord(prtReturnsResult))
      else if GetString(PROCEDURE_TYPE_Index) = 'PROCDEURE' then
        Result.UpdateByte(ProcedureTypeIndex, Ord(prtNoResult))
      else
        Result.UpdateByte(ProcedureTypeIndex, Ord(prtUnknown)); //Package
      Result.InsertRow;
    end;
    Close;
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
function TZOracleDatabaseMetadata.UncachedGetSchemas: IZResultSet;
begin
  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(
      'SELECT USERNAME AS TABLE_SCHEM FROM SYS.ALL_USERS'),
    ConstructVirtualResultSet(SchemaColumnsDynArray));
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
function TZOracleDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypeCount = 4;
  Types: array [1..TableTypeCount] of String = (
    'TABLE', 'SYNONYM', 'VIEW', 'SEQUENCE'
  );
var
  I: Integer;
begin
  Result:=inherited UncachedGetTableTypes;

  for I := 1 to TableTypeCount do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(TableTypeColumnTableTypeIndex, Types[I]);
      Result.InsertRow;
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
function TZOracleDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
const
  OWNER_Index          = FirstDbcIndex + 0;
  TABLE_NAME_Index     = FirstDbcIndex + 1;
  COLUMN_NAME_Index    = FirstDbcIndex + 2;
  DATA_TYPE_Index      = FirstDbcIndex + 3;
  DATA_LENGTH_Index    = FirstDbcIndex + 4;
  DATA_PRECISION_Index = FirstDbcIndex + 5;
  DATA_SCALE_Index     = FirstDbcIndex + 6;
  NULLABLE_Index       = FirstDbcIndex + 7;
  DATA_DEFAULT_Index   = FirstDbcIndex + 8;
  COLUMN_ID_Index      = FirstDbcIndex + 9;
  REMARKS_Index        = FirstDbcIndex + 10;
  CHAR_LENGTH_Index    = FirstDbcIndex + 11;
var
  Len: NativeUInt;
  SQL, oDataType: string;
  SQLType: TZSQLType;
  OwnerCondition,TableCondition,ColumnCondition: String;
  FieldSize, Precision, CharLength: Integer;
  B: Boolean;
  function CreateWhere: String;
  begin
    Result := '';
    If OwnerCondition <> '' then
      Result := 'ALL_TAB_COLUMNS.' + OwnerCondition;
    If TableCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + 'ALL_TAB_COLUMNS.' + TableCondition
      Else
        Result := 'ALL_TAB_COLUMNS.' + TableCondition;
    If ColumnCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + 'ALL_TAB_COLUMNS.' + ColumnCondition
      Else
        Result := 'ALL_TAB_COLUMNS.' + ColumnCondition;
    If Result <> '' then
      Result := ' WHERE ' + Result;
  end;

begin
  OwnerCondition := ConstructNameCondition(SchemaPattern,'OWNER');
  TableCondition := ConstructNameCondition(TableNamePattern,'TABLE_NAME');
  ColumnCondition := ConstructNameCondition(ColumnNamePattern,'COLUMN_NAME');
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

  SQL := 'SELECT ALL_TAB_COLUMNS.OWNER, ALL_TAB_COLUMNS.TABLE_NAME, ALL_TAB_COLUMNS.COLUMN_NAME, ' +
         'ALL_TAB_COLUMNS.DATA_TYPE, ALL_TAB_COLUMNS.DATA_LENGTH, ALL_TAB_COLUMNS.DATA_PRECISION, ' +
         'ALL_TAB_COLUMNS.DATA_SCALE, ALL_TAB_COLUMNS.NULLABLE, ALL_TAB_COLUMNS.DATA_DEFAULT, '+
         'ALL_TAB_COLUMNS.COLUMN_ID, ALL_COL_COMMENTS.COMMENTS, ALL_TAB_COLUMNS.CHAR_LENGTH ' +
         'FROM ALL_TAB_COLUMNS JOIN ALL_COL_COMMENTS '+
         'ON ALL_COL_COMMENTS.TABLE_NAME = ALL_TAB_COLUMNS.TABLE_NAME AND ALL_COL_COMMENTS.COLUMN_NAME = '+
         'ALL_TAB_COLUMNS.COLUMN_NAME AND ALL_COL_COMMENTS.OWNER = ALL_TAB_COLUMNS.OWNER ' + CreateWhere + ' ORDER BY ALL_TAB_COLUMNS.COLUMN_ID';
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  with GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      if FConSettings.ClientCodePage.Encoding = ceUTF16 then begin
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(OWNER_Index, Len), Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideChar(TABLE_NAME_Index, Len), Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideChar(COLUMN_NAME_Index, Len), Len);
        Result.UpdatePWideChar(TableColColumnTypeNameIndex, GetPWideChar(DATA_TYPE_Index, Len), Len);
        Result.UpdatePWideChar(TableColColumnRemarksIndex, GetPWideChar(REMARKS_Index, Len), Len);
        Result.UpdatePWideChar(TableColColumnColDefIndex, GetPWideChar(DATA_DEFAULT_Index, Len), Len);
      end else begin
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(OWNER_Index, Len), Len);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(TABLE_NAME_Index, Len), Len);
        Result.UpdatePAnsiChar(ColumnNameIndex, GetPAnsiChar(COLUMN_NAME_Index, Len), Len);
        Result.UpdatePAnsiChar(TableColColumnTypeNameIndex, GetPAnsiChar(DATA_TYPE_Index, Len), Len);
        Result.UpdatePAnsiChar(TableColColumnRemarksIndex, GetPAnsiChar(REMARKS_Index, Len), Len);
        Result.UpdatePAnsiChar(TableColColumnColDefIndex, GetPAnsiChar(DATA_DEFAULT_Index, Len), Len);
      end;
      oDataType := GetString(DATA_TYPE_Index);
      Precision := GetInt(DATA_PRECISION_Index);
      SQLType := ConvertOracleTypeToSQLType(oDataType,
        Precision, GetInt(DATA_SCALE_Index));
      Result.UpdateByte(TableColColumnTypeIndex, Ord(SQLType));
      FieldSize := GetInt(DATA_LENGTH_Index);
      CharLength := GetInt(CHAR_LENGTH_Index);
      if SQLType = stString then begin
        Result.UpdateInt(TableColColumnBufLengthIndex, CharLength * ConSettings^.ClientCodePage^.CharWidth +1);
        Result.UpdateInt(TableColColumnCharOctetLengthIndex, FieldSize);
        Result.UpdateInt(TableColColumnSizeIndex, CharLength);
      end else if SQLType = stUnicodeString then begin
        Result.UpdateInt(TableColColumnBufLengthIndex, (FieldSize+2));
        Result.UpdateInt(TableColColumnCharOctetLengthIndex, FieldSize);
        Result.UpdateInt(TableColColumnSizeIndex, CharLength);
      end else if SQLType = stBytes then begin
        Result.UpdateInt(TableColColumnBufLengthIndex, FieldSize);
        Result.UpdateInt(TableColColumnSizeIndex, FieldSize);
        Result.UpdateInt(TableColColumnCharOctetLengthIndex, FieldSize);
      end else begin
        Result.UpdateInt(TableColColumnBufLengthIndex, ZSQLTypeToBuffSize[SQLType]);
        Result.UpdateInt(TableColColumnSizeIndex, Precision);
        Result.UpdateInt(TableColColumnDecimalDigitsIndex, GetInt(DATA_SCALE_Index));
      end;

      if UpperCase(GetString(NULLABLE_Index)) = 'N' then
      begin
        Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNoNulls));
        Result.UpdateString(TableColColumnIsNullableIndex, 'NO');
      end
      else
      begin
        Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNullable));
        Result.UpdateString(TableColColumnIsNullableIndex, 'YES');
      end;
      Result.UpdateInt(TableColColumnOrdPosIndex, GetInt(COLUMN_ID_Index));

      Result.UpdateBoolean(TableColColumnCaseSensitiveIndex,
        IC.IsCaseSensitive(GetString(COLUMN_NAME_Index)));
      Result.UpdateBoolean(TableColColumnSearchableIndex, True);
      B := (oDataType = 'BFILE') or (oDataType = 'CFILE');
      Result.UpdateBoolean(TableColColumnWritableIndex, not B);
      Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, not B);
      Result.UpdateBoolean(TableColColumnReadonlyIndex, B);

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
function TZOracleDatabaseMetadata.UncachedGetCrossReference(
  const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog,
  ForeignSchema, ForeignTable: string): IZResultSet;
begin
  Result := InternalGetCrossReference(PrimarySchema, PrimaryTable, ForeignSchema, ForeignTable,
    'order by ACFK.OWNER, ACFK.TABLE_NAME, CCFK.POSITION', CrossRefColumnsDynArray);
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
function TZOracleDatabaseMetadata.UncachedGetExportedKeys(const Catalog, Schema,
  Table: string): IZResultSet;
begin
  Result := InternalGetCrossReference(Schema, Table, '', '',
    'order by ACFK.OWNER, ACFK.TABLE_NAME, CCFK.POSITION', ExportedKeyColumnsDynArray);
end;

{**
  Gets a description of the access rights for a table's columns.

  <P>Only privileges matching the column name criteria are
  returned.  They are ordered by COLUMN_NAME and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column privilege description
  @see #getSearchStringEscape
}
function TZOracleDatabaseMetadata.InternalGetCrossReference(const PrimarySchema,
  PrimaryTable, ForeignSchema, ForeignTable, OrderBy: string;
  const ColumnsDefs: TZMetadataColumnDefs): IZResultSet;
var
  SQL: String;
  procedure Append(const Condition: String; var SQL: String);
  begin
    if Condition <> ''
      then SQL := SQL + ' and '+Condition;
  end;
begin
  //EH: dead slow but first approach avg~3sec
  //So we should go away from using the giant views of oracle ....
  SQL := 'select'
    + ' null as PKTABLE_CAT, ACPK.OWNER as PKTABLE_SCHEM, ACPK.TABLE_NAME as PKTABLE_NAME, CCPK.COLUMN_NAME as PKCOLUMN_NAME,'
    + ' null as FKTABLE_CAT, ACFK.OWNER as FKTABLE_SCHEM, ACFK.TABLE_NAME as FKTABLE_NAME, CCFK.COLUMN_NAME as FKCOLUMN_NAME,'
    + ' CCFK.POSITION as KEY_SEQ,'
    + ' null as UPDATE_RULE,' //is not given or correct me
    + ' case ACFK.DELETE_RULE '
    + '  when ''RESTRICT'' then '+ZFastCode.IntToStr(Ord(ikRestrict))
    + '  when ''NO ACTION'' then '+ZFastCode.IntToStr(Ord(ikNoAction))
    + '  when ''CASCADE'' then '+ZFastCode.IntToStr(Ord(ikCascade))
    + '  when ''SET DEFAULT'' then '+ZFastCode.IntToStr(Ord(ikSetDefault))
    + '  when ''SET NULL'' then '+ZFastCode.IntToStr(Ord(ikSetNull))
    + '  else '+ZFastCode.IntToStr(Ord(ikNotDeferrable))+' end as DELETE_RULE,'
    + ' ACFK.CONSTRAINT_NAME as FK_NAME, ACPK.CONSTRAINT_NAME as PK_NAME,'
    + ' case ACFK.DEFERRABLE'
    + '  when ''NOT DEFERRABLE'' then '+ZFastCode.IntToStr(Ord(ikNotDeferrable))
    + '  else '+ZFastCode.IntToStr(Ord(ikInitiallyDeferred))+' end as DEFERRABILITY'
    + ' from ALL_CONS_COLUMNS CCFK'
    + ' join ALL_CONSTRAINTS ACFK on ACFK.OWNER = CCFK.OWNER and ACFK.CONSTRAINT_NAME = CCFK.CONSTRAINT_NAME'
    + ' join ALL_CONSTRAINTS ACPK on ACPK.OWNER = ACFK.R_OWNER and ACPK.CONSTRAINT_NAME = ACFK.R_CONSTRAINT_NAME'
    + ' join ALL_CONS_COLUMNS CCPK ON CCPK.OWNER = ACPK.OWNER and CCPK.CONSTRAINT_NAME = ACPK.CONSTRAINT_NAME'
    + ' where ACFK.CONSTRAINT_TYPE=''R'' and ACPK.CONSTRAINT_TYPE=''P''';
  Append(ConstructNameCondition(AddEscapeCharToWildcards(PrimarySchema), 'ACPK.OWNER'), SQL);
  Append(ConstructNameCondition(AddEscapeCharToWildcards(PrimaryTable), 'ACPK.TABLE_NAME'), SQL);
  Append(ConstructNameCondition(AddEscapeCharToWildcards(ForeignSchema), 'ACFK.OWNER'), SQL);
  Append(ConstructNameCondition(AddEscapeCharToWildcards(ForeignTable), 'ACFK.TABLE_NAME'), SQL);
  SQL := SQL+OrderBy;

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(ColumnsDefs));
end;

function TZOracleDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  SQL: string;
  OwnerCondition,TableCondition,ColumnCondition: String;

  function CreateWhere: String;
  begin
    Result := '';
    If OwnerCondition <> '' then
      Result := OwnerCondition;
    If TableCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + TableCondition
      Else
        Result := TableCondition;
    If ColumnCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + ColumnCondition
      Else
        Result := ColumnCondition;
    If Result <> '' then
      Result := ' Where ' + Result;
  end;

begin
  OwnerCondition := ConstructNameCondition(Schema,'TABLE_SCHEMA');
  TableCondition := ConstructNameCondition(Table,'TABLE_NAME');
  ColumnCondition := ConstructNameCondition(ColumnNamePattern,'COLUMN_NAME');
  SQL := 'SELECT NULL AS TABLE_CAT, TABLE_SCHEMA AS TABLE_SCHEM, TABLE_NAME,'
    + ' COLUMN_NAME, GRANTOR, GRANTEE, PRIVILEGE, GRANTABLE AS IS_GRANTABLE'
    + ' FROM SYS.ALL_COL_PRIVS'
    + CreateWhere;

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(TableColPrivColumnsDynArray));
end;

function TZOracleDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  SQL: string;
  OwnerCondition,TableCondition: String;

  function CreateWhere: String;
  begin
    Result := '';
    If OwnerCondition <> '' then
      Result := OwnerCondition;
    If TableCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + TableCondition
      Else
        Result := TableCondition;
    If Result <> '' then
      Result := ' Where ' + Result;
  end;

begin
  OwnerCondition := ConstructNameCondition(SchemaPattern,'TABLE_SCHEMA');
  TableCondition := ConstructNameCondition(TableNamePattern,'TABLE_NAME');
  SQL := 'SELECT NULL AS TABLE_CAT, TABLE_SCHEMA AS TABLE_SCHEM, TABLE_NAME,'
    + ' GRANTOR, GRANTEE, PRIVILEGE, GRANTABLE AS IS_GRANTABLE'
    + ' FROM SYS.ALL_TAB_PRIVS '
    + CreateWhere;

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(TablePrivColumnsDynArray));
end;

function TZOracleDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL: string;
  OwnerCondition,TableCondition: String;

  function CreateExtraWhere: String;
  begin
    Result := '';
    If OwnerCondition <> '' then
      Result := OwnerCondition;
    If TableCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + TableCondition
      Else
        Result := TableCondition;
    If Result <> '' then
      Result := ' AND ' + Result;
  end;

begin
  OwnerCondition := ConstructNameCondition(Schema,'A.OWNER');
  TableCondition := ConstructNameCondition(Table,'A.TABLE_NAME');
  SQL := 'SELECT NULL AS TABLE_CAT, A.OWNER AS TABLE_SCHEM, A.TABLE_NAME,'
        + ' B.COLUMN_NAME, B.POSITION AS KEY_SEQ, A.INDEX_NAME AS PK_NAME'
        + ' FROM ALL_CONSTRAINTS A, ALL_CONS_COLUMNS B'
        + ' WHERE A.CONSTRAINT_TYPE = ''P'''
        + ' AND A.CONSTRAINT_NAME = B.CONSTRAINT_NAME'
        + ' AND A.OWNER = B.OWNER'
        + CreateExtraWhere
        + ' ORDER BY A.INDEX_NAME, B.POSITION';

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(PrimaryKeyColumnsDynArray));
end;

function TZOracleDatabaseMetadata.UncachedGetImportedKeys(const Catalog, Schema,
  Table: string): IZResultSet;
begin
  Result := InternalGetCrossReference('', '', Schema, Table,
    'order by ACPK.OWNER, ACPK.TABLE_NAME, CCPK.POSITION', ExportedKeyColumnsDynArray);
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>NON_UNIQUE</B> Boolean => Can index values be non-unique?
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
function TZOracleDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
const
  OWNER_Index           = FirstDbcIndex + 0;
  TABLE_NAME_Index      = FirstDbcIndex + 1;
  UNIQUENESS_Index      = FirstDbcIndex + 2;
  INDEX_NAME_Index      = FirstDbcIndex + 3;
  COLUMN_POSITION_Index = FirstDbcIndex + 4;
  COLUMN_NAME_Index     = FirstDbcIndex + 5;
  DESCEND_Index         = FirstDbcIndex + 6;
var
  Len: NativeUint;
  SQL: string;
  OwnerCondition,TableCondition: String;

  function CreateExtraWhere: String;
  begin
    Result := '';
    If OwnerCondition <> '' then
      Result := OwnerCondition;
    If TableCondition <> '' then
      If Result <> '' then
        Result := Result + ' AND ' + TableCondition
      Else
        Result := TableCondition;
    If Result <> '' then
      Result := ' AND ' + Result;
  end;

begin
  OwnerCondition := ConstructNameCondition(Schema,'A.TABLE_OWNER');
  TableCondition := ConstructNameCondition(Table,'A.TABLE_NAME');
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  SQL := 'SELECT A.OWNER, A.TABLE_NAME, A.UNIQUENESS, '
    + ' A.INDEX_NAME, B.COLUMN_POSITION, B.COLUMN_NAME, B.DESCEND'
    + ' FROM ALL_INDEXES A, ALL_IND_COLUMNS B'
    + ' WHERE A.OWNER=B.INDEX_OWNER AND A.INDEX_NAME=B.INDEX_NAME'
    + ' AND A.TABLE_OWNER=B.TABLE_OWNER AND A.TABLE_NAME=B.TABLE_NAME'
    + CreateExtraWhere;
  if Unique then
    SQL := SQL + ' AND A.UNIQUENESS=''UNIQUE''';
  SQL := SQL + ' ORDER BY A.UNIQUENESS DESC, A.INDEX_NAME, B.COLUMN_POSITION';
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  with GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      if FConSettings.ClientCodePage.Encoding = ceUTF16 then begin
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar(OWNER_Index, Len), Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideChar(TABLE_NAME_Index, Len), Len);
        Result.UpdatePWideChar(IndexInfoColIndexNameIndex, GetPWideChar(INDEX_NAME_Index, Len), Len);
        Result.UpdatePWideChar(IndexInfoColColumnNameIndex, GetPWideChar(COLUMN_NAME_Index, Len), Len);
      end else begin
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(OWNER_Index, Len), Len);
        Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(TABLE_NAME_Index, Len), Len);
        Result.UpdatePAnsiChar(IndexInfoColIndexNameIndex, GetPAnsiChar(INDEX_NAME_Index, Len), Len);
        Result.UpdatePAnsiChar(IndexInfoColColumnNameIndex, GetPAnsiChar(COLUMN_NAME_Index, Len), Len);
      end;
      Result.UpdateBoolean(IndexInfoColNonUniqueIndex,
        UpperCase(GetString(UNIQUENESS_Index)) <> 'UNIQUE');
      //Result.UpdateNull(IndexInfoColIndexQualifierIndex);
      Result.UpdateInt(IndexInfoColTypeIndex, 3);
      Result.UpdateInt(IndexInfoColOrdPositionIndex, GetInt(COLUMN_POSITION_Index));
      if GetString(DESCEND_Index) = 'ASC' then
        Result.UpdateString(IndexInfoColAscOrDescIndex, 'A')
      else Result.UpdateString(IndexInfoColAscOrDescIndex, 'D');
      Result.UpdateInt(IndexInfoColCardinalityIndex, 0);
      Result.UpdateInt(IndexInfoColPagesIndex, 0);
      //Result.UpdateNull(IndexInfoColFilterConditionIndex);

      Result.InsertRow;
    end;
    Close;
  end;
end;

function TZOracleDatabaseMetadata.UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
  const SequenceNamePattern: string): IZResultSet;
const
  SEQ_OWNER_INDEX= FirstDbcIndex + 0;
  SEQ_NAME_INDEX = FirstDbcIndex + 1;
Var
  sql: String;
  len: NativeUInt;
Begin
  Result := inherited UncachedGetSequences(Catalog, SchemaPattern, SequenceNamePattern);

  sql := 'SELECT SEQUENCE_OWNER, SEQUENCE_NAME FROM ALL_SEQUENCES';

  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  with GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;

      if FConSettings.ClientCodePage.Encoding = ceUTF16 then begin
        Result.UpdatePWideChar(SequenceSchemaIndex, GetPWideChar(SEQ_OWNER_INDEX, Len), Len);
        Result.UpdatePWideChar(SequenceNameIndex, GetPWideChar(SEQ_NAME_INDEX, Len), Len);
      end else begin
        Result.UpdatePAnsiChar(SequenceSchemaIndex, GetPAnsiChar(SEQ_OWNER_INDEX, Len), Len);
        Result.UpdatePAnsiChar(SequenceNameIndex, GetPAnsiChar(SEQ_NAME_INDEX, Len), Len);
      end;

      Result.InsertRow;
    end;
    Close;
  end;
End;

{$ENDIF ZEOS_DISABLE_ORACLE}

end.
