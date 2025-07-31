{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer DuckDB Connectivity Classes         }
{                                                         }
{        Originally written by Jan Baumgarten             }
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
{  http://zeoslib.sourceforge.net  (FORUM)                }
{  http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER) }
{  http://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{  http://www.sourceforge.net/projects/zeoslib.           }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcDuckDBMetadata;

interface

{$I ZDbc.inc}

{$IFDEF ENABLE_PROXY} //if set we have an empty unit
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcMetadata, ZCompatibility, ZSelectSchema;

type

  {** Implements DBC Layer Proxy driver Database Information. }
  TZDuckDBDatabaseInfo = class(TZAbstractDatabaseInfo)
  public
    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
    function GetDriverVersion: string; override;
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
    function GetServerVersion: string; override;

    // capabilities (what it can/cannot do):
    function AllProceduresAreCallable: Boolean; override;
    function AllTablesAreSelectable: Boolean; override;
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
    function SupportsAlterTableWithAddColumn: Boolean; override;
    function SupportsAlterTableWithDropColumn: Boolean; override;
    function SupportsColumnAliasing: Boolean; override;
    function SupportsConvert: Boolean; override;
    // todo implement SupportsConvertForTypes
//    function SupportsConvertForTypes({%H-}FromType: TZSQLType; {%H-}ToType: TZSQLType):
//      Boolean; override;
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
    function SupportsCoreSQLGrammar: Boolean; override;
    function SupportsExtendedSQLGrammar: Boolean; override;
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
    function SupportsParameterBinding: Boolean; override;
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
    // todo implement supported transaction isolation levels
//    function SupportsTransactionIsolationLevel(const {%H-}Level: TZTransactIsolationLevel):
//      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    // todo implement supported result set types
//    function SupportsResultSetType(const {%H-}_Type: TZResultSetType): Boolean; override;
    // todo implement supported result set concurrencies
//    function SupportsResultSetConcurrency(const {%H-}_Type: TZResultSetType;
//      const {%H-}Concurrency: TZResultSetConcurrency): Boolean; override;
    function SupportsBatchUpdates: Boolean; override;
    function SupportsNonEscapedSearchStrings: Boolean; override;
    function SupportsMilliSeconds: Boolean; override;
    function SupportsUpdateAutoIncrementFields: Boolean; override;
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
    function NullPlusNonNullIsNull: Boolean; override;
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
    function GetIdentifierQuoteString: string;
    //function GetSchemaTerm: string; override;
    //function GetProcedureTerm: string; override;
    //function GetCatalogTerm: string; override;
    function GetCatalogSeparator: string; override;
    //function GetSQLKeywords: string; override;
    // todo implement GetIdentifierQuoteKeywordsSorted
//    function GetIdentifierQuoteKeywordsSorted: TStringList;
    //function GetNumericFunctions: string; override;
    //function GetStringFunctions: string; override;
    //function GetSystemFunctions: string; override;
    //function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;
  end;

  IZDuckDBDatabaseMetadata = Interface(IZDatabaseMetadata)
    ['{89439611-FAE2-48F2-BBF3-AFD70520CDE9}']
    // hier
  End;

  {** Implements PostgreSQL Database Metadata. }
  TZDuckDBDatabaseMetadata = class(TZAbstractDatabaseMetadata, IZDuckDBDatabaseMetadata)
  private
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override;

//    function EscapeString(const S: string): string; override;
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    //function UncachedGetSchemas: IZResultSet; override;
    function UncachedGetCatalogs: IZResultSet; override;
    //function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    //function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
    //  const TableNamePattern: string): IZResultSet; override;
    //function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
    //  const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    //function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
    //  const Table: string): IZResultSet; override;
    //function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
    //  const Table: string): IZResultSet; override;
    //function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
    //  const Table: string): IZResultSet; override;
    //function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
    //  const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
    //  const ForeignTable: string): IZResultSet; override;
    //function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
    //  Unique: Boolean; Approximate: Boolean): IZResultSet; override;
    //function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
    //  const SequenceNamePattern: string): IZResultSet; override;
    //function UncachedGetTriggers(const Catalog: string; const SchemaPattern: string;
    //  const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet; override;
    //function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
    //  const ProcedureNamePattern: string): IZResultSet; override;
    //function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
    //  const ProcedureNamePattern: string; const ColumnNamePattern: string):
    //  IZResultSet; override;
//    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
//    function UncachedGetTypeInfo: IZResultSet; override;
    //function UncachedGetCharacterSets: IZResultSet; override; //EgonHugeist
  public
    destructor Destroy; override;
//    function GetIdentifierConvertor: IZIdentifierConvertor; override;
 end;

{$ENDIF ENABLE_PROXY} //if set we have an empty unit
implementation
{$IFDEF ENABLE_PROXY} //if set we have an empty unit

uses
  TypInfo,
  ZFastCode, ZMessages, ZSysUtils, ZPlainProxyDriverIntf, ZDbcProxy, ZDbcProxyResultSet,
  ZDbcCachedResultSet, ZExceptions;

{ TZDuckDBDatabaseInfo }

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What
  's the name of this database product?
  @return database product name
}
function TZDuckDBDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'DuckDB';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZDuckDBDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZDuckDBDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for DuckDB';
end;

function TZDuckDBDatabaseInfo.GetDriverVersion: string;
begin
  Result := '1';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZDuckDBDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZDuckDBDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Returns the server version
  @return the server version string
}
function TZDuckDBDatabaseInfo.GetServerVersion: string;
begin
  // todo: Verrsion aus DLL auslesen
  Result := '1.x.x';
end;

function TZDuckDBDatabaseInfo.NullsAreSortedHigh: Boolean;
begin
  Result := False;
end;

function TZDuckDBDatabaseInfo.NullsAreSortedLow: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.NullsAreSortedAtStart: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.NullsAreSortedAtEnd: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.NullPlusNonNullIsNull: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.UsesLocalFiles: Boolean;
begin
  Result := true;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZDuckDBDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.AllProceduresAreCallable: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.AllTablesAreSelectable: Boolean;
begin
  Result := true;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := true;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := false;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := false;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := false;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsAlterTableWithAddColumn: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsAlterTableWithDropColumn: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsColumnAliasing: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsConvert: Boolean;
begin
  Result := False;
end;

function TZDuckDBDatabaseInfo.SupportsTableCorrelationNames: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsDifferentTableCorrelationNames: Boolean;
begin
  Result := true;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := false;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := false;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := true;
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
function TZDuckDBDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZDuckDBDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := true;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := true;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := true;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := true;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsLikeEscapeClause: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsMultipleResultSets: Boolean;
begin
  // todo implement support for multiple result sets
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsMultipleTransactions: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsNonNullableColumns: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsMinimumSQLGrammar: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsCoreSQLGrammar: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsExtendedSQLGrammar: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsANSI92EntryLevelSQL: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsANSI92IntermediateSQL: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsANSI92FullSQL: Boolean;
begin
  Result := false;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise.

  The SQL Integrity Enhancement facility offers additional tools for referential
  integrity, CHECK constraint clauses, and DEFAULT clauses. Referential integrity
  allows specification of primary and foreign keys with the requirement that no
  foreign key row may be inserted or updated unless a matching primary key row
  exists. Check clauses allow specification of inter-column constraints to be
  maintained by the database system. Default clauses provide optional default
  values for missing data.
}
function TZDuckDBDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsOuterJoins: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsFullOuterJoins: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsLimitedOuterJoins: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.GetIdentifierQuoteString: string;
begin
  Result := '"';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZDuckDBDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result :=true;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := true;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := true;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := true;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := true;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  //Result := true;
  Result := false;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := true;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := true;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := true;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsOverloadPrefixInStoredProcedureName: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsParameterBinding: Boolean;
begin
  Result := false;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := false;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := false;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := false;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := false and false;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := true;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := true;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := true;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := true;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := true;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := true;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := true;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZDuckDBDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := false;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZDuckDBDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := false;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZDuckDBDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := false;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZDuckDBDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsBatchUpdates: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsNonEscapedSearchStrings: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsMilliSeconds: Boolean;
begin
  Result := true;
end;

function TZDuckDBDatabaseInfo.SupportsUpdateAutoIncrementFields: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.SupportsArrayBindings: Boolean;
begin
  Result := false and false;
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
function TZDuckDBDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 0;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxCursorNameLength: Integer;
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
function TZDuckDBDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 0;
end;

function TZDuckDBDatabaseInfo.IsReadOnly: Boolean;
begin
  Result := false;
end;

function TZDuckDBDatabaseInfo.IsCatalogAtStart: Boolean;
begin
  Result := false
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := false;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 0
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZDuckDBDatabaseInfo.GetMaxUserNameLength: Integer;
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
function TZDuckDBDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := true;
end;

//{**
//  Does this database support the given transaction isolation level?
//  @param level the values are defined in <code>java.sql.Connection</code>
//  @return <code>true</code> if so; <code>false</code> otherwise
//  @see Connection
//}
//function TZDuckDBDatabaseInfo.SupportsTransactionIsolationLevel(
//  const Level: TZTransactIsolationLevel): Boolean;
//begin
//  Result := FSupportsTransactionIsolationLevel;
//end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := false;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := true;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := false;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZDuckDBDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := false;
end;

//{**
//  Does the database support the given result set type?
//  @param type defined in <code>java.sql.ResultSet</code>
//  @return <code>true</code> if so; <code>false</code> otherwise
//}
//function TZDuckDBDatabaseInfo.SupportsResultSetType(
//  const _Type: TZResultSetType): Boolean;
//begin
//  Result := _Type = rtScrollInsensitive;
//end;
//
//{**
//  Does the database support the concurrency type in combination
//  with the given result set type?
//
//  @param type defined in <code>java.sql.ResultSet</code>
//  @param concurrency type defined in <code>java.sql.ResultSet</code>
//  @return <code>true</code> if so; <code>false</code> otherwise
//}
//function TZDuckDBDatabaseInfo.SupportsResultSetConcurrency(
//  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
//begin
//  Result := (_Type = rtScrollInsensitive) and (Concurrency = rcReadOnly);
//end;

//----------------------------------------------------------------------

{ TZDuckDBDatabaseMetadata }


{**
  Destroys this object and cleanups the memory.
}
destructor TZDuckDBDatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZDuckDBDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZDuckDBDatabaseInfo.Create(Self);
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
function TZDuckDBDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  //I: Integer;
  //TableType, OrderBy,
  TempIS, TempRes, SQL: string;
  //UseSchemas: Boolean;
  //LTypes: TStringDynArray;

  TableNameCondition, SchemaCondition, CatalogCondition: string;
begin
  Result := inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

  // todo: Implement the types list
  (Result as IZVirtualResultSet).SetConcurrency(rcUpdatable);
  CatalogCondition := ConstructNameCondition(Catalog,'information_schema.tables.table_catalog');
  SchemaCondition := ConstructNameCondition(SchemaPattern,'information_schema.tables.table_schema');
  TableNameCondition := ConstructNameCondition(TableNamePattern,'information_schema.tables.table_name');
  SQL :='SELECT table_catalog,table_schema,table_name, table_type, table_comment from information_schema.tables';
  if (SchemaCondition <> '') or (TableNameCondition <> '')then
  begin
    SQL := SQL + ' where ';
    if (SchemaCondition <> '') then
    begin
      SQL := SQL + SchemaCondition;
      if (TableNameCondition <> '') then
        SQL := SQL + ' and '+TableNameCondition;
    end
    else
      SQL := SQL + TableNameCondition;
  end;
  SQL := SQL + ' order by table_catalog, table_schema, table_name';
  with GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, GetString(CatalogNameIndex));
      Result.UpdateString(SchemaNameIndex, GetString(SchemaNameIndex));
      Result.UpdateString(TableNameIndex, GetString(TableNameIndex));
      Result.UpdateString(3, GetString(3));
      Result.UpdateStringByName('remarks', GetStringByName('table_comment'));
      Result.InsertRow;
    end;
    Close;
  end;
  (Result as IZVirtualResultSet).BeforeFirst;
  (Result as IZVirtualResultSet).SetConcurrency(rcReadOnly);
end;

(*

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
function TZDuckDBDatabaseMetadata.UncachedGetSchemas: IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetSchemas;

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
end;
*)
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
function TZDuckDBDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
begin
  Result := inherited UncachedGetCatalogs;

  (Result as IZVirtualResultSet).SetConcurrency(rcUpdatable);
  with GetConnection.CreateStatement.ExecuteQuery('select distinct catalog_name from information_schema.schemata order by catalog_name') do begin
    while Next do begin
      Result.MoveToInsertRow;
      Result.UpdateString(0, GetString(0));
      Result.InsertRow;
    end;
  end;
  (Result as IZVirtualResultSet).SetConcurrency(rcReadOnly);
end;

(*
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
function TZDuckDBDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetTableTypes;

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
end;
*)
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
function TZDuckDBDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  CatalogCondition, SchemaCondition, TableNameCondition, ColumnNameCondition: String;
  SQL: String;
  FirstCond: Boolean;
  DuckData: IZResultSet;

  procedure AddCondition(Condition: String);
  begin
    if Condition = '' then
      exit;

    if FirstCond then begin
      SQL := SQL + ' where ' + Condition;
      FirstCond := False;
    end else
      SQL := SQL + ' and ' + Condition;
  end;

  procedure CopyData(const Src, Dst: String; SqlType: TZSQLType);
  begin
    if DuckData.IsNullByName(Src) then
      Result.UpdateNullByName(Dst)
    else
      case SqlType of
        stBoolean: Result.UpdateBooleanByName(Dst, DuckData.GetBooleanByName(Src));
        stInteger: Result.UpdateIntByName(Dst, DuckData.GetIntByName(Src));
        stString: Result.UpdateStringByName(Dst, DuckData.GetStringByName(Src));
        else
          raise EZSQLException.Create('Cannot convert');
      end;

  end;
begin
  FirstCond := True;
  Result := inherited UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

  CatalogCondition := ConstructNameCondition(Catalog,'information_schema.columns.table_catalog');
  SchemaCondition := ConstructNameCondition(SchemaPattern,'information_schema.columns.table_schema');
  TableNameCondition := ConstructNameCondition(TableNamePattern,'information_schema.columns.table_name');
  ColumnNameCondition := ConstructNameCondition(ColumnNamePattern, 'information_schema.columns.column_name');
  SQL :='SELECT * from information_schema.columns';
  AddCondition(CatalogCondition);
  AddCondition(SchemaCondition);
  AddCondition(TableNameCondition);
  AddCondition(ColumnNameCondition);

  SQL := SQL + ' order by table_catalog, table_schema, table_name, ordinal_position';
  (Result as IZVirtualResultSet).SetConcurrency(rcUpdatable);
  DuckData := GetConnection.CreateStatement.ExecuteQuery(SQL);
  try
    while DuckData.Next do
    begin
      Result.MoveToInsertRow;
      CopyData('table_catalog', 'TABLE_CAT', stString);
      CopyData('table_schema', 'TABLE_SCHEM', stString);
      CopyData('table_name', 'TABLE_NAME', stString);
      CopyData('column_name', 'COLUMN_NAME', stString);
      CopyData('ordinal_position', 'ORDINAL_POSITION', stInteger);
      CopyData('column_default', 'COLUMN_DEF', stString);
      CopyData('is_nullable', 'IS_NULLABLE', stBoolean);
      if DuckData.IsNullByName('is_nullable') then
        Result.UpdateIntByName('NULLABLE', ord(TZColumnNullableType.ntNullableUnknown))
      else if DuckData.GetBooleanByName('is_nullable') then
        Result.UpdateIntByName('NULLABLE', ord(TZColumnNullableType.ntNullable))
      else
        Result.UpdateIntByName('NULLABLE', ord(TZColumnNullableType.ntNoNulls));
      CopyData('data_type', 'TYPE_NAME', stString);
      CopyData('character_octet_length', 'CHAR_OCTET_LENGTH', stInteger);
      CopyData('numeric_precision', 'COLUMN_SIZE', stInteger);
      CopyData('numeric_precision_radix', 'NUM_PREC_RADIX', stInteger);
      CopyData('numeric_scale', 'DECIMAL_DIGITS', stInteger);
      CopyData('is_identity', 'AUTO_INCREMENT', stBoolean);
      {if DuckData.IsNullByName('is_updatable') then begin
        Result.UpdateNullByName('WRITABLE');
        Result.UpdateNullByName('DEFINITELYWRITABLE');
        Result.UpdateNullByName('READONLY')
      end else} if DuckData.IsNullByName('is_updatable') or DuckData.GetBooleanByName('is_updatable') then begin
        Result.UpdateBooleanByName('WRITABLE', true);
        Result.UpdateBooleanByName('DEFINITELYWRITABLE', true);
        Result.UpdateBooleanByName('READONLY', false);
      end else begin
        Result.UpdateBooleanByName('WRITABLE', false);
        Result.UpdateBooleanByName('DEFINITELYWRITABLE', false);
        Result.UpdateBooleanByName('READONLY', true);
      end;
      CopyData('COLUMN_COMMENT', 'REMARKS', stString);
      Result.UpdateNullByName('BUFFER_LENGTH');
      Result.UpdateNullByName('SQL_DATA_TYPE');
      Result.UpdateNullByName('SQL_DATETIME_SUB');
      Result.UpdateNullByName('CASE_SENSITIVE');
      Result.UpdateBooleanByName('SEARCHABLE', true);
      Result.InsertRow;
    end;
  finally
    DuckData.Close;
  end;
  (Result as IZVirtualResultSet).BeforeFirst;
  (Result as IZVirtualResultSet).SetConcurrency(rcReadOnly);
end;

(*
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
function TZDuckDBDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
end;

{**
  Gets a description of the access rights for each table available
  in a catalog. Note that a table privilege applies to one or
  more columns in the table. It would be wrong to assume that
  this priviledge applies to all columns (this may be true for
  some systems but is not true for all.)

  <P>Only privileges matching the schema and table name
  criteria are returned.  They are ordered by TABLE_SCHEM,
  TABLE_NAME, and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @return <code>ResultSet</code> - each row is a table privilege description
  @see #getSearchStringEscape
}
function TZDuckDBDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
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
//function TZDuckDBDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
//  const Schema: string; const Table: string): IZResultSet;
//begin
//    Result:=inherited UncachedGetVersionColumns(Catalog, Schema, Table);
//
//    Result.MoveToInsertRow;
//    //Result.UpdateNull(TableColVerScopeIndex);
//    Result.UpdateString(TableColVerColNameIndex, 'ctid');
//    Result.UpdateInt(TableColVerDataTypeIndex, Ord(GetSQLTypeByName('tid')));
//    Result.UpdateString(TableColVerTypeNameIndex, 'tid');
//    //Result.UpdateNull(TableColVerColSizeIndex);
//    //Result.UpdateNull(TableColVerBufLengthIndex);
//    //Result.UpdateNull(TableColVerDecimalDigitsIndex);
//    Result.UpdateInt(TableColVerPseudoColumnIndex, Ord(vcPseudo));
//    Result.InsertRow;
//end;

{**
  Gets a description of a table's primary key columns.  They
  are ordered by COLUMN_NAME.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>KEY_SEQ</B> short => sequence number within primary key
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @exception SQLException if a database access error occurs
}
function TZDuckDBDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetPrimaryKeys(Catalog, Schema, Table);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
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
function TZDuckDBDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetImportedKeys(Catalog, Schema, Table);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
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
function TZDuckDBDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetExportedKeys(Catalog, Schema, Table);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
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
function TZDuckDBDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
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
 	<LI><B>CASE_SENSITIVE</B> Boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> Boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> Boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> Boolean => can it be used for an
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
//function TZPostgreSQLDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
//var
//  SQL: string;
//  Len: NativeUInt;
//begin
//    Result:=inherited UncachedGetTypeInfo;
//
//    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
//      SQL := ' SELECT typname FROM pg_catalog.pg_type '
//    else SQL := ' SELECT typname FROM pg_type ';
//
//    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
//    begin
//      while Next do
//      begin
//        Result.MoveToInsertRow;
//        Result.UpdatePAnsiChar(TypeInfoTypeNameIndex, GetPAnsiChar(FirstDbcIndex, Len), @Len);
//        Result.UpdateInt(TypeInfoDataTypeIndex, Ord(GetSQLTypeByName(GetString(FirstDbcIndex))));
//        Result.UpdateInt(TypeInfoPecisionIndex, 9);
//        Result.UpdateInt(TypeInfoNullAbleIndex, Ord(ntNoNulls));
//        Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, False);
//        Result.UpdateBoolean(TypeInfoSearchableIndex, False);
//        Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, False);
//        Result.UpdateBoolean(TypeInfoAutoIncrementIndex, False);
//        Result.UpdateInt(TypeInfoNumPrecRadix, 10);
//        Result.InsertRow;
//      end;
//      Close;
//    end;
//end;

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
function TZDuckDBDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
end;

function TZDuckDBDatabaseMetadata.UncachedGetSequences(const Catalog, SchemaPattern,
  SequenceNamePattern: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetSequences(Catalog, SchemaPattern, SequenceNamePattern);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
end;

function TZDuckDBDatabaseMetadata.UncachedGetTriggers(const Catalog: string; const SchemaPattern: string;
  const TableNamePattern: string; const TriggerNamePattern: string): IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetTriggers(Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern);

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
end;

{**
  Gets the all supported CharacterSets:
  @return <code>ResultSet</code> - each row is a CharacterSetName and it's ID
}
function TZDuckDBDatabaseMetadata.UncachedGetCharacterSets: IZResultSet;
var
  Res: WideString;
begin
  Res := (GetConnection as IZDbcProxyConnection).GetConnectionInterface.GetCharacterSets;

  Result := TZDbcProxyResultSet.Create(GetConnection, '', Res);
end;

*)

{$ENDIF ENABLE_PROXY} //if set we have an empty unit
end.
