{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Sybase Database metadata information           }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcDbLibSybaseMetadata;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata,
  ZCompatibility, ZGenericSqlAnalyser, ZDbcConnection;

type

  {** Implements Sybase Database Metadata. }
  TZSybaseDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  protected
    function GetStatement: IZStatement;
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetSchemas: IZResultSet; override;
    function UncachedGetCatalogs: IZResultSet; override;
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
//     function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
//      const SequenceNamePattern: string): IZResultSet; virtual; -> Not implemented
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
  public
    constructor Create(Connection: TZAbstractConnection; Url: string;
      Info: TStrings);
    destructor Destroy; override;

    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
    function UsesLocalFilePerTable: Boolean; override;
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function StoresUpperCaseIdentifiers: Boolean; override;
    function StoresLowerCaseIdentifiers: Boolean; override;
    function StoresMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
    function StoresUpperCaseQuotedIdentifiers: Boolean; override;
    function StoresLowerCaseQuotedIdentifiers: Boolean; override;
    function StoresMixedCaseQuotedIdentifiers: Boolean; override;
    function GetIdentifierQuoteString: string; override;
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;

    function SupportsExpressionsInOrderBy: Boolean; override;
    function SupportsOrderByUnrelated: Boolean; override;
    function SupportsGroupBy: Boolean; override;
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
    function SupportsIntegrityEnhancementFacility: Boolean; override;
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
    function GetCatalogSeparator: string; override;
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
    function SupportsUnionAll: Boolean;  override;
    function SupportsOpenCursorsAcrossCommit: Boolean; override;
    function SupportsOpenCursorsAcrossRollback: Boolean; override;
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;

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
    function DoesMaxRowSizeIncludeBlobs: Boolean; override;
    function GetMaxStatementLength: Integer; override;
    function GetMaxStatements: Integer; override;
    function GetMaxTableNameLength: Integer; override;
    function GetMaxTablesInSelect: Integer; override;
    function GetMaxUserNameLength: Integer; override;

    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
    function SupportsTransactions: Boolean; override;
    function SupportsTransactionIsolationLevel(Level: TZTransactIsolationLevel):
      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function DataDefinitionCausesTransactionCommit: Boolean; override;
    function DataDefinitionIgnoredInTransactions: Boolean; override;

    function SupportsResultSetType(_Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(_Type: TZResultSetType;
      Concurrency: TZResultSetConcurrency): Boolean; override;
  end;

implementation

uses ZDbcUtils, ZDbcDbLibUtils;

{ TZSybaseDatabaseMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZSybaseDatabaseMetadata.Create(Connection: TZAbstractConnection;
  Url: string; Info: TStrings);
begin
  inherited Create(Connection, Url, Info);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSybaseDatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZSybaseDatabaseMetadata.GetDatabaseProductName: string;
begin
  Result := 'Sybase';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZSybaseDatabaseMetadata.GetDatabaseProductVersion: string;
begin
  Result := '12+';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZSybaseDatabaseMetadata.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Sybase ASE Server';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZSybaseDatabaseMetadata.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZSybaseDatabaseMetadata.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZSybaseDatabaseMetadata.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  What's the string used to quote SQL identifiers?
  This returns a space " " if identifier quoting isn't supported.
  A JDBC Compliant<sup><font size=-2>TM</font></sup>
  driver always uses a double quote character.
  @return the quoting string
}
function TZSybaseDatabaseMetadata.GetIdentifierQuoteString: string;
begin
  Result := '"';
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZSybaseDatabaseMetadata.GetSQLKeywords: string;
begin
{ TODO -ofjanos -cAPI : SQL Keywords that are not SQL92 compliant }
  Result := '';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZSybaseDatabaseMetadata.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATN2,CEILING,COS,COT,DEGREES,EXP,FLOOR,LOG,LOG10,'+
            'PI,POWER,RADIANS,RAND,ROUND,SIGN,SIN,SQUARE,SQRT,TAN';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZSybaseDatabaseMetadata.GetStringFunctions: string;
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
function TZSybaseDatabaseMetadata.GetSystemFunctions: string;
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
function TZSybaseDatabaseMetadata.GetTimeDateFunctions: string;
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
function TZSybaseDatabaseMetadata.GetSearchStringEscape: string;
begin
{ TODO -ofjanos -cgeneral : 
In sql server this must be specified as the parameter of like.
example: WHERE ColumnA LIKE '%5/%%' ESCAPE '/' }
  Result := '/';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZSybaseDatabaseMetadata.GetExtraNameCharacters: string;
begin
  Result := '@$#';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZSybaseDatabaseMetadata.GetSchemaTerm: string;
begin
  Result := 'owner';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZSybaseDatabaseMetadata.GetProcedureTerm: string;
begin
  Result := 'procedure';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZSybaseDatabaseMetadata.GetCatalogTerm: string;
begin
  Result := 'database';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZSybaseDatabaseMetadata.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsPositionedDelete: Boolean;
begin
//CURRENT OF
//Specifies that the DELETE is done at the current position of the specified cursor.
  Result := True;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsPositionedUpdate: Boolean;
begin
  Result := True;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSubqueriesInIns: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZSybaseDatabaseMetadata.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZSybaseDatabaseMetadata.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZSybaseDatabaseMetadata.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZSybaseDatabaseMetadata.SupportsOpenStatementsAcrossRollback: Boolean;
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
function TZSybaseDatabaseMetadata.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 16000;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxCharLiteralLength: Integer;
begin
  Result := 8000;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxColumnNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxColumnsInSelect: Integer;
begin
  Result := 4096;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxColumnsInTable: Integer;
begin
  Result := 1024;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxCursorNameLength: Integer;
begin
  Result := 128;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxIndexLength: Integer;
begin
  Result := 900;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxSchemaNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxProcedureNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxCatalogNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxRowSize: Integer;
begin
  Result := 8060;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := False;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxStatementLength: Integer;
begin
  Result := 0;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxTableNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxTablesInSelect: Integer;
begin
  Result := 256;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSybaseDatabaseMetadata.GetMaxUserNameLength: Integer;
begin
  Result := 128;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZSybaseDatabaseMetadata.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZSybaseDatabaseMetadata.SupportsTransactionIsolationLevel(
  Level: TZTransactIsolationLevel): Boolean;
begin
  Result := True;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := False;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Gets a description of the stored procedures available in a
  catalog.

  <P>Only procedure descriptions matching the schema and
  procedure name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM, and PROCEDURE_NAME.

  <P>Each procedure description has the the following columns:
   <OL>
        <LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
        <LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
        <LI><B>PROCEDURE_NAME</B> String => procedure name
   <LI> reserved for future use
   <LI> reserved for future use
   <LI> reserved for future use
        <LI><B>REMARKS</B> String => explanatory comment on the procedure
        <LI><B>PROCEDURE_TYPE</B> short => kind of procedure:
       <UL>
       <LI> procedureResultUnknown - May return a result
       <LI> procedureNoResult - Does not return a result
       <LI> procedureReturnsResult - Returns a result
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @return <code>ResultSet</code> - each row is a procedure description
  @see #getSearchStringEscape
}
function TZSybaseDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(ProceduresColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_stored_procedures %s, %s, %s',
      [AQSNull(Catalog), AQSNull(SchemaPattern), AQSNull(ProcedureNamePattern)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('PROCEDURE_CAT',
          GetStringByName('PROCEDURE_CAT'));
        Result.UpdateStringByName('PROCEDURE_SCHEM',
          GetStringByName('PROCEDURE_SCHEM'));
        Result.UpdateStringByName('PROCEDURE_NAME',
          GetStringByName('PROCEDURE_NAME'));
        Result.UpdateStringByName('REMARKS',
          GetStringByName('REMARKS'));
        Result.UpdateShortByName('PROCEDURE_TYPE',
          GetShortByName('PROCEDURE_TYPE'));
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
function TZSybaseDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  ProcNamePart: string;
  NumberPart: string;
  status2: Integer;
begin
    Result := ConstructVirtualResultSet(ProceduresColColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_getprocedurecolumns %s, %s, %s, %s',
      [AQSNull(Catalog), AQSNull(SchemaPattern), AQSNull(ProcedureNamePattern), AQSNull(ColumnNamePattern)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('PROCEDURE_CAT',
          GetStringByName('PROCEDURE_CAT'));
        Result.UpdateStringByName('PROCEDURE_SCHEM',
          GetStringByName('PROCEDURE_SCHEM'));
        Result.UpdateStringByName('PROCEDURE_NAME',
          GetStringByName('PROCEDURE_NAME'));
        Result.UpdateStringByName('COLUMN_NAME',
          GetStringByName('COLUMN_NAME'));
        case GetShortByName('COLUMN_TYPE') of
          0, 1: Result.UpdateShortByName('COLUMN_TYPE', 1); //ptInput
          2: Result.UpdateShortByName('COLUMN_TYPE', 3); //ptInputOutput
          3: Result.UpdateShortByName('COLUMN_TYPE', 0); //ptUnknown
          4: Result.UpdateShortByName('COLUMN_TYPE', 3); //ptInputOutput
          5: Result.UpdateShortByName('COLUMN_TYPE', 4); //ptResult
        else
          Result.UpdateShortByName('COLUMN_TYPE', 0); //ptUnknown
        end;
        Result.UpdateShortByName('DATA_TYPE',
          Ord(ConvertODBCToSqlType(GetShortByName('DATA_TYPE'))));
        Result.UpdateStringByName('TYPE_NAME',
          GetStringByName('TYPE_NAME'));
        Result.UpdateIntByName('PRECISION',
          GetIntByName('PRECISION'));
        Result.UpdateIntByName('LENGTH',
          GetIntByName('LENGTH'));
        Result.UpdateShortByName('SCALE',
          GetShortByName('SCALE'));
        Result.UpdateShortByName('RADIX',
          GetShortByName('RADIX'));
        Result.UpdateShortByName('NULLABLE',
          GetShortByName('NULLABLE'));
        Result.UpdateStringByName('REMARKS',
          GetStringByName('REMARKS'));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;

    NumberPart := '1';
    ProcNamePart := '';
    if AnsiPos(';', ProcNamePart) > 0 then
    begin
      NumberPart := Copy(ProcNamePart, LastDelimiter(';', ProcNamePart) + 1,
        Length(ProcNamePart));
      if NumberPart = '' then
        NumberPart := '1';

      ProcNamePart := Copy(ProcNamePart, 1, LastDelimiter(';', ProcNamePart));
      if ProcNamePart[Length(ProcNamePart)] = ';' then
        Delete(ProcNamePart, Length(ProcNamePart), 1);
    end;
  //status2 is added in sybase ASE 12.5 to store the storedprocedure parameters
  // input/output type this column does not exists in prior versions.
  // In prior versions there is no way to determine between input or output type.
    with GetStatement.ExecuteQuery(
      Format('select c.* from syscolumns c inner join sysobjects o on'
      + ' (o.id = c.id) where o.name = %s and c.number = %s order by colid',
      [AnsiQuotedStr(ProcNamePart, ''''), NumberPart])) do
    begin
      Result.Next;//Skip return parameter
      while Next do
      begin
        Result.Next;
        if FindColumn('status2') >= 1 then
          status2 := GetShortByName('status2')
        else
          status2 := 0;
        case status2 of
          0, 1: Result.UpdateShortByName('COLUMN_TYPE', 1); //ptInput
          2: Result.UpdateShortByName('COLUMN_TYPE', 3); //ptInputOutput
          3: Result.UpdateShortByName('COLUMN_TYPE', 0); //ptUnknown
          4: Result.UpdateShortByName('COLUMN_TYPE', 3); //ptInputOutput
          5: Result.UpdateShortByName('COLUMN_TYPE', 4); //ptResult
        else
          Result.UpdateShortByName('COLUMN_TYPE', 0); //ptUnknown
        end;
        Result.UpdateRow;
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
                        "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY",
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
function TZSybaseDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  TableTypes: string;
begin
    Result := ConstructVirtualResultSet(TableColumnsDynArray);

    TableTypes := '';
    for I := 0 to Length(Types) - 1 do
    begin
      if TableTypes <> '' then
        TableTypes := TableTypes + ',';
      TableTypes := TableTypes + AnsiQuotedStr(Types[I], '''');
    end;

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_tables %s, %s, %s, %s',
      [AQSNull(TableNamePattern), AQSNull(SchemaPattern), AQSNull(Catalog), AQSNull(TableTypes, '"')])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_CAT',
          GetStringByName('TABLE_CAT'));
        Result.UpdateStringByName('TABLE_SCHEM',
          GetStringByName('TABLE_SCHEM'));
        Result.UpdateStringByName('TABLE_NAME',
          GetStringByName('TABLE_NAME'));
        Result.UpdateStringByName('TABLE_TYPE',
          GetStringByName('TABLE_TYPE'));
        Result.UpdateStringByName('REMARKS',
          GetStringByName('REMARKS'));
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
function TZSybaseDatabaseMetadata.UncachedGetSchemas: IZResultSet;
begin
    Result := ConstructVirtualResultSet(SchemaColumnsDynArray);

    with GetStatement.ExecuteQuery('exec sp_jdbc_getschemas') do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_SCHEM',
          GetStringByName('TABLE_SCHEM'));
        Result.InsertRow;
      end;
      Close;
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
function TZSybaseDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
begin
    Result := ConstructVirtualResultSet(CatalogColumnsDynArray);

    with GetStatement.ExecuteQuery('exec sp_jdbc_getcatalogs') do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_CAT',
          GetStringByName('TABLE_CAT'));
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets the table types available in this database.  The results
  are ordered by table type.

  <P>The table type is:
   <OL>
        <LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
                        "VIEW", "SYSTEM TABLE", "GLOBAL TEMPORARY",
                        "LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZSybaseDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypes: array[0..2] of string = ('SYSTEM TABLE', 'TABLE', 'VIEW');
var
  I: Integer;
begin
    Result := ConstructVirtualResultSet(TableTypeColumnsDynArray);
    for I := 0 to 2 do
    begin
      Result.MoveToInsertRow;
      Result.UpdateStringByName('TABLE_TYPE', TableTypes[I]);
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
        <LI><B>ORDINAL_POSITION</B> int => index of column in table
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
function TZSybaseDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableColColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_columns %s, %s, %s, %s',
      [AQSNull(TableNamePattern), AQSNull(SchemaPattern), AQSNull(Catalog), AQSNull(ColumnNamePattern)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_CAT',
          ''{GetStringByName('TABLE_CAT')});
        Result.UpdateStringByName('TABLE_SCHEM',
          ''{GetStringByName('TABLE_SCHEM')});
        Result.UpdateStringByName('TABLE_NAME',
          GetStringByName('TABLE_NAME'));
        Result.UpdateStringByName('COLUMN_NAME',
          GetStringByName('COLUMN_NAME'));
  //The value in the resultset will be used
  //      Result.UpdateShortByName('DATA_TYPE',
  //        Ord(ConvertODBCToSqlType(GetShortByName('DATA_TYPE'))));
        Result.UpdateStringByName('TYPE_NAME',
          GetStringByName('TYPE_NAME'));
        Result.UpdateIntByName('COLUMN_SIZE',
          GetIntByName('COLUMN_SIZE'));
        Result.UpdateIntByName('BUFFER_LENGTH',
          GetIntByName('BUFFER_LENGTH'));
        Result.UpdateIntByName('DECIMAL_DIGITS',
          GetIntByName('DECIMAL_DIGITS'));
        Result.UpdateIntByName('NUM_PREC_RADIX',
          GetShortByName('NUM_PREC_RADIX'));
        Result.UpdateShortByName('NULLABLE',
          GetShortByName('NULLABLE'));
        Result.UpdateStringByName('REMARKS',
          GetStringByName('REMARKS'));
        Result.UpdateStringByName('COLUMN_DEF',
          GetStringByName('COLUMN_DEF'));
        Result.UpdateShortByName('SQL_DATA_TYPE',
          GetShortByName('SQL_DATA_TYPE'));
        Result.UpdateShortByName('SQL_DATETIME_SUB',
          GetShortByName('SQL_DATETIME_SUB'));
        Result.UpdateIntByName('CHAR_OCTET_LENGTH',
          GetIntByName('CHAR_OCTET_LENGTH'));
        Result.UpdateIntByName('ORDINAL_POSITION',
          GetIntByName('ORDINAL_POSITION'));
        Result.UpdateStringByName('IS_NULLABLE',
          GetStringByName('IS_NULLABLE'));
        Result.InsertRow;
      end;
      Close;
    end;
    Result.BeforeFirst;
    with GetStatement.ExecuteQuery(
      Format('select c.colid, c.name, c.type, c.prec, c.scale, c.status'
      + ' from syscolumns c inner join sysobjects o on (o.id = c.id)'
      + ' where o.name = %s order by colid', [AnsiQuotedStr(TableNamePattern, '''')])) do
    begin
      while Next do
      begin
        Result.Next;
        Result.UpdateBooleanByName('AUTO_INCREMENT',
          (GetShortByName('status') and $80) <> 0);
        Result.UpdateNullByName('CASE_SENSITIVE');
        Result.UpdateBooleanByName('SEARCHABLE',
          not (GetShortByName('type') in [34, 35]));
        Result.UpdateBooleanByName('WRITABLE',
          ((GetShortByName('status') and $80) = 0)
          and (GetShortByName('type') <> 37));
        Result.UpdateBooleanByName('DEFINITELYWRITABLE',
          Result.GetBooleanByName('WRITABLE'));
        Result.UpdateBooleanByName('READONLY',
          not Result.GetBooleanByName('WRITABLE'));
        if Result.GetBooleanByName('AUTO_INCREMENT') then
        begin
          Result.UpdateShortByName('NULLABLE', 1);
          Result.UpdateStringByName('IS_NULLABLE', 'YES');
        end;
        Result.UpdateRow;
      end;
      Close;
    end;
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
function TZSybaseDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableColPrivColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_getcolumnprivileges %s, %s, %s, %s',
      [AQSNull(Catalog), AQSNull(Schema), AQSNull(Table),
       AQSNullText(ColumnNamePattern, '''%''')])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_CAT',
          GetStringByName('TABLE_CAT'));
        Result.UpdateStringByName('TABLE_SCHEM',
          GetStringByName('TABLE_SCHEM'));
        Result.UpdateStringByName('TABLE_NAME',
          GetStringByName('TABLE_NAME'));
        Result.UpdateStringByName('COLUMN_NAME',
          GetStringByName('COLUMN_NAME'));
        Result.UpdateStringByName('GRANTOR',
          GetStringByName('GRANTOR'));
        Result.UpdateStringByName('GRANTEE',
          GetStringByName('GRANTEE'));
        Result.UpdateStringByName('PRIVILEGE',
          GetStringByName('PRIVILEGE'));
        Result.UpdateStringByName('IS_GRANTABLE',
          GetStringByName('IS_GRANTABLE'));
        Result.InsertRow;
      end;
      Close;
    end;
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
function TZSybaseDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TablePrivColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_gettableprivileges %s, %s, %s',
      [AQSNull(Catalog), AQSNull(SchemaPattern), AQSNull(TableNamePattern)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_CAT',
          GetStringByName('TABLE_CAT'));
        Result.UpdateStringByName('TABLE_SCHEM',
          GetStringByName('TABLE_SCHEM'));
        Result.UpdateStringByName('TABLE_NAME',
          GetStringByName('TABLE_NAME'));
        Result.UpdateStringByName('GRANTOR',
          GetStringByName('GRANTOR'));
        Result.UpdateStringByName('GRANTEE',
          GetStringByName('GRANTEE'));
        Result.UpdateStringByName('PRIVILEGE',
          GetStringByName('PRIVILEGE'));
        Result.UpdateStringByName('IS_GRANTABLE',
          GetStringByName('IS_GRANTABLE'));
        Result.InsertRow;
      end;
      Close;
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
        <LI><B>DECIMAL_DIGITS</B> short  => scale
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
function TZSybaseDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(TableColVerColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_getversioncolumns %s, %s, %s',
      [AQSNull(Catalog), AQSNull(Schema), AQSNull(Table)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateShortByName('SCOPE',
          GetShortByName('SCOPE'));
        Result.UpdateStringByName('COLUMN_NAME',
          GetStringByName('COLUMN_NAME'));
        Result.UpdateShortByName('DATA_TYPE',
          Ord(ConvertODBCToSqlType(GetShortByName('DATA_TYPE'))));
        Result.UpdateStringByName('TYPE_NAME',
          GetStringByName('TYPE_NAME'));
        Result.UpdateIntByName('COLUMN_SIZE',
          GetIntByName('COLUMN_SIZE'));
        Result.UpdateIntByName('BUFFER_LENGTH',
          GetIntByName('BUFFER_LENGTH'));
        Result.UpdateIntByName('DECIMAL_DIGITS',
          GetIntByName('DECIMAL_DIGITS'));
        Result.UpdateShortByName('PSEUDO_COLUMN',
          GetShortByName('PSEUDO_COLUMN'));
        Result.InsertRow;
      end;
      Close;
    end;
end;

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
function TZSybaseDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(PrimaryKeyColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_primarykey %s, %s, %s',
      [AQSNull(Catalog), AQSNull(Schema), AQSNull(Table)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_CAT',
          GetStringByName('TABLE_CAT'));
        Result.UpdateStringByName('TABLE_SCHEM',
          GetStringByName('TABLE_SCHEM'));
        Result.UpdateStringByName('TABLE_NAME',
          GetStringByName('TABLE_NAME'));
        Result.UpdateStringByName('COLUMN_NAME',
          GetStringByName('COLUMN_NAME'));
        Result.UpdateShortByName('KEY_SEQ',
          GetShortByName('KEY_SEQ'));
        Result.UpdateStringByName('PK_NAME',
          GetStringByName('PK_NAME'));
        Result.InsertRow;
      end;
      Close;
    end;
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
function TZSybaseDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(ImportedKeyColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_importkey %s, %s, %s',
      [AQSNull(Catalog), AQSNull(Schema), AQSNull(Table)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('PKTABLE_CAT',
          GetStringByName('PKTABLE_CAT'));
        Result.UpdateStringByName('PKTABLE_SCHEM',
          GetStringByName('PKTABLE_SCHEM'));
        Result.UpdateStringByName('PKTABLE_NAME',
          GetStringByName('PKTABLE_NAME'));
        Result.UpdateStringByName('PKCOLUMN_NAME',
          GetStringByName('PKCOLUMN_NAME'));
        Result.UpdateStringByName('FKTABLE_CAT',
          GetStringByName('FKTABLE_CAT'));
        Result.UpdateStringByName('FKTABLE_SCHEM',
          GetStringByName('FKTABLE_SCHEM'));
        Result.UpdateStringByName('FKTABLE_NAME',
          GetStringByName('FKTABLE_NAME'));
        Result.UpdateStringByName('FKCOLUMN_NAME',
          GetStringByName('FKCOLUMN_NAME'));
        Result.UpdateShortByName('KEY_SEQ',
          GetShortByName('KEY_SEQ'));
        Result.UpdateShortByName('UPDATE_RULE',
          GetShortByName('UPDATE_RULE'));
        Result.UpdateShortByName('DELETE_RULE',
          GetShortByName('DELETE_RULE'));
        Result.UpdateStringByName('FK_NAME',
          GetStringByName('FK_NAME'));
        Result.UpdateStringByName('PK_NAME',
          GetStringByName('PK_NAME'));
        Result.UpdateIntByName('DEFERRABILITY',
          GetIntByName('DEFERRABILITY'));
        Result.InsertRow;
      end;
      Close;
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
function TZSybaseDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(ExportedKeyColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_exportkey %s, %s, %s',
      [AQSNull(Catalog), AQSNull(Schema), AQSNull(Table)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('PKTABLE_CAT',
          GetStringByName('PKTABLE_CAT'));
        Result.UpdateStringByName('PKTABLE_SCHEM',
          GetStringByName('PKTABLE_SCHEM'));
        Result.UpdateStringByName('PKTABLE_NAME',
          GetStringByName('PKTABLE_NAME'));
        Result.UpdateStringByName('PKCOLUMN_NAME',
          GetStringByName('PKCOLUMN_NAME'));
        Result.UpdateStringByName('FKTABLE_CAT',
          GetStringByName('FKTABLE_CAT'));
        Result.UpdateStringByName('FKTABLE_SCHEM',
          GetStringByName('FKTABLE_SCHEM'));
        Result.UpdateStringByName('FKTABLE_NAME',
          GetStringByName('FKTABLE_NAME'));
        Result.UpdateStringByName('FKCOLUMN_NAME',
          GetStringByName('FKCOLUMN_NAME'));
        Result.UpdateShortByName('KEY_SEQ',
          GetShortByName('KEY_SEQ'));
        Result.UpdateShortByName('UPDATE_RULE',
          GetShortByName('UPDATE_RULE'));
        Result.UpdateShortByName('DELETE_RULE',
          GetShortByName('DELETE_RULE'));
        Result.UpdateStringByName('FK_NAME',
          GetStringByName('FK_NAME'));
        Result.UpdateStringByName('PK_NAME',
          GetStringByName('PK_NAME'));
        Result.UpdateIntByName('DEFERRABILITY',
          GetIntByName('DEFERRABILITY'));
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
function TZSybaseDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
begin
    Result := ConstructVirtualResultSet(CrossRefColumnsDynArray);

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_getcrossreferences %s, %s, %s, %s, %s, %s',
      [AQSNull(PrimaryCatalog), AQSNull(PrimarySchema), AQSNull(PrimaryTable),
       AQSNull(ForeignCatalog), AQSNull(ForeignSchema), AQSNull(ForeignTable)])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('PKTABLE_CAT',
          GetStringByName('PKTABLE_CAT'));
        Result.UpdateStringByName('PKTABLE_SCHEM',
          GetStringByName('PKTABLE_SCHEM'));
        Result.UpdateStringByName('PKTABLE_NAME',
          GetStringByName('PKTABLE_NAME'));
        Result.UpdateStringByName('PKCOLUMN_NAME',
          GetStringByName('PKCOLUMN_NAME'));
        Result.UpdateStringByName('FKTABLE_CAT',
          GetStringByName('FKTABLE_CAT'));
        Result.UpdateStringByName('FKTABLE_SCHEM',
          GetStringByName('FKTABLE_SCHEM'));
        Result.UpdateStringByName('FKTABLE_NAME',
          GetStringByName('FKTABLE_NAME'));
        Result.UpdateStringByName('FKCOLUMN_NAME',
          GetStringByName('FKCOLUMN_NAME'));
        Result.UpdateShortByName('KEY_SEQ',
          GetShortByName('KEY_SEQ'));
        Result.UpdateShortByName('UPDATE_RULE',
          GetShortByName('UPDATE_RULE'));
        Result.UpdateShortByName('DELETE_RULE',
          GetShortByName('DELETE_RULE'));
        Result.UpdateStringByName('FK_NAME',
          GetStringByName('FK_NAME'));
        Result.UpdateStringByName('PK_NAME',
          GetStringByName('PK_NAME'));
        Result.UpdateIntByName('DEFERRABILITY',
          GetIntByName('DEFERRABILITY'));
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
function TZSybaseDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
begin
    Result := ConstructVirtualResultSet(TypeInfoColumnsDynArray);

    with GetStatement.ExecuteQuery('exec sp_jdbc_datatype_info') do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TYPE_NAME',
          GetStringByName('TYPE_NAME'));
        Result.UpdateShortByName('DATA_TYPE',
          Ord(ConvertODBCToSqlType(GetShortByName('DATA_TYPE'))));
        Result.UpdateIntByName('PRECISION',
          GetIntByName('PRECISION'));
        Result.UpdateStringByName('LITERAL_PREFIX',
          GetStringByName('LITERAL_PREFIX'));
        Result.UpdateStringByName('LITERAL_SUFFIX',
          GetStringByName('LITERAL_SUFFIX'));
        Result.UpdateStringByName('CREATE_PARAMS',
          GetStringByName('CREATE_PARAMS'));
        Result.UpdateShortByName('NULLABLE',
          GetShortByName('NULLABLE'));
        Result.UpdateBooleanByName('CASE_SENSITIVE',
          GetShortByName('CASE_SENSITIVE') = 1);
        Result.UpdateShortByName('SEARCHABLE',
          GetShortByName('SEARCHABLE'));
        Result.UpdateBooleanByName('UNSIGNED_ATTRIBUTE',
          GetShortByName('UNSIGNED_ATTRIBUTE') = 1);
        Result.UpdateBooleanByName('FIXED_PREC_SCALE',
          GetShortByName('FIXED_PREC_SCALE') = 1);
        Result.UpdateBooleanByName('AUTO_INCREMENT',
          GetShortByName('AUTO_INCREMENT') = 1);
        Result.UpdateStringByName('LOCAL_TYPE_NAME',
          GetStringByName('LOCAL_TYPE_NAME'));
        Result.UpdateShortByName('MINIMUM_SCALE',
          GetShortByName('MINIMUM_SCALE'));
        Result.UpdateShortByName('MAXIMUM_SCALE',
          GetShortByName('MAXIMUM_SCALE'));
        Result.UpdateShortByName('SQL_DATA_TYPE',
          GetShortByName('SQL_DATA_TYPE'));
        Result.UpdateShortByName('SQL_DATETIME_SUB',
          GetShortByName('SQL_DATETIME_SUB'));
        Result.UpdateShortByName('NUM_PREC_RADIX',
          GetShortByName('NUM_PREC_RADIX'));
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
function TZSybaseDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  Is_Unique, Accuracy: string;
begin
    Result := ConstructVirtualResultSet(IndexInfoColumnsDynArray);

    if Unique then
      Is_Unique := '''1'''
    else Is_Unique := '''0''';
    if Approximate then
      Accuracy := '''1'''
    else Accuracy := '''0''';

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_getindexinfo %s, %s, %s, %s, %s',
      [AQSNull(Catalog), AQSNull(Schema), AQSNull(Table), Is_Unique, Accuracy])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TABLE_CAT',
          GetStringByName('TABLE_CAT'));
        Result.UpdateStringByName('TABLE_SCHEM',
          GetStringByName('TABLE_SCHEM'));
        Result.UpdateStringByName('TABLE_NAME',
          GetStringByName('TABLE_NAME'));
        Result.UpdateBooleanByName('NON_UNIQUE',
          GetShortByName('NON_UNIQUE') = 1);
        Result.UpdateStringByName('INDEX_QUALIFIER',
          GetStringByName('INDEX_QUALIFIER'));
        Result.UpdateStringByName('INDEX_NAME',
          GetStringByName('INDEX_NAME'));
        Result.UpdateShortByName('TYPE',
          GetShortByName('TYPE'));
        Result.UpdateShortByName('ORDINAL_POSITION',
          GetShortByName('ORDINAL_POSITION'));
        Result.UpdateStringByName('COLUMN_NAME',
          GetStringByName('COLUMN_NAME'));
        Result.UpdateStringByName('ASC_OR_DESC',
          GetStringByName('ASC_OR_DESC'));
        Result.UpdateIntByName('CARDINALITY',
          GetIntByName('CARDINALITY'));
        Result.UpdateIntByName('PAGES',
          GetIntByName('PAGES'));
        Result.UpdateStringByName('FILTER_CONDITION',
          GetStringByName('FILTER_CONDITION'));
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSybaseDatabaseMetadata.SupportsResultSetType(
  _Type: TZResultSetType): Boolean;
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
function TZSybaseDatabaseMetadata.SupportsResultSetConcurrency(
  _Type: TZResultSetType; Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := True;
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
function TZSybaseDatabaseMetadata.UncachedGetUDTs(const Catalog: string;
  const SchemaPattern: string; const TypeNamePattern: string;
  const Types: TIntegerDynArray): IZResultSet;
var
  I: Integer;
  UDTypes: string;
begin
    Result := ConstructVirtualResultSet(UDTColumnsDynArray);

    UDTypes := '';
    for I := 0 to Length(Types) - 1 do
    begin
      if Length(UDTypes) > 0 then
        UDTypes := UDTypes + ',';
      UDTypes := UDTypes + AnsiQuotedStr(IntToStr(Types[I]), '''');
    end;

    with GetStatement.ExecuteQuery(
      Format('exec sp_jdbc_getudts %s, %s, %s, %s',
      [AQSNull(Catalog), AQSNullText(SchemaPattern, '''%'''),
       AQSNullText(TypeNamePattern, '''%'''), AQSNull(UDTypes, '"')])) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateStringByName('TYPE_CAT',
          GetStringByName('TYPE_CAT'));
        Result.UpdateStringByName('TYPE_SCHEM',
          GetStringByName('TYPE_SCHEM'));
        Result.UpdateStringByName('TYPE_NAME',
          GetStringByName('TYPE_NAME'));
        Result.UpdateStringByName('JAVA_CLASS',
          GetStringByName('JAVA_CLASS'));
        Result.UpdateShortByName('DATA_TYPE',
          Ord(ConvertODBCToSqlType(GetShortByName('DATA_TYPE'))));
        Result.UpdateStringByName('REMARKS',
          GetStringByName('REMARKS'));
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Create a statement for use

  @return TZDBLibStatement
}
function TZSybaseDatabaseMetadata.GetStatement: IZSTatement;
begin
  Result := GetConnection.CreateStatement;
end;

end.


