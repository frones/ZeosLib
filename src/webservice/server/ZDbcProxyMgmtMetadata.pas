{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
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

unit ZDbcProxyMgmtMetadata;

interface

{$I ../../Zeos.inc}

uses
  Types, Classes, SysUtils,
  ZDbcIntfs, ZDbcMetadata, ZCompatibility, ZSelectSchema, ZDbcCachedResultSet;

type

  {** Implements DBC Layer Proxy driver Database Information. }
  TZProxyMgmtDatabaseInfo = class(TZAbstractDatabaseInfo)
  protected
    // database/driver/server info:
    FDatabaseProductName: string;
    FDatabaseProductVersion: string;
    FDriverName: string;
    FDriverVersion: string;
    FDriverMajorVersion: Integer;
    FDriverMinorVersion: Integer;
    FServerVersion: string;

  public
    constructor Create(const Metadata: TZAbstractDatabaseMetadata; const PropertyList: String); virtual;
    destructor Destroy; override;

    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
    function GetDriverVersion: string; override;
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
    function GetServerVersion: string; override;
  end;

  IZProxyMgmtDatabaseMetadata = Interface(IZDatabaseMetadata)
    ['{149B3E07-E31C-41CB-BA08-E10D12BC728D}']
    function ConstructVirtualResultSet(ColumnsDefs: TZMetadataColumnDefs):
      IZVirtualResultSet;
  End;

  {** Implements PostgreSQL Database Metadata. }
  TZProxyMgmtDatabaseMetadata = class(TZAbstractDatabaseMetadata, IZProxyMgmtDatabaseMetadata)
  private
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-27
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
  public
    function ConstructVirtualResultSet(ColumnsDefs: TZMetadataColumnDefs):
      IZVirtualResultSet; override;
    destructor Destroy; override;
 end;

implementation

uses
  TypInfo,
  ZFastCode, ZMessages, ZSysUtils, ZPlainProxyDriverIntf, ZDbcProxyMgmtDriver, ZDbcProxyResultSet;

{ TZProxyDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZProxyMgmtDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata; const PropertyList: String);
var
  Temp: String;
begin
  inherited Create(Metadata);

  // database/driver/server info:
  FDatabaseProductName := 'Zeos Webservice Proxy';
  FDatabaseProductVersion := 'Beta';
  FDriverName := 'WebServiceProxyManagement';
  FDriverVersion := '0.0.1';
  FDriverMajorVersion := 0;
  FDriverMinorVersion := 0;
  FServerVersion := '0.0.1';
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZProxyMgmtDatabaseInfo.Destroy;
begin
  inherited;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What
  's the name of this database product?
  @return database product name
}
function TZProxyMgmtDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := FDatabaseProductName;
end;

{**
  What's the version of this database product?
  @return database version
}
function TZProxyMgmtDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := FDatabaseProductVersion;
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZProxyMgmtDatabaseInfo.GetDriverName: string;
begin
  Result := FDriverName;
end;

function TZProxyMgmtDatabaseInfo.GetDriverVersion: string;
begin
  Result := FDriverVersion;
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZProxyMgmtDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := FDriverMajorVersion;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZProxyMgmtDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := FDriverMinorVersion;
end;

{**
  Returns the server version
  @return the server version string
}
function TZProxyMgmtDatabaseInfo.GetServerVersion: string;
begin
  Result := FServerVersion;
end;


{ TZPostgreSQLDatabaseMetadata }


{**
  Destroys this object and cleanups the memory.
}
destructor TZProxyMgmtDatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZProxyMgmtDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZProxyMgmtDatabaseInfo.Create(Self, (GetConnection as IZDbcProxyMgmtConnection).GetDbInfoStr);
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
function TZProxyMgmtDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  Res: WideString;
begin
  Result := inherited;
  Result.MoveToInsertRow;
  Result.UpdateAnsiString(FirstDbcIndex, '');
  Result.UpdateAnsiString(FirstDbcIndex+1, '');
  Result.UpdateAnsiString(FirstDbcIndex+2, 'ATTACHMENTS');
  Result.UpdateAnsiString(FirstDbcIndex+3, 'SYSTEM TABLE');
  Result.UpdateAnsiString(FirstDbcIndex+4, 'List of available attachments.');
  Result.InsertRow;
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
function TZProxyMgmtDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
begin
  Result := inherited;
  Result.MoveToInsertRow;
  Result.UpdateAnsiString(FirstDbcIndex, 'SYSTEM TABLE');
  Result.InsertRow;
  Result.MoveAbsolute(0);
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
function TZProxyMgmtDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  MyTableName: String;
begin
  MyTableName := UpperCase(TableNamePattern);
  Result := inherited;
  if (Catalog = '') and (SchemaPattern = '') and (TableNamePattern = 'ATTACHMENTS') then begin
    // ID Field
    Result.MoveToInsertRow;
    Result.UpdateNullByName('TABLE_CAT');
    Result.UpdateNullByName('TABLE_SCHEM');
    Result.UpdateAnsiStringByName('TABLE_NAME', MyTableName);
    Result.UpdateAnsiStringByName('COLUMN_NAME', 'ID');
    Result.UpdateShortByName('DATA_TYPE', ord(stString));
    Result.UpdateAnsiStringByName('TYPE_NAME', 'VARCHAR');
    Result.UpdateIntByName('COLUMN_SIZE', 25);
    Result.UpdateNullByName('BUFFER_LENGTH');
    Result.UpdateNullByName('DECIMAL_DIGITS');
    Result.UpdateNullByName('NUM_PREC_RADIX');
    Result.UpdateIntByName('NULLABLE', 0{ord(columnNoNulls)});
    Result.UpdateNullByName('REMARKS');
    Result.UpdateNullByName('COLUMN_DEF');
    Result.UpdateNullByName('SQL_DATA_TYPE');
    Result.UpdateNullByName('SQL_DATETIME_SUB');
    Result.UpdateIntByName('CHAR_OCTET_LENGTH', 4 * 25);
    Result.UpdateIntByName('ORDINAL_POSITION', 1);
    Result.UpdateAnsiStringByName('IS_NULLABLE', 'NO');
    Result.InsertRow;
    // Database Name
    Result.MoveToInsertRow;
    Result.UpdateNullByName('TABLE_CAT');
    Result.UpdateNullByName('TABLE_SCHEM');
    Result.UpdateAnsiStringByName('TABLE_NAME', MyTableName);
    Result.UpdateAnsiStringByName('COLUMN_NAME', 'DATABASE_NAME');
    Result.UpdateShortByName('DATA_TYPE', ord(stString));
    Result.UpdateAnsiStringByName('TYPE_NAME', 'VARCHAR');
    Result.UpdateIntByName('COLUMN_SIZE', 25);
    Result.UpdateNullByName('BUFFER_LENGTH');
    Result.UpdateNullByName('DECIMAL_DIGITS');
    Result.UpdateNullByName('NUM_PREC_RADIX');
    Result.UpdateIntByName('NULLABLE', 0{ord(columnNoNulls)});
    Result.UpdateNullByName('REMARKS');
    Result.UpdateNullByName('COLUMN_DEF');
    Result.UpdateNullByName('SQL_DATA_TYPE');
    Result.UpdateNullByName('SQL_DATETIME_SUB');
    Result.UpdateIntByName('CHAR_OCTET_LENGTH', 4 * 25);
    Result.UpdateIntByName('ORDINAL_POSITION', 2);
    Result.UpdateAnsiStringByName('IS_NULLABLE', 'NO');
    Result.InsertRow;
    // User Name
    Result.MoveToInsertRow;
    Result.UpdateNullByName('TABLE_CAT');
    Result.UpdateNullByName('TABLE_SCHEM');
    Result.UpdateAnsiStringByName('TABLE_NAME', MyTableName);
    Result.UpdateAnsiStringByName('COLUMN_NAME', 'USER_NAME');
    Result.UpdateShortByName('DATA_TYPE', ord(stString));
    Result.UpdateAnsiStringByName('TYPE_NAME', 'VARCHAR');
    Result.UpdateIntByName('COLUMN_SIZE', 25);
    Result.UpdateNullByName('BUFFER_LENGTH');
    Result.UpdateNullByName('DECIMAL_DIGITS');
    Result.UpdateNullByName('NUM_PREC_RADIX');
    Result.UpdateIntByName('NULLABLE', 0{ord(columnNoNulls)});
    Result.UpdateNullByName('REMARKS');
    Result.UpdateNullByName('COLUMN_DEF');
    Result.UpdateNullByName('SQL_DATA_TYPE');
    Result.UpdateNullByName('SQL_DATETIME_SUB');
    Result.UpdateIntByName('CHAR_OCTET_LENGTH', 4 * 25);
    Result.UpdateIntByName('ORDINAL_POSITION', 3);
    Result.UpdateAnsiStringByName('IS_NULLABLE', 'NO');
    Result.InsertRow;
    // Creation Time
    Result.MoveToInsertRow;
    Result.UpdateNullByName('TABLE_CAT');
    Result.UpdateNullByName('TABLE_SCHEM');
    Result.UpdateAnsiStringByName('TABLE_NAME', MyTableName);
    Result.UpdateAnsiStringByName('COLUMN_NAME', 'CREATED');
    Result.UpdateShortByName('DATA_TYPE', ord(stTimestamp));
    Result.UpdateAnsiStringByName('TYPE_NAME', 'DATETIME');
    Result.UpdateNullByName('COLUMN_SIZE');
    Result.UpdateNullByName('BUFFER_LENGTH');
    Result.UpdateNullByName('DECIMAL_DIGITS');
    Result.UpdateNullByName('NUM_PREC_RADIX');
    Result.UpdateIntByName('NULLABLE', 0{ord(columnNoNulls)});
    Result.UpdateNullByName('REMARKS');
    Result.UpdateNullByName('COLUMN_DEF');
    Result.UpdateNullByName('SQL_DATA_TYPE');
    Result.UpdateNullByName('SQL_DATETIME_SUB');
    Result.UpdateNullByName('CHAR_OCTET_LENGTH');
    Result.UpdateIntByName('ORDINAL_POSITION', 4);
    Result.UpdateAnsiStringByName('IS_NULLABLE', 'NO');
    Result.InsertRow;
    // Creation Time
    Result.MoveToInsertRow;
    Result.UpdateNullByName('TABLE_CAT');
    Result.UpdateNullByName('TABLE_SCHEM');
    Result.UpdateAnsiStringByName('TABLE_NAME', MyTableName);
    Result.UpdateAnsiStringByName('COLUMN_NAME', 'LAST_ACCESS');
    Result.UpdateShortByName('DATA_TYPE', ord(stTimestamp));
    Result.UpdateAnsiStringByName('TYPE_NAME', 'DATETIME');
    Result.UpdateNullByName('COLUMN_SIZE');
    Result.UpdateNullByName('BUFFER_LENGTH');
    Result.UpdateNullByName('DECIMAL_DIGITS');
    Result.UpdateNullByName('NUM_PREC_RADIX');
    Result.UpdateIntByName('NULLABLE', 0{ord(columnNoNulls)});
    Result.UpdateNullByName('REMARKS');
    Result.UpdateNullByName('COLUMN_DEF');
    Result.UpdateNullByName('SQL_DATA_TYPE');
    Result.UpdateNullByName('SQL_DATETIME_SUB');
    Result.UpdateNullByName('CHAR_OCTET_LENGTH');
    Result.UpdateIntByName('ORDINAL_POSITION', 5);
    Result.UpdateAnsiStringByName('IS_NULLABLE', 'NO');
    Result.InsertRow;
  end;
end;

function TZProxyMgmtDatabaseMetadata.ConstructVirtualResultSet(ColumnsDefs: TZMetadataColumnDefs):
  IZVirtualResultSet;
begin
  Result := inherited;
end;

end.
