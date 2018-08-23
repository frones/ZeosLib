{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Ado metadata information                  }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcAdoMetadata;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcMetadata, ZDbcResultSet, ZURL,
  ZCompatibility, ZGenericSqlAnalyser, ZPlainAdo, ZDbcConnection,
  ZOleDB, ActiveX;

type
  {** Implements Ado Metadata. }
  TZAdoDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    FAdoConnection: ZPlainAdo.Connection;
    FSupportedSchemasInitialized: Boolean;
    function AdoOpenSchema(Schema: Integer; const Args: array of const): ZPlainAdo.RecordSet;
    procedure InitializeSchemas;
    function SchemaSupported(SchemaId: Integer): Boolean; // (technobot) should be moved to TZAdoDatabaseInfo?
    function FindSchema(SchemaId: Integer): Integer;
    function BuildRestrictions(SchemaId: Integer; const Args: array of const): Variant;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-27

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
    constructor Create(Connection: TZAbstractDbcConnection; const Url: TZURL); override;
//    function GetTokenizer: IZTokenizer; override;
  end;

implementation

uses
  Variants,
  Math, ZGenericSqlToken, ZDbcAdoUtils, ZDbcAdo, ZFastCode,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ZDbcAdoResultSet, ZDbcOleDBMetadata;

type
  TSuppSchemaRec = record
    SchemaGuid: TGuid;
    SupportedRestrictions: Integer;
    AdoSchemaId: Integer;
  end;

  IDBSchemaRowset = interface(IUnknown)
    ['{0c733a7b-2a1c-11ce-ade5-00aa0044773d}']
    function GetRowset(
            const pUnkOuter : IUnknown;
            const rguidSchema : TGUID;
            cRestrictions : Integer;
            var rgRestrictions : PVariant;{!!was: const VARIANT __RPC_FAR rgRestrictions[  ],}
            const riid : IUnknown;
            cPropertySets : Integer;
            var rgPropertySets : TDBPROPSET;
            var ppRowset : IUnknown) : HResult; stdcall;
      function GetSchemas(
            var pcSchemas : Integer;
            var prgSchemas : PGUID;
            var prgRestrictionSupport : PInteger) : HResult; stdcall;
    end;

var
  SupportedSchemas: array of TSuppSchemaRec;

{ TZAdoDatabaseMetadata }


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZAdoDatabaseMetadata.Create(Connection: TZAbstractDbcConnection;
  const Url: TZURL);
begin
  inherited Create(Connection, Url);
  FAdoConnection := nil;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZAdoDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZOleDBDatabaseInfo.Create(Self);
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
function TZAdoDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaProcedures,
    [Catalog, SchemaPattern, ProcedureNamePattern, '']);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('PROCEDURE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('PROCEDURE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(ProcedureNameIndex, GetPWideCharByName('PROCEDURE_NAME', Len), @Len);
        Result.UpdatePWideChar(ProcedureRemarksIndex, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.UpdateSmall(ProcedureTypeIndex, GetSmallByName('PROCEDURE_TYPE') - 1);
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaProcedureParameters,
    [Catalog, SchemaPattern, ProcedureNamePattern]);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('PROCEDURE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('PROCEDURE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(ProcColProcedureNameIndex, GetPWideCharByName('PROCEDURE_NAME', Len), @Len);
        Result.UpdatePWideChar(ProcColColumnNameIndex, GetPWideCharByName('PARAMETER_NAME', Len), @Len);
        case GetSmallByName('PARAMETER_TYPE') of
          1: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctIn));
          2: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
          3: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctOut));
          4: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctReturn));
        else
          Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
        end;
        Result.UpdateSmall(ProcColDataTypeIndex, Ord(ConvertAdoToSqlType(
          GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
        Result.UpdatePWideChar(ProcColTypeNameIndex, GetPWideCharByName('TYPE_NAME', Len), @Len);
        Result.UpdateInt(ProcColPrecisionIndex, GetIntByName('NUMERIC_PRECISION'));
        Result.UpdateInt(ProcColLengthIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateSmall(ProcColScaleIndex, GetSmallByName('NUMERIC_SCALE'));
  //      Result.UpdateSmall(ProcColRadixIndex, GetSmallByName('RADIX'));
        if GetStringByName('IS_NULLABLE') = 'NO' then
          Result.UpdateSmall(ProcColNullableIndex, 0)
        else
          if GetStringByName('IS_NULLABLE') = 'YES' then
            Result.UpdateSmall(ProcColNullableIndex, 1)
          else
            Result.UpdateSmall(ProcColNullableIndex, 2);
        Result.UpdatePWideChar(ProcColRemarksIndex, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  TableTypes: string;
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

  TableTypes := '';
  for I := Low(Types) to High(Types) do
  begin
    if Length(TableTypes) > 0 then
      TableTypes := TableTypes + ',';
    TableTypes := TableTypes + Types[I];
  end;

  AdoRecordSet := AdoOpenSchema(adSchemaTables,
    [Catalog, SchemaPattern, TableNamePattern, TableTypes]);
  if Assigned(AdoRecordSet) then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordset) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(TableColumnsSQLType, GetPWideCharByName('TABLE_TYPE', Len), @Len);
        Result.UpdatePWideChar(TableColumnsRemarks, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetSchemas: IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result := inherited UncachedGetSchemas;

  AdoRecordSet := AdoOpenSchema(adSchemaSchemata, []);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(SchemaColumnsTableSchemaIndex,
          GetPWideCharByName('SCHEMA_NAME', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetCatalogs;

  AdoRecordSet := AdoOpenSchema(adSchemaCatalogs, []);
  if Assigned(AdoRecordSet) then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('CATALOG_NAME', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypes: array[0..7] of ZWideString = (
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
function TZAdoDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Flags: Integer;
  SQLType: TZSQLType;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaColumns,
    [DecomposeObjectString(Catalog), DecomposeObjectString(SchemaPattern),
    DecomposeObjectString(TableNamePattern), DecomposeObjectString(ColumnNamePattern)]);
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet.Sort := 'ORDINAL_POSITION';
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);

        SQLType := ConvertAdoToSqlType(GetSmallByName('DATA_TYPE'),
          ConSettings.CPType);
        Flags := GetIntByName('COLUMN_FLAGS');
//!!!If the field type is long then this is the only way to know it because it just returns string type
        if ((Flags and DBCOLUMNFLAGS_ISLONG) <> 0 ) and (SQLType in [stBytes, stString, stUnicodeString]) then
          case SQLType of
            stBytes: SQLType := stBinaryStream;
            stString: SQLType := stAsciiStream;
            stUnicodeString: SQLType := stUnicodeStream;
          end;
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(SQLType));
        Result.UpdateInt(TableColColumnSizeIndex, GetIntByName('CHARACTER_MAXIMUM_LENGTH'));
        Result.UpdateInt(TableColColumnBufLengthIndex, GetIntByName('CHARACTER_MAXIMUM_LENGTH'));
        Result.UpdateInt(TableColColumnDecimalDigitsIndex, GetIntByName('NUMERIC_SCALE'));
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, GetSmallByName('NUMERIC_PRECISION'));
        if GetBooleanByName('IS_NULLABLE') then
          Result.UpdateSmall(TableColColumnNullableIndex, 1)
        else
          Result.UpdateSmall(TableColColumnNullableIndex, 0);
        Result.UpdatePWideChar(TableColColumnRemarksIndex, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.UpdatePWideChar(TableColColumnColDefIndex, GetPWideCharByName('COLUMN_DEFAULT', Len), @Len);
        Result.UpdateSmall(TableColColumnSQLDataTypeIndex, GetSmallByName('DATETIME_PRECISION'));
        Result.UpdateInt(TableColColumnCharOctetLengthIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateInt(TableColColumnOrdPosIndex, GetIntByName('ORDINAL_POSITION'));
        if UpperCase(GetStringByName('IS_NULLABLE')) = 'FALSE' then
          Result.UpdateString(TableColColumnIsNullableIndex, 'NO')
        else
          Result.UpdateString(TableColColumnIsNullableIndex, 'YES');

        //Result.UpdateNullByName(TableColColumnAutoIncIndex);
        Result.UpdateBoolean(TableColColumnSearchableIndex, (Flags and (DBCOLUMNFLAGS_ISLONG) = 0));
        Result.UpdateBoolean(TableColColumnWritableIndex, (Flags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0));
        Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, (Flags and (DBCOLUMNFLAGS_WRITE) <> 0));
        Result.UpdateBoolean(TableColColumnReadonlyIndex, (Flags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0));
        Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, IC.IsCaseSensitive(GetStringByName('COLUMN_NAME')));
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaColumnPrivileges,
    [Catalog, Schema, Table, ColumnNamePattern]);
  if Assigned(AdoRecordSet) then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
        Result.UpdatePWideChar(TableColPrivGrantorIndex, GetPWideCharByName('GRANTOR', Len), @Len);
        Result.UpdatePWideChar(TableColPrivGranteeIndex, GetPWideCharByName('GRANTEE', Len), @Len);
        Result.UpdatePWideChar(TableColPrivPrivilegeIndex, GetPWideCharByName('PRIVILEGE_TYPE', Len), @Len);
        if GetBooleanByName('IS_GRANTABLE') then
          Result.UpdateString(TableColPrivIsGrantableIndex, 'YES')
        else
          Result.UpdateString(TableColPrivIsGrantableIndex, 'NO');
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaTablePrivileges,
    [Catalog, SchemaPattern, TableNamePattern]);
  if Assigned(AdoRecordSet) then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(TablePrivGrantorIndex, GetPWideCharByName('GRANTOR', Len), @Len);
        Result.UpdatePWideChar(TablePrivGranteeIndex, GetPWideCharByName('GRANTEE', Len), @Len);
        Result.UpdatePWideChar(TablePrivPrivilegeIndex, GetPWideCharByName('PRIVILEGE_TYPE', Len), @Len);
        if GetBooleanByName('IS_GRANTABLE') then
          Result.UpdateString(TablePrivIsGrantableIndex, 'YES')
        else
          Result.UpdateString(TablePrivIsGrantableIndex, 'NO');
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
const
  DBCOLUMNFLAGS_ISROWVER = $00000200;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetVersionColumns(Catalog, Schema, Table);

  AdoRecordSet := AdoOpenSchema(adSchemaColumns, [Catalog, Schema, Table]);
  if Assigned(AdoRecordSet) then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        if (GetIntByName('COLUMN_FLAGS')
          and DBCOLUMNFLAGS_ISROWVER) = 0 then
          Continue;
        Result.MoveToInsertRow;
        Result.UpdateSmall(TableColVerScopeIndex, 0);
        Result.UpdatePWideChar(TableColVerColNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
        Result.UpdateSmall(TableColVerDataTypeIndex, Ord(ConvertAdoToSqlType(
          GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
        Result.UpdatePWideChar(TableColVerTypeNameIndex, GetPWideCharByName('TYPE_NAME', Len), @Len);
        Result.UpdateInt(TableColVerColSizeIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateInt(TableColVerBufLengthIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateInt(TableColVerDecimalDigitsIndex, GetIntByName('NUMERIC_SCALE'));
        Result.UpdateSmall(TableColVerPseudoColumnIndex, 0);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);

  AdoRecordSet := AdoOpenSchema(adSchemaPrimaryKeys,
    [Catalog, Schema, Table]);
  if AdoRecordSet <> nil then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(PrimaryKeyColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
        Result.UpdateSmall(PrimaryKeyKeySeqIndex, GetSmallByName('ORDINAL'));
        if FindColumn('PK_NAME') > FirstDbcIndex then
          Result.UpdatePWideChar(PrimaryKeyPKNameIndex, GetPWideCharByName('PK_NAME', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference('', '', '', Catalog, Schema, Table);
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
function TZAdoDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference(Catalog, Schema, Table, '', '', '');
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
function TZAdoDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;

  function GetRuleType(const Rule: String): TZImportedKey;
  begin
    if Rule = 'RESTRICT' then
      Result := ikRestrict
    else if Rule = 'NO ACTION' then
      Result := ikNoAction
    else if Rule = 'CASCADE' then
      Result := ikCascade
    else if Rule = 'SET DEFAULT' then
      Result := ikSetDefault
    else if Rule = 'SET NULL' then
      Result := ikSetNull
    else
      Result := ikNotDeferrable; //impossible!
  end;
begin
  Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                              ForeignCatalog, ForeignSchema, ForeignTable);

  AdoRecordSet := AdoOpenSchema(adSchemaForeignKeys, [PrimaryCatalog,
    PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable]);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CrossRefKeyColPKTableCatalogIndex, GetPWideCharByName('PK_TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKTableSchemaIndex, GetPWideCharByName('PK_TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKTableNameIndex, GetPWideCharByName('PK_TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKColumnNameIndex, GetPWideCharByName('PK_COLUMN_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableCatalogIndex, GetPWideCharByName('FK_TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableSchemaIndex, GetPWideCharByName('FK_TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableNameIndex, GetPWideCharByName('FK_TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKColumnNameIndex, GetPWideCharByName('FK_COLUMN_NAME', Len), @Len);
        Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmallByName('ORDINAL'));
        Result.UpdateSmall(CrossRefKeyColUpdateRuleIndex, Ord(GetRuleType(GetStringByName('UPDATE_RULE'))));
        Result.UpdateSmall(CrossRefKeyColDeleteRuleIndex, Ord(GetRuleType(GetStringByName('DELETE_RULE'))));
        Result.UpdatePWideChar(CrossRefKeyColFKNameIndex, GetPWideCharByName('FK_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKNameIndex, GetPWideCharByName('PK_NAME', Len), @Len);
        Result.UpdateInt(CrossRefKeyColDeferrabilityIndex, GetSmallByName('DEFERRABILITY'));
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetTypeInfo;

  AdoRecordSet := AdoOpenSchema(adSchemaProviderTypes, []);
  if AdoRecordSet <> nil then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(TypeInfoTypeNameIndex, GetPWideCharByName('TYPE_NAME', Len), @Len);
        Result.UpdateSmall(TypeInfoDataTypeIndex, Ord(ConvertAdoToSqlType(
          GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
        Result.UpdateInt(TypeInfoPecisionIndex, 0);//GetIntByName('PRECISION'));
        Result.UpdatePWideChar(TypeInfoLiteralPrefixIndex, GetPWideCharByName('LITERAL_PREFIX', Len), @Len);
        Result.UpdatePWideChar(TypeInfoLiteralSuffixIndex, GetPWideCharByName('LITERAL_SUFFIX', Len), @Len);
        Result.UpdatePWideChar(TypeInfoCreateParamsIndex, GetPWideCharByName('CREATE_PARAMS', Len), @Len);
        if GetBooleanByName('IS_NULLABLE') then
          Result.UpdateSmall(TypeInfoNullAbleIndex, 1)
        else
          Result.UpdateSmall(TypeInfoNullAbleIndex, 0);
        Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, GetBooleanByName('CASE_SENSITIVE'));
        Result.UpdateSmall(TypeInfoSearchableIndex, GetSmallByName('SEARCHABLE'));
        Result.UpdateBoolean(TypeInfoUnsignedAttributeIndex, GetBooleanByName('UNSIGNED_ATTRIBUTE'));
        Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, GetBooleanByName('FIXED_PREC_SCALE'));
        Result.UpdateBoolean(TypeInfoAutoIncrementIndex, False);
        Result.UpdatePWideChar(TypeInfoLocaleTypeNameIndex, GetPWideCharByName('LOCAL_TYPE_NAME', Len), @Len);
        Result.UpdateSmall(TypeInfoMinimumScaleIndex, GetSmallByName('MINIMUM_SCALE'));
        Result.UpdateSmall(TypeInfoMaximumScaleIndex, GetSmallByName('MAXIMUM_SCALE'));
  //      Result.UpdateSmall(TypeInfoSQLDataTypeIndex, GetSmallByName('SQL_DATA_TYPE'));
  //      Result.UpdateSmall(TypeInfoSQLDateTimeSubIndex, GetSmallByName('SQL_DATETIME_SUB'));
  //      Result.UpdateSmall(TypeInfoNumPrecRadix, GetSmallByName('NUM_PREC_RADIX'));
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  AdoRecordSet := AdoOpenSchema(adSchemaIndexes,
    [Catalog, Schema, '', '', Table]);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdateBoolean(IndexInfoColNonUniqueIndex, not GetBooleanByName('UNIQUE'));
        Result.UpdatePWideChar(IndexInfoColIndexQualifierIndex, GetPWideCharByName('INDEX_CATALOG', Len), @Len);
        Result.UpdatePWideChar(IndexInfoColIndexNameIndex, GetPWideCharByName('INDEX_NAME', Len), @Len);
        Result.UpdateSmall(IndexInfoColTypeIndex, GetSmallByName('TYPE'));
        Result.UpdateSmall(IndexInfoColOrdPositionIndex, GetSmallByName('ORDINAL_POSITION'));
        Result.UpdatePWideChar(IndexInfoColColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
  //!!!      Result.UpdatePWideChar(IndexInfoColAscOrDescIndex, GetPWideCharByName('COLLATION', Len), @Len);
        Result.UpdateInt(IndexInfoColCardinalityIndex, GetIntByName('CARDINALITY'));
        Result.UpdateInt(IndexInfoColPagesIndex, GetIntByName('PAGES'));
        Result.UpdatePWideChar(IndexInfoColFilterConditionIndex, GetPWideCharByName('FILTER_CONDITION', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
  const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet;
begin

  Result:=inherited UncachedGetUDTs(Catalog, SchemaPattern, TypeNamePattern, Types);

//  AdoRecordSet := AdoOpenSchema(adSchemaIndexes, Restrictions);
//  if Assigned(AdoRecordSet) then
//  with GetStatement.ExecuteQuery(
//    Format('select TYPE_CAT = db_name(), TYPE_SCHEM = user_name(uid),'
//      + ' TYPE_NAME = st.name, DATA_TYPE from master.dbo.spt_datatype_info'
//      + ' sti left outer join systypes st on (sti.ss_dtype = st.xtype)'
//      + ' where st.xusertype > 255 and user_name(uid) like %s and st.name'
//      + ' like %s', [SchemaPattern, TypeNamePattern])) do
//    while Next do
//    begin
//      Result.MoveToInsertRow;
//      Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar('TYPE_CAT', Len), @Len);
//      Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar('TYPE_SCHEM', Len), @Len);
//      Result.UpdatePWideChar(UDTColTypeNameIndex, GetPWideChar('TYPE_NAME', Len), @Len);
//      Result.UpdateNull(UDTColClassNameIndex);
//      Result.UpdateSmall(UDTColDataTypeIndex, GetSmall('DATA_TYPE'));
//      Result.UpdateNull(UDTColRemarksIndex);
//      Result.InsertRow;
//    end;

end;

{**
  Open a schema rowset from ado

  @Schema Ado identifier
  @Args Variant array with restrictions
  @return ADO recordset with the schemas; nil if the schema is not supported
}
function TZAdoDatabaseMetadata.AdoOpenSchema(Schema: Integer; const Args: array of const): ZPlainAdo.RecordSet;
var
  Restrictions: Variant;
begin
  Result := nil;
  if not FSupportedSchemasInitialized then
    InitializeSchemas;
  if not SchemaSupported(Schema) then
    Exit;
  try
    Restrictions := BuildRestrictions(Schema, Args);
    Result := (GetConnection as IZAdoConnection).GetAdoConnection.
      OpenSchema(Schema, Restrictions, EmptyParam);
  except
    Result := nil;
  end;
end;

{**
  Initialize supported schemas and restrictions from the OleDB provider
}
procedure TZAdoDatabaseMetadata.InitializeSchemas;
var
  AdoConnection: IZAdoConnection;
  OleDBSession: IUnknown;
  SchemaRS: IDBSchemaRowset;
  PG, OriginalPG: PGUID;
  IA: PIntegerArray;
  Nr: Integer;
  I: Integer;
begin
  if not FSupportedSchemasInitialized then begin
    if not Assigned(FAdoConnection) then begin
      GetConnection.QueryInterface(IZAdoConnection, AdoConnection);
      FAdoConnection := AdoConnection.GetAdoConnection;
    end;
    (FAdoConnection as ADOConnectionConstruction).Get_Session(OleDBSession);
    if Assigned(OleDBSession) then begin
      OleDBSession.QueryInterface(IDBSchemaRowset, SchemaRS);
      if Assigned(SchemaRS) then begin
        SchemaRS.GetSchemas(Nr{%H-}, PG{%H-}, PInteger({%H-}IA));
        OriginalPG := PG;
        SetLength(SupportedSchemas, Nr);
        for I := 0 to Nr - 1 do begin
          SupportedSchemas[I].SchemaGuid := PG^;
          SupportedSchemas[I].SupportedRestrictions := IA^[I];
          SupportedSchemas[I].AdoSchemaId := ConvertOleDBToAdoSchema(PG^);
          Inc({%H-}NativeInt(PG), SizeOf(TGuid));  //M.A. Inc(Integer(PG), SizeOf(TGuid));
        end;
        FSupportedSchemasInitialized := True;
        if Assigned(OriginalPG) then ZAdoMalloc.Free(OriginalPG);
        if Assigned(IA) then ZAdoMalloc.Free(IA);
      end;
    end;
  end;
end;

{**
  Find the Schema Id in the supported schemas

  @SchemaId Ado identifier
  @return Index of the schema in the supported schemas array
}
function TZAdoDatabaseMetadata.FindSchema(SchemaId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(SupportedSchemas) - 1 do
    if SupportedSchemas[I].AdoSchemaId = SchemaId then
    begin
      Result := I;
      Break;
    end;
end;

{**
  Is the schema supported by the OleDB provider?

  @SchemaId Ado identifier
  @return True if the schema is supported
}
function TZAdoDatabaseMetadata.SchemaSupported(SchemaId: Integer): Boolean;
begin
  Result := FindSchema(SchemaId) > -1;
end;

{**
  Build a variant array from the provided parameters based on the supported restrictions

  @SchemaId Ado identifier
  @Args Restrictions
  @return Variant array of restrictions
}
function TZAdoDatabaseMetadata.BuildRestrictions(SchemaId: Integer;
  const Args: array of const): Variant;
var
  SchemaIndex: Integer;
  I: Integer;
begin
  Result := Null;
  if High(Args) = -1 then
    Exit; 
  SchemaIndex := FindSchema(SchemaId);
  if SchemaIndex = -1 then
    Exit;

  Result := VarArrayCreate([0, High(Args)], varVariant);
  for I := 0 to High(Args) do
  begin
    if (SupportedSchemas[SchemaIndex].SupportedRestrictions
      and (1 shl I)) <> 0 then
    begin
      {$IFDEF UNICODE}
      Result[I] := string(Args[I].VPWideChar);
      if (Args[I].VType = VtUnicodeString) then
        if string(Args[I].VPWideChar) = '' then
      {$ELSE}
      Result[I] := string(Args[I].VAnsiString);
      if (Args[I].VType = vtAnsiString) then
        if string(Args[I].VAnsiString) = '' then
      {$ENDIF}
          Result[I] := UnAssigned;
    end else
      Result[I] := UnAssigned;
  end;
end;

end.
