{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Case for Generic Metadata Classes            }
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

unit ZTestDbcMetadata;

interface
{$I ZDbc.inc}
uses
  Types, Classes, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, SysUtils,
  ZDbcIntfs, ZSqlTestCase, ZCompatibility, ZDbcConnection
  {$IFDEF ENABLE_ASA}        , ZDbcASAMetadata {$ENDIF}
  {$IFDEF ENABLE_DBLIB}      , ZDbcDbLibMetadata {$ENDIF}
  {$IFDEF ENABLE_INTERBASE}  , ZDbcInterbaseFirebirdMetadata {$ENDIF}
  {$IFDEF ENABLE_MYSQL}      , ZDbcMySqlMetadata {$ENDIF}
  {$IFDEF ENABLE_OLEDB}      , ZDbcOleDBMetadata {$ENDIF}
  {$IFDEF ENABLE_ORACLE}     , ZDbcOracleMetadata {$ENDIF}
  {$IFDEF ENABLE_POSTGRESQL} , ZDbcPostgreSqlMetadata {$ENDIF}
  {$IFDEF ENABLE_SQLITE}     , ZDbcSqLiteMetadata {$ENDIF}
  ;

type
  {** Implements a test case for. }

  { TZGenericTestDbcMetadata }

  TZGenericTestDbcMetadata = class(TZAbstractDbcSQLTestCase)
  private
    MD: IZDatabaseMetadata;
    Catalog, Schema: string;
    ResultSet: IZResultSet;
    TableTypes: TStringDynArray;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMetadataKeyWords;
    procedure TestMetadataIdentifierQuoting;
    procedure TestMetadataGetCatalogs;
    procedure TestMetadataGetSchemas;
    procedure TestMetadataGetTableTypes;
    procedure TestMetadataGetTables;
    procedure TestMetadataGetColumns;
    procedure TestMetadataGetTablePrivileges;
    procedure TestMetadataGetColumnPrivileges;
    procedure TestMetadataGetBestRowIdentifier;
    procedure TestMetadataGetVersionColumns;
    procedure TestMetadataGetPrimaryKeys;
    procedure TestMetadataGetImportedKeys;
    procedure TestMetadataGetExportedKeys;
    procedure TestMetadataGetCrossReference;
    procedure TestMetadataGetIndexInfo;
    procedure TestMetadataGetProcedures;
    procedure TestMetadataGetProcedureColumns;
    procedure TestMetadataGetTypeInfo;
    procedure TestMetadataGetUDTs;
    procedure TestMetadataGetCharacterSets;
    procedure TestMetadataGetSequences;
    procedure TestMetadataGetTriggers;
  end;

implementation

uses ZSysUtils, ZDbcMetadata;

{ TZGenericTestDbcMetadata }

{**
   Create objects and allocate memory for variables
}
procedure TZGenericTestDbcMetadata.SetUp;
begin
  inherited SetUp;
  CheckNotNull(Connection);
  MD := Connection.GetMetadata;
  CheckNotNull(MD);

  SetLength(TableTypes, 1);
  TableTypes[0] := 'TABLE';

  ResultSet := MD.GetTables('', '', 'people', TableTypes);
  CheckEquals(ResultSet.First, True, 'No people table');
  Catalog := ResultSet.GetStringByName('TABLE_CAT');
  Schema := ResultSet.GetStringByName('TABLE_SCHEM');
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZGenericTestDbcMetadata.TearDown;
begin
  ResultSet := nil;
  MD := nil;
  inherited TearDown;
end;

procedure TZGenericTestDbcMetadata.TestMetadataIdentifierQuoting;
var QuoteStr: string;
begin
  QuoteStr := MD.GetDatabaseInfo.GetIdentifierQuoteString;

  CheckEquals(QuoteStr[1]+'99'+QuoteStr[Length(QuoteStr)],   MD.GetIdentifierConvertor.Quote('99'));
  CheckEquals(QuoteStr[1]+'9A'+QuoteStr[Length(QuoteStr)],   MD.GetIdentifierConvertor.Quote('9A'));
  CheckEquals(QuoteStr[1]+'A9 A'+QuoteStr[Length(QuoteStr)], MD.GetIdentifierConvertor.Quote('A9 A'));
  CheckEquals(QuoteStr[1]+'VALUE'+QuoteStr[Length(QuoteStr)], MD.GetIdentifierConvertor.Quote('VALUE'));
  CheckEquals(QuoteStr[1]+'values'+QuoteStr[Length(QuoteStr)], MD.GetIdentifierConvertor.Quote('values'));

  if MD.GetDatabaseInfo.StoresUpperCaseIdentifiers then
    CheckEquals('A9A', MD.GetIdentifierConvertor.Quote('A9A'));
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetTableTypes;
begin
  Resultset := MD.GetTableTypes;
  CheckNotNull(ResultSet, 'The resultset is nil');
  PrintResultset(Resultset, False, 'GetTableTypes');
  CheckEquals(TableTypeColumnTableTypeIndex, Resultset.FindColumn('TABLE_TYPE'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetCatalogs;
begin
  Resultset := MD.GetCatalogs;
  CheckNotNull(ResultSet, 'The resultset is nil');
  PrintResultset(Resultset, False, 'GetCatalogs');
  CheckEquals(CatalogNameIndex, Resultset.FindColumn('TABLE_CAT'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetSchemas;
begin
  Resultset := MD.GetSchemas;
  CheckNotNull(ResultSet, 'The resultset is nil');
  PrintResultset(Resultset, False, 'GetSchemas');
  CheckEquals(SchemaColumnsTableSchemaIndex, Resultset.FindColumn('TABLE_SCHEM'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetTables;
const
  Tables: array[0..8] of string = ('people', 'blob_values', 'cargo', 'date_values', 'department', 'equipment', 'equipment2', 'number_values', 'string_values');
var
  I: Integer;
  Table: String;
begin
  ResultSet := MD.GetTables(Catalog, Schema, '%', TableTypes);
  CheckNotNull(ResultSet);
  PrintResultSet(Resultset, False, 'GetTables');

  for I := Low(Tables) to High(Tables) do
  begin
    Table := Connection.GetMetadata.AddEscapeCharToWildcards(Tables[I]);
    if Connection.GetServerProvider = spIB_FB then Table := UpperCase(Table);
    ResultSet := MD.GetTables(Catalog, Schema, Table, TableTypes);
    CheckNotNull(ResultSet, 'The resultset is nil');
    CheckEquals(ResultSet.First, True, 'No ' + Tables[I] + ' table');
    CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
    CheckEquals(UpperCase(Tables[I]), UpperCase(Resultset.GetStringByName('TABLE_NAME')));
    CheckEquals(UpperCase('table'), UpperCase(Resultset.GetStringByName('TABLE_TYPE')));
    CheckEquals('', Resultset.GetStringByName('REMARKS'));
  end;
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetColumns;
var
  Index: Integer;
  procedure CheckColumns(Catalog, Schema, TableName, ColumnName: string;
  DataType: SmallInt; TypeName: string; ColumnSize, BufferLength, DecimalDigits,
  Radix, Nullable: Integer; Remarks, ColumnDef: string; SqlDataType,
  SqlDateTimeSub, CharOctetLength, OrdinalPosition: Integer; IsNullable: string);
  begin
    CheckEquals(ResultSet.Next, True, 'The column is missing: ' + ColumnName);
    CheckEquals(Catalog, ResultSet.GetStringByName('TABLE_CAT'), 'The column catalog information is wrong: ');
    CheckEquals(Schema, ResultSet.GetStringByName('TABLE_SCHEM'), 'The column schema information is wrong: ');
    CheckEquals(UpperCase(TableName), UpperCase(ResultSet.GetStringByName('TABLE_NAME')));
    CheckEquals(UpperCase(ColumnName), UpperCase(ResultSet.GetStringByName('COLUMN_NAME')));
//    CheckEquals(DataType, ResultSet.GetSmallByName('DATA_TYPE'));
//    CheckEquals(TypeName, ResultSet.GetStringByName('TYPE_NAME'));
//    CheckEquals(ColumnSize, ResultSet.GetIntByName('COLUMN_SIZE'));
//    CheckEquals(BufferLength, ResultSet.GetIntByName('BUFFER_LENGTH'));
//    CheckEquals(DecimalDigits, ResultSet.GetIntByName('DECIMAL_DIGITS'));
//    CheckEquals(Radix, ResultSet.GetIntByName('NUM_PREC_RADIX'));
    CheckEquals(Nullable, ResultSet.GetIntByName('NULLABLE'));
//    CheckEquals(UpperCase(Remarks), UpperCase(ResultSet.GetStringByName('REMARKS')));
//    CheckEquals(UpperCase(ColumnDef), UpperCase(ResultSet.GetStringByName('COLUMN_DEF')));
//    CheckEquals(SqlDataType, ResultSet.GetIntByName('SQL_DATA_TYPE'));
//    CheckEquals(SqlDateTimeSub, ResultSet.GetIntByName('SQL_DATETIME_SUB'));
//    CheckEquals(CharOctetLength, ResultSet.GetIntByName('CHAR_OCTET_LENGTH'));
    CheckEquals(OrdinalPosition, ResultSet.GetIntByName('ORDINAL_POSITION'));
    CheckEquals(UpperCase(IsNullable), UpperCase(ResultSet.GetStringByName('IS_NULLABLE')));
    Inc(Index);
  end;
begin
  Index := 1;
  ResultSet := MD.GetColumns(Catalog, Schema, 'people', '');
  CheckNotNull(ResultSet);
  PrintResultSet(ResultSet, False);

  CheckColumns(Catalog, Schema, 'people', 'p_id', 5, '', 2, 2, 0, 10, 0, '', '', 5, 0, 0, Index, 'no');
  CheckColumns(Catalog, Schema, 'people', 'p_dep_id', 5, '', 2, 2, 0, 10, 1, '', '', 5, 0, 0, Index, 'yes');
  CheckColumns(Catalog, Schema, 'people', 'p_name', 12, '', 40, 40, 0, 0, 1, '', '', 12, 0, 40, Index, 'yes');
  CheckColumns(Catalog, Schema, 'people', 'p_begin_work', 11, '', 16, 16, 0, 0, 1, '', '', 9, 3, 0, Index, 'yes');
  CheckColumns(Catalog, Schema, 'people', 'p_end_work', 11, '', 16, 16, 0, 0, 1, '', '', 9, 3, 0, Index, 'yes');
  CheckColumns(Catalog, Schema, 'people', 'p_picture', -4, '', 2147483647, 2147483647, 0, 0, 1, '', '', -4, 0, 2147483647, Index, 'yes');
  CheckColumns(Catalog, Schema, 'people', 'p_resume', -1, '', 2147483647, 2147483647, 0, 0, 1, '', '', -1, 0, 2147483647, Index, 'yes');
  CheckColumns(Catalog, Schema, 'people', 'p_redundant', -6, '', 1, 1, 0, 10, 1, '', '', -6, 0, 0, Index, 'yes');
  Check(not Resultset.Next, 'There should not be more columns');
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetTablePrivileges;
begin
  ResultSet := MD.GetTablePrivileges(Catalog, Schema, 'people');
  PrintResultSet(ResultSet, False);
  while ResultSet.Next do
  begin
    CheckEquals(Catalog, ResultSet.GetStringByName('TABLE_CAT'));
    CheckEquals(Schema, ResultSet.GetStringByName('TABLE_SCHEM'));
    CheckEquals('PEOPLE', UpperCase(ResultSet.GetStringByName('TABLE_NAME')));
    CheckEquals(TablePrivGrantorIndex, ResultSet.FindColumn('GRANTOR'));
    CheckEquals(TablePrivGranteeIndex, ResultSet.FindColumn('GRANTEE'));
    CheckEquals(TablePrivPrivilegeIndex, ResultSet.FindColumn('PRIVILEGE'));
    CheckEquals(TablePrivIsGrantableIndex, ResultSet.FindColumn('IS_GRANTABLE'));
  end;
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetColumnPrivileges;
begin
  ResultSet := MD.GetColumnPrivileges(Catalog, Schema, 'people', '');
  PrintResultSet(ResultSet, False);
  while ResultSet.Next do
  begin
    CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
    CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('TABLE_NAME')));
    CheckEquals(ColumnNameIndex, Resultset.FindColumn('COLUMN_NAME'));
    CheckEquals(TableColPrivGrantorIndex, Resultset.FindColumn('GRANTOR'));
    CheckEquals(TableColPrivGranteeIndex, Resultset.FindColumn('GRANTEE'));
    CheckEquals(TableColPrivPrivilegeIndex, Resultset.FindColumn('PRIVILEGE'));
    CheckEquals(TableColPrivIsGrantableIndex, Resultset.FindColumn('IS_GRANTABLE'));
  end;
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetBestRowIdentifier;
begin
  ResultSet := MD.GetBestRowIdentifier(Catalog, Schema, 'people', 0, True);
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be 1 bestRow Identifier in the people table');
  CheckEquals(BestRowIdentScopeIndex, Resultset.FindColumn('SCOPE'));
  CheckEquals(UpperCase('p_id'), UpperCase(Resultset.GetStringByName('COLUMN_NAME')));
  CheckEquals(BestRowIdentDataTypeIndex, Resultset.FindColumn('DATA_TYPE'));
  CheckEquals(BestRowIdentTypeNameIndex, Resultset.FindColumn('TYPE_NAME'));
  CheckEquals(BestRowIdentColSizeIndex, Resultset.FindColumn('COLUMN_SIZE'));
  CheckEquals(BestRowIdentBufLengthIndex, Resultset.FindColumn('BUFFER_LENGTH'));
  CheckEquals(BestRowIdentDecimalDigitsIndex, Resultset.FindColumn('DECIMAL_DIGITS'));
  CheckEquals(BestRowIdentPseudoColumnIndex, Resultset.FindColumn('PSEUDO_COLUMN'));
  CheckEquals(False, ResultSet.Next,
    'There should not be more than 1 bestRow Identifier in the people table');
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetVersionColumns;
begin
  ResultSet := MD.GetVersionColumns(Catalog, Schema, 'people');
  PrintResultSet(ResultSet, False);
  CheckEquals(TableColVerScopeIndex, Resultset.FindColumn('SCOPE'));
  CheckEquals(TableColVerColNameIndex, Resultset.FindColumn('COLUMN_NAME'));
  CheckEquals(TableColVerDataTypeIndex, Resultset.FindColumn('DATA_TYPE'));
  CheckEquals(TableColVerTypeNameIndex, Resultset.FindColumn('TYPE_NAME'));
  CheckEquals(TableColVerColSizeIndex, Resultset.FindColumn('COLUMN_SIZE'));
  CheckEquals(TableColVerBufLengthIndex, Resultset.FindColumn('BUFFER_LENGTH'));
  CheckEquals(TableColVerDecimalDigitsIndex, Resultset.FindColumn('DECIMAL_DIGITS'));
  CheckEquals(TableColVerPseudoColumnIndex, Resultset.FindColumn('PSEUDO_COLUMN'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetPrimaryKeys;
begin
  ResultSet := MD.GetPrimaryKeys(Catalog, Schema, 'people');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be primary key in the people table');
  CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
  CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('TABLE_NAME')));
  CheckEquals('P_ID', UpperCase(Resultset.GetStringByName('COLUMN_NAME')));
  CheckEquals(1, Resultset.GetSmallByName('KEY_SEQ'));
  CheckEquals(PrimaryKeyPKNameIndex, Resultset.FindColumn('PK_NAME'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetImportedKeys;
begin
  if ProtocolType in [protSQLite, protMySQL] then
    Exit;

  ResultSet := MD.GetImportedKeys(Catalog, Schema, 'people');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be an imported key in the people table');
  CheckEquals(Catalog, Resultset.GetStringByName('PKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('PKTABLE_SCHEM'));
  CheckEquals('DEPARTMENT', UpperCase(Resultset.GetStringByName('PKTABLE_NAME')));
  CheckEquals('DEP_ID', UpperCase(Resultset.GetStringByName('PKCOLUMN_NAME')));
  CheckEquals(Catalog, Resultset.GetStringByName('FKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('FKTABLE_SCHEM'));
  CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('FKTABLE_NAME')));
  CheckEquals('P_DEP_ID', UpperCase(Resultset.GetStringByName('FKCOLUMN_NAME')));
  CheckEquals(1, Resultset.GetSmallByName('KEY_SEQ'));
  {had two testdatabases with ADO both did allways return 'NO ACTION' as DELETE/UPDATE_RULE so test will be fixed}
  //Oracle does not provide a update_rule and delete_role is 'no action'
  if not (ProtocolType in [protADO, protOracle, protOleDB]) then
  begin
    CheckEquals(1, Resultset.GetSmallByName('UPDATE_RULE'));
    CheckEquals(1, Resultset.GetSmallByName('DELETE_RULE'));
  end;
  CheckEquals(ImportedKeyColFKNameIndex, Resultset.FindColumn('FK_NAME'));
  CheckEquals(ImportedKeyColPKNameIndex, Resultset.FindColumn('PK_NAME'));
  CheckEquals(ImportedKeyColDeferrabilityIndex, Resultset.FindColumn('DEFERRABILITY'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetExportedKeys;

  procedure CheckExportedKey(PKTable, PKColumn, FKTable, FKColumn: string;
    KeySeq, UpdateRule, DeleteRule: Integer);
  begin
    CheckEquals(Catalog, Resultset.GetStringByName('PKTABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('PKTABLE_SCHEM'));
    CheckEquals(PKTable, UpperCase(Resultset.GetStringByName('PKTABLE_NAME')));
    CheckEquals(PKColumn, UpperCase(Resultset.GetStringByName('PKCOLUMN_NAME')));
    CheckEquals(Catalog, Resultset.GetStringByName('FKTABLE_CAT'));
    CheckEquals(Schema, Resultset.GetStringByName('FKTABLE_SCHEM'));
    CheckEquals(FKTable, UpperCase(Resultset.GetStringByName('FKTABLE_NAME')));
    CheckEquals(FKColumn, UpperCase(Resultset.GetStringByName('FKCOLUMN_NAME')));
    CheckEquals(KeySeq, Resultset.GetSmallByName('KEY_SEQ'));
    {had two testdatabases with ADO both did allways return 'NO ACTION' as DELETE/UPDATE_RULE so test will be fixed}
    //Oracle does not provide a update_rule and delete_role is 'no action'
    if not (ProtocolType in [protADO, protOracle, protOleDB]) then
    begin
      CheckEquals(UpdateRule, Resultset.GetSmallByName('UPDATE_RULE'));
      CheckEquals(DeleteRule, Resultset.GetSmallByName('DELETE_RULE'));
    end;
    CheckEquals(ExportedKeyColFKNameIndex, Resultset.FindColumn('FK_NAME'));
    CheckEquals(ExportedKeyColPKNameIndex, Resultset.FindColumn('PK_NAME'));
    CheckEquals(ExportedKeyColDeferrabilityIndex, Resultset.FindColumn('DEFERRABILITY'));
  end;

begin
  if ProtocolType in [protSQLite, protMySQL] then
    Exit;

  ResultSet := MD.GetExportedKeys(Catalog, Schema, 'department');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be more imported key in the department table');
  CheckExportedKey('DEPARTMENT', 'DEP_ID', 'CARGO', 'C_DEP_ID', 1, 1, 1);
  CheckEquals(True, ResultSet.Next, 'There should be more imported key in the department table');
  CheckExportedKey('DEPARTMENT', 'DEP_ID', 'EQUIPMENT2', 'DEP_ID', 1, 1, 1);
  CheckEquals(True, ResultSet.Next, 'There should be more imported key in the department table');
  CheckExportedKey('DEPARTMENT', 'DEP_ID', 'PEOPLE', 'P_DEP_ID', 1, 1, 1);
  CheckEquals(False, ResultSet.Next, 'There should not be more imported key in the department table');
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetCrossReference;
begin
  if ProtocolType in [protSQLite, protMySQL] then
    Exit;

  ResultSet := MD.GetCrossReference(Catalog, Schema, 'department', Catalog, Schema, 'people');
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be a cross reference between people and department table');
  CheckEquals(Catalog, Resultset.GetStringByName('PKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('PKTABLE_SCHEM'));
  CheckEquals('DEPARTMENT', UpperCase(Resultset.GetStringByName('PKTABLE_NAME')));
  CheckEquals('DEP_ID', UpperCase(Resultset.GetStringByName('PKCOLUMN_NAME')));
  CheckEquals(Catalog, Resultset.GetStringByName('FKTABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('FKTABLE_SCHEM'));
  CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('FKTABLE_NAME')));
  CheckEquals('P_DEP_ID', UpperCase(Resultset.GetStringByName('FKCOLUMN_NAME')));
  CheckEquals(1, Resultset.GetSmallByName('KEY_SEQ'));
  {had two testdatabases with ADO both did allways return 'NO ACTION' as DELETE/UPDATE_RULE so test will be fixed}
  //Oracle does not provide a update_rule and delete_role is 'no action'
  if not (ProtocolType in [protADO, protOracle, protOleDB]) then
  begin
    CheckEquals(1, Resultset.GetSmallByName('UPDATE_RULE'));
    CheckEquals(1, Resultset.GetSmallByName('DELETE_RULE'));
  end;
  CheckEquals(CrossRefKeyColFKNameIndex, Resultset.FindColumn('FK_NAME'));
  CheckEquals(CrossRefKeyColPKNameIndex, Resultset.FindColumn('PK_NAME'));
  CheckEquals(CrossRefKeyColDeferrabilityIndex, Resultset.FindColumn('DEFERRABILITY'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetIndexInfo;
begin
  ResultSet := MD.GetIndexInfo(Catalog, Schema, 'people', False, False);
  PrintResultSet(ResultSet, False);
  CheckEquals(True, ResultSet.Next, 'There should be an index on the people table');
  CheckEquals(Catalog, Resultset.GetStringByName('TABLE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('TABLE_SCHEM'));
  CheckEquals('PEOPLE', UpperCase(Resultset.GetStringByName('TABLE_NAME')));
  CheckEquals(IndexInfoColNonUniqueIndex, Resultset.FindColumn('NON_UNIQUE'));
  CheckEquals(IndexInfoColIndexQualifierIndex, Resultset.FindColumn('INDEX_QUALIFIER'));
  CheckEquals(IndexInfoColIndexNameIndex, Resultset.FindColumn('INDEX_NAME'));
  CheckEquals(IndexInfoColTypeIndex, Resultset.FindColumn('TYPE'));
  CheckEquals(IndexInfoColOrdPositionIndex, Resultset.FindColumn('ORDINAL_POSITION'));
  CheckEquals(IndexInfoColColumnNameIndex, Resultset.FindColumn('COLUMN_NAME'));
  CheckEquals(IndexInfoColAscOrDescIndex, Resultset.FindColumn('ASC_OR_DESC'));
  CheckEquals(IndexInfoColCardinalityIndex, Resultset.FindColumn('CARDINALITY'));
  CheckEquals(IndexInfoColPagesIndex, Resultset.FindColumn('PAGES'));
  CheckEquals(IndexInfoColFilterConditionIndex, Resultset.FindColumn('FILTER_CONDITION'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetProcedures;
begin
  ResultSet := MD.GetProcedures(Catalog, Schema, '');
  PrintResultSet(ResultSet, False);
  CheckEquals(CatalogNameIndex, Resultset.FindColumn('PROCEDURE_CAT'));
  CheckEquals(SchemaNameIndex, Resultset.FindColumn('PROCEDURE_SCHEM'));
  CheckEquals(ProcedureNameIndex, Resultset.FindColumn('PROCEDURE_NAME'));
  CheckEquals(ProcedureOverloadIndex, Resultset.FindColumn('PROCEDURE_OVERLOAD'));
  CheckEquals(ProcedureReserved1Index, Resultset.FindColumn('RESERVED1'));
  CheckEquals(ProcedureReserved2Index, Resultset.FindColumn('RESERVED2'));
  CheckEquals(ProcedureRemarksIndex, Resultset.FindColumn('REMARKS'));
  CheckEquals(ProcedureTypeIndex, Resultset.FindColumn('PROCEDURE_TYPE'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetProcedureColumns;
begin
  ResultSet := MD.GetProcedureColumns(Catalog, Schema, '', '');
  PrintResultSet(ResultSet, False);
  CheckEquals(CatalogNameIndex, Resultset.FindColumn('PROCEDURE_CAT'));
  CheckEquals(SchemaNameIndex, Resultset.FindColumn('PROCEDURE_SCHEM'));
  CheckEquals(ProcColProcedureNameIndex, Resultset.FindColumn('PROCEDURE_NAME'));
  CheckEquals(ProcColColumnNameIndex, Resultset.FindColumn('COLUMN_NAME'));
  CheckEquals(ProcColColumnTypeIndex, Resultset.FindColumn('COLUMN_TYPE'));
  CheckEquals(ProcColDataTypeIndex, Resultset.FindColumn('DATA_TYPE'));
  CheckEquals(ProcColTypeNameIndex, Resultset.FindColumn('TYPE_NAME'));
  CheckEquals(ProcColPrecisionIndex, Resultset.FindColumn('PRECISION'));
  CheckEquals(ProcColLengthIndex, Resultset.FindColumn('LENGTH'));
  CheckEquals(ProcColScaleIndex, Resultset.FindColumn('SCALE'));
  CheckEquals(ProcColRadixIndex, Resultset.FindColumn('RADIX'));
  CheckEquals(ProcColNullableIndex, Resultset.FindColumn('NULLABLE'));
  CheckEquals(ProcColRemarksIndex, Resultset.FindColumn('REMARKS'));
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetTypeInfo;
begin
  ResultSet := MD.GetTypeInfo;
  PrintResultSet(ResultSet, False);
  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetUDTs;
begin
  if ProtocolType in [protPostgre, protMySQL] then
    Exit;
  ResultSet := MD.GetUDTs(Catalog, Schema, '', nil);
  PrintResultSet(ResultSet, False);
  ResultSet.Close;
end;

// We need this class because TZAbstractDbcConnection has abstract methods and
// instances of this class can't be created
type

  { TDummyDbcConnection }

  TDummyDbcConnection = class(TZAbstractDbcConnection, IZConnection)
  public
    procedure InternalClose; override;
    function StartTransaction: Integer;
    procedure Commit;
    procedure Rollback;
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings):
      IZPreparedStatement;
    function PrepareCallWithParams(const SQL: string; Info: TStrings):
      IZCallableStatement;
  protected
    procedure InternalCreate; override;
  public
    Metadata: TZAbstractDatabaseMetadata; // arrrgh... need to keep object pointer
  end;

{ TDummyDbcConnection }

procedure TDummyDbcConnection.InternalCreate;
begin
  Metadata := TZAbstractDatabaseMetadata.Create(Self, Url);
  FMetadata := Metadata;
end;

function TDummyDbcConnection.PrepareCallWithParams(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  Result := nil;
end;

function TDummyDbcConnection.PrepareStatementWithParams(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  Result := nil;
end;

procedure TDummyDbcConnection.Rollback;
begin
  //dummy
end;

procedure TDummyDbcConnection.Commit;
begin
  //dummy
end;

function TDummyDbcConnection.StartTransaction: Integer;
begin
  //dummy
  Result := 0;
end;

function TDummyDbcConnection.CreateStatementWithParams(
  Info: TStrings): IZStatement;
begin
  Result := nil;
end;

procedure TDummyDbcConnection.InternalClose;
begin
end;

// Check if all keywords are correct
procedure TZGenericTestDbcMetadata.TestMetadataKeyWords;
{ We want to create DBInfo instances for each driver available but they require valid
  metadata object (not interface) which we don't have. Objects cannot be obtained from
  interfaces easily. And metadata constructor requires connection object (ARRGH).
  So we have to declare a special connection class just to get things work. }
type
  TZAbstractDatabaseInfoClass = class of TZAbstractDatabaseInfo;
var
  // we need this to keep reference to connection. Otherwise bad things happen!
  // maybe smb could manage to avoid use of this variable
  {%H-}IConn: IZConnection;
  AbsConn: TDummyDbcConnection;
  Url: TZUrl;

  // Currently check only that keyword is not empty and doesn't contain spaces
  function CheckKeyword(const KeyWord: string): Boolean;
  begin
    Result :=
      (KeyWord <> '') and (Pos(' ', KeyWord) = 0);
  end;

  procedure CheckKeywordsList(KeyWords: TStringList; const DBIClass: string);
  var
    i: Integer;
  begin
    for i := 0 to KeyWords.Count - 1 do
      Check(CheckKeyword(KeyWords[i]), Format('%s. Keyword incorrect: "%s"', [DBIClass, KeyWords[i]]));
  end;

  procedure CheckKeywords(DatabaseInfoClass: TZAbstractDatabaseInfoClass);
  var
    DBI: IZDatabaseInfo;
  begin
    DBI := DatabaseInfoClass.Create(AbsConn.Metadata);
    CheckKeywordsList(DBI.GetIdentifierQuoteKeywordsSorted, DatabaseInfoClass.ClassName);
  end;

begin
  Url := GetConnectionUrl('');
  AbsConn := TDummyDbcConnection.Create(Url);
  IConn := AbsConn;
  try
    {$IFDEF ENABLE_ASA}        CheckKeywords(TZASADatabaseInfo);        {$ENDIF}
    {$IFDEF ENABLE_DBLIB}      CheckKeywords(TZDbLibDatabaseInfo);      {$ENDIF}
    {$IFDEF ENABLE_INTERBASE}  CheckKeywords(TZInterbase6DatabaseInfo); {$ENDIF}
    {$IFDEF ENABLE_MYSQL}      CheckKeywords(TZMySqlDatabaseInfo);      {$ENDIF}
    {$IFDEF ENABLE_OLEDB}      CheckKeywords(TZOleDBDatabaseInfo);      {$ENDIF}
    {$IFDEF ENABLE_ORACLE}     CheckKeywords(TZOracleDatabaseInfo);     {$ENDIF}
    {$IFDEF ENABLE_POSTGRESQL} CheckKeywords(TZPostgreSqlDatabaseInfo); {$ENDIF}
    {$IFDEF ENABLE_SQLITE}     CheckKeywords(TZSqLiteDatabaseInfo);     {$ENDIF}
    {$IFDEF ENABLE_ODBC}
    // ODBC requests list of key words from server so we need active connection to run the test
    if ProtocolType = protODBC then
      with Connection.GetMetadata.GetDatabaseInfo do
        CheckKeywordsList(GetIdentifierQuoteKeywordsSorted, ClassName);
    {$ENDIF}
  finally
    FreeAndNil(Url);
  end;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetSequences;
begin
  // Some drivers don't implement this method so just pass the test
  case ProtocolType of
    protInterbase, protFirebird:
      ; // OK
    protPostgre:
      begin
        PrintLn('TODO: implement this');
        Exit;
      end;
    else
    begin
      BlankCheck;
      Exit;
    end;
  end;
 
  ResultSet := MD.GetSequences(Catalog, Schema, 'GEN\_ID');
  PrintResultSet(ResultSet, False);
  Check(ResultSet.Next, 'There should be a sequence "GEN\_ID"');
  CheckEquals(CatalogNameIndex, Resultset.FindColumn('SEQUENCE_CAT'));
  CheckEquals(SchemaNameIndex, Resultset.FindColumn('SEQUENCE_SCHEM'));
  CheckEquals(SequenceNameIndex, Resultset.FindColumn('SEQUENCE_NAME'));
  CheckEquals(Catalog, Resultset.GetStringByName('SEQUENCE_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('SEQUENCE_SCHEM'));
  CheckEquals('GEN_ID', Resultset.GetStringByName('SEQUENCE_NAME'));

  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetTriggers;
begin
  // Some drivers don't implement this method so just pass the test
  if not (ProtocolType in [protInterbase, protFirebird]) then
  begin
    BlankCheck;
    Exit;
  end;

  ResultSet := MD.GetTriggers(Catalog, Schema, '', 'INSERT\_RETURNING\_BI');
  PrintResultSet(ResultSet, False);
  Check(ResultSet.Next, 'There should be a trigger "INSERT_RETURNING_BI"');

  CheckEquals(CatalogNameIndex, Resultset.FindColumn('TRIGGER_CAT'));
  CheckEquals(SchemaNameIndex, Resultset.FindColumn('TRIGGER_SCHEM'));
  CheckEquals(TrgColTriggerNameIndex, Resultset.FindColumn('TRIGGER_NAME'));
  CheckEquals(TrgColRelationNameIndex, Resultset.FindColumn('TRIGGER_RELATION'));
  CheckEquals(TrgColTriggerTypeIndex, Resultset.FindColumn('TRIGGER_TYPE'));
  CheckEquals(TrgColTriggerInactiveIndex, Resultset.FindColumn('TRIGGER_INACTIVE'));
  CheckEquals(TrgColTriggerSourceIndex, Resultset.FindColumn('TRIGGER_SOURCE'));
  CheckEquals(TrgColDescriptionIndex, Resultset.FindColumn('TRIGGER_DESCRIPTION'));

  CheckEquals(Catalog, Resultset.GetStringByName('TRIGGER_CAT'));
  CheckEquals(Schema, Resultset.GetStringByName('TRIGGER_SCHEM'));
  CheckEquals('INSERT_RETURNING_BI', Resultset.GetStringByName('TRIGGER_NAME'));
  CheckEquals('INSERT_RETURNING', Resultset.GetStringByName('TRIGGER_RELATION'));
  CheckEquals(1, Resultset.GetSmallByName('TRIGGER_TYPE'));
  CheckEquals(0, Resultset.GetSmallByName('TRIGGER_INACTIVE'));

  ResultSet.Close;
end;

procedure TZGenericTestDbcMetadata.TestMetadataGetCharacterSets;
begin
  // Some drivers don't implement this method so just pass the test
  if not (ProtocolType in [protInterbase, protFirebird, protPostgre, protMySQL, protSQLite]) then
  begin
    BlankCheck;
    Exit;
  end;

  ResultSet := MD.GetCharacterSets;
  PrintResultSet(ResultSet, False);
  Check(ResultSet.Next, 'There should be a character set');
  CheckEquals(CharacterSetsNameIndex, Resultset.FindColumn('CHARACTER_SET_NAME'));
  CheckEquals(CharacterSetsIDIndex, Resultset.FindColumn('CHARACTER_SET_ID'));
end;

initialization
  RegisterTest('dbc',TZGenericTestDbcMetadata.Suite);
end.
