{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Tests for PostgreSQL Database Metadata Class     }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZTestDbcPostgreSqlMetadata;

interface
{$I ZDbc.inc}
uses
  SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZDbcIntfs, ZCompatibility, ZSqlTestCase, ZDbcPostgreSql;

type

 {** Implements a test case for TZMySqlMetadata. }
  TZTestPostgreSqlMetadataCase = class(TZAbstractDbcSQLTestCase)
  private
    FMetadata: IZDatabaseMetadata;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetSupportedProtocols: string; override;
    property Metadata: IZDatabaseMetadata read FMetadata write FMetadata;
  published
    procedure TestGetProcedures;
    procedure TestGetProcedureColumns;
    procedure TestGetTables;
    procedure TestGetSchemas;
    procedure TestGetCatalogs;
    procedure TestGetTableTypes;
    procedure TestGetColumns;
    procedure TestGetColumnPrivileges;
    procedure TestGetTablePrivileges;
    procedure TestGetBestRowIdentifier;
    procedure TestGetVersionColumns;
    procedure TestGetPrimaryKeys;
    procedure TestGetImportedKeys;
    procedure TestGetExportedKeys;
    procedure TestGetCrossReference;
    procedure TestGetTypeInfo;
    procedure TestGetIndexInfo;
    procedure TestIdentifierQuoting;
  end;

implementation

uses ZTestCase, ZDbcMetadata;

{ TZTestPostgreSqlMetadataCase }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestPostgreSqlMetadataCase.GetSupportedProtocols: string;
begin
  Result := pl_all_postgresql;
end;

{**
   Test version columns
}
procedure TZTestPostgreSqlMetadataCase.TestGetVersionColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetVersionColumns('', '', '');
  with ResultSet do
  begin
    CheckEquals(TableColVerScopeIndex, FindColumn('SCOPE'));
    CheckEquals(TableColVerColNameIndex, FindColumn('COLUMN_NAME'));
    CheckEquals(TableColVerDataTypeIndex, FindColumn('DATA_TYPE'));
    CheckEquals(TableColVerTypeNameIndex, FindColumn('TYPE_NAME'));
    CheckEquals(TableColVerColSizeIndex, FindColumn('COLUMN_SIZE'));
    CheckEquals(TableColVerBufLengthIndex, FindColumn('BUFFER_LENGTH'));
    CheckEquals(TableColVerDecimalDigitsIndex, FindColumn('DECIMAL_DIGITS'));
    CheckEquals(TableColVerPseudoColumnIndex, FindColumn('PSEUDO_COLUMN'));

    CheckEquals(True, Next);
    CheckEquals(True, IsNullByName('SCOPE'));
    CheckEquals('ctid', GetStringByName('COLUMN_NAME'));
    CheckEquals(0, GetIntByName('DATA_TYPE'));
    CheckEquals('tid', GetStringByName('TYPE_NAME'));
    CheckEquals(True, IsNullByName('COLUMN_SIZE'));
    CheckEquals(True, IsNullByName('BUFFER_LENGTH'));
    CheckEquals(True, IsNullByName('DECIMAL_DIGITS'));
    CheckEquals(ord(vcPseudo), GetIntByName('PSEUDO_COLUMN'));
    CheckEquals(False, Next);
    Close;
  end;
  ResultSet := nil;
end;

{**
   Create objects and allocate memory for variables
}
procedure TZTestPostgreSqlMetadataCase.SetUp;
begin
  inherited SetUp;
  Metadata := Connection.GetMetadata;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZTestPostgreSqlMetadataCase.TearDown;
begin
  Metadata := nil;
  inherited TearDown;
end;

{**
   Test method GetBestRowIdentifier
   <p><b>Note:</b><br>
   For adventure of the test it is necessary to execute sql
   <i>grant select(p_resume, p_redundant) on zeoslib.people to root@"%"</i></p>
}
procedure TZTestPostgreSqlMetadataCase.TestGetBestRowIdentifier;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetBestRowIdentifier('', '', 'people', 0, false);
  with ResultSet do
  begin
    CheckEquals(BestRowIdentScopeIndex, FindColumn('SCOPE'));
    CheckEquals(BestRowIdentColNameIndex, FindColumn('COLUMN_NAME'));
    CheckEquals(BestRowIdentDataTypeIndex, FindColumn('DATA_TYPE'));
    CheckEquals(BestRowIdentTypeNameIndex, FindColumn('TYPE_NAME'));
    CheckEquals(BestRowIdentColSizeIndex, FindColumn('COLUMN_SIZE'));
    CheckEquals(BestRowIdentBufLengthIndex, FindColumn('BUFFER_LENGTH'));
    CheckEquals(BestRowIdentDecimalDigitsIndex, FindColumn('DECIMAL_DIGITS'));
    CheckEquals(BestRowIdentPseudoColumnIndex, FindColumn('PSEUDO_COLUMN'));

    CheckEquals(True, Next);
    CheckEquals(2, GetIntByName('SCOPE'));
    CheckEquals('p_id', GetStringByName('COLUMN_NAME'));
    CheckEquals(Ord(stInteger), GetIntByName('DATA_TYPE'));
    CheckEquals('int4', GetStringByName('TYPE_NAME'));
    CheckEquals(4, GetIntByName('COLUMN_SIZE'));
    CheckEquals(0, GetIntByName('BUFFER_LENGTH'));
    CheckEquals(0, GetIntByName('DECIMAL_DIGITS'));
    CheckEquals(1, GetIntByName('PSEUDO_COLUMN'));
    Close;
  end;
  ResultSet := nil;
end;

procedure TZTestPostgreSqlMetadataCase.TestGetCatalogs;
var
  ResultSet: IZResultSet;
  DBFound: boolean;
  CatalogName: string;
begin
  DBFound := False;
  ResultSet := Metadata.GetCatalogs;
  CheckEquals(CatalogNameIndex, ResultSet.FindColumn('TABLE_CAT'));

  while ResultSet.Next do
  begin
    CatalogName := ResultSet.GetString(CatalogNameIndex);
    if CatalogName = Connection.GetMetadata.NormalizePatternCase(ConnectionConfig.Database) then
      DBFound := True;
  end;
  Check(DBFound);
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test method GetBestRowIdentifier
   <p><b>Note:</b><br>
   For adventure of the test it is necessary to execute sql
   <i>grant select privileges on zeoslib.people to root@localhist;</i></p>
}
procedure TZTestPostgreSqlMetadataCase.TestGetColumnPrivileges;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetColumnPrivileges('', '', 'people', 'p_r%');
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('TABLE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('TABLE_SCHEM'));
    CheckEquals(TableNameIndex, FindColumn('TABLE_NAME'));
    CheckEquals(ColumnNameIndex, FindColumn('COLUMN_NAME'));
    CheckEquals(TableColPrivGrantorIndex, FindColumn('GRANTOR'));
    CheckEquals(TableColPrivGranteeIndex, FindColumn('GRANTEE'));
    CheckEquals(TableColPrivPrivilegeIndex, FindColumn('PRIVILEGE'));
    CheckEquals(TableColPrivIsGrantableIndex, FindColumn('IS_GRANTABLE'));

{    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_redundant', GetStringByName('COLUMN_NAME'));
//    CheckEquals('root', GetStringByName('GRANTOR'));
//    CheckEquals('root', GetStringByName('GRANTEE'));
    CheckEquals('INSERT', GetStringByName('PRIVILEGE'));
    CheckEquals('YES', GetStringByName('IS_GRANTABLE'));}
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetColumns
}
procedure TZTestPostgreSqlMetadataCase.TestGetColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetColumns('', '', 'people', 'p_r%');
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('TABLE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('TABLE_SCHEM'));
    CheckEquals(TableNameIndex, FindColumn('TABLE_NAME'));
    CheckEquals(ColumnNameIndex, FindColumn('COLUMN_NAME'));
    CheckEquals(TableColColumnTypeIndex, FindColumn('DATA_TYPE'));
    CheckEquals(TableColColumnTypeNameIndex, FindColumn('TYPE_NAME'));
    CheckEquals(TableColColumnSizeIndex, FindColumn('COLUMN_SIZE'));
    CheckEquals(TableColColumnBufLengthIndex, FindColumn('BUFFER_LENGTH'));
    CheckEquals(TableColColumnDecimalDigitsIndex, FindColumn('DECIMAL_DIGITS'));
    CheckEquals(TableColColumnNumPrecRadixIndex, FindColumn('NUM_PREC_RADIX'));
    CheckEquals(TableColColumnNullableIndex, FindColumn('NULLABLE'));
    CheckEquals(TableColColumnRemarksIndex, FindColumn('REMARKS'));
    CheckEquals(TableColColumnColDefIndex, FindColumn('COLUMN_DEF'));
    CheckEquals(TableColColumnSQLDataTypeIndex, FindColumn('SQL_DATA_TYPE'));
    CheckEquals(TableColColumnSQLDateTimeSubIndex, FindColumn('SQL_DATETIME_SUB'));
    CheckEquals(TableColColumnCharOctetLengthIndex, FindColumn('CHAR_OCTET_LENGTH'));
    CheckEquals(TableColColumnOrdPosIndex, FindColumn('ORDINAL_POSITION'));
    CheckEquals(TableColColumnIsNullableIndex, FindColumn('IS_NULLABLE'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_resume', GetStringByName('COLUMN_NAME'));
    if ( Connection.GetConSettings.CPType = cCP_UTF16 ) then
      CheckEquals(ord(stUnicodeStream), ResultSet.GetIntByName('DATA_TYPE'))
    else
      CheckEquals(ord(stAsciiStream), ResultSet.GetIntByName('DATA_TYPE'));
    CheckEquals('TEXT', UpperCase(GetStringByName('TYPE_NAME')));
    CheckEquals(-1, GetIntByName('COLUMN_SIZE'));
    CheckEquals(0, GetIntByName('BUFFER_LENGTH'));
    CheckEquals(0, GetIntByName('DECIMAL_DIGITS'));
    CheckEquals(2, GetIntByName('NUM_PREC_RADIX'));
    CheckEquals(1, GetIntByName('NULLABLE'));
    CheckEquals('', GetStringByName('REMARKS'));
    CheckEquals('', GetStringByName('COLUMN_DEF'));
    CheckEquals(0, GetIntByName('SQL_DATA_TYPE'));
    CheckEquals(0, GetIntByName('SQL_DATETIME_SUB'));
    CheckEquals(-1, GetIntByName('CHAR_OCTET_LENGTH'));
    CheckEquals(7, GetIntByName('ORDINAL_POSITION'));
    CheckEquals('YES', GetStringByName('IS_NULLABLE'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_redundant', GetStringByName('COLUMN_NAME'));
    CheckEquals(ord(stSmall), GetIntByName('DATA_TYPE'));
    CheckEquals('INT2', UpperCase(GetStringByName('TYPE_NAME')));
    CheckEquals(2, GetIntByName('COLUMN_SIZE'));
    CheckEquals(0, GetIntByName('BUFFER_LENGTH'));
    CheckEquals(0, GetIntByName('DECIMAL_DIGITS'));
    CheckEquals(2, GetIntByName('NUM_PREC_RADIX'));
    CheckEquals(1, GetIntByName('NULLABLE'));
    CheckEquals('', GetStringByName('REMARKS'));
    CheckEquals('', GetStringByName('COLUMN_DEF'));
    CheckEquals(0, GetIntByName('SQL_DATA_TYPE'));
    CheckEquals(0, GetIntByName('SQL_DATETIME_SUB'));
    CheckEquals(2, GetIntByName('CHAR_OCTET_LENGTH'));
    CheckEquals(8, GetIntByName('ORDINAL_POSITION'));
    CheckEquals('YES', GetStringByName('IS_NULLABLE'));

    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetProcedureColumns
}
procedure TZTestPostgreSqlMetadataCase.TestGetProcedureColumns;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetProcedureColumns('', '', 'procedure1', '');
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('PROCEDURE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('PROCEDURE_SCHEM'));
    CheckEquals(ProcColProcedureNameIndex, FindColumn('PROCEDURE_NAME'));
    CheckEquals(ProcColColumnNameIndex, FindColumn('COLUMN_NAME'));
    CheckEquals(ProcColColumnTypeIndex, FindColumn('COLUMN_TYPE'));
    CheckEquals(ProcColDataTypeIndex, FindColumn('DATA_TYPE'));
    CheckEquals(ProcColTypeNameIndex, FindColumn('TYPE_NAME'));
    CheckEquals(ProcColPrecisionIndex, FindColumn('PRECISION'));
    CheckEquals(ProcColLengthIndex, FindColumn('LENGTH'));
    CheckEquals(ProcColScaleIndex, FindColumn('SCALE'));
    CheckEquals(ProcColRadixIndex, FindColumn('RADIX'));
    CheckEquals(ProcColNullableIndex, FindColumn('NULLABLE'));
    CheckEquals(ProcColRemarksIndex, FindColumn('REMARKS'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PROCEDURE_CAT'));
//    CheckEquals('public', GetStringByName('PROCEDURE_SCHEM'));
    CheckEquals('procedure1', GetStringByName('PROCEDURE_NAME'));
    CheckEquals('$1', GetStringByName('COLUMN_NAME'));
    CheckEquals('1', GetStringByName('COLUMN_TYPE'));
    CheckEquals(Ord(stInteger), GetByteByName('DATA_TYPE'));
    CheckEquals('int4', GetStringByName('TYPE_NAME'));
    CheckEquals('', GetStringByName('PRECISION'));
    CheckEquals('', GetStringByName('LENGTH'));
    CheckEquals('', GetStringByName('SCALE'));
    CheckEquals('', GetStringByName('RADIX'));
    CheckEquals('2', GetStringByName('NULLABLE'));
    CheckEquals('', GetStringByName('REMARKS'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PROCEDURE_CAT'));
//    CheckEquals('public', GetStringByName('PROCEDURE_SCHEM'));
    CheckEquals('procedure1', GetStringByName('PROCEDURE_NAME'));
    CheckEquals('returnValue', GetStringByName('COLUMN_NAME'));
    CheckEquals('4', GetStringByName('COLUMN_TYPE'));
    CheckEquals(IntToStr(Ord(stInteger)), GetStringByName('DATA_TYPE'));
    CheckEquals('int4', GetStringByName('TYPE_NAME'));
    CheckEquals('', GetStringByName('PRECISION'));
    CheckEquals('', GetStringByName('LENGTH'));
    CheckEquals('', GetStringByName('SCALE'));
    CheckEquals('', GetStringByName('RADIX'));
    CheckEquals('2', GetStringByName('NULLABLE'));
    CheckEquals('', GetStringByName('REMARKS'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetProcedures
}
procedure TZTestPostgreSqlMetadataCase.TestGetProcedures;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetProcedures('', '', 'procedure%');
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('PROCEDURE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('PROCEDURE_SCHEM'));
    CheckEquals(ProcedureNameIndex, FindColumn('PROCEDURE_NAME'));
    CheckEquals(ProcedureRemarksIndex, FindColumn('REMARKS'));
    CheckEquals(ProcedureTypeIndex, FindColumn('PROCEDURE_TYPE'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PROCEDURE_CAT'));
//    CheckEquals('public', GetStringByName('PROCEDURE_SCHEM'));
    CheckEquals('procedure1', GetStringByName('PROCEDURE_NAME'));
    CheckEquals('', GetStringByName('REMARKS'));
    CheckEquals(2, GetIntByName('PROCEDURE_TYPE'));

    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetSchemas
}
procedure TZTestPostgreSqlMetadataCase.TestGetSchemas;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetSchemas;
  CheckEquals(SchemaColumnsTableSchemaIndex, ResultSet.FindColumn('TABLE_SCHEM'));
  ResultSet.Close;
  ResultSet := nil;
end;

{**
   Test for method GetTablePrivileges
}
procedure TZTestPostgreSqlMetadataCase.TestGetTablePrivileges;
var
  ResultSet: IZResultSet;
begin
  { To grant privileges
    grant select on people to root }
  ResultSet := Metadata.GetTablePrivileges('', '', 'people');
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('TABLE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('TABLE_SCHEM'));
    CheckEquals(TableNameIndex, FindColumn('TABLE_NAME'));
    CheckEquals(TablePrivGrantorIndex, FindColumn('GRANTOR'));
    CheckEquals(TablePrivGranteeIndex, FindColumn('GRANTEE'));
    CheckEquals(TablePrivPrivilegeIndex, FindColumn('PRIVILEGE'));
    CheckEquals(TablePrivIsGrantableIndex, FindColumn('IS_GRANTABLE'));

{    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    //CheckEquals('root', GetStringByName('GRANTOR'));
    CheckEquals('root', GetStringByName('GRANTEE'));
    CheckEquals('SELECT', GetStringByName('PRIVILEGE'));
    CheckEquals('NO', GetStringByName('IS_GRANTABLE'));}
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetTables
}
procedure TZTestPostgreSqlMetadataCase.TestGetTables;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTables('', '', 'people', nil);
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('TABLE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('TABLE_SCHEM'));
    CheckEquals(TableNameIndex, FindColumn('TABLE_NAME'));
    CheckEquals(TableColumnsSQLType, FindColumn('TABLE_TYPE'));
    CheckEquals(TableColumnsRemarks, FindColumn('REMARKS'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
    //CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('TABLE', GetStringByName('TABLE_TYPE'));
    CheckEquals('', GetStringByName('REMARKS'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetTableTypes
}
procedure TZTestPostgreSqlMetadataCase.TestGetTableTypes;
const
  Types: array [0..10] of string = ('TABLE', 'VIEW', 'INDEX',
    'SEQUENCE', 'SYSTEM TABLE', 'SYSTEM TOAST TABLE',
    'SYSTEM TOAST INDEX', 'SYSTEM VIEW', 'SYSTEM INDEX',
    'TEMPORARY TABLE', 'TEMPORARY INDEX');
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTableTypes;
  with ResultSet do
  begin
    CheckEquals(TableTypeColumnTableTypeIndex, FindColumn('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[0], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[1], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[2], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[3], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[4], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[5], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[6], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[7], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[8], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[9], GetStringByName('TABLE_TYPE'));
    CheckEquals(True, Next);
    CheckEquals(Types[10], GetStringByName('TABLE_TYPE'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetPrimaryKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetPrimaryKeys;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetPrimaryKeys('', '', 'people');
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('TABLE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('TABLE_SCHEM'));
    CheckEquals(TableNameIndex, FindColumn('TABLE_NAME'));
    CheckEquals(PrimaryKeyColumnNameIndex, FindColumn('COLUMN_NAME'));
    CheckEquals(PrimaryKeyKeySeqIndex, FindColumn('KEY_SEQ'));
    CheckEquals(PrimaryKeyPKNameIndex, FindColumn('PK_NAME'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('people', GetStringByName('TABLE_NAME'));
    CheckEquals('p_id', GetStringByName('COLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals('people_pkey', GetStringByName('PK_NAME'));
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetImportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetImportedKeys;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetImportedKeys('', '', 'people');
  with ResultSet do
  begin
    CheckEquals(ImportedKeyColPKTableCatalogIndex, FindColumn('PKTABLE_CAT'));
    CheckEquals(ImportedKeyColPKTableSchemaIndex, FindColumn('PKTABLE_SCHEM'));
    CheckEquals(ImportedKeyColPKTableNameIndex, FindColumn('PKTABLE_NAME'));
    CheckEquals(ImportedKeyColPKColumnNameIndex, FindColumn('PKCOLUMN_NAME'));
    CheckEquals(ImportedKeyColFKTableCatalogIndex, FindColumn('FKTABLE_CAT'));
    CheckEquals(ImportedKeyColFKTableSchemaIndex, FindColumn('FKTABLE_SCHEM'));
    CheckEquals(ImportedKeyColFKTableNameIndex, FindColumn('FKTABLE_NAME'));
    CheckEquals(ImportedKeyColFKColumnNameIndex, FindColumn('FKCOLUMN_NAME'));
    CheckEquals(ImportedKeyColKeySeqIndex, FindColumn('KEY_SEQ'));
    CheckEquals(ImportedKeyColUpdateRuleIndex, FindColumn('UPDATE_RULE'));
    CheckEquals(ImportedKeyColDeleteRuleIndex, FindColumn('DELETE_RULE'));
    CheckEquals(ImportedKeyColFKNameIndex, FindColumn('FK_NAME'));
    CheckEquals(ImportedKeyColPKNameIndex, FindColumn('PK_NAME'));
    CheckEquals(ImportedKeyColDeferrabilityIndex, FindColumn('DEFERRABILITY'));

    CheckEquals(True, Next);
//    CheckEquals('', GetStringByName('PKTABLE_CAT'));
    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));

    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));

//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('people', GetStringByName('FKTABLE_NAME'));
    CheckEquals('p_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
    CheckEquals('people_p_dep_id_fkey', GetStringByName('FK_NAME'));
    CheckEquals('department_pkey', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
end;

{**
   Test for method GetExportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetExportedKeys;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetExportedKeys('', '', 'department');
  with ResultSet do
  begin
    CheckEquals(ExportedKeyColPKTableCatalogIndex, FindColumn('PKTABLE_CAT'));
    CheckEquals(ExportedKeyColPKTableSchemaIndex, FindColumn('PKTABLE_SCHEM'));
    CheckEquals(ExportedKeyColPKTableNameIndex, FindColumn('PKTABLE_NAME'));
    CheckEquals(ExportedKeyColPKColumnNameIndex, FindColumn('PKCOLUMN_NAME'));
    CheckEquals(ExportedKeyColFKTableCatalogIndex, FindColumn('FKTABLE_CAT'));
    CheckEquals(ExportedKeyColFKTableSchemaIndex, FindColumn('FKTABLE_SCHEM'));
    CheckEquals(ExportedKeyColFKTableNameIndex, FindColumn('FKTABLE_NAME'));
    CheckEquals(ExportedKeyColFKColumnNameIndex, FindColumn('FKCOLUMN_NAME'));
    CheckEquals(ExportedKeyColKeySeqIndex, FindColumn('KEY_SEQ'));
    CheckEquals(ExportedKeyColUpdateRuleIndex, FindColumn('UPDATE_RULE'));
    CheckEquals(ExportedKeyColDeleteRuleIndex, FindColumn('DELETE_RULE'));
    CheckEquals(ExportedKeyColFKNameIndex, FindColumn('FK_NAME'));
    CheckEquals(ExportedKeyColPKNameIndex, FindColumn('PK_NAME'));
    CheckEquals(ExportedKeyColDeferrabilityIndex, FindColumn('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('cargo', GetStringByName('FKTABLE_NAME'));
    CheckEquals('c_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
    CheckEquals('cargo_c_dep_id_fkey', GetStringByName('FK_NAME'));
    CheckEquals('department_pkey', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('equipment2', GetStringByName('FKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
    CheckEquals('equipment2_dep_id_fkey', GetStringByName('FK_NAME'));
    CheckEquals('department_pkey', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('PKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    CheckEquals('', GetStringByName('FKTABLE_CAT'));
//    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('people', GetStringByName('FKTABLE_NAME'));
    CheckEquals('p_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
     CheckEquals('people_p_dep_id_fkey', GetStringByName('FK_NAME'));
    CheckEquals('department_pkey', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));
    Check(Not Next);
  end;
  ResultSet := nil;
end;

{**
   Test for method GetExportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetCrossReference;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetCrossReference('', '', 'department', '', '', 'people');
  with ResultSet do
  begin
    CheckEquals(CrossRefKeyColPKTableCatalogIndex, FindColumn('PKTABLE_CAT'));
    CheckEquals(CrossRefKeyColPKTableSchemaIndex, FindColumn('PKTABLE_SCHEM'));
    CheckEquals(CrossRefKeyColPKTableNameIndex, FindColumn('PKTABLE_NAME'));
    CheckEquals(CrossRefKeyColPKColumnNameIndex, FindColumn('PKCOLUMN_NAME'));
    CheckEquals(CrossRefKeyColFKTableCatalogIndex, FindColumn('FKTABLE_CAT'));
    CheckEquals(CrossRefKeyColFKTableSchemaIndex, FindColumn('FKTABLE_SCHEM'));
    CheckEquals(CrossRefKeyColFKTableNameIndex, FindColumn('FKTABLE_NAME'));
    CheckEquals(CrossRefKeyColFKColumnNameIndex, FindColumn('FKCOLUMN_NAME'));
    CheckEquals(CrossRefKeyColKeySeqIndex, FindColumn('KEY_SEQ'));
    CheckEquals(CrossRefKeyColUpdateRuleIndex, FindColumn('UPDATE_RULE'));
    CheckEquals(CrossRefKeyColDeleteRuleIndex, FindColumn('DELETE_RULE'));
    CheckEquals(CrossRefKeyColFKNameIndex, FindColumn('FK_NAME'));
    CheckEquals(CrossRefKeyColPKNameIndex, FindColumn('PK_NAME'));
    CheckEquals(CrossRefKeyColDeferrabilityIndex, FindColumn('DEFERRABILITY'));

    CheckEquals(True, Next);
    //CheckEquals('zeoslib', GetStringByName('PKTABLE_CAT'));
    CheckEquals('public', GetStringByName('PKTABLE_SCHEM'));
    CheckEquals('department', GetStringByName('PKTABLE_NAME'));
    CheckEquals('dep_id', GetStringByName('PKCOLUMN_NAME'));
    //CheckEquals('zeoslib', GetStringByName('FKTABLE_CAT'));
    CheckEquals('public', GetStringByName('FKTABLE_SCHEM'));
    CheckEquals('people', GetStringByName('FKTABLE_NAME'));
    CheckEquals('p_dep_id', GetStringByName('FKCOLUMN_NAME'));
    CheckEquals(1, GetIntByName('KEY_SEQ'));
    CheckEquals(ord(ikRestrict), GetIntByName('UPDATE_RULE'));
    CheckEquals(ord(ikRestrict), GetIntByName('DELETE_RULE'));
    CheckEquals('people_p_dep_id_fkey', GetStringByName('FK_NAME'));
    CheckEquals('department_pkey', GetStringByName('PK_NAME'));
    CheckEquals(ord(ikNotDeferrable), GetIntByName('DEFERRABILITY'));
    CheckEquals(False, Next);
  end;
  ResultSet := nil;
end;

{**
   Test for method GetExportedKeys
}
procedure TZTestPostgreSqlMetadataCase.TestGetTypeInfo;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetTypeInfo;
  with ResultSet do
  begin
    CheckEquals(True, Next);
    Close;
  end;
  ResultSet := nil;
end;

{**
   Test for method GetIndexInfo
}
procedure TZTestPostgreSqlMetadataCase.TestGetIndexInfo;
var
  ResultSet: IZResultSet;
begin
  ResultSet := Metadata.GetIndexInfo('', '', 'department', True, True);
  with ResultSet do
  begin
    CheckEquals(CatalogNameIndex, FindColumn('TABLE_CAT'));
    CheckEquals(SchemaNameIndex, FindColumn('TABLE_SCHEM'));
    CheckEquals(TableNameIndex, FindColumn('TABLE_NAME'));
    CheckEquals(IndexInfoColNonUniqueIndex, FindColumn('NON_UNIQUE'));
    CheckEquals(IndexInfoColIndexQualifierIndex, FindColumn('INDEX_QUALIFIER'));
    CheckEquals(IndexInfoColIndexNameIndex, FindColumn('INDEX_NAME'));
    CheckEquals(IndexInfoColTypeIndex, FindColumn('TYPE'));
    CheckEquals(IndexInfoColOrdPositionIndex, FindColumn('ORDINAL_POSITION'));
    CheckEquals(IndexInfoColColumnNameIndex, FindColumn('COLUMN_NAME'));
    CheckEquals(IndexInfoColAscOrDescIndex, FindColumn('ASC_OR_DESC'));
    CheckEquals(IndexInfoColCardinalityIndex, FindColumn('CARDINALITY'));
    CheckEquals(IndexInfoColPagesIndex, FindColumn('PAGES'));
    CheckEquals(IndexInfoColFilterConditionIndex, FindColumn('FILTER_CONDITION'));

    CheckEquals(True, Next);
    CheckEquals('', GetStringByName('TABLE_CAT'));
//    CheckEquals('public', GetStringByName('TABLE_SCHEM'));
    CheckEquals('department', GetStringByName('TABLE_NAME'));
    CheckEquals(False, GetBooleanByName('NON_UNIQUE'));
    CheckEquals('', GetStringByName('INDEX_QUALIFIER'));
    CheckEquals('department_pkey', GetStringByName('INDEX_NAME'));
    CheckEquals(3, GetIntByName('TYPE'));
    CheckEquals(1, GetIntByName('ORDINAL_POSITION'));
    CheckEquals('dep_id', GetStringByName('COLUMN_NAME'));
    CheckEquals('', GetStringByName('ASC_OR_DESC'));
//    CheckEquals(1000, GetIntByName('CARDINALITY'));
    CheckEquals(1, GetIntByName('PAGES'));
    CheckEquals('', GetStringByName('FILTER_CONDITION'));
    Close;
  end;
  ResultSet := nil;
end;

procedure TZTestPostgreSqlMetadataCase.TestIdentifierQuoting;
var QuoteStr: string;
begin
  QuoteStr := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;

  CheckEquals(QuoteStr[1]+'A9A'+QuoteStr[Length(QuoteStr)], Metadata.GetIdentifierConvertor.Quote('A9A'));
  CheckEquals(QuoteStr[1]+'a9A'+QuoteStr[Length(QuoteStr)], Metadata.GetIdentifierConvertor.Quote('a9A'));
end;

initialization
  RegisterTest('dbc',TZTestPostgreSqlMetadataCase.Suite);
end.
