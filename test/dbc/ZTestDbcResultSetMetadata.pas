{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Test Case for ResultSet Metadata Classes         }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZTestDbcResultSetMetadata;

interface
{$I ZDbc.inc}
uses Classes, SysUtils, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF},
  ZSqlTestCase, ZDbcIntfs;

type

 {** Implements a test case for TZAbstractBlob. }
  TZTestResultSetMetadataCase = class(TZAbstractDbcSQLTestCase)
  private
    function GetFieldNameFromUnQuoted(const Value: String): String;
    function GetColumnLabelFromUnQuoted(const Value: String; DuplicateIndex: Integer): String;
  protected
    procedure CheckColumnMetadata(const Metadata: IZResultSetMetadata;
      ColumnIndex: Integer; ColumnLabel, ColumnName, ColumnTable: string;
      IsAutoIncrement, IsWritable: Boolean); overload;
  published
    procedure TestResultSetMetadata;
    procedure TestColumnTypeAndTableDetermination;
    procedure TestDefault_valuesJoinsDefault_values2Lower;
    procedure TestDefault_values2JoinsDefault_valuesLower;
    procedure TestResultSetMetadata1;
  end;

implementation

uses ZSysUtils;

{ TZTestResultSetMetadataCase }

{**
  Checks metadata for one single resultset column.
}
procedure TZTestResultSetMetadataCase.CheckColumnMetadata(
  const Metadata: IZResultSetMetadata; ColumnIndex: Integer; ColumnLabel,
  ColumnName, ColumnTable: string; IsAutoIncrement, IsWritable: Boolean);
begin
  CheckEquals(ColumnLabel, Metadata.GetColumnLabel(ColumnIndex), 'ColumnLabel does not match');
  CheckEquals(ColumnName, Metadata.GetColumnName(ColumnIndex), 'ColumnName does not match');
  CheckEquals(ColumnTable, Metadata.GetTableName(ColumnIndex), 'TableName does not match');
//  CheckEquals(IsAutoIncrement, Metadata.IsAutoIncrement(ColumnIndex), 'IsAutoIncrement does not match');
  CheckEquals(IsWritable, Metadata.IsWritable(ColumnIndex), 'IsWritable does not match');
  CheckEquals(IsWritable, Metadata.IsDefinitelyWritable(ColumnIndex), 'IsDefinitelyWritable does not match');
end;

{**
  Runs a test for resultset metadata.
}
procedure TZTestResultSetMetadataCase.TestResultSetMetadata;
const
  department_dep_id_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  department_dep_name_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  department_dep_shname_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  //department_dep_address_Index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  department_computed_col1_Index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  department_computed_col2_Index = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  Connection.Open;
  if Connection.GetServerProvider in [spIB_FB, spOracle] then
    Exit;

  Statement := Connection.CreateStatement;

  ResultSet := Statement.ExecuteQuery('SELECT t.dep_id AS id, dep_name AS name,'
    + ' t.dep_shname, 2+2 AS dep_address FROM department as t WHERE dep_id < 100');
  Metadata := ResultSet.GetMetadata;

  CheckEquals(4, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, department_dep_id_Index, 'id', 'dep_id', 'department',
    True, True);
  CheckColumnMetadata(Metadata, department_dep_name_Index, 'name', 'dep_name', 'department',
    False, True);
  CheckColumnMetadata(Metadata, department_dep_shname_Index, 'dep_shname', 'dep_shname', 'department',
    False, True);
  CheckColumnMetadata(Metadata, department_computed_col1_Index, 'dep_address', '', '',
    False, False);

  ResultSet := Statement.ExecuteQuery('SELECT t.*, 2+2 as dep_address'
    + ' FROM department as t where dep_id < 100');
  Metadata := ResultSet.GetMetadata;

  CheckEquals(5, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, department_dep_id_Index, 'dep_id', 'dep_id', 'department',
    True, True);
  CheckColumnMetadata(Metadata, department_dep_name_Index, 'dep_name', 'dep_name', 'department',
    False, True);
  CheckColumnMetadata(Metadata, department_dep_shname_Index, 'dep_shname', 'dep_shname', 'department',
    False, True);
//  CheckColumnMetadata(Metadata, department_dep_address_Index, 'dep_address', 'dep_address', 'department',
//    False, True);
  CheckColumnMetadata(Metadata, department_computed_col2_Index, 'dep_address_1', '', '', False, False);

  ResultSet := Statement.ExecuteQuery('SELECT *, 2+2 as dep_address'
    + ' FROM department as t where dep_id < 100');
  Metadata := ResultSet.GetMetadata;

  CheckEquals(5, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, department_dep_id_Index, 'dep_id', 'dep_id', 'department',
    True, True);
  CheckColumnMetadata(Metadata, department_dep_name_Index, 'dep_name', 'dep_name', 'department',
    False, True);
  CheckColumnMetadata(Metadata, department_dep_shname_Index, 'dep_shname', 'dep_shname', 'department',
    False, True);
//  CheckColumnMetadata(Metadata, department_dep_address_Index, 'dep_address', 'dep_address', 'department',
//    False, True);
  CheckColumnMetadata(Metadata, department_computed_col2_Index, 'dep_address_1', '', '', False, False);
end;

{**
  Runs a test for resultset metadata specific to Interbase, Firebird and Oracle.
}
procedure TZTestResultSetMetadataCase.TestResultSetMetadata1;
const
  DEP_ID_Index = FirstDbcIndex;
  DEP_NAME_Index = FirstDbcIndex +1;
  DEP_SHNAME_Index = FirstDbcIndex +2;
  DEP_ADDRESS_Index = FirstDbcIndex + 3;
  DEP_ADDRESS_1_Index = FirstDbcIndex +4;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  if not (ProtocolType in [protInterbase, protFirebird, protOracle]) then
    Exit;

  Statement := Connection.CreateStatement;

  ResultSet := Statement.ExecuteQuery('SELECT T.DEP_ID AS ID, DEP_NAME AS NAME,'
    + ' T.DEP_SHNAME, 2+2 AS DEP_ADDRESS FROM DEPARTMENT T WHERE DEP_ID < 100');
  Metadata := ResultSet.GetMetadata;

  CheckEquals(4, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, DEP_ID_Index, 'ID', 'DEP_ID', 'DEPARTMENT',
    True, True);
  CheckColumnMetadata(Metadata, DEP_NAME_Index, 'NAME', 'DEP_NAME', 'DEPARTMENT',
    False, True);
  CheckColumnMetadata(Metadata, DEP_SHNAME_Index, 'DEP_SHNAME', 'DEP_SHNAME', 'DEPARTMENT',
    False, True);
  CheckColumnMetadata(Metadata, DEP_ADDRESS_Index, 'DEP_ADDRESS', '', '',
    False, False);

  ResultSet := Statement.ExecuteQuery('SELECT T.*, 2+2 AS DEP_ADDRESS'
    + ' FROM DEPARTMENT T where DEP_ID < 100');
  Metadata := ResultSet.GetMetadata;

  CheckEquals(5, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, DEP_ID_Index, 'DEP_ID', 'DEP_ID', 'DEPARTMENT',
    True, True);
  CheckColumnMetadata(Metadata, DEP_NAME_Index, 'DEP_NAME', 'DEP_NAME', 'DEPARTMENT',
    False, True);
  CheckColumnMetadata(Metadata, DEP_SHNAME_Index, 'DEP_SHNAME', 'DEP_SHNAME', 'DEPARTMENT',
    False, True);
  CheckColumnMetadata(Metadata, DEP_ADDRESS_1_Index, 'DEP_ADDRESS_1', '', '',
    False, False);

  ResultSet := Statement.ExecuteQuery('SELECT T.*, 2+2 as DEP_ADDRESS'
    + ' FROM DEPARTMENT T where DEP_ID < 100');
  Metadata := ResultSet.GetMetadata;

  CheckEquals(5, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, DEP_ID_Index, 'DEP_ID', 'DEP_ID', 'DEPARTMENT',
    True, True);
  CheckColumnMetadata(Metadata, DEP_NAME_Index, 'DEP_NAME', 'DEP_NAME', 'DEPARTMENT',
    False, True);
  CheckColumnMetadata(Metadata, DEP_SHNAME_Index, 'DEP_SHNAME', 'DEP_SHNAME', 'DEPARTMENT',
    False, True);
  CheckColumnMetadata(Metadata, DEP_ADDRESS_1_Index, 'DEP_ADDRESS_1', '', '',
    False, False);
end;

function TZTestResultSetMetadataCase.GetColumnLabelFromUnQuoted(
  const Value: String; DuplicateIndex: Integer): String;
begin
  if Connection.GetMetadata.GetDatabaseInfo.StoresUpperCaseIdentifiers then
    Result := UpperCase(Value)
  else if Connection.GetMetadata.GetDatabaseInfo.StoresLowerCaseIdentifiers then
    Result := LowerCase(Value)
  else Result := Value;
  if DuplicateIndex > 0 then
   Result := Result +'_'+IntToStr(DuplicateIndex);
end;

function TZTestResultSetMetadataCase.GetFieldNameFromUnQuoted(const Value: String): String;
begin
  if Connection.GetMetadata.GetDatabaseInfo.StoresUpperCaseIdentifiers then
    Result := UpperCase(Value)
  else if Connection.GetMetadata.GetDatabaseInfo.StoresLowerCaseIdentifiers then
    Result := LowerCase(Value)
  else Result := Value;
end;

procedure TZTestResultSetMetadataCase.TestColumnTypeAndTableDetermination;
const
  DEP_ID_Index        = FirstDbcIndex;
  DEP_NAME_Index      = FirstDbcIndex +1;
  DEP_SHNAME_Index    = FirstDbcIndex +2;
  DEP_ADDRESS_Index   = FirstDbcIndex +3;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;

  function GetIdentifierName(const Value: String): String;
  begin
    if Connection.GetMetadata.GetDatabaseInfo.StoresUpperCaseIdentifiers then
      Result := UpperCase(Value)
    else if Connection.GetMetadata.GetDatabaseInfo.StoresLowerCaseIdentifiers then
      Result := LowerCase(Value)
    else Result := Value;
  end;
  function GetColumnLabeName(ColIndex: Integer; const Value: String): String;
  begin
    if ProtocolType = protPostgre then //unquoted alias is also lowercase pffff...
      Result := lowerCase(Value)
    else
      Result := Value;
  end;
  procedure TestAll;
  begin
    CheckEquals(4, Metadata.GetColumnCount);

    CheckColumnMetadata(Metadata, DEP_ID_Index, GetColumnLabeName(DEP_ID_Index, 'DEP_NAME'),
      GetFieldNameFromUnQuoted('dep_id'), GetIdentifierName('department'), True, True);

    case ProtocolType of
      protPostgre:
        CheckEquals(Ord(stInteger), Ord(Metadata.GetColumnType(DEP_ID_Index)), 'ColumnType does not match');
      protOracle:
        CheckEquals(Ord(stBigDecimal), Ord(Metadata.GetColumnType(DEP_ID_Index)), 'ColumnType does not match');
      else
        CheckEquals(Ord(stSmall), Ord(Metadata.GetColumnType(DEP_ID_Index)), 'ColumnType does not match');
    end;

    CheckColumnMetadata(Metadata, DEP_NAME_Index, GetColumnLabeName(DEP_NAME_Index, 'DEP_ID'),
      GetFieldNameFromUnQuoted('dep_name'), GetIdentifierName('department'), False, True);
    Check(Metadata.GetColumnType(DEP_NAME_Index) in [stString, stUnicodeString], 'ColumnType does not match');

    CheckColumnMetadata(Metadata, DEP_SHNAME_Index, GetColumnLabeName(DEP_SHNAME_Index, 'DEP_ADDRESS') ,
      GetFieldNameFromUnQuoted('dep_shname'), GetIdentifierName('department'), False, True);
    Check(Metadata.GetColumnType(DEP_SHNAME_Index) in [stString, stUnicodeString], 'ColumnType does not match');

    CheckColumnMetadata(Metadata, DEP_ADDRESS_Index, GetColumnLabeName(DEP_ADDRESS_Index, 'DEP_ADDRESS_1'), '', '',
      False, False);
  end;
begin
  Statement := Connection.CreateStatement;
  Check(Statement <> nil);
  if Pos('Sybase', Connection.GetIZPlainDriver.GetDescription) > 0 then
    Exit; //Sybase simply does not allow duplicate columnlabel
  ResultSet := Statement.ExecuteQuery('SELECT T.dep_id AS DEP_NAME, T.dep_name AS DEP_ID,'
    + ' T.dep_shname as DEP_ADDRESS, 2+2 AS DEP_ADDRESS FROM department T WHERE T.dep_id < 100');
  Metadata := ResultSet.GetMetadata;
  TestAll;

  ResultSet := Statement.ExecuteQuery('SELECT dep_id AS DEP_NAME, dep_name AS DEP_ID,'
    + ' dep_shname as DEP_ADDRESS, 2+2 AS DEP_ADDRESS FROM department WHERE dep_id < 100');
  Metadata := ResultSet.GetMetadata;
  TestAll;
end;

procedure TZTestResultSetMetadataCase.TestDefault_values2JoinsDefault_valuesLower;
const
  Field1 = FirstDbcIndex;
  Field2 = FirstDbcIndex +1;
  Field3 = FirstDbcIndex +2;
  Field4 = FirstDbcIndex +3;
  Field5 = FirstDbcIndex +4;
  Field6 = FirstDbcIndex +5;
  Field7 = FirstDbcIndex +6;
  Field8 = FirstDbcIndex +7;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  Statement := Connection.CreateStatement;

  ResultSet := Statement.ExecuteQuery('SELECT dv2.d_fld1, dv1.d_fld1, dv2.d_fld2,'
    + ' dv1.d_fld2, dv2.d_fld3, dv1.d_fld3, dv2.d_fld4, dv1.d_fld4'
    +' from default_values dv1 left join default_values2 dv2 on dv1.d_id = dv2.d_id');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(8, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, Field1, GetColumnLabelFromUnQuoted('d_fld1',0),
    GetFieldNameFromUnQuoted('d_fld1'), GetFieldNameFromUnQuoted('default_values2'), False, True);
  Check(Metadata.GetColumnType(Field1) in [stFloat, stDouble], 'ColumnType does not match');

  CheckColumnMetadata(Metadata, Field2, GetColumnLabelFromUnQuoted('d_fld1', 1),
    GetFieldNameFromUnQuoted('d_fld1'), GetFieldNameFromUnQuoted('default_values'), False, True);
  if ProtocolType = protOracle
    then Check(Metadata.GetColumnType(Field2) in [stBigDecimal], 'ColumnType does not match')
    else Check(Metadata.GetColumnType(Field2) in [stInteger], 'ColumnType does not match');

  CheckColumnMetadata(Metadata, Field3, GetColumnLabelFromUnQuoted('d_fld2', 0),
    GetFieldNameFromUnQuoted('d_fld2'), GetFieldNameFromUnQuoted('default_values2'), False, True);
  if ProtocolType = protOracle
    then Check(Metadata.GetColumnType(Field3) in [stBigDecimal], 'ColumnType does not match')
    else Check(Metadata.GetColumnType(Field3) in [stInteger], 'ColumnType does not match');

  CheckColumnMetadata(Metadata, Field4, GetColumnLabelFromUnQuoted('d_fld2', 1),
    GetFieldNameFromUnQuoted('d_fld2'), GetFieldNameFromUnQuoted('default_values'), False, True);
  Check(Metadata.GetColumnType(Field4) in [stFloat, stDouble], 'ColumnType does not match')
end;

procedure TZTestResultSetMetadataCase.TestDefault_valuesJoinsDefault_values2Lower;
const
  Field1 = FirstDbcIndex;
  Field2 = FirstDbcIndex +1;
  Field3 = FirstDbcIndex +2;
  Field4 = FirstDbcIndex +3;
  Field5 = FirstDbcIndex +4;
  Field6 = FirstDbcIndex +5;
  Field7 = FirstDbcIndex +6;
  Field8 = FirstDbcIndex +7;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  Statement := Connection.CreateStatement;

  ResultSet := Statement.ExecuteQuery('SELECT dv1.d_fld1, dv2.d_fld1, dv1.d_fld2,'
    + ' dv2.d_fld2, dv1.d_fld3, dv2.d_fld3, dv1.d_fld4, dv2.d_fld4'
    + ' from default_values dv1 left join default_values2 dv2 on dv1.d_id = dv2.d_id');
  Metadata := ResultSet.GetMetadata;
  CheckEquals(8, Metadata.GetColumnCount);

  CheckColumnMetadata(Metadata, FirstDbcIndex, GetColumnLabelFromUnQuoted('d_fld1', 0),
    GetFieldNameFromUnQuoted('d_fld1'), GetFieldNameFromUnQuoted('default_values'), False, True);
  if ProtocolType = protOracle
    then Check(Metadata.GetColumnType(Field1) in [stBigDecimal], 'ColumnType does not match')
    else Check(Metadata.GetColumnType(Field1) in [stInteger], 'ColumnType does not match');

  CheckColumnMetadata(Metadata, Field2, GetColumnLabelFromUnQuoted('d_fld1', 1),
    GetFieldNameFromUnQuoted('d_fld1'), GetFieldNameFromUnQuoted('default_values2'), False, True);
  Check(Metadata.GetColumnType(Field2) in [stFloat, stDouble], 'ColumnType does not match');

  CheckColumnMetadata(Metadata, Field3, GetColumnLabelFromUnQuoted('d_fld2', 0),
    GetFieldNameFromUnQuoted('d_fld2'), GetFieldNameFromUnQuoted('default_values'), False, True);
  Check(Metadata.GetColumnType(Field3) in [stFloat, stDouble], 'ColumnType does not match');

  CheckColumnMetadata(Metadata, Field4, GetColumnLabelFromUnQuoted('d_fld2', 1),
    GetFieldNameFromUnQuoted('d_fld2'), GetFieldNameFromUnQuoted('default_values2'), False, True);
  if ProtocolType = protOracle
    then Check(Metadata.GetColumnType(Field4) in [stBigDecimal], 'ColumnType does not match')
    else Check(Metadata.GetColumnType(Field4) in [stInteger], 'ColumnType does not match');
end;

initialization
  RegisterTest('dbc',TZTestResultSetMetadataCase.Suite);
end.
