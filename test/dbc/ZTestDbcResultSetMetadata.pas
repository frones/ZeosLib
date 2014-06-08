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
  ZSqlTestCase, ZDbcIntfs, ZClasses;

type

 {** Implements a test case for TZAbstractBlob. }
  TZTestResultSetMetadataCase = class(TZAbstractDbcSQLTestCase)
  private
  protected
    procedure CheckColumnMetadata(Metadata: IZResultSetMetadata;
      ColumnIndex: Integer; ColumnLabel, ColumnName, ColumnTable: string;
      IsAutoIncrement, IsWritable: Boolean);
  published
    procedure TestResultSetMetadata;
    procedure TestResultSetMetadata1;
  end;

implementation

uses ZSysUtils;

{ TZTestResultSetMetadataCase }

{**
  Checks metadata for one single resultset column.
}
procedure TZTestResultSetMetadataCase.CheckColumnMetadata(
  Metadata: IZResultSetMetadata; ColumnIndex: Integer; ColumnLabel,
  ColumnName, ColumnTable: string; IsAutoIncrement, IsWritable: Boolean);
begin
  CheckEquals(ColumnLabel, Metadata.GetColumnLabel(ColumnIndex));
  CheckEquals(ColumnName, Metadata.GetColumnName(ColumnIndex));
  CheckEquals(ColumnTable, Metadata.GetTableName(ColumnIndex));
//  CheckEquals(IsAutoIncrement, Metadata.IsAutoIncrement(ColumnIndex));
  CheckEquals(IsWritable, Metadata.IsWritable(ColumnIndex));
  CheckEquals(IsWritable, Metadata.IsDefinitelyWritable(ColumnIndex));
end;

{**
  Runs a test for resultset metadata.
}
procedure TZTestResultSetMetadataCase.TestResultSetMetadata;
const
  department_dep_id_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  department_dep_name_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  department_dep_shname_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  department_dep_address_Index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  department_computed_col1_Index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  department_computed_col2_Index = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  if StartsWith(Protocol, 'interbase')
    or StartsWith(Protocol, 'firebird')
    or StartsWith(Protocol, 'oracle') then
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
  DEP_ID_Index = {$IFDEF GENERIC_INDEX}0{$ELSE}1{$ENDIF};
  DEP_NAME_Index = {$IFDEF GENERIC_INDEX}1{$ELSE}2{$ENDIF};
  DEP_SHNAME_Index = {$IFDEF GENERIC_INDEX}2{$ELSE}3{$ENDIF};
  DEP_ADDRESS_Index = {$IFDEF GENERIC_INDEX}3{$ELSE}4{$ENDIF};
  DEP_ADDRESS_1_Index = {$IFDEF GENERIC_INDEX}4{$ELSE}5{$ENDIF};
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
begin
  if not StartsWith(Protocol, 'interbase')
     and not StartsWith(Protocol,'firebird')
     and not StartsWith(Protocol, 'oracle') then
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

initialization
  RegisterTest('dbc',TZTestResultSetMetadataCase.Suite);
end.
