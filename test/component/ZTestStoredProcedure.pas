{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Query Components               }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZTestStoredProcedure;

interface
{$I ZComponent.inc}

uses
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, Db, ZSqlStrings, SysUtils, ZTokenizer, ZGenericSqlToken,
  ZConnection, ZDataset, ZTestDefinitions, ZStoredProcedure;

type
  {** Implements a generic test case for class TZStoredProc. }
  TZTestStoredProcedure = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
    StoredProc: TZStoredProc;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetConnectionUrl: string;
  end;

  {** Implements a protocol specific test case for class TZStoredProc. }
  TZTestStoredProcedureSpecific = class(TZComponentSpecificSQLTestCase)
  private
    Connection: TZConnection;
    StoredProc: TZStoredProc;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetConnectionUrl: string;
  end;

  {** Implements a test case for class TZStoredProc. }
  TZTestInterbaseStoredProcedure = class(TZTestStoredProcedureSpecific)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestStoredProc;
    procedure Test_abtest;
  end;


  {** Implements a test case for class TZStoredProc. }
  TZTestDbLibStoredProcedure = class(TZTestStoredProcedureSpecific)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure TestStoredProc;
  end;

  {** Impleme nts a test case for class TZStoredProc. }
  TZTestPostgreSQLStoredProcedure = class(TZTestStoredProcedureSpecific)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test_abtest;
    procedure Test_nonames;
    procedure Test_onename;
    procedure Test_noout;
    procedure Test_composite;
    procedure Test_mixedorder;
    procedure Test_set;
  end;

  {** Implements a test case for class TZStoredProc. }
  TZTestMySQLStoredProcedure = class(TZTestStoredProcedureSpecific)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test_abtest;
    procedure Test_TEST_All_TYPES;
    procedure Test_FuncReturnInteger;
    procedure MultipleVaryingResultSets;
  end;


  {** Implements a test case for class TZStoredProc. }
  TZTestADOStoredProcedure = class(TZTestStoredProcedureSpecific)
  protected
    function GetSupportedProtocols: string; override;
  published
    procedure Test_abtest;
  end;

  {** Implements a test case for class TZStoredProc. }

  { TZTestOracleStoredProcedure }

  TZTestOracleStoredProcedure = class(TZTestStoredProcedureSpecific)
  protected
    function GetSupportedProtocols: string; override;
    procedure abtest(prefix:string ='');
    procedure myfuncInOutReturn(prefix:string ='');
    procedure simple_func(prefix:string ='');
    procedure simplefunc(prefix:string ='');
  published
    procedure Test_abtest;
    procedure Test_myfuncInOutReturn;
    procedure Test_simple_func;
    procedure Test_simplefunc;
    procedure Test_packaged;
    procedure Test_MYPACKAGE;
  end;

implementation

uses Classes, ZSysUtils, ZDbcUtils, ZTestConsts, ZDbcIntfs, ZAbstractDataset,
  ZTestCase, ZCompatibility, ZVariant, Types;


{ TZTestStoredProcedure }

{**
  Prepares initial data before each test.
}
procedure TZTestStoredProcedure.SetUp;
begin
  Connection := CreateDatasetConnection;
  Connection.Connect;
  StoredProc := TZStoredProc.Create(nil);
  StoredProc.Connection := Connection;
  StoredProc.ParamCheck := True;
end;

{**
  Removes data after each test.
}
procedure TZTestStoredProcedure.TearDown;
begin
  StoredProc.Close;
  StoredProc.Free;
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Gets a connection URL string.
  @return a built connection URL string.
}
function TZTestStoredProcedure.GetConnectionUrl: string;
var
  TempProperties :TStrings;
  I: Integer;
begin
  TempProperties := TStringList.Create;
  for I := 0 to High(Properties) do
  begin
    TempProperties.Add(Properties[I])
  end;
  Result := DriverManager.ConstructURL(Protocol, HostName, Database,
  UserName, Password, Port, TempProperties);
  TempProperties.Free;
end;

{ TZTestStoredProcedureSpecific }

{**
  Prepares initial data before each test.
}
procedure TZTestStoredProcedureSpecific.SetUp;
begin
  Connection := CreateDatasetConnection;
  Connection.Connect;
  StoredProc := TZStoredProc.Create(nil);
  StoredProc.Connection := Connection;
  StoredProc.ParamCheck := True;
end;

{**
  Removes data after each test.
}
procedure TZTestStoredProcedureSpecific.TearDown;
begin
  StoredProc.Close;
  StoredProc.Free;
  Connection.Disconnect;
  Connection.Free;
end;

{**
  Gets a connection URL string.
  @return a built connection URL string.
}
function TZTestStoredProcedureSpecific.GetConnectionUrl: string;
var
  TempProperties :TStrings;
  I: Integer;
begin
  TempProperties := TStringList.Create;
  for I := 0 to High(Properties) do
  begin
    TempProperties.Add(Properties[I])
  end;
  Result := DriverManager.ConstructURL(Protocol, HostName, Database,
  UserName, Password, Port, TempProperties);
  TempProperties.Free;
end;

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestInterbaseStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'interbase,interbase-6.5,interbase-7.2,firebird-1.0,firebird-1.5,'+
    'firebird-2.0,firebird-2.1,firebird-2.5,firebirdd-1.5,firebirdd-2.0,'+
    'firebirdd-2.1,firebirdd-2.5';
end;

{**
  Gets a connection URL string.
  @return a built connection URL string.
}
{**
   Testing executil stored procedures
}
procedure TZTestInterbaseStoredProcedure.TestStoredProc;
begin
  StoredProc.StoredProcName := 'PROCEDURE1';

  CheckEquals(2, StoredProc.Params.Count);
  CheckEquals('P1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('R1', StoredProc.Params[1].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[1].ParamType));
  StoredProc.ParamByName('P1').AsInteger := 12345;
  StoredProc.ExecProc;
  CheckEquals(12346, StoredProc.ParamByName('R1').AsInteger);
  CheckEquals(2, StoredProc.Params.Count);
end;

{**
   Testing executil stored procedures
}
procedure TZTestInterbaseStoredProcedure.Test_abtest;
var
  i, P2: integer;
  S: String;
begin
  StoredProc.StoredProcName := 'ABTEST';
  CheckEquals(5, StoredProc.Params.Count);
  CheckEquals('P1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('P2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('P3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  CheckEquals('P4', StoredProc.Params[3].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[3].ParamType));
  CheckEquals('P5', StoredProc.Params[4].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[4].ParamType));

  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  StoredProc.ExecProc;
  CheckEquals(600, StoredProc.ParamByName('P4').AsInteger);
  CheckEquals('aa', StoredProc.ParamByName('P5').AsString);
  CheckEquals(5, StoredProc.Params.Count);

  StoredProc.Prepare;
  S := 'a';
  P2 := 100;
  for i:= 1 to 100 do
  begin
    StoredProc.Params[0].AsInteger:= i;
    StoredProc.Params[1].AsInteger:= P2;
    StoredProc.Params[2].AsString:= S;
    StoredProc.ExecProc;
    CheckEquals(S+S, StoredProc.ParamByName('P5').AsString);
    CheckEquals(I*10+P2, StoredProc.ParamByName('P4').AsInteger);
    if Length(S) = 10 then s := 'a'
    else S := S+'a';
    P2 := 100 - I;
  end;
  StoredProc.Unprepare;
  S := StoredProc.ParamByName('P4').AsString +
    ' ' + StoredProc.ParamByName('P5').AsString;
  StoredProc.Open;
  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  StoredProc.Open;
end;

{ TZTestDbLibStoredProcedure }

{**
  Gets an array of protocols valid for this test.
  @return an array of valid protocols
}
function TZTestDbLibStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'sybase, mssql';
end;

{**
   Testing executil stored procedures
}
procedure TZTestDbLibStoredProcedure.TestStoredProc;
begin
  StoredProc.StoredProcName := 'procedure1';

  CheckEquals(3, StoredProc.Params.Count);
  CheckEquals('@RETURN_VALUE', StoredProc.Params[0].Name);
  CheckEquals('@p1', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('@r1', StoredProc.Params[2].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));

  StoredProc.Params[1].AsInteger := 12345;
  StoredProc.ExecProc;
  CheckEquals(12346, StoredProc.Params[1].AsInteger);
  CheckEquals(2, StoredProc.Params.Count);
end;

{ TZTestPosgreSQLStoredProcedure }

{**
  Prepares initial data before each test.
}
function TZTestPostgreSQLStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'postgresql,postgresql-7,postgresql-8,postgresql-9';
end;

{**
   Testing executil stored procedures
}
procedure TZTestPostgreSQLStoredProcedure.Test_abtest;
var
  i: integer;
begin
  StoredProc.StoredProcName := '"ABTEST"';
  CheckEquals(5, StoredProc.Params.Count);
  CheckEquals('p1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('p2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('p3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  CheckEquals('p4', StoredProc.Params[3].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[3].ParamType));
  CheckEquals('p5', StoredProc.Params[4].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[4].ParamType));

  StoredProc.ParamByName('p1').AsInteger := 50;
  StoredProc.ParamByName('p2').AsInteger := 100;
  StoredProc.ParamByName('p3').AsString := 'a';
  StoredProc.ExecProc;
  CheckEquals(600, StoredProc.ParamByName('p4').AsInteger);
  CheckEquals('aa', StoredProc.ParamByName('p5').AsString);
  CheckEquals(5, StoredProc.Params.Count);

  StoredProc.Prepare;
  for i:= 0 to 99 do
  begin
    StoredProc.Params[0].AsInteger:= i;
    StoredProc.Params[1].AsInteger:= 100;
    StoredProc.Params[2].AsString:= 'a';
    StoredProc.ExecProc;
  end;
  StoredProc.Unprepare;
  StoredProc.Open;
  StoredProc.ParamByName('p1').AsInteger := 50;
  StoredProc.ParamByName('p2').AsInteger := 100;
  StoredProc.ParamByName('p3').AsString := 'a';
  StoredProc.Open;
end;

procedure TZTestPostgreSQLStoredProcedure.Test_composite;
begin
  StoredProc.StoredProcName := 'proc_composite';
  CheckEquals(4, StoredProc.Params.Count);
  CheckEquals('p1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('p2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('f1', StoredProc.Params[2].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[2].ParamType));
  CheckEquals('f2', StoredProc.Params[3].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[3].ParamType));

  StoredProc.ParamByName('p1').AsInteger := 50;
  StoredProc.ParamByName('p2').AsInteger := 100;
  StoredProc.ExecProc;
  CheckEquals(50, StoredProc.ParamByName('f1').AsInteger);
  CheckEquals(100, StoredProc.ParamByName('f2').AsInteger);
  StoredProc.Unprepare;

  StoredProc.ParamByName('p1').AsInteger := 20;
  StoredProc.ParamByName('p2').AsInteger := 30;
  StoredProc.Open;
  CheckEquals(20, StoredProc.ParamByName('f1').AsInteger);
  CheckEquals(30, StoredProc.ParamByName('f2').AsInteger);
  CheckEquals(2, StoredProc.FieldCount);
  CheckEquals(20, StoredProc.Fields[0].AsInteger);
  CheckEquals(30, StoredProc.Fields[1].AsInteger);
end;

procedure TZTestPostgreSQLStoredProcedure.Test_mixedorder;
begin
  StoredProc.StoredProcName := 'proc_mixedorder';
  CheckEquals(3, StoredProc.Params.Count);
  CheckEquals('p1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('p2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('p3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));

  StoredProc.ParamByName('p2').AsInteger := 50;
  StoredProc.ParamByName('p3').AsInteger := 100;
  StoredProc.ExecProc;
  CheckEquals(150, StoredProc.ParamByName('p1').AsInteger);
  CheckEquals(5000, StoredProc.ParamByName('p2').AsInteger);
  StoredProc.Unprepare;

  StoredProc.ParamByName('p2').AsInteger := 20;
  StoredProc.ParamByName('p3').AsInteger := 30;
  StoredProc.Open;
  CheckEquals(50, StoredProc.ParamByName('p1').AsInteger);
  CheckEquals(600, StoredProc.ParamByName('p2').AsInteger);
  CheckEquals(2, StoredProc.FieldCount);
  CheckEquals(50, StoredProc.Fields[0].AsInteger);
  CheckEquals(600, StoredProc.Fields[1].AsInteger);
end;

procedure TZTestPostgreSQLStoredProcedure.Test_nonames;
begin
  StoredProc.StoredProcName := 'proc_nonames';
  CheckEquals(3, StoredProc.Params.Count);
  CheckEquals('$1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('$2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('$3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[2].ParamType));

  StoredProc.ParamByName('$1').AsInteger := 50;
  StoredProc.ParamByName('$2').AsInteger := 100;
  StoredProc.ExecProc;
  CheckEquals(150, StoredProc.ParamByName('$3').AsInteger);
  StoredProc.Unprepare;

  StoredProc.ParamByName('$1').AsInteger := 20;
  StoredProc.ParamByName('$2').AsInteger := 30;
  StoredProc.Open;
  CheckEquals(50, StoredProc.ParamByName('$3').AsInteger);
  CheckEquals(1, StoredProc.FieldCount);
  CheckEquals(50, StoredProc.Fields[0].AsInteger);
end;

procedure TZTestPostgreSQLStoredProcedure.Test_noout;
begin
  StoredProc.StoredProcName := 'proc_noout';
  CheckEquals(3, StoredProc.Params.Count);
  CheckEquals('p1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('returnValue', StoredProc.Params[2].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[2].ParamType));

  StoredProc.ParamByName('p1').AsInteger := 50;
  StoredProc.Params[1].AsInteger := 100;
  StoredProc.ExecProc;
  CheckEquals(150, StoredProc.Params[2].AsInteger);
  StoredProc.Unprepare;

  StoredProc.ParamByName('p1').AsInteger := 20;
  StoredProc.Params[1].AsInteger := 30;
  StoredProc.Open;
  CheckEquals(50, StoredProc.Params[2].AsInteger);
  CheckEquals(1, StoredProc.FieldCount);
  CheckEquals(50, StoredProc.Fields[0].AsInteger);
end;

procedure TZTestPostgreSQLStoredProcedure.Test_onename;
begin
  StoredProc.StoredProcName := 'proc_onename';
  CheckEquals(3, StoredProc.Params.Count);
  CheckEquals('p1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals('', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('', StoredProc.Params[2].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[2].ParamType));

  StoredProc.ParamByName('p1').AsInteger := 50;
  StoredProc.Params[1].AsInteger := 100;
  StoredProc.ExecProc;
  CheckEquals(150, StoredProc.Params[2].AsInteger);
  StoredProc.Unprepare;

  StoredProc.ParamByName('p1').AsInteger := 20;
  StoredProc.Params[1].AsInteger := 30;
  StoredProc.Open;
  CheckEquals(50, StoredProc.Params[2].AsInteger);
  CheckEquals(1, StoredProc.FieldCount);
  CheckEquals(50, StoredProc.Fields[0].AsInteger);
end;

procedure TZTestPostgreSQLStoredProcedure.Test_set;
begin
  StoredProc.StoredProcName := 'proc_set';
  CheckEquals(1, StoredProc.Params.Count);
  CheckEquals('returnValue', StoredProc.Params[0].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));

  StoredProc.ExecProc;
  CheckEquals('Volvo', StoredProc.ParamByName('returnValue').AsString);
  StoredProc.Unprepare;

  StoredProc.Open;
  CheckEquals('Volvo', StoredProc.ParamByName('returnValue').AsString);
  CheckEquals(1, StoredProc.FieldCount);
  CheckEquals('Volvo', StoredProc.Fields[0].AsString);
  StoredProc.Next;
  CheckEquals('Laboratoy', StoredProc.Fields[0].AsString);
end;

{ TZTestMySQLStoredProcedure }
function TZTestMySQLStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'mysql,mysql-4.1,mysql-5,mysqld-4.1,mysqld-5';
end;

procedure TZTestMySQLStoredProcedure.Test_abtest;
var
  i, P2: integer;
  S: String;
begin
  StoredProc.StoredProcName := 'ABTEST';
  CheckEquals(5, StoredProc.Params.Count);
  CheckEquals('P1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));
  CheckEquals('P2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[1].DataType));
  CheckEquals('P3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[2].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[2].DataType));
  CheckEquals('P4', StoredProc.Params[3].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[3].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[3].DataType));
  CheckEquals('P5', StoredProc.Params[4].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[4].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));

  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  StoredProc.ExecProc;
  CheckEquals(600, StoredProc.ParamByName('P4').AsInteger);
  CheckEquals('aa', StoredProc.ParamByName('P5').AsString);
  CheckEquals(5, StoredProc.Params.Count);

  CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[1].DataType));
  {$IFDEF DELPHI12_UP}
  CheckEquals(ord(ftWideString), ord(StoredProc.Params[2].DataType));
  {$ELSE}
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[2].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[2].DataType));
  {$ENDIF}
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[3].DataType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));

  StoredProc.Prepare;
  S := 'a';
  P2 := 100;
  for i:= 1 to 100 do
  begin
    StoredProc.Params[0].AsInteger:= i;
    StoredProc.Params[1].AsInteger:= P2;
    StoredProc.Params[2].AsString:= S;
    StoredProc.ExecProc;
    CheckEquals(S+S, StoredProc.ParamByName('P5').AsString);
    CheckEquals(I*10+P2, StoredProc.ParamByName('P4').AsInteger);
    if Length(S) = 10 then s := 'a'
    else S := S+'a';
    P2 := 100 - I;
  end;
  StoredProc.Unprepare;
  S := StoredProc.ParamByName('P4').AsString +
    ' ' + StoredProc.ParamByName('P5').AsString;
  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  CheckEquals('P4', StoredProc.Params[3].Name);
  CheckEquals('P5', StoredProc.Params[4].Name);
  StoredProc.Open;

  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Fields[1].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Fields[1].DataType));
  CheckEquals(2, ord(StoredProc.Fields.Count));

  CheckEquals(ord(ftLargeint), ord(StoredProc.Fields[0].DataType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));
end;

procedure TZTestMySQLStoredProcedure.Test_TEST_All_TYPES;
const Str1 = 'צהüךבאüהצ';
var
  SQLTime: TDateTime;
  TempBytes: TByteDynArray;
begin
  StoredProc.StoredProcName := 'TEST_All_TYPES';
  CheckEquals(28, StoredProc.Params.Count);

  CheckEquals('P1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[0].ParamType));
  CheckEquals(ord(ftSmallInt), ord(StoredProc.Params[0].DataType));

  CheckEquals('P2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[1].ParamType));
  CheckEquals(ord(ftSmallInt), ord(StoredProc.Params[1].DataType));

  CheckEquals('P3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[2].ParamType));
  CheckEquals(ord(ftSmallInt), ord(StoredProc.Params[2].DataType));

  CheckEquals('P4', StoredProc.Params[3].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[3].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[3].DataType));

  CheckEquals('P5', StoredProc.Params[4].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[4].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[4].DataType));

  CheckEquals('P6', StoredProc.Params[5].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[5].ParamType));
  CheckEquals(ord(ftLargeInt), ord(StoredProc.Params[5].DataType));

  CheckEquals('P7', StoredProc.Params[6].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[6].ParamType));
  CheckEquals(ord(ftFloat), ord(StoredProc.Params[6].DataType));

  CheckEquals('P8', StoredProc.Params[7].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[7].ParamType));
  CheckEquals(ord(ftFloat), ord(StoredProc.Params[7].DataType));

  CheckEquals('P9', StoredProc.Params[8].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[8].ParamType));
  CheckEquals(ord(ftFloat), ord(StoredProc.Params[8].DataType));

  CheckEquals('P10', StoredProc.Params[9].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[9].ParamType));
  CheckEquals(ord(ftLargeInt), ord(StoredProc.Params[9].DataType));

  CheckEquals('P11', StoredProc.Params[10].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[10].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[10].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[10].DataType));

  CheckEquals('P12', StoredProc.Params[11].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[11].ParamType));
  CheckEquals(ord(ftDate), ord(StoredProc.Params[11].DataType));

  CheckEquals('P13', StoredProc.Params[12].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[12].ParamType));
  CheckEquals(ord(ftTime), ord(StoredProc.Params[12].DataType));

  CheckEquals('P14', StoredProc.Params[13].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[13].ParamType));
  CheckEquals(ord(ftSmallInt), ord(StoredProc.Params[13].DataType));

  CheckEquals('P15', StoredProc.Params[14].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[14].ParamType));
  CheckEquals(ord(ftDateTime), ord(StoredProc.Params[14].DataType));

  CheckEquals('P16', StoredProc.Params[15].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[15].ParamType));
  CheckEquals(ord(ftDateTime), ord(StoredProc.Params[15].DataType));

  CheckEquals('P17', StoredProc.Params[16].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[16].ParamType));
  CheckEquals(ord(ftBlob), ord(StoredProc.Params[16].DataType));

  CheckEquals('P18', StoredProc.Params[17].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[17].ParamType));
  CheckEquals(ord(ftBlob), ord(StoredProc.Params[17].DataType));

  CheckEquals('P19', StoredProc.Params[18].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[18].ParamType));
  CheckEquals(ord(ftBlob), ord(StoredProc.Params[18].DataType));

  CheckEquals('P20', StoredProc.Params[19].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[19].ParamType));
  CheckEquals(ord(ftBlob), ord(StoredProc.Params[19].DataType));

  CheckEquals('P21', StoredProc.Params[20].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[20].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Params[20].DataType))
  else
    CheckEquals(ord(ftMemo), ord(StoredProc.Params[20].DataType));

  CheckEquals('P22', StoredProc.Params[21].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[21].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Params[21].DataType))
  else
    CheckEquals(ord(ftMemo), ord(StoredProc.Params[21].DataType));

  CheckEquals('P23', StoredProc.Params[22].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[22].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Params[22].DataType))
  else
    CheckEquals(ord(ftMemo), ord(StoredProc.Params[22].DataType));

  CheckEquals('P24', StoredProc.Params[23].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[23].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Params[23].DataType))
  else
    CheckEquals(ord(ftMemo), ord(StoredProc.Params[23].DataType));

  CheckEquals('P25', StoredProc.Params[24].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[24].ParamType));
  CheckEquals(ord(ftBytes), ord(StoredProc.Params[24].DataType));

  CheckEquals('P26', StoredProc.Params[25].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[25].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord({$IFDEF WITH_FTWIDESTRING}ftWideString{$ELSE}ftString{$ENDIF}), ord(StoredProc.Params[25].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[25].DataType));

  CheckEquals('P27', StoredProc.Params[26].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[26].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[26].DataType));

  CheckEquals('P28', StoredProc.Params[27].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[27].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[27].DataType));

  StoredProc.Params[0].AsSmallInt := 10;
  StoredProc.Params[1].AsSmallInt := 20;
  StoredProc.Params[2].AsSmallInt := 30;
  StoredProc.Params[3].AsInteger := 1000;
  StoredProc.Params[4].AsInteger := 2000;
  StoredProc.Params[5].AsInteger := 30000;
  SQLTime := now;
  StoredProc.Params[6].AsFloat := SQLTime;
  StoredProc.Params[7].AsFloat := SQLTime;
  StoredProc.Params[8].AsFloat := SQLTime;
  StoredProc.Params[9].AsInteger := 40000;
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    StoredProc.Params[10].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := {$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
      StoredProc.Params[10].AsString := UTF8Encode(WideString(Str1))
    else
      StoredProc.Params[10].AsString := AnsiString(Str1);
  StoredProc.Params[11].AsDate := SQLTime;
  StoredProc.Params[12].AsTime := SQLTime;
  StoredProc.Params[13].AsSmallInt := 40;
  StoredProc.Params[14].AsDateTime := SQLTime;
  StoredProc.Params[15].AsDateTime := SQLTime;
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    StoredProc.Params[20].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := {$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
      StoredProc.Params[20].AsString := UTF8Encode(WideString(Str1))
    else
      StoredProc.Params[20].AsString := AnsiString(Str1);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    StoredProc.Params[21].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := {$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
      StoredProc.Params[21].AsString := UTF8Encode(WideString(Str1))
    else
      StoredProc.Params[21].AsString := AnsiString(Str1);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    StoredProc.Params[22].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := {$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
      StoredProc.Params[22].AsString := UTF8Encode(WideString(Str1))
    else
      StoredProc.Params[22].AsString := AnsiString(Str1);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    StoredProc.Params[23].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF} := {$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
      StoredProc.Params[23].AsString := UTF8Encode(WideString(Str1))
    else
      StoredProc.Params[23].AsString := AnsiString(Str1);
  StoredProc.Params[24].Value := StrToBytes('121415');
  StoredProc.Params[25].AsString := 'a';
  StoredProc.Params[26].AsInteger := 50000;
  StoredProc.Params[27].AsInteger := 60000;
  StoredProc.ExecProc;
  CheckEquals(28, StoredProc.Params.Count);
  StoredProc.Open;
  CheckEquals(28, StoredProc.Fields.Count);

  CheckEquals('P1', StoredProc.Fields[0].DisplayName);
  CheckEquals(10, StoredProc.Fields[0].AsInteger);
  //CheckEquals(ord(ftSmallint), ord(StoredProc.Fields[0].DataType));

  CheckEquals('P2', StoredProc.Fields[1].DisplayName);
  CheckEquals(20, StoredProc.Fields[1].AsInteger);
  //CheckEquals(ord(ftSmallint), ord(StoredProc.Fields[1].DataType));

  CheckEquals('P3', StoredProc.Fields[2].DisplayName);
  CheckEquals(30, StoredProc.Fields[2].AsInteger);
  //CheckEquals(ord(ftSmallint), ord(StoredProc.Fields[2].DataType));

  CheckEquals('P4', StoredProc.Fields[3].DisplayName);
  CheckEquals(1000, StoredProc.Fields[3].AsInteger);
  //CheckEquals(ord(ftInteger), ord(StoredProc.Fields[3].DataType));

  CheckEquals('P5', StoredProc.Fields[4].DisplayName);
  CheckEquals(2000, StoredProc.Fields[4].AsInteger);
  //CheckEquals(ord(ftInteger), ord(StoredProc.Fields[4].DataType));

  CheckEquals('P6', StoredProc.Fields[5].DisplayName);
  CheckEquals(30000, StoredProc.Fields[5].AsInteger);
  CheckEquals(ord(ftLargeInt), ord(StoredProc.Fields[5].DataType));

  CheckEquals('P7', StoredProc.Fields[6].DisplayName);
  CheckEquals(True, Abs(SQLTime - StoredProc.Fields[6].AsFloat) < FLOAT_COMPARE_PRECISION);
  CheckEquals(ord(ftFloat), ord(StoredProc.Fields[6].DataType));

  CheckEquals('P8', StoredProc.Fields[7].DisplayName);
  //CheckEquals(True, Abs(SQLTime - StoredProc.Fields[7].AsFloat) < FLOAT_COMPARE_PRECISION_SINGLE);
  CheckEquals(ord(ftFloat), ord(StoredProc.Fields[7].DataType));

  CheckEquals('P9', StoredProc.Fields[8].DisplayName);
  //CheckEquals(SQLTime, StoredProc.Fields[8].AsFloat);
  CheckEquals(ord(ftFloat), ord(StoredProc.Fields[8].DataType));

  CheckEquals('P10', StoredProc.Fields[9].DisplayName);
  CheckEquals(40000, StoredProc.Fields[9].AsInteger);
  CheckEquals(ord(ftFloat), ord(StoredProc.Fields[9].DataType));

  CheckEquals('P11', StoredProc.Fields[10].DisplayName);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
  begin
    CheckEquals({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1), StoredProc.Fields[10].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF});
    CheckEquals(ord({$IFDEF WITH_FTWIDESTRING}ftWideString{$ELSE}ftString{$ENDIF}), ord(StoredProc.Fields[10].DataType));
  end
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
    begin
      CheckEquals(UTF8Encode({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)), StoredProc.Fields[10].AsString);
      CheckEquals(ord(ftString), ord(StoredProc.Fields[10].DataType));
    end
    else
    begin
      CheckEquals(Str1, StoredProc.Fields[10].AsString);
      CheckEquals(ord(ftString), ord(StoredProc.Fields[10].DataType));
    end;

  CheckEquals('P12', StoredProc.Fields[11].DisplayName);
  CheckEquals(Int(SQLTime), StoredProc.Fields[11].AsDateTime);
  CheckEquals(ord(ftDate), ord(StoredProc.Fields[11].DataType));

  CheckEquals('P13', StoredProc.Fields[12].DisplayName);
  CheckEquals(StrToTime(TimeToStr(SQLTime)), StoredProc.Fields[12].AsDateTime);
  CheckEquals(ord(ftTime), ord(StoredProc.Fields[12].DataType));

  CheckEquals('P14', StoredProc.Fields[13].DisplayName);
  CheckEquals(2040, StoredProc.Fields[13].AsInteger);
  CheckEquals(ord(ftLargeInt), ord(StoredProc.Fields[13].DataType));

  CheckEquals('P15', StoredProc.Fields[14].DisplayName);
  CheckEquals(DateTimeToStr(SQLTime), DateTimeToStr(StoredProc.Fields[14].AsDateTime));
  CheckEquals(ord(ftDateTime), ord(StoredProc.Fields[14].DataType));

  CheckEquals('P16', StoredProc.Fields[15].DisplayName);
  CheckEquals(DateTimeToStr(SQLTime), DateTimeToStr(StoredProc.Fields[15].AsDateTime));
  CheckEquals(ord(ftDateTime), ord(StoredProc.Fields[15].DataType));

  CheckEquals('P17', StoredProc.Fields[16].DisplayName);
  //CheckEquals(DateTimeToStr(SQLTime), DateTimeToStr(StoredProc.Fields[16].AsDateTime));
  CheckEquals(ord(ftBlob), ord(StoredProc.Fields[16].DataType));

  CheckEquals('P18', StoredProc.Fields[17].DisplayName);
  //CheckEquals(DateTimeToStr(SQLTime), DateTimeToStr(StoredProc.Fields[17].AsDateTime));
  CheckEquals(ord(ftBlob), ord(StoredProc.Fields[17].DataType));

  CheckEquals('P19', StoredProc.Fields[18].DisplayName);
  //CheckEquals(DateTimeToStr(SQLTime), DateTimeToStr(StoredProc.Fields[18].AsDateTime));
  CheckEquals(ord(ftBlob), ord(StoredProc.Fields[18].DataType));

  CheckEquals('P20', StoredProc.Fields[19].DisplayName);
  //CheckEquals(DateTimeToStr(SQLTime), DateTimeToStr(StoredProc.Fields[19].AsDateTime));
  CheckEquals(ord(ftBlob), ord(StoredProc.Fields[19].DataType));

  CheckEquals('P21', StoredProc.Fields[20].DisplayName);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
  begin
    CheckEquals({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1), StoredProc.Fields[20].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF});
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Fields[20].DataType));
  end
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
    begin
      CheckEquals(UTF8Encode({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)), StoredProc.Fields[20].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[20].DataType));
    end
    else
    begin
      CheckEquals(Str1, StoredProc.Fields[20].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[20].DataType));
    end;


  CheckEquals('P22', StoredProc.Fields[21].DisplayName);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
  begin
    CheckEquals({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1), StoredProc.Fields[21].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF});
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Fields[21].DataType));
  end
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
    begin
      CheckEquals(UTF8Encode({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)), StoredProc.Fields[21].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[21].DataType));
    end
    else
    begin
      CheckEquals(Str1, StoredProc.Fields[21].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[21].DataType));
    end;

  CheckEquals('P23', StoredProc.Fields[22].DisplayName);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
  begin
    CheckEquals({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1), StoredProc.Fields[22].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF});
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Fields[22].DataType));
  end
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
    begin
      CheckEquals(UTF8Encode({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)), StoredProc.Fields[22].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[22].DataType));
    end
    else
    begin
      CheckEquals(Str1, StoredProc.Fields[22].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[22].DataType));
    end;

  CheckEquals('P24', StoredProc.Fields[23].DisplayName);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
  begin
    CheckEquals({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1), StoredProc.Fields[23].{$IFDEF WITH_FTWIDESTRING}AsWideString{$ELSE}AsString{$ENDIF});
    CheckEquals(ord({$IFDEF WITH_WIDEMEMO}ftWideMemo{$ELSE}ftMemo{$ENDIF}), ord(StoredProc.Fields[23].DataType));
  end
  else
    if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) then
    begin
      CheckEquals(UTF8Encode({$IFDEF WITH_FTWIDESTRING}WideString{$ENDIF}(Str1)), StoredProc.Fields[23].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[23].DataType));
    end
    else
    begin
      CheckEquals(Str1, StoredProc.Fields[23].AsString);
      CheckEquals(ord(ftMemo), ord(StoredProc.Fields[23].DataType));
    end;

  CheckEquals('P25', StoredProc.Fields[24].DisplayName);
  TempBytes :=StrToBytes(ZAnsiString('121415'));
  SetLength(TempBytes, StoredProc.Fields[24].Size);
  CheckEquals(TempBytes, {$IFDEF WITH_ASBYTES}TByteDynArray(StoredProc.Fields[24].AsBytes){$ELSE}StrToBytes(StoredProc.Fields[24].AsString){$ENDIF});
  CheckEquals(ord(ftBytes), ord(StoredProc.Fields[24].DataType));

  CheckEquals('P26', StoredProc.Fields[25].DisplayName);
  CheckEquals('a', StoredProc.Fields[25].AsString);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord({$IFDEF WITH_FTWIDESTRING}ftWideString{$ELSE}ftString{$ENDIF}), ord(StoredProc.Fields[25].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Fields[25].DataType));

  CheckEquals('P27', StoredProc.Fields[26].DisplayName);
  CheckEquals(50000, StoredProc.Fields[26].AsInteger);
  //CheckEquals(ord(ftInteger), ord(StoredProc.Fields[26].DataType));

  CheckEquals('P28', StoredProc.Fields[27].DisplayName);
  CheckEquals(60000, StoredProc.Fields[27].AsInteger);
  //CheckEquals(ord(ftInteger), ord(StoredProc.Fields[27].DataType));
end;

procedure TZTestMySQLStoredProcedure.Test_FuncReturnInteger;
begin
  StoredProc.StoredProcName := 'FuncReturnInteger';
  CheckEquals(2, StoredProc.Params.Count);

  CheckEquals('p_in', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Params[0].DataType));

  CheckEquals('ReturnValue', StoredProc.Params[1].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[1].ParamType));
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Params[1].DataType));

  StoredProc.Params[0].AsInteger := 100;
  StoredProc.ExecProc;

  CheckEquals('ReturnValue', StoredProc.Params[1].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[1].ParamType));
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Params[1].DataType));
  CheckEquals(110, StoredProc.Params[1].AsInteger);

  StoredProc.Params[0].AsInteger := 200;
  StoredProc.Open;
  CheckEquals(1, StoredProc.Fields.Count);

  CheckEquals('ReturnValue', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(210, StoredProc.Fields[0].AsInteger);
end;

procedure TZTestMySQLStoredProcedure.MultipleVaryingResultSets;
begin
  StoredProc.StoredProcName := 'MultipleVaryingResultSets';
  CheckEquals(3, StoredProc.Params.Count);

  CheckEquals('p_in', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Params[0].DataType));

  CheckEquals('p_out', StoredProc.Params[1].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[1].ParamType));
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Params[1].DataType));

  CheckEquals('p_inout', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[2].ParamType));
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Params[2].DataType));

  StoredProc.Params[0].AsInteger := 100;
  StoredProc.Params[1].AsInteger := 200;
  StoredProc.Params[2].AsInteger := 300;
  StoredProc.ExecProc;
  StoredProc.Open;

  CheckEquals(True, StoredProc.EOR);
  CheckEquals(False, StoredProc.BOR);

  //5 Resultsets Returned What now?

  //The call resultset is retieved..
  CheckEquals(2, StoredProc.Fields.Count);

  CheckEquals('p_out', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(200, StoredProc.Fields[0].AsInteger);

  CheckEquals('p_inout', StoredProc.Fields[1].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[1].DataType));
  CheckEquals(300, StoredProc.Fields[1].AsInteger);

  {check first resultset of procedure body}
  StoredProc.FirstResultSet;

  CheckEquals(3, StoredProc.Fields.Count);

  CheckEquals('p_in', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(100, StoredProc.Fields[0].AsInteger);

  CheckEquals('p_out', StoredProc.Fields[1].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[1].DataType));
  CheckEquals(0, StoredProc.Fields[1].AsInteger);

  CheckEquals('p_inout', StoredProc.Fields[2].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[2].DataType));
  CheckEquals(300, StoredProc.Fields[2].AsInteger);

  {check second resultset}
  StoredProc.NextResultSet;

  CheckEquals(3, StoredProc.Fields.Count);

  CheckEquals('p_in', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(100, StoredProc.Fields[0].AsInteger);

  CheckEquals('p_out', StoredProc.Fields[1].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[1].DataType));
  CheckEquals(200, StoredProc.Fields[1].AsInteger);

  CheckEquals('p_inout', StoredProc.Fields[2].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[2].DataType));
  CheckEquals(300, StoredProc.Fields[2].AsInteger);

  {check third resultset}
  StoredProc.NextResultSet;

  CheckEquals(2, StoredProc.Fields.Count);

  CheckEquals('p_in', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(100, StoredProc.Fields[0].AsInteger);

  CheckEquals('p_inout', StoredProc.Fields[1].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[1].DataType));
  CheckEquals(300, StoredProc.Fields[1].AsInteger);

  {check fourths resultset}
  StoredProc.NextResultSet;

  CheckEquals(1, StoredProc.Fields.Count);

  CheckEquals('10', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(10, StoredProc.Fields[0].AsInteger);

  CheckEquals(False, StoredProc.EOR);
  CheckEquals(False, StoredProc.BOR);

  {check call resultset again}
  StoredProc.LastResultSet;

  CheckEquals(2, StoredProc.Fields.Count);

  CheckEquals('p_out', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(200, StoredProc.Fields[0].AsInteger);  //these are the paramters. They have been resettet now

  CheckEquals('p_inout', StoredProc.Fields[1].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[1].DataType));
  CheckEquals(300, StoredProc.Fields[1].AsInteger);  //these are the paramters. They have been resettet now

  CheckEquals(True, StoredProc.EOR);
  CheckEquals(False, StoredProc.BOR);

  {check first resultset of procedure body again}
  StoredProc.FirstResultSet;

  CheckEquals(3, StoredProc.Fields.Count);

  CheckEquals('p_in', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(100, StoredProc.Fields[0].AsInteger);

  CheckEquals('p_out', StoredProc.Fields[1].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[1].DataType));
  CheckEquals(0, StoredProc.Fields[1].AsInteger);

  CheckEquals('p_inout', StoredProc.Fields[2].DisplayName);
  CheckEquals(Ord(ftInteger), Ord(StoredProc.Fields[2].DataType));
  CheckEquals(300, StoredProc.Fields[2].AsInteger);

  CheckEquals(False, StoredProc.EOR);
  CheckEquals(True, StoredProc.BOR);

  {check call resultset again}
  StoredProc.LastResultSet;

  CheckEquals(2, StoredProc.Fields.Count);

  CheckEquals('p_out', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(200, StoredProc.Fields[0].AsInteger);

  CheckEquals('p_inout', StoredProc.Fields[1].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[1].DataType));
  CheckEquals(300, StoredProc.Fields[1].AsInteger);

  {check third resultset again}
  StoredProc.PreviousResultSet;

  CheckEquals(1, StoredProc.Fields.Count);

  CheckEquals('10', StoredProc.Fields[0].DisplayName);
  CheckEquals(Ord(ftLargeInt), Ord(StoredProc.Fields[0].DataType));
  CheckEquals(10, StoredProc.Fields[0].AsInteger);

end;

{ TZTestADOStoredProcedure }
function TZTestADOStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'ado';
end;

procedure TZTestADOStoredProcedure.Test_abtest;
var
  i, P2: integer;
  S: String;
begin
  StoredProc.StoredProcName := 'ABTEST';
  CheckEquals(6, StoredProc.Params.Count);
  CheckEquals('@RETURN_VALUE', StoredProc.Params[0].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));
  CheckEquals('@p1', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[1].DataType));
  CheckEquals('@p2', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[2].DataType));
  CheckEquals('@p3', StoredProc.Params[3].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[3].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[3].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[3].DataType));
  CheckEquals('@p4', StoredProc.Params[4].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[4].ParamType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[4].DataType));
  CheckEquals('@p5', StoredProc.Params[5].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[5].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[5].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[5].DataType));

  StoredProc.ParamByName('@p1').AsInteger := 50;
  StoredProc.ParamByName('@p2').AsInteger := 100;
  StoredProc.ParamByName('@p3').AsString := 'a';
  StoredProc.ExecProc;
  CheckEquals(600, StoredProc.ParamByName('@p4').AsInteger);
  CheckEquals('aa', StoredProc.ParamByName('@p5').AsString);
  CheckEquals(6, StoredProc.Params.Count);

  CheckEquals(ord(ftInteger), ord(StoredProc.Params[1].DataType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[2].DataType));
  {$IFDEF DELPHI12_UP}
  CheckEquals(ord(ftWideString), ord(StoredProc.Params[3].DataType));
  {$ELSE}
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[3].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[3].DataType));
  {$ENDIF}
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[4].DataType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[5].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[5].DataType));

  StoredProc.Prepare;
  S := 'a';
  P2 := 100;
  for i:= 1 to 100 do
  begin
    StoredProc.Params[1].AsInteger:= i;
    StoredProc.Params[2].AsInteger:= P2;
    StoredProc.Params[3].AsString:= S;
    StoredProc.ExecProc;
    CheckEquals(S+S, StoredProc.ParamByName('@p5').AsString);
    CheckEquals(I*10+P2, StoredProc.ParamByName('@p4').AsInteger);
    if Length(S) = 10 then s := 'a'
    else S := S+'a';
    P2 := 100 - I;
  end;
  StoredProc.Unprepare;
  S := StoredProc.ParamByName('@p4').AsString +
    ' ' + StoredProc.ParamByName('@p5').AsString;
  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  CheckEquals('@p4', StoredProc.Params[3].Name);
  CheckEquals('@p5', StoredProc.Params[4].Name);
  StoredProc.Open;

  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Fields[1].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Fields[1].DataType));
  CheckEquals(2, ord(StoredProc.Fields.Count));

  CheckEquals(ord(ftLargeint), ord(StoredProc.Fields[0].DataType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));
end;

{ TZTestOracleStoredProcedure }
function TZTestOracleStoredProcedure.GetSupportedProtocols: string;
begin
  Result := 'oracle,oracle-9i';
end;

procedure TZTestOracleStoredProcedure.abtest(prefix: string);
var
  i, P2: integer;
  S: String;
begin
  StoredProc.StoredProcName := prefix+'ABTEST';
  CheckEquals(5, StoredProc.Params.Count);
  CheckEquals('P1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));
  CheckEquals('P2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[1].DataType));
  CheckEquals('P3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[2].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[2].DataType));
  CheckEquals('P4', StoredProc.Params[3].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[3].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[3].DataType));
  CheckEquals('P5', StoredProc.Params[4].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[4].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));

  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  StoredProc.ExecProc;
  CheckEquals(600, StoredProc.ParamByName('P4').AsInteger);
  CheckEquals('aa', StoredProc.ParamByName('P5').AsString);
  CheckEquals(5, StoredProc.Params.Count);

  CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));
  CheckEquals(ord(ftInteger), ord(StoredProc.Params[1].DataType));
  {$IFDEF DELPHI12_UP}
  CheckEquals(ord(ftWideString), ord(StoredProc.Params[2].DataType));
  {$ELSE}
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[2].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[2].DataType));
  {$ENDIF}
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[3].DataType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));

  StoredProc.Prepare;
  S := 'a';
  P2 := 100;
  for i:= 1 to 100 do
  begin
    StoredProc.Params[0].AsInteger:= i;
    StoredProc.Params[1].AsInteger:= P2;
    StoredProc.Params[2].AsString:= S;
    StoredProc.ExecProc;
    CheckEquals(S+S, StoredProc.ParamByName('P5').AsString);
    CheckEquals(I*10+P2, StoredProc.ParamByName('P4').AsInteger);
    if Length(S) = 10 then s := 'a'
    else S := S+'a';
    P2 := 100 - I;
  end;
  StoredProc.Unprepare;
  S := StoredProc.ParamByName('P4').AsString +
    ' ' + StoredProc.ParamByName('P5').AsString;
  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  CheckEquals('P4', StoredProc.Params[3].Name);
  CheckEquals('P5', StoredProc.Params[4].Name);
  StoredProc.Open;

  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Fields[1].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Fields[1].DataType));
  CheckEquals(2, ord(StoredProc.Fields.Count));

 // CheckEquals(ord(ftLargeint), ord(StoredProc.Fields[0].DataType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));
  CheckEquals(600, StoredProc.FieldByName('P4').AsInteger);
  CheckEquals('aa', StoredProc.FieldByName('P5').AsString);
end;

procedure TZTestOracleStoredProcedure.myfuncInOutReturn(prefix: string);
begin
  StoredProc.StoredProcName := prefix+'"myfuncInOutReturn"';
  CheckEquals(2, StoredProc.Params.Count);
  CheckEquals('X', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[0].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[0].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[0].DataType));
  CheckEquals('ReturnValue', StoredProc.Params[1].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[1].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[1].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[1].DataType));

  StoredProc.ParamByName('x').AsString := 'a';
  StoredProc.ExecProc;

  CheckEquals('aoutvalue', StoredProc.ParamByName('x').AsString);
  CheckEquals('returned string', StoredProc.ParamByName('ReturnValue').AsString);
  CheckEquals(2, StoredProc.Params.Count);

  StoredProc.Open;
  CheckEquals(2, StoredProc.Fields.Count);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Fields[0].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Fields[0].DataType));
  CheckEquals('X', StoredProc.Fields[0].DisplayName);
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Fields[1].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Fields[1].DataType));
  CheckEquals('ReturnValue', StoredProc.Fields[1].DisplayName);

  CheckEquals('aoutvalueoutvalue', StoredProc.ParamByName('X').AsString);
  CheckEquals('returned string', StoredProc.ParamByName('ReturnValue').AsString);
  CheckEquals('aoutvalueoutvalue', StoredProc.FieldByName('X').AsString);
  CheckEquals('returned string', StoredProc.FieldByName('ReturnValue').AsString);
end;

procedure TZTestOracleStoredProcedure.simple_func(prefix: string);
begin
  StoredProc.StoredProcName := prefix+'simple_func';
  CheckEquals(1, StoredProc.Params.Count);
  CheckEquals('ReturnValue', StoredProc.Params[0].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));

  StoredProc.ExecProc;

  CheckEquals(1111, StoredProc.ParamByName('ReturnValue').AsInteger);
  CheckEquals(1, StoredProc.Params.Count);
end;

procedure TZTestOracleStoredProcedure.simplefunc(prefix: string);
begin
  StoredProc.StoredProcName := prefix+'simplefunc';
  CheckEquals(1, StoredProc.Params.Count);
  CheckEquals('ReturnValue', StoredProc.Params[0].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));

  StoredProc.ExecProc;

  CheckEquals(2222, StoredProc.ParamByName('ReturnValue').AsInteger);
  CheckEquals(1, StoredProc.Params.Count);
end;

procedure TZTestOracleStoredProcedure.Test_abtest;
begin
  abtest();
end;

procedure TZTestOracleStoredProcedure.Test_myfuncInOutReturn;
begin
  myfuncInOutReturn();
end;

procedure TZTestOracleStoredProcedure.Test_simple_func;
begin
  simple_func();
end;

procedure TZTestOracleStoredProcedure.Test_simplefunc;
begin
  simplefunc();
end;

procedure TZTestOracleStoredProcedure.Test_packaged;
begin
  abtest('MYPACKAGE.');
  myfuncInOutReturn('MYPACKAGE.');
  simple_func('MYPACKAGE.');
  simplefunc('MYPACKAGE.');
end;

procedure TZTestOracleStoredProcedure.Test_MYPACKAGE;
begin
  StoredProc.StoredProcName := 'MYPACKAGE';
  CheckEquals(9, StoredProc.Params.Count);

  CheckEquals('ABTEST.P1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[0].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[0].DataType));

  CheckEquals('ABTEST.P2', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[1].DataType));

  CheckEquals('ABTEST.P3', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[2].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[2].DataType));

  CheckEquals('ABTEST.P4', StoredProc.Params[3].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[3].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[3].DataType));

  CheckEquals('ABTEST.P5', StoredProc.Params[4].Name);
  CheckEquals(ord(ptOutput), ord(StoredProc.Params[4].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[4].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[4].DataType));

  CheckEquals('myfuncInOutReturn.X', StoredProc.Params[5].Name);
  CheckEquals(ord(ptInputOutput), ord(StoredProc.Params[5].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[5].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[5].DataType));

  CheckEquals('myfuncInOutReturn.ReturnValue', StoredProc.Params[6].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[6].ParamType));
  if ( Connection.DbcConnection.GetEncoding = ceUTF8 ) and
    ( Connection.DbcConnection.UTF8StringAsWideField) then
    CheckEquals(ord(ftWideString), ord(StoredProc.Params[6].DataType))
  else
    CheckEquals(ord(ftString), ord(StoredProc.Params[6].DataType));

  CheckEquals('SIMPLE_FUNC.ReturnValue', StoredProc.Params[7].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[7].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[7].DataType));

  CheckEquals('SIMPLEFUNC.ReturnValue', StoredProc.Params[8].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[8].ParamType));
  //CheckEquals(ord(ftInteger), ord(StoredProc.Params[8].DataType));

  StoredProc.ParamByName('myfuncInOutReturn.X').AsString := 'myfuncInOutReturn';
  StoredProc.ParamByName('ABTEST.P1').AsInteger := 50;
  StoredProc.ParamByName('ABTEST.P2').AsInteger := 100;
  StoredProc.ParamByName('ABTEST.P3').AsString := 'abc';
  StoredProc.ExecProc;
  CheckEquals(600, StoredProc.ParamByName('ABTEST.P4').AsInteger);
  CheckEquals('abcabc', StoredProc.ParamByName('ABTEST.P5').AsString);
  CheckEquals('myfuncInOutReturnoutvalue', StoredProc.ParamByName('myfuncInOutReturn.X').AsString);
  CheckEquals('returned string', StoredProc.ParamByName('myfuncInOutReturn.ReturnValue').AsString);
  CheckEquals(1111, StoredProc.ParamByName('SIMPLE_FUNC.ReturnValue').AsInteger);
  CheckEquals(2222, StoredProc.ParamByName('SIMPLEFUNC.ReturnValue').AsInteger);

  StoredProc.Open;

  CheckEquals(600, StoredProc.ParamByName('ABTEST.P4').AsInteger);
  CheckEquals('abcabc', StoredProc.ParamByName('ABTEST.P5').AsString);
  CheckEquals('myfuncInOutReturnoutvalueoutvalue', StoredProc.ParamByName('myfuncInOutReturn.X').AsString);
  CheckEquals('returned string', StoredProc.ParamByName('myfuncInOutReturn.ReturnValue').AsString);
  CheckEquals(1111, StoredProc.ParamByName('SIMPLE_FUNC.ReturnValue').AsInteger);
  CheckEquals(2222, StoredProc.ParamByName('SIMPLEFUNC.ReturnValue').AsInteger);

  CheckEquals(600, StoredProc.FieldByName('ABTEST.P4').AsInteger);
  CheckEquals('abcabc', StoredProc.FieldByName('ABTEST.P5').AsString);
  CheckEquals('myfuncInOutReturnoutvalueoutvalue', StoredProc.FieldByName('myfuncInOutReturn.X').AsString);
  CheckEquals('returned string', StoredProc.FieldByName('myfuncInOutReturn.ReturnValue').AsString);
  CheckEquals(1111, StoredProc.FieldByName('SIMPLE_FUNC.ReturnValue').AsInteger);
  CheckEquals(2222, StoredProc.FieldByName('SIMPLEFUNC.ReturnValue').AsInteger);
end;
initialization
  RegisterTest('component',TZTestInterbaseStoredProcedure.Suite);
  RegisterTest('component',TZTestDbLibStoredProcedure.Suite);
  RegisterTest('component',TZTestPostgreSQLStoredProcedure.Suite);
  RegisterTest('component',TZTestMySQLStoredProcedure.Suite);
  RegisterTest('component',TZTestADOStoredProcedure.Suite);
  RegisterTest('component',TZTestOracleStoredProcedure.Suite);
//  RegisterTest('component',TZTestStoredProcedure.Suite);
end.
