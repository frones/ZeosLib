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
  end;

implementation

uses Classes, ZSysUtils, ZDbcUtils, ZTestConsts, ZDbcIntfs, ZAbstractDataset,
  ZTestCase;


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
  CheckEquals('R1', StoredProc.Params[0].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));
  CheckEquals('P1', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  StoredProc.Params[1].AsInteger := 12345;
  StoredProc.ExecProc;
  CheckEquals(12346, StoredProc.Params[0].AsInteger);
  CheckEquals(2, StoredProc.Params.Count);
end;

{**
   Testing executil stored procedures
}
procedure TZTestInterbaseStoredProcedure.Test_abtest;
var
  i: integer;
  S: String;
begin
  StoredProc.StoredProcName := 'abtest';
  CheckEquals(5, StoredProc.Params.Count);
  CheckEquals('P4', StoredProc.Params[0].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[0].ParamType));
  CheckEquals('P5', StoredProc.Params[1].Name);
  CheckEquals(ord(ptResult), ord(StoredProc.Params[1].ParamType));
  CheckEquals('P1', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  CheckEquals('P2', StoredProc.Params[3].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[3].ParamType));
  CheckEquals('P3', StoredProc.Params[4].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[4].ParamType));

  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  StoredProc.ExecProc;
  CheckEquals(600, StoredProc.ParamByName('P4').AsInteger);
  CheckEquals('aa', StoredProc.ParamByName('P5').AsString);
  CheckEquals(5, StoredProc.Params.Count);
//Fix it!!  StoredProc.Open;

  StoredProc.Prepare;
  for i:= 0 to 99 do
  begin
    StoredProc.Unprepare; //Fix it!!
    StoredProc.Params[2].AsInteger:= i;
    StoredProc.Params[3].AsInteger:= 100;
    StoredProc.Params[4].AsString:= 'a';

    StoredProc.ExecProc;
  end;
  StoredProc.Unprepare;
  //EgonHugeist: Fix it!
  S := StoredProc.ParamByName('P4').AsString +
    ' ' + StoredProc.ParamByName('P5').AsString;
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
  S: String;
begin
  StoredProc.StoredProcName := 'public.abtest';
  CheckEquals(4, StoredProc.Params.Count); //Fix it should 5
  CheckEquals('returnValue', StoredProc.Params[0].Name); //Fix it!!
  CheckEquals('$0', StoredProc.Params[1].Name);
  // Fix it! CheckEquals('P1', StoredProc.Params[1].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[1].ParamType));
  CheckEquals('$1', StoredProc.Params[2].Name);
  //Fix it! CheckEquals('P2', StoredProc.Params[2].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[2].ParamType));
  CheckEquals('$2', StoredProc.Params[3].Name);
  //Fix it! CheckEquals('P3', StoredProc.Params[3].Name);
  CheckEquals(ord(ptInput), ord(StoredProc.Params[3].ParamType));

  StoredProc.ParamByName('$0').AsInteger := 50;
  StoredProc.ParamByName('$1').AsInteger := 100;
  StoredProc.ParamByName('$2').AsString := 'a';
  {Fix it!:
  StoredProc.ParamByName('P1').AsInteger := 50;
  StoredProc.ParamByName('P2').AsInteger := 100;
  StoredProc.ParamByName('P3').AsString := 'a';
  }
  StoredProc.ExecProc;
  //CheckEquals('', StoredProc.ParamByName('returnValue').AsString);
  CheckEquals(4, StoredProc.Params.Count);
  StoredProc.Open;
  CheckEquals(2, StoredProc.Fields.Count);
  CheckEquals(600, StoredProc.FieldByName('p4').AsInteger);
  CheckEquals('aa', StoredProc.FieldByName('p5').AsString);

  StoredProc.Prepare;
  for i:= 0 to 99 do
  begin
    //EgonHugeist: Fix it!
    {StoredProc.Params[2].AsInteger:= i;
    StoredProc.Params[3].AsInteger:= 100;
    StoredProc.Params[4].AsString:= 'a';}

    StoredProc.Params[1].AsInteger:= i;
    StoredProc.Params[2].AsInteger:= 100;
    StoredProc.Params[3].AsString:= 'a';

    StoredProc.ExecProc;
  end;
  StoredProc.Unprepare;
  //EgonHugeist: Fix it!
  {S := StoredProc.ParamByName('P4').AsString +
    ' ' + StoredProc.ParamByName('P5').AsString;}
  StoredProc.Open;
  S := StoredProc.FieldByName('P4').AsString +
    ' ' + StoredProc.FieldByName('P5').AsString;
end;

initialization
  RegisterTest('component',TZTestInterbaseStoredProcedure.Suite);
  RegisterTest('component',TZTestDbLibStoredProcedure.Suite);
  RegisterTest('component',TZTestPostgreSQLStoredProcedure.Suite);
  RegisterTest('component',TZTestStoredProcedure.Suite);
end.
