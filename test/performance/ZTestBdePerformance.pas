{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for BDE API Performance            }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZTestBdePerformance;

interface

uses TestFramework, SysUtils, Classes,
{$IFNDEF EXCLUDE_BDE_TEST}
  DB, DbTables,
{$ENDIF}
  ZPerformanceTestCase;

type

  {** Implements a performance test case for BDE TDataset API. }
  TZBDEPerformanceTestCase = class (TZPerformanceSQLTestCase)
  protected
    function GetImplementedAPI: string; override;

{$IFNDEF EXCLUDE_BDE_TEST}
  private
    FQuery: TQuery;
    FDatabase: TDatabase;
  protected
    property Query: TQuery read FQuery write FQuery;
    property Database: TDatabase read FDatabase write FDatabase;

    procedure SetUp; override;
    procedure TearDown; override;

    { Implementation of different tests. }
    procedure DefaultSetUpTest; override;
    procedure DefaultTearDownTest; override;

    procedure SetUpTestConnect; override;
    procedure RunTestConnect; override;
    procedure SetUpTestInsert; override;
    procedure RunTestInsert; override;
    procedure RunTestOpen; override;
    procedure RunTestFetch; override;
    procedure RunTestUpdate; override;
    procedure RunTestDelete; override;
    procedure SetUpTestLocate; override;
    procedure RunTestLocate; override;
    procedure SetUpTestLookup; override;
    procedure RunTestLookup; override;
{$ENDIF}
  end;

implementation

{ TZBDEPerformanceTestCase }

{**
  Gets a name of the implemented API.
  @return the name of the implemented tested API.
}
function TZBDEPerformanceTestCase.GetImplementedAPI: string;
begin
  Result := 'bde';
end;

{$IFNDEF EXCLUDE_BDE_TEST}

{**
   Create objects and allocate memory for variables
}
procedure TZBDEPerformanceTestCase.SetUp;
begin
  Database := TDatabase.Create(nil);
  Database.LoginPrompt := False;
  Database.AliasName := inherited Alias;
  Database.DatabaseName := inherited Alias;
  Database.Params.Add( 'USER NAME=' + UserName);      //Markus
  Database.Params.Add( 'PASSWORD=' + Password);      //Markus

  Query := TQuery.Create(nil);
  Query.DatabaseName := Database.DatabaseName;
  Query.RequestLive := True;
end;

{**
   Destroy objects and free allocated memory for variables
}
procedure TZBDEPerformanceTestCase.TearDown;
begin
  if Query <> nil then
  begin
    Query.Free;
    Query := nil;
  end;

  if Database <> nil then
  begin
    Database.Free;
    Database := nil;
  end;
end;

{**
  The default empty Set Up method for all tests.
}
procedure TZBDEPerformanceTestCase.DefaultSetUpTest;
begin
  Database.Open;
  Database.StartTransaction;
end;

{**
  The default empty Tear Down method for all tests.
}
procedure TZBDEPerformanceTestCase.DefaultTearDownTest;
begin
  Query.Close;
  if Database.InTransaction then
    Database.Commit;
  Database.Close;
end;

{**
  The empty Set Up method for connect test.
}
procedure TZBDEPerformanceTestCase.SetUpTestConnect;
begin
end;

{**
  Performs a connect test.
}
procedure TZBDEPerformanceTestCase.RunTestConnect;
begin
  Database.Open;
end;

{**
  Performs an insert test.
}
procedure TZBDEPerformanceTestCase.SetUpTestInsert;
begin
  inherited SetUpTestInsert;
  Query.SQL.Text := 'DELETE FROM high_load';
  Query.ExecSQL;
end;

{**
  Performs an insert test.
}
procedure TZBDEPerformanceTestCase.RunTestInsert;
var
  I: Integer;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
  for I := 1 to GetRecordCount do
  begin
    with Query do
    begin
      Append;
      Fields[0].AsInteger := I;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
    end;
  end;
end;

{**
  Performs an open test.
}
procedure TZBDEPerformanceTestCase.RunTestOpen;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
end;

{**
   Performs a fetch data
}
procedure TZBDEPerformanceTestCase.RunTestFetch;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
  while not Query.EOF do
  begin
    with Query do
    begin
      Fields[0].AsInteger;
      Fields[1].AsFloat;
      Fields[2].AsString;
      Next;
    end;
  end;
end;

{**
  Performs an update test.
}
procedure TZBDEPerformanceTestCase.RunTestUpdate;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
  while not Query.EOF do
  begin
    with Query do
    begin
      Edit;
      Fields[1].AsFloat := RandomFloat(-100, 100);
      Fields[2].AsString := RandomStr(10);
      Post;
      Next;
    end;
  end;
end;

{**
  Performs a delete test.
}
procedure TZBDEPerformanceTestCase.RunTestDelete;
begin
  Query.SQL.Text := 'SELECT * FROM high_load';
  Query.Open;
  while not Query.EOF do
    Query.Delete;
end;

{**
  The empty Set Up method for locate test.
}
procedure TZBDEPerformanceTestCase.SetUpTestLocate;
begin
  inherited SetUpTestLocate;
  Query.SQL.Text := 'SELECT * FROM high_load ORDER BY hl_id';
  Query.Open;
  Query.Last;
  Query.First;
end;

{**
  Performs a locate test.
}
procedure TZBDEPerformanceTestCase.RunTestLocate;
begin
  Query.Locate('data2','AAAAAAAAAA',[]);
end;

{**
  The empty Set Up method for lookup test.
}
procedure TZBDEPerformanceTestCase.SetUpTestLookup;
begin
  inherited SetUpTestLookup;
  Query.SQL.Text := 'SELECT * FROM high_load ORDER BY hl_id';
  Query.Open;
  Query.Last;
  Query.First;
end;

{**
  Performs a lookup test.
}
procedure TZBDEPerformanceTestCase.RunTestLookup;
begin
  Query.Lookup('data2','AAAAAAAAAA','hl_id');
end;

{$ENDIF}

initialization
  TestFramework.RegisterTest(TZBDEPerformanceTestCase.Suite);
end.

