{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Test Case for SQL Metadata Dataset            }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{               Written by Sergey Seroukhov               }
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

unit ZTestSqlMetadata;

interface

uses
  TestFramework, Db, SysUtils, ZConnection, ZSqlMetadata, ZTestDefinitions;

type

  {** Implements a test case for class TZReadOnlyQuery. }
  TZTestSQLMetadataCase = class(TZComponentPortableSQLTestCase)
  private
    Connection: TZConnection;
    Metadata: TZSQLMetadata;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMainDatasets;
  end;

implementation

uses Classes, ZDbcUtils, ZSysUtils, ZTestConsts, ZDbcIntfs;

{ TZTestSQLMetadataCase }

{**
  Prepares initial data before each test.
}
procedure TZTestSQLMetadataCase.SetUp;
begin
  Connection := CreateDatasetConnection;

  Metadata := TZSQLMetadata.Create(nil);
  Metadata.Connection := Connection;
end;

{**
  Removes data after each test.
}
procedure TZTestSQLMetadataCase.TearDown;
begin
  Metadata.Close;
  Metadata.Free;

  Connection.Disconnect;
  Connection.Free;
end;

{**
  Runs a test for main datasets.
}
procedure TZTestSQLMetadataCase.TestMainDatasets;
begin
  Metadata.MetadataType := mdTables;
  Metadata.Open;
  try
    Check(Metadata.RecordCount > 0);
  finally
    Metadata.Close;
  end;

  if StartsWith(Protocol, 'interbase')
    or StartsWith(Protocol, 'firebird')
    or StartsWith(Protocol, 'oracle') then
    Metadata.TableName := 'PEOPLE'
  else
    Metadata.TableName := 'people';

  Metadata.MetadataType := mdColumns;
  Metadata.Open;
  try
    Check(Metadata.RecordCount > 0);
  finally
    Metadata.Close;
  end;
end;

initialization
  TestFramework.RegisterTest(TZTestSQLMetadataCase.Suite);
end.
