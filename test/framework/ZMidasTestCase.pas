{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{   Generic Test Case for Midas (DataSnap) components     }
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

unit ZMidasTestCase;

interface

{$I ZTestFramework.inc}

{$WARN SYMBOL_PLATFORM OFF}

uses
{$IFNDEF VER140BELOW}
  TConnect,
{$ENDIF}
  Classes, DB, TestFramework, ZConnection, ZDataset, ZSqlTestCase, DataBkr,
  Provider, DBClient, MConnect, ZMidasTestServer_TLB;

type

  {** Implements a Data Module for Local Data Provider. }
  TZRemoteDM = class(TRemoteDataModule, IZRemoteDM)
    DSProvider: TDataSetProvider;
    FMasterSource: TDataSource;

    procedure RemoteDataModuleCreate(Sender: TObject);

  private
    FConnection: TZConnection;
    FQuery: TZQuery;
    FDetailQuery: TZQuery;

  public
    procedure SetOptions(const Protocol: WideString;
      const HostName: WideString; Port: Integer;
      const Database: WideString; const UserName: WideString;
      const Password: WideString); safecall;
    procedure MasterDetail(const Value: Integer); safecall;

    class procedure UpdateRegistry(Register: Boolean;
      const ClassID, ProgID: string); override;

    property Provider: TDataSetProvider read DSProvider write DSProvider;
    property MasterSource: TDataSource read FMasterSource write FMasterSource;
    property Connection: TZConnection read FConnection write FConnection;
    property Query: TZQuery read FQuery write FQuery;
    property DetailQuery: TZQuery read FDetailQuery write FDetailQuery;
  end;

  {** Implements a generic test case for Midas (DataSnap) components. }
  TZMidasPortableSQLTestCase = class(TZPortableSQLTestCase)
  private
    FRemoteDM: TZRemoteDM;
    FConnection: TLocalConnection;
    FDataSet: TClientDataSet;

  protected
    property RemoteDM: TZRemoteDM read FRemoteDM write FRemoteDM;
    property Connection: TLocalConnection read FConnection write FConnection;
    property DataSet: TClientDataSet read FDataSet write FDataSet;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

{$R *.tlb}
{$R *.dfm}

{ TZRemoteDM }

{**
  Performs initialization of the data module on create event.
  @param Sender a sender object reference.
}
procedure TZRemoteDM.RemoteDataModuleCreate(Sender: TObject);
begin
  Connection := TZConnection.Create(Self);

  Query := TZQuery.Create(Self);
  Query.Connection := Connection;

  DetailQuery := TZQuery.Create(Self);
  DetailQuery.Connection := Connection;

  MasterSource.DataSet := Query;
  Provider.DataSet := Query;
end;

procedure TZRemoteDM.SetOptions(const Protocol, HostName: WideString;
  Port: Integer; const Database, UserName, Password: WideString);
begin
  Connection.Protocol := Protocol;
  Connection.HostName := HostName;
  Connection.Port := Port;
  Connection.Database := Database;
  Connection.User := UserName;
  Connection.Password := Password;
  Connection.Connect;
end;

procedure TZRemoteDM.MasterDetail(const Value: Integer);
begin
  if Value = 0 then
  begin
    Query.SQL.Text := 'select * from department';
    DetailQuery.SQL.Text := 'select * from people';
    DetailQuery.MasterSource := MasterSource;
    DetailQuery.IndexFieldNames := 'p_dep_id';
    DetailQuery.MasterFields := 'dep_id';
    Query.Open;
    DetailQuery.Open;
  end
  else
  begin
    DetailQuery.MasterSource := nil;
    DetailQuery.IndexFieldNames := '';
    DetailQuery.MasterFields := '';
    DetailQuery.Close;
  end;
end;

{**
  Updates a data module in a registry.
  @param Register <code>True</code> to register data module
    and <code>False</code> to unregister it.
  @param ClassID a GUID of data module class.
  @param ProgID a GUID of data module program.
}
class procedure TZRemoteDM.UpdateRegistry(Register: Boolean; const ClassID,
  ProgID: string);
begin
  if Register then
  begin
    inherited UpdateRegistry(Register, ClassID, ProgID);
    EnableSocketTransport(ClassID);
    EnableWebTransport(ClassID);
  end
  else
  begin
    DisableSocketTransport(ClassID);
    DisableWebTransport(ClassID);
    inherited UpdateRegistry(Register, ClassID, ProgID);
  end;
end;

{ TZMidasPortableSQLTestCase }

{**
  Sets up the test main properties.
}
procedure TZMidasPortableSQLTestCase.SetUp;
begin
  FRemoteDM := TZRemoteDM.Create(nil);
  FConnection := TLocalConnection.Create(FRemoteDM);
  FRemoteDM.SetOptions(Protocol, HostName, Port,
    Database, UserName, Password);

  FDataSet := TClientDataSet.Create(nil);
  FDataSet.ProviderName := 'DSProvider';
  FDataSet.RemoteServer := FConnection;
end;

{**
  Frees the test main properties.
}
procedure TZMidasPortableSQLTestCase.TearDown;
begin
  FDataSet.Close;
  FDataSet.Free;
  FConnection.Free;
  FRemoteDM.Free;
end;

end.
