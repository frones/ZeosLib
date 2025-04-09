{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
{                                                         }
{        Originally written by Jan Baumgarten             }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{  http://zeoslib.sourceforge.net  (FORUM)                }
{  http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER) }
{  http://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{  http://www.sourceforge.net/projects/zeoslib.           }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainProxyDriverInternalProxy;

interface

{$I ZPlain.inc}

{$IF DEFINED(ENABLE_PROXY) AND DEFINED(ENABLE_INTERNAL_PROXY)}

uses
  Classes, ZPlainProxyDriverIntf, ZPlainProxyDriverSoapProxy;

function GetLastErrorStr: WideString; stdcall;
function GetInterface: IZDbcProxy; stdcall;

{$IFEND}

implementation

{$IF DEFINED(ENABLE_PROXY) AND DEFINED(ENABLE_INTERNAL_PROXY)}

uses SysUtils, {$IFNDEF NO_SAFECALL}ActiveX, ComObj,{$ENDIF} SOAPHTTPClient, ZExceptions, SOAPHTTPTrans, Types {$IFDEF TCERTIFICATE_HAS_PUBLICKEY}, Net.URLClient, Net.HttpClient{$ENDIF};

type
  TZDbcProxy = class(TInterfacedObject, IZDbcProxy{$IFNDEF NO_SAFECALL}, ISupportErrorInfo{$ENDIF})
    protected
      FService: IZeosProxy;
      FConnectionID: WideString;
      FValidPublicKeys: TStringList;
      procedure CheckConnected;
      // this is necessary for safecall exception handling
      {$IFNDEF NO_SAFECALL}
      function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
      {$ENDIF}

      {$IFDEF TCERTIFICATE_HAS_PUBLICKEY}
      procedure ValidateServerCertificate(const Sender: TObject; const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
      procedure BeforePostData(const HTTPReqResp: THTTPReqResp; Client: THTTPClient);
      {$ENDIF}
    public
      // this is necessary for safecall exception handling
      {$IFNDEF NO_SAFECALL}
      function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
      {$ENDIF}

      procedure Connect(const UserName, Password, ServiceEndpoint, DbName: WideString; var Properties: WideString; out DbInfo: WideString); {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      procedure Disconnect; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      procedure SetAutoCommit(const Value: LongBool); {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function StartTransaction: Integer; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      procedure Commit; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      procedure Rollback; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function SetProperties(const Properties : WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetSchemas: WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetCatalogs: WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetTableTypes: WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetCharacterSets(): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetPublicKeys: WideString; overload; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
      function GetPublicKeys(EndPoint: WideString): WideString; overload; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}


      constructor Create;
      destructor Destroy; override;
  end;


var
  LastErrorStr: UnicodeString;

function GetLastErrorStr: WideString; stdcall;
begin
  Result := LastErrorStr;
end;

function GetInterface: IZDbcProxy; stdcall;
begin
  try
    result := TZDbcProxy.Create as IZDbcProxy;
  except
    on E: Exception do begin
      LastErrorStr := E.Message;
      result := nil;
    end;
  end;
end;

procedure TZDbcProxy.CheckConnected;
begin
  if not Assigned(FService) then
    raise EZSQLException.Create('No connection has been established yet!');
end;

{$IFNDEF NO_SAFECALL}
function TZDbcProxy.InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
begin
  if GetInterfaceEntry(iid) <> nil then
    Result := S_OK else
    Result := S_FALSE;
end;

function TZDbcProxy.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, StringToGUID('{374CAA55-95CD-44FE-8FF3-F90BF8D1DF8C}'), 'libzdbcproxy.dll', '');
end;
{$ENDIF}

constructor TZDbcProxy.Create;
begin
  FService := nil;
  FValidPublicKeys := TStringList.Create;
  FValidPublicKeys.Delimiter := ':';
end;

destructor TZDbcProxy.Destroy;
begin
 if Assigned(FValidPublicKeys) then
   FreeAndNil(FValidPublicKeys);
 FService := nil;
end;

{$IFDEF TCERTIFICATE_HAS_PUBLICKEY}
procedure TZDbcProxy.ValidateServerCertificate(const Sender: TObject; const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
var
  PubKey: String;
begin
  PubKey := LowerCase(Certificate.PublicKey);
  Accepted := FValidPublicKeys.Count = 0;
  if Accepted then begin
    if PubKey <> '' then
      FValidPublicKeys.Add(PubKey)
  end else
    Accepted := 0 <= FValidPublicKeys.IndexOf(PubKey);
end;

procedure TZDbcProxy.BeforePostData(const HTTPReqResp: THTTPReqResp; Client: THTTPClient);
begin
  //{$IFNDEF ANDROID}
  HTTPReqResp.HTTP.OnValidateServerCertificate := ValidateServerCertificate;
  //{$ENDIF}
end;
{$ENDIF}

procedure TZDbcProxy.Connect(const UserName, Password, ServiceEndpoint, DbName: WideString; var Properties: WideString; out DbInfo: WideString); {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
var
  Url: String;
  FRIO: THTTPRIO;
  MyInProperties: UnicodeString;
  MyOutProperties: UnicodeString;
  MyDbInfo: UnicodeString;
  PropList: TStringList;
  Certs: String;
begin
  FRIO := THTTPRIO.Create(nil);
  Url := ServiceEndpoint;
  FRIO.HTTPWebNode.InvokeOptions := [];
  PropList := TStringList.Create;
  try
    PropList.DelimitedText := Properties;
    {$IFDEF TCERTIFICATE_HAS_PUBLICKEY}
    if PropList.IndexOfName('TofuPubKeys') > 0 then begin
      {$IFDEF ANDROID}
      FRIO.HTTPWebNode.InvokeOptions := FRIO.HTTPWebNode.InvokeOptions + [soIgnoreInvalidCerts];
      {$ELSE}
      FRIO.HTTPWebNode.OnBeforePost := BeforePostData;
      Certs := LowerCase(Trim(PropList.Values['TofuPubKeys']));
      if Certs <> 'yes' then
        FValidPublicKeys.DelimitedText := Certs;
      {$ENDIF}
    end;
    {$ENDIF}
  finally
    FreeAndNil(PropList);
  end;
  FService := GetIZeosProxy(false, Url, FRIO);
  if Assigned(FService) then begin
    MyInProperties := Properties;
    FConnectionID := FService.Connect(UserName, Password, DbName, MyInProperties, MyOutProperties, MyDbInfo);
    Properties := MyOutProperties;
    DbInfo := MyDbInfo;
  end else begin
    FreeAndNil(FRIO);
  end;
end;

procedure TZDbcProxy.Disconnect; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
 CheckConnected;
 try
   FService.Disconnect(FConnectionID);
 finally
   FConnectionID := '';
 end;
end;

procedure TZDbcProxy.SetAutoCommit(const Value: LongBool); {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  FService.SetAutoCommit(FConnectionID, Value);
end;

function TZDbcProxy.StartTransaction: Integer; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.StartTransaction(FConnectionID);
end;

procedure TZDbcProxy.Commit; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  FService.Commit(FConnectionID);
end;

procedure TZDbcProxy.Rollback; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  FService.Rollback(FConnectionID);
end;

function TZDbcProxy.SetProperties(const Properties : WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.SetProperties(FConnectionID, Properties);
end;

function TZDbcProxy.ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.ExecuteStatement(FConnectionID, SQL, Parameters, MaxRows);
end;

function TZDbcProxy.GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTables(FConnectionID, Catalog, SchemaPattern, TableNamePattern, Types);
end;

function TZDbcProxy.GetSchemas: WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetSchemas(FConnectionID);
end;

function TZDbcProxy.GetCatalogs: WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetCatalogs(FConnectionID);
end;

function TZDbcProxy.GetTableTypes: WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTableTypes(FConnectionID);
end;

function TZDbcProxy.GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetColumns(FConnectionID, Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);
end;

function TZDbcProxy.GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTablePrivileges(FConnectionID, Catalog, SchemaPattern, TableNamePattern);
end;

function TZDbcProxy.GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetColumnPrivileges(FConnectionID, Catalog, Schema, Table, ColumnNamePattern);
end;

function TZDbcProxy.GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetPrimaryKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetImportedKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetExportedKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetCrossReference(FConnectionID, PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable);
end;

function TZDbcProxy.GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetIndexInfo(FConnectionID, Catalog, Schema, Table, Unique, Approximate);
end;

function TZDbcProxy.GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetSequences(FConnectionID, Catalog, SchemaPattern, SequenceNamePattern);
end;

function TZDbcProxy.GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTriggers(FConnectionID, Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern);
end;

function TZDbcProxy.GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetProcedures(FConnectionID, Catalog, SchemaPattern, ProcedureNamePattern);
end;

function TZDbcProxy.GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetProcedureColumns(FConnectionID, Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);
end;

function TZDbcProxy.GetCharacterSets(): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
 CheckConnected;
 Result := FService.GetCharacterSets(FConnectionID);
end;

function TZDbcProxy.GetPublicKeys: WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetPublicKeys;
  if Result <> '' then
    FValidPublicKeys.DelimitedText := LowerCase(Result)
  else
    Result := FValidPublicKeys.DelimitedText;
end;

function TZDbcProxy.GetPublicKeys(EndPoint: WideString): WideString; {$IFNDEF NO_SAFECALL}safecall;{$ENDIF}
var
  Url: String;
  FRIO: THTTPRIO;
  MyService: IZeosProxy;
begin
  FRIO := THTTPRIO.Create(nil);
  Url := Endpoint;
  FRIO.HTTPWebNode.InvokeOptions := [soIgnoreInvalidCerts];
  MyService := GetIZeosProxy(false, Url, FRIO);
  if Assigned(MyService) then
    Result := MyService.GetPublicKeys
  else
    FreeAndNil(FRIO);
end;

initialization
  LastErrorStr := 'No Error happened yet!'

{$IFEND}

end.
