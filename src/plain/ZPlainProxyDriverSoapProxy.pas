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

// ************************************************************************ //
// Die in dieser Datei deklarierten Typen wurden aus Daten der unten
// beschriebenen WSDL-Datei generiert:

// WSDL     : C:\Komponenten\Zeos 8\src\webservice\wsdl\zeosproxy.WSDL
//  >Import : C:\Komponenten\Zeos 8\src\webservice\wsdl\zeosproxy.WSDL>0
// Codierung : utf-8
// Version: 1.0
// (15.12.2023 18:06:55 - - $Rev: 113440 $)
// ************************************************************************ //

unit ZPlainProxyDriverSoapProxy;

interface

{$I ZPlain.inc}

{$IF DEFINED(ENABLE_PROXY) AND DEFINED(ENABLE_INTERNAL_PROXY)}

uses Soap.InvokeRegistry, Soap.SOAPHTTPClient, System.Types, Soap.XSBuiltIns;

const
  IS_OPTN = $0001;
  IS_UNBD = $0002;
  IS_UNQL = $0008;


type

  // ************************************************************************ //
  // Die folgenden Typen, auf die im WSDL-Dokument Bezug genommen wird, sind in dieser Datei
  // nicht repräsentiert. Sie sind entweder Aliase[@] anderer repräsentierter Typen oder auf sie wurde Bezug genommen,
  // aber sie sind in diesem Dokument nicht[!] deklariert. Die Typen aus letzterer Kategorie
  // sind in der Regel vordefinierten/bekannten XML- oder Embarcadero-Typen zugeordnet; sie könnten aber auf 
  // ein inkorrektes WSDL-Dokument hinweisen, das einen Schematyp nicht deklariert oder importiert hat.
  // ************************************************************************ //
  // !:unsignedInt     - "http://www.w3.org/2001/XMLSchema"[]
  // !:int             - "http://www.w3.org/2001/XMLSchema"[Lit][]
  // !:boolean         - "http://www.w3.org/2001/XMLSchema"[]
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Lit][Gbl]


  // ************************************************************************ //
  // Namespace : zproxy
  // Transport : http://schemas.xmlsoap.org/soap/http
  // Stil     : rpc
  // Verwenden von       : literal
  // Bindung   : IZeosProxyBinding
  // Service   : IZeosProxy
  // Port      : IZeosProxyPort
  // URL       : 127.0.0.1/services/IZeosProxy
  // ************************************************************************ //
  IZeosProxy = interface(IInvokable)
  ['{269AF2BC-9AAB-FBA4-61C1-37129CFC7BFC}']

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabemeldung besteht aus mehreren Parts
    function  Connect(const UserName: string; const Password: string; const DbName: string; const InProperties: string; out OutProperties: string; out DbInfo: string
                      ): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    procedure Disconnect(const ConnectionID: string); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    procedure SetAutoCommit(const ConnectionID: string; const Value: Boolean); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    procedure Commit(const ConnectionID: string); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    procedure Rollback(const ConnectionID: string); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  SetProperties(const ConnectionID: string; const Properties: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  ExecuteStatement(const ConnectionID: string; const SQL: string; const Parameters: string; const MaxRows: Cardinal): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTables(const ConnectionID: string; const Catalog: string; const SchemaPattern: string; const TableNamePattern: string; const Types: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetSchemas(const ConnectionID: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetCatalogs(const ConnectionID: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTableTypes(const ConnectionID: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetColumns(const ConnectionID: string; const Catalog: string; const SchemaPattern: string; const TableNamePattern: string; const ColumnNamePattern: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTablePrivileges(const ConnectionID: string; const Catalog: string; const SchemaPattern: string; const TableNamePattern: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetColumnPrivileges(const ConnectionID: string; const Catalog: string; const Schema: string; const Table: string; const ColumnNamePattern: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetPrimaryKeys(const ConnectionID: string; const Catalog: string; const Schema: string; const Table: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetImportedKeys(const ConnectionID: string; const Catalog: string; const Schema: string; const Table: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetExportedKeys(const ConnectionID: string; const Catalog: string; const Schema: string; const Table: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetCrossReference(const ConnectionID: string; const PrimaryCatalog: string; const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string; 
                                const ForeignTable: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetIndexInfo(const ConnectionID: string; const Catalog: string; const Schema: string; const Table: string; const Unique: Boolean; const Approximate: Boolean
                           ): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetSequences(const ConnectionID: string; const Catalog: string; const SchemaPattern: string; const SequenceNamePattern: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTriggers(const ConnectionID: string; const Catalog: string; const SchemaPattern: string; const TableNamePattern: string; const TriggerNamePattern: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetProcedures(const ConnectionID: string; const Catalog: string; const SchemaPattern: string; const ProcedureNamePattern: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetProcedureColumns(const ConnectionID: string; const Catalog: string; const SchemaPattern: string; const ProcedureNamePattern: string; const ColumnNamePattern: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetCharacterSets(const ConnectionID: string): string; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  StartTransaction(const ConnectionID: string): Integer; stdcall;

    // Entpacken nicht möglich: 
    //     - Ausgabe-Part verweist auf kein Element
    function  GetPublicKeys: string; stdcall;
  end;

function GetIZeosProxy(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): IZeosProxy;


{$IFEND}
implementation
{$IF DEFINED(ENABLE_PROXY) AND DEFINED(ENABLE_INTERNAL_PROXY)}

uses System.SysUtils;

function GetIZeosProxy(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): IZeosProxy;
const
  defWSDL = 'C:\Komponenten\Zeos 8\src\webservice\wsdl\zeosproxy.WSDL';
  defURL  = 'https://www.iks.ag/services/ZeosProxyBinding';
  defSvc  = 'IZeosProxy';
  defPrt  = 'IZeosProxyPort';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as IZeosProxy);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


initialization
  { IZeosProxy }
  InvRegistry.RegisterInterface(TypeInfo(IZeosProxy), 'zproxy', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(IZeosProxy), '');
  InvRegistry.RegisterInvokeOptions(TypeInfo(IZeosProxy), ioLiteral);

{$IFEND}
end.