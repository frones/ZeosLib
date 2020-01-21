{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
{                                                         }
{        Originally written by Jan Baumgarten             }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

unit ZPlainProxyDriverIntf;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_PROXY}

uses
  Classes;

  type IZDbcProxy = Interface(IUnknown)
    ['{374CAA55-95CD-44FE-8FF3-F90BF8D1DF8C}']
    procedure Connect(const UserName, Password, DbHost, DbName: WideString; var Properties: WideString; out DbInfo: WideString); safecall;
    procedure Disconnect; safecall;
    procedure SetAutoCommit(const Value: LongBool); safecall;
    procedure Commit; safecall;
    procedure Rollback; safecall;
    function SetProperties(const Properties : WideString): WideString; safecall;
    function ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; safecall;
    function GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; safecall;
    function GetSchemas: WideString; safecall;
    function GetCatalogs: WideString; safecall;
    function GetTableTypes: WideString; safecall;
    function GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; safecall;
    function GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; safecall;
    function GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; safecall;
    function GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
    function GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
    function GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
    function GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; safecall;
    function GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; safecall;
    function GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; safecall;
    function GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; safecall;
    function GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; safecall;
    function GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; safecall;
    function GetCharacterSets(): WideString; safecall;	
  end;

{$ENDIF ZEOS_DISABLE_PROXY}

implementation

{$IFNDEF ZEOS_DISABLE_PROXY}

{$ENDIF ZEOS_DISABLE_PROXY}

end.

