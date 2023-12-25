{
This unit has been produced by ws_helper.
  Input unit name : "zeosproxy".
  This unit name  : "zeosproxy".
  Date            : "15.12.2023 17:53:32".
}
unit zeosproxy;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$DEFINE WST_RECORD_RTTI}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'zproxy';
  sUNIT_NAME = 'zeosproxy';

type

  StringArray = class;

  StringArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of UnicodeString;
  private
    function GetItem(AIndex: Integer): UnicodeString;
    procedure SetItem(AIndex: Integer; const AValue: UnicodeString);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : UnicodeString read GetItem write SetItem; default;
  end;

  IZeosProxy = interface(IInvokable)
    ['{DD0144C6-0CD5-483C-A8D9-28A00BAD1C75}']
    function Connect(
      const  UserName : UnicodeString; 
      const  Password : UnicodeString; 
      const  DbName : UnicodeString; 
      const  InProperties : UnicodeString; 
      out  OutProperties : UnicodeString; 
      out  DbInfo : UnicodeString
    ):UnicodeString;
    procedure Disconnect(
      const  ConnectionID : UnicodeString
    );
    procedure SetAutoCommit(
      const  ConnectionID : UnicodeString; 
      const  Value : boolean
    );
    procedure Commit(
      const  ConnectionID : UnicodeString
    );
    procedure Rollback(
      const  ConnectionID : UnicodeString
    );
    function SetProperties(
      const  ConnectionID : UnicodeString; 
      const  Properties : UnicodeString
    ):UnicodeString;
    function ExecuteStatement(
      const  ConnectionID : UnicodeString; 
      const  SQL : UnicodeString; 
      const  Parameters : UnicodeString; 
      const  MaxRows : LongWord
    ):UnicodeString;
    function GetTables(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString; 
      const  Types : UnicodeString
    ):UnicodeString;
    function GetSchemas(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
    function GetCatalogs(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
    function GetTableTypes(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
    function GetColumns(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString; 
      const  ColumnNamePattern : UnicodeString
    ):UnicodeString;
    function GetTablePrivileges(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString
    ):UnicodeString;
    function GetColumnPrivileges(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString; 
      const  ColumnNamePattern : UnicodeString
    ):UnicodeString;
    function GetPrimaryKeys(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString
    ):UnicodeString;
    function GetImportedKeys(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString
    ):UnicodeString;
    function GetExportedKeys(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString
    ):UnicodeString;
    function GetCrossReference(
      const  ConnectionID : UnicodeString; 
      const  PrimaryCatalog : UnicodeString; 
      const  PrimarySchema : UnicodeString; 
      const  PrimaryTable : UnicodeString; 
      const  ForeignCatalog : UnicodeString; 
      const  ForeignSchema : UnicodeString; 
      const  ForeignTable : UnicodeString
    ):UnicodeString;
    function GetIndexInfo(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString; 
      const  Unique : boolean; 
      const  Approximate : boolean
    ):UnicodeString;
    function GetSequences(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  SequenceNamePattern : UnicodeString
    ):UnicodeString;
    function GetTriggers(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString; 
      const  TriggerNamePattern : UnicodeString
    ):UnicodeString;
    function GetProcedures(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  ProcedureNamePattern : UnicodeString
    ):UnicodeString;
    function GetProcedureColumns(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  ProcedureNamePattern : UnicodeString; 
      const  ColumnNamePattern : UnicodeString
    ):UnicodeString;
    function GetCharacterSets(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
    function StartTransaction(
      const  ConnectionID : UnicodeString
    ):integer;
    function GetPublicKeys():string;
  end;

  procedure Register_zeosproxy_ServiceMetadata();

Implementation
uses metadata_repository, record_rtti, wst_types;

{ StringArray }

function StringArray.GetItem(AIndex: Integer): UnicodeString;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure StringArray.SetItem(AIndex: Integer;const AValue: UnicodeString);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function StringArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure StringArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('_item',TypeInfo(UnicodeString),FData[AIndex]);
end;

procedure StringArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := '_item';
  AStore.Get(TypeInfo(UnicodeString),sName,FData[AIndex]);
end;

class function StringArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(UnicodeString);
end;

procedure StringArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure StringArray.Assign(Source: TPersistent);
var
  src : StringArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(StringArray) then begin
    src := StringArray(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;


procedure Register_zeosproxy_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'TRANSPORT_Address',
    'https://www.iks.ag/services/ZeosProxyBinding'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Connect',
    '_E_N_',
    'Connect'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Connect',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Connect',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Connect',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Disconnect',
    '_E_N_',
    'Disconnect'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Disconnect',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Disconnect',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Disconnect',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetAutoCommit',
    '_E_N_',
    'SetAutoCommit'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetAutoCommit',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetAutoCommit',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetAutoCommit',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Commit',
    '_E_N_',
    'Commit'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Commit',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Commit',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Commit',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Rollback',
    '_E_N_',
    'Rollback'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Rollback',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Rollback',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'Rollback',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetProperties',
    '_E_N_',
    'SetProperties'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetProperties',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetProperties',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'SetProperties',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'ExecuteStatement',
    '_E_N_',
    'ExecuteStatement'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'ExecuteStatement',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'ExecuteStatement',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'ExecuteStatement',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTables',
    '_E_N_',
    'GetTables'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTables',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTables',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTables',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSchemas',
    '_E_N_',
    'GetSchemas'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSchemas',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSchemas',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSchemas',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCatalogs',
    '_E_N_',
    'GetCatalogs'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCatalogs',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCatalogs',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCatalogs',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTableTypes',
    '_E_N_',
    'GetTableTypes'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTableTypes',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTableTypes',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTableTypes',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumns',
    '_E_N_',
    'GetColumns'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumns',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumns',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumns',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTablePrivileges',
    '_E_N_',
    'GetTablePrivileges'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTablePrivileges',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTablePrivileges',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTablePrivileges',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumnPrivileges',
    '_E_N_',
    'GetColumnPrivileges'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumnPrivileges',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumnPrivileges',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetColumnPrivileges',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPrimaryKeys',
    '_E_N_',
    'GetPrimaryKeys'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPrimaryKeys',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPrimaryKeys',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPrimaryKeys',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetImportedKeys',
    '_E_N_',
    'GetImportedKeys'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetImportedKeys',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetImportedKeys',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetImportedKeys',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetExportedKeys',
    '_E_N_',
    'GetExportedKeys'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetExportedKeys',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetExportedKeys',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetExportedKeys',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCrossReference',
    '_E_N_',
    'GetCrossReference'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCrossReference',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCrossReference',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCrossReference',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetIndexInfo',
    '_E_N_',
    'GetIndexInfo'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetIndexInfo',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetIndexInfo',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetIndexInfo',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSequences',
    '_E_N_',
    'GetSequences'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSequences',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSequences',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetSequences',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTriggers',
    '_E_N_',
    'GetTriggers'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTriggers',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTriggers',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetTriggers',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedures',
    '_E_N_',
    'GetProcedures'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedures',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedures',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedures',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedureColumns',
    '_E_N_',
    'GetProcedureColumns'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedureColumns',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedureColumns',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetProcedureColumns',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCharacterSets',
    '_E_N_',
    'GetCharacterSets'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCharacterSets',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCharacterSets',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetCharacterSets',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'StartTransaction',
    '_E_N_',
    'StartTransaction'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'StartTransaction',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'StartTransaction',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'StartTransaction',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPublicKeys',
    '_E_N_',
    'GetPublicKeys'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPublicKeys',
    'TRANSPORT_soapAction',
    ''
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPublicKeys',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IZeosProxy',
    'GetPublicKeys',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;


var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(StringArray),'StringArray');



End.
