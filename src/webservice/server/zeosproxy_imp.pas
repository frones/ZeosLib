{
This unit has been produced by ws_helper.
  Input unit name : "zeosproxy".
  This unit name  : "zeosproxy_imp".
  Date            : "21.09.2024 17:27:26".
}
Unit zeosproxy_imp;

{$I dbcproxy.inc}

Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, zeosproxy,
     ZDbcProxyManagement, DbcProxyConnectionManager, DbcProxyConfigManager,
     DbcProxyFileLogger, DbcProxyConfigStore, ZDbcIntfs;

type
  TZeosProxy_ServiceImp=class(TBaseServiceImplementation,IZeosProxy)
  Private
    function ExecuteSingleStmt(
      Connection: IZConnection;
      const  SQL : UnicodeString;
      const  Parameters : UnicodeString;
      const  MaxRows : LongWord
    ): UnicodeString;
  Protected
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
    function GetPublicKeys():UnicodeString;
    function ExecuteMultipleStmts(
      const  ConnectionID : UnicodeString;
       Statements : TStatementDescriptions
    ):TStringArray;
  End;


  procedure RegisterZeosProxyImplementationFactory();

var
  ConnectionManager: TDbcProxyConnectionManager;
  ConfigManager: IZDbcProxyConfigStore;
  Logger: TDbcProxyLogger;
  AuditLogger: TDbcProxyFileLogger;

Implementation

uses config_objects, DbcProxyUtils, ZDbcXmlUtils{$IFDEF ENABLE_TOFU_CERTIFICATES}, dbcproxycertstore, types{$ENDIF};

{ TZeosProxy_ServiceImp implementation }
function TZeosProxy_ServiceImp.Connect(
  const  UserName : UnicodeString; 
  const  Password : UnicodeString; 
  const  DbName : UnicodeString; 
  const  InProperties : UnicodeString; 
  out  OutProperties : UnicodeString; 
  out  DbInfo : UnicodeString
):UnicodeString;
var
  Connection: IZConnection;
  Url: String;
  PropertiesList: TStringList;
Begin
  if not Assigned(ConfigManager) then raise
    Exception.Create('Config Manager is not assigned...');
  Logger.Debug('Constructing URL');
  Url := ConfigManager.ConstructUrl(UTF8Encode(DbName), UTF8Encode(UserName), UTF8Encode(Password));
  PropertiesList := TStringList.Create;
  try
    Logger.Debug('Getting Zeos connection...');
    Connection := DriverManager.GetConnectionWithParams(UTF8Encode(Url), PropertiesList);
  finally
    FreeAndNil(PropertiesList);
  end;

  applyConnectionProperties(Connection, UTF8Encode(InProperties));
  OutProperties := UnicodeString(encodeConnectionProperties(Connection));
  Connection.Open;
  DbInfo := UnicodeString(encodeDatabaseInfo(Connection));
  Result := UnicodeString(ConnectionManager.AddConnection(Connection, DbName, UserName));
  if not Assigned(AuditLogger) then
    raise Exception.Create('Audit logger is not assigned.');
  AuditLogger.LogLine('Connect');
End;

procedure TZeosProxy_ServiceImp.Disconnect(
  const  ConnectionID : UnicodeString
);
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ZeosConnection.Close;
  finally
    Unlock;
  end;
  ConnectionManager.RemoveConnection(UTF8Encode(ConnectionID));
  AuditLogger.LogLine('Disconnect');
End;

procedure TZeosProxy_ServiceImp.SetAutoCommit(
  const  ConnectionID : UnicodeString; 
  const  Value : boolean
);
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ZeosConnection.SetAutoCommit(Value);
  finally
    Unlock;
  end;
  AuditLogger.LogLine('SetAutoCommit ' + BoolToStr(Value, True));
End;

procedure TZeosProxy_ServiceImp.Commit(
  const  ConnectionID : UnicodeString
);
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ZeosConnection.Commit;
  finally
    Unlock;
  end;
  AuditLogger.LogLine('Commit');
End;

procedure TZeosProxy_ServiceImp.Rollback(
  const  ConnectionID : UnicodeString
);
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ZeosConnection.Rollback;
  finally
    Unlock;
  end;
  AuditLogger.LogLine('Rollback');
End;

function TZeosProxy_ServiceImp.SetProperties(
  const  ConnectionID : UnicodeString; 
  const  Properties : UnicodeString
):UnicodeString;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    applyConnectionProperties(ZeosConnection, UTF8Encode(Properties));
    Result := UnicodeString(encodeConnectionProperties(ZeosConnection));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('SetProperties');
End;

function TZeosProxy_ServiceImp.ExecuteSingleStmt(
  Connection: IZConnection;
  const  SQL : UnicodeString;
  const  Parameters : UnicodeString;
  const  MaxRows : LongWord
): UnicodeString;
var
  Statement: IZPreparedStatement;
  ResultSet: IZResultSet;
  ResultStr: String;
begin
  try
    Statement := Connection.PrepareStatementWithParams(UTF8Encode(SQL), nil);
    if Parameters <> '' then
      DecodeParameters(UTF8Encode(Parameters), Statement);
    Statement.SetResultSetConcurrency(rcReadOnly);
    Statement.SetResultSetType(rtForwardOnly);
    if Statement.ExecutePrepared then begin
      ResultSet := Statement.GetResultSet;
      if Assigned(ResultSet) then begin
        ResultStr := ZxmlEncodeResultSet(ResultSet, MaxRows, Statement.GetUpdateCount);
        Result := UnicodeString(ResultStr);
      end else
        Result := UnicodeString(IntToStr(Statement.GetUpdateCount));
    end else
      Result := UnicodeString(IntToStr(Statement.GetUpdateCount));
  finally
    ResultSet := nil;
    Statement := nil;
  end;
end;

function TZeosProxy_ServiceImp.ExecuteStatement(
  const  ConnectionID : UnicodeString; 
  const  SQL : UnicodeString; 
  const  Parameters : UnicodeString; 
  const  MaxRows : LongWord
):UnicodeString;
var
  ResultStr: UTF8String;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    Result := ExecuteSingleStmt(ZeosConnection, SQL, Parameters, MaxRows);
  finally
    Unlock;
  end;
  AuditLogger.LogLine('ExecuteStatement');
  AuditLogger.LogLine(String(SQL));
End;

function TZeosProxy_ServiceImp.GetTables(
  const  ConnectionID : UnicodeString;
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  Types : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
  TypesArray: Array of String;
Begin
  SetLength(TypesArray, 0);
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    //todo: implement some exploding of Types into the TypesArray
    ResultSet := ZeosConnection.GetMetadata.GetTables(UTF8Encode(Catalog), UTF8Encode(SchemaPattern), UTF8Encode(TableNamePattern), TypesArray);
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetTables');
End;

function TZeosProxy_ServiceImp.GetSchemas(
  const  ConnectionID : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetSchemas;
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetSchemas');
End;

function TZeosProxy_ServiceImp.GetCatalogs(
  const  ConnectionID : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetCatalogs;
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetCatalogs');
End;

function TZeosProxy_ServiceImp.GetTableTypes(
  const  ConnectionID : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetTableTypes;
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetTableTypes');
End;

function TZeosProxy_ServiceImp.GetColumns(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetColumns(UTF8Encode(Catalog), UTF8Encode(SchemaPattern), UTF8Encode(TableNamePattern), UTF8Encode(ColumnNamePattern));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;;
  end;
  AuditLogger.LogLine('GetColumns');
End;

function TZeosProxy_ServiceImp.GetTablePrivileges(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetTablePrivileges(UTF8Encode(Catalog), UTF8Encode(SchemaPattern), UTF8Encode(TableNamePattern));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetTablePrivileges');
End;

function TZeosProxy_ServiceImp.GetColumnPrivileges(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetColumnPrivileges(UTF8Encode(Catalog), UTF8Encode(Schema), UTF8Encode(Table), UTF8Encode(ColumnNamePattern));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetColumnPrivileges');
End;

function TZeosProxy_ServiceImp.GetPrimaryKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetPrimaryKeys(UTF8Encode(Catalog), UTF8Encode(Schema), UTF8Encode(Table));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetPrimaryKeys');
End;

function TZeosProxy_ServiceImp.GetImportedKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetImportedKeys(UTF8Encode(Catalog), UTF8Encode(Schema), UTF8Encode(Table));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetImportedKeys');
End;

function TZeosProxy_ServiceImp.GetExportedKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetExportedKeys(UTF8Encode(Catalog), UTF8Encode(Schema), UTF8Encode(Table));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetExportedKeys');
End;

function TZeosProxy_ServiceImp.GetCrossReference(
  const  ConnectionID : UnicodeString; 
  const  PrimaryCatalog : UnicodeString; 
  const  PrimarySchema : UnicodeString; 
  const  PrimaryTable : UnicodeString; 
  const  ForeignCatalog : UnicodeString; 
  const  ForeignSchema : UnicodeString; 
  const  ForeignTable : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetCrossReference(UTF8Encode(PrimaryCatalog), UTF8Encode(PrimarySchema), UTF8Encode(PrimaryTable), UTF8Encode(ForeignCatalog), UTF8Encode(ForeignSchema), UTF8Encode(ForeignTable));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetCrossReference');
End;

function TZeosProxy_ServiceImp.GetIndexInfo(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString; 
  const  Unique : boolean; 
  const  Approximate : boolean
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetIndexInfo(UTF8Encode(Catalog), UTF8Encode(Schema), UTF8Encode(Table), Unique, Approximate);
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetIndexInfo');
End;

function TZeosProxy_ServiceImp.GetSequences(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  SequenceNamePattern : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetSequences(UTF8Encode(Catalog), UTF8Encode(SchemaPattern), UTF8Encode(SequenceNamePattern));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetSequences');
End;

function TZeosProxy_ServiceImp.GetTriggers(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  TriggerNamePattern : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetTriggers(UTF8Encode(Catalog), UTF8Encode(SchemaPattern), UTF8Encode(TableNamePattern), UTF8Encode(TriggerNamePattern));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetTriggers');
End;

function TZeosProxy_ServiceImp.GetProcedures(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  ProcedureNamePattern : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetProcedures(UTF8Encode(Catalog), UTF8Encode(SchemaPattern), UTF8Encode(ProcedureNamePattern));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetProcedures');
End;

function TZeosProxy_ServiceImp.GetProcedureColumns(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  ProcedureNamePattern : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetProcedureColumns(UTF8Encode(Catalog), UTF8Encode(SchemaPattern), UTF8Encode(ProcedureNamePattern), UTF8Encode(ColumnNamePattern));
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetProcedureColumns');
End;

function TZeosProxy_ServiceImp.GetCharacterSets(
  const  ConnectionID : UnicodeString
):UnicodeString;
var
  ResultSet: IZResultSet;
Begin
  with ConnectionManager.LockConnection(UTF8Encode(ConnectionID)) do
  try
    ResultSet := ZeosConnection.GetMetadata.GetCharacterSets;
    Result := UnicodeString(ZXmlEncodeResultSet(ResultSet));
  finally
    Unlock;
  end;
  AuditLogger.LogLine('GetCharacterSets');
End;

function TZeosProxy_ServiceImp.StartTransaction(
  const  ConnectionID : UnicodeString
):integer;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    Result := ZeosConnection.StartTransaction;
  finally
    Unlock;
  end;
  AuditLogger.LogLine('StartTransaction');
End;

function TZeosProxy_ServiceImp.GetPublicKeys():UnicodeString;
{$IFDEF ENABLE_TOFU_CERTIFICATES}
var
  X: Integer;
  ValidKeys: TStringDynArray;
{$ENDIF}
Begin
  Result := '';
  {$IFDEF ENABLE_TOFU_CERTIFICATES}
  if Assigned(TofuCertStore) then begin
    ValidKeys := TofuCertStore.GetValidPublicKeys;
    for x := 0 to Length(ValidKeys) - 1 do begin
      if x > 0 then
        Result := Result + ':';
      Result := Result + ValidKeys[x];
    end;
  end else
    Result := '';
  {$ENDIF}
End;

function TZeosProxy_ServiceImp.ExecuteMultipleStmts(
  const  ConnectionID : UnicodeString;
   Statements : TStatementDescriptions
):TStringArray;
var
  x: Integer;
  Desc: TStatementDescription;
Begin
  with ConnectionManager.LockConnection(Utf8Encode(ConnectionID)) do
  try
    Result := TStringArray.Create;
    Result.SetLength(Statements.Length);
    for x := 0 to Statements.Length - 1 do begin
      Desc := Statements.Item[x];
      Result.Item[x] := ExecuteSingleStmt(ZeosConnection, Desc.SQL, Desc.Parameters, Desc.MaxRows);
    end;
  finally
    Unlock;
  end;
  AuditLogger.LogLine('ExecuteMultiple');
End;



procedure RegisterZeosProxyImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('IZeosProxy',TImplementationFactory.Create(TZeosProxy_ServiceImp,wst_GetServiceConfigText('IZeosProxy')) as IServiceImplementationFactory);
End;

initialization
  {$IFDEF WINDOWS}
  AuditLogger := TDbcProxyFileLogger.Create(ExtractFilePath(ParamStr(0)) + 'audit.log');
  {$ELSE}
  AuditLogger := TDbcProxyFileLogger.Create('/var/log/' + ExtractFileName(ParamStr(0)) + '.audit.log');
  {$ENDIF}

finalization
  if Assigned(Logger) then
    FreeAndNil(Logger);
  if Assigned(AuditLogger) then
    FreeAndNil(AuditLogger);

End.
