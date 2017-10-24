{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 ADO Statement Classes                   }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcAdoStatement;

interface

{$I ZDbc.inc}
{$IFDEF ENABLE_ADO}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  ZCompatibility, {$IFDEF OLD_FPC}ZClasses, {$ENDIF} ZSysUtils, ZOleDB,
  ZDbcIntfs, ZDbcStatement, ZDbcAdo, ZPlainAdo, ZVariant, ZDbcAdoUtils,
  ZDbcOleDBUtils;

type
  {** Implements Prepared ADO Statement. }
  TZAdoPreparedStatement = class(TZAbstractPreparedStatement)
  private
    AdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FAdoConnection: IZAdoConnection;
    FIsSelectSQL: Boolean;
    {Ole direct fast idea -> avoid External allocations and the OleVariants}
    FZBufferSize: Integer;
    FParameterAccessor: IAccessor;
    FDBBindingArray: TDBBindingDynArray;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FDBUPARAMS: DB_UPARAMS;
    FRowSize: NativeUInt;
    FArrayOffSet: DB_UPARAMS;
    FDBParams: TDBParams;
    FRowCount: DBROWCOUNT;
    FParamsBuffer: TByteDynArray; //mem-leak safe! IDE cleans it allways
    FTempLobs: TInterfacesDynArray;
    FUseOle: Boolean;
    procedure CalcParamSetsAndBufferSize;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(Connection: IZConnection; const SQL: string;
      const Info: TStrings); overload;
    constructor Create(Connection: IZConnection; const Info: TStrings); overload;
    destructor Destroy; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; overload; override;
    function GetMoreResults(var RS: IZResultSet): Boolean; reintroduce; overload;

    procedure ClearParameters; override;

    procedure SetDataArray(ParameterIndex: Integer; const Value;
      const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
  end;
  TZAdoStatement = class(TZAdoPreparedStatement);

  {** Implements Callable ADO Statement. }
  TZAdoCallableStatement = class(TZAbstractCallableStatement)
  private
    AdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FAdoConnection: IZAdoConnection;
    FDirectionTypes: TDirectionTypes;
  protected
    function GetOutParam(ParameterIndex: Integer): TZVariant; override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  public
    constructor Create(Connection: IZConnection; const SQL: string;
      const Info: TStrings);
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure RegisterParamType(ParameterIndex: Integer; ParamType: Integer); override;
    function GetMoreResults: Boolean; override;
    procedure Unprepare; override;
  end;

implementation

uses
  Variants, ComObj, Math,
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs{$ELSE} Contnrs{$ENDIF},
  ZEncoding, ZDbcLogging, ZDbcCachedResultSet, ZDbcResultSet, ZFastCode,
  ZDbcMetadata, ZDbcResultSetMetadata, ZDbcUtils, ZMessages, ZDbcProperties;

{ TZAdoPreparedStatement }

constructor TZAdoPreparedStatement.Create(Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  FAdoCommand := CoCommand.Create;
  inherited Create(Connection, SQL, Info);
  FAdoCommand.CommandText := WSQL;
  FAdoConnection := Connection as IZAdoConnection;
  FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072); //by default 128KB
  FUseOle := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'use_ole_update_params', 'true')); //not set by default on 7.2
end;

constructor TZAdoPreparedStatement.Create(Connection: IZConnection;
  const Info: TStrings);
begin
  Create(Connection, '', Info);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072); //by default 128KB
  FUseOle := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'use_ole_update_params', ''));
end;

destructor TZAdoPreparedStatement.Destroy;
begin
  AdoRecordSet := nil;
  FAdoConnection := nil;
  inherited Destroy;
  FAdoCommand := nil;
end;

procedure TZAdoPreparedStatement.CalcParamSetsAndBufferSize;
var I: Integer;
begin
  if ArrayCount > 0 then
    //indicate rows for batch executions
    if (FRowSize > Cardinal(FZBufferSize)) or (FRowSize * Cardinal(ArrayCount) > Cardinal(FZBufferSize)) then
      FDBParams.cParamSets := Max(1, FZBufferSize div NativeInt(FRowSize))
    else
      FDBParams.cParamSets := ArrayCount
  else
    FDBParams.cParamSets := 1; //indicate rows for single executions
  SetLength(FParamsBuffer, FDBParams.cParamSets * FRowSize);
  FDBParams.pData := Pointer(FParamsBuffer); //set entry pointer
  if FDBParams.hAccessor = 0 then
    FParameterAccessor.CreateAccessor(DBACCESSOR_PARAMETERDATA,
      FDBUPARAMS, Pointer(FDBBindingArray), FRowSize, @FDBParams.hAccessor,
      Pointer(FDBBINDSTATUSArray));
  for i := 0 to Length(FTempLobs)-1 do
    SetLength(FTempLobs[i], Max(1, ArrayCount));
end;

procedure TZAdoPreparedStatement.Prepare;
begin
  if Not Prepared then //prevent PrepareInParameters
  begin
    if FUseOle then //note: ADO or MSSQL allow execute multiple stmts
      //so a "insert into foo values('bar'); select scope_identity()"
      //would return a rowset -> fall back to ADO behavior
      FIsSelectSQL := (ZFastCode.Pos('SELECT', UpperCase(SQL)) > 0)
    else
      FIsSelectSQL := IsSelect(SQL);
    if not Assigned(FAdoCommand) then
    begin
      FAdoCommand := CoCommand.Create;
      FAdoCommand.CommandText := WSQL;
      FAdoConnection := Connection as IZAdoConnection;
      FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
    end;
    inherited Prepare;
    if not Assigned(FParameterAccessor) then
      FAdoCommand.Prepared := True;
  end
  else
    if Assigned(FParameterAccessor) and ((ArrayCount > 0) and
      (FDBParams.cParamSets = 0)) or //new arrays have been set
      ((ArrayCount = 0) and (FDBParams.cParamSets > 1)) then //or single exec follows
      CalcParamSetsAndBufferSize
end;

procedure TZAdoPreparedStatement.PrepareInParameters;
var
  FNamesBuffer: PPOleStr; //we don't need this here except as param!
  FParamInfoArray: PDBParamInfoArray;
  FOleParamCommand: ICommandWithParameters;
  FOlePrepareCommand: ICommandPrepare;
begin
  if InParamCount > 0 then
  begin
    if FUseOle and (not FIsSelectSQL) and
       Supports((FAdoCommand as ADOCommandConstruction).OLEDBCommand as ICommand,
        IID_ICommandWithParameters, FOleParamCommand) and
       Supports((FAdoCommand as ADOCommandConstruction).OLEDBCommand as ICommand,
        IID_ICommandPrepare, FOlePrepareCommand) and
       Supports(FOleParamCommand, IID_IAccessor, FParameterAccessor) then
    begin
      OleDBCheck(FOlePrepareCommand.Prepare(0)); //0 indicates a non known count of execution
      {check out the Parameter informations}
      FNamesBuffer := nil; FParamInfoArray := nil;
      try
        OleDBCheck(FOleParamCommand.GetParameterInfo(FDBUPARAMS, PDBPARAMINFO(FParamInfoArray), FNamesBuffer));
        Assert(FDBUPARAMS = Cardinal(InParamCount), SInvalidInputParameterCount);
        SetLength(FDBBINDSTATUSArray, FDBUPARAMS);
        FRowSize := PrepareOleParamDBBindings(FDBUPARAMS, FDBBindingArray,
          InParamTypes, FParamInfoArray, FTempLobs);
        CalcParamSetsAndBufferSize;
      finally
        if Assigned(FParamInfoArray) then ZAdoMalloc.Free(FParamInfoArray);
        if Assigned(FNamesBuffer) then ZAdoMalloc.Free(FNamesBuffer);
      end;
      Assert(FDBParams.hAccessor = 1, 'Accessor handle should be unique!');
    end
    else
      RefreshParameters(FAdoCommand);
  end;
end;

procedure TZAdoPreparedStatement.BindInParameters;
var
  I: Integer;
begin
  if InParamCount = 0 then
    Exit
  else
    if ArrayCount = 0 then
      if Assigned(FParameterAccessor) then
      begin
        FRowCount := 0; //init
        OleBindParams(FDBParams, ConSettings, FDBBindingArray,
          InParamValues, InParamTypes, ClientVarManager);
      end
      else
      begin
        for i := 0 to InParamCount-1 do
          if ClientVarManager.IsNull(InParamValues[i]) then
            if (InParamDefaultValues[i] <> '') and (UpperCase(InParamDefaultValues[i]) <> 'NULL') and
              StrToBoolEx(DefineStatementParameter(Self, DSProps_Defaults, 'true')) then
            begin
              ClientVarManager.SetAsString(InParamValues[i], InParamDefaultValues[i]);
              ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
            end
            else
              ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], NullVariant, adParamInput)
          else
            ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
    end
    else
      if Assigned(FParameterAccessor) then
      begin
        FRowCount := 0; //init
        LastUpdateCount := 0;
        while True do
        begin
          if (FArrayOffSet+FDBParams.cParamSets >= Cardinal(ArrayCount)) then
          begin
            FDBParams.cParamSets := DBLENGTH(ArrayCount) - FArrayOffSet;
            OleBindArrayParams(FDBParams, FArrayOffSet, FRowSize, ConSettings,
              FDBBindingArray, ClientVarManager, InParamValues, FTempLobs);
            FRowCount := LastUpdateCount; //restore for final execution!
            FArrayOffSet := 0; //Reset!
            Break
          end
          else
          begin
            OleBindArrayParams(FDBParams, FArrayOffSet, FRowSize, ConSettings,
              FDBBindingArray, ClientVarManager, InParamValues, FTempLobs);
            ((FAdoCommand as ADOCommandConstruction).OLEDBCommand as ICommand).Execute(
              nil,DB_NULLGUID,FDBParams,@FRowCount,nil);
            Inc(FArrayOffSet, FDBParams.cParamSets);
            LastUpdateCount := LastUpdateCount + FRowCount;
          end
        end
      end
      else
        LastUpdateCount := ADOBindArrayParams(FAdoCommand, FAdoConnection,
          ConSettings, InParamValues, adParamInput, ArrayCount);
end;

procedure TZAdoPreparedStatement.UnPrepareInParameters;
var
  FAccessorRefCount: DBREFCOUNT;
begin
  if Assigned(FParameterAccessor) then
  begin
    //don't forgett to release the Accessor else we're leaking mem on Server!
    FParameterAccessor.ReleaseAccessor(FDBParams.hAccessor, @FAccessorRefCount);
    Assert(FAccessorRefCount = 0, 'Some more Accessors wrongly have been created!');
    FParameterAccessor := nil;
  end;
end;

function TZAdoPreparedStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
var
  RC: OleVariant;
begin
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close;
  FOpenResultSet := nil;
  WSQL := SQL;
  Result := nil;
  LastUpdateCount := -1;
  if IsSelect(Self.SQL) then
  begin
    AdoRecordSet := CoRecordSet.Create;
    AdoRecordSet.MaxRecords := MaxRows;
    AdoRecordSet.Open(WSQL, (Connection as IZAdoConnection).GetAdoConnection,
      adOpenStatic, adLockOptimistic, adAsyncFetch);
  end
  else
    AdoRecordSet := (Connection as IZAdoConnection).GetAdoConnection.Execute(WSQL, RC, adExecuteNoRecords);
  Result := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
    Self.SQL, ConSettings, ResultSetConcurrency);
  if not Assigned(Result) then
    while (not GetMoreResults(Result)) and (LastUpdateCount > -1) do ;
  FOpenResultSet := Pointer(Result);
end;

function TZAdoPreparedStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;
  WSQL := SQL;
  try
    (Connection as IZAdoConnection).GetAdoConnection.Execute(WSQL, RC, adExecuteNoRecords);
    Result := RC;
    LastUpdateCount := Result;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

function TZAdoPreparedStatement.Execute(const SQL: ZWideString): Boolean;
var
  RC: OleVariant;
begin
  WSQL := SQL;
  LastResultSet := nil;
  LastUpdateCount := -1;
  try
    if IsSelect(Self.SQL) then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet.Open(SQL, (Connection as IZAdoConnection).GetAdoConnection,
        adOpenStatic, adLockOptimistic, adAsyncFetch);
    end
    else
      AdoRecordSet := (Connection as IZAdoConnection).GetAdoConnection.Execute(WSQL, RC, adExecuteNoRecords);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      Self.SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

function TZAdoPreparedStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  Result := ExecuteQuery(WSQL);
end;

function TZAdoPreparedStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  Result := ExecuteUpdate(WSQL);
end;

function TZAdoPreparedStatement.Execute(const SQL: RawByteString): Boolean;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  Result := Execute(WSQL);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAdoPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  RC: OleVariant;
begin
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close; //Note keep track we close the RS and DO NOT Try to resync them!
  FOpenResultSet := nil;
  Prepare;
  LastUpdateCount := -1;
  BindInParameters;
  try
    if FIsSelectSQL then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
      AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
    end
    else
      AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
    Result := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := {%H-}RC;
    if not Assigned(Result) then
      while (not GetMoreResults(Result)) and (LastUpdateCount > -1) do ;
    FOpenResultSet := Pointer(Result);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZAdoPreparedStatement.ExecuteUpdatePrepared: Integer;
var
  RC: OleVariant;
begin
  Prepare;
  LastUpdateCount := -1;
  BindInParameters;
  try
    if Assigned(FParameterAccessor) then
    begin
      LastUpdateCount := FRowCount; //store tempory possible array bound update-counts
      OleDBCheck(((FAdoCommand as ADOCommandConstruction).OLEDBCommand as ICommand).Execute(
        nil, DB_NULLGUID,FDBParams,@FRowCount,nil));
      LastUpdateCount := LastUpdateCount + FrowCount;
    end
    else
    begin
      AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, adExecuteNoRecords);
      LastUpdateCount := {%H-}RC;
    end;
    Result := LastUpdateCount;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAdoPreparedStatement.ExecutePrepared: Boolean;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;

  Prepare;
  BindInParameters;
  try
    if FIsSelectSQL then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
      AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
      LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
        SQL, ConSettings, ResultSetConcurrency);
    end
    else
      if Assigned(FParameterAccessor) then
      begin
        LastUpdateCount := FRowCount; //store tempory possible array bound update-counts
        Succeeded(((FAdoCommand as ADOCommandConstruction).OLEDBCommand as ICommand).Execute(
          nil, DB_NULLGUID,FDBParams,@FRowCount, nil));
        LastUpdateCount := LastUpdateCount + FrowCount;
      end
      else
      begin
        AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
        LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
          SQL, ConSettings, ResultSetConcurrency);
        LastUpdateCount := {%H-}RC;
      end;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

function TZAdoPreparedStatement.GetMoreResults: Boolean;
var
  RC: OleVariant;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
  end;
end;

function TZAdoPreparedStatement.GetMoreResults(var RS: IZResultSet): Boolean;
var RC: OleVariant;
begin
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    RS := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(RS);
    LastUpdateCount := RC;
  end
  else Result := False;
end;

procedure TZAdoPreparedStatement.Unprepare;
begin
  if FAdoCommand.Prepared then
    FAdoCommand.Prepared := False;
  if Assigned(FParameterAccessor) then
    ((FAdoCommand as ADOCommandConstruction).OLEDBCommand as ICommandPrepare).UnPrepare;
  inherited Unprepare;
end;

procedure TZAdoPreparedStatement.ClearParameters;
begin
  inherited ClearParameters;
  if (FParameterAccessor = nil) and Prepared then
    RefreshParameters(FAdoCommand);
end;

procedure TZAdoPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
begin
  if ParameterIndex = FirstDbcIndex then
  begin
    FArrayOffSet := 0;
    FDBParams.cParamSets := 0;
  end;
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
end;
{ TZAdoCallableStatement }

constructor TZAdoCallableStatement.Create(Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FAdoCommand := CoCommand.Create;
  FAdoCommand.CommandText := WSQL;
  FAdoConnection := Connection as IZAdoConnection;
  FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
  FAdoCommand.CommandType := adCmdStoredProc;
end;

function TZAdoCallableStatement.ExecuteQueryPrepared: IZResultSet;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  ColumnsInfo: TObjectList;
  RS: TZVirtualResultSet;
  IndexAlign: TIntegerDynArray;
  P: Pointer;
  Stream: TStream;
begin
  ExecutePrepared;
  SetLength(IndexAlign, 0);
  ColumnsInfo := TObjectList.Create(True);
  Stream := nil;
  try
    for I := 0 to FAdoCommand.Parameters.Count -1 do
      if FAdoCommand.Parameters.Item[i].Direction in [adParamOutput,
        adParamInputOutput, adParamReturnValue] then
    begin
      SetLength(IndexAlign, Length(IndexAlign)+1);
      ColumnInfo := TZColumnInfo.Create;
      with ColumnInfo do
      begin
        {$IFNDEF UNICODE}
        ColumnLabel := PUnicodeToString(Pointer(FAdoCommand.Parameters.Item[i].Name), Length(FAdoCommand.Parameters.Item[i].Name), ConSettings^.CTRL_CP);
        {$ELSE}
        ColumnLabel := FAdoCommand.Parameters.Item[i].Name;
        {$ENDIF}
        ColumnType := ConvertAdoToSqlType(FAdoCommand.Parameters.Item[I].Type_, ConSettings.CPType);
        ColumnDisplaySize := FAdoCommand.Parameters.Item[I].Precision;
        Precision := FAdoCommand.Parameters.Item[I].Precision;
        IndexAlign[High(IndexAlign)] := I;
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;

    RS := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '', ConSettings);
    with RS do
    begin
      SetType(rtScrollInsensitive);
      SetConcurrency(rcReadOnly);
      RS.MoveToInsertRow;
      for i := FirstDbcIndex to ColumnsInfo.Count{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
        case TZColumnInfo(ColumnsInfo[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType of
          stBoolean:
            RS.UpdateBoolean(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stByte:
            RS.UpdateByte(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stShort:
            RS.UpdateShort(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stWord:
            RS.UpdateWord(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stSmall:
            RS.UpdateSmall(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stLongWord:
            RS.UpdateUInt(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stInteger:
            RS.UpdateInt(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stULong:
            RS.UpdateULong(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stLong:
            RS.UpdateLong(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stFloat, stDouble, stBigDecimal:
            RS.UpdateFloat(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stString:
            RS.UpdateString(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stAsciiStream:
            begin
              Stream := TStringStream.Create(AnsiString(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value));
              RS.UpdateAsciiStream(I, Stream);
              Stream.Free;
            end;
          stUnicodeString:
            RS.UpdateUnicodeString(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stUnicodeStream:
            begin
              Stream := WideStringStream(WideString(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value));
              RS.UpdateUnicodeStream(I, Stream);
              FreeAndNil(Stream);
            end;
          stBytes:
            RS.UpdateBytes(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stDate:
            RS.UpdateDate(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stTime:
            RS.UpdateTime(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stTimestamp:
            RS.UpdateTimestamp(i, FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
          stBinaryStream:
            begin
              if VarIsStr(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value) then
              begin
                Stream := TStringStream.Create(AnsiString(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value));
                RS.UpdateBinaryStream(I, Stream);
                FreeAndNil(Stream);
              end
              else
                if VarIsArray(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value) then
                begin
                  P := VarArrayLock(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
                  try
                    Stream := TMemoryStream.Create;
                    Stream.Size {%H-}:= VarArrayHighBound(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value, 1)+1;
                    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, TMemoryStream(Stream).Memory^, Stream.Size);
                    RS.UpdateBinaryStream(I, Stream);
                    FreeAndNil(Stream);
                  finally
                    VarArrayUnLock(FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]].Value);
                  end;
                end;
            end
          else
            RS.UpdateNull(i);
        end;
      RS.InsertRow;
    end;
    Result := RS;
  finally
    ColumnsInfo.Free;
    if Stream <> nil then
      Stream.Free;
  end;
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZAdoCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  ExecutePrepared;
  Result := LastUpdateCount;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAdoCallableStatement.ExecutePrepared: Boolean;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Not Prepared then Prepare;

  BindInParameters;
  try
    if IsSelect(SQL) then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
      AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
    end
    else
      AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := RC;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0,
        ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

procedure TZAdoCallableStatement.RegisterParamType(ParameterIndex: Integer;
  ParamType: Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if Length(FDirectionTypes) < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} then
    SetLength(FDirectionTypes, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  case Self.FDBParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] of
    1: //ptInput
      FDirectionTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := adParamInput;
    2: //ptOut
      FDirectionTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := adParamOutput;
    3: //ptInputOutput
      FDirectionTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := adParamInputOutput;
    4: //ptResult
      FDirectionTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := adParamReturnValue;
    else
      //ptUnknown
      FDirectionTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := adParamUnknown;
  end;
end;

function TZAdoCallableStatement.GetMoreResults: Boolean;
var
  RC: OleVariant;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
  end;
end;

procedure TZAdoCallableStatement.Unprepare;
begin
  if FAdoCommand.Prepared then
    FAdoCommand.Prepared := False;
  inherited;
end;

function TZAdoCallableStatement.GetOutParam(ParameterIndex: Integer): TZVariant;
var
  Temp: Variant;
  P: Pointer;
  TempBlob: IZBLob;
begin

  if ParameterIndex > OutParamCount then
    Result := NullVariant
  else
  begin
    Temp := FAdoCommand.Parameters.Item[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value;

    case ConvertAdoToSqlType(FAdoCommand.Parameters.Item[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Type_,
      ConSettings.CPType) of
      stBoolean:
        ClientVarManager.SetAsBoolean(Result, Temp);
      stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong:
        ClientVarManager.SetAsInteger(Result, Temp);
      stFloat, stDouble, stCurrency, stBigDecimal:
        ClientVarManager.SetAsFloat(Result, Temp);
      stGUID:
        ClientVarManager.SetAsString(Result, Temp);
      stString, stAsciiStream:
        ClientVarManager.SetAsString(Result, Temp);
      stUnicodeString, stUnicodeStream:
        ClientVarManager.SetAsUnicodeString(Result, Temp);
      stBytes:
        ClientVarManager.SetAsBytes(Result, VarToBytes(Temp));
      stDate, stTime, stTimestamp:
        ClientVarManager.SetAsDateTime(Result, Temp);
      stBinaryStream:
        begin
          if VarIsStr(Temp) then
          begin
            TempBlob := TZAbstractBlob.CreateWithStream(nil);
            TempBlob.SetString(AnsiString(Temp));
          end
          else
            if VarIsArray(Temp) then
            begin
              P := VarArrayLock(Temp);
              try
                TempBlob := TZAbstractBlob.CreateWithData(P, VarArrayHighBound(Temp, 1)+1);
              finally
                VarArrayUnLock(Temp);
              end;
            end;
          ClientVarManager.SetAsInterface(Result, TempBlob);
          TempBlob := nil;
        end
      else
        ClientVarManager.SetNull(Result);
    end;
  end;

  LastWasNull := ClientVarManager.IsNull(Result) or VarIsNull(Temp) or VarIsClear(Temp);
end;

procedure TZAdoCallableStatement.PrepareInParameters;
begin
  if InParamCount > 0 then
    RefreshParameters(FAdoCommand, @FDirectionTypes);
  FAdoCommand.Prepared := True;
end;

procedure TZAdoCallableStatement.BindInParameters;
var
  I: Integer;
begin
  if InParamCount = 0 then
    Exit
  else
    for i := 0 to InParamCount-1 do
      if FDBParamTypes[i] in [1,3] then //ptInput, ptInputOutput
        if ClientVarManager.IsNull(InParamValues[i]) then
          if (InParamDefaultValues[i] <> '') and (UpperCase(InParamDefaultValues[i]) <> 'NULL') and
            StrToBoolEx(DefineStatementParameter(Self, DSProps_Defaults, 'true')) then
          begin
            ClientVarManager.SetAsString(InParamValues[i], InParamDefaultValues[i]);
            ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
          end
          else
            ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], NullVariant, FDirectionTypes[i])
        else
          ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], FDirectionTypes[i])
      else
        ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], NullVariant, FDirectionTypes[i]);
end;

{$ELSE}
implementation
{$ENDIF ENABLE_ADO}

end.



