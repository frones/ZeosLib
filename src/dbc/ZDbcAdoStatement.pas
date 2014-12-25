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

uses
  Types, Classes, SysUtils, ZCompatibility, ZClasses, ZSysUtils, ZCollections,
  ZDbcIntfs, ZPlainDriver, ZDbcStatement, ZDbcAdo, ZPlainAdoDriver, ZPlainAdo,
  ZVariant, ZDbcAdoUtils;

type
  {** Implements Generic ADO Statement. }
  TZAdoStatement = class(TZAbstractStatement)
  protected
    AdoRecordSet: ZPlainAdo.RecordSet;
  public
    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function GetMoreResults: Boolean; override;
  end;

  {** Implements Prepared ADO Statement. }
  TZAdoPreparedStatement = class(TZAbstractPreparedStatement)
  private
    AdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  public
    constructor Create(Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; override;
    procedure Unprepare; override;
  end;

  {** Implements Callable ADO Statement. }
  TZAdoCallableStatement = class(TZAbstractCallableStatement)
  private
    AdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FDirectionTypes: TDirectionTypes;
  protected
    function GetOutParam(ParameterIndex: Integer): TZVariant; override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  public
    constructor Create(Connection: IZConnection;
      SQL: string; Info: TStrings);
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure RegisterParamType(ParameterIndex: Integer; ParamType: Integer); override;
    function GetMoreResults: Boolean; override;
    procedure Unprepare; override;
  end;

implementation

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  OleDB, ComObj,
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs{$ELSE} Contnrs{$ENDIF},
  {$IFNDEF UNICODE}ZEncoding,{$ENDIF}
  ZDbcLogging, ZDbcCachedResultSet, ZDbcResultSet, ZDbcAdoResultSet,
  ZDbcMetadata, ZDbcResultSetMetadata, ZDbcUtils;

function TZAdoStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
var
  RC: OleVariant;
  function GetMoreResults(var RS: IZResultSet): Boolean;
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
begin
  {$IFDEF UNICODE}
  WSQL := SQL;
  {$ENDIF}
  Result := nil;
  LastUpdateCount := -1;
  if IsSelect(SSQL) then
  begin
    AdoRecordSet := CoRecordSet.Create;
    AdoRecordSet.MaxRecords := MaxRows;
    AdoRecordSet.Open(SQL, (Connection as IZAdoConnection).GetAdoConnection,
      adOpenStatic, adLockOptimistic, adAsyncFetch);
  end
  else
    AdoRecordSet := (Connection as IZAdoConnection).GetAdoConnection.Execute(WSQL, RC, adExecuteNoRecords);
  Result := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
    SSQL, ConSettings, ResultSetConcurrency);
  if not Assigned(Result) then
    while (not GetMoreResults(Result)) and (LastUpdateCount > -1) do ;
end;

function TZAdoStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
var
  RC: OleVariant;
begin
    LastResultSet := nil;
    LastUpdateCount := -1;
    {$IFDEF UNICODE}
    WSQL := SQL;
    {$ENDIF}
  try
    (Connection as IZAdoConnection).GetAdoConnection.Execute(WSQL, RC, adExecuteNoRecords);
    Result := RC;
    LastUpdateCount := Result;
    DriverManager.LogMessage(lcExecute, Connection.GetIZPlainDriver.GetProtocol, LogSQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, Connection.GetIZPlainDriver.GetProtocol, LogSQL, 0, E.Message);
      raise;
    end;
  end
end;

function TZAdoStatement.Execute(const SQL: ZWideString): Boolean;
var
  RC: OleVariant;
begin
    {$IFDEF UNICODE}
    WSQL := SQL;
    {$ENDIF}
    LastResultSet := nil;
    LastUpdateCount := -1;
  try
    if IsSelect(SSQL) then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet.Open(SQL, (Connection as IZAdoConnection).GetAdoConnection,
        adOpenStatic, adLockOptimistic, adAsyncFetch);
    end
    else
      AdoRecordSet := (Connection as IZAdoConnection).GetAdoConnection.Execute(WSQL, RC, adExecuteNoRecords);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      LogSQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
    DriverManager.LogMessage(lcExecute, Connection.GetIZPlainDriver.GetProtocol, LogSQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, Connection.GetIZPlainDriver.GetProtocol, LogSQL, 0, E.Message);
      raise;
    end;
  end
end;

function TZAdoStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  Result := ExecuteQuery(WSQL);
end;

function TZAdoStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  Result := ExecuteUpdate(WSQL);
end;

function TZAdoStatement.Execute(const SQL: RawByteString): Boolean;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  Result := Execute(WSQL);
end;

function TZAdoStatement.GetMoreResults: Boolean;
var
  RC: OleVariant;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      SSQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
  end;
end;

{ TZAdoPreparedStatement }

constructor TZAdoPreparedStatement.Create(Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  FAdoCommand := CoCommand.Create;
  inherited Create(Connection, SQL, Info);
  FAdoCommand.CommandText := WSQL;
  FAdoCommand._Set_ActiveConnection((Connection as IZAdoConnection).GetAdoConnection);
end;

destructor TZAdoPreparedStatement.Destroy;
begin
  AdoRecordSet := nil;
  inherited;
  FAdoCommand := nil;
end;

procedure TZAdoPreparedStatement.PrepareInParameters;
begin
  if InParamCount > 0 then
    RefreshParameters(FAdoCommand);
  FAdoCommand.Prepared := True;
end;

procedure TZAdoPreparedStatement.BindInParameters;
var I: Integer;
begin
  if InParamCount = 0 then
    Exit
  else
    for i := 0 to InParamCount-1 do
      if DefVarManager.IsNull(InParamValues[i]) then
        if (InParamDefaultValues[i] <> '') and (UpperCase(InParamDefaultValues[i]) <> 'NULL') and
          StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
        begin
          DefVarManager.SetAsString(InParamValues[i], InParamDefaultValues[i]);
          ADOSetInParam(FAdoCommand, (Connection as IZAdoConnection), InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
        end
        else
          ADOSetInParam(FAdoCommand, (Connection as IZAdoConnection), InParamCount, I+1, InParamTypes[i], NullVariant, adParamInput)
      else
        ADOSetInParam(FAdoCommand, (Connection as IZAdoConnection), InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAdoPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if not ExecutePrepared then
    while (not GetMoreResults) and (LastUpdateCount > -1) do ;
  Result := LastResultSet;
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
function TZAdoPreparedStatement.ExecutePrepared: Boolean;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;

  Prepare;
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
    LastResultSet := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := RC;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, Connection.GetIZPlainDriver.GetProtocol, SQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, Connection.GetIZPlainDriver.GetProtocol, SQL, 0, E.Message);
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
    LastResultSet := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
  end;
end;

procedure TZAdoPreparedStatement.Unprepare;
begin
  if FAdoCommand.Prepared then
    FAdoCommand.Prepared := False;
  inherited;
end;

{ TZAdoCallableStatement }

constructor TZAdoCallableStatement.Create(Connection: IZConnection;
  SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FAdoCommand := CoCommand.Create;
  FAdoCommand.CommandText := WSQL;
  FAdoCommand._Set_ActiveConnection((Connection as IZAdoConnection).GetAdoConnection);
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
        ColumnLabel := FAdoCommand.Parameters.Item[i].Name;
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
      for i := 1 to ColumnsInfo.Count do
        case TZColumnInfo(ColumnsInfo[i-1]).ColumnType of
          stBoolean:
            RS.UpdateBoolean(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stByte, stShort, stInteger, stLong:
            RS.UpdateInt(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stFloat, stDouble, stBigDecimal:
            RS.UpdateFloat(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stString:
            RS.UpdateString(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stAsciiStream:
            begin
              Stream := TStringStream.Create(AnsiString(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value));
              RS.UpdateAsciiStream(I, Stream);
              Stream.Free;
            end;
          stUnicodeString:
            RS.UpdateUnicodeString(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stUnicodeStream:
            begin
              Stream := WideStringStream(WideString(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value));
              RS.UpdateUnicodeStream(I, Stream);
              FreeAndNil(Stream);
            end;
          stBytes:
            RS.UpdateBytes(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stDate:
            RS.UpdateDate(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stTime:
            RS.UpdateTime(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stTimestamp:
            RS.UpdateTimestamp(i, FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
          stBinaryStream:
            begin
              if VarIsStr(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value) then
              begin
                Stream := TStringStream.Create(AnsiString(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value));
                RS.UpdateBinaryStream(I, Stream);
                FreeAndNil(Stream);
              end
              else
                if VarIsArray(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value) then
                begin
                  P := VarArrayLock(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
                  try
                    Stream := TMemoryStream.Create;
                    Stream.Size := VarArrayHighBound(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value, 1)+1;
                    System.Move(P^, TMemoryStream(Stream).Memory^, Stream.Size);
                    RS.UpdateBinaryStream(I, Stream);
                    FreeAndNil(Stream);
                  finally
                    VarArrayUnLock(FAdoCommand.Parameters.Item[IndexAlign[i-1]].Value);
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
    if Assigned(Stream) then Stream.Free;

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
    LastResultSet := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := RC;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, Connection.GetIZPlainDriver.GetProtocol, SQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, Connection.GetIZPlainDriver.GetProtocol, SQL, 0, E.Message);
      raise;
    end;
  end
end;

procedure TZAdoCallableStatement.RegisterParamType(ParameterIndex: Integer;
  ParamType: Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if Length(FDirectionTypes) < ParameterIndex then
    SetLength(FDirectionTypes, ParameterIndex);

  case Self.FDBParamTypes[ParameterIndex-1] of
    1: //ptInput
      FDirectionTypes[ParameterIndex-1] := adParamInput;
    2: //ptOut
      FDirectionTypes[ParameterIndex-1] := adParamOutput;
    3: //ptInputOutput
      FDirectionTypes[ParameterIndex-1] := adParamInputOutput;
    4: //ptResult
      FDirectionTypes[ParameterIndex-1] := adParamReturnValue;
    else
      //ptUnknown
      FDirectionTypes[ParameterIndex-1] := adParamUnknown;
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
    LastResultSet := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
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
  V: Variant;
  P: Pointer;
  TempBlob: IZBLob;
begin

  if ParameterIndex > OutParamCount then
    Result := NullVariant
  else
  begin
    Temp := FAdoCommand.Parameters.Item[ParameterIndex - 1].Value;

    case ConvertAdoToSqlType(FAdoCommand.Parameters.Item[ParameterIndex - 1].Type_,
      ConSettings.CPType) of
      stBoolean:
        DefVarManager.SetAsBoolean(Result, Temp);
      stByte, stShort, stInteger, stLong:
        DefVarManager.SetAsInteger(Result, Temp);
      stFloat, stDouble, stBigDecimal:
        DefVarManager.SetAsFloat(Result, Temp);
      stString, stAsciiStream:
        DefVarManager.SetAsString(Result, Temp);
      stUnicodeString, stUnicodeStream:
        DefVarManager.SetAsUnicodeString(Result, Temp);
      stBytes:
        DefVarManager.SetAsBytes(Result, VarToBytes(Temp));
      stDate, stTime, stTimestamp:
        DefVarManager.SetAsDateTime(Result, Temp);
      stBinaryStream:
        begin
          if VarIsStr(V) then
          begin
            TempBlob := TZAbstractBlob.CreateWithStream(nil, GetConnection);
            TempBlob.SetString(AnsiString(V));
          end
          else
            if VarIsArray(V) then
            begin
              P := VarArrayLock(V);
              try
                TempBlob := TZAbstractBlob.CreateWithData(P, VarArrayHighBound(V, 1)+1, GetConnection);
              finally
                VarArrayUnLock(V);
              end;
            end;
          DefVarManager.SetAsInterface(Result, TempBlob);
          TempBlob := nil;
        end
      else
        DefVarManager.SetNull(Result);
    end;
  end;

  LastWasNull := DefVarManager.IsNull(Result) or VarIsNull(Temp) or VarIsClear(Temp);
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
        if DefVarManager.IsNull(InParamValues[i]) then
          if (InParamDefaultValues[i] <> '') and (UpperCase(InParamDefaultValues[i]) <> 'NULL') and
            StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
          begin
            DefVarManager.SetAsString(InParamValues[i], InParamDefaultValues[i]);
            ADOSetInParam(FAdoCommand, (Connection as IZAdoConnection), InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
          end
          else
            ADOSetInParam(FAdoCommand, (Connection as IZAdoConnection), InParamCount, I+1, InParamTypes[i], NullVariant, FDirectionTypes[i])
        else
          ADOSetInParam(FAdoCommand, (Connection as IZAdoConnection), InParamCount, I+1, InParamTypes[i], InParamValues[i], FDirectionTypes[i])
      else
        ADOSetInParam(FAdoCommand, (Connection as IZAdoConnection), InParamCount, I+1, InParamTypes[i], NullVariant, FDirectionTypes[i]);
end;

end.



