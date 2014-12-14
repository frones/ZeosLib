{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           OleDB Database Connectivity Classes           }
{                                                         }
{            Originally written by EgonHugeist            }
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

unit ZDbcOleDBStatement;

interface

{$I ZDbc.inc}
{.$IFDEF ENABLE_OLEDB}
{$IFDEF WIN64}
{$ALIGN 8}
{$ELSE}
{$ALIGN 2}
{$ENDIF}
{$MINENUMSIZE 4}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  ZCompatibility, {$IFDEF OLD_FPC}ZClasses, {$ENDIF} ZSysUtils, ZOleDB,
  ZDbcOleDBUtils, ZDbcIntfs, ZDbcStatement, ZDbcAdo, ZPlainAdo, ZVariant;

type
  IZOleDBPreparedStatement = Interface(IZPreparedStatement)
    ['{42A4A633-C63D-4EFA-A8BC-CF755237D0AD}']
    function GetInternalBufferSize: Integer;
  End;
  {** Implements Prepared ADO Statement. }
  TZOleDBPreparedStatement = class(TZAbstractPreparedStatement,
    IZOleDBPreparedStatement)
  private
    FMultipleResults: IMultipleResults;
    FZBufferSize: Integer;
    FEnhancedColInfo: Boolean;
    FCommand: ICommandText;
    FParameterAccessor: IAccessor;
    FDBBindingArray: TDBBindingDynArray;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FDBUPARAMS: DB_UPARAMS;
    FRowSize: NativeUInt;
    FArrayOffSet: DB_UPARAMS;
    FDBParams: TDBParams;
    FRowsAffected: DBROWCOUNT;
    FParamsBuffer: TByteDynArray;
    FTempLobs: TInterfacesDynArray; //Temporary storage for LOB's if a conversion from different Types than IZLob's is required. May be dropped if someone know how to play with IPersistStream
    procedure CalcParamSetsAndBufferSize;
  protected
    function GetInternalBufferSize: Integer;
  protected //overrides
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

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; overload; override;
    function GetMoreResults(var RS: IZResultSet): Boolean; reintroduce; overload;

    procedure SetDataArray(ParameterIndex: Integer; const Value;
      const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
  end;
  TZOleDBStatement = class(TZOleDBPreparedStatement);

{.$ENDIF ENABLE_OLEDB}
implementation
{.$IFDEF ENABLE_OLEDB}

uses
  Variants, ComObj, Math,
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs{$ELSE} Contnrs{$ENDIF},
  ZDbcOleDB, ZDbcOleDBResultSet, ZEncoding, ZDbcLogging, ZDbcCachedResultSet,
  ZDbcResultSet, ZFastCode, ZDbcMetadata, ZDbcResultSetMetadata, ZDbcUtils, ZMessages;

{ TZOleDBPreparedStatement }

constructor TZOleDBPreparedStatement.Create(Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  OleCheck((Connection as IZOleDBConnection).GetIDBCreateCommand.CreateCommand(nil, IID_ICommandText,IUnknown(FCommand)));
  inherited Create(Connection, SQL, Info);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072); //by default 128KB
  FEnhancedColInfo := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'enhanced_column_info', 'True'));
end;

constructor TZOleDBPreparedStatement.Create(Connection: IZConnection;
  const Info: TStrings);
begin
  Create(Connection, '', Info);
end;

destructor TZOleDBPreparedStatement.Destroy;
begin
  inherited Destroy;
  FCommand := nil;
end;

procedure TZOleDBPreparedStatement.CalcParamSetsAndBufferSize;
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

function TZOleDBPreparedStatement.GetInternalBufferSize: Integer;
begin
  Result := FZBufferSize;
end;

procedure TZOleDBPreparedStatement.Prepare;
var
  FOlePrepareCommand: ICommandPrepare;
begin
  if Not Prepared then //prevent PrepareInParameters
  begin
    try
      OleDBCheck(fCommand.SetCommandText(DBGUID_DEFAULT, Pointer(WSQL)));
      OleCheck(fCommand.QueryInterface(IID_ICommandPrepare, FOlePrepareCommand));
      OleDBCheck(FOlePrepareCommand.Prepare(0)); //unknown count of executions
    finally
      FOlePrepareCommand := nil;
    end;
    inherited Prepare;
  end
  else
    if Assigned(FParameterAccessor) and ((ArrayCount > 0) and
       (FDBParams.cParamSets = 0)) or //new arrays have been set
       ((ArrayCount = 0) and (FDBParams.cParamSets > 1)) then //or single exec follows
      CalcParamSetsAndBufferSize;
end;

procedure TZOleDBPreparedStatement.PrepareInParameters;
var
  FNamesBuffer: PPOleStr; //we don't need this here except as param!
  FParamInfoArray: PDBParamInfoArray;
  FCommandWithParameters: ICommandWithParameters;
begin
  {check out the Parameter informations }
  if InParamCount >0 then
  begin
    FParamInfoArray := nil; FNamesBuffer := nil;
    OleCheck(FCommand.QueryInterface(IID_ICommandWithParameters, FCommandWithParameters));
    try
      FCommandWithParameters.GetParameterInfo(FDBUPARAMS,PDBPARAMINFO(FParamInfoArray), FNamesBuffer);
      Assert(FDBUPARAMS = Cardinal(InParamCount), SInvalidInputParameterCount);
      if FDBUPARAMS > 0 then
      begin
        OleCheck(FCommand.QueryInterface(IID_IAccessor, FParameterAccessor));
        SetLength(FDBBINDSTATUSArray, FDBUPARAMS);
        FRowSize := PrepareOleParamDBBindings(FDBUPARAMS, FDBBindingArray,
          InParamTypes, FParamInfoArray, FTempLobs);
        CalcParamSetsAndBufferSize;
        Assert(FDBParams.hAccessor = 1, 'Accessor handle should be unique!');
      end
      else
        FDBParams.cParamSets := 0;
    finally
      if Assigned(FParamInfoArray) then (GetConnection as IZOleDBConnection).GetMalloc.Free(FParamInfoArray);
      if Assigned(FNamesBuffer) then (GetConnection as IZOleDBConnection).GetMalloc.Free(FNamesBuffer);
      FCommandWithParameters := nil;
    end;
  end;
end;

procedure TZOleDBPreparedStatement.BindInParameters;
begin
  if FDBUPARAMS = 0 then
    Exit
  else
    if ArrayCount = 0 then
    begin
      FRowsAffected := 0; //init
      OleBindParams(FDBParams, ConSettings, FDBBindingArray,
        InParamValues, InParamTypes, ClientVarManager);
    end
    else
    begin
      FRowsAffected := 0; //init
      LastUpdateCount := 0;
      while True do
      begin
        if (FArrayOffSet+FDBParams.cParamSets >= Cardinal(ArrayCount)) then
        begin
          FDBParams.cParamSets := DBLENGTH(ArrayCount) - FArrayOffSet;
          OleBindArrayParams(FDBParams, FArrayOffSet, FRowSize, ConSettings,
            FDBBindingArray, ClientVarManager, InParamValues, FTempLobs);
          FRowsAffected := LastUpdateCount; //restore for final execution!
          FArrayOffSet := 0; //Reset!
          Break
        end
        else
        begin
          OleBindArrayParams(FDBParams, FArrayOffSet, FRowSize, ConSettings,
            FDBBindingArray, ClientVarManager, InParamValues, FTempLobs);
          (FCommand as ICommand).Execute(nil,DB_NULLGUID,FDBParams,@FRowsAffected,nil);
          Inc(FArrayOffSet, FDBParams.cParamSets);
          LastUpdateCount := LastUpdateCount + FRowsAffected;
        end
      end
    end
end;

procedure TZOleDBPreparedStatement.UnPrepareInParameters;
var
  FAccessorRefCount: DBREFCOUNT;
begin
  if Assigned(FParameterAccessor) then
  begin
    //don't forgett to release the Accessor else we're leaking mem on Server!
    FParameterAccessor.ReleaseAccessor(FDBParams.hAccessor, @FAccessorRefCount);
    FDBParams.hAccessor := 0;
    FParameterAccessor := nil;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOleDBPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  FRowSet: IRowSet;
begin
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close; //Note keep track we close the RS and DO NOT Try to resync them!
  FOpenResultSet := nil;
  Result := nil;
  Prepare;
  BindInParameters;
  try
    FRowsAffected := DB_COUNTUNAVAILABLE;
    OleDBCheck((FCommand as ICommand).Execute(nil, IID_IMultipleResults,
      FDBParams,@FRowsAffected,@FMultipleResults));
    if Assigned(FMultipleResults) then
    begin
      FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
        IID_IRowset, @FRowsAffected, @FRowSet);
      Result := GetCurrentResultSet(FRowSet, Self, SQL, ConSettings, FZBufferSize,
        FEnhancedColInfo, FOpenResultSet);
    end;
    LastUpdateCount := FRowsAffected;
    if not Assigned(Result) then
    begin
      while (not GetMoreResults(Result)) and (LastUpdateCount > -1) do ;
      FOpenResultSet := Pointer(Result);
    end;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E:Exception do
      raise;
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
function TZOleDBPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  BindInParameters;
  try
    FRowsAffected := DB_COUNTUNAVAILABLE;
    OleDBCheck(FCommand.Execute(nil, DB_NULLGUID,FDBParams,@FRowsAffected,nil));
    LastUpdateCount := FRowsAffected;
    Result := LastUpdateCount;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    raise
  end;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZOleDBPreparedStatement.ExecutePrepared: Boolean;
var
  FRowSet: IRowSet;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;

  Prepare;
  BindInParameters;
  try
    LastUpdateCount := FRowsAffected; //store tempory possible array bound update-counts
    FRowsAffected := DB_COUNTUNAVAILABLE;
    OleDbCheck((FCommand as ICommand).Execute(nil, IID_IMultipleResults,
      FDBParams,@FRowsAffected,@FMultipleResults));
    if Assigned(FMultipleResults) then
    begin
      FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
        IID_IRowset, @FRowsAffected, @FRowSet);
      LastResultSet := GetCurrentResultSet(FRowSet, Self, SQL, ConSettings, FZBufferSize,
        FEnhancedColInfo, FOpenResultSet);
    end;
    LastUpdateCount := LastUpdateCount + FRowsAffected;

    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    raise
  end;
end;

function TZOleDBPreparedStatement.GetMoreResults: Boolean;
var
  FRowSet: IRowSet;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(FMultipleResults) then
  begin
    FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
      IID_IRowset, @FRowsAffected, @FRowSet);
    LastResultSet := GetCurrentResultSet(FRowSet, Self, SQL, ConSettings,
      FZBufferSize, FEnhancedColInfo, FOpenResultSet);
    Result := Assigned(LastResultSet);
    LastUpdateCount := FRowsAffected;
  end;
end;

function TZOleDBPreparedStatement.GetMoreResults(var RS: IZResultSet): Boolean;
var
  FRowSet: IRowSet;
begin
  if Assigned(FMultipleResults) then
  begin
    FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
      IID_IRowset, @FRowsAffected, @FRowSet);
    RS := GetCurrentResultSet(FRowSet, Self, SQL, ConSettings, FZBufferSize,
      FEnhancedColInfo, FOpenResultSet);
    Result := Assigned(RS);
    LastUpdateCount := FRowsAffected;
  end
  else
    Result := False;
end;

procedure TZOleDBPreparedStatement.Unprepare;
var
  Status: HRESULT;
  FRowSet: IRowSet;
begin
  if Prepared then
  begin
    try
      if FMultipleResults <> nil then
      repeat
        FRowSet := nil;
        Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
          IID_IRowset, @FRowsAffected, @FRowSet);
      until (FRowSet = nil) or (Status = DB_S_NORESULT);
      FMultipleResults := nil;
      inherited Unprepare;
      (FCommand as ICommandPrepare).UnPrepare;
    except
      raise;
    end;
  end;
end;

procedure TZOleDBPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
begin
  if ParameterIndex = FirstDbcIndex then
  begin
    FArrayOffSet := 0;
    FDBParams.cParamSets := 0;
  end;
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
end;

{.$ENDIF ENABLE_OLEDB}
end.



