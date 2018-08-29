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

{$IFDEF WIN64}
{$ALIGN 8}
{$ELSE}
{$ALIGN 2}
{$ENDIF}
{$MINENUMSIZE 4}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  ZCompatibility, ZSysUtils, ZOleDB,
  ZDbcOleDBUtils, ZDbcIntfs, ZDbcStatement, ZVariant;

type
  IZOleDBPreparedStatement = Interface(IZPreparedStatement)
    ['{42A4A633-C63D-4EFA-A8BC-CF755237D0AD}']
    function GetInternalBufferSize: Integer;
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
    function GetNewRowSet(var RowSet: IRowSet): Boolean;
  End;
  {** Implements Prepared ADO Statement. }
  TZOleDBPreparedStatement = class(TZAbstractPreparedStatement,
    IZOleDBPreparedStatement)
  private
    FMultipleResults: IMultipleResults;
    FZBufferSize, fStmtTimeOut: Integer;
    fDEFERPREPARE: Boolean;
    FInMemoryDataLobs: Boolean;
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
    fMoreResultsIndicator: TZMoreResultsIndicator;
    procedure CalcParamSetsAndBufferSize;
    procedure PrepareOpenedResultSetsForReusing;
  protected //overrides
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings); overload;
    constructor Create(const Connection: IZConnection; const Info: TStrings); overload;
    destructor Destroy; override;

    procedure Close; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; overload; override;
    function GetMoreResults(var RS: IZResultSet): Boolean; reintroduce; overload;

    procedure SetDataArray(ParameterIndex: Integer; const Value;
      const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;

  protected //interface based!
    function GetInternalBufferSize: Integer;
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
    function GetNewRowSet(var RowSet: IRowSet): Boolean;
  end;
  TZOleDBStatement = class(TZOleDBPreparedStatement);

implementation

uses
  Variants, Math,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ZDbcOleDB, ZDbcOleDBResultSet, ZEncoding, ZDbcLogging,
  ZFastCode, ZDbcMetadata, ZDbcUtils, ZMessages, ZClasses;

{ TZOleDBPreparedStatement }

constructor TZOleDBPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072); //by default 128KB
  FInMemoryDataLobs := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'InMemoryDataLobs', 'False'));
  fStmtTimeOut := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'StatementTimeOut', ''), 60); //execution timeout in seconds by default 1 min
  fDEFERPREPARE := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'preferprepared', 'True'));
  FMultipleResults := nil;
end;

constructor TZOleDBPreparedStatement.Create(const Connection: IZConnection;
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

procedure TZOleDBPreparedStatement.Close;
begin
  inherited Close;
  FCommand := nil;
end;

procedure TZOleDBPreparedStatement.Prepare;
var
  FOlePrepareCommand: ICommandPrepare;
begin
  if Not Prepared then //prevent PrepareInParameters
  begin
    FCommand := (Connection as IZOleDBConnection).CreateCommand;
    try
      SetOleCommandProperties(FCommand, fStmtTimeOut, Connection.GetServerProvider, (Connection as IZOleDBConnection).SupportsMARSConnection, fDEFERPREPARE);
      OleDBCheck(fCommand.SetCommandText(DBGUID_DEFAULT, Pointer(WSQL)));
      OleCheck(fCommand.QueryInterface(IID_ICommandPrepare, FOlePrepareCommand));
      if fDEFERPREPARE then
        OleDBCheck(FOlePrepareCommand.Prepare(0)); //unknown count of executions
    finally
      FOlePrepareCommand := nil;
    end;
    if Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets then
      fMoreResultsIndicator := mriUnknown
    else
      fMoreResultsIndicator := mriHasNoMoreResults;
    inherited Prepare;
  end
  else
  begin
    FMultipleResults := nil; //release this interface! else we can't free the command in some tests
    if Assigned(FParameterAccessor) and ((ArrayCount > 0) and
       (FDBParams.cParamSets = 0)) or //new arrays have been set
       ((ArrayCount = 0) and (FDBParams.cParamSets > 1)) then //or single exec follows
      CalcParamSetsAndBufferSize;
  end;
end;

procedure TZOleDBPreparedStatement.PrepareInParameters;
var
  FNamesBuffer: PPOleStr; //we don't need this here except as param!
  FParamInfoArray: PDBParamInfoArray;
  FCommandWithParameters: ICommandWithParameters;
  DescripedDBPARAMINFO: TDBParamInfoDynArray;
begin
  {check out the Parameter informations }
  if InParamCount >0 then
  begin
    FParamInfoArray := nil; FNamesBuffer := nil;
    OleCheck(FCommand.QueryInterface(IID_ICommandWithParameters, FCommandWithParameters));
    try
      if fDEFERPREPARE then
        FCommandWithParameters.GetParameterInfo(FDBUPARAMS,PDBPARAMINFO(FParamInfoArray), FNamesBuffer)
      else begin
        FDBUPARAMS := InParamCount;
        InitOleParamDBBindings(DescripedDBPARAMINFO, InParamTypes, InParamValues, ClientVarManager);
        FParamInfoArray := Pointer(DescripedDBPARAMINFO);
      end;
      if FDBUPARAMS <> Cardinal(InParamCount) then
        raise EZSQLException.Create(SInvalidInputParameterCount);
      if FDBUPARAMS > 0 then begin
        OleCheck(FCommand.QueryInterface(IID_IAccessor, FParameterAccessor));
        SetLength(FDBBINDSTATUSArray, FDBUPARAMS);
        FRowSize := PrepareOleParamDBBindings(FDBUPARAMS, FDBBindingArray,
          InParamTypes, FParamInfoArray, FTempLobs);
        CalcParamSetsAndBufferSize;
        if not (FDBParams.hAccessor = 1) then
          raise EZSQLException.Create('Accessor handle should be unique!');
      end else begin
        { init ! }
        FDBParams.pData := nil;
        FDBParams.cParamSets := 0;
        FDBParams.hAccessor := 0;
      end;
    finally
      if Assigned(FParamInfoArray) and (Pointer(FParamInfoArray) <> Pointer(DescripedDBPARAMINFO)) then
        (GetConnection as IZOleDBConnection).GetMalloc.Free(FParamInfoArray);
      if Assigned(FNamesBuffer) then (GetConnection as IZOleDBConnection).GetMalloc.Free(FNamesBuffer);
      FCommandWithParameters := nil;
    end;
  end;
end;

procedure TZOleDBPreparedStatement.PrepareOpenedResultSetsForReusing;
  procedure SetMoreResInd;
  begin
    if (fMoreResultsIndicator = mriUnknown) and Assigned(FMultipleResults) then begin
      if GetMoreResults and (Assigned(LastResultSet) or (FRowsAffected <> -1)) then
        fMoreResultsIndicator := mriHasMoreResults
      else
        fMoreResultsIndicator := mriHasNoMoreResults;
    end;
    if Assigned(LastResultSet) then begin
      LastResultSet.Close;
      LastResultSet := nil;
    end;
  end;
begin
  if Assigned(FOpenResultSet) then
    if fMoreResultsIndicator <> mriHasNoMoreResults then begin
      if (Pointer(LastResultSet) = FOpenResultSet) then begin
        LastResultSet.Close;
        LastResultSet := nil;
      end else begin
        IZResultSet(FOpenResultSet).Close;
        FOpenResultSet := nil;
      end;
      SetMoreResInd;
    end else
      IZResultSet(FOpenResultSet).ResetCursor;
  if Assigned(LastResultSet) then begin
    if (fMoreResultsIndicator <> mriHasNoMoreResults) then begin
      LastResultSet.Close;
      LastResultSet := nil;
      SetMoreResInd;
    end else
      LastResultSet.ResetCursor;
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
          OleDbCheck((FCommand as ICommand).Execute(nil,DB_NULLGUID,FDBParams,@FRowsAffected,nil));
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
  PrepareOpenedResultSetsForReusing;
  Prepare;
  BindInParameters;
  try
    FRowsAffected := DB_COUNTUNAVAILABLE;
    FRowSet := nil;
    if Assigned(FOpenResultSet) then
      Result := IZResultSet(FOpenResultSet)
    else begin
      Result := nil;
      if Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets then
      begin
        OleDbCheck((FCommand as ICommand).Execute(nil, IID_IMultipleResults,
          FDBParams,@FRowsAffected,@FMultipleResults));
        if Assigned(FMultipleResults) then
          OleDbCheck(FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
            IID_IRowset, @FRowsAffected, @FRowSet), FDBBINDSTATUSArray);
      end
      else
        OleDbCheck((FCommand as ICommand).Execute(nil, IID_IRowset,
          FDBParams,@FRowsAffected,@FRowSet), FDBBINDSTATUSArray);
      Result := GetCurrentResultSet(FRowSet, Self, SQL, ConSettings, FZBufferSize,
        ChunkSize, FInMemoryDataLobs, FOpenResultSet);
      LastUpdateCount := FRowsAffected;
      if not Assigned(Result) then
        while (not GetMoreResults(Result)) and (LastUpdateCount > -1) do ;
    end;
  finally
    FRowSet := nil;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
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

  FRowsAffected := DB_COUNTUNAVAILABLE; //init
  try
    OleDBCheck(FCommand.Execute(nil, DB_NULLGUID,FDBParams,@FRowsAffected,nil));
    LastUpdateCount := FRowsAffected;
    Result := LastUpdateCount;
  finally
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
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
  PrepareOpenedResultSetsForReusing;
  LastUpdateCount := -1;

  Prepare;
  BindInParameters;
  LastUpdateCount := FRowsAffected; //store tempory possible array bound update-counts
  FRowsAffected := DB_COUNTUNAVAILABLE;
  try
    FRowSet := nil;
    if Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets then
    begin
      OleDbCheck((FCommand as ICommand).Execute(nil, IID_IMultipleResults,
        FDBParams,@FRowsAffected,@FMultipleResults));
      if Assigned(FMultipleResults) then
        OleDbCheck(FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
          IID_IRowset, @FRowsAffected, @FRowSet));
    end
    else
      OleDbCheck((FCommand as ICommand).Execute(nil, IID_IRowset,
        FDBParams,@FRowsAffected,@FRowSet));

    LastResultSet := GetCurrentResultSet(FRowSet, Self, SQL, ConSettings, FZBufferSize,
      ChunkSize, FInMemoryDataLobs, FOpenResultSet);
    LastUpdateCount := LastUpdateCount + FRowsAffected;
    Result := Assigned(LastResultSet);
  finally
    FRowSet := nil;
    if LastResultSet = nil then FMultipleResults := nil;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  end;
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
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
    LastResultSet := GetCurrentResultSet(FRowSet, Self, SQL, ConSettings, FZBufferSize,
      ChunkSize, FInMemoryDataLobs, FOpenResultSet);
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
      ChunkSize, FInMemoryDataLobs, FOpenResultSet);
    Result := Assigned(RS);
    LastUpdateCount := FRowsAffected;
  end
  else
    Result := False;
end;

function TZOleDBPreparedStatement.GetMoreResultsIndicator: TZMoreResultsIndicator;
begin
  Result := fMoreResultsIndicator;
end;

function TZOleDBPreparedStatement.GetNewRowSet(var RowSet: IRowSet): Boolean;
begin
  RowSet := nil;
  if Prepared then begin
    OleDbCheck((FCommand as ICommand).Execute(nil, IID_IRowset,
      FDBParams,@FRowsAffected,@RowSet));
    Result := Assigned(RowSet);
  end else Result := False;
end;

procedure TZOleDBPreparedStatement.Unprepare;
var
  Status: HRESULT;
  FRowSet: IRowSet;
begin
  if Prepared then
  begin
    try
      inherited Unprepare;
      if FMultipleResults <> nil then
      repeat
        FRowSet := nil;
        Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_DEFAULT),
          IID_IRowset, @FRowsAffected, @FRowSet);
        //Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_DEFAULT),
          //IID_IRowset, @FRowsAffected, @FRowSet);
      until Failed(Status) or (Status = DB_S_NORESULT);
      {OleDBCheck}((FCommand as ICommandPrepare).UnPrepare);
    finally
      FCommand := nil;
      FMultipleResults := nil;
    end;
  end;
end;

procedure TZOleDBPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
begin
  if ParameterIndex = FirstDbcIndex then begin
    FArrayOffSet := 0;
    FDBParams.cParamSets := 0;
  end;
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
end;

procedure TZOleDBPreparedStatement.SetMoreResultsIndicator(
  Value: TZMoreResultsIndicator);
begin
  fMoreResultsIndicator := Value;
end;

end.
