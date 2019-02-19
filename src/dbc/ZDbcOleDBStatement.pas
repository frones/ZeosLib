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

{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
{$IFDEF WIN64}
{$ALIGN 8}
{$ELSE}
{$ALIGN 2}
{$ENDIF}
{$MINENUMSIZE 4}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZCompatibility, ZSysUtils, ZOleDB, ZDbcLogging, ZDbcStatement,
  ZDbcOleDBUtils, ZDbcIntfs, ZVariant, ZDbcProperties;

type
  IZOleDBPreparedStatement = Interface(IZStatement)
    ['{42A4A633-C63D-4EFA-A8BC-CF755237D0AD}']
    function GetInternalBufferSize: Integer;
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
    function GetNewRowSet(var RowSet: IRowSet): Boolean;
  End;

  {** Implements Prepared ADO Statement. }
  TZAbstractOleDBStatement = class(TZUTF16ParamDetectPreparedStatement,
    IZOleDBPreparedStatement)
  private
    FMultipleResults: IMultipleResults;
    FZBufferSize, fStmtTimeOut: Integer;
    FInMemoryDataLobs: Boolean;
    FCommand: ICommandText;
    FRowSize: NativeUInt;
    FDBParams: TDBParams;
    FRowsAffected: DBROWCOUNT;
    fMoreResultsIndicator: TZMoreResultsIndicator;
    FDBBINDSTATUSArray: TDBBINDSTATUSDynArray;
    FSupportsMultipleResultSets: Boolean;
    FHasOutParams: Boolean;
    procedure CheckError(Status: HResult; LoggingCategory: TZLoggingCategory;
       const DBBINDSTATUSArray: TDBBINDSTATUSDynArray = nil);
    procedure PrepareOpenedResultSetsForReusing;
  protected
    function AlignParamterIndex2ResultSetIndex(Value: Integer): Integer; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);
    destructor Destroy; override;

    procedure AfterClose; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; overload; override;
    function GetMoreResults(var RS: IZResultSet): Boolean; reintroduce; overload;

    procedure Cancel; override;
  public
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;
  protected //interface based!
    function CreateResultSet(const RowSet: IRowSet): IZResultSet; virtual;
    function GetInternalBufferSize: Integer;
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
    function GetNewRowSet(var RowSet: IRowSet): Boolean;
  end;

  TZOleDBPreparedStatement = class(TZAbstractOleDBStatement, IZPreparedStatement)
  private
    FDBBindingArray: TDBBindingDynArray;
    FParamNamesArray: TStringDynArray;
    FDBUPARAMS: DB_UPARAMS;
    fDEFERPREPARE, //ole: if set the stmt will be prepared immediatelly and we'll try to decribe params
    fBindImmediat, //the param describe did fail! we'll try to bind the params with describe emulation
    fBindAgain, //param type or sizes have been changed need to create a new accessor handle
    fSupportsByRef: Boolean; //are by REF bound values supported by provider?
    FParamsBuffer: TByteDynArray; //our value buffer
    FParameterAccessor: IAccessor;
    FClientCP: Word;
    procedure CalcParamSetsAndBufferSize;
    procedure SetPWideChar(Index: Word; Value: PWideChar; Len: Cardinal);
    procedure SetPAnsiChar(Index: Word; Value: PAnsiChar; Len: Cardinal);
    procedure BindBatchDMLArrays;
    procedure BindRaw(Index: Integer; const Value: RawByteString; CP: Word);
    procedure Dyn_W_Convert(Index, Len: Integer; var Arr: PZArray);
    procedure SetOleCommandProperties;
    procedure InitVaryBind(Index: Integer; Len: Cardinal; _Type: DBTYPE);
    procedure InitFixedBind(Index: Integer; Size: Cardinal; _Type: DBTYPE);
    procedure InitDateBind(Index: Integer; SQLType: TZSQLType);
    procedure InitLongBind(Index: Integer; _Type: DBTYPE);
    procedure SetBindOffsets;
  protected
    function SupportsBidirectionalParams: Boolean; override;

    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure CheckParameterIndex(Value: Integer); override;
    procedure SetParamCount(NewParamCount: Integer); override;
    procedure RaiseUnsupportedParamType(Index: Integer; WType: Word; SQLType: TZSQLType);
    procedure RaiseExceeded(Index: Integer);
    function CreateResultSet(const RowSet: IRowSet): IZResultSet; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);

    procedure Prepare; override;
  public
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;
  public //setters
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual
    procedure SetNull(Index: Integer; {%H-}SQLType: TZSQLType); reintroduce;
    procedure SetBoolean(Index: Integer; Value: Boolean); reintroduce;
    procedure SetByte(Index: Integer; Value: Byte); reintroduce;
    procedure SetShort(Index: Integer; Value: ShortInt); reintroduce;
    procedure SetWord(Index: Integer; Value: Word); reintroduce;
    procedure SetSmall(Index: Integer; Value: SmallInt); reintroduce;
    procedure SetUInt(Index: Integer; Value: Cardinal); reintroduce;
    procedure SetInt(Index: Integer; Value: Integer); reintroduce;
    procedure SetULong(Index: Integer; const Value: UInt64); reintroduce;
    procedure SetLong(Index: Integer; const Value: Int64); reintroduce;
    procedure SetFloat(Index: Integer; Value: Single); reintroduce;
    procedure SetDouble(Index: Integer; const Value: Double); reintroduce;
    procedure SetCurrency(Index: Integer; const Value: Currency); reintroduce;
    procedure SetBigDecimal(Index: Integer; const Value: {$IFDEF BCD_TEST}TBCD{$ELSE}Extended{$ENDIF}); reintroduce;

    procedure SetCharRec(Index: Integer; const Value: TZCharRec); reintroduce;
    procedure SetString(Index: Integer; const Value: String); reintroduce;
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(Index: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(Index: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(Index: Integer; const Value: RawByteString); reintroduce;
    procedure SetUnicodeString(Index: Integer; const Value: ZWideString); reintroduce;

    procedure SetDate(Index: Integer; const Value: TDateTime); reintroduce;
    procedure SetTime(Index: Integer; const Value: TDateTime); reintroduce;
    procedure SetTimestamp(Index: Integer; const Value: TDateTime); reintroduce;

    procedure SetBytes(Index: Integer; const Value: TBytes); reintroduce;
    procedure SetGUID(Index: Integer; const Value: TGUID); reintroduce;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};

    procedure SetDataArray(ParameterIndex: Integer; const Value;
      const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;

    procedure RegisterParameter(Index: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
  end;

  TZOleDBStatement = class(TZAbstractOleDBStatement, IZStatement)
    constructor Create(const Connection: IZConnection; const Info: TStrings);
  end;

  TZOleDBCallableStatementMSSQL = class(TZAbstractCallableStatement_W,
    IZCallableStatement)
  protected
    function CreateExecutionStatement(Mode: TZCallExecKind; const StoredProcName: String): TZAbstractPreparedStatement2; override;
    function SupportsBidirectionalParams: Boolean; override;
    procedure PrepareInParameters; override;
  end;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_OLEDB} //if set we have an empty unit

uses
  Variants, Math,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF}, TypInfo,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} DateUtils,
  ZDbcOleDB, ZDbcOleDBResultSet, ZEncoding, ZDbcOleDBMetadata,
  ZFastCode, ZDbcMetadata, ZDbcUtils, ZMessages, ZClasses, ZDbcResultSet,
  ZDbcCachedResultSet, ZDbcGenericResolver;

{ TZAbstractOleDBStatement }

function TZAbstractOleDBStatement.AlignParamterIndex2ResultSetIndex(
  Value: Integer): Integer;
var I: Integer;
begin
  Result := inherited AlignParamterIndex2ResultSetIndex(Value);
  for i := Value downto 0 do
    if BindList.ParamTypes[i] in [pctUnknown, pctIn] then
      Dec(Result);
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZAbstractOleDBStatement.Cancel;
begin
  if FCommand <> nil
  then CheckError(FCommand.Cancel, lcOther, nil)
  else inherited Cancel;
end;

procedure TZAbstractOleDBStatement.CheckError(Status: HResult;
  LoggingCategory: TZLoggingCategory;
  const DBBINDSTATUSArray: TDBBINDSTATUSDynArray = nil);
begin
  if DriverManager.HasLoggingListener and
     ((LoggingCategory = lcExecute) or (Ord(LoggingCategory) > ord(lcOther))) then
    DriverManager.LogMessage(LoggingCategory, ConSettings^.Protocol, ASQL);
  if Failed(Status) then
    OleDBCheck(Status, SQL, Self, DBBINDSTATUSArray);
end;

constructor TZAbstractOleDBStatement.Create(const Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_InternalBufSize, ''), 131072); //by default 128KB
  FInMemoryDataLobs := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_InMemoryDataLobs, 'False'));
  fStmtTimeOut := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_StatementTimeOut, ''), 60); //execution timeout in seconds by default 1 min
  FSupportsMultipleResultSets := Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets;
  FMultipleResults := nil;
end;

function TZAbstractOleDBStatement.CreateResultSet(const RowSet: IRowSet): IZResultSet;
var
  CachedResolver: IZCachedResolver;
  NativeResultSet: TZOleDBResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  Result := nil;
  if Assigned(RowSet) then begin
    NativeResultSet := TZOleDBResultSet.Create(Self, SQL, RowSet,
      FZBufferSize, ChunkSize, FInMemoryDataLobs);
    if (ResultSetConcurrency = rcUpdatable) or (ResultSetType <> rtForwardOnly) then begin
      if (Connection.GetServerProvider = spMSSQL) and (Self.GetResultSetConcurrency = rcUpdatable)
      then CachedResolver := TZOleDBMSSQLCachedResolver.Create(Self, NativeResultSet.GetMetaData)
      else CachedResolver := TZGenericCachedResolver.Create(Self, NativeResultSet.GetMetaData);
      CachedResultSet := TZOleDBCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(ResultSetConcurrency);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
  end;
  FOpenResultSet := Pointer(Result);
end;

destructor TZAbstractOleDBStatement.Destroy;
begin
  inherited Destroy;
  FCommand := nil;
end;

function TZAbstractOleDBStatement.GetInternalBufferSize: Integer;
begin
  Result := FZBufferSize;
end;

procedure TZAbstractOleDBStatement.AfterClose;
begin
  FCommand := nil;
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractOleDBStatement.Prepare;
begin
  if FCommand = nil then
    FCommand := (Connection as IZOleDBConnection).CreateCommand;
  inherited Prepare;
end;

procedure TZAbstractOleDBStatement.PrepareOpenedResultSetsForReusing;
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

procedure TZAbstractOleDBStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
begin
  inherited ReleaseImmediat(Sender);
  FMultipleResults := nil;
  FCommand := nil;
  SetLength(FDBBINDSTATUSArray, 0);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractOleDBStatement.ExecuteQueryPrepared: IZResultSet;
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
      if FSupportsMultipleResultSets then begin
        CheckError(FCommand.Execute(nil, IID_IMultipleResults, FDBParams,@FRowsAffected,@FMultipleResults),
          lcExecute, fDBBINDSTATUSArray);
        if Assigned(FMultipleResults) then
          CheckError(FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
            IID_IRowset, @FRowsAffected, @FRowSet), lcOther);
      end else
        CheckError(FCommand.Execute(nil, IID_IRowset,
          FDBParams,@FRowsAffected,@FRowSet), lcExecute, fDBBINDSTATUSArray);
      Result := CreateResultSet(FRowSet);
      LastUpdateCount := FRowsAffected;
      if not Assigned(Result) then
        while (not GetMoreResults(Result)) and (LastUpdateCount > -1) do ;
    end;
  finally
    FRowSet := nil;
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
function TZAbstractOleDBStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  BindInParameters;

  FRowsAffected := DB_COUNTUNAVAILABLE; //init
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  CheckError(FCommand.Execute(nil, DB_NULLGUID,FDBParams,@FRowsAffected,nil), lcExecute, FDBBINDSTATUSArray);
  if FHasOutParams then
    LastResultSet := CreateResultSet(nil);
  LastUpdateCount := FRowsAffected;
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
function TZAbstractOleDBStatement.ExecutePrepared: Boolean;
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
    if FSupportsMultipleResultSets then begin
      CheckError(FCommand.Execute(nil, IID_IMultipleResults,
        FDBParams,@FRowsAffected,@FMultipleResults), lcExecute, FDBBINDSTATUSArray);
      if Assigned(FMultipleResults) then
        CheckError(FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
          IID_IRowset, @FRowsAffected, @FRowSet), lcOther);
    end else
      CheckError(FCommand.Execute(nil, IID_IRowset,
        FDBParams,@FRowsAffected,@FRowSet), lcExecute, FDBBINDSTATUSArray);
    LastResultSet := CreateResultSet(FRowSet);
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
function TZAbstractOleDBStatement.GetMoreResults: Boolean;
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
    LastResultSet := CreateResultSet(FRowSet);
    Result := Assigned(LastResultSet);
    LastUpdateCount := FRowsAffected;
  end;
end;

function TZAbstractOleDBStatement.GetMoreResults(var RS: IZResultSet): Boolean;
var
  FRowSet: IRowSet;
begin
  if Assigned(FMultipleResults) then
  begin
    FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_ROWSET),
      IID_IRowset, @FRowsAffected, @FRowSet);
    RS := CreateResultSet(FRowSet);
    Result := Assigned(RS);
    LastUpdateCount := FRowsAffected;
  end
  else
    Result := False;
end;

function TZAbstractOleDBStatement.GetMoreResultsIndicator: TZMoreResultsIndicator;
begin
  Result := fMoreResultsIndicator;
end;

function TZAbstractOleDBStatement.GetNewRowSet(var RowSet: IRowSet): Boolean;
begin
  RowSet := nil;
  if Prepared then begin
    CheckError(FCommand.Execute(nil, IID_IRowset,
      FDBParams,@FRowsAffected,@RowSet), lcExecute);
    Result := Assigned(RowSet);
  end else Result := False;
end;

procedure TZAbstractOleDBStatement.Unprepare;
var
  Status: HRESULT;
  FRowSet: IRowSet;
begin
  if Prepared then
    try
      inherited Unprepare;
      if FMultipleResults <> nil then begin
        repeat
          FRowSet := nil;
          Status := FMultipleResults.GetResult(nil, DBRESULTFLAG(DBRESULTFLAG_DEFAULT),
            IID_IRowset, @FRowsAffected, @FRowSet);
        until Failed(Status) or (Status = DB_S_NORESULT);
        FMultipleResults := nil;
      end;
      CheckError((FCommand as ICommandPrepare).UnPrepare, lcOther, nil);
    finally
      FCommand := nil;
      FMultipleResults := nil;
    end;
end;

procedure TZAbstractOleDBStatement.SetMoreResultsIndicator(
  Value: TZMoreResultsIndicator);
begin
  fMoreResultsIndicator := Value;
end;

{ TZOleDBPreparedStatement }

const OleDbNotNullTable: array[Boolean] of DBSTATUS = (DBSTATUS_S_ISNULL, DBSTATUS_S_OK);
procedure TZOleDBPreparedStatement.BindBatchDMLArrays;
var
  ZData, Data, P: Pointer;
  PLen: PDBLENGTH;
  MaxL, CPL: DBLENGTH;
  ZArray: PZArray;
  I, j: Integer;
  BuffOffSet: NativeUInt;
  SQLType: TZSQLType;
  DateTimeTemp: TDateTime;
  W1, MS: Word;
  WType: Word;
  Native: Boolean;

  (*function IsNotNull(I, j: Cardinal): Boolean;
  var OffSet: NativeUInt;
  begin
    OffSet := (j*fRowSize);
    Result := not IsNullFromArray(ZArray, J);
    PDBSTATUS(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obStatus + OffSet))^ := OleDbNotNullTable[Result];
    if Result then begin
      Data := Pointer(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obValue + OffSet));
      //note PLen is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
      PLen := PDBLENGTH(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obLength + OffSet));
    end;
  end;
  procedure SetData(ByRef: Boolean; P: Pointer; Len: DBLENGTH);
  begin
    PLen^ := Len;
    if ByRef then PPointer(Data)^:= P
    else if (PLen^ >= 0) and (PLen^ <= MaxL)
      then Move(P^, Pointer(Data)^, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(MaxL, PLen^))
      else RaiseExceeded(I);
  end;
  procedure Bind_DBTYPE_BYTES(ByRef: Boolean);
  var TempLob: IZBlob;
    P: Pointer;
    j: Integer;
  begin
    case SQLType of
      stBinaryStream: for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then
                with TInterfaceDynArray(ZData)[J] as IZBLob do
                  SetData(ByRef, TempLob.GetBuffer, TempLob.Length);
      stBytes: for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then
                SetData(ByRef, Pointer(TBytesDynArray(ZData)[J]), Length(TBytesDynArray(ZData)[J]));
      stGUID: for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then
                SetData(ByRef, @TGUIDDynArray(ZData)[J].D1, SizeOf(TGUID));
      else
        raise Exception.Create('Unsupported Byte-Array Variant');
    end;
  end;
  //*)
  procedure Bind_DBTYPE_BYTES(ByRef: Boolean);
  var TempLob: IZBlob;
    P: Pointer;
  begin
    case SQLType of
      stBinaryStream: begin
                TempLob := TInterfaceDynArray(ZData)[J] as IZBLob;
                PLen^ := TempLob.Length;
                P := TempLob.GetBuffer;
                TempLob := nil;
              end;
      stBytes: begin
                PLen^ := Length(TBytesDynArray(ZData)[J]);
                P := Pointer(TBytesDynArray(ZData)[J]);
              end;
      stGUID: begin
                PLen^ := SizeOf(TGUID);
                P := @TGUIDDynArray(ZData)[J].D1;
              end;
      else
        raise Exception.Create('Unsupported Byte-Array Variant');
    end;
    if ByRef then PPointer(Data)^:= P
    else if (PLen^ > 0) and (PLen^ <= MaxL)
      then Move(P^, Pointer(Data)^, {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(MaxL, PLen^))
      else RaiseExceeded(I);
  end;
  procedure Bind_Long_DBTYPE_WSTR_BY_REF;
  var TempLob: IZBlob;
    TmpStream: TStream;
  begin
    TempLob := TInterfaceDynArray(ZData)[J] as IZBLob;
    if not TempLob.IsClob then begin
      TmpStream := GetValidatedUnicodeStream(TempLob.GetBuffer, TempLob.Length, ConSettings, False);
      TempLob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
      TInterfaceDynArray(ZData)[J] := TempLob; //keep mem alive!
      TmpStream.Free;
    end;
    PPointer(Data)^:= TempLob.GetPWideChar;
    PLen^ := TempLob.Length;
  end;
label W_Len, WStr;
begin
  {$R-}
  MaxL := 0; CPL := 0; W1 := 0; Native := False;//satisfy the compiler
  //http://technet.microsoft.com/de-de/library/ms174522%28v=sql.110%29.aspx
  for i := 0 to BindList.Count -1 do begin
    if not (BindList[I].BindType in [zbtRefArray, zbtArray]) then
      Continue;
    ZArray := BindList[I].Value;
    ZData := ZArray.VArray;
    SQLType := TZSQLType(ZArray.VArrayType);
    BuffOffSet := 0;
    WType := fDBBindingArray[i].wType;
    if (Wtype = DBTYPE_WSTR) then begin
      MaxL := fDBBindingArray[i].cbMaxLen -2; //omit trailing zero
      CPL := MaxL shr 1;  //need codepoint len
      if (SQLType in [stString, stUnicodeString]) then
      case ZArray.VArrayVariantType of
        {$IFNDEF UNICODE}
        vtString: if ConSettings^.AutoEncode
                  then W1 := zCP_None
                  else W1 := ConSettings^.CTRL_CP;
        {$ENDIF}
        vtAnsiString: W1 := ZOSCodePage;
        vtUTF8String: W1 := zCP_UTF8;
        vtRawByteString: W1 := FClientCP;
      end;
    end else if (wType = DBTYPE_BYTES) or (wType = DBTYPE_STR) then
      MaxL := fDBBindingArray[i].cbMaxLen - Byte(Ord(wType = DBTYPE_STR))
    else Native := (SQLType2OleDBTypeEnum[SQLType] = wType) and (ZArray.VArrayVariantType = vtNull);
      (*case wType of
        DBTYPE_I1: for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PShortInt(Data)^  := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI1:   for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PByte(Data)^      := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I2:    for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PSmallInt(Data)^  := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI2:   for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PWord(Data)^      := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I4:    for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PInteger(Data)^   := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI4:   for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PCardinal(Data)^  := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I8:    for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PInt64(Data)^     := ArrayValueToInt64(ZArray, j);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        DBTYPE_UI8:   for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PUInt(Data)^       := ArrayValueToUInt64(ZArray, j);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
        DBTYPE_R4:    for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PSingle(Data)^     := ArrayValueToDouble(ZArray, j);
        DBTYPE_R8:    for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PDouble(Data)^     := ArrayValueToDouble(ZArray, j);
        DBTYPE_CY:    for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PCurrency(Data)^   := ArrayValueToCurrency(ZArray, j);
        DBType_BOOL:  for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PWordBool(Data)^   := ArrayValueToBoolean(ZArray, j);
        DBTYPE_DATE, DBTYPE_DBDATE, DBTYPE_DBTIME, DBTYPE_DBTIME2, DBTYPE_DBTIMESTAMP:  for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then begin
            case SQLType of
              stTime:       DateTimeTemp := ArrayValueToTime(ZArray, j, ConSettings.WriteFormatSettings);
              stDate:       DateTimeTemp := ArrayValueToDate(ZArray, j, ConSettings.WriteFormatSettings);
              else          DateTimeTemp := ArrayValueToDateTime(ZArray, j, ConSettings.WriteFormatSettings);
            end;
            case wType of
              DBTYPE_DATE: PDateTime(Data)^ := DateTimeTemp;
              DBTYPE_DBDATE: begin
                  DecodeDate(DateTimeTemp, W1, PDBDate(Data)^.month, PDBDate(Data)^.day);
                  PDBDate(Data)^.year := W1;
                end;
              DBTYPE_DBTIME: DecodeTime(DateTimeTemp, PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second, MS);
              DBTYPE_DBTIME2: begin
                  DecodeTime(DateTimeTemp,
                    PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second, MS);
                    PDBTIME2(Data)^.fraction := MS * 1000000;
                end;
              DBTYPE_DBTIMESTAMP: begin
                  DecodeDate(DateTimeTemp, W1, PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day);
                  PDBTimeStamp(Data)^.year := W1;
                  if SQLType <> stDate then begin
                    DecodeTime(DateTimeTemp, PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute, PDBTimeStamp(Data)^.second, MS);
                    {if fSupportsMilliseconds
                    then} PDBTimeStamp(Data)^.fraction := MS * 1000*1000
                    {else PDBTimeStamp(Data)^.fraction := 0};
                  end else begin
                    PDBTimeStamp(Data)^.hour := 0; PDBTimeStamp(Data)^.minute := 0;
                    PDBTimeStamp(Data)^.second := 0; PDBTimeStamp(Data)^.fraction := 0;
                  end;
                end;
            end;
          end;
        { next types are automatically prepared on binding the arrays }
        DBTYPE_GUID: for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then ArrayValueToGUID(ZArray, j, PGUID(Data));
        DBTYPE_GUID or DBTYPE_BYREF: for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then PPointer(Data)^ := @TGUIDDynArray(ZData)[J].D1;
        DBTYPE_BYTES: Bind_DBTYPE_BYTES(False);
        DBTYPE_BYTES or DBTYPE_BYREF: Bind_DBTYPE_BYTES(True);
        DBTYPE_WSTR, DBTYPE_WSTR or DBTYPE_BYREF: begin
            case SQLType of
             { stBoolean:      FUniTemp := BoolToUnicodeEx(TBooleanDynArray(ZData)[J]);
              stByte:         FUniTemp := IntToUnicode(TByteDynArray(ZData)[J]);
              stShort:        FUniTemp := IntToUnicode(TShortIntDynArray(ZData)[J]);
              stWord:         FUniTemp := IntToUnicode(TWordDynArray(ZData)[J]);
              stSmall:        FUniTemp := IntToUnicode(TSmallIntDynArray(ZData)[J]);
              stLongWord:     FUniTemp := IntToUnicode(TCardinalDynArray(ZData)[J]);
              stInteger:      FUniTemp := IntToUnicode(TIntegerDynArray(ZData)[J]);
              stULong:        FUniTemp := IntToUnicode(TUInt64DynArray(ZData)[J]);
              stLong:         FUniTemp := IntToUnicode(TInt64DynArray(ZData)[J]);
              stFloat:        FUniTemp := FloatToUnicode(TSingleDynArray(ZData)[J]);
              stDouble:       FUniTemp := FloatToUnicode(TDoubleDynArray(ZData)[J]);
              stCurrency:     FUniTemp := FloatToUnicode(TCurrencyDynArray(ZData)[J]);
              stBigDecimal:   FUniTemp := FloatToUnicode(TExtendedDynArray(ZData)[J]);
              stTime:         FUniTemp := DateTimeToUnicodeSQLTime(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);
              stDate:         FUniTemp := DateTimeToUnicodeSQLDate(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);
              stTimeStamp:    FUniTemp := DateTimeToUnicodeSQLTimeStamp(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);}
              stString, stUnicodeString: begin
                case ZArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString, {$ENDIF}
                  vtAnsiString,vtUTF8String,vtRawByteString: for j := 0 to fDBParams.cParamSets-1 do if IsNotNull(I, J) then begin
                      if wType = DBTYPE_WSTR then begin
                        P := Pointer(TRawByteStringDynArray(ZData)[J]);
                        SetData(False, PWideChar(Data), PRaw2PUnicode(PAnsiChar(P), PWideChar(Data), W1, LengthInt(Length(TRawByteStringDynArray(ZData)[J])), LengthInt(CPL)) shl 1);
                      end else begin
                        Dyn_W_Convert(I, Length(TRawByteStringDynArray(ZData)), ZArray);
                        ZData := ZArray.VArray;
                        goto WStr;
                      end;
                  end;
                  {$IFDEF UNICODE}vtString,{$ENDIF} vtUnicodeString: begin
WStr:                 PLen^ := Length(TUnicodeStringDynArray(ZData)[J]) shl 1;
                      if PLen^ > 0 then
                        if wType = DBTYPE_WSTR then begin
                          Move(Pointer(TUnicodeStringDynArray(ZData)[J])^, PWideChar(Data)^, ({$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(PLen^, MaxL)+2));
                          goto W_Len
                        end else
                          PPointer(Data)^ := Pointer(TUnicodeStringDynArray(ZData)[J])
                      else if wType = DBTYPE_WSTR
                        then PWord(Data)^ := 0
                        else PPointer(Data)^ := PEmptyUnicodeString;
                    end;
                  vtCharRec: begin
                      if TZCharRecDynArray(ZData)[J].CP = zCP_UTF16 then begin
                        PLen^ := TZCharRecDynArray(ZData)[J].Len shl 1;
                        if wType = DBTYPE_WSTR
                        then Move(PWideChar(TZCharRecDynArray(ZData)[J].P)^, PWideChar(Data)^, ({$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(PLen^, MaxL)+2))
                        else PPointer(Data)^ := TZCharRecDynArray(ZData)[J].P;
                      end else begin
                        if wType = DBTYPE_WSTR
                        then PLen^ := PRaw2PUnicode(PAnsiChar(TZCharRecDynArray(ZData)[J].P), PWideChar(Data), TZCharRecDynArray(ZData)[J].CP, LengthInt(TZCharRecDynArray(ZData)[J].Len), LengthInt(MaxL))
                        else begin
                          Dyn_W_Convert(I, Length(TZCharRecDynArray(ZData)), ZArray);
                          ZData := ZArray.VArray;
                          goto WStr;
                        end;
                      end;
W_Len:                if PLen^ > MaxL then
                        RaiseExceeded(I);
                    end;
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              end;
              (*stAsciiStream, stUnicodeStream:
                begin
                  TempLob := TInterfaceDynArray(ZData)[J] as IZBLob;
                  if TempLob.IsClob then
                    TempLob.GetPWideChar //make internal conversion first
                  else begin
                    TmpStream := GetValidatedUnicodeStream(TempLob.GetBuffer, TempLob.Length, ConSettings, False);
                    TempLob := TZAbstractClob.CreateWithStream(TmpStream, zCP_UTF16, ConSettings);
                    TInterfaceDynArray(ZData)[J] := TempLob; //keep mem alive!
                    TmpStream.Free;
                  end;
                end;
              else
                raise Exception.Create('Unsupported AnsiString-Array Variant');
            end;
          end;
        else RaiseUnsupportedParamType(I, WType, SQLType);
        //DBTYPE_UDT: ;
        //DBTYPE_HCHAPTER:;
        //DBTYPE_PROPVARIANT:;
        //DBTYPE_VARNUMERIC:;
      end;//*)
    for J := 0 to fDBParams.cParamSets-1 do begin
      if IsNullFromArray(ZArray, J) {or (wType = DBTYPE_NULL)} then begin
        PDBSTATUS(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_ISNULL;
        Inc(BuffOffSet, fRowSize);
        Continue;
      end else
        PDBSTATUS(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obStatus + BuffOffSet))^ := DBSTATUS_S_OK;
      Data := Pointer(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obValue + BuffOffSet));
      //note PLen is valid only if DBPART_LENGTH was set in Bindings.dwFlags!!!
      PLen := PDBLENGTH(NativeUInt(fDBParams.pData)+(fDBBindingArray[i].obLength + BuffOffSet));
      case wType of
        DBTYPE_I1:    if Native
                      then PShortInt(Data)^   := TShortIntDynArray(ZData)[j]
                      else PShortInt(Data)^   := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI1:   if Native
                      then PByte(Data)^       := TByteDynArray(ZData)[j]
                      else PByte(Data)^       := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I2:    if Native
                      then PSmallInt(Data)^   := TSmallIntDynArray(ZData)[j]
                      else PSmallInt(Data)^   := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI2:   if Native
                      then PWord(Data)^       := TWordDynArray(ZData)[j]
                      else PWord(Data)^       := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I4:    if Native
                      then PInteger(Data)^    := TIntegerDynArray(ZData)[j]
                      else PInteger(Data)^    := ArrayValueToInteger(ZArray, j);
        DBTYPE_UI4:   if Native
                      then PCardinal(Data)^   := TCardinalDynArray(ZData)[j]
                      else PCardinal(Data)^   := ArrayValueToCardinal(ZArray, j);
        DBTYPE_I8:    if Native
                      then PInt64(Data)^      := TInt64DynArray(ZData)[j]
                      else PInt64(Data)^      := ArrayValueToInt64(ZArray, j);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        DBTYPE_UI8:   if Native
                      then PUInt64(Data)^     := TUInt64DynArray(ZData)[j]
                      else PUInt(Data)^       := ArrayValueToUInt64(ZArray, j);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
        DBTYPE_R4:    if Native
                      then PSingle(Data)^     := TSingleDynArray(ZData)[j]
                      else PSingle(Data)^     := ArrayValueToDouble(ZArray, j);
        DBTYPE_R8:    if Native
                      then PDouble(Data)^     := TDoubleDynArray(ZData)[j]
                      else PDouble(Data)^     := ArrayValueToDouble(ZArray, j);
        DBTYPE_CY:    if Native
                      then PCurrency(Data)^   := TCurrencyDynArray(ZData)[j]
                      else PCurrency(Data)^   := ArrayValueToCurrency(ZArray, j);
        DBType_BOOL:  if Native
                      then PWordBool(Data)^   := TBooleanDynArray(ZData)[j]
                      else PWordBool(Data)^   := ArrayValueToBoolean(ZArray, j);
        DBTYPE_DATE, DBTYPE_DBDATE, DBTYPE_DBTIME, DBTYPE_DBTIME2, DBTYPE_DBTIMESTAMP:  begin
            case SQLType of
              stTime:       DateTimeTemp := ArrayValueToTime(ZArray, j, ConSettings.WriteFormatSettings);
              stDate:       DateTimeTemp := ArrayValueToDate(ZArray, j, ConSettings.WriteFormatSettings);
              else          DateTimeTemp := ArrayValueToDateTime(ZArray, j, ConSettings.WriteFormatSettings);
            end;
            case wType of
              DBTYPE_DATE: PDateTime(Data)^ := DateTimeTemp;
              DBTYPE_DBDATE: begin
                  DecodeDate(DateTimeTemp, W1, PDBDate(Data)^.month, PDBDate(Data)^.day);
                  PDBDate(Data)^.year := W1;
                end;
              DBTYPE_DBTIME: DecodeTime(DateTimeTemp, PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second, MS);
              DBTYPE_DBTIME2: begin
                  DecodeTime(DateTimeTemp,
                    PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second, MS);
                    PDBTIME2(Data)^.fraction := MS * 1000000;
                end;
              DBTYPE_DBTIMESTAMP: begin
                  DecodeDate(DateTimeTemp, W1, PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day);
                  PDBTimeStamp(Data)^.year := W1;
                  if SQLType <> stDate then begin
                    DecodeTime(DateTimeTemp, PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute, PDBTimeStamp(Data)^.second, MS);
                    {if fSupportsMilliseconds
                    then} PDBTimeStamp(Data)^.fraction := MS * 1000*1000
                    {else PDBTimeStamp(Data)^.fraction := 0};
                  end else begin
                    PDBTimeStamp(Data)^.hour := 0; PDBTimeStamp(Data)^.minute := 0;
                    PDBTimeStamp(Data)^.second := 0; PDBTimeStamp(Data)^.fraction := 0;
                  end;
                end;
            end;
          end;
        { next types are automatically prepared on binding the arrays }
        DBTYPE_GUID: ArrayValueToGUID(ZArray, j, PGUID(Data));
        DBTYPE_GUID or DBTYPE_BYREF: PPointer(Data)^ := @TGUIDDynArray(ZData)[J].D1;
        DBTYPE_BYTES: Bind_DBTYPE_BYTES(False);
        DBTYPE_BYTES or DBTYPE_BYREF: Bind_DBTYPE_BYTES(True);
        DBTYPE_WSTR, DBTYPE_WSTR or DBTYPE_BYREF: begin
            case SQLType of
             { stBoolean:      FUniTemp := BoolToUnicodeEx(TBooleanDynArray(ZData)[J]);
              stByte:         FUniTemp := IntToUnicode(TByteDynArray(ZData)[J]);
              stShort:        FUniTemp := IntToUnicode(TShortIntDynArray(ZData)[J]);
              stWord:         FUniTemp := IntToUnicode(TWordDynArray(ZData)[J]);
              stSmall:        FUniTemp := IntToUnicode(TSmallIntDynArray(ZData)[J]);
              stLongWord:     FUniTemp := IntToUnicode(TCardinalDynArray(ZData)[J]);
              stInteger:      FUniTemp := IntToUnicode(TIntegerDynArray(ZData)[J]);
              stULong:        FUniTemp := IntToUnicode(TUInt64DynArray(ZData)[J]);
              stLong:         FUniTemp := IntToUnicode(TInt64DynArray(ZData)[J]);
              stFloat:        FUniTemp := FloatToUnicode(TSingleDynArray(ZData)[J]);
              stDouble:       FUniTemp := FloatToUnicode(TDoubleDynArray(ZData)[J]);
              stCurrency:     FUniTemp := FloatToUnicode(TCurrencyDynArray(ZData)[J]);
              stBigDecimal:   FUniTemp := FloatToUnicode(TExtendedDynArray(ZData)[J]);
              stTime:         FUniTemp := DateTimeToUnicodeSQLTime(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);
              stDate:         FUniTemp := DateTimeToUnicodeSQLDate(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);
              stTimeStamp:    FUniTemp := DateTimeToUnicodeSQLTimeStamp(TDateTimeDynArray(ZData)[J], ConSettings.WriteFormatSettings, False);}
              stString, stUnicodeString: begin
                case ZArray.VArrayVariantType of
                  {$IFNDEF UNICODE}vtString, {$ENDIF}
                  vtAnsiString,vtUTF8String,vtRawByteString:
                      if wType = DBTYPE_WSTR then begin
                        P := Pointer(TRawByteStringDynArray(ZData)[J]);
                        PLen^ := PRaw2PUnicode(PAnsiChar(P), PWideChar(Data), W1, LengthInt(Length(TRawByteStringDynArray(ZData)[J])), LengthInt(CPL)) shl 1;
                        goto W_Len;
                      end else begin
                        Dyn_W_Convert(I, Length(TRawByteStringDynArray(ZData)), ZArray);
                        ZData := ZArray.VArray;
                        goto WStr;
                      end;
                  {$IFDEF UNICODE}vtString,{$ENDIF} vtUnicodeString: begin
WStr:                 PLen^ := Length(TUnicodeStringDynArray(ZData)[J]) shl 1;
                      if PLen^ > 0 then
                        if wType = DBTYPE_WSTR then begin
                          Move(Pointer(TUnicodeStringDynArray(ZData)[J])^, PWideChar(Data)^, ({$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(PLen^, MaxL)+2));
                          goto W_Len
                        end else
                          PPointer(Data)^ := Pointer(TUnicodeStringDynArray(ZData)[J])
                      else if wType = DBTYPE_WSTR
                        then PWord(Data)^ := 0
                        else PPointer(Data)^ := PEmptyUnicodeString;
                    end;
                  vtCharRec: begin
                      if TZCharRecDynArray(ZData)[J].CP = zCP_UTF16 then begin
                        PLen^ := TZCharRecDynArray(ZData)[J].Len shl 1;
                        if wType = DBTYPE_WSTR
                        then Move(PWideChar(TZCharRecDynArray(ZData)[J].P)^, PWideChar(Data)^, ({$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(PLen^, MaxL)+2))
                        else PPointer(Data)^ := TZCharRecDynArray(ZData)[J].P;
                      end else begin
                        if wType = DBTYPE_WSTR
                        then PLen^ := PRaw2PUnicode(PAnsiChar(TZCharRecDynArray(ZData)[J].P), PWideChar(Data), TZCharRecDynArray(ZData)[J].CP, LengthInt(TZCharRecDynArray(ZData)[J].Len), LengthInt(MaxL))
                        else begin
                          Dyn_W_Convert(I, Length(TZCharRecDynArray(ZData)), ZArray);
                          ZData := ZArray.VArray;
                          goto WStr;
                        end;
                      end;
W_Len:                if PLen^ > MaxL then
                        RaiseExceeded(I);
                    end;
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
              end;
              stAsciiStream, stUnicodeStream: Bind_Long_DBTYPE_WSTR_BY_REF;
              else
                raise Exception.Create('Unsupported AnsiString-Array Variant');
            end;
          end;
        else RaiseUnsupportedParamType(I, WType, SQLType);
        //DBTYPE_UDT: ;
        //DBTYPE_HCHAPTER:;
        //DBTYPE_PROPVARIANT:;
        //DBTYPE_VARNUMERIC:;
      end;
      Inc(BuffOffSet, fRowSize);
    end;  //*)
  end;
  {$IF defined (RangeCheckEnabled)}{$R+}{$IFEND}
end;

procedure TZOleDBPreparedStatement.BindInParameters;
begin
  if BindList.Count = 0 then
    Exit;
  if not fBindImmediat or (BatchDMLArrayCount > 0) then
    try
      fBindImmediat := True;
      if fBindAgain or (FDBParams.hAccessor = 0) then
        PrepareInParameters;
      if BatchDMLArrayCount = 0
      then BindList.BindValuesToStatement(Self, SupportsBidirectionalParams)
      else BindBatchDMLArrays;
      fBindAgain := False;
    finally
      fBindImmediat := fDEFERPREPARE;
    end;
end;

procedure TZOleDBPreparedStatement.BindRaw(Index: Integer;
  const Value: RawByteString; CP: Word);
var L: Cardinal;
  PLen: PDBLENGTH;
  Bind: PDBBINDING;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat and (FDBBindingArray[Index].wType = DBTYPE_WSTR) then begin
    Bind := @FDBBindingArray[Index];
    PLen := PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength);
    L := Bind.cbMaxLen-2;
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    PLen^ := PRaw2PUnicode(Pointer(Value), Pointer(NativeUInt(fDBParams.pData)+Bind.obValue), CP, LengthInt(Length(Value)), LengthInt(L shr 1)) shl 1;
    if PLen^ > L then
      RaiseExceeded(Index)
  end else begin
    FUniTemp := PRawToUnicode(Pointer(Value), Length(Value), CP);
    BindList.Put(Index, stUnicodeString, FUniTemp);
    L := Length(FUniTemp);
    if fBindImmediat then
      if Value <> ''
      then SetPWideChar(Index, Pointer(FUniTemp), L)
      else SetPWideChar(Index, PEmptyUnicodeString, 0)
    else InitVaryBind(Index, (L+1) shl 1, DBTYPE_WSTR);
  end;
end;

procedure TZOleDBPreparedStatement.CalcParamSetsAndBufferSize;
var
  FAccessorRefCount: DBREFCOUNT;
begin
  FDBParams.cParamSets := Max(1, BatchDMLArrayCount); //indicate rows for single executions
  if (FDBParams.hAccessor <> 0) and fBindAgain then begin
    FParameterAccessor.ReleaseAccessor(FDBParams.hAccessor, @FAccessorRefCount);
    FDBParams.hAccessor := 0;
  end;
  SetLength(FParamsBuffer, FDBParams.cParamSets * FRowSize);
  FDBParams.pData := Pointer(FParamsBuffer); //set entry pointer
  if (FDBParams.hAccessor = 0) then
    CheckError(FParameterAccessor.CreateAccessor(DBACCESSOR_PARAMETERDATA,
      FDBUPARAMS, Pointer(FDBBindingArray), FRowSize, @FDBParams.hAccessor,
      Pointer(FDBBINDSTATUSArray)), lcOther, FDBBINDSTATUSArray);
end;

procedure TZOleDBPreparedStatement.CheckParameterIndex(Value: Integer);
begin
  if not Prepared then
    Prepare;
  if (BindList.Capacity < Value+1) then
    if fBindImmediat
    then raise EZSQLException.Create(SInvalidInputParameterCount)
    else inherited CheckParameterIndex(Value);
end;

constructor TZOleDBPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FClientCP := ConSettings^.ClientCodePage.CP;
end;

function TZOleDBPreparedStatement.CreateResultSet(
  const RowSet: IRowSet): IZResultSet;
var
  NativeResultSet: TZOleDBParamResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  Result := nil;
  if (RowSet = nil) and FHasOutParams then begin
    NativeResultSet := TZOleDBParamResultSet.Create(Self, FParamsBuffer,
      FDBBindingArray, FParamNamesArray);
    if (ResultSetConcurrency = rcUpdatable) or (ResultSetType <> rtForwardOnly) then begin
      CachedResultSet := TZOleDBCachedResultSet.Create(NativeResultSet, SQL,
        TZGenericCachedResolver.Create(Self, NativeResultSet.GetMetaData), ConSettings);
      CachedResultSet.SetConcurrency(ResultSetConcurrency);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
  end else
    Result := inherited CreateResultSet(RowSet);
  FOpenResultSet := Pointer(Result);
end;

procedure TZOleDBPreparedStatement.Dyn_W_Convert(Index, Len: Integer; var Arr: PZArray);
var
  W_Dyn: TUnicodeStringDynArray;
  CP: Word;
  I: Integer;
  NewArr: TZArray;
label SetUniArray;
begin
  SetLength(W_Dyn, Len);
  CP := zCP_NONE;
  case TZSQLType(Arr.VArrayType) of
    stString, stUnicodeString:
      case Arr.VArrayVariantType of
        {$IFNDEF UNICODE}
        vtString:   if not ConSettings^.AutoEncode then
                      CP := ConSettings^.CTRL_CP;
        {$ENDIF}
        vtUTF8String: CP := zCP_UTF8;
        vtAnsiString: CP := ZOSCodePage;
        vtRawByteString: CP := FClientCP;
        vtCharRec: begin
                    W_Dyn := CharRecArray2UnicodeStrArray(TZCharRecDynArray(Arr.VArray));
                    goto SetUniArray;
                   end;
      end;
  end;
  for I := 0 to High(W_Dyn) do
    W_Dyn[i] := ZRawToUnicode(TRawByteStringDynArray(Arr.VArray)[i], CP);
SetUniArray:
  NewArr := Arr^; //localize
  NewArr.VArrayType := Ord(stUnicodeString);
  NewArr.VArrayVariantType := vtUnicodeString;
  NewArr.VArray := Pointer(W_Dyn);
  BindList.Put(Index, NewArr, True);
  Arr := BindList[Index].Value;
end;

procedure TZOleDBPreparedStatement.InitDateBind(Index: Integer;
  SQLType: TZSQLType);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and (Bind.wType <> SQLType2OleDBTypeEnum[SQLType]));
  if fBindagain then begin
    Bind.wType := SQLType2OleDBTypeEnum[SQLType];
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and not DBPARAMFLAGS_ISLONG;
    case SQLType of
      stDate: Bind.cbMaxLen := SizeOf(TDBDate);
      stTime: Bind.cbMaxLen := SizeOf(TDBTime2);
      else    Bind.cbMaxLen := SizeOf(Double);
    end;
    Bind.dwPart := DBPART_VALUE or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.InitFixedBind(Index: Integer; Size: Cardinal;
  _Type: DBTYPE);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and ((Bind.wType <> _Type) or (Bind.cbMaxLen <> Size)));
  if fBindagain then begin
    Bind.wType := _Type;
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and not DBPARAMFLAGS_ISLONG;
    Bind.cbMaxLen := Size;
    Bind.dwPart := DBPART_VALUE or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.InitLongBind(Index: Integer; _Type: DBTYPE);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and (Bind.wType <> _Type or DBTYPE_BYREF));
  if fBindagain then begin
    Bind.wType := _Type or DBTYPE_BYREF;
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and DBPARAMFLAGS_ISLONG;
    Bind.cbMaxLen := SizeOf(Pointer);
    Bind.dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.InitVaryBind(Index: Integer; Len: Cardinal;
  _Type: DBTYPE);
var Bind: PDBBINDING;
begin
  Bind := @FDBBindingArray[Index];
  fBindAgain := fBindAgain or ((BindList.ParamTypes[Index] = pctUnknown) and ((Bind.wType <> _Type) or (Bind.cbMaxLen < Len)));
  if fBindagain then begin
    Bind.wType := _Type;
    Bind.dwFlags := FDBBindingArray[Index].dwFlags and not DBPARAMFLAGS_ISLONG;
    Bind.cbMaxLen := {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Max(512,
      {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Max(Bind.cbMaxLen, Len));
    Bind.dwPart := DBPART_VALUE or DBPART_LENGTH or DBPART_STATUS;
  end;
end;

procedure TZOleDBPreparedStatement.Prepare;
var
  FOlePrepareCommand: ICommandPrepare;
  DBInfo: IZDataBaseInfo;
begin
  if Not Prepared then begin//prevent PrepareInParameters
    fDEFERPREPARE := {False;//}StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_PreferPrepared, 'True'));
    fBindImmediat := {False;//}fDEFERPREPARE;
    FCommand := (Connection as IZOleDBConnection).CreateCommand;
    try
      SetOleCommandProperties;
      CheckError(fCommand.SetCommandText(DBGUID_DEFAULT, Pointer(WSQL)), lcOther);
      OleCheck(fCommand.QueryInterface(IID_ICommandPrepare, FOlePrepareCommand));
      if fDEFERPREPARE then begin
        CheckError(FOlePrepareCommand.Prepare(0), lcOther);
        fBindImmediat := True;
      end else
        fBindImmediat := False;
    finally
      FOlePrepareCommand := nil;
    end;
    DBInfo := Connection.GetMetadata.GetDatabaseInfo;
    if FSupportsMultipleResultSets
    then fMoreResultsIndicator := mriUnknown
    else fMoreResultsIndicator := mriHasNoMoreResults;
    fSupportsByRef := (DBInfo as IZOleDBDatabaseInfo).SupportsByRefAccessors;
    DBInfo := nil;
    inherited Prepare;
  end else begin
    FMultipleResults := nil; //release this interface! else we can't free the command in some tests
    if Assigned(FParameterAccessor) and ((BatchDMLArrayCount > 0) and
       (FDBParams.cParamSets = 0)) or //new arrays have been set
       ((BatchDMLArrayCount = 0) and (FDBParams.cParamSets > 1)) then //or single exec follows
      CalcParamSetsAndBufferSize;
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZOleDBPreparedStatement.PrepareInParameters;
var
  FNamesBuffer: PPOleStr; //we don't need this here except as param!
  FParamInfoArray: PDBParamInfoArray;
  FCommandWithParameters: ICommandWithParameters;
  DescripedDBPARAMINFO: TDBParamInfoDynArray;
  Status: HResult;
begin
  if not fBindImmediat then
    Exit;
  if not Prepared then begin
    {check out the parameter informations }
    FParamInfoArray := nil; FNamesBuffer := nil; DescripedDBPARAMINFO := nil;
    OleCheck(fcommand.QueryInterface(IID_ICommandWithParameters, FCommandWithParameters));
    Status := FCommandWithParameters.GetParameterInfo(FDBUPARAMS,PDBPARAMINFO(FParamInfoArray), FNamesBuffer);
    if Status = DB_E_PARAMUNAVAILABLE then begin
      fDEFERPREPARE := false;
      Exit;
    end else if Failed(Status) then
      CheckError(Status, lcOther, FDBBINDSTATUSArray);
    try
      SetParamCount(FDBUPARAMS);
      if FDBUPARAMS > 0 then begin
        OleCheck(FCommand.QueryInterface(IID_IAccessor, FParameterAccessor));
        FRowSize := PrepareOleParamDBBindings(FDBUPARAMS, FDBBindingArray,
          FParamInfoArray, fSupportsByRef);
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
  end else begin
    FDBUPARAMS := BindList.Count;
    SetBindOffsets;
    if FParameterAccessor = nil then
      OleCheck(FCommand.QueryInterface(IID_IAccessor, FParameterAccessor));
    CalcParamSetsAndBufferSize;
  end;
end;

procedure TZOleDBPreparedStatement.RaiseExceeded(Index: Integer);
begin
  raise EZSQLException.Create(Format(cSParamValueExceeded, [Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}])+LineEnding+
    'Stmt: '+GetSQL);
end;

procedure TZOleDBPreparedStatement.RaiseUnsupportedParamType(Index: Integer;
  WType: Word; SQLType: TZSQLType);
begin
  raise EZSQLException.Create('Index: '+ZFastCode.IntToStr(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF})+
    ', OleType: '+ZFastCode.IntToStr(wType)+', SQLType: '+GetEnumName(TypeInfo(TZSQLType), Ord(SQLType))+
    LineEnding+SUnsupportedParameterType+LineEnding+ 'Stmt: '+GetSQL);
end;

procedure TZOleDBPreparedStatement.RegisterParameter(Index: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
var Bind: PDBBINDING;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if (Name <> '') then begin
    if (High(FParamNamesArray) < Index) then
      SetLength(FParamNamesArray, Index+1);
    FParamNamesArray[Index] := Name;
  end;
  FHasOutParams := FHasOutParams or (Ord(ParamType) >= Ord(pctInOut));
  Bind := @FDBBindingArray[Index];
  if fDEFERPREPARE then begin
    case ParamType of
      pctReturn, pctOut: if Bind.dwFlags and DBPARAMFLAGS_ISINPUT <> 0 then
                           Bind.dwFlags := (Bind.dwFlags and not DBPARAMFLAGS_ISINPUT) or DBPARAMFLAGS_ISOUTPUT;
      pctIn, pctInOut: if Bind.dwFlags and DBPARAMFLAGS_ISINPUT = 0 then
                           Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISINPUT;
    end;
  end else begin
    Bind.wType := SQLType2OleDBTypeEnum[SQLType];
    if (Ord(SQLType) < Ord(stBigDecimal)) or (SQLtype = stGUID) then
      InitFixedBind(Index, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType])
    else if Ord(SQLType) <= Ord(stTimestamp) then
       InitDateBind(Index, SQLType)
    else if Ord(SQLType) < Ord(stAsciiStream) then
      InitVaryBind(Index, Max(512, PrecisionOrSize), SQLType2OleDBTypeEnum[SQLType])
    else InitLongBind(Index, SQLType2OleDBTypeEnum[SQLType]);
  end;
  Bind.eParamIO :=  ParamType2OleIO[ParamType];
  inherited RegisterParameter(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType, ParamType, Name, PrecisionOrSize, Scale);
end;

procedure TZOleDBPreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
begin
  FParameterAccessor := nil;
  SetLength(FDBBindingArray, 0);
  SetLength(FParamsBuffer, 0);
  inherited ReleaseImmediat(Sender);
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZOleDBPreparedStatement.SetAnsiString(Index: Integer;
  const Value: AnsiString);
begin
  BindRaw(Index, Value, zOSCodePage);
end;
{$ENDIF}

procedure TZOleDBPreparedStatement.SetBigDecimal(Index: Integer;
  const Value: {$IFDEF BCD_TEST}TBCD{$ELSE}Extended{$ENDIF});
begin
  SetDouble(Index, Value);
end;

procedure TZOleDBPreparedStatement.SetBindOffsets;
var I: Integer;
  Bind: PDBBINDING;
begin
  FRowSize := 0;
  FHasOutParams := False;
  for I := 0 to BindList.Count -1 do begin
    Bind := @FDBBindingArray[I];
    Bind.iOrdinal := I +1;
    Bind.obStatus := FRowSize;
    Inc(FRowSize, SizeOf(DBSTATUS));
    Bind.obLength := FRowSize;
    if Bind.dwPart and DBPART_LENGTH <> 0 then
      Inc(FRowSize, SizeOf(DBLENGTH));
    Bind.obValue := FRowSize;
    Inc(FRowSize, Bind.cbMaxLen);
    Bind.eParamIO := ParamType2OleIO[BindList.ParamTypes[I]];
    if Ord(BindList.ParamTypes[I]) >= Ord(pctInOut) then begin
      FHasOutParams := True;
      if BindList.ParamTypes[I] = pctInOut then
        Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISINPUT or DBPARAMFLAGS_ISOUTPUT
      else
        Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISOUTPUT;
    end else
      Bind.dwFlags := Bind.dwFlags or DBPARAMFLAGS_ISINPUT;
  end;
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large binary value is input to a <code>LONGVARBINARY</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the binary parameter value
}
procedure TZOleDBPreparedStatement.SetBlob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
var Bind: PDBBINDING;
  Data: PAnsichar;
  DBStatus: PDBSTATUS;
  DBLENGTH: PDBLENGTH;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  BindList.Put(Index, SQLType, Value);//keep alive
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    DBStatus := PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus);
    if (Value = nil) or Value.IsEmpty then begin
      DBSTATUS^ := DBSTATUS_S_ISNULL;
      Exit;
    end;
    DBSTATUS^ := DBSTATUS_S_OK;
    DBLENGTH := PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength);
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      (DBTYPE_STR or DBTYPE_BYREF):
        if Value.IsClob then begin
          PPointer(Data)^ := Value.GetPAnsiChar(FClientCP);
          DBLENGTH^ := Value.Length;
        end else begin
          FRawTemp := GetValidatedAnsiStringFromBuffer(Value.GetBuffer, Value.Length, ConSettings);
          SetBLob(Index, stAsciiStream, TZAbstractCLob.CreateWithData(Pointer(FRawTemp),
            Length(FRawTemp), FClientCP, ConSettings));
        end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
              PPointer(Data)^ := Value.GetPWideChar;
              DBLENGTH^ := Value.Length;
            end;
      (DBTYPE_GUID or DBTYPE_BYREF):;
      (DBTYPE_BYTES or DBTYPE_BYREF): begin
          PPointer(Data)^ := Value.GetBuffer;
          DBLENGTH^ := Value.Length;
        end;
      else RaiseUnsupportedParamType(Index, Bind.wType, SQLType);
    end;
  end else
    InitLongBind(Index, SQLType2OleDBTypeEnum[SQLType]);
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetBoolean(Index: Integer; Value: Boolean);
var Bind: PDBBINDING;
  Data: PAnsichar;
label {set_a_len, }set_w_len;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Ord(Value);
      DBTYPE_I4:        PInteger(Data)^ := Ord(Value);
      DBTYPE_R4:        PSingle(Data)^ := Ord(Value);
      DBTYPE_R8:        PDouble(Data)^ := Ord(Value);
      DBTYPE_CY:        PCurrency(Data)^ := Ord(Value);
      DBTYPE_BOOL:      PWordBool(Data)^ := Value;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Ord(Value);
      DBTYPE_I1:        PShortInt(Data)^ := Ord(Value);
      DBTYPE_UI2:       PWord(Data)^ := Ord(Value);
      DBTYPE_UI4:       PCardinal(Data)^ := Ord(Value);
      DBTYPE_I8:        PInt64(Data)^ := Ord(Value);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := Ord(Value);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
(*      DBTYPE_STR:       begin
                          if Bind.cbMaxLen >= 6
                          then Move(Pointer(BoolStrsUpRaw[Value])^, Data, Length(BoolStrsUpRaw[Value])+1)
                          else RaiseExceeded(Index);
                          goto set_a_len;
                        end;
      (DBTYPE_STR or DBTYPE_BYREF): begin
                                      PPointer(Data)^ := Pointer(BoolStrsUpRaw[Value]);
set_a_len:                            if Value
                                      then PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 4
                                      else PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 5;
                                    end;*)
      DBTYPE_WSTR:      begin
                          if Bind.cbMaxLen >= 12
                          then Move(Pointer(BoolStrsUpW[Value])^, Data, (5+Ord(not Value)) shl 1)
                          else RaiseExceeded(Index);
                          goto set_w_len;
                        end;
      DBTYPE_WSTR or DBTYPE_BYREF: begin
                                      PPointer(Data)^ := Pointer(BoolStrsUpW[Value]);
set_w_len:                            if Value
                                      then PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 8
                                      else PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 10;
                                    end;
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
     else RaiseUnsupportedParamType(Index, Bind.wType, stBoolean);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(WordBool), DBTYPE_BOOL);
    BindList.Put(Index, Value);
  end;
end;

{**
  Sets the designated parameter to a Java <code>unsigned 8Bit int</code> value.
  The driver converts this
  to an SQL <code>BYTE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetByte(Index: Integer; Value: Byte);
var C: Cardinal;
begin
  if fBindImmediat then
    SetUInt(Index, Value)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
    InitFixedBind(Index, SizeOf(Byte), DBTYPE_UI1);
    C := Value;
    BindList.Put(Index, stByte, P4Bytes(@C));
  end;
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetBytes(Index: Integer;
  const Value: TBytes);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  BindList.Put(Index, stBytes, Value); //localize
  if fBindImmediat
  then SetPAnsiChar(Index, Pointer(Value), Length(Value))
  else InitVaryBind(Index, Length(Value), DBTYPE_BYTES);
end;

procedure TZOleDBPreparedStatement.SetCharRec(Index: Integer;
  const Value: TZCharRec);
label set_from_tmp;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then
    if Value.CP = zCP_UTF16 then
      case FDBBindingArray[Index].wType of
        DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF):
          SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUnicodeToRaw(Value.P, Value.Len, FClientCP));
        else SetPWideChar(Index, Value.P, Value.Len)
      end
    else case FDBBindingArray[Index].wType of
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF):
        goto set_from_tmp;
      DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF):
        if FClientCP = Value.CP then
          SetPAnsiChar(Index, Value.P, Value.Len);
        else begin
set_from_tmp:
          FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
          SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp));
        end;
    end
  else begin
    InitVaryBind(Index, (Value.Len+1) shl 1, DBTYPE_WSTR);
    BindList.Put(Index, stString, Value.P, Value.Len, Value.CP);
  end;
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var Bind: PDBBINDING;
  Data, PEnd: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_I4:        PInteger(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_I1:        PShortInt(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_UI2:       PWord(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_UI4:       PCardinal(Data)^ := PInt64(@Value)^ div 10000;
      DBTYPE_I8:        PInt64(Data)^ := PInt64(@Value)^ div 10000;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := PInt64(@Value)^ div 10000;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      (*DBTYPE_STR: if Bind.cbMaxLen < 22 then begin //(19digits+dot+neg sign) -> test final length
                    CurrToRaw(Value, @fABuffer[0], @PEnd);
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-@fABuffer[0];
                    if PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ < Bind.cbMaxLen
                    then Move(fABuffer[0], Data^, PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^)
                    else RaiseExceeded(Index);
                  end else begin
                    CurrToRaw(Value, Data, @PEnd);
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-Data;
                  end;
      (DBTYPE_STR or DBTYPE_BYREF): begin
                   PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 24); //8Byte align
                   CurrToRaw(Value, PPointer(Data)^, @PEnd);
                   PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-PPAnsiChar(Data)^;
                 end; *)
      DBTYPE_WSTR: if Bind.cbMaxLen < 44 then begin //(19digits+dot+neg sign) -> test final length
                    CurrToUnicode(Value, @fWBuffer[0], @PEnd);
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-@fWBuffer[0];
                    if PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ < Bind.cbMaxLen
                    then Move(fWBuffer[0], Data^, PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^)
                    else RaiseExceeded(Index);
                  end else begin
                    CurrToUnicode(Value, PWideChar(Data), @PEnd);
                    PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-Data;
                  end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                   PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 48); //8Byte align
                   CurrToUnicode(Value, ZPPWideChar(Data)^, @PEnd);
                   PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := PEnd-PPAnsiChar(Data)^;
                 end;
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
     else RaiseUnsupportedParamType(Index, Bind.wType, stCurrency);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(Currency), DBTYPE_CY);
    BindList.Put(Index, stCurrency, P8Bytes(@Value));
  end;
end;

procedure TZOleDBPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType);
var arr: TZArray;
  GUID_Dyn: TGUIDDynArray;
  i,L: LengthInt;
  P: Pointer;
begin
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
  if (ParameterIndex = FirstDbcIndex) and (BindList.ParamTypes[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] <> pctResultSet) then
    FDBParams.cParamSets := 0;
  if (SQLType = stGUID) and not (VariantType in [vtNull, vtBytes]) then begin
    SetLength(GUID_Dyn, Length(TRawByteStringDynArray(Value)));
    Arr := PZArray(BindList[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}].Value)^;
    for I := 0 to High(GUID_Dyn) do
      ArrayValueToGUID(@Arr, i, @GUID_Dyn[i]);
    Arr.VArrayType := Ord(stGUID);
    Arr.VArrayVariantType := vtNull;
    BindList.Put(ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Arr, True);
  end;
  if not fDEFERPREPARE then begin
    {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
    case SQLtype of
      stBigDecimal: InitFixedBind(ParameterIndex, SizeOf(Double), DBTYPE_R8);
      stDate: InitFixedBind(ParameterIndex, SizeOf(TDBDate), DBTYPE_DBDATE);
      stTime: InitFixedBind(ParameterIndex, SizeOf(TDBTIME2), DBTYPE_DBTIME2);
      stTimestamp: InitFixedBind(ParameterIndex, SizeOf(Double), DBTYPE_DATE);
      stString,
      stUnicodeString: begin
         P := PZArray(BindList[ParameterIndex].Value).VArray;
         L := 0;
         for I := 0 to {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFNDEF FPC}-1{$ENDIF} do
           case PZArray(BindList[ParameterIndex].Value).VArrayVariantType of
              {$IFNDEF UNICODE}vtString,{$ENDIF}
              vtAnsiString, vtUTF8String, VtRawByteString:  L := Max(L, Length(TRawByteStringDynArray(P)[I]));
              vtCharRec:                                    L := Max(L, TZCharRecDynArray(P)[I].Len);
              {$IFDEF UNICODE}vtString,{$ENDIF}
              vtUnicodeString:                              L := Max(L, Length(TUnicodeStringDynArray(P)[I]));
            end;
          InitVaryBind(ParameterIndex, (L+1) shl 1, DBTYPE_WSTR);
        end;
      stBytes: begin
          L := 0;
          for I := 0 to High(TBytesDynArray(Value)) do
            L := Max(L, Length(TBytesDynArray(Value)[I]));
          InitVaryBind(ParameterIndex, L, DBTYPE_BYTES);
        end;
      stAsciiStream,
      stUnicodeStream,
      stBinaryStream: InitLongBind(ParameterIndex, SQLType2OleDBTypeEnum[SQLType]);
      stUnknown, stArray, stDataSet: Self.RaiseUnsupportedParamType(ParameterIndex, DBTYPE_WSTR, SQLType);
      else InitFixedBind(ParameterIndex, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType]);
    end;
  end;
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetDate(Index: Integer;
  const Value: TDateTime);
var Bind: PDBBINDING;
  Data: PAnsichar;
  W: word;
label RConv, WConv;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4,
      DBTYPE_I8, DBTYPE_UI8: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Trunc(Value));
      DBTYPE_DATE:      PDateTime(Data)^ := Value;
      DBTYPE_DBDATE: begin
          DecodeDate(Value, W, PDBDate(Data)^.month, PDBDate(Data)^.day);
          PDBDate(Data)^.year := W;
        end;
      DBTYPE_DBTIME:  FillChar(Data^, SizeOf(TDBTime), #0);
      DBTYPE_DBTIME2: FillChar(Data^, SizeOf(TDBTIME2), #0);
      DBTYPE_DBTIMESTAMP: begin
          FillChar(Data^, SizeOF(TDBTimeStamp), #0);
          DecodeDate(Value, W, PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day);
          PDBTimeStamp(Data)^.year := W;
        end;
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      (DBTYPE_STR or DBTYPE_BYREF): begin
          PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 12);
          Data := PPointer(Data)^;
          goto RConv;
        end;
      DBTYPE_STR: if Bind.cbMaxLen >= 11 then
RConv:              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                      DateTimeToRawSQLDate(Value, PAnsiChar(Data), ConSettings.WriteFormatSettings, False)
                  else RaiseExceeded(Index);
      DBTYPE_WSTR or DBTYPE_BYREF: begin
          PPointer(Data)^ := BindList.AquireCustomValue(Index, stUnicodeString, 24);
          Data := PPointer(Data)^;
          goto WConv;
        end;
      DBTYPE_WSTR: if Bind.cbMaxLen >= 22 then
WConv:          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                  DateTimeToUnicodeSQLDate(Value, PWideChar(Data), ConSettings.WriteFormatSettings, False)
              else RaiseExceeded(Index);
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
      else RaiseUnsupportedParamType(Index, Bind.wType, stDate);
    end;
  end else begin//Late binding
    InitDateBind(Index, stDate);
    BindList.Put(Index, stDate, P8Bytes(@Value));
  end;
end;

procedure TZOleDBPreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
var Bind: PDBBINDING;
  Data: PAnsichar;
  L: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_I8,
      DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4, DBTYPE_UI8:
                        SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Trunc(Value));
      DBTYPE_DATE:      PDateTime(Data)^ := Value;
      DBTYPE_DBDATE,
      DBTYPE_DBTIME,
      DBTYPE_DBTIME2,
      DBTYPE_DBTIMESTAMP: SetTimeStamp(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      (*DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF): begin
          if Bind.wType = (DBTYPE_STR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 64);
            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := FloatToRaw(Value, PPansiChar(Data)^);
          end else if Bind.cbMaxLen < 64 then begin
            L := FloatToRaw(Value, @fABuffer[0]);
            if L < Bind.cbMaxLen
            then Move(fABuffer[0], Data^, L)
            else RaiseExceeded(Index);
            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L;
          end else
            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := FloatToRaw(Value, Data);
        end;*)
      DBTYPE_WSTR: if Bind.cbMaxLen < 128 then begin
            L := FloatToUnicode(Value, @fWBuffer[0]) shl 1;
            if L < Bind.cbMaxLen
            then Move(fWBuffer[0], Data^, L)
            else RaiseExceeded(Index);
            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L;
          end else
            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := FloatToUnicode(Value, PWideChar(Data)) shl 1;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 128);
            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := FloatToUnicode(Value, ZPPWideChar(Data)^) shl 1;
          end;
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
      else RaiseUnsupportedParamType(Index, Bind.wType, stDouble);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(Double), DBTYPE_R8);
    BindList.Put(Index, stDouble, P8Bytes(@Value));
  end;
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetFloat(Index: Integer; Value: Single);
begin
  if fBindImmediat
  then SetDouble(Index, Value)
  else begin
    {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
    InitFixedBind(Index, SizeOf(Single), DBTYPE_R4);
    BindList.Put(Index, stFloat, P4Bytes(@Value));
  end;
end;

procedure TZOleDBPreparedStatement.SetGUID(Index: Integer; const Value: TGUID);
var Bind: PDBBINDING;
  Data: Pointer;
label set_uni_len, set_uid_len;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_GUID:  PGUID(Data)^ := Value;
      (DBTYPE_GUID or DBTYPE_BYREF): begin
                        BindList.Put(Index, Value); //localize
                        PPointer(Data)^ := BindList[Index].Value;
                      end;
      DBTYPE_BYTES: if Bind.cbMaxLen < SizeOf(TGUID) then
                      RaiseExceeded(Index)
                    else begin
                      PGUID(Data)^ := Value;
                      goto set_uid_len;
                    end;
      DBTYPE_BYTES or DBTYPE_BYREF: begin
                        BindList.Put(Index, Value); //localize
                        PPointer(Data)^ := BindList[Index].Value;
set_uid_len:            PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=  SizeOf(TGUID);
                      end;
(*      DBTYPE_STR: if Bind.cbMaxLen < 37 then
                    RaiseExceeded(Index)
                  else begin
                    GUIDToBuffer(@Value.D1, PAnsiChar(Data), [guidSet0Term]);
                    goto set_raw_len;
                  end;
      (DBTYPE_STR or DBTYPE_BYREF): begin
                    PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 37);
                    GUIDToBuffer(@Value.D1, PPAnsiChar(Data)^, [guidSet0Term]);
set_raw_len:        PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 36;
                  end; *)
      DBTYPE_WSTR:if Bind.cbMaxLen < 74 then
                    RaiseExceeded(Index)
                  else begin
                    GUIDToBuffer(@Value.D1, PWideChar(Data), [guidSet0Term]);
                    goto set_uni_len;
                  end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                    PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 74);
                    GUIDToBuffer(@Value.D1, ZPPWideChar(Data)^, [guidSet0Term]);
set_uni_len:        PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := 72;
                  end;
      else RaiseUnsupportedParamType(Index, Bind.wType, stGUID);
    end;
  end else begin
    InitFixedBind(Index, SizeOf(TGUID), DBTYPE_GUID);
    BindList.Put(Index, Value);
  end;
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetInt(Index, Value: Integer);
var Bind: PDBBINDING;
  Data: Pointer;
  C, L: Cardinal;
  Negative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Value;
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Value;
      DBTYPE_I1:        PShortInt(Data)^ := Value;
      DBTYPE_UI2:       PWord(Data)^ := Value;
      DBTYPE_UI4:       PCardinal(Data)^ := Value;
      DBTYPE_I8:        PInt64(Data)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      (*DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value, C, Negative);
          if Bind.wType = (DBTYPE_STR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 12);
            Data := PPointer(Data)^;
          end else if (Bind.cbMaxLen <= L +Byte(Ord(Negative))) then
            RaiseExceeded(Index);
          if Negative then
            PByte(Data)^ := Ord('-');
          IntToRaw(C, PAnsiChar(Data)+Ord(Negative), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L+Byte(Ord(Negative));
        end;*)
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value, C, Negative);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 24);
            Data := PPointer(Data)^;
          end else if (Bind.cbMaxLen <= (L +Byte(Ord(Negative))) shl 1) then
            RaiseExceeded(Index);
          if Negative then
            PWord(Data)^ := Ord('-');
          IntToUnicode(C, PWideChar(Data)+Ord(Negative), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1 + Byte(Ord(Negative));
        end;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
     else RaiseUnsupportedParamType(Index, Bind.wType, stInteger);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(Integer), DBTYPE_I4);
    BindList.Put(Index, stInteger, P4Bytes(@Value));
  end;
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetLong(Index: Integer; const Value: Int64);
var Bind: PDBBINDING;
  Data: PAnsichar;
  u64: UInt64;
  L: Cardinal;
  Negative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Value;
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Value;
      DBTYPE_I1:        PShortInt(Data)^ := Value;
      DBTYPE_UI2:       PWord(Data)^ := Value;
      DBTYPE_UI4:       PCardinal(Data)^ := Value;
      DBTYPE_I8:        PInt64(Data)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      (*DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value, u64, Negative);
          if Bind.wType = (DBTYPE_STR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 24); //8Byte align
            Data := PPointer(Data)^; //-9.223.372.036.854.775.808
          end else if (Bind.cbMaxLen <= L +Byte(Ord(Negative))) then
            RaiseExceeded(Index);
          if Negative then
            PByte(Data)^ := Ord('-');
          IntToRaw(u64, PAnsiChar(Data)+Ord(Negative), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L+Byte(Ord(Negative));
        end;*)
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value, u64, Negative);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 24); //8Byte align
            Data := PPointer(Data)^; //-9.223.372.036.854.775.808
          end else if (Bind.cbMaxLen <= (L +Byte(Ord(Negative))) shl 1) then
            RaiseExceeded(Index);
          Negative := Value < 0;
          if Negative then
            PWord(PPointer(Data)^)^ := Ord('-');
          IntToUnicode(u64, PWideChar(Data)+Ord(Negative), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1 + Byte(Ord(Negative));
        end;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
      else RaiseUnsupportedParamType(Index, Bind.wType, stLong);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(Int64), DBTYPE_I8);
    BindList.Put(Index, stLong, P8Bytes(@Value));
  end;
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZOleDBPreparedStatement.SetNull(Index: Integer; SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then
    PDBSTATUS(NativeUInt(FDBParams.pData)+FDBBindingArray[Index].obStatus)^ := DBSTATUS_S_ISNULL
  else begin
    if SQLType = stUnknown then
      SQLtype := BindList.SQLTypes[Index];
    BindList.SetNull(Index, SQLType);
    if Ord(SQLType) < Ord(stString) then
      InitFixedBind(Index, ZSQLTypeToBuffSize[SQLType], SQLType2OleDBTypeEnum[SQLType])
    else if Ord(SQLType) < Ord(stAsciiStream) then
      InitFixedBind(Index, 512, SQLType2OleDBTypeEnum[SQLType])
    else InitLongBind(Index, SQLType2OleDBTypeEnum[SQLType])
  end;
end;

procedure TZOleDBPreparedStatement.SetOleCommandProperties;
var
  FCmdProps: ICommandProperties;
  rgCommonProperties: array[0..20] of TDBProp;
  rgProviderProperties: TDBProp;
  rgPropertySets: array[0..1] of TDBPROPSET;
  Provider: TZServerProvider;
  Status: HResult;

  procedure SetProp(var PropSet: TDBPROPSET; PropertyID: DBPROPID; Value: SmallInt);
  begin
    //initialize common property options
    //VariantInit(PropSet.rgProperties^[PropSet.cProperties].vValue);
    PropSet.rgProperties^[PropSet.cProperties].dwPropertyID := PropertyID;
    PropSet.rgProperties^[PropSet.cProperties].dwOptions    := DBPROPOPTIONS_REQUIRED;
    PropSet.rgProperties^[PropSet.cProperties].dwStatus     := 0;
    PropSet.rgProperties^[PropSet.cProperties].colid        := DB_NULLID;
    PropSet.rgProperties^[PropSet.cProperties].vValue       := Value;
    Inc(PropSet.cProperties);
  end;
begin
  FCmdProps := nil; //init
  if Succeeded(fCommand.QueryInterface(IID_ICommandProperties, FCmdProps)) then begin
    Provider := Connection.GetServerProvider;
    //http://msdn.microsoft.com/en-us/library/windows/desktop/ms723066%28v=vs.85%29.aspx
    rgPropertySets[0].cProperties     := 0; //init
    rgPropertySets[0].guidPropertySet := DBPROPSET_ROWSET;
    rgPropertySets[0].rgProperties    := @rgCommonProperties[0];
    rgPropertySets[1].cProperties     := 0;
    case Provider of
      spMSSQL: rgPropertySets[1].guidPropertySet := DBPROPSET_SQLSERVERROWSET
      else rgPropertySets[1].guidPropertySet := DBPROPSET_ROWSET;
    end;
    rgPropertySets[1].rgProperties    := @rgProviderProperties;

    SetProp(rgPropertySets[0], DBPROP_COMMANDTIMEOUT,    Max(0, fStmtTimeOut)); //Set command time_out static!
    SetProp(rgPropertySets[0], DBPROP_SERVERCURSOR,      VARIANT_TRUE); //force a server side cursor
    if (Provider = spMSSQL) then begin
      //turn off deferred prepare -> raise exception on Prepare if command can't be executed!
      //http://msdn.microsoft.com/de-de/library/ms130779.aspx
      if fDEFERPREPARE
      then SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, VARIANT_FALSE)
      else SetProp(rgPropertySets[1], SSPROP_DEFERPREPARE, VARIANT_TRUE);
    end else begin
      //to avoid http://support.microsoft.com/kb/272358/de we need a
      //FAST_FORWARD(RO) server cursor
      {common sets which are NOT default: according the cursor models of
      http://msdn.microsoft.com/de-de/library/ms130840.aspx }
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,        VARIANT_FALSE);
      if (Connection as IZOleDBConnection).SupportsMARSConnection then begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         VARIANT_FALSE);
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   VARIANT_FALSE);
      end else begin
        SetProp(rgPropertySets[0], DBPROP_OWNINSERT,         VARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
        SetProp(rgPropertySets[0], DBPROP_OWNUPDATEDELETE,   VARIANT_TRUE);  //slow down by 20% but if isn't set it breaks multiple connection ):
      end;
      SetProp(rgPropertySets[0], DBPROP_OTHERINSERT,       VARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_OTHERUPDATEDELETE, VARIANT_TRUE);
      SetProp(rgPropertySets[0], DBPROP_UNIQUEROWS,         VARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANFETCHBACKWARDS,  VARIANT_FALSE);
      SetProp(rgPropertySets[0], DBPROP_CANSCROLLBACKWARDS, VARIANT_FALSE);
    end;
    try
      Status := FCmdProps.SetProperties(2,@rgPropertySets[0]);
      if Failed(Status) then
        OleDBCheck(Status, SQL, Self, nil);
    finally
      FCmdProps := nil;
    end;
  end;
end;

procedure TZOleDBPreparedStatement.SetPAnsiChar(Index: Word; Value: PAnsiChar;
  Len: Cardinal);
var Bind: PDBBINDING;
  Data: PAnsichar;
  W1, W2: Word;
  Failed: Boolean;
begin
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := RawToIntDef(Value, Value+Len, 0);
      DBTYPE_I4:        PInteger(Data)^  := RawToIntDef(Value, Value+Len, 0);
      DBTYPE_R4:        SQLStrToFloatDef(Value, 0, PSingle(Data)^, Len);
      DBTYPE_R8:        SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
      DBTYPE_CY:        SQLStrToFloatDef(Value, 0, PCurrency(Data)^, Len);
      DBTYPE_DATE:      PDateTime(Data)^ := RawSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed);
      //DBTYPE_IDISPATCH	= 9;
      //DBTYPE_ERROR	= 10;
      DBTYPE_BOOL:      PWordBool(Data)^ := StrToBoolEx(Value, Value+Len, True, False);
      //DBTYPE_VARIANT	= 12;
      //DBTYPE_IUNKNOWN	= 13;
      DBTYPE_UI1:       PByte(Data)^    := RawToIntDef(Value, Value+Len, 0);
      DBTYPE_I1:        PShortInt(Data)^:= RawToIntDef(Value, Value+Len, 0);
      DBTYPE_UI2:       PWord(Data)^    := RawToIntDef(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI4:       PCardinal(Data)^:= RawToUInt64Def(Value, Value+Len, 0);
      DBTYPE_UI8:       PUInt64(Data)^  := RawToUInt64Def(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      DBTYPE_GUID:      if Len = SizeOf(TGUID)
                        then Move(Value^, Data^, SizeOf(TGUID))
                        else ValidGUIDToBinary(Value, Data);
      (DBTYPE_GUID or DBTYPE_BYREF): if Len = SizeOf(TGUID) then
                          PPointer(Data)^ := Value
                        else begin
                          ValidGUIDToBinary(Value, @FABuffer[0]);
                          BindList.Put(Index, PGUID(@FABuffer[0])^);
                          PPointer(Data)^ := BindList[Index].Value;
                        end;
      DBTYPE_BYTES,
      DBTYPE_STR:       if Bind.cbMaxLen < Len+Byte(Ord(Bind.wType = DBTYPE_STR)) then
                          RaiseExceeded(Index)
                        else begin
                          Move(Value^, Data^, Len+Byte(Ord(Bind.wType = DBTYPE_STR)));
                          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
                        end;
      (DBTYPE_BYTES or DBTYPE_BYREF),
      (DBTYPE_STR or DBTYPE_BYREF): begin
              PPointer(Data)^ := Value;
              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
            end;
      DBTYPE_DBDATE: begin
          DecodeDate(RawSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed), W1,
            PDBDate(Data)^.month, PDBDate(Data)^.day);
          PDBDate(Data)^.year := W1;
        end;
      DBTYPE_DBTIME:
        DecodeTime(RawSQLTimeToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed),
          PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second, W1);
      DBTYPE_DBTIME2: begin
          DecodeTime(RawSQLTimeToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed),
            PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second, W1);
            PDBTIME2(Data)^.fraction := W1 * 1000000;
        end;
      DBTYPE_DBTIMESTAMP:  begin
          DecodeDateTime(RawSQLTimeStampToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed), W2,
            PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day, PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute,
            PDBTimeStamp(Data)^.second, W2);
          PDBTimeStamp(Data)^.year := W1;
          PDBTimeStamp(Data)^.fraction := W2*1000000
        end;
     else RaiseUnsupportedParamType(Index, Bind.wType, stString);
      //DBTYPE_UDT: ;
      //DBTYPE_HCHAPTER:;
      //DBTYPE_PROPVARIANT:;
      //DBTYPE_VARNUMERIC:;
    end;
  end;
end;

procedure TZOleDBPreparedStatement.SetParamCount(NewParamCount: Integer);
var OldParamCount: Integer;
begin
  OldParamCount := BindList.Count;
  if OldParamCount <> NewParamCount then begin
    inherited SetParamCount(NewParamCount);
    SetLength(FDBBindingArray, NewParamCount);
    SetLength(FDBBINDSTATUSArray, NewParamCount);
  end;
end;

procedure TZOleDBPreparedStatement.SetPWideChar(Index: Word; Value: PWideChar;
  Len: Cardinal);
var Bind: PDBBINDING;
  Data: PAnsichar;
  W1, W2: Word;
  Failed: Boolean;
label set_Raw;
begin
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_I4:        PInteger(Data)^  := UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_R4:        SQLStrToFloatDef(Value, 0, PSingle(Data)^, Len);
      DBTYPE_R8:        SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
      DBTYPE_CY:        SQLStrToFloatDef(Value, 0, PCurrency(Data)^, Len);
      DBTYPE_DATE:      PDateTime(Data)^ := UnicodeSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed);
      //DBTYPE_IDISPATCH	= 9;
      //DBTYPE_ERROR	= 10;
      DBTYPE_BOOL:      PWordBool(Data)^ := StrToBoolEx(Value, Value+Len, True, False);
      //DBTYPE_VARIANT	= 12;
      //DBTYPE_IUNKNOWN	= 13;
      DBTYPE_UI1:       PByte(Data)^    := UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_I1:        PShortInt(Data)^:= UnicodeToIntDef(Value, Value+Len, 0);
      DBTYPE_UI2:       PWord(Data)^    := UnicodeToIntDef(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI4:       PCardinal(Data)^:= UnicodeToUInt64Def(Value, Value+Len, 0);
      DBTYPE_UI8:       PUInt64(Data)^  := UnicodeToUInt64Def(Value, Value+Len, 0);
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      DBTYPE_I8:        PInt64(Data)^   := UnicodeToInt64Def(Value, Value+Len, 0);
      DBTYPE_GUID:      ValidGUIDToBinary(Value, Data);
      DBTYPE_GUID or DBTYPE_BYREF: begin
                          ValidGUIDToBinary(Value, @FABuffer[0]);
                          BindList.Put(Index, PGUID(@FABuffer[0])^);
                          PPointer(Data)^ := BindList[Index].Value;
                        end;
      DBTYPE_BYTES, (DBTYPE_BYTES or DBTYPE_BYREF): begin
            FRawTemp := UnicodeStringToAscii7(Value, Len);
            goto set_Raw;
          end;
      DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF): begin
            FRawTemp := PUnicodeToRaw(Value, Len, FClientCP);
            Len := Length(FRawTemp);
set_Raw:    if Bind.wType and DBTYPE_BYREF <> 0 then begin
              BindList.Put(Index, stString, FRawTemp, FClientCP); //keep alive
              PPointer(Data)^ := Pointer(FRawTemp);
              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
            end else if Bind.cbMaxLen < Len then
              RaiseExceeded(Index)
            else begin
              Move(Pointer(FRawTemp)^, Data^, Len);
              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len;
            end;
          end;
      DBTYPE_WSTR:  if Bind.cbMaxLen < Len*2 then
                      RaiseExceeded(Index)
                    else begin
                      Move(Value^, Data^, Len shl 1);
                      PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len shl 1;
                    end;
      (DBTYPE_WSTR or DBTYPE_BYREF): begin
                  PPointer(Data)^ := Value;
                  PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := Len shl 1;
                end;
      DBTYPE_DBDATE: begin
          DecodeDate(UnicodeSQLDateToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed), W1,
            PDBDate(Data)^.month, PDBDate(Data)^.day);
          PDBDate(Data)^.year := W1;
        end;
      DBTYPE_DBTIME:
        DecodeTime(UnicodeSQLTimeToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed),
          PDBTime(Data)^.hour, PDBTime(Data)^.minute, PDBTime(Data)^.second, W1);
      DBTYPE_DBTIME2: begin
          DecodeTime(UnicodeSQLTimeToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed),
            PDBTIME2(Data)^.hour, PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second, W1);
            PDBTIME2(Data)^.fraction := W1 * 1000000;
        end;
      DBTYPE_DBTIMESTAMP:  begin
          DecodeDateTime(UnicodeSQLTimeStampToDateTime(Value,  Len, ConSettings^.WriteFormatSettings, Failed), W2,
            PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day, PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute,
            PDBTimeStamp(Data)^.second, W2);
          PDBTimeStamp(Data)^.year := W1;
          PDBTimeStamp(Data)^.fraction := W2*1000000
        end;
      else RaiseUnsupportedParamType(Index, Bind.wType, stUnicodeString);
      //DBTYPE_UDT: ;
      //DBTYPE_HCHAPTER:;
      //DBTYPE_PROPVARIANT:;
      //DBTYPE_VARNUMERIC:;
    end;
  end;
end;

{**
  Sets the designated parameter to a Java <code>raw encoded string</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetRawByteString(Index: Integer;
  const Value: RawByteString);
begin
  BindRaw(Index, Value, FClientCP);
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetShort(Index: Integer; Value: ShortInt);
var I: Integer;
begin
  if fBindImmediat then
    SetUInt(Index, Value)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
    InitFixedBind(Index, SizeOf(ShortInt), DBTYPE_I1);
    I := Value;
    BindList.Put(Index, stShort, P4Bytes(@I));
  end;
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetSmall(Index: Integer; Value: SmallInt);
var I: Integer;
begin
  if fBindImmediat then
    SetInt(Index, Value)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
    InitFixedBind(Index, SizeOf(SmallInt), DBTYPE_I2);
    I := Value;
    BindList.Put(Index, stSmall, P4Bytes(@I));
  end;
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetString(Index: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  SetUnicodeString(Index, Value);
  {$ELSE}
  if ConSettings^.AutoEncode
  then BindRaw(Index, Value, zCP_NONE)
  else BindRaw(Index, Value, ConSettings^.CTRL_CP);
  {$ENDIF}
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetTime(Index: Integer;
  const Value: TDateTime);
var Bind: PDBBINDING;
  Data: PAnsichar;
  Y, MS: word;
label RConv, WConv;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4,
      DBTYPE_I8, DBTYPE_UI8: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Trunc(Value));
      DBTYPE_DATE:        PDateTime(Data)^ := Value;
      DBTYPE_DBDATE:      FillChar(Data^, SizeOf(TDBDate), #0);
      DBTYPE_DBTIME:      DecodeTime(Value, PDBTIME(Data)^.hour,
                              PDBTIME(Data)^.minute, PDBTIME(Data)^.second, MS);
      DBTYPE_DBTIME2:     begin
                            DecodeTime(Value, PDBTIME2(Data)^.hour,
                              PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second, MS);
                            PDBTIME2(Data)^.fraction := MS * 1000000;
                          end;
      DBTYPE_DBTIMESTAMP: begin  { note if we've a time field .. SQLServer still get stuck if date is not encoded to mindate }
          DecodeDateTime(Value, Y, PDBTimeStamp(Data)^.month, PDBTimeStamp(Data)^.day,
            PDBTimeStamp(Data)^.hour, PDBTimeStamp(Data)^.minute, PDBTimeStamp(Data)^.second, MS);
          PDBTimeStamp(Data)^.year := Y;
          PDBTimeStamp(Data)^.fraction := MS * 1000000;
        end;
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      (DBTYPE_STR or DBTYPE_BYREF): begin
          PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 13);
          Data := PPointer(Data)^;
          goto RConv;
        end;
      DBTYPE_STR: if (Bind.cbMaxLen >= 13){00.00.00.000#0} or (Bind.cbMaxLen-1 = DBLENGTH(ConSettings.WriteFormatSettings.TimeFormatLen)) then
RConv:              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                      DateTimeToRawSQLTime(Value, PAnsiChar(Data), ConSettings.WriteFormatSettings, False)
                  else RaiseExceeded(Index);
      DBTYPE_WSTR or DBTYPE_BYREF: begin
          PPointer(Data)^ := BindList.AquireCustomValue(Index, stUnicodeString, 26);
          Data := PPointer(Data)^;
          goto WConv;
        end;
      DBTYPE_WSTR: if (Bind.cbMaxLen >= 26 ){00.00.00.000#0} or ((Bind.cbMaxLen-2) shr 1 = DBLENGTH(ConSettings.WriteFormatSettings.TimeFormatLen)) then
WConv:          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                  DateTimeToUnicodeSQLTime(Value, PWideChar(Data), ConSettings.WriteFormatSettings, False)
              else RaiseExceeded(Index);
     else RaiseUnsupportedParamType(Index, Bind.wType, stTime);
    end;
  end else begin//Late binding
    InitDateBind(Index, stTime);
    BindList.Put(Index, stTime, P8Bytes(@Value));
  end;
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetTimestamp(Index: Integer;
  const Value: TDateTime);
var Bind: PDBBINDING;
  Data: PAnsichar;
  MS, Y: Word;
label RConv, WConv;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4,
      DBTYPE_I8, DBTYPE_UI8: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Trunc(Value));
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_R4, DBTYPE_R8, DBTYPE_CY, DBTYPE_NUMERIC, DBTYPE_VARNUMERIC:
        SetDouble(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_DATE:        PDateTime(Data)^ := Value;
      DBTYPE_DBDATE:      begin
                            DecodeDate(Value, Y, PDBDate(Data).month, PDBDate(Data).day);
                            PDBDate(Data).year := Y;
                          end;
      DBTYPE_DBTIME:      DecodeTime(Value, PDBTIME(Data)^.hour,
                              PDBTIME(Data)^.minute, PDBTIME(Data)^.second, MS);
      DBTYPE_DBTIME2:     begin
                            DecodeTime(Value, PDBTIME2(Data)^.hour,
                              PDBTIME2(Data)^.minute, PDBTIME2(Data)^.second, MS);
                            PDBTIME2(Data)^.fraction := MS * 1000000;
                          end;
      DBTYPE_DBTIMESTAMP: begin
          DecodeDateTime(Value, Y, PDBTimeStamp(Data).month, PDBTimeStamp(Data).day,
            PDBTimeStamp(Data).hour, PDBTimeStamp(Data)^.minute, PDBTimeStamp(Data)^.second, MS);
          PDBTimeStamp(Data)^.year := Y;
          PDBTimeStamp(Data)^.fraction := MS * 1000000;
        end;
      (DBTYPE_STR or DBTYPE_BYREF): begin
          PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 24);
          Data := PPointer(Data)^;
          goto RConv;
        end;
      DBTYPE_STR: if (Bind.cbMaxLen >= 24){0000-00-00T00.00.00.000#0} or (Bind.cbMaxLen-1 = DBLENGTH(ConSettings.WriteFormatSettings.DateTimeFormatLen)) then
RConv:              PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                      DateTimeToRawSQLTimestamp(Value, PAnsiChar(Data), ConSettings.WriteFormatSettings, False)
                  else RaiseExceeded(Index);
      DBTYPE_WSTR or DBTYPE_BYREF: begin
          PPointer(Data)^ := BindList.AquireCustomValue(Index, stUnicodeString, 48);
          Data := PPointer(Data)^;
          goto WConv;
        end;
      DBTYPE_WSTR: if (Bind.cbMaxLen >= 48){0000-00-00T00.00.00.000#0}  or ((Bind.cbMaxLen-2) shr 1 = DBLENGTH(ConSettings.WriteFormatSettings.DateTimeFormatLen)) then
WConv:          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ :=
                  DateTimeToUnicodeSQLTime(Value, PWideChar(Data), ConSettings.WriteFormatSettings, False)
              else RaiseExceeded(Index);
      else RaiseUnsupportedParamType(Index, Bind.wType, stTimeStamp);
    end;
  end else begin//Late binding
    InitDateBind(Index, stTimeStamp);
    BindList.Put(Index, stTimeStamp, P8Bytes(@Value));
  end;
end;

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetUInt(Index: Integer; Value: Cardinal);
var Bind: PDBBINDING;
  Data: PAnsichar;
  L: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Value;
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Value;
      DBTYPE_I1:        PShortInt(Data)^ := Value;
      DBTYPE_UI2:       PWord(Data)^ := Value;
      DBTYPE_UI4:       PCardinal(Data)^ := Value;
      DBTYPE_I8:        PInt64(Data)^ := Value;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      (*DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value);
          if Bind.wType = (DBTYPE_STR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 12);
            Data := PPointer(Data)^;
          end else if (L >= Bind.cbMaxLen) then
            RaiseExceeded(Index);
          PByte(Data+L)^ := Ord(#0);
          IntToRaw(Value, Data, L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L;
        end;*)
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 24);
            Data := PPointer(Data)^;
          end else if (L shl 1 >= Bind.cbMaxLen) then
            RaiseExceeded(Index);
          PWord(PWideChar(Data)+ L)^ := Ord(#0);
          IntToUnicode(Value, PWideChar(Data), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1;
        end;
      {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
      else RaiseUnsupportedParamType(Index, Bind.wType, stLongWord);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(Cardinal), DBTYPE_UI4);
    BindList.Put(Index, stLongWord, P4Bytes(@Value));
  end;
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZOleDBPreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
var Bind: PDBBINDING;
  Data: PAnsichar;
  L: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if fBindImmediat then begin
    Bind := @FDBBindingArray[Index];
    PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_OK;
    Data := PAnsiChar(fDBParams.pData)+Bind.obValue;
    case Bind.wType of
      DBTYPE_NULL:      PDBSTATUS(PAnsiChar(FDBParams.pData)+Bind.obStatus)^ := DBSTATUS_S_ISNULL; //Shouldn't happen
      DBTYPE_I2:        PSmallInt(Data)^ := Value;
      DBTYPE_I4:        PInteger(Data)^ := Value;
      DBTYPE_R4:        PSingle(Data)^ := Value;
      DBTYPE_R8:        PDouble(Data)^ := Value;
      DBTYPE_CY:        PCurrency(Data)^ := Value;
      DBTYPE_BOOL:      PWordBool(Data)^ := Value <> 0;
      DBTYPE_VARIANT:   POleVariant(Data)^ := Value;
      DBTYPE_UI1:       PByte(Data)^ := Value;
      DBTYPE_I1:        PShortInt(Data)^ := Value;
      DBTYPE_UI2:       PWord(Data)^ := Value;
      DBTYPE_UI4:       PCardinal(Data)^ := Value;
      DBTYPE_I8:        PInt64(Data)^ := Value;
      DBTYPE_UI8:       PUInt64(Data)^ := Value;
      (*DBTYPE_STR, (DBTYPE_STR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value);
          if Bind.wType = (DBTYPE_STR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 24); //8Byte align
            Data := PPointer(Data)^; //18.446.744.073.709.551.615
          end else if (Bind.cbMaxLen <= L) then
            RaiseExceeded(Index);
          IntToRaw(Value, PAnsiChar(Data), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L;
        end; *)
      DBTYPE_WSTR, (DBTYPE_WSTR or DBTYPE_BYREF): begin
          L := GetOrdinalDigits(Value);
          if Bind.wType = (DBTYPE_WSTR or DBTYPE_BYREF) then begin
            PPointer(Data)^ := BindList.AquireCustomValue(Index, stString, 48); //8Byte align
            Data := PPointer(Data)^; //18.446.744.073.709.551.615
          end else if (Bind.cbMaxLen <= L shl 1) then
            RaiseExceeded(Index);
          IntToUnicode(Value, PWideChar(Data), L);
          PDBLENGTH(PAnsiChar(fDBParams.pData)+Bind.obLength)^ := L shl 1;
        end;
      //DBTYPE_NUMERIC:;
      //DBTYPE_VARNUMERIC:;
      else RaiseUnsupportedParamType(Index, Bind.wType, stULong);
    end;
  end else begin//Late binding
    InitFixedBind(Index, SizeOf(UInt64), DBTYPE_UI8);
    BindList.Put(Index, stULong, P8Bytes(@Value));
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Sets the designated parameter to a Object Pascal <code>WideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetUnicodeString(Index: Integer;
  const Value: ZWideString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  BindList.Put(Index, stUnicodeString, Value);
  if fBindImmediat then
    if Value <> ''
    then SetPWideChar(Index, Pointer(Value), Length(Value))
    else SetPWideChar(Index, PEmptyUnicodeString, 0)
  else InitVaryBind(Index, (Length(Value)+1) shl 1, DBTYPE_WSTR);
end;

{**
  Sets the designated parameter to a Java <code>UTF8String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_UTF8STRING}
procedure TZOleDBPreparedStatement.SetUTF8String(Index: Integer;
  const Value: UTF8String);
begin
  BindRaw(Index, Value, zCP_UTF8);
end;
{$ENDIF}

{**
  Sets the designated parameter to a Java <code>unsigned 16bit int</code> value.
  The driver converts this
  to an SQL <code>WORD</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZOleDBPreparedStatement.SetWord(Index: Integer; Value: Word);
var C: Cardinal;
begin
  if fBindImmediat then
    SetUInt(Index, Value)
  else begin
    {$IFNDEF GENERIC_INDEX}
    Index := Index -1;
    {$ENDIF}
    CheckParameterIndex(Index);
    InitFixedBind(Index, SizeOf(Word), DBTYPE_UI2);
    C := Value;
    BindList.Put(Index, stWord, P4Bytes(@C));
  end;
end;

function TZOleDBPreparedStatement.SupportsBidirectionalParams: Boolean;
begin
  Result := True;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOleDBPreparedStatement.UnPrepareInParameters;
var FAccessorRefCount: DBREFCOUNT;
begin
  if Assigned(FParameterAccessor) then begin
    //don't forgett to release the Accessor else we're leaking mem on Server!
    FParameterAccessor.ReleaseAccessor(FDBParams.hAccessor, @FAccessorRefCount);
    FDBParams.hAccessor := 0;
    FParameterAccessor := nil;
  end;
  inherited UnPrepareInParameters;
end;

{ TZOleDBStatement }

constructor TZOleDBStatement.Create(const Connection: IZConnection;
  const Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZOleDBCallableStatementMSSQL }

function TZOleDBCallableStatementMSSQL.CreateExecutionStatement(Mode: TZCallExecKind;
  const StoredProcName: String): TZAbstractPreparedStatement2;
var  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  Buf: {$IFDEF UNICODE}TUCS2Buff{$ELSE}TRawBuff{$ENDIF};
begin
  //https://docs.microsoft.com/en-us/sql/relational-databases/native-client-ole-db-how-to/results/execute-stored-procedure-with-rpc-and-process-output?view=sql-server-2017
  Buf.Pos := 0;
  SQL := '';
  ZDbcUtils.ToBuff('{? = CALL ', Buf, SQL);
  ZDbcUtils.ToBuff(StoredProcName, Buf, SQL);
  ZDbcUtils.ToBuff('(', Buf, SQL);
  for i := 1 to BindList.Count-1 do
    ZDbcUtils.ToBuff('?,', Buf, SQL);
  ReplaceOrAddLastChar(',', ')', Buf, SQL);
  ZDbcUtils.ToBuff('}', Buf, SQL);
  ZDbcUtils.FlushBuff(Buf, SQL);
  Result := TZOleDBPreparedStatement.Create(Connection, SQL, Info);
  TZOleDBPreparedStatement(Result).Prepare;
  FExecStatements[TZCallExecKind(not Ord(Mode) and 1)] := Result;
  TZOleDBPreparedStatement(Result)._AddRef;
end;

procedure TZOleDBCallableStatementMSSQL.PrepareInParameters;
var I: Integer;
  RS: IZResultSet;
begin
  if Connection.UseMetadata then begin
    //Register the ParamNames
    RS := Connection.GetMetadata.GetProcedureColumns(Connection.GetCatalog,
      '', StoredProcName, '');
    I := FirstDbcIndex;
    while RS.Next do begin
      FExecStatements[FCallExecKind].RegisterParameter(I,
        TZSQLType(RS.GetInt(ProcColDataTypeIndex)),
        TZProcedureColumnType(RS.GetInt(ProcColColumnTypeIndex)),
        RS.GetString(ProcColColumnNameIndex), RS.GetInt(ProcColPrecisionIndex),
        RS.GetInt(ProcColScaleIndex));
      Inc(I);
    end;
    Assert(I-FirstDbcIndex = BindList.Count);
  end else
    inherited PrepareInParameters;
end;

function TZOleDBCallableStatementMSSQL.SupportsBidirectionalParams: Boolean;
begin
  Result := True;
end;

{$ENDIF ZEOS_DISABLE_OLEDB} //if set we have an empty unit
end.
