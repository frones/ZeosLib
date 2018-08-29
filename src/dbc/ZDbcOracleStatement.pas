{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcOracleStatement;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  {$IFDEF MSWINDOWS}{%H-}Windows,{$ENDIF}
  ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainOracleDriver,
  ZCompatibility, ZVariant, ZDbcOracleUtils, ZPlainOracleConstants,
  ZDbcOracle;

type

  {** Implements a abstract prepared SQL Statement for Oracle }
  TZAbstractOraclePreparedStatement = class(TZRawParamDetectPreparedStatement)
  private
    FOCIStmt: POCIStmt;
    FOCIError: POCIError;
    FPlainDriver: TZOraclePlainDriver;
    FOracleConnection: IZOracleConnection;
    FOraVariables: PZOCIParamBinds;
    FRowPrefetchMemory: ub4;
    FZBufferSize: Integer;
    FStatementType: ub2;
    FServerStmtCache: Boolean;
    FCanBindInt64: Boolean;
  protected
    procedure InitBuffer(SQLType: TZSQLType; OCIBind: PZOCIParamBind; Index, ElementCnt: Cardinal; ActualLength: LengthInt = 0);
    function CreateResultSet: IZResultSet;
    procedure SetBindCapacity(Capacity: Integer); override;
    procedure CheckParameterIndex(Index: Integer); override;
    function GetInParamLogValue(Index: Integer): RawByteString; override;
  protected
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindBoolean(Index: Integer; Value: Boolean); override;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); override;
    procedure BindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindNull(Index: Integer; SQLType: TZSQLType); override;
    procedure BindSignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: Int64); override;
    procedure BindUnsignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: UInt64); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString);override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);

    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZParamType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); override;
  end;

  {** Implements Prepared SQL Statement for Oracle }
  TZOraclePreparedStatement = class(TZAbstractOraclePreparedStatement, IZPreparedStatement);

  {** Implements SQL Statement for Oracle }
  TZOracleStatement = class(TZAbstractOraclePreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
  end;

  TZOracleCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FOutParamCount: Integer;
    FErrorHandle: POCIError;
    FParams: PZSQLVars;
    FPlainDriver: TZOraclePlainDriver;
    FHandle: POCIStmt;
    FOracleParams: TZOracleParams;
    FOracleParamsCount: Integer;
    FParamNames: TStringDynArray;
    PackageIncludedList: TStrings;
    FParamsBuffer: TByteDynArray;
    FRowPrefetchSize: ub4;
    FZBufferSize: Integer;
    FStatementType: ub2;
    FIteration: Integer;
    FCanBindInt64: Boolean;
    FOracleConnection: IZOracleConnection;
    procedure SortZeosOrderToOCIParamsOrder;
    procedure FetchOutParamsFromOracleVars;
    function GetProcedureSql: RawByteString;
  protected
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); override;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      const ParamTypeName: String; const ParamName: String; Const {%H-}ColumnSize, {%H-}Precision: Integer);
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer); override;
    procedure RegisterParamType(ParameterIndex: integer; ParamType: Integer); override;
    procedure Prepare; override;
    procedure Unprepare; override;

    Function ExecuteUpdatePrepared: Integer; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    constructor Create(const Connection: IZConnection; const pProcName: string; Info: TStrings);
    destructor Destroy; override;
    procedure ClearParameters; override;
  end;

implementation

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZDbcOracleResultSet, ZTokenizer, ZDbcCachedResultSet,
  ZEncoding, ZDbcUtils, ZDbcProperties, ZMessages, ZClasses;

const
  CommitMode: array[Boolean] of ub4 = (OCI_DEFAULT, OCI_COMMIT_ON_SUCCESS);
  StrGUIDLen = 36;
  NO_DTYPE = 0;
  SQLType2OCIType: array[stBoolean..stBinaryStream] of ub2 = (
    SQLT_INT, SQLT_UIN, SQLT_INT, SQLT_UIN, SQLT_INT, SQLT_UIN, SQLT_INT, SQLT_UIN, SQLT_INT,  //ordinals
    SQLT_FLT, SQLT_FLT, SQLT_FLT, SQLT_FLT, //floats
    SQLT_DAT, SQLT_TIMESTAMP, SQLT_TIMESTAMP, //time values
    SQLT_AFC, //GUID
    SQLT_LVC, SQLT_LVC, SQLT_LVB, //varying size types in equal order
    SQLT_CLOB, SQLT_CLOB, SQLT_BLOB); //lob's
  SQLType2OCISize: array[stBoolean..stBinaryStream] of sb2 = (
    SizeOf(Integer), SizeOf(Word), SizeOf(SmallInt), SizeOf(Word), SizeOf(SmallInt), SizeOf(Cardinal), SizeOf(Integer), SizeOf(UInt64), SizeOf(Int64),  //ordinals
    SizeOf(Single), SizeOf(Double), SizeOf(Double), SizeOf(Double), //floats
    SizeOf(TOraDate), SizeOf(POCIDescriptor), SizeOf(POCIDescriptor), //time values
    StrGUIDLen, //GUID
    8+SizeOf(Integer), 8+SizeOf(Integer), 8+SizeOf(Integer),  //varying size types in equal order minimum sizes for 8Byte alignment
    SizeOf(POCIDescriptor), SizeOf(POCIDescriptor), SizeOf(POCIDescriptor)); //lob's
  SQLType2OCIDescriptor: array[stBoolean..stBinaryStream] of sb2 = (
    NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE,  //ordinals
    NO_DTYPE, NO_DTYPE, NO_DTYPE, NO_DTYPE, //floats
    NO_DTYPE, OCI_DTYPE_TIMESTAMP, OCI_DTYPE_TIMESTAMP, //time values
    NO_DTYPE, //GUID
    NO_DTYPE, NO_DTYPE, NO_DTYPE,  //varying size types in equal order minimum sizes for 8Byte alignment
    OCI_DTYPE_LOB, OCI_DTYPE_LOB, OCI_DTYPE_LOB); //lob's

{ TZOracleCallableStatement }

procedure TZOracleCallableStatement.Prepare;
begin
  if not Prepared then
  begin
    ASQL := GetProcedureSql;
    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    PrepareOracleStatement(FPlainDriver, nil, ASQL, FHandle, FErrorHandle,
          FRowPrefetchSize, False, ConSettings);
    FPlainDriver.OCIAttrGet(FHandle, OCI_HTYPE_STMT, @FStatementType, nil,
      OCI_ATTR_STMT_TYPE, FErrorHandle);
    inherited Prepare;
  end;
end;

procedure TZOracleCallableStatement.UnPrepare;
const {%H-}RELEASE_MODE: array[boolean] of integer = (OCI_DEFAULT,OCI_STMTCACHE_DELETE);
begin
  try
    if False{FServerStmtCache} then
      {%H-}CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.OCIStmtRelease(FHandle, FErrorHandle, nil, 0, RELEASE_MODE[False]),
      lcExecute, ASQL, ConSettings)
    else
      FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  finally
    inherited Unprepare;
  end;
end;

procedure TZOracleCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  inherited RegisterOutParameter(ParameterIndex,SQLType);
  with FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
  begin
    if not GetConnection.UseMetadata then
      pName := 'pOut'+ZFastCode.IntToStr(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
    pSQLType := SQLType;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamType(ParameterIndex: integer;
  ParamType: Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  if ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} > FOracleParamsCount then
    FOracleParamsCount := ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF};
  FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pType := ParamType;
  FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pParamIndex := ParameterIndex;
  if ParamType in [2,3,4] then //ptInOut, ptOut, ptResult
  begin
    FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pOutIndex := FOutParamCount;
    Inc(FOutParamCount);
  end;
end;

procedure TZOracleCallableStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
var 
  AConnection: IZConnection;

  function GetOracleParamIndexOfParameterIndex: Integer;
  var I: Integer;
  begin
    Result := 0;
    for i := 0 to high(FOracleParams) do
      if ParameterIndex = FOracleParams[i].pParamIndex then
      begin
        Result := I;
        Break;
      end;
  end;

begin
  inherited SetInParam(ParameterIndex, SQLType, Value);
  with FOracleParams[GetOracleParamIndexOfParameterIndex] do
  begin
    AConnection := GetConnection;
    if Assigned(AConnection) and ( not AConnection.UseMetadata ) then
      pName := 'p'+ZFastCode.IntToStr(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
    pSQLType := ord(SQLType);
  end;
end;

procedure TZOracleCallableStatement.RegisterParamTypeAndName(const ParameterIndex: integer;
  const ParamTypeName: String; const ParamName: String; Const ColumnSize, Precision: Integer);
var
  iPos: Integer;
  ProcName: String;
begin
  FOracleParams[ParameterIndex].pName := ParamName;
  FOracleParams[ParameterIndex].pTypeName := ParamTypeName;
  iPos := ZFastCode.Pos('.', ParamName);
  if iPos > 0 then
  begin
    ProcName := Copy(ParamName, 1, iPos-1); //extract function or Procedure names
    FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.IndexOf(ProcName); //check index
    if FOracleParams[ParameterIndex].pProcIndex = -1 then //if not exists
      FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.Add(ProcName); //Add to List
  end
  else //No package
    FOracleParams[ParameterIndex].pProcIndex := 0;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZOracleCallableStatement.PrepareInParameters;
var
  I: Integer;
  CurrentVar: PZSQLVar;
  Status: Integer;
  BufferSize: Int64;
  CurrentBufferEntry: PAnsiChar;
  SQLType: TZSQLType;
  Label CheckMaxIter;
begin
  AllocateOracleSQLVars(FParams, FOracleParamsCount);
  SortZeosOrderToOCIParamsOrder;
  SetLength(FParamNames, FOracleParamsCount);
  BufferSize := 0;
  FIteration := 0;
  if FParams^.AllocNum = 0 then goto CheckMaxIter; //nothing to do here

  {first determine oracle type and check out required buffer-size we need }
  for I := 0 to FParams^.AllocNum - 1 do
  begin
    FParamNames[I] := Self.FOracleParams[I].pName;
    {$R-}
    CurrentVar := @FParams.Variables[I];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    CurrentVar.Handle := nil;
    SQLType := TZSQLType(FOracleParams[I].pSQLType);
    { Artificially define Oracle internal type. }
    if SQLType = stBytes then
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_Raw_Size, SQLT_LVC, FCanBindInt64)
    else if SQLType = stBinaryStream then
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_String_Size, SQLT_BLOB, FCanBindInt64)
    else if SQLType in [stAsciiStream, stUnicodeStream] then
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_String_Size, SQLT_CLOB, FCanBindInt64)
    else
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_String_Size, SQLT_STR, FCanBindInt64);
    Inc(BufferSize, CalcBufferSizeOfSQLVar(CurrentVar));
  end; //Buffer size is determined now
  FIteration := Ord((ArrayCount = 0) and (InparamCount > 0)) or ArrayCount; //determine initial iters
  Inc(BufferSize, BufferSize * FIteration); //determine inital buffersize
  if BufferSize >= High(LongWord)-1 then
    raise Exception.Create('Memory out of bounds! OCI-Limit = 4GB -1Byte');
  if Length(FParamsBuffer) < BufferSize then SetLength(FParamsBuffer, BufferSize); //Alloc new buffer if required
  CurrentBufferEntry := Pointer(FParamsBuffer);

  { now let's set data-entries, bind them }
  for i := 0 to FParams.AllocNum -1 do
  begin
    {$R-}
    CurrentVar := @FParams.Variables[I];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    CurrentVar.Handle := nil;
    SetVariableDataEntrys(CurrentBufferEntry, CurrentVar, FIteration);
    AllocDesriptors(FPlainDriver, (Connection as IZOracleConnection).GetConnectionHandle,
      CurrentVar, FIteration, True);
    Status := FPlainDriver.OCIBindByPos(FHandle, CurrentVar^.BindHandle, FErrorHandle,
      I + 1, CurrentVar^.Data, CurrentVar^.Length, CurrentVar^.TypeCode,
      CurrentVar^.oIndicatorArray, CurrentVar^.oDataSizeArray, nil, 0, nil, OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, ASQL, ConSettings);
  end;
  CheckMaxIter:
  FIteration := Max(FIteration, 1);
end;

{**
  Binds the input parameters
}
procedure TZOracleCallableStatement.BindInParameters;
var
  I: Integer;
begin
  FIteration := Max(1, Min(FIteration, ArrayCount));
  if FParams^.AllocNum > 0 then
    {$R-}
    for I := 0 to FParams^.AllocNum - 1 do
      if (FOracleParams[i].pType in [1,3]) then
        LoadOracleVar(FPlainDriver, Connection, FErrorHandle, @FParams.Variables[I],
          InParamValues[FOracleParams[i].pParamIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}],
            ChunkSize, Max(1, Min(FIteration, ArrayCount)))
      else
        LoadOracleVar(FPlainDriver, Connection, FErrorHandle,
          @FParams.Variables[I], NullVariant, ChunkSize,
            Max(1, Min(FIteration, ArrayCount)));
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOracleCallableStatement.UnPrepareInParameters;
begin
  FreeOracleSQLVars(FPlainDriver, FParams, FIteration,
    (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings)
end;

procedure TZOracleCallableStatement.SortZeosOrderToOCIParamsOrder;
var
  I, J, NewProcIndex, StartProcIndex: Integer;
  TempOraVar: TZOracleParam;
begin
  NewProcIndex := -1;
  StartProcIndex := 0;
  if IsFunction then
    for i := 0 to high(FOracleParams) do
    begin
      if not ( FOracleParams[i].pProcIndex = NewProcIndex ) then
      begin
        NewProcIndex := FOracleParams[i].pProcIndex;
        StartProcIndex := I;
      end;
      if ( FOracleParams[i].pType = 4 ) then //Result value
      begin
        if not (i = StartProcIndex) then
        begin
          TempOraVar := FOracleParams[I];
          for J := I downto StartProcIndex+1 do
            FOracleParams[j] := FOracleParams[j-1];
          FOracleParams[StartProcIndex] := TempOraVar;
        end;
      end;
    end;
end;

procedure TZOracleCallableStatement.FetchOutParamsFromOracleVars;
var
  LobLocator: POCILobLocator;
  I: integer;
  TempBlob: IZBlob;

  procedure SetOutParam(CurrentVar: PZSQLVar; Index: Integer);
  var
    Year:SmallInt;
    Month, Day:Byte; Hour, Min, Sec:ub1; MSec: ub4;
    {$IFDEF UNICODE}
    {$ELSE}
    RawTemp: RawByteString;
    {$ENDIF}
  begin
    if CurrentVar^.oIndicatorArray[0] < 0 then
      outParamValues[Index] := NullVariant
    else
      case CurrentVar^.TypeCode of
        SQLT_INT: outParamValues[Index] := EncodeInteger(PLongInt(CurrentVar^.Data)^ );
        SQLT_FLT: outParamValues[Index] := EncodeFloat(PDouble(CurrentVar^.Data)^ );
        SQLT_STR:
          begin
            {$IFDEF UNICODE}
            outParamValues[Index] := EncodeString(PRawToUnicode(CurrentVar^.Data,
              CurrentVar^.oDataSizeArray[0], ConSettings^.ClientCodePage^.CP));
            {$ELSE}
            ZSetString(CurrentVar^.Data, CurrentVar^.oDataSizeArray[0], RawTemp{%H-});
            outParamValues[Index] := EncodeString(ConSettings.ConvFuncs.ZRawToString(RawTemp, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP));
            {$ENDIF}
          end;
        SQLT_TIMESTAMP:
          begin
            FPlainDriver.OCIDateTimeGetDate(
              FOracleConnection.GetConnectionHandle ,
              FErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
              Year{%H-}, Month{%H-}, Day{%H-});
            FPlainDriver.OCIDateTimeGetTime(
              FOracleConnection.GetConnectionHandle ,
              FErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
              Hour{%H-}, Min{%H-}, Sec{%H-},MSec{%H-});
            outParamValues[Index] := EncodeDateTime(EncodeDate(year,month,day )+EncodeTime(Hour,min,sec,  msec div 1000000));
          end;
        SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE:
          begin
            LobLocator := PPOCIDescriptor(CurrentVar^.Data)^;
            if CurrentVar^.TypeCode in [SQLT_BLOB, SQLT_BFILEE] then
              TempBlob := TZOracleBlob.Create(FPlainDriver, nil, 0,
                FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
                  LobLocator, GetChunkSize, ConSettings)
            else
              TempBlob := TZOracleClob.Create(FPlainDriver, nil, 0,
                FOracleConnection.GetConnectionHandle,
                FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
                LobLocator, GetChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
            outParamValues[Index] := EncodeInterface(TempBlob);
            TempBlob := nil;
          end;
        SQLT_NTY: //currently not supported
          outParamValues[Index] := NullVariant;
      end;
  end;
begin
  {$R-}
  for I := 0 to FOracleParamsCount -1 do
    if FOracleParams[i].pType in [2,3,4] then
      SetOutParam(@FParams^.Variables[I], FOracleParams[i].pParamIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZOracleCallableStatement.GetProcedureSql: RawByteString;
var
  sFunc: string;
  I, IncludeCount, LastIndex: Integer;
  PackageBody: TStrings;
  TempResult: String;

  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if ( FDBParamTypes[I] = zptResult ) then
      begin
        sFunc := ' :'+FOracleParams[0].pName+' := ';
        continue;
      end;
      if Result <> '' then
        Result := Result + ',';
      if IsFunction then
        Result := Result + ':'+FOracleParams[I+1].pName
      else
        Result := Result + ':'+FOracleParams[I].pName;
    end;
    Result := '('+Result+')'
  end;

var
  InParams, sName: string;
begin
  sFunc := '';
  if PackageIncludedList.Count > 0 then
  begin
    PackageBody := TStringList.Create;
    PackageBody.Add('BEGIN');
    LastIndex := 0;
    for IncludeCount := 0 to PackageIncludedList.Count -1 do
    begin
      InParams := '';
      sFunc := '';
      for i := LastIndex to high(FOracleParams) do
        if IncludeCount = FOracleParams[i].pProcIndex then
        begin
          sName := RemoveChar('.', FOracleParams[I].pName);
          if ( FOracleParams[I].pType = 4 ) then //ptResult
            sFunc := ' :'+sName+' := '
          else
            if InParams <> '' then
              InParams := InParams +', :'+sName
            else
              InParams := InParams +':'+sName
        end
        else
        begin
          LastIndex := I;
          break;
        end;
      PackageBody.Add('BEGIN '+sFunc+SQL+
        '.'+GetConnection.GetMetadata.GetIdentifierConvertor.Quote(PackageIncludedList[IncludeCount])+'('+InParams+'); END;');
    end;
    PackageBody.Add('END;');
    TempResult := TrimRight(PackageBody.Text);
    FreeAndNil(PackageBody);
  end
  else
  begin
    InParams := GenerateParamsStr( FOracleParamsCount );
    TempResult := 'BEGIN ' + sFunc +SQL + InParams+'; END;';
  end;
  Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(TempResult);
end;

procedure TZOracleCallableStatement.ClearParameters;
begin
  inherited;
  FOracleParamsCount := 0;
  SetLength(FOracleParams, 0);
end;

constructor TZOracleCallableStatement.Create(const Connection: IZConnection;
  const pProcName: string; Info: TStrings);
begin
  inherited Create(Connection, pProcName, Info);
  FOracleConnection := Connection as IZOracleConnection;
  FOracleParamsCount := 0;
  FPlainDriver := TZOraclePlainDriver(Connection.GetIZPlainDriver.GetInstance);
  ResultSetType := rtForwardOnly;
  PackageIncludedList := TStringList.Create;
  FOutParamCount := 0;
  FCanBindInt64 := Connection.GetClientVersion >= 11002000;
  FRowPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_RowPrefetchSize, ''), 131072);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_InternalBufSize, ''), 131072);
  FIteration := 1;
end;

destructor TZOracleCallableStatement.Destroy;
begin
  FreeOracleSQLVars(FPlainDriver, FParams, FIteration, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings);
  PackageIncludedList.Free;
  inherited;
end;

function TZOracleCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  { Prepares a statement. }
  Prepare;

  BindInParameters;
  try
    CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.OCIStmtExecute(FOracleConnection.GetContextHandle,
        FHandle, FErrorHandle, FIteration, 0, nil, nil, CommitMode[Connection.GetAutoCommit]),
      lcExecute, ASQL, ConSettings);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    FetchOutParamsFromOracleVars;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FParams)
  end;

  { Autocommit statement. done by ExecuteOracleStatement}
  Result := LastUpdateCount;
end;

function TZOracleCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;

  BindInParameters;
  try
    CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.OCIStmtExecute(FOracleConnection.GetContextHandle,
        FHandle, FErrorHandle, FIteration, 0, nil, nil, CommitMode[Connection.GetAutoCommit]),
      lcExecute, ASQL, ConSettings);
    FetchOutParamsFromOracleVars;
    LastResultSet := CreateOracleResultSet(Self, Self.SQL,
      FHandle, FErrorHandle, FParams, FOracleParams);
    Result := LastResultSet;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FParams);
  end;
end;

var
  OraPreparableTokens: TPreparablePrefixTokens;

{ TZAbstractOraclePreparedStatement }

procedure TZAbstractOraclePreparedStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var
  Bind: PZOCIParamBind;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.value_sz < Len+SizeOf(Integer)) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1, Len);
  PInteger(Bind^.valuep)^ := Len;
  if Buf <> nil then
    Move(Buf^, (Bind.valuep+SizeOf(Integer))^, Len);
  Bind.indp[0] := 0;
end;

procedure TZAbstractOraclePreparedStatement.BindBoolean(Index: Integer;
  Value: Boolean);
begin
  BindSignedOrdinal(Index, stBoolean, Ord(Value));
end;

procedure TZAbstractOraclePreparedStatement.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
var
  Bind: PZOCIParamBind;
  TS: TZTimeStamp;
  Status: sword;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  DecodeDate(Value, TS.Year, TS.Month, TS.Day); //oracle does not accept 0 dates
  if SQLType in [stTime, stTimeStamp] then begin
    DecodeTime(Value, TS.Hour, TS.Minute, TS.Second, PWord(@TS.Fractions)^);
    TS.Fractions := Word(TS.Fractions) * 1000000;
  end else begin
    PInt64(@TS.Hour)^ := 0; //init
    TS.Fractions := 0;
  end;
  if SQLType = stDate then begin
    POraDate(Bind^.valuep).Cent   := TS.Year div 100 +100;
    POraDate(Bind^.valuep).Year   := TS.Year mod 100 +100;
    POraDate(Bind^.valuep).Month  := TS.Month;
    PLongInt(@POraDate(Bind^.valuep).Day)^ := 0; //init all remaining fields to 0 with one 4Byte value
    POraDate(Bind^.valuep).Day    := TS.Day;
  end else begin //switch to msec precision
    Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
      FOCIError, PPOCIDescriptor(Bind.valuep)^, TS.Year, TS.Month, TS.Day,
        TS.Hour, TS.Minute, TS.Second, TS.Fractions, nil, 0);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FOCIError, Status, lcOther, '', ConSettings);
  end;
  Bind.indp[0] := 0;
end;

procedure TZAbstractOraclePreparedStatement.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var
  Bind: PZOCIParamBind;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  if Bind.value_sz = sizeOf(Double)
  then PDouble(Bind.valuep)^ := Value
  else PSingle(Bind.valuep)^ := Value;
  Bind.indp[0] := 0;
end;

procedure TZAbstractOraclePreparedStatement.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var
  Bind: PZOCIParamBind;
  WriteTempBlob: IZOracleBlob;
begin
  Inherited BindLob(Index, SQLType, Value);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (Value = nil) or Value.IsEmpty then
    Bind.indp[0] := -1
  else begin
    if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
      InitBuffer(SQLType, Bind, Index, 1, SizeOf(POCIDescriptor));
    if not Supports(Value, IZOracleBlob, WriteTempBlob) then
      if Bind.dty = SQLT_BLOB
      then WriteTempBlob := TZOracleBlob.Create(FPlainDriver, nil, 0,
          FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
          PPOCIDescriptor(Bind^.valuep)^, ChunkSize, ConSettings)
      else WriteTempBlob := TZOracleClob.Create(FPlainDriver, nil, 0,
          FOracleConnection.GetConnectionHandle,
          FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
          PPOCIDescriptor(Bind^.valuep)^, ChunkSize, ConSettings, ConSettings.ClientCodePage^.CP);
    WriteTempBlob.CreateBlob;
    WriteTempBlob.WriteLobFromBuffer(Value.GetBuffer, Value.Length);
    IZBLob(BindList[Index].Value) := WriteTempBlob;
    Bind.indp[0] := 0;
  end;
end;

procedure TZAbstractOraclePreparedStatement.BindNull(Index: Integer;
  SQLType: TZSQLType);
var
  Bind: PZOCIParamBind;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  Bind.indp[0] := -1;
end;

procedure TZAbstractOraclePreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
var
  Bind: PZOCIParamBind;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (BindList[Index].SQLType <> stString) or (Bind.valuep = nil) or (Bind.value_sz < Len+SizeOf(Integer)) or (Bind.curelen <> 1) then
    InitBuffer(stString, Bind, Index, 1, Len);
  PInteger(Bind.valuep)^ := Len;
  if Buf <> nil then
    Move(Buf^, (Bind.valuep+SizeOf(Integer))^, Len);
  Bind.indp[0] := 0;
end;

procedure TZAbstractOraclePreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  BindRawStr(Index, Pointer(Value), Length(Value){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
end;

procedure TZAbstractOraclePreparedStatement.BindSignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: Int64);
var
  Bind: PZOCIParamBind;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  if Bind.dty = SQLT_INT then
    if Bind.value_sz = SizeOf(Int64) then
      PInt64(Bind.valuep)^ := Value
    else if Bind.value_sz = SizeOf(LongInt) then
      PLongInt(Bind.valuep)^ := Value
    else
      PSmallInt(Bind.valuep)^ := Value
  else begin
    FRawTemp := IntToRaw(Value);
    PSmallInt(Bind.valuep)^ := Length(FRawTemp){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
    Move(Pointer(FRawTemp)^, (Bind.valuep+SizeOf(SmallInt))^, PSmallInt(Bind.valuep)^);
  end;
  Bind.indp[0] := 0;
end;

procedure TZAbstractOraclePreparedStatement.BindUnsignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: UInt64);
var
  Bind: PZOCIParamBind;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  if Bind.dty = SQLT_UIN then
    if Bind.value_sz = SizeOf(UInt64) then
      PUInt64(Bind.valuep)^ := Value
    else if Bind.value_sz = SizeOf(Cardinal) then
      PCardinal(Bind.valuep)^ := Value
    else
      PWord(Bind.valuep)^ := Value
  else begin
    FRawTemp := IntToRaw(Value);
    PSmallInt(Bind.valuep)^ := Length(FRawTemp){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
    Move(Pointer(FRawTemp)^, (Bind.valuep+SizeOf(SmallInt))^, PSmallInt(Bind.valuep)^);
  end;
  Bind.indp[0] := 0;
end;

procedure TZAbstractOraclePreparedStatement.CheckParameterIndex(Index: Integer);
begin
  if not Prepared then
    Prepare;
  inherited CheckParameterIndex(Index);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param SQL a query to execute.
  @param Info a statement parameters.
}
constructor TZAbstractOraclePreparedStatement.Create(
  const Connection: IZConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := TZOraclePlainDriver(Connection.GetIZPlainDriver.GetInstance);
  ResultSetType := rtForwardOnly;
  fOracleConnection := Connection as IZOracleConnection;
  FCanBindInt64 := Connection.GetClientVersion >= 11002000;
  FRowPrefetchMemory := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_RowPrefetchSize, ''), 131072);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_InternalBufSize, ''), 131072);
end;

function TZAbstractOraclePreparedStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZOracleResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  if FOpenResultSet = nil then begin
    NativeResultSet := TZOracleResultSet.Create(Self, SQL, FOCIStmt, FOCIError, FZBufferSize);
    NativeResultSet.SetConcurrency(rcReadOnly);
    if (GetResultSetConcurrency = rcUpdatable) or (GetResultSetType <> rtForwardOnly) then
    begin
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil, ConSettings);
      if (GetResultSetConcurrency = rcUpdatable) then
        CachedResultSet.SetConcurrency(rcUpdatable);
      CachedResultSet.SetResolver(TZOracleCachedResolver.Create(Self, NativeResultSet.GetMetadata));
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
    FOpenResultSet := Pointer(Result);
  end else
    Result := IZResultSet(FOpenResultSet);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractOraclePreparedStatement.ExecutePrepared: Boolean;
var
  Status: sword;
  upCnt: ub4;
begin
  Result := False;
  PrepareLastResultSetForReUse;
  { Prepares a statement. }
  Prepare;
  { logs the values }
  BindInParameters;

  if FStatementType = OCI_STMT_SELECT then begin
    { Executes the statement and gets a resultset. }
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
    Result := LastResultSet <> nil;
  end else begin
    { Executes the statement and gets a result. }
    Status := FPlainDriver.OCIStmtExecute(FOracleConnection.GetContextHandle,
        FOCIStmt, FOCIError, Max(1, ArrayCount), 0, nil, nil, CommitMode[Connection.GetAutoCommit]);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FOCIError, status, lcExecute, ASQL, ConSettings);
    Status := FPlainDriver.OCIAttrGet(FOCIStmt, OCI_HTYPE_STMT, @upCnt, nil,
      OCI_ATTR_ROW_COUNT, FOCIError);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FOCIError, status, lcExecute, ASQL, ConSettings);
    LastUpdateCount := upCnt;
  end;
  inherited ExecutePrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractOraclePreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  PrepareOpenResultSetForReUse;
  { Prepares a statement. }
  Prepare;
  BindInParameters; //log values

  { Executes the statement and gets a resultset. }
  Result := CreateResultSet;
  inherited ExecuteQueryPrepared; //log
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
function TZAbstractOraclePreparedStatement.ExecuteUpdatePrepared: Integer;
var
  ResultSet: IZResultSet;
  Status: sword;
  upCnt: ub4;
begin
  { Prepares a statement. }
  Prepare;

  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;

  BindInParameters;
  if FStatementType = OCI_STMT_SELECT then begin
    LastUpdateCount := -1;
    { Executes the statement and gets a resultset. }
    ResultSet := CreateResultSet;
    try
      while ResultSet.Next do;
      LastUpdateCount := ResultSet.GetRow;
    finally
      ResultSet.Close;
    end;
  end else begin
    { Executes the statement and gets a result. }
    Status := FPlainDriver.OCIStmtExecute(FOracleConnection.GetContextHandle,
        FOCIStmt, FOCIError, Max(1, ArrayCount), 0, nil, nil, CommitMode[Connection.GetAutoCommit]);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FOCIError, status, lcExecute, ASQL, ConSettings);
    FPlainDriver.OCIAttrGet(FOCIStmt, OCI_HTYPE_STMT, @upCnt, nil, OCI_ATTR_ROW_COUNT, FOCIError);
    LastUpdateCount := upCnt;
  end;
  Result := LastUpdateCount;
  inherited ExecuteUpdatePrepared;
end;

function TZAbstractOraclePreparedStatement.GetInParamLogValue(
  Index: Integer): RawByteString;
var
  Bind: PZOCIParamBind;
  TS: TZTimeStamp;
begin
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Bind.curelen > 1 then
    Result := '(Array)'
  else if Bind.indp[0] = -1 then
    Result := 'null'
  else case Bind.dty of
    SQLT_INT: if Bind.value_sz = SizeOf(Int64) then
                Result := IntToRaw(PInt64(Bind.valuep)^)
              else if Bind.value_sz = SizeOf(LongInt) then
                Result := IntToRaw(PLongInt(Bind.valuep)^)
              else
                Result := IntToRaw(PSmallInt(Bind.valuep)^);
    SQLT_UIN: if Bind.value_sz = SizeOf(UInt64) then
                Result := IntToRaw(PUInt64(Bind.valuep)^)
              else if Bind.value_sz = SizeOf(Cardinal) then
                Result := IntToRaw(PCardinal(Bind.valuep)^)
              else
                Result := IntToRaw(PWord(Bind.valuep)^);
    SQLT_FLT: if Bind.value_sz = SizeOf(Double) then
                Result := FloatToSqlRaw(PDouble(Bind.valuep)^)
              else
                Result := FloatToSqlRaw(PSingle(Bind.valuep)^);
    SQLT_DAT: Result := DateTimeToRawSQLDate(EncodeDate((POraDate(Bind.valuep).Cent-100)*100+(POraDate(Bind.valuep).Year-100),
      POraDate(Bind.valuep).Month,POraDate(Bind.valuep).Day), ConSettings.DisplayFormatSettings, True);
    SQLT_TIMESTAMP: begin
            FPlainDriver.OCIDateTimeGetDate(FOracleConnection.GetConnectionHandle, FOCIError,
              PPOCIDescriptor(Bind.valuep)^, PSB2(@TS.Year)^, PUB1(@TS.Month)^, PUB1(@Ts.Day)^);
            FPlainDriver.OCIDateTimeGetTime(FOracleConnection.GetConnectionHandle, FOCIError,
              PPOCIDescriptor(Bind.valuep)^, PUB1(@Ts.Hour)^, PUB1(@Ts.Minute)^, PUB1(@Ts.Second)^, Ts.Fractions);
            Result := DateTimeToRawSQLTimeStamp(EncodeDate(PSB2(@TS.Year)^, PUB1(@TS.Month)^, PUB1(@Ts.Day)^)+
              EncodeTime(PUB1(@Ts.Hour)^, PUB1(@Ts.Minute)^, PUB1(@Ts.Second)^, Ts.Fractions div 1000000), ConSettings.DisplayFormatSettings, True);
      end;
    SQLT_AFC: Result := SQLQuotedStr(Bind.valuep, Bind.Value_sz, #39);
    SQLT_VCS: ZSetString(Bind.valuep+SizeOf(SmallInt), PSmallInt(Bind.valuep)^, Result); //used for big (s/u) ordinals on old oracle
    SQLT_LVC: Result := SQLQuotedStr(Bind.valuep+SizeOf(Integer), PInteger(Bind.valuep)^, #39);
    SQLT_LVB: Result := GetSQLHexAnsiString(Bind.valuep+SizeOf(Integer), PInteger(Bind.valuep)^);
    SQLT_CLOB: Result := '(CLOB)';
    SQLT_BLOB: Result := '(BLOB)';
    else Result := 'unknown'
  end;
end;

function TZAbstractOraclePreparedStatement.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  I, C, N, FirstComposePos, ParamsCnt: Integer;
  Tokens: TZTokenList;
  Token: PZToken;
  {$IFNDEF UNICODE}
  tmp: RawByteString;
  List: TStrings;
  {$ENDIF}
  ComparePrefixTokens: TPreparablePrefixTokens;
  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(FCachedQueryRaw, Length(FCachedQueryRaw)+1);
    FCachedQueryRaw[High(FCachedQueryRaw)] := Value;
    SetLength(FIsParamIndex, Length(FCachedQueryRaw));
    FIsParamIndex[High(FIsParamIndex)] := Param;
    ToBuff(Value, Result);
  end;
  function IsNumeric(P, PEnd: PChar): Boolean;
  begin
    Result := P<= PEnd;
    repeat
      Result := Result and ((Ord(P^) >= Ord('0')) and (Ord(P^) <= Ord('9')));
      if not Result
      then Break
      else Inc(P);
    until P > PEnd;
  end;
begin
  Result := '';
  if (Length(FCachedQueryRaw) = 0) and (SQL <> '') then begin
    Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
    {$IFNDEF UNICODE}
    if ConSettings.AutoEncode
    then List := TStringList.Create
    else List := nil; //satisfy comiler
    {$ENDIF}
    try
      ComparePrefixTokens := OraPreparableTokens;
      N := -1;
      FTokenMatchIndex := -1;
      ParamsCnt := 0;
      FirstComposePos := 0;
      for I := 0 to Tokens.Count -1 do begin
        Token := Tokens[I];
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if Assigned(ComparePrefixTokens) and (Token.TokenType = ttWord) then
          if N = -1 then begin
            for C := 0 to high(ComparePrefixTokens) do
              if Tokens.IsEqual(i, ComparePrefixTokens[C].MatchingGroup, tcInsensitive) then begin
                if Length(ComparePrefixTokens[C].ChildMatches) = 0 then begin
                  FTokenMatchIndex := C;
                  ComparePrefixTokens := nil;
                end else
                  N := C; //save group
                Break;
              end;
            if N = -1 then //no sub-tokens ?
              ComparePrefixTokens := nil; //stop compare sequence
          end else begin //we already got a group
            FTokenMatchIndex := -1;
            for C := 0 to high(ComparePrefixTokens[N].ChildMatches) do
              if Tokens.IsEqual(i, ComparePrefixTokens[N].ChildMatches[C], tcInsensitive) then begin
                FTokenMatchIndex := N;
                Break;
              end;
            ComparePrefixTokens := nil; //stop compare sequence
          end;
        if ((Token.P^ = '?') and (Token.L = 1)) or
           ((Token.TokenType = ttWord) and (Token.P^ = ':') and (Token.L > 2) and
           (Ord((Token.P+1)^) or $20 = Ord('p')){lowercase 'P'} and IsNumeric(Token.P+2, Token.P+Token.L-2)) then begin
          Inc(ParamsCnt);
          {$IFDEF UNICODE}
          Add(ZUnicodeToRaw(Tokens.AsString(FirstComposePos, I-1), ConSettings^.ClientCodePage^.CP));
          if (Token.P^ = '?')
          then Add(':P'+IntToRaw(ParamsCnt), True)
          else Add(UnicodeStringToAscii7(Token.P, Token.L), True);
          {$ELSE}
          Add(Tokens.AsString(FirstComposePos, I-1));
          if (Token.P^ = '?')
          then Add(':P'+IntToRaw(ParamsCnt), True)
          else Add(TokenAsString(Token^), True);
          {$ENDIF}
          FirstComposePos := i + 1;
        end {$IFNDEF UNICODE}
        else if ConSettings.AutoEncode then
          case (Token.TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier: begin
              tmp := ConSettings^.ConvFuncs.ZStringToRaw(TokenAsString(Token^), ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
              Token^.P := Pointer(tmp);
              Token^.L := Length(tmp);
              List.Add(tmp); //keep alive
            end;
        end
        {$ENDIF};
      end;
      if (FirstComposePos <= Tokens.Count-1) then
        Add(ConSettings^.ConvFuncs.ZStringToRaw(Tokens.AsString(FirstComposePos, Tokens.Count -1), ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
      SetBindCapacity(ParamsCnt);
      FServerStmtCache := (FTokenMatchIndex > -1) and (FTokenMatchIndex < OCI_STMT_CREATE) and (ParamsCnt > 0);
    finally
      FlushBuff(Result);
      Tokens.Free;
      {$IFNDEF UNICODE}
      if ConSettings.AutoEncode then
        List.Free;
      {$ENDIF}
    end;
  end else
    Result := ASQL;
end;

procedure TZAbstractOraclePreparedStatement.InitBuffer(SQLType: TZSQLType;
  OCIBind: PZOCIParamBind; Index, ElementCnt: Cardinal; ActualLength: LengthInt);
var
  Status: sword;
  I: Integer;
begin
  { free Desciptors }
  if (OCIBind.DescriptorType <> 0) then begin
    if (OCIBind.DescriptorType <> SQLType2OCIDescriptor[SQLType]) then
      Status := 0
    else if (OCIBind.DescriptorType = SQLType2OCIDescriptor[SQLType]) and (ElementCnt < OCIBind.curelen) then
      Status := ElementCnt
    else Status := OCIBind.curelen;
    for I := OCIBind.curelen-1 downto Status do begin
      Status := FPlainDriver.OCIDescriptorFree(PPOCIDescriptor(PAnsiChar(OCIBind.valuep)+I*SizeOf(POCIDescriptor))^, OCIBind.DescriptorType);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcExecute, ASQL, ConSettings);
    end;
  end;

  OCIBind.DescriptorType := SQLType2OCIDescriptor[SQLType];
  OCIBind.dty := SQLType2OCIType[SQLType];

  {check if the parameter type was registered before -> they should be valid only }
  if (BindList[Index].ParamType <> zptUnknown) and (SQLType <> BindList[Index].SQLType) then
    raise EZSQLException.Create(SUnKnownParamDataType);
  case SQLType of
    stBoolean..stBigDecimal: begin
        if (SQLType in [stLong, stULong]) and not FCanBindInt64 then begin
          OCIBind.dty := SQLT_VCS;
          OCIBind.value_sz := 20+SizeOf(SmallInt); //-9,223,372,036,854,775,808 or UInt64: 18,446,744,073,709,551,615
        end else
          OCIBind.value_sz := SQLType2OCISize[SQLType];
      end;
    stString, stUnicodeString, stBytes: { 8 byte aligned buffer -> }
      OCIBind.value_sz := Max((((Max(Max(OCIBind.Precision, ActualLength)+SizeOf(Integer), SQLType2OCISize[SQLType])-1) shr 3)+1) shl 3, OCIBind.value_sz);
    else OCIBind.value_sz := SQLType2OCISize[SQLType];
  end;
  if ElementCnt = 1 then
    BindList[Index].SQLType := SQLType;
  if OCIBind.curelen <> ElementCnt then
    ReallocMem(OCIBind.indp, SizeOf(SB2)*ElementCnt); //alloc mem for indicators
  //alloc buffer space
  ReallocMem(OCIBind.valuep, OCIBind.value_sz*Integer(ElementCnt)*Ord(not ((ElementCnt > 1) and (Ord(SQLType) > Ord(stShort)) and (Ord(SQLType) < Ord(stCurrency)))));
  if (OCIBind.DescriptorType <> 0) then
    for I := OCIBind.curelen to ElementCnt -1 do begin
      { allocate lob/time oci descriptors }
      Status := FPlainDriver.OCIDescriptorAlloc(FOracleConnection.GetConnectionHandle,
          PPOCIDescriptor(OCIBind.valuep+I*SizeOf(POCIDescriptor))^, OCIBind.DescriptorType, 0, nil);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcExecute, ASQL, ConSettings);
    end;
  OCIBind.curelen := ElementCnt;
  { in array bindings we directly OCIBind the pointer of the dyn arrays instead of moving data}
  if not ((ElementCnt > 1) and (Ord(SQLType) > Ord(stShort)) and (Ord(SQLType) < Ord(stCurrency))) then begin
    Status := FPlainDriver.OCIBindByPos(FOCIStmt, OCIBind.bindpp, FOCIError, Index + 1,
      OCIBind.valuep, OCIBind.value_sz, OCIBind.dty, OCIBind.indp, nil, nil, 0, nil, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FOCIError, Status, lcExecute, ASQL, ConSettings);
  end;
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractOraclePreparedStatement.Prepare;
var
  Status: sword;
  Prefetch: ub4;
begin
  if not Prepared then begin
    // we need a errorhandle per stmt
    if (FOCIError = nil) then begin
      Status := FPlainDriver.OCIHandleAlloc(FOracleConnection.GetConnectionHandle,
        FOCIError, OCI_HTYPE_ERROR, 0, nil);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcOther, 'OCIHandleAlloc(OCIError-Handle)', ConSettings);
    end;

    if (FOCIStmt = nil) then begin
      if FServerStmtCache then begin
        //check if query is found in the server cache
        Status := FPlainDriver.OCIStmtPrepare2(FOracleConnection.GetContextHandle,
          FOCIStmt, FOCIError, Pointer(FASQL), Length(FASQL)+1,nil,0,OCI_NTV_SYNTAX,
            OCI_PREP2_CACHE_SEARCHONLY);
        if Status <> OCI_SUCCESS then //not found! Create new handle in server cache
          Status := FPlainDriver.OCIStmtPrepare2(FOracleConnection.GetContextHandle,
            FOCIStmt, FOCIError, Pointer(FASQL), Length(FASQL)+1,nil,0,OCI_NTV_SYNTAX,
              OCI_DEFAULT);
      end else begin
        Status := FPlainDriver.OCIHandleAlloc(FOracleConnection.GetConnectionHandle,
          FOCIStmt, OCI_HTYPE_STMT, 0, nil);
        if Status <> OCI_SUCCESS then
          CheckOracleError(FPlainDriver, FOCIError, Status, lcOther, 'OCIHandleAlloc(OCIStmt-Handle)', ConSettings);
        Status := FPlainDriver.OCIStmtPrepare(FOCIStmt, FOCIError, Pointer(FASQL),
          Length(FASQL)+1, OCI_NTV_SYNTAX, OCI_DEFAULT);
      end;
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcPrepStmt, 'prepare statement', ConSettings);
    end;
    { get statement type }
    Status := FPlainDriver.OCIAttrGet(FOCIStmt, OCI_HTYPE_STMT, @FStatementType,
      nil, OCI_ATTR_STMT_TYPE, FOCIError);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FOCIError, Status, lcOther, 'OCIAttrGet(OCI_ATTR_STMT_TYPE)', ConSettings);
    if FStatementType = OCI_STMT_SELECT then begin
      //set prefetch by memory! not by Rows!
      Prefetch := 0;
      Status := FPlainDriver.OCIAttrSet(FOCIStmt,OCI_HTYPE_STMT, @Prefetch ,0, OCI_ATTR_PREFETCH_ROWS,FOCIError);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcOther, 'OCIAttrSet(OCI_ATTR_PREFETCH_ROWS)', ConSettings);
      Prefetch := FRowPrefetchMemory;
      Status := FPlainDriver.OCIAttrSet(FOCIStmt,OCI_HTYPE_STMT,@Prefetch,0,OCI_ATTR_PREFETCH_MEMORY,FOCIError);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcOther, 'OCIAttrSet(OCI_ATTR_PREFETCH_MEMORY)', ConSettings);
    end;
    inherited Prepare;
  end;
end;

procedure TZAbstractOraclePreparedStatement.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZParamType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
var
  Bind: PZOCIParamBind;
begin
  inherited RegisterParameter(ParameterIndex, SQLType, ParamType, Name,
    PrecisionOrSize, Scale);
  {$R-}
  Bind := @FOraVariables[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Bind.Precision := PrecisionOrSize;
  Bind.Scale := Scale;
end;

{**
  Sets a new parameter capacity and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractOraclePreparedStatement.SetBindCapacity(Capacity: Integer);
var
  OldCapacity, I, J: Integer;
  Bind: PZOCIParamBind;
begin
  OldCapacity := BindList.Capacity;
  inherited SetBindCapacity(Capacity);
  if OldCapacity <> Capacity then begin
    for I := OldCapacity-1 downto Capacity do begin
      {$R-}
      Bind := @FOraVariables[I];
      {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
      if Bind.DescriptorType <> 0 then //deallocate the descriptors
        for J := 0 to Bind.curelen-1 do begin
          FPlainDriver.OCIDescriptorFree(PPOCIDescriptor(PAnsiChar(Bind.valuep)+J*SizeOf(POCIDescriptor))^, Bind.DescriptorType);
        end;
      if Bind.valuep <> nil then
        FreeMem(Bind.valuep, Bind.value_sz*Integer(Bind.curelen));
      if Bind.indp <> nil then
        FreeMem(Bind.indp, SizeOf(SB2)*Bind.curelen);
    end;
    ReallocMem(FOraVariables, Capacity * SizeOf(TZOCIParamBind));
    if FOraVariables <> nil then
      FillChar((PAnsichar(FOraVariables)+(OldCapacity*SizeOf(TZOCIParamBind)))^,
        (Capacity-OldCapacity)*SizeOf(TZOCIParamBind), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  end;
end;

procedure TZAbstractOraclePreparedStatement.SetDataArray(
  ParameterIndex: Integer; const Value; const SQLType: TZSQLType;
  const VariantType: TZVariantType);
var
  ClientStrings: TRawByteStringDynArray; //conversion buffer
  Bind: PZOCIParamBind; //ora bind variable
  BufferSize, I: Integer;
  ArrayLen: Cardinal;
  ClientCP: Word;
  Lob: IZBLob;
  P: PAnsiChar;
  TS: TZTimeStamp;
  Status: sword;
  WriteTempBlob: IZOracleBlob;
  OraDate: POraDate;
label set_raw, from_raw, bind_direct;
begin
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  {$R-}
  Bind := @FOraVariables[ParameterIndex];
  ClientCP := ConSettings^.ClientCodePage.CP;
  ClientStrings := nil;
  ArrayLen := {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
  case SQLType of
    stBoolean: begin //Oracle doesn't support inparam boolean types so lets use integers and OCI converts it..
        if (Bind.dty <> SQLT_INT) or (Bind.value_sz <> SizeOf(LongInt)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(LongInt));
        for i := 0 to ArrayLen -1 do PLongInt(Bind.valuep+I*SizeOf(LongInt))^ := Ord(TBooleanDynArray(Value)[i]);
      end;
    stByte: begin //Oracle doesn't support byte types so lets use integers and OCI converts it..
        if (Bind.dty <> SQLT_UIN) or (Bind.value_sz <> SizeOf(Word)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen);
        for i := 0 to ArrayLen -1 do PWord(Bind.valuep+I*SizeOf(Word))^ := TByteDynArray(Value)[i];
      end;
    stShort: begin //Oracle doesn't support shortint types so lets use integers and OCI converts it..
        if (Bind.dty <> SQLT_INT) or (Bind.value_sz <> SizeOf(SmallInt)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen);
        for i := 0 to ArrayLen -1 do PSmallInt(Bind.valuep+I*SizeOf(SmallInt))^ := TShortIntDynArray(Value)[i];
      end;
    stWord, stSmall, stLongWord, stInteger, stFloat, stDouble: begin
bind_direct:
        InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen);
        Status := FPlainDriver.OCIBindByPos(FOCIStmt, Bind.bindpp, FOCIError, ParameterIndex + 1,
          Pointer(Value), Bind.value_sz, Bind.dty, Bind.indp, nil, nil, 0, nil, OCI_DEFAULT);
        if Status <> OCI_SUCCESS then
          CheckOracleError(FPlainDriver, FOCIError, Status, lcExecute, ASQL, ConSettings);
      end;
    stLong, stULong: //old oracle does not support 8 byte ordinals
        if FCanBindInt64 then
          goto bind_direct
        else begin
          if (Bind.dty <> SQLT_VCS) or (Bind.value_sz <> 20) or (Bind.curelen < ArrayLen) then
            InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, 20);
          for i := 0 to ArrayLen -1 do begin
            if SQLType = stLong
            then FRawTemp := IntToRaw(TInt64DynArray(Value)[i])
            else FRawTemp := IntToRaw(TUInt64DynArray(Value)[i]);
            BufferSize := Length(FRawTemp){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
            PSmallInt(Bind.valuep+i*20)^ := BufferSize;
            Move(Pointer(FRawTemp)^, (Bind.valuep+I*20+SizeOf(SmallInt))^, BufferSize);
          end;
        end;
    stCurrency: begin
        if (Bind.dty <> SQLT_FLT) or (Bind.value_sz <> SizeOf(Double)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(Double));
        for i := 0 to ArrayLen -1 do PDouble(Bind.valuep+I*SizeOf(Double))^ := TCurrencyDynArray(Value)[i];
      end;
    stBigDecimal: begin
        if (Bind.dty <> SQLT_FLT) or (Bind.value_sz <> SizeOf(Double)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(Double));
        for i := 0 to ArrayLen -1 do PDouble(Bind.valuep+I*SizeOf(Double))^ := TExtendedDynArray(Value)[i];
      end;
    stDate: begin
        if (Bind.dty <> SQLT_DAT) or (Bind.value_sz <> SizeOf(TOraDate)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(TOraDate));
        for i := 0 to ArrayLen -1 do begin
          DecodeDate(TDateTimeDynArray(Value)[i], TS.Year, TS.Month, TS.Day);
          OraDate := POraDate(Bind^.valuep+I*SizeOf(TOraDate));
          OraDate.Cent := Ts.Year div 100 + 100;
          OraDate.Year := Ts.Year mod 100 + 100;
          POraDate(Bind^.valuep).Month := TS.Month;
          PLongInt(@POraDate(Bind^.valuep).Day)^ := 0; //init all remaining fields to 0 with one 4Byte value
          POraDate(Bind^.valuep).Day    := TS.Day;
        end;
      end;
    stTime, stTimeStamp: begin //msec precision -> need a descriptor
        if (Bind.dty <> SQLT_TIMESTAMP) or (Bind.value_sz <> SizeOf(POCIDescriptor)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(SizeOf(POCIDescriptor)));
        for i := 0 to ArrayLen -1 do begin
          DecodeDate(TDateTimeDynArray(Value)[i], TS.Year, TS.Month, TS.Day); //oracle doesn't accept 0 dates
          DecodeTime(TDateTimeDynArray(Value)[i], TS.Hour, TS.Minute, TS.Second, PWord(@TS.Fractions)^);
          TS.Fractions := PWord(@TS.Fractions)^ * 1000000;
          Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
              FOCIError, PPOCIDescriptor(Bind^.valuep+I*SizeOf(POCIDescriptor))^, //direct addressing descriptor to array. So we don't need to free the mem again
              TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions, nil, 0);
          if Status <> OCI_SUCCESS then
            CheckOracleError(FPlainDriver, FOCIError, Status, lcOther, 'OCIDateTimeConstruct', ConSettings);
        end;
      end;
    stGUID: begin
        if (Bind.dty <> SQLT_AFC) or (Bind.value_sz <> StrGUIDLen) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen);
        for i := 0 to ArrayLen -1 do
          GUIDToBuffer(@TGUIDDynArray(Value)[I], (Bind.valuep+I*StrGUIDLen), []);
      end;
    stBytes: begin
        BufferSize := 0;
        for i := 0 to ArrayLen -1 do
          BufferSize := Max(BufferSize, Length(TBytesDynArray(Value)[I]));
        if (Bind.dty <> SQLT_LVB) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
        for i := 0 to ArrayLen -1 do begin
          BufferSize := Length(TBytesDynArray(Value)[I]);
          PInteger(Bind.valuep+I*Bind.value_sz)^ := BufferSize;
          if BufferSize > 0 then
            Move(Pointer(TBytesDynArray(Value)[I])^,(Bind.valuep+I*Bind.value_sz+SizeOf(Integer))^, BufferSize);
        end;
    end;
    stString, stUnicodeString: begin
      BufferSize := 0;
      case VariantType of
        vtString{$IFDEF UNICODE}, vtUnicodeString{$ENDIF}:
          {$IFNDEF UNICODE}
          if not ConSettings.AutoEncode then
            goto from_raw
          else {$ENDIF} begin
            SetLength(ClientStrings, ArrayLen);
            for i := 0 to ArrayLen -1 do
              if (Pointer(TStringDynArray(Value)[I]) <> nil) then begin
                {$IFDEF UNICODE}
                ClientStrings[i] := ZUnicodeToRaw(TStringDynArray(Value)[I], ClientCP);
                {$ELSE}
                ClientStrings[i] := ConSettings^.ConvFuncs.ZStringToRaw(TStringDynArray(Value)[I], ConSettings^.CTRL_CP, ClientCP);
                {$ENDIF}
                BufferSize := Max(BufferSize, Length(ClientStrings[i]));
              end else
                ClientStrings[i] := '';
            goto set_raw;
          end;
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString: begin
            if ZCompatibleCodePages(ClientCP, ZOSCodePage) then
              goto from_raw
            else begin
              SetLength(ClientStrings, ArrayLen);
              BufferSize := 0;
              for i := 0 to ArrayLen -1 do
                if (Pointer(TAnsiStringDynArray(Value)[I]) <> nil) then begin
                  FUniTemp := PRawToUnicode(Pointer(TAnsiStringDynArray(Value)[I]), Length(TAnsiStringDynArray(Value)[I]), ZOSCodePage);
                  ClientStrings[I] := PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), ClientCP);
                  BufferSize := Max(BufferSize, Length(ClientStrings[i]));
                end else
                  ClientStrings[i] := '';
            end;
            goto set_raw;
          end;
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String: begin
          if ZCompatibleCodePages(ClientCP, zCP_UTF8) then
            goto from_raw
          else begin
            SetLength(ClientStrings, ArrayLen);
            for i := 0 to ArrayLen -1 do
              if (Pointer(TUTF8StringDynArray(Value)[I]) <> nil) then begin
                FUniTemp := PRawToUnicode(Pointer(TUTF8StringDynArray(Value)[I]), Length(TUTF8StringDynArray(Value)[I]), zCP_UTF8);
                ClientStrings[I] := PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), ClientCP);
                BufferSize := Max(BufferSize, Length(ClientStrings[i]));
              end else
                ClientStrings[i] := '';
          end;
          goto set_raw;
        end;
        {$ENDIF}
        vtRawByteString: begin
from_raw:   ClientStrings := TRawByteStringDynArray(Value);
            for i := 0 to ArrayLen -1 do
              if Pointer(ClientStrings[i]) <> nil then
                {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
                BufferSize := Max(BufferSize, Length(ClientStrings[I]) -1);
                {$ELSE}
                BufferSize := Max(BufferSize, PLengthInt(NativeUInt(ClientStrings[I]) - StringLenOffSet)^);
                {$ENDIF}
set_raw:    if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen < ArrayLen) then
              InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
            for i := 0 to ArrayLen -1 do begin
              if (Pointer(ClientStrings[I]) = nil) then
                PInteger(Bind.valuep+I*Bind.value_sz)^ := 0
              else begin
                {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
                BufferSize := Length(ClientStrings[I]) -1;
                {$ELSE}
                BufferSize := PLengthInt(NativeUInt(ClientStrings[I]) - StringLenOffSet)^;
                {$ENDIF}
                PInteger(Bind.valuep+I*Bind.value_sz)^ := BufferSize;
                Move(Pointer(ClientStrings[i])^,(Bind.valuep+I*Bind.value_sz+SizeOf(Integer))^, BufferSize);
              end;
            end;
          end;
        vtCharRec: begin
            {in array bindings we assume all codepages are equal!}
            if ZCompatibleCodePages(TZCharRecDynArray(Value)[0].CP, ClientCP) then begin
              for i := 0 to ArrayLen -1 do
                BufferSize := Max(BufferSize, TZCharRecDynArray(Value)[i].Len);
              if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen < ArrayLen) then
                InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
              for i := 0 to ArrayLen -1 do begin
                PInteger(Bind.valuep+I*Bind.value_sz)^ := TZCharRecDynArray(Value)[i].Len;
                if (TZCharRecDynArray(Value)[i].P <> nil) and (TZCharRecDynArray(Value)[i].Len <> 0) then
                  Move(TZCharRecDynArray(Value)[i].P^,(Bind.valuep+I*Bind.value_sz+SizeOf(Integer))^, TZCharRecDynArray(Value)[i].Len);
              end;
            end else begin
              SetLength(ClientStrings, ArrayLen);
              if ZCompatibleCodePages(TZCharRecDynArray(Value)[0].CP, zCP_UTF16) then
                for I := 0 to ArrayLen -1 do begin
                  ClientStrings[I] := PUnicodeToRaw(TZCharRecDynArray(Value)[I].P, TZCharRecDynArray(Value)[I].Len, ClientCP);
                  BufferSize := Max(BufferSize, Length(ClientStrings[I]));
                end
              else
                for I := 0 to ArrayLen -1 do begin
                  FUniTemp := PRawToUnicode(TZCharRecDynArray(Value)[I].P, TZCharRecDynArray(Value)[I].Len, TZCharRecDynArray(Value)[I].CP);
                  ClientStrings[I] := PUnicodeToRaw(Pointer(FUniTemp), Length(FUniTemp), ClientCP);
                  BufferSize := Max(BufferSize, Length(ClientStrings[I]));
                end;
              goto set_raw;
            end;
          end;
        {$IFNDEF UNICODE}
        vtUnicodeString: begin
            SetLength(ClientStrings, ArrayLen);
            for I := 0 to ArrayLen -1 do begin
              ClientStrings[I] := PUnicodeToRaw(Pointer(TUnicodeStringDynArray(Value)[I]), Length(TUnicodeStringDynArray(Value)[I]), ClientCP);
              BufferSize := Max(BufferSize, Length(ClientStrings[I]));
            end;
            goto set_raw;
          end;
        {$ENDIF}
        else
          raise Exception.Create('Unsupported String Variant');
      end;
    end;
    stAsciiStream, stUnicodeStream: begin
        if (Bind.dty <> SQLT_CLOB) or (Bind.value_sz <> SizeOf(POCIDescriptor)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(POCIDescriptor));
        for i := 0 to ArrayLen -1 do
          if (TInterfaceDynArray(Value)[I] <> nil) and Supports(TInterfaceDynArray(Value)[I], IZBlob, Lob) and not Lob.IsEmpty then begin
            if Lob.IsClob then begin
              P := Lob.GetPAnsiChar(ClientCP);
              BufferSize := Lob.Length;
            end else begin
              FRawTemp := GetValidatedAnsiStringFromBuffer(Lob.GetBuffer, lob.Length, Connection.GetConSettings);
              P := Pointer(FRawTemp);
              BufferSize := Length(FRawTemp);
            end;
            WriteTempBlob := TZOracleClob.Create(FPlainDriver,
              nil, 0, FOracleConnection.GetConnectionHandle,
              FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
              PPOCIDescriptor(Bind^.valuep+I*SizeOf(POCIDescriptor))^,
              ChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
            WriteTempBlob.CreateBlob;
            WriteTempBlob.WriteLobFromBuffer(P, BufferSize);
            TInterfaceDynArray(Value)[I] := WriteTempBlob;
            Bind.indp[i] := 0;
          end else
            Bind.indp[i] := -1;
        Exit;
      end;
    stBinaryStream: begin
        if (Bind.dty <> SQLT_BLOB) or (Bind.value_sz <> SizeOf(POCIDescriptor)) or (Bind.curelen < ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(POCIDescriptor));
        for i := 0 to ArrayLen -1 do
          if (TInterfaceDynArray(Value)[I] <> nil) and Supports(TInterfaceDynArray(Value)[I], IZBlob, Lob) and not Lob.IsEmpty then begin
            WriteTempBlob := TZOracleBlob.Create(FPlainDriver,
              nil, 0, FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
              PPOCIDescriptor(Bind^.valuep+I*SizeOf(POCIDescriptor))^, ChunkSize, ConSettings);
            WriteTempBlob.CreateBlob;
            WriteTempBlob.WriteLobFromBuffer(Lob.GetBuffer, Lob.Length);
            TInterfaceDynArray(Value)[I] := WriteTempBlob;
            Bind.indp[i] := 0;
          end else
            Bind.indp[i] := -1;
        Exit;
      end;
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  FillChar(Bind.indp^, SizeOf(SB2)*ArrayLen, #0);
end;

procedure TZAbstractOraclePreparedStatement.SetNullArray(
  ParameterIndex: Integer; const SQLType: TZSQLType; const Value;
  const VariantType: TZVariantType = vtNull);
var I: Cardinal;
  Bind: PZOCIParamBind;
begin
  inherited SetNullArray(ParameterIndex, SQLType, Value, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  {$R-}
  Bind := @FOraVariables[ParameterIndex];
  for i := 0 to ArrayCount -1 do
    Bind.indp[I] := -Ord(ZDbcUtils.IsNullFromArray(BindList[ParameterIndex].Value, i));
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

procedure TZAbstractOraclePreparedStatement.Unprepare;
var Status: sword;
begin
  try
    inherited Unprepare;
  finally
    if FOCIStmt <> nil then begin
      if FServerStmtCache
      then Status := FPlainDriver.OCIStmtRelease(FOCIStmt, FOCIError, nil, 0, OCI_STMTCACHE_DELETE)
      else Status := FPlainDriver.OCIHandleFree(FOCIStmt, OCI_HTYPE_STMT);
      FOCIStmt := nil;
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcUnprepStmt, ASQL, ConSettings)
    end;
    if FOCIError <> nil then begin
      Status := FPlainDriver.OCIHandleFree(FOCIError, OCI_HTYPE_ERROR);
      FOCIError := nil;
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FOCIError, Status, lcExecute, ASQL, ConSettings)
    end;
  end;
end;

{ TZOracleStatement }

constructor TZOracleStatement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

initialization

{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
SetLength(OraPreparableTokens, OCI_STMT_DECLARE);
OraPreparableTokens[OCI_STMT_SELECT-1].MatchingGroup  := 'SELECT';
OraPreparableTokens[OCI_STMT_UPDATE-1].MatchingGroup  := 'UPDATE';
OraPreparableTokens[OCI_STMT_DELETE-1].MatchingGroup  := 'DELETE';
OraPreparableTokens[OCI_STMT_INSERT-1].MatchingGroup  := 'INSERT';
OraPreparableTokens[OCI_STMT_CREATE-1].MatchingGroup  := 'CREATE';
OraPreparableTokens[OCI_STMT_DROP-1].MatchingGroup    := 'DROP';
OraPreparableTokens[OCI_STMT_ALTER-1].MatchingGroup   := 'ALTER';
OraPreparableTokens[OCI_STMT_BEGIN-1].MatchingGroup   := 'BEGIN';
OraPreparableTokens[OCI_STMT_DECLARE-1].MatchingGroup := 'DECLARE';

end.
