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
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainOracleDriver,
  ZCompatibility, ZVariant, ZDbcOracleUtils, ZPlainOracleConstants;

type

  {** Implements Prepared SQL Statement. }

  { TZOraclePreparedStatement }
  TZOraclePreparedStatement = class(TZAbstractPreparedStatement)
  private
    FHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FInVars: PZSQLVars;
    FPrefetchCount: Integer;
    FStatementType: ub2;
    FConnectionHandle: POCIEnv;
    function ConvertToOracleSQLQuery: RawByteString;
    function CreateResultSet: IZResultSet;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    property InVars: PZSQLVars read FInVars write FInVars;
  public
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; const SQL: string; Info: TStrings); overload;
    constructor Create(PlainDriver: IZOraclePlainDriver;
      Connection: IZConnection; Info: TStrings); overload;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;
  TZOracleStatement = class(TZAbstractPreparedStatement);


  TZOracleCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FOutParamCount: Integer;
    FErrorHandle: POCIError;
    FInVars: PZSQLVars;
    FPlainDriver:IZOraclePlainDriver;
    FPrepared:boolean;
    FHandle: POCIStmt;
    FOracleParams: TZOracleParams;
    FOracleParamsCount: Integer;
    FParamNames: TStringDynArray;
    PackageIncludedList: TStrings;
    FPrefetchCount: Integer;
    FConnectionHandle: POCIEnv;
    procedure ArrangeInParams;
    procedure FetchOutParamsFromOracleVars;
  protected
    function GetProcedureSql(SelectProc: boolean): RawByteString;
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); override;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      ParamTypeName: String; const ParamName: String; Const {%H-}ColumnSize, {%H-}Precision: Integer);
  public
    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer); override;
    procedure RegisterParamType(ParameterIndex: integer; ParamType: Integer); override;
    procedure Prepare; override;
    function IsNull(ParameterIndex: Integer): Boolean;override;

    Function ExecuteUpdatePrepared: Integer; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    constructor Create(Connection: IZConnection; const pProcName: string; Info: TStrings);
    destructor Destroy; override;
    procedure ClearParameters; override;
  end;

implementation

uses
  ZFastCode, ZTokenizer, ZDbcOracle, ZDbcOracleResultSet
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IFDEF UNICODE}, ZEncoding{$ENDIF}, ZDbcUtils;

{ TZOraclePreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZOraclePreparedStatement.Create(
  PlainDriver: IZOraclePlainDriver; Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
  ASQL := ConvertToOracleSQLQuery;
  FPrefetchCount := StrToIntDef(ZDbcUtils.DefineStatementParameter(Self, 'prefetch_count', '1000'), 1000);
  FConnectionHandle := (Connection as IZOracleConnection).GetConnectionHandle;
end;

constructor TZOraclePreparedStatement.Create(PlainDriver: IZOraclePlainDriver;
  Connection: IZConnection; Info: TStrings);
begin
  Create(PlainDriver, Connection, '', Info);
end;

{**
  Converts an SQL query into Oracle format.
  @param SQL a query with parameters defined with '?'
  @returns a query with parameters in Oracle format ':pN'.
}
function TZOraclePreparedStatement.ConvertToOracleSQLQuery: RawByteString;
var
  I, N: Integer;
begin
  N := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[i] then
    begin
      Inc(N);
      Result := Result + ':P' + IntToRaw(N);
    end else
      Result := Result + CachedQueryRaw[i];
  {$IFNDEF UNICODE}
  if ConSettings^.AutoEncode then
     Result := GetConnection.GetDriver.GetTokenizer.GetEscapeString(Result);
  {$ENDIF}
end;

function TZOraclePreparedStatement.CreateResultSet: IZResultSet;
begin
  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
  Result := CreateOracleResultSet(FPlainDriver, Self, SQL, FHandle, FErrorHandle);
  FOpenResultSet := Pointer(Result);
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZOraclePreparedStatement.PrepareInParameters;
begin
  AllocateOracleSQLVars(FInVars, InParamCount);
end;

{**
  Binds the input parameters
}
procedure TZOraclePreparedStatement.BindInParameters;
var
  I: Integer;
  CurrentVar: PZSQLVar;
  Status: Integer;
  CharRec: TZCharRec;
begin
    for I := 0 to InParamCount - 1 do
    begin
      CurrentVar := @FInVars.Variables[I + 1];
      CurrentVar.Handle := nil;

      { Artificially define Oracle internal type. }
      if InParamTypes[I] in [stBytes, stBinaryStream] then
        InitializeOracleVar(FPlainDriver, FConnectionHandle, CurrentVar,
          InParamTypes[I], SQLT_BLOB, Max_OCI_String_Size)
      else if InParamTypes[I] in [stAsciiStream, stUnicodeStream] then
        InitializeOracleVar(FPlainDriver, FConnectionHandle, CurrentVar,
          InParamTypes[I], SQLT_CLOB, Max_OCI_String_Size)
      else if InParamTypes[I] in [stString, stUnicodeString] then
      begin
        CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], ConSettings^.ClientCodePage^.CP);
        //(pl/sql statement) possible out params!!
        //so take care to use a new bidirectional memory addressation
        InitializeOracleVar(FPlainDriver, FConnectionHandle, CurrentVar,
          InParamTypes[I], SQLT_STR, Max_OCI_String_Size, (CharRec.Len > Max_OCI_String_Size) or (FStatementType in [OCI_STMT_BEGIN, OCI_STMT_DECLARE]));
      end
      else
        InitializeOracleVar(FPlainDriver, FConnectionHandle, CurrentVar,
          InParamTypes[I], SQLT_STR, Max_OCI_String_Size);

      if CurrentVar.FreeMem then //point to allocated mem ?
        Status := FPlainDriver.BindByPos(FHandle, CurrentVar.BindHandle,
          FErrorHandle, I + 1, CurrentVar.Data, CurrentVar.Length,
          CurrentVar.TypeCode, @CurrentVar.Indicator, nil, nil, 0, nil, OCI_DEFAULT)
      else //nope, just to local data
        if InParamTypes[I] in [stString, stUnicodeString] then
        begin //point to data of string
          CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], ConSettings^.ClientCodePage^.CP);
          CurrentVar.Data := CharRec.P;
          Status := FPlainDriver.BindByPos(FHandle, CurrentVar.BindHandle,
            FErrorHandle, I + 1, CharRec.P, CharRec.Len,
            CurrentVar.TypeCode, @CurrentVar.Indicator, @CurrentVar.DataSize,
            nil, 0, nil, OCI_DEFAULT);
          //set datasize, so oracle can move data instead of use StrPCopy
          CurrentVar.DataSize := CharRec.Len+1; //include trailing #0
        end //currently not handled.. just make compiler happy
        else
          Status := FPlainDriver.BindByPos(FHandle, CurrentVar.BindHandle,
            FErrorHandle, I + 1, CurrentVar.Data, CurrentVar.Length,
            CurrentVar.TypeCode, @CurrentVar.Indicator, nil, nil, 0, nil,
            OCI_DEFAULT);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, ASQL, ConSettings);
    end;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver, Connection, FErrorHandle,
    FInVars, InParamValues,ChunkSize);

  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOraclePreparedStatement.UnPrepareInParameters;
begin
  FreeOracleSQLVars(FPlainDriver, FInVars, FConnectionHandle, FErrorHandle, ConSettings);
end;

{**
  Prepares an SQL statement
}
procedure TZOraclePreparedStatement.Prepare;
begin
  if not Prepared then
  begin
    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    { prepare stmt on server side }
    PrepareOracleStatement(FPlainDriver, ASQL, FHandle, FErrorHandle,
      FPrefetchCount, ConSettings);
    { get Statemant type }
    FPlainDriver.AttrGet(FHandle, OCI_HTYPE_STMT, @FStatementType, nil,
      OCI_ATTR_STMT_TYPE, FErrorHandle);
    inherited Prepare;
  end;
end;

procedure TZOraclePreparedStatement.UnPrepare;
begin
  FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  inherited Unprepare;
end;



{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;

  { Prepares a statement. }
  Prepare;

  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;

  BindInParameters;

  if FStatementType = OCI_STMT_SELECT then
  begin
    { Executes the statement and gets a resultset. }
    LastResultSet := CreateResultSet;
    Result := LastResultSet <> nil;
  end
  else
  begin
    { Executes the statement and gets a result. }
    ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
      ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
  end;
  inherited ExecutePrepared;

  { Unloads binded variables with values. }
  UnloadOracleVars(FInVars);

  { Autocommit statement. done by ExecuteOracleStatement}
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;
  BindInParameters;

  { Executes the statement and gets a resultset. }
  Result := CreateResultSet;
  inherited ExecuteQueryPrepared;

  { Unloads binded variables with values. }
  UnloadOracleVars(FInVars);
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
function TZOraclePreparedStatement.ExecuteUpdatePrepared: Integer;
var
  ResultSet: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;

  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;

  BindInParameters;
  try
    if FStatementType = OCI_STMT_SELECT then
    begin
      Result := -1;

      { Executes the statement and gets a resultset. }
      ResultSet := CreateResultSet;
      try
        while ResultSet.Next do;
        LastUpdateCount := ResultSet.GetRow;
      finally
        ResultSet.Close;
      end;
    end
    else
    begin
      { Executes the statement and gets a result. }
      ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
        ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit);
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    end;
    Result := LastUpdateCount;
    inherited ExecuteUpdatePrepared;
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FInVars);
  end;

  { Autocommit statement. done by ExecuteOracleStatement}
end;

procedure TZOracleCallableStatement.Prepare;
var
  I: Integer;
  TypeCode: ub2;
  CurrentVar: PZSQLVar;
  SQLType:TZSQLType;
begin
  if not FPrepared then
  begin
    ArrangeInParams; //need to sort ReturnValues for functions
    ASQL := GetProcedureSql(False);
    SetLength(FParamNames, FOracleParamsCount);
    for i := 0 to FOracleParamsCount -1 do
      FParamNames[I] := Self.FOracleParams[I].pName;

    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
    begin
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    end;

    PrepareOracleStatement(FPlainDriver, ASQL, FHandle, FErrorHandle,
          FPrefetchCount, ConSettings);
    //make sure eventual old buffers are cleaned
    FreeOracleSQLVars(FPlainDriver, FInVars, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings);
    AllocateOracleSQLVars(FInVars, FOracleParamsCount);

    for I := 0 to FOracleParamsCount - 1 do
    begin
      CurrentVar := @FInVars.Variables[I + 1];
      CurrentVar.Handle := nil;
      SQLType := TZSQLType(FOracleParams[I].pSQLType);

    { Artificially define Oracle internal type. }
      if SQLType = stBinaryStream then
        TypeCode := SQLT_BLOB
      else if SQLType in [stAsciiStream, stUnicodeStream] then
        TypeCode := SQLT_CLOB
      else TypeCode := SQLT_STR;

      InitializeOracleVar(FPlainDriver, FConnectionHandle, CurrentVar,
        SQLType, TypeCode, Max_OCI_String_Size);

      if SQLType in [stString, stUnicodeString] then
        CheckOracleError(FPlainDriver, FErrorHandle, FPlainDriver.BindByPos(
          FHandle, CurrentVar.BindHandle, FErrorHandle, I + 1, CurrentVar.Data,
          CurrentVar.Length, CurrentVar.TypeCode, @CurrentVar.Indicator,
          @CurrentVar.DataSize, nil, 0, nil, OCI_DEFAULT), lcExecute,
          'OCIBindByPos', ConSettings)
      else
        CheckOracleError(FPlainDriver, FErrorHandle, FPlainDriver.BindByPos(
          FHandle, CurrentVar.BindHandle, FErrorHandle, I + 1, CurrentVar.Data,
          CurrentVar.Length, CurrentVar.TypeCode, @CurrentVar.Indicator, nil,
          nil, 0, nil, OCI_DEFAULT), lcExecute, 'OCIBindByPos', ConSettings);
    end;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  end;
end;


procedure TZOracleCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  inherited RegisterOutParameter(ParameterIndex,SQLType);
  with FOracleParams[ParameterIndex-1] do
  begin
    if not GetConnection.UseMetadata then
      pName := 'pOut'+ZFastCode.IntToStr(ParameterIndex);
    pSQLType := SQLType;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamType(ParameterIndex: integer;
  ParamType: Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex);
  if ParameterIndex > FOracleParamsCount then
    FOracleParamsCount := ParameterIndex;
  FOracleParams[ParameterIndex-1].pType := ParamType;
  FOracleParams[ParameterIndex-1].pParamIndex := ParameterIndex;
  if ParamType in [2,3,4] then //ptInOut, ptOut, ptResult
  begin
    Inc(FOutParamCount);
    FOracleParams[ParameterIndex-1].pOutIndex := FOutParamCount;
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
      pName := 'p'+ZFastCode.IntToStr(ParameterIndex);
    pSQLType := ord(SQLType);
    pValue := Value;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamTypeAndName(const ParameterIndex: integer;
  ParamTypeName: String; const ParamName: String; Const ColumnSize, Precision: Integer);
var
  iPos: Integer;
  ProcName: String;
begin
  FOracleParams[ParameterIndex].pName := ParamName;
  FOracleParams[ParameterIndex].pTypeName := ParamTypeName;
  iPos := Pos('.', ParamName);
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

procedure TZOracleCallableStatement.ArrangeInParams;
var
  I, J, NewProcIndex, StartProcIndex: Integer;
  TempVars: TZVariantDynArray;
  TempOraVar: TZOracleParam;
begin
  NewProcIndex := -1;
  StartProcIndex := 0;
  if IsFunction then
  begin
    for i := 0 to high(FOracleParams) do
    begin
      if not ( FOracleParams[i].pProcIndex = NewProcIndex ) then
      begin
        NewProcIndex := FOracleParams[i].pProcIndex;
        StartProcIndex := I;
      end;
      if ( FOracleParams[i].pType = 4 ) then
      begin
        ClientVarManager.SetNull(FOracleParams[i].pValue);
        if not (i = StartProcIndex) then
        begin
          TempOraVar := FOracleParams[I];
          for J := I downto StartProcIndex+1 do
            FOracleParams[j] := FOracleParams[j-1];
          FOracleParams[StartProcIndex] := TempOraVar;
        end;
      end;
    end;
    SetLength(TempVars, Length(FOracleParams));
    for i := 0 to high(FOracleParams) do
      TempVars[i] := FOracleParams[i].pValue;
    InParamValues := TempVars;  
  end;
end;

procedure TZOracleCallableStatement.FetchOutParamsFromOracleVars;
var
  CurrentVar: PZSQLVar;
  LobLocator: POCILobLocator;
  I: integer;
  L: Cardinal;
  TempBlob: IZBlob;

  procedure SetOutParam(CurrentVar: PZSQLVar; Index: Integer);
  var
    OracleConnection :IZOracleConnection;
    Year:SmallInt;
    Month, Day:Byte; Hour, Min, Sec:ub1; MSec: ub4;
    dTmp:TDateTime;
    {$IFDEF UNICODE}
    AnsiRec: TZAnsiRec;
    {$ELSE}
    RawTemp: RawByteString;
    {$ENDIF}
  begin
    case CurrentVar.TypeCode of
      SQLT_INT: outParamValues[Index] := EncodeInteger(PLongInt(CurrentVar.Data)^ );
      SQLT_FLT: outParamValues[Index] := EncodeFloat(PDouble(CurrentVar.Data)^ );
      SQLT_STR:
        begin
          try
            if Currentvar.Data = nil then
              outParamValues[Index] := NullVariant
            else
            begin
              L := ZFastCode.StrLen(CurrentVar.Data);
              {$IFDEF UNICODE}
              AnsiRec.Len := L;
              AnsiRec.P := CurrentVar.Data;// .Data;
              outParamValues[Index] := EncodeString(ZAnsiRecToUnicode(AnsiRec, ConSettings^.ClientCodePage^.CP));
              {$ELSE}
              ZSetString(CurrentVar.Data, L, RawTemp{%H-});
              outParamValues[Index] := EncodeString(ConSettings.ConvFuncs.ZRawToString(RawTemp, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP));
              {$ENDIF}
            end;
          finally
          end;
        end;
      SQLT_TIMESTAMP:
        begin
          OracleConnection := Connection as IZOracleConnection;
          FPlainDriver.DateTimeGetDate(
            OracleConnection.GetConnectionHandle ,
            FErrorHandle, PPOCIDescriptor(CurrentVar.Data)^,
            Year{%H-}, Month{%H-}, Day{%H-});
          FPlainDriver.DateTimeGetTime(
            OracleConnection.GetConnectionHandle ,
            FErrorHandle, PPOCIDescriptor(CurrentVar.Data)^,
            Hour{%H-}, Min{%H-}, Sec{%H-},MSec{%H-});
          dTmp := EncodeDate(year,month,day )+EncodeTime(Hour,min,sec,msec) ;
          outParamValues[Index] := EncodeDateTime(dTmp);
        end;
      SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE:
        begin
          if CurrentVar.Indicator >= 0 then
            LobLocator := PPOCIDescriptor(CurrentVar.Data)^
          else
            LobLocator := nil;

          OracleConnection := Connection as IZOracleConnection;
          if CurrentVar.TypeCode in [SQLT_BLOB, SQLT_BFILEE] then
            TempBlob := TZOracleBlob.Create(FPlainDriver, nil, 0,
              OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                LobLocator, GetChunkSize, ConSettings)
          else
            TempBlob := TZOracleClob.Create(FPlainDriver, nil, 0,
              OracleConnection.GetConnectionHandle,
              OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
              LobLocator, GetChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
          outParamValues[Index] := EncodeInterface(TempBlob);
          TempBlob := nil;
        end;
      SQLT_NTY: //currently not supported
        outParamValues[Index] := NullVariant;
    end;
  end;
begin
  for I := 0 to FOracleParamsCount -1 do
    if FOracleParams[i].pType in [2,3,4] then
    begin
      CurrentVar:= @FInVars.Variables[I+1];
      SetOutParam(CurrentVar, FOracleParams[i].pParamIndex-1);
    end;
end;

function TZOracleCallableStatement.GetProcedureSql(SelectProc: boolean): RawByteString;
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
      if ( FDBParamTypes[I] = 4 ) then //ptResult
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
  InParams: string;
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
          if ( FOracleParams[I].pType = 4 ) then //ptResult
            sFunc := ' :'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])+' := '
          else
            if InParams <> '' then
              InParams := InParams +', :'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])
            else
              InParams := InParams +':'+StringReplace(FOracleParams[I].pName, '.', '', [rfReplaceAll])
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
  Result := NotEmptyStringToASCII7(TempResult);
end;

function TZOracleCallableStatement.IsNull(ParameterIndex: Integer): Boolean;
begin
  result := inherited IsNull(ParameterIndex);
end;

procedure TZOracleCallableStatement.ClearParameters;
begin
  inherited;
  FOracleParamsCount := 0;
  SetLength(FOracleParams, 0);
end;

constructor TZOracleCallableStatement.Create(Connection: IZConnection;
  const pProcName: string; Info: TStrings);
begin
  inherited Create(Connection, pProcName, Info);

  FOracleParamsCount := 0;
  FPlainDriver := Connection.GetIZPlainDriver as IZOraclePlainDriver;
  ResultSetType := rtForwardOnly;
  FPrepared := False;
  PackageIncludedList := TStringList.Create;
  FOutParamCount := 0;
  FPrefetchCount := StrToIntDef(ZDbcUtils.DefineStatementParameter(Self, 'prefetch_count', '1000'), 1000);
  FConnectionHandle := (Connection as IZOracleConnection).GetConnectionHandle;
end;

destructor TZOracleCallableStatement.Destroy;
begin
  FreeOracleSQLVars(FPlainDriver, FInVars, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings);
  PackageIncludedList.Free;
  inherited;
end;

function TZOracleCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver , Connection, FErrorHandle,
    FInVars, InParamValues, ChunkSize);

  try
    ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
      ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    FetchOutParamsFromOracleVars;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FInVars);
  end;

  { Autocommit statement. done by ExecuteOracleStatement}
  Result := LastUpdateCount;
end;

function TZOracleCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  if not Prepared then
    Prepare;

  { Loads binded variables with values. }
  LoadOracleVars(FPlainDriver , Connection, FErrorHandle,
    FInVars, InParamValues, ChunkSize);

  try
    ExecuteOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
      ASQL, FHandle, FErrorHandle, ConSettings, Connection.GetAutoCommit);
    FetchOutParamsFromOracleVars;
    LastResultSet := CreateOracleResultSet(FPlainDriver, Self, Self.SQL,
      FHandle, FErrorHandle, FInVars, FOracleParams);
    Result := LastResultSet;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FInVars);
  end;
end;

end.
