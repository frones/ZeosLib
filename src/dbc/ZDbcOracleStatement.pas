{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types, FmtBCD,
  {$IFDEF MSWINDOWS}{%H-}Windows,{$ENDIF}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZSysUtils, ZClasses, ZCompatibility, ZVariant, ZPlainOracleDriver,
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZDbcOracleUtils, ZDbcUtils, ZDbcOracle;

type

  {** Implements a abstract prepared SQL Statement for Oracle }
  TZAbstractOracleStatement = class(TZAbstractPreparedStatement)
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
    FCharSetID: ub2;
    FParamNames: TRawByteStringDynArray;
    FByteBuffer: PByteBuffer;
  protected
    procedure InitBuffer(SQLType: TZSQLType; OCIBind: PZOCIParamBind; Index, ElementCnt: Cardinal; ActualLength: LengthInt = 0);
    function CreateResultSet: IZResultSet;
    procedure SetBindCapacity(Capacity: Integer); override;
    procedure CheckParameterIndex(var Index: Integer); override;
    procedure ReleaseConnection; override;
  protected
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  {** Implements Prepared SQL Statement for Oracle }
  TZAbstractOraclePreparedStatement = class(TZAbstractOracleStatement)
  private
    procedure BindSInteger(Index: Integer; SQLType: TZSQLType; Value: NativeInt);
    procedure BindUInteger(Index: Integer; SQLType: TZSQLType; Value: NativeUInt);
    procedure InternalBindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double);
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt);
  protected
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZRawSQLStringWriter; Var Result: RawByteString); override;
  public
    procedure SetPWideChar(Index: Integer; Value: PWideChar; WLen: NativeUInt);
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    procedure SetBoolean(Index: Integer; Value: Boolean);
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetShort(Index: Integer; Value: ShortInt);
    procedure SetWord(Index: Integer; Value: Word); reintroduce;
    procedure SetSmall(Index: Integer; Value: SmallInt); reintroduce;
    procedure SetUInt(Index: Integer; Value: Cardinal); reintroduce;
    procedure SetInt(Index: Integer; Value: Integer); reintroduce;
    procedure SetULong(Index: Integer; const Value: UInt64); reintroduce;
    procedure SetLong(Index: Integer; const Value: Int64); reintroduce;
    procedure SetFloat(Index: Integer; Value: Single); reintroduce;
    procedure SetDouble(Index: Integer; const Value: Double); reintroduce;
    procedure SetCurrency(Index: Integer; const Value: Currency); reintroduce;
    procedure SetBigDecimal(Index: Integer; const Value: TBCD); reintroduce;
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;

    procedure SetDate(Index: Integer; const Value: TZDate); reintroduce; overload;
    procedure SetTime(Index: Integer; const Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(Index: Integer; const Value: TZTimeStamp); reintroduce; overload;

    {string bindings }
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: UnicodeString);
    procedure SetString(ParameterIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString);
    { array bindings }
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); reintroduce;
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); reintroduce;
  end;

  TZOraclePreparedStatement_W = Class(TZAbstractOraclePreparedStatement, IZPreparedStatement)
  protected
    FCachedQueryUni, FParamNames: TUnicodeStringDynArray;
    FIsParamIndex: TBooleanDynArray;
    property IsParamIndex: TBooleanDynArray read FIsParamIndex;
  public
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): UnicodeString; override;
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
    procedure Unprepare; override;
  End;

  TZOraclePreparedStatement_A = Class(TZAbstractOraclePreparedStatement, IZPreparedStatement)
  protected
    FCachedQueryRaw: TRawByteStringDynArray;
    FIsParamIndex: TBooleanDynArray;
    property IsParamIndex: TBooleanDynArray read FIsParamIndex;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
    procedure Unprepare; override;
  End;

  {** Implements SQL Statement for Oracle }
  TZOracleStatement_A = class(TZAbstractOracleStatement, IZStatement)
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
  end;

  {** Implements SQL Statement for Oracle }
  TZOracleStatement_W = class(TZOracleStatement_A);

  TZOracleCallableStatement_A = class(TZAbstractCallableStatement_A, IZCallableStatement)
  private
    FProcDescriptor: TZOraProcDescriptor_A;
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
    procedure PrepareInParameters; override;
  public
    procedure Unprepare; override;
  end;

  TZOracleCallableStatement_W = class(TZAbstractCallableStatement_W, IZCallableStatement)
  private
    FProcDescriptor: TZOraProcDescriptor_W;
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
    procedure PrepareInParameters; override;
  public
    procedure Unprepare; override;
  end;

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZDbcOracleResultSet, ZTokenizer,
  ZEncoding, ZDbcProperties, ZMessages, ZDbcResultSet,
  ZSelectSchema;

const
  StrGUIDLen = 36;
  SQLType2OCIType: array[stUnknown..stBinaryStream] of ub2 = (SQLT_INT,
    SQLT_UIN, SQLT_UIN, SQLT_INT, SQLT_UIN, SQLT_INT, SQLT_UIN, SQLT_INT, SQLT_UIN, SQLT_INT,  //ordinals
    SQLT_BFLOAT, SQLT_BDOUBLE, SQLT_VNU, SQLT_VNU, //floats
    SQLT_DAT, SQLT_TIMESTAMP, SQLT_TIMESTAMP, //time values
    SQLT_AFC, //GUID
    SQLT_LVC, SQLT_LVC, SQLT_LVB, //varying size types in equal order
    SQLT_CLOB, SQLT_CLOB, SQLT_BLOB); //lob's
  SQLType2OCISize: array[stUnknown..stBinaryStream] of sb2 = (SizeOf(Integer),
    SizeOf(Boolean), SizeOf(Byte), SizeOf(ShortInt), SizeOf(Word), SizeOf(SmallInt), SizeOf(Cardinal), SizeOf(Integer), SizeOf(UInt64), SizeOf(Int64),  //ordinals
    SizeOf(Single), SizeOf(Double), SizeOf(TOCINumber), SizeOf(TOCINumber), //floats
    SizeOf(TOraDate), SizeOf(POCIDescriptor), SizeOf(POCIDescriptor), //time values
    StrGUIDLen, //GUID
    SizeOf(TOCILong), SizeOf(TOCILong), SizeOf(TOCILong),  //varying size types in equal order minimum sizes for 8Byte alignment
    SizeOf(POCIDescriptor), SizeOf(POCIDescriptor), SizeOf(POCIDescriptor)); //lob's
var
  OraPreparableTokens: TPreparablePrefixTokens;

{ TZAbstractOracleStatement }

procedure TZAbstractOracleStatement.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var
  Bind: PZOCIParamBind;
  procedure AsBlob;
  var Blob: IZBlob;
  begin
    Blob := TZOracleBlob.Create(FOracleConnection, nil, SQLT_BLOB, FOpenLobStreams);
    Blob.SetBuffer(Buf, Len);
    BindLob(Index, stBinaryStream, Blob);
  end;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) then
    SQLType := BindList[Index].SQLType;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.value_sz < Len+SizeOf(Integer)) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1, Len);
  if Bind.dty = SQLT_LVB then begin
    POCILong(Bind.valuep).Len := Len;
    if Buf <> nil then
      Move(Buf^, POCILong(Bind.valuep).data[0], Len);
  end else if Bind.dty = SQLT_AFC then
    GUIDToBuffer(Buf, Bind.valuep, [])
  else if Bind.dty = SQLT_BLOB
    then AsBlob
    else raise CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType, stBytes);
  Bind.indp[0] := 0;
end;

procedure TZAbstractOracleStatement.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var
  Bind: PZOCIParamBind;
  OCILob: IZOracleLob;
  RefCntLob: IZBlob;
  Stream: TStream;
  CLob: IZCLob;
begin
  CheckParameterIndex(Index);
  if (Value = nil)
  then BindList.SetNull(Index, SQLType)
  else begin
    BindList.Put(Index, SQLType, Value); //incing the refcount 4 FPC
    {$R-}
    Bind := @FOraVariables[Index];
    {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
      InitBuffer(SQLType, Bind, Index, 1, SizeOf(POCIDescriptor));
    if Value.IsEmpty then
      Bind.indp[0] := -1
    else if Value.QueryInterface(IZOracleLob, OCILob) <> S_OK then begin
      if Bind.dty = SQLT_BLOB
      then RefCntLob := TZOracleBlob.CreateFromBlob(Value, nil, FOracleConnection, FOpenLobStreams)
      else begin
        if Value.IsClob then
          if ZEncoding.IsMBCSCodePage(FClientCP)
          then Value.SetCodePageTo(zCP_UTF16)
          else Value.SetCodePageTo(FClientCP)
        else if SQLType = stAsciiStream
          then PIZLob(BindList[Index].Value)^ := CreateRawCLobFromBlob(Value, ConSettings, FOpenLobStreams)
          else begin
            Stream := Value.GetStream;
            try
              PIZLob(BindList[Index].Value)^ := TZLocalMemCLob.CreateWithStream(Stream, zCP_UTF16, ConSettings, FOpenLobStreams);
            finally
              Stream.Free;
            end;
          end;
        IZBlob(BindList[Index].Value).QueryInterface(IZCLob, Clob);
        if ZEncoding.IsMBCSCodePage(FClientCP) then begin
          RefCntLob := TZOracleClob.CreateFromClob(Clob, nil,
            SQLCS_IMPLICIT, OCI_UTF16ID, FOracleConnection, FOpenLobStreams);
          SQLType := stUnicodeStream;
        end else begin
          RefCntLob := TZOracleClob.CreateFromClob(Clob, nil,
            SQLCS_IMPLICIT, ConSettings.ClientCodePage.ID, FOracleConnection, FOpenLobStreams);
          SQLType := stAsciiStream;
        end;
      end;
      BindLob(Index, SQLType, RefCntLob);
    end else begin
      PPOCILobLocator(Bind^.valuep)^ := OCILob.GetLobLocator;
      Bind.indp[0] := 0;
    end;
  end;
end;

procedure TZAbstractOracleStatement.CheckParameterIndex(var Index: Integer);
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
constructor TZAbstractOracleStatement.Create(
  const Connection: IZConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := TZOraclePlainDriver(Connection.GetIZPlainDriver.GetInstance);
  ResultSetType := rtForwardOnly;
  fOracleConnection := Connection as IZOracleConnection;
  FByteBuffer := fOracleConnection.GetByteBufferAddress;

  FCanBindInt64 := Connection.GetClientVersion >= 11002000;
  FRowPrefetchMemory := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_RowPrefetchSize, ''), 131072);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, DSProps_InternalBufSize, ''), 131072);
  FCharSetID := ConSettings.ClientCodePage.ID;
end;

function TZAbstractOracleStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: IZResultSet;
  CachedResultSet: TZOracleCachedResultSet;
begin
  if FOpenResultSet = nil then begin
    if FStatementType = OCI_STMT_SELECT
    then NativeResultSet := TZOracleResultSet_A.Create(Self, SQL, FOCIStmt, FOCIError, FZBufferSize)
    else NativeResultSet := TZOracleCallableResultSet.Create(Self, SQL, FOCIStmt, FOCIError, FOraVariables, BindList);
    if (GetResultSetConcurrency = rcUpdatable) or (GetResultSetType <> rtForwardOnly) then
    begin
      CachedResultSet := TZOracleCachedResultSet.Create(NativeResultSet, SQL, nil, ConSettings);
      if (GetResultSetConcurrency = rcUpdatable) and (FStatementType = OCI_STMT_SELECT) then
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
function TZAbstractOracleStatement.ExecutePrepared: Boolean;
var
  Status: sword;
  upCnt: ub4;
begin
  Result := False;
  PrepareLastResultSetForReUse;
  { Prepares a statement. }
  Prepare;
  { logs the values }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  RestartTimer;
  if (FStatementType = OCI_STMT_SELECT) then begin
    { Executes the statement and gets a resultset. }
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
    Result := LastResultSet <> nil;
  end else begin
    { Executes the statement and gets a result. }
    Status := FPlainDriver.OCIStmtExecute(FOracleConnection.GetServiceContextHandle,
        FOCIStmt, FOCIError, Max(1, BatchDMLArrayCount), 0, nil, nil, CommitMode[Connection.GetAutoCommit]);
    if Status <> OCI_SUCCESS then
      if FCharSetID = OCI_UTF16ID
      then FOracleConnection.HandleErrorOrWarningW(FOCIError, status, lcExecPrepStmt, fWSQL, Self)
      else FOracleConnection.HandleErrorOrWarningA(FOCIError, status, lcExecPrepStmt, fASQL, Self);
    Status := FPlainDriver.OCIAttrGet(FOCIStmt, OCI_HTYPE_STMT, @upCnt, nil,
      OCI_ATTR_ROW_COUNT, FOCIError);
    if Status <> OCI_SUCCESS then
      if FCharSetID = OCI_UTF16ID
      then FOracleConnection.HandleErrorOrWarningW(FOCIError, status, lcExecPrepStmt, fWSQL, Self)
      else FOracleConnection.HandleErrorOrWarningA(FOCIError, status, lcExecPrepStmt, fASQL, Self);
    LastUpdateCount := upCnt;
    if (FStatementType = OCI_STMT_BEGIN) and (BindList.HasOutOrInOutOrResultParam) then
      FOutParamResultSet := CreateResultSet;
    { logging execution }
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecPrepStmt,Self);
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractOracleStatement.ExecuteQueryPrepared: IZResultSet;
var
  Status: sword;
  upCnt: ub4;
begin
  PrepareOpenResultSetForReUse;
  { Prepares a statement. }
  Prepare;
  { log values }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  RestartTimer;
  { Executes the statement and gets a resultset. }
  if (FStatementType = OCI_STMT_BEGIN) and (BindList.HasOutOrInOutOrResultParam) then begin
    Status := FPlainDriver.OCIStmtExecute(FOracleConnection.GetServiceContextHandle,
        FOCIStmt, FOCIError, Max(1, BatchDMLArrayCount), 0, nil, nil, CommitMode[Connection.GetAutoCommit]);
    if Status <> OCI_SUCCESS then
      if ConSettings.ClientCodePage.Encoding = ceUTF16
      then FOracleConnection.HandleErrorOrWarningW(FOCIError, status, lcExecPrepStmt, fWSQL, Self)
      else FOracleConnection.HandleErrorOrWarningA(FOCIError, status, lcExecPrepStmt, fASQL, Self);
    FPlainDriver.OCIAttrGet(FOCIStmt, OCI_HTYPE_STMT, @upCnt, nil, OCI_ATTR_ROW_COUNT, FOCIError);
    LastUpdateCount := upCnt;
    Result := CreateResultSet;
    FOutParamResultSet := Result;
    { Logging Execution }
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecPrepStmt,Self);
  end else if (FStatementType = OCI_STMT_SELECT)  then
    Result := CreateResultSet
  else begin
    Result := nil; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
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
function TZAbstractOracleStatement.ExecuteUpdatePrepared: Integer;
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

  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  RestartTimer;
  if (FStatementType = OCI_STMT_SELECT) then begin
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
    Status := FPlainDriver.OCIStmtExecute(FOracleConnection.GetServiceContextHandle,
        FOCIStmt, FOCIError, Max(1, BatchDMLArrayCount), 0, nil, nil, CommitMode[Connection.GetAutoCommit]);
    if Status <> OCI_SUCCESS then
      if ConSettings.ClientCodePage.Encoding = ceUTF16
      then FOracleConnection.HandleErrorOrWarningW(FOCIError, status, lcExecPrepStmt, fWSQL, Self)
      else FOracleConnection.HandleErrorOrWarningA(FOCIError, status, lcExecPrepStmt, fASQL, Self);
    FPlainDriver.OCIAttrGet(FOCIStmt, OCI_HTYPE_STMT, @upCnt, nil, OCI_ATTR_ROW_COUNT, FOCIError);
    LastUpdateCount := upCnt;
    if ((FStatementType = OCI_STMT_BEGIN) or (FStatementType = OCI_STMT_DECLARE)) and (BindList.HasOutOrInOutOrResultParam) then
      FOutParamResultSet := CreateResultSet;
    { logging execution }
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcExecPrepStmt,Self);
  end;
  Result := LastUpdateCount;
end;

{$IFDEF NEXTGEN}{$HINTS OFF}{$ENDIF}//wrong hint OldSize assigned value is never used
procedure TZAbstractOracleStatement.InitBuffer(SQLType: TZSQLType;
  OCIBind: PZOCIParamBind; Index, ElementCnt: Cardinal; ActualLength: LengthInt);
var
  Status: sword;
  I, OldSize: Integer;
  acsid: ub2;
begin
  { free Desciptors }
  if (OCIBind.DescriptorType <> 0) and (OCIBind.DescriptorType <> OCI_DTYPE_LOB) then begin //do not free the descripors of the lobs
    if (OCIBind.DescriptorType <> SQLType2OCIDescriptor[SQLType]) then
      OldSize := 0
    else if (OCIBind.DescriptorType = SQLType2OCIDescriptor[SQLType]) and (ElementCnt < OCIBind.curelen) then
      OldSize := ElementCnt
    else OldSize := OCIBind.curelen;
    for I := OCIBind.curelen-1 downto OldSize do begin
      Status := FPlainDriver.OCIDescriptorFree(PPOCIDescriptor(PAnsiChar(OCIBind.valuep)+I*SizeOf(POCIDescriptor))^, OCIBind.DescriptorType);
      if Status <> OCI_SUCCESS then
        FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcExecute, 'OCIDescriptorFree', Self);
    end;
  end;

  OCIBind.DescriptorType := SQLType2OCIDescriptor[SQLType];
  OCIBind.dty := SQLType2OCIType[SQLType];

  OldSize := OCIBind.value_sz;
  {check if the parameter type was registered before -> they should be valid only }
  if (BindList[Index].ParamType <> pctUnknown) and (SQLType <> BindList[Index].SQLType) then
    raise EZSQLException.Create(SUnKnownParamDataType);
  if (SQLType in [stLong, stULong]) and not FCanBindInt64 then begin
    OCIBind.dty := SQLT_VNU;
    OCIBind.value_sz := SizeOf(TOCINumber);
  end else if SQLType in [stString, stUnicodeString, stBytes] then { 8 byte aligned buffer -> }
    OCIBind.value_sz := Max((((Max(Max(OCIBind.Precision, ActualLength)+SizeOf(Sb4)+2, SQLType2OCISize[SQLType])) shr 3)+1) shl 3, OCIBind.value_sz)
  else OCIBind.value_sz := SQLType2OCISize[SQLType];

  if ElementCnt = 1 then
    BindList[Index].SQLType := SQLType;
  { allocmem for the null indicators }
  if OCIBind.curelen <> ElementCnt then begin
    if OCIBind.indp <> nil then
      FreeMem(OCIBind.indp, OCIBind.curelen*SizeOf(SB2));
    GetMem(OCIBind.indp, SizeOf(SB2)*ElementCnt); //alloc mem for indicators
  end;
  //alloc buffer space
  if (OCIBind.DescriptorType <> 0) then begin
    ReallocMem(OCIBind.valuep, OCIBind.value_sz*Integer(ElementCnt));
    if (OCIBind.DescriptorType <> OCI_DTYPE_LOB) then //EH: do not alloc descrptors for the lobs
      for I := OCIBind.curelen to ElementCnt -1 do begin
        { allocate lob/time oci descriptors }
        Status := FPlainDriver.OCIDescriptorAlloc(FOracleConnection.GetConnectionHandle,
            PPOCIDescriptor(OCIBind.valuep+I*SizeOf(POCIDescriptor))^, OCIBind.DescriptorType, 0, nil);
        if Status <> OCI_SUCCESS then
          FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcExecute, 'OCIDescriptorAlloc', Self);
      end;
  end else begin
    if OCIBind.valuep <> nil then begin
      FreeMem(OCIBind.valuep, OldSize*Integer(OCIBind.curelen));
      OCIBind.valuep := nil;
    end;
    if (not ((ElementCnt > 1) and (Ord(SQLType) < Ord(stCurrency)) and (OCIBind.dty <> SQLT_VNU) )) then
      GetMem(OCIBind.valuep, OCIBind.value_sz*Integer(ElementCnt));
  end;
  OCIBind.curelen := ElementCnt;
  { in array bindings we directly OCIBind the pointer of the dyn arrays instead of moving data}
  if OCIBind.valuep <> nil then begin
    Status := FPlainDriver.OCIBindByPos(FOCIStmt, OCIBind.bindpp, FOCIError, Index + 1,
      OCIBind.valuep, OCIBind.value_sz, OCIBind.dty, OCIBind.indp, nil, nil, 0, nil, OCI_DEFAULT);
    if Status <> OCI_SUCCESS then
      FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIBindByPos', Self);
    if SQLType = stUnicodeString then begin
      acsid := OCI_UTF16ID;
      Status := FplainDriver.OCIAttrSet(OCIBind.bindpp, OCI_HTYPE_BIND, @acsid,
           0, OCI_ATTR_CHARSET_ID, FOCIError);
      if Status <> OCI_SUCCESS then
        FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIAttrSet(OCI_ATTR_CHARSET_ID)', Self);
    end;
  end;
end;
{$IFDEF NEXTGEN}{$HINTS ON}{$ENDIF}//wrong hint OldSize assigned value is never used

{**
  prepares the statement on the server
}
procedure TZAbstractOracleStatement.Prepare;
var
  Status: sword;
  Prefetch: ub4;
  stmt: Pointer;
  stmt_len: ub4;
begin
  if not Prepared then begin
    // we need a errorhandle per stmt
    RestartTimer;
    if (FOCIError = nil) then begin
      Status := FPlainDriver.OCIHandleAlloc(FOracleConnection.GetConnectionHandle,
        FOCIError, OCI_HTYPE_ERROR, 0, nil);
      if Status <> OCI_SUCCESS then
        FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcPrepStmt, 'OCIAttrSet(OCI_ATTR_CHARSET_ID)', Self);
    end;

    if (FOCIStmt = nil) then begin
      if FCharSetID = OCI_UTF16ID then begin
        stmt := Pointer(FWSQL);
        stmt_len := (Length(FWSQL)+1) shl 1;
      end else begin
        stmt := Pointer(FASQL);
        stmt_len := Length(FASQL)+1;
      end;
      if stmt = nil then stmt := PEmptyUnicodeString;
      if FServerStmtCache then begin
        //check if query is found in the server cache
        Status := FPlainDriver.OCIStmtPrepare2(FOracleConnection.GetServiceContextHandle,
          FOCIStmt, FOCIError, stmt, stmt_len,nil,0,OCI_NTV_SYNTAX, OCI_PREP2_CACHE_SEARCHONLY);
        if Status <> OCI_SUCCESS then //not found! Create new handle in server cache
          Status := FPlainDriver.OCIStmtPrepare2(FOracleConnection.GetServiceContextHandle,
            FOCIStmt, FOCIError, stmt, stmt_len,nil,0,OCI_NTV_SYNTAX,OCI_DEFAULT);
      end else begin
        Status := FPlainDriver.OCIHandleAlloc(FOracleConnection.GetConnectionHandle,
          FOCIStmt, OCI_HTYPE_STMT, 0, nil);
        if Status <> OCI_SUCCESS then
          FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcPrepStmt, 'OCIHandleAlloc(OCIStmt-Handle)', Self);
        Status := FPlainDriver.OCIStmtPrepare(FOCIStmt, FOCIError, stmt,
          stmt_len, OCI_NTV_SYNTAX, OCI_DEFAULT);
      end;
      if Status <> OCI_SUCCESS then
        if FCharSetID = OCI_UTF16ID
        then FOracleConnection.HandleErrorOrWarningW(FOCIError, Status, lcPrepStmt, FWSQL, Self)
        else FOracleConnection.HandleErrorOrWarningA(FOCIError, Status, lcPrepStmt, FASQL, Self);
    end;
    { get statement type }
    Status := FPlainDriver.OCIAttrGet(FOCIStmt, OCI_HTYPE_STMT, @FStatementType,
      nil, OCI_ATTR_STMT_TYPE, FOCIError);
    if Status <> OCI_SUCCESS then
      Self.FOracleConnection.HandleErrorOrWarning(FOCIError, Status, lcOther,
        'OCIAttrGet(OCI_ATTR_STMT_TYPE)', Self);
    if FStatementType = OCI_STMT_SELECT then begin
      //set prefetch by memory! not by Rows!
      Prefetch := 0;
      Status := FPlainDriver.OCIAttrSet(FOCIStmt,OCI_HTYPE_STMT, @Prefetch ,0, OCI_ATTR_PREFETCH_ROWS,FOCIError);
      if Status <> OCI_SUCCESS then
        FOracleConnection.HandleErrorOrWarning(FOCIError, Status, lcOther, 'OCIAttrSet(OCI_ATTR_PREFETCH_ROWS)', Self);
      Prefetch := FRowPrefetchMemory;
      Status := FPlainDriver.OCIAttrSet(FOCIStmt,OCI_HTYPE_STMT,@Prefetch,0,OCI_ATTR_PREFETCH_MEMORY,FOCIError);
      if Status <> OCI_SUCCESS then
        FOracleConnection.HandleErrorOrWarning(FOCIError, Status, lcOther, 'OCIAttrSet(OCI_ATTR_PREFETCH_MEMORY)', Self);
    end;
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcPrepStmt,Self);
    inherited Prepare;
  end;
end;

procedure TZAbstractOracleStatement.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FOracleConnection := nil;
end;

{**
  Sets a new parameter capacity and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractOracleStatement.SetBindCapacity(Capacity: Integer);
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
      Bind.ParamName := '';
      if Bind.DescriptorType <> 0 then //deallocate the descriptors
        for J := 0 to Bind.curelen-1 do
          //this tempory descriprors /locators are cleared by the lobs
          if (Bind.DescriptorType <> OCI_DTYPE_LOB) and (Bind.DescriptorType <> SQLT_BFILEE) then
            FPlainDriver.OCIDescriptorFree(PPOCIDescriptor(PAnsiChar(Bind.valuep)+J*SizeOf(POCIDescriptor))^, Bind.DescriptorType);
      if Bind.valuep <> nil then
        FreeMem(Bind.valuep, Bind.value_sz*Integer(Bind.curelen));
      if Bind.indp <> nil then
        FreeMem(Bind.indp, SizeOf(SB2)*Bind.curelen);
    end;
    ReallocMem(FOraVariables, Capacity * SizeOf(TZOCIParamBind));
    SetLength(FParamNames, Capacity);
    if FOraVariables <> nil then
      FillChar((PAnsichar(FOraVariables)+(OldCapacity*SizeOf(TZOCIParamBind)))^,
        (Capacity-OldCapacity)*SizeOf(TZOCIParamBind), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  end;
end;

procedure TZAbstractOracleStatement.Unprepare;
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
        FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcUnprepStmt, SQL, Self);
    end;
    if FOCIError <> nil then begin
      Status := FPlainDriver.OCIHandleFree(FOCIError, OCI_HTYPE_ERROR);
      FOCIError := nil;
      if Status <> OCI_SUCCESS then
        FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcUnprepStmt, SQL, Self);
    end;
  end;
end;

{ TZOracleStatement_A }

constructor TZOracleStatement_A.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZOracleCallableStatement_A }

function TZOracleCallableStatement_A.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  ProcSQL: RawByteString;
  SQLWriter: TZRawSQLStringWriter;
  IC: IZIdentifierConvertor;

  procedure AddArgs({$IFDEF AUTOREFCOUNT}const{$ENDIF}Params: TObjectList);
  var I: Integer;
  begin
    SQLWriter.AddChar(AnsiChar('('), ProcSQL);
    for I := 0 to Params.Count-1 do
      if TZOraProcDescriptor_A(Params[i]).OrdPos > 0 then begin
        SQLWriter.AddChar(AnsiChar(':'), ProcSQL);
        TZOraProcDescriptor_A(Params[i]).ConcatParentName(False, SQLWriter, ProcSQL, IC);
        SQLWriter.AddText(TZOraProcDescriptor_A(Params[i]).AttributeName, ProcSQL);
        SQLWriter.AddChar(AnsiChar(','), ProcSQL);
      end;
    SQLWriter.ReplaceOrAddLastChar(AnsiChar(','),AnsiChar(')'),ProcSQL);
  end;

  procedure BuildFunction({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_A);
  {$IFDEF UNICODE}
  var R: RawByteString;
      S: UnicodeString;
  {$ENDIF}
  begin
    SQLWriter.AddChar(AnsiChar(':'), ProcSQL);
    TZOraProcDescriptor_A(Descriptor.Args[0]).ConcatParentName(False, SQLWriter, ProcSQL, IC);
    SQLWriter.AddText(TZOraProcDescriptor_A(Descriptor.Args[0]).AttributeName, ProcSQL);
    SQLWriter.AddText(' := ', ProcSQL);
    Descriptor.ConcatParentName(True, SQLWriter, ProcSQL, IC);
    {$IFDEF UNICODE}
    S := ZRawToUnicode(Descriptor.AttributeName, FClientCP);
    S := IC.Quote(S);
    R := ZUnicodeToRaw(S, FClientCP);
    SQLWriter.AddText(R, ProcSQL);
    {$ELSE}
    SQLWriter.AddText(IC.Quote(Descriptor.AttributeName), ProcSQL);
    {$ENDIF}
    AddArgs(Descriptor.Args);
    SQLWriter.AddChar(AnsiChar(';'), ProcSQL);
  end;
  procedure BuildProcedure({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_A);
  {$IFDEF UNICODE}
  var R: RawByteString;
      S: UnicodeString;
  {$ENDIF}
  begin
    Descriptor.ConcatParentName(True, SQLWriter, ProcSQL, IC);
    {$IFDEF UNICODE}
    S := ZRawToUnicode(Descriptor.AttributeName, FClientCP);
    S := IC.Quote(S);
    R := ZUnicodeToRaw(S, FClientCP);
    SQLWriter.AddText(R, ProcSQL);
    {$ELSE}
    SQLWriter.AddText(IC.Quote(Descriptor.AttributeName), ProcSQL);
    {$ENDIF}
    AddArgs(Descriptor.Args);
    SQLWriter.AddChar(AnsiChar(';'), ProcSQL);
  end;
  procedure BuildPackage({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_A);
  var I: Integer;
  begin
    for I := 0 to Descriptor.Args.Count -1 do begin
      if Descriptor.Parent <> nil then
        SQLWriter.AddText('BEGIN'#10, ProcSQL);
      if TZOraProcDescriptor_A(Descriptor.Args[I]).ObjType = OCI_PTYPE_PKG then
        BuildPackage(TZOraProcDescriptor_A(Descriptor.Args[I]))
      else if TZOraProcDescriptor_A(Descriptor.Args[I]).ObjType = OCI_PTYPE_PROC then
        BuildProcedure(TZOraProcDescriptor_A(Descriptor.Args[I]))
      else if TZOraProcDescriptor_A(Descriptor.Args[I]).ObjType = OCI_PTYPE_FUNC then
        BuildFunction(TZOraProcDescriptor_A(Descriptor.Args[I]))
      else
        AddArgs(Descriptor.Args);
      if Descriptor.Parent <> nil then
        SQLWriter.AddText(#10'END;', ProcSQL);
    end;
  end;
begin
  IC := Connection.GetMetadata.GetIdentifierConvertor;
  SQLWriter := TZRawSQLStringWriter.Create(1024);
  try
    if FProcDescriptor = nil then
      { describe the object: }
      FProcDescriptor := TZOraProcDescriptor_A.Create(nil, Connection as IZOracleConnection, FClientCP);
    if FProcDescriptor.ObjType = OCI_PTYPE_UNK then begin
      {$IFDEF UNICODE}
      ProcSQL := ZUnicodeToRaw(StoredProcName, FClientCP);
      FProcDescriptor.Describe(OCI_PTYPE_UNK, ProcSQL);
      {$ELSE}
      FProcDescriptor.Describe(OCI_PTYPE_UNK, StoredProcName);
      {$ENDIF}
    end;
    ProcSQL := '';
    SQLWriter.AddText('BEGIN'#10, ProcSQL);
    if FProcDescriptor.ObjType = OCI_PTYPE_PKG then
      BuildPackage(FProcDescriptor)
    else if FProcDescriptor.ObjType = OCI_PTYPE_PROC then
      BuildProcedure(FProcDescriptor)
    else
      BuildFunction(FProcDescriptor);
    SQLWriter.AddText(#10'END;', ProcSQL);
    SQLWriter.Finalize(ProcSQL);
  finally
    FreeAndNil(SQLWriter);
  end;
  Result := TZOraclePreparedStatement_A.Create(Connection, '', Info);
  TZOraclePreparedStatement_A(Result).FASQL := ProcSQL;
  {$IFDEF UNICODE}
  TZOraclePreparedStatement_A(Result).fWSQL := ZRawToUnicode(ProcSQL, FClientCP);
  {$ENDIF}
  TZOraclePreparedStatement_A(Result).Prepare;
end;

const OCIParamTypeMatrix: array[boolean] of array[OCI_TYPEPARAM_IN..OCI_TYPEPARAM_INOUT] of TZProcedureColumnType =
  ((pctIn, pctOut, pctInOut),(pctReturn,pctReturn,pctReturn));
procedure TZOracleCallableStatement_A.PrepareInParameters;
var Idx: Integer;
  {$IFDEF UNICODE}
  R: RawByteString;
  {$ENDIF}
  procedure RegisterFromDescriptor(ParentDescriptor: TZOraProcDescriptor_A;
    var IDX: Integer);
  var i: Integer;
    Descriptor: TZOraProcDescriptor_A;
    Tmp: RawByteString;
    SQLWriter: TZRawSQLStringWriter;
    {$IFDEF UNICODE}
    S: UnicodeString;
    {$ENDIF}
  begin
    SQLWriter := TZRawSQLStringWriter.Create(1024);
    try
      for I := 0 to ParentDescriptor.Args.Count-1 do begin
        Descriptor := TZOraProcDescriptor_A(ParentDescriptor.Args[i]);
        if Descriptor.ObjType <> OCI_PTYPE_ARG then
          RegisterFromDescriptor(Descriptor, IDX)
        else begin
          Tmp := '';
          Descriptor.ConcatParentName(False, SQLWriter, Tmp, nil);
          SQLWriter.AddText(Descriptor.AttributeName, Tmp);
          SQLWriter.Finalize(tmp);
          {$IFDEF UNICODE}
          S := ZRawToUnicode(tmp, FClientCP);
          {$ENDIF}
          if FExecStatement = nil then
            RegisterParameter(IDX,
              Descriptor.SQLType, OCIParamTypeMatrix[Descriptor.OrdPos = 0][Descriptor.IODirection], {$IFDEF UNICODE}S{$ELSE}tmp{$ENDIF},
                Max(LengthInt(Descriptor.DataSize), LengthInt(Descriptor.Precision)), Descriptor.Scale)
          else begin
            Self.RegisterParameter(IDX,
              Descriptor.SQLType, OCIParamTypeMatrix[Descriptor.OrdPos = 0][Descriptor.IODirection], {$IFDEF UNICODE}S{$ELSE}tmp{$ENDIF},
                Max(LengthInt(Descriptor.DataSize), LengthInt(Descriptor.Precision)), Descriptor.Scale);
            FExecStatement.RegisterParameter(IDX,
              Descriptor.SQLType, OCIParamTypeMatrix[Descriptor.OrdPos = 0][Descriptor.IODirection], {$IFDEF UNICODE}S{$ELSE}tmp{$ENDIF},
                Max(LengthInt(Descriptor.DataSize), LengthInt(Descriptor.Precision)), Descriptor.Scale);
          end;
          Inc(IDX);
        end;
      end;
    finally
      FreeAndNil(SQLWriter);
    end;
  end;
begin
  if not FParamsRegistered then begin
    FParamsRegistered := True;
    FRegisteringParamFromMetadata := True;
    if FProcDescriptor = nil then
      FProcDescriptor := TZOraProcDescriptor_A.Create(nil, Connection as IZOracleConnection, FClientCP);
    if FProcDescriptor.ObjType = OCI_PTYPE_UNK then begin
      { describe the object: }
      {$IFDEF UNICODE}
      R := ZUnicodeToRaw(StoredProcName, FClientCP);
      FProcDescriptor.Describe(OCI_PTYPE_UNK, R);
      {$ELSE}
      FProcDescriptor.Describe(OCI_PTYPE_UNK, StoredProcName);
      {$ENDIF}
    end;
    if FProcDescriptor <> nil then begin
      Idx := 0;
      RegisterFromDescriptor(FProcDescriptor, IDX);
    end;
    FRegisteringParamFromMetadata := False;
  end;
end;

procedure TZOracleCallableStatement_A.Unprepare;
begin
  inherited Unprepare;
  if FProcDescriptor <> nil then
    FreeAndNil(FProcDescriptor);
end;

{ TZAbstractOraclePreparedStatement }

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractOraclePreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
var
  Bind: PZOCIParamBind;
  SQLType: TZSQLType;
  Lob: IZClob;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType)
  then SQLType := BindList[Index].SQLType
  else SQLType := stString;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.value_sz < Len+SizeOf(Integer)) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1, Len);
  if Bind.dty = SQLT_LVC then begin
    POCILong(Bind.valuep).Len := Len;
    if Len > 0 then
      Move(Buf^, POCILong(Bind.valuep).data[0], Len);
    Bind.indp[0] := 0;
  end else if Bind.dty = SQLT_CLOB then begin
    Lob := TZOracleClob.Create(FOracleConnection, nil, SQLCS_IMPLICIT, FClientCP, FOpenLobStreams);
    if Len > 0 then
      Lob.SetPAnsiChar(Buf, FClientCP, Len);
    BindLob(Index, stAsciiStream, Lob);
  end; //else ? should not happen
end;

procedure TZAbstractOraclePreparedStatement.BindSInteger(Index: Integer;
  SQLType: TZSQLType; Value: NativeInt);
var
  Bind: PZOCIParamBind;
  P: PAnsiChar;
  Status: sword;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (SQLType <> BindList[Index].SQLType) then //keep registered types alive
    SQLType := BindList[Index].SQLType;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  case Bind.dty of
    SQLT_VNU: begin
        Status := FPlainDriver.OCINumberFromInt(FOCIError, @Value, SizeOf(NativeInt), OCI_NUMBER_SIGNED, POCINumber(Bind.valuep));
        if Status <> OCI_SUCCESS then
          FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCINumberFromInt', Self);
      end;
    SQLT_INT: if Bind.value_sz = SizeOf(Int64) then
                PInt64(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Integer) then
                PInteger(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(SmallInt) then
                PSmallInt(Bind.valuep)^ := Value
              else
                PShortInt(Bind.valuep)^ := Value;
    SQLT_UIN: if Bind.value_sz = SizeOf(UInt64) then
                PUInt64(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Cardinal) then
                PCardinal(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Word) then
                PWord(Bind.valuep)^ := Value
              else
                PByte(Bind.valuep)^ := Value;
    SQLT_BFLOAT,
    SQLT_BDOUBLE,
    SQLT_DAT,
    SQLT_TIMESTAMP: InternalBindDouble(Index, SQLtype, Value);
    SQLT_LVC: begin
                IntToRaw(Value, PAnsiChar(@POCIVary(Bind.valuep).data[0]), @P);
                POCIVary(Bind.valuep).Len := P-@POCIVary(Bind.valuep).data[0];
              end
  end;
  Bind.indp[0] := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractOraclePreparedStatement.BindUInteger(Index: Integer;
  SQLType: TZSQLType; Value: NativeUInt);
var
  Bind: PZOCIParamBind;
  P: PAnsiChar;
  Status: sword;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (SQLType <> BindList[Index].SQLType) then //keep registered types alive
    SQLType := BindList[Index].SQLType;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  if Bind.dty = SQLT_VNU then begin
    Status := FPlainDriver.OCINumberFromInt(FOCIError, @Value, SizeOf(NativeUInt), OCI_NUMBER_UNSIGNED, POCINumber(Bind.valuep));
    if Status <> OCI_SUCCESS then
      FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCINumberFromInt', Self);
  end else if Bind.dty = SQLT_INT then
    if Bind.value_sz = SizeOf(Int64) then
      PInt64(Bind.valuep)^ := Value
    else if Bind.value_sz = SizeOf(Integer) then
      PInteger(Bind.valuep)^ := Value
    else if Bind.value_sz = SizeOf(SmallInt) then
      PSmallInt(Bind.valuep)^ := Value
    else
      PShortInt(Bind.valuep)^ := Value
  else if Bind.dty = SQLT_UIN then
    if Bind.value_sz = SizeOf(UInt64) then
      PUInt64(Bind.valuep)^ := Value
    else if Bind.value_sz = SizeOf(Cardinal) then
      PCardinal(Bind.valuep)^ := Value
    else if Bind.value_sz = SizeOf(Word) then
      PWord(Bind.valuep)^ := Value
    else
      PByte(Bind.valuep)^ := Value
  else begin
    IntToRaw(Value, PAnsiChar(@POCIVary(Bind.valuep).data[0]), @P);
    POCIVary(Bind.valuep).Len := P-@POCIVary(Bind.valuep).data[0];
  end;
  Bind.indp[0] := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZAbstractOraclePreparedStatement.InternalBindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var
  Bind: PZOCIParamBind;
  status: sword;
  TS: TZTimeStamp;
  procedure SetRaw;
  begin
    case SQLType of
      stBoolean:    fRawTemp := BoolStrIntsRaw[Value <> 0];
      stSmall,
      stInteger,
      stLong:       fRawTemp := IntToRaw(Trunc(Value));
      stDate:       fRawTemp := DateTimeToRawSQLDate(Value, ConSettings^.WriteFormatSettings, False);
      stTime:       fRawTemp := DateTimeToRawSQLTime(Value, ConSettings^.WriteFormatSettings, False);
      stTimeStamp:  fRawTemp := DateTimeToRawSQLTimeStamp(Value, ConSettings^.WriteFormatSettings, False);
      else          fRawTemp := FloatToSqlRaw(Value);
    end;
    SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, fRawTemp)
  end;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (SQLType <> BindList[Index].SQLType) then
    SQLType := BindList[Index].SQLType;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  case Bind.dty of
    SQLT_VNU:   begin
                  status := FPlainDriver.OCINumberFromReal(FOracleConnection.GetErrorHandle, @Value, SizeOf(Double), POCINumber(Bind.valuep));
                  if Status <> OCI_SUCCESS then
                    FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCINumberFromReal', Self);
                end;
    SQLT_BFLOAT:  PSingle(Bind.valuep)^ := Value;
    SQLT_BDOUBLE: PDouble(Bind.valuep)^ := Value;
    SQLT_DAT:   begin
                  DecodeDate(Value, TS.Year, TS.Month, TS.Day); //oracle does not accept 0 dates
                  POraDate(Bind^.valuep).Cent   := TS.Year div 100 +100;
                  POraDate(Bind^.valuep).Year   := TS.Year mod 100 +100;
                  POraDate(Bind^.valuep).Month  := TS.Month;
                  PInteger(@POraDate(Bind^.valuep).Day)^ := 0; //init all remaining fields to 0 with one 4Byte value
                  POraDate(Bind^.valuep).Day    := TS.Day;
                end;
    SQLT_TIMESTAMP: begin
                  DecodeDate(Value, TS.Year, TS.Month, TS.Day); //oracle does not accept 0 dates
                  DecodeTime(Value, TS.Hour, TS.Minute, TS.Second, PWord(@TS.Fractions)^);
                  TS.Fractions := Word(TS.Fractions) * 1000000;
                  Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
                    FOCIError, PPOCIDescriptor(Bind.valuep)^, TS.Year, TS.Month, TS.Day,
                      TS.Hour, TS.Minute, TS.Second, TS.Fractions, nil, 0);
                  if Status <> OCI_SUCCESS then
                    FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIDateTimeConstruct', Self);
                end;
    SQLT_INT,
    SQLT_UIN:   {$IFDEF CPU64}
                BindSInteger(Index, SQLType, Trunc(Value));
                {$ELSE}
                SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Trunc(Value));
                {$ENDIF}
    else SetRaw;
  end;
  Bind.indp[0] := 0;
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.
  The codepage is equal to the raw codepage of the operating system.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractOraclePreparedStatement.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  if (FCharSetID = OCI_UTF16ID) or (ZOSCodePage <> FClientCP) then begin
    FUniTemp := ZRawToUnicode(Value, ZOSCodePage);
    SetUnicodeString(ParameterIndex, FUniTemp);
  end else SetRawByteString(ParameterIndex, Value);
end;
{$ENDIF NO_ANSISTRING}

procedure TZAbstractOraclePreparedStatement.SetBigDecimal(Index: Integer;
  const Value: TBCD);
var
  Bind: PZOCIParamBind;
  status: sword;
  SQLType: TZSQLType;
  Msec: Word;
  TS: TZTimeStamp;
  I64: Int64;
  U64: UInt64 absolute I64;
  Dbl: Double absolute i64;
  procedure SetRaw;
  begin
    SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(BCDToStr(Value)))
  end;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (stBigDecimal <> BindList[Index].SQLType)
  then SQLType := BindList[Index].SQLType
  else SQLType := stBigDecimal;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  case Bind.dty of
    SQLT_VNU:     begin
                    Bind.indp[0] := BCD2Nvu(Value, POCINUmber(Bind.valuep));
                    Exit;
                  end;
    SQLT_BFLOAT:  PSingle(Bind.valuep)^ := BCDToDouble(Value);
    SQLT_BDOUBLE: PDouble(Bind.valuep)^ := BCDToDouble(Value);
    SQLT_DAT:   begin
                  Dbl := BCDToDouble(Value);
                  DecodeDate(Dbl, PZDate(fByteBuffer).Year, TS.Month, TS.Day); //oracle does not accept 0 dates
                  POraDate(Bind^.valuep).Cent   := TS.Year div 100 +100;
                  POraDate(Bind^.valuep).Year   := TS.Year mod 100 +100;
                  POraDate(Bind^.valuep).Month  := TS.Month;
                  PInteger(@POraDate(Bind^.valuep).Day)^ := 0; //init all remaining fields to 0 with one 4Byte value
                  POraDate(Bind^.valuep).Day    := TS.Day;
                end;
    SQLT_TIMESTAMP: begin
                  Dbl := BCDToDouble(Value);
                  DecodeDate(Dbl, TS.Year, TS.Month, TS.Day); //oracle does not accept 0 dates
                  DecodeTime(Dbl, TS.Hour, TS.Minute, TS.Second, Msec);
                  TS.Fractions := Msec * 1000000;
                  Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
                    FOCIError, PPOCIDescriptor(Bind.valuep)^, TS.Year, TS.Month, TS.Day,
                      TS.Hour, TS.Minute, TS.Second, TS.Fractions, nil, 0);
                  if Status <> OCI_SUCCESS then
                    FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIDateTimeConstruct', Self);
                end;

    SQLT_INT:   begin
                  i64 := BCD2Int64(Value);
                  {$IFDEF CPU64}
                  BindSInteger(Index, stLong, i64);
                  {$ELSE}
                  SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, i64);
                  {$ENDIF}
                end;
    SQLT_UIN:   begin
                  U64 := BCD2UInt64(Value);
                  {$IFDEF CPU64}
                  BindUInteger(Index, stULong, U64);
                  {$ELSE}
                  SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, U64);
                  {$ENDIF}
                end;
    else        SetRaw;
  end;
  Bind.indp[0] := 0;
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetBoolean(Index: Integer;
  Value: Boolean);
begin
  BindUInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBoolean, Ord(Value));
end;

{**
  Sets the designated parameter to a Java <code>unsigned 8Bit int</code> value.
  The driver converts this
  to an SQL <code>BYTE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetByte(Index: Integer; Value: Byte);
begin
  BindUInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stByte, Cardinal(Value));
end;

{**
  Sets the designated parameter to a Java array of bytes by reference.
  The driver converts this to an SQL <code>VARBINARY</code> or
  <code>LONGVARBINARY</code> (depending on the argument's size relative to
  the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the parameter value address
  @param Len the length of the addressed value
}
procedure TZAbstractOraclePreparedStatement.SetBytes(Index: Integer; Value: PByte;
  Len: NativeUInt);
var
  Bind: PZOCIParamBind;
  SQLType: TZSQLType;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType)
  then SQLType := BindList[Index].SQLType
  else SQLType := stBytes;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (ub4(Bind.value_sz) < Len+SizeOf(Integer)) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1, Len);
  POCILong(Bind.valuep).Len := Len;
  if Value <> nil then
    Move(Value^, POCILong(Bind.valuep).data[0], Len);
  Bind.indp[0] := -ShortInt(Value = nil);
end;

{**
  Sets the designated parameter to <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  if Value.CP = zCP_UTF16 then
    SetPWideChar(ParameterIndex, Value.P, Value.Len)
  else if (Value.CP = FClientCP) then
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value.P, Value.Len)
  else begin
    FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
    SetUnicodeString(ParameterIndex, FUniTemp);
  end;
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var
  Bind: PZOCIParamBind;
  SQLType: TZSQLType;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType)
  then SQLType := BindList[Index].SQLType
  else SQLType := stCurrency;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  if Bind.dty = SQLT_VNU then
    Curr2vnu(Value, POCINumber(Bind.valuep))
  else if Bind.dty = SQLT_BDOUBLE then
    PDouble(Bind.valuep)^ := Value
  else begin
    CurrToRaw(Value, PAnsiChar(@POCIVary(Bind.valuep).data[0]), @P);
    POCIVary(Bind.valuep).Len := P-@POCIVary(Bind.valuep).data[0];
  end;
  Bind.indp[0] := 0;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "TS,BCD does not seem to be initialized} {$ENDIF}
procedure TZAbstractOraclePreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType);
var
  Bind: PZOCIParamBind; //ora bind variable
  I, BufferSize: Integer;
  ArrayLen: Cardinal;
  ClientCP: Word;
  P: PAnsiChar;
  DT: TDateTime;
  TS: TZTimeStamp;
  D: TZDate absolute TS;
  PTS: PZTimeStamp;
  PD: PZDate absolute PTS;
  Status: sword;
  OraDate: POraDate;
label bind_direct;
  {$R-}
  procedure SetLobs;
  var I: Integer;
    Lob: IZBLob;
    OCIlob: IZOracleLob;
    OraLobs: TInterfaceDynArray;
    Blob: IZBlob;
    Clob: IZClob;
    Arr: TZArray;
  label write_lob;
  begin
    {$IFDEF WITH_VAR_INIT_WARNING}OraLobs := nil;{$ENDIF}
    SetLength(OraLobs, ArrayLen);
    Arr := PZArray(BindList[ParameterIndex].Value)^;
    Arr.VArray := Pointer(OraLobs);
    BindList.Put(ParameterIndex, Arr, True);
    if SQLType = stBinaryStream then begin
      if (Bind.dty <> SQLT_BLOB) or (Bind.value_sz <> SizeOf(POCIDescriptor)) or (Bind.curelen <> ArrayLen) then
        InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(POCIDescriptor));
      for i := 0 to ArrayLen -1 do
        if (TInterfaceDynArray(Value)[I] <> nil) and Supports(TInterfaceDynArray(Value)[I], IZLob, Lob) and not Lob.IsEmpty and
            Supports(Lob, IZBlob, Blob) and not Supports(Lob, IZOracleLob, OCILob) then begin
write_lob:OciLob := TZOracleBlob.CreateFromBlob(Blob, nil, FOracleConnection, FOpenLobStreams);
          PPOCIDescriptor(PAnsiChar(Bind.valuep)+SizeOf(Pointer)*I)^ := OciLob.GetLobLocator;
          OraLobs[i] := OciLob; //destroy old interface or replace it
        {$R-}
          Bind.indp[i] := 0;
        end else
          Bind.indp[i] := -1;
        {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
    end else begin
      if (Bind.dty <> SQLT_CLOB) or (Bind.value_sz <> SizeOf(POCIDescriptor)) or (Bind.curelen <> ArrayLen) then
        InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(POCIDescriptor));
      for i := 0 to ArrayLen -1 do
        if (TInterfaceDynArray(Value)[I] <> nil) and Supports(TInterfaceDynArray(Value)[I], IZBlob, Lob) and not Lob.IsEmpty then begin
          if Supports(Lob, IZCLob, CLob) then
            CLob.SetCodePageTo(ClientCP)
          else begin
            Clob := ZDbcResultSet.CreateRawCLobFromBlob(Lob, ConSettings, FOpenLobStreams);
            TInterfaceDynArray(Value)[I] := CLob;
            Lob := Clob;
          end;
          if not Supports(Lob, IZOracleLob, OCILob) then begin
            OciLob := TZOracleClob.CreateFromClob(Clob, nil, SQLCS_IMPLICIT, 0, FOracleConnection, FOpenLobStreams);
            PPOCIDescriptor(PAnsiChar(Bind.valuep)+SizeOf(Pointer)*I)^ := OciLob.GetLobLocator;
            TInterfaceDynArray(Value)[I] := CLob;
          end;

          goto write_lob;
        end else
          Bind.indp[i] := -1;
    end;
  end;
  procedure BindRawStrings(const ClientStrings: TRawByteStringDynArray);
  var BufferSize, I: Integer;
  begin
    BufferSize := 0;
    for i := 0 to ArrayLen -1 do
      if Pointer(ClientStrings[i]) <> nil then
        {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
        BufferSize := Max(BufferSize, Length(ClientStrings[I]) -1);
        {$ELSE}
        BufferSize := Max(BufferSize, {%H-}PLengthInt(NativeUInt(ClientStrings[I]) - StringLenOffSet)^);
        {$ENDIF}
    if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen <> ArrayLen) then
      InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
    P := Bind.valuep;
    for i := 0 to ArrayLen -1 do begin
      if (Pointer(ClientStrings[I]) = nil) then
        POCILong(P).Len := 0
      else begin
        {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
        POCILong(P).Len := Length(ClientStrings[I]) -1;
        {$ELSE}
        POCILong(P).Len := {%H-}PLengthInt(NativeUInt(ClientStrings[I]) - StringLenOffSet)^;
        {$ENDIF}
        Move(Pointer(ClientStrings[i])^,POCILong(P).data[0], POCILong(P).Len);
      end;
      Inc(P, Bind.value_sz);
    end;
  end;
  procedure BindRawFromUnicodeStrings;
  var BufferSize, I: Integer;
  begin
    BufferSize := 0;
    for i := 0 to ArrayLen -1 do
      BufferSize := Max(BufferSize, Length(TUnicodeStringDynArray(Value)[I]));
    BufferSize := (BufferSize shl 2);
    if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen <> ArrayLen) then
      InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
    P := Bind.valuep;
    for i := 0 to ArrayLen -1 do begin
      POCILong(P).Len := Length(TUnicodeStringDynArray(Value)[I]);
      if POCILong(P).Len > 0 then
        POCILong(P).Len := PUnicode2PRawBuf(Pointer(TUnicodeStringDynArray(Value)[I]), @POCILong(P).data[0], POCILong(P).Len, BufferSize, ClientCP);
      Inc(P, Bind.value_sz);
    end;
  end;
  procedure MoveUnicodeStrings;
  var BufferSize, I: Integer;
  begin
    BufferSize := 0;
    for i := 0 to ArrayLen -1 do
      BufferSize := Max(BufferSize, Length(TUnicodeStringDynArray(Value)[I]));
    BufferSize := (BufferSize shl 1);
    if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen <> ArrayLen) then
      InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
    P := Bind.valuep;
    for i := 0 to ArrayLen -1 do begin
      POCILong(P).Len := Length(TUnicodeStringDynArray(Value)[I]) shl 1;
      if POCILong(P).Len > 0 then
        Move(Pointer(TUnicodeStringDynArray(Value)[I])^, POCILong(P).data[0], POCILong(P).Len);
      Inc(P, Bind.value_sz);
    end;
  end;
  procedure BindRawFromCharRec;
  var BufferSize, I: Integer;
    SQLType: TZSQLType;
    CP: Word;
  begin
    BufferSize := 0;
    for i := 0 to ArrayLen -1 do
      BufferSize := Max(BufferSize, TZCharRecDynArray(Value)[I].Len);
    CP := TZCharRecDynArray(Value)[0].CP;
    if (CP = zCP_UTF16) or (CP = zCP_UTF8) or (CP <> FClientCP) then begin
      BufferSize := BufferSize shl 1;
      SQLType := stUnicodeString;
    end else begin
      SQLType := stString;
      if CP <> ClientCP then
        BufferSize := BufferSize shl 2; //oversized for best fit
    end;
    if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen <> ArrayLen) then
      InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
    P := Bind.valuep;
    if SQLType = stString then for i := 0 to ArrayLen -1 do begin
        if CP = ClientCP then begin
          POCILong(P).Len := TZCharRecDynArray(Value)[I].Len;
          Move(TZCharRecDynArray(Value)[I].P^,POCILong(P).data[0], POCILong(P).Len);
        end else POCILong(P).Len := ZEncoding.PRawToPRawBuf(TZCharRecDynArray(Value)[I].P,
          @POCILong(P).data[0], TZCharRecDynArray(Value)[I].Len, BufferSize, CP, ClientCP);
        Inc(P, Bind.value_sz);
    end else for i := 0 to ArrayLen -1 do begin
        if CP = zCP_UTF16 then begin
          POCILong(P).Len := TZCharRecDynArray(Value)[I].Len shl 1;
          if POCILong(P).Len > 0 then
            Move(TZCharRecDynArray(Value)[I].P^, POCILong(P).data[0], POCILong(P).Len);
        end else
          POCILong(P).Len := ZEncoding.PRaw2PUnicodeBuf(TZCharRecDynArray(Value)[I].P, @POCILong(P).data[0], TZCharRecDynArray(Value)[I].Len, CP) shl 1;
        Inc(P, Bind.value_sz);
      end;
  end;

  procedure BindFromAutoEncode;
  var ClientStrings: TRawByteStringDynArray;
    var I: Integer;
  begin
    {$IFDEF WITH_VAR_INIT_WARNING}ClientStrings := nil;{$ENDIF}
    SetLength(ClientStrings, ArrayLen);
      for i := 0 to ArrayLen -1 do
         if (Pointer(TStringDynArray(Value)[I]) <> nil) then
    BindRawStrings(ClientStrings);
  end;
  procedure BindConvertedRaw2RawStrings(CP: Word);
  var BufferSize, I: Integer;
  begin
    BufferSize := 0;
    for i := 0 to ArrayLen -1 do
      if Pointer(TRawByteStringDynArray(Value)[i]) <> nil then
        {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
        BufferSize := Max(BufferSize, Length(TRawByteStringDynArray(Value)[I]) -1);
        {$ELSE}
        BufferSize := Max(BufferSize, {%H-}PLengthInt(NativeUInt(TRawByteStringDynArray(Value)[I]) - StringLenOffSet)^);
        {$ENDIF}
    BufferSize := BufferSize shl 1; //oversized
    if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen <> ArrayLen) then
      InitBuffer(stUnicodeString, Bind, ParameterIndex, ArrayLen, BufferSize);
    P := Bind.valuep;
    for i := 0 to ArrayLen -1 do begin
      POCILong(P).Len := ZEncoding.PRaw2PUnicodeBuf(Pointer(TRawByteStringDynArray(Value)[I]),
        @POCILong(P).data[0], Length(TRawByteStringDynArray(Value)[I]), CP);
      Inc(P, Bind.value_sz);
    end;
  end;
  procedure UTF8ToUTF16Strings;
  var BufferSize, I: Integer;
    r: PAnsiChar;
  begin
    BufferSize := 0;
    for i := 0 to ArrayLen -1 do
      if Pointer(TRawByteStringDynArray(Value)[i]) <> nil then
        {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
        BufferSize := Max(BufferSize, Length(TRawByteStringDynArray(Value)[I]) -1);
        {$ELSE}
        BufferSize := Max(BufferSize, {%H-}PLengthInt(NativeUInt(TRawByteStringDynArray(Value)[I]) - StringLenOffSet)^);
        {$ENDIF}
    BufferSize := BufferSize shl 1; //oversized
    if (Bind.dty <> SQLT_LVC) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen <> ArrayLen) then
      InitBuffer(stUnicodeString, Bind, ParameterIndex, ArrayLen, BufferSize);
    P := Bind.valuep;
    for i := 0 to ArrayLen -1 do begin
      r := Pointer(TRawByteStringDynArray(Value)[I]);
      if r = nil
      then POCILong(P).Len := 0
      else POCILong(P).Len := ZEncoding.UTF8ToWideChar(r,
        Length(TRawByteStringDynArray(Value)[I]), @POCILong(P).data[0]) shl 1;
      Inc(P, Bind.value_sz);
    end;
  end;
begin
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  Bind := @FOraVariables[ParameterIndex];
  ClientCP := ConSettings^.ClientCodePage.CP;
  ArrayLen := {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
  case SQLType of
    stBoolean, stByte, stShort, stWord, stSmall, stLongWord, stInteger, stFloat, stDouble: begin
bind_direct:
        InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen);
        if BatchDMLArrayCount > 1 then begin
          Status := FPlainDriver.OCIBindByPos(FOCIStmt, Bind.bindpp, FOCIError, ParameterIndex + 1,
            Pointer(Value), Bind.value_sz, Bind.dty, Bind.indp, nil, nil, 0, nil, OCI_DEFAULT);
          if Status <> OCI_SUCCESS then
            FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIBindByPos', Self);
        end else case Bind.value_sz of
          8: PDouble(Bind^.valuep)^ := TDoubleDynArray(Value)[0];
          4: PCardinal(Bind^.valuep)^ := TCardinalDynArray(Value)[0];
          2: PWord(Bind^.valuep)^ := TWordDynArray(Value)[0];
          else PByte(Bind^.valuep)^ := TByteDynArray(Value)[0];
        end;
      end;
    stLong, stULong: //old oracle does not support 8 byte ordinals
        if FCanBindInt64 and (VariantType <> vtRawByteString) then
          goto bind_direct
        else begin
          if (Bind.dty <> SQLT_VNU) or (Bind.value_sz <> SizeOf(TOciNumber)) or (Bind.curelen <> ArrayLen) then
            InitBuffer(stBigDecimal, Bind, ParameterIndex, ArrayLen, 20);
          P := Bind.valuep;
          if (VariantType <> vtRawByteString) then begin
            if SQLType = stLong then
              for i := 0 to ArrayLen -1 do begin
                FPlainDriver.OCINumberFromInt(FOCIError, @TInt64DynArray(Value)[i],
                  SizeOf(Int64), OCI_NUMBER_SIGNED, POCINumber(P));
                Inc(P, Bind.value_sz);
              end
            else
              for i := 0 to ArrayLen -1 do begin
                FPlainDriver.OCINumberFromInt(FOCIError, @TUInt64DynArray(Value)[i],
                  SizeOf(Int64), OCI_NUMBER_UNSIGNED, POCINumber(P));
                Inc(P, Bind.value_sz);
              end;
          end else begin

          end;
        end;
    stCurrency: begin
        if (Bind.dty <> SQLT_VNU) or (Bind.value_sz <> SizeOf(TOCINumber)) or (Bind.curelen <> ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(TOCINumber));
        for i := 0 to ArrayLen -1 do
          Curr2vnu(TCurrencyDynArray(Value)[i], POCINumber(Bind.valuep+I*SizeOf(TOCINumber)));
      end;
    stBigDecimal: begin
        if (Bind.dty <> SQLT_VNU) or (Bind.value_sz <> SizeOf(TOCINumber)) or (Bind.curelen <> ArrayLen) then
          //note as long we do not have a Value2OraNumber conversion we'll use the ora double instead!!
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(TOCINumber));
        for i := 0 to ArrayLen -1 do
          {$R-}
          Bind.indp[I] := BCD2Nvu(TBCDDynArray(Value)[i], POCINumber(Bind.valuep+I*SizeOf(TOCINumber)));
          {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
        Exit;
      end;
    stDate: begin
        if (Bind.dty <> SQLT_DAT) or (Bind.value_sz <> SizeOf(TOraDate)) or (Bind.curelen <> ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(TOraDate));
        FillChar(Bind^.valuep^, ArrayLen*SizeOf(TOraDate), #0);
        for i := 0 to ArrayLen -1 do begin
          if (VariantType in [vtNull, vtDateTime]) then begin
            DecodeDateTimeToDate(TDateTimeDynArray(Value)[I], D);
            PD := @D;
          end else if VariantType = vtDate then
            PD := @TZDateDynArray(Value)[I]
          else begin
            DT := ArrayValueToDate(BindList[ParameterIndex].Value, I, ConSettings^.WriteFormatSettings);
            DecodeDateTimeToDate(DT, D);
            PD := @D;
          end;
          OraDate := POraDate(Bind^.valuep+I*SizeOf(TOraDate));
          OraDate.Cent  := PD^.Year div 100 + 100;
          OraDate.Year  := PD^.Year mod 100 + 100;
          OraDate.Month := PD^.Month;
          OraDate.Day   := PD^.Day;
        end;
      end;
    stTime, stTimeStamp: begin //msec precision -> need a descriptor
        if (Bind.dty <> SQLT_TIMESTAMP) or (Bind.value_sz <> SizeOf(POCIDescriptor)) or (Bind.curelen <> ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, SizeOf(SizeOf(POCIDescriptor)));
        for i := 0 to ArrayLen -1 do begin
          if VariantType = vtTimeStamp then
            PTS := @TZTimeStampDynArray(Value)[i]
          else begin
            PTS := @TS;
            if (VariantType in [vtNull, vtDateTime]) then
              DecodeDateTimeToTimeStamp(TDateTimeDynArray(Value)[i], TS)
            else if VariantType = vtTime then
              ZSysUtils.TimeStampFromTime(TZTimeDynArray(Value)[i], TS)
            else begin
              DT := ArrayValueToDatetime(BindList[ParameterIndex].Value, I, ConSettings^.WriteFormatSettings);
              DecodeDateTimeToTimeStamp(DT, TS);
            end;
          end;
          Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
              FOCIError, PPOCIDescriptor(Bind^.valuep+I*SizeOf(POCIDescriptor))^, //direct addressing descriptor to array. So we don't need to free the mem again
              PTS^.Year, PTS^.Month, PTS^.Day, PTS^.Hour, PTS^.Minute, PTS^.Second, PTS^.Fractions, nil, 0);
          if Status <> OCI_SUCCESS then
            FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIDateTimeConstruct', Self);
        end;
      end;
    stGUID: begin
        if (Bind.dty <> SQLT_AFC) or (Bind.value_sz <> StrGUIDLen) or (Bind.curelen <> ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen);
        for i := 0 to ArrayLen -1 do
          GUIDToBuffer(@TGUIDDynArray(Value)[I], (Bind.valuep+I*StrGUIDLen), []);
      end;
    stBytes: begin
        BufferSize := 0;
        for i := 0 to ArrayLen -1 do
          BufferSize := Max(BufferSize, Length(TBytesDynArray(Value)[I]));
        if (Bind.dty <> SQLT_LVB) or (Bind.value_sz < BufferSize+SizeOf(Integer)) or (Bind.curelen <> ArrayLen) then
          InitBuffer(SQLType, Bind, ParameterIndex, ArrayLen, BufferSize);
        for i := 0 to ArrayLen -1 do begin
          BufferSize := Length(TBytesDynArray(Value)[I]);
          PInteger(Bind.valuep+I*Bind.value_sz)^ := BufferSize;
          if BufferSize > 0 then
            Move(Pointer(TBytesDynArray(Value)[I])^,(Bind.valuep+I*Bind.value_sz+SizeOf(Integer))^, BufferSize);
        end;
    end;
    stString, stUnicodeString:
      case VariantType of
        {$IFNDEF UNICODE}
        vtString:
          if not ConSettings.AutoEncode
          then BindRawStrings(TRawByteStringDynArray(Value))
          else BindFromAutoEncode;
        {$ENDIF}
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:  if (ClientCP = ZOSCodePage)
            then BindRawStrings(TRawByteStringDynArray(Value))
            else BindConvertedRaw2RawStrings(ZOSCodePage);
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String: if FCharSetID  = OCI_UTF16ID
            then UTF8ToUTF16Strings
            else BindRawStrings(TRawByteStringDynArray(Value));
        {$ENDIF}
        vtRawByteString: BindRawStrings(TRawByteStringDynArray(Value));
        vtCharRec: BindRawFromCharRec;
        {$IFDEF UNICODE}vtString,{$ENDIF}
        vtUnicodeString: if FCharSetID  = OCI_UTF16ID
          then MoveUnicodeStrings
          else BindRawFromUnicodeStrings;
        else raise Exception.Create('Unsupported String Variant');
      end;
    stAsciiStream, stUnicodeStream, stBinaryStream: begin
        SetLobs;
        Exit;
      end;
    else raise ZDbcUtils.CreateUnsupportedParameterTypeException(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType);
  end;
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  FillChar(Bind.indp^, SizeOf(SB2)*ArrayLen, #0);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5036 off : Local variable DT does not seem to be initialized} {$ENDIF}
procedure TZAbstractOraclePreparedStatement.SetDate(Index: Integer;
  const Value: TZDate);
var
  Bind: PZOCIParamBind;
  DT: TDateTime;
  Status: sword absolute DT;
  OraType: TZSQLType absolute DT;
  Len: LengthInt absolute DT;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (stDate <> BindList[Index].SQLType)
  then OraType := BindList[Index].SQLType
  else OraType := stDate;
  if (BindList[Index].SQLType <> OraType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(OraType, Bind, Index, 1);
  case Bind.dty of
    SQLT_DAT:   begin
                  POraDate(Bind^.valuep).Cent   := Value.Year div 100 +100;
                  POraDate(Bind^.valuep).Year   := Value.Year mod 100 +100;
                  POraDate(Bind^.valuep).Month  := Value.Month;
                  PInteger(@POraDate(Bind^.valuep).Day)^ := 0; //init all remaining fields to 0 with one 4Byte value
                  POraDate(Bind^.valuep).Day    := Value.Day;
                end;
    SQLT_TIMESTAMP: begin
                  Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
                    FOCIError, PPOCIDescriptor(Bind.valuep)^, Value.Year, Value.Month, Value.Day,
                      0, 0, 0, 0, nil, 0);
                  if Status <> OCI_SUCCESS then
                    FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIDateTimeConstruct', Self);
                end;
    SQLT_CLOB,
    SQLT_LVC: begin
                Len := DateToRaw(Value.Year, Value.Month, Value.Day, PAnsiChar(FByteBuffer),
                  ConSettings^.WriteFormatSettings.DateFormat, True, Value.IsNegative);
                BindRawStr(Index, PAnsiChar(FByteBuffer), Len);
                Exit;
              end;
    else      begin
                if TryDateToDateTime(Value, DT)
                then InternalBindDouble(Index, stDate, DT)
                else BindSInteger(Index, stDate, 1);
                Exit;
              end;
  end;
  Bind.indp[0] := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
begin
  InternalBindDouble(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stDouble, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetFloat(Index: Integer; Value: Single);
begin
  InternalBindDouble(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stFloat, Value);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable BCD does not seem to be initialized} {$ENDIF}
procedure TZAbstractOraclePreparedStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZRawSQLStringWriter; var Result: RawByteString);
var
  Bind: PZOCIParamBind;
  BCD: TBCD;
  TS: TZTimeStamp absolute BCD;
  P:  Pointer absolute TS;
  PA: PAnsiChar absolute P;
  PW: PWideChar absolute P;
  DT: TDateTime;
  L: NativeUInt absolute DT;
  Year: SB2;
  Month, Day: Ub1;
  Hour: ub1 absolute Year;
  Minute: ub1 absolute Month;
  Second: ub1 absolute Day;
label jmpTestW2A;
begin
  {$R-}
  Bind := @FOraVariables[ParamIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Bind.curelen > 1 then
    Result := '(ARRAY)'
  else if Bind.indp[0] = -1 then
    Result := '(NULL)'
  else case Bind.dty of
    SQLT_VNU: begin
                Nvu2BCD(POCINumber(Bind.valuep), Bcd);
                SQLWriter.AddDecimal(Bcd, Result);
              end;
    SQLT_INT: if Bind.value_sz = SizeOf(Int64) then
                SQLWriter.AddOrd(PInt64(Bind.valuep)^, Result)
              else if Bind.value_sz = SizeOf(Integer) then
                SQLWriter.AddOrd(PInteger(Bind.valuep)^, Result)
              else
                SQLWriter.AddOrd(PSmallInt(Bind.valuep)^, Result);
    SQLT_UIN: if Bind.value_sz = SizeOf(UInt64) then
                SQLWriter.AddOrd(PUInt64(Bind.valuep)^, Result)
              else if Bind.value_sz = SizeOf(Cardinal) then
                SQLWriter.AddOrd(PCardinal(Bind.valuep)^, Result)
              else
                SQLWriter.AddOrd(PWord(Bind.valuep)^, Result);
    SQLT_BFLOAT: SQLWriter.AddFloat(PSingle(Bind.valuep)^, Result);
    SQLT_BDOUBLE: SQLWriter.AddFloat(PDouble(Bind.valuep)^, Result);
    SQLT_DAT: begin
                DT := EncodeDate((POraDate(Bind.valuep).Cent-100)*100+(POraDate(Bind.valuep).Year-100),
                  POraDate(Bind.valuep).Month,POraDate(Bind.valuep).Day);
                SQLWriter.AddDate(DT, ConSettings.WriteFormatSettings.DateFormat, Result);
              end;
    SQLT_TIMESTAMP: begin
            FPlainDriver.OCIDateTimeGetDate(FOracleConnection.GetConnectionHandle, FOCIError,
              PPOCIDescriptor(Bind.valuep)^, Year, Month, Day);
            TS.Year := Abs(Year);
            TS.IsNegative := Year < 0;
            TS.Month := Month;
            TS.Day := Day;
            FPlainDriver.OCIDateTimeGetTime(FOracleConnection.GetConnectionHandle, FOCIError,
              PPOCIDescriptor(Bind.valuep)^, Hour, Minute, Second, Ts.Fractions);
            TS.Hour := Hour;
            TS.Minute := Minute;
            TS.Second := Second;
            SQLWriter.AddTimeStamp(TS, ConSettings.WriteFormatSettings.DateTimeFormat, Result);
            PCardinal(@TS.TimeZoneHour)^ := 0;
          end;
    SQLT_AFC: begin
                PW := Pointer(Bind.valuep);
                L := Bind.Value_sz;
                goto jmpTestW2A;
              end;
    SQLT_VCS: begin
                PW := @POCIVary(Bind.valuep).data[0];
                L  := POCIVary(Bind.valuep).Len;
                goto jmpTestW2A;
              end;
    SQLT_LVC: begin
                P := @POCILong(Bind.valuep).data[0];
                L  := POCILong(Bind.valuep).Len;
jmpTestW2A:     if BindList[ParamIndex].SQLType = stUnicodeString then begin
                  L := L shr 1;
                  fRawTemp := PUnicodeToRaw(PW, L, zCP_UTF8);
                  L := Length(fRawTemp);
                  P := Pointer(fRawTemp);
                end;
                SQLWriter.AddTextQuoted(PA, L, AnsiChar(#39), Result);
                fRawTemp := EmptyRaw;
              end;
    SQLT_LVB: SQLWriter.AddHexBinary(@POCILong(Bind.valuep).data[0], POCILong(Bind.valuep).Len, False, Result);
    SQLT_CLOB: if BindList[ParamIndex].SQLType = stAsciiStream
               then SQLWriter.AddText('(CLOB)', Result)
               else SQLWriter.AddText('(NCLOB)', Result);
    SQLT_BLOB: SQLWriter.AddText('(BLOB)', Result);
    else       SQLWriter.AddText('(UNKNOWN)', Result);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetInt(Index, Value: Integer);
begin
  BindSInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stInteger, Value);
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractOraclePreparedStatement.SetLong(Index: Integer;
  const Value: Int64);
{$IFDEF CPU64}
begin
  BindSInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLong, Value);
{$ELSE}
var
  Bind: PZOCIParamBind;
  P: PAnsiChar;
  Status: sword;
  SQLType: TZSQLType;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (stLong <> BindList[Index].SQLType) //keep registered types alive
  then SQLType := BindList[Index].SQLType
  else SQLType := stLong;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  case Bind.dty of
    SQLT_VNU: begin
        Status := FPlainDriver.OCINumberFromInt(FOCIError, @Value, SizeOf(NativeInt), OCI_NUMBER_SIGNED, POCINumber(Bind.valuep));
        if Status <> OCI_SUCCESS then
          FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCINumberFromInt', Self);
      end;
    SQLT_INT: if Bind.value_sz = SizeOf(Int64) then
                PInt64(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Integer) then
                PInteger(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(SmallInt) then
                PSmallInt(Bind.valuep)^ := Value
              else
                PShortInt(Bind.valuep)^ := Value;
    SQLT_UIN: if Bind.value_sz = SizeOf(UInt64) then
                PUInt64(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Cardinal) then
                PCardinal(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Word) then
                PWord(Bind.valuep)^ := Value
              else
                PByte(Bind.valuep)^ := Value;
    SQLT_BFLOAT,
    SQLT_BDOUBLE,
    SQLT_DAT,
    SQLT_TIMESTAMP: InternalBindDouble(Index, SQLtype, Value);
    SQLT_LVC: begin
                IntToRaw(Value, PAnsiChar(@POCIVary(Bind.valuep).data[0]), @P);
                POCIVary(Bind.valuep).Len := P-@POCIVary(Bind.valuep).data[0];
              end
  end;
  Bind.indp[0] := 0;
{$ENDIF}
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractOraclePreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
var
  Bind: PZOCIParamBind;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) then
    SQLType := BindList[Index].SQLType
  else if SQLType = stUnknown then
    SQLType := stInteger;

  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  Bind.indp[0] := -1;
end;

procedure TZAbstractOraclePreparedStatement.SetNullArray(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value; const VariantType: TZVariantType);
var I: Cardinal;
  Bind: PZOCIParamBind;
  P: PZArray;
begin
  inherited SetNullArray(ParameterIndex, SQLType, Value, VariantType);
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex - 1;
  {$ENDIF}
  {$R-}
  Bind := @FOraVariables[ParameterIndex];
  P := BindList[ParameterIndex].Value;
  for i := 0 to {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFNDEF FPC}-1{$ENDIF} do
    Bind.indp[I] := -Ord(ZDbcUtils.IsNullFromArray(P, i));
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable BCD does not seem to be initialized} {$ENDIF}
procedure TZAbstractOraclePreparedStatement.SetPWideChar(Index: Integer;
  Value: PWideChar; WLen: NativeUInt);
var
  Bind: PZOCIParamBind;
  SQLType: TZSQLType;
  BCD: TBCD;
  DT: TZDate absolute BCD;
  TS: TZTimeStamp absolute BCD;
  ByteSize: NativeInt absolute BCD;
  procedure AsLob;
  var Lob: IZCLob;
  begin
    Lob := TZOracleClob.Create(FOracleConnection, nil, SQLCS_NCHAR, OCI_UTF16ID, FOpenLobStreams);
    try
      Lob.SetPWideChar(Value, WLen);
      BindLob(Index, stUnicodeStream, Lob);
    finally
      Lob := nil;
    end;
  end;
label jmpFail;
begin
  if Value = nil then begin
    SetNull(Index, stUnicodeString);
    Exit;
  end;
  {$IFNDEF GENERIC_INDEX}Index := Index -1;{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType)
  then SQLType := BindList[Index].SQLType
  else SQLType := stUnicodeString;
  if SQLType = stString then
    SQLType := stUnicodeString;
  ByteSize := WLen shl 1;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) or (Bind.value_sz < ByteSize +SizeOf(Integer) ) then
    InitBuffer(SQLType, Bind, Index, 1, ByteSize);
  case Bind.dty of
    SQLT_VNU: if TryUniToBcd(Value, Wlen, BCD, '.')
        then BCD2Nvu(BCD, POCINumber(Bind.valuep))
        else goto jmpFail;
    SQLT_INT: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, UnicodeToInt64(Value, Value+WLen));
    SQLT_UIN: SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, UnicodeToUInt64(Value, Value+WLen));
    SQLT_BFLOAT:  SQLStrToFloatDef(Value, 0, PSingle(Bind.valuep)^, Wlen);
    SQLT_BDOUBLE: SQLStrToFloatDef(Value, 0, PDouble(Bind.valuep)^, Wlen);
    SQLT_DAT: if TryPCharToDate(Value, WLen, ConSettings.WriteFormatSettings, DT)
              then SetDate(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, DT)
              else goto jmpFail;
    SQLT_TIMESTAMP: if TryPCharToTimeStamp(Value, WLen, ConSettings.WriteFormatSettings, TS)
              then SetTimeStamp(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TS)
              else goto jmpFail;
    SQLT_LVC: begin
                POCILong(Bind.valuep).Len := ByteSize;
                if WLen > 0 then
                  Move(Value^, POCILong(Bind.valuep).data[0], ByteSize);
              end;
    SQLT_CLOB: AsLob;
    else
jmpFail: CreateConversionError({$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stUnicodeString, SQLType);
  end;
  Bind.indp[0] := 0;
  {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
procedure TZAbstractOraclePreparedStatement.SetRawByteString(ParameterIndex: Integer;
  const Value: RawByteString);
var P: PAnsiChar;
  L: NativeUint;
begin
  if (FCharSetID = OCI_UTF16ID) then begin
    FUniTemp := ZRawToUnicode(Value, ConSettings.CTRL_CP);
    SetUnicodeString(ParameterIndex, FUniTemp);
  end else begin
    L := Length(Value);
    if L = 0
    then P := PEmptyAnsiString
    else P := Pointer(Value);
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, P, L);
  end;
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetShort(Index: Integer; Value: ShortInt);
begin
  BindSInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stShort, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetSmall(Index: Integer; Value: SmallInt);
begin
  BindSInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stSmall, Value);
end;

{**
  Sets the designated parameter to a TZCharRec value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  SetUnicodeString(ParameterIndex, Value);
  {$ELSE}
  if FCharSetID = OCI_UTF16ID then begin
    FUniTemp := ZRawToUnicode(Value, ConSettings^.Ctrl_CP);
    SetUnicodeString(ParameterIndex, FUniTemp);
  end else begin
    FRawTemp := ConSettings^.ConvFuncs.ZStringtoRaw(Value, ConSettings^.Ctrl_CP, FClientCP);
    SetRawByteString(ParameterIndex, FRawTemp);
  end;
  {$ENDIF}
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5036 off : Local variable DT does not seem to be initialized} {$ENDIF}
procedure TZAbstractOraclePreparedStatement.SetTime(Index: Integer;
  const Value: TZTime);
var
  Bind: PZOCIParamBind;
  DT: TDateTime;
  Status: sword absolute DT;
  OraType: TZSQLType absolute DT;
  Len: LengthInt absolute DT;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (stTime <> BindList[Index].SQLType)
  then OraType := BindList[Index].SQLType
  else OraType := stTime;
  if (BindList[Index].SQLType <> OraType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(OraType, Bind, Index, 1);
  case Bind.dty of
    SQLT_DAT:   begin
                  POraDate(Bind^.valuep).Cent   := cPascalIntegralDatePart.Year div 100 +100;
                  POraDate(Bind^.valuep).Year   := cPascalIntegralDatePart.Year mod 100 +100;
                  POraDate(Bind^.valuep).Month  := cPascalIntegralDatePart.Month;
                  POraDate(Bind^.valuep).Day    := cPascalIntegralDatePart.Day;
                  POraDate(Bind^.valuep).Hour := Value.Hour +1;
                  POraDate(Bind^.valuep).Min := Value.Minute +1;
                  POraDate(Bind^.valuep).Sec := Value.Second +1;
                end;
    SQLT_TIMESTAMP: begin
                  Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
                    FOCIError, PPOCIDescriptor(Bind.valuep)^, cPascalIntegralDatePart.Year,
                      cPascalIntegralDatePart.Month, cPascalIntegralDatePart.Day,
                      Value.Hour, Value.Minute, Value.Second, Value.Fractions, nil, 0);
                  if Status <> OCI_SUCCESS then
                    FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIDateTimeConstruct', Self);
                end;
    SQLT_CLOB,
    SQLT_LVC: begin
                Len := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
                  PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateFormat, True, Value.IsNegative);
                BindRawStr(Index, PAnsiChar(FByteBuffer), Len);
                Exit;
              end;
    else      begin
                if TryTimeToDateTime(Value, DT)
                then InternalBindDouble(Index, stDate, DT)
                else BindSInteger(Index, stDate, 1);
                Exit;
              end;
  end;
  Bind.indp[0] := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetTimestamp(Index: Integer;
  const Value: TZTimeStamp);
var
  Bind: PZOCIParamBind;
  DT: TDateTime;
  Status: sword absolute DT;
  OraType: TZSQLType absolute DT;
  Len: LengthInt absolute DT;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (stTimeStamp <> BindList[Index].SQLType)
  then OraType := BindList[Index].SQLType
  else OraType := stTimeStamp;
  if (BindList[Index].SQLType <> OraType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(OraType, Bind, Index, 1);
  case Bind.dty of
    SQLT_DAT:   begin
                  POraDate(Bind^.valuep).Cent := Value.Year div 100 +100;
                  POraDate(Bind^.valuep).Year := Value.Year mod 100 +100;
                  POraDate(Bind^.valuep).Day  := Value.Day;
                  POraDate(Bind^.valuep).Month:= Value.Month;
                  POraDate(Bind^.valuep).Hour := Value.Hour +1;
                  POraDate(Bind^.valuep).Min  := Value.Minute +1;
                  POraDate(Bind^.valuep).Sec  := Value.Second +1;
                end;
    SQLT_TIMESTAMP: begin
                  Status := FPlainDriver.OCIDateTimeConstruct(FOracleConnection.GetConnectionHandle,
                    FOCIError, PPOCIDescriptor(Bind.valuep)^, Value.Year, Value.Month, Value.Day,
                      Value.Hour, Value.Minute, Value.Second, Value.Fractions, nil, 0);
                  if Status <> OCI_SUCCESS then
                    FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCIDateTimeConstruct', Self);
                end;
    SQLT_CLOB,
    SQLT_LVC: begin
                Len := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
                  Value.Hour, Value.Minute, Value.Second, Value.Fractions,
                  PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateFormat, True, Value.IsNegative);
                BindRawStr(Index, PAnsiChar(FByteBuffer), Len);
                Exit;
              end;
    else      begin
                if TryTimeStampToDateTime(Value, DT)
                then InternalBindDouble(Index, stDate, DT)
                else BindSInteger(Index, stDate, 1);
                Exit;
              end;
  end;
  Bind.indp[0] := 0;
end;

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetUInt(Index: Integer; Value: Cardinal);
begin
  BindSInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLongWord, Value);
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractOraclePreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
{$IFDEF CPU64}
begin
  BindUInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stULong, Value);
{$ELSE}
var
  Bind: PZOCIParamBind;
  P: PAnsiChar;
  Status: sword;
  SQLType: TZSQLType;
begin
  CheckParameterIndex(Index);
  {$R-}
  Bind := @FOraVariables[Index];
  {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
  if Boolean(BindList[Index].ParamType) and Boolean(BindList[Index].SQLType) and
     (stULong <> BindList[Index].SQLType) //keep registered types alive
  then SQLType := BindList[Index].SQLType
  else SQLType := stULong;
  if (BindList[Index].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then
    InitBuffer(SQLType, Bind, Index, 1);
  case Bind.dty of
    SQLT_VNU: begin
        Status := FPlainDriver.OCINumberFromInt(FOCIError, @Value, SizeOf({$IFNDEF CPU64}Integer{$ELSE}Int64{$ENDIF}), OCI_NUMBER_SIGNED, POCINumber(Bind.valuep));
        if Status <> OCI_SUCCESS then
          FOracleConnection.HandleErrorOrWarning(FOCIError, status, lcBindPrepStmt, 'OCINumberFromInt', Self);
      end;
    SQLT_INT: if Bind.value_sz = SizeOf(Int64) then
                PInt64(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Integer) then
                PInteger(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(SmallInt) then
                PSmallInt(Bind.valuep)^ := Value
              else
                PShortInt(Bind.valuep)^ := Value;
    SQLT_UIN: if Bind.value_sz = SizeOf(UInt64) then
                PUInt64(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Cardinal) then
                PCardinal(Bind.valuep)^ := Value
              else if Bind.value_sz = SizeOf(Word) then
                PWord(Bind.valuep)^ := Value
              else
                PByte(Bind.valuep)^ := Value;
    SQLT_BFLOAT,
    SQLT_BDOUBLE,
    SQLT_DAT,
    SQLT_TIMESTAMP: InternalBindDouble(Index, SQLtype, Value);
    SQLT_LVC: begin
                IntToRaw(Value, PAnsiChar(@POCIVary(Bind.valuep).data[0]), @P);
                POCIVary(Bind.valuep).Len := P-@POCIVary(Bind.valuep).data[0];
              end
  end;
  Bind.indp[0] := 0;
{$ENDIF}
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Sets the designated parameter to a Java <code>UnicodeString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetUnicodeString(ParameterIndex: Integer;
  const Value: UnicodeString);
var L: NativeUInt;
  P: PWideChar;
begin
  L := Length(Value);
  if L = 0
  then P := PEmptyUnicodeString
  else P := Pointer(Value);
  SetPWidechar(ParameterIndex, P, L);
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
procedure TZAbstractOraclePreparedStatement.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  if (FCharSetID = OCI_UTF16ID) or (FClientCP <> zCP_UTF8) then begin
    FUniTemp := ZRawToUnicode(Value, zCP_UTF8);
    SetUnicodeString(ParameterIndex, FUniTemp);
  end else SetRawByteString(ParameterIndex, Value);
end;
{$ENDIF NO_UTF8STRING}

{**
  Sets the designated parameter to a Java <code>unsigned 16bit int</code> value.
  The driver converts this
  to an SQL <code>WORD</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractOraclePreparedStatement.SetWord(Index: Integer; Value: Word);
begin
  BindUInteger(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stWord, Value);
end;

{ TZOraclePreparedStatement_A }

function TZOraclePreparedStatement_A.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  I, C, FirstComposePos: Integer;
  ParamsCnt: Cardinal;
  Tokens: TZTokenList;
  Token: PZToken;
  tmp{$IFNDEF UNICODE}, Fraction{$ENDIF}: RawByteString;
  SQLWriter, ParamWriter: TZRawSQLStringWriter;
  ComparePrefixTokens: TPreparablePrefixTokens;
  procedure Add(const Value: RawByteString; const Param: Boolean);
  var H: Integer;
  begin
    H := Length(FCachedQueryRaw);
    SetLength(FCachedQueryRaw, H+1);
    FCachedQueryRaw[H] := Value;
    SetLength(FIsParamIndex, H+1);
    FIsParamIndex[H] := Param;
    SQLWriter.AddText(Value, Result);
  end;
begin
  if (Length(FCachedQueryRaw) = 0) and (SQL <> '') then begin
    Result := '';
    Tmp := '';
    Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
    C := Length(SQL);
    SQLWriter := TZRawSQLStringWriter.Create(C);
    ParamWriter := TZRawSQLStringWriter.Create({$IFDEF UNICODE}16{$ELSE}C shr 4{$ENDIF});
    try
      ComparePrefixTokens := OraPreparableTokens;
      FTokenMatchIndex := -1;
      ParamsCnt := 0;
      FirstComposePos := 0;
      for I := 0 to Tokens.Count -1 do begin
        Token := Tokens[I];
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if Assigned(ComparePrefixTokens) and (Token.TokenType = ttWord) then begin
          for C := 0 to high(ComparePrefixTokens) do
            if Tokens.IsEqual(i, ComparePrefixTokens[C].MatchingGroup, tcInsensitive) then begin
              FTokenMatchIndex := C;
              Break;
            end;
          ComparePrefixTokens := nil; //stop compare sequence
        end;
        if (Token.L = 1) and ((Token.P^ = '?') or ((Token.P^ = ':') and (Tokens.Count > i+1) and (Tokens[I+1].TokenType = ttWord))) then begin
          Inc(ParamsCnt);
          {$IFDEF UNICODE}
          Tmp := PUnicodeToRaw(Tokens[FirstComposePos].P, Tokens[I-1].P-Tokens[FirstComposePos].P+Tokens[I-1].L, FClientCP);
          {$ELSE}
          if Consettings.AutoEncode
          then ParamWriter.Finalize(Tmp)
          else Tmp := Tokens.AsString(FirstComposePos, I-1);
          {$ENDIF}
          Add(Tmp, False);
          if (Token.P^ = '?') then begin
            Tmp := '';
            ParamWriter.AddChar(AnsiChar(':'), Tmp);
            if (FParamNames <> nil) and (Cardinal(Length(FParamNames)) >= ParamsCnt) and (FParamNames[ParamsCnt-1] <> '')
            then ParamWriter.AddText(FParamNames[ParamsCnt-1], Tmp)
            else begin
              ParamWriter.AddChar(AnsiChar('P'), Tmp);
              ParamWriter.AddOrd(ParamsCnt, Tmp);
            end;
            ParamWriter.Finalize(Tmp);
            FirstComposePos := i + 1;
          end else begin
            {$IFDEF UNICODE}
            Tmp := UnicodeStringToAscii7(Token.P, Tokens[i+1].L+1);
            {$ELSE}
            ZSetString(Token.P, Tokens[i+1].L+1, Tmp);
            {$ENDIF}
            FirstComposePos := i + 2;
          end;
          Add(Tmp, True);
          Tmp := '';
        end {$IFNDEF UNICODE} else if (FirstComposePos <= I) and ConSettings.AutoEncode then
          case (Token.TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier: begin
                Fraction := ConSettings^.ConvFuncs.ZStringToRaw(TokenAsString(Token^), ConSettings^.CTRL_CP, FClientCP);
                ParamWriter.AddText(Fraction, Tmp);
              end;
            else ParamWriter.AddText(Token.P, Token.L, tmp);
          end
        {$ENDIF};
      end;
      I := Tokens.Count -1;
      if (FirstComposePos <= I) then begin
        {$IFDEF UNICODE}
        Tmp := PUnicodeToRaw(Tokens[FirstComposePos].P, Tokens[I].P-Tokens[FirstComposePos].P+Tokens[I].L, FClientCP);
        {$ELSE}
        if ConSettings.AutoEncode
        then ParamWriter.Finalize(Tmp)
        else Tmp := Tokens.AsString(FirstComposePos, I);
        {$ENDIF}
        Add(Tmp, False);
      end;
      SetBindCapacity(ParamsCnt);
      FServerStmtCache := (FTokenMatchIndex > -1) and (FTokenMatchIndex < OCI_STMT_CREATE) and (ParamsCnt > 0);
    finally
      SQLWriter.Finalize(Result);
      FreeAndNil(SQLWriter);
      FreeAndNil(ParamWriter);
      FreeAndNil(Tokens);
    end;
  end else
    Result := ASQL;
end;

procedure TZOraclePreparedStatement_A.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
var
  Bind: PZOCIParamBind;
  i, j: Integer;
begin
  CheckParameterIndex(ParameterIndex);
  inherited RegisterParameter(ParameterIndex, SQLType, ParamType, Name,
    PrecisionOrSize, Scale);
  {$R-}
  Bind := @FOraVariables[ParameterIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (PrecisionOrSize = 0) and (SQLType in [stString, stUnicodeString, stBytes]) then
    PrecisionOrSize := 4000;
  Bind.Precision := PrecisionOrSize;
  Bind.Scale := Scale;
  Bind.ParamName := Name;
  if (Name <> '') then begin
    J := 0;
    for i := 0 to high(FCachedQueryRaw) do
      if FIsParamIndex[i] then begin
        if (J = ParameterIndex) then begin
          if (FCachedQueryRaw[j] = '?') then
            FCachedQueryRaw[j] := ':'+ConSettings.ConvFuncs.ZStringToRaw(Name, ConSettings.CTRL_CP, FClientCP);
          Break;
        end;
        Inc(J);
      end;
    end;

  if ParamType <> pctUnknown then begin
    if (Scale > 0) and (SQLType in [stBoolean..stBigDecimal]) then
      SQLType := stBigDecimal;
    if (BindList[ParameterIndex].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then begin
      if SQLType = stString then
        PrecisionOrSize := PrecisionOrSize * ConSettings.ClientCodePage.CharWidth
      else if SQLType = stUnicodeString then
        PrecisionOrSize := PrecisionOrSize shl 1;
      InitBuffer(SQLType, Bind, ParameterIndex, 1, PrecisionOrSize);
      FillChar(Bind.valuep^, Bind.Value_sz, #0);
    end;
    Bind.indp[0] := -1;
  end;
end;

procedure TZOraclePreparedStatement_A.Unprepare;
begin
  inherited Unprepare;
  SetLength(FCachedQueryRaw, 0);
end;

{ TZOraclePreparedStatement_W }

function TZOraclePreparedStatement_W.GetUnicodeEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): UnicodeString;
var
  I, C, FirstComposePos: Integer;
  ParamsCnt: Cardinal;
  Tokens: TZTokenList;
  Token: PZToken;
  tmp{$IFNDEF UNICODE}, Fraction{$ENDIF}: UnicodeString;
  SQLWriter, ParamWriter: TZUnicodeSQLStringWriter;
  ComparePrefixTokens: TPreparablePrefixTokens;
  procedure Add(const Value: UnicodeString; const Param: Boolean);
  var H: Integer;
  begin
    H := Length(FCachedQueryUni);
    SetLength(FCachedQueryUni, H+1);
    FCachedQueryUni[H] := Value;
    SetLength(FIsParamIndex, H+1);
    FIsParamIndex[H] := Param;
    SQLWriter.AddText(Value, Result);
  end;
begin
  if (Length(FCachedQueryUni) = 0) and (SQL <> '') then begin
    Result := '';
    Tmp := '';
    Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
    C := Length(SQL);
    SQLWriter := TZUnicodeSQLStringWriter.Create(C);
    ParamWriter := TZUnicodeSQLStringWriter.Create({$IFDEF UNICODE}16{$ELSE}C shr 4{$ENDIF});
    try
      ComparePrefixTokens := OraPreparableTokens;
      FTokenMatchIndex := -1;
      ParamsCnt := 0;
      FirstComposePos := 0;
      for I := 0 to Tokens.Count -1 do begin
        Token := Tokens[I];
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if Assigned(ComparePrefixTokens) and (Token.TokenType = ttWord) then begin
          for C := 0 to high(ComparePrefixTokens) do
            if Tokens.IsEqual(i, ComparePrefixTokens[C].MatchingGroup, tcInsensitive) then begin
              FTokenMatchIndex := C;
              Break;
            end;
          ComparePrefixTokens := nil; //stop compare sequence
        end;
        if (Token.L = 1) and ((Token.P^ = '?') or ((Token.P^ = ':') and (Tokens.Count > i+1) and (Tokens[I+1].TokenType = ttWord))) then begin
          Inc(ParamsCnt);
          {$IFDEF UNICODE}
          Tmp := Tokens.AsString(FirstComposePos, I-1);
          {$ELSE}
          if Consettings.AutoEncode
          then ParamWriter.Finalize(Tmp)
          else Tmp := PRawToUnicode(Tokens[FirstComposePos].P, Tokens[I-1].P-Tokens[FirstComposePos].P+Tokens[I-1].L, ConSettings.CTRL_CP);
          {$ENDIF}
          Add(Tmp, False);
          if (Token.P^ = '?') then begin
            Tmp := '';
            ParamWriter.AddChar(':', Tmp);
            if (FParamNames <> nil) and (Cardinal(Length(FParamNames)) >= ParamsCnt) and (FParamNames[ParamsCnt-1] <> '')
            then ParamWriter.AddText(FParamNames[ParamsCnt-1], Tmp)
            else begin
              ParamWriter.AddChar('P', Tmp);
              ParamWriter.AddOrd(ParamsCnt, Tmp);
            end;
            ParamWriter.Finalize(Tmp);
            FirstComposePos := i + 1;
          end else begin
            {$IFDEF UNICODE}
            System.SetString(Tmp, Token.P, Tokens[i+1].L+1);
            {$ELSE}
            Tmp := Ascii7ToUnicodeString(Token.P, Tokens[i+1].L+1);
            {$ENDIF}
            FirstComposePos := i + 2;
          end;
          Add(Tmp, True);
          Tmp := '';
        end {$IFNDEF UNICODE} else if (FirstComposePos <= I) and ConSettings.AutoEncode then
          case (Token.TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier: begin
                Fraction := ConSettings^.ConvFuncs.ZStringToUnicode(TokenAsString(Token^), ConSettings^.CTRL_CP);
                ParamWriter.AddText(Fraction, Tmp);
              end;
            else ParamWriter.AddAscii7Text(Token.P, Token.L, tmp);
          end
        {$ENDIF};
      end;
      I := Tokens.Count -1;
      if (FirstComposePos <= I) then begin
        {$IFDEF UNICODE}
        Tmp := Tokens.AsString(FirstComposePos, I);
        {$ELSE}
        if ConSettings.AutoEncode
        then ParamWriter.Finalize(Tmp)
        else Tmp := PRawToUnicode(Tokens[FirstComposePos].P, Tokens[I].P-Tokens[FirstComposePos].P+Tokens[I].L, ConSettings^.CTRL_CP);
        {$ENDIF}
        Add(Tmp, False);
      end;
      SetBindCapacity(ParamsCnt);
      FServerStmtCache := (FTokenMatchIndex > -1) and (FTokenMatchIndex < OCI_STMT_CREATE) and (ParamsCnt > 0);
    finally
      SQLWriter.Finalize(Result);
      FreeAndNil(SQLWriter);
      FreeAndNil(ParamWriter);
      FreeAndNil(Tokens);
    end;
  end else
    Result := WSQL;
end;

procedure TZOraclePreparedStatement_W.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
var
  Bind: PZOCIParamBind;
  i, j: Integer;
begin
  CheckParameterIndex(ParameterIndex);
  inherited RegisterParameter(ParameterIndex, SQLType, ParamType, Name,
    PrecisionOrSize, Scale);
  {$R-}
  Bind := @FOraVariables[ParameterIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  if (PrecisionOrSize = 0) and (SQLType in [stString, stUnicodeString, stBytes]) then
    PrecisionOrSize := 4000;
  Bind.Precision := PrecisionOrSize;
  Bind.Scale := Scale;
  Bind.ParamName := Name;
  if (Name <> '') then begin
    J := 0;
    for i := 0 to high(FCachedQueryUni) do
      if FIsParamIndex[i] then begin
        if (J = ParameterIndex) then begin
          if (FCachedQueryUni[j] = '?') then
            FCachedQueryUni[j] := ':'+ConSettings.ConvFuncs.ZStringToUnicode(Name, ConSettings.CTRL_CP);
          Break;
        end;
        Inc(J);
      end;
    end;

  if ParamType <> pctUnknown then begin
    if (Scale > 0) and (SQLType in [stBoolean..stBigDecimal]) then
      SQLType := stBigDecimal;
    if (BindList[ParameterIndex].SQLType <> SQLType) or (Bind.valuep = nil) or (Bind.curelen <> 1) then begin
      if SQLType = stString then
        PrecisionOrSize := PrecisionOrSize * ConSettings.ClientCodePage.CharWidth
      else if SQLType = stUnicodeString then
        PrecisionOrSize := PrecisionOrSize shl 1;
      InitBuffer(SQLType, Bind, ParameterIndex, 1, PrecisionOrSize);
      FillChar(Bind.valuep^, Bind.Value_sz, #0);
    end;
    Bind.indp[0] := -1;
  end;
end;

procedure TZOraclePreparedStatement_W.Unprepare;
begin
  inherited Unprepare;
  SetLength(FCachedQueryUni, 0);
end;

{ TZOracleCallableStatement_W }

function TZOracleCallableStatement_W.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  ProcSQL: UnicodeString;
  SQLWriter: TZUnicodeSQLStringWriter;
  IC: IZIdentifierConvertor;

  procedure AddArgs({$IFDEF AUTOREFCOUNT}const{$ENDIF}Params: TObjectList);
  var I: Integer;
  begin
    SQLWriter.AddChar('(', ProcSQL);
    for I := 0 to Params.Count-1 do
      if TZOraProcDescriptor_W(Params[i]).OrdPos > 0 then begin
        SQLWriter.AddChar(':', ProcSQL);
        TZOraProcDescriptor_W(Params[i]).ConcatParentName(False, SQLWriter, ProcSQL, IC);
        SQLWriter.AddText(TZOraProcDescriptor_W(Params[i]).AttributeName, ProcSQL);
        SQLWriter.AddChar(',', ProcSQL);
      end;
    SQLWriter.ReplaceOrAddLastChar(',',')',ProcSQL);
  end;

  procedure BuildFunction({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_W);
  {$IFNDEF UNICODE}
  var R: RawByteString;
      S: UnicodeString;
  {$ENDIF}
  begin
    SQLWriter.AddChar(':', ProcSQL);
    TZOraProcDescriptor_W(Descriptor.Args[0]).ConcatParentName(False, SQLWriter, ProcSQL, IC);
    SQLWriter.AddText(TZOraProcDescriptor_W(Descriptor.Args[0]).AttributeName, ProcSQL);
    SQLWriter.AddText(' := ', ProcSQL);
    Descriptor.ConcatParentName(True, SQLWriter, ProcSQL, IC);
    {$IFNDEF UNICODE}
    R := ZUnicodeToRaw(Descriptor.AttributeName, ConSettings.CTRL_CP);
    R := IC.Quote(R);
    S := ZRawToUnicode(R, ConSettings.CTRL_CP);
    SQLWriter.AddText(S, ProcSQL);
    {$ELSE}
    SQLWriter.AddText(IC.Quote(Descriptor.AttributeName), ProcSQL);
    {$ENDIF}
    AddArgs(Descriptor.Args);
    SQLWriter.AddChar(';', ProcSQL);
  end;
  procedure BuildProcedure({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_W);
  {$IFNDEF UNICODE}
  var R: RawByteString;
      S: UnicodeString;
  {$ENDIF}
  begin
    Descriptor.ConcatParentName(True, SQLWriter, ProcSQL, IC);
    {$IFNDEF UNICODE}
    R := ZUnicodeToRaw(Descriptor.AttributeName, ConSettings.CTRL_CP);
    R := IC.Quote(R);
    S := ZRawToUnicode(R, ConSettings.CTRL_CP);
    SQLWriter.AddText(S, ProcSQL);
    {$ELSE}
    SQLWriter.AddText(Descriptor.AttributeName, ProcSQL);
    {$ENDIF}
    AddArgs(Descriptor.Args);
    SQLWriter.AddChar(';', ProcSQL);
  end;
  procedure BuildPackage({$IFDEF AUTOREFCOUNT}const{$ENDIF}Descriptor: TZOraProcDescriptor_W);
  var I: Integer;
  begin
    for I := 0 to Descriptor.Args.Count -1 do begin
      if Descriptor.Parent <> nil then
        SQLWriter.AddText('BEGIN'#10, ProcSQL);
      if TZOraProcDescriptor_W(Descriptor.Args[I]).ObjType = OCI_PTYPE_PKG then
        BuildPackage(TZOraProcDescriptor_W(Descriptor.Args[I]))
      else if TZOraProcDescriptor_W(Descriptor.Args[I]).ObjType = OCI_PTYPE_PROC then
        BuildProcedure(TZOraProcDescriptor_W(Descriptor.Args[I]))
      else if TZOraProcDescriptor_W(Descriptor.Args[I]).ObjType = OCI_PTYPE_FUNC then
        BuildFunction(TZOraProcDescriptor_W(Descriptor.Args[I]))
      else
        AddArgs(Descriptor.Args);
      if Descriptor.Parent <> nil then
        SQLWriter.AddText(#10'END;', ProcSQL);
    end;
  end;
begin
  IC := Connection.GetMetadata.GetIdentifierConvertor;
  SQLWriter := TZUnicodeSQLStringWriter.Create(1024);
  try
    if FProcDescriptor = nil then
      { describe the object: }
      FProcDescriptor := TZOraProcDescriptor_W.Create(nil, Connection as IZOracleConnection, ConSettings.CTRL_CP);
    if FProcDescriptor.ObjType = OCI_PTYPE_UNK then begin
      {$IFDEF UNICODE}
      FProcDescriptor.Describe(OCI_PTYPE_UNK, StoredProcName);
      {$ELSE}
      ProcSQL := ZRawToUnicode(StoredProcName, ConSettings.CTRL_CP);
      FProcDescriptor.Describe(OCI_PTYPE_UNK, ProcSQL);
      {$ENDIF}
    end;
    ProcSQL := '';
    SQLWriter.AddText('BEGIN'#10, ProcSQL);
    if FProcDescriptor.ObjType = OCI_PTYPE_PKG then
      BuildPackage(FProcDescriptor)
    else if FProcDescriptor.ObjType = OCI_PTYPE_PROC then
      BuildProcedure(FProcDescriptor)
    else
      BuildFunction(FProcDescriptor);
    SQLWriter.AddText(#10'END;', ProcSQL);
    SQLWriter.Finalize(ProcSQL);
  finally
    FreeAndNil(SQLWriter);
  end;
  Result := TZOraclePreparedStatement_W.Create(Connection, '', Info);
  TZOraclePreparedStatement_W(Result).FWSQL := ProcSQL;
  {$IFDEF UNICODE}
  TZOraclePreparedStatement_W(Result).fASQL := ZUnicodeToRaw(ProcSQL, ConSettings.CTRL_CP);
  {$ENDIF}
  TZOraclePreparedStatement_W(Result).Prepare;
end;

procedure TZOracleCallableStatement_W.PrepareInParameters;
var Idx: Integer;
  {$IFNDEF UNICODE}
  S: UnicodeString;
  {$ENDIF}
  procedure RegisterFromDescriptor(ParentDescriptor: TZOraProcDescriptor_W;
    var IDX: Integer);
  var i: Integer;
    Descriptor: TZOraProcDescriptor_W;
    Tmp: UnicodeString;
    {$IFNDEF UNICODE}
    S: String;
    {$ENDIF}
    SQLWriter: TZUnicodeSQLStringWriter;
  begin
    SQLWriter := TZUnicodeSQLStringWriter.Create(1024);
    try
      for I := 0 to ParentDescriptor.Args.Count-1 do begin
        Descriptor := TZOraProcDescriptor_W(ParentDescriptor.Args[i]);
        if Descriptor.ObjType <> OCI_PTYPE_ARG then
          RegisterFromDescriptor(Descriptor, IDX)
        else begin
          Tmp := '';
          Descriptor.ConcatParentName(False, SQLWriter, Tmp, nil);
          SQLWriter.AddText(Descriptor.AttributeName, Tmp);
          SQLWriter.Finalize(tmp);
          {$IFNDEF UNICODE}
          S := ZUnicodeToRaw(tmp, ConSettings.CTRL_CP);
          {$ENDIF}
          if FExecStatement = nil then
            RegisterParameter(IDX,
              Descriptor.SQLType, OCIParamTypeMatrix[Descriptor.OrdPos = 0][Descriptor.IODirection], {$IFNDEF UNICODE}S{$ELSE}tmp{$ENDIF},
                Max(LengthInt(Descriptor.DataSize), LengthInt(Descriptor.Precision)), Descriptor.Scale)
          else begin
            RegisterParameter(IDX,
              Descriptor.SQLType, OCIParamTypeMatrix[Descriptor.OrdPos = 0][Descriptor.IODirection], {$IFNDEF UNICODE}S{$ELSE}tmp{$ENDIF},
                Max(LengthInt(Descriptor.DataSize), LengthInt(Descriptor.Precision)), Descriptor.Scale);
            FExecStatement.RegisterParameter(IDX,
              Descriptor.SQLType, OCIParamTypeMatrix[Descriptor.OrdPos = 0][Descriptor.IODirection], {$IFNDEF UNICODE}S{$ELSE}tmp{$ENDIF},
                Max(LengthInt(Descriptor.DataSize), LengthInt(Descriptor.Precision)), Descriptor.Scale);
          end;
          Inc(IDX);
        end;
      end;
    finally
      FreeAndNil(SQLWriter);
    end;
  end;
begin
  if not FParamsRegistered then begin
    FParamsRegistered := True;
    FRegisteringParamFromMetadata := True;
    if FProcDescriptor = nil then
      FProcDescriptor := TZOraProcDescriptor_W.Create(nil, Connection as IZOracleConnection, FClientCP);
    if FProcDescriptor.ObjType = OCI_PTYPE_UNK then begin
      { describe the object: }
      {$IFNDEF UNICODE}
      S := ZRawToUnicode(StoredProcName, ConSettings.CTRL_CP);
      FProcDescriptor.Describe(OCI_PTYPE_UNK, S);
      {$ELSE}
      FProcDescriptor.Describe(OCI_PTYPE_UNK, StoredProcName);
      {$ENDIF}
    end;
    if FProcDescriptor <> nil then begin
      Idx := 0;
      RegisterFromDescriptor(FProcDescriptor, IDX);
    end;
    FRegisteringParamFromMetadata := False;
  end;
end;

procedure TZOracleCallableStatement_W.Unprepare;
begin
  inherited Unprepare;
  if FProcDescriptor <> nil then
    FreeAndNil(FProcDescriptor);
end;

initialization

{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
{$IFDEF WITH_VAR_INIT_WARNING}
OraPreparableTokens := nil;
{$ENDIF}
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

{$ENDIF ZEOS_DISABLE_ORACLE}
end.
