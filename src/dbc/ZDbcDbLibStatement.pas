{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          DBLib Statement common functionality           }
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

unit ZDbcDbLibStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types, FmtBCD,
  ZCompatibility, ZClasses, ZSysUtils, ZCollections, ZDbcIntfs, ZDbcStatement,
  ZDbcDbLib, ZPlainDbLibConstants, ZPlainDbLibDriver;

type
  {** Implements Prepared SQL Statement for DBLib. With emulation}
  TZAbstractDBLibStatement = class(TZRawParamDetectPreparedStatement)
  private
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: TZDBLIBPLainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    FUserEncoding: TZCharEncoding;
    FLastOptainedRS: IZResultSet;
    FIsNCharIndex: TBooleanDynArray;
    procedure CreateOutParamResultSet; virtual;
    procedure InternalExecute; virtual; abstract;
  protected
    procedure FetchResults;
    procedure FlushPendingResults;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);
    procedure Prepare; override;
    procedure Unprepare; override;
    function GetMoreResults: Boolean; override;
    function GetUpdateCount: Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZAbstracDBLibSQLStatement = Class(TZAbstractDBLibStatement)
  private
    procedure InternalExecute; override;
    function GetRawSQL: RawByteString; virtual; abstract;
  End;

  {** Implements generic DBLib Statement. }
  TZDBLibStatement = class(TZAbstracDBLibSQLStatement)
  private
    function GetRawSQL: RawByteString; override;
  public
    constructor Create(const Connection: IZConnection; const Info: TStrings);
  end;

  {** Implements Prepared SQL Statement for DBLib. With emulation of course }
  TZDBLibPreparedStatementEmulated = class(TZAbstracDBLibSQLStatement, IZPreparedStatement)
  private
    function GetRawSQL: RawByteString; override;
  protected
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; override;
  public
    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    procedure SetByte(ParameterIndex: Integer; Value: Byte);
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt);
    procedure SetWord(ParameterIndex: Integer; Value: Word);
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt);
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal);
    procedure SetInt(ParameterIndex: Integer; Value: Integer);
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64);
    procedure SetLong(ParameterIndex: Integer; const Value: Int64);
    procedure SetFloat(ParameterIndex: Integer; Value: Single);
    procedure SetDouble(ParameterIndex: Integer; const Value: Double);
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency);
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: TBCD);
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);reintroduce;
    procedure SetString(ParameterIndex: Integer; const Value: String);reintroduce;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); reintroduce;
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); reintroduce;
    procedure SetGuid(ParameterIndex: Integer; const Value: TGUID); reintroduce;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); reintroduce;
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); reintroduce;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); reintroduce;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); reintroduce;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  end;

  TZDBLIBPreparedRPCStatement = class(TZAbstractDBLibStatement, IZPreparedStatement)
  private
    FParamNames: TRawByteStringDynArray;
    procedure InternalExecute; override;
    procedure CreateOutParamResultSet; override;
  protected
    procedure BindInParameters; override;
    procedure SetBindCapacity(Capacity: Integer); override;
  public
    constructor Create(const Connection: IZConnection;
      const RemoteProcedureName: String; const Info: TStrings);
  public
    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    procedure SetByte(ParameterIndex: Integer; Value: Byte);
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt);
    procedure SetWord(ParameterIndex: Integer; Value: Word);
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt);
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal);
    procedure SetInt(ParameterIndex: Integer; Value: Integer);
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64);
    procedure SetLong(ParameterIndex: Integer; const Value: Int64);
    procedure SetFloat(ParameterIndex: Integer; Value: Single);
    procedure SetDouble(ParameterIndex: Integer; const Value: Double);
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency);
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: TBCD);
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);reintroduce;
    procedure SetString(ParameterIndex: Integer; const Value: String);reintroduce;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); reintroduce;
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); reintroduce;
    procedure SetGuid(ParameterIndex: Integer; const Value: TGUID); reintroduce;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); reintroduce;
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); reintroduce;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); reintroduce;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); reintroduce;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  public
    function ExecuteQuery(const {%H-}SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const {%H-}SQL: ZWideString): Integer; override;
    function Execute(const {%H-}SQL: ZWideString): Boolean; override;

    function ExecuteQuery(const {%H-}SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const {%H-}SQL: RawByteString): Integer; override;
    function Execute(const {%H-}SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
  end;

  TZDBLibCallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement2; override;
  end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

(* target:
  declare @p1 int
set @p1=-1
exec sp_prepexec @p1 output,NULL,N'select [PersonID] from [Tasks] t join [PersonSnapShots] pss on t.[CostSnapShotID]=pss.ID where t.[TaskTypeID]=21 and [CompletionDate] is null'
select @p1

https://docs.microsoft.com/en-us/sql/relational-databases/system-stored-procedures/sp-prepare-transact-sql?view=sql-server-2017
https://docs.microsoft.com/en-us/sql/relational-databases/system-stored-procedures/sp-unprepare-transact-sql?view=sql-server-2017
https://docs.microsoft.com/en-us/sql/relational-databases/system-stored-procedures/sp-describe-undeclared-parameters-transact-sql?view=sql-server-2017
*)

uses
  Math,
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs,
  {$ELSE}{$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  {$ENDIF}{$IFDEF WITH_UNITANSISTRINGS} AnsiStrings, {$ENDIF}
  ZDbcLogging, ZDbcCachedResultSet, ZDbcDbLibUtils, ZDbcDbLibResultSet,
  ZVariant, ZDbcUtils, ZEncoding, ZDbcResultSet, ZDbcProperties,
  ZFastCode, ZMessages, ZDbcResultSetMetadata, ZDbcMetadata;

{ TZAbstractDBLibStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param SQL the command text.
  @param Info a statement parameters.
}
constructor TZAbstractDBLibStatement.Create(
  const Connection: IZConnection; const SQL: string; const Info: TStrings);
begin
  FNCharDetected := @FIsNCharIndex;
  inherited Create(Connection, SQL, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := TZDBLIBPLainDriver(Connection.GetIZPlainDriver.GetInstance);
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  FResults := TZCollection.Create;
  {note: this is a hack! Purpose is to notify Zeos all Charakter columns are
    UTF8-encoded. e.g. N(VAR)CHAR. Initial idea is made for MSSQL where we've NO
    valid tdsType to determine (Var)Char(Ansi-Encoding) or N(Var)Char encoding
    So this is stopping all encoding detections and increases the performance in
    a high rate. If Varchar fields are fetched you Should use a cast to N-Fields!
    Else all results are invalid!!!!! Just to invoke later questions!}
  if DefineStatementParameter(Self, DSProps_ResetCodePage, '') = 'UTF8' then
    FUserEncoding := ceUTF8
  else
    Self.FUserEncoding := ceDefault;
end;

procedure TZAbstractDBLibStatement.CreateOutParamResultSet;
var I: Integer;
begin
  for I := FResults.Count -1 downto 0 do
    if Supports(FResults[I], IZResultSet, FOutParamResultSet) then
      Break;
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
function TZAbstractDBLibStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZAnyValue;
  I: Integer;
begin
  FLastOptainedRS := nil;
  Result := False;
  for i := 0 to FResults.Count -1 do begin
    Result := FResults.Items[I].QueryInterface(IZResultSet, ResultSet) = S_OK;
    if Result then begin
      FLastOptainedRS := ResultSet;
      FResults.Delete(I);
      Break;
    end else//else TestStatement can't be resolved
      if FResults.Items[I].QueryInterface(IZAnyValue, UpdateCount) = S_OK then
        LastUpdateCount := UpdateCount.GetInteger;
  end;
end;

{**
  Returns the current result as an update count;
  if the result is a <code>ResultSet</code> object or there are no more results, -1
  is returned. This method should be called only once per result.

  @return the current result as an update count; -1 if the current result is a
    <code>ResultSet</code> object or there are no more results
  @see #execute
}
function TZAbstractDBLibStatement.GetUpdateCount: Integer;
var
  UpdateCount: IZAnyValue;
  I: Integer;
begin
  Result := inherited GetUpdateCount;
  if (Result = -1) and (FResults.Count > 0) then
    for i := 0 to FResults.Count -1 do
      try
        if FResults.Items[I].QueryInterface(IZAnyValue, UpdateCount) = S_OK then begin
          Result := UpdateCount.GetInteger;
          FResults.Delete(I);
          Break;
        end;
      finally
        UpdateCount := nil;
      end;
end;

procedure TZAbstractDBLibStatement.Prepare;
begin
  FlushPendingResults;
  inherited Prepare;
end;

procedure TZAbstractDBLibStatement.Unprepare;
begin
  FlushPendingResults;
  inherited UnPrepare;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractDBLibStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  InternalExecute;
  FetchResults;
  LastUpdateCount := GetUpdateCount;
  Result := GetMoreResults;
  LastResultSet := FLastOptainedRS;
  FLastOptainedRS := nil;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractDBLibStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  InternalExecute;
  FetchResults;
  if GetMoreResults then begin
    Result := FLastOptainedRS;
    FOpenResultSet := Pointer(Result);
    FLastOptainedRS := nil;
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
function TZAbstractDBLibStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  InternalExecute;
  FetchResults;
  Result := GetUpdateCount;
end;

{**
  Fetches all results and creates a cachedresultset object for each resultset
  and a ZAnyValue object for each count value.
}
procedure TZAbstractDBLibStatement.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
  RowsAffected: Integer;
  Status: Integer;
begin
  while True do begin
    Status := FPlainDriver.dbresults(FHandle);
    if Status = NO_MORE_RESULTS then
      Break;
    if Status <> DBSUCCEED then
      FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS/dbresults');
    Status := FPlainDriver.dbcmdrow(FHandle);
    if Status = DBSUCCEED then begin
      {EH: Developer notes:
       the TDS protocol does NOT support any stmt handles. All actions are
       executed sequentially so in ALL cases we need cached Results NO WAY around!!!}
      NativeResultSet := TZDBLibResultSet.Create(Self, Self.SQL, FUserEncoding);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet,
        Self.SQL, TZDBLibCachedResolver.Create(Self, NativeResultSet.GetMetaData), ConSettings);
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last;  //!!!Just to invoke fetchall
      CachedResultSet.BeforeFirst;
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResults.Add(CachedResultSet);
    end else begin
      RowsAffected := FPlainDriver.dbCount(FHandle);
      if RowsAffected > -1 then
        FResults.Add(TZAnyValue.CreateWithInteger(RowsAffected));
    end;
    FPlainDriver.dbCanQuery(FHandle);
  end;
  if BindList.HasOutOrInOutOrResultParam then
    CreateOutParamResultSet;
end;

procedure TZAbstractDBLibStatement.FlushPendingResults;
var I: Integer;
begin
  if FLastOptainedRS <> nil then begin
    FLastOptainedRS.Close;
    FLastOptainedRS := nil;
  end;
  FLastOptainedRS := nil;
  for I := 0 to FResults.Count -1 do
    if Supports(FResults[I], IZResultSet, FLastOptainedRS) then begin
      FLastOptainedRS.Close;
      FLastOptainedRS := nil;
    end;
  FResults.Clear;
end;

{ TZAbstracDBLibSQLStatement }

procedure TZAbstracDBLibSQLStatement.InternalExecute;
var Raw: RawByteString;
begin
  Raw := GetRawSQL;
  if FDBLibConnection.GetProvider = dpMsSQL then
    //This one is to avoid a bug in dblib interface as it drops a single backslash before line end
    Raw := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}StringReplace(Raw, '\'#13, '\\'#13, [rfReplaceAll])
  else
    //This one is to avoid sybase error: Invalid operator for datatype op: is null type: VOID TYPE
    Raw := StringReplaceAll_CS_LToEQ(Raw, RawByteString(' AND NULL IS NULL'), EmptyRaw);

  FHandle := FDBLibConnection.GetConnectionHandle;
  //2018-09-16 Commented by marsupilami79 because this hides errors in the logic
  //result sets might get only partial data without an error
  //if FPlainDriver.dbcancel(FHandle) <> DBSUCCEED then
  //  FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbcmd(FHandle, Pointer(Raw)) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, Raw);

  if FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, Raw);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, Raw);
end;

{ TZDBLibPreparedStatementEmulated }

function TZDBLibPreparedStatementEmulated.GetInParamLogValue(
  ParamIndex: Integer): RawByteString;
var Bind: PZBindValue;
begin
  Bind := BindList[ParamIndex];
  if Bind.BindType = zbtNull then
    Result := '(NULL)'
  else if Bind.BindType = zbtArray then
    Result := '(ARRAY)'
  else case Bind.SQLType of
    stBoolean:      if PByte(Bind.Value)^ = Ord('0')
                    then Result := '(FALSE)'
                    else Result := '(TRUE)';
    stAsciiStream:  Result := '(CLOB)';
    stBinaryStream: Result := '(BLOB)';
    else            Result := RawByteString(Bind.Value);
  end;
end;

function TZDBLibPreparedStatementEmulated.GetRawSQL: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
  Bind: PZBindValue;
begin
  ParamIndex := 0;
  Result := EmptyRaw;
  for I := 0 to High(FCachedQueryRaw) do
    if IsParamIndex[i] then begin
      Bind := BindList[ParamIndex];
      if Bind.BindType = zbtNull
      then if FInParamDefaultValues[ParamIndex] <> '' then
        ToBuff(FInParamDefaultValues[ParamIndex], Result)
        else ToBuff('null', Result)
      else begin
        if (Bind.SQLType in [stString, stAsciiStream]) and (Bind.BindType = zbtUTF8String) and not FIsNCharIndex[ParamIndex] then
          ToBuff('N', Result);
        ToBuff(RawByteString(Bind.Value), Result);
      end;
      Inc(ParamIndex);
    end else
      ToBuff(FCachedQueryRaw[I], Result);
  FlushBuff(Result);
end;

{$IFNDEF NO_ANSISTRING}
procedure TZDBLibPreparedStatementEmulated.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
var P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  P := Pointer(Value);
  L := Length(Value);
  if (FClientCP = zOSCodePage) or not FIsNCharIndex[ParameterIndex] then begin
    FRawTemp := SQLQuotedStr(P, L, #39); //localize -> no hidden LStrClear in call
    BindList.Put(ParameterIndex, stString, FRawTemp, zOSCodePage)
  end else begin
    FUniTemp := PRawToUnicode(P, L, zOSCodePage);
    SetUnicodeString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FUniTemp);
  end;
end;
{$ENDIF NO_ANSISTRING}

procedure TZDBLibPreparedStatementEmulated.SetBigDecimal(ParameterIndex: Integer;
  const Value: TBCD);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stBigDecimal, BcdToSQLRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetBlob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
var InParamIdx: Integer;
  RefCntLob: IZBlob;
  CP: Word;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  InParamIdx := Index;
  CheckParameterIndex(InParamIdx);
  RefCntLob := Value; //inc RefCount
  if (RefCntLob <> nil) and not RefCntLob.IsEmpty then
    if (SQLType in [stAsciiStream, stUnicodeStream]) then begin
      if (FClientCP = zCP_UTF8) or FIsNCharIndex[Index]
      then CP := zCP_UTF8
      else CP := FClientCP;
      if Value.IsClob then begin
        RefCntLob.GetPAnsiChar(CP);
        FRawTemp := SQLQuotedStr(RefCntLob.GetPAnsiChar(CP), refCntLob.Length, AnsiChar(#39))
      end else begin
        FRawTemp := GetValidatedAnsiStringFromBuffer(Value.GetBuffer, Value.Length, ConSettings, CP);
        FRawTemp := SQLQuotedStr(FRawTemp, AnsiChar(#39));
      end;
      BindList.Put(Index, stAsciiStream, FRawTemp, CP);
    end else
      BindList.Put(Index, stBinaryStream, GetSQLHexAnsiString(RefCntLob.GetBuffer, RefCntLob.Length, True), FClientCP)
  else BindList.SetNull(Index, SQLType);
end;

procedure TZDBLibPreparedStatementEmulated.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stBoolean, BoolStrIntsRaw[Value], FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stByte, IntToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetBytes(ParameterIndex: Integer;
  const Value: TBytes);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stBytes, GetSQLHexAnsiString(Pointer(Value), Length(Value), True), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
var CP: Word;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if (Value.CP = FClientCP) or ((Value.CP = zCP_UTF8) and FIsNCharIndex[ParameterIndex]) then begin
    FRawTemp := SQLQuotedStr(PAnsiChar(Value.P), Value.Len, AnsiChar(#39));
    BindList.Put(ParameterIndex, stString, FRawTemp, Value.CP);
  end else begin
    if FIsNCharIndex[ParameterIndex] or (FClientCP = zCP_UTF8)
    then CP := zCP_UTF8
    else CP := FClientCP;
    if Value.CP = zCP_UTF16 then
      fRawTemp := PUnicodeToRaw(Value.P, Value.Len, CP)
    else begin
      fUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
      fRawTemp := ZUnicodeToRaw(fUniTemp, CP)
    end;
    FRawTemp := SQLQuotedStr(fRawTemp, AnsiChar(#39));
    BindList.Put(ParameterIndex, stString, FRawTemp, Value.CP);
  end;
end;

procedure TZDBLibPreparedStatementEmulated.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stCurrency, CurrToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetDate(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stDate, ZSysUtils.DateTimeToRawSQLDate(Value, ConSettings.WriteFormatSettings, True), FClientCP);
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZDBLibPreparedStatementEmulated.SetDouble(ParameterIndex: Integer;
  const Value: Double);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stDouble, FloatToSQLRaw(Value), FClientCP);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZDBLibPreparedStatementEmulated.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stFloat, FloatToSQLRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetGuid(ParameterIndex: Integer;
  const Value: TGUID);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stGUID, GUIDToRaw(Value, [guidWithBrackets, guidQuoted]), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetInt(ParameterIndex, Value: Integer);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stInteger, IntToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetLong(ParameterIndex: Integer;
  const Value: Int64);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stLong, IntToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.SetNull(ParameterIndex, SQLType);
end;

procedure TZDBLibPreparedStatementEmulated.SetRawByteString(ParameterIndex: Integer;
  const Value: RawByteString);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  FRawTemp := SQLQuotedStr(Value, AnsiChar(#39));
  BindList.Put(ParameterIndex, stString, FRawTemp, FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stShort, IntToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stSmall, IntToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetString(ParameterIndex: Integer;
  const Value: String);
{$IFNDEF UNICODE}
var CP: Word;
  P: PAnsichar;
  L: LengthInt;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  SetUnicodeString(ParameterIndex, Value);
  {$ELSE}
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if (FClientCP = zCP_UTF8) or FIsNCharIndex[ParameterIndex]
  then CP := zCP_UTF8
  else CP := FClientCP;
  if ConSettings.AutoEncode then begin
    FRawTemp := ConSettings.ConvFuncs.ZStringToRaw(Value, ConSettings.CTRL_CP, CP);
    P := Pointer(FRawTemp);
    L := Length(FRawTemp);
  end else begin
    P := Pointer(Value);
    L := Length(Value);
  end;
  FRawTemp := SQLQuotedStr(P, L, #39);
  BindList.Put(ParameterIndex, stString, FRawTemp, CP);
  {$ENDIF}
end;

procedure TZDBLibPreparedStatementEmulated.SetTime(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stTime, ZSysUtils.DateTimeToRawSQLTime(Value, ConSettings.WriteFormatSettings, True), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetTimestamp(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stTimeStamp, ZSysUtils.DateTimeToRawSQLTimeStamp(Value, ConSettings.WriteFormatSettings, True), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stLongWord, IntToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stULong, IntToRaw(Value), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetUnicodeString(ParameterIndex: Integer;
  const Value: ZWideString);
var CP: Word;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if (FClientCP = zCP_UTF8) or FIsNCharIndex[ParameterIndex]
  then CP := zCP_UTF8
  else CP := FClientCP;
  FRawTemp := PUnicodeToRaw(Pointer(Value), Length(Value), CP);
  FRawTemp := SQLQuotedStr(FRawTemp, AnsiChar(#39)); //localize -> no hidden LStrClear in call
  BindList.Put(ParameterIndex, stString, FRawTemp, CP)
end;

{$IFNDEF NO_UTF8STRING}
procedure TZDBLibPreparedStatementEmulated.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
var P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  P := Pointer(Value);
  L := Length(Value);
  if (FClientCP = zCP_UTF8) or FIsNCharIndex[ParameterIndex] then begin
    FRawTemp := SQLQuotedStr(P, L, AnsiChar(#39)); //localize -> no hidden LStrClear in call
    BindList.Put(ParameterIndex, stString,FRawTemp, zCP_UTF8)
  end else begin
    FUniTemp := PRawToUnicode(P, L, zCP_UTF8);
    SetUnicodeString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FUniTemp);
  end;
end;
{$ENDIF NO_UTF8STRING}

procedure TZDBLibPreparedStatementEmulated.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stWord, IntToRaw(Value), FClientCP);
end;

{ TZDBLibStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
}
constructor TZDBLibStatement.Create(const Connection: IZConnection;
  const Info: TStrings);
begin
  inherited Create(Connection, '', Info)
end;

function TZDBLibStatement.GetRawSQL: RawByteString;
begin
  Result := fASQL;
end;

{ TZDBLIBPreparedRPCStatement }

{**
  Binds the input parameters
}
procedure TZDBLIBPreparedRPCStatement.BindInParameters;
var I: Integer;
  Bind: PZBindValue;
begin
  if FPLainDriver.dbRPCInit(FHandle, Pointer(fASQL), 0) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCInit');
  for i := 1 to BindList.Count -1 do begin //skip the returnparam
    Bind := BindList[I];
    case Bind.BindType of
      zbtNull: FPlainDriver.dbRpcParam(FHandle, Pointer(FParamNames[I]), Ord(Bind.ParamType >= pctInOut),
        Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, 0, nil);
      zbtPointer: FPlainDriver.dbRpcParam(FHandle, Pointer(FParamNames[I]), Ord(Bind.ParamType >= pctInOut),
        Ord(tdsBit), -1, 0, @Bind.Value); //stBoolean
      zbt4Byte: FPlainDriver.dbRpcParam(FHandle, Pointer(FParamNames[I]), Ord(Bind.ParamType >= pctInOut),
        Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, -1, @Bind.Value);
      zbt8Byte: FPlainDriver.dbRpcParam(FHandle, Pointer(FParamNames[I]), Ord(Bind.ParamType >= pctInOut),
        Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, -1, {$IFDEF CPU64}@{$ENDIF}Bind.Value);
      zbtRawString, zbtUTF8String {$IFNDEF NEXTGEN}, zbtAnsiString{$ENDIF}:
        FPlainDriver.dbRpcParam(FHandle, Pointer(FParamNames[I]), Ord(Bind.ParamType >= pctInOut),
          Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, Length(RawByteString(Bind.Value)), Bind.Value);
    end;
  end;
end;

constructor TZDBLIBPreparedRPCStatement.Create(const Connection: IZConnection;
  const RemoteProcedureName: String; const Info: TStrings);
begin
  inherited Create(Connection, Trim(RemoteProcedureName), Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := TZDBLIBPLainDriver(Connection.GetIZPlainDriver.GetInstance);
  FHandle := FDBLibConnection.GetConnectionHandle;
  FResults := TZCollection.Create;
  if DefineStatementParameter(Self, DSProps_ResetCodePage, '') = 'UTF8' then
    FUserEncoding := ceUTF8
  else
    Self.FUserEncoding := ceDefault;
end;

procedure TZDBLIBPreparedRPCStatement.CreateOutParamResultSet;
var I, N, RetType, Len: Integer;
  BindValue: PZBindValue;
  Data: Pointer;
  OutDBDATETIME: TDBDATETIME;
  ColumnsInfo: TObjectList;
  ColumnInfo: TZColumnInfo;
  RS: TZVirtualResultSet;
  procedure SetBytes;
  var Bts: TBytes;
  begin
    Bts := BufferToBytes(Data, Len);
    BindList.Put(I, stBytes, Bts);
  end;
begin
  I := 0;
  CheckParameterIndex(I); //check if RetValue has been initialized
  if FPLainDriver.dbHasRetStat(FHandle) = DBSUCCEED then begin
    I := FPlainDriver.dbRetStatus(FHandle);
    BindList.Put(0, stInteger, P4Bytes(@I));
  end else
    BindList.SetNull(0, stInteger);
  N := 1;
  ColumnsInfo := TObjectList.Create;
  ColumnInfo := TZColumnInfo.Create;
  ColumnInfo.ColumnLabel := '@RETURN_VALUE'; //that's what ADO returns for
  ColumnInfo.ColumnType := stInteger;
  ColumnsInfo.Add(ColumnInfo);
  try
    { first fetch data into bind buffer }
    for i := 1 to BindList.Count -1 do begin
      BindValue := BindList[I];
      if Ord(BindValue.ParamType) >= Ord(pctInOut) then begin
        ColumnInfo := TZColumnInfo.Create;
        Data := FPLainDriver.dbretname(FHandle, N);
        {$IFDEF UNICODE}
        ColumnInfo.ColumnLabel := PRawToUnicode(Data, StrLen(Data), FClientCP);
        {$ELSE}
        ZSetString(PAnsiChar(Data), StrLen(Data), fRawTemp);
        ColumnInfo.ColumnLabel := ConSettings.ConvFuncs.ZRawToString(fRawTemp, FClientCP, ConSettings.CTRL_CP);
        {$ENDIF}
        RetType := FPLainDriver.dbRetType(FHandle, N);
        Data := FPlainDriver.dbRetData(FHandle, N);
        Len := FPLainDriver.dbRetLen(FHandle, N);
        ColumnInfo.Precision := Len;
        if (Data = nil) or (RetType = Ord(tdsVoid)) then
          BindList.SetNull(I, ConvertTDSTypeToSqlType(TTDSType(RetType), Len, 0, ConSettings.CPType))
        else case TTDSType(RetType) of
          tdsNVarChar, tdsBigNChar, tdsBigNVarChar:
            begin
              ZSetString(Data, Len, fRawTemp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF});
              BindList.Put(I, stString, fRawTemp, zCP_UTF8);
            end;
          tdsChar, tdsVarchar, tdsBigChar, tdsBigVarChar:
              if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then begin
                ZSetString(Data, Len, fRawTemp{$IFDEF WITH_RAWBYTESTRING}, FClientCP{$ENDIF});
                BindList.Put(I, stString, fRawTemp, FClientCP);
              end else case ZDetectUTF8Encoding(Data, Len) of
                etUTF8: begin
                    ZSetString(Data, Len, fRawTemp{$IFDEF WITH_RAWBYTESTRING}, zCP_UTF8{$ENDIF});
                    BindList.Put(I, stString, fRawTemp, zCP_UTF8);
                  end;
                etUSASCII: begin
                    ZSetString(Data, Len, fRawTemp{$IFDEF WITH_RAWBYTESTRING}, FClientCP{$ENDIF});
                    BindList.Put(I, stString, fRawTemp, FClientCP);
                  end;
                else begin
                    ZSetString(Data, Len, fRawTemp{$IFDEF WITH_RAWBYTESTRING}, FClientCP{$ENDIF});
                    BindList.Put(I, stString, fRawTemp, FClientCP);
                end;
              end;
          tdsBinary, tdsVarBinary, tdsBigBinary, tdsBigVarBinary: SetBytes;
          tdsInt1: if BindValue.SQLType = stShort then begin
                     BindList.Put(I, stShort, P4Bytes(@I));
                     PShortInt(@BindValue.Value)^ := PShortInt(Data)^;
                   end else begin
                     BindList.Put(I, stByte, P4Bytes(@I));
                     PByte(@BindValue.Value)^ := PByte(Data)^;
                   end;
          tdsInt2: if BindValue.SQLType = stWord then begin
                     BindList.Put(I, stWord, P4Bytes(@I));
                     PWord(@BindValue.Value)^ := PWord(Data)^;
                   end else begin
                     BindList.Put(I, stSmall, P4Bytes(@I));
                     PSmallInt(@BindValue.Value)^ := PSmallInt(Data)^;
                   end;
          tdsInt4:  if BindValue.SQLType = stLongWord
                    then BindList.Put(I, stLongWord, P4Bytes(Data))
                    else BindList.Put(I, stInteger, P4Bytes(Data));
          tdsInt8:  if BindValue.SQLType = stULong
                    then BindList.Put(I, stULong, P8Bytes(Data))
                    else BindList.Put(I, stLong, P8Bytes(Data));
          tdsFlt4:  BindList.Put(I, stFloat, P4Bytes(Data));
          tdsFlt8:  BindList.Put(I, stDouble, P8Bytes(Data));
          tdsNumeric,
          tdsDecimal:
            begin
              Len := FPlainDriver.dbConvert(FHandle, RetType, Data, Len, Ord(tdsVarChar),
                @fABuffer[0], SizeOf(fABuffer));
              ZSetString(PAnsichar(@fABuffer[0]), Len, fRawTemp);
              BindList.Put(I, stBigDecimal, fRawTemp, FClientCP);
            end;
          tdsMoney,
          tdsMoney4:
            begin
              Len := FPlainDriver.dbConvert(FHandle, RetType, Data, Len, Ord(tdsVarChar),
                @fABuffer[0], SizeOf(fABuffer));
              ZSetString(PAnsichar(@fABuffer[0]), Len, fRawTemp);
              BindList.Put(I, stCurrency, fRawTemp, FClientCP);
            end;
          tdsDateTime4, tdsDateTimeN:
            begin
              FPLainDriver.dbConvert(FHandle, RetType, Data, Len, RetType, @OutDBDATETIME, 8);
              PDouble(@fABuffer[0])^ := OutDBDATETIME.dtdays + 2 + (OutDBDATETIME.dttime / 25920000);
              BindList.Put(I, stTimeStamp, P8Bytes(@fABuffer[0]));
            end;
          tdsDateTime:
            begin
              PDouble(@fABuffer[0])^ := PDBDATETIME(Data).dtdays + 2 + (PDBDATETIME(Data).dttime / 25920000);
              BindList.Put(I, stTimeStamp, P8Bytes(@fABuffer[0]));
            end;
          tdsImage: BindList.Put(I, stBinaryStream, TZAbstractBlob.CreateWithData(Data, Len));
          tdsText:  BindList.Put(I, stBinaryStream, TZAbstractClob.CreateWithData(Data, Len, FClientCP, ConSettings));
          tdsNText: BindList.Put(I, stBinaryStream, TZAbstractClob.CreateWithData(Data, Len, zCP_UTF8, ConSettings));
          tdsBit: BindList.Put(I, PByte(Data)^ <> 0);
          tdsUnique: BindList.Put(I, PGuid(Data)^);
          else BindList.SetNull(I, BindValue.SQLType);
            {
            tdsFltN,
            tdsFltN,
            tdsMoneyN:
            tdsIntN:
            tdsVariant:
            tdsBitN:
            tdsUDT:
            tdsMSXML:}
        end;
        if (BindValue.SQLType in [stString, stAsciiStream]) and (ConSettings.CPType = cCP_UTF16)
        then ColumnInfo.ColumnType := TZSQLType(Ord(BindValue.SQLType)+1)
        else ColumnInfo.ColumnType := BindValue.SQLType;
        ColumnsInfo.Add(ColumnInfo);
        Inc(N);
      end;
    end;
  finally
    RS := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '', ConSettings);
    ColumnsInfo.Free;
    FOutParamresultSet := RS;
    RS.MoveToInsertRow;
    N := FirstDbcIndex;
    for i := 0 to BindList.Count -1 do begin
      BindValue := BindList[I];
      if Ord(BindValue.ParamType) >= Ord(pctInOut) then begin
        case BindValue.SQLType of
          stBoolean: RS.UpdateBoolean(N, PByte(@BindValue.Value)^ <> 0);
          stShort: RS.UpdateShort(N, PShortInt(@BindValue.Value)^);
          stByte: RS.UpdateByte(N, PByte(@BindValue.Value)^);
          stSmall: RS.UpdateSmall(N, PSmallInt(@BindValue.Value)^);
          stWord: RS.UpdateWord(N, PWord(@BindValue.Value)^);
          stInteger: RS.UpdateInt(N, PInteger(@BindValue.Value)^);
          stLongword: RS.UpdateUInt(N, PCardinal(@BindValue.Value)^);
          stLong: RS.UpdateLong(N, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
          stULong: RS.UpdateULong(N, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
          stFloat: RS.UpdateFloat(N, PSingle(@BindValue.Value)^);
          stDouble, stDate, stTime, stTimeStamp:
            RS.UpdateDouble(N, PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
          stCurrency,
          stBigDecimal,
          stString, stUnicodeString: RS.UpdateRawByteString(N, RawByteString(BindValue.Value));
          stGUID: RS.UpdateBytes(N, BufferToBytes(BindValue.Value, SizeOf(TGUID)));
          stBytes: RS.UpdateBytes(N, TBytes(BindValue.Value));
          stAsciiStream, stUnicodeStream, stBinaryStream: RS.UpdateLob(N, IZBlob(BindValue.Value));
        end;
        Inc(N);
      end;
    end;
    RS.InsertRow;
    RS.BeforeFirst;
  end;
end;

function TZDBLIBPreparedRPCStatement.Execute(const SQL: RawByteString): Boolean;
begin
  Result := False;
  RaiseUnsupportedException
end;

function TZDBLIBPreparedRPCStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  BindInParameters;
  if FPLainDriver.dbRpcExec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCExec');
  FetchResults;
  Result := (FResults.Count > 0) and Supports(FResults[0], IZResultSet, FLastResultSet);
end;

function TZDBLIBPreparedRPCStatement.Execute(const SQL: ZWideString): Boolean;
begin
  Result := False;
  RaiseUnsupportedException
end;

function TZDBLIBPreparedRPCStatement.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
begin
  Result := nil;
  RaiseUnsupportedException
end;

function TZDBLIBPreparedRPCStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if not ExecutePrepared then
    while GetMoreResults and (FLastResultSet = nil) do ;
  Result := FLastResultSet;
  FLastResultSet := nil;
end;

function TZDBLIBPreparedRPCStatement.ExecuteQuery(
  const SQL: ZWideString): IZResultSet;
begin
  Result := nil;
  RaiseUnsupportedException
end;

function TZDBLIBPreparedRPCStatement.ExecuteUpdate(
  const SQL: ZWideString): Integer;
begin
  Result := -1;
  RaiseUnsupportedException
end;

function TZDBLIBPreparedRPCStatement.ExecuteUpdate(
  const SQL: RawByteString): Integer;
begin
  Result := -1;
  RaiseUnsupportedException
end;

function TZDBLIBPreparedRPCStatement.ExecuteUpdatePrepared: Integer;
begin
  if ExecutePrepared then
    while GetMoreResults and (FLastResultSet <> nil) do ;
  Result := LastUpdateCount;
end;

procedure TZDBLIBPreparedRPCStatement.InternalExecute;
begin
  if FPLainDriver.dbRpcExec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCExec');
end;

procedure TZDBLIBPreparedRPCStatement.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
begin
  inherited;
  FParamNames[ParameterIndex] := ConSettings.ConvFuncs.ZStringToRaw(Name, ConSettings.CTRL_CP, FClientCP);
end;

{$IFNDEF NO_ANSISTRING}
procedure TZDBLIBPreparedRPCStatement.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FClientCP = ZOSCodePage
  then BindList.Put(ParameterIndex, stString, Value, ZOSCodePage)
  else SetUnicodeString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PRawToUnicode(Pointer(Value), Length(Value), ZOSCodePage));
end;
{$ENDIF NO_ANSISTRING}

procedure TZDBLIBPreparedRPCStatement.SetBigDecimal(ParameterIndex: Integer;
  const Value: TBCD);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stString, BcdToSQLRaw(Value), FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetBindCapacity(Capacity: Integer);
begin
  inherited;
  SetLength(FParamNames, Capacity);
end;

procedure TZDBLIBPreparedRPCStatement.SetBlob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
begin
  inherited;
end;

procedure TZDBLIBPreparedRPCStatement.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, Value);
  PByte(@BindList[ParameterIndex].Value)^ := Ord(Value);
end;

procedure TZDBLIBPreparedRPCStatement.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stByte, P4Bytes(@ParameterIndex));
  PByte(@BindList[ParameterIndex].Value)^ := Ord(Value);
end;

procedure TZDBLIBPreparedRPCStatement.SetBytes(ParameterIndex: Integer;
  const Value: TBytes);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stBytes, Value);
end;

procedure TZDBLIBPreparedRPCStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  if Value.CP = FClientCP then
    ZSetString(Value.P, Value.Len, FRawTemp{$IFDEF WITH_RAWBYTESTRING}, Value.CP{$ENDIF})
  else if Value.CP = zCP_UTF16 then
    FRawTemp := PUnicodeToRaw(Value.P, Value.Len, FClientCP)
  else begin
    FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
    FRawTemp := PUnicodeToRaw(Pointer(FUniTemp), Length(fUniTemp), FClientCP);
  end;
  SetRawByteString(ParameterIndex, fRawTemp);
end;

procedure TZDBLIBPreparedRPCStatement.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stString, CurrToRaw(Value), FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetDate(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stString, DateTimeToRawSQLDate(Value, ConSettings.WriteFormatSettings, False), FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetDouble(ParameterIndex: Integer;
  const Value: Double);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stFloat, P8Bytes(@Value));
end;

procedure TZDBLIBPreparedRPCStatement.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stFloat, P4Bytes(@Value));
end;

procedure TZDBLIBPreparedRPCStatement.SetGuid(ParameterIndex: Integer;
  const Value: TGUID);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, Value);
end;

procedure TZDBLIBPreparedRPCStatement.SetInt(ParameterIndex, Value: Integer);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stInteger, P4Bytes(@Value));
end;

procedure TZDBLIBPreparedRPCStatement.SetLong(ParameterIndex: Integer;
  const Value: Int64);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stLong, P8Bytes(@Value));
end;

procedure TZDBLIBPreparedRPCStatement.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.SetNull(ParameterIndex, SQLType);
end;

procedure TZDBLIBPreparedRPCStatement.SetRawByteString(ParameterIndex: Integer;
  const Value: RawByteString);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stString, Value, FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stShort, P4Bytes(@ParameterIndex));
  PShortInt(@BindList[ParameterIndex].Value)^ := Value;
end;

procedure TZDBLIBPreparedRPCStatement.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stSmall, P4Bytes(@ParameterIndex));
  PSmallInt(@BindList[ParameterIndex].Value)^ := Value;
end;

procedure TZDBLIBPreparedRPCStatement.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  SetUnicodeString(ParameterIndex, Value);
  {$ELSE}
  if ConSettings.AutoEncode
  then SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings.CTRL_CP, FClientCP))
  else SetRawByteString(ParameterIndex, Value)
  {$ENDIF}
end;

procedure TZDBLIBPreparedRPCStatement.SetTime(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stString, DateTimeToRawSQLTime(Value, ConSettings.WriteFormatSettings, False), FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stString, DateTimeToRawSQLTimeStamp(Value, ConSettings.WriteFormatSettings, False), FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stLongWord, P4Bytes(@Value));
end;

procedure TZDBLIBPreparedRPCStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stULong, P8Bytes(@Value));
end;

procedure TZDBLIBPreparedRPCStatement.SetUnicodeString(ParameterIndex: Integer;
  const Value: ZWideString);
begin
  SetRawByteString(ParameterIndex, PUnicodeToRaw(Pointer(Value), Length(Value), FClientCP));
end;

{$IFNDEF NO_UTF8STRING}
procedure TZDBLIBPreparedRPCStatement.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if FClientCP = zCP_UTF8
  then BindList.Put(ParameterIndex, stString, Value, zCP_UTF8)
  else SetUnicodeString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PRawToUnicode(Pointer(Value), Length(Value), zCP_UTF8));
end;
{$ENDIF NO_UTF8STRING}

procedure TZDBLIBPreparedRPCStatement.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stWord, P4Bytes(@ParameterIndex));
  PWord(@BindList[ParameterIndex].Value)^ := Value;
end;

{ TZDBLibCallableStatement }

function TZDBLibCallableStatement.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement2;
begin
  Result := TZDBLIBPreparedRPCStatement.Create(Connection, StoredProcName, Info);
end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
