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
  TZDBLibEmulatedStatement = class(TZRawParamDetectPreparedStatement)
  private
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: TZDBLIBPLainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    FUserEncoding: TZCharEncoding;
    FLastOptainedRS: IZResultSet;
    FClientCP: Word;
    FIsNCharIndex: TBooleanDynArray;
    function ComposeRawSQLQuery: RawByteString;
  protected
    procedure InternalExecuteStatement(const SQL: RawByteString);
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

  {** Implements Generic DBLib Statement. }
  TZDBLibStatement = class(TZDBLibEmulatedStatement)
  public
    constructor Create(const Connection: IZConnection; const Info: TStrings);
  end;

  TZDBLibPreparedStatementEmulated = class(TZDBLibEmulatedStatement, IZPreparedStatement)
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

  TZDBLibCallableStatement = class(TZAbstractCallableStatement)
  private
    FSQL: string;
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: TZDBLIBPLainDriver;
    FHandle: PDBPROCESS;
    FLastRowsAffected: Integer;//Workaround for sybase
    FRetrievedResultSet: IZResultSet;
    FRetrievedUpdateCount: Integer;
    FUserEncoding: TZCharEncoding;
    procedure FetchResults;
  protected
    procedure SetInParamCount(NewParamCount: Integer); override;
  public
    constructor Create(const Connection: IZConnection; const ProcName: string; Info: TStrings);
    procedure BeforeClose; override;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SqlType: Integer); override;
    function GetMoreResults: Boolean; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
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
  ZDbcLogging, ZDbcCachedResultSet, ZDbcDbLibUtils, ZDbcDbLibResultSet,
  ZVariant, ZDbcUtils, ZEncoding, ZDbcResultSet, ZDbcProperties,
  {$IFDEF WITH_UNITANSISTRINGS} AnsiStrings, {$ENDIF}
  ZFastCode, ZMessages;

{ TZDBLibEmulatedStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param SQL the command text.
  @param Info a statement parameters.
}
constructor TZDBLibEmulatedStatement.Create(
  const Connection: IZConnection; const SQL: string; const Info: TStrings);
begin
  FNCharDetected := @FIsNCharIndex;
  inherited Create(Connection, SQL, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := TZDBLIBPLainDriver(Connection.GetIZPlainDriver.GetInstance);
  //if Assigned(FDBLibConnection) then
    //FPLainDriver := FDBLibConnection.GetPlainDriver;
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
  FClientCP := ConSettings.ClientCodePage.CP;
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
function TZDBLibEmulatedStatement.GetMoreResults: Boolean;
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
function TZDBLibEmulatedStatement.GetUpdateCount: Integer;
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

{**
  Executes a Statement.
  Used internally to execute statements.

  @param Handle a DBLib connection handle.
  @sql string containing the statements to execute
}
procedure TZDBLibEmulatedStatement.InternalExecuteStatement(
  const SQL: RawByteString);
var Ansi: RawByteString;
begin
  if FDBLibConnection.GetProvider = dpMsSQL then
    //This one is to avoid a bug in dblib interface as it drops a single backslash before line end
    Ansi := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}StringReplace(SQL, '\'#13, '\\'#13, [rfReplaceAll])
  else
    //This one is to avoid sybase error: Invalid operator for datatype op: is null type: VOID TYPE
    Ansi := StringReplaceAll_CS_LToEQ(SQL, RawByteString(' AND NULL IS NULL'), EmptyRaw);

  FHandle := FDBLibConnection.GetConnectionHandle;
  //2018-09-16 Commented by marsupilami79 because this hides errors in the logic
  //result sets might get only partial data without an error
  //if FPlainDriver.dbcancel(FHandle) <> DBSUCCEED then
  //  FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbcmd(FHandle, Pointer(Ansi)) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
end;

procedure TZDBLibEmulatedStatement.Prepare;
begin
  FlushPendingResults;
  inherited Prepare;
end;

procedure TZDBLibEmulatedStatement.Unprepare;
begin
  FlushPendingResults;
  inherited UnPrepare;
end;

function TZDBLibEmulatedStatement.ComposeRawSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
  Bind: PZBindValue;
begin
  ParamIndex := 0;
  Result := '';
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

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZDBLibEmulatedStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  InternalExecuteStatement(ComposeRawSQLQuery);
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
function TZDBLibEmulatedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  InternalExecuteStatement(ComposeRawSQLQuery);
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
function TZDBLibEmulatedStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  InternalExecuteStatement(ComposeRawSQLQuery);
  FetchResults;
  Result := GetUpdateCount;
end;

{**
  Fetches all results and creates a cachedresultset object for each resultset
  and a ZAnyValue object for each count value.
}
procedure TZDBLibEmulatedStatement.FetchResults;
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
      CachedResultSet.Last;
      CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResults.Add(CachedResultSet);
    end else begin
      RowsAffected := FPlainDriver.dbCount(FHandle);
      if RowsAffected > -1 then
        FResults.Add(TZAnyValue.CreateWithInteger(RowsAffected));
    end;
    FPlainDriver.dbCanQuery(FHandle);
  end;
end;

procedure TZDBLibEmulatedStatement.FlushPendingResults;
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

constructor TZDBLibCallableStatement.Create(const Connection: IZConnection;
  const ProcName: string; Info: TStrings);
begin
  inherited Create(Connection, ProcName, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPLainDriver := TZDBLIBPLainDriver(FDBLibConnection.GetIZPlainDriver.GetInstance);
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  {note: this is a hack! Purpose is to notify Zeos all Character columns are
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

procedure TZDBLibCallableStatement.BeforeClose;
begin
  FRetrievedResultSet := nil;
  inherited BeforeClose;
end;

procedure TZDBLibCallableStatement.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
begin
//Sybase does not seem to return dbCount at all, so a workaround is made
  FLastRowsAffected := -2;
  while FPlainDriver.dbresults(FHandle) = DBSUCCEED do
  begin
    if FPlainDriver.dbcmdrow(FHandle) = DBSUCCEED then
    begin
      NativeResultSet := TZDBLibResultSet.Create(Self, FSQL);
      NativeResultSet.SetConcurrency(rcReadOnly);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet, FSQL,
        TZDBLibCachedResolver.Create(Self, NativeResultSet.GetMetaData), ConSettings);
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last;
      CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResultSets.Add(CachedResultSet);
    end
    else
    begin
      FLastRowsAffected := FPlainDriver.dbCount(FHandle);
      if FLastRowsAffected > -1 then
        FResultSets.Add(TZAnyValue.CreateWithInteger(FLastRowsAffected));
    end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
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
function TZDBLibCallableStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZAnyValue;
begin
  Result := False;
  FRetrievedResultSet := nil;
  FRetrievedUpdateCount := -1;
  if FResultSets.Count > 0 then
  begin
    try
      Result := Supports(FResultSets[0], IZResultSet, ResultSet);
      if Result then
      begin
        FRetrievedResultSet := ResultSet;
        FRetrievedUpdateCount := 0;
      end
      else
        if Supports(FResultSets[0], IZAnyValue, UpdateCount) then
          FRetrievedUpdateCount := UpdateCount.GetInteger;
      FResultSets.Delete(0);
    finally
      ResultSet := nil;
      UpdateCount := nil;
    end;
  end;
end;

function TZDBLibCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if not ExecutePrepared then
    while not GetMoreResults and (FRetrievedUpdateCount <> -1) do;
  Result := FRetrievedResultSet;
  FRetrievedResultSet := nil;
end;

function TZDBLibCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if ExecutePrepared then
    while GetMoreResults and (FRetrievedUpdateCount = -1) do;
  Result := FRetrievedUpdateCount;
  FRetrievedResultSet := nil;
end;

procedure TZDBLibCallableStatement.RegisterOutParameter(ParameterIndex: Integer;
  SqlType: Integer);
begin
  SetOutParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  OutParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TZSqlType(SqlType);

  //Count inparams must equal count outparams to correct set paramters
  if InParamCount < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} then
    SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
end;

type TZDbLibParam = record
  AsString: RawByteString;
  CharRec: TZCharRec;
  AsBytes: TBytes;
  TempBlob: IZBlob;
  case word of
    0: (AsBoolean: Boolean);
    1: (AsByte: Byte);
    2: (AsSmall: SmallInt);
    3: (AsInteger: Integer);
    4: (AsFloat: Single);
    5: (AsDouble: Double);
    6: (AsDBDATETIME: DBDATETIME);
end;

function TZDBLibCallableStatement.ExecutePrepared: Boolean;
var
  S: RawByteString;
  I, ParamIndex, DatLen: Integer;
  RetParam: Byte;
  ParamType: TZSQLType;
  P: Pointer;
  Len: NativeUInt;
  RetType: DBINT;
  Temp: TZVariant;
  Params: array of TZDbLibParam;

  OutString: RawByteString;
  OutBytes: TBytes;
  OutDouble: Double;
  OutDBDATETIME: DBDATETIME;
begin
  S := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}Trim(ASql);
  if FPLainDriver.dbRPCInit(FHandle, Pointer(S), 0) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCInit');

  SetLength(Params, InParamCount);

  for I := 1 to InParamCount - 1 do//The 0 parameter is the return value
  begin
    RetParam := 0;
    if OutParamTypes[I] <> stUnknown then
      RetParam := DBRPCRETURN;

    ParamType := InParamTypes[I];
    if ParamType = stUnknown then
      ParamType := OutParamTypes[I];

    if SoftVarManager.IsNull(InParamValues[I]) and (InParamTypes[I] <> stUnknown) then
      FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
        Ord(ConvertSqlTypeToTDSType(InParamTypes[I])), -1, 0, nil)
    else
      case ParamType of
        stBoolean:
          begin
            Params[I].AsBoolean := SoftVarManager.GetAsBoolean(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsBit), -1, -1, @(Params[I].AsBoolean));
          end;
        stByte:
          begin
            Params[I].AsByte := Byte(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsInt1), -1, -1, @(Params[I].AsByte));
          end;
        stShort, stSmall:
          begin
            Params[I].AsSmall := SmallInt(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsInt2), -1, -1, @(Params[I].AsSmall));
          end;
        stWord, stInteger:
          begin
            Params[I].AsInteger := Integer(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsInt4), -1, -1, @(Params[I].AsInteger));
          end;
        stFloat:
          begin
            Params[I].AsFloat := SoftVarManager.GetAsDouble(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsFlt4), -1, -1, @(Params[I].AsFloat));
          end;
        stLong, stULong, stDouble, stBigDecimal, stCurrency:
          begin
            Params[I].AsFloat := SoftVarManager.GetAsDouble(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsFlt8), -1, -1, @(Params[I].AsDouble));
          end;
        stString, stUnicodeString:
          if IsNCharIndex[i] then
          begin
            Params[I].CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], zCP_UTF8);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsVarchar),
              -1, Max(1, Params[I].CharRec.Len), Params[I].CharRec.P);
          end else
          begin
            Params[I].CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], ConSettings^.ClientCodePage^.CP);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsVarchar),
              -1, Max(1, Params[I].CharRec.Len), Params[I].CharRec.P);
          end;
        stDate:
          begin
            Params[I].AsString := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(InParamValues[I]),
              ConSettings^.WriteFormatSettings, False);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, ord(tdsChar),
              -1, ConSettings^.WriteFormatSettings.DateFormatLen, Pointer(Params[I].AsString));
          end;
        stTime:
          begin
            Params[I].AsString := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(InParamValues[I]),
              ConSettings^.WriteFormatSettings, False);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, ord(tdsChar),
              -1, ConSettings^.WriteFormatSettings.TimeFormatLen, Pointer(Params[I].AsString));
          end;
        stTimeStamp:
          begin
            Params[I].AsString := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(InParamValues[I]),
              ConSettings^.WriteFormatSettings, False);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, ord(tdsChar),
              -1, ConSettings^.WriteFormatSettings.DateTimeFormatLen, Pointer(Params[I].AsString));
          end;
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            Params[I].TempBlob := SoftVarManager.GetAsInterface(InParamValues[I]) as IZBlob;
            if ParamType = stBinaryStream then
              FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsImage),
                -1, Max(1, Params[I].TempBlob.Length), Params[I].TempBlob.GetBuffer)
            else
              if IsNCharIndex[i] then
              begin
                if Params[I].TempBlob.IsClob then
                begin
                  Params[I].CharRec.P := Params[I].TempBlob.GetPAnsiChar(zCP_UTF8);
                  Params[I].CharRec.Len := Max(1, Params[I].TempBlob.Length);
                end
                else
                begin
                  Params[I].AsString := GetValidatedAnsiStringFromBuffer(Params[I].TempBlob.GetBuffer, Params[I].TempBlob.Length, ConSettings, zCP_UTF8);
                  if Pointer(Params[I].AsString) = nil then
                  begin
                    Params[I].CharRec.P := PEmptyAnsiString;
                    Params[I].CharRec.Len := 1;
                  end
                  else
                  begin
                    Params[I].CharRec.P := Pointer(Params[I].AsString);
                    Params[I].CharRec.Len := {%H-}PLengthInt(NativeUInt(Params[I].AsString) - StringLenOffSet)^;
                  end;
                end;
                FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsText),
                    -1, Params[I].CharRec.Len, Params[I].CharRec.P)
              end
              else
              begin
                if Params[I].TempBlob.IsClob then
                begin
                  Params[I].CharRec.P := Params[I].TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                  Params[I].CharRec.Len := Max(1, Params[I].TempBlob.Length);
                end
                else
                begin
                  Params[I].AsString := GetValidatedAnsiStringFromBuffer(Params[I].TempBlob.GetBuffer,
                    Params[I].TempBlob.Length, ConSettings);
                  if Pointer(Params[I].AsString) = nil then
                  begin
                    Params[I].CharRec.P := PEmptyAnsiString;
                    Params[I].CharRec.Len := 1;
                  end
                  else
                  begin
                    Params[I].CharRec.P := Pointer(Params[I].AsString);
                    Params[I].CharRec.Len := {%H-}PLengthInt(NativeUInt(Params[I].AsString) - StringLenOffSet)^;;
                  end;
                end;
                FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsText),
                    -1, Params[I].CharRec.Len, Params[I].CharRec.P)
              end;
          end;
        stBytes:
          begin
            Params[I].AsBytes := SoftVarManager.GetAsBytes(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsBinary),
              -1, Length(Params[I].AsBytes), Pointer(Params[I].AsBytes));
          end;
      else
        FPlainDriver.dbRpcParam(FHandle, nil, 0, Ord(tdsChar), 0, 0, nil);
    end;
  end;

  if FPLainDriver.dbRpcExec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCExec');
  SetLength(Params, 0);
  FetchResults;
  Result := GetMoreResults;

  if FPLainDriver.dbHasRetStat(FHandle) = DBSUCCEED then
    Temp := EncodeInteger(FPlainDriver.dbRetStatus(FHandle))
  else
    Temp := NullVariant;
  if Length(OutParamValues) = 0 then // check if DynArray is initialized for RETURN_VALUE
    SetOutParamCount(1);
  OutParamValues[0] := Temp; //set function RETURN_VALUE
  OutString := '';
  ParamIndex := 1;
  for I := 1 to OutParamCount - 1 do
  begin
    if OutParamTypes[I] = stUnknown then
      Continue;
    RetType := FPLainDriver.dbRetType(FHandle, ParamIndex);
    if (FPlainDriver.dbRetData(FHandle, ParamIndex) = nil) or
       (RetType = Ord(tdsVoid)) then
      Temp := NullVariant
    else
      case TTDSType(RetType) of
        tdsNVarChar, tdsBigNChar, tdsBigNVarChar:
          begin
            ZSetString(FPLainDriver.dbRetData(FHandle, ParamIndex),
              FPLainDriver.dbRetLen(FHandle, ParamIndex), OutString);
            ClientVarManager.SetAsUTF8String(Temp, OutString);
          end;
        tdsChar, tdsVarchar, tdsBigChar, tdsBigVarChar:
          begin
            P := FPLainDriver.dbRetData(FHandle, ParamIndex);
            Len := NativeUInt(FPLainDriver.dbRetLen(FHandle, ParamIndex));
            if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then begin
              ZSetString(P, Len, OutString);
              ClientVarManager.SetAsRawByteString(Temp, OutString);
            end else
              case ZDetectUTF8Encoding(P, Len) of
                etUTF8:
                  begin
                    ZSetString(P, Len, OutString);
                    ClientVarManager.SetAsUTF8String(Temp, OutString);
                  end;
                etUSASCII:
                  begin
                    ZSetString(P, Len, OutString);
                    ClientVarManager.SetAsRawByteString(Temp, OutString);
                  end;
                else
                  ClientVarManager.SetAsUnicodeString(Temp, PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP));
              end;
          end;
        tdsBinary, tdsVarBinary, tdsBigBinary, tdsBigVarBinary:
          begin
            DatLen := FPLainDriver.dbRetLen(FHandle, ParamIndex);
            OutBytes := BufferToBytes(FPLainDriver.dbRetData(FHandle, ParamIndex), DatLen);
            SoftVarManager.SetAsBytes(Temp, OutBytes);
          end;
        tdsInt1:
          SoftVarManager.SetAsInteger(Temp,
            PByte(FPlainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsInt2:
          SoftVarManager.SetAsInteger(Temp,
            PSmallInt(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsInt4:
          SoftVarManager.SetAsInteger(Temp,
            PInteger(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsInt8:
          SoftVarManager.SetAsInteger(Temp,
            PInt64(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsFlt4:
          SoftVarManager.SetAsDouble(Temp,
            PSingle(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsFlt8:
          SoftVarManager.SetAsDouble(Temp,
            PDouble(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsNumeric,
        tdsDecimal,
        tdsMoney,
        tdsMoney4:
          begin
            FPlainDriver.dbConvert(FHandle, RetType,
              FPlainDriver.dbRetData(FHandle, ParamIndex),
                FPLainDriver.dbRetLen(FHandle, ParamIndex), Ord(tdsFlt8),
              @OutDouble, 8);
            SoftVarManager.SetAsDouble(Temp, OutDouble);
          end;
        tdsDateTime4, tdsDateTimeN:
          begin
            FPLainDriver.dbConvert(FHandle, RetType,
              FPLainDriver.dbRetData(FHandle, ParamIndex), 4,
              RetType, @OutDBDATETIME, 8);
            SoftVarManager.SetAsDateTime(Temp,
              OutDBDATETIME.dtdays + 2 + (OutDBDATETIME.dttime / 25920000));
          end;
        tdsDateTime:
          begin
            OutDBDATETIME := PDBDATETIME(
              FPLainDriver.dbRetData(FHandle, ParamIndex))^;
            SoftVarManager.SetAsDateTime(Temp,
              OutDBDATETIME.dtdays + 2 + (OutDBDATETIME.dttime / 25920000));
          end;
        tdsImage:
          Temp := EncodeInterface(TZAbstractBlob.CreateWithData(
            FPlainDriver.dbRetData(FHandle, ParamIndex),
            FPLainDriver.dbRetLen(FHandle, ParamIndex)));
        tdsText:
          Temp := EncodeInterface(TZAbstractClob.CreateWithData(
            FPlainDriver.dbRetData(FHandle, ParamIndex),
            FPLainDriver.dbRetLen(FHandle, ParamIndex),
            ConSettings^.ClientCodePage^.CP, ConSettings));
        tdsNText:
          Temp := EncodeInterface(TZAbstractClob.CreateWithData(
            FPlainDriver.dbRetData(FHandle, ParamIndex),
            FPLainDriver.dbRetLen(FHandle, ParamIndex),
            zCP_UTF8, ConSettings));
        tdsBit:
          Temp := EncodeBoolean(PBoolean(FPlainDriver.dbRetData(FHandle, ParamIndex))^);
        else
          {
          tdsFltN,
          tdsFltN,
          tdsMoneyN:
          tdsUnique:
          tdsIntN:
          tdsVariant:
          tdsBitN:
          tdsUDT:
          tdsMSXML:}
          Temp := NullVariant;

      end;
    OutParamValues[I] := Temp;
    Inc(ParamIndex);
  end;

//Workaround for sybase. the dbCount does not work, so a select @@rowcount is
//made but this cleared the returned output parameters, so this is moved here
//after reading the output parameters
  //if Self.FDBLibConnection.GetProvider = dpSybase then
    //FetchRowCount;

  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'EXEC '+ ASQL);
end;

procedure TZDBLibCallableStatement.SetInParamCount(NewParamCount: Integer);
begin
  inherited SetInParamCount(NewParamCount);

  if OutParamCount < NewParamCount then
    SetOutParamCount(NewParamCount);
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
    stBoolean:      if PAnsiChar(Bind.Value)^ = AnsiChar('0')
                    then Result := '(FALSE)'
                    else Result := '(TRUE)';
    stAsciiStream:  Result := '(CLOB)';
    stBinaryStream: Result := '(BLOB)';
    else            Result := RawByteString(Bind.Value);
  end;
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
        FRawTemp := SQLQuotedStr(RefCntLob.GetPAnsiChar(CP), refCntLob.Length, #39)
      end else begin
        FRawTemp := GetValidatedAnsiStringFromBuffer(Value.GetBuffer, Value.Length, ConSettings, CP);
        FRawTemp := SQLQuotedStr(FRawTemp, #39);
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
    FRawTemp := SQLQuotedStr(PAnsiChar(Value.P), Value.Len, #39);
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
    FRawTemp := SQLQuotedStr(fRawTemp, #39);
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
  FRawTemp := SQLQuotedStr(Value, #39);
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
  FRawTemp := SQLQuotedStr(FRawTemp, #39); //localize -> no hidden LStrClear in call
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
    FRawTemp := SQLQuotedStr(P, L, #39); //localize -> no hidden LStrClear in call
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

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

end.
