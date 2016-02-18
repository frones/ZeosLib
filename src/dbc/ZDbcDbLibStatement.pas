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

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZCompatibility, ZClasses, ZSysUtils, ZCollections, ZDbcIntfs, ZDbcStatement,
  ZDbcDbLib, ZPlainDbLibConstants, ZPlainDbLibDriver;

type
  {** Implements Generic DBLib Statement. }
  TZDBLibStatement = class(TZAbstractStatement)
  protected
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    FRetrievedResultSet: IZResultSet;
    FRetrievedUpdateCount: Integer;
    FUserEncoding: TZCharEncoding;

    procedure InternalExecuteStatement(SQL: RawByteString);
    procedure FetchResults; virtual;

  public
    constructor Create(Connection: IZConnection; Info: TStrings);
    procedure Close; override;

    function GetMoreResults: Boolean; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. With emulation}
  TZDBLibPreparedStatementEmulated = class(TZEmulatedPreparedStatement)
  private
    FPlainDriver: IZDBLibPlainDriver;
  protected
    function GetEscapeString(Value: string): string;
    function PrepareAnsiSQLQuery: RawByteString; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer;
      const NChar: Boolean): RawByteString; reintroduce;
    function CreateExecStatement: IZStatement; override;
  public
    constructor Create(Connection: IZConnection; SQL: string; Info: TStrings);
    function GetMetaData: IZResultSetMetaData; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  TZDBLibCallableStatement = class(TZAbstractCallableStatement)
  private
    FSQL: string;
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FHandle: PDBPROCESS;
    FLastRowsAffected: Integer;//Workaround for sybase
    FRetrievedResultSet: IZResultSet;
    FRetrievedUpdateCount: Integer;
    FUserEncoding: TZCharEncoding;

    procedure FetchResults; virtual;
    procedure FetchRowCount; virtual;

  protected
    procedure SetInParamCount(const NewParamCount: Integer); override;
  public
    constructor Create(Connection: IZConnection; ProcName: string; Info: TStrings);
    procedure Close; override;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SqlType: Integer); override;
    function GetMoreResults: Boolean; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

  end;

type
  {** Interface for storing counter. }
  IZUpdateCount = interface(IZInterface)
    ['{03219BB4-E07F-4A50-80CD-291FEA629697}']
    procedure SetCount(Value: Integer);
    function GetCount: Integer;
  end;

  TZUpdateCount = class(TInterfacedObject, IZUpdateCount)
  private
    FCount: Integer;
  public
    constructor Create(ACount: Integer);
    procedure SetCount(Value: Integer); virtual;
    function GetCount: Integer; virtual;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

uses
  Types, Math,
  ZDbcLogging, ZDbcCachedResultSet, ZDbcDbLibUtils, ZDbcDbLibResultSet,
  ZVariant, ZDbcUtils, ZEncoding, ZDbcResultSet
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

constructor TZUpdateCount.Create(ACount: Integer);
begin
  inherited Create;
  FCount := ACount;
end;

procedure TZUpdateCount.SetCount(Value: Integer);
begin
  FCount := Value;
end;

function TZUpdateCount.GetCount: Integer;
begin
  Result := FCount;
end;

{ TZDBLibStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
}
constructor TZDBLibStatement.Create(Connection: IZConnection; Info: TStrings);
begin
  inherited Create(Connection, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  if Assigned(FDBLibConnection) then
    FPLainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  FResults := TZCollection.Create;
  {note: this is a hack! Purpose is to notify Zeos all Charakter columns are
    UTF8-encoded. e.g. N(VAR)CHAR. Initial idea is made for MSSQL where we've NO
    valid tdsType to determine (Var)Char(Ansi-Encoding) or N(Var)Char encoding
    So this is stopping all encoding detections and increases the performance in
    a high rate. If Varchar fields are fetched you Should use a cast to N-Fields!
    Else all results are invalid!!!!! Just to invoke later questions!}
  if DefineStatementParameter(Self, 'ResetCodePage', '') = 'UTF8' then
    FUserEncoding := ceUTF8
  else
    Self.FUserEncoding := ceDefault;
end;

procedure TZDBLibStatement.Close;
var
  I: Integer;
  RS: IZResultSet;
begin
  for i := 0 to FResults.Count -1 do
    if supports(FResults[i], IZResultSet, RS) then    //possible IZUpdateCount
      RS.Close;
  FResults.Clear;
  FRetrievedResultSet := nil;
  inherited Close;
end;

{**
  Executes a Statement.
  Used internally to execute statements.

  @param Handle a DBLib connection handle.
  @sql string containing the statements to execute
}
procedure TZDBLibStatement.InternalExecuteStatement(SQL: RawByteString);
var Ansi: RawByteString;
begin
  if FDBLibConnection.GetProvider = dpMsSQL then
    //This one is to avoid a bug in dblib interface as it drops a single backslash before line end
    Ansi := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}StringReplace(SQL, '\'#13, '\\'#13, [rfReplaceAll])
  else
    //This one is to avoid sybase error: Invalid operator for datatype op: is null type: VOID TYPE
    Ansi := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}StringReplace(SQL, ' AND NULL IS NULL', '', [rfReplaceAll]);

  FHandle := FDBLibConnection.GetConnectionHandle;
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  if FPlainDriver.dbcancel(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbcmd(FHandle, Pointer(Ansi)) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
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
function TZDBLibStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZUpdateCount;
begin
  Result := False;
  FRetrievedResultSet := nil;
  FRetrievedUpdateCount := -1;
  if FResults.Count > 0 then
  begin
    try
      Result := FResults.Items[0].QueryInterface(IZResultSet, ResultSet) = 0;
      if Result then
      begin
        FRetrievedResultSet := ResultSet;
        FRetrievedUpdateCount := 0;
      end
      else
      begin
        if FResults.Items[0].QueryInterface(IZUpdateCount, UpdateCount) = 0 then
          FRetrievedUpdateCount := UpdateCount.GetCount;
      end;
      FResults.Delete(0);
    finally
      ResultSet := nil;
      UpdateCount := nil;
    end;
  end;
end;

{**
  Fetches all results and creates a cachedresultset object for each resultset
  and a ZUpdateCount object for each count value.
}
procedure TZDBLibStatement.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
  RS: IZResultSet;
  RowsAffected: Integer;
begin
  for RowsAffected := 0 to FResults.Count -1 do
    if Supports(FResults[RowsAffected], IZResultSet, RS) then
      RS.Close;
  FResults.Clear;
//Sybase does not seem to return dbCount at all, so a workaround is made
  RowsAffected := -2;
  while FPlainDriver.dbresults(FHandle) = DBSUCCEED do
  begin
    if FPlainDriver.dbcmdrow(FHandle) = DBSUCCEED then
    begin
      {EH: Developer notes:
       the TDS protocol does NOT support any stmt handles. All actions are
       executed sequentially so in ALL cases we need cached Results NO WAY araound!!!}
      NativeResultSet := TZDBLibResultSet.Create(Self, Self.SQL, FUserEncoding);
      NativeResultSet.SetConcurrency(rcReadOnly);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet,
        Self.SQL, TZDBLibCachedResolver.Create(Self, NativeResultSet.GetMetaData), ConSettings);
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last;
      CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResults.Add(CachedResultSet);
    end
    else
    begin
      RowsAffected := FPlainDriver.dbCount(FHandle);
      if RowsAffected > -1 then
        FResults.Add(TZUpdateCount.Create(RowsAffected));
    end;
    FPlainDriver.dbCanQuery(FHandle);
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');

  if not FDBLibConnection.FreeTDS then
    if RowsAffected = -1 then
    begin
      FDBLibConnection.InternalExecuteStatement('select @@rowcount');
      try
        FPlainDriver.dbresults(FHandle);
        NativeResultSet := TZDBLibResultSet.Create(Self, 'select @@rowcount');
        try
          if NativeResultset.Next then
            RowsAffected := NativeResultSet.GetInt(FirstDbcIndex);
        finally
          NativeResultSet.Close;
        end;
        FResults.Add(TZUpdateCount.Create(RowsAffected));
      finally
        FPlainDriver.dbCancel(FHandle);
      end;
      FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
    end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZDBLibStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  Result := nil;
  if ASQL <> SQL then
    ASQL := SQL;
  try
    InternalExecuteStatement(ASQL);
    FetchResults;
    repeat
      if GetMoreResults then
        Result := FRetrievedResultSet
      else if FRetrievedUpdateCount = -1 then
        Break;
    until False;
  finally
    FRetrievedResultSet := nil;
  end;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZDBLibStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  InternalExecuteStatement(ASQL);
  FetchResults;
  GetMoreResults;
  Result := FRetrievedUpdateCount;
  FRetrievedResultSet := nil;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
}
function TZDBLibStatement.Execute(const SQL: RawByteString): Boolean;
begin
  if ASQL <> SQL then
    ASQL := SQL;
  InternalExecuteStatement(ASQL);
  FetchResults;
  Result := GetMoreResults;
  LastResultSet := FRetrievedResultSet;
  LastUpdateCount := FRetrievedUpdateCount;
  FRetrievedResultSet := nil;
end;

{ TZDBLibPreparedStatementEmulated }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZDBLibPreparedStatementEmulated.Create(Connection: IZConnection;
  SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := (Connection as IZDBLibConnection).GetPlainDriver;
  ResultSetType := rtScrollInsensitive;
  FNeedNCharDetection := True;
end;

{**
  Converts an string into escape DBLib format.
  @param Value a regular string.
  @return a string in DBLib escape format.
}
function TZDBLibPreparedStatementEmulated.GetEscapeString(Value: string): string;
begin
  Result := AnsiQuotedStr(Value, '''');
end;

function TZDBLibPreparedStatementEmulated.PrepareAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  TokenizeSQLQueryRaw;

  for I := 0 to High(CachedQueryRaw) do
  begin
    if IsParamIndex[i] then
    begin
      Result := Result + PrepareAnsiSQLParam(ParamIndex, IsNCharIndex[i]);
      Inc(ParamIndex);
    end
    else
      Result := Result + CachedQueryRaw[I];
  end;
  {$IFNDEF UNICODE}
  if GetConnection.AutoEncodeStrings then
     Result := GetConnection.GetDriver.GetTokenizer.GetEscapeString(Result);
  {$ENDIF}
end;
{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZDBLibPreparedStatementEmulated.PrepareAnsiSQLParam(ParamIndex: Integer;
  const NChar: Boolean): RawByteString;
begin
  if InParamCount <= ParamIndex then
    Result := 'NULL'
  else
  begin
    Result := PrepareSQLParameter(InParamValues[ParamIndex],
      InParamTypes[ParamIndex], ClientVarManager, ConSettings, NChar);
  end;
end;

{**
  Gets the number, types and properties of a <code>ResultSet</code>
  object's columns.
  @return the description of a <code>ResultSet</code> object's columns
}
function TZDBLibPreparedStatementEmulated.GetMetaData: IZResultSetMetaData;
begin
  Result := nil;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZDBLibPreparedStatementEmulated.ExecutePrepared: Boolean;
begin
  Result := inherited Execute(PrepareAnsiSQLQuery);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZDBLibPreparedStatementEmulated.ExecuteQueryPrepared: IZResultSet;
begin
  Result := inherited ExecuteQuery(PrepareAnsiSQLQuery);
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
function TZDBLibPreparedStatementEmulated.ExecuteUpdatePrepared: Integer;
begin
  Result := inherited ExecuteUpdate(PrepareAnsiSQLQuery);
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZDBLibPreparedStatementEmulated.CreateExecStatement: IZStatement;
begin
  Result := TZDBLibStatement.Create(Connection, Info);
end;

constructor TZDBLibCallableStatement.Create(Connection: IZConnection;
  ProcName: string; Info: TStrings);
begin
  inherited Create(Connection, ProcName, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  if Assigned(FDBLibConnection) then
    FPLainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  {note: this is a hack! Purpose is to notify Zeos all Charakter columns are
    UTF8-encoded. e.g. N(VAR)CHAR. Initial idea is made for MSSQL where we've NO
    valid tdsType to determine (Var)Char(Ansi-Encoding) or N(Var)Char encoding
    So this is stopping all encoding detections and increases the performance in
    a high rate. If Varchar fields are fetched you Should use a cast to N-Fields!
    Else all results are invalid!!!!! Just to invoke later questions!}
  if DefineStatementParameter(Self, 'ResetCodePage', '') = 'UTF8' then
    FUserEncoding := ceUTF8
  else
    Self.FUserEncoding := ceDefault;
end;

procedure TZDBLibCallableStatement.Close;
begin
  FRetrievedResultSet := nil;
  inherited Close;
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
        FResultSets.Add(TZUpdateCount.Create(FLastRowsAffected));
    end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
end;

procedure TZDBLibCallableStatement.FetchRowCount;
var
  NativeResultSet: TZDBLibResultSet;
begin
//Sybase does not seem to return dbCount at all, so a workaround is made
  if FLastRowsAffected = -1 then
  begin
    FDBLibConnection.InternalExecuteStatement('select @@rowcount');
    try
      FPlainDriver.dbresults(FHandle);
      NativeResultSet := TZDBLibResultSet.Create(Self, 'select @@rowcount');
      try
        if NativeResultset.Next then
          FLastRowsAffected := NativeResultSet.GetInt(1);
      finally
        NativeResultset.Close;
      end;
      FResultSets.Add(TZUpdateCount.Create(FLastRowsAffected));
    finally
      FPlainDriver.dbCancel(FHandle);
    end;
    FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
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
function TZDBLibCallableStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZUpdateCount;
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
        if Supports(FResultSets[0], IZUpdateCount, UpdateCount) then
          FRetrievedUpdateCount := UpdateCount.GetCount;
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
            Params[I].AsFloat := SoftVarManager.GetAsFloat(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsFlt4), -1, -1, @(Params[I].AsFloat));
          end;
        stLong, stULong, stDouble, stBigDecimal, stCurrency:
          begin
            Params[I].AsDouble := SoftVarManager.GetAsFloat(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsFlt8), -1, -1, @(Params[I].AsDouble));
          end;
        stString, stUnicodeString:
          if IsNCharIndex[i] then
          begin
            Params[I].CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], zCP_UTF8);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsChar),
              -1, Max(1, Params[I].CharRec.Len), Params[I].CharRec.P);
          end
          else
          begin
            Params[I].CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], ConSettings^.ClientCodePage^.CP);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsChar),
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

  if FPLainDriver.dbHasRetStat(FHandle) then
    Temp := EncodeInteger(FPlainDriver.dbRetStatus(FHandle))
  else
    Temp := NullVariant;
  if Length(OutParamValues) = 0 then // check if DynArray is initialized for RETURN_VALUE
    SetOutParamCount(1);
  OutParamValues[0] := Temp; //set function RETURN_VALUE

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
            SetLength(OutBytes, DatLen);
            Move(FPLainDriver.dbRetData(FHandle, ParamIndex)^,
              Pointer(OutBytes)^, DatLen);
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
            PLongInt(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsInt8:
          SoftVarManager.SetAsInteger(Temp,
            PInt64(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsFlt4:
          SoftVarManager.SetAsFloat(Temp,
            PSingle(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsFlt8:
          SoftVarManager.SetAsFloat(Temp,
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
            SoftVarManager.SetAsFloat(Temp, OutDouble);
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
  FetchRowCount;

  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'EXEC '+ ASQL);
end;

procedure TZDBLibCallableStatement.SetInParamCount(const NewParamCount: Integer);
begin
  inherited SetInParamCount(NewParamCount);

  if OutParamCount < NewParamCount then
    SetOutParamCount(NewParamCount);
end;

end.


