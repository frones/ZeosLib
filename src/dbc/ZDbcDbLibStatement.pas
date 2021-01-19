{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          DBLib Statement common functionality           }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
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
  ZDbcDbLib, ZPlainDbLibDriver;

type
  /// <author>EgonHugeist</author>
  /// <summary>Defines a reference of the TZDBLibBindValue record</summary>
  PZDBLibBindValue = ^TZDBLibBindValue;
  /// <author>EgonHugeist</author>
  /// <summary>Defines a BindValue record which widened the TZBindValue by a
  ///  question mark position indicator</summary>
  TZDBLibBindValue = record
    /// <summary>the TZQMarkPosBindValue record</summary>
    BindValue:  TZQMarkPosBindValue;
    /// <summary>Is the parameter tagged as NationalChar?</summary>
    IsNCharIndex: Boolean;
    ParamName: RawByteString;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a DBLIB Bindlist object</summary>
  TZDBLibBindList = class(TZQuestionMarkBindList)
  protected
    class function GetElementSize: Integer; override;
    /// <summary>Notify about an action which will or was performed.
    ///  if ElementNeedsFinalize is False the method will never be called.
    ///  Otherwise you may finalize managed types beeing part of each element,
    ///  such as Strings, Objects etc.</summary>
    /// <param>"Ptr" the address of the element an action happens for.</param>
    /// <param>"Index" the index of the element.</param>
    /// <returns>The address or raises an EListError if the Index is invalid.</returns>
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  {** Implements Prepared SQL Statement for DBLib. With emulation}
  TZAbstractDBLibStatement = class(TZRawParamDetectPreparedStatement)
  private
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: TZDBLIBPLainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    FByteBuffer: PByteBuffer;
    procedure CreateOutParamResultSet; virtual;
    procedure InternalExecute; virtual; abstract;
  protected
    procedure FetchResults;
    procedure FlushPendingResults;
    class function GetBindListClass: TZBindListClass; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);
    procedure Prepare; override;
    procedure Unprepare; override;
    function GetMoreResults: Boolean; override;

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
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZSQLStringWriter; Var Result: SQLString); override;
  public
    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
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
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: UnicodeString); reintroduce;
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); reintroduce; overload;
    procedure SetGuid(ParameterIndex: Integer; const Value: TGUID); reintroduce;
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); reintroduce;
    procedure SetDate(ParameterIndex: Integer; const Value: TZDate); reintroduce; overload;
    procedure SetTime(ParameterIndex: Integer; const Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp); reintroduce; overload;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  end;

  TZDBLIBPreparedRPCStatement = class(TZAbstractDBLibStatement, IZPreparedStatement)
  private
    procedure InternalExecute; override;
    procedure CreateOutParamResultSet; override;
  protected
    procedure BindInParameters; override;
  public
    constructor Create(const Connection: IZConnection;
      const RemoteProcedureName: String; const Info: TStrings);
  public
    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
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
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: UnicodeString); reintroduce;
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); reintroduce; overload;
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    procedure SetGuid(ParameterIndex: Integer; const Value: TGUID); reintroduce;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); reintroduce;
    procedure SetDate(ParameterIndex: Integer; const Value: TZDate); reintroduce; overload;
    procedure SetTime(ParameterIndex: Integer; const Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp); reintroduce; overload;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  public
    function ExecuteQuery(const {%H-}SQL: UnicodeString): IZResultSet; override;
    function ExecuteUpdate(const {%H-}SQL: UnicodeString): Integer; override;
    function Execute(const {%H-}SQL: UnicodeString): Boolean; override;

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
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
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
https://docs.microsoft.com/en-us/sql/relational-databases/system-stored-procedures/sp-describe-first-result-set-transact-sql?view=sql-server-2017
*)

uses
  Math,
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs,
  {$ELSE}{$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  {$ENDIF}{$IFDEF WITH_UNITANSISTRINGS} AnsiStrings, {$ENDIF}
  ZDbcLogging, ZDbcCachedResultSet, ZDbcDbLibUtils, ZDbcDbLibResultSet,
  ZVariant, ZDbcUtils, ZEncoding, ZDbcResultSet,
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
  inherited Create(Connection, SQL, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FByteBuffer := FDBLibConnection.GetByteBufferAddress;
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  FResults := TZCollection.Create;
end;

procedure TZAbstractDBLibStatement.CreateOutParamResultSet;
var I: Integer;
begin
  for I := FResults.Count -1 downto 0 do
    if Supports(FResults[I], IZResultSet, FOutParamResultSet) then
      Break;
end;

class function TZAbstractDBLibStatement.GetBindListClass: TZBindListClass;
begin
  Result := TZDBLibBindList;
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
begin
  Result := FResults.Count > 0;
  if Result then begin
    if FResults.Items[0].QueryInterface(IZResultSet, ResultSet) = S_OK then begin
      LastResultSet := ResultSet;
      FOpenResultSet := Pointer(FLastResultSet);
    end else begin
      LastResultSet := nil;
      FOpenResultSet := nil;
      if FResults.Items[0].QueryInterface(IZAnyValue, UpdateCount) = S_OK then
        LastUpdateCount := UpdateCount.GetInteger;
    end;
    FResults.Delete(0);
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
  Result := GetMoreResults and (FLastResultSet <> nil);
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
  while GetMoreResults and (FlastResultSet = nil) do ;
  Result := GetResultSet;
  FlastResultSet := nil;
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
  while GetMoreResults and (FlastResultSet <> nil) do ;
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
  ResultsRETCODE, cmdRowRETCODE: RETCODE;
begin
  repeat
    ResultsRETCODE := FPlainDriver.dbresults(FHandle);
    if ResultsRETCODE = DBFAIL then
      FDBLibConnection.CheckDBLibError(lcFetch, 'FETCHRESULTS/dbresults', IImmediatelyReleasable(FWeakImmediatRelPtr));
    cmdRowRETCODE := FPlainDriver.dbcmdrow(FHandle);
    //EH: if NO_MORE_RESULTS there might be a final update count see TestSF380(a/b)
    if (cmdRowRETCODE = DBSUCCEED) and (ResultsRETCODE <> NO_MORE_RESULTS) then begin
      {EH: Developer notes:
       the TDS protocol does NOT support any stmt handles. All actions are
       executed sequentially so in ALL cases we need cached Results NO WAY around!!!}
      NativeResultSet := TZDBLibResultSet.Create(Self, Self.SQL);
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
  until ResultsRETCODE = NO_MORE_RESULTS;
  if BindList.HasOutOrInOutOrResultParam then
    CreateOutParamResultSet;
end;

procedure TZAbstractDBLibStatement.FlushPendingResults;
var I: Integer;
begin
  if FLastResultSet <> nil then
    FLastResultSet.Close;
  for I := 0 to FResults.Count -1 do
    if Supports(FResults[I], IZResultSet, FLastResultSet) then
      FLastResultSet.Close;
  FLastResultSet := nil;
  FResults.Clear;
end;

{ TZAbstracDBLibSQLStatement }

procedure TZAbstracDBLibSQLStatement.InternalExecute;
var Raw: RawByteString;
begin
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  LastUpdateCount := -1;
  RestartTimer;
  Raw := GetRawSQL;
  if FDBLibConnection.GetProvider = dpMsSQL then
    //This one is to avoid a bug in dblib interface as it drops a single backslash before line end
    Raw := StringReplaceAll_CS_GToEQ(Raw, RawByteString('\'#13), RawByteString('\\'#13))
  else
    //This one is to avoid sybase error: Invalid operator for datatype op: is null type: VOID TYPE
    Raw := StringReplaceAll_CS_LToEQ(Raw, RawByteString(' AND NULL IS NULL'), EmptyRaw);

  FHandle := FDBLibConnection.GetConnectionHandle;
  //2018-09-16 Commented by marsupilami79 because this hides errors in the logic
  //result sets might get only partial data without an error
  //if FPlainDriver.dbcancel(FHandle) <> DBSUCCEED then
  //  FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbcmd(FHandle, Pointer(Raw)) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL, IImmediatelyReleasable(FWeakImmediatRelPtr));

  if FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecute, Self);
end;

{ TZDBLibPreparedStatementEmulated }

function TZDBLibPreparedStatementEmulated.GetRawSQL: RawByteString;
var
  I, LastPos, L: Cardinal;
  SQLWriter: TZRawSQLStringWriter;
  P: PAnsiChar;
  BindValue: PZBindValue;
  DBLibBindValue: PZDBLibBindValue absolute BindValue;
  QMarkBindValue: PZQMarkPosBindValue absolute BindValue;
begin
  if BindList.Count = 0
  then Result := FASQL
  else begin
    Result := EmptyRaw;
    P := Pointer(FASQL);
    L := Length(FASQL);
    LastPos := 0;
    I := L + Cardinal(BindList.Count) shl 5; //add 32 bytes/param by default
    SQLWriter := TZRawSQLStringWriter.Create(I);
    try
      for I := 0 to BindList.Count -1 do begin
        BindValue := BindList[I];
        SQLWriter.AddText(P+LastPos, QMarkBindValue.QMarkPosition - LastPos, Result);
        if BindValue.BindType = zbtNull
        then SQLWriter.AddText('null', Result)
        else begin
          if (BindValue.SQLType in [stString, stAsciiStream]) and (BindValue.BindType = zbtUTF8String) and not DBLibBindValue.IsNCharIndex then
            SQLWriter.AddChar(AnsiChar('N'), Result);
          SQLWriter.AddText(RawByteString(BindValue.Value), Result);
        end;
        LastPos := QMarkBindValue.QMarkPosition + 1;
      end;
      SQLWriter.AddText(P+LastPos, L - LastPos, Result);
      SQLWriter.Finalize(Result);
    finally
      FreeAndNil(SQLWriter);
    end;
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
  if (FClientCP = zOSCodePage) or not PZDBLibBindValue(TZDBLibBindList(BindList).Items[ParameterIndex]).IsNCharIndex then begin
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
  P: PAnsiChar;
  Len: NativeUInt;
  CP: Word;
  R: RawByteString;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  InParamIdx := Index;
  CheckParameterIndex(InParamIdx);
  RefCntLob := Value; //inc RefCount
  R := EmptyRaw;
  if (RefCntLob <> nil) and not RefCntLob.IsEmpty then
    if (SQLType in [stAsciiStream, stUnicodeStream]) then begin
      if (FClientCP = zCP_UTF8) or PZDBLibBindValue(TZDBLibBindList(BindList).Items[Index]).IsNCharIndex
      then CP := zCP_UTF8
      else CP := FClientCP;
      if Value.IsClob then begin
        P := RefCntLob.GetPAnsiChar(CP, R, Len);
        FRawTemp := SQLQuotedStr(P, Len, AnsiChar(#39))
      end else raise CreateConversionError(Index, stBinaryStream, stAsciiStream);
      BindList.Put(Index, stAsciiStream, FRawTemp, CP);
    end else begin
      P := RefCntLob.GetBuffer(R, Len);
      FRawTemp := GetSQLHexAnsiString(P, Len, True);
      BindList.Put(Index, stBinaryStream, FRawTemp, FClientCP)
    end
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
procedure TZDBLibPreparedStatementEmulated.SetBytes(ParameterIndex: Integer;
  Value: PByte; Len: NativeUInt);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stBytes, GetSQLHexAnsiString(PAnsiChar(Value), Len, True), FClientCP);
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
  if (Value.CP = FClientCP) or ((Value.CP = zCP_UTF8) and PZDBLibBindValue(TZDBLibBindList(BindList).Items[ParameterIndex]).IsNCharIndex) then begin
    FRawTemp := SQLQuotedStr(PAnsiChar(Value.P), Value.Len, AnsiChar(#39));
    BindList.Put(ParameterIndex, stString, FRawTemp, Value.CP);
  end else begin
    if PZDBLibBindValue(TZDBLibBindList(BindList).Items[ParameterIndex]).IsNCharIndex or (FClientCP = zCP_UTF8)
    then CP := zCP_UTF8
    else CP := FClientCP;
    if Value.CP = zCP_UTF16 then
      fRawTemp := PUnicodeToRaw(Value.P, Value.Len, CP)
    else begin
      fUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
      fRawTemp := ZUnicodeToRaw(fUniTemp, CP)
    end;
    FRawTemp := SQLQuotedStr(fRawTemp, AnsiChar(#39));
    BindList.Put(ParameterIndex, stString, FRawTemp, CP);
  end;
end;

procedure TZDBLibPreparedStatementEmulated.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stCurrency, CurrToRaw(Value, '.'), FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetDate(ParameterIndex: Integer;
  const Value: TZDate);
var Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Len := DateToRaw(Value.Year, Value.Month, Value.Day,
    PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateFormat, True, Value.IsNegative);
  ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
  BindList.Put(ParameterIndex, stDate, fRawTemp, FClientCP);
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

procedure TZDBLibPreparedStatementEmulated.AddParamLogValue(
  ParamIndex: Integer; SQLWriter: TZSQLStringWriter; var Result: SQLString);
var Bind: PZBindValue;
  {$IFDEF UNICODE}
  DBLibBindValue: PZDBLibBindValue absolute Bind;
  CP: Word;
  {$ENDIF}
begin
  Bind := BindList[ParamIndex];
  if Bind.BindType = zbtNull then
    SQLWriter.AddText('(NULL)', Result)
  else if Bind.BindType = zbtArray then
    SQLWriter.AddText('(ARRAY)', Result)
  else case Bind.SQLType of
    stBoolean:      if PByte(Bind.Value)^ = Ord('0')
                    then SQLWriter.AddText('(FALSE)', Result)
                    else SQLWriter.AddText('(TRUE)', Result);
    stAsciiStream:  SQLWriter.AddText('(CLOB)', Result);
    stBinaryStream: SQLWriter.AddText('(BLOB)', Result);
    {$IFDEF UNICODE}
    stString: begin
        if (FClientCP = zCP_UTF8) or DBLibBindValue.IsNCharIndex
        then CP := zCP_UTF8
        else CP := FClientCP;
        fUniTemp := ZRawToUnicode(RawByteString(Bind.Value), CP);
        SQLWriter.AddTextQuoted(fUniTemp, #39, Result);
        fUniTemp := '';
      end;
    else            SQLWriter.AddAscii7Text(Pointer(RawByteString(Bind.Value)), Length(RawByteString(Bind.Value)), Result);
    {$ELSE}
    else            SQLWriter.AddText(RawByteString(Bind.Value), Result);
    {$ENDIF}
  end;
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
  if (FClientCP = zCP_UTF8) or PZDBLibBindValue(TZDBLibBindList(BindList).Items[ParameterIndex]).IsNCharIndex
  then CP := zCP_UTF8
  else CP := FClientCP;
  P := Pointer(Value);
  L := Length(Value);
  FRawTemp := SQLQuotedStr(P, L, #39);
  BindList.Put(ParameterIndex, stString, FRawTemp, CP);
  {$ENDIF}
end;

procedure TZDBLibPreparedStatementEmulated.SetTime(ParameterIndex: Integer;
  const Value: TZTime);
var Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Len := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
    PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.TimeFormat, True, Value.IsNegative);
  ZSetString(PAnsiChar(FByteBuffer), Len ,fRawTemp);
  BindList.Put(ParameterIndex, stTime, fRawTemp, FClientCP);
end;

procedure TZDBLibPreparedStatementEmulated.SetTimestamp(ParameterIndex: Integer;
  const Value: TZTimeStamp);
var Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  Len := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
    Value.Hour, Value.Minute, Value.Second, Value.Fractions,
    PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateTimeFormat, True, Value.IsNegative);
  ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
  BindList.Put(ParameterIndex, stTimeStamp, fRawTemp, FClientCP);
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
  const Value: UnicodeString);
var CP: Word;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if (FClientCP = zCP_UTF8) or PZDBLibBindValue(TZDBLibBindList(BindList).Items[ParameterIndex]).IsNCharIndex
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
  if (FClientCP = zCP_UTF8) or PZDBLibBindValue(TZDBLibBindList(BindList).Items[ParameterIndex]).IsNCharIndex then begin
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
  DBLibBindValue: PZDBLibBindValue absolute Bind;
begin
  if FPLainDriver.dbRPCInit(FHandle, Pointer(fASQL), 0) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCInit', IImmediatelyReleasable(FWeakImmediatRelPtr));
  for i := 1 to BindList.Count -1 do begin //skip the returnparam
    Bind := BindList[I];
    case Bind.BindType of
      zbtNull: FPlainDriver.dbRpcParam(FHandle, Pointer(DBLibBindValue.ParamName), Ord(Bind.ParamType >= pctInOut),
        Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, 0, nil);
      zbtPointer: FPlainDriver.dbRpcParam(FHandle, Pointer(DBLibBindValue.ParamName), Ord(Bind.ParamType >= pctInOut),
        Ord(tdsBit), -1, 0, @Bind.Value); //stBoolean
      zbt4Byte: FPlainDriver.dbRpcParam(FHandle, Pointer(DBLibBindValue.ParamName), Ord(Bind.ParamType >= pctInOut),
        Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, -1, @Bind.Value);
      zbt8Byte: FPlainDriver.dbRpcParam(FHandle, Pointer(DBLibBindValue.ParamName), Ord(Bind.ParamType >= pctInOut),
        Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, -1, {$IFDEF CPU64}@{$ENDIF}Bind.Value);
      zbtBinByRef: FPlainDriver.dbRpcParam(FHandle, Pointer(DBLibBindValue.ParamName), Ord(Bind.ParamType >= pctInOut),
        Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, PZBufRec(Bind.Value).Len, PZBufRec(Bind.Value).Buf);
      zbtRawString, zbtUTF8String {$IFNDEF NEXTGEN}, zbtAnsiString{$ENDIF}:
        FPlainDriver.dbRpcParam(FHandle, Pointer(DBLibBindValue.ParamName), Ord(Bind.ParamType >= pctInOut),
          Ord(ConvertSqlTypeToTDSType(Bind.SQLType)), -1, Length(RawByteString(Bind.Value)), Bind.Value);
      {$IFDEF FPC}else ;{$ENDIF}
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
        ZSetString(PAnsiChar(Data), StrLen(Data), fRawTemp{$IFDEF WITH_RAWBYTESTRING}, fClientCP{$ENDIF});
        ColumnInfo.ColumnLabel := fRawTemp;
        {$ENDIF}
        RetType := FPLainDriver.dbRetType(FHandle, N);
        Data := FPlainDriver.dbRetData(FHandle, N);
        Len := FPLainDriver.dbRetLen(FHandle, N);
        ColumnInfo.Precision := Len;
        if (Data = nil) or (RetType = Ord(tdsVoid)) then
          BindList.SetNull(I, ConvertTDSTypeToSqlType(TTDSType(RetType), Len, 0))
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
                PByte(FByteBuffer), SizeOf(TByteBuffer));
              ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
              BindList.Put(I, stBigDecimal, fRawTemp, FClientCP);
            end;
          tdsMoney,
          tdsMoney4:
            begin
              Len := FPlainDriver.dbConvert(FHandle, RetType, Data, Len, Ord(tdsVarChar),
                PByte(FByteBuffer), SizeOf(TByteBuffer));
              ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
              BindList.Put(I, stCurrency, fRawTemp, FClientCP);
            end;
          tdsDateTime4, tdsDateTimeN:
            begin
              FPLainDriver.dbConvert(FHandle, RetType, Data, Len, RetType, @OutDBDATETIME, 8);
              PDouble(FByteBuffer)^ := OutDBDATETIME.dtdays + 2 + (OutDBDATETIME.dttime / 25920000);
              BindList.Put(I, stTimeStamp, P8Bytes(FByteBuffer));
            end;
          tdsDateTime:
            begin
              PDouble(FByteBuffer)^ := PDBDATETIME(Data).dtdays + 2 + (PDBDATETIME(Data).dttime / 25920000);
              BindList.Put(I, stTimeStamp, P8Bytes(FByteBuffer));
            end;
          tdsImage: BindList.Put(I, stBinaryStream, TZLocalMemBLob.CreateWithData(Data, Len));
          tdsText:  BindList.Put(I, stBinaryStream, TZLocalMemCLob.CreateWithData(Data, Len, FClientCP, ConSettings));
          tdsNText: BindList.Put(I, stBinaryStream, TZLocalMemCLob.CreateWithData(Data, Len, zCP_UTF8, ConSettings));
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
        ColumnInfo.ColumnType := BindValue.SQLType;
        ColumnsInfo.Add(ColumnInfo);
        Inc(N);
      end;
    end;
  finally
    RS := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '', ConSettings);
    ColumnsInfo.Free;
    RS.SetConcurrency(rcUpdatable);
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
          {$IFDEF FPC}else ;{$ENDIF} //weird FPC warning
        end;
        Inc(N);
      end;
    end;
    RS.InsertRow;
    RS.BeforeFirst;
    RS.SetConcurrency(rcReadonly);
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZDBLIBPreparedRPCStatement.Execute(const SQL: RawByteString): Boolean;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDBLIBPreparedRPCStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  BindInParameters;
  if FPLainDriver.dbRpcExec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, 'EXECUTEPREPARED:dbRPCExec', IImmediatelyReleasable(FWeakImmediatRelPtr));
  FetchResults;
  Result := (FResults.Count > 0) and Supports(FResults[0], IZResultSet, FLastResultSet);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZDBLIBPreparedRPCStatement.Execute(const SQL: UnicodeString): Boolean;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZDBLIBPreparedRPCStatement.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDBLIBPreparedRPCStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if not ExecutePrepared then
    while GetMoreResults and (FLastResultSet = nil) do ;
  Result := FLastResultSet;
  FLastResultSet := nil;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZDBLIBPreparedRPCStatement.ExecuteQuery(
  const SQL: UnicodeString): IZResultSet;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZDBLIBPreparedRPCStatement.ExecuteUpdate(
  const SQL: UnicodeString): Integer;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5033 off : Function result does not seem to be set} {$ENDIF}
function TZDBLIBPreparedRPCStatement.ExecuteUpdate(
  const SQL: RawByteString): Integer;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZDBLIBPreparedRPCStatement.ExecuteUpdatePrepared: Integer;
begin
  if ExecutePrepared then
    while GetMoreResults and (FLastResultSet <> nil) do ;
  Result := LastUpdateCount;
end;

procedure TZDBLIBPreparedRPCStatement.InternalExecute;
begin
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  RestartTimer;
  LastUpdateCount := -1;
  if FPLainDriver.dbRpcExec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL, IImmediatelyReleasable(FWeakImmediatRelPtr));
end;

procedure TZDBLIBPreparedRPCStatement.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
begin
  inherited;
  {$IFDEF UNICODE}
  PZDBLibBindValue(TZDBLibBindList(BindList)[ParameterIndex]).ParamName := ZUnicodeToRaw(Name, FClientCP);
  {$ELSE}
  PZDBLibBindValue(TZDBLibBindList(BindList)[ParameterIndex]).ParamName := Name;
  {$ENDIF}
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
procedure TZDBLIBPreparedRPCStatement.SetBytes(ParameterIndex: Integer;
  Value: PByte; Len: NativeUInt);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stBytes, Value, Len);
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
  BindList.Put(ParameterIndex, stString, CurrToRaw(Value, '.'), FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetDate(ParameterIndex: Integer;
  const Value: TZDate);
var Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Len := DateToRaw(Value.Year, Value.Month, Value.Day,
    PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateFormat, False, Value.IsNegative);
  ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
  BindList.Put(ParameterIndex, stDate, fRawTemp, FClientCP);
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
  SetRawByteString(ParameterIndex, Value)
  {$ENDIF}
end;

procedure TZDBLIBPreparedRPCStatement.SetTime(ParameterIndex: Integer;
  const Value: TZTime);
var Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Len := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions div NanoSecsPerMSec,
    PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.TimeFormat, False, Value.IsNegative);
  ZSetString(PAnsiChar(FByteBuffer), Len ,fRawTemp);
  BindList.Put(ParameterIndex, stString, fRawTemp, FClientCP);
end;

procedure TZDBLIBPreparedRPCStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TZTimeStamp);
var Len: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  Len := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
    Value.Hour, Value.Minute, Value.Second, Value.Fractions,
    PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateTimeFormat, False, Value.IsNegative);
  ZSetString(PAnsiChar(FByteBuffer), Len, fRawTemp);
  BindList.Put(ParameterIndex, stTimeStamp, fRawTemp, FClientCP);
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
  const Value: UnicodeString);
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
  const StoredProcName: String): TZAbstractPreparedStatement;
begin
  Result := TZDBLIBPreparedRPCStatement.Create(Connection, StoredProcName, Info);
end;

{ TZDBLibBindList }

class function TZDBLibBindList.GetElementSize: Integer;
begin
  Result := SizeOf(TZDBLibBindValue);
end;

procedure TZDBLibBindList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) then
    PZDBLibBindValue(Ptr).ParamName := '';
  inherited Notify(Ptr, Action);
end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
