{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6Statement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS) and not defined(FPC)}Windows, {$IFEND}//FPC does not inline the methods
  ZDbcIntfs, ZDbcStatement, ZDbcInterbase6, ZDbcInterbase6Utils,
  ZPlainFirebirdInterbaseDriver, ZCompatibility,
  ZDbcFirebirdInterbase,
  ZDbcLogging, ZVariant, ZMessages, ZDbcCachedResultSet, ZDbcUtils;

type
  /// <summary>Implements a abstract prepared SQL Statement for Interbase or
  ///  Firebird using the "legacy" api.</summary>
  TZAbstractInterbase6PreparedStatement = class(TZAbstractFirebirdInterbasePreparedStatement)
  private
    FResultXSQLDA: IZSQLDA; //the out param or resultset Interface
    FIBConnection: IZInterbase6Connection; //the IB/FB connection interface
    FParamSQLData: IZParamsSQLDA;//the in param Interface
    FParamXSQLDA: PXSQLDA;
    FPlainDriver: TZInterbasePlainDriver; //the api holder object of the provider
    FStatusVector: TARRAY_ISC_STATUS; //the errorcode vector
    FStmtHandle: TISC_STMT_HANDLE; //the smt handle
    FMaxRowsPerBatch: Integer;
    /// <summary>Internal execute the prepared statment.</summary>
    procedure ExecuteInternal;
  protected
    /// <summary>Prepares eventual structures for binding input parameters.</summary>
    procedure PrepareInParameters; override;
    /// <summary>Removes eventual structures for binding input parameters.</summary>
    procedure UnPrepareInParameters; override;
  protected
    /// <summary>Removes the current connection reference from this object.</summary>
    /// <remarks>This method will be called only if the object is garbage.</remarks>
    procedure ReleaseConnection; override;
    /// <summary>Creates an resultset if the stmt has been executed.</summary>
    /// <returns>Returns a new or the same object interface if nothing did
    ///  change on the object settings.</returns>
    function CreateResultSet: IZResultSet;
    /// <summary>Creates a temporary lob and sends the buffer to the server.</summary>
    /// <param>"Index" the index of the blobid field in our databuffer.</param>
    /// <param>"P" the buffer we send to the server.</param>
    /// <param>"Len" the length of the buffer we send to the server.</param>
    procedure WriteLobBuffer(Index: Cardinal; P: PAnsiChar; Len: NativeUInt); override;
    /// <summary>Creates a conversion error.</summary>
    /// <param>"Index" the index of the parameter.</param>
    /// <param>"Current" the the current SQLType which can't get converted.</param>
    /// <returns>The error object.</returns>
    function CreateConversionError(Index: Cardinal; Current: TZSQLType): EZSQLException; override;
  public
    /// <summary>Constructs this object and assignes the main properties.</summary>
    /// <param>"Connection" the IZInterbase6Connection interface which creates
    ///  this object.</param>
    /// <param>"SQL" the SQL used for this prepared statement</param>
    /// <param>"Info" a statement parameters list.</param>
    constructor Create(const Connection: IZInterbase6Connection; const SQL: string; Info: TStrings);
    /// <summary>Do tasks after the statement was closed. For example
    ///  dispose statement handles.</summary>
    procedure AfterClose; override;
    /// <summary>prepares the statement on the server, allocates all bindings
    ///  and handles</summary>
    procedure Prepare; override;
    /// <summary>unprepares the statement, deallocates all bindings and
    ///  handles</summary>
    procedure Unprepare; override;
    /// <summary>Executes the SQL query in this <c>PreparedStatement</c> object
    ///  and returns the result set generated by the query.
    /// <returns>a <c>IZResultSet</c> interface that contains the data produced
    ///  by the query; never <c>nil</c></returns>
    function ExecuteQueryPrepared: IZResultSet; override;
    /// <summary>Executes the SQL INSERT, UPDATE or DELETE statement in this
    ///  <c>PreparedStatement</c> object. In addition, SQL statements that
    ///  return nothing, such as SQL DDL statements, can be executed.
    /// <returns>either the row count for INSERT, UPDATE or DELETE statements;
    ///  or -1 for SQL statements that return nothing</returns>
    function ExecuteUpdatePrepared: Integer; override;
    /// <summary>Executes any kind of SQL statement. Some prepared statements
    ///  return multiple results; the <c>ExecutePrepared</c> method handles these
    ///  complex statements as well as the simpler form of statements handled
    ///  by the methods <c>ExecuteQuery</c> and <c>ExecuteUpdate</c>.
    ///  see IStatement.execute
    /// <returns>True if a ResultSet is available otherwise false.</returns>
    function ExecutePrepared: Boolean; override;
    /// <summary>Releases all driver handles and set the object in a closed
    ///  Zombi mode waiting for destruction. Each known supplementary object,
    ///  supporting this interface, gets called too. This may be a recursive
    ///  call from parant to childs or vice vera. So finally all resources
    ///  to the servers are released. This method is triggered by a connecton
    ///  loss. Don't use it by hand except you know what you are doing.</summary>
    /// <param>"Sender" the object that did notice the connection lost.</param>
    /// <param>"AError" a reference to an EZSQLConnectionLost error.
    ///  You may free and nil the error object so no Error is thrown by the
    ///  generating method. So we start from the premisse you have your own
    ///  error handling in any kind.</param>
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  end;

  /// <summary>Implements a prepared SQL Statement for Interbase or Firebird
  ///  using the "legacy" api.</summary>
  TZInterbase6PreparedStatement = class(TZAbstractInterbase6PreparedStatement,
    IZPreparedStatement);

  /// <summary>Implements a SQL Statement for Interbase or Firebird
  ///  using the "legacy" api.</summary>
  TZInterbase6Statement = class(TZAbstractInterbase6PreparedStatement, IZStatement)
  public
    constructor Create(const Connection: IZInterbase6Connection; Info: TStrings);
  end;

  /// <summary>Implements a Callable SQL Statement for Interbase or Firebird.</summary>
  TZInterbase6CallableStatement = class(TZAbstractInterbaseFirebirdCallableStatement)
  protected
    /// <summary>creates an exceution Statement</summary>
    /// <param>"Connection" the owner firebird/interbase-connection interface.</param>
    /// <param>"SQL" the SQL to be prepared and executed.</param>
    /// <param>"Params" a parameter list to setup behaviors.</param>
    /// <returns>a TZAbstractPreparedStatement object.</returns>
    function InternalCreateExecutionStatement(const Connection: IZInterbaseFirebirdConnection;
      const SQL: String; Params: TStrings): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZSysUtils, ZFastCode, ZEncoding, ZClasses, ZDbcInterbase6ResultSet,
  ZDbcResultSet, ZDbcProperties;

{ TZAbstractInterbase6PreparedStatement }

type
  TCountType = (cntSel, cntIns, cntDel, cntUpd);

procedure TZAbstractInterbase6PreparedStatement.ExecuteInternal;
var iError: ISC_STATUS;
  ISC_TR_HANDLE: PISC_TR_HANDLE;
  dialect: Word;
var
  ReqInfo: AnsiChar;
  pBuf, pBufStart: PAnsiChar;
  Len, Item, Count: Integer;
  Counts: array[TCountType] of Integer;
begin
  if BatchDMLArrayCount = 0 then begin
    if DriverManager.HasLoggingListener then
      DriverManager.LogMessage(lcBindPrepStmt,Self);
    RestartTimer;
    ISC_TR_HANDLE := FIBConnection.GetTrHandle;
    dialect := FIBConnection.GetDialect;
    if (FStatementType = stExecProc)
    then iError := FPlainDriver.isc_dsql_execute2(@FStatusVector, ISC_TR_HANDLE,
      @FStmtHandle, Dialect, FParamXSQLDA, FResultXSQLDA.GetData) //expecting out params
    else iError := FPlainDriver.isc_dsql_execute(@FStatusVector, ISC_TR_HANDLE,
      @FStmtHandle, Dialect, FParamXSQLDA); //not expecting a result
    if (iError <> 0) or (FStatusVector[2] = isc_arg_warning) then
      FIBConnection.HandleErrorOrWarning(lcExecPrepStmt, @FStatusVector, SQL, Self);
    LastUpdateCount := -1;
    if FStatementType <> stDDL then begin
      ReqInfo := AnsiChar(isc_info_sql_records);

      if FPlainDriver.isc_dsql_sql_info(@FStatusVector, @FStmtHandle, 1,
          @ReqInfo, SizeOf(TByteBuffer), PAnsiChar(FByteBuffer)) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_sql_info'{$ELSE}''{$ENDIF}, Self);
      if FByteBuffer[0] <> isc_info_sql_records then
        Exit;

      pBufStart := @FByteBuffer[1];
      pBuf := pBufStart;
      Len := FPlainDriver.isc_vax_integer(pBuf, 2) + 2;
      Inc(pBuf, 2);
      if FByteBuffer[Len] <> isc_info_end then
        Exit;

      FillChar(Counts{%H-}, SizeOf(Counts), #0);
      while pBuf - pBufStart <= Len do
      begin
        Item := Byte(pBuf^);

        if Item = isc_info_end then
          Break;

        Inc(pBuf);
        Count := ReadInterbase6NumberWithInc(FPlainDriver, pBuf);

        case Item of
          isc_info_req_select_count: Counts[cntSel] := Count;
          isc_info_req_insert_count: Counts[cntIns] := Count;
          isc_info_req_update_count: Counts[cntUpd] := Count;
          isc_info_req_delete_count: Counts[cntDel] := Count;
          else
            raise EZSQLException.Create(SInternalError);
        end;
      end;

      { Note: Update statements could have Select counter <> 0 as well }

      case FStatementType of
        stSelect, //selectable procedure could have a update count but FB does not return them.
        stSelectForUpdate:LastUpdateCount := Counts[cntSel];
        stInsert:         LastUpdateCount := Counts[cntIns];
        stUpdate:         LastUpdateCount := Counts[cntUpd];
        stDelete:         LastUpdateCount := Counts[cntDel];
        stExecProc: begin
                          { Exec proc could have any counter... So search for the first non-zero counter }
                          LastUpdateCount := Counts[cntIns];
                          if LastUpdateCount > 0 then Exit;
                          LastUpdateCount := Counts[cntUpd];
                          if LastUpdateCount > 0 then Exit;
                          LastUpdateCount := Counts[cntDel];
                          if LastUpdateCount > 0 then Exit;
                          LastUpdateCount := Counts[cntSel];
                        end;
        else            LastUpdateCount := -1;
      end;
    end;
  end else ExecuteBatchDml;
end;

procedure TZAbstractInterbase6PreparedStatement.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FIBConnection := nil;
end;

procedure TZAbstractInterbase6PreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FStmtHandle := 0;
  inherited ReleaseImmediat(Sender, AError);
end;

constructor TZAbstractInterbase6PreparedStatement.Create(const Connection: IZInterbase6Connection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FIBConnection := Connection;
  FPlainDriver := TZInterbasePlainDriver(FIBConnection.GetIZPlainDriver.GetInstance);
  ResultSetType := rtForwardOnly;
  FStmtHandle := 0;
  FMaxRowsPerBatch := 0;
end;

function TZAbstractInterbase6PreparedStatement.CreateConversionError(
  Index: Cardinal; Current: TZSQLType): EZSQLException;
var Expected: TZSQLType;
begin
  Expected := Self.FParamSQLData.GetFieldSqlType(Index);
  Result := ZDbcUtils.CreateConversionError(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
    Current, Expected)
end;

function TZAbstractInterbase6PreparedStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZInterbase6XSQLDAResultSet;
  CachedResolver: TZInterbaseFirebirdCachedResolver;
  CachedResultSet: TZCachedResultSet;
begin
  if FOpenResultSet <> nil then
    Result := IZResultSet(FOpenResultSet)
  else begin
    NativeResultSet := TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
      FResultXSQLDA, FOrgTypeList, FStatementType);
    if (GetResultSetConcurrency = rcUpdatable) or (GetResultSetType <> rtForwardOnly) then begin
      if FIBConnection.IsFirebirdLib and (FIBConnection.GetHostVersion >= 2000000) //is the SQL2003 st. IS DISTINCT FROM supported?
      then CachedResolver  := TZFirebird2upCachedResolver.Create(Self, NativeResultSet.GetMetadata)
      else CachedResolver  := TZInterbaseFirebirdCachedResolver.Create(Self, NativeResultSet.GetMetadata);
      if CachedLob
      then CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings)
      else CachedResultSet := TZInterbaseCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
    NativeResultSet.TransactionResultSet := Pointer(Result);
    FOpenResultSet := Pointer(Result);
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.AfterClose;
begin
  if (FStmtHandle <> 0) then begin// Free statement-handle! Otherwise: Exception!
    if FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_drop) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, 'isc_dsql_free_statement', FIBConnection);
    FStmtHandle := 0;
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.Prepare;
var
  eBlock: RawByteString;
  PreparedRowsOfArray: Integer;
  TypeItem: AnsiChar;
  Buffer: array[0..7] of AnsiChar;
  FinalChunkSize: Integer;
  L,MaxLen: LengthInt;
  Mem, Index: Cardinal;
  XSQLVAR: PXSQLVAR;
  XSQLDA: PXSQLDA;
  P: PAnsiChar;
  Status: ISC_STATUS;
  NewSQLType: ISC_SHORT;
label jmpEB;

  procedure PrepareArrayStmt(var Slot: TZIB_FBStmt);
  begin
    if (Slot.Obj = nil) or (Slot.PreparedRowsOfArray <> PreparedRowsOfArray) then begin
      if Slot.Obj <> nil then begin
        TZInterbase6PreparedStatement(Slot.Obj).BindList.Count := 0;
        {$IFNDEF AUTOREFCOUNT}
        TZInterbase6PreparedStatement(Slot.Obj)._Release;
        {$ENDIF}
        Slot.Obj := nil;
      end;
      Slot.Obj := TZInterbase6PreparedStatement.Create(FIBConnection, '', Info);
      {$IFNDEF AUTOREFCOUNT}
      TZInterbase6PreparedStatement(Slot.Obj)._AddRef;
      {$ENDIF}
      TZInterbase6PreparedStatement(Slot.Obj).FASQL := eBlock;
      TZInterbase6PreparedStatement(Slot.Obj).BindList.Count := BindList.Count*PreparedRowsOfArray;
      Slot.PreparedRowsOfArray := PreparedRowsOfArray;
      Slot.Obj.Prepare;
    end;
  end;
  procedure PrepareFinalChunk(Rows: Integer);
  begin
    eBlock := GetExecuteBlockString(Rows, FIBConnection.GetXSQLDAMaxSize,
      PreparedRowsOfArray, FMaxRowsPerBatch, FPlainDriver);
    PrepareArrayStmt(FBatchStmts[False]);
  end;
  procedure SplitQueryIntoPieces;
  begin
    FASQL := SplittQuery(SQL);
  end;
begin
  if (not Prepared) then begin
    with FIBConnection do begin
    { Allocate an sql statement }
    if FStmtHandle = 0 then
      if FPlainDriver.isc_dsql_allocate_statement(@FStatusVector, GetDBHandle, @FStmtHandle) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, 'isc_dsql_allocate_statement', Self);
      { Prepare an sql statement }
      //get overlong string running:
      //see request https://zeoslib.sourceforge.io/viewtopic.php?f=40&p=147689#p147689
      //http://tracker.firebirdsql.org/browse/CORE-1117?focusedCommentId=31493&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel#action_31493
      L := Length(ASQL);
      if L > High(Word) then begin//test word range overflow
        if ZFastCode.Pos(RawByteString(#0), ASQL) > 0 then
          raise EZSQLException.Create('Statements longer than 64KB may not contain the #0 character.');
        MaxLen := GetConnection.GetMetadata.GetDatabaseInfo.GetMaxStatementLength;
        if L > MaxLen then
          raise Exception.Create('Statements longer than ' + ZFastCode.IntToStr(MaxLen) + ' bytes are not supported by your database.');
        L := 0; //fall back to C-String behavior
      end;
      Status := FPlainDriver.isc_dsql_prepare(@FStatusVector, GetTrHandle, @FStmtHandle,
          Word(L), Pointer(ASQL), GetDialect, nil);
      if (Status <> 0) or (FStatusVector[2] = isc_arg_warning) then
        FIBConnection.HandleErrorOrWarning(lcPrepStmt, @FStatusVector, SQL, Self);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcPrepStmt,Self);
      if Assigned(FPlainDriver.fb_dsql_set_timeout) then begin
        Mem := StrToInt(DefineStatementParameter(Self, DSProps_StatementTimeOut, '0'));
        if Mem <> 0 then
          if FPlainDriver.fb_dsql_set_timeout(@FStatusVector, @FStmtHandle, Mem) <> 0 then
            FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, 'fb_dsql_set_timeout', Self);
      end;
      { Set Statement Type }
      TypeItem := AnsiChar(isc_info_sql_stmt_type);

      { Get information about a prepared DSQL statement. }
      if FPlainDriver.isc_dsql_sql_info(@FStatusVector, @FStmtHandle, 1,
          @TypeItem, SizeOf(Buffer), @Buffer[0]) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcPrepStmt, @FStatusVector, SQL, Self);

      if Buffer[0] = AnsiChar(isc_info_sql_stmt_type)
      then FStatementType := TZIbSqlStatementType(ReadInterbase6Number(FPlainDriver, @Buffer[1]))
      else FStatementType := stUnknown;

      if FStatementType in [stUnknown, stGetSegment, stPutSegment, stStartTrans, stCommit, stRollback] then begin
        FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_CLOSE);
        raise EZSQLException.Create(SStatementIsNotAllowed);
      end else if FStatementType in [stSelect, stExecProc, stSelectForUpdate] then begin
        FResultXSQLDA := TZSQLDA.Create(Connection, ConSettings);
        { Initialise ouput param and fields }
        XSQLDA := FResultXSQLDA.GetData;
        if FPlainDriver.isc_dsql_describe(@FStatusVector, @FStmtHandle, Word(FDialect), XSQLDA) <> 0 then
          FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe'{$ELSE}''{$ENDIF}, Self);
        FOrgTypeList.Clear;
        if FResultXSQLDA.GetData^.sqld <> FResultXSQLDA.GetData^.sqln then begin
          XSQLDA := FResultXSQLDA.AllocateSQLDA;
          if FPlainDriver.isc_dsql_describe(@FStatusVector, @FStmtHandle, Word(FDialect), XSQLDA) <> 0 then
            FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe'{$ELSE}''{$ENDIF}, Self);
        end;
        FOutMessageCount := FResultXSQLDA.GetData.sqld;
        FOrgTypeList.Capacity := FOutMessageCount;
        if FOutMessageCount > 0 then begin
          Mem := 0;
          {$R-}
          for Index := 0 to FOutMessageCount -1 do begin
            XSQLVAR := @XSQLDA.sqlvar[Index];
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
            NewSQLType := XSQLVAR.sqltype and not 1;
            FOrgTypeList.Add(NewSQLType, XSQLVAR.sqlscale, XSQLVAR.sqltype and 1 = 1);
            if (NewSQLType = SQL_INT128) or (NewSQLType = SQL_DEC_FIXED) then begin
              NewSQLType := SQL_VARYING;
              if XSQLVAR.sqltype and 1 = 1 then
                NewSQLType := NewSQLType and 1;
              XSQLVAR.sqltype := NewSQLType;
              XSQLVAR.sqlsubtype := CS_NONE;
              XSQLVAR.sqllen := 46;
            end else if (NewSQLType = SQL_TIME_TZ_EX) or (NewSQLType = SQL_TIME_TZ) then begin
              NewSQLType := SQL_TYPE_TIME;
              if XSQLVAR.sqltype and 1 = 1 then
                NewSQLType := NewSQLType and 1;
              XSQLVAR.sqltype := NewSQLType;
              XSQLVAR.sqllen := SizeOf(TISC_TIME);
            end else if (NewSQLType = SQL_DEC16) or (NewSQLType = SQL_DEC34) then begin
              NewSQLType := SQL_DOUBLE;
              if XSQLVAR.sqltype and 1 = 1 then
                NewSQLType := NewSQLType and 1;
              XSQLVAR.sqltype := NewSQLType;
              XSQLVAR.sqllen := SizeOf(Double);
            end;
            Mem := mem + XSQLVAR.sqllen;
            if XSQLVAR.sqltype and not (1) = SQL_VARYING then
              Mem := mem + SizeOf(ISC_USHORT);
            if XSQLVAR.sqltype and 1 = 1 then //nullable?
              Mem := mem + SizeOf(ISC_SHORT); //null indicator;
          end;
          if Mem = 0 then //see TestTicket426
            Mem := SizeOf(Cardinal);
          GetMem(FOutData, Mem); //alloc space as one block
          P := FOutData;
          {$R-}
          for Index := 0 to FOutMessageCount -1 do begin
            XSQLVAR := @XSQLDA.sqlvar[Index];
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
            XSQLVAR.sqldata := P;
            Inc(P, XSQLVAR.sqllen);
            if XSQLVAR.sqltype and not (1) = SQL_VARYING then
              Inc(P, SizeOf(ISC_SHORT));
            if XSQLVAR.sqltype and 1 = 1 then begin //nullable?
              XSQLVAR.sqlind := PISC_SHORT(P);
              Inc(P, SizeOf(ISC_SHORT));
            end else
              XSQLVAR.sqlind := nil;
          end;
        end;
      end;
    end;
    inherited Prepare; //log action and prepare params
  end;
  if BatchDMLArrayCount > 0 then begin
    //if not done already then split our query into pieces to build the
    //exceute block query
    if (not FQuerySplitted) then
      SplitQueryIntoPieces;

    {$IFDEF WITH_VAR_INIT_WARNING}PreparedRowsOfArray := 0;{$ENDIF}
    if FMaxRowsPerBatch = 0 then begin //init to find out max rows per batch
jmpEB:eBlock := GetExecuteBlockString(BatchDMLArrayCount, FIBConnection.GetXSQLDAMaxSize,
      PreparedRowsOfArray, FMaxRowsPerBatch, FPlainDriver);
    end else
      eBlock := '';
    FinalChunkSize := (BatchDMLArrayCount mod FMaxRowsPerBatch);
    if (FMaxRowsPerBatch <= BatchDMLArrayCount) and (FBatchStmts[True].Obj = nil) then begin
      if eBlock = '' then goto jmpEB;
      PrepareArrayStmt(FBatchStmts[True]); //max block size per batch
    end;
    if (FinalChunkSize > 0) and ((FBatchStmts[False].Obj = nil) or
       (FinalChunkSize <> FBatchStmts[False].PreparedRowsOfArray)) then //if final chunk then
      PrepareFinalChunk(FinalChunkSize);
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractInterbase6PreparedStatement.PrepareInParameters;
var
  StatusVector: TARRAY_ISC_STATUS;
  Index: Cardinal;
  XSQLVAR: PXSQLVAR;
  P: PAnsiChar;
  CodePageInfo: PZCodePage;
label jmpSetL_T;
begin
  With FIBConnection do begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(Connection, ConSettings);
    FParamXSQLDA := FParamSQLData.GetData;
    if FParamXSQLDA.sqln < BindList.Capacity then begin
      FParamXSQLDA.sqld := BindList.Capacity;
      FParamXSQLDA := FParamSQLData.AllocateSQLDA;
    end;
    {check dynamic sql}
    if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, Word(FDialect), FParamXSQLDA) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe_bind'{$ELSE}''{$ENDIF}, Self);

    //alloc space for lobs, arrays, param-types
    if ((FStatementType = stExecProc) and (FResultXSQLDA.GetFieldCount > 0)) or
       ((FStatementType = stSelect) and (BindList.HasOutOrInOutOrResultParam))
    then SetParamCount(FParamXSQLDA^.sqld + FResultXSQLDA.GetFieldCount)
    else SetParamCount(FParamXSQLDA^.sqld);

    { Resize XSQLDA structure if required }
    if FParamXSQLDA^.sqld <> FParamXSQLDA^.sqln then begin
      FParamXSQLDA := FParamSQLData.AllocateSQLDA;
      if FPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, Word(FDialect),FParamXSQLDA) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcOther, @FStatusVector, {$IFDEF DEBUG}'isc_dsql_describe_bind'{$ELSE}''{$ENDIF}, Self);
    end;
    FInMessageCount := FParamXSQLDA^.sqld;
    ReallocMem(FInParamDescripors, FInMessageCount * SizeOf(TZInterbaseFirerbirdParam));
    if FInMessageCount > 0 then begin
      FMemPerRow := 0;
      {$R-}
      for Index := 0 to FInMessageCount -1 do with FInParamDescripors[Index] do begin
        XSQLVAR := @FParamXSQLDA.sqlvar[Index];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        sqltype := XSQLVAR.sqltype and not (1);
        if sqltype = SQL_TEXT then begin //length might be zero
          //we don't use the fixed char fields. We don't space padd the data nor changing the sqllen
          sqltype := SQL_VARYING;
          goto jmpSetL_T;
        end else if (sqltype = SQL_TIMESTAMP_TZ) or (sqltype = SQL_TIMESTAMP_TZ_EX) then begin
          XSQLVAR.sqllen := SizeOf(TISC_TIMESTAMP);
          sqltype := SQL_TIMESTAMP;
          goto jmpSetL_T;
        end else if (sqltype = SQL_TIME_TZ) or  (sqltype = SQL_TIME_TZ_EX) then begin
          XSQLVAR.sqllen := SizeOf(TISC_TIME);
          sqltype := SQL_TYPE_TIME;
          goto jmpSetL_T;
        end else if (sqltype = SQL_DEC16) or (sqltype = SQL_DEC34) then begin
          XSQLVAR.sqllen := SQL_DOUBLE;
          sqllen := SizeOf(Double);
          goto jmpSetL_T;
        end else if (sqltype = SQL_INT128) or (sqltype = SQL_DEC_FIXED) then begin
          sqltype := SQL_VARYING;
          XSQLVAR.sqllen := 46;
jmpSetL_T:if XSQLVAR.sqltype and 1 = 1
          then XSQLVAR.sqltype := sqltype and not 1
          else XSQLVAR.sqltype := sqltype;
          CodePage := FClientCP;
        end;
        FMemPerRow := FMemPerRow + SizeOf(ISC_SHORT); //null indicator
        sqllen := XSQLVAR.sqllen;
        if sqltype = SQL_VARYING then begin
          CodePageInfo := FPlainDriver.ValidateCharEncoding(XSQLVAR.sqlsubtype and 255);
          sqllen := XSQLVAR.sqllen;
          if CodePageInfo <> nil then begin
            sqllen := sqllen + Byte(CodePageInfo.CharWidth);
            CodePage := CodePageInfo.CP;
          end;
          sqllen := (((sqllen-1) shr 2) +1) shl 2; //4Byte align incluing 4 bytes reserved for overlongs {let fb raise the Exception}
          XSQLVAR.sqllen := sqllen;
          FMemPerRow := FMemPerRow + SizeOf(ISC_SHORT); //len indicator
        end else if (sqltype = SQL_BLOB) and (XSQLVAR.sqlsubtype = isc_blob_text)
          then codepage := FClientCP
          else CodePage := zCP_Binary;
        FMemPerRow := FMemPerRow + sqllen; //data
        sqlscale := XSQLVAR.sqlscale;
        XSQLVAR.sqltype := XSQLVAR.sqltype or 1;
        sqlsubType := XSQLVAR.sqlsubtype;
      end;
      GetMem(FInData, FMemPerRow); //alloc space as one block
      P := FInData;
      { write offset addresses we scribble in }
      {$R-}
      for Index := 0 to FInMessageCount -1 do with FInParamDescripors[Index] do begin
        XSQLVAR := @FParamXSQLDA.sqlvar[Index];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        sqldata := P;
        sqllen := XSQLVAR.sqllen;
        XSQLVAR.sqldata := P;
        Inc(P, sqllen);
        if sqltype = SQL_VARYING then
          Inc(P, SizeOf(ISC_SHORT));
        sqlind := PISC_SHORT(P);
        XSQLVAR.sqlind := PISC_SHORT(P);
        Inc(P, SizeOf(ISC_SHORT));
      end;
    end;
  end;
end;

procedure TZAbstractInterbase6PreparedStatement.Unprepare;
begin
  FMaxRowsPerBatch := 0;
  FResultXSQLDA := nil;
  FParamSQLData := nil;
  inherited Unprepare;
  if (FStmtHandle <> 0) then //check if prepare did fail. otherwise we unprepare the handle
    if FPlainDriver.isc_dsql_free_statement(@fStatusVector, @FStmtHandle, DSQL_UNPREPARE) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcUnprepStmt, @FStatusVector, SQL, Self);
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcUnprepStmt,Self);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractInterbase6PreparedStatement.UnPrepareInParameters;
begin
  inherited UnPrepareInParameters;
  FParamXSQLDA := nil;
end;

procedure TZAbstractInterbase6PreparedStatement.WriteLobBuffer(Index: Cardinal; P: PAnsiChar; Len: NativeUInt);
var
  BlobHandle: TISC_BLOB_HANDLE;
  StatusVector: TARRAY_ISC_STATUS;
  CurPos, SegLen: Integer;
begin
  BlobHandle := 0;
  {$R-}
  with FInParamDescripors[Index] do begin
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    { create blob handle }
    with FIBConnection do
      if FPlainDriver.isc_create_blob2(@StatusVector, GetDBHandle, GetTrHandle,
        @BlobHandle, PISC_QUAD(sqldata), 0, nil) <> 0 then
      HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, 'create lob', Self);

    { put data to blob }
    CurPos := 0;
    SegLen := DefaultBlobSegmentSize;
    while (NativeUint(CurPos) < Len) do begin
      if (NativeUInt(CurPos + SegLen) > Len) then
        SegLen := Len - NativeUint(CurPos);
      if FPlainDriver.isc_put_segment(@StatusVector, @BlobHandle, SegLen, P) <> 0 then
        FIBConnection.HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, 'write lob', Self);
      Inc(CurPos, SegLen);
      Inc(P, SegLen);
    end;

    { close blob handle }
    if FPlainDriver.isc_close_blob(@StatusVector, @BlobHandle) <> 0 then
      FIBConnection.HandleErrorOrWarning(lcBindPrepStmt, @FStatusVector, 'close lob', Self);
    sqlind^ := ISC_NOTNULL;
  end;
end;

function TZAbstractInterbase6PreparedStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  ExecuteInternal;
  { Create ResultSet if possible else free Statement Handle }
  if (FStatementType in [stSelect, stExecProc, stSelectForUpdate]) and (FResultXSQLDA.GetFieldCount <> 0) then begin
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
    if (FStatementType = stExecProc) or BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := LastResultSet;
  end else
    LastResultSet := nil;
  Result := LastResultSet <> nil;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

function TZAbstractInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  ExecuteInternal;
  if (FResultXSQLDA <> nil) and (FResultXSQLDA.GetFieldCount <> 0) then begin
    if (FStatementType = stSelect) and Assigned(FOpenResultSet) and not BindList.HasOutOrInOutOrResultParam
    then Result := IZResultSet(FOpenResultSet)
    else Result := CreateResultSet;
    if (FStatementType = stExecProc) or BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := Result;
  end else begin
    Result := nil;
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

function TZAbstractInterbase6PreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  LastResultSet := nil;
  PrepareOpenResultSetForReUse;
  ExecuteInternal;
  Result := LastUpdateCount;
  if BatchDMLArrayCount = 0 then
    case FStatementType of
      stCommit, stRollback, stUnknown: Result := -1;
      stSelect: if BindList.HasOutParam then begin
          FOutParamResultSet := CreateResultSet;
          FOpenResultSet := nil;
        end else if FResultXSQLDA.GetFieldCount <> 0 then
          if FPlainDriver.isc_dsql_free_statement(@FStatusVector, @FStmtHandle, DSQL_CLOSE) <> 0 then
            FIBConnection.HandleErrorOrWarning(lcExecPrepStmt, @FStatusVector, SQL, Self);
      stExecProc: { Create ResultSet if possible }
        if FResultXSQLDA.GetFieldCount <> 0 then
          FOutParamResultSet := CreateResultSet;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //nothing todo
    end;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{ TZInterbase6Statement }

constructor TZInterbase6Statement.Create(const Connection: IZInterbase6Connection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZInterbase6CallableStatement }

function TZInterbase6CallableStatement.InternalCreateExecutionStatement(
  const Connection: IZInterbaseFirebirdConnection; const SQL: String;
  Params: TStrings): TZAbstractPreparedStatement;
begin
  Result := TZInterbase6PreparedStatement.Create(Connection as IZInterbase6Connection, SQL, Params);
end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.

