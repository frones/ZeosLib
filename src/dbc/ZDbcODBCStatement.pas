{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           ODBC Database Connectivity Classes           }
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

unit ZDbcODBCStatement;

interface

{$I ZDbc.inc}

uses Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZCompatibility, ZDbcIntfs, ZDbcStatement, ZVariant,
  ZDbcODBCCon, ZPlainODBCDriver;

type
  PSQLHDBC = ^SQLHDBC;

  IZODBCStatement = interface(IZPreparedStatement)
    ['{FF765EE4-B278-451E-A534-333C22D234B1}']
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
  end;

  PZODBCParamInfo = ^TZODBCParamInfo;
  TZODBCParamInfo = record
    DataType: SQLSMALLINT;
    C_DataType: SQLSMALLINT;
    InputDataType: SQLSMALLINT;
    DecimalDigits: SQLSMALLINT;
    LastParamDataPtr, CurrParamDataPtr: SQLPOINTER;
    LastStrLen_or_IndPtr, CurrStrLen_or_IndPtr: SQLPOINTER;
    Nullable: SQLSMALLINT;
    ColumnSize: SQLLEN;
    BufferSize: SQLLEN;
    ValueCount: SQLULEN;
    SQLType: TZSQLType;
    DescBindBuffer: TByteDynArray;
  end;

  TZAbstractODBCStatement = class(TZAbstractPreparedStatement, IZODBCStatement)
  private
    fPlainDriver: TZODBC3PlainDriver;
    fPHDBC: PSQLHDBC;
    fHSTMT: SQLHSTMT;
    fParamInfos: array of TZODBCParamInfo;
    fStreamSupport: Boolean;
    fZBufferSize, fArrayOffSet, fMaxBufArrayBound, fCurrentIterations: Integer;
    fStmtTimeOut: SQLULEN;
    fEnhancedColInfo: Boolean;
    fBufferSize: Integer;
    fBatchLobBuf: TIZBlobsDynArray;
    fMoreResultsIndicator: TZMoreResultsIndicator;
    fBindBuffer: TByteDynArray;
    fBindRowWise: Boolean;
    fLastAutoCommit: Boolean;
    procedure InternalExecute;
    procedure InternalBindParams;
    procedure PrepareOpenedResultSetsForReusing;
  protected
    procedure CheckStmtError(RETCODE: SQLRETURN);
    function InternalCreateResultSet: IZResultSet; virtual; abstract;
    procedure InternalBeforePrepare;
    function GetCurrentResultSet: IZResultSet;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function SupportsSingleColumnArrays: Boolean; override;
  public
    constructor Create(const Connection: IZODBCConnection;
      var ConnectionHandle: SQLHDBC; const SQL: string; Info: TStrings);

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Close; override;
    procedure Cancel; override;


    procedure SetDataArray(ParameterIndex: Integer; const Value;
      const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); override;

    procedure ClearParameters; override;
  public
    function GetMoreResultsIndicator: TZMoreResultsIndicator;
    procedure SetMoreResultsIndicator(Value: TZMoreResultsIndicator);
  end;

  TZODBCPreparedStatementW = class(TZAbstractODBCStatement)
  protected
    function InternalCreateResultSet: IZResultSet; override;
  public
    procedure Prepare; override;
  end;

  TZODBCPreparedStatementA = class(TZAbstractODBCStatement)
  protected
    function InternalCreateResultSet: IZResultSet; override;
  public
    procedure Prepare; override;
  end;

implementation

uses Math, DateUtils,
  ZSysUtils, ZMessages, ZEncoding, ZDbcUtils, ZDbcResultSet, ZFastCode, ZDbcLogging,
  ZDbcODBCUtils, ZDbcODBCResultSet, ZDbcCachedResultSet, ZDbcGenericResolver, ZClasses;

const
  NullInd: array[Boolean] of SQLLEN = (SQL_NO_NULLS, SQL_NULL_DATA);

type
  PLobArray = ^TLobArray;
  TLobArray = array[0..High(Word)] of IInterface;

{ TZAbstractODBCStatement }

procedure TZAbstractODBCStatement.BindInParameters;
var
  RowCount: SQLLEN;
begin
  fLastAutoCommit := Connection.GetAutoCommit;
  if Length(fParamInfos) = 0 then
    exit
  else
    if (ArrayCount = 0) or (fMaxBufArrayBound = -1) then begin
      if fCurrentIterations <> 1 then begin
        fCurrentIterations := 1;
        SetLength(fBindBuffer, fBufferSize)
      end;
      InternalBindParams;
    end else begin
      LastUpdateCount := 0;
      fArrayOffSet := 0;
      Connection.SetAutoCommit(False);
      while True do
      begin
        if (FArrayOffSet+fMaxBufArrayBound >= ArrayCount) then
        begin //left space for last excution
          if fCurrentIterations <> ArrayCount - FArrayOffSet then begin
            fCurrentIterations := ArrayCount - FArrayOffSet;
            SetLength(fBindBuffer, fBufferSize*fCurrentIterations)
          end;
          InternalBindParams;
          FArrayOffSet := 0; //Reset!
          Break
        end
        else
        begin
          if fCurrentIterations <> fMaxBufArrayBound then begin
            fCurrentIterations := fMaxBufArrayBound;
            SetLength(fBindBuffer, fBufferSize*fMaxBufArrayBound)
          end;
          InternalBindParams;
          InternalExecute;
          Inc(FArrayOffSet, fMaxBufArrayBound);
          CheckStmtError(fPlainDriver.SQLRowCount(fHSTMT, @RowCount));
          LastUpdateCount := LastUpdateCount + RowCount;
        end
      end
    end;
  inherited BindInParameters;
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZAbstractODBCStatement.Cancel;
var RetCode: SQLRETURN;
begin
  if fHSTMT <> nil then begin
    RetCode := FPlainDriver.SQLCancel(fHSTMT);
    if RETCODE <> SQL_SUCCESS then
      CheckODBCError(RETCODE, fHSTMT, SQL_HANDLE_STMT, Connection as IZODBCConnection);
  end;
end;

procedure TZAbstractODBCStatement.CheckStmtError(RETCODE: SQLRETURN);
begin
  CheckODBCError(RETCODE, fHSTMT, SQL_HANDLE_STMT, Connection as IZODBCConnection);
end;

procedure TZAbstractODBCStatement.ClearParameters;
var I: Integer;
begin
  inherited ClearParameters;
  for i := low(fBatchLobBuf) to high(fBatchLobBuf) do
    SetLength(fBatchLobBuf[i], 0);
end;

procedure TZAbstractODBCStatement.Close;
begin
  inherited Close;
  if Assigned(fHSTMT) then begin
    fPlainDriver.SQLFreeHandle(SQL_HANDLE_STMT, fHSTMT);
    fHSTMT := nil;
  end;
end;

constructor TZAbstractODBCStatement.Create(const Connection: IZODBCConnection;
  var ConnectionHandle: SQLHDBC; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  //inherited Create(Connection, Connection.NativeSQL(SQL), Info);
  fPlainDriver := TZODBC3PlainDriver(Connection.GetPlainDriver.GetInstance);
  fStreamSupport := Connection.ODBCVersion >= {%H-}Word(SQL_OV_ODBC3_80);
  fPHDBC := @ConnectionHandle;
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072); //by default 128KB
  FEnhancedColInfo := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'enhanced_column_info', 'True'));
  fStmtTimeOut := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'StatementTimeOut', ''), SQL_QUERY_TIMEOUT_DEFAULT); //execution timeout in seconds by default 1
  fMoreResultsIndicator := TZMoreResultsIndicator(Ord(not Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets));
  fBindRowWise := False;
end;

function TZAbstractODBCStatement.ExecutePrepared: Boolean;
var
  RowCount: SQLLEN;
  ColumnCount: SQLSMALLINT;
  RETCODE: SQLRETURN;
begin
  PrepareOpenedResultSetsForReusing;
  LastUpdateCount := 0;
  Prepare;
  BindInParameters;
  try
    InternalExecute;
    CheckStmtError(fPlainDriver.SQLRowCount(fHSTMT, @RowCount));
    LastUpdateCount := LastUpdateCount + RowCount;
    CheckStmtError(fPlainDriver.SQLNumResultCols(fHSTMT, @ColumnCount));
    if ColumnCount > 0 then begin
      LastUpdateCount := -1;
      LastResultSet := GetCurrentResultSet;
    end else
      if Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets and
         (fMoreResultsIndicator <> mriHasNoMoreResults) then
        repeat
          RETCODE := fPlainDriver.SQLMoreResults(fHSTMT);
          if RETCODE = SQL_SUCCESS then begin
            fMoreResultsIndicator := mriHasMoreResults;
            CheckStmtError(fPlainDriver.SQLNumResultCols(fHSTMT, @ColumnCount));
            if ColumnCount > 0 then
              LastResultSet := GetCurrentResultSet;
          end else if RETCODE = SQL_NO_DATA then begin
            if fMoreResultsIndicator <> mriHasMoreResults then
              fMoreResultsIndicator := mriHasNoMoreResults;
          end else
            CheckStmtError(RETCODE);
        until RETCODE = SQL_NO_DATA;
    Result := Assigned(LastResultSet);
  finally
    Connection.SetAutoCommit(fLastAutoCommit);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  end;
end;

function TZAbstractODBCStatement.ExecuteQueryPrepared: IZResultSet;
var
  ColumnCount: SQLSMALLINT;
  RETCODE: SQLRETURN;
begin
  PrepareOpenedResultSetsForReusing;
  Prepare;
  BindInParameters;
  try
    InternalExecute;
    CheckStmtError(fPlainDriver.SQLNumResultCols(fHSTMT, @ColumnCount));
    if ColumnCount > 0 then
      if Assigned(FOpenResultSet) then
        Result := IZResultSet(FOpenResultSet)
      else
        Result := GetCurrentResultSet
    else begin
      Result := nil;
      if Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets and
        (fMoreResultsIndicator <> mriHasNoMoreResults) then
      begin
        repeat
          RETCODE := fPlainDriver.SQLMoreResults(fHSTMT);
          if RETCODE = SQL_SUCCESS then begin
            fMoreResultsIndicator := mriHasMoreResults;
            CheckStmtError(fPlainDriver.SQLNumResultCols(fHSTMT, @ColumnCount));
            if ColumnCount > 0 then
              Result := GetCurrentResultSet
          end else if RETCODE = SQL_NO_DATA then begin
            if fMoreResultsIndicator <> mriHasMoreResults then
              fMoreResultsIndicator := mriHasNoMoreResults;
          end else
            CheckStmtError(RETCODE);
        until RETCODE = SQL_NO_DATA;
      end;
    end;
    if Result = nil then
      raise EZSQLException.Create(SCanNotOpenResultSet);
  finally
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  end;
end;

function TZAbstractODBCStatement.ExecuteUpdatePrepared: Integer;
var RowCount: SQLLEN;
begin
  if Assigned(FOpenResultSet) then IZResultSet(FOpenResultSet).Close;
  if Assigned(LastResultSet) then LastResultSet.Close;
  FOpenResultSet := nil; LastResultSet := nil;
  Prepare;
  BindInParameters;
  LastUpdateCount := 0;
  try
    InternalExecute;
    CheckStmtError(fPlainDriver.SQLRowCount(fHSTMT, @RowCount));
    LastUpdateCount := LastUpdateCount + RowCount;
    Result := RowCount;
  finally
    Connection.SetAutoCommit(fLastAutoCommit);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  end;
end;

function TZAbstractODBCStatement.GetCurrentResultSet: IZResultSet;
var
  CachedResolver: IZCachedResolver;
  NativeResultSet: IZResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  Result := nil;
  NativeResultSet := InternalCreateResultSet;
  if (GetResultSetConcurrency = rcUpdatable) or
     (GetResultSetType <> rtForwardOnly) then
  begin
    CachedResolver := TZGenericCachedResolver.Create(Self, NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
  FOpenResultSet := Pointer(Result);
end;

function TZAbstractODBCStatement.GetMoreResultsIndicator: TZMoreResultsIndicator;
begin
  Result := fMoreResultsIndicator;
end;

procedure TZAbstractODBCStatement.InternalBeforePrepare;
begin
  if not Assigned(fHSTMT) then begin
    CheckODBCError(fPlainDriver.SQLAllocHandle(SQL_HANDLE_STMT, fPHDBC^, fHSTMT), fPHDBC^, SQL_HANDLE_DBC, Connection as IZODBCConnection);
    CheckStmtError(fPlainDriver.SQLSetStmtAttr(fHSTMT, SQL_ATTR_QUERY_TIMEOUT, SQLPOINTER(fStmtTimeOut), 0));
    fMoreResultsIndicator := mriUnknown;
  end;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractODBCStatement.InternalBindParams;
var
  I, J: SQLUSMALLINT;
  Value: TZVariant;
  ParamSetChanged: Boolean;
  TmpLobIndex: Integer;
  Param: PZODBCParamInfo;
  StrLen_or_IndOffSet, ParameterDataOffSet: Integer;

  GUID: TGUID;
  { array DML bindings }
  ZData: Pointer; //array entry
  { using mem entry of ZData saves cast code }
  ZBooleanArray: TBooleanDynArray absolute ZData;
  ZByteArray: TByteDynArray absolute ZData;
  ZShortIntArray: TShortIntDynArray absolute ZData;
  ZWordArray: TWordDynArray absolute ZData;
  ZSmallIntArray: TSmallIntDynArray absolute ZData;
  ZLongWordArray: TLongWordDynArray absolute ZData;
  ZIntegerArray: TIntegerDynArray absolute ZData;
  ZInt64Array: TInt64DynArray absolute ZData;
  ZUInt64Array: TUInt64DynArray absolute ZData;
  ZSingleArray: TSingleDynArray absolute ZData;
  ZDoubleArray: TDoubleDynArray absolute ZData;
  ZCurrencyArray: TCurrencyDynArray absolute ZData;
  ZExtendedArray: TExtendedDynArray absolute ZData;
  ZDateTimeArray: TDateTimeDynArray absolute ZData;
  ZRawByteStringArray: TRawByteStringDynArray absolute ZData;
  {$IFNDEF NO_ANSISTRING}
  ZAnsiStringArray: TAnsiStringDynArray absolute ZData;
  {$ENDIF}
  ZUTF8StringArray: TUTF8StringDynArray absolute ZData;
  {$IFNDEF NO_UTF8STRING}
  ZStringArray: TStringDynArray absolute ZData;
  {$ENDIF}
  ZUnicodeStringArray: TUnicodeStringDynArray absolute ZData;
  ZCharRecArray: TZCharRecDynArray absolute ZData;
  ZBytesArray: TBytesDynArray absolute ZData;
  ZInterfaceArray: TInterfaceDynArray absolute ZData;
  ZGUIDArray: TGUIDDynArray absolute ZData;

  ParameterDataPtr: SQLPOINTER;
  StrLen_or_IndPtr: SQLPOINTER;

  year, fraction: Word;
  TempBlob: IZBlob;
  TmpStream: TStream;
  RawTemp: RawByteString;
  CharRec: TZCharRec;

  function IsNotNull: Boolean;
  begin
    if Assigned(Value.VArray.VIsNullArray) and TBooleanDynArray(Value.VArray.VIsNullArray)[j+fArrayOffSet] then begin
      PSQLLEN(StrLen_or_IndPtr)^ := SQL_NULL_DATA;
      Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
      Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
      Result := False;
    end else begin
      PSQLLEN(StrLen_or_IndPtr)^ := SQL_NO_NULLS;
      Result := True;
    end;
  end;

  function LobIsNotNull: Boolean;
  begin
    Result := not (Assigned(Value.VArray.VIsNullArray) and TBooleanDynArray(Value.VArray.VIsNullArray)[j+fArrayOffSet]);
    PSQLLEN(StrLen_or_IndPtr)^ := SQL_DATA_AT_EXEC;
    if (not Result) then
      Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;

  procedure SetByte(B: Byte);
  begin
    PByte(ParameterDataPtr)^ := B;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetShort(S: ShortInt);
  begin
    PShortInt(ParameterDataPtr)^ := s;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetWord(W: Word);
  begin
    PWord(ParameterDataPtr)^ := W;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetSmall(S: SmallInt);
  begin
    PSmallInt(ParameterDataPtr)^ := S;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetCardinal(C: Cardinal);
  begin
    PLongWord(ParameterDataPtr)^ := C;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetInteger(I: Integer);
  begin
    PInteger(ParameterDataPtr)^ := I;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetUInt64(const I: UInt64);
  begin
    PUInt64(ParameterDataPtr)^ := I;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetInt64(const I: Int64);
  begin
    PInt64(ParameterDataPtr)^ := I;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetSingle(s: Single); overload;
  begin
    PSingle(ParameterDataPtr)^ := S;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetSingle(P: PAnsiChar; Len: Integer); overload;
  begin
    SQLStrToFloatDef(P, 0, PSingle(ParameterDataPtr)^, Len);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetSingle(P: PWideChar; Len: Integer); overload;
  begin
    SQLStrToFloatDef(P, 0, PSingle(ParameterDataPtr)^, Len);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetDouble(const d: Double); overload;
  begin
    PDouble(ParameterDataPtr)^ := D;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetDouble(P: PAnsiChar; Len: Integer); overload;
  begin
    SQLStrToFloatDef(P, 0, PDouble(ParameterDataPtr)^, Len);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetDouble(P: PWideChar; Len: Integer); overload;
  begin
    SQLStrToFloatDef(P, 0, PDouble(ParameterDataPtr)^, Len);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetBinary(P: Pointer; Len: Integer);
  begin
    PSQLLEN(StrLen_or_IndPtr)^ := Len;
    if Assigned(P) then
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, ParameterDataPtr^, PSQLLEN(StrLen_or_IndPtr)^);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetDate(const D: TDateTime);
  begin
    DecodeDate(D, Year, PSQL_DATE_STRUCT(ParameterDataPtr)^.month, PSQL_DATE_STRUCT(ParameterDataPtr)^.day);
    PSQL_DATE_STRUCT(ParameterDataPtr)^.year := year;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetTime(const D: TDateTime);
  begin
    if Param.C_DataType = SQL_C_BINARY then begin
      //https://bytes.com/topic/sql-server/answers/851494-sql-state-22003-numeric-value-out-range-time-data-type
      PSQLLEN(StrLen_or_IndPtr)^ := SizeOf(TSQL_SS_TIME2_STRUCT);
      DecodeTime(D, PSQL_SS_TIME2_STRUCT(ParameterDataPtr)^.hour, PSQL_SS_TIME2_STRUCT(ParameterDataPtr)^.minute,
        PSQL_SS_TIME2_STRUCT(ParameterDataPtr)^.second, fraction);
      //https://msdn.microsoft.com/de-de/library/bb677243%28v=sql.120%29.aspx
      PSQL_SS_TIME2_STRUCT(ParameterDataPtr)^.fraction := fraction*1000000;
    end else
      DecodeTime(D, PSQL_TIME_STRUCT(ParameterDataPtr)^.hour, PSQL_TIME_STRUCT(ParameterDataPtr)^.minute,
        PSQL_TIME_STRUCT(ParameterDataPtr)^.second, fraction);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetTimeStamp(const D: TDateTime);
  begin
    DecodeDateTime(D, Year, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.day,
      PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.hour, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.second, fraction);
    PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.year := year;
    PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.fraction := fraction*1000000;
      //https://social.msdn.microsoft.com/Forums/sqlserver/en-US/ac1b5a6d-5e64-4603-9c92-b75ba4e51bf2/error-22008-datetime-field-overflow-when-inserting-a-record-with-datetime2-field-via-odbc?forum=sqldataaccess
    //PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.fraction := 0;//puff what a shit: fraction available but can't be set???;
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetPUnicode(P: PWidechar; Len: Integer);
  begin
    PSQLLEN(StrLen_or_IndPtr)^ := Min(Param.BufferSize-2, Len shl 1);
    if Assigned(P) then
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, ParameterDataPtr^, PSQLLEN(StrLen_or_IndPtr)^);
    PWord(PWideChar(ParameterDataPtr)+(PSQLLEN(StrLen_or_IndPtr)^ shr 1))^ := Ord(#0);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetUnicode(const U: ZWideString);{$IFDEF WITH_INLINE}inline;{$ENDIF}
  begin
    SetPUnicode(Pointer(U), Length(U));
  end;
  procedure SetPRaw(P: PAnsiChar; Len: Integer);
  begin
    PSQLLEN(StrLen_or_IndPtr)^ := Min(Param.BufferSize-1, Len);
    if Assigned(P) then
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, ParameterDataPtr^, PSQLLEN(StrLen_or_IndPtr)^);
    PByte(PAnsiChar(ParameterDataPtr)+PSQLLEN(StrLen_or_IndPtr)^)^ := Ord(#0);
    Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetRaw(const R: RawByteString);{$IFDEF WITH_INLINE}inline;{$ENDIF}
  begin
    SetPRaw(Pointer(R), Length(R));
  end;
  procedure SetLob(const Lob: IInterface);
  begin
    PLobArray(PPointer(Param.CurrParamDataPtr)^)[j] := lob;
    Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
  end;
  procedure SetCLobFromPUnicode(P: PWidechar; Len: Integer);
  begin
    SetLob(TZAbstractCLob.CreateWithData(P, Len, ConSettings));
  end;
  procedure SetCLobFromUnicode(const U: ZWideString); {$IFDEF WITH_INLINE}inline;{$ENDIF}
  begin
    if Pointer(U) = nil then
      SetCLobFromPUnicode(PEmptyUnicodeString, 0)
    else
      SetCLobFromPUnicode(PWideChar(Pointer(U)), Length(U));
  end;
  procedure SetClobFromPRaw(P: PAnsiChar; Len: Integer);
  begin
    SetLob(TZAbstractCLob.CreateWithData(P, Len, ConSettings^.ClientCodePage^.CP, ConSettings));
  end;
  procedure SetClobFromRaw(const R: RawByteString); {$IFDEF WITH_INLINE}inline;{$ENDIF}
  begin
    if Pointer(R) = nil then
      SetClobFromPRaw(PEmptyAnsiString, 0)
    else
      SetClobFromPRaw(Pointer(R), Length(R));
  end;
  procedure SetBlob(P: Pointer; Len: Integer); {$IFDEF WITH_INLINE}inline;{$ENDIF}
  begin
    SetLob(TZAbstractBLob.CreateWithData(P, Len));
  end;
  procedure SetBlobFromBts(const B: TBytes); {$IFDEF WITH_INLINE}inline;{$ENDIF}
  begin
    SetBlob(Pointer(B), Length(B));
  end;
begin
  { missing point(no test case): desc array bindings of a single column so not params are arrays indicated by fMaxBufArrayBound = -1 }
  if Length(fParamInfos) > 0 then begin
    ParamSetChanged := False;
    TmpLobIndex := -1;
    Assert(((fMaxBufArrayBound = -1) and (ArrayCount = 0)) or (fMaxBufArrayBound > 0), 'Desc array binding not done yet');
    StrLen_or_IndPtr := Pointer(fBindBuffer);
    if fBindRowWise then
      ParameterDataPtr := {%H-}Pointer({%H-}NativeUInt(StrLen_or_IndPtr)+SizeOf(SQLLEN))
    else
      ParameterDataPtr := {%H-}Pointer({%H-}NativeUInt(StrLen_or_IndPtr)+(SizeOf(SQLLEN)*Cardinal(fCurrentIterations)));
    { initialize offset ranges }
    if (not fBindRowWise) or (fMaxBufArrayBound < 0) or (ArrayCount = 0) then begin
      StrLen_or_IndOffSet := SizeOf(SQLLEN);
      ParameterDataOffSet := 0;
    end else begin
      StrLen_or_IndOffSet := fBufferSize;
      ParameterDataOffSet := fBufferSize;
    end;
    for i := low(fParamInfos) to high(fParamInfos) do begin
      Value := InParamValues[i];
      Param := @fParamInfos[I];
      Param.CurrStrLen_or_IndPtr := StrLen_or_IndPtr;
      if (ArrayCount = 0) or (not fBindRowWise) or (fMaxBufArrayBound < 0) then
        ParameterDataOffSet := Param^.BufferSize;
      if Value.VType = vtArray then begin //array DML binding
        if Assigned(Value.VArray.VArray) then begin
          if fMaxBufArrayBound < 0 then begin
            fCurrentIterations := {%H-}PArrayLenInt({%H-}NativeUInt(Value.VArray.VArray) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF} //FPC returns High() for this pointer location
          end else
            fCurrentIterations := Min(fMaxBufArrayBound, {%H-}PArrayLenInt({%H-}NativeUInt(Value.VArray.VArray) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}-fArrayOffSet);
          { fast binary C and Pascal equal binary types first if we bind column wise }
          if (not fBindRowWise) and (TZSQLType(Value.VArray.VArrayType) = Param.SQLType) and
             ((Ord(Param.SQLType) <= Ord(stDouble)) and (Ord(Param.SQLType) > Ord(stBoolean))) then begin
            ZData := Value.VArray.VArray;
            //!no! move just reference -> can be used for inout param as well
            case Param.SQLType of
              stByte:     Param.CurrParamDataPtr := @ZByteArray[fArrayOffSet];
              stShort:    Param.CurrParamDataPtr := @ZShortIntArray[fArrayOffSet];
              stWord:     Param.CurrParamDataPtr := @ZWordArray[fArrayOffSet];
              stSmall:    Param.CurrParamDataPtr := @ZSmallIntArray[fArrayOffSet];
              stLongWord: Param.CurrParamDataPtr := @ZLongWordArray[fArrayOffSet];
              stInteger:  Param.CurrParamDataPtr := @ZIntegerArray[fArrayOffSet];
              stULong:    Param.CurrParamDataPtr := @ZUInt64Array[fArrayOffSet];
              stLong:     Param.CurrParamDataPtr := @ZInt64Array[fArrayOffSet];
              stFloat:    Param.CurrParamDataPtr := @ZSingleArray[fArrayOffSet];
              stDouble:   Param.CurrParamDataPtr := @ZDoubleArray[fArrayOffSet];
              else Param.CurrParamDataPtr := nil; //satisfy compiler
            end;
            Param.ValueCount := fCurrentIterations;
            if Assigned(Value.VArray.VIsNullArray) then
              case TZSQLType(Value.VArray.VIsNullArrayType) of
                stBoolean: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TBooleanDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stByte: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TByteDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stShort: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TShortIntDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stWord: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TWordDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stSmall: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TSmallIntDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stInteger: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TIntegerDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stLongWord: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TLongwordDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stLong: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TInt64DynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stULong: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TUInt64DynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stFloat: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TSingleDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stDouble: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TDoubleDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stBigDecimal: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TExtendedDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stTime, stDate, stTimeStamp: for J := fArrayOffSet to fCurrentIterations -1 do begin
                    PSQLLEN(StrLen_or_IndPtr)^ := NullInd[TDateTimeDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]<>0];
                    Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                  end;
                stString, stUnicodeString:
                    case Value.VArray.VIsNullArrayVariantType of
                      {$IFNDEF UNICODE}vtString, {$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String, {$ENDIF}
                      vtRawByteString:
                        for J := fArrayOffSet to fCurrentIterations -1 do begin
                          PSQLLEN(StrLen_or_IndPtr)^ := NullInd[StrToBoolEx(TRawByteStringDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet])];
                          Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                        end;
                      {$IFDEF UNICODE} vtString, {$ENDIF}
                      vtUnicodeString:
                        for J := fArrayOffSet to fCurrentIterations -1 do begin
                          PSQLLEN(StrLen_or_IndPtr)^ := NullInd[StrToBoolEx(TUnicodeStringDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet])];
                          Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                        end;
                      vtCharRec:
                        for J := fArrayOffSet to fCurrentIterations -1 do begin
                          if ZCompatibleCodePages(TZCharRecDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet].CP, zCP_UTF16)
                          then PSQLLEN(StrLen_or_IndPtr)^ := NullInd[StrToBoolEx(PWideChar(TZCharRecDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet].P))]
                          else PSQLLEN(StrLen_or_IndPtr)^ := NullInd[StrToBoolEx(PAnsiChar(TZCharRecDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet].P))];
                          Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
                        end;
                      else
                        raise EZSQLException.Create(sUnsupportedOperation);
                    end;
              end
            else if Assigned(Value.VArray.VArray) then
              FillChar(StrLen_or_IndPtr^, fCurrentIterations*SizeOf(SQLLEN), #0) { fast not null indcator.. }
            else
              for J := fArrayOffSet to fCurrentIterations -1 do begin
                PSQLLEN(StrLen_or_IndPtr)^ := SQL_NULL_DATA;
                Inc(PAnsiChar(StrLen_or_IndPtr), SizeOf(SQLLEN));
              end;
            Inc(PAnsiChar(ParameterDataPtr), fCurrentIterations*Param.BufferSize);//don't forget!
          end else begin
            { remaining are conversion effected or move required types only }
            if SQLULEN(fCurrentIterations) <> Param.ValueCount then begin
              Param.ValueCount := fCurrentIterations;
              ParamSetChanged := True;
            end;
            Param.CurrParamDataPtr := ParameterDataPtr;
            { EH note : the following code make 2 case(ODBC expected and Zeos given) checks and the array loop follows
              this is up to a half second faster for 5000 rows with 5 field bug ugly to read  ... is it?
              optimized code ... however }
            ZData := Value.VArray.VArray;
            case Param.SQLType of
              stBoolean:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZByteArray[J+fArrayOffSet] <> 0));
                  stShort:          for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZShortIntArray[J+fArrayOffSet] <> 0));
                  stWord:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZWordArray[J+fArrayOffSet] <> 0));
                  stSmall:          for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZSmallIntArray[J+fArrayOffSet] <> 0));
                  stLongWord:       for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZLongWordArray[J+fArrayOffSet] <> 0));
                  stInteger:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZIntegerArray[J+fArrayOffSet] <> 0));
                  stLong:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZInt64Array[J+fArrayOffSet] <> 0));
                  stULong:          for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZUInt64Array[J+fArrayOffSet] <> 0));
                  stFloat:          for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZSingleArray[J+fArrayOffSet] <> 0));
                  stDouble:         for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZDoubleArray[J+fArrayOffSet] <> 0));
                  stCurrency:       for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZCurrencyArray[J+fArrayOffSet] <> 0));
                  stBigDecimal:     for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZExtendedArray[J+fArrayOffSet] <> 0));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(StrToBoolEx(ZRawByteStringArray[J+fArrayOffSet])));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(StrToBoolEx(ZUnicodeStringArray[J+fArrayOffSet])));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do If IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetByte(Ord(StrToBoolEx(PWideChar(ZCharRecArray[J+fArrayOffSet].P))))
                                          else
                                            SetByte(Ord(StrToBoolEx(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P))));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZDateTimeArray[J+fArrayOffSet] <> 0));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stByte:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZLongWordArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZIntegerArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZInt64Array[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(ZUInt64Array[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetByte(UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetByte(RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do If IsNotNull then SetByte(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stShort:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZLongWordArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZIntegerArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZInt64Array[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(ZUInt64Array[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetShort(RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetShort(UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetShort(UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetShort(RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetShort(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetShort(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stWord:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZLongWordArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZIntegerArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZInt64Array[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(ZUInt64Array[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case InParamValues[i].VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString, {$ENDIF}
                      {$IFNDEF NO_ANSISTRING} vtAnsiString, {$ENDIF}
                      {$IFNDEF NO_UTF8STRING} vtUTF8String, {$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetWord(RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE} vtString, {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetWord(UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetWord(UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetWord(RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetWord(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetWord(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stSmall:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZLongWordArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZIntegerArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZInt64Array[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(ZUInt64Array[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetSmall(RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetSmall(UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetSmall(UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetSmall(RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetSmall(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSmall(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stLongWord:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZLongWordArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZIntegerArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZInt64Array[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(ZUInt64Array[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetCardinal(RawToUInt64Def(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetCardinal(UnicodeToUInt64Def(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetCardinal(UnicodeToUInt64Def(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetCardinal(RawToUInt64Def(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetCardinal(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetCardinal(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stInteger:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZLongWordArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZIntegerArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZInt64Array[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(ZUInt64Array[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetInteger(RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetInteger(UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetInteger(UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetInteger(RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetInteger(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInteger(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stULong:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZLongWordArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZIntegerArray[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZUInt64Array[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(ZInt64Array[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetUInt64(RawToUInt64Def(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetUInt64(UnicodeToUInt64Def(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetUInt64(UnicodeToUInt64Def(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetUInt64(RawToUInt64Def(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetUInt64(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:              for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUInt64(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stLong:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZLongWordArray[J+fArrayOffSet]);
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZUInt64Array[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZInt64Array[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(ZIntegerArray[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Trunc(ZSingleArray[J+fArrayOffSet]));
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Trunc(ZDoubleArray[J+fArrayOffSet]));
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Trunc(ZCurrencyArray[J+fArrayOffSet]));
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Trunc(ZExtendedArray[J+fArrayOffSet]));
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetUInt64(RawToInt64Def(ZRawByteStringArray[J+fArrayOffSet], 0));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then SetUInt64(UnicodeToInt64Def(ZUnicodeStringArray[J+fArrayOffSet], 0));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            SetInt64(UnicodeToInt64Def(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0))
                                          else
                                            SetInt64(RawToInt64Def(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then SetInt64(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:              for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetInt64(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stFloat:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZLongWordArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZSingleArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZIntegerArray[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(PAnsiChar(Pointer(ZRawByteStringArray[J+fArrayOffSet])), Length(ZRawByteStringArray[J+fArrayOffSet]));
                      {$IFDEF UNICODE}vtString, {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(PWideChar(Pointer(ZUnicodeStringArray[J+fArrayOffSet])), Length(ZUnicodeStringArray[J+fArrayOffSet]));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            if IsNotNull then SetSingle(PWideChar(ZCharRecArray[J+fArrayOffSet].P), ZCharRecArray[J+fArrayOffSet].Len)
                                          else
                                            if IsNotNull then SetSingle(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P), ZCharRecArray[J+fArrayOffSet].Len);
                      vtNull:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:              for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetSingle(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stDouble, stCurrency, stBigDecimal:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(Ord(ZBooleanArray[J+fArrayOffSet]));
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZByteArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZShortIntArray[J+fArrayOffSet]);
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZWordArray[J+fArrayOffSet]);
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZSmallIntArray[J+fArrayOffSet]);
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZLongWordArray[J+fArrayOffSet]);
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZDoubleArray[J+fArrayOffSet]);
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZIntegerArray[J+fArrayOffSet]);
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE} vtString,{$ENDIF}
                      {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(PAnsiChar(Pointer(ZRawByteStringArray[J+fArrayOffSet])), Length(ZRawByteStringArray[J+fArrayOffSet]));
                      {$IFDEF UNICODE}vtString,{$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(PWidechar(Pointer(ZUnicodeStringArray[J+fArrayOffSet])), Length(ZUnicodeStringArray[J+fArrayOffSet]));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            if IsNotNull then SetDouble(PWideChar(ZCharRecArray[J+fArrayOffSet].P), ZCharRecArray[J+fArrayOffSet].Len)
                                          else
                                            if IsNotNull then SetDouble(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P), ZCharRecArray[J+fArrayOffSet].Len);
                      vtNull:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(0);
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(Ord(ZBytesArray[J+fArrayOffSet] = nil));
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(Trunc(ZDateTimeArray[J+fArrayOffSet]));
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDouble(Ord(ZInterfaceArray[J+fArrayOffSet] = nil));
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stGUID:
                  case TZSQLType(Value.VArray.VArrayType) of
                    stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                      Assert(Length(ZBytesArray[J+fArrayOffSet])=16, 'Wrong byte Array size for GUID column');
                                      SetBinary(Pointer(ZBytesArray[J+fArrayOffSet]), 16);
                                    end;
                    stGUID:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetBinary(@ZGUIDArray[J+fArrayOffSet].D1, 16);
                    stString, stUnicodeString: begin
                        case InParamValues[i].VArray.VArrayVariantType of
                          {$IFNDEF UNICODE} vtString,{$ENDIF}
                          {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                          {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                          vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                              GUID := StringToGUID({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(ZRawByteStringArray[J+fArrayOffSet]));
                                              SetBinary(@GUID.D1, 16);
                                            end;
                          {$IFDEF UNICODE}
                          vtString,
                          {$ENDIF}
                          vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                              GUID := StringToGUID({$IFNDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(ZUnicodeStringArray[J+fArrayOffSet]));
                                              SetBinary(@GUID.D1, 16);
                                            end;
                          vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                              GUID := StringToGUID(ClientVarManager.GetAsString(EncodeCharRec(ZCharRecArray[J+fArrayOffSet])));
                                              SetBinary(@GUID.D1, 16);
                                            end;
                          else
                            raise Exception.Create('Unsupported String Variant');
                        end;
                      end;
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                  end;
              stBytes:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                    TempBlob := ZInterfaceArray[J+fArrayOffSet] as IZBLob;
                                    SetBinary(TempBlob.GetBuffer, Min(TempBlob.Length, Param.BufferSize));
                                  end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetBinary(Pointer(ZBytesArray[J+fArrayOffSet]), Min(Length(ZBytesArray[J+fArrayOffSet]), Param.BufferSize));
                  stGUID:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetBinary(@ZGUIDArray[J+fArrayOffSet].D1, Min(16, Param.BufferSize));
                  stString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        {$IFNDEF UNICODE} vtString,{$ENDIF}
                        {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                        {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetBinary(Pointer(ZRawByteStringArray[J+fArrayOffSet]), Min(Length(ZRawByteStringArray[J+fArrayOffSet]), Param.BufferSize));
                        {$IFDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                            RawTemp := UnicodeStringToASCII7(ZUnicodeStringArray[J+fArrayOffSet]);
                                            SetBinary(Pointer(RawTemp), Min(Length(RawTemp), Param.BufferSize));
                                          end;
                        vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then begin
                                            RawTemp := UnicodeStringToASCII7(ZCharRecArray[J+fArrayOffSet].P, Min(ZCharRecArray[J+fArrayOffSet].Len,Param.BufferSize));
                                            SetBinary(Pointer(RawTemp), Length(RawTemp));
                                          end else
                                            SetBinary(ZCharRecArray[J+fArrayOffSet].P, Min(ZCharRecArray[J+fArrayOffSet].Len, Param.BufferSize));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    else
                      raise Exception.Create('Unsupported Byte-Array Variant');
                  end;
              stDate:
                begin
                  case TZSQLType(Value.VArray.VArrayType) of
                    stByte:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZByteArray[J+fArrayOffSet]);
                    stShort:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZShortIntArray[J+fArrayOffSet]);
                    stWord:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZWordArray[J+fArrayOffSet]);
                    stSmall:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZSmallIntArray[J+fArrayOffSet]);
                    stLongWord:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZLongWordArray[J+fArrayOffSet]);
                    stInteger:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZIntegerArray[J+fArrayOffSet]);
                    stLong:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZInt64Array[J+fArrayOffSet]);
                    stULong:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZUInt64Array[J+fArrayOffSet]);
                    stFloat:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZSingleArray[J+fArrayOffSet]);
                    stDouble:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZDoubleArray[J+fArrayOffSet]);
                    stCurrency:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZCurrencyArray[J+fArrayOffSet]);
                    stBigDecimal: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZExtendedArray[J+fArrayOffSet]);
                    stString, stUnicodeString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        {$IFNDEF UNICODE} vtString,{$ENDIF}
                        {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                        {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[J+fArrayOffSet])));
                        {$IFDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[J+fArrayOffSet])));
                        vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[J+fArrayOffSet])));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    stTime,
                    stDate,
                    stTimeStamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then SetDate(ZDateTimeArray[J+fArrayOffSet]);
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                  end;
                end;
              stTime:
                begin
                  case TZSQLType(Value.VArray.VArrayType) of
                    stByte:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZByteArray[J+fArrayOffSet]);
                    stShort:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZShortIntArray[J+fArrayOffSet]);
                    stWord:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZWordArray[J+fArrayOffSet]);
                    stSmall:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZSmallIntArray[J+fArrayOffSet]);
                    stLongWord:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZLongWordArray[J+fArrayOffSet]);
                    stInteger:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZIntegerArray[J+fArrayOffSet]);
                    stLong:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZInt64Array[J+fArrayOffSet]);
                    stULong:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZUInt64Array[J+fArrayOffSet]);
                    stFloat:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZSingleArray[J+fArrayOffSet]);
                    stDouble:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZDoubleArray[J+fArrayOffSet]);
                    stCurrency:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZCurrencyArray[J+fArrayOffSet]);
                    stBigDecimal: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZExtendedArray[J+fArrayOffSet]);
                    stString, stUnicodeString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        {$IFNDEF UNICODE} vtString,{$ENDIF}
                        {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                        {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[J+fArrayOffSet])));
                        {$IFDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[J+fArrayOffSet])));
                        vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[J+fArrayOffSet])));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    stTime,
                    stDate,
                    stTimeStamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTime(ZDateTimeArray[J+fArrayOffSet]);
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                  end;
                end;
              stTimeStamp:
                begin
                  case TZSQLType(Value.VArray.VArrayType) of
                    stByte:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZByteArray[J+fArrayOffSet]);
                    stShort:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZShortIntArray[J+fArrayOffSet]);
                    stWord:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZWordArray[J+fArrayOffSet]);
                    stSmall:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZSmallIntArray[J+fArrayOffSet]);
                    stLongWord:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZLongWordArray[J+fArrayOffSet]);
                    stInteger:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZIntegerArray[J+fArrayOffSet]);
                    stLong:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZInt64Array[J+fArrayOffSet]);
                    stULong:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZUInt64Array[J+fArrayOffSet]);
                    stFloat:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZSingleArray[J+fArrayOffSet]);
                    stDouble:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZDoubleArray[J+fArrayOffSet]);
                    stCurrency:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZCurrencyArray[J+fArrayOffSet]);
                    stBigDecimal: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZExtendedArray[J+fArrayOffSet]);
                    stString, stUnicodeString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        {$IFNDEF UNICODE} vtString,{$ENDIF}
                        {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
                        {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ClientVarManager.GetAsDateTime(EncodeRawByteString(ZRawByteStringArray[J+fArrayOffSet])));
                        {$IFDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ClientVarManager.GetAsDateTime(EncodeUnicodeString(ZUnicodeStringArray[J+fArrayOffSet])));
                        vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ClientVarManager.GetAsDateTime(EncodeCharRec(ZCharRecArray[J+fArrayOffSet])));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    stTime,
                    stDate,
                    stTimeStamp:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetTimeStamp(ZDateTimeArray[J+fArrayOffSet]);
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                  end;
                end;
              stString, stUnicodeString:
                if ConSettings^.ClientCodePage.Encoding = ceUTF16 then
                  case TZSQLType(Value.VArray.VArrayType) of
                    stBoolean:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(BoolToUnicodeEx(ZBooleanArray[J+fArrayOffSet]));
                    stByte:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZByteArray[J+fArrayOffSet]));
                    stShort:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZShortIntArray[J+fArrayOffSet]));
                    stWord:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZWordArray[J+fArrayOffSet]));
                    stSmall:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZSmallIntArray[J+fArrayOffSet]));
                    stLongWord:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZLongWordArray[J+fArrayOffSet]));
                    stInteger:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZIntegerArray[J+fArrayOffSet]));
                    stULong:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZUInt64Array[J+fArrayOffSet]));
                    stLong:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(IntToUnicode(ZInt64Array[J+fArrayOffSet]));
                    stFloat:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(FloatToUnicode(ZSingleArray[J+fArrayOffSet]));
                    stDouble:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(FloatToUnicode(ZDoubleArray[J+fArrayOffSet]));
                    stCurrency:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(FloatToUnicode(ZCurrencyArray[J+fArrayOffSet]));
                    stBigDecimal: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(FloatToUnicode(ZExtendedArray[J+fArrayOffSet]));
                    stTime:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(DateTimeToUnicodeSQLTime(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stDate:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(DateTimeToUnicodeSQLDate(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stTimeStamp:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(DateTimeToUnicodeSQLTimeStamp(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stString, stUnicodeString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        vtString:
                          {$IFDEF UNICODE}
                          for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(ZStringArray[J+fArrayOffSet]);
                          {$ELSE}
                          for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(ConSettings^.ConvFuncs.ZStringToUnicode(ZStringArray[J+fArrayOffSet], ConSettings^.CTRL_CP));
                          {$ENDIF}
                        {$IFNDEF NO_ANSISTRING}
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(PRawToUnicode(Pointer(ZAnsiStringArray[J+fArrayOffSet]), Length(ZAnsiStringArray[J+fArrayOffSet]), ZOSCodePage));
                        {$ENDIF}
                        {$IFNDEF NO_UTF8STRING}
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if IsNotNull then
                          if Assigned(Pointer(ZUTF8StringArray[J+fArrayOffSet])) and (Length(ZUTF8StringArray[J+fArrayOffSet]) <= Param.ColumnSize) then begin
                            PSQLLEN(StrLen_or_IndPtr)^ := UTF8ToWideChar(Pointer(ZUTF8StringArray[J+fArrayOffSet]), Length(ZUTF8StringArray[J+fArrayOffSet]), ParameterDataPtr) shl 1;
                            (PWideChar(ParameterDataPtr)+(PSQLLEN(StrLen_or_IndPtr)^ shr 1))^ := #0;
                            Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
                            Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
                          end else
                            SetUnicode(PRawToUnicode(Pointer(ZUTF8StringArray[J+fArrayOffSet]), Length(ZUTF8StringArray[J+fArrayOffSet]), zCP_UTF8));
                        {$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(ConSettings.ConvFuncs.ZRawToUnicode(ZRawByteStringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(ZUnicodeStringArray[J+fArrayOffSet]);
                        vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                            SetPUnicode(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len)
                          else
                            SetUnicode(PRawToUnicode(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len, ZCharRecArray[J+fArrayOffSet].CP));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    stAsciiStream,
                    stUnicodeStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                        TempBlob := ZInterfaceArray[J+fArrayOffSet] as IZBLob;
                        if TempBlob.IsClob then begin
                          TempBlob.GetPWideChar; //make conversion first
                          SetPUnicode(TempBlob.GetPWideChar, TempBlob.Length shr 1);
                        end else begin
                          TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                          SetPUnicode(TMemoryStream(TmpStream).Memory, TmpStream.Size shr 1);
                          TmpStream.Free;
                        end;
                      end;
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                  end
                else
                  case TZSQLType(Value.VArray.VArrayType) of
                    stBoolean:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(BoolToRawEx(ZBooleanArray[J+fArrayOffSet]));
                    stByte:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZByteArray[J+fArrayOffSet]));
                    stShort:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZShortIntArray[J+fArrayOffSet]));
                    stWord:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZWordArray[J+fArrayOffSet]));
                    stSmall:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZSmallIntArray[J+fArrayOffSet]));
                    stLongWord:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZLongWordArray[J+fArrayOffSet]));
                    stInteger:    for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZIntegerArray[J+fArrayOffSet]));
                    stULong:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZUInt64Array[J+fArrayOffSet]));
                    stLong:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(IntToRaw(ZInt64Array[J+fArrayOffSet]));
                    stFloat:      for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(FloatToRaw(ZSingleArray[J+fArrayOffSet]));
                    stDouble:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(FloatToRaw(ZDoubleArray[J+fArrayOffSet]));
                    stCurrency:   for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(FloatToRaw(ZCurrencyArray[J+fArrayOffSet]));
                    stBigDecimal: for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(FloatToRaw(ZExtendedArray[J+fArrayOffSet]));
                    stTime:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(DateTimeToRawSQLTime(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stDate:       for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(DateTimeToRawSQLDate(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stTimeStamp:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(DateTimeToRawSQLTimeStamp(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stString, stUnicodeString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        vtString:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[J+fArrayOffSet], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
                        {$IFNDEF NO_ANSISTRING}
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[J+fArrayOffSet], ZOSCodePage, ConSettings^.ClientCodePage^.CP));
                        {$ENDIF}
                        {$IFNDEF NO_UTF8STRING}
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(ConSettings^.ConvFuncs.ZUTF8ToRaw(ZUTF8StringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
                        {$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(ZRawByteStringArray[J+fArrayOffSet]);
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(ConSettings^.ConvFuncs.ZUnicodeToRaw(ZUnicodeStringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
                        vtCharRec: for J := 0 to fCurrentIterations -1 do if IsNotNull then
                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, ConSettings^.ClientCodePage^.CP) then
                            SetPRaw(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len)
                          else if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                            SetRaw(PUnicodeToRaw(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len, ConSettings^.ClientCodePage^.CP))
                          else
                            SetRaw(ConSettings^.ConvFuncs.ZUnicodeToRaw(PRawToUnicode(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len, ZCharRecArray[J+fArrayOffSet].CP), ConSettings^.ClientCodePage^.CP));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    stAsciiStream,
                    stUnicodeStream:  for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                        TempBlob := ZInterfaceArray[J+fArrayOffSet] as IZBLob;
                        if TempBlob.IsClob then begin
                          TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP); //make internal conversion first
                          SetPRaw(TempBlob.GetBuffer,TempBlob.Length);
                        end else
                          SetRaw(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings));
                      end;
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                end;
              stAsciiStream, stUnicodeStream: begin
                Param.CurrParamDataPtr := Pointer(Param.DescBindBuffer); //little trick for get data
                if (not (TZSQLType(Value.VArray.VArrayType) in [stAsciiStream, stUnicodeStream])) then begin
                  Inc(TmpLobIndex);
                  SetLength(FBatchLobBuf[TmpLobIndex], Min(fCurrentIterations, fMaxBufArrayBound)); //minimal length
                  PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(Param.CurrParamDataPtr)^:= @FBatchLobBuf[TmpLobIndex][fArrayOffSet];
                end else begin
                  PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(Param.CurrParamDataPtr)^:= @TInterfaceDynArray(Value.VArray.VArray)[fArrayOffSet];
                end;
                //if Param IO is out we need to know which kind of IZLob need to load the data
                PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobParameterIndexOffSet))^ := I;
                if ConSettings^.ClientCodePage.Encoding = ceUTF16 then
                  case TZSQLType(Value.VArray.VArrayType) of
                    stBoolean:    for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(BoolToUnicodeEx(ZBooleanArray[J+fArrayOffSet]));
                    stByte:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZByteArray[J+fArrayOffSet]));
                    stShort:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZShortIntArray[J+fArrayOffSet]));
                    stWord:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZWordArray[J+fArrayOffSet]));
                    stSmall:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZSmallIntArray[J+fArrayOffSet]));
                    stLongWord:   for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZLongWordArray[J+fArrayOffSet]));
                    stInteger:    for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZIntegerArray[J+fArrayOffSet]));
                    stULong:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZUInt64Array[J+fArrayOffSet]));
                    stLong:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(IntToUnicode(ZInt64Array[J+fArrayOffSet]));
                    stFloat:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(FloatToUnicode(ZSingleArray[J+fArrayOffSet]));
                    stDouble:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(FloatToUnicode(ZDoubleArray[J+fArrayOffSet]));
                    stCurrency:   for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(FloatToUnicode(ZCurrencyArray[J+fArrayOffSet]));
                    stBigDecimal: for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(FloatToUnicode(ZExtendedArray[J+fArrayOffSet]));
                    stTime:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(DateTimeToUnicodeSQLTime(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stDate:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(DateTimeToUnicodeSQLDate(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stTimeStamp:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(DateTimeToUnicodeSQLTimeStamp(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stString, stUnicodeString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        vtString:
                          {$IFDEF UNICODE}
                          for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(ZStringArray[J+fArrayOffSet]);
                          {$ELSE}
                          for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(ConSettings^.ConvFuncs.ZStringToUnicode(ZStringArray[J+fArrayOffSet], ConSettings^.CTRL_CP));
                          {$ENDIF}
                        {$IFNDEF NO_ANSISTRING}
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(PRawToUnicode(Pointer(ZAnsiStringArray[J+fArrayOffSet]), Length(ZAnsiStringArray[J+fArrayOffSet]), ZOSCodePage));
                        {$ENDIF}
                        {$IFNDEF NO_UTF8STRING}
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(PRawToUnicode(Pointer(ZUTF8StringArray[J+fArrayOffSet]), Length(ZUTF8StringArray[J+fArrayOffSet]), zCP_UTF8));
                        {$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(ConSettings.ConvFuncs.ZRawToUnicode(ZRawByteStringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(ZUnicodeStringArray[J+fArrayOffSet]);
                        vtCharRec:        for J := 0 to fCurrentIterations -1 do if LobIsNotNull then
                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                            SetCLobFromPUnicode(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len)
                          else
                            SetCLobFromUnicode(PRawToUnicode(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len, ZCharRecArray[J+fArrayOffSet].CP));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    stAsciiStream,
                    stUnicodeStream: for J := 0 to fCurrentIterations -1 do if LobIsNotNull then begin
                        TempBlob := ZInterfaceArray[J+fArrayOffSet] as IZBLob;
                        if TempBlob.IsClob then begin
                          TempBlob.GetPWideChar; //make conversion first
                          Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
                        end else begin
                          TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                          SetCLobFromPUnicode(TMemoryStream(TmpStream).Memory, TmpStream.Size shr 1); //replaces ZInterfaceArray[J+fArrayOffSet]
                          TmpStream.Free;
                        end;
                      end;
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                  end
                else
                  case TZSQLType(Value.VArray.VArrayType) of
                    stBoolean:    for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(BoolToRawEx(ZBooleanArray[J+fArrayOffSet]));
                    stByte:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZByteArray[J+fArrayOffSet]));
                    stShort:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZShortIntArray[J+fArrayOffSet]));
                    stWord:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZWordArray[J+fArrayOffSet]));
                    stSmall:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZSmallIntArray[J+fArrayOffSet]));
                    stLongWord:   for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZLongWordArray[J+fArrayOffSet]));
                    stInteger:    for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZIntegerArray[J+fArrayOffSet]));
                    stULong:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZUInt64Array[J+fArrayOffSet]));
                    stLong:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(IntToRaw(ZInt64Array[J+fArrayOffSet]));
                    stFloat:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(FloatToRaw(ZSingleArray[J+fArrayOffSet]));
                    stDouble:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(FloatToRaw(ZDoubleArray[J+fArrayOffSet]));
                    stCurrency:   for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(FloatToRaw(ZCurrencyArray[J+fArrayOffSet]));
                    stBigDecimal: for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(FloatToRaw(ZExtendedArray[J+fArrayOffSet]));
                    stTime:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(DateTimeToRawSQLTime(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stDate:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(DateTimeToRawSQLDate(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stTimeStamp:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(DateTimeToRawSQLTimeStamp(ZDateTimeArray[J+fArrayOffSet], ConSettings.WriteFormatSettings, False));
                    stString, stUnicodeString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        vtString:         for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[J+fArrayOffSet], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
                        {$IFNDEF NO_ANSISTRING}
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[J+fArrayOffSet], ZOSCodePage, ConSettings^.ClientCodePage^.CP));
                        {$ENDIF}
                        {$IFNDEF NO_UTF8STRING}
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(ConSettings^.ConvFuncs.ZUTF8ToRaw(ZUTF8StringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
                        {$ENDIF}
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(ZRawByteStringArray[J+fArrayOffSet]);
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(ConSettings^.ConvFuncs.ZUnicodeToRaw(ZUnicodeStringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
                        vtCharRec: for J := 0 to fCurrentIterations -1 do if LobIsNotNull then
                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, ConSettings^.ClientCodePage^.CP) then
                            SetClobFromPRaw(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len)
                          else if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                            SetClobFromRaw(PUnicodeToRaw(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len, ConSettings^.ClientCodePage^.CP))
                          else
                            SetClobFromRaw(ConSettings^.ConvFuncs.ZUnicodeToRaw(PRawToUnicode(ZCharRecArray[J+fArrayOffSet].P, ZCharRecArray[J+fArrayOffSet].Len, ZCharRecArray[J+fArrayOffSet].CP), ConSettings^.ClientCodePage^.CP));
                        else
                          raise Exception.Create('Unsupported String Variant');
                      end;
                    stAsciiStream,
                    stUnicodeStream:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then begin
                        TempBlob := ZInterfaceArray[J+fArrayOffSet] as IZBLob;
                        if TempBlob.IsClob then begin
                          TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP); //make internal conversion first
                          Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
                        end else
                          SetClobFromRaw(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings));
                      end;
                    else
                      raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                  end;
                end;
              stBinaryStream: begin
                Param.CurrParamDataPtr := Pointer(Param.DescBindBuffer); //little trick for get data
                if (TZSQLType(Value.VArray.VArrayType) <> stBinaryStream) then begin
                  Inc(TmpLobIndex);
                  SetLength(FBatchLobBuf[TmpLobIndex], Min(fCurrentIterations, fMaxBufArrayBound)); //minimal length
                  PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(Param.CurrParamDataPtr)^:= @FBatchLobBuf[TmpLobIndex][fArrayOffSet];
                end else begin
                  PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(Param.CurrParamDataPtr)^:= @TInterfaceDynArray(Value.VArray.VArray)[fArrayOffSet];
                end;
                PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobParameterIndexOffSet))^ := I;
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:    for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZBooleanArray[J+fArrayOffSet], SizeOf(Boolean));
                  stByte:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZByteArray[J+fArrayOffSet], 1);
                  stShort:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZShortIntArray[J+fArrayOffSet], 1);
                  stWord:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZWordArray[J+fArrayOffSet], 2);
                  stSmall:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZSmallIntArray[J+fArrayOffSet], 2);
                  stLongWord:   for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZLongWordArray[J+fArrayOffSet], 4);
                  stInteger:    for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZIntegerArray[J+fArrayOffSet], 4);
                  stULong:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZUInt64Array[J+fArrayOffSet], 8);
                  stLong:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZInt64Array[J+fArrayOffSet], 8);
                  stFloat:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZSingleArray[J+fArrayOffSet], 4);
                  stDouble:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZDoubleArray[J+fArrayOffSet], 8);
                  stCurrency:   for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZCurrencyArray[J+fArrayOffSet], 8);
                  stBigDecimal: for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZExtendedArray[J+fArrayOffSet], 8);
                  stTime,
                  stDate,
                  stTimeStamp:  for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZDateTimeArray[J+fArrayOffSet], 8);
                  stGUID:       for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(@ZGUIDArray[J+fArrayOffSet].D1, 16);
                  stBytes:      for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetBlob(Pointer(ZBytesArray[J+fArrayOffSet]), Length(ZBytesArray[J+fArrayOffSet]));
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if LobIsNotNull then Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
                  else
                    raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                end
              end;
            end; //for j := 0 to CurrArrayCount -1 do begin
          end; //if (TZSQLType(Value.VArray.VIsNullArrayType) = fSQLTypes[
        end else begin
          for J := 0 to fCurrentIterations -1 do begin
            PSQLLEN(StrLen_or_IndPtr)^ := SQL_NULL_DATA;
            Inc(PAnsiChar(ParameterDataPtr), ParameterDataOffSet);
            Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
          end;
        end;
        if (fBindRowWise) then begin
          { enter new param in 1. row }
          StrLen_or_IndPtr := {%H-}Pointer({%H-}NativeUInt(Param.CurrStrLen_or_IndPtr)+Cardinal(Param.BufferSize)+SizeOf(SQLLEN));
          ParameterDataPtr := {%H-}Pointer({%H-}NativeUInt(StrLen_or_IndPtr)+SizeOf(SQLLEN));
        end else begin
          { enter new param array }
          StrLen_or_IndPtr := ParameterDataPtr;
          Inc(PAnsiChar(ParameterDataPtr), fCurrentIterations*SizeOf(SQLLEN))
        end;
      end else begin //single value binding
        Param.CurrParamDataPtr := ParameterDataPtr;
        if Param.ValueCount <> 1 then begin
          Param.ValueCount := 1;
          ParamSetChanged := True;
        end;
        if Value.VType = vtNull then begin
          PSQLLEN(StrLen_or_IndPtr)^ := SQL_NULL_DATA;
          if Param.SQLType in [stDate, stTimeStamp] then begin//year 0.0.0 is invalid also for NULL
            PSQL_DATE_STRUCT(ParameterDataPtr)^.year := 1899;
            PSQL_DATE_STRUCT(ParameterDataPtr)^.month := 12;
            PSQL_DATE_STRUCT(ParameterDataPtr)^.day := 31;
          end;

          Inc(PAnsiChar(ParameterDataPtr), Param.BufferSize);
        end else begin
          PSQLLEN(StrLen_or_IndPtr)^ := SQL_NO_NULLS;
          case Param.SQLType of
            stBoolean:    SetByte(Ord(ClientVarManager.GetAsBoolean(Value)));
            stByte:       SetByte(ClientVarManager.GetAsUInteger(Value));
            stShort:      SetShort(ClientVarManager.GetAsInteger(Value));
            stWord:       SetWord(ClientVarManager.GetAsUInteger(Value));
            stSmall:      SetSmall(ClientVarManager.GetAsInteger(Value));
            stLongWord:   SetCardinal(ClientVarManager.GetAsUInteger(Value));
            stInteger:    SetInteger(ClientVarManager.GetAsInteger(Value));
            stULong:      SetUInt64(ClientVarManager.GetAsUInteger(Value));
            stLong:       SetInt64(ClientVarManager.GetAsInteger(Value));
            stFloat:      SetSingle(ClientVarManager.GetAsFloat(Value));
            stDouble,
            stCurrency,
            stBigDecimal: SetDouble(ClientVarManager.GetAsFloat(Value));
            stString, stUnicodeString: begin
                if Param.C_DataType = SQL_C_WCHAR then
                begin
                  CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], zCP_UTF16);
                  PSQLLEN(StrLen_or_IndPtr)^ := Min(Param.ColumnSize shl 1, CharRec.Len shl 1);
                  if Param.InputDataType = SQL_PARAM_INPUT then
                    Param.CurrParamDataPtr := CharRec.P
                  else
                    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(CharRec.P^, ParameterDataPtr^, PSQLLEN(StrLen_or_IndPtr)^);
                  if not ((PWideChar(Param.CurrParamDataPtr)+(PSQLLEN(StrLen_or_IndPtr)^ shr 1))^ = WideChar(#0)) then //WideChar use for FPC else dead slow
                    (PWideChar(Param.CurrParamDataPtr)+(PSQLLEN(StrLen_or_IndPtr)^ shr 1))^ := WideChar(#0); //set a terminating #0 to top of data
                end else begin
                  CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], ConSettings^.ClientCodePage^.CP);
                  PSQLLEN(StrLen_or_IndPtr)^ := {%H-}Min((Param.ColumnSize*ConSettings^.ClientCodePage^.CharWidth), CharRec.Len);
                  if Param.InputDataType = SQL_PARAM_INPUT then
                    Param.CurrParamDataPtr := CharRec.P
                  else
                    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(CharRec.P^, ParameterDataPtr^, PSQLLEN(StrLen_or_IndPtr)^);
                  if not (PByte(PAnsiChar(Param.CurrParamDataPtr)+PSQLLEN(StrLen_or_IndPtr)^)^ = Ord(#0)) then
                    PByte(PAnsiChar(Param.CurrParamDataPtr)+PSQLLEN(StrLen_or_IndPtr)^)^ := Ord(#0); //terminate the String if a truncation happens
                end;
                Inc(PAnsiChar(ParameterDataPtr), Param.BufferSize);
              end;
            stBytes, stGUID: begin
                if Value.VType <> vtBytes then begin
                  Value := EncodeBytes(ClientVarManager.GetAsBytes(Value));
                  InParamValues[i] := Value;
                end;
                if Param.InputDataType = SQL_PARAM_INPUT then begin
                  Param.CurrParamDataPtr := Pointer(InParamValues[i].VBytes);
                  PSQLLEN(StrLen_or_IndPtr)^ := Min(Length(InParamValues[i].VBytes), Param.ColumnSize);
                  Inc(PAnsiChar(ParameterDataPtr), Param.ColumnSize);
                end else
                  SetBinary(Value.VBytes, Min(Length(Value.VBytes), Param.ColumnSize));
              end;
            stDate: SetDate(ClientVarManager.GetAsDateTime(Value));
            stTime: SetTime(ClientVarManager.GetAsDateTime(Value));
            stTimestamp: SetTimeStamp(ClientVarManager.GetAsDateTime(Value));
            //stArray, stDataSet,
            stAsciiStream, stUnicodeStream: begin
              Param.CurrParamDataPtr := Pointer(Param.DescBindBuffer); //little trick for get data
              PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobArrayIndexOffSet))^ := 0;//initialize a virtual index
              PSQLLEN(StrLen_or_IndPtr)^ := SQL_DATA_AT_EXEC; //indicate streamed mode
              if (not (InParamTypes[i] in [stAsciiStream, stUnicodeStream])) then begin
                Inc(TmpLobIndex);
                SetLength(FBatchLobBuf[TmpLobIndex], 1); //minimal length
                PPointer(Param.CurrParamDataPtr)^:= @FBatchLobBuf[TmpLobIndex][0];
                J := 0;
              end else
                PPointer(Param.CurrParamDataPtr)^:= @InParamValues[I].VInterface;
              PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobParameterIndexOffSet))^ := I;
              if Param.C_DataType = SQL_C_WCHAR then
                case InParamTypes[i] of
                  stBoolean:    SetCLobFromUnicode(BoolToUnicodeEx(ClientVarManager.GetAsBoolean(Value)));
                  stByte, stWord, stLongWord, stULong:
                    SetCLobFromUnicode(IntToUnicode(ClientVarManager.GetAsInteger(Value)));
                  stShort, stSmall, stInteger, stLong:
                    SetCLobFromUnicode(IntToUnicode(ClientVarManager.GetAsUInteger(Value)));
                  stFloat, stDouble, stCurrency, stBigDecimal:
                    SetCLobFromUnicode(FloatToUnicode(ClientVarManager.GetAsFloat(Value)));
                  stTime:       SetCLobFromUnicode(DateTimeToUnicodeSQLTime(ClientVarManager.GetAsDateTime(Value), ConSettings.WriteFormatSettings, False));
                  stDate:       SetCLobFromUnicode(DateTimeToUnicodeSQLDate(ClientVarManager.GetAsDateTime(Value), ConSettings.WriteFormatSettings, False));
                  stTimeStamp:  SetCLobFromUnicode(DateTimeToUnicodeSQLTimeStamp(ClientVarManager.GetAsDateTime(Value), ConSettings.WriteFormatSettings, False));
                  //stBytes, stGUID:
                  stString, stUnicodeString:
                    begin
                      CharRec := ClientVarManager.GetAsCharRec(Value, zCP_UTF16);
                      SetCLobFromPUnicode(CharRec.P, CharRec.Len);
                    end;
                  stAsciiStream,
                  stUnicodeStream: begin
                      TempBlob := ClientVarManager.GetAsInterface(Value) as IZBLob;
                      if TempBlob.IsClob then
                        TempBlob.GetPWideChar //make conversion first
                      else begin
                        TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                        InParamValues[i].VInterface := TZODBCClobW.CreateWithData(PWideChar(TMemoryStream(TmpStream).Memory), TmpStream.Size shr 1, ConSettings);
                        TmpStream.Free;
                      end;
                    end;
                  else
                    raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                end
              else
                case InParamTypes[i] of
                  stBoolean:    SetCLobFromRaw(BoolToRawEx(ClientVarManager.GetAsBoolean(Value)));
                  stByte, stWord, stLongWord, stULong:
                    SetCLobFromRaw(IntToRaw(ClientVarManager.GetAsInteger(Value)));
                  stShort, stSmall, stInteger, stLong:
                    SetCLobFromRaw(IntToRaw(ClientVarManager.GetAsUInteger(Value)));
                  stFloat, stDouble, stCurrency, stBigDecimal:
                    SetCLobFromRaw(FloatToRaw(ClientVarManager.GetAsFloat(Value)));
                  stTime:       SetCLobFromRaw(DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(Value), ConSettings.WriteFormatSettings, False));
                  stDate:       SetCLobFromRaw(DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(Value), ConSettings.WriteFormatSettings, False));
                  stTimeStamp:  SetCLobFromRaw(DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(Value), ConSettings.WriteFormatSettings, False));
                  stString, stUnicodeString:
                    begin
                      CharRec := ClientVarManager.GetAsCharRec(Value, ConSettings^.ClientCodePage^.CP);
                      SetClobFromPRaw(CharRec.P, CharRec.Len)
                    end;
                  stAsciiStream,
                  stUnicodeStream:  begin
                      TempBlob := ClientVarManager.GetAsInterface(Value) as IZBLob;
                      if TempBlob.IsClob then
                        TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP) //make internal conversion first
                      else begin
                        RawTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                        InParamValues[i].VInterface := TZODBCClobA.CreateWithData(Pointer(RawTemp), Length(RawTemp), ConSettings^.ClientCodePage^.CP, ConSettings);
                      end;
                    end;
                  else
                    raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
                end;
            end;
          stBinaryStream: begin
              Param.CurrParamDataPtr := Pointer(Param.DescBindBuffer); //little trick for get data
              PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobArrayIndexOffSet))^ := 0;//initialize a virtual index
              PSQLLEN(StrLen_or_IndPtr)^ := SQL_DATA_AT_EXEC; //indicate streamed mode
              //Inc(PAnsiChar(StrLen_or_IndPtr), StrLen_or_IndOffSet);
              if (InParamTypes[i] <> stBinaryStream) then begin
                Inc(TmpLobIndex);
                SetLength(FBatchLobBuf[TmpLobIndex], 1); //minimal length
                PPointer(Param.CurrParamDataPtr)^:= @FBatchLobBuf[TmpLobIndex][0];
                j:= 0;
              end else
                PPointer(Param.CurrParamDataPtr)^ := @InParamValues[I].VInterface;
              PInteger({%H-}Pointer({%H-}NativeUInt(Param.CurrParamDataPtr)+LobParameterIndexOffSet))^ := I;
              case InParamTypes[i] of
                stGUID, stBytes: SetBlobFromBts(ClientVarManager.GetAsBytes(Value));
                stBinaryStream: ;
                else
                  raise EZSQLException.Create(IntToStr(Ord(Param.SQLType))+' '+SUnsupportedParameterType);
              end;
            end;
          end
        end;
        StrLen_or_IndPtr := ParameterDataPtr;
        Inc(PAnsiChar(ParameterDataPtr), SizeOf(SQLLEN));
      end;
      if (Param.CurrParamDataPtr <> Param.LastParamDataPtr) or
         (Param.CurrStrLen_or_IndPtr <> Param.LastStrLen_or_IndPtr) then begin //..Bindings remain in effect until the application calls SQLBindParameter again...oslt.
        CheckStmtError(fPlainDriver.SQLBindParameter(fHSTMT, I+1,//0=bookmark and Params do starts with 1
          Param.InputDataType, Param.C_DataType, Param.DataType, Param.ColumnSize, Param.DecimalDigits * Ord(Param.SQLType in [stDouble, stTime, stTimeStamp]),
            Param.CurrParamDataPtr, Param.BufferSize, Param.CurrStrLen_or_IndPtr));
        Param.LastParamDataPtr := Param.CurrParamDataPtr;
        Param.LastStrLen_or_IndPtr := Param.CurrStrLen_or_IndPtr;
      end;
    end;
    if ParamSetChanged then
      if (fMaxBufArrayBound = -1)
      then CheckStmtError(fPlainDriver.SQLSetStmtAttr(fHSTMT, SQL_ATTR_PARAMSET_SIZE, Pointer(1), 0))
      else CheckStmtError(fPlainDriver.SQLSetStmtAttr(fHSTMT, SQL_ATTR_PARAMSET_SIZE, SQLPOINTER(NativeUInt(fCurrentIterations)), 0));
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZAbstractODBCStatement.InternalExecute;
var
  RETCODE, RETCODE2: SQLRETURN;
  ValuePtr: SQLPOINTER;
  TempBlob: IZBLob;
  Buf: PAnsiChar; //simple to increment by ide
  StrLen_or_Ind: SQLLEN;
  PRowIndex: PInteger;
  I: Integer;
begin
  CheckStmtError(fPlainDriver.SQLFreeStmt(fHSTMT,SQL_CLOSE)); //handle a get data issue
  RETCODE := fPlainDriver.SQLExecute(fHSTMT);
  while RETCODE = SQL_NEED_DATA do begin
    RETCODE := fPlainDriver.SQLParamData(fHSTMT, @ValuePtr);
    if RetCode <> SQL_NEED_DATA then break;
    Assert(Assigned(ValuePtr), 'wrong descriptor token');
    PRowIndex := {%H-}Pointer({%H-}NativeUInt(ValuePtr)+LobArrayIndexOffSet);
    {$R-}
    TempBlob := PLobArray(PPointer(ValuePtr)^)^[PRowIndex^] as IZBlob; //note ValuePtr is a user defined token we also could use the columnNumber on binding the column -> this is faster
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if (TempBlob = nil) or TempBlob.IsEmpty then begin
      CheckStmtError(fPlainDriver.SQLPutData(fHSTMT, nil, SQL_NULL_DATA)); //set to null
    end else begin
      Buf := TempBlob.GetBuffer;
      { put data chunked }
      StrLen_or_Ind := Min(ChunkSize, TempBlob.Length);
      for i := 1 to TempBlob.Length div ChunkSize do begin
        CheckStmtError(fPlainDriver.SQLPutData(fHSTMT, Buf, StrLen_or_Ind));
        Inc(Buf, ChunkSize);
      end;
      StrLen_or_Ind := TempBlob.Length - NativeInt(({%H-}NativeUInt(Buf)-{%H-}NativeUInt(TempBlob.GetBuffer)));
      CheckStmtError(fPlainDriver.SQLPutData(fHSTMT, Buf, StrLen_or_Ind)); //final chunk
    end;
    inc(PRowIndex^);
  end;
  { roll back PRowIndex^ misses yet}
  if RETCODE = SQL_PARAM_DATA_AVAILABLE then begin //check output params ...
    RETCODE2 := fPlainDriver.SQLMoreResults(fHSTMT);
    if RETCODE2 = SQL_NO_DATA then
      //???
    else begin
      { get data chunked }
      CheckStmtError(RETCODE2);
      CheckStmtError(fPlainDriver.SQLParamData(fHSTMT, @ValuePtr));
      Assert(Assigned(ValuePtr), 'wrong descriptor pointer');
      TempBlob := IZBlob(ValuePtr);
    end;
  end else if not RETCODE in [SQL_NO_DATA, SQL_SUCCESS] then
    CheckStmtError(RetCode);
end;

procedure TZAbstractODBCStatement.Prepare;
begin
  if not Prepared then begin
    if Connection.GetServerProvider = spMSSQL then
      CheckStmtError(FPlainDriver.SQLSetStmtAttr(fHSTMT, SQL_SOPT_SS_CURSOR_OPTIONS, Pointer(SQL_CO_FFO),0));
    inherited Prepare;
  end;
end;

procedure TZAbstractODBCStatement.PrepareInParameters;
var
  ParameterNumber: SQLUSMALLINT;
  ParameterCount: SQLSMALLINT;
  NoLobBoundParamCount: Integer;
  AllParamsAreArrays: Boolean;
begin
  CheckStmtError(fPlainDriver.SQLNumParams(fHSTMT, @ParameterCount));
  if ParameterCount > 0 then begin
    if ParameterCount <> InParamCount then
      raise EZSQLException.Create(SInvalidInputParameterCount);
    SetLength(fParamInfos, ParameterCount);
    NoLobBoundParamCount := 0;
    fBufferSize := 0;
    AllParamsAreArrays := False;
    for ParameterNumber := 0 to ParameterCount-1 do begin
      CheckStmtError(fPlainDriver.SQLDescribeParam(fHSTMT, ParameterNumber +1, //0=bookmark and Params do starts with 1
        @fParamInfos[ParameterNumber].DataType, @fParamInfos[ParameterNumber].ColumnSize,
        @fParamInfos[ParameterNumber].DecimalDigits, @fParamInfos[ParameterNumber].Nullable));
      //get "best" TZSQLType -> ODBC does not returns the C-Data types
      fParamInfos[ParameterNumber].SQLType := ConvertODBCTypeToSQLType(fParamInfos[ParameterNumber].DataType,
                                      InParamTypes[ParameterNumber], Consettings^.CPType);
      //now assign minimal conversion datatypes
      fParamInfos[ParameterNumber].DataType := ConvertSQLTypeToODBCType(fParamInfos[ParameterNumber].SQLType,
                                    fParamInfos[ParameterNumber].DataType, ConSettings^.ClientCodePage^.Encoding);
      fParamInfos[ParameterNumber].C_DataType := ConvertODBCTypeToODBC_CType(fParamInfos[ParameterNumber].DataType,
        fParamInfos[ParameterNumber].SQLType in [stByte, stWord, stLongWord, stULong], ConSettings^.ClientCodePage^.Encoding);
      //test for (N)VARCHAR(MAX)/VARBINARY(MAX)
      if (fParamInfos[ParameterNumber].SQLType in [stBytes, stString, stUnicodeString]) and
         (fParamInfos[ParameterNumber].ColumnSize = 0) then
        fParamInfos[ParameterNumber].SQLType := TZSQLType(Ord(fParamInfos[ParameterNumber].SQLType)+3); //switch to streamed mode
      Inc(NoLobBoundParamCount, Ord((fParamInfos[ParameterNumber].SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) and
         (not (InParamTypes[ParameterNumber] in [stAsciiStream, stUnicodeStream, stBinaryStream]))));
      //note: Code is prepared to handle any case of Param-Directions  except fetching returned data
      fParamInfos[ParameterNumber].InputDataType := ParamTypeToODBCParamType(pctIn, fParamInfos[ParameterNumber].SQLType, fStreamSupport);
      fParamInfos[ParameterNumber].BufferSize := CalcBufSize(fParamInfos[ParameterNumber].ColumnSize * Ord(not (fParamInfos[ParameterNumber].SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream])),
        fParamInfos[ParameterNumber].C_DataType, fParamInfos[ParameterNumber].SQLType, ConSettings^.ClientCodePage);
      if fParamInfos[ParameterNumber].SQLType = stTimeStamp then begin
        fParamInfos[ParameterNumber].ColumnSize := 23;
        fParamInfos[ParameterNumber].DecimalDigits := 3;
      end else if (fParamInfos[ParameterNumber].SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
                                                              {1. Intf entry} {cur. array index} {ParameterIndex}
        SetLength(fParamInfos[ParameterNumber].DescBindBuffer, SizeOf(Pointer)+  SizeOf(Integer)+  SizeOf(Integer));
      AllParamsAreArrays := ((ParameterNumber = 0) and (InParamValues[ParameterNumber].VType = vtArray)) or
                              (AllParamsAreArrays and (InParamValues[ParameterNumber].VType = vtArray));
      inc(fBufferSize, fParamInfos[ParameterNumber].BufferSize+SizeOf(SQLLEN))
    end;
    SetLength(fBatchLobBuf, NoLobBoundParamCount);
    if AllParamsAreArrays then
      fMaxBufArrayBound := Max(1, fZBufferSize div fBufferSize)
    else
      fMaxBufArrayBound := -1;
    if fBindRowWise
    then CheckStmtError(fPlainDriver.SQLSetStmtAttr(fHSTMT, SQL_ATTR_PARAM_BIND_TYPE, SQLPOINTER(fBufferSize), 0))
    else CheckStmtError(fPlainDriver.SQLSetStmtAttr(fHSTMT, SQL_ATTR_PARAM_BIND_TYPE, Pointer(SQL_PARAM_BIND_BY_COLUMN), 0));
  end;
end;

procedure TZAbstractODBCStatement.PrepareOpenedResultSetsForReusing;
begin
  if Assigned(FOpenResultSet) then
    if fMoreResultsIndicator <> mriHasNoMoreResults then
      if (Pointer(LastResultSet) = FOpenResultSet) then begin
        LastResultSet.Close;
        LastResultSet := nil;
      end else begin
        IZResultSet(FOpenResultSet).Close;
        FOpenResultSet := nil;
      end
    else
      IZResultSet(FOpenResultSet).ResetCursor;
  if Assigned(LastResultSet) then
    if (fMoreResultsIndicator <> mriHasNoMoreResults) then begin
      LastResultSet.Close;
      LastResultSet := nil;
    end else
      LastResultSet.ResetCursor;
end;

procedure TZAbstractODBCStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType);
begin
  if ParameterIndex = FirstDbcIndex then
    FArrayOffSet := 0;
  inherited SetDataArray(ParameterIndex, Value, SQLType, VariantType);
end;

procedure TZAbstractODBCStatement.SetMoreResultsIndicator(
  Value: TZMoreResultsIndicator);
begin
  fMoreResultsIndicator := Value;
end;

function TZAbstractODBCStatement.SupportsSingleColumnArrays: Boolean;
begin
  Result := (GetConnection as IZODBCConnection).GetArraySelectSupported;
end;

procedure TZAbstractODBCStatement.Unprepare;
begin
  inherited Unprepare;
  if Assigned(fHSTMT) then begin
    CheckStmtError(fPlainDriver.SQLFreeHandle(SQL_HANDLE_STMT, fHSTMT)); //<- does the trick to get a instance reused
    fHSTMT := nil;
  end;
end;

procedure TZAbstractODBCStatement.UnPrepareInParameters;
begin
  SetLength(fParamInfos, 0);
  SetLength(fBatchLobBuf, 0);
  if Assigned(fHSTMT) and Assigned(fPHDBC^) then
    CheckStmtError(fPlainDriver.SQLFreeStmt(fHSTMT,SQL_RESET_PARAMS));
end;

{ TZODBCPreparedStatementW }

function TZODBCPreparedStatementW.InternalCreateResultSet: IZResultSet;
begin
  Result := TODBCResultSetW.Create(Self, fHSTMT, fPHDBC^, SQL, Connection as IZODBCConnection,
    fZBufferSize, ChunkSize, FEnhancedColInfo);
end;

procedure TZODBCPreparedStatementW.Prepare;
begin
  if Not Prepared then begin
    InternalBeforePrepare;
    CheckStmtError(TODBC3UnicodePlainDriver(fPlainDriver).SQLPrepareW(fHSTMT, Pointer(WSQL), Length(WSQL)));
    inherited Prepare;
  end else
    if Assigned(fHSTMT) and Assigned(fPHDBC^) then
      CheckStmtError(fPlainDriver.SQLFreeStmt(fHSTMT,SQL_CLOSE));
end;

{ TZODBCPreparedStatementA }

function TZODBCPreparedStatementA.InternalCreateResultSet: IZResultSet;
begin
  Result := TODBCResultSetA.Create(Self, fHSTMT, fPHDBC^, SQL, Connection as IZODBCConnection,
    fZBufferSize, ChunkSize, FEnhancedColInfo);
end;

procedure TZODBCPreparedStatementA.Prepare;
begin
  if Not Prepared then begin
    InternalBeforePrepare;
    CheckStmtError(TODBC3RawPlainDriver(fPlainDriver).SQLPrepare(fHSTMT, Pointer(ASQL), Length(ASQL)));
    inherited Prepare;
  end else
    if Assigned(fHSTMT) and Assigned(fPHDBC^) then
      CheckStmtError(fPlainDriver.SQLFreeStmt(fHSTMT,SQL_CLOSE));
end;

end.


