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

  TZODBCParamInfo = record
    DataType: SQLSMALLINT;
    C_DataType: SQLSMALLINT;
    InputDataType: SQLSMALLINT;
    DecimalDigits: SQLSMALLINT;
    ParamDataPtr: SQLPOINTER;
    Nullable: SQLSMALLINT;
    ColumnSize: SQLLEN;
    BufferSize: SQLLEN;
    ValueCount: SQLULEN;
    SQLType: TZSQLType;
    Bound: Boolean;
    BindBuffer: TByteDynArray;
  end;

  TZAbstractODBCStatement = class(TZAbstractPreparedStatement, IZODBCStatement)
  private
    fPlainDriver: IODBC3BasePlainDriver;
    fPHDBC: PSQLHDBC;
    fHSTMT: SQLHSTMT;
    fParamInfos: array of TZODBCParamInfo;
    fStreamSupport: Boolean;
    fZBufferSize, fArrayOffSet, fMaxBufArrayBound, fCurrentIterations: Integer;
    fStmtTimeOut: SQLULEN;
    fEnhancedColInfo: Boolean;
    fBatchLobBuf: TIZBlobsDynArray;
    fMoreResultsIndicator: TZMoreResultsIndicator;
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
    constructor Create(Connection: IZODBCConnection; var ConnectionHandle: SQLHDBC; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Unprepare; override;
    procedure Close; override;

    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); override;
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
  ZDbcODBCUtils, ZDbcODBCResultSet, ZDbcCachedResultSet, ZDbcGenericResolver;

const
  NullInd: array[Boolean] of SQLLEN = (SQL_NO_NULLS, SQL_NULL_DATA);

type
  PLobArray = ^TLobArray;
  TLobArray = array[0..High(Word)] of IZBlob;

{ TZAbstractODBCStatement }

procedure TZAbstractODBCStatement.BindInParameters;
var
  fAutoCommit: Boolean;
  RowCount: SQLLEN;
begin
  if Length(fParamInfos) = 0 then
    exit
  else
    if (ArrayCount = 0) or (fMaxBufArrayBound = -1) then begin
      fCurrentIterations := 1;
      InternalBindParams;
    end else begin
      LastUpdateCount := 0;
      fArrayOffSet := 0;
      fAutoCommit := Connection.GetAutoCommit;
      Connection.SetAutoCommit(False);
      while True do
      begin
        if (FArrayOffSet+fMaxBufArrayBound >= ArrayCount) then
        begin //left space for last excution
          fCurrentIterations := ArrayCount - FArrayOffSet;
          InternalBindParams;
          Connection.SetAutoCommit(fAutoCommit);
          FArrayOffSet := 0; //Reset!
          Break
        end
        else
        begin
          InternalBindParams;
          InternalExecute;
          Inc(FArrayOffSet, fMaxBufArrayBound);
          CheckStmtError(fPlainDriver.RowCount(fHSTMT, @RowCount));
          LastUpdateCount := LastUpdateCount + RowCount;
        end
      end
    end;
  inherited BindInParameters;
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
    fPlainDriver.FreeHandle(SQL_HANDLE_STMT, fHSTMT);
    fHSTMT := nil;
  end;
end;

constructor TZAbstractODBCStatement.Create(Connection: IZODBCConnection;
  var ConnectionHandle: SQLHDBC; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  //inherited Create(Connection, Connection.NativeSQL(SQL), Info);
  fPlainDriver := Connection.GetPlainDriver;
  fStreamSupport := Connection.ODBCVersion >= {%H-}Word(SQL_OV_ODBC3_80);
  fPHDBC := @ConnectionHandle;
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072); //by default 128KB
  FEnhancedColInfo := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'enhanced_column_info', 'True'));
  fStmtTimeOut := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'StatementTimeOut', ''), SQL_QUERY_TIMEOUT_DEFAULT); //execution timeout in seconds by default 1
  fMoreResultsIndicator := TZMoreResultsIndicator(Ord(not Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets));
end;

destructor TZAbstractODBCStatement.Destroy;
begin
  (Connection as IZODBCConnection).UnRegisterPendingStatement(Self);
  inherited Destroy;
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
    CheckStmtError(fPlainDriver.RowCount(fHSTMT, @RowCount));
    LastUpdateCount := LastUpdateCount + RowCount;
    CheckStmtError(fPlainDriver.NumResultCols(fHSTMT, @ColumnCount));
    if ColumnCount > 0 then begin
      LastUpdateCount := -1;
      LastResultSet := GetCurrentResultSet;
    end else
      if Connection.GetMetadata.GetDatabaseInfo.SupportsMultipleResultSets and
         (fMoreResultsIndicator <> mriHasNoMoreResults) then
        repeat
          RETCODE := fPlainDriver.MoreResults(fHSTMT);
          if RETCODE = SQL_SUCCESS then begin
            fMoreResultsIndicator := mriHasMoreResults;
            CheckStmtError(fPlainDriver.NumResultCols(fHSTMT, @ColumnCount));
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
    CheckStmtError(fPlainDriver.NumResultCols(fHSTMT, @ColumnCount));
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
          RETCODE := fPlainDriver.MoreResults(fHSTMT);
          if RETCODE = SQL_SUCCESS then begin
            fMoreResultsIndicator := mriHasMoreResults;
            CheckStmtError(fPlainDriver.NumResultCols(fHSTMT, @ColumnCount));
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
    CheckStmtError(fPlainDriver.RowCount(fHSTMT, @RowCount));
    LastUpdateCount := LastUpdateCount + RowCount;
    Result := RowCount;
  finally
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
    CheckODBCError(fPlainDriver.AllocHandle(SQL_HANDLE_STMT, fPHDBC^, fHSTMT), fPHDBC^, SQL_HANDLE_DBC, Connection as IZODBCConnection);
    //force column wise bindings
    CheckStmtError(fPlainDriver.SetStmtAttr(fHSTMT, SQL_ATTR_PARAM_BIND_TYPE, Pointer(SQL_PARAM_BIND_BY_COLUMN), 0));
    CheckStmtError(fPlainDriver.SetStmtAttr(fHSTMT, SQL_ATTR_QUERY_TIMEOUT, {%H-}Pointer(fStmtTimeOut), 0));
    fMoreResultsIndicator := mriUnknown;
  end;
end;

procedure TZAbstractODBCStatement.InternalBindParams;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..High(Word)] of Byte;
  PShortIntArray = ^TShortIntArray;
  TShortIntArray = array[0..High(Word)] of ShortInt;
  PWordArray = ^TWordArray;
  TWordArray = array[0..High(Word)] of Word;
  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[0..High(Word)] of SmallInt;
  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..High(Word)] of LongWord;
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..High(Word)] of Integer;
  PUInt64Array = ^TUInt64Array;
  TUInt64Array = array[0..High(Word)] of UInt64;
  PInt64Array = ^TInt64Array;
  TInt64Array = array[0..High(Word)] of Int64;
  PSingleArray = ^TSingleArray;
  TSingleArray = array[0..High(Word)] of Single;
  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array[0..High(Word)] of Double;

var
  I, J : SQLUSMALLINT;
  Value: TZVariant;
  ParamSetChanged: Boolean;
  MaxArrayCount: SQLULEN;
  TmpLobIndex: Integer;

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
  ZAnsiStringArray: TAnsiStringDynArray absolute ZData;
  ZUTF8StringArray: TUTF8StringDynArray absolute ZData;
  ZStringArray: TStringDynArray absolute ZData;
  ZUnicodeStringArray: TUnicodeStringDynArray absolute ZData;
  ZCharRecArray: TZCharRecDynArray absolute ZData;
  ZBytesArray: TBytesDynArray absolute ZData;
  ZInterfaceArray: TInterfaceDynArray absolute ZData;
  ZGUIDArray: TGUIDDynArray absolute ZData;

  ParameterDataPtr: Pointer;

  year, fraction: Word;
  TempBlob: IZBlob;
  TmpStream: TStream;
  RawTemp: RawByteString;
  CharRec: TZCharRec;
  StrLen_or_IndPtr: SQLPOINTER;
  StrLen_or_IndArray: PStrLen_or_IndArray absolute StrLen_or_IndPtr;

  function IsNotNull: Boolean;
  begin
    StrLen_or_IndArray^[J] := NullInd[Assigned(Value.VArray.VIsNullArray) and TBooleanDynArray(Value.VArray.VIsNullArray)[j+fArrayOffSet]];
    Result := StrLen_or_IndArray^[J] <> SQL_NULL_DATA
  end;

  function LobIsNotNull: Boolean;
  begin
    Result := not (Assigned(Value.VArray.VIsNullArray) and TBooleanDynArray(Value.VArray.VIsNullArray)[j+fArrayOffSet]);
    StrLen_or_IndArray^[J] := SQL_DATA_AT_EXEC;
  end;

  procedure SetBinary(P: Pointer; Len: Integer);
  begin
    StrLen_or_IndArray^[J] := Len;
    if Assigned(P) then
      Move(P^, ParameterDataPtr^, StrLen_or_IndArray^[J]);
  end;
  procedure SetDate(D: TDateTime);
  begin
    DecodeDate(D, Year, PSQL_DATE_STRUCT(ParameterDataPtr)^.month, PSQL_DATE_STRUCT(ParameterDataPtr)^.day);
    PSQL_DATE_STRUCT(ParameterDataPtr)^.year := year;
    Inc(PAnsiChar(ParameterDataPtr), SizeOf(TSQL_DATE_STRUCT));
  end;
  procedure SetTime(D: TDateTime);
  begin
    DecodeTime(D, PSQL_TIME_STRUCT(ParameterDataPtr)^.hour, PSQL_TIME_STRUCT(ParameterDataPtr)^.minute,
      PSQL_TIME_STRUCT(ParameterDataPtr)^.second, fraction);
    Inc(PAnsiChar(ParameterDataPtr), SizeOf(TSQL_TIME_STRUCT));
  end;
  procedure SetTimeStamp(D: TDateTime);
  begin
    DecodeDateTime(D, Year, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.month, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.day,
      PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.hour, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.minute, PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.second, fraction);
    PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.year := year;
    if fParamInfos[i].DecimalDigits = 7 then
      PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.fraction := fraction
    else
      //https://social.msdn.microsoft.com/Forums/sqlserver/en-US/ac1b5a6d-5e64-4603-9c92-b75ba4e51bf2/error-22008-datetime-field-overflow-when-inserting-a-record-with-datetime2-field-via-odbc?forum=sqldataaccess
      PSQL_TIMESTAMP_STRUCT(ParameterDataPtr)^.fraction := 0;//puff what a shit: fraction available but can't be set???;
    Inc(PAnsiChar(ParameterDataPtr), SizeOf(TSQL_TIMESTAMP_STRUCT));
  end;
  procedure SetPUnicode(P: PWidechar; Len: Integer);
  begin
    StrLen_or_IndArray^[J] := Min(fParamInfos[i].BufferSize-2, Len shl 1);
    if Assigned(P) then
      Move(P^, ParameterDataPtr^, StrLen_or_IndArray^[J]);
    (PWideChar(ParameterDataPtr)+(StrLen_or_IndArray^[J] shr 1))^ := #0;
    Inc(PAnsiChar(ParameterDataPtr), fParamInfos[i].BufferSize);
  end;
  procedure SetUnicode(const U: ZWideString);
  begin
    SetPUnicode(Pointer(U), Length(U));
  end;
  procedure SetPRaw(P: PAnsiChar; Len: Integer);
  begin
    StrLen_or_IndArray^[J] := Min(fParamInfos[i].BufferSize-1, Len);
    if Assigned(P) then
      Move(P^, ParameterDataPtr^, StrLen_or_IndArray^[J]);
    (PAnsiChar(ParameterDataPtr)+StrLen_or_IndArray^[J])^ := #0;
    Inc(PAnsiChar(ParameterDataPtr), fParamInfos[i].BufferSize);
  end;
  procedure SetRaw(const R: RawByteString);
  begin
    SetPRaw(Pointer(R), Length(R));
  end;
  procedure SetCLobFromPUnicode(P: PWidechar; Len: Integer);
  begin
    PLobArray(PPointer(ParameterDataPtr)^)[j] := TZAbstractCLob.CreateWithData(P, Len, ConSettings);
  end;
  procedure SetCLobFromUnicode(const U: ZWideString);
  begin
    if Pointer(U) = nil then
      SetCLobFromPUnicode(PEmptyUnicodeString, 0)
    else
      SetCLobFromPUnicode(PWideChar(Pointer(U)), Length(U));
  end;
  procedure SetClobFromPRaw(P: PAnsiChar; Len: Integer);
  begin
    PLobArray(PPointer(ParameterDataPtr)^)[j] := TZAbstractCLob.CreateWithData(P, Len, ConSettings^.ClientCodePage^.CP, ConSettings);
  end;
  procedure SetClobFromRaw(const R: RawByteString);
  begin
    if Pointer(R) = nil then
      SetClobFromPRaw(PEmptyAnsiString, 0)
    else
      SetClobFromPRaw(Pointer(R), Length(R));
  end;
  procedure SetBlob(P: Pointer; Len: Integer);
  begin
    PLobArray(PPointer(ParameterDataPtr)^)[j] := TZAbstractBLob.CreateWithData(P, Len);
  end;
  procedure SetBlobFromBts(const B: TBytes);
  begin
    SetBlob(Pointer(B), Length(B));
  end;
begin
  { missing point(no test case): desc array bindings of a single column so not params are arrays indicated by fMaxBufArrayBound = -1 }
  MaxArrayCount := Min(Abs(fMaxBufArrayBound), ArrayCount);
  if Length(fParamInfos) > 0 then begin
    ParamSetChanged := False;
    TmpLobIndex := -1;
    Assert(((fMaxBufArrayBound = -1) and (ArrayCount = 0)) or (fMaxBufArrayBound > 0), 'Desc array binding not done yet');
    for i := low(fParamInfos) to high(fParamInfos) do begin
      Value := InParamValues[i];
      if Value.VType = vtArray then begin //array DML binding
        if Assigned(Value.VArray.VArray) then begin
          if fMaxBufArrayBound < 0 then begin
            fCurrentIterations := {%H-}PArrayLenInt({%H-}NativeUInt(Value.VArray.VArray) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF} //FPC returns High() for this pointer location
          end else
            fCurrentIterations := Min(fMaxBufArrayBound, {%H-}PArrayLenInt({%H-}NativeUInt(Value.VArray.VArray) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}-fArrayOffSet);
          //MaxArrayCount := Max(MaxArrayCount, CurrArrayCount);
          { fast binary C and Pascal equal binary types first }
          if (TZSQLType(Value.VArray.VArrayType) = fParamInfos[i].SQLType) and
             ((Ord(fParamInfos[i].SQLType) <= Ord(stDouble)) and (Ord(fParamInfos[i].SQLType) > Ord(stBoolean))) then begin
            SetLength(fParamInfos[i].BindBuffer, SizeOf(SQLLEN) * SQLLEN(fCurrentIterations)); //reserve space for Null indicators
            ZData := Value.VArray.VArray;
            //!no! move just reference -> can be used for inout param as well
            case fParamInfos[i].SQLType of
              stByte:     ParameterDataPtr := @ZByteArray[fArrayOffSet];
              stShort:    ParameterDataPtr := @ZShortIntArray[fArrayOffSet];
              stWord:     ParameterDataPtr := @ZWordArray[fArrayOffSet];
              stSmall:    ParameterDataPtr := @ZSmallIntArray[fArrayOffSet];
              stLongWord: ParameterDataPtr := @ZLongWordArray[fArrayOffSet];
              stInteger:  ParameterDataPtr := @ZIntegerArray[fArrayOffSet];
              stULong:    ParameterDataPtr := @ZUInt64Array[fArrayOffSet];
              stLong:     ParameterDataPtr := @ZInt64Array[fArrayOffSet];
              stFloat:    ParameterDataPtr := @ZSingleArray[fArrayOffSet];
              stDouble:   ParameterDataPtr := @ZDoubleArray[fArrayOffSet];
              else ParameterDataPtr := nil; //satisfy compiler
            end;
            fParamInfos[I].ValueCount := fCurrentIterations;
            fParamInfos[I].Bound := fParamInfos[i].ParamDataPtr = ParameterDataPtr; //force new pointer binding
            fParamInfos[i].ParamDataPtr := ParameterDataPtr;
            StrLen_or_IndPtr := Pointer(fParamInfos[i].BindBuffer);
            if Assigned(Value.VArray.VIsNullArray) then
              for J := fArrayOffSet to fCurrentIterations -1 do
                StrLen_or_IndArray^[j] := NullInd[TBooleanDynArray(Value.VArray.VIsNullArray)[J+fArrayOffSet]];
          end else begin
            { remaining are conversion effected or move required types only }
            if SQLULEN(fCurrentIterations) <> fParamInfos[I].ValueCount then begin
              SetLength(fParamInfos[i].BindBuffer, (fParamInfos[i].BufferSize+SizeOf(SQLLEN)) * SQLLEN(fCurrentIterations));
              fParamInfos[I].Bound := False; //force new pointer binding
              fParamInfos[I].ValueCount := fCurrentIterations;
              ParamSetChanged := True; //force new pointer binding
            end;
            StrLen_or_IndArray := Pointer(fParamInfos[i].BindBuffer);
            ParameterDataPtr := {%H-}Pointer({%H-}NativeUInt(StrLen_or_IndArray)+(SizeOf(SQLLEN)*NativeUInt(fCurrentIterations)));
            fParamInfos[I].ParamDataPtr := ParameterDataPtr;
            { EH note : the following code make 2 case(ODBC expected and Zeos given) checks and the array loop follows
              this is up to a half second faster for 5000 rows with 5 field bug ugly to read  ... is it?
              optimized code ... however }
            ZData := Value.VArray.VArray;
            case fParamInfos[i].SQLType of
              stBoolean:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[J] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZByteArray[J+fArrayOffSet] <> 0);
                  stShort:          for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZShortIntArray[J+fArrayOffSet] <> 0);
                  stWord:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZWordArray[J+fArrayOffSet] <> 0);
                  stSmall:          for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZSmallIntArray[J+fArrayOffSet] <> 0);
                  stLongWord:       for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZLongWordArray[J+fArrayOffSet] <> 0);
                  stInteger:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZIntegerArray[J+fArrayOffSet] <> 0);
                  stLong:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZInt64Array[J+fArrayOffSet] <> 0);
                  stULong:          for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZUInt64Array[J+fArrayOffSet] <> 0);
                  stFloat:          for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZSingleArray[J+fArrayOffSet] <> 0);
                  stDouble:         for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZDoubleArray[J+fArrayOffSet] <> 0);
                  stCurrency:       for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZCurrencyArray[J+fArrayOffSet] <> 0);
                  stBigDecimal:     for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZExtendedArray[J+fArrayOffSet] <> 0);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(StrToBoolEx(ZRawByteStringArray[J+fArrayOffSet]));
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(StrToBoolEx(ZUnicodeStringArray[J+fArrayOffSet]));
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do If IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PByteArray(ParameterDataPtr)^[j] := Ord(StrToBoolEx(PWideChar(ZCharRecArray[J+fArrayOffSet].P)))
                                          else
                                            PByteArray(ParameterDataPtr)^[j] := Ord(StrToBoolEx(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P)));
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZDateTimeArray[J+fArrayOffSet] <> 0);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stByte:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stShort:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stULong:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := ZUInt64Array[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PByteArray(ParameterDataPtr)^[j] := UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PByteArray(ParameterDataPtr)^[j] := RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do If IsNotNull then PByteArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stShort:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := ZUInt64Array[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PShortIntArray(ParameterDataPtr)^[j] := UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PShortIntArray(ParameterDataPtr)^[j] := RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then PShortIntArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stWord:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := ZUInt64Array[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case InParamValues[i].VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PWordArray(ParameterDataPtr)^[j] := RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PWordArray(ParameterDataPtr)^[j] := UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PWordArray(ParameterDataPtr)^[j] := UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PWordArray(ParameterDataPtr)^[j] := RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PWordArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then PWordArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stSmall:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := ZUInt64Array[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PSmallIntArray(ParameterDataPtr)^[j] := UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PSmallIntArray(ParameterDataPtr)^[j] := RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then PSmallIntArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stLongWord:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := ZUInt64Array[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := RawToUInt64Def(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := UnicodeToUInt64Def(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PLongWordArray(ParameterDataPtr)^[j] := UnicodeToUInt64Def(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PLongWordArray(ParameterDataPtr)^[j] := RawToUInt64Def(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then PLongWordArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stInteger:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stULong:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := ZUInt64Array[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := RawToIntDef(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := UnicodeToIntDef(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PIntegerArray(ParameterDataPtr)^[j] := UnicodeToIntDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PIntegerArray(ParameterDataPtr)^[j] := RawToIntDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then PIntegerArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stULong:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := RawToUInt64Def(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := UnicodeToUInt64Def(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PUInt64Array(ParameterDataPtr)^[j] := UnicodeToUInt64Def(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PUInt64Array(ParameterDataPtr)^[j] := RawToUInt64Def(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:              for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stLong:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := ZInt64Array[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Trunc(ZSingleArray[J+fArrayOffSet]);
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Trunc(ZDoubleArray[J+fArrayOffSet]);
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Trunc(ZCurrencyArray[J+fArrayOffSet]);
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Trunc(ZExtendedArray[J+fArrayOffSet]);
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := RawToInt64Def(ZRawByteStringArray[J+fArrayOffSet], 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do If IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := UnicodeToInt64Def(ZUnicodeStringArray[J+fArrayOffSet], 0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            PUInt64Array(ParameterDataPtr)^[j] := UnicodeToInt64Def(PWideChar(ZCharRecArray[J+fArrayOffSet].P),0)
                                          else
                                            PUInt64Array(ParameterDataPtr)^[j] := RawToInt64Def(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P),0);
                      vtNull:           for J := 0 to fCurrentIterations -1 do If IsNotNull then PUInt64Array(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:              for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PInt64Array(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stFloat:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZSingleArray[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZSingleArray[J+fArrayOffSet];
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZDoubleArray[J+fArrayOffSet];
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZCurrencyArray[J+fArrayOffSet];
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := ZExtendedArray[J+fArrayOffSet];
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := RawToFloatDef(ZRawByteStringArray[J+fArrayOffSet], '.', 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := UnicodeToFloatDef(ZUnicodeStringArray[J+fArrayOffSet], WideChar('.'),0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := UnicodeToFloatDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P), WideChar('.'),0)
                                          else
                                            if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := RawToFloatDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P), '.',0);
                      vtNull:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:              for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:          for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
                  else
                    raise EZSQLException.Create(SUnsupportedParameterType);
                end;
              stDouble, stCurrency, stBigDecimal:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBoolean:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := Ord(ZBooleanArray[J+fArrayOffSet]);
                  stByte:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZByteArray[J+fArrayOffSet];
                  stShort:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZShortIntArray[J+fArrayOffSet];
                  stWord:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZWordArray[J+fArrayOffSet];
                  stSmall:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZSmallIntArray[J+fArrayOffSet];
                  stLongWord:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZLongWordArray[J+fArrayOffSet];
                  stLong:         for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZDoubleArray[J+fArrayOffSet];
                  stInteger:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZIntegerArray[J+fArrayOffSet];
                  stFloat:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZDoubleArray[J+fArrayOffSet];
                  stDouble:       for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZDoubleArray[J+fArrayOffSet];
                  stCurrency:     for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZCurrencyArray[J+fArrayOffSet];
                  stBigDecimal:   for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := ZExtendedArray[J+fArrayOffSet];
                  //stGUID:
                  stString, stUnicodeString:
                    case Value.VArray.VArrayVariantType of
                      {$IFNDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtAnsiString,
                      vtUTF8String,
                      vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := RawToFloatDef(ZRawByteStringArray[J+fArrayOffSet], '.', 0);
                      {$IFDEF UNICODE}
                      vtString,
                      {$ENDIF}
                      vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := UnicodeToFloatDef(ZUnicodeStringArray[J+fArrayOffSet], WideChar('.'),0);
                      vtCharRec:        for J := 0 to fCurrentIterations -1 do
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then
                                            if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := UnicodeToFloatDef(PWideChar(ZCharRecArray[J+fArrayOffSet].P), WideChar('.'),0)
                                          else
                                            if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := RawToFloatDef(PAnsiChar(ZCharRecArray[J+fArrayOffSet].P), '.',0);
                      vtNull:      for J := 0 to fCurrentIterations -1 do if IsNotNull then PDoubleArray(ParameterDataPtr)^[j] := 0;
                      else
                        raise Exception.Create('Unsupported String Variant');
                    end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := Ord(ZBytesArray[J+fArrayOffSet] = nil);
                  stDate,
                  stTime,
                  stTimestamp:    for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := Trunc(ZDateTimeArray[J+fArrayOffSet]);
                  stAsciiStream,
                  stUnicodeStream,
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then PSingleArray(ParameterDataPtr)^[j] := Ord(ZInterfaceArray[J+fArrayOffSet] = nil);
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
                          {$IFNDEF UNICODE}
                          vtString,
                          {$ENDIF}
                          vtAnsiString,
                          vtUTF8String,
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
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
                  end;
              stBytes:
                case TZSQLType(Value.VArray.VArrayType) of
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                    TempBlob := ZInterfaceArray[J+fArrayOffSet] as IZBLob;
                                    SetBinary(TempBlob.GetBuffer, Min(TempBlob.Length, fParamInfos[i].BufferSize));
                                  end;
                  stBytes:        for J := 0 to fCurrentIterations -1 do if IsNotNull then SetBinary(Pointer(ZBytesArray[J+fArrayOffSet]), Min(Length(ZBytesArray[J+fArrayOffSet]), fParamInfos[i].BufferSize));
                  stGUID:         for J := 0 to fCurrentIterations -1 do if IsNotNull then SetBinary(@ZGUIDArray[J+fArrayOffSet].D1, Min(16, fParamInfos[i].BufferSize));
                  stString:
                      case InParamValues[i].VArray.VArrayVariantType of
                        {$IFNDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtAnsiString,
                        vtUTF8String,
                        vtRawByteString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then SetBinary(Pointer(ZRawByteStringArray[J+fArrayOffSet]), Min(Length(ZRawByteStringArray[J+fArrayOffSet]), fParamInfos[i].BufferSize));
                        {$IFDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtUnicodeString:  for J := 0 to fCurrentIterations -1 do if IsNotNull then begin
                                            RawTemp := UnicodeStringToASCII7(ZUnicodeStringArray[J+fArrayOffSet]);
                                            SetBinary(Pointer(RawTemp), Min(Length(RawTemp), fParamInfos[i].BufferSize));
                                          end;
                        vtCharRec:        for J := 0 to fCurrentIterations -1 do if IsNotNull then
                                          if ZCompatibleCodePages(ZCharRecArray[J+fArrayOffSet].CP, zCP_UTF16) then begin
                                            RawTemp := UnicodeStringToASCII7(ZCharRecArray[J+fArrayOffSet].P, Min(ZCharRecArray[J+fArrayOffSet].Len,fParamInfos[i].BufferSize));
                                            SetBinary(Pointer(RawTemp), Length(RawTemp));
                                          end else
                                            SetBinary(ZCharRecArray[J+fArrayOffSet].P, Min(ZCharRecArray[J+fArrayOffSet].Len, fParamInfos[i].BufferSize));
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
                        {$IFNDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtAnsiString,
                        vtUTF8String,
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
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
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
                        {$IFNDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtAnsiString,
                        vtUTF8String,
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
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
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
                        {$IFNDEF UNICODE}
                        vtString,
                        {$ENDIF}
                        vtAnsiString,
                        vtUTF8String,
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
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
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
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetUnicode(PRawToUnicode(Pointer(ZAnsiStringArray[J+fArrayOffSet]), Length(ZAnsiStringArray[J+fArrayOffSet]), ZDefaultSystemCodePage));
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if IsNotNull then
                          if Assigned(Pointer(ZUTF8StringArray[J+fArrayOffSet])) and (Length(ZUTF8StringArray[J+fArrayOffSet]) <= fParamInfos[I].ColumnSize) then begin
                            StrLen_or_IndArray^[J] := UTF8ToWideChar(Pointer(ZUTF8StringArray[J+fArrayOffSet]), Length(ZUTF8StringArray[J+fArrayOffSet]), ParameterDataPtr) shl 1;
                            (PWideChar(ParameterDataPtr)+(StrLen_or_IndArray^[J] shr 1))^ := #0;
                            Inc(PAnsiChar(ParameterDataPtr), fParamInfos[I].BufferSize);
                          end else
                            SetUnicode(PRawToUnicode(Pointer(ZUTF8StringArray[J+fArrayOffSet]), Length(ZUTF8StringArray[J+fArrayOffSet]), zCP_UTF8));
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
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
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
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[J+fArrayOffSet], ZDefaultSystemCodePage, ConSettings^.ClientCodePage^.CP));
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if IsNotNull then SetRaw(ConSettings^.ConvFuncs.ZUTF8ToRaw(ZUTF8StringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
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
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
                end;
              stAsciiStream, stUnicodeStream: begin
                if (not (TZSQLType(Value.VArray.VArrayType) in [stAsciiStream, stUnicodeStream])) then begin
                  Inc(TmpLobIndex);
                  SetLength(FBatchLobBuf[TmpLobIndex], Min(fCurrentIterations, fMaxBufArrayBound)); //minimal length
                  PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(ParameterDataPtr)^:= @FBatchLobBuf[TmpLobIndex][fArrayOffSet];
                end else begin
                  PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(ParameterDataPtr)^:= @TInterfaceDynArray(Value.VArray.VArray)[fArrayOffSet];
                end;
                //if Param IO is out we need to know which kind of IZLob need to load the data
                PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobParameterIndexOffSet))^ := I;
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
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(PRawToUnicode(Pointer(ZAnsiStringArray[J+fArrayOffSet]), Length(ZAnsiStringArray[J+fArrayOffSet]), ZDefaultSystemCodePage));
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetCLobFromUnicode(PRawToUnicode(Pointer(ZUTF8StringArray[J+fArrayOffSet]), Length(ZUTF8StringArray[J+fArrayOffSet]), zCP_UTF8));
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
                          TempBlob.GetPWideChar //make conversion first
                        end else begin
                          TmpStream := GetValidatedUnicodeStream(TempBlob.GetBuffer, TempBlob.Length, ConSettings, False);
                          SetCLobFromPUnicode(TMemoryStream(TmpStream).Memory, TmpStream.Size shr 1); //replaces ZInterfaceArray[J+fArrayOffSet]
                          TmpStream.Free;
                        end;
                      end;
                    else
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
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
                        vtAnsiString:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[J+fArrayOffSet], ZDefaultSystemCodePage, ConSettings^.ClientCodePage^.CP));
                        vtUTF8String:     for J := 0 to fCurrentIterations -1 do if LobIsNotNull then SetClobFromRaw(ConSettings^.ConvFuncs.ZUTF8ToRaw(ZUTF8StringArray[J+fArrayOffSet], ConSettings^.ClientCodePage^.CP));
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
                        if TempBlob.IsClob then
                          TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP) //make internal conversion first
                        else
                          SetClobFromRaw(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer, TempBlob.Length, ConSettings));
                      end;
                    else
                      raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
                  end;
                end;
              stBinaryStream: begin
                if (TZSQLType(Value.VArray.VArrayType) <> stBinaryStream) then begin
                  Inc(TmpLobIndex);
                  SetLength(FBatchLobBuf[TmpLobIndex], Min(fCurrentIterations, fMaxBufArrayBound)); //minimal length
                  PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(ParameterDataPtr)^:= @FBatchLobBuf[TmpLobIndex][fArrayOffSet];
                end else begin
                  PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobArrayIndexOffSet))^ := fArrayOffSet; //write offset to buff -> getdata
                  PPointer(ParameterDataPtr)^:= @TInterfaceDynArray(Value.VArray.VArray)[fArrayOffSet];
                end;
                PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobParameterIndexOffSet))^ := I;
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
                  stBinaryStream: for J := 0 to fCurrentIterations -1 do StrLen_or_IndArray^[J] := SQL_DATA_AT_EXEC;
                  else
                    raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
                end
              end;
            end; //for j := 0 to CurrArrayCount -1 do begin
          end; //if (TZSQLType(Value.VArray.VIsNullArrayType) = fSQLTypes[
        end else begin
          fCurrentIterations := 0; //if Assigned(Value.VArray.VArray)
          fParamInfos[I].ParamDataPtr := nil;
        end;
        if (Value.VArray.VIsNullArray = nil) then
          FillChar(StrLen_or_IndPtr^, fCurrentIterations*SizeOf(SQLLEN), #0); { fast not null indcator.. }
      end else begin //single value binding
        J := 0; //e.g. SetDate/SetTimeStamp
        if fParamInfos[I].ValueCount <> 1 then begin
          fParamInfos[I].ValueCount := 1;
          ParamSetChanged := True;
          MaxArrayCount := Max(1, MaxArrayCount);
          SetLength(fParamInfos[i].BindBuffer, fParamInfos[i].BufferSize+SizeOf(SQLLEN));
          fParamInfos[i].ParamDataPtr := {%H-}Pointer({%H-}NativeUInt(Pointer(fParamInfos[i].BindBuffer))+SizeOf(SQLLEN));
          fParamInfos[i].Bound := False;
        end;
        StrLen_or_IndPtr := Pointer(fParamInfos[i].BindBuffer);
        if Value.VType = vtNull then
           PSQLLEN(StrLen_or_IndPtr)^ := SQL_NULL_DATA
        else begin
          PSQLLEN(StrLen_or_IndPtr)^ := SQL_NO_NULLS;
          ParameterDataPtr := fParamInfos[i].ParamDataPtr;
          case fParamInfos[i].SQLType of
            stBoolean:    PByte(ParameterDataPtr)^ := Ord(ClientVarManager.GetAsBoolean(Value));
            stByte:       PByte(ParameterDataPtr)^ := ClientVarManager.GetAsUInteger(Value);
            stShort:      PShortInt(ParameterDataPtr)^ := ClientVarManager.GetAsInteger(Value);
            stWord:       PWord(ParameterDataPtr)^ := ClientVarManager.GetAsUInteger(Value);
            stSmall:      PSmallInt(ParameterDataPtr)^ := ClientVarManager.GetAsInteger(Value);
            stLongWord:   PLongWord(ParameterDataPtr)^ := ClientVarManager.GetAsUInteger(Value);
            stInteger:    PInteger(ParameterDataPtr)^ := ClientVarManager.GetAsInteger(Value);
            stULong:      PUInt64(ParameterDataPtr)^ := ClientVarManager.GetAsUInteger(Value);
            stLong:       PInt64(ParameterDataPtr)^ := ClientVarManager.GetAsInteger(Value);
            stFloat:      PSingle(ParameterDataPtr)^ := ClientVarManager.GetAsFloat(Value);
            stDouble,
            stCurrency,
            stBigDecimal: PDouble(ParameterDataPtr)^ := ClientVarManager.GetAsFloat(Value);
            stString, stUnicodeString:
              if fParamInfos[I].C_DataType = SQL_C_WCHAR then
              begin
                CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], zCP_UTF16);
                PSQLLEN(StrLen_or_IndPtr)^ := Min(fParamInfos[i].ColumnSize shl 1, CharRec.Len shl 1);
                if fParamInfos[i].InputDataType = SQL_PARAM_INPUT then begin
                  fParamInfos[I].Bound := False;
                  fParamInfos[i].ParamDataPtr := CharRec.P;
                end else
                  System.Move(CharRec.P^, ParameterDataPtr^, PSQLLEN(StrLen_or_IndPtr)^);
                (PWideChar(fParamInfos[i].ParamDataPtr)+(PSQLLEN(StrLen_or_IndPtr)^ shr 1))^ := #0; //set a terminating #0 to top of data
              end else begin
                CharRec := ClientVarManager.GetAsCharRec(InParamValues[i], ConSettings^.ClientCodePage^.CP);
                PSQLLEN(StrLen_or_IndPtr)^ := Min((fParamInfos[i].ColumnSize*ConSettings^.ClientCodePage^.CharWidth), CharRec.Len);
                if fParamInfos[i].InputDataType = SQL_PARAM_INPUT then begin
                  fParamInfos[I].Bound := False; //force new pointer binding
                  fParamInfos[i].ParamDataPtr := CharRec.P;
                end else
                  System.Move(CharRec.P^, ParameterDataPtr^, PSQLLEN(StrLen_or_IndPtr)^);
                (PAnsiChar(fParamInfos[i].ParamDataPtr)+PSQLLEN(StrLen_or_IndPtr)^)^ := #0; //terminate the String if a truncation happens
              end;
            stBytes, stGUID: begin
                if Value.VType <> vtBytes then begin
                  Value := EncodeBytes(ClientVarManager.GetAsBytes(Value));
                  InParamValues[i] := Value;
                end;
                if fParamInfos[i].InputDataType = SQL_PARAM_INPUT then begin
                  fParamInfos[i].ParamDataPtr := Pointer(InParamValues[i].VBytes);
                  PSQLLEN(StrLen_or_IndPtr)^ := Min(Length(InParamValues[i].VBytes), fParamInfos[i].ColumnSize);
                  fParamInfos[I].Bound := False; //force new pointer binding
                end else
                  SetBinary(Value.VBytes, Min(Length(Value.VBytes), fParamInfos[i].ColumnSize));
              end;
            stDate: SetDate(ClientVarManager.GetAsDateTime(Value));
            stTime: SetTime(ClientVarManager.GetAsDateTime(Value));
            stTimestamp: SetTimeStamp(ClientVarManager.GetAsDateTime(Value));
            //stArray, stDataSet,
            stAsciiStream, stUnicodeStream: begin
              PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobArrayIndexOffSet))^ := 0;//initialize a virtual index
              PSQLLEN(StrLen_or_IndPtr)^ := SQL_DATA_AT_EXEC; //indicate streamed mode
              if (not (InParamTypes[i] in [stAsciiStream, stUnicodeStream])) then begin
                Inc(TmpLobIndex);
                SetLength(FBatchLobBuf[TmpLobIndex], 1); //minimal length
                PPointer(ParameterDataPtr)^:= @FBatchLobBuf[TmpLobIndex][0];
              end;
              PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobParameterIndexOffSet))^ := I;
              if fParamInfos[I].C_DataType = SQL_C_WCHAR then
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
                      PPointer(ParameterDataPtr)^ := @InParamValues[I].VInterface;
                    end;
                  else
                    raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
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
                      PPointer(ParameterDataPtr)^ := @InParamValues[I].VInterface;
                    end;
                  else
                    raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
                end;
            end;
          stBinaryStream: begin
              PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobArrayIndexOffSet))^ := 0;//initialize a virtual index
              PSQLLEN(StrLen_or_IndPtr)^ := SQL_DATA_AT_EXEC; //indicate streamed mode
              if (InParamTypes[i] <> stBinaryStream) then begin
                Inc(TmpLobIndex);
                SetLength(FBatchLobBuf[TmpLobIndex], 1); //minimal length
                PPointer(ParameterDataPtr)^:= @FBatchLobBuf[TmpLobIndex][0];
              end;
              PInteger({%H-}Pointer({%H-}NativeUInt(ParameterDataPtr)+LobParameterIndexOffSet))^ := I;
              case InParamTypes[i] of
                stGUID, stBytes: SetBlobFromBts(ClientVarManager.GetAsBytes(Value));
                stBinaryStream: PPointer(ParameterDataPtr)^ := @InParamValues[I].VInterface;
                else
                  raise EZSQLException.Create(IntToStr(Ord(fParamInfos[i].SQLType))+' '+SUnsupportedParameterType);
              end;
            end;
          end
        end;
      end;
      if not fParamInfos[I].Bound then begin //..Bindings remain in effect until the application calls SQLBindParameter again...oslt.
        CheckStmtError(fPlainDriver.BindParameter(fHSTMT, I+1,//0=bookmark and Params do starts with 1
          fParamInfos[i].InputDataType, fParamInfos[I].C_DataType, fParamInfos[i].DataType, fParamInfos[i].ColumnSize, 0,
            fParamInfos[i].ParamDataPtr, fParamInfos[i].BufferSize*Ord(not (fParamInfos[i].SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream])), StrLen_or_IndPtr));
        fParamInfos[I].Bound := True;
      end;
    end;
    if ParamSetChanged then
      if (fMaxBufArrayBound = -1) then
        CheckStmtError(fPlainDriver.SetStmtAttr(fHSTMT, SQL_ATTR_PARAMSET_SIZE, Pointer(1), 0))
      else
        CheckStmtError(fPlainDriver.SetStmtAttr(fHSTMT, SQL_ATTR_PARAMSET_SIZE, {%H-}Pointer(NativeUInt(fCurrentIterations)), 0));
  end;
end;

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
  CheckStmtError(fPlainDriver.FreeStmt(fHSTMT,SQL_CLOSE)); //handle a get data issue
  RETCODE := fPlainDriver.Execute(fHSTMT);
  while RETCODE = SQL_NEED_DATA do begin
    RETCODE := fPlainDriver.ParamData(fHSTMT, @ValuePtr);
    if RetCode <> SQL_NEED_DATA then break;
    Assert(Assigned(ValuePtr), 'wrong descriptor token');
    PRowIndex := {%H-}Pointer({%H-}NativeUInt(ValuePtr)+LobArrayIndexOffSet);
    TempBlob := IInterface(Pointer(PLobArray(PPointer(ValuePtr)^)[PRowIndex^])) as IZBlob; //note ValuePtr is a user defined token we also could use the columnNumber on binding the column -> this is faster
    if (TempBlob = nil) or TempBlob.IsEmpty then begin
      StrLen_or_Ind := SQL_NULL_DATA;
      CheckStmtError(fPlainDriver.PutData(fHSTMT, nil, StrLen_or_Ind)); //set to null
    end else begin
      Buf := TempBlob.GetBuffer;
      { put data chunked }
      StrLen_or_Ind := Min(ChunkSize, TempBlob.Length);
      for i := 1 to TempBlob.Length div ChunkSize do begin
        CheckStmtError(fPlainDriver.PutData(fHSTMT, Buf, StrLen_or_Ind));
        Inc(Buf, ChunkSize);
      end;
      StrLen_or_Ind := TempBlob.Length - NativeInt(({%H-}NativeUInt(Buf)-{%H-}NativeUInt(TempBlob.GetBuffer)));
      CheckStmtError(fPlainDriver.PutData(fHSTMT, Buf, StrLen_or_Ind)); //final chunk
    end;
    inc(PRowIndex^);
  end;
  { roll back PRowIndex^ misses yet}
  if RETCODE = SQL_PARAM_DATA_AVAILABLE then begin //check output params ...
    RETCODE2 := fPlainDriver.MoreResults(fHSTMT);
    if RETCODE2 = SQL_NO_DATA then
      //???
    else begin
      { get data chunked }
      CheckStmtError(RETCODE2);
      CheckStmtError(fPlainDriver.ParamData(fHSTMT, @ValuePtr));
      Assert(Assigned(ValuePtr), 'wrong descriptor pointer');
      TempBlob := IZBlob(ValuePtr);
    end;
  end else if not RETCODE in [SQL_NO_DATA, SQL_SUCCESS] then
    CheckStmtError(RetCode);
end;

procedure TZAbstractODBCStatement.PrepareInParameters;
var
  ParameterNumber: SQLUSMALLINT;
  ParameterCount: SQLSMALLINT;
  NoLobBoundParamCount, RowSize: Integer;
  AllParamsAreArrays: Boolean;
begin
  CheckStmtError(fPlainDriver.NumParams(fHSTMT, @ParameterCount));
  if ParameterCount > 0 then begin
    if ParameterCount <> InParamCount then
      raise EZSQLException.Create(SInvalidInputParameterCount);
    SetLength(fParamInfos, ParameterCount);
    NoLobBoundParamCount := 0; RowSize := 0;
    AllParamsAreArrays := False;
    for ParameterNumber := 0 to ParameterCount-1 do begin
      CheckStmtError(fPlainDriver.DescribeParam(fHSTMT, ParameterNumber +1, //0=bookmark and Params do starts with 1
        @fParamInfos[ParameterNumber].DataType, @fParamInfos[ParameterNumber].ColumnSize,
        @fParamInfos[ParameterNumber].DecimalDigits, @fParamInfos[ParameterNumber].Nullable));
      //get "best" TZSQLType -> ODBC does not returns the C-Data types
      fParamInfos[ParameterNumber].SQLType := ConvertODBCTypeToSQLType(fParamInfos[ParameterNumber].DataType,
                                      InParamTypes[ParameterNumber], Consettings^.CPType);
      //now assign minimal conversion datatypes
      fParamInfos[ParameterNumber].DataType := ConvertSQLTypeToODBCType(fParamInfos[ParameterNumber].SQLType,
                                      fParamInfos[ParameterNumber].DataType, ConSettings^.ClientCodePage^.Encoding);
      fParamInfos[ParameterNumber].C_DataType := SQL2ODBC_Types[ConSettings^.ClientCodePage^.Encoding = ceUTF16][fParamInfos[ParameterNumber].SQLType];
      Inc(NoLobBoundParamCount, Ord((fParamInfos[ParameterNumber].SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) and
         (not (InParamTypes[ParameterNumber] in [stAsciiStream, stUnicodeStream, stBinaryStream]))));
      //note: Code is prepared to handle any case of Param-Directions
      fParamInfos[ParameterNumber].InputDataType := ParamTypeToODBCParamType(pctIn, fParamInfos[ParameterNumber].SQLType, fStreamSupport);
      fParamInfos[ParameterNumber].BufferSize := CalcBufSize(fParamInfos[ParameterNumber].ColumnSize, fParamInfos[ParameterNumber].SQLType, ConSettings^.ClientCodePage);
      AllParamsAreArrays := ((ParameterNumber = 0) and (InParamValues[ParameterNumber].VType = vtArray)) or
                              (AllParamsAreArrays and (InParamValues[ParameterNumber].VType = vtArray));
      inc(RowSize, fParamInfos[ParameterNumber].BufferSize+SizeOf(SQLLEN))
    end;
    SetLength(fBatchLobBuf, NoLobBoundParamCount);
    if AllParamsAreArrays then
      fMaxBufArrayBound := Max(1, fZBufferSize div RowSize)
    else
      fMaxBufArrayBound := -1;
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

procedure TZAbstractODBCStatement.SetNullArray(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value; const VariantType: TZVariantType);
begin
  if Assigned(Pointer(Value)) and (SQLType <> stBoolean) then
    raise EZSQLException.Create('Unsupported Null-Indicator Array: supported is stBoolean')
  else
    inherited SetNullArray(ParameterIndex, SQLType, Value, VariantType);
end;

function TZAbstractODBCStatement.SupportsSingleColumnArrays: Boolean;
begin
  Result := (GetConnection as IZODBCConnection).GetArraySelectSupported;
end;

procedure TZAbstractODBCStatement.Unprepare;
begin
  inherited Unprepare;
  if Assigned(fHSTMT) then
  begin
    CheckStmtError(fPlainDriver.FreeHandle(SQL_HANDLE_STMT, fHSTMT)); //<- does the trick to get a instance reused
    fHSTMT := nil;
  end;
end;

procedure TZAbstractODBCStatement.UnPrepareInParameters;
begin
  SetLength(fParamInfos, 0);
  SetLength(fBatchLobBuf, 0);
  if Assigned(fHSTMT) and Assigned(fPHDBC^) then
    CheckStmtError(fPlainDriver.FreeStmt(fHSTMT,SQL_RESET_PARAMS));
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
    CheckStmtError((fPlainDriver as IODBC3UnicodePlainDriver).Prepare(fHSTMT, Pointer(WSQL), Length(WSQL)));
    inherited Prepare;
  end else
    if Assigned(fHSTMT) and Assigned(fPHDBC^) then
      CheckStmtError(fPlainDriver.FreeStmt(fHSTMT,SQL_CLOSE));
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
    CheckStmtError((fPlainDriver as IODBC3RawPlainDriver).Prepare(fHSTMT, Pointer(ASQL), Length(ASQL)));
    inherited Prepare;
  end else
    if Assigned(fHSTMT) and Assigned(fPHDBC^) then
      CheckStmtError(fPlainDriver.FreeStmt(fHSTMT,SQL_CLOSE));
end;

end.


