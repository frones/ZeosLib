{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Firebird Database Connectivity Classes         }
{                                                         }
{           Originally written by EgonHugeist             }
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

unit ZDbcFirebirdStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_FIREBIRD} //if set we have an empty unit

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} FmtBCD, Types, SysUtils,
  ZCompatibility, Firebird, ZDbcIntfs, ZClasses, ZDbcStatement, ZDbcFirebird,
  ZDbcInterbase6Utils, ZPlainFirebirdDriver;

type
  {** Implements Prepared SQL Statement for Firebird. }
  TZFirebirdPreparedStatement = class;

  {** record for holding batch dml stmts }
  TZFBStmt = record
    Obj: TZFirebirdPreparedStatement;
    PreparedRowsOfArray: Integer;
  end;

  {** Implements a abstract Statement for Firebird. }
  TZAbstractFirebirdStatement = Class(TZRawParamDetectPreparedStatement)
  private
    FBatchStmts: array[Boolean] of TZFBStmt;
    FMaxRowsPerBatch: Integer;
    FFBConnection: IZFirebirdConnection;
    FAttachment: IAttachment;
    FFBStatement: IStatement;
    FFBTransaction: ITransaction;
    FStatus: IStatus;
    FDialect: Cardinal;
    FInData, FOutData: Pointer;
    FStatementType: TZIbSqlStatementType;
    FInMessageMetadata, FOutMessageMetadata: IMessageMetadata;
    FPlainDriver: TZFirebird3UpPlainDriver;
    FDB_CP_ID: Cardinal;
    FCodePageArray: TWordDynArray;
    FTypeTokens: TRawByteStringDynArray;
    FResultSet: IResultSet;
  protected
    function CreateResultSet: IZResultSet;
    procedure ExecuteInternal;
    procedure ExceuteBatch;
    function SplittQuery(const SQL: SQLString): RawByteString;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
  public
    Constructor Create(const Connection: IZFirebirdConnection;
      const SQL: String; Info: TStrings);
    Destructor Destroy; override;
  public
    procedure Prepare; override;
    procedure Unprepare; override;
  public
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  {** Implements a regular Statement for Firebird. }

  TZFirebirdStatement = Class(TZAbstractFirebirdStatement)
  public
    Constructor Create(const Connection: IZFirebirdConnection;
      Info: TStrings);
  end;

  {** Implements IZPreparedStatement for Firebird. }
  TZFirebirdPreparedStatement = class(TZAbstractFirebirdStatement, IZPreparedStatement)
  private
    procedure InternalBindDouble(Index: Cardinal; const Value: Double);
    procedure SetPAnsiChar(Index: Cardinal; Value: PAnsiChar; Len: LengthInt);
    procedure SetPWideChar(Index: Cardinal; Value: PWideChar; Len: LengthInt);
    procedure WriteLobBuffer(Index: Cardinal; P: PAnsiChar; Len: NativeUInt);
    function CreateConversionError(Index: Cardinal; Current: TZSQLType): EZSQLException;
  protected
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure CheckParameterIndex(var Value: Integer); override;
  public
    procedure Unprepare; override;
  public //setters
    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); override;
    //a performance thing: direct dispatched methods for the interfaces :
    //https://stackoverflow.com/questions/36137977/are-interface-methods-always-virtual
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    procedure SetBoolean(Index: Integer; Value: Boolean);
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetShort(Index: Integer; Value: ShortInt);
    procedure SetWord(Index: Integer; Value: Word);
    procedure SetSmall(Index: Integer; Value: SmallInt);
    procedure SetUInt(Index: Integer; Value: Cardinal);
    procedure SetInt(Index: Integer; Value: Integer);
    procedure SetULong(Index: Integer; const Value: UInt64);
    procedure SetLong(Index: Integer; const Value: Int64);
    procedure SetFloat(Index: Integer; Value: Single);
    procedure SetDouble(Index: Integer; const Value: Double);
    procedure SetCurrency(Index: Integer; const Value: Currency);
    procedure SetBigDecimal(Index: Integer; const Value: TBCD);

    procedure SetCharRec(Index: Integer; const Value: TZCharRec); reintroduce;
    procedure SetString(Index: Integer; const Value: String); reintroduce;
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(Index: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(Index: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(Index: Integer; const Value: RawByteString); reintroduce;
    procedure SetUnicodeString(Index: Integer; const Value: ZWideString); reintroduce;

    procedure SetDate(Index: Integer; const Value: TZDate); reintroduce; overload;
    procedure SetTime(Index: Integer; const Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(Index: Integer; const Value: TZTimeStamp); reintroduce; overload;

    procedure SetBytes(Index: Integer; const Value: TBytes); reintroduce; overload;
    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
    procedure SetGUID(Index: Integer; const Value: TGUID); reintroduce;
    procedure SetBlob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override{keep it virtual because of (set)ascii/uniocde/binary streams};
  end;

  {** Implements IZCallableStatement for Firebird. }
  TZFirebirdCallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

{$ENDIF ZEOS_DISABLE_FIREBIRD}
implementation
{$IFNDEF ZEOS_DISABLE_FIREBIRD}

uses Math,
  ZMessages, ZSysUtils, ZFastCode, ZEncoding, ZVariant, ZTokenizer,
  ZPlainFirebirdInterbaseConstants,
  ZDbcLogging, ZDbcFirebirdResultSet, ZDbcFirebirdInterbase, ZDbcResultSet,
  ZDbcUtils;

procedure BindSQLDAInParameters(BindList: TZBindList;
  Stmt: TZFirebirdPreparedStatement; ArrayOffSet, ArrayItersCount: Integer);
var
  I, J, ParamIndex: Integer;
  IsNull: Boolean;
  { array DML bindings }
  ZData: Pointer; //array entry
begin
  ParamIndex := FirstDbcIndex;
  for J := ArrayOffSet to ArrayOffSet+ArrayItersCount-1 do
    for i := 0 to BindList.Count -1 do
    begin
      IsNull := IsNullFromArray(BindList[i].Value, J);
      ZData := PZArray(BindList[i].Value).VArray;
      if (ZData = nil) or (IsNull) then
        Stmt.SetNull(ParamIndex, ZDbcIntfs.stUnknown)
      else
        case TZSQLType(PZArray(BindList[i].Value).VArrayType) of
          stBoolean: Stmt.SetBoolean(ParamIndex, TBooleanDynArray(ZData)[J]);
          stByte: Stmt.SetSmall(ParamIndex, TByteDynArray(ZData)[J]);
          stShort: Stmt.SetSmall(ParamIndex, TShortIntDynArray(ZData)[J]);
          stWord: Stmt.SetInt(ParamIndex, TWordDynArray(ZData)[J]);
          stSmall: Stmt.SetSmall(ParamIndex, TSmallIntDynArray(ZData)[J]);
          stLongWord: Stmt.SetLong(ParamIndex, TLongWordDynArray(ZData)[J]);
          stInteger: Stmt.SetInt(ParamIndex, TIntegerDynArray(ZData)[J]);
          stLong: Stmt.SetLong(ParamIndex, TInt64DynArray(ZData)[J]);
          stULong: Stmt.SetLong(ParamIndex, TUInt64DynArray(ZData)[J]);
          stFloat: Stmt.SetFloat(ParamIndex, TSingleDynArray(ZData)[J]);
          stDouble: Stmt.SetDouble(ParamIndex, TDoubleDynArray(ZData)[J]);
          stCurrency: Stmt.SetCurrency(ParamIndex, TCurrencyDynArray(ZData)[J]);
          stBigDecimal: Stmt.SetBigDecimal(ParamIndex, TBCDDynArray(ZData)[J]);
          stGUID: Stmt.SetGUID(ParamIndex, TGUIDDynArray(ZData)[j]);
          stString, stUnicodeString:
                case PZArray(BindList[i].Value).VArrayVariantType of
                  vtString: Stmt.SetString(ParamIndex, TStringDynArray(ZData)[j]);
                  {$IFNDEF NO_ANSISTRING}
                  vtAnsiString: Stmt.SetAnsiString(ParamIndex, TAnsiStringDynArray(ZData)[j]);
                  {$ENDIF}
                  {$IFNDEF NO_UTF8STRING}
                  vtUTF8String: Stmt.SetUTF8String(ParamIndex, TUTF8StringDynArray(ZData)[j]);
                  {$ENDIF}
                  vtRawByteString: Stmt.SetRawByteString(ParamIndex, TRawByteStringDynArray(ZData)[j]);
                  vtUnicodeString: Stmt.SetUnicodeString(ParamIndex, TUnicodeStringDynArray(ZData)[j]);
                  vtCharRec: Stmt.SetCharRec(ParamIndex, TZCharRecDynArray(ZData)[j]);
                  else
                    raise Exception.Create('Unsupported String Variant');
                end;
          stBytes:      Stmt.SetBytes(ParamIndex, TBytesDynArray(ZData)[j]);
          stDate:       if PZArray(BindList[i].Value).VArrayVariantType = vtDate
                        then Stmt.SetDate(ParamIndex, TZDateDynArray(ZData)[j])
                        else Stmt.SetDate(ParamIndex, TDateTimeDynArray(ZData)[j]);
          stTime:       if PZArray(BindList[i].Value).VArrayVariantType = vtTime
                        then Stmt.SetTime(ParamIndex, TZTimeDynArray(ZData)[j])
                        else Stmt.SetTime(ParamIndex, TDateTimeDynArray(ZData)[j]);
          stTimestamp:  if PZArray(BindList[i].Value).VArrayVariantType = vtTimeStamp
                        then Stmt.SetTimestamp(ParamIndex, TZTimeStampDynArray(ZData)[j])
                        else Stmt.SetTimestamp(ParamIndex, TDateTimeDynArray(ZData)[j]);
          stAsciiStream,
          stUnicodeStream,
          stBinaryStream: Stmt.SetBlob(ParamIndex, TZSQLType(PZArray(BindList[i].Value).VArrayType), TInterfaceDynArray(ZData)[j] as IZBlob);
          else
            raise EZIBConvertError.Create(SUnsupportedParameterType);
        end;
      Inc(ParamIndex);
    end;
end;

const
  EBStart = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('EXECUTE BLOCK(');
  EBBegin =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}(')AS BEGIN'+LineEnding);
  EBSuspend =  {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('SUSPEND;'+LineEnding); //required for RETURNING syntax
  EBEnd = {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}('END');
  LBlockLen = Length(EBStart)+Length(EBBegin)+Length(EBEnd);
  cRETURNING: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = ('RETURNING');
function GetExecuteBlockString(const Stmt: TZAbstractFirebirdStatement;
  const IsParamIndexArray: TBooleanDynArray;
  const InParamCount, RemainingArrayRows: Integer;
  const CurrentSQLTokens: TRawByteStringDynArray;
  const PlainDriver: TZFirebird3UpPlainDriver;
  var PreparedRowsOfArray,MaxRowsPerBatch: Integer;
  var TypeTokens: TRawByteStringDynArray;
  InitialStatementType: TZIbSqlStatementType;
  const XSQLDAMaxSize: Cardinal): RawByteString;
var
  IndexName, ArrayName, tmp: RawByteString;
  ParamIndex, J: Cardinal;
  I, BindCount, ParamNameLen, SingleStmtLength, LastStmLen,
  HeaderLen, FullHeaderLen, StmtLength:  Integer;
  CodePageInfo: PZCodePage;
  PStmts, PResult, P: PAnsiChar;
  ReturningFound: Boolean;
  MemPerRow: Cardinal;

  procedure Put(const Args: array of RawByteString; var Dest: PAnsiChar);
  var I: Integer;
    L: LengthInt;
  begin
    for I := low(Args) to high(Args) do //Move data
      if Pointer(Args[i]) <> nil then begin
        L := {%H-}PLengthInt(NativeUInt(Args[i]) - StringLenOffSet)^;
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Args[i])^, Dest^, L);
        Inc(Dest, L);
      end;
  end;
  procedure AddParam(const Args: array of RawByteString; var Dest: RawByteString);
  var I, L: Integer;
    P: PAnsiChar;
  begin
    Dest := ''; L := 0;
    for I := low(Args) to high(Args) do //Calc String Length
      Inc(L ,Length(Args[i]));
    SetLength(Dest, L);
    P := Pointer(Dest);
    Put(Args, P);
  end;
begin
  if Pointer(TypeTokens) = nil then
  begin
    BindCount := Stmt.FInMessageMetadata.GetCount(Stmt.FStatus);
    Assert(InParamCount=BindCount, 'ParamCount missmatch');
    SetLength(TypeTokens, BindCount);
    MemPerRow := XSQLDA_LENGTH(BindCount);
    for ParamIndex := 0 to BindCount-1 do
    begin
      case Stmt.FInMessageMetadata.GetType(Stmt.FStatus, ParamIndex) and not (1) of
        SQL_VARYING, SQL_TEXT:
          begin
            CodePageInfo := Stmt.FPlainDriver.ValidateCharEncoding(Word(Stmt.FInMessageMetadata.GetCharSet(Stmt.FStatus, ParamIndex)) and 255);
            AddParam([' VARCHAR(', IntToRaw(Stmt.FInMessageMetadata.GetLength(Stmt.FStatus, ParamIndex) div CodePageInfo.CharWidth),
            ') CHARACTER SET ', {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(CodePageInfo.Name), '=?' ], TypeTokens[ParamIndex]);
          end;
        SQL_DOUBLE, SQL_D_FLOAT:
           AddParam([' DOUBLE PRECISION=?'], TypeTokens[ParamIndex]);
        SQL_FLOAT:
           AddParam([' FLOAT=?'],TypeTokens[ParamIndex]);
        SQL_LONG:
          if Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex) = 0 then
            AddParam([' INTEGER=?'],TypeTokens[ParamIndex])
          else begin
            tmp := IntToRaw(-Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex));
            if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(9,', Tmp,')=?'], TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(9,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_SHORT:
          if Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex) = 0 then
            AddParam([' SMALLINT=?'],TypeTokens[ParamIndex])
          else begin
            tmp := IntToRaw(-Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex));
            if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(4,', Tmp,')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(4,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_TIMESTAMP:
           AddParam([' TIMESTAMP=?'],TypeTokens[ParamIndex]);
        SQL_BLOB:
          if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = isc_blob_text then
            AddParam([' BLOB SUB_TYPE TEXT=?'],TypeTokens[ParamIndex])
          else
            AddParam([' BLOB=?'],TypeTokens[ParamIndex]);
        //SQL_ARRAY                      = 540;
        //SQL_QUAD                       = 550;
        SQL_TYPE_TIME:
           AddParam([' TIME=?'],TypeTokens[ParamIndex]);
        SQL_TYPE_DATE:
           AddParam([' DATE=?'],TypeTokens[ParamIndex]);
        SQL_INT64: // IB7
          if Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex) = 0 then
            AddParam([' BIGINT=?'],TypeTokens[ParamIndex])
          else begin
            tmp := IntToRaw(-Stmt.FInMessageMetadata.GetScale(Stmt.FStatus, ParamIndex));
            if Stmt.FInMessageMetadata.GetSubType(Stmt.FStatus, ParamIndex) = RDB_NUMBERS_NUMERIC then
              AddParam([' NUMERIC(18,', Tmp,')=?'],TypeTokens[ParamIndex])
            else
              AddParam([' DECIMAL(18,', Tmp,')=?'],TypeTokens[ParamIndex]);
          end;
        SQL_BOOLEAN, SQL_BOOLEAN_FB{FB30}:
           AddParam([' BOOLEAN=?'],TypeTokens[ParamIndex]);
        SQL_NULL{FB25}:
           AddParam([' CHAR(1)=?'],TypeTokens[ParamIndex]);
      end;
      (*Inc(MemPerRow, ParamsSQLDA.GetFieldLength(ParamIndex) +
        2*Ord((ParamsSQLDA.GetIbSqlType(ParamIndex) and not 1) = SQL_VARYING));*)
    end;
  end;
  {now let's calc length of stmt to know if we can bound all array data or if we need some more calls}
  StmtLength := 0;
  FullHeaderLen := 0;
  ReturningFound := False;
  PreparedRowsOfArray := 0;

  for J := 0 to RemainingArrayRows -1 do
  begin
    ParamIndex := 0;
    SingleStmtLength := 0;
    LastStmLen := StmtLength;
    HeaderLen := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do begin
      if IsParamIndexArray[i] then begin //calc Parameters size
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        ParamNameLen := {P}1+GetOrdinalDigits(ParamIndex)+1{_}+GetOrdinalDigits(j);
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        {inc header}
        Inc(HeaderLen, ParamNameLen+ {%H-}PLengthInt(NativeUInt(TypeTokens[ParamIndex]) - StringLenOffSet)^+Ord(not ((ParamIndex = 0) and (J=0))){,});
        {inc stmt}
        Inc(SingleStmtLength, 1+{:}ParamNameLen);
        Inc(ParamIndex);
      end else begin
        Inc(SingleStmtLength, {%H-}PLengthInt(NativeUInt(CurrentSQLTokens[i]) - StringLenOffSet)^);
        P := Pointer(CurrentSQLTokens[i]);
        if not ReturningFound and (Ord(P^) in [Ord('R'), Ord('r')]) and (Length(CurrentSQLTokens[i]) = Length(cRETURNING)) then begin
          ReturningFound := ZSysUtils.SameText(P, Pointer(cReturning), Length(cRETURNING));
          Inc(StmtLength, Ord(ReturningFound)*Length(EBSuspend));
        end;
      end;
    end;
    Inc(SingleStmtLength, 1{;}+Length(LineEnding));
    if MaxRowsPerBatch = 0 then //calc maximum batch count if not set already
      MaxRowsPerBatch := Min((XSQLDAMaxSize div Int64(MemPerRow)),     {memory limit of XSQLDA structs}
        (((32*1024)-LBlockLen) div Int64(HeaderLen+SingleStmtLength)))+1; {32KB limited Also with FB3};
    Inc(StmtLength, HeaderLen+SingleStmtLength);
    Inc(FullHeaderLen, HeaderLen);
    //we run into XSQLDA !update! count limit of 255 see:
    //http://tracker.firebirdsql.org/browse/CORE-3027?page=com.atlassian.jira.plugin.system.issuetabpanels%3Aall-tabpanel
    if (PreparedRowsOfArray = MaxRowsPerBatch-1) or
       ((InitialStatementType = stInsert) and (PreparedRowsOfArray > 255)) or
       ((InitialStatementType <> stInsert) and (PreparedRowsOfArray > 125)) then begin
      StmtLength := LastStmLen;
      Dec(FullHeaderLen, HeaderLen);
      Break;
    end else
      PreparedRowsOfArray := J;
  end;

  {EH: now move our data to result ! ONE ALLOC ! of result (: }
  SetLength(Result, StmtLength+LBlockLen);
  PResult := Pointer(Result);
  Put([EBStart], PResult);
  PStmts := PResult + FullHeaderLen+Length(EBBegin);
  for J := 0 to PreparedRowsOfArray do begin
    ParamIndex := 0;
    for i := low(CurrentSQLTokens) to high(CurrentSQLTokens) do begin
      if IsParamIndexArray[i] then begin
        IndexName := IntToRaw(ParamIndex);
        ArrayName := IntToRaw(J);
        Put([':P', IndexName, '_', ArrayName], PStmts);
        if (ParamIndex = 0) and (J=0)
        then Put(['P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult)
        else Put([',P', IndexName, '_', ArrayName, TypeTokens[ParamIndex]], PResult);
        Inc(ParamIndex);
      end else
        Put([CurrentSQLTokens[i]], PStmts);
    end;
    Put([';',LineEnding], PStmts);
  end;
  Put([EBBegin], PResult);
  if ReturningFound then
    Put([EBSuspend], PStmts);
  Put([EBEnd], PStmts);
  Inc(PreparedRowsOfArray);
end;

{ TZAbstractFirebirdStatement }

constructor TZAbstractFirebirdStatement.Create(
  const Connection: IZFirebirdConnection; const SQL: String; Info: TStrings);
begin
  Self.ConSettings := Connection.GetConSettings;
  FDB_CP_ID := ConSettings^.ClientCodePage^.ID;
  inherited Create(Connection, SQL, Info);
  FFBConnection := Connection;
  FAttachment := Connection.GetAttachment;
  FAttachment.addRef;
  FStatus := Connection.GetStatus;
  FDialect := Connection.GetDialect;
  FPlainDriver := FFBConnection.GetPlainDriver;
  FCodePageArray := FPlainDriver.GetCodePageArray;
  FCodePageArray[FDB_CP_ID] := ConSettings^.ClientCodePage^.CP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250
end;

{**
  destroys this object and releases all memory
}
function TZAbstractFirebirdStatement.CreateResultSet: IZResultSet;
var
  NativeResultSet: TZAbstractFirebirdResultSet;
  CachedResolver: TZInterbaseFirebirdCachedResolver;
  CachedResultSet: TZFirebirdCachedResultSet;
begin
  if FOpenResultSet <> nil then
    Result := IZResultSet(FOpenResultSet)
  else begin
    if FResultSet <> nil
    then NativeResultSet := TZFirebirdResultSet.Create(Self, SQL, FFBStatement, FOutMessageMetadata, FStatus, @FResultSet)
    else NativeResultSet := TZFirebirdOutParamResultSet.Create(Self, SQL, FFBStatement, FOutMessageMetadata, FStatus, FOutData);
    if (GetResultSetConcurrency = rcUpdatable) then begin
      CachedResolver := TZInterbaseFirebirdCachedResolver.Create(Self, NativeResultSet.GetMetadata);
      CachedResultSet := TZFirebirdCachedResultSet.Create(NativeResultSet, SQL, CachedResolver, ConSettings);
      CachedResultSet.SetConcurrency(rcUpdatable);
      Result := CachedResultSet;
    end else
      Result := NativeResultSet;
    NativeResultSet.TransactionResultSet := Pointer(Result);
    NativeResultSet.RegisterCursor;
    FOpenResultSet := Pointer(Result);
  end;
end;

destructor TZAbstractFirebirdStatement.Destroy;
begin
  inherited;
  FAttachment.release;
end;

procedure TZAbstractFirebirdStatement.ExceuteBatch;
var ArrayOffSet: Integer;
begin
  Connection.StartTransaction;
  ArrayOffSet := 0;
  try
    if (FBatchStmts[True].Obj <> nil) and (BatchDMLArrayCount >= FBatchStmts[True].PreparedRowsOfArray) then
      while (ArrayOffSet+FBatchStmts[True].PreparedRowsOfArray <= BatchDMLArrayCount) do begin
        BindSQLDAInParameters(BindList, FBatchStmts[True].Obj,
          ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
        FBatchStmts[True].Obj.ExecuteInternal;
        Inc(ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
      end;
    if (FBatchStmts[False].Obj <> nil) and (ArrayOffSet < BatchDMLArrayCount) then begin
      BindSQLDAInParameters(BindList, FBatchStmts[False].Obj,
        ArrayOffSet, FBatchStmts[False].PreparedRowsOfArray);
      FBatchStmts[False].Obj.ExecuteInternal;
    end;
    Connection.Commit;
  except
    Connection.Rollback;
    raise;
  end;
  LastUpdateCount := BatchDMLArrayCount;
end;

procedure TZAbstractFirebirdStatement.ExecuteInternal;
var flags: Cardinal;
begin
  if BatchDMLArrayCount = 0 then begin
    FFBTransaction := FFBConnection.GetActiveTransaction.GetTransaction;
    if FStatementType in [stSelect, stSelectForUpdate] then begin
      if (GetResultSetType <> rtForwardOnly) and (GetResultSetConcurrency = rcReadOnly)
      then flags := IStatement.CURSOR_TYPE_SCROLLABLE
      else flags := 0;
      FResultSet := FFBStatement.openCursor(FStatus, FFBTransaction,
        FInMessageMetadata, FInData, FOutMessageMetadata, flags)
    end else FFBTransaction := FFBStatement.execute(FStatus, FFBTransaction,
      FInMessageMetadata, FInData, FOutMessageMetadata, FOutData);
    if (FStatus.getState and IStatus.STATE_ERRORS) <> 0 then
      FFBConnection.HandleError(FStatus, fASQL, Self, lcExecute);
  end else ExceuteBatch;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractFirebirdStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  ExecuteInternal;
  { Create ResultSet if possible else free Statement Handle }
  if (FStatementType in [stSelect, stExecProc, stSelectForUpdate]) and (FOutMessageMetadata.getCount(FStatus) <> 0) then begin
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
    if (FStatementType = stExecProc) or BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := LastResultSet;
  end else
    LastResultSet := nil;
  LastUpdateCount := FFBStatement.getAffectedRecords(FStatus);
  Result := LastResultSet <> nil;
  inherited ExecutePrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractFirebirdStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  ExecuteInternal;

  if (FOutMessageMetadata <> nil) and (FOutMessageMetadata.getCount(FStatus) <> 0) then begin
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

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or -1 for SQL statements that return nothing
}
function TZAbstractFirebirdStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  LastResultSet := nil;
  PrepareOpenResultSetForReUse;
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  ExecuteInternal;
  Result := LastUpdateCount;
  if BatchDMLArrayCount = 0 then begin
    case FStatementType of
      stSelect, stSelectForUpdate: if BindList.HasOutParam then begin
          FOutParamResultSet := CreateResultSet;
          FOpenResultSet := nil;
        end else if FResultSet <> nil then begin
          FResultSet.Close(FStatus);
          if (FStatus.getState and IStatus.STATE_ERRORS) <> 0 then
            FFBConnection.HandleError(FStatus, fASQL, Self, lcExecute);
          FResultSet.Release;
        end;
      stExecProc: begin{ Create ResultSet if possible }
          if FOutMessageMetadata.getCount(FStatus) <> 0 then
            FOutParamResultSet := CreateResultSet;
          Result := FFBStatement.getAffectedRecords(FStatus)
        end;
      stInsert, stUpdate, stDelete: Result := FFBStatement.getAffectedRecords(FStatus);
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
    end;
    LastUpdateCount := Result;
  end;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

function TZAbstractFirebirdStatement.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
begin
  if (BatchDMLArrayCount > 0) or ConSettings^.AutoEncode or (FDB_CP_ID = CS_NONE)
  then Result := SplittQuery(SQL)
  else Result := ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
end;

{**
  prepares the statement on the server
}
procedure TZAbstractFirebirdStatement.Prepare;
var Transaction: ITransaction;
  flags: Cardinal;
  PreparedRowsOfArray: Integer;
  FinalChunkSize: Integer;
label jmpEB;
  procedure PrepareArrayStmt(var Slot: TZFBStmt);
  begin
    if (Slot.Obj = nil) or (Slot.PreparedRowsOfArray <> PreparedRowsOfArray) then begin
      if Slot.Obj <> nil then begin
        Slot.Obj.BindList.Count := 0;
        {$IFNDEF AUTOREFCOUNT}
        Slot.Obj._Release;
        {$ENDIF}
        Slot.Obj := nil;
      end;
      Slot.Obj := TZFirebirdPreparedStatement.Create(FFBConnection, '', Info);
      {$IFNDEF AUTOREFCOUNT}
      Slot.Obj._AddRef;
      {$ENDIF}
      Slot.Obj.FASQL := FRawTemp;
      Slot.Obj.BindList.Count := BindList.Count*PreparedRowsOfArray;
      Slot.PreparedRowsOfArray := PreparedRowsOfArray;
      Slot.Obj.Prepare;
    end;
  end;
  procedure PrepareFinalChunk(Rows: Integer);
  begin
    FRawTemp := GetExecuteBlockString(Self,
      IsParamIndex, BindList.Count, Rows, FCachedQueryRaw,
      FPlainDriver, PreparedRowsOfArray, FMaxRowsPerBatch,
      FTypeTokens, FStatementType, FFBConnection.GetXSQLDAMaxSize);
    PrepareArrayStmt(FBatchStmts[False]);
  end;
  procedure SplitQueryIntoPieces;
  var CurrentCS_ID: Integer;
  begin
    CurrentCS_ID := FDB_CP_ID;
    try
      FDB_CP_ID := CS_NONE;
      GetRawEncodedSQL(SQL);
    finally
      FDB_CP_ID := CurrentCS_ID;
    end;
  end;
begin
  if not Prepared then begin
    Transaction := FFBConnection.GetActiveTransaction.GetTransaction;
    if FWeakIntfPtrOfIPrepStmt <> nil
    then flags := IStatement.PREPARE_PREFETCH_METADATA
    else flags := IStatement.PREPARE_PREFETCH_TYPE or IStatement.PREPARE_PREFETCH_OUTPUT_PARAMETERS;
    FFBStatement := FAttachment.prepare(FStatus, Transaction, Length(fASQL),
      Pointer(fASQL), FDialect, flags);
    if (FStatus.getState and IStatus.STATE_ERRORS) <> 0 then
      FFBConnection.HandleError(FStatus, fASQL, Self, lcExecute);
    FStatementType := TZIbSqlStatementType(FFBStatement.getType(FStatus));
    FOutMessageMetadata := FFBStatement.getOutputMetadata(FStatus);
    if (FStatementType = stExecProc) and (FOutMessageMetadata <> nil) then
      GetMem(FOutData, FOutMessageMetadata.getMessageLength(FStatus));
    //FFBStatement.addRef;
    inherited Prepare;
  end;
  if BatchDMLArrayCount > 0 then begin
    //if not done already then split our query into pieces to build the
    //exceute block query
    if (FCachedQueryRaw = nil) then
      SplitQueryIntoPieces;
    if FMaxRowsPerBatch = 0 then begin //init to find out max rows per batch
jmpEB:fRawTemp := GetExecuteBlockString(Self,
        IsParamIndex, BindList.Count, BatchDMLArrayCount, FCachedQueryRaw,
        FPlainDriver, PreparedRowsOfArray, FMaxRowsPerBatch,
          FTypeTokens, FStatementType, FFBConnection.GetXSQLDAMaxSize);
    end else
      fRawTemp := '';
    FinalChunkSize := (BatchDMLArrayCount mod FMaxRowsPerBatch);
    if (FMaxRowsPerBatch <= BatchDMLArrayCount) and (FBatchStmts[True].Obj = nil) then begin
      if fRawTemp = '' then goto jmpEB;
      PrepareArrayStmt(FBatchStmts[True]); //max block size per batch
    end;
    if (FinalChunkSize > 0) and ((FBatchStmts[False].Obj = nil) or
       (FinalChunkSize <> FBatchStmts[False].PreparedRowsOfArray)) then //if final chunk then
      PrepareFinalChunk(FinalChunkSize);
  end;
end;

function TZAbstractFirebirdStatement.SplittQuery(
  const SQL: SQLString): RawByteString;
var
  I, ParamCnt, FirstComposePos: Integer;
  Tokens: TZTokenList;
  Token: PZToken;
  Tmp, Tmp2: RawByteString;
  ResultWriter, SectionWriter: TZRawSQLStringWriter;
  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(FCachedQueryRaw, Length(FCachedQueryRaw)+1);
    FCachedQueryRaw[High(FCachedQueryRaw)] := Value;
    SetLength(FIsParamIndex, Length(FCachedQueryRaw));
    IsParamIndex[High(FIsParamIndex)] := Param;
    ResultWriter.AddText(Value, Result);
  end;

begin
  ParamCnt := 0;
  Result := '';
  Tmp2 := '';
  Tmp := '';
  Tokens := Connection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
  SectionWriter := TZRawSQLStringWriter.Create(Length(SQL) shr 5);
  ResultWriter := TZRawSQLStringWriter.Create(Length(SQL) shl 1);
  try
    FirstComposePos := 0;
    FTokenMatchIndex := -1;
    Token := nil;
    for I := 0 to Tokens.Count -1 do begin
      Token := Tokens[I];
      if Tokens.IsEqual(I, Char('?')) then begin
        if (FirstComposePos < I) then
          {$IFDEF UNICODE}
          SectionWriter.AddText(PUnicodeToRaw(Tokens[FirstComposePos].P, (Tokens[I-1].P-Tokens[FirstComposePos].P)+ Tokens[I-1].L, FClientCP), Tmp);
          {$ELSE}
          SectionWriter.AddText(Tokens[FirstComposePos].P, (Tokens[I-1].P-Tokens[FirstComposePos].P)+ Tokens[I-1].L, Tmp);
          {$ENDIF}
        SectionWriter.Finalize(Tmp);
        Add(Tmp, False);
        Tmp := '';
        {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
        Add(ZUnicodeToRaw(Tokens.AsString(I, I), ConSettings^.ClientCodePage^.CP));
        {$ELSE}
        Add('?', True);
        {$ENDIF}
        Inc(ParamCnt);
        FirstComposePos := i +1;
      end
      {$IFNDEF UNICODE}
      else if ConSettings.AutoEncode or (FDB_CP_ID = CS_NONE) then
        case (Tokens[i].TokenType) of
          ttQuoted, ttComment,
          ttWord: begin
              if (FirstComposePos < I) then
                SectionWriter.AddText(Tokens[FirstComposePos].P, (Tokens[I-1].P-Tokens[FirstComposePos].P)+ Tokens[I-1].L, Tmp);
              if (FDB_CP_ID = CS_NONE) and ( //all identifiers collate unicode_fss if CS_NONE
                 (Token.TokenType = ttQuotedIdentifier) or
                 ((Token.TokenType = ttWord) and (Token.L > 1) and (Token.P^ = '"')))
              then Tmp2 := ZConvertStringToRawWithAutoEncode(Tokens.AsString(i), ConSettings^.CTRL_CP, zCP_UTF8)
              else Tmp2 := ConSettings^.ConvFuncs.ZStringToRaw(Tokens.AsString(i), ConSettings^.CTRL_CP, FClientCP);
              SectionWriter.AddText(Tmp2, Tmp);
              Tmp2 := '';
              FirstComposePos := I +1;
            end;
          else ;//satisfy FPC
        end
      {$ELSE}
      else if (FDB_CP_ID = CS_NONE) and (//all identifiers collate unicode_fss if CS_NONE
               (Token.TokenType = ttQuotedIdentifier) or
               ((Token.TokenType = ttWord) and (Token.L > 1) and (Token.P^ = '"'))) then begin
        if (FirstComposePos < I) then begin
          Tmp2 := PUnicodeToRaw(Tokens[FirstComposePos].P, (Tokens[I-1].P-Tokens[FirstComposePos].P)+ Tokens[I-1].L, FClientCP);
          SectionWriter.AddText(Tmp2, Tmp);
        end;
        Tmp2 := PUnicodeToRaw(Token.P, Token.L, zCP_UTF8);
        SectionWriter.AddText(Tmp2, Result);
        Tmp2 := EmptyRaw;
        FirstComposePos := I +1;
      end;
      {$ENDIF};
    end;
    if (FirstComposePos <= Tokens.Count-1) then begin
      {$IFDEF UNICODE}
      Tmp2 := PUnicodeToRaw(Tokens[FirstComposePos].P, (Token.P-Tokens[FirstComposePos].P)+ Token.L, FClientCP);
      SectionWriter.AddText(Tmp2, Tmp);
      Tmp2 := '';
      {$ELSE}
      SectionWriter.AddText(Tokens[FirstComposePos].P, (Token.P-Tokens[FirstComposePos].P)+ Token.L, Tmp);
      {$ENDIF}
    end;
    SectionWriter.Finalize(Tmp);
    if Tmp <> EmptyRaw then
      Add(Tmp, False);
    ResultWriter.Finalize(Result);
  finally
    Tokens.Free;
    FreeAndNil(SectionWriter);
    FreeAndNil(ResultWriter);
  end;
  SetBindCapacity(ParamCnt);
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractFirebirdStatement.Unprepare;
begin
  inherited Unprepare;
  if FInMessageMetadata <> nil then begin
    FInMessageMetadata.release;
    FInMessageMetadata := nil;
  end;
  if FOutMessageMetadata <> nil then begin
    FOutMessageMetadata.release;
    FOutMessageMetadata := nil;
  end;
  if FFBStatement <> nil then begin
    FFBStatement.free(FStatus);
    FFBStatement.release;
    FFBStatement := nil;
  end;
end;

{ TZFirebirdStatement }

constructor TZFirebirdStatement.Create(const Connection: IZFirebirdConnection;
  Info: TStrings);
begin
  inherited Create(Connection, '', Info);
end;

{ TZFirebirdPreparedStatement }

procedure TZFirebirdPreparedStatement.CheckParameterIndex(var Value: Integer);
var I: Integer;
begin
  if not Prepared then
    Prepare;
  if (Value<0) or (Value+1 > BindList.Count) then
    raise EZSQLException.Create(SInvalidInputParameterCount);
  if BindList.HasOutOrInOutOrResultParam then
    for I := 0 to Value do
      if Ord(BindList[I].ParamType) > Ord(pctInOut) then
        Dec(Value);
end;

{**
  Prepares eventual structures for binding input parameters.
}
function TZFirebirdPreparedStatement.CreateConversionError(Index: Cardinal;
  Current: TZSQLType): EZSQLException;
var Expected: TZSQLType;
    AType, ASubType: Cardinal;
    Scale: Integer;
begin
  AType := FInMessageMetadata.getType(FStatus, Index);
  ASubType := FInMessageMetadata.getSubType(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  Expected := ConvertIB_FBType2SQLType(AType, ASubType, Scale);
  Result := ZDbcUtils.CreateConversionError(Index, Current, Expected);
end;

procedure TZFirebirdPreparedStatement.InternalBindDouble(Index: Cardinal;
  const Value: Double);
var TimeStamp: TZTimeStamp;
    AType: Cardinal;
    Scale: Integer;
    Data: Pointer;
begin
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case AType of
    SQL_FLOAT     : PSingle(Data)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Value;
    SQL_LONG      : if Scale = 0
                    then PISC_LONG(Data)^ := Round(Value)
                    else PISC_LONG(Data)^ := Round(Value*IBScaleDivisor[Scale]);
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(Value <> 0);
    SQL_SHORT     : if Scale = 0
                    then PISC_SHORT(Data)^ := Round(Value)
                    else PISC_SHORT(Data)^ := Round(Value*IBScaleDivisor[Scale]);
    SQL_INT64,
    SQL_QUAD      : if Scale = 0
                    then PISC_INT64(Data)^ := Round(Value)
                    else PISC_INT64(Data)^ := Round(Value*IBScaleDivisor[Scale]);
    SQL_TYPE_DATE : begin
                      DecodeDate(Value, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                      isc_encode_date(PISC_DATE(Data)^, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                    end;
    SQL_TYPE_TIME : begin
                      DecodeTime(Value, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
                      TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
                      isc_encode_time(PISC_TIME(Data)^, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
                    end;
    SQL_TIMESTAMP : begin
                      DecodeDate(Value, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                      isc_encode_date(PISC_TIMESTAMP(Data).timestamp_date, TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
                      DecodeTime(Value, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, PWord(@TimeStamp.Fractions)^);
                      TimeStamp.Fractions := PWord(@TimeStamp.Fractions)^*10;
                      isc_encode_time(PISC_TIMESTAMP(Data).timestamp_time, TimeStamp.Hour, TimeStamp.Minute, TimeStamp.Second, TimeStamp.Fractions);
                    end;
    else raise CreateConversionError(Index, stDouble);
  end;
end;

procedure TZFirebirdPreparedStatement.PrepareInParameters;
var MessageMetadata: IMessageMetadata;
    MetadataBuilder: IMetadataBuilder;
    Index, Tmp, AType: Cardinal;
begin
  MessageMetadata := FFBStatement.getInputMetadata(FStatus);
  try
    Tmp := MessageMetadata.getCount(FStatus);
    BindList.SetCount(Tmp);
    if Tmp > 0 then begin
      MetadataBuilder := MessageMetadata.getBuilder(FStatus);
      try
        for Index := 0 to Tmp -1 do begin
          AType := MessageMetadata.getType(FStatus, Index);
          if AType = SQL_TEXT then //length might be zero
            AType := SQL_VARYING; //we don't use the fixed char fields. We don't space padd the data
          MetadataBuilder.setType(FStatus, Index, AType);
          Tmp := MessageMetadata.getSubType(FStatus, Index);
          MetadataBuilder.setSubType(FStatus, Index, Tmp);
          Tmp := MessageMetadata.getLength(FStatus, Index);
          if Tmp = SQL_VARYING then
            Tmp := ((Tmp shr 2) + 1) shl 1; //4Byte align incluing 2 bytes reserved for overlongs {let fb raise the Exception}
          MetadataBuilder.setLength(FStatus, Index, Tmp);
          Integer(Tmp) := MessageMetadata.getScale(FStatus, Index);
          MetadataBuilder.setScale(FStatus, Index, Integer(Tmp));
          Tmp := MessageMetadata.getCharSet(FStatus, Index);
          MetadataBuilder.setCharSet(FStatus, Index, Tmp);
        end;
        FInMessageMetadata := MetadataBuilder.getMetadata(FStatus);
      finally
        MetadataBuilder.release;
      end;
      Tmp := FInMessageMetadata.getMessageLength(FStatus);
      GetMem(FInData, Tmp);
    end;
  finally
    MessageMetadata.release;
  end;
end;

procedure TZFirebirdPreparedStatement.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize, Scale: LengthInt);
begin
  if ParamType = pctResultSet then
    Raise EZUnsupportedException.Create(SUnsupportedOperation);
  inherited;
end;

{**
  Sets the designated parameter to a <code>AnsiString</code> value.
  The parameter must have operating-system encoding. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZFirebirdPreparedStatement.SetAnsiString(Index: Integer;
  const Value: AnsiString);
var CS_ID, AType, ASubType: Cardinal;
    Data: Pointer;
    Len: LengthInt;
    P: PAnsiChar;
label jmpWriteLob;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> '' then begin
    Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
    AType := FInMessageMetadata.getType(FStatus, Index);
    CS_ID := FInMessageMetadata.getCharSet(FStatus, Index);
    Len := Length(Value);
    P := Pointer(Value);
    case AType of
      SQL_TEXT,
      SQL_VARYING   : begin
                        if (FCodePageArray[CS_ID] <> ZOSCodePage) and (CS_ID = CS_BINARY) then begin
                          PRawToRawConvert(P, Len, zCP_UTF8, FCodePageArray[CS_ID], FRawTemp);
                          Len := Length(FRawTemp);
                          P := Pointer(FRawTemp);
                        end;
                        ASubType := FInMessageMetadata.getLength(FStatus, Index);
                        if LengthInt(ASubType) < Len then
                          Len := LengthInt(ASubType);
                        Move(P^, PISC_VARYING(Data).str[0], Len);
                        PISC_VARYING(Data).strlen := Len;
                      end;
      SQL_BLOB      : begin
                      AType := FInMessageMetadata.getSubType(FStatus, Index);
                      if AType = isc_blob_text then
                        if (FCodePageArray[CS_ID] = ZOSCodePage) or (CS_ID = CS_BINARY)
                        then goto jmpWriteLob
                        else begin
                          PRawToRawConvert(P, Len, zCP_UTF8, FCodePageArray[CS_ID], FRawTemp);
                          Len := Length(FRawTemp);
                          if Len = 0
                          then P := PEmptyAnsiString
                          else P := Pointer(FRawTemp);
                          goto jmpWriteLob;
                        end
                      else
jmpWriteLob:            WriteLobBuffer(Index, P, Len);
                    end;
      else SetPAnsiChar(Index, P, Len);
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;
{$ENDIF NO_ANSISTRING}

{**
  Sets the designated parameter to a <code>BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetBigDecimal(Index: Integer;
  const Value: TBCD);
var Data: Pointer;
  AType: Cardinal;
  L, Scale: Integer;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case (AType) of
    SQL_LONG      : BCD2ScaledOrdinal(Value, Data, SizeOf(ISC_LONG), -Scale);
    SQL_SHORT     : BCD2ScaledOrdinal(Value, Data, SizeOf(ISC_SHORT), -Scale);
    SQL_INT64,
    SQL_QUAD      : BCD2ScaledOrdinal(Value, Data, SizeOf(ISC_INT64), -Scale);
    SQL_TEXT,
    SQL_VARYING   : begin
                      L := BcdToRaw(Value, @fABuffer[0], '.');
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      if LengthInt(AType) < L then
                        L := AType;
                      Move(fABuffer[0], PISC_VARYING(Data).str[0], L);
                      PISC_VARYING(Data).strlen := L;
                    end;
    else InternalBindDouble(Index, BCDToDouble(Value));
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

procedure TZFirebirdPreparedStatement.SetBlob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var Data: Pointer;
  P: PAnsiChar;
  AType, CS_ID, L: Cardinal;
  IBLob: IZFirebirdLob;
label jmpNotNull;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  CS_ID := FInMessageMetadata.getCharSet(FStatus, Index);
  BindList.Put(Index, SQLType, Value); //localize for the refcount

  if (Value = nil) or Value.IsEmpty then begin
    BindList.SetNull(Index, SQLType);
    P := nil;
    L := 0;//satisfy compiler
  end else if Supports(Value, IZFirebirdLob, IBLob) and ((AType = SQL_QUAD) or (AType = SQL_BLOB)) then begin
    PISC_QUAD(Data)^ := IBLob.GetBlobId;
    goto jmpNotNull;
  end else begin
    BindList.Put(Index, SQLType, Value); //localize for the refcount
    if (Value <> nil) and (SQLType in [stAsciiStream, stUnicodeStream]) then
      if Value.IsClob then begin
        Value.SetCodePageTo(FCodePageArray[CS_ID]);
        P := Value.GetPAnsiChar(ConSettings^.ClientCodePage.CP, FRawTemp, L)
      end else begin
        BindList.Put(Index, stAsciiStream, CreateRawCLobFromBlob(Value, ConSettings, FOpenLobStreams));
        P := IZCLob(BindList[Index].Value).GetPAnsiChar(FCodePageArray[CS_ID], FRawTemp, L);
      end
    else
      P := Value.GetBuffer(FRawTemp, L);
  end;
  if P <> nil then begin
    case AType of
      SQL_TEXT,
      SQL_VARYING   : begin
                        AType := FInMessageMetadata.getLength(FStatus, Index);
                        if AType < L then
                          L := AType;
                        Move(P^, PISC_VARYING(Data).str[0], L);
                        PISC_VARYING(Data).strlen := L;
                      end;
      SQL_BLOB,
      SQL_QUAD      : WriteLobBuffer(Index, P, L);
      else raise CreateConversionError(Index, stGUID);
    end;
jmpNotNull:
    PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
  end else
    SetNull(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType);
end;

{**
  Sets the designated parameter to <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetBoolean(Index: Integer;
  Value: Boolean);
var Data: Pointer;
  Scale: Integer;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  case FInMessageMetadata.getType(FStatus, Index) of
    SQL_FLOAT     : PSingle(Data)^   := Ord(Value);
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Ord(Value);
    SQL_LONG      : if Value
                    then PISC_LONG(Data)^ := IBScaleDivisor[Scale]
                    else PISC_LONG(Data)^ := 0;
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(Value);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(Value);
    SQL_SHORT     : if Value
                    then PISC_SHORT(Data)^ := IBScaleDivisor[Scale]
                    else PISC_SHORT(Data)^ := 0;
    SQL_INT64,
    SQL_QUAD      : if Value
                    then PISC_INT64(Data)^ := IBScaleDivisor[Scale]
                    else PISC_INT64(Data)^ := 0;
    SQL_VARYING   : begin
                      PISC_VARYING(Data).strlen := 1;
                      Byte(PISC_VARYING(Data).str) := Byte('0')+Ord(Value);
                    end;
    else raise CreateConversionError(Index, stBoolean);
  end;
  PISC_Short(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a <code>unsigned 8Bit integer</code> value.
  The driver converts this
  to an SQL <code>Byte</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetByte(Index: Integer; Value: Byte);
begin
  SetSmall(Index, Value);
end;

{**
  Sets the designated parameter to an array of bytes by reference.
  The driver converts this to an SQL <code>VARBINARY</code> or
  <code>LONGVARBINARY</code> (depending on the argument's size relative to
  the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the parameter value address
  @param Len the length of the addressed value
}
procedure TZFirebirdPreparedStatement.SetBytes(Index: Integer;
  const Value: TBytes);
var Len: NativeUInt;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Len := Length(Value);
  if Len = 0
  then P := PEmptyAnsiString
  else P := Pointer(Value);
  SetPAnsiChar(Index, P, Len);
end;

{**
  Sets the designated parameter to an array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetBytes(Index: Integer;
  Value: PByte; Len: NativeUInt);
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  if Value = nil then
    Value := Pointer(PEmptyAnsiString);
  SetPAnsiChar(Index, PAnsiChar(Value), Len);
end;

{**
  Sets the designated parameter to a <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetCharRec(Index: Integer;
  const Value: TZCharRec);
var CS_ID, AType: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value.CP = zCP_UTF16 then
    SetPWideChar(Index, Value.P, Value.Len)
  else begin
    AType := FInMessageMetadata.getType(FStatus, Index);
    CS_ID := FInMessageMetadata.getCharSet(FStatus, Index);
    if (Value.CP = ClientCP) or (CS_ID = CS_BINARY) or
      ((FDB_CP_ID = CS_NONE) and
      ((AType = SQL_VARYING)) and (FCodePageArray[CS_ID] = Value.CP))
    then SetPAnsiChar(Index, Value.P, Value.Len)
    else begin
      FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP); //localize it
      if FUniTemp <> ''
      then SetPWideChar(Index, Pointer(FUniTemp), Length(FUniTemp))
      else SetPWideChar(Index, PEmptyUnicodeString, 0);
    end;
  end;
end;

{**
  Sets the designated parameter to a <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetCurrency(Index: Integer;
  const Value: Currency);
var Data: Pointer;
  AType: Cardinal;
  L, Scale: Integer;
  i64: Int64 absolute Value;
  P: PAnsiChar;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case (AType) of
    SQL_FLOAT     : PSingle(Data)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Value;
    SQL_LONG      : if Scale = -4 then  //scale fits!
                      PISC_LONG(Data)^ := I64
                    else if Scale > -4 then //EH: check the modulo?
                      PISC_LONG(Data)^ := I64 div IBScaleDivisor[-4-Scale] //dec scale digits
                    else
                      PISC_LONG(Data)^ := I64 * IBScaleDivisor[4+Scale]; //inc scale digits
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(Value <> 0);
    SQL_SHORT     : if Scale = -4 then  //scale fits!
                      PISC_SHORT(Data)^ := I64
                    else if Scale > -4 then //EH: check the modulo?
                      PISC_SHORT(Data)^ := I64 div IBScaleDivisor[-4-Scale] //dec scale digits
                    else
                      PISC_SHORT(Data)^ := I64 * IBScaleDivisor[4+Scale]; //inc scale digits
    SQL_INT64,
    SQL_QUAD      : if Scale = -4 then //scale fits!
                      PISC_INT64(Data)^ := I64
                    else if Scale > -4 then //EH: check the modulo?
                      PISC_INT64(Data)^ := I64 div IBScaleDivisor[-4-Scale]//dec scale digits
                    else
                      PISC_INT64(Data)^ := I64 * IBScaleDivisor[4+Scale]; //inc scale digits
    SQL_TEXT,
    SQL_VARYING   : begin
                      CurrToRaw(Value, @fABuffer[0], @P);
                      L := P - PAnsiChar(@fABuffer[0]);
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      if LengthInt(AType) < L then
                        L := AType;
                      Move(fABuffer[0], PISC_VARYING(Data).str[0], L);
                      PISC_VARYING(Data).strlen := L;
                    end;
    else raise CreateConversionError(Index, stCurrency);
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a <code<TZDate</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "DT" does not seem to be initialized} {$ENDIF}
procedure TZFirebirdPreparedStatement.SetDate(Index: Integer;
  const Value: TZDate);
var Data: Pointer;
  AType: Cardinal;
  L: LengthInt;
  DT: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case (AType) of
    SQL_VARYING   : begin
                      L := DateToRaw(Value.Year, Value.Month, Value.Day, @fABuffer[0],
                          ConSettings^.WriteFormatSettings.DateFormat, False, Value.IsNegative);
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      if LengthInt(AType) < L then
                        L := LengthInt(AType);
                      Move(fABuffer[0], PISC_VARYING(Data).str[0], L);
                      PISC_VARYING(Data).strlen := L;
                    end;
    SQL_TYPE_TIME : PISC_TIME(Data)^ := 0;
    SQL_TYPE_DATE : isc_encode_date(PISC_DATE(Data)^, Value.Year, Value.Month, Value.Day);
    SQL_TIMESTAMP : begin
                      isc_encode_date(PISC_TIMESTAMP(Data).timestamp_date, Value.Year, Value.Month, Value.Day);
                      PISC_TIMESTAMP(Data).timestamp_time := 0;
                    end;
    else begin
      ZSysUtils.TryDateToDateTime(Value, DT);
      InternalBindDouble(Index, DT);
    end;
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetDouble(Index: Integer;
  const Value: Double);
var Data: Pointer;
  AType, L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case (AType) of
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Value;
    SQL_FLOAT     : PSingle(Data)^   := Value;
    SQL_TEXT,
    SQL_VARYING   : begin
        L := FloatToSqlRaw(Value, @fABuffer[0]);
        AType := FInMessageMetadata.getLength(FStatus, Index);
        if AType < L then
          L := AType;
        Move(fABuffer[0], PISC_VARYING(Data).str[0], L);
        PISC_VARYING(Data).strlen := L;
      end;
    else InternalBindDouble(Index, Value);
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a <code>single</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetFloat(Index: Integer; Value: Single);
var Data: Pointer;
  AType, L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case (AType) of
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Value;
    SQL_FLOAT     : PSingle(Data)^   := Value;
    SQL_TEXT,
    SQL_VARYING   : begin
        L := FloatToSqlRaw(Value, @fABuffer[0]);
        AType := FInMessageMetadata.getLength(FStatus, Index);
        if AType < L then
          L := AType;
        Move(fABuffer[0], PISC_VARYING(Data).str[0], L);
        PISC_VARYING(Data).strlen := L;
      end;
    else InternalBindDouble(Index, Value);
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetGUID(Index: Integer;
  const Value: TGUID);
var Data: Pointer;
  P: PAnsiChar;
  AType, CS_ID, L: Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  CS_ID := FInMessageMetadata.getCharSet(FStatus, Index);
  case (AType) of
    SQL_VARYING   : begin
                      if CS_ID = CS_BINARY then begin
                        P := @Value.D1;
                        L := SizeOf(TGUID);
                      end else begin
                        //see https://firebirdsql.org/refdocs/langrefupd25-intfunc-uuid_to_char.html
                        P := @fABuffer[0];
                        GUIDToBuffer(@Value.D1, P, []);
                        L := 36;
                      end;
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      if AType < L then
                        L := AType;
                      Move(P^, PISC_VARYING(Data).str[0], L);
                      PISC_VARYING(Data).strlen := L;
                    end;
    SQL_BLOB,
    SQL_QUAD      : if CS_ID = CS_BINARY then
                      WriteLobBuffer(Index, @Value.D1, SizeOf(TGUID))
                    else begin
                      P := @fABuffer[0];
                      GUIDToBuffer(@Value.D1, P, []);
                      WriteLobBuffer(Index, P, 36)
                    end;
    else raise CreateConversionError(Index, stGUID);
  end;
end;

{**
  Sets the designated parameter to a <code>signed 32bit integer</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetInt(Index, Value: Integer);
var Data: Pointer;
  AType: Cardinal;
  Scale: Integer;
  P: PAnsiChar;
  Digits: Byte;
  IsNegative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case AType of
    SQL_FLOAT     : PSingle(Data)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Value;
    SQL_LONG      : if Scale = 0
                    then PISC_LONG(Data)^ := Value
                    else PISC_LONG(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(Value <> 0);
    SQL_SHORT     : if Scale = 0
                    then PISC_SHORT(Data)^ := Value
                    else PISC_SHORT(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_QUAD,
    SQL_INT64     : if Scale = 0
                    then PISC_INT64(Data)^ := Value
                    else PISC_INT64(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_TEXT,
    SQL_VARYING   : begin
                      Scale := FInMessageMetadata.getLength(FStatus, Index);
                      Digits := GetOrdinalDigits(Value, AType, IsNegative);
                      if Digits+Byte(IsNegative) > Scale then begin
                        PISC_VARYING(Data).strlen := Scale;
                        Digits := Scale - Byte(IsNegative);
                      end;
                      P := @PISC_VARYING(Data).str[0];
                      if IsNegative then begin
                        PByte(P)^ := Byte('-');
                        Inc(P);
                      end;
                      IntToRaw(AType, P, Digits);
                    end;
    else raise CreateConversionError(Index, stInteger);
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a <code>signed 64Bit integer</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetLong(Index: Integer;
  const Value: Int64);
var Data: Pointer;
  AType: Cardinal;
  Scale: Integer;
  U: UInt64;
  P: PAnsiChar;
  Digits: Byte;
  IsNegative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case AType of
    SQL_FLOAT     : PSingle(Data)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Value;
    SQL_LONG      : if Scale = 0
                    then PISC_LONG(Data)^ := Value
                    else PISC_LONG(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(Value <> 0);
    SQL_SHORT     : if Scale = 0
                    then PISC_SHORT(Data)^ := Value
                    else PISC_SHORT(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_QUAD,
    SQL_INT64     : if Scale = 0
                    then PISC_INT64(Data)^ := Value
                    else PISC_INT64(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_TEXT,
    SQL_VARYING   : begin
                      Scale := FInMessageMetadata.getLength(FStatus, Index);
                      Digits := GetOrdinalDigits(Value, U, IsNegative);
                      if Digits+Byte(IsNegative) > Scale then begin
                        PISC_VARYING(Data).strlen := Scale;
                        Digits := Scale - Byte(IsNegative);
                      end;
                      P := @PISC_VARYING(Data).str[0];
                      if IsNegative then begin
                        PByte(P)^ := Byte('-');
                        Inc(P);
                      end;
                      IntToRaw(U, P, Digits);
                    end;
    else raise CreateConversionError(Index, stLong);
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>ZDbcIntfs.pas.TZSQLType</code>
}
procedure TZFirebirdPreparedStatement.SetNull(Index: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NULL
end;

{**
   Set up parameter PAnsiChar value
   @param Index the target parameter index
   @param Value the source value
   @param Len the length in bytes of the source value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZFirebirdPreparedStatement.SetPAnsiChar(Index: Cardinal;
  Value: PAnsiChar; Len: LengthInt);
var Data: Pointer;
    AType, Scale: Cardinal;
    TS: TZTimeStamp;
    D: TZDate absolute TS;
    T: TZTime absolute TS;
Label Fail;
begin
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case AType of
    SQL_VARYING   : begin
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      if LengthInt(AType) < Len then
                        Len := LengthInt(AType);
                      Move(Value^, PISC_VARYING(Data).str[0], Len);
                      PISC_VARYING(Data).strlen := Len;
                    end;
    SQL_LONG      : if Scale = 0
                    then PISC_LONG (Data)^ := RawToIntDef(Value, Value+Len, 0)
                    else PISC_LONG (Data)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[Scale], 0));
    SQL_SHORT     : if Scale = 0
                    then PISC_LONG (Data)^ := RawToIntDef(Value, Value+Len, 0)
                    else PISC_SHORT (Data)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[Scale], 0));
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_D_FLOAT,
    SQL_DOUBLE    : SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
    SQL_FLOAT     : SQLStrToFloatDef(Value, 0, PSingle (Data)^, Len);
    SQL_INT64     : if Scale = 0
                    then PISC_LONG (Data)^ := RawToInt64Def(Value, Value+Len, 0)
                    else PISC_INT64(Data)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[Scale], 0)); //AVZ - INT64 value was not recognized
    SQL_BLOB, SQL_QUAD: WriteLobBuffer(Index, Value, Len);
    SQL_TYPE_DATE : if TryPCharToDate(Value, Len, ConSettings^.WriteFormatSettings, D)
                    then isc_encode_date(PISC_DATE(Data)^, D.Year, D.Month, D.Day)
                    else goto Fail;
    SQL_TYPE_TIME:  if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T)
                    then isc_encode_time(PISC_TIME(Data)^, T.Hour, T.Minute, T.Second, T.Fractions div 100000)
                    else goto Fail;
    SQL_TIMESTAMP:  if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                      isc_encode_date(PISC_TIMESTAMP(Data).timestamp_date, TS.Year, TS.Month, TS.Day);
                      isc_encode_time(PISC_TIMESTAMP(Data).timestamp_time, TS.Hour, TS.Minute, TS.Second, TS.Fractions div 100000);
                    end else goto Fail;
    else
Fail: raise CreateConversionError(Index, stString);
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
   Set up parameter PWideChar value
   @param Index the target parameter index
   @param Value the source value
   @param Len the length in words of the source value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZFirebirdPreparedStatement.SetPWideChar(Index: Cardinal;
  Value: PWideChar; Len: LengthInt);
var Data: Pointer;
    AType, Scale: Cardinal;
    TS: TZTimeStamp;
    D: TZDate absolute TS;
    T: TZTime absolute TS;
Label Fail;
begin
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case AType of
    SQL_TEXT,
    SQL_VARYING   : begin
                      Scale := FInMessageMetadata.getCharSet(FStatus, Index) and 255;
                      if Scale <> CS_BINARY
                      then FRawTemp := PUnicodeToRaw(Value, Len, FCodePageArray[Scale])
                      else FRawTemp := UnicodeStringToAscii7(Value, Len);
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      Len := Length(FRawTemp);
                      if LengthInt(AType) < Len then
                        Len := LengthInt(AType);
                      if Len > 0 then
                        Move(Pointer(FRawTemp)^, PISC_VARYING(Data).str[0], Len);
                      PISC_VARYING(Data).strlen := Len;
                    end;
    SQL_LONG      : if Scale = 0
                    then PISC_LONG(Data)^ := UnicodeToIntDef(Value, Value+Len, 0)
                    else PISC_LONG(Data)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[scale], 0));
    SQL_SHORT     : if Scale = 0
                    then PISC_LONG(Data)^ := UnicodeToIntDef(Value, Value+Len, 0)
                    else PISC_SHORT(Data)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[scale], 0));
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(StrToBoolEx(Value, Value+Len));
    SQL_D_FLOAT,
    SQL_DOUBLE    : SQLStrToFloatDef(Value, 0, PDouble(Data)^, Len);
    SQL_FLOAT     : SQLStrToFloatDef(Value, 0, PSingle (Data)^, Len);
    SQL_INT64     : if Scale = 0
                    then PISC_LONG(Data)^ := UnicodeToInt64Def(Value, Value+Len, 0)
                    else PISC_INT64(Data)^ := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(RoundTo(SQLStrToFloatDef(Value, 0, Len) * IBScaleDivisor[scale], 0)); //AVZ - INT64 value was not recognized
    SQL_BLOB,
    SQL_QUAD      : begin
                      Scale := FInMessageMetadata.getCharSet(FStatus, Index) and 255;
                      if Scale = isc_blob_text
                      then FRawTemp := PUnicodeToRaw(Value, Len, FCodePageArray[Scale])
                      else FRawTemp := UnicodeStringToAscii7(Value, Len);
                      if FRawTemp <> ''
                      then Data := Pointer(FRawTemp)
                      else Data := PEmptyAnsiString;
                      Len := Length(FRawTemp);
                      WriteLobBuffer(Index, Data, Len)
                    end;
    SQL_TYPE_DATE : if TryPCharToDate(Value, Len, ConSettings^.WriteFormatSettings, D)
                    then isc_encode_date(PISC_DATE(Data)^, D.Year, D.Month, D.Day)
                    else goto Fail;
    SQL_TYPE_TIME:  if TryPCharToTime(Value, Len, ConSettings^.WriteFormatSettings, T)
                    then isc_encode_time(PISC_TIME(Data)^, T.Hour, T.Minute, T.Second, T.Fractions div 100000)
                    else goto Fail;
    SQL_TIMESTAMP:  if TryPCharToTimeStamp(Value, Len, ConSettings^.WriteFormatSettings, TS) then begin
                      isc_encode_date(PISC_TIMESTAMP(Data).timestamp_date, TS.Year, TS.Month, TS.Day);
                      isc_encode_time(PISC_TIMESTAMP(Data).timestamp_time, TS.Hour, TS.Minute, TS.Second, TS.Fractions div 100000);
                    end else goto Fail;
    else
Fail:   raise CreateConversionError(Index, stUnicodeString);
  end;
  if FInMessageMetadata.isNullable(FStatus, Index) then
     PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to <code>raw database encoded string</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetRawByteString(Index: Integer;
  const Value: RawByteString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> ''
  then SetPAnsiChar(Index, Pointer(Value), Length(Value))
  else SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;

{**
  Sets the designated parameter to a <code>signed 8bit integer</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetShort(Index: Integer; Value: ShortInt);
begin
  SetSmall(Index, Value);
end;

{**
  Sets the designated parameter to a <code>signed 16bit integer</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetSmall(Index: Integer; Value: SmallInt);
var Data: Pointer;
  AType: Cardinal;
  Scale: Integer;
  P: PAnsiChar;
  W: Word;
  Digits: Byte;
  IsNegative: Boolean;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  Scale := FInMessageMetadata.getScale(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case AType of
    SQL_FLOAT     : PSingle(Data)^   := Value;
    SQL_D_FLOAT,
    SQL_DOUBLE    : PDouble(Data)^   := Value;
    SQL_LONG      : if Scale = 0
                    then PISC_LONG(Data)^ := Value
                    else PISC_LONG(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_BOOLEAN   : PISC_BOOLEAN(Data)^ := Ord(Value <> 0);
    SQL_BOOLEAN_FB: PISC_BOOLEAN_FB(Data)^ := Ord(Value <> 0);
    SQL_SHORT     : if Scale = 0
                    then PISC_SHORT(Data)^ := Value
                    else PISC_SHORT(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_QUAD,
    SQL_INT64     : if Scale = 0
                    then PISC_INT64(Data)^ := Value
                    else PISC_INT64(Data)^ := Value*IBScaleDivisor[Scale];
    SQL_TEXT,
    SQL_VARYING   : begin
                      Scale := FInMessageMetadata.getLength(FStatus, Index);
                      Digits := GetOrdinalDigits(Value, W, IsNegative);
                      if Digits+Byte(IsNegative) > Scale then begin
                        PISC_VARYING(Data).strlen := Scale;
                        Digits := Scale - Byte(IsNegative);
                      end;
                      P := @PISC_VARYING(Data).str[0];
                      if IsNegative then begin
                        PISC_VARYING(Data).str[0] := AnsiChar('-');
                        Inc(P);
                      end;
                      IntToRaw(W, P, Digits);
                    end;
    else raise CreateConversionError(Index, stSmall);
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{**
  Sets the designated parameter to a <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetString(Index: Integer;
  const Value: String);
{$IFDEF UNICODE}
begin
  SetUnicodeString(Index, Value);
{$ELSE}
var CS_ID, AType, ASubType: Cardinal;
    Data: Pointer;
    L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> '' then begin
    Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
    AType := FInMessageMetadata.getType(FStatus, Index);
    CS_ID := FInMessageMetadata.getCharSet(FStatus, Index);
    L := Length(Value);
    case AType of
      SQL_VARYING   : if not ConSettings^.AutoEncode or (CS_ID = CS_BINARY) then begin
                        ASubType := FInMessageMetadata.getLength(FStatus, Index);
                        if L > LengthInt(ASubType) then
                          L := LengthInt(ASubType);
                        Move(Pointer(Value)^, PISC_VARYING(Data).str[0], L);
                        PISC_VARYING(Data).strlen := L;
                      end else
                        SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
                          ConSettings^.ConvFuncs.ZStringToRaw( Value, ConSettings^.Ctrl_CP, FCodePageArray[CS_ID]));
      SQL_BLOB      : begin
                        ASubType := FInMessageMetadata.getSubType(FStatus, Index);
                        if not ConSettings^.AutoEncode or (ASubType <> isc_blob_text)
                        then WriteLobBuffer(Index, Pointer(Value), L)
                        else SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
                          ConSettings^.ConvFuncs.ZStringToRaw( Value, ConSettings^.Ctrl_CP, ClientCP));
      else SetPAnsiChar(Index, Pointer(Value), L);
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
  {$ENDIF}
end;

{**
  Sets the designated parameter to a <code>TZTime</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZFirebirdPreparedStatement.SetTime(Index: Integer;
  const Value: TZTime);
var Data: Pointer;
  AType: Cardinal;
  L: LengthInt;
  DT: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case (AType) of
    SQL_TEXT,
    SQL_VARYING   : begin
                      L := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
                          @fABuffer[0], ConSettings^.WriteFormatSettings.TimeFormat, False, False);
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      if LengthInt(AType) < L then
                        L := LengthInt(AType);
                      Move(fABuffer[0], PISC_VARYING(Data).str[0], L);
                      PISC_VARYING(Data).strlen := L;
                    end;
    SQL_TYPE_DATE : isc_encode_date(PISC_DATE(Data)^, 1899, 12, 31);
    SQL_TYPE_TIME : isc_encode_time(PISC_TIME(Data)^, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
    SQL_TIMESTAMP : begin
                      isc_encode_date(PISC_TIMESTAMP(Data).timestamp_date,
                        cPascalIntegralDatePart.Year, cPascalIntegralDatePart.Month, cPascalIntegralDatePart.Day);
                      isc_encode_time(PISC_TIMESTAMP(Data).timestamp_time, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
                    end;
    else            begin
                      ZSysUtils.TryTimeToDateTime(Value, DT);
                      InternalBindDouble(Index, DT);
                    end;
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>TZTimestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFDEF FPC} {$PUSH} {$WARN 5057 off : Local variable "$1" does not seem to be initialized} {$ENDIF}
procedure TZFirebirdPreparedStatement.SetTimestamp(Index: Integer;
  const Value: TZTimeStamp);
var Data: Pointer;
  AType: Cardinal;
  L: LengthInt;
  DT: TDateTime;
begin
  {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
  CheckParameterIndex(Index);
  Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
  AType := FInMessageMetadata.getType(FStatus, Index);
  case (AType) of
    SQL_TEXT,
    SQL_VARYING   : begin
                      L := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
                          Value.Hour, Value.Minute, Value.Second, Value.Fractions,
                          @fABuffer[0], ConSettings^.WriteFormatSettings.DateTimeFormat, False, Value.IsNegative);
                      AType := FInMessageMetadata.getLength(FStatus, Index);
                      if LengthInt(AType) < L then
                        L := LengthInt(AType);
                      Move(fABuffer[0], PISC_VARYING(Data).str[0], L);
                      PISC_VARYING(Data).strlen := L;
                    end;
    SQL_TYPE_DATE : isc_encode_date(PISC_DATE(Data)^, Value.Year, Value.Month, Value.Day);
    SQL_TYPE_TIME : isc_encode_time(PISC_TIME(Data)^, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
    SQL_TIMESTAMP : begin
                      isc_encode_date(PISC_TIMESTAMP(Data).timestamp_date, Value.Year, Value.Month, Value.Day);
                      isc_encode_time(PISC_TIMESTAMP(Data).timestamp_time, Value.Hour, Value.Minute, Value.Second, Value.Fractions div 100000);
                    end;
    else begin
      ZSysUtils.TryTimeStampToDateTime(Value, DT);
      InternalBindDouble(Index, DT);
    end;
  end;
  PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to a <code>usigned 32bit integer</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetUInt(Index: Integer; Value: Cardinal);
begin
  SetLong(Index, Value);
end;

{**
  Sets the designated parameter to a <code>unsigned 64Bit integer</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetULong(Index: Integer;
  const Value: UInt64);
begin
  SetLong(Index, Value);
end;

{**
  Sets the designated parameter to a <code>UnicodeString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetUnicodeString(Index: Integer;
  const Value: ZWideString);
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> ''
  then SetPWideChar(Index, Pointer(Value), Length(Value))
  else SetPAnsiChar(Index, PEmptyAnsiString, 0);
end;

{**
  Sets the designated parameter to a <code>UTF8String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_UTF8STRING}
procedure TZFirebirdPreparedStatement.SetUTF8String(Index: Integer;
  const Value: UTF8String);
var CS_ID, AType, ASubType: Cardinal;
    Data: Pointer;
    Len: LengthInt;
    P: PAnsiChar;
label jmpWriteLob, jmpMov;
begin
  {$IFNDEF GENERIC_INDEX}
  Index := Index -1;
  {$ENDIF}
  CheckParameterIndex(Index);
  if Value <> '' then begin
    Data := PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index);
    AType := FInMessageMetadata.getType(FStatus, Index);
    CS_ID := FInMessageMetadata.getCharSet(FStatus, Index);
    Len := Length(Value);
    P := Pointer(Value);
    case AType of
      SQL_TEXT,
      SQL_VARYING   : begin
                        if (CS_ID <> CS_UTF8) and (CS_ID <> CS_UNICODE_FSS) and (CS_ID <> CS_BINARY) then begin
                          PRawToRawConvert(P, Len, zCP_UTF8, FCodePageArray[CS_ID], FRawTemp);
                          Len := Length(FRawTemp);
                          P := Pointer(FRawTemp);
                        end;
                        ASubType := FInMessageMetadata.getLength(FStatus, Index);
                        if LengthInt(ASubType) < Len then
                          Len := LengthInt(ASubType);
                        Move(P^, PISC_VARYING(Data).str[0], Len);
                        PISC_VARYING(Data).strlen := Len;
                      end;
      SQL_BLOB      : begin
                      AType := FInMessageMetadata.getSubType(FStatus, Index);
                      if AType = isc_blob_text then
                        if (FCodePageArray[CS_ID] = zCP_UTF8) or (CS_ID = CS_BINARY)
                        then goto jmpWriteLob
                        else begin
                          PRawToRawConvert(P, Len, zCP_UTF8, FCodePageArray[CS_ID], FRawTemp);
                          Len := Length(FRawTemp);
                          if Len = 0
                          then P := PEmptyAnsiString
                          else P := Pointer(FRawTemp);
                          goto jmpWriteLob;
                        end
                      else
jmpWriteLob:            WriteLobBuffer(Index, P, Len);
                    end;
      else SetPAnsiChar(Index, P, Len);
    end;
  end else
    SetPAnsiChar(Index, PEmptyAnsiString, 0)
end;
{$ENDIF NO_UTF8STRING}

{**
  Sets the designated parameter to a <code>unsigned 16bit integer</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZFirebirdPreparedStatement.SetWord(Index: Integer; Value: Word);
begin
  SetInt(Index, Value);
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZFirebirdPreparedStatement.Unprepare;
var b: Boolean;
begin
  FMaxRowsPerBatch := 0;
  try
    for b := False to True do
      if FBatchStmts[b].Obj <> nil then begin
        FBatchStmts[b].Obj.BindList.Count := 0;
        {$IFNDEF AUTOREFCOUNT}
        FBatchStmts[b].Obj._Release;
        {$ENDIF}
        FBatchStmts[b].Obj := nil;
      end;
  finally
    inherited Unprepare;
  end;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZFirebirdPreparedStatement.UnPrepareInParameters;
begin
  if (FInMessageMetadata <> nil) then begin
    FInMessageMetadata.Release;
    FInMessageMetadata := nil;
  end;
  if FInData <> nil then begin
    FreeMem(FInData);
    FInData := nil;
  end;
  inherited;
end;

procedure TZFirebirdPreparedStatement.WriteLobBuffer(Index: Cardinal;
  P: PAnsiChar; Len: NativeUInt);
var
  BlobId: TISC_QUAD;
  CurPos: NativeUInt;
  SegLen: Cardinal;
  Attachment: IAttachment;
  Transaction: ITransaction;
  Blob: IBlob;
begin
  { create blob handle }
  Attachment := FFBConnection.GetAttachment;
  Transaction := FFBConnection.GetActiveTransaction.GetTransaction;
  Blob := Attachment.createBlob(FStatus, Transaction, @BlobId, 0, nil);
  { put data to blob }
  CurPos := 0;
  SegLen := DefaultBlobSegmentSize;
  while (CurPos < Len) do begin
    if (CurPos + SegLen > Len) then
      SegLen := Len - CurPos;
    Blob.putSegment(FStatus, SegLen, P);
    Inc(CurPos, SegLen);
    Inc(P, SegLen);
  end;
  { close blob handle }
  Blob.close(FStatus);
  PISC_QUAD(PAnsiChar(FInData)+FInMessageMetadata.getOffset(FStatus, Index))^ := BlobId;
  if FInMessageMetadata.isNullable(FStatus, Index) then
     PISC_SHORT(PAnsiChar(FInData)+FInMessageMetadata.getNullOffset(FStatus, Index))^ := ISC_NOTNULL;
end;

{ TZFirebirdCallableStatement }

function TZFirebirdCallableStatement.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  I: Integer;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  SQLWriter: TZSQLStringWriter;
  FBConnection: IZFirebirdConnection;
begin
  SQL := '';
  I := Length(StoredProcName);
  i := I + 6+BindList.Count shl 1;
  SQLWriter := TZSQLStringWriter.Create(I);
  FBConnection := Connection as IZFirebirdConnection;
  if FBConnection.StoredProcedureIsSelectable(StoredProcName)
  then SQLWriter.AddText('SELECT * FROM ', SQL)
  else SQLWriter.AddText('EXECUTE PROCEDURE ', SQL);
  SQLWriter.AddText(StoredProcName, SQL);
  if BindList.Capacity >0 then
    SQLWriter.AddChar('(', SQL);
  for I := 0 to BindList.Capacity -1 do
    if not (BindList.ParamTypes[I] in [pctOut,pctReturn]) then
      SQLWriter.AddText('?,', SQL);
  if BindList.Capacity > 0 then begin
    SQLWriter.CancelLastComma(SQL);
    SQLWriter.AddChar(')', SQL);
  end;
  SQLWriter.Finalize(SQL);
  FreeAndNil(SQLWriter);
  Result := TZFirebirdPreparedStatement.Create(FBConnection, SQL, Info);
end;

initialization
{$ENDIF ZEOS_DISABLE_FIREBIRD}
end.
