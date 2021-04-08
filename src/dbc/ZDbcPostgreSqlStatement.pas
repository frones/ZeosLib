{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcPostgreSqlStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainPostgreSqlDriver,
  ZCompatibility, ZVariant, ZDbcGenericResolver,
  ZDbcPostgreSql, ZDbcUtils, ZClasses;

type
  /// <author>EgonHugeist</author>
  /// <summary>Defines array dml type</summary>
  TArrayDMLType = (dmlInsert = 1, dmlUpdate, dmlDelete);

  /// <author>EgonHugeist</author>
  /// <summary>forwards the TZPostgreSQLPreparedStatementV3 object</summary>
  TZPostgreSQLPreparedStatementV3 = class; //forward
  /// <author>EgonHugeist</author>
  /// <summary>Defines a record holding a batch array dml statement</summary>
  TPGArrayDMLStmt = record
    Obj: TZPostgreSQLPreparedStatementV3;
    Intf: IZPreparedStatement;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Defines a reference of the TZPostgreSQLBindValue record</summary>
  PZPostgreSQLBindValue = ^TZPostgreSQLBindValue;
  /// <author>EgonHugeist</author>
  /// <summary>Defines a BindValue record which widened the TZQMarkPosBindValue
  ///  by a ParameterName</summary>
  TZPostgreSQLBindValue = record
    /// <summary>the TZQMarkPosBindValue record</summary>
    BindValue:  TZQMarkPosBindValue;
    /// <summary>The parametername</summary>
    ParamName: RawByteString;
  end;

  /// <author>EgonHugeist</author>
  /// <summary>Implements a PostgreSQL Bindlist object</summary>
  TZPostgreSQLBindList = class(TZQuestionMarkBindList)
  protected
    /// <summary>Get the size of the custom element of this class.</summary>
    /// <returns>the size of the custom element.</returns>
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

  {** implements a abstract prepared statement for PostgreSQL protocol V3+ }

  TZAbstractPostgreSQLPreparedStatementV3 = class(TZRawParamDetectPreparedStatement)
  private
    FPostgreSQLConnection: IZPostgreSQLConnection;//local connection object
    FPlainDriver: TZPostgreSQLPlainDriver; //PG API
    FconnAddress: PPGconn; //the Connection-ROW_IDX
    Fres: TPGresult; //Current query handle we'd obtained
    FRawPlanName: RawByteString; //a name we use to prepare (oddly PG still has no handle instead)
    FOidAsBlob: Boolean; //are blob's threaded as oid-lobs?
    FBindDoubleAsString: Boolean; //compatibility for users who use doubles for the BCD fields
    FDoUpdateTimestampOffet: Boolean;
    Findeterminate_datatype, //did PG Fail to determine the datatypes? (mostly just because of bad queries)
    fAsyncQueries, //get the GetMoreResults logic with multiple results or SingleRowMode running
    fServerCursor, //PQsingleRowMode? is implizit the Async api
    FUseEmulatedStmtsOnly: Boolean; //no Prepareds?
    FUndefinedVarcharAsStringLength: Integer; //compatibility: in earlier version of Zeos the fieldlength for VARCHAR without given length was 255 now we assume text instead
    fPrepareCnt: Cardinal; //we have no stmt handle! The same object might be reused with another SQL
                           //and the previous used planname did run into the oddly broken transaction issue of pg
                           //we need a secondary uniqueness idea for the planname as a fallback
    Finteger_datetimes: Boolean;  //what's the binary format to bind datetimes? Integer or Double
    FPQparamValues: TPQparamValues; //postgres array of data
    FPQparamLengths: TPQparamLengths; //usually just for binary bindings used PG up until today loves the StrLen() instead of given lengtes for the strings (they can't store #0 bytes)
    FPQparamFormats: TPQparamFormats; //indicate binary or string format for the bindings
    FPQParamOIDs: TPQparamTypes; //the Parameter OID's
    FPQResultFormat: Integer; //which format will the tubles have binary or string? note if any unsupported oid  given we use string format only as a fallback
    FPGArrayDMLStmts: array[TArrayDMLType] of TPGArrayDMLStmt;
    FOutParamCount: Integer;
    FMinExecCount2Prepare: Integer; //how many executions must be done to fall into a real prepared mode?
    FExecCount: Integer; //How often did we execute the stmt until we reached MinExecCount2Prepare?
    FParamNames: TRawByteStringDynArray;
    FByteBuffer: PByteBuffer;
    function CheckPrepareSwitchMode: Boolean;
    procedure InternalRealPrepare;
    function OIDToSQLType(var Index: Integer; SQLType: TZSQLType): TZSQLType;
    function ExecuteDMLBatchWithUnnestVarlenaArrays: TPGresult;
    procedure LinkBinParam2PG(Index: Integer; Buf: Pointer; Len: LengthInt);
    procedure LinkTxtParam2PG(Index: Integer; Buf: Pointer; Len: LengthInt);
    procedure BindArray(Index: Cardinal; Stmt: TZPostgreSQLPreparedStatementV3); reintroduce;
  protected
    procedure SetBindCapacity(Capacity: Integer); override;
    procedure CheckParameterIndex(var Value: Integer); override;
    /// <summary>Removes the current connection reference from this object.</summary>
    /// <remarks>This method will be called only if the object is garbage.</remarks>
    procedure ReleaseConnection; override;
    class function GetBindListClass: TZBindListClass; override;
  protected
    procedure FlushPendingResults;
    function CreateResultSet(ServerCursor: Boolean): IZResultSet;
    function PGExecute: TPGresult; virtual;
    procedure PGExecutePrepare;
    function PGExecutePrepared: TPGresult;
    procedure PGExecuteUnPrepare;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; override;
  public
    /// <summary>Constructs this object and assigns main properties.</summary>
    /// <param>"Connection" a IZPostgreSQLConnection connection object which
    ///  creates this object</param>
    /// <param>"Sql" a Sql statement.</param>
    /// <param>"Info" a statement parameters list.</param>
    constructor Create(const Connection: IZPostgreSQLConnection;
      const SQL: string; Info: TStrings);
  public
    function GetRawEncodedSQL(const SQL: SQLString): RawByteString; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Prepare; override;
    procedure Unprepare; override;
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

  {** implements a prepared statement for PostgreSQL protocol V3+ }
  TZPostgreSQLPreparedStatementV3 = class(TZAbstractPostgreSQLPreparedStatementV3, IZPreparedStatement)
  protected
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); override;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); override;
    procedure BindRawStr(Index: Integer; const Value: RawByteString); override;

    procedure InternalBindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double);
    procedure InternalBindInt(Index: Integer; SQLType: TZSQLType; Value: {$IFNDEF CPU64}Integer{$ELSE}Int64{$ENDIF});
  protected
    procedure PrepareInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZSQLStringWriter; Var Result: SQLString); override;
  public
    /// <summary>Clears the current parameter values immediately.
    ///  In general, parameter values remain in force for repeated use of a
    ///  statement. Setting a parameter value automatically clears its
    ///  previous value.  However, in some cases it is useful to immediately
    ///  release the resources used by the current parameter values; this can
    ///  be done by calling the method <c>ClearParameters</c>.</summary>
    procedure ClearParameters; reintroduce;

    /// <summary>Sets the designated parameter to SQL <c>NULL</c>.
    ///  <B>Note:</B> You must specify the parameter's SQL type. </summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"SQLType" the SQL type code defined in <c>ZDbcIntfs.pas</c></param>
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    /// <summary>Sets the designated parameter to a <c>boolean</c> value.
    ///  The driver converts this to a SQL <c>Ordinal</c> value when it sends it
    ///  to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBoolean(Index: Integer; Value: Boolean);
    /// <summary>Sets the designated parameter to a <c>Byte</c> value.
    ///  If not supported by provider, the driver converts this to a SQL
    ///  <c>Ordinal</c> value when it sends it to the database.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetByte(Index: Integer; Value: Byte);
    procedure SetShort(Index: Integer; Value: ShortInt);
    /// <summary>Sets the designated parameter to a <c>Word</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetWord(Index: Integer; Value: Word);
    procedure SetSmall(Index: Integer; Value: SmallInt);
    procedure SetUInt(Index: Integer; Value: Cardinal);
    procedure SetInt(Index: Integer; Value: Integer);
    procedure SetULong(Index: Integer; const Value: UInt64);
    procedure SetLong(Index: Integer; const Value: Int64);
    procedure SetFloat(Index: Integer; Value: Single);
    procedure SetDouble(Index: Integer; const Value: Double);
    procedure SetCurrency(Index: Integer; const Value: Currency); reintroduce;
    /// <summary>Sets the designated parameter to a <c>BigDecimal(TBCD)</c> value.</summary>
    /// <param>"ParameterIndex" the first parameter is 1, the second is 2, ...
    ///  unless <c>GENERIC_INDEX</c> is defined. Then the first parameter is 0,
    ///  the second is 1. This will change in future to a zero based index.
    ///  It's recommented to use an incrementation of FirstDbcIndex.</param>
    /// <param>"Value" the parameter value</param>
    procedure SetBigDecimal(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD); reintroduce;

    procedure SetDate(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate); reintroduce; overload;
    procedure SetTime(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(Index: Integer; {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp); reintroduce; overload;

    procedure SetBytes(Index: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;

    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = '';
      {%H-}PrecisionOrSize: LengthInt = 0; {%H-}Scale: LengthInt = 0); override;
  end;

  {** implements a emulated prepared statement for PostgresSQL protocol V2 }
  TZPostgrePreparedStatementV2 = class(TZPostgreSQLPreparedStatementV3)
  public
    constructor Create(const Connection: IZPostgreSQLConnection;
      const SQL: string; Info: TStrings);
    procedure Unprepare; override;
  end;

  {** implements a statement for PostgresSQL }
  TZPostgreSQLStatement = class(TZPostgrePreparedStatementV2, IZStatement)
  public
    constructor Create(const Connection: IZPostgreSQLConnection;
      Info: TStrings); overload;
  end;

  TZPostgreSQLCallableStatement = class(TZAbstractCallableStatement_A, IZCallableStatement)
  protected
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; override;
  end;

const ZSQLType2PGBindTypes: array[stUnknown..stGUID] of TZBindType = (zbtNull,
    zbt4Byte{stBoolean},
    zbt4Byte{stByte}, zbt4Byte{stShort}, zbt4Byte{stWord},
    zbt4Byte{stSmall}, zbt4Byte{stLongWord}, zbt4Byte{stInteger}, zbt8Byte{stULong}, zbt8Byte{stLong},  //ordinals
    zbt4Byte{stFloat}, zbt8Byte{stDouble}, zbtCustom{stCurrency}, zbtCustom{stBigDecimal}, //floats
    zbt4Byte{stDate}, zbt8Byte{stTime}, zbt8Byte{stTimestamp},
    zbtGUID{stGUID});
{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}Types,
  ZSysUtils, ZFastCode, ZMessages, ZEncoding, ZTokenizer, ZDbcCachedResultSet,
  ZDbcPostgreSqlUtils, ZDbcProperties, ZDbcResultSet, ZDbcPostgreSqlResultSet
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

const
   cLoggingType: array[Boolean] of TZLoggingCategory = (lcExecPrepStmt,lcExecute);
var
  PGPreparableTokens: TPreparablePrefixTokens;

{ TZAbstractPostgreSQLPreparedStatementV3 }

procedure TZAbstractPostgreSQLPreparedStatementV3.BindArray(Index: Cardinal;
  Stmt: TZPostgreSQLPreparedStatementV3);
var
  SQLType: TZSQLType;

  procedure AllocArray(Index: Cardinal; TotalSize: Integer;
    out A: PArrayType; out Buf: PAnsiChar);
  var
    aOID: OID;
    BuffSize: Integer;
  begin
    BuffSize := ARR_OVERHEAD_NONULLS(1)+TotalSize;
    //alloc mem for the varlena array struct ->
    A := Stmt.BindList.AcquireMinCustomValue(Index, SQLType, BuffSize);
    SQLTypeToPostgreSQL(SQLType, FOidAsBlob, aOID);
    //write dimension(s)
    Integer2PG(1, ARR_NDIM(A));
    //indicate nullable items
    Integer2PG(0, @A.flags);
    //Write the OID
    Cardinal2PG(aOID, ARR_ELEMTYPE(A));
    //write item count
    Integer2PG(BatchDMLArrayCount, ARR_DIMS(A));
    //write lower bounds
    Integer2PG(1, ARR_LBOUND(A));
    Buf := ARR_DATA_PTR(A);
    Stmt.FPQparamValues[Index] := a;
    Stmt.FPQparamFormats[Index] := ParamFormatBin;
    Stmt.FPQparamLengths[Index] := BuffSize;
  end;
var
  OffSet, J: Cardinal;
  Dyn: Pointer;
  P: PAnsiChar;
  A: PArrayType;
  CP: word;
  X, N, DynArrayLen: Integer;
  Arr: PZArray;
  Native: Boolean;
  DT: TDateTime;
  TS: TZTimeStamp;
  Dat: TZDate absolute TS;
  T: TZTime absolute TS;
  Dbl: Double absolute TS;
  i64: Int64 absolute TS;
  cur: Currency absolute TS;
  PTS: PZTimeStamp;
  PD: PZDate absolute PTS;
  PT: PZTime absolute PTS;

  procedure BindLobs;
  var J: Cardinal;
    N: Integer;
    Lob: IZlob;
    CLob: IZCLob;
    BLob: IZBlob;
    WriteTempBlob: IZPostgreSQLOidBlob;
    PA: Pointer;
    L: NativeUInt;
  begin
    CP := ConSettings^.ClientCodePage.CP;
    N := 0;
    for J := 0 to DynArrayLen -1 do
      if (TInterfaceDynArray(Dyn)[j] <> nil) and Supports(TInterfaceDynArray(Dyn)[j], IZClob, Lob) and not Lob.IsEmpty then
        if TZSQLType(Arr.VArrayType) in [stUnicodeStream, stAsciiStream] then begin
          if Lob.QueryInterface(IZClob, CLob) = S_OK
          then CLob.SetCodePageTo(FClientCP)
          else raise CreateConversionError(Index, stBinaryStream, stAsciiStream);
          Clob.GetBuffer(FRawTemp, L);
          N := N + Integer(L);
        end else if Lob.QueryInterface(IZLob, BLob) = S_OK then begin
          if FOidAsBlob then begin
            if BLob.QueryInterface(IZPostgreSQLOidBlob, WriteTempBlob) <> S_OK then begin
              WriteTempBlob := TZPostgreSQLOidBlob.CreateFromBlob(Blob, FPostgreSQLConnection, FOpenLobStreams);
              TInterfaceDynArray(Dyn)[j] := WriteTempBlob;
            end;
            Inc(N, SizeOf(OID));
          end else begin
            Blob.GetBuffer(FRawTemp, L);
            Inc(N, Integer(L));
          end;
        end else raise CreateConversionError(Index, stBinaryStream, stBinaryStream);
    AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
    if (BindList.SQLtypes[Index] = stBinaryStream) and FOidAsBlob then begin
      for j := 0 to DynArrayLen -1 do
        if TInterfaceDynArray(Dyn)[j] = nil then begin
          Integer2PG(-1, P);
          Inc(P,SizeOf(int32));
        end else begin
          Integer2PG(SizeOf(OID), P);
          WriteTempBlob := TInterfaceDynArray(Dyn)[j] as IZPostgreSQLOidBlob;
          Cardinal2PG(WriteTempBlob.GetBlobOid,P+SizeOf(int32));
          Inc(P,SizeOf(int32)+SizeOf(OID));
        end;
    end else begin
      AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
      for J := 0 to DynArrayLen -1 do
        if (TInterfaceDynArray(Dyn)[j] = nil) or not Supports(TInterfaceDynArray(Dyn)[j], IZCLob, CLob) or CLob.IsEmpty then begin
          Integer2PG(-1, P);
          Inc(P,SizeOf(int32));
        end else begin
          PA := CLob.GetBuffer(FRawTemp, L);
          N := L;
          Integer2PG(N, P);
          Move(PA^, (PAnsiChar(P)+SizeOf(int32))^, N);
          Inc(P,SizeOf(int32)+N);
        end;
    end;
  end;
  procedure BindRawStrings;
  var J: Cardinal;
    N: Integer;
  begin
    N := 0;
    for j := 0 to DynArrayLen -1 do
      if not IsNullFromArray(Arr, j) then
        Inc(N, Length(TRawByteStringDynArray(Dyn)[j]));
    AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
    for j := 0 to DynArrayLen -1 do
      if IsNullFromArray(Arr, j) then begin
        Integer2PG(-1, P);
        Inc(P,SizeOf(int32));
      end else begin
        N := Length(TRawByteStringDynArray(Dyn)[j]);
        Integer2PG(N, P);
        Move(Pointer(TRawByteStringDynArray(Dyn)[j])^, (P+SizeOf(int32))^, N);
        Inc(P,SizeOf(int32)+N);
      end;
  end;
  procedure BindUniStrings;
  var J: Cardinal;
    N, charWidth, RawLen, MaxBytes: Integer;
  begin
    N := 0;
    charWidth := ConSettings^.ClientCodePage.CharWidth;
    for j := 0 to DynArrayLen -1 do
      if not IsNullFromArray(Arr, j) then
        Inc(N, Length(TUnicodeStringDynArray(Dyn)[j]));
    AllocArray(Index, (N*CharWidth)+(DynArrayLen*SizeOf(int32)), A, P);
    for j := 0 to DynArrayLen -1 do
      if IsNullFromArray(Arr, j) then begin
        Integer2PG(-1, P);
        Inc(P,SizeOf(int32));
      end else begin
        N := Length(TUnicodeStringDynArray(Dyn)[j]);
        MaxBytes := N*CharWidth;
        RawLen := ZEncoding.PUnicode2PRawBuf(Pointer(TUnicodeStringDynArray(Dyn)[j]), (P+SizeOf(int32)), N, MaxBytes, CP);
        Integer2PG(RawLen, P);
        Inc(P,SizeOf(int32)+RawLen);
        Dec(Stmt.FPQparamLengths[Index], (MaxBytes-RawLen));
      end;
  end;
  procedure BindConvertedStrings;
  var FTempRaws: TRawByteStringDynArray;
    J: Cardinal;
  begin
    {$IFDEF WITH_VAR_INIT_WARNING}FTempRaws := nil;{$ENDIF}
    SetLength(FTempRaws, DynArrayLen);
    case Arr.VArrayVariantType of
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString: for J := 0 to DynArrayLen -1 do begin
                      FUniTemp := PRawToUnicode(Pointer(TRawByteStringDynArray(Dyn)[j]), Length(TRawByteStringDynArray(Dyn)[j]), zOSCodePage);
                      FTempRaws[j] := PUnicodeToRaw(Pointer(FUniTemp), Length(FUnitemp), CP);
                    end;
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String: for J := 0 to DynArrayLen -1 do begin
                      FUniTemp := PRawToUnicode(Pointer(TRawByteStringDynArray(Dyn)[j]), Length(TRawByteStringDynArray(Dyn)[j]), zCP_UTF8);
                      FTempRaws[j] := PUnicodeToRaw(Pointer(FUniTemp), Length(FUnitemp), CP);
                    end;
      {$ENDIF}
      {$IFDEF UNICODE}
      vtString,
      {$ENDIF}
      vtUnicodeString: for J := 0 to DynArrayLen -1 do
                          FTempRaws[j] := ZUnicodeToRaw(TUnicodeStringDynArray(Dyn)[j], CP);
      vtCharRec:  for J := 0 to DynArrayLen -1 do
                    if (TZCharRecDynArray(Dyn)[j].CP = cp) or (TZCharRecDynArray(Dyn)[j].Len = 0) then
                      ZSetString(TZCharRecDynArray(Dyn)[j].P, TZCharRecDynArray(Dyn)[j].Len, FTempRaws[j])
                    else if (TZCharRecDynArray(Dyn)[j].CP = zCP_UTF16) then
                      FTempRaws[j] := PUnicodeToRaw(TZCharRecDynArray(Dyn)[j].P, TZCharRecDynArray(Dyn)[j].Len, CP)
                    else begin
                      fUniTemp := PRawToUnicode(TZCharRecDynArray(Dyn)[j].P, TZCharRecDynArray(Dyn)[j].Len, TZCharRecDynArray(Dyn)[j].CP);
                      FTempRaws[j] := ZUnicodeToRaw(fUniTemp, CP)
                    end;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} {inherited checked this already}
    end;
    Dyn := FTempRaws;
    BindRawStrings;
  end;
  {$if declared(vtSynRawUTF8Array)}
  procedure BindDequotedStrings;
  var J: Cardinal;
    L,N: LengthInt;
    Src: PAnsiChar;
  begin
    N := 0;
    for j := 0 to DynArrayLen -1 do
      if (TRawByteStringDynArray(Dyn)[j] <> 'null') then
        Inc(N, Length(TRawByteStringDynArray(Dyn)[j]));
    AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
    for j := 0 to DynArrayLen -1 do
      if (TRawByteStringDynArray(Dyn)[j] = 'null') then begin
        Integer2PG(-1, P);
        Inc(P,SizeOf(int32));
      end else begin
        L := Length(TRawByteStringDynArray(Dyn)[j]);
        N := L;
        Src := Pointer(TRawByteStringDynArray(Dyn)[j]);
        ZSysUtils.SQLDequotedStr(Src, (P+SizeOf(int32)), L, AnsiChar(#39));
        Integer2PG(L, P);
        Inc(P,SizeOf(int32)+L);
        N := N-L;
        Dec(Stmt.FPQparamLengths[Index], N);
      end;
  end;
  {$IFEND}
begin
  Arr := BindList[Index].Value; //localize -> next steps will free the memory
  SQLType := TZSQLType(Arr.VArrayType);
  Dyn := Arr.VArray;
  P := nil;
  Native := Arr.VArrayVariantType in NativeArrayValueTypes[SQLType];
  DynArrayLen := Length(TByteDynArray(Dyn));
  case SQLType of
    stBoolean:    begin
                    AllocArray(Index, SizeOf(Byte)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index]);
                      end else begin
                        Integer2PG(SizeOf(Byte), P);
                        if Native
                        then PByte(P+SizeOf(int32))^ := Ord(TBooleanDynArray(Dyn)[j])
                        else PByte(P+SizeOf(int32))^ := Ord(ArrayValueToBoolean(Arr, J));
                        Inc(P,SizeOf(int32)+SizeOf(Byte));
                      end;
                  end;
    stByte:       begin
                    AllocArray(Index, SizeOf(SmallInt)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(SmallInt));
                      end else begin
                        Integer2PG(SizeOf(SmallInt), P);
                        SmallInt2PG(TByteDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                      end;
                  end;
    stShort:      begin
                    AllocArray(Index, SizeOf(SmallInt)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(SmallInt));
                      end else begin
                        Integer2PG(SizeOf(SmallInt), P);
                        SmallInt2PG(TShortIntDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                      end;
                  end;
    stWord:       begin
                    AllocArray(Index, SizeOf(Integer)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Integer));
                      end else begin
                        Integer2PG(SizeOf(Integer), P);
                        SmallInt2PG(TWordDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Integer));
                      end;
                  end;
    stSmall:      begin
                    AllocArray(Index, SizeOf(SmallInt)*DynArrayLen+SizeOf(Int32)*DynArrayLen, A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(SmallInt));
                      end else begin
                        Integer2PG(SizeOf(SmallInt), P);
                        SmallInt2PG(TSmallIntDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(SmallInt));
                      end;
                  end;
    stInteger:    begin
                    AllocArray(Index, SizeOf(Integer)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Integer));
                      end else begin
                        Integer2PG(SizeOf(Integer), P);
                        Integer2PG(TIntegerDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Integer));
                      end;
                  end;
    stLongWord:   if (stmt.FPQParamOIDs[Index] = OIDOID) then begin
                    AllocArray(Index, SizeOf(OID)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(OID));
                      end else begin
                        Integer2PG(SizeOf(OID), P);
                        Cardinal2PG(TLongWordDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(OID));
                      end;
                  end else begin
                    AllocArray(Index, SizeOf(Int64)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(LongWord), P);
                        Int642PG(TLongWordDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Int64));
                      end;
                  end;
    stLong:       begin
                    AllocArray(Index, SizeOf(Int64)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j)
                      {$IF declared(vtSynRawUTF8Array)}
                      or ((Arr.VArrayVariantType = vtSynRawUTF8Array) and
                         (TRawByteStringDynArray(Dyn)[j] = 'null')) {$IFEND}then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(Int64), P);
                        if Native
                        then Int642PG(TInt64DynArray(Dyn)[j],P+SizeOf(int32))
                        else begin
                          i64 := ArrayValueToInt64(Arr, j);
                          Int642PG(i64, P+SizeOf(int32));
                        end;
                        Inc(P,SizeOf(int32)+SizeOf(Int64));
                      end;
                  end;
    stULong:      begin
                    AllocArray(Index, SizeOf(UInt64)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(Int64), P);
                        Int642PG(Int64(TUInt64DynArray(Dyn)[j]),P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(UInt64));
                      end;
                  end;
    stFloat:      begin
                    AllocArray(Index, SizeOf(Single)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Single));
                      end else begin
                        Integer2PG(SizeOf(Single), P);
                        Single2PG(TSingleDynArray(Dyn)[j],P+SizeOf(int32));
                        Inc(P,SizeOf(int32)+SizeOf(Single));
                      end;
                  end;
    stDouble:     begin
                    AllocArray(Index, SizeOf(Double)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j)
                      {$IF declared(vtSynRawUTF8Array)}
                      or ((Arr.VArrayVariantType = vtSynRawUTF8Array) and
                         (TRawByteStringDynArray(Dyn)[j] = 'null')) {$IFEND}then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Int64));
                      end else begin
                        Integer2PG(SizeOf(Double), P);
                        if Native
                        then Double2PG(TDoubleDynArray(Dyn)[j],P+SizeOf(int32))
                        else begin
                          Dbl := ArrayValueToDouble(Dyn,J);
                          Double2PG(Dbl, P+SizeOf(int32));
                        end;
                        Inc(P,SizeOf(int32)+SizeOf(Double));
                      end;
                  end;
    stCurrency:   if (stmt.FPQParamOIDs[Index] = CASHOID) then begin
                    AllocArray(Index, 8*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j)
                      {$IF declared(vtSynRawUTF8Array)}
                      or ((Arr.VArrayVariantType = vtSynRawUTF8Array) and
                         (TRawByteStringDynArray(Dyn)[j] = 'null')) {$IFEND}then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], 8);
                      end else begin
                        Integer2PG(8, P);
                        if Native
                        then Currency2PGCash(TCurrencyDynArray(Dyn)[j],P+SizeOf(int32))
                        else begin
                          Cur := ArrayValueToCurrency(Arr, J);
                          Currency2PGCash(Cur,P+SizeOf(int32))
                        end;
                        Inc(P,SizeOf(int32)+8);
                      end;
                  end else begin
                    AllocArray(Index, MaxCurr2NumSize*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j)
                        {$IF declared(vtSynRawUTF8Array)}
                        or ((Arr.VArrayVariantType = vtSynRawUTF8Array) and
                           (TRawByteStringDynArray(Dyn)[j] = 'null')) {$IFEND}then begin
                        Integer2PG(-1, P);
                        X := SizeOf(int32);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], MaxCurr2NumSize);
                      end else begin
                        if Native
                        then Currency2PGNumeric(TCurrencyDynArray(Dyn)[j], P+SizeOf(int32), x)
                        else begin
                          Cur := ArrayValueToCurrency(Arr, J);
                          Currency2PGNumeric(Cur,P+SizeOf(int32), x);
                        end;
                        Integer2PG(X, P);
                        Inc(P,SizeOf(int32)+X);
                        Dec(Stmt.FPQparamLengths[Index], MaxCurr2NumSize-X);
                      end
                  end;
    stBigDecimal: begin
                    AllocArray(Index, MaxBCD2NumSize*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], MaxBCD2NumSize);
                      end else begin
                        BCD2PGNumeric(TBCDDynArray(Dyn)[j], P+SizeOf(int32), x);
                        Integer2PG(X, P);
                        Inc(P,SizeOf(int32)+X);
                        Dec(Stmt.FPQparamLengths[Index], MaxBCD2NumSize-X);
                      end;
                  end;
    stDate:       begin
                    AllocArray(Index, SizeOf(Integer)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(Integer));
                      end else begin
                        if Arr.VArrayVariantType = vtDate then
                          PD := @TZDateDynArray(Dyn)[j]
                        else begin
                          PD := @Dat;
                          if Arr.VArrayVariantType = vtTimeStamp then
                            DateFromTimeStamp(TZTimeStampDynArray(Dyn)[j], Dat)
                          else if Arr.VArrayVariantType = vtTime then
                            FillChar(Dat, SizeOf(TZDate), #0)
                          else if (Arr.VArrayVariantType in [vtNull, vtDateTime])
                          then DecodeDateTimeToDate(TDateTimeDynArray(Dyn)[j], Dat)
                          else begin
                            DT := ArrayValueToDate(Arr, J, ConSettings^.WriteFormatSettings);
                            DecodeDateTimeToDate(DT, Dat);
                          end;
                        end;
                        Date2PG(PD^.Year, PD^.Month, PD^.Day, PInteger(PAnsiChar(P)+SizeOf(int32))^);
                        Integer2PG(SizeOf(Integer), P);
                        Date2PG(TDateTimeDynArray(Dyn)[j], PInteger(PAnsiChar(P)+SizeOf(int32))^);
                        Inc(P,SizeOf(int32)+SizeOf(Integer));
                      end;
                  end;
    stTime:       begin
                    AllocArray(Index, 8*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], 8);
                      end else begin
                        if Arr.VArrayVariantType = vtTime then
                          PT := @TZTimeDynArray(Dyn)[J]
                        else begin
                          PT := @T;
                          if (Arr.VArrayVariantType = vtTimeStamp) then
                            TimeFromTimeStamp(TZTimestampDynArray(Dyn)[J], T)
                          else if (Arr.VArrayVariantType = vtDate) then
                            FillChar(T, SizeOf(TZTime), #0)
                          else if (Arr.VArrayVariantType in [vtNull, vtDateTime])
                          then DecodeDateTimeToTime(TDateTimeDynArray(Dyn)[J], T)
                          else begin
                            DT := ArrayValueToTime(Arr, J, ConSettings^.WriteFormatSettings);
                            DecodeDateTimeToTime(DT, T);
                          end;
                        end;
                        Integer2PG(8, P);
                        if Finteger_datetimes
                        then Time2PG(PT^.Hour, PT^.Minute, PT^.Second, PT^.Fractions, PInt64(PAnsiChar(P)+SizeOf(int32))^)
                        else Time2PG(PT^.Hour, PT^.Minute, PT^.Second, PT^.Fractions, PDouble(PAnsiChar(P)+SizeOf(int32))^);
                        Inc(P,SizeOf(int32)+8);
                      end
                  end;
    stTimeStamp:  begin
                    AllocArray(Index, 8*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j)
                      {$IF declared(vtSynRawUTF8Array)}
                      or ((Arr.VArrayVariantType = vtSynRawUTF8Array) and
                         (TRawByteStringDynArray(Dyn)[j] = 'null')) {$IFEND}then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], 8);
                      end else begin
                        if Arr.VArrayVariantType = vtTimeStamp then
                          PTS := @TZTimeStampDynArray(Dyn)[J]
                        else begin
                          PTS := @TS;
                          if (Arr.VArrayVariantType = vtDate) then
                            TimestampFromDate(TZDateDynArray(Dyn)[J], TS)
                          else if (Arr.VArrayVariantType = vtTime) then
                            TimestampFromTime(TZTimeDynArray(Dyn)[J], TS)
                          else if (Arr.VArrayVariantType in [vtNull, vtDateTime])
                          then DecodeDateTimeToTimeStamp(TDateTimeDynArray(Dyn)[J], TS)
                          else begin
                            DT := ArrayValueToDatetime(Arr, J, ConSettings^.WriteFormatSettings);
                            DecodeDateTimeToTimeStamp(DT, TS);
                          end;
                        end;
                        Integer2PG(8, P);
                        if Finteger_datetimes
                        then TimeStamp2PG(PTS^, PInt64(PAnsiChar(P)+SizeOf(int32))^)
                        else TimeStamp2PG(PTS^, PDouble(PAnsiChar(P)+SizeOf(int32))^);
                        Inc(P,SizeOf(int32)+8);
                      end
                  end;
    stGUID:       begin
                    AllocArray(Index, SizeOf(TGUID)*DynArrayLen+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                        Dec(Stmt.FPQparamLengths[Index], SizeOf(TGUID));
                      end else begin
                        Integer2PG(SizeOf(TGUID), P);
                        //eh: Network byteOrder?
                        PGUID(PAnsiChar(P)+SizeOf(int32))^ := TGUIDDynArray(Dyn)[j];
                        Inc(P,SizeOf(int32)+SizeOf(TGUID));
                      end
                  end;
    stBytes:      begin
                    N := 0;
                    for J := 0 to DynArrayLen -1 do
                      if not (IsNullFromArray(Arr, j) or (Pointer(TBytesDynArray(Dyn)[j]) = nil)) then
                        Inc(N, Length(TBytesDynArray(Dyn)[j]));
                    AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
                    for j := 0 to DynArrayLen -1 do
                      if IsNullFromArray(Arr, j) or (Pointer(TBytesDynArray(Dyn)[j]) = nil) then begin
                        Integer2PG(-1, P);
                        Inc(P,SizeOf(int32));
                      end else begin
                        N := Length(TBytesDynArray(Dyn)[j]);
                        Integer2PG(N, P);
                        //eh: Network byteOrder?
                        Move(Pointer(TBytesDynArray(Dyn)[j])^, Pointer(PAnsiChar(P)+SizeOf(int32))^, N);
                        Inc(P,SizeOf(int32)+N);
                      end
                  end;
    stString, stUnicodeString: begin
        CP := ConSettings^.ClientCodePage.CP;
        case Arr.VArrayVariantType of
          {$if declared(vtSynRawUTF8Array)}
          vtSynRawUTF8Array: if (CP = zCP_UTF8)
                          then BindDequotedStrings;
          {$IFEND}
          {$IFNDEF UNICODE}

          vtString:       BindRawStrings;
          {$ENDIF}
          {$IFNDEF NO_ANSISTRING}
          vtAnsiString:   if (CP= ZOSCodePage)
                          then BindRawStrings
                          else BindConvertedStrings;
          {$ENDIF}
          {$IFNDEF NO_UTF8STRING}
          vtUTF8String:   if (CP = zCP_UTF8)
                          then BindRawStrings
                          else BindConvertedStrings;
          {$ENDIF}
          vtRawByteString:BindRawStrings;
          {$IFDEF UNICODE}
          vtString,
          {$ENDIF}
          vtUnicodeString: BindUniStrings;
          vtCharRec:  begin
                        N := 0;
                        OffSet := DynArrayLen;
                        for J := 0 to DynArrayLen -1 do begin
                          Dec(OffSet, Ord(not (((TZCharRecDynArray(Dyn)[j].CP = cp) and (TZCharRecDynArray(Dyn)[j].Len > 0) and not IsNullFromArray(Arr, j)))));
                          Inc(N, Integer(TZCharRecDynArray(Dyn)[j].Len)*Ord(not IsNullFromArray(Arr, j)));
                        end;
                        if OffSet <> Cardinal(DynArrayLen) then
                          BindConvertedStrings
                        else begin
                          AllocArray(Index, N+(DynArrayLen*SizeOf(int32)), A, P);
                          for J := 0 to DynArrayLen -1 do
                            if IsNullFromArray(Arr, j) then begin
                              Integer2PG(-1, P);
                              Inc(P,SizeOf(int32));
                            end else begin
                              N := TZCharRecDynArray(Dyn)[j].Len;
                              Integer2PG(N, P);
                              Move(TZCharRecDynArray(Dyn)[j].P^, Pointer(PAnsiChar(P)+SizeOf(int32))^, N);
                              Inc(P,SizeOf(int32)+N);
                            end;
                        end;
                      end;
          else
            raise Exception.Create('Unsupported String Variant');
        end;
      end;
    stAsciiStream, stUnicodeStream, stBinaryStream: BindLobs;
    else raise ZdbcUtils.CreateUnsupportedParameterTypeException(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, SQLType);
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.CheckParameterIndex(var Value: Integer);
var I: Integer;
begin
  if not Prepared then
    Prepare;
  if (BindList.Capacity < Value+1) then
    if fRawPlanname <> '' then begin
      {$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF} := Format(SBindVarOutOfRange, [Value]);
      raise EZSQLException.Create({$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF});
    end else inherited CheckParameterIndex(Value);
  { now change the index to the !in!-param ordinal index }
  if BindList.HasOutOrInOutOrResultParam then
    for I := 0 to Value do
      if Ord(BindList[I].ParamType) > Ord(pctInOut) then
        Dec(Value);
end;

function TZAbstractPostgreSQLPreparedStatementV3.CheckPrepareSwitchMode: Boolean;
begin
  Result := (FRawPlanName = '') and not Findeterminate_datatype and ((not FUseEmulatedStmtsOnly) or (BatchDMLArrayCount > 0 )) and
    (TokenMatchIndex <> -1) and ((BatchDMLArrayCount > 0 ) or (FExecCount = FMinExecCount2Prepare));
end;

constructor TZAbstractPostgreSQLPreparedStatementV3.Create(
  const Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
begin
  FByteBuffer := Connection.GetByteBufferAddress;
  FPlainDriver := Connection.GetPlainDriver;
  inherited Create(Connection, SQL, Info);
  FPostgreSQLConnection := Connection;
  FOidAsBlob := StrToBoolEx(Self.Info.Values[DSProps_OidAsBlob]) or Connection.IsOidAsBlob;
  //ResultSetType := rtScrollInsensitive;
  FconnAddress := Connection.GetPGconnAddress;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, DSProps_UndefVarcharAsStringLength, '0'));
  { see http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151
    the pgBouncer does not support the RealPrepareds.... }
  FUseEmulatedStmtsOnly := not Assigned(FplainDriver.PQexecParams) or not Assigned(FplainDriver.PQexecPrepared) or
    StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_EmulatePrepares, 'FALSE'));
  FBindDoubleAsString := FUseEmulatedStmtsOnly or StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, ConnProps_BindDoublesAsString, 'FALSE'));
  Findeterminate_datatype := FUseEmulatedStmtsOnly;
  Finteger_datetimes := Connection.integer_datetimes;
  if Assigned(FPlainDriver.PQexecParams) and StrToBoolEx(DefineStatementParameter(Self, DSProps_BinaryWireResultMode, 'TRUE'))
  then FPQResultFormat := ParamFormatBin
  else FPQResultFormat := ParamFormatStr;
  fPrepareCnt := 0;
  //JDBC prepares after 4th execution
  if not FUseEmulatedStmtsOnly //and not StrToBoolEx(DefineStatementParameter(Self, DSProps_PreferPrepared, 'False'))
  then FMinExecCount2Prepare := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, DSProps_MinExecCntBeforePrepare, '2'), 2)
  else FMinExecCount2Prepare := -1;
  fAsyncQueries := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_ExecAsync, 'FALSE'))
    and Assigned(FplainDriver.PQsendQuery) and Assigned(FplainDriver.PQsendQueryParams) and
    Assigned(FplainDriver.PQsendQueryPrepared);
  fServerCursor := fAsyncQueries and StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, DSProps_SingleRowMode, 'FALSE'));
end;

function TZAbstractPostgreSQLPreparedStatementV3.CreateResultSet(
  ServerCursor: Boolean): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
  Resolver: TZPostgreSQLCachedResolver;
  Metadata: IZResultSetMetadata;
  ServerMajorVersion: Integer;
begin
  NativeResultSet := TZPostgreSQLResultSet.Create(Self, Self.SQL, FPostgreSQLConnection,
    @Fres, @FPQResultFormat, fServerCursor, FUndefinedVarcharAsStringLength);
  if (GetResultSetConcurrency = rcUpdatable) or (ServerCursor and (GetResultSetType <> rtForwardOnly)) then begin
    Metadata := NativeResultSet.GetMetaData;
    ServerMajorVersion := FPostgreSQLConnection.GetServerMajorVersion;
    if (ServerMajorVersion >= 10) then
      Resolver := TZPostgreSQLCachedResolverV10up.Create(Self, Metadata)
    else if (ServerMajorVersion > 7) then
      Resolver := TZPostgreSQLCachedResolverV8up.Create(Self, Metadata)
    else if ((ServerMajorVersion = 7) and (FPostgreSQLConnection.GetServerMinorVersion >= 4))
    then Resolver := TZPostgreSQLCachedResolverV74up.Create(Self, Metadata)
    else Resolver := TZPostgreSQLCachedResolver.Create(Self, Metadata);
    if (LobCacheMode = lcmOnLoad)
    then CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL, Resolver, ConSettings)
    else CachedResultSet := TZPostgresCachedResultSet.Create(NativeResultSet, Self.SQL, Resolver, ConSettings);
    if (GetResultSetConcurrency = rcUpdatable) then begin
      CachedResultSet.SetConcurrency(rcUpdatable);
      if ServerCursor then begin //pg is tabular streamed, break blocking for next queries
        CachedResultSet.Last;
        CachedResultSet.BeforeFirst;
      end;
    end;
    Result := CachedResultSet;
  end else
    Result := NativeResultSet;
  FOpenResultSet := Pointer(Result);
end;

function TZAbstractPostgreSQLPreparedStatementV3.ExecuteDMLBatchWithUnnestVarlenaArrays: TPGresult;
var
  Stmt: TZPostgreSQLPreparedStatementV3;
  I: Cardinal;
  function CreateBatchDMLStmt: TZPostgreSQLPreparedStatementV3;
  var I, LastPos, L: Cardinal;
    aOID: OID;
    P, PEnd: PAnsiChar;
    BindValue: PZBindValue absolute PEnd;
    QMarkBindValue: PZQMarkPosBindValue absolute PEnd;
    PQBindValue: PZPostgreSQLBindValue absolute PEnd;
    SQL: RawByteString;
    SQLWriter: TZRawSQLStringWriter;
  begin
//EgonHugeist: introduction
// the posgres server support bulk ops since 8.4
// but the free version of libpq/server simply misses this API:
// https://www.enterprisedb.com/docs/en/9.4/eeguide/Postgres_Plus_Enterprise_Edition_Guide.1.088.html
// its made for pay customers only

//so find something we can do with the free libpq:

//diaray:
//first approach:
//pg 9.6 pipelined batch of https://github.com/2ndQuadrant/postgres/tree/dev/libpq-async-batch
//see: https://www.postgresql.org/message-id/flat/CAMsr+YFUjJytRyV4J-16bEoiZyH=4nj+sQ7JP9ajwz=B4dMMZw@mail.gmail.com#CAMsr+YFUjJytRyV4J-16bEoiZyH=4nj+sQ7JP9ajwz=B4dMMZw@mail.gmail.com

//to behonest:
// It's a bit(1.5x) faster than single executions because of the batch
// and onetime send of all queries. But we need the compiled version of 2ndQuadrant
//code goes here :
(*
  if FPlainDriver.PQbeginBatchMode(FconnAddress^) = Ord(PGRES_COMMAND_OK) then begin
   ///bind data
      if FplainDriver.PQsendQueryPrepared(FconnAddress^, Pointer(FRawPlanName), FInParamCount,
         Pointer(FPQparamValues), Pointer(FPQparamLengths), Pointer(FPQparamFormats),
         ParamFormatStr) <> Ord(PGRES_COMMAND_OK) then
      HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcExecPrepStmt, 'PQsendQueryPrepared', nil);
    end;
    if not FplainDriver.PQsendEndBatch(FconnAddress^)  <> Ord(PGRES_COMMAND_OK) then
       HandlePostgreSQLError(Self, FplainDriver, FconnAddress^, lcExecPrepStmt, 'PQsendQueryPrepared', nil);
    J := 0;
    while True do begin
      //now we've to obtain all resuls, else we can't leave the batchmode
      I := FplainDriver.PQgetNextQuery(FconnAddress^);
      if i = Ord(PGRES_BATCH_END) then
        Break;
      P := FplainDriver.PQgetResult(FconnAddress^);
      if P <> nil then begin
        FplainDriver.PQclear(P);
        Inc(J);
      end else
        break;
    end;
    if J <> BatchDMLArrayCount+1 then
       raise Exception.Create('Fehlermeldung');
    if FplainDriver.PQisInBatchMode(FconnAddress^) = Ord(PGRES_COMMAND_OK) then
      FplainDriver.PQEndBatchMode(FconnAddress^);
*) //->>>>> trash, worth in vain

//second approach:
//sadly postgres "DO" anonymous block does not support paramaters !

//so create a function with array params as input and execute the dml in a loop
//works ok and is portable for all kind of user queries.
//but we've a sp now which first need to be created and if we unprepare the stmt
//we also nee to drop the function again
//what about name collisions ?
//perfornmance is 4x faster than single executions
// example

{  function "sp_in_batch_example" (int[],text[]..)
returns int[] as $$
declare i INT;
begin
   FOR i IN 1 .. array_upper($1, 1)
    LOOP
       insert into "example" values(nextval('"example_ID_Seq"'),$1[i],$2[i]);
    END LOOP;

   return $1;
end;
$$ LANGUAGE plpgsql;}

// EH: my third approach ..
// using unnest(array[$1,$2.....]::bigint[]
// performance is 9x faster than single executions
// but we've to send data chunked -> using more than 5000 params lead to performance loss again even if 32/64k are supported
// and there is the final chunk of x params for the unprepared stmt
(*
insert into public.SampleRecord (ID,FirstName,LastName,Amount,BirthDate,LastChange,CreatedAt) values (
unnest(array[$1]::int8[])
,unnest(array[$2]::text[])
,unnest(array[3]::text[])
,unnest(array[$4]::numeric[])
,unnest(array[$5]::timestamp[])
,unnest(array[$6]::int8[])
,unnest(array[$7]::int8[]))
*)
//my final and fastest approch bind binary arrays and pass data once per param and one prepared stmt:
    SQL := '';
    SQLWriter := TZRawSQLStringWriter.Create(Length(fASQL)+(BindList.Count shl 4));
    P := Pointer(FASQL);
    if FTokenMatchIndex = Ord(dmlDelete) then begin
      LastPos := PZQMarkPosBindValue(BindList[0]).QMarkPosition-1;
      PEnd := P + LastPos;
      //Take care the "=" oparater gets replaced by and "in" op if not in brackets already
      while (PEnd > P) and (PByte(PEnd)^ <= Byte(' ')) do
        Dec(PEnd);
      if (PEnd > P) and (PByte(PEnd)^ = Byte('=')) then begin
        SQLWriter.AddText(P, PEnd - P, SQL);
        SQLWriter.AddText(' in ', SQL);
      end else
        SQLWriter.AddText(P, LastPos, SQL);
    end else LastPos := 0;
    //first build up a new string with unnest($n)::xyz[] surrounded params
    for I := 0 to BindList.Count -1 do begin
      BindValue := BindList[I];
      SQLWriter.AddText(P+LastPos, QMarkBindValue.QMarkPosition - LastPos, SQL);
      L := Length(PQBindValue.ParamName);
      if BindValue.BindType in [zbtArray, zbtRefArray] then begin
        if FTokenMatchIndex = Ord(dmlDelete) then
          SQLWriter.AddText('(select ', SQL);
        SQLWriter.AddText('unnest(', SQL);
        SQLWriter.AddText(Pointer(PQBindValue.ParamName), L, SQL);
        SQLWriter.AddText('::', SQL);
        SQLTypeToPostgreSQL(TZSQLType(PZArray(BindValue.Value).VArrayType), FOidAsBlob, aOID);
        fRawTemp := {$IFDEF UNICODE}ZSysUtils.UnicodeStringToASCII7{$ENDIF}(FPostgreSQLConnection.GetTypeNameByOid(aOID));
        SQLWriter.AddText(fRawTemp, SQL);
        SQLWriter.AddText('[])', SQL);
        if FTokenMatchIndex = Ord(dmlDelete) then
          SQLWriter.AddChar(AnsiChar(')'), SQL);
      end else
        SQLWriter.AddText(Pointer(PQBindValue.ParamName), L, SQL);
      LastPos := QMarkBindValue.QMarkPosition + L;
    end;
    L := Length(FASQL);
    SQLWriter.AddText(P+LastPos, L - LastPos, SQL);
    SQLWriter.Finalize(SQL);
    FreeAndNil(SQLWriter);
  (* gives such a string:
  insert into public.SampleRecord (ID,FirstName,LastName,Amount,BirthDate,LastChange,CreatedAt) values (
  unnest($1::int8[])
  ,unnest($2::text[])
  ,unnest($3::text[])
  ,unnest($4::float8[])
  ,unnest($5::timestamp[])
  ,unnest($6::int8[])
  ,unnest($7::int8[]))
  or
  delete from public.SampleRecord where ID in (select unnest($1::int8[]))

  This approach took me ages. No description somewhere, just two (not working) example on
  Stack overflow... The Array.h ported Macros -> Trash !
  Postgres makes differences between external and internal format.
  The external format passes all items as verlena struct and uses Len=-1 as NullIndicator
  If null we've to send the length indicator only else each value is passed by length+bytes of val

  However all findings happen on debugging the postgres server -> thanks to Jan for helping me.

  *)
    Result := TZPostgreSQLPreparedStatementV3.Create(FPostgreSQLConnection, '', Info);
    Result.FaSQL := SQL;
    Result.SetParamCount(BindList.Count);
    Result.InternalRealPrepare; //force describe the params to get the array oids
  end;
begin
  if FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Intf = nil then begin
    Stmt := CreateBatchDMLStmt;
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj := Stmt;
    FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Intf := Stmt;
  end else
    Stmt := FPGArrayDMLStmts[TArrayDMLType(FTokenMatchIndex)].Obj;
  for i := 0 to bindList.Count -1 do
    if BindList[I].BindType in [zbtArray, zbtRefArray] then
      BindArray(i, stmt);
  Result := Stmt.PGExecutePrepared;
  Stmt.BindList.ClearValues; //free allocated mem
  if not PGSucceeded(FPlainDriver.PQerrorMessage(FconnAddress^)) then
    FPostgreSQLConnection.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecPrepStmt, Stmt.SQL, Self, Result);
end;

function TZAbstractPostgreSQLPreparedStatementV3.PGExecute: TPGresult;
var
  PError: PAnsiChar;
  Status: TZPostgreSQLExecStatusType;
  label ExecWithParams;
  function ExecEmulated: TPGresult;
  var TmpSQL, tmp: RawByteString;
    I, N: Integer;
    BindValue: PZBindValue;
    QMarkBindValue: PZQMarkPosBindValue absolute BindValue;
    PostgreSQLBindValue: PZPostgreSQLBindValue absolute BindValue;
    SQLWriter: TZRawSQLStringWriter;
    Data, PA: Pointer;
    P: PAnsiChar;
    L, LastPos: NativeUInt;
    BCD: TBCD;
    I64: Int64 absolute BCD;
    C: Currency absolute BCD;
    TS: TZTimeStamp absolute BCD;
    D: TZDate absolute BCD;
    T: TZTime absolute BCD;
  begin
    L := Length(FASQL);
    N := BindList.Count shl 4;
    I := L + Cardinal(N);
    if BindList.Count = 0
    then TmpSQL := FASQL
    else begin
      SQLWriter := TZRawSQLStringWriter.Create(I);
      TmpSQL := '';
      P := Pointer(FASQL);
      LastPos := 0;
      try
        for I := 0 to BindList.Count -1 do begin
          BindValue := BindList[I];
          SQLWriter.AddText((P+LastPos), QMarkBindValue.QMarkPosition - LastPos, TmpSQL);
          If AnsiChar((P + QMarkBindValue.QMarkPosition)^) = AnsiChar('?')
          then LastPos := QMarkBindValue.QMarkPosition + 1
          else LastPos := QMarkBindValue.QMarkPosition + NativeUInt(Length(PostgreSQLBindValue.ParamName));
          case BindValue.BindType of
            zbtNull: SQLWriter.AddText('null', TmpSQL);
            zbt4Byte: begin
                Data := FPQparamValues[i];
                case FPQParamOIDs[I] of
                  BOOLOID:  SQLWriter.AddOrd(Ord(Data <> nil), TmpSQL);
                  INT2OID:  SQLWriter.AddOrd(PG2SmallInt(Data), TmpSQL);
                  INT4OID:  SQLWriter.AddOrd(PG2Integer(Data), TmpSQL);
                  OIDOID:   SQLWriter.AddOrd(PG2Cardinal(Data), TmpSQL);
                  FLOAT4OID:SQLWriter.AddFloat(PG2Single(Data), TmpSQL);
                  DATEOID:  begin
                              PG2Date(PInteger(Data)^, D.Year, D.Month, d.Day);
                              D.IsNegative := False;
                              SQLWriter.AddDate(D, ConSettings^.WriteFormatSettings.DateFormat, TmpSQL);
                              SQLWriter.AddText('::date', TmpSQL);
                            end;
                end;
              end;
            zbt8Byte: begin
                Data := FPQparamValues[i];
                case FPQParamOIDs[I] of
                  INT8OID:  SQLWriter.AddOrd(PG2Int64(Data), TmpSQL);
                  FLOAT8OID:SQLWriter.AddFloat(PG2Double(Data), TmpSQL);
                  CASHOID:  begin
                              i64 := PG2Int64(Data) * 100;
                              SQLWriter.AddDecimal(C, TmpSQL);
                            end;
                  TIMEOID:  begin
                            if Finteger_datetimes
                            then PG2Time(PInt64(Data)^, T.Hour, T.Minute, T.Second, T.Fractions)
                            else PG2Time(PDouble(Data)^, T.Hour, T.Minute, T.Second, T.Fractions);
                            T.IsNegative := False;
                            SQLWriter.AddTime(T, ConSettings^.WriteFormatSettings.TimeFormat, TmpSQL);
                            SQLWriter.AddText('::time', TmpSQL);
                          end;
                  TIMESTAMPOID: begin
                            if Finteger_datetimes
                            then PG2DateTime(PInt64(Data)^, 0, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions)
                            else PG2DateTime(PDouble(Data)^, 0, TS.Year, TS.Month, TS.Day, TS.Hour, TS.Minute, TS.Second, TS.Fractions);
                            TS.IsNegative := False;
                            SQLWriter.AddTimeStamp(TS, ConSettings^.WriteFormatSettings.DateTimeFormat, TmpSQL);
                            SQLWriter.AddText('::timestamp', TmpSQL);
                          end;
                end;
              end;
            zbtRawString, zbtUTF8String {$IFNDEF NEXTGEN}, zbtAnsiString{$ENDIF}: begin
                FPostgreSQLConnection.GetEscapeString(PAnsiChar(BindValue.Value), Length(RawByteString(BindValue.Value)), Tmp);
                SQLWriter.AddText(Tmp, TmpSQL);
              end;
            zbtCharByRef: begin
                            FPostgreSQLConnection.GetEscapeString(PAnsiChar(PZCharRec(BindValue.Value)^.P), PZCharRec(BindValue.Value)^.Len, Tmp);
                            SQLWriter.AddText(Tmp, TmpSQL);
                          end;
            zbtBinByRef:  begin
                            FPostgreSQLConnection.GetBinaryEscapeString(PZBufRec(BindValue.Value).Buf, PZBufRec(BindValue.Value).Len, Tmp);
                            SQLWriter.AddText(Tmp, TmpSQL);
                          end;
            zbtGUID:      SQLWriter.AddGUID(PGUID(BindValue.Value)^, [guidWithBrackets, guidQuoted], TmpSQL);
            zbtBytes:     begin
                            FPostgreSQLConnection.GetBinaryEscapeString(BindValue.Value, Length(TBytes(BindValue.Value)), Tmp);
                            SQLWriter.AddText(Tmp, TmpSQL);
                          end;
            zbtLob: begin
                      PA := IZBlob(BindValue.Value).GetBuffer(FrawTemp, L);
                      if BindValue.SQLType = stBinaryStream
                      then FPostgreSQLConnection.GetBinaryEscapeString(PA, L, Tmp)
                      else FPostgreSQLConnection.GetEscapeString(PA, L, Tmp);
                      SQLWriter.AddText(Tmp, TmpSQL);
                    end;
            zbtPointer: SQLWriter.AddOrd(Ord(BindValue.Value <> nil), TmpSQL);
            zbtCustom: if BindValue.SQLType = stArray
                        then SQLWriter.AddText('(Array to complete)', TmpSQL)
                        else if BindValue.SQLType = stCurrency
                        then SQLWriter.AddDecimal(PGNumeric2Currency(PAnsiChar(BindValue.Value)+SizeOf(LengthInt)), TmpSQL)
                        else begin
                          PGNumeric2BCD(PAnsiChar(BindValue.Value)+SizeOf(LengthInt), BCD{%H-});
                          SQLWriter.AddDecimal(BCD, TmpSQL);
                        end;
            {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} {inherited checked this already}
          end;
        end;
        L := Length(FASQL);
        SQLWriter.AddText(P+LastPos, L - LastPos, TmpSQL);
        SQLWriter.Finalize(TmpSQL);
      finally
        SQLWriter.Free;
      end;
    end;
    if (FPQResultFormat = ParamFormatBin)
    then Result := FPlainDriver.PQexecParams(FconnAddress^, Pointer(TmpSQL),
        0, nil, nil, nil, nil, ParamFormatBin)
    else Result := FPlainDriver.PQExec(FconnAddress^, Pointer(TmpSQL));
  end;
begin
  { logs the values }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  RestartTimer;
  Result := nil;
  if not Assigned(FconnAddress^) then
    Exit;
  if fAsyncQueries then begin
    if (FPQResultFormat = ParamFormatBin) or (BindList.Capacity > 0) then begin
      if FplainDriver.PQsendQueryParams(FconnAddress^,
         Pointer(FASQL), BindList.Count-FOutParamCount, Pointer(FPQParamOIDs), Pointer(FPQparamValues),
         Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat) <> Ord(PGRES_COMMAND_OK) then
        FPostgreSQLConnection.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecute, SQL, Self, nil);
    end else begin
      if FplainDriver.PQsendQuery(FconnAddress^, Pointer(FASQL)) <> Ord(PGRES_COMMAND_OK) then
        FPostgreSQLConnection.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecute, SQL, Self, nil);
    end;
    if FServerCursor then
      FPlainDriver.PQsetSingleRowMode(FconnAddress^);
    if FplainDriver.PQconsumeInput(FconnAddress^) <> Ord(PGRES_COMMAND_OK) then
      FPostgreSQLConnection.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecute, SQL, Self, nil);
    Result := FPlainDriver.PQgetResult(FconnAddress^); //obtain the first result
  end else begin
    if (BindList.Capacity > 0) then begin
      if not Findeterminate_datatype then begin
ExecWithParams:
        Result := FPlainDriver.PQexecParams(FconnAddress^, Pointer(FASQL),
          BindList.Count-FOutParamCount, Pointer(FPQParamOIDs), Pointer(FPQparamValues),
          Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
        Status := FPlainDriver.PQresultStatus(Result);
        if (Status = PGRES_FATAL_ERROR) then begin
          if Assigned(FPlainDriver.PQresultErrorField)
          then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
          else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
          if (PError <> nil) and (PError^ <> #0) and
            FPostgreSQLConnection.GetAutoCommit {we can re-exec only if the txn isn't broken} then begin
            { check for indermine datatype error}
            if Assigned(FPlainDriver.PQresultErrorField) and CompareMem(PError, indeterminate_datatype, 5) then begin
              FPlainDriver.PQclear(Result);
              Findeterminate_datatype := True;
              Result := ExecEmulated;
            end else if Assigned(FPlainDriver.PQresultErrorField) and (FPQResultFormat = ParamFormatBin) and
              CompareMem(PError, no_binary_output_function_available_for_type_void, 5) then begin
              FPlainDriver.PQclear(Result);
              FPQResultFormat := ParamFormatStr; //fall back to string format
              goto ExecWithParams;
            end;
          end;
        end else begin
          Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
          CheckPrepareSwitchMode;
        end;
      end else
        Result := ExecEmulated;
    end else if FPQResultFormat = ParamFormatStr
      then Result := FPlainDriver.PQExec(FconnAddress^, Pointer(FASQL))
      else goto ExecWithParams;
    Status := FPlainDriver.PQresultStatus(Result);
    if (Status = PGRES_BAD_RESPONSE) or (Status = PGRES_NONFATAL_ERROR) or (Status = PGRES_FATAL_ERROR) then
      FPostgreSQLConnection.HandleErrorOrWarning(Status, lcExecute, SQL, Self, Result);
  end;
  if DriverManager.HasLoggingListener then
     DriverManager.LogMessage(lcExecute,Self);
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractPostgreSQLPreparedStatementV3.ExecutePrepared: Boolean;
var Status: TZPostgreSQLExecStatusType;
begin
  LastUpdatecount := -1;
  Prepare;
  PrepareLastResultSetForReUse;
  if BatchDMLArrayCount > 0
  then FRes := ExecuteDMLBatchWithUnnestVarlenaArrays
  else if Findeterminate_datatype or (FRawPlanName = '')
    then Fres := PGExecute
    else Fres := PGExecutePrepared;

  { Process queries with result sets }
  Status := FPlainDriver.PQresultStatus(Fres);
  if ((Status = PGRES_TUPLES_OK) or (Status = PGRES_SINGLE_TUPLE)) then begin
    Result := True;
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet(fServerCursor);
    if BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := LastResultSet;
  end else begin
    Result := False;
    LastUpdateCount := RawToIntDef(FPlainDriver.PQcmdTuples(Fres), 0);
    FPlainDriver.PQclear(Fres);
    if FDoUpdateTimestampOffet then
      FPostgreSQLConnection.UpdateTimestampOffset;
  end;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(cLoggingType[Findeterminate_datatype or (FRawPlanName = '')],Self);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractPostgreSQLPreparedStatementV3.ExecuteQueryPrepared: IZResultSet;
var Status: TZPostgreSQLExecStatusType;
begin
  LastUpdateCount := -1;
  PrepareOpenResultSetForReUse;
  Prepare;
  if BatchDMLArrayCount > 0
  then FRes := ExecuteDMLBatchWithUnnestVarlenaArrays
  else if Findeterminate_datatype or (FRawPlanName = '')
    then Fres := PGExecute
    else Fres := PGExecutePrepared;
  Status := FPlainDriver.PQresultStatus(Fres);
  if (Fres <> nil) and ((Status = PGRES_TUPLES_OK) or (Status = PGRES_SINGLE_TUPLE)) then begin
    if Assigned(FOpenResultSet)
    then Result := IZResultSet(FOpenResultSet)
    else Result := CreateResultSet(fServerCursor);
    if BindList.HasOutOrInOutOrResultParam then
      FOutParamResultSet := Result;
  end else
    Result := nil;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(cLoggingType[Findeterminate_datatype or (FRawPlanName = '')],Self);
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
function TZAbstractPostgreSQLPreparedStatementV3.ExecuteUpdatePrepared: Integer;
var
  Status: TZPostgreSQLExecStatusType;
begin
  PrepareLastResultSetForReuse;
  Prepare;
  if BatchDMLArrayCount > 0
  then FRes := ExecuteDMLBatchWithUnnestVarlenaArrays
  else if (FRawPlanName = '') or Findeterminate_datatype
    then Fres := PGExecute
    else Fres := PGExecutePrepared;
  if Fres <> nil then begin
    Status := FPlainDriver.PQresultStatus(Fres);
    LastUpdateCount := RawToIntDef(FPlainDriver.PQcmdTuples(Fres), 0);
    if ((Status = PGRES_TUPLES_OK) or (Status = PGRES_SINGLE_TUPLE)) then begin
      if LastResultSet = nil then
        LastResultSet := CreateResultSet(fServerCursor);
      if (BindList.HasOutOrInOutOrResultParam) then
          FOutParamResultSet := CreateResultSet(fServerCursor);
    end else
      FPlainDriver.PQclear(Fres);
  end;
  Result := LastUpdateCount;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(cLoggingType[Findeterminate_datatype or (FRawPlanName = '')],Self);
  if FDoUpdateTimestampOffet then
    FPostgreSQLConnection.UpdateTimestampOffset;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.FlushPendingResults;
var PQRes: TPGresult;
begin
  while True do begin
    PQRes := FPlainDriver.PQgetResult(FconnAddress^);
    if PQRes = nil
    then break
    else FplainDriver.PQclear(PQRes);
  end;
end;

class function TZAbstractPostgreSQLPreparedStatementV3.GetBindListClass: TZBindListClass;
begin
  Result := TZPostgreSQLBindList;
end;

function TZAbstractPostgreSQLPreparedStatementV3.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
  Result := @PGPreparableTokens;
end;

const cFrom: PChar = 'FROM';
  cSET: PChar = 'SET';
  cSESSION: PChar = 'SESSION';
  cLOCAL: PChar = 'LOCAL';
  cTime: PChar = 'TIME';
  cZone: PChar = 'ZONE';
  cTimeZone: PChar = 'TIMEZONE';
function TZAbstractPostgreSQLPreparedStatementV3.GetRawEncodedSQL(
  const SQL: SQLString): RawByteString;
var
  I, C, J: Integer;
  ParamsCnt: Cardinal;
  Tokens: TZTokenList;
  Token, NextToken: PZToken;
  BindValue: PZQMarkPosBindValue;
  PQBindValue: PZPostgreSQLBindValue absolute BindValue;
  L: Cardinal;
  P: PAnsiChar;
  FirstComposeToken: PZToken;
  SQLWriter: TZRawSQLStringWriter;
  ComparePrefixTokens: PPreparablePrefixTokens;
  Tokenizer: IZTokenizer;
  function IsDuplicate(const Value: RawByteString; Index: NativeInt): Boolean;
  var I: NativeInt;
      PQBindValue: PZPostgreSQLBindValue;
  begin
    Result := False;
    for i := 0 to Index -1 do begin
      PQBindValue := PZPostgreSQLBindValue(BindList[I]);
      if (Value = PQBindValue.ParamName) then begin
        Result := True;
        Break;
      end;
    end;
  end;
label jmpScanCommentOrWhiteSpace;
begin
  Result := '';
  Tokenizer := Connection.GetTokenizer;
  Tokens := Tokenizer.TokenizeBufferToList(SQL, [toSkipEOF]);
  C := Length(SQL);
  SQLWriter := TZRawSQLStringWriter.Create(C);
  try
    ComparePrefixTokens := @PGPreparableTokens;
    ParamsCnt := 0;
    FirstComposeToken := Tokens[0];
    Token := FirstComposeToken;
    for I := 0 to Tokens.Count -1 do begin
      Token := Tokens[I];
      {check if we've a preparable statement. If ComparePrefixTokens = nil then
        comparing is not required or already done }
      if (ComparePrefixTokens <> nil) and (Token.TokenType = ttWord) then begin
        for C := 0 to high(PGPreparableTokens) do
          if Tokens.IsEqual(i, PGPreparableTokens[C].MatchingGroup, tcInsensitive) then begin
            if C = 0 then begin //EH: scan for a valid from clause
              Findeterminate_datatype := True; //set this to avoid prepares for selects like "select ? as a, ? as b"
              L := 0;
              for J := i+1 to Tokens.Count -1 do begin
                NextToken := Tokens[J];
                if (NextToken.L = 1) then begin
                  if (NextToken.P^ = '(') then
                    Inc(L)
                  else if (NextToken.P^ = ')') then
                    Dec(L);
                end else if (L = 0) and (NextToken.TokenType = ttWord)
                  and (NextToken.L = 4) and SameText(NextToken.P, cFrom, 4) then begin
                  Findeterminate_datatype := False;
                  FTokenMatchIndex := 0;
                  Break;
                end;
              end;
              Token := Tokens[I];
            end else
              FTokenMatchIndex := C;
            Break;
          end;
        ComparePrefixTokens := nil; //stop compare sequence
        if (FTokenMatchIndex = -1) and Assigned(FPlainDriver.PQexecParams) then
          //scan for "SET TIME ZONE ...." see https://zeoslib.sourceforge.io/viewtopic.php?f=50&t=135039
          if (Token.L = 3) and SameText(Token.P, cSET, 3) and (Tokens.Count > I+1) then begin
            J := I+1;
            NextToken := Tokens[j];
jmpScanCommentOrWhiteSpace:
            while (NextToken.TokenType in [ttWhitespace, ttComment]) and (Tokens.Count > J+1) do begin
              Inc(J);
              NextToken := Tokens[J];
            end;
            if (NextToken.TokenType = ttWord) and (Tokens.Count > J+1) then //skip SESSION/LOCAL keywords
              if ((NextToken.L = 5) and SameText(NextToken.P, cLOCAL, 5)) or ((NextToken.L = 7) and SameText(NextToken.P, cSESSION, 7)) then begin
                Inc(J);
                NextToken := Tokens[J];
                goto jmpScanCommentOrWhiteSpace;
              end;
            if (NextToken.TokenType = ttWord) then
              if (NextToken.L = 8) and SameText(NextToken.P, cTimeZone, 8) then
                FDoUpdateTimestampOffet := True
              else if (NextToken.L = 4) and SameText(NextToken.P, cTime, 4) and (Tokens.Count > J+1) then begin
                Inc(J);
                NextToken := Tokens[J];
                while (NextToken.TokenType in [ttWhitespace, ttComment]) and (Tokens.Count > J+1) do begin
                  Inc(J);
                  NextToken := Tokens[J];
                end;
                FDoUpdateTimestampOffet := (NextToken.TokenType = ttWord) and (NextToken.L = 4) and SameText(NextToken.P, cZone, 4);
              end;
          end;
      end;
      if (Token.L = 1) and ((Token.P^ = '?') or ((Token.P^ = '$') and (Tokens.Count > i+1) and (Tokens[I+1].TokenType = ttInteger))) then begin
        if BindList.Capacity <= NativeInt(ParamsCnt) then
          TZPostgreSQLBindList(BindList).Grow;
        BindValue := PZQMarkPosBindValue(BindList[ParamsCnt]);
        Inc(ParamsCnt);
        if (FirstComposeToken <> nil) then begin
          L := (Token.P-FirstComposeToken.P);
          {$IFDEF UNICODE}
          if (L = 1) and (Ord(FirstComposeToken.P^) <= 127) //micro optimization if previous token is just a ',' f.e.
          then SQLWriter.AddChar(AnsiChar(FirstComposeToken.P^), Result)
          else begin
            PUnicodeToRaw(FirstComposeToken.P, L, FClientCP, FRawTemp);
            SQLWriter.AddText(FRawTemp, Result);
          end;
          {$ELSE}
          SQLWriter.AddText(FirstComposeToken.P, L, Result);
          {$ENDIF}
        end;
        BindValue.QMarkPosition := SQLWriter.GetCurrentLength(Result);
        if FUseEmulatedStmtsOnly
        then SQLWriter.AddChar(AnsiChar('?'), Result)
        else if (Token.P^ = '?') then begin
          PByte(FByteBuffer)^ := Byte('$');
          if (PQBindValue.ParamName <> '') then begin
            P := Pointer(PQBindValue.ParamName);
            L := Length(PQBindValue.ParamName);
            Move(P^, (PAnsiChar(fByteBuffer)+1)^, L);
          end else begin
            L := GetOrdinalDigits(ParamsCnt);
            IntToRaw(ParamsCnt, PAnsiChar(fByteBuffer)+1, Byte(L));
          end;
          P := PAnsiChar(fByteBuffer);
          Inc(L);
          ZSetString(P, L, PQBindValue.ParamName{$IFDEF WITH_RAWBYTESTRING}, FClientCP{$ENDIF});
          P := Pointer(PQBindValue.ParamName);
          SQLWriter.AddText(P, L, Result);
        end else begin
          L := Tokens[i+1].L+1;
          {$IFDEF UNICODE}
          UnicodeStringToAscii7(Token.P, L, PQBindValue.ParamName);
          SQLWriter.AddText(PQBindValue.ParamName, Result);
          {$ELSE}
          ZSetString(Token.P, L, PQBindValue.ParamName{$IFDEF WITH_RAWBYTESTRING}, FClientCP{$ENDIF});
          SQLWriter.AddText(Token.P, L, Result);
          {$ENDIF}
          if IsDuplicate(PQBindValue.ParamName, ParamsCnt-1) then begin
            Dec(ParamsCnt);
            PQBindValue.ParamName := '';
          end;
        end;
        L := I + 1 + Byte(Token.P^ <> '?');
        if NativeInt(L) <= Tokens.Count -1
        then FirstComposeToken := Tokens[L]
        else FirstComposeToken := nil;
      end;
    end;
    if (FirstComposeToken <> nil) then begin
      {$IFDEF UNICODE}
      PUnicodeToRaw(FirstComposeToken.P, (Token.P-FirstComposeToken.P)+Token.L, FClientCP, FRawTemp);
      SQLWriter.AddText(FRawTemp, Result);
      {$ELSE}
      SQLWriter.AddText(FirstComposeToken.P, (Token.P-FirstComposeToken.P)+Token.L, Result);
      {$ENDIF}
    end;
    SetBindCapacity(ParamsCnt);
    SQLWriter.Finalize(Result);
  finally
    FreeAndNil(SQLWriter);
    FreeAndNil(Tokens);
    {$IFDEF UNICODE}
    FRawTemp := '';
    {$ENDIF UNICODE}
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.InternalRealPrepare;
begin
  FRawPlanName := IntToRaw(FStatementId)+'_'+IntToRaw({%H-}NativeUInt(FconnAddress^))+'_'+IntToRaw(fPrepareCnt);
  PGExecutePrepare;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.SetBindCapacity(Capacity: Integer);
var i: Integer;
begin
  inherited SetBindCapacity(Capacity);
  if (Length(FPQparamValues) <> BindList.Capacity) then begin
    SetLength(FPQparamValues, BindList.Capacity);
    SetLength(FPQparamLengths, BindList.Capacity);
    SetLength(FPQparamFormats, BindList.Capacity);
    SetLength(FPQParamOIDs, BindList.Capacity);
    SetLength(FParamNames, BindList.Capacity);
    { rebind since the realloc did change the addresses of the hooked values}
    for i := 0 to BindList.Count -1 do
      if BindList.BindTypes[i] {$IFDEF CPU64}in [zbt4Byte, zbt8Byte]{$ELSE}= zbt4Byte{$ENDIF} then
        FPQparamValues[I] := @BindList[I].Value;
  end;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.LinkBinParam2PG(
  Index: Integer; Buf: Pointer; Len: LengthInt);
begin
  FPQparamLengths[Index] := Len;
  FPQparamValues[Index] := Buf;
  FPQparamFormats[Index] := ParamFormatBin;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.LinkTxtParam2PG(
  Index: Integer; Buf: Pointer; Len: LengthInt);
begin
  FPQparamLengths[Index] := Len;
  FPQparamValues[Index] := Buf;
  FPQparamFormats[Index] := ParamFormatStr;
end;

function TZAbstractPostgreSQLPreparedStatementV3.OIDToSQLType(var Index: Integer;
  SQLType: TZSQLType): TZSQLType;
begin
  CheckParameterIndex(Index);
  if (FPQParamOIDs[Index] = INVALIDOID) or (FRawPlanname = '') or Findeterminate_datatype then begin
    if FBindDoubleAsString and (SQLType = stDouble) then begin
      FPQParamOIDs[Index] := INVALIDOID;
      Result := stUnknown;
      Exit;
    end else FPQParamOIDs[Index] := ZSQLType2OID[FOidAsBlob][SQLType];
    if (Ord(SQLType) > Ord(stBoolean)) and (Ord(SQLType) < Ord(stLongWord)) and not Odd(Ord(SQLType))
    then Result := TZSQLType(Ord(SQLType)+3)
    else Result := SQLType
  end else case FPQParamOIDs[Index] of
    { these types are binary supported by now }
    BOOLOID:    Result := stBoolean;
    BYTEAOID:   Result := stBytes;
    INT8OID:    Result := stLong;
    INT2OID:    Result := stSmall;
    INT4OID:    Result := stInteger;
    OIDOID:     Result := stLongWord;
    FLOAT4OID:  Result := stFloat;
    FLOAT8OID:  Result := stDouble;
    CASHOID:    Result := stCurrency;
    NUMERICOID: if SQLType in [stCurrency, stBigDecimal]
                then Result := SQLType
                else Result := stCurrency;
    DATEOID:  Result := stDate;
    TIMEOID:  Result := stTime;
    TIMESTAMPOID: Result := stTimeStamp;
    UUIDOID:  Result := stGUID;
    else if SQLType in [stBytes, stString, stUnicodeString, stAsciistream, stUnicodeStream, stBinaryStream] then
      Result := SQLType
    else
      Result := stUnknown; //indicate unsupport types as fallback to String format
  end;
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractPostgreSQLPreparedStatementV3.PGExecutePrepare;
var PError: PAnsiChar;
  Res: TPGresult;
  Status: TZPostgreSQLExecStatusType;
  AC: Boolean;
begin
  AC := Connection.GetAutoCommit;
  RestartTimer;
  if not AC then
    Connection.StartTransaction;
  try
    Res := FPlainDriver.PQprepare(FconnAddress^, Pointer(FRawPlanName),
      Pointer(FASQL), BindList.Count-FOutParamCount, nil{Pointer(FPQParamOIDs)});
    Status := FPlainDriver.PQresultStatus(Res);
    if (Ord(Status) > ord(PGRES_TUPLES_OK)) then begin
      if Assigned(FPlainDriver.PQresultErrorField)
      then PError := FPlainDriver.PQresultErrorField(Res,Ord(PG_DIAG_SQLSTATE))
      else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
      if (PError <> nil) and (PError^ <> #0) then
        { check for indermine datatype error}
        if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, indeterminate_datatype, 5) <> 0) then
          FPostgreSQLConnection.HandleErrorOrWarning(Status, lcPrepStmt, SQL, Self, Res)
        else begin
          FPlainDriver.PQclear(Res);
          Findeterminate_datatype := True
        end
    end else begin
      FPlainDriver.PQclear(Res);
      PrepareInParameters;
    end;
  finally
    if not AC then
      Connection.Rollback;
  end;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcPrepStmt,Self);
end;

function TZAbstractPostgreSQLPreparedStatementV3.PGExecutePrepared: TPGresult;
var PError: PAnsiChar;
  Status: TZPostgreSQLExecStatusType;
label ReExecuteStr;
begin
  { logs the values }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
  RestartTimer;
  if fAsyncQueries then begin
    Result := nil; //satisfy compiler
    if FPlainDriver.PQsendQueryPrepared(FconnAddress^,
       Pointer(FRawPlanName), BindList.Count-FOutParamCount, Pointer(FPQparamValues),
       Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat) <> Ord(PGRES_COMMAND_OK) then
      FPostgreSQLConnection.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecPrepStmt, SQL, Self, nil)
    else begin
      if FServerCursor then
        FPlainDriver.PQsetSingleRowMode(FconnAddress^);
      if FplainDriver.PQconsumeInput(FconnAddress^) <> Ord(PGRES_COMMAND_OK) then
        FPostgreSQLConnection.HandleErrorOrWarning(PGRES_FATAL_ERROR, lcExecPrepStmt, SQL, Self, nil);
      Result := FPlainDriver.PQgetResult(FconnAddress^); //obtain the first result
    end
  end else begin
ReExecuteStr:
    Result := FPlainDriver.PQexecPrepared(FconnAddress^,
      Pointer(FRawPlanName), BindList.Count-FOutParamCount, Pointer(FPQparamValues),
      Pointer(FPQparamLengths), Pointer(FPQparamFormats), FPQResultFormat);
    Status := FPlainDriver.PQresultStatus(Result);
    if (Ord(Status) > ord(PGRES_TUPLES_OK)) then begin
      if Assigned(FPlainDriver.PQresultErrorField)
      then PError := FPlainDriver.PQresultErrorField(Result,Ord(PG_DIAG_SQLSTATE))
      else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
      if (PError <> nil) and (PError^ <> #0) then
        if Assigned(FPlainDriver.PQresultErrorField) and
         (FPQResultFormat = ParamFormatBin) and CompareMem(PError, no_binary_output_function_available_for_type_void, 5) then begin
        FPlainDriver.PQclear(Result);
        FPQResultFormat := ParamFormatStr; //fall back to string format
        goto ReExecuteStr;
      end else
        FPostgreSQLConnection.HandleErrorOrWarning(Status, lcExecPrepStmt, SQL, Self, Result)
    end;
  end;
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.PGExecuteUnPrepare;
var
  PError: PAnsiChar;
  Res: TPGresult;
  Status: TZPostgreSQLExecStatusType;
  procedure DoOnFail;
  begin
    FPostgreSQLConnection.RegisterTrashPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName))
  end;
begin
  fRawTemp := 'DEALLOCATE "'+FRawPlanName+'"';
  Res := FPlainDriver.PQExec(FconnAddress^, Pointer(fRawTemp));
  Status := FPlainDriver.PQresultStatus(Res);
  if Status <> PGRES_COMMAND_OK then begin
    if Assigned(FPlainDriver.PQresultErrorField)
    then PError := FPlainDriver.PQresultErrorField(Res,Ord(PG_DIAG_SQLSTATE))
    else PError := FPLainDriver.PQerrorMessage(FconnAddress^);
    if (PError <> nil) and (PError^ <> #0) then
      { check for current transaction is aborted error}
      if Assigned(FPlainDriver.PQresultErrorField) and (ZSysUtils.ZMemLComp(PError, current_transaction_is_aborted, 5) <> 0) then begin
        {$IFDEF UNICODE}
        FUniTemp := ASCII7ToUnicodeString(fRawTemp);
        FPostgreSQLConnection.HandleErrorOrWarning(status, lcUnprepStmt, FUniTemp, Self, Res);
        {$ELSE}
        FPostgreSQLConnection.HandleErrorOrWarning(status, lcUnprepStmt, fRawTemp, Self, Res)
        {$ENDIF}
      end else
        DoOnFail
  end else FPlainDriver.PQclear(Res);
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcUnprepStmt,Self);
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractPostgreSQLPreparedStatementV3.Prepare;
begin
  if fAsyncQueries then
    FlushPendingResults;
  if not Prepared then begin
    Inc(fPrepareCnt);
    inherited Prepare; //we need this step always for Set(A/W)SQL overloads if SQL changes
  end;
  if CheckPrepareSwitchMode then
    InternalRealPrepare;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.ReleaseConnection;
begin
  inherited ReleaseConnection;
  FPostgreSQLConnection := nil;
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var ArrayDMLType: TArrayDMLType;
begin
  inherited ReleaseImmediat(Sender, AError);
  Fres := nil;
  FRawPlanName := '';
  fPrepareCnt := 0;
  FExecCount := 0;
  SetParamCount(0);
  for ArrayDMLType := low(TArrayDMLType) to high(ArrayDMLType) do
    if Assigned(FPGArrayDMLStmts[ArrayDMLType].Intf) then
      (FPGArrayDMLStmts[ArrayDMLType].Intf as IImmediatelyReleasable).ReleaseImmediat(Sender, AError);
end;

procedure TZAbstractPostgreSQLPreparedStatementV3.Unprepare;
var ArrayDMLType: TArrayDMLType;
begin
  if fAsyncQueries then
    FlushPendingResults;
  inherited Unprepare;
  FExecCount := 0;
  if Prepared and Assigned(FconnAddress^) and (FRawPlanName <> '') and not Findeterminate_datatype then
    PGExecuteUnPrepare;
  Findeterminate_datatype := False;
  FRawPlanName := '';
  FOutParamCount := 0;
  for ArrayDMLType := low(TArrayDMLType) to high(TArrayDMLType) do begin
    FPGArrayDMLStmts[ArrayDMLType].Obj := nil;
    FPGArrayDMLStmts[ArrayDMLType].Intf := nil;
  end;
  FDoUpdateTimestampOffet := False;
end;

{ TZPostgreSQLStatement }

constructor TZPostgreSQLStatement.Create(
  const Connection: IZPostgreSQLConnection; Info: TStrings);
begin
  inherited Create(Connection, '', Info);
  FMinExecCount2Prepare := -1;
  fAsyncQueries := False;
  Findeterminate_datatype := True;
  FUseEmulatedStmtsOnly := True;
end;

{ TZPostgrePreparedStatementV2 }

constructor TZPostgrePreparedStatementV2.Create(
  const Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
begin
  FUseEmulatedStmtsOnly := True;
  inherited Create(Connection, SQL, Info);
  FMinExecCount2Prepare := -1;
  fAsyncQueries := False;
  fServerCursor := False;
  Findeterminate_datatype := True;
  FUseEmulatedStmtsOnly := True;
end;

procedure TZPostgrePreparedStatementV2.Unprepare;
begin
  inherited;
  Findeterminate_datatype := True;
end;

{ TZPostgreSQLCallableStatement }

function TZPostgreSQLCallableStatement.CreateExecutionStatement(
  const StoredProcName: String): TZAbstractPreparedStatement;
var
  I: Integer;
  J: Cardinal;
  SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND};
  SQLWriter: TZSQLStringWriter;
begin
  if (Connection as IZPostgreSQLConnection).StoredProcedureIsSelectable(StoredProcName)
  then SQL := 'SELECT * FROM '
  else SQL := 'CALL ';
  I := Length(StoredProcName);
  i := I + 14+BindList.Count shl 2;
  SQLWriter := TZSQLStringWriter.Create(I);
  SQLWriter.AddText(StoredProcName, SQL);
  SQLWriter.AddChar('(', SQL);
  J := 1;
  for I := 0 to BindList.Capacity -1 do
    if Ord(BindList.ParamTypes[I]) < Ord(pctOut) then begin
      SQLWriter.AddChar('$', SQL);
      SQLWriter.AddOrd(J, SQL);
      SQLWriter.AddChar(',', SQL);
      Inc(J);
    end;
  if J > 1 then
    SQLWriter.CancelLastComma(SQL);
  SQLWriter.AddChar(')', SQL);
  SQLWriter.Finalize(SQL);
  FreeAndNil(SQLWriter);
  Result := TZPostgreSQLPreparedStatementV3.Create(Connection as IZPostgreSQLConnection, SQL, Info);
end;

{ TZPostgreSQLPreparedStatementV3 }

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
var InParamIdx: Integer;
begin
  InParamIdx := Index;
  CheckParameterIndex(InParamIdx);
  if SQLType = stBytes
  then BindList.Put(Index, SQLType, TBytes(Buf))
  else BindList.Put(Index, SQLtype, Buf, Len);
  LinkBinParam2PG(InParamIdx, Buf, Len);
end;

procedure TZPostgreSQLPreparedStatementV3.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
var WriteTempBlob: IZPostgreSQLOidBlob;
  Lob_OID: OID;
  InParamIdx: Integer;
  RefCntLob: IZBlob;
  P: Pointer;
  L: NativeUInt;
label BufL;
begin
  InParamIdx := Index;
  CheckParameterIndex(InParamIdx);
  RefCntLob := Value; //inc RefCount
  if (Value <> nil) and (SQLType in [stAsciiStream, stUnicodeStream]) then begin
    if Value.IsClob
    then Value.SetCodePageTo(FClientCP)
    else raise CreateConversionError(Index, stBinaryStream, stAsciiStream);
    SQLType := stAsciiStream;
  end;
  BindList.Put(Index, SQLType, RefCntLob);
  if (RefCntLob <> nil) and not RefCntLob.IsEmpty then
    if ((SQLType = stBinaryStream) and FOidAsBlob) then begin
      if not Supports(Value, IZPostgreSQLOidBlob, WriteTempBlob) then
        WriteTempBlob := TZPostgreSQLOidBlob.CreateFromBlob(Value, FPostgreSQLConnection, FOpenLobStreams);
      Lob_OID := WriteTempBlob.GetBlobOid;
      FPQParamOIDs[InParamIdx] := OIDOID;
      BindList.Put(Index, stBinaryStream, P4Bytes(@Lob_OID));
      LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], SizeOf(OID));
      {$IFNDEF ENDIAN_BIG}Cardinal2PG(Lob_OID, FPQparamValues[Index]);{$ENDIF}
      WriteTempBlob := nil;
    end else if SQLType = stBinaryStream then begin
      FPQparamFormats[InParamIdx] := ParamFormatBin;
BufL: P := RefCntLob.GetBuffer(FRawTemp, L);
      FPQparamValues[InParamIdx] := P;
      FPQparamLengths[InParamIdx] := L;
    end else begin
      FPQparamFormats[InParamIdx] := ParamFormatStr;
      goto BufL;
    end
end;

procedure TZPostgreSQLPreparedStatementV3.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
var InParamIDX: Integer;
begin
  InParamIDX := Index;
  CheckParameterIndex(InParamIDX);
  if Buf = nil then begin
    Buf := PEmptyAnsiString;
    Len := 0;
  end;
  BindList.Put(Index, stString, Buf, Len, FClientCP);
  LinkTxtParam2PG(InParamIDX, Buf, Len);
end;

procedure TZPostgreSQLPreparedStatementV3.BindRawStr(Index: Integer;
  const Value: RawByteString);
var InParamIDX: Integer;
begin
  InParamIDX := Index;
  CheckParameterIndex(InParamIDX);
  BindList.Put(Index, stString, Value, FClientCP);
  LinkTxtParam2PG(InParamIDX, Pointer(Value), Length(Value));
  if Pointer(Value) = nil then
    FPQparamValues[InParamIDX] := PEmptyAnsiString;
end;

procedure TZPostgreSQLPreparedStatementV3.ClearParameters;
begin
  BatchDMLArrayCount := 0;
end;

{**
  Binds a double value
}
procedure TZPostgreSQLPreparedStatementV3.InternalBindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var PGSQLType: TZSQLType;
  InParamIdx: Integer;
  { move the string conversions into a own proc -> no (U/L)StrClear}
  procedure SetAsRaw;
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
    BindRawStr(InParamIdx, fRawTemp)
  end;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(InParamIdx, SQLType);
  if PGSQLType in [stCurrency, stBigDecimal] then
    if PGSQLType = stBigDecimal then begin
      Double2BCD(Value, PBCD(fByteBuffer)^);
      SetBigDecimal(Index, PBCD(fByteBuffer)^);
    end else
      SetCurrency(Index, Value)
  else if (Ord(PGSQLType) < Ord(stGUID)) and Boolean(PGSQLType) then begin
    {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
    if PGSQLType in [stBoolean, stFloat, stSmall, stInteger, stDate] then begin
      BindList.Put(Index, PGSQLType, P4Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], 4);
    end else begin
      BindList.Put(Index, PGSQLType, P8Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._8Bytes[Index], 8);
    end;
    case PGSQLType of
      stBoolean:    PByte(FPQparamValues[InParamIdx])^ := Byte(Ord(Value <> 0));
      stSmall:      SmallInt2PG(Trunc(Value), FPQparamValues[InParamIdx]);
      stInteger:    Integer2PG(Trunc(Value), FPQparamValues[InParamIdx]);
      stLong:       Int642PG(Trunc(Value), FPQparamValues[InParamIdx]);
      stFloat:      Single2PG(Value, FPQparamValues[InParamIdx]);
      stDouble:     {$IFNDEF ENDIAN_BIG}Reverse8Bytes(FPQparamValues[InParamIdx]){$ENDIF};
      stDate:       Date2PG(Value, PInteger(FPQparamValues[InParamIdx])^);
      stTime:       if Finteger_datetimes
                    then Time2PG(Value, PInt64(FPQparamValues[InParamIdx])^)
                    else Time2PG(Value, PDouble(FPQparamValues[InParamIdx])^);
      stTimeStamp:  if Finteger_datetimes
                    then DateTime2PG(Value, PInt64(FPQparamValues[InParamIdx])^)
                    else DateTime2PG(Value, PDouble(FPQparamValues[InParamIdx])^);
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //impossible
    end;
  end else SetAsRaw;
end;

procedure TZPostgreSQLPreparedStatementV3.InternalBindInt(Index: Integer;
  SQLType: TZSQLType; Value: {$IFNDEF CPU64}Integer{$ELSE}Int64{$ENDIF});
var PGSQLType: TZSQLType;
    InParamIdx: Integer;
{ move the string conversions into a own proc -> no (U/L)StrClear}
procedure SetAsRaw; begin BindRawStr(InParamIdx, IntToRaw(Value)); end;
var P: Pointer;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(InParamIdx, SQLType);
  if (Ord(PGSQLType) < Ord(stGUID)) and Boolean(PGSQLType) then begin
    {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
    if (FPQparamValues[InParamIdx] = nil) or (BindList[Index].SQLType <> PGSQLType) then
      if ZSQLType2PGBindSizes[PGSQLType] <= 4 then begin
        BindList.Put(Index, PGSQLType, P4Bytes(@Value));
        LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], ZSQLType2PGBindSizes[PGSQLType]);
      end else if ZSQLType2PGBindSizes[PGSQLType] = 8 then begin
        BindList.Put(Index, PGSQLType, P8Bytes({$IFNDEF CPU64}fByteBuffer{$ELSE}@Value{$ENDIF}));
        LinkBinParam2PG(InParamIdx, BindList._8Bytes[Index], 8);
      end else
        LinkBinParam2PG(InParamIdx, BindList.AcquireCustomValue(Index, PGSQLType,
          ZSQLType2PGBindSizes[PGSQLType]), ZSQLType2PGBindSizes[PGSQLType]);
    P := FPQparamValues[InParamIdx];
    case PGSQLType of
      stBoolean:  PByte(P)^ := Ord(Value);
      stSmall:    SmallInt2PG(Value, P);
      stInteger,
      stDate:     Integer2PG(Value, P);
      stFloat:    Single2PG(Value, P);
      stLongWord: Cardinal2PG(Value, P);
      stLong:     Int642PG(Value, P);
      stDouble:   Double2PG(Value, P);
      stCurrency: Currency2PGNumeric(Value, P, FPQparamLengths[InParamIdx]);
      stBigDecimal: Begin
                      ScaledOrdinal2BCD(Value, 0, PBCD(FByteBuffer)^);
                      BCD2PGNumeric(PBCD(FByteBuffer)^, P, FPQparamLengths[InParamIdx]);
                    end;
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //impossible
    end;
  end else SetAsRaw;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZPostgreSQLPreparedStatementV3.PrepareInParameters;
var
  res: TPGresult;
  I: Integer;
  pgOID, tmpOID: OID;
  NewSQLType: TZSQLType;
  AllOIDsKnown: Boolean;
  boundOIDs: array of OID;
  procedure RebindIfRequired(i: Integer; NewSQLType: TZSQLType; oldoid, newoid: OID);
  var
    TS: TZTimeStamp;
    D: TZDate absolute TS;
    T: TZTime absolute TS;
    OldSQLType: TZSQLType;
  begin
    OldSQLType := BindList.SQLTypes[i];
    if NewSQLType in [stUnicodeString, stUnicodeStream]
    then NewSQLType := TZSQLType(Ord(NewSQLType) -1)
    else if (NewSQLType = stBigDecimal) and (OldSQLType = stCurrency)
      then NewSQLType := stCurrency;
    if (NewSQLType <> OldSQLType) and (FPQparamValues[I] <> nil) and ((oldoid <> newoid) and
       ((Ord(NewSQLType) <= Ord(stTimestamp)) or ((Ord(OldSQLType) <= Ord(stTimestamp))))) then
      case OldSQLType of
        stBoolean:  SetBoolean(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Boolean(PByte(FPQparamValues[i])^));
        stSmall:    SetSmall(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2SmallInt(FPQparamValues[i]));
        stInteger:  SetInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Integer(FPQparamValues[i]));
        stLongWord,
        stLong:     SetLong(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Int64(FPQparamValues[i]));
        stCurrency: if oldoid  = CASHOID
                    then SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGCash2Currency(FPQparamValues[i]))
                    else SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGNumeric2Currency(FPQparamValues[i]));
        stFloat:    SetFloat(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Single(FPQparamValues[i]));
        stDouble:   SetDouble(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PG2Double(FPQparamValues[i]));
        stBigDecimal:begin
                      PGNumeric2BCD(FPQparamValues[i], PBCD(FByteBuffer)^);
                      SetBigDecimal(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PBCD(FByteBuffer)^);
                    end;
        stTime:     begin
                      if Finteger_datetimes
                      then PG2Time(PInt64(FPQparamValues[i])^, T.Hour, T.Minute, T.Second, T.Fractions)
                      else PG2Time(PDouble(FPQparamValues[i])^, T.Hour, T.Minute, T.Second, T.Fractions);
                      T.IsNegative := False;
                      SetTime(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, T);
                    end;
        stDate:     begin
                      PG2Date(PInteger(FPQparamValues[i])^, D.Year, D.Month, D.Day);
                      D.IsNegative := False;
                      SetDate(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},D);
                    end;
        stTimeStamp:begin
                      if Finteger_datetimes
                      then PG2DateTime(PInt64(FPQparamValues[i])^, 0, TS.Year, TS.Month, TS.Day,
                          TS.Hour, Ts.Minute, Ts.Second, Ts.Fractions)
                      else PG2DateTime(PDouble(FPQparamValues[i])^, 0, TS.Year, TS.Month, TS.Day,
                        TS.Hour, Ts.Minute, Ts.Second, Ts.Fractions);
                      TS.IsNegative := False;
                      SetTimeStamp(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},TS);
                    end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //impossible ?
      end;
  end;
begin
  if (fRawPlanName <> '') and not (Findeterminate_datatype) and (BindList.Capacity > 0) then begin
    AllOIDsKnown := True;
    if Assigned(FPlainDriver.PQdescribePrepared) then begin
      res := FPlainDriver.PQdescribePrepared(FconnAddress^, Pointer(FRawPlanname));
      boundOIDs := nil;
      try
        BindList.Count := FplainDriver.PQnparams(res)+FOutParamCount;
        for i := 0 to BindList.Count-FOutParamCount-1 do begin
          pgOID := FplainDriver.PQparamtype(res, i);
          if (pgOID = TIMESTAMPTZOID) and (FPostgreSQLConnection.GetTimeZoneOffset <> 0) then
            pgOID := TIMESTAMPOID;
          NewSQLType := PostgreSQLToSQLType(fOIDAsBlob, pgOID, -1);
          if NewSQLType = stUnknown then //EH: domain types are unknonw for us..
            //pg does not return the underlaying OID grumble...
            if FPostgreSQLConnection.FindDomainBaseType(pgOID, tmpOID) then begin
              pgOID := tmpOID;
              NewSQLType := PostgreSQLToSQLType(fOIDAsBlob, tmpOID, -1);
            end else begin
              SetLength(boundOIDs, BindList.Count);
              boundOIDs[i] := FPQParamOIDs[i];
              AllOIDsKnown := False;
              FPQParamOIDs[i] := pgOID;
              continue;
            end;
          tmpOID := FPQParamOIDs[i];
          FPQParamOIDs[i] := pgOID;
          if tmpOID <> pgOID then
            RebindIfRequired(i, NewSQLType, tmpOID, pgOID);
          if (tmpOID <> pgOID) and Assigned(FOpenResultSet) then
            IZResultSet(FOpenResultSet).Close;
        end;
      finally
        FPlainDriver.PQclear(res);
      end;
      if not AllOIDsKnown then begin
        FPostgreSQLConnection.FillUnknownDomainOIDs;
        for i := 0 to BindList.Count-1 do begin
          pgOID := FPQParamOIDs[i];
          NewSQLType := PostgreSQLToSQLType(fOIDAsBlob, pgOID, -1);
          if NewSQLType = stUnknown then begin//EH: domain types are unknonw for us..
            //pg does not return the underlaying OID grumble...
            Assert(FPostgreSQLConnection.FindDomainBaseType(pgOID, tmpOID));
            NewSQLType := PostgreSQLToSQLType(fOIDAsBlob, tmpOID, -1);
            FPQParamOIDs[i] := tmpOID;
            RebindIfRequired(i, NewSQLType, boundOIDs[i], tmpOID);
          end;
        end;
      end;
    end else
      for i := 0 to BindList.Count-1 do
        SQLTypeToPostgreSQL(BindList.SQLTypes[i], fOIDAsBlob, FPQParamOIDs[i]);
  end;
end;

procedure TZPostgreSQLPreparedStatementV3.RegisterParameter(
  ParameterIndex: Integer; SQLType: TZSQLType; ParamType: TZProcedureColumnType;
  const Name: String; PrecisionOrSize, Scale: LengthInt);
var I: Integer;
begin
  if SQLType in [stUnicodeString, stUnicodeStream] then
    SQLType := TZSQLType(Ord(SQLType)-1);
  I := ParameterIndex;
  CheckParameterIndex(I);
  BindList.SetParamTypes(ParameterIndex , SQLType, ParamType);
  if Name <> '' then
    {$IFDEF UNICODE}
    FParamNames[ParameterIndex] := ZUnicodeToRaw(Name, FClientCP);
    {$ELSE}
    FParamNames[ParameterIndex] := Name;
    {$ENDIF}

  if ParamType in [pctOut, pctReturn] then begin
    FOutParamCount := 0;
    for i := 0 to BindList.Count -1 do
      Inc(FOutParamCount, Ord(BindList.ParamTypes[i] in [pctOut, pctReturn]));
  end;
end;

procedure TZPostgreSQLPreparedStatementV3.SetBigDecimal(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TBCD);
var Idx: Integer;
    SQLType: TZSQLType;
procedure SetAsRaw; begin BindRawStr(Idx, BcdToSQLRaw(Value)); end;
begin
  Idx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  SQLType := OIDToSQLType(Idx, stBigDecimal);
  if (FPQParamOIDs[Idx] = NUMERICOID) then begin
    if (FPQparamValues[Idx] = nil) or (BindList.SQLTypes[Idx] <> SQLType) then begin
      FPQparamValues[Idx] := BindList.AcquireCustomValue(Idx, SQLType, MaxBCD2NumSize);
      FPQparamFormats[Idx] := ParamFormatBin;
    end;
    BCD2PGNumeric(Value, FPQparamValues[Idx], FPQparamLengths[Idx]);
  end else case SQLType of
    stBoolean,
    stSmall,
    stInteger,
    {$IFDEF CPU64}stLong,{$ENDIF}
    stLongWord: InternalBindInt(Index, SQLType, ZSysUtils.BCD2Int64(Value));
    {$IFNDEF CPU64}
    stLong:     SetLong(Index, BCD2Int64(Value));
    {$ENDIF}
    stTime, stDate, stTimeStamp, stFloat,
    stDouble: InternalBindDouble(Index, SQLType, BCDToDouble(Value));
    else SetAsRaw;
  end;
end;

procedure TZPostgreSQLPreparedStatementV3.SetBoolean(Index: Integer;
  Value: Boolean);
begin
  InternalBindInt(Index, stBoolean, Ord(Value));
end;

procedure TZPostgreSQLPreparedStatementV3.SetByte(Index: Integer; Value: Byte);
begin
  InternalBindInt(Index, stSmall, Value);
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
procedure TZPostgreSQLPreparedStatementV3.SetBytes(Index: Integer;
  Value: PByte; Len: NativeUInt);
var InParamIdx: Integer;
    SQLType: TZSQLType;
  procedure BindAsLob;
  var Lob: IZBlob;
  begin
    Lob := TZPostgreSQLOidBlob.Create(FPostgreSQLConnection, 0, lsmWrite, FOpenLobStreams);
    Lob.SetBuffer(Value, Len);
    SetBlob(Index, stBinaryStream, Lob);
  end;
begin
  if (Value = nil) or (Len = 0) then begin
    SetNull(Index, stBytes);
    Exit;
  end;
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  SQLType := OIDToSQLType(InParamIdx, stBytes);
  if (FPQParamOIDs[InParamIdx] = BYTEAOID) then begin
    BindList.Put(InParamIdx, stBytes, Value, Len);
    LinkBinParam2PG(InParamIdx, Value, Len);
  end else if (FPQParamOIDs[InParamIdx] = UUIDOID) and (Len = SizeOf(TGUID)) then begin
    BindList.Put(InParamIdx, PGUID(Value)^);
    LinkBinParam2PG(InParamIdx, Value, Len);
  end else if (FPQParamOIDs[InParamIdx] = OIDOID) then
    BindAsLob
  else raise CreateConversionError(Index, stBytes, SQLType);
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetCurrency(Index: Integer;
  const Value: Currency);
var SQLType: TZSQLType;
    Idx: Integer;
procedure SetAsRaw; begin BindRawStr(Idx, CurrToRaw(Value,'.')); end;
begin
  Idx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  SQLType := OIDToSQLType(Idx, stCurrency);
  if (FPQParamOIDs[Idx] = NUMERICOID) then begin
    if (FPQparamValues[Idx] = nil) or (BindList.SQLTypes[Idx] <> SQLType) then begin
      FPQparamValues[Idx] := BindList.AcquireCustomValue(Idx, SQLType, MaxCurr2NumSize);
      FPQparamFormats[Idx] := ParamFormatBin;
    end;
    Currency2PGNumeric(Value, FPQparamValues[Idx], FPQparamLengths[Idx]);
  end else case SQLType of
    stBoolean:  InternalBindInt(Idx, stBoolean, Ord(Value <> 0));
    stSmall,
    stInteger:  InternalBindInt(Idx, SQLType, PInt64(@Value)^ div 10000);
    stLongWord: begin
                  BindList.Put(Idx, stLong, P4Bytes(@Value));
                  LinkBinParam2PG(Idx, BindList._4Bytes[Idx], SizeOf(Cardinal));
                  Cardinal2PG(Cardinal(PInt64(@Value)^ div 10000), FPQparamValues[Idx]);
                end;
    stLong:     begin
                  BindList.Put(Idx, stLong, P8Bytes(@Value));
                  LinkBinParam2PG(Idx, BindList._8Bytes[Idx], SizeOf(Int64));
                  Int642PG(PInt64(@Value)^ div 10000, FPQparamValues[Idx]);
                end;
    stTime, stDate, stTimeStamp, stFloat,
    stDouble:   InternalBindDouble(Idx, SQLType, Value);
    stCurrency: begin
                  BindList.Put(Idx, stCurrency, P8Bytes(@Value));
                  LinkBinParam2PG(Idx, BindList._8Bytes[Idx], SizeOf(Currency));
                  Currency2PGCash(Value, FPQparamValues[Idx]);
                end;
    else SetAsRaw;
  end;
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetDate(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZDate);
var PGSQLType: TZSQLType;
  InParamIdx: Integer;
  TS: TZTimeStamp;
  DT: TDateTime absolute TS;
  Len: LengthInt absolute TS;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(InParamIdx, stDate);
  if (Ord(PGSQLType) >= Ord(stDate)) and (Ord(PGSQLType) <= Ord(stTimestamp)) then begin
    {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
    if PGSQLType = stDate then begin
      BindList.Put(Index, PGSQLType, P4Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], 4);
    end else begin
      BindList.Put(Index, PGSQLType, P8Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._8Bytes[Index], 8);
    end;
    case PGSQLType of
      stDate:       Date2PG(Value.Year, Value.Month, Value.Day, PInteger(FPQparamValues[InParamIdx])^);
      stTime:       PInt64(FPQparamValues[InParamIdx])^ := 0;
      else          begin
                      ZSysUtils.TimeStampFromDate(Value, TS{%H-});
                      if Finteger_datetimes
                      then TimeStamp2PG(TS, PInt64(FPQparamValues[InParamIdx])^)
                      else TimeStamp2PG(TS, PDouble(FPQparamValues[InParamIdx])^);
                    end;
      end;
  end else if (PGSQLType in [stUnknown, stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then begin
    Len := DateToRaw(Value.Year, Value.Month, Value.Day, PAnsiChar(FByteBuffer),
      ConSettings^.WriteFormatSettings.DateFormat, False, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), Len ,fRawTemp);
    BindRawStr(InParamIdx, fRawTemp)
  end else if TryDateToDateTime(Value, DT)
    then InternalBindDouble(Index, stDate, DT)
    else InternalBindInt(Index, stDate, 1);
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetDouble(Index: Integer;
  const Value: Double);
begin
  InternalBindDouble(Index, stDouble, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetFloat(Index: Integer;
  Value: Single);
begin
  InternalBindDouble(Index, stFloat, Value);
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.AddParamLogValue(
  ParamIndex: Integer; SQLWriter: TZSQLStringWriter; var Result: SQLString);
var P: Pointer;
  BindValue: PZBindValue;
  BCD: TBCD;
{$IFDEF UNICODE}
  CP: Word;
label jmpMov;
{$ENDIF}
begin
  CheckParameterIndex(ParamIndex);
  BindValue := BindList[ParamIndex];
  case BindValue.BindType of
    zbtNull: Result := '(NULL)';
    zbt4Byte: begin
        P := BindList._4Bytes[ParamIndex];
        case FPQParamOIDs[ParamIndex] of
          BOOLOID:  if BindValue.Value <> nil
                    then SQLWriter.AddText('(TRUE)', Result)
                    else SQLWriter.AddText('(FALSE)', Result);
          INT2OID:  SQLWriter.AddOrd(PG2SmallInt(P), Result);
          INT4OID:  SQLWriter.AddOrd(PG2Integer(P), Result);
          OIDOID:   SQLWriter.AddOrd(PG2Cardinal(P), Result);
          FLOAT4OID:SQLWriter.AddFloat(PG2Single(P), Result);
          DATEOID:  SQLWriter.AddDate(PG2Date(PInteger(P)^), ConSettings^.WriteFormatSettings.DateFormat, Result);
        end;
      end;
    zbt8Byte: begin
        P := BindList._8Bytes[ParamIndex];
        case FPQParamOIDs[ParamIndex] of
          INT8OID:  SQLWriter.AddOrd(PG2Int64(P), Result);
          FLOAT8OID:SQLWriter.AddFloat(PG2Double(P), Result);
          CASHOID:  SQLWriter.AddFloat(PG2Int64(P)/100, Result);
          TIMEOID:  if Finteger_datetimes
                    then SQLWriter.AddTime(PG2Time(PInt64(P)^), ConSettings^.WriteFormatSettings.TimeFormat, Result)
                    else SQLWriter.AddTime(PG2Time(PDouble(P)^), ConSettings^.WriteFormatSettings.TimeFormat, Result);
          TIMESTAMPOID: if Finteger_datetimes
                    then SQLWriter.AddDateTime(PG2DateTime(PInt64(P)^, 0), ConSettings^.WriteFormatSettings.DateTimeFormat, Result)
                    else SQLWriter.AddDateTime(PG2DateTime(PDouble(P)^, 0), ConSettings^.WriteFormatSettings.DateTimeFormat, Result);
        end;
      end;
    {$IFDEF UNICODE}
    zbtRawString, zbtUTF8String {$IFNDEF NEXTGEN}, zbtAnsiString{$ENDIF}: begin
            if BindValue.BindType = zbtRawString then
              CP := FClientCP
            else if BindValue.BindType = zbtUTF8String then
              CP := zCP_UTF8
            else CP := ZOSCodePage;
            FUniTemp := ZRawToUnicode(RawByteString(BindValue.Value), CP);
            goto jmpMov;
          end;
    {$ELSE}
    zbtRawString, zbtUTF8String {$IFNDEF NEXTGEN}, zbtAnsiString{$ENDIF}: SQLWriter.AddTextQuoted(RawByteString(BindValue.Value), AnsiChar(#39), Result);
    {$ENDIF}
    zbtCharByRef: {$IFDEF UNICODE} begin
                  FUniTemp := PRawToUnicode(PZCharRec(BindValue.Value)^.P, PZCharRec(BindValue.Value)^.Len, PZCharRec(BindValue.Value).CP);
jmpMov:           SQLWriter.AddTextQuoted(FUniTemp, #39, Result);
                  FUniTemp := '';
                  end; {$ELSE}
                  SQLWriter.AddTextQuoted(PAnsiChar(PZCharRec(BindValue.Value)^.P), PZCharRec(BindValue.Value)^.Len, AnsiChar(#39), Result);
                  {$ENDIF}
    zbtBinByRef: SQLWriter.AddHexBinary(PZBufRec(BindValue.Value).Buf, PZBufRec(BindValue.Value).Len, False, Result);
    zbtGUID:     SQLWriter.AddGUID(PGUID(BindValue.Value)^, [guidWithBrackets, guidQuoted], Result);
    zbtBytes:    SQLWriter.AddHexBinary(TBytes(BindValue.Value), False, Result);
    zbtLob: if BindValue.SQLType = stBinaryStream
            then SQLWriter.AddText('(BLOB)', Result)
            else SQLWriter.AddText('(CLOB)', Result);
    zbtPointer: Result := BoolStrInts[BindValue.Value <> nil];
    zbtCustom: if BindValue.SQLType = stArray
                then SQLWriter.AddText('(ARRAY)', Result)
                else if BindValue.SQLType = stCurrency
                then SQLWriter.AddDecimal(PGNumeric2Currency(PAnsiChar(BindValue.Value)+SizeOf(LengthInt)), Result)
                else begin
                  PGNumeric2BCD(PAnsiChar(BindValue.Value)+SizeOf(LengthInt), BCD{%H-});
                  SQLWriter.AddDecimal(BCD, Result);
                end;
    //zbtBCD: Result := '';
    else Result := '';
  end;
end;

procedure TZPostgreSQLPreparedStatementV3.SetInt(Index, Value: Integer);
begin
  InternalBindInt(Index, stInteger, Value);
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetLong(Index: Integer;
  const Value: Int64);
{$IFDEF CPU64}
begin
  InternalBindInt(Index, stLong, Value);
{$ELSE}
{ move the string conversions into a own proc -> no (U/L)StrClear}
var PGSQLType: TZSQLType;
    Idx: Integer;
procedure SetAsRaw; begin SetRawByteString(Idx, IntToRaw(Value)); end;
begin
  Idx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(Idx, stLong);
  if (Ord(PGSQLType) < Ord(stGUID)) and Boolean(PGSQLType) then begin
    if (FPQparamValues[Idx] = nil) or (BindList[Idx].SQLType <> PGSQLType) then
      if ZSQLType2PGBindSizes[PGSQLType] <= 4 then begin
        BindList.Put(Idx, PGSQLType, P4Bytes(@Value));
        LinkBinParam2PG(Idx, BindList._4Bytes[Idx], ZSQLType2PGBindSizes[PGSQLType]);
      end else if ZSQLType2PGBindSizes[PGSQLType] = 8 then begin
        BindList.Put(Idx, PGSQLType, P8Bytes(@Value));
        LinkBinParam2PG(Idx, BindList._8Bytes[Idx], 8);
      end else
        LinkBinParam2PG(Idx, BindList.AcquireCustomValue(Idx, PGSQLType,
          ZSQLType2PGBindSizes[PGSQLType]), ZSQLType2PGBindSizes[PGSQLType]);
    case PGSQLtype of
      stLong:     Int642PG(Value, FPQparamValues[Idx]);
      stBoolean:  PByte(FPQparamValues[Idx])^ := Ord(Value);
      stSmall:    SmallInt2PG(Value, FPQparamValues[Idx]);
      stInteger,
      stDate:     Integer2PG(Value, FPQparamValues[Idx]);
      stLongWord: Cardinal2PG(Value, FPQparamValues[Idx]);
      stFloat:    Single2PG(Value, FPQparamValues[Idx]);
      stDouble:   Double2PG(Value, FPQparamValues[Idx]);
      stCurrency: Currency2PGNumeric(Value, FPQparamValues[Idx], FPQparamLengths[Idx]);
      {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF} //impossible
    end;
  end else SetAsRaw;
{$ENDIF}
end;

procedure TZPostgreSQLPreparedStatementV3.SetNull(Index: Integer;
  SQLType: TZSQLType);
var InParamIdx: Integer;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX} -1{$ENDIF};
  SQLType := OIDToSQLType(InParamIdx, SQLType);
  BindList.SetNull(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SQLType);
  FPQparamFormats[InParamIdx] := ParamFormatStr;
  FPQparamValues[InParamIdx] := nil
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetShort(Index: Integer;
  Value: ShortInt);
begin
  InternalBindInt(Index, stSmall, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetSmall(Index: Integer;
  Value: SmallInt);
begin
  InternalBindInt(Index, stSmall, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetTime(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTime);
var PGSQLType: TZSQLType;
  InParamIdx: Integer;
  TS: TZTimeStamp;
  DT: TDateTime absolute TS;
  Len: LengthInt absolute TS;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(InParamIdx, stTime);
  if (Ord(PGSQLType) >= Ord(stDate)) and (Ord(PGSQLType) <= Ord(stTimestamp)) then begin
    {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
    if PGSQLType = stDate then begin
      BindList.Put(Index, PGSQLType, P4Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], 4);
    end else begin
      BindList.Put(Index, PGSQLType, P8Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._8Bytes[Index], 8);
    end;
    case PGSQLType of
      stDate:       PInteger(FPQparamValues[InParamIdx])^ := 0;
      stTime:       if Finteger_datetimes
                    then Time2PG(Value.Hour, Value.Minute, Value.Second, Value.Fractions, PInt64(FPQparamValues[InParamIdx])^)
                    else Time2PG(Value.Hour, Value.Minute, Value.Second, Value.Fractions, PDouble(FPQparamValues[InParamIdx])^);
      else          begin
                      TimeStampFromTime(Value, TS{%H-});
                      if Finteger_datetimes
                      then TimeStamp2PG(TS, PInt64(FPQparamValues[InParamIdx])^)
                      else TimeStamp2PG(TS, PDouble(FPQparamValues[InParamIdx])^);
                    end;
      end;
  end else if (PGSQLType in [stUnknown, stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then begin
    Len := TimeToRaw(Value.Hour, Value.Minute, Value.Second, Value.Fractions,
      PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.TimeFormat, False, False);
    ZSetString(PAnsiChar(FByteBuffer), Len ,fRawTemp);
    BindRawStr(InParamIdx, fRawTemp);
  end else if TryTimeToDateTime(Value, DT)
    then InternalBindDouble(Index, stTime, DT)
    else InternalBindInt(Index, stTime, 1);
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetTimestamp(Index: Integer;
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TZTimeStamp);
var PGSQLType: TZSQLType;
  InParamIdx: Integer;
  DT: TDateTime;
  Len: LengthInt absolute DT;
begin
  InParamIdx := Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF};
  PGSQLType := OIDToSQLType(InParamIdx, stTimeStamp);
  if (Ord(PGSQLType) >= Ord(stDate)) and (Ord(PGSQLType) <= Ord(stTimestamp)) then begin
    {$IFNDEF GENERIC_INDEX}Dec(Index);{$ENDIF}
    if PGSQLType = stDate then begin
      BindList.Put(Index, PGSQLType, P4Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._4Bytes[Index], 4);
    end else begin
      BindList.Put(Index, PGSQLType, P8Bytes(@Value));
      LinkBinParam2PG(InParamIdx, BindList._8Bytes[Index], 8);
    end;
    case PGSQLType of
      stDate:       Date2PG(Value.Year, Value.Month, Value.Day, PInteger(FPQparamValues[InParamIdx])^);
      stTime:       if Finteger_datetimes
                    then Time2PG(Value.Hour, Value.Minute, Value.Second, Value.Fractions, PInt64(FPQparamValues[InParamIdx])^)
                    else Time2PG(Value.Hour, Value.Minute, Value.Second, Value.Fractions, PDouble(FPQparamValues[InParamIdx])^);
      else          if Finteger_datetimes
                    then TimeStamp2PG(Value, PInt64(FPQparamValues[InParamIdx])^)
                    else TimeStamp2PG(Value, PDouble(FPQparamValues[InParamIdx])^);
      end;
  end else if (PGSQLType in [stUnknown, stString, stUnicodeString, stAsciiStream, stUnicodeStream]) then begin
    Len := DateTimeToRaw(Value.Year, Value.Month, Value.Day,
      Value.Hour, Value.Minute, Value.Second, Value.Fractions,
      PAnsiChar(FByteBuffer), ConSettings^.WriteFormatSettings.DateTimeFormat, False, Value.IsNegative);
    ZSetString(PAnsiChar(FByteBuffer), Len ,fRawTemp);
    BindRawStr(InParamIdx, fRawTemp)
  end else if TryTimeStampToDateTime(Value, DT)
    then InternalBindDouble(Index, stTimeStamp, DT)
    else InternalBindInt(Index, stTimeStamp, 1);
end;

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetUInt(Index: Integer;
  Value: Cardinal);
begin
  {$IFDEF CPU64}
  InternalBindInt(Index, stLong, Value);
  {$ELSE}
  SetLong(Index, Value);
  {$ENDIF}
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZPostgreSQLPreparedStatementV3.SetULong(Index: Integer;
  const Value: UInt64);
begin
  {$IFDEF CPU64}
  InternalBindInt(Index, stLong, Value);
  {$ELSE}
  SetLong(Index, Value);
  {$ENDIF}
end;

procedure TZPostgreSQLPreparedStatementV3.SetWord(Index: Integer; Value: Word);
begin
  InternalBindInt(Index, stInteger, Value);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZPostgreSQLPreparedStatementV3.UnPrepareInParameters;
begin
  { release allocated memory }
  SetParamCount(0);
  Findeterminate_datatype := False;
end;

{ TZPostgreSQLBindList }

class function TZPostgreSQLBindList.GetElementSize: Integer;
begin
  Result := SizeOf(TZPostgreSQLBindValue);
end;

procedure TZPostgreSQLBindList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) then
    PZPostgreSQLBindValue(Ptr).ParamName := '';
  inherited Notify(Ptr, Action);
end;

initialization

{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
SetLength(PGPreparableTokens, 5);
PGPreparableTokens[0].MatchingGroup := 'SELECT';
PGPreparableTokens[Ord(dmlInsert)].MatchingGroup := 'INSERT';
PGPreparableTokens[Ord(dmlUpdate)].MatchingGroup := 'UPDATE';
PGPreparableTokens[Ord(dmlDelete)].MatchingGroup := 'DELETE';
PGPreparableTokens[4].MatchingGroup := 'VALUES';

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.

