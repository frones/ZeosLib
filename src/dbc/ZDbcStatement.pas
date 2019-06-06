{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZDbcStatement;

interface

{$I ZDbc.inc}
{$Z-}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZTokenizer, ZCompatibility, ZVariant, ZDbcLogging, ZClasses,
  ZDbcUtils, FmtBCD;

type
  TZSQLTypeArray = array of TZSQLType;

  {** Implements Abstract Generic SQL Statement. }

  { TZAbstractStatement }

  TZAbstractStatement = class(TZCodePagedObject, IZStatement, IZLoggingObject,
    IImmediatelyReleasable)
  private
    fABufferIndex, fWBufferIndex: Integer;
    FMaxFieldSize: Integer;
    FMaxRows: Integer;
    FEscapeProcessing: Boolean;
    FQueryTimeout: Integer;
    FLastUpdateCount: Integer;
    FLastResultSet: IZResultSet;
    FFetchDirection: TZFetchDirection;
    FFetchSize: Integer;
    FResultSetConcurrency: TZResultSetConcurrency;
    FResultSetType: TZResultSetType;
    FPostUpdates: TZPostUpdatesMode;
    FLocateUpdates: TZLocateUpdatesMode;
    FBatchQueries: TStrings;
    FConnection: IZConnection;
    FInfo: TStrings;
    FChunkSize: Integer; //size of buffer chunks for large lob's related to network settings
    FClosed: Boolean;
    FCachedLob: Boolean;
    procedure SetLastResultSet(const ResultSet: IZResultSet); virtual;
  protected
    FCursorName: RawByteString;
    fWBuffer: array[Byte] of WideChar;
    fABuffer: array[Byte] of AnsiChar;
    FWSQL: ZWideString;
    FaSQL: RawByteString;
    FStatementId : Integer;
    FOpenResultSet: Pointer; //weak reference to avoid memory-leaks and cursor issues
    FClientCP: Word;
    procedure PrepareOpenResultSetForReUse; virtual;
    procedure PrepareLastResultSetForReUse; virtual;
    procedure FreeOpenResultSetReference(const ResultSet: IZResultSet);
    procedure SetASQL(const Value: RawByteString); virtual;
    procedure SetWSQL(const Value: ZWideString); virtual;
    class function GetNextStatementId : integer;
    procedure RaiseUnsupportedException;

    property MaxFieldSize: Integer read FMaxFieldSize write FMaxFieldSize;
    property MaxRows: Integer read FMaxRows write FMaxRows;
    property EscapeProcessing: Boolean
      read FEscapeProcessing write FEscapeProcessing;
    property QueryTimeout: Integer read FQueryTimeout write FQueryTimeout;
    property LastUpdateCount: Integer
      read FLastUpdateCount write FLastUpdateCount;
    property LastResultSet: IZResultSet
      read FLastResultSet write SetLastResultSet;
    property FetchDirection: TZFetchDirection
      read FFetchDirection write FFetchDirection;
    property FetchSize: Integer read FFetchSize write FFetchSize;
    property ResultSetConcurrency: TZResultSetConcurrency
      read FResultSetConcurrency write FResultSetConcurrency;
    property ResultSetType: TZResultSetType
      read FResultSetType write FResultSetType;
    property CursorName: RawByteString read FCursorName write FCursorName;
    property BatchQueries: TStrings read FBatchQueries;
    property Connection: IZConnection read FConnection;
    property Info: TStrings read FInfo;
    property Closed: Boolean read FClosed write FClosed;

    property SQL: String read {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
    property WSQL: ZWideString read FWSQL write SetWSQL;
    property ASQL: RawByteString read FaSQL write SetASQL;
    property ChunkSize: Integer read FChunkSize;
    property CachedLob: Boolean read FCachedLob;
    property ClientCP: word read FClientCP;
    function CreateStmtLogEvent(Category: TZLoggingCategory;
      const Msg: RawByteString=EmptyRaw): TZLoggingEvent;
  public
    procedure ToBuff(const Value: ZWideString; var Result: ZWideString); overload;
    procedure ToBuff(const Value: RawByteString; var Result: RawByteString); overload;
    procedure FlushBuff(var Result: ZWideString); overload;
    procedure FlushBuff(var Result: RawByteString); overload;
  public
    constructor Create(const Connection: IZConnection; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; overload; virtual;
    function ExecuteUpdate(const SQL: ZWideString): Integer; overload; virtual;
    function Execute(const SQL: ZWideString): Boolean; overload; virtual;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; overload; virtual;
    function ExecuteUpdate(const SQL: RawByteString): Integer; overload; virtual;
    function Execute(const SQL: RawByteString): Boolean; overload; virtual;

    function GetSQL : String;

    procedure BeforeClose; virtual;
    procedure Close;
    procedure AfterClose; virtual;
    function IsClosed: Boolean;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost); virtual;

    function GetMaxFieldSize: Integer; virtual;
    procedure SetMaxFieldSize(Value: Integer); virtual;
    function GetMaxRows: Integer; virtual;
    procedure SetMaxRows(Value: Integer); virtual;
    function GetQueryTimeout: Integer; virtual;
    procedure SetQueryTimeout(Value: Integer); virtual;
    procedure Cancel; virtual;
    procedure SetCursorName(const Value: String); virtual;

    function GetResultSet: IZResultSet; virtual;
    function GetUpdateCount: Integer; virtual;
    function GetMoreResults: Boolean; virtual;

    procedure SetFetchDirection(Value: TZFetchDirection); virtual;
    function GetFetchDirection: TZFetchDirection; virtual;
    procedure SetFetchSize(Value: Integer); virtual;
    function GetFetchSize: Integer; virtual;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); virtual;
    function GetResultSetConcurrency: TZResultSetConcurrency; virtual;
    procedure SetResultSetType(Value: TZResultSetType); virtual;
    function GetResultSetType: TZResultSetType; virtual;

    procedure SetPostUpdates(Value: TZPostUpdatesMode);
    function GetPostUpdates: TZPostUpdatesMode;
    procedure SetLocateUpdates(Value: TZLocateUpdatesMode);
    function GetLocateUpdates: TZLocateUpdatesMode;

    procedure AddBatch(const SQL: string); virtual;
    procedure AddBatchRequest(const SQL: string); virtual;
    procedure ClearBatch; virtual;
    function ExecuteBatch: TIntegerDynArray; virtual;

    function GetConnection: IZConnection;
    function GetParameters: TStrings;
    function GetChunkSize: Integer;

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; virtual;
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString; virtual;
    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; virtual;
  end;

  TZBindType = (zbtNull, zbt8Byte, zbt4Byte,
    zbtRawString, zbtUTF8String, {$IFNDEF NEXTGEN}zbtAnsiString,{$ENDIF}
    zbtUniString, zbtCharByRef, zbtBinByRef, zbtGUID, zbtBytes,
    zbtArray, zbtRefArray, zbtLob, zbtPointer, zbtBCD, zbtTimeStamp, zbtCustom);

  PZBindValue = ^TZBindValue;
  TZBindValue = record
    Value:      Pointer;
    BindType:   TZBindType;
    ParamType:  TZProcedureColumnType;
    SQLType:    TZSQLType;
  end;
  PZBufRec = ^TZBufRec;
  TZBufRec = record
    Len: LengthInt;
    Buf: Pointer;
  end;
  PZCustomData =  ^TZCustomData;
  TZCustomData = packed record
    Len: LengthInt;
    //data goes here : array[0..?] of byte;
  end;

const
  TZBindTypeSize: array[TZBindType] of Integer = (0,{$IFNDEF CPU64}8{$ELSE}0{$ENDIF}, 0,
    0, 0, {$IFNDEF NEXTGEN}0,{$ENDIF}
    0, SizeOf(TZCharRec), SizeOf(TZBufRec), SizeOf(TGUID), 0,
    SizeOf(TZArray), SizeOf(TZArray), 0, 0, SizeOf(TBCD), SizeOf(TZTimeStamp), 0);
type
  P8Bytes = PInt64;
  P4Bytes = PCardinal;

  PZTimeStamp = ^TZTimeStamp;

  PZTParamValueArray = ^TZTParamValueArray;
  {** Defines a static array of bind values. }
  TZTParamValueArray = array[0..High(Word)] of TZBindValue;

  TZAbstractPreparedStatement2 = class; //forward
  TZBindList = class
  private
    FValues: PZTParamValueArray;
    FCount: Integer;
    FCapacity: Integer;
    FConSettings: PZConSettings;
    procedure Grow;
    {$IFNDEF DISABLE_CHECKING}
    class procedure Error(const Msg: string; Data: Integer);
    {$ENDIF}
    function AquireBuffer(Index: Integer; SQLType: TZSQLType; BindType: TZBindType): PZBindValue; //{$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure SetCapacity(NewCapacity: Integer);
    function Get(Index: Integer): PZBindValue; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetBindType(Index: Integer): TZBindType; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetSQLType(Index: Integer): TZSQLType; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetType(Index: Integer): TZProcedureColumnType; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetVariant(Index: Integer): TZVariant;
    function GetArray(Index: Integer): PZArray; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function Get8Byte(Index: Integer): P8Bytes; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function Get4Byte(Index: Integer): P4Bytes; {$IFDEF WITH_INLINE}inline;{$ENDIF}
  public
    constructor Create(ConSettings: PZConSettings);
    destructor Destroy; override;
  public
    procedure Clear;
    procedure ClearValue(Index: Integer); //{$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure Delete(Index: Integer);
    procedure ClearValues;

    procedure Put(Index: Integer; Value: Boolean); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; _8Byte: P8Bytes); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; _4Byte: P4Bytes); overload;
    procedure Put(Index: Integer; const Value: TBCD); overload;
    procedure Put(Index: Integer; const Value: TZTimeStamp); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: TBytes); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: RawByteString; CP: Word); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt; CP: Word); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: ZWideString); overload;
    procedure Put(Index: Integer; const Value: TZArray; AddArrayRef: Boolean); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: IZBLob); overload;
    procedure Put(Index: Integer; Value: PZBindValue); overload;
    procedure Put(Index: Integer; const Value: TGUID); overload;
    function AquireCustomValue(Index: Integer; SQLType: TZSQLType; Len: LengthInt): Pointer;
    function AquireMinCustomValue(Index: Integer; SQLType: TZSQLType; Len: LengthInt): Pointer;

    procedure SetCount(NewCount: Integer);
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    procedure FlushAll;

    procedure BindValuesToStatement(Stmt: TZAbstractPreparedStatement2; SupportsBidirectionalParams: Boolean);
    function HasOutParams: Boolean;
  public
    property Count: Integer read FCount write SetCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Bindings[Index: Integer]: PZBindValue read Get; default;
    property ParamTypes[Index: Integer]: TZProcedureColumnType read GetType;
    property Variants[Index: Integer]: TZVariant read GetVariant;
    property SQLTypes[Index: Integer]: TZSQLType read GetSQLType;
    property BindTypes[Index: Integer]: TZBindType read GetBindType;
    property Arrays[Index: Integer]: PZArray read GetArray;
    property _8Bytes[Index: Integer]: P8Bytes read Get8Byte;
    property _4Bytes[Index: Integer]: P4Bytes read Get4Byte;
  end;

  {** Implements Abstract Prepared SQL Statement. }

  { TZAbstractPreparedStatement }
  TZAbstractPreparedStatement2 = class(TZAbstractStatement, IImmediatelyReleasable)
  private
    FBatchDMLArrayCount: ArrayLenInt;
    FPrepared : Boolean;
    FSupportsDMLBatchArrays: Boolean;
    FBindList: TZBindList;
  protected
    FUniTemp: ZWideString;
    FRawTemp: RawByteString;
    FTokenMatchIndex, //did we match a token to indicate if Prepare makes sense?
    FCountOfQueryParams: Integer; //how many params did we found to prepvent mem-reallocs?
    FGUIDAsString: Boolean; //How should a GUID value be treaded?
    FHasInOutParams: Boolean; //are Input/output params registered?
    FWeakIntfPtrOfIPrepStmt: Pointer; //EH: address of IZPreparedStatement(Self) to access non virtual methods
    property TokenMatchIndex: Integer read FTokenMatchIndex;
    procedure CheckParameterIndex(Value: Integer); virtual;
    procedure PrepareInParameters; virtual;
    procedure BindInParameters; virtual;
    procedure UnPrepareInParameters; virtual;

    procedure ValidateArraySizeAndType(const Value: Pointer; SQLType: TZSQLType;
      VariantType: TZVariantType; ParamIndex: Integer);

    procedure SetParamCount(NewParamCount: Integer); virtual;
    procedure SetBindCapacity(Capacity: Integer); virtual;

    procedure LogPrepStmtMessage(Category: TZLoggingCategory; const Msg: RawByteString = EmptyRaw);
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; virtual;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; virtual;
    function SupportsBidirectionalParams: Boolean; virtual;
    function AlignParamterIndex2ResultSetIndex(Value: Integer): Integer; virtual;
  protected //Properties
    property BatchDMLArrayCount: ArrayLenInt read FBatchDMLArrayCount write FBatchDMLArrayCount;
    property SupportsDMLBatchArrays: Boolean read FSupportsDMLBatchArrays;
    property BindList: TZBindList read FBindList;
  protected //the sql conversions
    procedure SetASQL(const Value: RawByteString); override;
    procedure SetWSQL(const Value: ZWideString); override;
  protected //binding
    procedure BindArray(Index: Integer; const Value: TZArray); virtual;
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); virtual;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); virtual;
    procedure BindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double); virtual;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); virtual;
    procedure BindSignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: Int64); virtual;
    procedure BindUnsignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: UInt64); virtual;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; virtual;
    function ExecuteUpdatePrepared: Integer; virtual;
    function ExecutePrepared: Boolean; virtual;

    procedure BeforeClose; override;
    function GetSQL : String;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function IsPrepared: Boolean; virtual;
    property Prepared: Boolean read IsPrepared;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); virtual; abstract;

    procedure SetByte(ParameterIndex: Integer; Value: Byte); virtual;
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt); virtual;
    procedure SetWord(ParameterIndex: Integer; Value: Word); virtual;
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt); virtual;
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal); virtual;
    procedure SetInt(ParameterIndex: Integer; Value: Integer); virtual;
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64); virtual;
    procedure SetLong(ParameterIndex: Integer; const Value: Int64); virtual;
    procedure SetFloat(ParameterIndex: Integer; Value: Single); virtual;
    procedure SetDouble(ParameterIndex: Integer; const Value: Double); virtual;
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency); virtual;
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: TBCD); virtual;

    procedure SetPChar(ParameterIndex: Integer; Value: PChar); virtual;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); virtual; abstract;
    procedure SetString(ParameterIndex: Integer; const Value: String); virtual; abstract;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); virtual; abstract;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); virtual; abstract;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); virtual; abstract;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);  virtual; abstract;
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); virtual;
    procedure SetGUID(ParameterIndex: Integer; const Value: TGUID); virtual;
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetAsciiStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetUnicodeStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetBinaryStream(ParameterIndex: Integer; const Value: TStream);
    procedure SetBlob(ParameterIndex: Integer; SQLType: TZSQLType; const Value: IZBlob); virtual;
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant);
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); virtual;
    procedure SetDataArray(Index: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); virtual;

    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); virtual;

    function IsNull(Index: Integer): Boolean; virtual;
    procedure GetBoolean(Index: Integer; out Result: Boolean); overload; virtual;
    procedure GetOrdinal(Index: Integer; out Result: Int64); overload; virtual;
    procedure GetOrdinal(Index: Integer; out Result: UInt64); overload; virtual;
    procedure GetCurrency(Index: Integer; out Result: Currency); overload; virtual;
    procedure GetDouble(Index: Integer; out Result: Double); overload; virtual;
    procedure GetBigDecimal(Index: Integer; var Result: TBCD); overload; virtual;
    procedure GetBytes(Index: Integer; out Buf: Pointer; out Len: LengthInt); overload; virtual;
    procedure GetDateTime(Index: Integer; out Result: TDateTime); virtual;
    procedure GetTimeStamp(Index: Integer; out Result: TZTimeStamp); overload; virtual;
    procedure GetLob(Index: Integer; out Result: IZBlob); virtual;
    procedure GetPChar(Index: Integer; out Buf: Pointer; out Len: LengthInt; CodePage: Word); overload; virtual;

    procedure ClearParameters; virtual;

    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; override;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); override;
    procedure SetResultSetType(Value: TZResultSetType); override;
  end;

  TZRawPreparedStatement = class(TZAbstractPreparedStatement2)
  protected
    FInParamDefaultValues: TRawByteStringDynArray;
    procedure BindRawStr(Index: Integer; const Value: RawByteString); overload; virtual;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); overload; virtual;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure SetBindCapacity(Capacity: Integer); override;
    property InParamDefaultValues: TRawByteStringDynArray read FInParamDefaultValues;
  public
    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); override;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); override;
    procedure SetString(ParameterIndex: Integer; const Value: String); override;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); override;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); override;
  end;

  TZRawParamDetectPreparedStatement = class(TZRawPreparedStatement)
  protected
    FCachedQueryRaw: TRawByteStringDynArray;
    FNCharDetected: PBooleanDynArray;
    FIsParamIndex: TBooleanDynArray;
    property IsParamIndex: TBooleanDynArray read FIsParamIndex;
    //property IsNCharIndex: TBooleanDynArray read FNCharDetected;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    procedure Unprepare; override;
  end;

  TZUTF16PreparedStatement = class(TZAbstractPreparedStatement2)
  protected
    FInParamDefaultValues: TUnicodeStringDynArray;
    procedure BindUniStr(Index: Integer; const Value: ZWideString); overload; virtual;
    procedure BindUniStr(Index: Integer; Buf: PWideChar; CodePoints: LengthInt); overload; virtual;
    property InParamDefaultValues: TUnicodeStringDynArray read FInParamDefaultValues;
    procedure SetBindCapacity(Capacity: Integer); override;
  public
    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); override;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); override;
    procedure SetString(ParameterIndex: Integer; const Value: String); override;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); override;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); override;
  end;

  TZUTF16ParamDetectPreparedStatement = class(TZUTF16PreparedStatement)
  protected
    FCachedQueryUni: TUnicodeStringDynArray;
    FNCharDetected: PBooleanDynArray;
    FIsParamIndex: TBooleanDynArray;
    //property IsParamIndex: TBooleanDynArray read FIsParamIndex;
    //property IsNCharIndex: TBooleanDynArray read FNCharDetected;
  public
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString; override;
    procedure Unprepare; override;
  end;

  TZCallExecKind = (zcekParams, zcekSelect);

  TZAbstractCallableStatement2 = class(TZAbstractPreparedStatement2)
  private
    FStoredProcName: String;
    FBindAgain: array[TZCallExecKind] of Boolean;
    FCharRec: TZCharRec;
  protected
    FCallExecKind: TZCallExecKind;
    FExecStatements: array[TZCallExecKind] of TZAbstractPreparedStatement2;
    function CreateExecutionStatement(Mode: TZCallExecKind; const StoredProcName: String): TZAbstractPreparedStatement2; virtual; abstract;
    function IsFunction: Boolean;
    procedure BindInParameters; override;
    procedure PrepareInParameters; override;
    property StoredProcName: String read FStoredProcName;
  public //value getter procs
    procedure GetBoolean(Index: Integer; out Result: Boolean); override;
    procedure GetOrdinal(Index: Integer; out Result: Int64); override;
    procedure GetOrdinal(Index: Integer; out Result: UInt64); override;
    procedure GetCurrency(Index: Integer; out Result: Currency); override;
    procedure GetDouble(Index: Integer; out Result: Double); override;
    procedure GetBigDecimal(Index: Integer; var Result: TBCD); override;
    procedure GetBytes(Index: Integer; out Buf: Pointer; out Len: LengthInt); override;
    procedure GetDateTime(Index: Integer; out Result: TDateTime); override;
    procedure GetTimeStamp(Index: Integer; out Result: TZTimeStamp); override;
    procedure GetLob(Index: Integer; out Result: IZBlob); override;
    procedure GetPChar(Index: Integer; out Buf: Pointer; out Len: LengthInt; CodePage: Word); override;
  public //value getter methods
    function IsNull(ParameterIndex: Integer): Boolean; override;

    function GetPChar(ParameterIndex: Integer): PChar; overload;
    function GetBoolean(ParameterIndex: Integer): Boolean; overload;
    function GetByte(ParameterIndex: Integer): Byte;
    function GetShort(ParameterIndex: Integer): ShortInt;
    function GetWord(ParameterIndex: Integer): Word;
    function GetSmall(ParameterIndex: Integer): SmallInt;
    function GetUInt(ParameterIndex: Integer): Cardinal;
    function GetInt(ParameterIndex: Integer): Integer;
    function GetULong(ParameterIndex: Integer): UInt64;
    function GetLong(ParameterIndex: Integer): Int64;
    function GetFloat(ParameterIndex: Integer): Single;
    function GetDouble(ParameterIndex: Integer): Double; overload;
    function GetCurrency(ParameterIndex: Integer): Currency; overload;
    function GetBigDecimal(ParameterIndex: Integer): Extended; overload;
    function GetBytes(ParameterIndex: Integer): TBytes; overload;
    function GetDate(ParameterIndex: Integer): TDateTime;
    function GetTime(ParameterIndex: Integer): TDateTime;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; overload;
    function GetValue(ParameterIndex: Integer): TZVariant;
  public //value setter methods
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
  public
    function GetResultSet: IZResultSet; override;
    function GetUpdateCount: Integer; override;
    function GetMoreResults: Boolean; override;
  public
    procedure SetDefaultValue(ParameterIndex: Integer; const Value: String); override;
  public //additional IZCallableStatement api
    function GetFirstResultSet: IZResultSet; virtual;
    function GetPreviousResultSet: IZResultSet; virtual;
    function GetNextResultSet: IZResultSet; virtual;
    function GetLastResultSet: IZResultSet; virtual;
    function BOR: Boolean; virtual;
    function EOR: Boolean; virtual;
    function GetResultSetByIndex(const Index: Integer): IZResultSet; virtual;
    function GetResultSetCount: Integer; virtual;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SQLType: Integer); virtual;
    procedure RegisterParamType(ParameterIndex:integer;ParamType:Integer); virtual;
  public
    procedure Prepare; override;
    procedure Unprepare; override;
  public
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    constructor Create(const Connection: IZConnection; const StoredProcOrFuncIdentifier: string;
      {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);

    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;
  end;

  TZAbstractCallableStatement_A = class(TZAbstractCallableStatement2)
  protected
    procedure BindRawStr(Index: Integer; const Value: RawByteString); overload;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); overload;
  public
    procedure AfterConstruction; override;
  public //setters
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); override;
    procedure SetString(ParameterIndex: Integer; const Value: String); override;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); override;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); override;
  public //getters
    function GetString(ParameterIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString;
  end;

  TZAbstractCallableStatement_W = class(TZAbstractCallableStatement2)
  public
    procedure AfterConstruction; override;
    procedure FillAndBindCharRec(Index: Integer; out Len: LengthInt);
  public //setters
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); reintroduce;
    procedure SetString(ParameterIndex: Integer; const Value: String); reintroduce;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); reintroduce;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); reintroduce;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); reintroduce;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); reintroduce;
  public //getters
    function GetString(ParameterIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString;
  end;

  {** Implements Abstract Prepared SQL Statement. }

  { TZAbstractPreparedStatement }

  TZAbstractPreparedStatement = class(TZAbstractStatement, IZPreparedStatement,
    IImmediatelyReleasable)
  private
    FInParamValues: TZVariantDynArray;
    FBatchDMLArrayCount: ArrayLenInt;
    FPrepared : Boolean;
    FClientVariantManger: IZClientVariantManager;
    FExecCount: Integer;
    FSupportsDMLBatchArrays: Boolean;
  protected
    FCachedQueryRaw: TRawByteStringDynArray;
    FCachedQueryUni: TUnicodeStringDynArray;
    FNCharDetected: TBooleanDynArray;
    FIsParamIndex: TBooleanDynArray;
    FTokenMatchIndex, FParamsCnt: Integer;
    FInParamTypes: TZSQLTypeArray;
    FInParamDefaultValues: TStringDynArray;
    FInParamCount: Integer;
    function GetClientVariantManger: IZClientVariantManager;
    function SupportsSingleColumnArrays: Boolean; virtual;
    procedure PrepareInParameters; virtual;
    procedure BindInParameters; virtual;
    procedure UnPrepareInParameters; virtual;
    procedure ValidateArraySizeAndType(const Value: Pointer; SQLType: TZSQLType;
      VariantType: TZVariantType; ParamIndex: Integer);

    procedure SetInParamCount(NewParamCount: Integer); virtual;
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); virtual;
    procedure LogPrepStmtMessage(Category: TZLoggingCategory; const Msg: RawByteString = EmptyRaw);
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; virtual;
    function GetOmitComments: Boolean; virtual;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; virtual;

    property InParamValues: TZVariantDynArray read FInParamValues write FInParamValues;
    property InParamTypes: TZSQLTypeArray read FInParamTypes write FInParamTypes;
    property InParamDefaultValues: TStringDynArray
      read FInParamDefaultValues write FInParamDefaultValues;
    property InParamCount: Integer read FInParamCount write FInParamCount;
    property ClientVarManager: IZClientVariantManager read FClientVariantManger;
    property CachedQueryRaw: TRawByteStringDynArray read FCachedQueryRaw;
    property CachedQueryUni: TUnicodeStringDynArray read FCachedQueryUni;
    property IsParamIndex: TBooleanDynArray read FIsParamIndex;
    property IsNCharIndex: TBooleanDynArray read FNCharDetected;
    property TokenMatchIndex: Integer read FTokenMatchIndex;
    property CountOfQueryParams: Integer read fParamsCnt;
    property ArrayCount: ArrayLenInt read FBatchDMLArrayCount;
    property ExecutionCount: Integer read FExecCount;
    property SupportsDMLBatchArrays: Boolean read FSupportsDMLBatchArrays;
    procedure SetASQL(const Value: RawByteString); override;
    procedure SetWSQL(const Value: ZWideString); override;

    procedure InternalSetOrdinal(ParameterIndex: Integer; SQLType: TZSQLType; const Value: Int64); virtual;
    procedure InternalSetDouble(ParameterIndex: Integer; SQLType: TZSQLType; const Value: Double); virtual;
    procedure InternalSetDateTime(ParameterIndex: Integer; SQLType: TZSQLType; const Value: TDateTime); virtual;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; virtual;
    function ExecuteUpdatePrepared: Integer; virtual;
    function ExecutePrepared: Boolean; virtual;

    procedure BeforeClose; override;
    function GetSQL : String;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function IsPrepared: Boolean; virtual;
    property Prepared: Boolean read IsPrepared;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable;
      var AError: EZSQLConnectionLost); override;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); virtual;

    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType); virtual;
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean); virtual;
    procedure SetByte(ParameterIndex: Integer; Value: Byte); virtual;
    procedure SetShort(ParameterIndex: Integer; Value: ShortInt); virtual;
    procedure SetWord(ParameterIndex: Integer; Value: Word); virtual;
    procedure SetSmall(ParameterIndex: Integer; Value: SmallInt); virtual;
    procedure SetUInt(ParameterIndex: Integer; Value: Cardinal); virtual;
    procedure SetInt(ParameterIndex: Integer; Value: Integer); virtual;
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64); virtual;
    procedure SetLong(ParameterIndex: Integer; const Value: Int64); virtual;
    procedure SetFloat(ParameterIndex: Integer; Value: Single); virtual;
    procedure SetDouble(ParameterIndex: Integer; const Value: Double); virtual;
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency); virtual;
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: TBCD); virtual;
    procedure SetPChar(ParameterIndex: Integer; Value: PChar); virtual;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); virtual;
    procedure SetString(ParameterIndex: Integer; const Value: String); virtual;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); virtual;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); virtual;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);  virtual; //AVZ
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); virtual;
    procedure SetGUID(ParameterIndex: Integer; const Value: TGUID); virtual;
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetAsciiStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetUnicodeStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetBinaryStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetBlob(ParameterIndex: Integer; SQLType: TZSQLType; const Value: IZBlob); virtual;
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant); virtual;
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); virtual;
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); virtual;

    procedure ClearParameters; virtual;

    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString; override;
    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; override;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); override;
    procedure SetResultSetType(Value: TZResultSetType); override;
  end;

  {** Implements Abstract Callable SQL statement. }

  TZAbstractCallableStatement = class(TZAbstractPreparedStatement,
    IZCallableStatement)
  private
    FOutParamValues: TZVariantDynArray;
    FOutParamTypes: TZSQLTypeArray;
    FOutParamCount: Integer;
    FLastWasNull: Boolean;
    FTemp: String;
    FSQL: String;
    FProcSql: RawByteString;
    FIsFunction: Boolean;
    FHasOutParameter: Boolean;
  protected
    FResultSets: IZCollection;
    FActiveResultset: Integer;
    FDBParamTypes: TZProcedureColumnTypeDynArray;
    procedure ClearResultSets; virtual;
    procedure TrimInParameters; virtual;
    procedure SetOutParamCount(NewParamCount: Integer); virtual;
    function GetOutParam(ParameterIndex: Integer): TZVariant; virtual;
    procedure SetProcSQL(const Value: RawByteString); virtual;

    property OutParamValues: TZVariantDynArray
      read FOutParamValues write FOutParamValues;
    property OutParamTypes: TZSQLTypeArray
      read FOutParamTypes write FOutParamTypes;
    property OutParamCount: Integer read FOutParamCount write FOutParamCount;
    property LastWasNull: Boolean read FLastWasNull write FLastWasNull;
    property ProcSql: RawByteString read FProcSQL write SetProcSQL;
    property SQL: String read FSQL;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    procedure ClearParameters; override;
    procedure BeforeClose; override;

    function IsFunction: Boolean;
    function HasOutParameter: Boolean;
    function GetFirstResultSet: IZResultSet; virtual;
    function GetPreviousResultSet: IZResultSet; virtual;
    function GetNextResultSet: IZResultSet; virtual;
    function GetLastResultSet: IZResultSet; virtual;
    function BOR: Boolean; virtual;
    function EOR: Boolean; virtual;
    function GetResultSetByIndex(const Index: Integer): IZResultSet; virtual;
    function GetResultSetCount: Integer; virtual;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SQLType: Integer); virtual;// deprecated;
    procedure RegisterParamType(ParameterIndex:integer;ParamType:Integer);virtual;
    function WasNull: Boolean; virtual;// deprecated;

    procedure RegisterParameter(ParameterIndex: Integer; SQLType: TZSQLType;
      ParamType: TZProcedureColumnType; const Name: String = ''; PrecisionOrSize: LengthInt = 0;
      Scale: LengthInt = 0); virtual;

    function IsNull(ParameterIndex: Integer): Boolean; virtual;
    function GetPChar(ParameterIndex: Integer): PChar; virtual;
    function GetString(ParameterIndex: Integer): String; virtual;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString; virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String; virtual;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString; virtual;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString; virtual;
    function GetBoolean(ParameterIndex: Integer): Boolean; virtual;
    function GetByte(ParameterIndex: Integer): Byte; virtual;
    function GetShort(ParameterIndex: Integer): ShortInt; virtual;
    function GetWord(ParameterIndex: Integer): Word; virtual;
    function GetSmall(ParameterIndex: Integer): SmallInt; virtual;
    function GetUInt(ParameterIndex: Integer): Cardinal; virtual;
    function GetInt(ParameterIndex: Integer): Integer; virtual;
    function GetULong(ParameterIndex: Integer): UInt64; virtual;
    function GetLong(ParameterIndex: Integer): Int64; virtual;
    function GetFloat(ParameterIndex: Integer): Single; virtual;
    function GetDouble(ParameterIndex: Integer): Double; virtual;
    function GetCurrency(ParameterIndex: Integer): Currency; virtual;
    procedure GetBigDecimal(ParameterIndex: Integer; var Result: TBCD);
    function GetBytes(ParameterIndex: Integer): TBytes; virtual;
    function GetDate(ParameterIndex: Integer): TDateTime; virtual;
    function GetTime(ParameterIndex: Integer): TDateTime; virtual;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; virtual;
    function GetValue(ParameterIndex: Integer): TZVariant; virtual;
  end;

  {** Implements a real Prepared Callable SQL Statement. }
  TZAbstractPreparedCallableStatement = CLass(TZAbstractCallableStatement)
  protected
    FProcSQL: RawByteString;
    procedure SetProcSQL(const Value: RawByteString); override;
  public
    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;
    function Execute(const SQL: RawByteString): Boolean; override;
  end;

  {** Implements an Emulated Prepared SQL Statement. }

  { TZEmulatedPreparedStatement }

  TZEmulatedPreparedStatement_A = class(TZAbstractPreparedStatement)
  protected
    FNeedNCharDetection: Boolean;
  protected
    procedure TokenizeSQLQueryRaw;
    function GetParamAsString(ParamIndex: Integer): RawByteString; virtual; abstract;
    function ComposeRawSQLQuery: RawByteString;
  end;

  TZEmulatedPreparedStatement_W = class(TZAbstractPreparedStatement)
  protected
    FNeedNCharDetection: Boolean;
  protected
    procedure TokenizeSQLQueryUni;
    function GetParamAsString(ParamIndex: Integer): ZWideString; virtual; abstract;
    function ComposeWideSQLQuery: ZWideString;
  end;

implementation

uses ZFastCode, ZSysUtils, ZMessages, ZDbcResultSet, ZCollections,
  ZEncoding, ZDbcProperties
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

var
{**
  Holds the value of the last assigned statement ID.
  Only Accessible using TZAbstractStatement.GetNextStatementId.
}
  GlobalStatementIdCounter : integer;

{**
  Constructs this class and defines the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters;
}
constructor TZAbstractStatement.Create(const Connection: IZConnection;
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
begin
  { Sets the default properties. }
  inherited Create;
  Self.ConSettings := Connection.GetConSettings;
  FLastUpdateCount := -1;
  FClientCP := Connection.GetConSettings.ClientCodePage.CP;
  FConnection := Connection;
  Connection.RegisterStatement(Self);
  FBatchQueries := TStringList.Create;

  FInfo := TStringList.Create;
  if Info <> nil then
    FInfo.AddStrings(Info);
  FChunkSize := StrToIntDef(DefineStatementParameter(Self, DSProps_ChunkSize, '4096'), 4096);
  FCachedLob := StrToBoolEx(DefineStatementParameter(Self, DSProps_CachedLobs, 'false'));
  FStatementId := Self.GetNextStatementId;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractStatement.Destroy;
begin
  Close;
  FreeAndNil(FBatchQueries);
  FConnection.DeregisterStatement(Self);
  FConnection := nil;
  FreeAndNil(FInfo);
  inherited Destroy;
end;

{**
  Sets the preprepared SQL-Statement in an String and AnsiStringForm.
  @param Value: the SQL-String which has to be optional preprepared
}
procedure TZAbstractStatement.SetWSQL(const Value: ZWideString);
begin
  if FWSQL <> Value then
    {$IFDEF UNICODE}
    if not (ConSettings^.ClientCodePage^.Encoding = ceUTF16) then
      FASQL := GetRawEncodedSQL(Value);
    FWSQL := Value;
    {$ELSE !UNICODE}
    begin
      FaSQL := ConSettings^.ConvFuncs.ZUnicodeToRaw(Value, ConSettings^.ClientCodePage^.CP); //required for the resultsets
      FWSQL := Value;
    end;
    {$ENDIF UNICODE}
end;

procedure TZAbstractStatement.ToBuff(const Value: RawByteString;
  var Result: RawByteString);
var
  P: PAnsiChar;
  L: Integer;
begin
  L := Length(Value);
  if L = 0 then Exit;
  if L < (SizeOf(fABuffer)-fABufferIndex) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> no move
    then fABuffer[fABufferIndex] := AnsiChar(P^)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, fABuffer[fABufferIndex], L);
    Inc(fABufferIndex, L);
  end else begin
    SetLength(Result, Length(Result)+fABufferIndex+L);
    P := Pointer(Result);
    Inc(P, Length(Result)-fABufferIndex-L);
    if fABufferIndex > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(fABuffer[0], P^, fABufferIndex);
      Inc(P, fABufferIndex);
      fABufferIndex := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L);
  end;
end;

procedure TZAbstractStatement.ToBuff(const Value: ZWideString;
  var Result: ZWideString);
var
  P: PWideChar;
  L: Integer;
begin
  L := Length(Value);
  if L = 0 then Exit;
  if L < ((SizeOf(fWBuffer) shr 1)-fWBufferIndex) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> no move
    then fWBuffer[fWBufferIndex] := P^
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, fWBuffer[fWBufferIndex], L shl 1);
    Inc(fWBufferIndex, L);
  end else begin
    SetLength(Result, Length(Result)+fWBufferIndex+L);
    P := Pointer(Result);
    Inc(P, Length(Result)-fWBufferIndex-L);
    if fWBufferIndex > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(fWBuffer[0], P^, fWBufferIndex shl 1);
      Inc(P, fWBufferIndex);
      fWBufferIndex := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L shl 1);
  end;
end;

procedure TZAbstractStatement.SetASQL(const Value: RawByteString);
begin
  if FASQL <> Value then
  begin
    {$IFDEF UNICODE}
    FASQL := Value;
    FWSQL := ZRawToUnicode(FASQL, ConSettings^.ClientCodePage^.CP); //required for the resultsets
    {$ELSE !UNICODE}
    FASQL := GetRawEncodedSQL(Value);
    if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then
      FWSQL := ZRawToUnicode(FASQL, ConSettings^.ClientCodePage^.CP);
    {$ENDIF UNICODE}
  end;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractStatement.RaiseUnsupportedException;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
end;

procedure TZAbstractStatement.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
var ImmediatelyReleasable: IImmediatelyReleasable;
begin
  if not FClosed then begin
    FClosed := True;
    if (FOpenResultSet <> nil) and Supports(IZResultSet(FOpenResultSet), IImmediatelyReleasable, ImmediatelyReleasable) and
       (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
    if Assigned(FLastResultSet) and Supports(FLastResultSet, IImmediatelyReleasable, ImmediatelyReleasable) and
       (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
    if Assigned(Connection) and Supports(Connection, IImmediatelyReleasable, ImmediatelyReleasable) and
       (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
  end;
end;

{**
  Sets a last result set to avoid problems with reference counting.
  @param ResultSet the lastest executed result set.
}
procedure TZAbstractStatement.SetLastResultSet(const ResultSet: IZResultSet);
begin
  if (FLastResultSet <> nil) and (Pointer(ResultSet) <> Pointer(FLastResultSet)) then
    FLastResultSet.Close;

  FLastResultSet := ResultSet;
end;

procedure TZAbstractStatement.PrepareOpenResultSetForReUse;
begin
  if Assigned(FOpenResultSet) then
    if IZResultSet(FOpenResultSet).IsClosed then
      FOpenResultSet := nil
    else if (IZResultSet(FOpenResultSet).GetConcurrency = GetResultSetConcurrency) and
            (IZResultSet(FOpenResultSet).GetFetchDirection = GetFetchDirection) then
      IZResultSet(FOpenResultSet).ResetCursor
    else begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
end;

procedure TZAbstractStatement.PrepareLastResultSetForReUse;
begin
  if Assigned(FLastResultSet) then
    if FLastResultSet.IsClosed then //is there another way to test if open?
      FLastResultSet := nil
    else if (FLastResultSet.GetConcurrency = GetResultSetConcurrency) and
            (FLastResultSet.GetFetchDirection = GetFetchDirection) then
      FLastResultSet.ResetCursor
    else begin
      FLastResultSet.Close;
      FLastResultSet := nil;
    end;
end;

procedure TZAbstractStatement.FlushBuff(var Result: RawByteString);
var P: PAnsiChar;
begin
  if fABufferIndex > 0 then begin
    SetLength(Result, Length(Result)+fABufferIndex);
    P := Pointer(Result);
    Inc(P, Length(Result)-fABufferIndex);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(fABuffer[0], P^, fABufferIndex);
    fABufferIndex := 0;
  end;
end;

procedure TZAbstractStatement.FlushBuff(var Result: ZWideString);
var P: PWideChar;
begin
  if fWBufferIndex > 0 then begin
    SetLength(Result, Length(Result)+fWBufferIndex);
    P := Pointer(Result);
    Inc(P, Length(Result)-fWBufferIndex);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(fWBuffer[0], P^, fWBufferIndex shl 1);
    fWBufferIndex := 0;
  end;
end;

procedure TZAbstractStatement.FreeOpenResultSetReference(const ResultSet: IZResultSet);
begin
  if FOpenResultSet = Pointer(ResultSet) then
    FOpenResultSet := nil;
  if Pointer(FLastResultSet) = Pointer(ResultSet) then
    FLastResultSet := nil;
end;

class function TZAbstractStatement.GetNextStatementId: integer;
begin
  Inc(GlobalStatementIdCounter);
  Result := GlobalStatementIdCounter;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  WSQL := SQL;
  Result := ExecuteQuery(ASQL);
end;

function TZAbstractStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  ASQL := SQL;
  Result := nil;
  DriverManager.LogMessage(lcExecute,Self);
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
function TZAbstractStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  WSQL := SQL;
  Result := ExecuteUpdate(ASQL);
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
function TZAbstractStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  ASQL := SQL;
  Result := -1;
  DriverManager.LogMessage(lcExecute,Self);
end;

{**
  Releases this <code>Statement</code> object's database
  and JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.
  It is generally good practice to release resources as soon as
  you are finished with them to avoid tying up database
  resources.
  <P><B>Note:</B> A <code>Statement</code> object is automatically closed when it is
  garbage collected. When a <code>Statement</code> object is closed, its current
  <code>ResultSet</code> object, if one exists, is also closed.
}
procedure TZAbstractStatement.Close;
var RefCountAdded: Boolean;
begin
  RefCountAdded := (RefCount = 1) and Assigned(FOpenResultSet) or Assigned(FLastResultSet);
  if RefCountAdded then _AddRef;
  try
    BeforeClose;
    FClosed := True;
    AfterClose;
  finally
    FClosed := True;
    if RefCountAdded then begin
      if (RefCount = 1) then
        DriverManager.AddGarbage(Self);
      _Release;
    end;
  end;
end;

{**
  Returns the maximum number of bytes allowed
  for any column value.
  This limit is the maximum number of bytes that can be
  returned for any column value.
  The limit applies only to <code>BINARY</code>,
  <code>VARBINARY</code>, <code>LONGVARBINARY</code>, <code>CHAR</code>, <code>VARCHAR</code>, and <code>LONGVARCHAR</code>
  columns.  If the limit is exceeded, the excess data is silently
  discarded.
  @return the current max column size limit; zero means unlimited
}
function TZAbstractStatement.GetMaxFieldSize: Integer;
begin
  Result := FMaxFieldSize;
end;

{**
  Sets the limit for the maximum number of bytes in a column to
  the given number of bytes.  This is the maximum number of bytes
  that can be returned for any column value.  This limit applies
  only to <code>BINARY</code>, <code>VARBINARY</code>,
  <code>LONGVARBINARY</code>, <code>CHAR</code>, <code>VARCHAR</code>, and
  <code>LONGVARCHAR</code> fields.  If the limit is exceeded, the excess data
  is silently discarded. For maximum portability, use values
  greater than 256.

  @param max the new max column size limit; zero means unlimited
}
procedure TZAbstractStatement.SetMaxFieldSize(Value: Integer);
begin
  FMaxFieldSize := Value;
end;

{**
  Retrieves the maximum number of rows that a
  <code>ResultSet</code> object can contain.  If the limit is exceeded, the excess
  rows are silently dropped.

  @return the current max row limit; zero means unlimited
}
function TZAbstractStatement.GetMaxRows: Integer;
begin
  Result := FMaxRows;
end;

{**
  Sets the limit for the maximum number of rows that any
  <code>ResultSet</code> object can contain to the given number.
  If the limit is exceeded, the excess rows are silently dropped.

  @param max the new max rows limit; zero means unlimited
}
procedure TZAbstractStatement.SetMaxRows(Value: Integer);
begin
  FMaxRows := Value;
end;

{**
  Retrieves the number of seconds the driver will
  wait for a <code>Statement</code> object to execute. If the limit is exceeded, a
  <code>SQLException</code> is thrown.

  @return the current query timeout limit in seconds; zero means unlimited
}
function TZAbstractStatement.GetQueryTimeout: Integer;
begin
  Result := FQueryTimeout;
end;

{**
  Sets the number of seconds the driver will
  wait for a <code>Statement</code> object to execute to the given number of seconds.
  If the limit is exceeded, an <code>SQLException</code> is thrown.

  @param seconds the new query timeout limit in seconds; zero means unlimited
}
procedure TZAbstractStatement.SetQueryTimeout(Value: Integer);
begin
  FQueryTimeout := Value;
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZAbstractStatement.Cancel;
begin
  RaiseUnsupportedException;
end;

{**
  Retrieves the first warning reported by calls on this <code>Statement</code> object.
  Subsequent <code>Statement</code> object warnings will be chained to this
  <code>SQLWarning</code> object.

  <p>The warning chain is automatically cleared each time
    a statement is (re)executed.

  <P><B>Note:</B> If you are processing a <code>ResultSet</code> object, any
  warnings associated with reads on that <code>ResultSet</code> object
  will be chained on it.

  @return the first <code>SQLWarning</code> object or <code>null</code>
}
function TZAbstractStatement.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

function TZAbstractStatement.IsClosed: Boolean;
begin
  Result := fClosed;
end;

{**
  Clears all the warnings reported on this <code>Statement</code>
  object. After a call to this method,
  the method <code>getWarnings</code> will return
  <code>null</code> until a new warning is reported for this
  <code>Statement</code> object.
}
procedure TZAbstractStatement.ClearWarnings;
begin
end;

function TZAbstractStatement.GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  SQLTokens: TZTokenList;
  i: Integer;
begin
  if ConSettings^.AutoEncode then begin
    Result := EmptyRaw; //init for FPC
    SQLTokens := GetConnection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]); //Disassembles the Query
    try
      {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := '';
      for i := 0 to SQLTokens.Count-1 do begin //Assembles the Query
   //     {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} + SQLTokens[i].Value;
        case SQLTokens[i].TokenType of
          ttQuoted, ttComment,
          ttWord, ttQuotedIdentifier, ttKeyword:
            ToBuff(ConSettings^.ConvFuncs.ZStringToRaw(SQLTokens.AsString(i),
              ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP), Result);
          else
            ToBuff({$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(SQLTokens.AsString(i)), Result);
        end;
      end;
    finally
      FlushBuff(Result);
      SQLTokens.Free;
    end;
  end else begin
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
    {$IFDEF UNICODE}
    Result := ConSettings^.ConvFuncs.ZUnicodeToRaw(SQL, ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    Result := SQL;
    {$ENDIF}
  end;
end;

function TZAbstractStatement.GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString;
{$IFDEF UNICODE}
begin
  Result := SQL;
{$ELSE}
var
  SQLTokens: TZTokenList;
  i: Integer;
begin
  if ConSettings^.AutoEncode then begin
    Result := ''; //init
    SQLTokens := GetConnection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]); //Disassembles the Query
    try
      for i := 0 to SQLTokens.Count -1 do begin //Assembles the Query
        case (SQLTokens[i].TokenType) of
          ttQuoted, ttComment,
          ttWord, ttQuotedIdentifier, ttKeyword:
            ToBuff(ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings.CTRL_CP), Result);
          else
            ToBuff(ASCII7ToUnicodeString(SQLTokens.AsString(i)), Result);
        end;
      end;
    finally
      FlushBuff(Result);
      SQLTokens.Free;
    end;
  end else
    Result := ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings.CTRL_CP);
{$ENDIF}
end;

function TZAbstractStatement.CreateStmtLogEvent(Category: TZLoggingCategory;
  const Msg: RawByteString = EmptyRaw): TZLoggingEvent;
begin
  if msg <> EmptyRaw
  then result := TZLoggingEvent.Create(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId)+' : '+ Msg, 0, EmptyRaw)
  else result := TZLoggingEvent.Create(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId), 0, EmptyRaw);
end;

function TZAbstractStatement.CreateLogEvent(
  const Category: TZLoggingCategory ): TZLoggingEvent;
begin
  case Category of
    lcPrepStmt, lcExecute:
      result := CreateStmtLogEvent(Category, ASQL);
    lcExecPrepStmt, lcUnprepStmt:
      result := CreateStmtLogEvent(Category);
  else
    result := nil;
  end;
end;

{**
  Defines the SQL cursor name that will be used by
  subsequent <code>Statement</code> object <code>execute</code> methods.
  This name can then be
  used in SQL positioned update/delete statements to identify the
  current row in the <code>ResultSet</code> object generated by this statement.  If
  the database doesn't support positioned update/delete, this
  method is a noop.  To insure that a cursor has the proper isolation
  level to support updates, the cursor's <code>SELECT</code> statement should be
  of the form 'select for update ...'. If the 'for update' phrase is
  omitted, positioned updates may fail.

  <P><B>Note:</B> By definition, positioned update/delete
  execution must be done by a different <code>Statement</code> object than the one
  which generated the <code>ResultSet</code> object being used for positioning. Also,
  cursor names must be unique within a connection.

  @param name the new cursor name, which must be unique within a connection
}
procedure TZAbstractStatement.SetCursorName(const Value: String);
begin
  FCursorName := ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractStatement.Execute(const SQL: ZWideString): Boolean;
begin
  WSQL := SQL;
  Result := Execute(ASQL);
end;

function TZAbstractStatement.Execute(const SQL: RawByteString): Boolean;
begin
  ASQL := SQL;
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  DriverManager.LogMessage(lcExecute,Self);
end;

function TZAbstractStatement.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

{**
  Returns the current result as a <code>ResultSet</code> object.
  This method should be called only once per result.

  @return the current result as a <code>ResultSet</code> object;
  <code>null</code> if the result is an update count or there are no more results
  @see #execute
}
function TZAbstractStatement.GetResultSet: IZResultSet;
begin
  Result := FLastResultSet;
  { does not work as TZGenericTestDbcResultSet.TestLastQuery does expect it! }
  {FLastResultSet := nil;
  FOpenResultSet := Pointer(Result);}
end;

{**
  Returns the current result as an update count;
  if the result is a <code>ResultSet</code> object or there are no more results, -1
  is returned. This method should be called only once per result.

  @return the current result as an update count; -1 if the current result is a
    <code>ResultSet</code> object or there are no more results
  @see #execute
}
function TZAbstractStatement.GetUpdateCount: Integer;
begin
  Result := FLastUpdateCount;
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
function TZAbstractStatement.GetMoreResults: Boolean;
begin
  Result := False;
end;

{**
  Retrieves the direction for fetching rows from
  database tables that is the default for result sets
  generated from this <code>Statement</code> object.
  If this <code>Statement</code> object has not set
  a fetch direction by calling the method <code>setFetchDirection</code>,
  the return value is implementation-specific.

  @return the default fetch direction for result sets generated
    from this <code>Statement</code> object
}
function TZAbstractStatement.GetFetchDirection: TZFetchDirection;
begin
  Result := FFetchDirection;
end;

{**
  Gives the driver a hint as to the direction in which
  the rows in a result set
  will be processed. The hint applies only to result sets created
  using this <code>Statement</code> object.  The default value is
  <code>ResultSet.FETCH_FORWARD</code>.
  <p>Note that this method sets the default fetch direction for
  result sets generated by this <code>Statement</code> object.
  Each result set has its own methods for getting and setting
  its own fetch direction.
  @param direction the initial direction for processing rows
}
procedure TZAbstractStatement.SetFetchDirection(Value: TZFetchDirection);
begin
  FFetchDirection := Value;
end;

{**
  Retrieves the number of result set rows that is the default
  fetch size for result sets
  generated from this <code>Statement</code> object.
  If this <code>Statement</code> object has not set
  a fetch size by calling the method <code>setFetchSize</code>,
  the return value is implementation-specific.
  @return the default fetch size for result sets generated
    from this <code>Statement</code> object
}
function TZAbstractStatement.GetFetchSize: Integer;
begin
  Result := FFetchSize;
end;

{**
  Gives the JDBC driver a hint as to the number of rows that should
  be fetched from the database when more rows are needed.  The number
  of rows specified affects only result sets created using this
  statement. If the value specified is zero, then the hint is ignored.
  The default value is zero.

  @param rows the number of rows to fetch
}
procedure TZAbstractStatement.SetFetchSize(Value: Integer);
begin
  FFetchSize := Value;
end;

{**
  Sets a result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param Concurrency either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
procedure TZAbstractStatement.SetResultSetConcurrency(
  Value: TZResultSetConcurrency);
begin
  FResultSetConcurrency := Value;
end;

{**
  Retrieves the result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @return either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
function TZAbstractStatement.GetResultSetConcurrency: TZResultSetConcurrency;
begin
  Result := FResultSetConcurrency;
end;

{**
  Sets a result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param ResultSetType one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractStatement.SetResultSetType(Value: TZResultSetType);
begin
  FResultSetType := Value;
end;

{**
  Retrieves the result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @return one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
function TZAbstractStatement.GetResultSetType: TZResultSetType;
begin
  Result := FResultSetType;
end;

{**
  Gets the current value for locate updates.
  @returns the current value for locate updates.
}
function TZAbstractStatement.GetLocateUpdates: TZLocateUpdatesMode;
begin
  Result := FLocateUpdates;
end;

{**
  Sets a new value for locate updates.
  @param Value a new value for locate updates.
}
procedure TZAbstractStatement.SetLocateUpdates(Value: TZLocateUpdatesMode);
begin
  FLocateUpdates := Value;
end;

{**
  Gets the current value for post updates.
  @returns the current value for post updates.
}
function TZAbstractStatement.GetPostUpdates: TZPostUpdatesMode;
begin
  Result := FPostUpdates;
end;

{**
  Sets a new value for post updates.
  @param Value a new value for post updates.
}
procedure TZAbstractStatement.SetPostUpdates(Value: TZPostUpdatesMode);
begin
  FPostUpdates := Value;
end;

{**
  Adds an SQL command to the current batch of commmands for this
  <code>Statement</code> object. This method is optional.

  @param sql typically this is a static SQL <code>INSERT</code> or
  <code>UPDATE</code> statement
}
procedure TZAbstractStatement.AddBatch(const SQL: string);
begin
  AddBatchRequest(SQL);
end;

{**
  Adds an SQL command to the current batch of commmands for this
  <code>Statement</code> object. This method is optional.

  @param sql typically this is a static SQL <code>INSERT</code> or
  <code>UPDATE</code> statement
}
procedure TZAbstractStatement.AddBatchRequest(const SQL: string);
begin
  FBatchQueries.Add(SQL);
end;

procedure TZAbstractStatement.AfterClose;
begin

end;

procedure TZAbstractStatement.BeforeClose;
begin
  if Assigned(FLastResultSet) then
    LastResultSet.Close;
  if Assigned(FOpenResultSet) then begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
end;

{**
  Makes the set of commands in the current batch empty.
  This method is optional.
}
procedure TZAbstractStatement.ClearBatch;
begin
  FBatchQueries.Clear;
end;

{**
  Submits a batch of commands to the database for execution and
  if all commands execute successfully, returns an array of update counts.
  The <code>int</code> elements of the array that is returned are ordered
  to correspond to the commands in the batch, which are ordered
  according to the order in which they were added to the batch.
  The elements in the array returned by the method <code>executeBatch</code>
  may be one of the following:
  <OL>
  <LI>A number greater than or equal to zero -- indicates that the
  command was processed successfully and is an update count giving the
  number of rows in the database that were affected by the command's
  execution
  <LI>A value of <code>-2</code> -- indicates that the command was
  processed successfully but that the number of rows affected is
  unknown
  <P>
  If one of the commands in a batch update fails to execute properly,
  this method throws a <code>BatchUpdateException</code>, and a JDBC
  driver may or may not continue to process the remaining commands in
  the batch.  However, the driver's behavior must be consistent with a
  particular DBMS, either always continuing to process commands or never
  continuing to process commands.  If the driver continues processing
  after a failure, the array returned by the method
  <code>BatchUpdateException.getUpdateCounts</code>
  will contain as many elements as there are commands in the batch, and
  at least one of the elements will be the following:
  <P>
  <LI>A value of <code>-3</code> -- indicates that the command failed
  to execute successfully and occurs only if a driver continues to
  process commands after a command fails
  </OL>
  <P>
  A driver is not required to implement this method.
  The possible implementations and return values have been modified in
  the Java 2 SDK, Standard Edition, version 1.3 to
  accommodate the option of continuing to proccess commands in a batch
  update after a <code>BatchUpdateException</code> obejct has been thrown.

  @return an array of update counts containing one element for each
  command in the batch.  The elements of the array are ordered according
  to the order in which commands were added to the batch.
}
function TZAbstractStatement.ExecuteBatch: TIntegerDynArray;
var
  I: Integer;
begin
  SetLength(Result, FBatchQueries.Count);
  for I := 0 to FBatchQueries.Count -1 do
    Result[I] := ExecuteUpdate(FBatchQueries[I]);
  ClearBatch;
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractStatement.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{**
  Gets statement parameters.
  @returns a list with statement parameters.
}
function TZAbstractStatement.GetParameters: TStrings;
begin
  Result := FInfo;
end;

{**
  Returns the ChunkSize for reading/writing large lobs
  @returns the chunksize in bytes.
}
function TZAbstractStatement.GetChunkSize: Integer;
begin
  Result := FChunkSize;
end;

{ TZAbstractPreparedStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FClientVariantManger := Connection.GetClientVariantManager;
  FSupportsDMLBatchArrays := Connection.GetMetadata.GetDatabaseInfo.SupportsArrayBindings;
  {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := SQL;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractPreparedStatement.Destroy;
begin
  Unprepare;
  inherited Destroy;
  ClearParameters;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  WSQL := SQL;
  Result := ExecuteQueryPrepared;
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
function TZAbstractPreparedStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  WSQL := SQL;
  Result := ExecuteUpdatePrepared;
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractPreparedStatement.Execute(const SQL: ZWideString): Boolean;
begin
  WSQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  ASQL := SQL;
  Result := ExecuteQueryPrepared;
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
function TZAbstractPreparedStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  ASQL := SQL;
  Result := ExecuteUpdatePrepared;
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractPreparedStatement.Execute(const SQL: RawByteString): Boolean;
begin
  ASQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Return a VariantManager which supports client encoded RawByteStrings
  @returns IZClientVariantManager
}
function TZAbstractPreparedStatement.GetClientVariantManger: IZClientVariantManager;
begin
  Result := FClientVariantManger;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement.PrepareInParameters;
begin
end;

procedure TZAbstractPreparedStatement.ReleaseImmediat(const Sender: IImmediatelyReleasable;
  var AError: EZSQLConnectionLost);
begin
  FPrepared := False;
  FExecCount := 0;
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Binds the input parameters
}
procedure TZAbstractPreparedStatement.BindInParameters;
begin
  DriverManager.LogMessage(lcBindPrepStmt,Self);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement.UnPrepareInParameters;
begin
end;

procedure TZAbstractPreparedStatement.ValidateArraySizeAndType(const Value: Pointer;
  SQLType: TZSQLType; VariantType: TZVariantType; ParamIndex: Integer);
var Len: ArrayLenInt;
begin
  if Value = nil then Exit;
  case SQLType of
    stUnknown: raise Exception.Create('Invalid SQLType for Array binding!');
    stString: if not (VariantType in [vtString, {$IFNDEF NO_ANSISTRING}vtAnsiString,{$ENDIF}
      {$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF} vtRawByteString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stUnicodeString: if not (VariantType in [vtUnicodeString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stArray, stDataSet:
          raise Exception.Create(sUnsupportedOperation);
  end;
  Len := {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
  if (ParamIndex = 0) then
    FBatchDMLArrayCount := Len
  else if (FBatchDMLArrayCount <> 0) and (Len <> FBatchDMLArrayCount) and (SQLType <> stDataSet) then
    raise Exception.Create('Array count does not equal with initial count!')
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractPreparedStatement.SetInParamCount(NewParamCount: Integer);
var
  I: Integer;
begin
  SetLength(FInParamValues, NewParamCount);
  SetLength(FInParamTypes, NewParamCount);
  SetLength(FInParamDefaultValues, NewParamCount);
  for I := FInParamCount to NewParamCount - 1 do
  begin
    FInParamValues[I] := NullVariant;
    FInParamTypes[I] := stUnknown;

    FInParamDefaultValues[I] := '';
  end;
  FInParamCount := NewParamCount;
end;

{**
  Sets a variant value into specified parameter.
  @param ParameterIndex a index of the parameter.
  @param SqlType a parameter SQL type.
  @paran Value a new parameter value.
}
procedure TZAbstractPreparedStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
begin
  if ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} >= FInParamCount then
    SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FInParamTypes[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := SQLType;
  FInParamValues[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

{**
  Logs a message about prepared statement event with normal result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZAbstractPreparedStatement.LogPrepStmtMessage(Category: TZLoggingCategory;
  const Msg: RawByteString = EmptyRaw);
begin
  if DriverManager.HasLoggingListener then
    if msg <> EmptyRaw then
      DriverManager.LogMessage(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId)+' : '+Msg)
    else
      DriverManager.LogMessage(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId));
end;


function TZAbstractPreparedStatement.GetInParamLogValue(ParamIndex: Integer): RawByteString;
var Value: TZVariant;
begin
  Value := InParamValues[ParamIndex];
  With Value do
    case VType of
      vtNull : result := '(NULL)';
      vtBoolean : if VBoolean then result := '(TRUE)' else result := '(FALSE)';
      vtBytes : Result := GetSQLHexAnsiString(Pointer(VBytes), Length(VBytes), False);
      vtInteger : result := IntToRaw(VInteger);
      vtDouble: Result := FloatToRaw(VDouble);
      vtCurrency: Result := CurrToRaw(Value.VCurrency);
      vtBigDecimal: Result := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(BCDToStr(Value.VBigDecimal));
      vtString,
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString,
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String,
      {$ENDIF}
      vtRawByteString,
      vtUnicodeString,
      vtCharRec: result := #39 + ClientVarManager.GetAsRawByteString(Value) + #39;
      vtDateTime : result := ClientVarManager.GetAsRawByteString(Value);
      vtPointer : result := '(POINTER)';
      vtInterface : result := '(INTERFACE)';
    else
      result := '(UNKNOWN TYPE)'
    end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$IFDEF FPC}
  {$PUSH} {$WARN 5033 off : Function result does not seem to be set} // base class - result not returned intentionally
{$ENDIF}
function TZAbstractPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZAbstractPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
  Result := -1;
end;

{**
  Sets the designated parameter the default SQL value.
  <P><B>Note:</B> You must specify the default value.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the default value normally defined in the field's DML SQL statement
}
procedure TZAbstractPreparedStatement.SetDefaultValue(
  ParameterIndex: Integer; const Value: string);
begin
 if ParameterIndex >= FInParamCount then
   SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FInParamDefaultValues[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractPreparedStatement.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
begin
  SetInParam(ParameterIndex, SQLType, NullVariant);
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  SetInParam(ParameterIndex, stBoolean, EncodeBoolean(Value));
end;

{**
  Sets the designated parameter to a Java <code>byte</code> value.
  The driver converts this
  to an SQL <code>Byte</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  InternalSetOrdinal(ParameterIndex, stByte, Value);
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  InternalSetOrdinal(ParameterIndex, stShort, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  InternalSetOrdinal(ParameterIndex, stWord, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  InternalSetOrdinal(ParameterIndex, stSmall, Value);
end;

{**
  Sets the designated parameter to a Java <code>uint</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  InternalSetOrdinal(ParameterIndex, stLongWord, Value);
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetInt(ParameterIndex: Integer;
  Value: Integer);
begin
  InternalSetOrdinal(ParameterIndex, stInteger, Value);
end;

{**
  Sets the designated parameter to a Java <code>ulong</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  SetInParam(ParameterIndex, stULong, EncodeUInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetLong(ParameterIndex: Integer;
  const Value: Int64);
begin
  InternalSetOrdinal(ParameterIndex, stLong, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  InternalSetDouble(ParameterIndex, stFloat, Value);
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetDouble(ParameterIndex: Integer;
  const Value: Double);
begin
  InternalSetDouble(ParameterIndex, stDouble, Value);
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
begin
  InternalSetDouble(ParameterIndex, stCurrency, Value);
end;

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBigDecimal(
  ParameterIndex: Integer; const Value: TBCD);
begin
  InternalSetDouble(ParameterIndex, stBigDecimal, BcdToDouble(Value));
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetPChar(ParameterIndex: Integer;
   Value: PChar);
begin
  {$IFDEF UNICODE}
  SetUnicodeString(ParameterIndex, Value);
  {$ELSE}
  SetRawByteString(ParameterIndex, Value);
  {$ENDIF}
end;

{**
  Sets the designated parameter to a Java <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  SetInParam(ParameterIndex, stString, EncodeCharRec(Value));
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetString(ParameterIndex: Integer;
   const Value: String);
begin
  SetInParam(ParameterIndex, stString, EncodeString(Value));
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractPreparedStatement.SetAnsiString(ParameterIndex: Integer;
   const Value: AnsiString);
begin
  SetInParam(ParameterIndex, stString, EncodeAnsiString(Value));
end;
{$ENDIF}

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
procedure TZAbstractPreparedStatement.SetUTF8String(ParameterIndex: Integer;
   const Value: UTF8String);
begin
  SetInParam(ParameterIndex, stString, EncodeUTF8String(Value));
end;
{$ENDIF}
{**
  Sets the designated parameter to a Java <code>RawByteString</code> value.
  The driver dosn't converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetRawByteString(ParameterIndex: Integer;
   const Value: RawByteString);
begin
  SetInParam(ParameterIndex, stString, EncodeRawByteString(Value));
end;

{**
  Sets a result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param Concurrency either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
procedure TZAbstractPreparedStatement.SetResultSetConcurrency(
  Value: TZResultSetConcurrency);
begin
  if Value <> FResultSetConcurrency then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetConcurrency(Value);
  end;
end;

{**
  Sets a result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param ResultSetType one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractPreparedStatement.SetResultSetType(Value: TZResultSetType);
begin
  if Value <> FResultSetType then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetType(Value);
  end;
end;

{**
  Sets the designated parameter to a Object Pascal <code>WideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetUnicodeString(ParameterIndex: Integer;
  const Value: ZWideString);
begin
  SetInParam(ParameterIndex, stUnicodeString, EncodeUnicodeString(Value));
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBytes(ParameterIndex: Integer;
  const Value: TBytes);
begin
  SetInParam(ParameterIndex, stBytes, EncodeBytes(Value));
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetGUID(ParameterIndex: Integer; const Value: TGUID);
begin
  SetInParam(ParameterIndex, stGUID, EncodeGUID(Value));
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetDate(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  InternalSetDateTime(ParameterIndex, stDate, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTime(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  InternalSetDateTime(ParameterIndex, stTime, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  InternalSetDateTime(ParameterIndex, stTimestamp, Value);
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large ASCII value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code>. Data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from ASCII to the database char format.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the Java input stream that contains the ASCII parameter value
  @param length the number of bytes in the stream
}
procedure TZAbstractPreparedStatement.SetAsciiStream(
  ParameterIndex: Integer; const Value: TStream);
begin
  if TMemoryStream(Value).Memory = nil
  then SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(PEmptyAnsiString, Value.Size, ConSettings^.ClientCodePage^.CP, ConSettings))
  else if ConSettings^.AutoEncode
    then SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, zCP_NONE, ConSettings))
    else SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, ConSettings^.ClientCodePage^.CP, ConSettings));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large UNICODE value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from UNICODE to the database char format.
  The byte format of the Unicode stream must be Java UTF-8, as
  defined in the Java Virtual Machine Specification.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the UNICODE parameter value
}
procedure TZAbstractPreparedStatement.SetUnicodeStream(
  ParameterIndex: Integer; const Value: TStream);
begin
  if TMemoryStream(Value).Memory = nil
  then SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(PEmptyUnicodeString, Value.Size, ConSettings))
  else SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, zCP_UTF16, ConSettings));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large binary value is input to a <code>LONGVARBINARY</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the binary parameter value
}
procedure TZAbstractPreparedStatement.SetBinaryStream(
  ParameterIndex: Integer; const Value: TStream);
begin
  SetBlob(ParameterIndex, stBinaryStream, TZAbstractBlob.CreateWithStream(Value));
end;

{**
  Sets a blob object for the specified parameter.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @param Value the java blob object.
}
procedure TZAbstractPreparedStatement.SetBlob(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
begin
  if not (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
    raise EZSQLException.Create(SWrongTypeForBlobParameter);
  SetInParam(ParameterIndex, SQLType, EncodeInterface(Value));
end;

{**
  Sets a variant value for the specified parameter.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @param Value the variant value.
}
procedure TZAbstractPreparedStatement.SetValue(ParameterIndex: Integer;
  const Value: TZVariant);
var
  SQLType: TZSQLType;
  TempBlob: IZBlob;
begin
  case Value.VType of
    vtBoolean: SQLType := stBoolean;
    vtInteger: SQLType := stLong;
    vtUInteger: SQLType := stULong;
    vtDouble: SQLType := stDouble;
    vtCurrency: SQLType := stCurrency;
    vtBigDecimal: SQLType := stBigDecimal;
    vtGUID: SQLType := stGUID;
    vtUnicodeString: SQLType := stUnicodeString;
    vtDateTime: SQLType := stTimestamp;
    vtBytes: SQLType := stBytes;
    vtArray: SQLType := TZSQLType(Value.VArray.VArrayType);
    vtInterface:
      if Supports(Value.VInterface, IZBlob, TempBlob) then
        if TempBlob.IsClob then
          SQLType := stAsciiStream
        else
          SQLType := stBinaryStream
      else
        SQLType := stString; //???
  else
    SQLType := stString;
  end;
  SetInParam(ParameterIndex, SQLType, Value);
end;

{**
  Sets the designated parameter to a <code>T???DynArray</code> value.
  The driver converts this to an SQL <code>Array of X</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the TZSQLType
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetNullArray(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull);
begin
  if FSupportsDMLBatchArrays then begin
    if InParamCount < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} then
      raise Exception.Create('Set Array-Value first');
    {$IFNDEF GENERIC_INDEX}
    ParameterIndex := ParameterIndex -1;
    {$ENDIF}
    if InParamValues[ParameterIndex].VType <> vtArray then
      raise Exception.Create('No Array bound before!');
    ValidateArraySizeAndType(Pointer(Value), SQLType, VariantType, ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
    InParamValues[ParameterIndex].VArray.VIsNullArray := Pointer(Value);
    InParamValues[ParameterIndex].VArray.VIsNullArrayType := Ord(SQLType);
    InParamValues[ParameterIndex].VArray.VIsNullArrayVariantType := VariantType;
  end else
    raise EZSQLException.Create(SUnsupportedOperation);
end;

procedure TZAbstractPreparedStatement.InternalSetDouble(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: Double);
begin
  SetInParam(ParameterIndex, SQLType, EncodeDouble(Value));
end;

procedure TZAbstractPreparedStatement.InternalSetOrdinal(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: Int64);
begin
  SetInParam(ParameterIndex, SQLType, EncodeInteger(Value));
end;

procedure TZAbstractPreparedStatement.InternalSetDateTime(
  ParameterIndex: Integer; SQLType: TZSQLType; const Value: TDateTime);
begin
  SetInParam(ParameterIndex, SQLType, EncodeDateTime(Value));
end;

{**
  Sets the designated parameter to a <code>Array of ???</code> value.
  The driver converts this to an SQL <code>Array of </code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param A dynamic array of X.
  @param SQLType the TZSQLType of the value
  @param VariantType the TZVariantType SubType of the value
}
procedure TZAbstractPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
var
  V: TZVariant;
begin
  if FSupportsDMLBatchArrays then begin
    ValidateArraySizeAndType(Pointer(Value), SQLType, VariantType, ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
    V.VType := vtArray;
    V.VArray.VArray := Pointer(Value);
    V.VArray.VArrayVariantType := VariantType;
    V.VArray.VArrayType := Ord(SQLType);
    V.VArray.VIsNullArray := nil;
    V.VArray.VIsNullArrayType := 0;
    V.VArray.VIsNullArrayVariantType := vtNull;
    SetInParam(ParameterIndex, SQLType, V);
  end else
    raise EZSQLException.Create(SUnsupportedOperation);
end;

{**
  Clears the current parameter values immediately.
  <P>In general, parameter values remain in force for repeated use of a
  statement. Setting a parameter value automatically clears its
  previous value.  However, in some cases it is useful to immediately
  release the resources used by the current parameter values; this can
  be done by calling the method <code>clearParameters</code>.
}
procedure TZAbstractPreparedStatement.ClearParameters;
var
  I: Integer;
begin
  for I := FirstDbcIndex to FInParamCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    SetInParam(I, stUnknown, NullVariant);
    SetDefaultValue(I, '');
  end;
  SetInParamCount(0);
  FBatchDMLArrayCount := 0;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractPreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

procedure TZAbstractPreparedStatement.BeforeClose;
begin
  inherited BeforeClose;
  if Prepared then
    Unprepare;
end;

function TZAbstractPreparedStatement.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

procedure TZAbstractPreparedStatement.Prepare;
begin
  DriverManager.LogMessage(lcPrepStmt,Self);
  PrepareInParameters;
  FPrepared := True;
end;

procedure TZAbstractPreparedStatement.Unprepare;
begin
  if Assigned(FOpenResultSet) then begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
  LastResultSet := nil;
  UnPrepareInParameters;
  FPrepared := False;
  FExecCount := 0;
  Self.FBatchDMLArrayCount := 0;
  SetLength(FCachedQueryRaw, 0);
  SetLength(FCachedQueryUni, 0);
end;

function TZAbstractPreparedStatement.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

function TZAbstractPreparedStatement.GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var I: Integer;
begin
  if Length(FCachedQueryRaw) = 0 then begin
    FCachedQueryRaw := ZDbcUtils.TokenizeSQLQueryRaw(SQL, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, @FNCharDetected, GetCompareFirstKeywordStrings, FTokenMatchIndex);
    FParamsCnt := 0;
    Result := EmptyRaw; //init Result
    for I := 0 to High(FCachedQueryRaw) do begin
      ToBuff(FCachedQueryRaw[i], Result);
      Inc(FParamsCnt, Ord(FIsParamIndex[i]));
    end;
    FlushBuff(Result);
  end else
    Result := Inherited GetRawEncodedSQL(SQL);
end;

function TZAbstractPreparedStatement.GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString;
var I: Integer;
begin
  if Length(FCachedQueryUni) = 0 then begin
    FCachedQueryUni := ZDbcUtils.TokenizeSQLQueryUni(SQL, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, @FNCharDetected, GetCompareFirstKeywordStrings, FTokenMatchIndex);

    Result := ''; //init Result
    for I := 0 to High(FCachedQueryUni) do begin
      ToBuff(FCachedQueryUni[i], Result);
      Inc(FParamsCnt, Ord(FIsParamIndex[i]));
    end;
    FlushBuff(Result);
  end else
    Result := inherited GetUnicodeEncodedSQL(SQL);
end;

function TZAbstractPreparedStatement.CreateLogEvent(
  const Category: TZLoggingCategory): TZLoggingEvent;
var
  I : integer;
  LogString : RawByteString;
begin
  case Category of
    lcBindPrepStmt:
        if InParamCount = 0 then
          result := nil
        else begin { Prepare Log Output}
          LogString := '';
          For I := 0 to InParamCount - 1 do begin
            ToBuff(GetInParamLogValue(I), LogString);
            ToBuff(',', LogString);
          end;
          FlushBuff(LogString);
          result := CreateStmtLogEvent(Category, Logstring);
       end;
  else
    result := inherited CreatelogEvent(Category);
  end;
end;

procedure TZAbstractPreparedStatement.SetASQL(const Value: RawByteString);
begin
  if ( ASQL <> Value ) then begin
    SetLength(FCachedQueryRaw, 0);
    SetLength(FCachedQueryUni, 0);
    if Prepared then
      Unprepare;
    inherited SetASQL(Value);
  end;
end;

procedure TZAbstractPreparedStatement.SetWSQL(const Value: ZWideString);
begin
  if ( WSQL <> Value ) then begin
    SetLength(FCachedQueryRaw, 0);
    SetLength(FCachedQueryUni, 0);
    if Prepared then
      Unprepare;
    inherited SetWSQL(Value);
  end;
end;

function TZAbstractPreparedStatement.SupportsSingleColumnArrays: Boolean;
begin
  Result := False;
end;

function TZAbstractPreparedStatement.GetOmitComments: Boolean;
begin
  Result := False;
end;

function TZAbstractPreparedStatement.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := nil;
end;

{ TZAbstractCallableStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractCallableStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FSQL := SQL;
  FOutParamCount := 0;
  SetOutParamCount(0);
  FProcSql := EmptyRaw; //Init -> FPC
  FLastWasNull := True;
  FResultSets := TZCollection.Create;
  FIsFunction := False;
end;

{**
  Close and release a list of returned resultsets.
}
procedure TZAbstractCallableStatement.ClearResultSets;
var
  I: Integer;
  RS: IZResultSet;
begin
  for i := 0 to FResultSets.Count -1 do
    if Supports(FResultSets[i], IZResultSet, RS) then //possible IZUpdateCount e.g. DBLib, ASA
      RS.Close;
  FResultSets.Clear;
  LastResultSet := nil;
end;

{**
   Function remove stUnknown and ptResult, ptOutput paramters from
   InParamTypes and InParamValues because the out-params are added after
   fetching.
}
procedure TZAbstractCallableStatement.TrimInParameters;
var
  I: integer;
  ParamValues: TZVariantDynArray;
  ParamTypes: TZSQLTypeArray;
  ParamCount: Integer;
begin
  ParamCount := 0;
  SetLength(ParamValues, InParamCount);
  SetLength(ParamTypes, InParamCount);

  {Need for dbc access, where no metadata is used to register the ParamTypes}
  if Length(FDBParamTypes) < InParamCount then
    SetLength(FDBParamTypes, InParamCount);
  {end for dbc access}

  for I := 0 to High(InParamTypes) do
  begin
    if ( InParamTypes[I] = ZDbcIntfs.stUnknown ) then
      Continue;
    if (FDBParamTypes[i] in [pctReturn, pctOut]) then
      continue; //EgonHugeist: Ignore known OutParams! else StatmentInparamCount <> expect ProcedureParamCount
    ParamTypes[ParamCount] := InParamTypes[I];
    ParamValues[ParamCount] := InParamValues[I];
    Inc(ParamCount);
  end;
  if ParamCount = InParamCount then
    Exit;
  InParamTypes := ParamTypes;
  InParamValues := ParamValues;
  SetInParamCount(ParamCount); //AVZ
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractCallableStatement.SetOutParamCount(NewParamCount: Integer);
var
  I: Integer;
begin
  SetLength(FOutParamValues, NewParamCount);
  SetLength(FOutParamTypes, NewParamCount);
  for I := FOutParamCount to NewParamCount - 1 do
  begin
    FOutParamValues[I] := NullVariant;
    FOutParamTypes[I] := stUnknown;
  end;
  FOutParamCount := NewParamCount;
end;

{**
  Clears the current parameter values immediately.
  <P>In general, parameter values remain in force for repeated use of a
  statement. Setting a parameter value automatically clears its
  previous value.  However, in some cases it is useful to immediately
  release the resources used by the current parameter values; this can
  be done by calling the method <code>clearParameters</code>.
}
procedure TZAbstractCallableStatement.ClearParameters;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FOutParamCount-1 do
  begin
    OutParamValues[I] := NullVariant;
    OutParamTypes[I] := stUnknown;
  end;
  SetOutParamCount(0);
end;

{**
  Releases this <code>Statement</code> object's database
  and JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.
  It is generally good practice to release resources as soon as
  you are finished with them to avoid tying up database
  resources.
  <P><B>Note:</B> A <code>Statement</code> object is automatically closed when it is
  garbage collected. When a <code>Statement</code> object is closed, its current
  <code>ResultSet</code> object, if one exists, is also closed.
}
procedure TZAbstractCallableStatement.BeforeClose;
begin
  ClearResultSets;
  inherited BeforeClose;
end;


{**
  Do we call a function or a procedure?
  @result Returns <code>True</code> if we call a function
}
function TZAbstractCallableStatement.IsFunction: Boolean;
begin
  Result := FIsFunction;
end;

{**
  Do we have ptInputOutput or ptOutput paramets in a function or procedure?
  @result Returns <code>True</code> if ptInputOutput or ptOutput is available
}
function TZAbstractCallableStatement.HasOutParameter: Boolean;
begin
  Result := FHasOutParameter;
end;

{**
  Get the first resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetFirstResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  Get the previous resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetPreviousResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  Get the next resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetNextResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  Get the last resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetLastResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZAbstractCallableStatement.BOR: Boolean;
begin
  Result := True;
end;

{**
  Last ResultSet?
  @result <code>True</code> if Last ResultSet
}
function TZAbstractCallableStatement.EOR: Boolean;
begin
  Result := True;
end;

{**
  Retrieves a ResultSet by his index.
  @param Index the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // encoding unknown - parameter not used intentionally
function TZAbstractCallableStatement.GetResultSetByIndex(const Index: Integer): IZResultSet;
begin
  Result := nil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZAbstractCallableStatement.GetResultSetCount: Integer;
begin
  Result := 0;
end;

{**
  Registers the OUT parameter in ordinal position
  <code>parameterIndex</code> to the JDBC type
  <code>sqlType</code>.  All OUT parameters must be registered
  before a stored procedure is executed.
  <p>
  The JDBC type specified by <code>sqlType</code> for an OUT
  parameter determines the Java type that must be used
  in the <code>get</code> method to read the value of that parameter.
  <p>
  If the JDBC type expected to be returned to this output parameter
  is specific to this particular database, <code>sqlType</code>
  should be <code>java.sql.Types.OTHER</code>.  The method retrieves the value.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @param sqlType the JDBC type code defined by <code>java.sql.Types</code>.
  If the parameter is of JDBC type <code>NUMERIC</code>
  or <code>DECIMAL</code>, the version of
  <code>registerOutParameter</code> that accepts a scale value should be used.
}
procedure TZAbstractCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  SetOutParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  OutParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TZSQLType(SQLType);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract method - parameters not used intentionally
procedure TZAbstractCallableStatement.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String;
  PrecisionOrSize: LengthInt; Scale: LengthInt);
begin
  if ParamType in [pctOut..pctReturn] then begin
    SetOutParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
    OutParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := SQLType;
  end;

  if ({$IFDEF GENERIC_INDEX}High{$ELSE}Length{$ENDIF}(FDBParamTypes) < ParameterIndex) then
    SetLength(FDBParamTypes, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FDBParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := ParamType;
  if not FIsFunction then FIsFunction := ParamType = pctReturn;
  if not FHasOutParameter then FHasOutParameter := ParamType in [pctOut, pctInOut];
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractCallableStatement.RegisterParamType(ParameterIndex,
  ParamType: Integer);
begin
  if ({$IFDEF GENERIC_INDEX}High{$ELSE}Length{$ENDIF}(FDBParamTypes) < ParameterIndex) then
    SetLength(FDBParamTypes, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FDBParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TZProcedureColumnType(ParamType);
  if not FIsFunction then FIsFunction := ParamType = 4; //ptResult
  if not FHasOutParameter then FHasOutParameter := ParamType in [2,3]; //ptOutput, ptInputOutput
end;

{**
  Gets a output parameter value by it's index.
  @param ParameterIndex a parameter index.
  @returns a parameter value.
}
function TZAbstractCallableStatement.GetOutParam(
  ParameterIndex: Integer): TZVariant;
begin
  if Assigned(OutParamValues) then
  begin
    Result := OutParamValues[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    FLastWasNull := ClientVarManager.IsNull(Result);
  end
  else
  begin
    Result := NullVariant;
    FLastWasNull := True;
  end;
end;

procedure TZAbstractCallableStatement.SetProcSQL(const Value: RawByteString);
begin
  FProcSql := Value;
end;

{**
  Indicates whether or not the last OUT parameter read had the value of
  SQL <code>NULL</code>.  Note that this method should be called only after
  calling a <code>getXXX</code> method; otherwise, there is no value to use in
  determining whether it is <code>null</code> or not.
  @return <code>true</code> if the last parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractCallableStatement.WasNull: Boolean;
begin
  Result := FLastWasNull;
end;

{**
  Indicates whether or not the specified OUT parameter read had the value of
  SQL <code>NULL</code>.
  @return <code>true</code> if the parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractCallableStatement.IsNull(ParameterIndex: Integer): Boolean;
begin
  GetOutParam(ParameterIndex);
  Result := FLastWasNull;
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetPChar(ParameterIndex: Integer): PChar;
begin
  FTemp := GetString(ParameterIndex);
  Result := PChar(FTemp);
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned is ControlsCodePage encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetString(ParameterIndex: Integer): String;
begin
  Result := ClientVarManager.GetAsString(GetOutParam(ParameterIndex));
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>AsniString</code> object
  returned is a Ansi(CP_GETACP) encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
{$IFNDEF NEXTGEN}
function TZAbstractCallableStatement.GetAnsiString(ParameterIndex: Integer): AnsiString;
begin
  Result := ClientVarManager.GetAsAnsiString(GetOutParam(ParameterIndex));
end;
{$ENDIF NEXTGEN}
{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>UTF8String</code> object
  returned is a UTF8 encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractCallableStatement.GetUTF8String(ParameterIndex: Integer): UTF8String;
begin
  Result := ClientVarManager.GetAsUTF8String(GetOutParam(ParameterIndex));
end;
{$ENDIF}

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>RawByteString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetRawByteString(ParameterIndex: Integer): RawByteString;
begin
  Result := ClientVarManager.GetAsRawByteString(GetOutParam(ParameterIndex));
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>WideString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetUnicodeString(
  ParameterIndex: Integer): ZWideString;
begin
  Result := ClientVarManager.GetAsUnicodeString(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>BIT</code> parameter as a <code>boolean</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>false</code>.
}
function TZAbstractCallableStatement.GetBoolean(ParameterIndex: Integer): Boolean;
begin
  Result := SoftvarManager.GetAsBoolean(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>TINYINT</code> parameter as a <code>byte</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetByte(ParameterIndex: Integer): Byte;
begin
  Result := Byte(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>SHORTINT</code> parameter as a <code>short</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetShort(ParameterIndex: Integer): ShortInt;
begin
  Result := ShortInt(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>SMALLINT</code> parameter as a <code>word</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetWord(ParameterIndex: Integer): Word;
begin
  Result := Word(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>SMALLINT</code> parameter as a <code>small</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetSmall(ParameterIndex: Integer): SmallInt;
begin
  Result := SmallInt(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>INTEGER</code> parameter as an <code>uint</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetUInt(ParameterIndex: Integer): Cardinal;
begin
  Result := Cardinal(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>INTEGER</code> parameter as an <code>int</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetInt(ParameterIndex: Integer): Integer;
begin
  Result := Integer(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>ulong</code> parameter as a <code>long</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetULong(ParameterIndex: Integer): UInt64;
begin
  Result := UInt64(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>BIGINT</code> parameter as a <code>long</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetLong(ParameterIndex: Integer): Int64;
begin
  Result := ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>FLOAT</code> parameter as a <code>float</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetFloat(ParameterIndex: Integer): Single;
begin
  Result := ClientVarManager.GetAsDouble(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>DOUBLE</code> parameter as a <code>double</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetDouble(ParameterIndex: Integer): Double;
begin
  Result := ClientVarManager.GetAsDouble(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>CURRENCY</code> parameter as a <code>double</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetCurrency(ParameterIndex: Integer): Currency;
begin
  Result := ClientVarManager.GetAsCurrency(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>NUMERIC</code> parameter as a
  <code>java.math.BigDecimal</code> object with scale digits to
  the right of the decimal point.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result is
  <code>null</code>.
}
procedure TZAbstractCallableStatement.GetBigDecimal(ParameterIndex: Integer; var Result: TBCD);
begin
  Result := ClientVarManager.GetAsBigDecimal(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>BINARY</code> or <code>VARBINARY</code>
  parameter as an array of <code>byte</code> values in the Java
  programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result is
   <code>null</code>.
}
function TZAbstractCallableStatement.GetBytes(ParameterIndex: Integer):
  TBytes;
begin
  Result := ClientVarManager.GetAsBytes(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>DATE</code> parameter as a
  <code>java.sql.Date</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetDate(ParameterIndex: Integer):
  TDateTime;
begin
  Result := ClientVarManager.GetAsDateTime(GetOutParam(ParameterIndex));
end;

{**
  Get the value of a JDBC <code>TIME</code> parameter as a
  <code>java.sql.Time</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetTime(ParameterIndex: Integer):
  TDateTime;
begin
  Result := ClientVarManager.GetAsDateTime(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>TIMESTAMP</code> parameter as a
  <code>java.sql.Timestamp</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetTimestamp(ParameterIndex: Integer):
  TDateTime;
begin
  Result := ClientVarManager.GetAsDateTime(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>Variant</code> parameter value.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>,
  the result is <code>null</code>.
}
function TZAbstractCallableStatement.GetValue(ParameterIndex: Integer):
  TZVariant;
begin
  Result := GetOutParam(ParameterIndex);
end;

{ TZAbstractPreparedCallableStatement }

procedure TZAbstractPreparedCallableStatement.SetProcSQL(const Value: RawByteString);
begin
  if Value <> ProcSQL then Unprepare;
  inherited SetProcSQL(Value);
  if (Value <> EmptyRaw) and ( not Prepared ) then Prepare;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedCallableStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  if (SQL <> Self.WSQL) and (Prepared) then Unprepare;
  WSQL := SQL;
  Result := ExecuteQueryPrepared;
end;

function TZAbstractPreparedCallableStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  if (SQL <> Self.ASQL) and (Prepared) then Unprepare;
  Self.ASQL := SQL;
  Result := ExecuteQueryPrepared;
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
function TZAbstractPreparedCallableStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  if (SQL <> WSQL) and (Prepared) then Unprepare;
  WSQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

function TZAbstractPreparedCallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  if (SQL <> ASQL) and (Prepared) then Unprepare;
  ASQL := SQL;
  Result := ExecuteUpdatePrepared;
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}

function TZAbstractPreparedCallableStatement.Execute(const SQL: ZWideString): Boolean;
begin
  if (SQL <> WSQL) and (Prepared) then Unprepare;
  WSQL := SQL;
  Result := ExecutePrepared;
end;

function TZAbstractPreparedCallableStatement.Execute(const SQL: RawByteString): Boolean;
begin
  if (SQL <> ASQL) and (Prepared) then Unprepare;
  ASQL := SQL;
  Result := ExecutePrepared;
end;

{ TZEmulatedPreparedStatement_A }

function TZEmulatedPreparedStatement_A.ComposeRawSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  TokenizeSQLQueryRaw;

  if Length(FCachedQueryRaw) = 1
  then Result := FCachedQueryRaw[0]
  else begin
    for I := 0 to High(FCachedQueryRaw) do
      if IsParamIndex[i] then begin
        ToBuff(GetParamAsString(ParamIndex), Result);
        Inc(ParamIndex);
      end else
        ToBuff(FCachedQueryRaw[I], Result);
    FlushBuff(Result);
  end;
end;

procedure TZEmulatedPreparedStatement_A.TokenizeSQLQueryRaw;
begin
  if Length(FCachedQueryRaw) = 0 then
    FCachedQueryRaw := ZDbcUtils.TokenizeSQLQueryRaw(
        {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, @FNCharDetected,
      GetCompareFirstKeywordStrings, FTokenMatchIndex);
end;

{ TZEmulatedPreparedStatement_W }

function TZEmulatedPreparedStatement_W.ComposeWideSQLQuery: ZWideString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  TokenizeSQLQueryUni;
  if Length(FCachedQueryUni) = 1
  then Result := FCachedQueryUni[0]
  else begin
    for I := 0 to High(FCachedQueryUni) do
      if FIsParamIndex[i] then begin
        ToBuff(GetParamAsString(ParamIndex), Result);
        Inc(ParamIndex);
      end else
        ToBuff(FCachedQueryUni[I], Result);
    FlushBuff(Result);
  end;
end;

procedure TZEmulatedPreparedStatement_W.TokenizeSQLQueryUni;
begin
  if Length(FCachedQueryUni) = 0 then
    FCachedQueryUni := ZDbcUtils.TokenizeSQLQueryUni(
        {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, @FNCharDetected,
      GetCompareFirstKeywordStrings, FTokenMatchIndex);
end;

{ TZBindList }

function TZBindList.AquireCustomValue(Index: Integer; SQLType: TZSQLType;
  Len: LengthInt): Pointer;
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, SQLType, zbtCustom);
  if (BindValue.Value <> nil) and (PLengthInt(BindValue.Value)^ <> Len) then begin
    FreeMem(BindValue.Value, PLengthInt(BindValue.Value)^+SizeOf(LengthInt));
    BindValue.Value := nil;
  end;
  if BindValue.Value = nil then begin
    GetMem(BindValue.Value, SizeOf(LengthInt)+Len);
    PLengthInt(BindValue.Value)^ := Len;
  end;
  Result := PAnsiChar(BindValue.Value)+SizeOf(LengthInt);
end;

function TZBindList.AquireMinCustomValue(Index: Integer; SQLType: TZSQLType;
  Len: LengthInt): Pointer;
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, SQLType, zbtCustom);
  if (BindValue.Value <> nil) and (PLengthInt(BindValue.Value)^ < Len) then begin
    FreeMem(BindValue.Value, PLengthInt(BindValue.Value)^+SizeOf(LengthInt));
    BindValue.Value := nil;
  end;
  if BindValue.Value = nil then begin
    GetMem(BindValue.Value, SizeOf(LengthInt)+Len);
    PLengthInt(BindValue.Value)^ := Len;
  end;
  Result := PAnsiChar(BindValue.Value)+SizeOf(LengthInt);
end;

procedure TZBindList.BindValuesToStatement(Stmt: TZAbstractPreparedStatement2;
  SupportsBidirectionalParams: Boolean);
var
  i,j: Integer;
  BindValue: PZBindValue;
  IStmt: IZPreparedStatement;
begin
  J := -1;
  Stmt.QueryInterface(IZPreparedStatement, IStmt);
  for i := 0 to FCount -1 do begin
    BindValue := Get(I);
    if not (BindValue.ParamType in [pctOut,pctReturn]) then begin
      if SupportsBidirectionalParams
      then J := i
      else Inc(J);
      case BindValue.BindType of
        zbtNull: IStmt.SetNull(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType);
        zbt4Byte: case BindValue.SQLType of
                    stBoolean:  IStmt.SetBoolean(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^ <> 0);
                    stByte:     IStmt.SetByte(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^);
                    stShort:    IStmt.SetShort(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInteger(@BindValue.Value)^);
                    stWord:     IStmt.SetWord(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^);
                    stSmall:    IStmt.SetSmall(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInteger(@BindValue.Value)^);
                    stLongWord: IStmt.SetUInt(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^);
                    stInteger:  IStmt.SetInt(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInteger(@BindValue.Value)^);
                    stFloat:    IStmt.SetFloat(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PSingle(@BindValue.Value)^);
                    //else RaiseUnsupportedException
                  end;
        zbt8Byte: case BindValue.SQLType of
                    stBoolean:  IStmt.SetBoolean(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^ <> 0);
                    stByte:     IStmt.SetByte(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stShort:    IStmt.SetShort(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stWord:     IStmt.SetWord(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stSmall:    IStmt.SetSmall(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stLongWord: IStmt.SetUInt(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stInteger:  IStmt.SetInt(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stULong:    IStmt.SetULong(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stLong:     IStmt.SetLong(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stFloat:    IStmt.SetFloat(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stDouble:   IStmt.SetDouble(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stCurrency: IStmt.SetCurrency(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCurrency({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stTime:     IStmt.SetTime(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stDate:     IStmt.SetDate(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stTimeStamp:IStmt.SetTimeStamp(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    //else RaiseUnsupportedException
                  end;
        zbtRawString: IStmt.SetRawByteString(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RawByteString(BindValue.Value));
        {$IFNDEF NO_UTF8STRING}
        zbtUTF8String: IStmt.SetUTF8String(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, UTF8String(BindValue.Value));
        {$ENDIF}
        {$IFNDEF NO_ANSISTRING}
        zbtAnsiString: IStmt.SetAnsiString(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, AnsiString(BindValue.Value));
        {$ENDIF}
        zbtUniString: IStmt.SetUnicodeString(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, ZWideString(BindValue.Value));
        zbtCharByRef: IStmt.SetCharRec(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZCharRec(BindValue.Value)^);
        //zbtBinByRef:  IStmt.BindBinary(J, BindValue.SQLType, PZBufRec(BindValue.Value).Buf, PZBufRec(BindValue.Value).Len);
        zbtGUID:      IStmt.SetGUID(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGUID(BindValue.Value)^);
        zbtBytes:     IStmt.SetBytes(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TBytes(BindValue.Value));
        zbtArray,
        zbtRefArray:  begin
                        IStmt.SetDataArray(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZArray(BindValue.Value).VArray,
                          TZSQLType(PZArray(BindValue.Value).VArrayType), PZArray(BindValue.Value).VArrayVariantType);
                        if PZArray(BindValue.Value).VIsNullArray <> nil then
                          IStmt.SetNullArray(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TZSQLType(PZArray(BindValue.Value).VIsNullArrayType),
                            PZArray(BindValue.Value).VIsNullArray, PZArray(BindValue.Value).VIsNullArrayVariantType);
                      end;
        zbtLob:       IStmt.SetBlob(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType, IZBlob(BindValue.Value));
        zbtPointer:   IStmt.SetBoolean(J{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.Value <> nil);
        //zbtBCD, zbtTimeStamp:;
      end;
    end;
  end;
end;

procedure TZBindList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TZBindList.ClearValue(Index: Integer);
var BindValue: PZBindValue;
begin
  BindValue := Get(Index);
  if BindValue.Value <> nil then begin
    if (TZBindTypeSize[BindValue.BindType] = 0) then
      case BindValue.BindType of
        zbtRawString,
        zbtUTF8String
        {$IFNDEF NO_ANSISTRING}
        ,zbtAnsiString{$ENDIF}: RawByteString(BindValue.Value) := ''; //dec refcnt
        zbtUniString: ZWideString(BindValue.Value) := '';
        zbtBytes:     TBytes(BindValue.Value) := nil;
        zbtLob:       IZBlob(BindValue.Value) := nil;
        zbtCustom:    begin
                        FreeMem(BindValue.Value, PLengthInt(BindValue.Value)^+SizeOf(LengthInt));
                        BindValue.Value := nil;
                      end;
        else          BindValue.Value := nil;
      end
    else begin
      if BindValue.BindType = zbtRefArray then begin
        if PZArray(BindValue.Value).vArray <> nil then
          ZDbcUtils.DeReferenceArray(PZArray(BindValue.Value).vArray,
            TZSQLType(PZArray(BindValue.Value).VArrayType), PZArray(BindValue.Value).VArrayVariantType);
        if PZArray(BindValue.Value).VIsNullArray <> nil then
          ZDbcUtils.DeReferenceArray(PZArray(BindValue.Value)^.VIsNullArray,
            TZSQLType(PZArray(BindValue.Value).VIsNullArrayType), PZArray(BindValue.Value).VIsNullArrayVariantType);
      end;
      FreeMem(BindValue.Value, TZBindTypeSize[BindValue.BindType]);
      BindValue.Value := nil;
    end;
  end;
  BindValue.BindType := zbtNull;
end;

procedure TZBindList.ClearValues;
var I: Integer;
begin
  for I := 0 to FCount -1 do
    ClearValue(I);
end;

constructor TZBindList.Create(ConSettings: PZConSettings);
begin
  inherited Create;
  FConSettings := ConSettings;
end;

procedure TZBindList.Delete(Index: Integer);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  ClearValue(Index);
  Dec(FCount);
  {$R-}
  if Index < FCount then
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FValues^[Index + 1], FValues^[Index],
      (FCount - Index) * SizeOf(TZBindValue));
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

destructor TZBindList.Destroy;
begin
  Clear;
  inherited;
end;

{$IFNDEF DISABLE_CHECKING}
class procedure TZBindList.Error(const Msg: string; Data: Integer);
begin
  {$IFDEF FPC}
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
  {$ELSE}
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
  {$ENDIF}
end;
{$ENDIF}

procedure TZBindList.FlushAll;
var I: Integer;
begin
  for i := 0 to FCount -1 do
    ClearValue(I);
end;

function TZBindList.Get(Index: Integer): PZBindValue;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  Result := @FValues^[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.Get4Byte(Index: Integer): P4Bytes;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  if FValues^[Index].BindType = zbt4Byte
  then Result := @FValues^[Index].Value
  else raise EZSQLException.Create(SUnsupportedDataType);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.Get8Byte(Index: Integer): P8Bytes;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  if FValues^[Index].BindType = zbt8Byte
  then Result := {$IFDEF CPU64}@{$ENDIF}FValues^[Index].Value
  else raise EZSQLException.Create(SUnsupportedDataType);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetArray(Index: Integer): PZArray;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  if FValues^[Index].BindType = zbtArray
  then Result := FValues^[Index].Value
  else raise EZSQLException.Create(SUnsupportedDataType);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetBindType(Index: Integer): TZBindType;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  Result := FValues^[Index].BindType
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetSQLType(Index: Integer): TZSQLType;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  Result := FValues^[Index].SQLType
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetType(Index: Integer): TZProcedureColumnType;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  {$R-}
  Result := FValues^[Index].ParamType
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetVariant(Index: Integer): TZVariant;
var BindValue: PZBindValue;
begin
  BindValue := Get(Index);
  if (BindValue.Value = nil) then
    Result := NullVariant
  else case BindValue.BindType of
    zbt4Byte: case BindValue.SQLType of
                stBoolean:
                  Result := EncodeBoolean(BindValue.Value <> nil);
                stByte, stWord, stLongWord:
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
                  Result := EncodeUInteger(PCardinal(@BindValue.Value)^);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
                stShort, stSmall, stInteger:
                  Result := EncodeInteger(PInteger(@BindValue.Value)^);
                stFloat:
                  Result := EncodeDouble(PSingle(@BindValue.Value)^);
                else Result := NullVariant
              end;
    zbt8Byte: case BindValue.SQLType of
                stBoolean:
                  Result := EncodeBoolean(PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^ <> 0);
                stByte, stWord, stLongWord, stULong:
                  Result := EncodeUInteger(PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                stShort, stSmall, stInteger, stLong:
                  Result := EncodeInteger(PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                stCurrency:
                  Result := EncodeCurrency(PCurrency({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                stFloat, stDouble:
                  Result := EncodeDouble(PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                else
                  Result := EncodeDateTime(PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
              end;
    zbtRawString: Result := EncodeRawByteString(RawByteString(BindValue.Value));
    {$IFNDEF NO_UTF8STRING}
    zbtUTF8String:Result := EncodeUTF8String(UTF8String(BindValue.Value));
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    zbtAnsiString:Result := EncodeAnsiString(AnsiString(BindValue.Value));
    {$ENDIF}
    zbtBCD:       Result := EncodeBigDecimal(PBCD(BindValue.Value)^);
    zbtUniString: Result := EncodeUnicodeString(ZWideString(BindValue.Value));
    zbtCharByRef: Result := EncodeCharRec(PZCharRec(BindValue.Value)^);
    zbtBinByRef:  Result := EncodeBytes(BufferToBytes(PZBufRec(BindValue.Value)^.Buf, PZBufRec(BindValue.Value)^.Len));
    zbtGUID:      Result := EncodeGUID(PGUID(BindValue.Value)^);
    zbtBytes:     Result := EncodeBytes(TBytes(BindValue.Value));
    zbtArray:     Result := EncodeArray(PZArray(BindValue.Value)^);
    zbtLob:       if BindValue.Value <> nil
                  then Result := EncodeInterface(IZBlob(BindValue.Value))
                  else Result := NullVariant;
    zbtPointer:   if BindValue.SQLType = stBoolean
                  then Result := EncodeBoolean(BindValue.Value <> nil)
                  else Result := EncodePointer(BindValue.Value);
    zbtNull:      Result := NullVariant;
  end;
end;

procedure TZBindList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TZBindList.HasOutParams: Boolean;
var I: Integer;
begin
  Result := False;
  for i := 0 to FCount -1 do
    if FValues^[I].ParamType in [pctOut..pctReturn] then begin
      Result := True;
      Break;
    end;
end;

function TZBindList.AquireBuffer(Index: Integer; SQLType: TZSQLType;
  BindType: TZBindType): PZBindValue;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index > High(Word)) then
    Error(SListIndexError, Index);
  {$ENDIF}
  while (Index+1 > FCapacity) do
    Grow;
  {$R-}
  if (FValues^[Index].BindType <> zbtNull) and (FValues^[Index].BindType <> BindType) then
    ClearValue(Index);
  if Index+1 > FCount then
    FCount := Index+1;
  Result := @FValues^[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  Result.SQLType := SQLType;
  Result.BindType := BindType;
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType;
  Buf: Pointer; Len: LengthInt);
var BindValue: PZBindValue;
begin
  if Buf = nil then
    raise EZSQLException.Create(SBindingFailure);
  if (SQLType = stGUID) then begin
    BindValue := AquireBuffer(Index, SQLType, zbtGUID);
    if BindValue.Value = nil then
      GetMem(BindValue.Value, SizeOf(TGUID));
    PGUID(BindValue.Value)^ := PGUID(Buf)^;
  end else begin
    BindValue := AquireBuffer(Index, SQLType, zbtBinByRef);
    if BindValue.Value = nil then
      GetMem(BindValue.Value, SizeOf(TZBufRec));
    PZBufRec(BindValue.Value).Buf := Buf;
    PZBufRec(BindValue.Value).Len := Len;
  end;
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType;
  const Value: ZWideString);
begin
  ZWideString(AquireBuffer(Index, SQLType, zbtUniString).Value) := Value;
end;

procedure TZBindList.Put(Index: Integer; const Value: TZArray; AddArrayRef: Boolean);
var BindValue: PZBindValue;
begin
  if AddArrayRef then begin
    BindValue := AquireBuffer(Index, stArray, zbtRefArray);
    if (BindValue.Value <> nil) then begin
      if (Value.VArray <> nil) and (PZArray(BindValue.Value)^.VArray <> nil) then
        DeReferenceArray(PZArray(BindValue.Value)^.VArray, TZSQLType(PZArray(BindValue.Value)^.VArrayType), PZArray(BindValue.Value)^.VArrayVariantType);
      if (Value.VIsNullArray <> nil) and (PZArray(BindValue.Value)^.VIsNullArray <> nil) then
        DeReferenceArray(PZArray(BindValue.Value)^.VIsNullArray, TZSQLType(PZArray(BindValue.Value)^.VIsNullArrayType), PZArray(BindValue.Value)^.VIsNullArrayVariantType);
    end;
  end else
    BindValue := AquireBuffer(Index, stArray, zbtArray);
  if BindValue.Value = nil then begin
    GetMem(BindValue.Value, SizeOf(TZArray));
    PZArray(BindValue.Value)^.VArray := nil;
    PZArray(BindValue.Value)^.VIsNullArray := nil;
  end;
  PZArray(BindValue.Value)^ := Value;
  if AddArrayRef then begin
    if Value.vArray <> nil then
      ZDbcUtils.ReferenceArray(Value.VArray, PZArray(BindValue.Value)^.vArray,
        TZSQLType(Value.VArrayType), Value.VArrayVariantType);
    if Value.VIsNullArray <> nil then
      ZDbcUtils.ReferenceArray(Value.VIsNullArray, PZArray(BindValue.Value)^.VIsNullArray,
        TZSQLType(Value.VIsNullArrayType), Value.VIsNullArrayVariantType);
  end;
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType;
  const Value: IZBLob);
begin
  IZBLob(AquireBuffer(Index, SQLType, zbtLob).Value) := Value;
end;

procedure TZBindList.Put(Index: Integer; const Value: TZTimeStamp);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stArray, zbtBCD);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TZTimeStamp));
  PZTimeStamp(BindValue.Value)^ := Value;
end;

procedure TZBindList.Put(Index: Integer; const Value: TBCD);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stArray, zbtBCD);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TBCD));
  PBCD(BindValue.Value)^ := Value;
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType;
  const Value: RawByteString; CP: Word);
var BindValue: PZBindValue;
begin
  {$IFNDEF NO_UTF8STRING}
  if CP = zCP_UTF8 then
    BindValue := AquireBuffer(Index, SQLType, zbtUTF8String)
  else {$IFNDEF NO_ANSISTRING}if CP = FConSettings.ClientCodePage^.CP then{$ENDIF}
  {$ENDIF}
    BindValue := AquireBuffer(Index, SQLType, zbtRawString)
  {$IFNDEF NO_ANSISTRING}
  else
    BindValue := AquireBuffer(Index, SQLType, zbtAnsiString);
  {$ELSE}
  ;
  {$ENDIF}

  {note: usually !if! we've a codepage aware str we could check and fix the
    codepage. but if we do this we need a unique str. So copy by ref is faster
    and we localize the codepage with the enum }
  RawByteString(BindValue.Value) := Value;
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType; const Value: TBytes);
begin
  if Pointer(Value) = nil then
    raise EZSQLException.Create(SBindingFailure);
  TBytes(AquireBuffer(Index, SQLType, zbtBytes).Value) := Value; //inc refcount
end;

procedure TZBindList.Put(Index: Integer; Value: Boolean);
begin
  AquireBuffer(Index, stBoolean, zbtPointer).Value := Pointer(Ord(Value));
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType; _8Byte: P8Bytes);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, SQLType, zbt8Byte);
  {$IFDEF CPU64}
  PInt64(@BindValue.Value)^ := _8Byte^;
  {$ELSE}
  if BindValue.Value = nil then
    GetMem(BindValue.Value, 8);
  P8Bytes(BindValue.Value)^ := _8Byte^;
  {$ENDIF}
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType; Buf: Pointer;
  Len: LengthInt; CP: Word);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, SQLType, zbtCharByRef);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TZCharRec));
  if Buf <> nil
  then PZCharRec(BindValue.Value).P := Buf
  else if CP = zCP_UTF16
    then PZCharRec(BindValue.Value).P := PEmptyUnicodeString
    else PZCharRec(BindValue.Value).P := PEmptyAnsiString;
  PZCharRec(BindValue.Value).Len := Len;
  PZCharRec(BindValue.Value).CP := CP;
end;

procedure TZBindList.Put(Index: Integer; Value: PZBindValue);
begin
  case Value.BindType of
    zbtNull:      SetNull(Index, Value.SQLType);
    zbt8Byte:     Put(Index, Value.SQLType, P8Bytes(Value.Value));
    zbt4Byte:     Put(Index, Value.SQLType, P4Bytes(Value.Value));
    zbtRawString: Put(Index, Value.SQLType, RawByteString(Value.Value), FConSettings.ClientCodePage.CP);
    zbtUTF8String:Put(Index, Value.SQLType, RawByteString(Value.Value), zCP_UTF8);
    {$IFNDEF NEXTGEN}
    zbtAnsiString:Put(Index, Value.SQLType, RawByteString(Value.Value), zOSCodePage);
    {$ENDIF NEXTGEN}
    zbtUniString: Put(Index, Value.SQLType, ZWideString(Value.Value));
    zbtCharByRef: Put(Index, Value.SQLType, PZCharRec(Value.Value).P, PZCharRec(Value.Value).Len, PZCharRec(Value.Value).CP);
    zbtBinByRef:  Put(Index, Value.SQLType, PZBufRec(Value.Value).Buf, PZBufRec(Value.Value).Len);
    zbtGUID:      Put(Index, stGUID, Value.Value, SizeOf(TGUID));
    zbtBytes:     Put(Index, Value.SQLType, TBytes(Value.Value));
    zbtArray:     Put(Index, PZArray(Value.Value)^, False);
    zbtRefArray:  Put(Index, PZArray(Value.Value)^, True);
    zbtLob:       Put(Index, Value.SQLType, IZBLob(Value.Value));
    zbtPointer:   AquireBuffer(Index, Value.SQLType, Value.BindType).Value := Value.Value;
    zbtBCD:       Put(Index, PBCD(Value.Value)^);
    zbtTimeStamp: Put(Index, PZTimeStamp(Value.Value)^)
  end;
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType; _4Byte: P4Bytes);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, SQLType, zbt4Byte);
  P4Bytes(@BindValue.Value)^ := _4Byte^;
end;

procedure TZBindList.SetCapacity(NewCapacity: Integer);
var
  I: Integer;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (NewCapacity < 0) or (NewCapacity > High(Word)) then
    Error(SListCapacityError, NewCapacity);
  {$ENDIF}
  if NewCapacity < FCount then begin
    for I := FCount - 1 downto NewCapacity do
      ClearValue(I);
    FCount := NewCapacity;
  end;
  {$R-}
  if NewCapacity <> FCapacity then begin
    ReallocMem(FValues, NewCapacity * SizeOf(TZBindValue));
    if NewCapacity > FCapacity then
      FillChar(FValues^[FCapacity], (NewCapacity - FCapacity) * SizeOf(TZBindValue), #0);
    FCapacity := NewCapacity;
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

procedure TZBindList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (NewCount < 0) or (NewCount > High(Word)) then
    Error(SListCountError, NewCount);
  {$ENDIF}
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount < FCount then
    for I := FCount - 1 downto NewCount do
      ClearValue(I);
  FCount := NewCount;
end;

procedure TZBindList.SetNull(Index: Integer; SQLType: TZSQLType);
begin
  AquireBuffer(Index, SQLType, zbtNull);
end;

{ TZAbstractPreparedStatement2 }

{**
  Sets a new parameter capacity and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractPreparedStatement2.SetBindCapacity(Capacity: Integer);
begin
  if (Capacity = 0) or (FBindList.Capacity < Capacity) then
    FBindList.Capacity := Capacity;
end;

function TZAbstractPreparedStatement2.AlignParamterIndex2ResultSetIndex(
  Value: Integer): Integer;
begin
  Result := Value;
  CheckParameterIndex(Value);
  if (FOpenResultSet = nil) or not (BindList.ParamTypes[Value] in [pctInOut..pctReturn])
  then raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  if IZResultSet(FOpenResultSet).IsBeforeFirst then
    IZResultSet(FOpenResultSet).Next;
  {$IFNDEF GENERIC_INDEX}Result := Result+1{$ENDIF};
end;

{**
  Releases this <code>Statement</code> object's database
  and JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.
  It is generally good practice to release resources as soon as
  you are finished with them to avoid tying up database
  resources.
  <P><B>Note:</B> A <code>Statement</code> object is automatically closed when it is
  garbage collected. When a <code>Statement</code> object is closed, its current
  <code>ResultSet</code> object, if one exists, is also closed.
}
procedure TZAbstractPreparedStatement2.BeforeClose;
begin
  inherited BeforeClose;
  if Prepared then
    Unprepare;
end;

procedure TZAbstractPreparedStatement2.BindArray(Index: Integer;
  const Value: TZArray);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, Value, False);
end;

{**
  Binds a binary value
}
procedure TZAbstractPreparedStatement2.BindBinary(Index: Integer;
  SQLType: TZSQLType; Buf: Pointer; Len: LengthInt);
begin
  CheckParameterIndex(Index);
  if SQLType = stBytes
  then FBindList.Put(Index, SQLType, TBytes(Buf))
  else FBindList.Put(Index, SQLtype, Buf, Len);
end;

{**
  Binds a large object value value
}
procedure TZAbstractPreparedStatement2.BindLob(Index: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
begin
  CheckParameterIndex(Index);
  if (Value = nil)
  then FBindList.SetNull(Index, SQLtype)
  else FBindList.Put(Index, SQLtype, Value);
end;

{**
  Binds a TDateTime value
}
procedure TZAbstractPreparedStatement2.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, SQLType, P8Bytes(@Value));
end;

{**
  Binds a double value
}
procedure TZAbstractPreparedStatement2.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, SQLType, P8Bytes(@Value));
end;

{**
  Binds the input parameters
}
procedure TZAbstractPreparedStatement2.BindInParameters;
begin
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
end;

{**
  Binds a signed long
}
procedure TZAbstractPreparedStatement2.BindSignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: Int64);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, SQLType, P8Bytes(@Value));
end;

{**
  Binds a unsigned long
}
procedure TZAbstractPreparedStatement2.BindUnsignedOrdinal(Index: Integer;
  SQLType: TZSQLType; const Value: UInt64);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, SQLType, P8Bytes(@Value));
end;

procedure TZAbstractPreparedStatement2.CheckParameterIndex(Value: Integer);
begin
  if FBindList.Count < Value + 1 then
    SetParamCount(Value+1);
end;

{**
  Clears the current parameter values immediately.
  <P>In general, parameter values remain in force for repeated use of a
  statement. Setting a parameter value automatically clears its
  previous value.  However, in some cases it is useful to immediately
  release the resources used by the current parameter values; this can
  be done by calling the method <code>clearParameters</code>.
}
procedure TZAbstractPreparedStatement2.ClearParameters;
begin
  FBatchDMLArrayCount := 0;
  FBindList.ClearValues;
end;

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractPreparedStatement2.Create(const Connection: IZConnection;
  const SQL: string; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
var iPStmt: IZPreparedStatement;
begin
  inherited Create(Connection, Info);
  if QueryInterface(IZPreparedStatement, iPStmt) = S_OK then begin
    FWeakIntfPtrOfIPrepStmt := Pointer(iPStmt);
    iPStmt := nil;
  end;
  FSupportsDMLBatchArrays := Connection.GetMetadata.GetDatabaseInfo.SupportsArrayBindings;
  FBindList := TZBindList.Create(ConSettings);
  {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := SQL;
end;

function TZAbstractPreparedStatement2.CreateLogEvent(
  const Category: TZLoggingCategory): TZLoggingEvent;
var
  I : integer;
  LogString : RawByteString;
begin
  case Category of
    lcBindPrepStmt:
        if (FBindList.Count=0) then
          result := nil
        else begin { Prepare Log Output}
          LogString := '';
          For I := 0 to FBindList.Count - 1 do begin
            ToBuff(GetInParamLogValue(I), LogString);
            ToBuff(',', LogString);
          end;
          FlushBuff(LogString);
          result := CreateStmtLogEvent(Category, Logstring);
       end;
  else
    result := inherited CreatelogEvent(Category);
  end;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractPreparedStatement2.Destroy;
begin
  inherited Destroy;
  if FBindList <> nil then
    FreeAndNil(FBindList);
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractPreparedStatement2.Execute(const SQL: ZWideString): Boolean;
begin
  WSQL := SQL;
  Result := ExecutePrepared;
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractPreparedStatement2.Execute(
  const SQL: RawByteString): Boolean;
begin
  ASQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractPreparedStatement2.ExecutePrepared: Boolean;
begin
  Result := False;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedStatement2.ExecuteQuery(
  const SQL: ZWideString): IZResultSet;
begin
  WSQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedStatement2.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
begin
  ASQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractPreparedStatement2.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
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
function TZAbstractPreparedStatement2.ExecuteUpdate(
  const SQL: ZWideString): Integer;
begin
  WSQL := SQL;
  Result := ExecuteUpdatePrepared;
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
function TZAbstractPreparedStatement2.ExecuteUpdate(
  const SQL: RawByteString): Integer;
begin
  ASQL := SQL;
  Result := ExecuteUpdatePrepared;
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
function TZAbstractPreparedStatement2.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract base class - parameters not used intentionally

procedure TZAbstractPreparedStatement2.GetBigDecimal(Index: Integer;
  var Result: TBCD);
begin
  IZResultSet(FOpenResultSet).GetBigDecimal(AlignParamterIndex2ResultSetIndex(Index), Result);
  if  not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBigDecimal(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
end;

{**
  Gets the value of a JDBC <code>BINARY</code> or <code>VARBINARY</code>
  parameter as an array of <code>byte</code> values in the Java
  programming language.
  @param Index the first parameter is 1, the second is 2, and so on
  @return the parameter value. If the value is SQL <code>NULL</code>,
  the result is <code>null</code>.
}
procedure TZAbstractPreparedStatement2.GetBytes(Index: Integer;
  out Buf: Pointer; out Len: LengthInt);
begin
  BindList.Put(Index, stBytes, IZResultSet(FOpenResultSet).GetBytes(AlignParamterIndex2ResultSetIndex(Index)));
  Buf := BindList[Index].Value;
  Len := Length(TBytes(BindList[Index].Value));
  if BindList.ParamTypes[Index] = pctInOut then
    BindBinary(Index, BindList.SQLTypes[Index], Buf, Len);
end;

function TZAbstractPreparedStatement2.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := nil;
end;

procedure TZAbstractPreparedStatement2.GetCurrency(Index: Integer;
  out Result: Currency);
begin
  Result := IZResultSet(FOpenResultSet).GetCurrency(AlignParamterIndex2ResultSetIndex(Index));
  if  not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetCurrency(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
end;

procedure TZAbstractPreparedStatement2.GetDateTime(Index: Integer;
  out Result: TDateTime);
begin
  Result := IZResultSet(FOpenResultSet).GetTimestamp(AlignParamterIndex2ResultSetIndex(Index));
  if not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    case BindList.SQLTypes[Index] of
      stTime: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTime(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stDate: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDate(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      else    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTimeStamp(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
    end;
end;

procedure TZAbstractPreparedStatement2.GetDouble(Index: Integer;
  out Result: Double);
begin
  Result := IZResultSet(FOpenResultSet).GetDouble(AlignParamterIndex2ResultSetIndex(Index));
  if not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    if BindList.SQLTypes[Index] = stFloat
    then IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetFloat(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
    else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDouble(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement2.GetInParamLogValue(
  ParamIndex: Integer): RawByteString;
var Value: TZVariant;
begin
  if FBindList.Count = 0 then
    Exit;
  Value := FBindList.Variants[ParamIndex];
  With Value do
    case VType of
      vtNull : result := '(NULL)';
      vtBoolean : if VBoolean then result := '(TRUE)' else result := '(FALSE)';
      vtBytes : Result := GetSQLHexAnsiString(Pointer(VBytes), Length(VBytes), False);
      vtInteger : result := IntToRaw(VInteger);
      vtUInteger : result := IntToRaw(VUInteger);
      vtDouble: Result := FloatToRaw(VDouble);
      vtCurrency: Result := CurrToRaw(VCurrency);
      vtBigDecimal: Result := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(BCDToStr(VBigDecimal));
      {$IFNDEF UNICODE}vtString,{$ENDIF}
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString,
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String,
      {$ENDIF}
      vtRawByteString: Result := Connection.GetEscapeString(VRawByteString);
      {$IFDEF UNICODE}vtString,{$ENDIF}
      vtUnicodeString: Result := Connection.GetEscapeString(ZUnicodeToRaw(VUnicodeString, ConSettings^.ClientCodePage^.CP));
      vtPointer : result := '(POINTER)';
      vtInterface : result := '(INTERFACE)';
      vtArray: Result := '(ARRAY)';
    else
      result := '(UNKNOWN TYPE)'
    end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>lob</code> object
  in the Java programming language.

  @param Index the first parameter is 0, the second is 1, ...
  @return a <code>lob</code> object representing the SQL <code>B/C-LOB</code> value in
    the specified column
}
procedure TZAbstractPreparedStatement2.GetLob(Index: Integer;
  out Result: IZBlob);
begin
  Result := IZResultSet(FOpenResultSet).GetBlob(AlignParamterIndex2ResultSetIndex(Index));
  if BindList.ParamTypes[Index] = pctInOut then
    BindLob(Index, BindList.SQLTypes[Index], Result);
end;

procedure TZAbstractPreparedStatement2.GetOrdinal(Index: Integer;
  out Result: UInt64);
begin
  Result := IZResultSet(FOpenResultSet).GetULong(AlignParamterIndex2ResultSetIndex(Index));
  if not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    case BindList.SQLTypes[Index] of
      stByte: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetByte(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stWord: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetWord(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stLongWord: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUInt(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
    end;
end;

procedure TZAbstractPreparedStatement2.GetPChar(Index: Integer;
  out Buf: Pointer; out Len: LengthInt; CodePage: Word);
var
  L: NativeUInt;
begin
  if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or
      not ConSettings.ClientCodePage.IsStringFieldCPConsistent then begin
    if ConSettings.ClientCodePage.IsStringFieldCPConsistent then begin
      FUniTemp := IZResultSet(FOpenResultSet).GetUnicodeString(AlignParamterIndex2ResultSetIndex(Index));
      if BindList.ParamTypes[Index] = pctInOut then
        SetUnicodeString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FUniTemp);
      Buf := Pointer(FUniTemp);
      Len := Length(FUniTemp);
    end else begin
      Buf := IZResultSet(FOpenResultSet).GetPWideChar(AlignParamterIndex2ResultSetIndex(Index), L);
      System.SetString(FUniTemp, PWideChar(Buf), L);
      if BindList.ParamTypes[Index] = pctInOut then begin
        SetUnicodeString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FUniTemp);
      end;
    end;
    if CodePage = zCP_UTF16 then begin
      Len := Length(FUniTemp);
      if L = 0 then
        Buf := PEmptyUnicodeString;
    end else begin
      FRawTemp := PUnicodeToRaw(Buf, L, CodePage);
      Len := Length(FRawTemp);
      if L = 0
      then Buf := PEmptyAnsiString
      else Buf := Pointer(FRawTemp);
    end;
  end else begin
    Buf := IZResultSet(FOpenResultSet).GetPAnsiChar(AlignParamterIndex2ResultSetIndex(Index), L);
    Len := L;
    if BindList.ParamTypes[Index] = pctInOut then begin
      ZSetString(Buf, l, fRawTemp);
      SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FRawTemp);
    end;
    if CodePage = zCP_UTF16 then begin
      if L = 0 then
        Buf := PEmptyUnicodeString
      else begin
        FUniTemp := PRawToUnicode(Buf, Len, ConSettings^.ClientCodePage.CP);
        Len := Length(FUniTemp);
        if Len = 0
        then Buf := PEmptyUnicodeString
        else Buf := Pointer(FUniTemp);
      end;
    end else if CodePage <> ConSettings^.ClientCodePage.CP then begin
      FUniTemp := PRawToUnicode(Buf, Len, ConSettings^.ClientCodePage.CP);
      Len := Length(FUniTemp);
      if Len = 0 then
        Buf := PEmptyAnsiString
      else begin
        FRawTemp := PUnicodeToRaw(Pointer(FUniTemp), Len, CodePage);
        Len := Length(FRawTemp);
        if Len = 0
        then Buf := PEmptyAnsiString
        else Buf := Pointer(FRawTemp);
      end;
    end;
  end;
end;

procedure TZAbstractPreparedStatement2.GetOrdinal(Index: Integer;
  out Result: Int64);
begin
  Result := IZResultSet(FOpenResultSet).GetLong(AlignParamterIndex2ResultSetIndex(Index));
  if not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    case BindList.SQLTypes[Index] of
      stShort: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetShort(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stSmall: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetSmall(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stInteger: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetInt(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
    end;
end;

{**
  Gets the value of a JDBC <code>BIT</code> parameter as a <code>boolean</code>
  in the Java programming language.
  @param Index the first parameter is 0, the second is 1, and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>false</code>.
}
procedure TZAbstractPreparedStatement2.GetBoolean(Index: Integer;
  out Result: Boolean);
begin
  Result := IZResultSet(FOpenResultSet).GetBoolean(AlignParamterIndex2ResultSetIndex(Index));
  if not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBoolean(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

{**
  get the current SQL string
}
function TZAbstractPreparedStatement2.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

procedure TZAbstractPreparedStatement2.GetTimeStamp(Index: Integer;
  out Result: TZTimeStamp);
begin
  AlignParamterIndex2ResultSetIndex(Index);
  RaiseUnsupportedException
end;

{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Indicates whether or not the specified OUT parameter read had the value of
  SQL <code>NULL</code>.
  @param Index the first parameter is 0, the second is 1, ...
  @return <code>true</code> if the parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractPreparedStatement2.IsNull(Index: Integer): Boolean;
begin
  Result := IZResultSet(FOpenResultSet).IsNull(AlignParamterIndex2ResultSetIndex(Index));
  if not SupportsBidirectionalParams and (BindList.ParamTypes[Index] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNull(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindList.SQLTypes[Index]);
end;

function TZAbstractPreparedStatement2.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

{**
  Logs a message about prepared statement event with normal result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZAbstractPreparedStatement2.LogPrepStmtMessage(
  Category: TZLoggingCategory; const Msg: RawByteString);
begin
  if DriverManager.HasLoggingListener then
    if msg <> ''
    then DriverManager.LogMessage(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId)+' : '+Msg)
    else DriverManager.LogMessage(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId));
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractPreparedStatement2.Prepare;
begin
  DriverManager.LogMessage(lcPrepStmt,Self);
  PrepareInParameters;
  FPrepared := True;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement2.PrepareInParameters;
begin
end;

{**
  Registers the OUT parameter in ordinal position
  <code>parameterIndex</code> to the JDBC type
  <code>sqlType</code>.  All OUT parameters must be registered
  before a stored procedure is executed.
  <p>
  The JDBC type specified by <code>sqlType</code> for an OUT
  parameter determines the Java type that must be used
  in the <code>get</code> method to read the value of that parameter.
  <p>
  If the JDBC type expected to be returned to this output parameter
  is specific to this particular database, <code>sqlType</code>
  should be <code>java.sql.Types.OTHER</code>.  The method retrieves the value.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @param sqlType the JDBC type code defined by <code>java.sql.Types</code>.
  If the parameter is of JDBC type <code>NUMERIC</code>
  or <code>DECIMAL</code>, the version of
  <code>registerOutParameter</code> that accepts a scale value should be used.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract method - parameters not used intentionally
procedure TZAbstractPreparedStatement2.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String = '';
  PrecisionOrSize: LengthInt = 0; Scale: LengthInt = 0);
var BindValue: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  if BindList.Count < ParameterIndex+1 then
    SetParamCount(ParameterIndex+1);
    //BindList.SetCount(ParameterIndex+1);
  BindValue := BindList[ParameterIndex];
  BindValue^.ParamType := ParamType;
  BindValue^.SQLType   := SQLType;
  FHasInOutParams := FHasInOutParams or (ParamType = pctInOut)
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractPreparedStatement2.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FPrepared := False;
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large ASCII value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code>. Data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from ASCII to the database char format.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the Java input stream that contains the ASCII parameter value
  @param length the number of bytes in the stream
}
procedure TZAbstractPreparedStatement2.SetAsciiStream(ParameterIndex: Integer;
  const Value: TStream);
begin
  if TMemoryStream(Value).Memory = nil
  then SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(PEmptyAnsiString, Value.Size, ConSettings^.ClientCodePage^.CP, ConSettings))
  else if ConSettings^.AutoEncode
    then SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, zCP_NONE, ConSettings))
    else SetBlob(ParameterIndex, stAsciiStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, ConSettings^.ClientCodePage^.CP, ConSettings));
end;

procedure TZAbstractPreparedStatement2.SetASQL(const Value: RawByteString);
begin
  if Value <> FASQL then begin
    if Prepared then
      UnPrepare;
    inherited SetASQL(Value);
  end;
end;

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetBigDecimal(ParameterIndex: Integer;
  const Value: TBCD);
begin
  BindDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBigDecimal, BCDToDouble(Value));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large binary value is input to a <code>LONGVARBINARY</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the binary parameter value
}
procedure TZAbstractPreparedStatement2.SetBinaryStream(ParameterIndex: Integer;
  const Value: TStream);
begin
  SetBlob(ParameterIndex, stBinaryStream, TZAbstractBlob.CreateWithStream(Value));
end;

procedure TZAbstractPreparedStatement2.SetBlob(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
begin
  BindLob(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SQLType, Value)
end;

{**
  Sets the designated parameter to a Java <code>unsigned 8Bit int</code> value.
  The driver converts this
  to an SQL <code>BYTE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractPreparedStatement2.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stByte, Value);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetBytes(ParameterIndex: Integer;
  const Value: TBytes);
begin
  BindBinary(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBytes, Pointer(Value), Length(Value));
end;

{**
  Sets the designated parameter to a Java <code>currency</code> value.
  The driver converts this
  to an SQL <code>CURRENCY</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
begin
  BindDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stCurrency, Value);
end;

procedure TZAbstractPreparedStatement2.SetDataArray(Index: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType);
var aArray: TZArray;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  CheckParameterIndex(Index);
  if FSupportsDMLBatchArrays or (BindList.ParamTypes[Index] = pctResultSet) then begin
    ValidateArraySizeAndType(Pointer(Value), SQLType, VariantType, Index);
    aArray.VArray := Pointer(Value);
    aArray.VArrayVariantType := VariantType;
    aArray.VArrayType := Ord(SQLType);
    aArray.VIsNullArray := nil;
    aArray.VIsNullArrayType := 0;
    aArray.VIsNullArrayVariantType := vtNull;
    BindArray(Index, aArray);
  end else
    raise EZSQLException.Create(SUnsupportedOperation);
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetDate(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  BindDateTime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stDate, Value);
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetDouble(ParameterIndex: Integer;
  const Value: Double);
begin
  BindDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stDouble, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  BindDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stFloat, Value);
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetGUID(ParameterIndex: Integer;
  const Value: TGUID);
begin
  BindBinary(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stGUID, @Value.D1,
    SizeOf(TGUID));
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractPreparedStatement2.SetParamCount(NewParamCount: Integer);
begin
  if (NewParamCount = 0) or (NewParamCount > FBindList.Capacity) then
    SetBindCapacity(NewParamCount);
  FBindList.SetCount(NewParamCount);
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetInt(ParameterIndex, Value: Integer);
begin
  BindSignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stInteger, Value);
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetLong(ParameterIndex: Integer;
  const Value: Int64);
begin
  BindSignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLong, Value);
end;

procedure TZAbstractPreparedStatement2.SetNullArray(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value; const VariantType: TZVariantType);
var BindValue: PZBindValue;
begin
  if FSupportsDMLBatchArrays then begin
    if (FBindList.Count < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}) then
      raise Exception.Create('Set Array-Value first');
    {$IFNDEF GENERIC_INDEX}
    ParameterIndex := ParameterIndex -1;
    {$ENDIF}
    BindValue := FBindList.Get(PArameterIndex);
    if BindValue.SQLType <> stArray then
      raise Exception.Create('No Array bound before!');
    ValidateArraySizeAndType(Pointer(Value), SQLType, VariantType, ParameterIndex);
    if BindValue.BindType = zbtRefArray then begin
      if PZArray(BindValue.Value).VIsNullArray <> nil then
        DeReferenceArray(PZArray(BindValue.Value).VIsNullArray, TZSQLType(PZArray(BindValue.Value).VIsNullArrayType),
          PZArray(BindValue.Value).VIsNullArrayVariantType);
      ReferenceArray(Pointer(Value), PZArray(BindValue.Value).VIsNullArray, SQLType, VariantType);
    end else
      PZArray(BindValue.Value).VIsNullArray := Pointer(Value);
    PZArray(BindValue.Value).VIsNullArrayType := Ord(SQLType);
    PZArray(BindValue.Value).VIsNullArrayVariantType := VariantType;
  end else
    raise EZSQLException.Create(SUnsupportedOperation);
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetPChar(ParameterIndex: Integer;
  Value: PChar);
begin
  {$IFDEF UNICODE}
  SetUnicodeString(ParameterIndex, Value);
  {$ELSE}
  SetRawByteString(ParameterIndex, Value);
  {$ENDIF}
end;

{**
  Sets a result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param Concurrency either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
procedure TZAbstractPreparedStatement2.SetResultSetConcurrency(
  Value: TZResultSetConcurrency);
begin
  if Value <> FResultSetConcurrency then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetConcurrency(Value);
  end;
end;

{**
  Sets a result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param ResultSetType one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractPreparedStatement2.SetResultSetType(Value: TZResultSetType);
begin
  if Value <> FResultSetType then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetType(Value);
  end;
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  BindSignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stShort, Value);
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  BindSignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stSmall, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetTime(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  BindDateTime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stTime, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetTimestamp(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  BindDateTime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stTimeStamp, Value);
end;

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractPreparedStatement2.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLongWord, Value);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stULong, Value);
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large UNICODE value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from UNICODE to the database char format.
  The byte format of the Unicode stream must be Java UTF-8, as
  defined in the Java Virtual Machine Specification.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the UNICODE parameter value
}
procedure TZAbstractPreparedStatement2.SetUnicodeStream(ParameterIndex: Integer;
  const Value: TStream);
begin
  if TMemoryStream(Value).Memory = nil
  then SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(PEmptyUnicodeString, Value.Size, ConSettings))
  else SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, zCP_UTF16, ConSettings));
end;

procedure TZAbstractPreparedStatement2.SetValue(ParameterIndex: Integer;
  const Value: TZVariant);
var TempBlob: IZBlob;
begin
  case Value.VType of
    vtBoolean: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBoolean(ParameterIndex, Value.VBoolean);
    vtInteger: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetLong(ParameterIndex, Value.VInteger);
    vtUInteger: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetULong(ParameterIndex, Value.VUInteger);
    vtDouble: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDouble(ParameterIndex, Value.VCurrency);
    vtCurrency: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetCurrency(ParameterIndex, Value.VCurrency);
    vtBigDecimal: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBigDecimal(ParameterIndex, Value.VBigDecimal);
    vtUnicodeString: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUnicodeString(ParameterIndex, Value.VUnicodeString);
    vtRawByteString: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetRawByteString(ParameterIndex, Value.VRawByteString);
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString:    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetAnsiString(ParameterIndex, Value.VAnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String:    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUTF8String(ParameterIndex, Value.VUTF8String);
    {$ENDIF}
    vtCharRec:       SetCharRec(ParameterIndex, Value.VCharRec);
    vtDateTime:      IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTimestamp(ParameterIndex, Value.VDateTime);
    vtBytes:         IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBytes(ParameterIndex, Value.VBytes);
    vtArray:  begin
                IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDataArray(ParameterIndex, Value.VArray.VArray, TZSQLType(Value.VArray.VArrayType), Value.VArray.VArrayVariantType);
                if Value.VArray.VIsNullArray <> nil then
                  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNullArray(ParameterIndex, TZSQLType(Value.VArray.VIsNullArrayType), Value.VArray.VIsNullArray, Value.VArray.VIsNullArrayVariantType);
              end;
    vtInterface:
      if Supports(Value.VInterface, IZBlob, TempBlob) then begin
        if TempBlob.IsClob
        then IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBlob(ParameterIndex, stAsciiStream, TempBlob)
        else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBlob(ParameterIndex, stBinaryStream, TempBlob);
        TempBlob := nil;
      end else
        raise EZSQLException.Create(sUnsupportedOperation);
    else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNull(ParameterIndex, stUnknown);
  end;
end;

{**
  Sets the designated parameter to a Java <code>unsigned 16bit int</code> value.
  The driver converts this
  to an SQL <code>WORD</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractPreparedStatement2.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stWord, Value);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZAbstractPreparedStatement2.SetWSQL(const Value: ZWideString);
begin
  if Value <> FWSQL then begin
    if Prepared then
      Unprepare;
    inherited SetWSQL(Value);
  end;
end;

function TZAbstractPreparedStatement2.SupportsBidirectionalParams: Boolean;
begin
  Result := False;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractPreparedStatement2.Unprepare;
var RefCountAdded: Boolean;
begin
  RefCountAdded := (RefCount = 1) and (Assigned(FOpenResultSet) or Assigned(FLastResultSet));
  if RefCountAdded then
    _AddRef;
  UnPrepareInParameters;
  FPrepared := False;
  FHasInOutParams := False;
  FBatchDMLArrayCount := 0;
  { closing the pending resultset automaticaly nils the Interface/Pointer addresses }
  if Assigned(FLastResultSet) then
    FLastResultSet.Close;
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close;
  if RefCountAdded then begin
    if (RefCount = 1) then
      DriverManager.AddGarbage(Self);
    _Release; //possible running into destructor now if just a ResultSet was last owner of Self-interface
  end;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement2.UnPrepareInParameters;
begin
  SetBindCapacity(0);
end;

procedure TZAbstractPreparedStatement2.ValidateArraySizeAndType(
  const Value: Pointer; SQLType: TZSQLType; VariantType: TZVariantType;
  ParamIndex: Integer);
var Len: ArrayLenInt;
begin
  if Value = nil then Exit;
  case SQLType of
    stUnknown: raise Exception.Create('Invalid SQLType for Array binding!');
    stString: if not (VariantType in [vtString,
      {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}
      {$IFNDEF NO_UTF8STRING}vtUTF8String, {$ENDIF}
      vtRawByteString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stUnicodeString: if not (VariantType in [vtUnicodeString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stArray, stDataSet:
          raise Exception.Create(sUnsupportedOperation);
  end;
  Len := {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
  if (BindList.ParamTypes[ParamIndex] <> pctResultSet) then
    if (ParamIndex = 0) then
      FBatchDMLArrayCount := Len
    else if (FBatchDMLArrayCount <> 0) and (Len <> FBatchDMLArrayCount) then
      raise Exception.Create('Array count does not equal with initial count!')
end;

procedure TZBindList.Put(Index: Integer; const Value: TGUID);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stGUID, zbtGUID);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TGUID));
  PGUID(BindValue.Value)^ := Value;
end;

{ TZRawPreparedStatement }

procedure TZRawPreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, stString, Value, ConSettings^.ClientCodePage.CP)
end;

procedure TZRawPreparedStatement.BindLob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
var RawTemp: RawByteString;
begin
  inherited BindLob(Index, SQLType, Value);
  if (Value <> nil) and (SQLType in [stAsciiStream, stUnicodeStream]) then
    if Value.IsClob then begin
      Value.GetPAnsiChar(ConSettings^.ClientCodePage.CP);
      BindList[Index].SQLType := stAsciiStream;
    end else begin
      RawTemp := GetValidatedAnsiStringFromBuffer(Value.GetBuffer, Value.Length, ConSettings);
      inherited BindLob(Index, stAsciiStream, TZAbstractCLob.CreateWithData(Pointer(RawTemp),
        Length(RawTemp), ConSettings^.ClientCodePage.CP, ConSettings));
    end;
end;

procedure TZRawPreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
begin
  CheckParameterIndex(Index);
  if Buf <> nil
  then FBindList.Put(Index, stString, Buf, Len, ConSettings^.ClientCodePage.CP)
  else FBindList.Put(Index, stString, PEmptyAnsiString, 0, ConSettings^.ClientCodePage.CP)
end;

{**
  Sets a new parameter capacity and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZRawPreparedStatement.SetBindCapacity(Capacity: Integer);
begin
  inherited SetBindCapacity(Capacity);
  if Length(FInParamDefaultValues) <> BindList.Capacity then
    SetLength(FInParamDefaultValues, BindList.Capacity);
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NEXTGEN}
procedure TZRawPreparedStatement.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  if ZCompatibleCodePages(ZOSCodePage, ConSettings^.ClientcodePage.CP)
  then BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value)
  else BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
    ConSettings^.ConvFuncs.ZAnsiToRaw(Value, ConSettings.ClientcodePage.CP));
end;
{$ENDIF}

{**
  Sets the designated parameter to a Java <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZRawPreparedStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
var UniTemp: ZWideString;
begin
  if ZCompatibleCodePages(Value.CP,ConSettings^.ClientcodePage.CP) then
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value.P, Value.Len)
  else if Value.CP = zCP_UTF16 then
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
      PUnicodeToRaw(Value.P, Value.Len, ConSettings^.ClientcodePage.CP))
  else begin
    UniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
      ZUnicodeToRaw(UniTemp, ConSettings^.ClientcodePage.CP))
  end;
end;

{**
  Sets the designated parameter the default SQL value.
  <P><B>Note:</B> You must specify the default value.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the default value normally defined in the field's DML SQL statement
}
procedure TZRawPreparedStatement.SetDefaultValue(ParameterIndex: Integer;
  const Value: string);
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex -1;
  {$ENDIF}
  CheckParameterIndex(ParameterIndex);
  FInParamDefaultValues[ParameterIndex] := ConSettings^.ConvFuncs.ZStringToRaw(
    Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage.CP);
end;

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
procedure TZRawPreparedStatement.SetRawByteString(
  ParameterIndex: Integer; const Value: RawByteString);
begin
  BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value);
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZRawPreparedStatement.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ZUnicodetoRaw(Value, ConSettings.ClientcodePage.CP));
  {$ELSE}
  BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ConSettings^.ConvFuncs.ZStringtoRaw(Value, ConSettings^.Ctrl_CP,
    ConSettings.ClientcodePage.CP));
  {$ENDIF}
end;

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
procedure TZRawPreparedStatement.SetUnicodeString(
  ParameterIndex: Integer; const Value: ZWideString);
begin
  BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ZUnicodetoRaw(Value, ConSettings.ClientcodePage.CP));
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
procedure TZRawPreparedStatement.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  if ZCompatibleCodepages(zCP_UTF8, ConSettings^.ClientCodePage.CP)
  then BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value)
  else BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ConSettings^.ConvFuncs.ZUTF8ToRaw(Value, ConSettings.ClientcodePage.CP));
end;
{$ENDIF}

{ TZUTF16PreparedStatement }

procedure TZUTF16PreparedStatement.BindUniStr(Index: Integer;
  const Value: ZWideString);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, stUnicodeString, Value);
end;

procedure TZUTF16PreparedStatement.BindUniStr(Index: Integer;
  Buf: PWideChar; CodePoints: LengthInt);
begin
  FBindList.Put(Index, stUnicodeString, Buf, CodePoints, zCP_UTF16);
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZUTF16PreparedStatement.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    PRawToUnicode(Pointer(Value), Length(Value), zOSCodePage))
end;
{$ENDIF NO_ANSISTRING}

procedure TZUTF16PreparedStatement.SetBindCapacity(Capacity: Integer);
begin
  inherited SetBindCapacity(Capacity);
  if Length(FInParamDefaultValues) <> BindList.Capacity then
    SetLength(FInParamDefaultValues, BindList.Capacity);
end;


{**
  Sets the designated parameter to a Java <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZUTF16PreparedStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  if ZCompatibleCodePages(Value.CP, zCP_UTF16) then
    BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value.P, Value.Len)
  else
    BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, PRawToUnicode(Value.P, Value.Len, Value.CP))
end;

{**
  Sets the designated parameter the default SQL value.
  <P><B>Note:</B> You must specify the default value.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the default value normally defined in the field's DML SQL statement
}
procedure TZUTF16PreparedStatement.SetDefaultValue(ParameterIndex: Integer;
  const Value: string);
begin
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex -1;
  {$ENDIF}
  CheckParameterIndex(ParameterIndex);
  {$IFDEF UNICODE}
  FInParamDefaultValues[ParameterIndex] := Value;
  {$ELSE}
  FInParamDefaultValues[ParameterIndex] := ConSettings^.ConvFuncs.ZStringToUnicode(
    Value, ConSettings^.CTRL_CP);
  {$ENDIF}
end;

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
procedure TZUTF16PreparedStatement.SetRawByteString(
  ParameterIndex: Integer; const Value: RawByteString);
begin
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ZRawToUnicode(Value, ConSettings^.ClientCodePage.CP));
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZUTF16PreparedStatement.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value);
  {$ELSE}
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ConSettings^.ConvFuncs.ZStringToUnicode(Value, ConSettings^.CTRL_CP));
  {$ENDIF}
end;

{**
  Sets the designated parameter to a Object Pascal <code>WideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZUTF16PreparedStatement.SetUnicodeString(
  ParameterIndex: Integer; const Value: ZWideString);
begin
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value);
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
procedure TZUTF16PreparedStatement.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    PRawToUnicode(Pointer(Value), Length(Value), zCP_UTF8))
end;
{$ENDIF}

{ TZRawParamDetectPreparedStatement }
function TZRawParamDetectPreparedStatement.GetRawEncodedSQL(const SQL:
  {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var I: Integer;
begin
  if Length(FCachedQueryRaw) = 0 then begin
    FCachedQueryRaw := ZDbcUtils.TokenizeSQLQueryRaw(SQL, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected, GetCompareFirstKeywordStrings, FTokenMatchIndex);
    FCountOfQueryParams := 0;
    Result := ''; //init Result
    for I := 0 to High(FCachedQueryRaw) do begin
      ToBuff(FCachedQueryRaw[i], Result);
      Inc(FCountOfQueryParams, Ord(FIsParamIndex[i]));
    end;
    FlushBuff(Result);
    SetBindCapacity(FCountOfQueryParams);
  end else
    Result := Inherited GetRawEncodedSQL(SQL);
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZRawParamDetectPreparedStatement.Unprepare;
begin
  inherited Unprepare;
  SetLength(FCachedQueryRaw, 0);
end;

{ TZUTF16ParamDetectPreparedStatement }

function TZUTF16ParamDetectPreparedStatement.GetUnicodeEncodedSQL(const SQL:
  {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString;
var I: Integer;
begin
  if Length(FCachedQueryUni) = 0 then begin
    FCachedQueryUni := ZDbcUtils.TokenizeSQLQueryUni(SQL, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected, GetCompareFirstKeywordStrings, FTokenMatchIndex);
    FCountOfQueryParams := 0;
    Result := ''; //init Result
    for I := 0 to High(FCachedQueryUni) do begin
      ToBuff(FCachedQueryUni[i], Result);
      Inc(FCountOfQueryParams, Ord(FIsParamIndex[i]));
    end;
    FlushBuff(Result);
    if FCountOfQueryParams > 0 then
      FBindList.SetCapacity(FCountOfQueryParams);
  end else
    Result := inherited GetUnicodeEncodedSQL(SQL);
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZUTF16ParamDetectPreparedStatement.Unprepare;
begin
  inherited UnPrepare;
  SetLength(FCachedQueryUni, 0);
end;

{ TZAbstractCallableStatement2 }

{**
  Binds the input parameters
}
procedure TZAbstractCallableStatement2.BindInParameters;
begin
  Bindlist.BindValuesToStatement(FExecStatements[FCallExecKind], SupportsBidirectionalParams);
  if (FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)] <> FExecStatements[FCallExecKind]) and
     (FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)] <> nil) then
    Bindlist.BindValuesToStatement(FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)], SupportsBidirectionalParams);
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZAbstractCallableStatement2.BOR: Boolean;
begin
  Result := False;
end;

constructor TZAbstractCallableStatement2.Create(const Connection: IZConnection;
  const StoredProcOrFuncIdentifier: string; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
begin
  inherited Create(Connection, '', Info);
  FStoredProcName := StoredProcOrFuncIdentifier;
end;

{**
  Last ResultSet?
  @result <code>True</code> if so
}
function TZAbstractCallableStatement2.EOR: Boolean;
begin
  Result := False;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract base class - parameters not used intentionally

function TZAbstractCallableStatement2.Execute(const SQL: ZWideString): Boolean;
begin
  Result := False;
  RaiseUnsupportedException;
end;

function TZAbstractCallableStatement2.Execute(
  const SQL: RawByteString): Boolean;
begin
  Result := False;
  RaiseUnsupportedException;
end;

function TZAbstractCallableStatement2.ExecutePrepared: Boolean;
begin
  FCallExecKind := zcekSelect;
  Prepare;
  BindInParameters;
  Result := FExecStatements[FCallExecKind].ExecutePrepared;
end;

function TZAbstractCallableStatement2.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

function TZAbstractCallableStatement2.ExecuteQuery(
  const SQL: ZWideString): IZResultSet;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

function TZAbstractCallableStatement2.ExecuteQueryPrepared: IZResultSet;
begin
  FCallExecKind := zcekSelect;
  Prepare;
  BindInParameters;
  Result := FExecStatements[FCallExecKind].ExecuteQueryPrepared;
end;

function TZAbstractCallableStatement2.ExecuteUpdate(
  const SQL: RawByteString): Integer;
begin
  Result := -1;
  RaiseUnsupportedException;
end;

function TZAbstractCallableStatement2.ExecuteUpdate(
  const SQL: ZWideString): Integer;
begin
  Result := -1;
  RaiseUnsupportedException;
end;

function TZAbstractCallableStatement2.ExecuteUpdatePrepared: Integer;
begin
  FCallExecKind := zcekParams;
  Prepare;
  BindInParameters;
  Result := FExecStatements[FCallExecKind].ExecuteUpdatePrepared;
end;

function TZAbstractCallableStatement2.GetBigDecimal(
  ParameterIndex: Integer): Extended;
var D: Double;
begin
  GetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, D);
  Result := D;
end;

procedure TZAbstractCallableStatement2.GetBigDecimal(Index: Integer;
  var Result: TBCD);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetBigDecimal(Index{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, Result);
  end else begin
    Result := NullBCD; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

procedure TZAbstractCallableStatement2.GetBoolean(Index: Integer;
  out Result: Boolean);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetBoolean(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, Result)
  end else begin
    Result := False; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement2.GetBoolean(
  ParameterIndex: Integer): Boolean;
begin
  GetBoolean(ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractCallableStatement2.GetByte(ParameterIndex: Integer): Byte;
var U: UInt64;
begin
  GetOrdinal(ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}, U);
  Result := Byte(U);
end;

procedure TZAbstractCallableStatement2.GetBytes(Index: Integer;
  out Buf: Pointer; out Len: LengthInt);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetBytes(Index, Buf, Len);
    if (BindList.ParamTypes[Index] = pctInOut) then
      inherited BindBinary(Index, FExecStatements[FCallExecKind].BindList.SQLTypes[Index], Buf, Len);
  end else begin
    Buf := nil; //satisfy compiler
    Len := 0;
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement2.GetBytes(ParameterIndex: Integer): TBytes;
var
  Buf: Pointer;
  L: LengthInt;
begin
  GetBytes(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Buf, L);
  Result := TBytes(Buf);
end;

procedure TZAbstractCallableStatement2.GetCurrency(Index: Integer;
  out Result: Currency);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetCurrency(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, stCurrency, P8Bytes(@Result));
  end else begin
    Result := 0; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement2.GetCurrency(
  ParameterIndex: Integer): Currency;
begin
  GetCurrency(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractCallableStatement2.GetDate(
  ParameterIndex: Integer): TDateTime;
begin
  GetDateTime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
  Result := Int(Result);
end;

procedure TZAbstractCallableStatement2.GetDateTime(Index: Integer;
  out Result: TDateTime);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetDateTime(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, FExecStatements[FCallExecKind].BindList.SQLTypes[Index], P8Bytes(@Result));
  end else begin
    Result := 0; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

procedure TZAbstractCallableStatement2.GetDouble(Index: Integer;
  out Result: Double);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetDouble(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, FExecStatements[FCallExecKind].BindList.SQLTypes[Index], P8Bytes(@Result));
  end else begin
    Result := 0; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement2.GetDouble(
  ParameterIndex: Integer): Double;
begin
  GetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

{**
  Get the first resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement2.GetFirstResultSet: IZResultSet;
begin
  Result := nil;
end;

function TZAbstractCallableStatement2.GetFloat(ParameterIndex: Integer): Single;
var D: Double;
begin
  GetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, D);
  Result := D;
end;

{**
  Gets the value of a JDBC <code>INTEGER</code> parameter as an <code>int</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement2.GetInt(ParameterIndex: Integer): Integer;
var I: Int64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, I);
  Result := Integer(I);
end;

{**
  Get the last resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement2.GetLastResultSet: IZResultSet;
begin
  Result := nil;
end;

procedure TZAbstractCallableStatement2.GetLob(Index: Integer;
  out Result: IZBlob);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetLob(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, FExecStatements[FCallExecKind].BindList.SQLTypes[Index], Result);
  end else begin
    Result := nil; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement2.GetLong(ParameterIndex: Integer): Int64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractCallableStatement2.GetMoreResults: Boolean;
begin
  Result := FExecStatements[FCallExecKind].GetMoreResults
end;

{**
  Get the next resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement2.GetNextResultSet: IZResultSet;
begin
  Result := nil;
end;

procedure TZAbstractCallableStatement2.GetOrdinal(Index: Integer;
  out Result: Int64);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetOrdinal(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, FExecStatements[FCallExecKind].BindList.SQLTypes[Index], P8Bytes(@Result));
  end else begin
    Result := 0; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZAbstractCallableStatement2.GetOrdinal(Index: Integer;
  out Result: UInt64);
begin
  if FExecStatements[FCallExecKind] <> nil then begin
    FExecStatements[FCallExecKind].GetOrdinal(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      BindList.Put(Index, FExecStatements[FCallExecKind].BindList.SQLTypes[Index], P8Bytes(@Result));
  end else begin
    Result := 0; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement2.GetPChar(ParameterIndex: Integer): PChar;
var
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  {$IFDEF UNICODE}
  FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Pointer(Result), L, zCP_UTF16);
  {$ELSE}
  FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Pointer(Result), L, ConSettings.CTRL_CP);
  {$ENDIF}
  if (FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)] <> FExecStatements[FCallExecKind]) and
     (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    BindList.Put(ParameterIndex, FExecStatements[FCallExecKind].BindList[ParameterIndex]);
end;

{**
  Get the previous resultset..
  @result <code>IZResultSet</code> if supported
}
procedure TZAbstractCallableStatement2.GetPChar(Index: Integer;
  out Buf: Pointer; out Len: LengthInt; CodePage: Word);
begin
  if FExecStatements[FCallExecKind] <> nil
  then FExecStatements[FCallExecKind].GetPChar(Index, Buf, Len, CodePage)
  else begin
    Buf := nil;
    Len := 0;
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement2.GetPreviousResultSet: IZResultSet;
begin
  Result := nil;
end;

function TZAbstractCallableStatement2.GetResultSet: IZResultSet;
begin
  Result := FExecStatements[FCallExecKind].GetResultSet;
end;

{**
  Retrieves a ResultSet by his index.
  @param Index the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
function TZAbstractCallableStatement2.GetResultSetByIndex(
  const Index: Integer): IZResultSet;
begin
  Result := nil;
end;

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZAbstractCallableStatement2.GetResultSetCount: Integer;
begin
  Result := 0;
end;

function TZAbstractCallableStatement2.GetShort(
  ParameterIndex: Integer): ShortInt;
var I: Int64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, I);
  Result := ShortInt(I);
end;

function TZAbstractCallableStatement2.GetSmall(
  ParameterIndex: Integer): SmallInt;
var I: UInt64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, I);
  Result := SmallInt(I);
end;

{**
  Get the value of a JDBC <code>TIME</code> parameter as a
  <code>java.sql.Time</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement2.GetTime(
  ParameterIndex: Integer): TDateTime;
begin
  GetDateTime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
  Result := Frac(Result);
end;

procedure TZAbstractCallableStatement2.GetTimeStamp(Index: Integer;
  out Result: TZTimeStamp);
begin
  RaiseUnsupportedException
end;

function TZAbstractCallableStatement2.GetTimestamp(
  ParameterIndex: Integer): TDateTime;
begin
  GetDateTime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractCallableStatement2.GetUInt(
  ParameterIndex: Integer): Cardinal;
var U: UInt64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, U);
  Result := Cardinal(U);
end;

function TZAbstractCallableStatement2.GetULong(ParameterIndex: Integer): UInt64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractCallableStatement2.GetUpdateCount: Integer;
begin
  Result := FExecStatements[FCallExecKind].GetUpdateCount
end;

function TZAbstractCallableStatement2.GetValue(
  ParameterIndex: Integer): TZVariant;
var
  L: LengthInt;
  {$IFDEF BCC32_vtDateTime_ERROR}
  DT: TDateTime;
  {$ENDIF}
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  Result := NullVariant;
  if not FExecStatements[FCallExecKind].IsNull(ParameterIndex) then
    case BindList.SQLTypes[ParameterIndex] of
      stBoolean: begin
          FExecStatements[FCallExecKind].GetBoolean(ParameterIndex, Result.VBoolean);
          Result.VType := vtBoolean;
        end;
      stByte..stInteger, stLong: begin
          FExecStatements[FCallExecKind].GetOrdinal(ParameterIndex, Result.VInteger);
          Result.VType := vtInteger;
        end;
      stULong: begin
          FExecStatements[FCallExecKind].GetOrdinal(ParameterIndex, Result.VUInteger);
          Result.VType := vtUInteger;
        end;
      stFloat, stDouble: begin
          InitializeVariant(Result, vtDouble);
          FExecStatements[FCallExecKind].GetDouble(ParameterIndex, Result.VDouble);
        end;
      stCurrency: begin
          InitializeVariant(Result, vtCurrency);
          FExecStatements[FCallExecKind].GetCurrency(ParameterIndex, Result.VCurrency);
        end;
      stBigDecimal: begin
          InitializeVariant(Result, vtBigDecimal);
          FExecStatements[FCallExecKind].GetBigDecimal(ParameterIndex, Result.VBigDecimal);
        end;
      stTime,stDate,stTimeStamp: begin
          {$IFDEF BCC32_vtDateTime_ERROR}
          FExecStatements[FCallExecKind].GetDateTime(ParameterIndex, DT);
          Result.VDateTime := DT;
          {$ELSE}
          FExecStatements[FCallExecKind].GetDateTime(ParameterIndex, Result.VDateTime);
          {$ENDIF}
          Result.VType := vtDateTime;
        end;
      stGUID: begin
          FExecStatements[FCallExecKind].GetBytes(ParameterIndex, Result.VPointer, L);
          Result := EncodeGUID(PGUID(Result.VPointer)^);
        end;
      stBytes: begin
          FExecStatements[FCallExecKind].GetBytes(ParameterIndex, Result.VPointer, L);
          Result := EncodeBytes(BufferToBytes(Result.VPointer, L));
        end;
      stString, stUnicodeString: begin
          if ConSettings^.AutoEncode then begin
            FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Result.VPointer, L, ConSettings^.CTRL_CP);
            Result.VType := vtString;
            System.SetString(Result.VString, PChar(Result.VPointer), L);
          end else if (ConSettings.ClientCodePage^.Encoding = ceUTF16) or (not ConSettings.ClientCodePage^.IsStringFieldCPConsistent) then begin
            FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Result.VPointer, L, zCP_UTF16);
            Result.VType := vtUnicodeString;
            System.SetString(Result.VUnicodeString, PWideChar(Result.VPointer), L);
          end else begin
            FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Result.VPointer, L, ConSettings^.ClientCodePage^.CP);
            Result.VType := vtRawByteString;
            ZSetString(PAnsiChar(Result.VPointer), L, Result.VRawByteString);
          end;
        end;
      stAsciiStream, stUnicodeStream, stBinaryStream: begin
          FExecStatements[FCallExecKind].GetLob(ParameterIndex, PIZLob(@Result.VInterface)^);
          Result := EncodeInterface(PIZLob(@Result.VInterface)^);
        end;
    end;
  if BindList.ParamTypes[ParameterIndex] = pctInOut then
    BindList.Put(ParameterIndex, FExecStatements[FCallExecKind].BindList[ParameterIndex]);
end;

function TZAbstractCallableStatement2.GetWord(ParameterIndex: Integer): Word;
var U: UInt64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, U);
  Result := Word(U);
end;

{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractCallableStatement2.IsFunction: Boolean;
var I: Integer;
begin
  Result := False;
  for I := 0 to BindList.Count -1 do
    if BindList.ParamTypes[i] = pctReturn then begin
      Result := True;
      Break;
    end;
end;

function TZAbstractCallableStatement2.IsNull(ParameterIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  Result := FExecStatements[FCallExecKind].IsNull(ParameterIndex);
  if Result and (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    BindList.SetNull(ParameterIndex, BindList.SQLTypes[ParameterIndex])
end;

procedure TZAbstractCallableStatement2.Prepare;
begin
  if FExecStatements[FCallExecKind] = nil then begin
    FExecStatements[FCallExecKind] := CreateExecutionStatement(FCallExecKind, FStoredProcName);
    FExecStatements[FCallExecKind]._AddRef;
  end;
  FExecStatements[FCallExecKind].SetResultSetType(GetResultSetType);
//  FExecStatements[FCallExecKind].SetResultSetConcurrency(GetResultSetConcurrency);
  if not Prepared then
    inherited Prepare;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractCallableStatement2.PrepareInParameters;
var I: Integer;
begin
  for i := BindList.Count -1 downto 0 do
    FExecStatements[FCallExecKind].RegisterParameter(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
      BindList.SQLTypes[i], BindList.ParamTypes[i]);
end;

procedure TZAbstractCallableStatement2.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  if BindList.Count < ParameterIndex {$IFDEF GENERIC_INDEX}+1{$ENDIF}
  then RegisterParameter(ParameterIndex, TZSQLType(SQLType), pctUnknown)
  else RegisterParameter(ParameterIndex, TZSQLType(SQLType), BindList[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].ParamType)
end;

procedure TZAbstractCallableStatement2.RegisterParamType(ParameterIndex,
  ParamType: Integer);
begin
  if BindList.Count < ParameterIndex {$IFDEF GENERIC_INDEX}+1{$ENDIF}
  then RegisterParameter(ParameterIndex, stUnknown, TZProcedureColumnType(ParamType))
  else RegisterParameter(ParameterIndex, BindList[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].SQLType, TZProcedureColumnType(ParamType))
end;

procedure TZAbstractCallableStatement2.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var CallExecKind: TZCallExecKind;
begin
  for CallExecKind := low(TZCallExecKind) to high(TZCallExecKind) do
    if Assigned(FExecStatements[CallExecKind]) then
      FExecStatements[CallExecKind].ReleaseImmediat(Sender, AError);
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement2.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, Value);
end;

{**
  Sets the designated parameter the default SQL value.
  <P><B>Note:</B> You must specify the default value.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the default value normally defined in the field's DML SQL statement
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract method - parameters not used intentionally
procedure TZAbstractCallableStatement2.SetDefaultValue(ParameterIndex: Integer;
  const Value: String);
begin
  //it's a nop
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractCallableStatement2.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  if Boolean(BindList.ParamTypes[ParameterIndex]) and Boolean(BindList.SQLTypes[ParameterIndex])
  then BindList.SetNull(ParameterIndex, BindList.SQLTypes[ParameterIndex])
  else BindList.SetNull(ParameterIndex, SQLType);
end;

procedure TZAbstractCallableStatement2.Unprepare;
var CallExecKind: TZCallExecKind;
begin
  for CallExecKind := Low(TZCallExecKind) to High(TZCallExecKind) do
    if FExecStatements[CallExecKind] <> nil then begin
      FExecStatements[CallExecKind].Close;
      FExecStatements[CallExecKind]._Release;
      FExecStatements[CallExecKind] := nil
    end;
  inherited Unprepare;
end;

{ TZAbstractCallableStatement_A }

procedure TZAbstractCallableStatement_A.AfterConstruction;
begin
  inherited;
  FCharRec.CP := ConSettings^.ClientCodePage.CP;
end;

procedure TZAbstractCallableStatement_A.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
begin
  CheckParameterIndex(Index);
  BindList.Put(Index, stString, Buf, Len, ConSettings^.ClientCodePage^.CP);
end;

procedure TZAbstractCallableStatement_A.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  CheckParameterIndex(Index);
  BindList.Put(Index, stString, Value, ConSettings^.ClientCodePage^.CP);
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>AsniString</code> object
  returned is a Ansi(CP_GETACP) encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractCallableStatement_A.GetAnsiString(
  ParameterIndex: Integer): AnsiString;
var
  P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Pointer(P), L, ConSettings.ClientCodePage.CP);
  if (FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)] <> FExecStatements[FCallExecKind]) and
     (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    if FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)] = nil then begin
      ZSetString(P, L, FRawTemp);
      BindList.Put(ParameterIndex, FExecStatements[FCallExecKind].BindList.SQLTypes[ParameterIndex], FRawTemp, ConSettings.ClientCodePage.CP)
    end else
      TZRawPreparedStatement(FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)]).BindRawStr(ParameterIndex, P, L);
  if ZCompatibleCodePAges(ConSettings^.ClientCodePage^.CP, ZOSCodePage) then
    ZSetString(P, L, Result)
  else begin
    FUniTemp := PRawToUnicode(P, L, ConSettings.ClientCodePage.CP);
    Result := ZUnicodeToRaw(FUniTemp, ZOSCodePage);
  end;
end;
{$ENDIF NO_ANSISTRING}

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>RawByteString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement_A.GetRawByteString(
  ParameterIndex: Integer): RawByteString;
var
  P: PAnsiChar;
  L: LengthInt;
  ReverseExecKind: TZCallExecKind;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Pointer(P), L, ConSettings.ClientCodePage.CP);
  ReverseExecKind := TZCallExecKind(not Ord(FCallExecKind) and 1);
  if (FExecStatements[ReverseExecKind] <> FExecStatements[FCallExecKind]) and
     (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    if FExecStatements[ReverseExecKind] = nil then begin
      ZSetString(P, L, FRawTemp);
      BindList.Put(ParameterIndex, FExecStatements[FCallExecKind].BindList.SQLTypes[ParameterIndex], FRawTemp, ConSettings.ClientCodePage.CP)
    end else
      TZRawPreparedStatement(FExecStatements[ReverseExecKind]).BindRawStr(ParameterIndex, P, L);
  ZSetString(P, L, Result)
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement_A.GetString(
  ParameterIndex: Integer): String;
begin
  {$IFDEF UNICODE}
  Result := GetUnicodeString(ParameterIndex);
  {$ELSE}
  if ConSettings.AutoEncode then
    if ZCompatibleCodePages(ConSettings.ClientCodePage.CP, ConSettings.CTRL_CP) then
      Result := GetRawByteString(ParameterIndex)
    else begin
      FUniTemp := GetUnicodeString(ParameterIndex);
      Result := ConSettings.ConvFuncs.ZUnicodeToString(FUniTemp, ConSettings.CTRL_CP);
    end
  else
    Result := GetRawByteString(ParameterIndex)
  {$ENDIF}
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>ZWideString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement_A.GetUnicodeString(
  ParameterIndex: Integer): ZWideString;
var
  L: LengthInt;
  ReverseExecKind: TZCallExecKind;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatements[FCallExecKind].GetPChar(ParameterIndex, FCharRec.P, L, ConSettings.ClientCodePage.CP);
  ReverseExecKind := TZCallExecKind(not Ord(FCallExecKind) and 1);
  if (FExecStatements[ReverseExecKind] <> FExecStatements[FCallExecKind]) and
     (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    if FExecStatements[ReverseExecKind] = nil then begin
      ZSetString(FCharRec.P, L, FRawTemp);
      BindList.Put(ParameterIndex, FExecStatements[FCallExecKind].BindList.SQLTypes[ParameterIndex], FRawTemp, ConSettings.ClientCodePage.CP);
      FBindAgain[ReverseExecKind] := True;
    end else begin
      FCharRec.Len := L;
      TZRawPreparedStatement(FExecStatements[ReverseExecKind]).BindRawStr(ParameterIndex, FCharRec.P, L);
    end;
  Result := PRawToUnicode(FCharRec.P, L, ConSettings.ClientCodePage.CP);
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object returned is a UTF8 encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractCallableStatement_A.GetUTF8String(
  ParameterIndex: Integer): UTF8String;
var
  P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatements[FCallExecKind].GetPChar(ParameterIndex, Pointer(P), L, ConSettings.ClientCodePage.CP);
  if (FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)] <> FExecStatements[FCallExecKind]) and
     (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    if FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)] = nil then begin
      ZSetString(P, L, FRawTemp);
      BindList.Put(ParameterIndex, FExecStatements[FCallExecKind].BindList.SQLTypes[ParameterIndex], FRawTemp, ConSettings.ClientCodePage.CP)
    end else
      TZRawPreparedStatement(FExecStatements[TZCallExecKind(not Ord(FCallExecKind) and 1)]).BindRawStr(ParameterIndex, P, L);
  if ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, zCP_UTF8) then
    ZSetString(P, L, Result)
  else begin
    FUniTemp := PRawToUnicode(P, L, ConSettings.ClientCodePage.CP);
    Result := ZUnicodeToRaw(FUniTemp, ZOSCodePage);
  end;
end;
{$ENDIF}

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractCallableStatement_A.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  if ZCompatibleCodepages(zOSCodePage, ConSettings^.ClientCodePage.CP)
  then IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetRawByteString(ParameterIndex, Value)
  else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZAnsiToRaw(Value, ConSettings^.ClientCodePage.CP));
end;
{$ENDIF}

procedure TZAbstractCallableStatement_A.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  if ZCompatibleCodepages(Value.CP, ConSettings^.ClientCodePage.CP) then
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value.P, Value.Len)
  else begin
    FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
    SetRawByteString(ParameterIndex, ZUnicodeToRaw(FUniTemp, ConSettings^.ClientCodePage.CP));
  end;
end;

procedure TZAbstractCallableStatement_A.SetRawByteString(
  ParameterIndex: Integer; const Value: RawByteString);
begin
  CheckParameterIndex(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
  BindList.Put(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stString, Value, ConSettings^.ClientCodePage^.CP);
end;

procedure TZAbstractCallableStatement_A.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  SetRawByteString(ParameterIndex, ZUnicodeToRaw(Value, ConSettings^.ClientCodePage.CP));
  {$ELSE}
  SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage.CP));
  {$ENDIF}
end;

procedure TZAbstractCallableStatement_A.SetUnicodeString(
  ParameterIndex: Integer; const Value: ZWideString);
begin
  SetRawByteString(ParameterIndex, ZUnicodeToRaw(Value, ConSettings^.ClientCodePage.CP));
end;

{$IFNDEF NO_UTF8STRING}
procedure TZAbstractCallableStatement_A.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  if ZCompatibleCodepages(zCP_UTF8, ConSettings^.ClientCodePage.CP)
  then SetRawByteString(ParameterIndex, Value)
  else SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZUTF8ToRaw(Value, ConSettings^.ClientCodePage.CP));
end;
{$ENDIF}

{ TZAbstractCallableStatement_W }

procedure TZAbstractCallableStatement_W.AfterConstruction;
begin
  inherited;
  FCharRec.CP := zCP_UTF16;
end;

procedure TZAbstractCallableStatement_W.FillAndBindCharRec(Index: Integer;
  out Len: LengthInt);
var ReverseExecKind: TZCallExecKind;
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  FExecStatements[FCallExecKind].GetPChar(Index, FCharRec.P, Len, zCP_UTF16);
  ReverseExecKind := TZCallExecKind(not Ord(FCallExecKind) and 1);
  if (FExecStatements[ReverseExecKind] <> FExecStatements[FCallExecKind]) and
     (BindList.ParamTypes[Index] = pctInOut) then
    if FExecStatements[ReverseExecKind] = nil then begin
      System.SetString(FUniTemp, PWideChar(FCharRec.P), Len);
      BindList.Put(Index, FExecStatements[FCallExecKind].BindList.SQLTypes[Index], FUniTemp)
    end else begin
      FCharRec.Len := Len;
      IZPreparedStatement(FExecStatements[ReverseExecKind].FWeakIntfPtrOfIPrepStmt).SetCharRec(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FCharRec);
    end;
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractCallableStatement_W.GetAnsiString(
  ParameterIndex: Integer): AnsiString;
var L: LengthInt;
begin
  FillAndBindCharRec(ParameterIndex, L);
  Result := ZEncoding.PUnicodeToRaw(FCharRec.P, L, ZOSCodePage)
end;
{$ENDIF}

function TZAbstractCallableStatement_W.GetRawByteString(
  ParameterIndex: Integer): RawByteString;
var L: LengthInt;
begin
  FillAndBindCharRec(ParameterIndex, L);
  Result := ZEncoding.PUnicodeToRaw(FCharRec.P, L, ConSettings.ClientCodePage.CP)
end;

function TZAbstractCallableStatement_W.GetString(
  ParameterIndex: Integer): String;
begin
  {$IFDEF UNICODE}
  Result := GetUnicodeString(ParameterIndex);
  {$ELSE}
  if ConSettings.AutoEncode then
    if ZCompatibleCodePages(ConSettings.ClientCodePage.CP, ConSettings.CTRL_CP) then
      Result := GetRawByteString(ParameterIndex)
    else begin
      FUniTemp := GetUnicodeString(ParameterIndex);
      Result := ConSettings.ConvFuncs.ZUnicodeToString(FUniTemp, ConSettings.CTRL_CP);
    end
  else
    Result := GetRawByteString(ParameterIndex)
  {$ENDIF}
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>ZWideString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement_W.GetUnicodeString(
  ParameterIndex: Integer): ZWideString;
var L: LengthInt;
begin
  FillAndBindCharRec(ParameterIndex, L);
  System.SetString(Result, PWideChar(FCharRec.P), L);
end;

{$IFNDEF NO_UTF8STRING}
function TZAbstractCallableStatement_W.GetUTF8String(
  ParameterIndex: Integer): UTF8String;
var L: LengthInt;
begin
  FillAndBindCharRec(ParameterIndex, L);
  Result := ZEncoding.PUnicodeToRaw(FCharRec.P, L, zCP_UTF8)
end;
{$ENDIF}

{$IFNDEF NO_ANSISTRING}
procedure TZAbstractCallableStatement_W.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  FUniTemp := ZRawToUnicode(Value, ZOSCodePage);
  SetUnicodeString(ParameterIndex, FUniTemp);
end;
{$ENDIF NO_ANSISTRING}

procedure TZAbstractCallableStatement_W.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stUnicodeString, Value.P, Value.Len, Value.CP);
end;

procedure TZAbstractCallableStatement_W.SetRawByteString(
  ParameterIndex: Integer; const Value: RawByteString);
begin
  FUniTemp := ZRawToUnicode(Value, ConSettings.ClientCodePage.CP);
  SetUnicodeString(ParameterIndex, FUniTemp);
end;

procedure TZAbstractCallableStatement_W.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  {$IFNDEF UNICODE}
  FUniTemp := ConSettings.ConvFuncs.ZStringToUnicode(Value, ConSettings.CTRL_CP);
  {$ENDIF}
  SetUnicodeString(ParameterIndex, {$IFDEF UNICODE}Value{$ELSE}FUniTemp{$ENDIF});
end;

{**
  Sets the designated parameter to a Object Pascal <code>ZWideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement_W.SetUnicodeString(
  ParameterIndex: Integer; const Value: ZWideString);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex -1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindList.Put(ParameterIndex, stUnicodeString, Value);
end;

{$IFNDEF NO_UTF8STRING}
procedure TZAbstractCallableStatement_W.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  FUniTemp := ZRawToUnicode(Value, zCP_UTF8);
  SetUnicodeString(ParameterIndex, FUniTemp);
end;
{$ENDIF}

end.

