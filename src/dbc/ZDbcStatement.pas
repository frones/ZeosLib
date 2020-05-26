{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

{$IFDEF WITH_LEGACYIFEND}
{$LEGACYIFEND ON}
{$ENDIF}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, FmtBCD,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZTokenizer, ZCompatibility, ZVariant, ZDbcLogging, ZClasses,
  ZDbcUtils;

type
  {** Implements Abstract Generic SQL Statement. }

  { TZAbstractStatement }

  TZAbstractStatement = class(TZCodePagedObject, IZStatement, IZLoggingObject,
    IImmediatelyReleasable)
  private
    FMaxFieldSize: Integer;
    FMaxRows: Integer;
    FEscapeProcessing: Boolean;
    FQueryTimeout: Integer;
    FLastUpdateCount: Integer;
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
    FLastResultSet: IZResultSet;
    FCursorName: RawByteString;
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
    procedure ReleaseConnection; virtual;
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

    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; virtual;
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString; virtual;
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

    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; virtual;
  end;

  TZBindType = (zbtNull, zbt8Byte, zbt4Byte,
    zbtRawString, zbtUTF8String, {$IFNDEF NEXTGEN}zbtAnsiString,{$ENDIF}
    zbtUniString, zbtCharByRef, zbtBinByRef, zbtGUID, zbtBytes,
    zbtArray, zbtRefArray, zbtLob, zbtPointer, zbtBCD,
    zbtDate, zbtTime, zbtTimeStamp, zbtCustom);

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
    SizeOf(TZArray), SizeOf(TZArray), 0, 0, SizeOf(TBCD),
    SizeOf(TZDate), SizeOf(TZTime), SizeOf(TZTimeStamp), 0);
type
  P8Bytes = PInt64;
  P4Bytes = PCardinal;

  PZTimeStamp = ^TZTimeStamp;

  PZTParamValueArray = ^TZTParamValueArray;
  {** Defines a static array of bind values. }
  TZTParamValueArray = array[0..High(Word)] of TZBindValue;

  TZAbstractPreparedStatement = class; //forward
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
    procedure Put(Index: Integer; const Value: TZTime); overload;
    procedure Put(Index: Integer; const Value: TZDate); overload;
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
    procedure SetParamTypes(Index: Integer; SQLType: TZSQLType; ParamIO: TZProcedureColumnType);

    procedure BindValuesToStatement(Stmt: TZAbstractPreparedStatement);
  public
    HasOutParam: Boolean;
    HasReturnParam: Boolean;
    HasInOutParam: Boolean;
    HasResultSetParam: Boolean;
    function HasOutOrInOutOrResultParam: Boolean; {$IFDEF WITH_INLINE}inline;{$ENDIF}
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
  TZAbstractPreparedStatement = class(TZAbstractStatement, IImmediatelyReleasable)
  private
    FBatchDMLArrayCount: ArrayLenInt;
    FPrepared : Boolean;
    FSupportsDMLBatchArrays: Boolean;
    FBindList: TZBindList;
  protected
    FOpenLobStreams: TZSortedList;
    FUniTemp: ZWideString;
    FRawTemp: RawByteString;
    FTokenMatchIndex, //did we match a token to indicate if Prepare makes sense?
    FCountOfQueryParams: Integer; //how many params did we found to prepvent mem-reallocs?
    FGUIDAsString: Boolean; //How should a GUID value be treaded?
    FWeakIntfPtrOfIPrepStmt: Pointer; //EH: address of IZPreparedStatement(Self) to access non virtual methods
    FOutParamResultSet: IZResultSet;
    FClientCP: Word;
    FSupportsBidirectionalParamIO: Boolean;
    property TokenMatchIndex: Integer read FTokenMatchIndex;
    procedure CheckParameterIndex(var Value: Integer); virtual;
    procedure PrepareInParameters; virtual;
    procedure BindInParameters; virtual;
    procedure UnPrepareInParameters; virtual;
    procedure PrepareOpenResultSetForReUse; override;
    procedure PrepareLastResultSetForReUse; override;


    procedure ValidateArraySizeAndType(const Value: Pointer; SQLType: TZSQLType;
      VariantType: TZVariantType; ParamIndex: Integer);

    procedure SetParamCount(NewParamCount: Integer); virtual;
    procedure SetBindCapacity(Capacity: Integer); virtual;

    procedure LogPrepStmtMessage(Category: TZLoggingCategory; const Msg: RawByteString = EmptyRaw);
    procedure AddParamLogValue(ParamIndex: Integer; SQLWriter: TZRawSQLStringWriter; Var Result: RawByteString); virtual;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; virtual;
    function ParamterIndex2ResultSetIndex(Value: Integer): Integer;
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
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); virtual;
  protected
    procedure GetLob(Index: Integer; out Result: IZBlob); virtual;
    procedure GetPChar(Index: Integer; out Buf: Pointer; out Len: LengthInt; CodePage: Word); overload; virtual;
  public
    function IsNull(ParameterIndex: Integer): Boolean;
    function GetBoolean(ParameterIndex: Integer): Boolean;
    function GetByte(ParameterIndex: Integer): Byte;
    function GetShort(ParameterIndex: Integer): ShortInt;
    function GetWord(ParameterIndex: Integer): Word;
    function GetSmall(ParameterIndex: Integer): SmallInt;
    function GetUInt(ParameterIndex: Integer): Cardinal;
    function GetInt(ParameterIndex: Integer): Integer;
    function GetULong(ParameterIndex: Integer): UInt64;
    function GetLong(ParameterIndex: Integer): Int64;
    function GetFloat(ParameterIndex: Integer): Single;
    function GetDouble(ParameterIndex: Integer): Double;
    function GetCurrency(ParameterIndex: Integer): Currency;
    procedure GetBigDecimal(ParameterIndex: Integer; var Result: TBCD);
    procedure GetGUID(ParameterIndex: Integer; var Result: TGUID);
    function GetBytes(ParameterIndex: Integer): TBytes; overload;
    function GetDate(ParameterIndex: Integer): TDateTime; overload;
    procedure GetDate(ParameterIndex: Integer; var Result: TZDate); overload;
    function GetTime(ParameterIndex: Integer): TDateTime; overload;
    procedure GetTime(ParameterIndex: Integer; var Result: TZTime); overload;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; overload;
    procedure GetTimeStamp(ParameterIndex: Integer; var Result: TZTimeStamp); overload; virtual;
    function GetString(ParameterIndex: Integer): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString;

    function GetBLob(ParameterIndex: Integer): IZBlob;
    function GetCLob(ParameterIndex: Integer): IZClob;

    function GetValue(ParameterIndex: Integer): TZVariant;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
    destructor Destroy; override;

    function GetResultSet: IZResultSet; override;

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

    procedure SetPChar(ParameterIndex: Integer; Value: PChar); virtual;
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); virtual;
    procedure SetGUID(ParameterIndex: Integer; const Value: TGUID); virtual;
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); overload; virtual;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); overload; virtual;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); overload;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp); overload;
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

    procedure ClearParameters; virtual;

    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; override;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); override;
    procedure SetResultSetType(Value: TZResultSetType); override;
  end;

  TZRawPreparedStatement = class(TZAbstractPreparedStatement)
  protected
    procedure BindRawStr(Index: Integer; const Value: RawByteString); overload; virtual;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); overload; virtual;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
  public
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);
    procedure SetString(ParameterIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString);
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);
  end;

  TZRawParamDetectPreparedStatement = class(TZRawPreparedStatement)
  protected
    FCachedQueryRaw: TRawByteStringDynArray;
    FNCharDetected: PBooleanDynArray;
    FIsParamIndex: TBooleanDynArray;
    property IsParamIndex: TBooleanDynArray read FIsParamIndex;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    procedure Unprepare; override;
  end;

  TZUTF16PreparedStatement = class(TZAbstractPreparedStatement)
  protected
    procedure BindUniStr(Index: Integer; const Value: ZWideString); overload; virtual;
    procedure BindUniStr(Index: Integer; Buf: PWideChar; CodePoints: LengthInt); overload; virtual;
  public
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);
    procedure SetString(ParameterIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString);
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);
  end;

  TZUTF16ParamDetectPreparedStatement = class(TZUTF16PreparedStatement)
  protected
    FCachedQueryUni: TUnicodeStringDynArray;
    FNCharDetected: PBooleanDynArray;
    FIsParamIndex: TBooleanDynArray;
  public
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString; override;
    procedure Unprepare; override;
  end;

  { EH: implements a wrapper class for the Stored-Procedures/Functions}
  TZAbstractCallableStatement = class(TZAbstractPreparedStatement)
  private
    FStoredProcName: String;
    FCharRec: TZCharRec;
    fBCDTemp: TBCD;
    procedure BindSignedOrdinal(Index: Integer; SQLType: TZSQLType; Value: NativeInt);
    procedure BindUnsignedOrdinal(Index: Integer; SQLType: TZSQLType; Value: NativeUint);
    procedure BindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double);
    procedure CopyCallResults;
  protected
    FParamsRegistered, FRegisteringParamFromMetadata: Boolean;
    FResults: IZCollection;
    FActiveResultIndex: Integer;
    FExecStatement: TZAbstractPreparedStatement;
    function CreateExecutionStatement(const StoredProcName: String): TZAbstractPreparedStatement; virtual; abstract;
    function IsFunction: Boolean;
    procedure BindInParameters; override;
    procedure PrepareInParameters; override;
    procedure CheckParameterIndex(var Value: Integer); override;
    property StoredProcName: String read FStoredProcName;
  public //value getter procs
    procedure GetLob(Index: Integer; out Result: IZBlob); override;
    procedure GetPChar(Index: Integer; out Buf: Pointer; out Len: LengthInt; CodePage: Word); override;
  public //deprecated value getter methods
    function GetPChar(ParameterIndex: Integer): PChar; overload;
  public //value getter methods
    function IsNull(ParameterIndex: Integer): Boolean; reintroduce;
    function GetBoolean(ParameterIndex: Integer): Boolean; reintroduce;
    function GetUInt(ParameterIndex: Integer): Cardinal; reintroduce;
    function GetInt(ParameterIndex: Integer): Integer; reintroduce;
    function GetULong(ParameterIndex: Integer): UInt64; reintroduce;
    function GetLong(ParameterIndex: Integer): Int64; reintroduce;
    function GetFloat(ParameterIndex: Integer): Single; reintroduce;
    function GetDouble(ParameterIndex: Integer): Double; reintroduce;
    function GetCurrency(ParameterIndex: Integer): Currency; reintroduce;
    procedure GetBigDecimal(ParameterIndex: Integer; var Result: TBCD); reintroduce;
    function GetBytes(ParameterIndex: Integer): TBytes; reintroduce;
    procedure GetDate(ParameterIndex: Integer; var Result: TZDate); reintroduce; overload;
    procedure GetTime(ParameterIndex: Integer; var Result: TZTime); reintroduce; overload;
    procedure GetTimeStamp(ParameterIndex: Integer; var Result: TZTimeStamp); reintroduce; overload;

    function GetValue(ParameterIndex: Integer): TZVariant;
  public //value setter methods
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean);
    procedure SetNull(ParameterIndex: Integer; SQLType: TZSQLType);
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
    procedure SetDate(ParameterIndex: Integer; const Value: TZDate); reintroduce; overload;
    procedure SetTime(ParameterIndex: Integer; const Value: TZTime); reintroduce; overload;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TZTimeStamp); reintroduce; overload;
    procedure SetBytes(ParameterIndex: Integer; Value: PByte; Len: NativeUInt); reintroduce; overload;
  public
    function GetResultSet: IZResultSet; override;
    function GetUpdateCount: Integer; override;
    function GetMoreResults: Boolean; override;
  public //additional IZCallableStatement api
    function GetFirstResultSet: IZResultSet; virtual;
    function GetPreviousResultSet: IZResultSet; virtual;
    function GetNextResultSet: IZResultSet; virtual;
    function GetLastResultSet: IZResultSet; virtual;
    function BOR: Boolean; virtual;
    function EOR: Boolean; virtual;
    function GetResultSetByIndex(Index: Integer): IZResultSet; virtual;
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

  TZAbstractCallableStatement_A = class(TZAbstractCallableStatement, IZPreparedStatement)
  protected
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); overload;
  public
    procedure AfterConstruction; override;
  public //setters
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec);
    procedure SetString(ParameterIndex: Integer; const Value: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String);
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString);
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);
  public //getters
    function GetString(ParameterIndex: Integer): String; reintroduce;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString; reintroduce;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String; reintroduce;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString; reintroduce;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString; reintroduce;
  end;

  TZAbstractCallableStatement_W = class(TZAbstractCallableStatement, IZPreparedStatement)
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
    function GetString(ParameterIndex: Integer): String; reintroduce;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString; reintroduce;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String; reintroduce;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString; reintroduce;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString; reintroduce;
  end;

implementation

uses ZFastCode, ZSysUtils, ZMessages, ZDbcResultSet, ZCollections,
  ZEncoding, ZDbcProperties, ZDbcMetadata, ZDbcConnection,
  Math
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND};

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
  FClientCP := ConSettings.ClientCodePage.CP;
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
  FConnection := nil;
  FreeAndNil(FInfo);
  inherited Destroy;
end;

{**
  Sets the preprepared SQL-Statement in an String and AnsiStringForm.
  @param Value: the SQL-String which has to be optional preprepared
}
procedure TZAbstractStatement.SetWSQL(const Value: ZWideString);
{$IFNDEF UNICODE}var CP: Word;{$ENDIF}
begin
  if FWSQL <> Value then
    FClosed := False;
    {$IFDEF UNICODE}
    if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) then begin
      FWSQL := GetUnicodeEncodedSQL(Value);
      {$IFDEF DEBUG}FASQL := ZUnicodeToRaw(FWSQL, ConSettings^.CTRL_CP);{$ENDIF}
    end else begin
      FASQL := GetRawEncodedSQL(Value);
      FWSQL := Value;
    end;
    {$ELSE !UNICODE}
    begin
      if (ConSettings^.ClientCodePage^.Encoding = ceUTF16)
      then CP := ConSettings^.CTRL_CP
      else CP := FClientCP;
      FaSQL := GetRawEncodedSQL(ZUnicodeToRaw(Value, CP)); //required for the resultsets
      FWSQL := Value;
    end;
    {$ENDIF UNICODE}
end;

procedure TZAbstractStatement.SetASQL(const Value: RawByteString);
begin
  if FASQL <> Value then begin
    FClosed := False;
    {$IFDEF UNICODE}
    FWSQL := ZRawToUnicode(Value, FClientCP); //required for the resultsets
    if ConSettings^.ClientCodePage^.Encoding <> ceUTF16 //params, CS_NONE ?
    then FASQL := GetRawEncodedSQL(FWSQL)
    else FASQL := Value;
    {$ELSE !UNICODE}
    if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then begin
      FWSQL := GetUnicodeEncodedSQL(Value);
      FASQL := ZUnicodeToRaw(FWSQL, ConSettings^.CTRL_CP);
    end else
      FASQL := GetRawEncodedSQL(Value);
      {$IFDEF DEBUG}FWSQL := ZRawToUnicode(FASQL, FClientCP);{$ENDIF}
    {$ENDIF UNICODE}
  end;
end;

procedure TZAbstractStatement.ReleaseConnection;
begin
  FConnection := nil;
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
    if FLastResultSet.IsClosed then
      FLastResultSet := nil
    else if (FLastResultSet.GetConcurrency = GetResultSetConcurrency) and
            (FLastResultSet.GetFetchDirection = GetFetchDirection) then
      FLastResultSet.ResetCursor
    else begin
      FLastResultSet.Close;
      FLastResultSet := nil;
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
  if not fClosed then begin
    RefCountAdded := (RefCount = 1) and (Assigned(FOpenResultSet) or Assigned(FLastResultSet));
    if RefCountAdded then _AddRef;
    try
      try
        BeforeClose;
        FClosed := True;
        AfterClose;
      finally
        if FConnection <> nil then
          FConnection.DeregisterStatement(Self);
      end;
    finally
      FClosed := True;
      if RefCountAdded then begin
        if (RefCount = 1) then begin
          DriverManager.AddGarbage(Self);
          ReleaseConnection;
        end;
        _Release;
      end;
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
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
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
{$IFDEF UNICODE}
begin
  FWSQL := SQL;
  Result := ZUnicodeToRaw(SQL, ConSettings^.ClientCodePage^.CP);
{$ELSE}
var
  SQLTokens: TZTokenList;
  i: Integer;
  SQLWriter: TZRawSQLStringWriter;
  Raw: RawByteString;
begin
  if ConSettings^.AutoEncode then begin
    Result := EmptyRaw; //init for FPC
    SQLTokens := GetConnection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]); //Disassembles the Query
    SQLWriter := TZRawSQLStringWriter.Create(Length(SQL));
    try
      for i := 0 to SQLTokens.Count-1 do begin //Assembles the Query
        case SQLTokens[i].TokenType of
          ttQuoted, ttComment, ttQuotedIdentifier,
          ttWord: begin
                    Raw := ConSettings^.ConvFuncs.ZStringToRaw(SQLTokens.AsString(i),
                      ConSettings^.CTRL_CP, FClientCP);
                    SQLWriter.AddText(Raw, Result);
                  end
          else SQLWriter.AddText(SQLTokens[I].P, SQLTokens[I].L, Result);
        end;
      end;
    finally
      SQLWriter.Finalize(Result);
      FASQL := '';
      FreeAndNil(SQLWriter);
      SQLTokens.Free;
    end;
  end else begin
    FASQL := SQL;
    Result := SQL;
  end;
{$ENDIF !UNICODE}
end;

function TZAbstractStatement.GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString;
{$IFDEF UNICODE}
begin
  Result := SQL;
{$ELSE}
var
  SQLTokens: TZTokenList;
  i: Integer;
  US: ZWideString;
  SQLWriter: TZUnicodeSQLStringWriter;
begin
  if ConSettings^.AutoEncode then begin
    Result := ''; //init
    SQLTokens := GetConnection.GetDriver.GetTokenizer.TokenizeBufferToList(SQL, [toSkipEOF]); //Disassembles the Query
    SQLWriter := TZUnicodeSQLStringWriter.Create(Length(SQL));
    try
      for i := 0 to SQLTokens.Count -1 do begin
        case (SQLTokens[i].TokenType) of
          ttQuoted, ttComment, ttQuotedIdentifier,
          ttWord: US := ConSettings^.ConvFuncs.ZStringToUnicode(SQLTokens.AsString(I), ConSettings.CTRL_CP);
          else    US := ASCII7ToUnicodeString(SQLTokens[i].P, SQLTokens[i].L);
        end;
        SQLWriter.AddText(US, Result);
      end;
    finally
      SQLWriter.Finalize(Result);
      FreeAndNil(SQLTokens);
      FreeAndNil(SQLWriter);
      FWSQL := '';
    end;
  end else
    Result := ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings.CTRL_CP);
{$ENDIF}
end;

function TZAbstractStatement.CreateStmtLogEvent(Category: TZLoggingCategory;
  const Msg: RawByteString = EmptyRaw): TZLoggingEvent;
var SQLWriter: TZRawSQLStringWriter;
  L: LengthInt;
  EventMsg: RawByteString;
begin
  L := Length(Msg);
  L := L + 40;
  SQLWriter := TZRawSQLStringWriter.Create(L);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  EventMsg := EmptyRaw;
  SQLWriter.AddText('Statement ', EventMsg);
  {$ELSE}
  EventMsg := 'Statement ';
  {$ENDIF}
  SQLWriter.AddOrd(FStatementId, EventMsg);
  if msg <> EmptyRaw then begin
    SQLWriter.AddText(' : ', EventMsg);
    SQLWriter.AddText(Msg, EventMsg);
  end;
  SQLWriter.Finalize(EventMsg);
  FreeAndNil(SQLWriter);
  Result := TZLoggingEvent.Create(Category, ConSettings^.Protocol, EventMsg, 0, EmptyRaw)
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
  {$IFDEF WITH_VAR_INIT_WARNING}Result := nil;{$ENDIF}
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

procedure TZBindList.BindValuesToStatement(Stmt: TZAbstractPreparedStatement);
var
  i: Integer;
  BindValue: PZBindValue;
  BCD: TBCD;
begin
  Assert(Stmt.FWeakIntfPtrOfIPrepStmt <> nil, 'Interface not supported');
  for i := 0 to FCount -1 do begin
    BindValue := Get(I);
    if Ord(BindValue.ParamType) < Ord(pctOut) then begin
      case BindValue.BindType of
        zbtNull: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetNull(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType);
        zbt4Byte: case BindValue.SQLType of
                    stBoolean:  IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBoolean(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^ <> 0);
                    stByte:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetByte(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^);
                    stShort:    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetShort(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInteger(@BindValue.Value)^);
                    stWord:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetWord(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^);
                    stSmall:    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetSmall(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInteger(@BindValue.Value)^);
                    stLongWord: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetUInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCardinal(@BindValue.Value)^);
                    stInteger:  IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInteger(@BindValue.Value)^);
                    stFloat:    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetFloat(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PSingle(@BindValue.Value)^);
                    else raise ZDbcUtils.CreateUnsupportedParameterTypeException(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType);
                  end;
        zbt8Byte: case BindValue.SQLType of
                    stBoolean:  IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBoolean(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^ <> 0);
                    stByte:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetByte(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stShort:    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetShort(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stWord:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetWord(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stSmall:    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetSmall(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stLongWord: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetUInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stInteger:  IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetInt(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stULong:    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetULong(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stLong:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetLong(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stFloat:    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetFloat(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stDouble:   IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetDouble(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stCurrency: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetCurrency(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PCurrency({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stTime:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetTime(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stDate:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetDate(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stTimeStamp:IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetTimeStamp(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                    stBigDecimal: begin
                                    ZSysUtils.Double2BCD(PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, BCD{%H-});
                                    IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBigDecimal(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BCD)
                                  end;
                    else raise ZDbcUtils.CreateUnsupportedParameterTypeException(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType);
                  end;
        zbtRawString: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetRawByteString(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RawByteString(BindValue.Value));
        {$IFNDEF NO_UTF8STRING}
        zbtUTF8String: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetUTF8String(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, UTF8String(BindValue.Value));
        {$ENDIF}
        {$IFNDEF NO_ANSISTRING}
        zbtAnsiString: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetAnsiString(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, AnsiString(BindValue.Value));
        {$ENDIF}
        zbtUniString: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetUnicodeString(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, ZWideString(BindValue.Value));
        zbtCharByRef: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetCharRec(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZCharRec(BindValue.Value)^);
        zbtBinByRef:  IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBytes(I, PZBufRec(BindValue.Value).Buf, PZBufRec(BindValue.Value).Len);
        zbtGUID:      IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetGUID(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGUID(BindValue.Value)^);
        zbtBytes:     IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBytes(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TBytes(BindValue.Value));
        zbtArray,
        zbtRefArray:  begin
                        IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetDataArray(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZArray(BindValue.Value).VArray,
                          TZSQLType(PZArray(BindValue.Value).VArrayType), PZArray(BindValue.Value).VArrayVariantType);
                        if PZArray(BindValue.Value).VIsNullArray <> nil then
                          IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetNullArray(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TZSQLType(PZArray(BindValue.Value).VIsNullArrayType),
                            PZArray(BindValue.Value).VIsNullArray, PZArray(BindValue.Value).VIsNullArrayVariantType);
                      end;
        zbtLob:       IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBlob(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType, IZBlob(BindValue.Value));
        zbtPointer:   IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBoolean(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.Value <> nil);
        zbtBCD:       IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetBigDecimal(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PBCD(BindValue.Value)^);
        zbtDate:      IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetDate(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZDate(BindValue.Value)^);
        zbtTime:      IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetTime(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZTime(BindValue.Value)^);
        zbtTimeStamp: IZPreparedStatement(Stmt.FWeakIntfPtrOfIPrepStmt).SetTimeStamp(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PZTimeStamp(BindValue.Value)^);
        else raise ZDbcUtils.CreateUnsupportedParameterTypeException(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType);
      end;
    end;
  end;
end;

procedure TZBindList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
  HasOutParam := False;
  HasInOutParam := False;
  HasReturnParam := False;
  HasResultSetParam := False;
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
        ,zbtAnsiString{$ENDIF}: RawByteString(BindValue.Value) := EmptyRaw; //dec refcnt
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
    else raise ZDbcUtils.CreateUnsupportedParameterTypeException(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindValue.SQLType);
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

function TZBindList.HasOutOrInOutOrResultParam: Boolean;
begin
  Result := HasOutParam or HasReturnParam or HasInOutParam or HasResultSetParam;
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
  BindValue := AquireBuffer(Index, stTimeStamp, zbtTimeStamp);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TZTimeStamp));
  PZTimeStamp(BindValue.Value)^ := Value;
end;

procedure TZBindList.Put(Index: Integer; const Value: TBCD);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stBigDecimal, zbtBCD);
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

{$IFDEF FPC} {$PUSH} {$WARN 4056 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
procedure TZBindList.Put(Index: Integer; Value: Boolean);
begin
  AquireBuffer(Index, stBoolean, zbtPointer).Value := Pointer(Byte(Value));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
    else raise ZDbcUtils.CreateUnsupportedParameterTypeException(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stUnknown);
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

procedure TZBindList.SetParamTypes(Index: Integer; SQLType: TZSQLType;
  ParamIO: TZProcedureColumnType);
var BindValue: PZBindValue;
  procedure Convert(Index: Integer; OldSQLType, NewSQLType: TZSQLType);
  var Src, Dest: TZVariant;
    VariantManager: IZVariantManager;
  begin
    if ((BindValue.SQLType = stString) and (NewSQLType = stUnicodeString)) or
       ((BindValue.SQLType = stUnicodeString) and (NewSQLType = stString)) or
       ((BindValue.SQLType = stAsciiStream) and (NewSQLType = stUnicodeStream)) or
       ((BindValue.SQLType = stUnicodeStream) and (NewSQLType = stAsciiStream))
    then Exit;
    VariantManager := TZClientVariantManager.Create(FConSettings);
    Assert(OldSQLType <> NewSQLType);
    Src := GetVariant(Index);
    try
      case NewSQLType of
        stBoolean: Put(Index, VariantManager.GetAsBoolean(Src));
        stShort, stSmall, stInteger, stLong: begin
            Dest := VariantManager.Convert(Src, vtInteger);
            Put(Index, NewSQLType, P8Bytes(@Dest.VInteger));
          end;
        stByte, stWord, stLongWord, stULong: begin
            Dest := VariantManager.Convert(Src, vtUInteger);
            Put(Index, NewSQLType, P8Bytes(@Dest.VUInteger));
          end;
        stFloat, stDouble: begin
            Dest := VariantManager.Convert(Src, vtDouble);
            Put(Index, NewSQLType, P8Bytes(@Dest.VDouble));
          end;
        stCurrency: begin
            Dest := VariantManager.Convert(Src, vtCurrency);
            Put(Index, NewSQLType, P8Bytes(@Dest.VCurrency));
          end;
        stBigDecimal: begin
            Dest := VariantManager.Convert(Src, vtBigDecimal);
            Put(Index, Dest.VBigDecimal);
          end;
        stDate: begin
            Dest := VariantManager.Convert(Src, vtDate);
            Put(Index, Dest.VDate);
          end;
        stTime: begin
            Dest := VariantManager.Convert(Src, vtTime);
            Put(Index, Dest.VTime);
          end;
        stTimeStamp: begin
            Dest := VariantManager.Convert(Src, vtTimeStamp);
            Put(Index, NewSQLType, P8Bytes(@Dest.VTimeStamp));
          end;
        stGUID: begin
            Dest := VariantManager.Convert(Src, vtGUID);
            Put(Index, Dest.VGUID);
          end;
        {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
      end;
    finally
      VariantManager := nil;
    end;
  end;
begin
  if FCount < Index+1 then
    SetCount(Index+1);
  BindValue := Get(Index);
  if (BindValue.SQLType <> SQLType) and (BindValue.SQLType <> stUnknown) then
    Convert(Index, BindValue.SQLType, SQLType);
  BindValue.ParamType := ParamIO;
  case ParamIO of
    pctInOut: HasInOutParam := True;
    pctOut:   HasOutParam := True;
    pctReturn: HasReturnParam := True;
    pctResultSet: HasResultSetParam := True;
    {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
  end;
  BindValue.SQLType := SQLType;
end;

{ TZAbstractPreparedStatement }

{**
  Sets a new parameter capacity and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractPreparedStatement.SetBindCapacity(Capacity: Integer);
begin
  if (FBindList <> nil) and ((Capacity = 0) or (FBindList.Capacity < Capacity)) then
    FBindList.Capacity := Capacity;
end;

function TZAbstractPreparedStatement.ParamterIndex2ResultSetIndex(
  Value: Integer): Integer;
var I: Integer;
begin
  Result := Value;
  CheckParameterIndex(Value);
  if (FOutParamResultSet = nil) or not (BindList.ParamTypes[Result] in [pctInOut..pctResultSet])
  then raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  for i := Result downto 0 do
    if Ord(BindList.ParamTypes[i]) <= Ord(pctIn) then
      Dec(Result);
  if FOutParamResultSet.IsBeforeFirst then
    FOutParamResultSet.Next;
  {$IFNDEF GENERIC_INDEX}Inc(Result);{$ENDIF} //add for the ResultSet columnIndex
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
procedure TZAbstractPreparedStatement.BeforeClose;
begin
  if (FOpenLobStreams<> nil) and (FOpenLobStreams.Count > 0) then
    raise EZSQLException.Create('close all open lobstreams before closing the statement');
  inherited BeforeClose;
  if Prepared then
    Unprepare;
end;

procedure TZAbstractPreparedStatement.BindArray(Index: Integer;
  const Value: TZArray);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, Value, False);
end;

{**
  Binds a binary value
}
procedure TZAbstractPreparedStatement.BindBinary(Index: Integer;
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
procedure TZAbstractPreparedStatement.BindLob(Index: Integer;
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
procedure TZAbstractPreparedStatement.BindDateTime(Index: Integer;
  SQLType: TZSQLType; const Value: TDateTime);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, SQLType, P8Bytes(@Value));
end;

{**
  Binds the input parameters
}
procedure TZAbstractPreparedStatement.BindInParameters;
begin
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcBindPrepStmt,Self);
end;

procedure TZAbstractPreparedStatement.CheckParameterIndex(var Value: Integer);
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
procedure TZAbstractPreparedStatement.ClearParameters;
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
constructor TZAbstractPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
var iPStmt: IZPreparedStatement;
begin
  FSupportsDMLBatchArrays := Connection.GetMetadata.GetDatabaseInfo.SupportsArrayBindings;
  inherited Create(Connection, Info);
  if QueryInterface(IZPreparedStatement, iPStmt) = S_OK then begin
    FWeakIntfPtrOfIPrepStmt := Pointer(iPStmt);
    iPStmt := nil;
  end;
  FBindList := TZBindList.Create(ConSettings);
  FClientCP := ConSettings.ClientCodePage.CP;
  FTokenMatchIndex := -1;
  FOpenLobStreams := TZSortedList.Create;
  {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := SQL;
end;

function TZAbstractPreparedStatement.CreateLogEvent(
  const Category: TZLoggingCategory): TZLoggingEvent;
var
  I: integer;
  LogString : RawByteString;
  SQLWriter: TZRawSQLStringWriter;
begin
  case Category of
    lcBindPrepStmt:
      if (FBindList.Count=0) then
        result := nil
      else begin { Prepare Log Output}
        SQLWriter := TZRawSQLStringWriter.Create(FBindList.Count shl 5);
        LogString := EmptyRaw;
        For I := 0 to FBindList.Count - 1 do begin
          if BindList[I].ParamType = pctOut then
            SQLWriter.AddText('(OUTPARAM)', LogString)
          else if BindList[I].ParamType = pctReturn then
            SQLWriter.AddText('(RETURN_VALUE)', LogString)
          else
            AddParamLogValue(I, SQLWriter, LogString);
          SQLWriter.AddChar(AnsiChar(','), LogString);
        end;
        SQLWriter.CancelLastComma(LogString);
        SQLWriter.Finalize(LogString);
        FreeAndNil(SQLWriter);
        result := CreateStmtLogEvent(Category, Logstring);
     end;
    else result := inherited CreatelogEvent(Category);
  end;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractPreparedStatement.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBindList);
  FreeAndNil(FOpenLobStreams);
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
function TZAbstractPreparedStatement.Execute(
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
function TZAbstractPreparedStatement.ExecutePrepared: Boolean;
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
function TZAbstractPreparedStatement.ExecuteQuery(
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
function TZAbstractPreparedStatement.ExecuteQuery(
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
function TZAbstractPreparedStatement.ExecuteQueryPrepared: IZResultSet;
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
function TZAbstractPreparedStatement.ExecuteUpdate(
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
function TZAbstractPreparedStatement.ExecuteUpdate(
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
function TZAbstractPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract base class - parameters not used intentionally
{$IFNDEF NO_ANSISTRING}
function TZAbstractPreparedStatement.GetAnsiString(
  ParameterIndex: Integer): AnsiString;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetAnsiString(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetAnsiString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
end;
{$ENDIF NO_ANSISTRING}

procedure TZAbstractPreparedStatement.GetBigDecimal(ParameterIndex: Integer;
  var Result: TBCD);
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  fOutParamResultSet.GetBigDecimal(ParamterIndex2ResultSetIndex(ParameterIndex), Result);
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBigDecimal(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
end;

function TZAbstractPreparedStatement.GetBLob(ParameterIndex: Integer): IZBlob;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetBlob(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBlob(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindList.SQLTypes[ParameterIndex], Result)
end;

function TZAbstractPreparedStatement.GetCLob(ParameterIndex: Integer): IZClob;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  fOutParamResultSet.GetBlob(ParamterIndex2ResultSetIndex(ParameterIndex)).QueryInterface(IZCLob, Result);
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBlob(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindList.SQLTypes[ParameterIndex], Result)
end;

function TZAbstractPreparedStatement.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := nil;
end;

function TZAbstractPreparedStatement.GetCurrency(
  ParameterIndex: Integer): Currency;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetCurrency(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetCurrency(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5060 off : Function result does not seem to be set}
  {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
{$ENDIF}
function TZAbstractPreparedStatement.GetDate(
  ParameterIndex: Integer): TDateTime;
var D: TZDate;
begin
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).GetDate(ParameterIndex, D);
  TryDateToDateTime(D, Result);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractPreparedStatement.GetDate(ParameterIndex: Integer;
  var Result: TZDate);
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  fOutParamResultSet.GetDate(ParamterIndex2ResultSetIndex(ParameterIndex), Result);
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDate(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement.GetDouble(ParameterIndex: Integer): Double;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetDouble(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    if BindList.SQLTypes[ParameterIndex] = stFloat
    then IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetFloat(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
    else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement.GetInt(ParameterIndex: Integer): Integer;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetInt(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    case BindList.SQLTypes[ParameterIndex] of
      stShort: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetShort(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stSmall: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetSmall(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stInteger: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetInt(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetLong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
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
procedure TZAbstractPreparedStatement.GetLob(Index: Integer;
  out Result: IZBlob);
begin
  Result := fOutParamResultSet.GetBlob(ParamterIndex2ResultSetIndex(Index));
  if BindList.ParamTypes[Index] = pctInOut then
    BindLob(Index, BindList.SQLTypes[Index], Result);
end;

function TZAbstractPreparedStatement.GetLong(ParameterIndex: Integer): Int64;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetLong(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    case BindList.SQLTypes[ParameterIndex] of
      stShort: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetShort(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stSmall: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetSmall(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stInteger: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetInt(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetLong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
    end;
end;

procedure TZAbstractPreparedStatement.GetPChar(Index: Integer;
  out Buf: Pointer; out Len: LengthInt; CodePage: Word);
var
  L: NativeUInt;
begin
  if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or
     not ConSettings.ClientCodePage.IsStringFieldCPConsistent then begin
    Buf := fOutParamResultSet.GetPWideChar(ParamterIndex2ResultSetIndex(Index), L);
    if BindList.ParamTypes[Index] = pctInOut then begin
      System.SetString(FUniTemp, PWideChar(Buf), L);
      IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUnicodeString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FUniTemp);
    end;
    if CodePage = zCP_UTF16 then begin
      Len := L;
      if Len = 0 then
        Buf := PEmptyUnicodeString;
    end else begin
      FRawTemp := PUnicodeToRaw(Buf, L, CodePage);
      Len := Length(FRawTemp);
      if Len = 0
      then Buf := PEmptyAnsiString
      else Buf := Pointer(FRawTemp);
    end;
  end else begin
    Buf := fOutParamResultSet.GetPAnsiChar(ParamterIndex2ResultSetIndex(Index), L);
    Len := L;
    if BindList.ParamTypes[Index] = pctInOut then begin
      ZSetString(Buf, l, fRawTemp);
      IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetRawByteString(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, FRawTemp);
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

function TZAbstractPreparedStatement.GetRawByteString(
  ParameterIndex: Integer): RawByteString;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetRawByteString(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetRawByteString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
end;

function TZAbstractPreparedStatement.GetResultSet: IZResultSet;
begin
  {EH: Improvement as long the IZPreparedResultSet does not export the Getter API}
  Result := inherited GetResultSet;
  if (Result = nil) and (FOutParamResultSet <> nil) then
    Result := FOutParamResultSet;
end;

{**
  Gets the value of a JDBC <code>BIT</code> parameter as a <code>boolean</code>
  in the Java programming language.
  @param Index the first parameter is 1, the second is 2, and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>false</code>.
}
function TZAbstractPreparedStatement.GetBoolean(
  ParameterIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetBoolean(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBoolean(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement.GetByte(ParameterIndex: Integer): Byte;
begin
  Result := IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).GetUInt(ParameterIndex);
end;

function TZAbstractPreparedStatement.GetBytes(ParameterIndex: Integer): TBytes;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetBytes(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBytes(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement.GetFloat(ParameterIndex: Integer): Single;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetFloat(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetFloat(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

procedure TZAbstractPreparedStatement.GetGUID(ParameterIndex: Integer;
  var Result: TGUID);
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  fOutParamResultSet.GetGUID(ParamterIndex2ResultSetIndex(ParameterIndex), Result);
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetGUID(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement.GetTime(
  ParameterIndex: Integer): TDateTime;
var T: TZTime;
begin
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).GetTime(ParameterIndex, T{%H-});
  TryTimeToDateTime(T, Result{%H-});
end;

function TZAbstractPreparedStatement.GetUInt(ParameterIndex: Integer): Cardinal;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetUInt(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    case BindList.SQLTypes[ParameterIndex] of
      stByte: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetByte(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stWord: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetWord(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stLongWord: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUInt(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetULong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    end;
end;

function TZAbstractPreparedStatement.GetULong(ParameterIndex: Integer): UInt64;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetULong(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    case BindList.SQLTypes[ParameterIndex] of
      stByte: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetByte(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stWord: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetWord(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      stLongWord: IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUInt(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetULong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
    end;
end;

function TZAbstractPreparedStatement.GetUnicodeString(
  ParameterIndex: Integer): ZWideString;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetUnicodeString(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUnicodeString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
end;

{$IFNDEF NO_UTF8STRING}
function TZAbstractPreparedStatement.GetUTF8String(
  ParameterIndex: Integer): UTF8String;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetUTF8String(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUTF8String(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result)
end;
{$ENDIF NO_UTF8STRING}

function TZAbstractPreparedStatement.GetValue(
  ParameterIndex: Integer): TZVariant;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetValue(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetValue(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement.GetWord(ParameterIndex: Integer): Word;
begin
  Result := IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).GetUInt(ParameterIndex);
end;

{**
  get the current SQL string
}
function TZAbstractPreparedStatement.GetShort(
  ParameterIndex: Integer): ShortInt;
begin
  Result := IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).GetInt(ParameterIndex);
end;

function TZAbstractPreparedStatement.GetSmall(
  ParameterIndex: Integer): SmallInt;
begin
  Result := IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).GetInt(ParameterIndex)
end;

function TZAbstractPreparedStatement.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

function TZAbstractPreparedStatement.GetString(ParameterIndex: Integer): String;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.GetString(ParamterIndex2ResultSetIndex(ParameterIndex));
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

procedure TZAbstractPreparedStatement.GetTime(ParameterIndex: Integer;
  var Result: TZTime);
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  fOutParamResultSet.GetTime(ParamterIndex2ResultSetIndex(ParameterIndex), Result);
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTime(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement.GetTimeStamp(
  ParameterIndex: Integer): TDateTime;
var TS: TZTimeStamp;
begin
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).GetTimeStamp(ParameterIndex, TS{%H-});
  TryTimeStampToDateTime(TS, Result{%H-});
end;

procedure TZAbstractPreparedStatement.GetTimeStamp(ParameterIndex: Integer;
  var Result: TZTimeStamp);
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  fOutParamResultSet.GetTimestamp(ParamterIndex2ResultSetIndex(ParameterIndex), Result);
  if (BindList.ParamTypes[ParameterIndex] = pctInOut) and not FSupportsBidirectionalParamIO then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTimestamp(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
end;

{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Indicates whether or not the specified OUT parameter read had the value of
  SQL <code>NULL</code>.
  @param Index the first parameter is 0, the second is 1, ...
  @return <code>true</code> if the parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractPreparedStatement.IsNull(ParameterIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  Result := fOutParamResultSet.IsNull(ParamterIndex2ResultSetIndex(ParameterIndex));
  if Result and (BindList.ParamTypes[ParameterIndex] = pctInOut) then
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNull(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindList.SQLTypes[ParameterIndex]);
end;

function TZAbstractPreparedStatement.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

{**
  Logs a message about prepared statement event with normal result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZAbstractPreparedStatement.LogPrepStmtMessage(
  Category: TZLoggingCategory; const Msg: RawByteString);
var SQLWriter: TZRawSQLStringWriter;
  LogMsg: RawByteString;
begin
  if DriverManager.HasLoggingListener then begin
    SQLWriter := TZRawSQLStringWriter.Create(30+Length(Msg));
    LogMsg := EmptyRaw;
    SQLWriter.AddText('Statement ', LogMsg);
    SQLWriter.AddOrd(FStatementId, LogMsg);
    if msg <> EmptyRaw then begin
      SQLWriter.AddText(' : ', LogMsg);
      SQLWriter.AddText(Msg, LogMsg);
    end;
    SQLWriter.Finalize(LogMsg);
    FreeAndNil(SQLWriter);
    DriverManager.LogMessage(Category, ConSettings^.Protocol, LogMsg);
  end;
end;

{**
  prepares the statement on the server if minimum execution
  count have been reached
}
procedure TZAbstractPreparedStatement.Prepare;
begin
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcPrepStmt,Self);
  PrepareInParameters;
  FPrepared := True;
  FClosed := False;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement.PrepareInParameters;
begin
end;

procedure TZAbstractPreparedStatement.PrepareLastResultSetForReUse;
begin
  inherited PrepareLastResultSetForReUse;
  if Assigned(FOutParamResultSet) and (Pointer(FOutParamResultSet) <> FOpenResultSet) then
    if not FOutParamResultSet.IsClosed and
       (FOutParamResultSet.GetConcurrency = GetResultSetConcurrency) and
       (FOutParamResultSet.GetFetchDirection = GetFetchDirection) then
      FOutParamResultSet.ResetCursor
    else begin
      FOutParamResultSet.Close;
      FOpenResultSet := nil;
    end;
end;

procedure TZAbstractPreparedStatement.PrepareOpenResultSetForReUse;
begin
  inherited PrepareOpenResultSetForReUse;
  if Assigned(FOutParamResultSet) and (Pointer(FOutParamResultSet) <> FOpenResultSet) then
    if not FOutParamResultSet.IsClosed and
       (FOutParamResultSet.GetConcurrency = GetResultSetConcurrency) and
       (FOutParamResultSet.GetFetchDirection = GetFetchDirection) then
      FOutParamResultSet.ResetCursor
    else begin
      FOutParamResultSet.Close;
      FOpenResultSet := nil;
    end;
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
  @param parameterIndex the first parameter is 1, the second is 2, and so on
  @param sqlType the JDBC type code defined by <code>java.sql.Types</code>.
  @param ParamType the ParamDirection type defined by <code>TZProcedureColumnType</code>.
  @param Name the name of the parameter
  @param PrecisionOrSize If the parameter is of JDBC type <code>NUMERIC</code>
    or <code>DECIMAL</code>, it's the Precision, for <code>STRINGS</code> or
    <code>BYTES</code> it's the size of the Parameter and for all other fixed
    size parameters the value is ignored.
  @param Scale If the parameter is of JDBC type <code>NUMERIC</code>
    or <code>DECIMAL</code>, it's the number of decimal digit's for Time/TimeStamp values
    it's the number of the fractional second part. For all other Types the value
    is ignored.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract method - parameters not used intentionally
procedure TZAbstractPreparedStatement.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZProcedureColumnType; const Name: String = '';
  PrecisionOrSize: LengthInt = 0; Scale: LengthInt = 0);
begin
  BindList.SetParamTypes(ParameterIndex , SQLType, ParamType);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZAbstractPreparedStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
var ImmediatelyReleasable: IImmediatelyReleasable;
begin
  FPrepared := False;
  if (FOutParamResultSet <> nil) then begin
    if (Pointer(FOutParamResultSet) = FOpenResultSet) then
      FOpenResultSet := nil;
    if Supports(FOutParamResultSet, IImmediatelyReleasable, ImmediatelyReleasable) then begin
      ImmediatelyReleasable.ReleaseImmediat(Sender, AError);
      ImmediatelyReleasable := nil;
    end;
    FOutParamResultSet := nil;
  end;
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
procedure TZAbstractPreparedStatement.SetAsciiStream(ParameterIndex: Integer;
  const Value: TStream);
var
  CLob: IZCLob; //use a local variable for the FPC
  CP: Word;
begin
  if Value = nil then begin
    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNull(ParameterIndex, stAsciiStream);
    Exit;
  end;
  if ConSettings^.ClientCodePage.Encoding = ceUTF16
  then CP := ConSettings^.CTRL_CP
  else CP := ConSettings^.ClientCodePage^.CP;
  CLob := TZLocalMemCLob.Create(CP, ConSettings, FOpenLobStreams);

  if ConSettings^.AutoEncode then
     CP := zCP_NONE;
  Clob.SetStream(Value, CP);
  SetBlob(ParameterIndex, stAsciiStream, Clob);
end;

procedure TZAbstractPreparedStatement.SetASQL(const Value: RawByteString);
begin
  if Value <> FASQL then begin
    if Prepared then
      UnPrepare;
    inherited SetASQL(Value);
  end;
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
procedure TZAbstractPreparedStatement.SetBinaryStream(ParameterIndex: Integer;
  const Value: TStream);
var Blob: IZBlob;
begin
  Blob := TZLocalMemBLob.CreateWithStream(Value, FOpenLobStreams);
  SetBlob(ParameterIndex, stBinaryStream, Blob);
end;

procedure TZAbstractPreparedStatement.SetBlob(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: IZBlob);
begin
  BindLob(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SQLType, Value)
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
  BindBinary(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBytes, Pointer(Value), Length(Value));
end;

procedure TZAbstractPreparedStatement.SetDataArray(Index: Integer;
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
    raise EZUnsupportedException.Create(SUnsupportedOperation);
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
var D: TZDate;
begin
  ZSysUtils.DecodeDateTimeToDate(Value, D{%H-});
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDate(ParameterIndex, D);
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetGUID(ParameterIndex: Integer;
  const Value: TGUID);
begin
  BindBinary(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stGUID, @Value.D1,
    SizeOf(TGUID));
end;

procedure TZAbstractPreparedStatement.AddParamLogValue(ParamIndex: Integer;
  SQLWriter: TZRawSQLStringWriter; var Result: RawByteString);
var BindValue: PZBindValue;
begin
  BindValue := FBindList[ParamIndex];
  case BindValue.BindType of
    zbt4Byte: case BindValue.SQLType of
                stBoolean: if BindValue.Value <> nil
                    then SQLWriter.AddText('(TRUE)', Result)
                    else SQLWriter.AddText('(FALSE)', Result);
                stByte, stWord, stLongWord:
                    SQLWriter.AddOrd(PCardinal(@BindValue.Value)^, Result);
                stShort, stSmall, stInteger:
                  SQLWriter.AddOrd(PInteger(@BindValue.Value)^, Result);
                stFloat:
                  SQLWriter.AddFloat(PSingle(@BindValue.Value)^, Result);
                else SQLWriter.AddText('(NULL)', Result)
              end;
    zbt8Byte: case BindValue.SQLType of
                stBoolean: if PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^ <> 0
                    then SQLWriter.AddText('(TRUE)', Result)
                    else SQLWriter.AddText('(FALSE)', Result);
                stByte, stWord, stLongWord, stULong:
                    SQLWriter.AddOrd(PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, Result);
                stShort, stSmall, stInteger, stLong:
                    SQLWriter.AddOrd(PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, Result);
                stCurrency:
                    SQLWriter.AddDecimal(PCurrency({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, Result);
                stFloat, stDouble:
                    SQLWriter.AddDecimal(PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, Result);
                stTime:
                    SQLWriter.AddTime(PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, ConSettings.WriteFormatSettings.TimeFormat, Result);
                stDate:
                    SQLWriter.AddDate(PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, ConSettings.WriteFormatSettings.DateFormat, Result);
                else
                    SQLWriter.AddDateTime(PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^, ConSettings.WriteFormatSettings.DateTimeFormat, Result);
              end;
    {$IFNDEF NO_UTF8STRING}zbtUTF8String,{$ENDIF}
    {$IFNDEF NO_ANSISTRING}zbtAnsiString,{$ENDIF}
    zbtRawString: SQLWriter.AddText(SQLQuotedStr(RawByteString(BindValue.Value), AnsiChar(#39)), Result);
    zbtBCD:       SQLWriter.AddDecimal(PBCD(BindValue.Value)^, Result);
    zbtUniString: SQLWriter.AddText(SQLQuotedStr(ZUnicodeToRaw(UnicodeString(BindValue.Value), FClientCP), AnsiChar(#39)), Result);
    zbtCharByRef: if PZCharRec(BindValue.Value)^.CP = zCP_UTF16
                  then SQLWriter.AddText(SQLQuotedStr(PUnicodeToRaw(PZCharRec(BindValue.Value)^.P, PZCharRec(BindValue.Value)^.Len, FClientCP), AnsiChar(#39)), Result)
                  else SQLWriter.AddText(SQLQuotedStr(PAnsiChar(PZCharRec(BindValue.Value)^.P), PZCharRec(BindValue.Value)^.Len, AnsiChar(#39)), Result);
    zbtBinByRef:  SQLWriter.AddHexBinary(PZBufRec(BindValue.Value)^.Buf, PZBufRec(BindValue.Value)^.Len, False, Result);
    zbtGUID:      SQLWriter.AddGUID(PGUID(BindValue.Value)^, [guidWithBrackets, guidQuoted], Result);
    zbtBytes:     SQLWriter.AddHexBinary(TBytes(BindValue.Value), False, Result);
    zbtArray:     SQLWriter.AddText('(ARRAY)', Result);
    zbtLob:       if BindValue.SQLType = stbinaryStream
                  then SQLWriter.AddText('(BLOB)', Result)
                  else SQLWriter.AddText('(CLOB)', Result);
    zbtPointer:   SQLWriter.AddText('(POINTER)', Result);
    zbtNull:      SQLWriter.AddText('(NULL)', Result);
    zbtDate:      SQLWriter.AddDate(PZDate(BindValue.Value)^, ConSettings.WriteFormatSettings.DateFormat, Result);
    zbtTime:      SQLWriter.AddTime(PZTime(BindValue.Value)^, ConSettings.WriteFormatSettings.TimeFormat, Result);
    zbtTimeStamp: SQLWriter.AddTimeStamp(PZTimeStamp(BindValue.Value)^, ConSettings.WriteFormatSettings.DateTimeFormat, Result);
    else          SQLWriter.AddText('(CUSTOM)', Result);
  end;
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractPreparedStatement.SetParamCount(NewParamCount: Integer);
begin
  if (NewParamCount = 0) or (NewParamCount > FBindList.Capacity) then
    SetBindCapacity(NewParamCount);
  FBindList.SetCount(NewParamCount);
end;

procedure TZAbstractPreparedStatement.SetNullArray(ParameterIndex: Integer;
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
    raise EZUnsupportedException.Create(SUnsupportedOperation);
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
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUnicodeString(ParameterIndex, Value);
  {$ELSE}
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetRawByteString(ParameterIndex, Value);
  {$ENDIF}
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
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTime(ParameterIndex: Integer;
  const Value: TDateTime);
var T: TZTime;
begin
  ZSysUtils.DecodeDateTimeToTime(Value, T{%H-});
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTime(ParameterIndex, T)
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TZTimeStamp);
var DT: TDateTime;
begin
  if TryTimeStampToDateTime(Value, DT{%H-})
  then IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTimeStamp(ParameterIndex, DT)
  else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNull(ParameterIndex, stTimeStamp);
end;

procedure TZAbstractPreparedStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TDateTime);
var TS: TZTimeStamp;
begin
  ZSysUtils.DecodeDateTimeToTimeStamp(Value, TS{%H-});
  IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTimeStamp(ParameterIndex, TS)
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
procedure TZAbstractPreparedStatement.SetUnicodeStream(ParameterIndex: Integer;
  const Value: TStream);
begin
  if Value = nil
  then IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNull(ParameterIndex, stUnicodeStream)
  else SetBlob(ParameterIndex, stUnicodeStream, TZLocalMemCLob.CreateWithStream(Value, zCP_UTF16, ConSettings, FOpenLobStreams));
end;

procedure TZAbstractPreparedStatement.SetValue(ParameterIndex: Integer;
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
    vtAnsiString:    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetAnsiString(ParameterIndex, Value.VRawByteString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String:    IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetUTF8String(ParameterIndex, Value.VRawByteString);
    {$ENDIF}
    vtCharRec:       IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetCharRec(ParameterIndex, Value.VCharRec);
    vtDate:          IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetDate(ParameterIndex, Value.VDate);
    vtDateTime:      IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTimestamp(ParameterIndex, Value.VDateTime);
    vtTime:          IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTime(ParameterIndex, Value.VTime);
    vtTimeStamp:     IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetTimeStamp(ParameterIndex, Value.VTimeStamp);
    vtBytes:         IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetBytes(ParameterIndex, {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}StrToBytes{$ENDIF}(Value.VRawByteString));
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
        raise EZUnsupportedException.Create(sUnsupportedOperation);
    else IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetNull(ParameterIndex, stUnknown);
  end;
end;

procedure TZAbstractPreparedStatement.SetWSQL(const Value: ZWideString);
begin
  if Value <> FWSQL then begin
    if Prepared then
      Unprepare;
    inherited SetWSQL(Value);
  end;
end;

{**
  unprepares the statement, deallocates all bindings and handles
}
procedure TZAbstractPreparedStatement.Unprepare;
var RefCountAdded: Boolean;
begin
  RefCountAdded := (RefCount = 1) and (Assigned(FOpenResultSet) or Assigned(FLastResultSet));
  if RefCountAdded then
    _AddRef;
  UnPrepareInParameters;
  FPrepared := False;
  FBatchDMLArrayCount := 0;
  { closing the pending resultset automaticaly nils the Interface/Pointer addresses }
  if Assigned(FLastResultSet) then
    FLastResultSet.Close;
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close;
  if Assigned(FOutParamResultSet) then begin
    IZResultSet(FOutParamResultSet).Close;
    FOutParamResultSet := nil;
  end;
  if RefCountAdded then begin
    if (RefCount = 1) then
      DriverManager.AddGarbage(Self);
    _Release; //possible running into destructor now if just a ResultSet was last owner of Self-interface
  end;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement.UnPrepareInParameters;
begin
  SetBindCapacity(0);
end;

procedure TZAbstractPreparedStatement.ValidateArraySizeAndType(
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
      {$IF declared(vtSynRawUTF8Array)}vtSynRawUTF8Array,{$IFEND}
      vtRawByteString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stUnicodeString: if not (VariantType in [vtUnicodeString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stArray, stDataSet:
          raise EZUnsupportedException.Create(sUnsupportedOperation);
    {$IFDEF WITH_CASE_WARNING}else ;{$ENDIF}
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

procedure TZBindList.Put(Index: Integer; const Value: TZDate);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stDate, zbtDate);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TZDate));
  PZDate(BindValue.Value)^ := Value;
end;

procedure TZBindList.Put(Index: Integer; const Value: TZTime);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stTime, zbtTime);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TZTime));
  PZTime(BindValue.Value)^ := Value;
end;

{ TZRawPreparedStatement }

procedure TZRawPreparedStatement.BindRawStr(Index: Integer;
  const Value: RawByteString);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, stString, Value, FClientCP)
end;

procedure TZRawPreparedStatement.BindLob(Index: Integer; SQLType: TZSQLType;
  const Value: IZBlob);
begin
  inherited BindLob(Index, SQLType, Value);
  if (Value <> nil) and (SQLType in [stAsciiStream, stUnicodeStream]) then begin
    if Value.IsClob
    then Value.SetCodePageTo(FClientCP)
    else PIZLob(BindList[Index].Value)^ := CreateRawCLobFromBlob(Value, ConSettings, FOpenLobStreams);
    BindList[Index].SQLType := stAsciiStream;
  end;
end;

procedure TZRawPreparedStatement.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
begin
  CheckParameterIndex(Index);
  if Buf <> nil
  then FBindList.Put(Index, stString, Buf, Len, FClientCP)
  else FBindList.Put(Index, stString, PEmptyAnsiString, 0, FClientCP)
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.
  The codepage is equal to the raw codepage of the operating system.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZRawPreparedStatement.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  if (ZOSCodePage = FClientCP)
  then BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value)
  else begin
    fUniTemp := PRawToUnicode(Pointer(Value), Length(Value), ZOSCodePage);
    SetUnicodeString(ParameterIndex, fUniTemp);
  end;
end;
{$ENDIF}

{**
  Sets the designated parameter to <code>TZCharRec</code> value.
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
  if (Value.CP = FClientCP) then
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value.P, Value.Len)
  else if Value.CP = zCP_UTF16 then
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
      PUnicodeToRaw(Value.P, Value.Len, FClientCP))
  else begin
    UniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
      ZUnicodeToRaw(UniTemp, FClientCP))
  end;
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
  Sets the designated parameter to a TZCharRec value.
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
    ZUnicodetoRaw(Value, FClientCP));
  {$ELSE}
  BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ConSettings^.ConvFuncs.ZStringtoRaw(Value, ConSettings^.Ctrl_CP,
    FClientCP));
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
    ZUnicodetoRaw(Value, FClientCP));
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
  if (zCP_UTF8 = FClientCP)
  then BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value)
  else begin
    ZEncoding.PRawToRawConvert(Pointer(Value), Length(Value), zCP_UTF8, FClientCP, fRawTemp);
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, fRawTemp);
  end;
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
  if (Value.CP = zCP_UTF16) then
    BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value.P, Value.Len)
  else
    BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, PRawToUnicode(Value.P, Value.Len, Value.CP))
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
  SQLWriter: TZRawSQLStringWriter;
begin
  if FCachedQueryRaw = nil then begin
    FCachedQueryRaw := ZDbcUtils.TokenizeSQLQueryRaw(SQL, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected,
      GetCompareFirstKeywordStrings, FTokenMatchIndex);
    FCountOfQueryParams := 0;
    Result := EmptyRaw; //init Result
    SQLWriter := TZRawSQLStringWriter.Create(Length(SQL));
    for I := 0 to High(FCachedQueryRaw) do begin
      SQLWriter.AddText(FCachedQueryRaw[i], Result);
      Inc(FCountOfQueryParams, Ord(FIsParamIndex[i]));
    end;
    SQLWriter.Finalize(Result);
    FreeAndNil(SQLWriter);
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
  SQLWriter: TZUnicodeSQLStringWriter;
begin
  if Length(FCachedQueryUni) = 0 then begin
    FCachedQueryUni := ZDbcUtils.TokenizeSQLQueryUni(SQL, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected, GetCompareFirstKeywordStrings, FTokenMatchIndex);
    FCountOfQueryParams := 0;
    Result := ''; //init Result
    SQLWriter := TZUnicodeSQLStringWriter.Create(Length(SQL));
    for I := 0 to High(FCachedQueryUni) do begin
      SQLWriter.AddText(FCachedQueryUni[i], Result);
      Inc(FCountOfQueryParams, Ord(FIsParamIndex[i]));
    end;
    SQLWriter.Finalize(Result);
    FreeAndNil(SQLWriter);
    SetBindCapacity(FCountOfQueryParams);
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

{ TZAbstractCallableStatement }

procedure TZAbstractCallableStatement.BindDouble(Index: Integer;
  SQLType: TZSQLType; const Value: Double);
var Bind: PZBindValue;
begin
  CheckParameterIndex(Index);
  Bind := FBindList[Index];
  {Registered Param ? }
  if (Bind.SQLType <> SQLType) and (Bind.ParamType <> pctUnknown) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    case Bind.SQLType of
      stBoolean: FBindList.Put(Index, Value <> 0);
      stByte, stWord, stLongWord, stShort, stSmall, stInteger,
      stLong, stULong: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Trunc(Value));
      stFloat, stDouble, stDate, stTime, stTimeStamp: BindList.Put(Index, Bind.SQLType, P8Bytes(@Value));
      stCurrency: SetCurrency(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stBigDecimal: begin
          ZSysUtils.Double2BCD(Value, fBCDTemp);
          BindList.Put(Index, fBCDTemp);
        end;
      else FBindList.Put(Index, SQLType, P8Bytes(@Value));
    end;
  end else FBindList.Put(Index, SQLType, P8Bytes(@Value));
end;

{**
  Binds the input parameters
}
procedure TZAbstractCallableStatement.BindInParameters;
begin
  Bindlist.BindValuesToStatement(FExecStatement);
end;

procedure TZAbstractCallableStatement.BindSignedOrdinal(Index: Integer;
  SQLType: TZSQLType; Value: NativeInt);
var Bind: PZBindValue;
begin
  CheckParameterIndex(Index);
  Bind := FBindList[Index];
  {Registered Param ? }
  if (Bind.SQLType <> SQLType) and (Bind.ParamType <> pctUnknown) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    case Bind.SQLType of
      stBoolean: FBindList.Put(Index, Value <> 0);
      stByte, stWord, stLongWord: BindUnsignedOrdinal(Index, Bind.SQLType, Value);
      stShort, stSmall, stInteger{$IFDEF CPU64},stLong{$ENDIF}: FBindList.Put(Index, Bind.SQLType, {$IFDEF CPU64}P8Bytes{$ELSE}P4Bytes{$ENDIF}(@Value));
      {$IFNDEF CPU64}
      stLong: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      {$ENDIF CPU64}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      stULong: SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      stFloat, stDouble, stDate, stTime, stTimeStamp: SetDouble(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stCurrency: SetCurrency(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stBigDecimal: begin
          ZSysUtils.ScaledOrdinal2Bcd(Value, 0, fBCDTemp);
          BindList.Put(Index, fBCDTemp);
        end;
      else FBindList.Put(Index, SQLType, P4Bytes(@Value));
    end;
  end else FBindList.Put(Index, SQLType, P4Bytes(@Value));
end;

procedure TZAbstractCallableStatement.BindUnsignedOrdinal(Index: Integer;
  SQLType: TZSQLType; Value: NativeUInt);
var Bind: PZBindValue;
begin
  CheckParameterIndex(Index);
  Bind := FBindList[Index];
  {Registered Param ? }
  if (Bind.SQLType <> SQLType) and (Bind.ParamType <> pctUnknown) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    case Bind.SQLType of
      stBoolean: FBindList.Put(Index, Value <> 0);
      stByte, stWord, stLongWord{$IFDEF CPU64},stULong{$ENDIF}: FBindList.Put(Index, Bind.SQLType, {$IFDEF CPU64}P8Bytes{$ELSE}P4Bytes{$ENDIF}(@Value));
      stShort, stSmall, stInteger: BindSignedOrdinal(Index, Bind.SQLType, Value);
      stLong: SetLong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      {$IFNDEF CPU64}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      stULong: SetULong(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      {$ENDIF}
      stFloat, stDouble, stDate, stTime, stTimeStamp: SetDouble(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stCurrency: SetCurrency(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stBigDecimal: begin
          ZSysUtils.ScaledOrdinal2Bcd(Value, 0, fBCDTemp, False);
          BindList.Put(Index, fBCDTemp);
        end;
      else FBindList.Put(Index, SQLType, P4Bytes(@Value));
    end;
  end else FBindList.Put(Index, SQLType, P4Bytes(@Value));
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZAbstractCallableStatement.BOR: Boolean;
begin
  Result := (FActiveResultIndex = 0) or not Supports(FResults[FActiveResultIndex], IZResultSet);
end;

procedure TZAbstractCallableStatement.CheckParameterIndex(var Value: Integer);
begin
  inherited CheckParameterIndex(Value);
  PrepareInParameters;
end;

procedure TZAbstractCallableStatement.CopyCallResults;
begin
  if (FResultSetType <> rtForwardOnly) then
    while FExecStatement.GetMoreResults do
      if (FExecStatement.FLastResultSet <> nil) then begin
        if not Supports(FExecStatement.FLastResultSet, IZVirtualResultSet)
        then FResults.Add(Connection.GetMetadata.CloneCachedResultSet(FExecStatement.FLastResultSet))
        else FResults.Add(FExecStatement.FLastResultSet);
        FExecStatement.FLastResultSet := nil;
        FExecStatement.FOpenResultSet := nil;
      end else
        FResults.Add(TZAnyValue.CreateWithInteger(FExecStatement.LastUpdateCount));
end;

constructor TZAbstractCallableStatement.Create(const Connection: IZConnection;
  const StoredProcOrFuncIdentifier: string; {$IFDEF AUTOREFCOUNT}const{$ENDIF}Info: TStrings);
begin
  inherited Create(Connection, '', Info);
  FStoredProcName := StoredProcOrFuncIdentifier;
  FResults := TZCollection.Create;
end;

{**
  Last ResultSet?
  @result <code>True</code> if so
}
function TZAbstractCallableStatement.EOR: Boolean;
var I: Integer;
begin
  Result := (FActiveResultIndex = FResults.Count -1);
  if not Result then begin
    Result := True;
    for i := FResults.Count -1 downto FActiveResultIndex+1 do
      if Supports(FResults[I], IZResultSet) then begin
        Result := False;
        Break;
      end;
  end;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 5033 off : Function result does not seem to be set}
{$ENDIF} // abstract base class - parameters not used intentionally
function TZAbstractCallableStatement.Execute(const SQL: ZWideString): Boolean;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

function TZAbstractCallableStatement.Execute(
  const SQL: RawByteString): Boolean;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractCallableStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  BindInParameters;
  Result := FExecStatement.ExecutePrepared;
  if Result then begin
    if (FResultSetType <> rtForwardOnly) then
      if not Supports(FExecStatement.FLastResultSet, IZVirtualResultSet)
      then FResults.Add(Connection.GetMetadata.CloneCachedResultSet(FExecStatement.FLastResultSet))
      else FResults.Add(FExecStatement.FLastResultSet);
    FLastUpdateCount := -1;
  end else
    FLastUpdateCount := FExecStatement.LastUpdateCount;
  CopyCallResults;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 5033 off : Function result does not seem to be set}
{$ENDIF} // abstract base class - parameters not used intentionally
function TZAbstractCallableStatement.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

function TZAbstractCallableStatement.ExecuteQuery(
  const SQL: ZWideString): IZResultSet;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  BindInParameters;
  if FExecStatement.ExecutePrepared then begin
    FLastUpdateCount := -1;
    Result := FExecStatement.FLastResultSet;
    if (FResultSetType <> rtForwardOnly) then begin
      if not Supports(Result, IZVirtualResultSet) then
        Result := Connection.GetMetadata.CloneCachedResultSet(FExecStatement.FLastResultSet);
      FResults.Add(Result);
    end;
    FExecStatement.FLastResultSet := nil;
  end else begin
    FLastUpdateCount := FExecStatement.LastUpdateCount;
    if (GetResultSetType <> rtForwardOnly) then
      FResults.Add(TZAnyValue.CreateWithInteger(FLastUpdateCount));
    Result := nil;
  end;
  CopyCallResults;
  if (Result = nil) and (FExecStatement.FOutParamResultSet <> nil) then
    Result := FExecStatement.FOutParamResultSet;
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 5033 off : Function result does not seem to be set}
{$ENDIF} // abstract base class - parameters not used intentionally
function TZAbstractCallableStatement.ExecuteUpdate(
  const SQL: RawByteString): Integer;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;

function TZAbstractCallableStatement.ExecuteUpdate(
  const SQL: ZWideString): Integer;
begin
  Raise EZUnsupportedException.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZAbstractCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  BindInParameters;
  if FExecStatement.ExecutePrepared then begin
    if FResultSetType <> rtForwardOnly then begin
      FResults.Add(Connection.GetMetadata.CloneCachedResultSet(FExecStatement.FLastResultSet));
      FExecStatement.FLastResultSet.ResetCursor;
    end{ else
      FLastResultSet := FExecStatement.FLastResultSet};
    Result := -1;
  end else
    Result := FExecStatement.LastUpdateCount;
  FLastUpdateCount := Result;
  CopyCallResults;
end;

procedure TZAbstractCallableStatement.GetBigDecimal(ParameterIndex: Integer;
  var Result: TBCD);
begin
  if FExecStatement <> nil then begin
    FExecStatement.GetBigDecimal(ParameterIndex, Result);
    if (BindList.ParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = pctInOut) then
      SetBigDecimal(ParameterIndex, Result);
  end else begin
    {$IFDEF FPC}Result := NullBCD;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetBoolean(
  ParameterIndex: Integer): Boolean;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetBoolean(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      BindSignedOrdinal(ParameterIndex, FExecStatement.BindList[ParameterIndex].SQLType, Ord(Result));
  end else begin
    {$IFDEF FPC}Result := False; {$ENDIF}//satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetBytes(ParameterIndex: Integer): TBytes;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetBytes(ParameterIndex);
    if (BindList.ParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = pctInOut) then
      SetBytes(ParameterIndex, Result);
  end else begin
    {$IFDEF FPC}Result := nil;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetCurrency(
  ParameterIndex: Integer): Currency;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetCurrency(ParameterIndex);
    if (BindList.ParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = pctInOut) then
      SetCurrency(ParameterIndex, Result);
  end else begin
    {$IFDEF FPC}Result := 0;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

procedure TZAbstractCallableStatement.GetDate(
  ParameterIndex: Integer; var Result: TZDate);
begin
  if FExecStatement <> nil then begin
    FExecStatement.GetDate(ParameterIndex, Result);
    if (BindList.ParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = pctInOut) then
      SetDate(ParameterIndex, Result);
  end else begin
    {$IFDEF FPC} FillChar(Result, SizeOf(TZDate), #0);{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetDouble(
  ParameterIndex: Integer): Double;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetDouble(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      BindDouble(ParameterIndex, FExecStatement.BindList[ParameterIndex].SQLType, Result);
  end else begin
    {$IFDEF FPC}Result := 0;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

{**
  Get the first resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetFirstResultSet: IZResultSet;
var I: Integer;
begin
  Result := nil;
  if FResults.Count > 0 then
    for I := 0 to FResults.Count -1 do
      if Supports(FResults[I], IZResultSet, Result) then begin
        FActiveResultIndex := I;
        Break;
      end;
  if (Result <> nil) and (Result.GetType <> rtForwardOnly) then
    Result.BeforeFirst;
end;

function TZAbstractCallableStatement.GetFloat(ParameterIndex: Integer): Single;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetFloat(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      BindDouble(ParameterIndex, FExecStatement.BindList[ParameterIndex].SQLType, Result);
  end else begin
    {$IFDEF FPC}Result := 0;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
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
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetInt(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      BindSignedOrdinal(ParameterIndex, FExecStatement.BindList[ParameterIndex].SQLType, Result);
  end else begin
    {$IFDEF FPC}Result := 0;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

{**
  Get the last resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetLastResultSet: IZResultSet;
var I: Integer;
begin
  Result := nil;
  if FResults.Count > 0 then
    for I := FResults.Count -1 downto 0 do
      if Supports(FResults[I], IZResultSet, Result) then begin
        FActiveResultIndex := I;
        Break;
      end;
  if (Result <> nil) and (Result.GetType <> rtForwardOnly) then
    Result.BeforeFirst;
end;

procedure TZAbstractCallableStatement.GetLob(Index: Integer;
  out Result: IZBlob);
begin
  if FExecStatement <> nil then begin
    FExecStatement.GetLob(Index, Result);
    if (BindList.ParamTypes[Index] = pctInOut) then
      SetBlob(Index{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, BindList.SQLTypes[Index], Result);
  end else begin
    Result := nil; //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetLong(ParameterIndex: Integer): Int64;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetLong(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      {$IFDEF CPU64}
      BindSignedOrdinal(ParameterIndex, FExecStatement.BindList[ParameterIndex].SQLType, Result);
      {$ELSE}
      SetLong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      {$ENDIF}
  end else begin
    {$IFDEF FPC}Result := 0;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetMoreResults: Boolean;
begin
  Result := FActiveResultIndex < FResults.Count;
end;

{**
  Get the next resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetNextResultSet: IZResultSet;
var I: Integer;
begin
  Result := nil;
  if FResults.Count > 0 then
    for I := FActiveResultIndex+1 to FResults.Count -1 do
      if Supports(FResults[I], IZResultSet, Result) then begin
        FActiveResultIndex := I;
        Break;
      end;
  if (Result <> nil) and (Result.GetType <> rtForwardOnly) then
    Result.BeforeFirst;
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
var
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  {$IFDEF UNICODE}
  FExecStatement.GetPChar(ParameterIndex, Pointer(Result), L, zCP_UTF16);
  {$ELSE}
  FExecStatement.GetPChar(ParameterIndex, Pointer(Result), L, ConSettings.CTRL_CP);
  {$ENDIF}
end;

procedure TZAbstractCallableStatement.GetPChar(Index: Integer;
  out Buf: Pointer; out Len: LengthInt; CodePage: Word);
begin
  if FExecStatement <> nil
  then FExecStatement.GetPChar(Index, Buf, Len, CodePage)
  else begin
    Buf := nil;
    Len := 0;
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

{**
  Get the previous resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetPreviousResultSet: IZResultSet;
var I: Integer;
begin
  Result := nil;
  if FResults.Count > 0 then
    for I := FActiveResultIndex -1 downto 0 do
      if Supports(FResults[I], IZResultSet, Result) then begin
        FActiveResultIndex := I;
        Break;
      end;
  if (Result <> nil) and (Result.GetType <> rtForwardOnly) then
    Result.BeforeFirst;
end;

function TZAbstractCallableStatement.GetResultSet: IZResultSet;
begin
  if (FActiveResultIndex >= FResults.Count) or not Supports(FResults[FActiveResultIndex], IZResultSet, Result) then
    Result := nil;
end;

{**
  Retrieves a ResultSet by his index.
  @param Index the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
function TZAbstractCallableStatement.GetResultSetByIndex(
  Index: Integer): IZResultSet;
var I: Integer;
begin
  Result := nil;
  for I := 0 to Math.Min(Index, FResults.Count-1) do
    if not Supports(FResults[i], IZResultSet) then
      Inc(Index);
  if (Index < FResults.Count-1) and Supports(FResults[Index], IZResultSet) then
    FResults[Index].QueryInterface(IZResultSet, Result);
end;

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZAbstractCallableStatement.GetResultSetCount: Integer;
var I: Integer;
begin
  Result := FResults.Count;
  for I := 0 to FResults.Count -1 do
    if not Supports(FResults[i], IZResultSet) then
      Dec(Result);
end;

{**
  Get the value of a JDBC <code>TIME</code> parameter as a
  <code>java.sql.Time</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
procedure TZAbstractCallableStatement.GetTime(
  ParameterIndex: Integer; Var Result: TZTime);
begin
  if FExecStatement <> nil then begin
    FExecStatement.GetTime(ParameterIndex, Result);
    if (BindList.ParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = pctInOut) then
      SetTime(ParameterIndex, Result);
  end else begin
    {$IFDEF FPC} FillChar(Result, SizeOf(TZTime), #0);{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

procedure TZAbstractCallableStatement.GetTimeStamp(ParameterIndex: Integer;
  var Result: TZTimeStamp);
begin
  if FExecStatement <> nil then begin
    FExecStatement.GetTimeStamp(ParameterIndex, Result);
    if (BindList.ParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = pctInOut) then
      SetTimeStamp(ParameterIndex, Result);
  end else begin
    {$IFDEF FPC} FillChar(Result, SizeOf(TZTimeStamp), #0);{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetUInt(
  ParameterIndex: Integer): Cardinal;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetUint(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      BindUnSignedOrdinal(ParameterIndex, FExecStatement.BindList[ParameterIndex].SQLType, Result);
  end else begin
    {$IFDEF FPC}Result := 0;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetULong(ParameterIndex: Integer): UInt64;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.GetULong(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      {$IFDEF CPU64}
      BindUnSignedOrdinal(ParameterIndex, FExecStatement.BindList[ParameterIndex].SQLType, Result);
      {$ELSE}
      SetULong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Result);
      {$ENDIF}
  end else begin
    {$IFDEF FPC}Result := 0;{$ENDIF} //satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;
end;

function TZAbstractCallableStatement.GetUpdateCount: Integer;
var AnyValue: IZAnyValue;
begin
  if (FActiveResultIndex < FResults.Count) and Supports(FResults[FActiveResultIndex], IZAnyValue, AnyValue) then
    Result := AnyValue.GetInteger
  else
    Result := -1;
end;

function TZAbstractCallableStatement.GetValue(
  ParameterIndex: Integer): TZVariant;
begin
  Result := FExecStatement.GetValue(ParameterIndex);
end;

function TZAbstractCallableStatement.IsFunction: Boolean;
var I: Integer;
begin
  Result := False;
  for I := 0 to BindList.Count -1 do
    if BindList.ParamTypes[i] = pctReturn then begin
      Result := True;
      Break;
    end;
end;

function TZAbstractCallableStatement.IsNull(ParameterIndex: Integer): Boolean;
begin
  if FExecStatement <> nil then begin
    Result := FExecStatement.IsNull(ParameterIndex);
    {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
    if Result and (BindList.ParamTypes[ParameterIndex] = pctInOut) then
      BindList.SetNull(ParameterIndex, BindList.SQLTypes[ParameterIndex])
  end else begin
    {$IFDEF FPC}Result := True; {$ENDIF}//satisfy compiler
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;

end;

procedure TZAbstractCallableStatement.Prepare;
begin
  if FExecStatement = nil then begin
    FExecStatement := CreateExecutionStatement(FStoredProcName);
    FExecStatement._AddRef;
    FParamsRegistered := not Connection.UseMetadata;
  end;
  if not Prepared then
    inherited Prepare;
  FExecStatement.SetResultSetType(GetResultSetType);
  if Assigned(FLastResultSet) then begin
    FLastResultSet.Close;
    FLastResultSet := nil;
  end;
  FResults.Clear;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractCallableStatement.PrepareInParameters;
var I: Integer;
  procedure RegisterFromMetadata;
  var I, Scale, Prec: Integer;
    RS: IZResultSet;
    S: String;
    SQLType: TZSQLType;
    ParamIO: TZProcedureColumnType;
    Catalog, Schema, ObjName: String;
  begin
    FParamsRegistered := True;
    FRegisteringParamFromMetadata := True;
    //Register the ParamNames
    with Connection.GetMetadata do begin
      with GetDatabaseInfo do
      SplitQualifiedObjectName(StoredProcName, SupportsCatalogsInProcedureCalls,
        SupportsSchemasInProcedureCalls, Catalog, Schema, ObjName);
      Schema := AddEscapeCharToWildcards(Schema);
      ObjName := AddEscapeCharToWildcards(ObjName);
      if Catalog = '' then
        Catalog := Connection.GetCatalog;
      RS := Connection.GetMetadata.GetProcedureColumns(Catalog, Schema, ObjName, '');
      RS.Last;
      I := RS.GetRow;
      if I > 0 then begin
        if FExecStatement = nil
        then SetParamCount(I)
        else FExecStatement.SetParamCount(I);
        I := 0;
        RS.BeforeFirst;
        while RS.Next do begin
          SQLType := TZSQLType(RS.GetInt(ProcColDataTypeIndex));
          ParamIO := TZProcedureColumnType(RS.GetInt(ProcColColumnTypeIndex));
          S := RS.GetString(ProcColColumnNameIndex);
          Prec := RS.GetInt(ProcColPrecisionIndex);
          Scale := RS.GetInt(ProcColScaleIndex);
          if FExecStatement = nil then
            RegisterParameter(I, SQLType, ParamIO, S, Prec, Scale)
          else begin
            FExecStatement.RegisterParameter(I, SQLType, ParamIO, S, Prec, Scale);
            if BindList.Count < I+1 then
              RegisterParameter(I, SQLType, ParamIO, S, Prec, Scale);
          end;
          Inc(I);
        end;
      end;
      FRegisteringParamFromMetadata := False;
    end;
//      Assert(I = BindList.Count);
  end;
begin
  if not FParamsRegistered then
    if Connection.UseMetadata
    then RegisterFromMetadata
    else if FExecStatement <> nil then
      for i := BindList.Count -1 downto 0 do
        FExecStatement.RegisterParameter(I,
          BindList.SQLTypes[i], BindList.ParamTypes[i]);
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
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  if Connection.UseMetadata
  then CheckParameterIndex(ParameterIndex)
  else if (BindList.Count < ParameterIndex +1) or
       (BindList[ParameterIndex].ParamType = pctUnknown)
    then RegisterParameter(ParameterIndex, TZSQLType(SQLType), pctOut)
    else RegisterParameter(ParameterIndex, TZSQLType(SQLType), BindList[ParameterIndex].ParamType)
end;

procedure TZAbstractCallableStatement.RegisterParamType(ParameterIndex,
  ParamType: Integer);
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  if Connection.UseMetadata
  then CheckParameterIndex(ParameterIndex)
  else if (BindList.Count < ParameterIndex +1)
    then RegisterParameter(ParameterIndex, stUnknown, TZProcedureColumnType(ParamType))
    else RegisterParameter(ParameterIndex, BindList[ParameterIndex].SQLType, TZProcedureColumnType(ParamType));
end;

procedure TZAbstractCallableStatement.ReleaseImmediat(
  const Sender: IImmediatelyReleasable; var AError: EZSQLConnectionLost);
begin
  FResults.Clear;
  if Assigned(FExecStatement) then
    FExecStatement.ReleaseImmediat(Sender, AError);
  inherited ReleaseImmediat(Sender, AError);
end;

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetBigDecimal(ParameterIndex: Integer;
  const Value: TBCD);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  FBindList.Put(ParameterIndex, Value);
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  BindSignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBoolean, Ord(Value));
end;

{**
  Sets the designated parameter to a Java <code>byte</code> value.
  The driver converts this
  to an SQL <code>Byte</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stByte, Value);
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
procedure TZAbstractCallableStatement.SetBytes(ParameterIndex: Integer;
  Value: PByte; Len: NativeUInt);
var Bind: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Bind := FBindList[ParameterIndex];
  {Registered Param ? }
  if (Bind.SQLType <> stBytes) and (Bind.ParamType <> pctUnknown) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    case Bind.SQLType of
      stBinaryStream: FBindList.Put(ParameterIndex, stBinaryStream, TZLocalMemBLob.CreateWithData(Value, Len, FOpenLobStreams));
      else FBindList.Put(ParameterIndex, stBytes, Value, Len);
    end;
  end else FBindList.Put(ParameterIndex, stBytes, Value, Len);
end;

procedure TZAbstractCallableStatement.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
var Bind: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Bind := FBindList[ParameterIndex];
  {Registered Param ? }
  if (Bind.SQLType <> stCurrency) and (Bind.ParamType <> pctUnknown) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    case Bind.SQLType of
      stBoolean: FBindList.Put(ParameterIndex, Value <> 0);
      stByte, stWord, stLongWord, stShort, stSmall, stInteger,
      stLong, stULong: SetLong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PInt64(@Value)^ div 10000);
      stFloat, stDouble, stDate, stTime, stTimeStamp: BindDouble(ParameterIndex, Bind.SQLType, Value);
      stCurrency: FBindList.Put(ParameterIndex, stCurrency, P8Bytes(@Value));
      stBigDecimal: begin
          ScaledOrdinal2BCD(PInt64(@Value)^, 0, fBCDTemp);
          BindList.Put(ParameterIndex, fBCDTemp);
        end;
      else FBindList.Put(ParameterIndex, stCurrency, P8Bytes(@Value));;
    end;
  end else FBindList.Put(ParameterIndex, stCurrency, P8Bytes(@Value));;
end;

procedure TZAbstractCallableStatement.SetDate(ParameterIndex: Integer;
  const Value: TZDate);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  FBindList.Put(ParameterIndex, Value);
end;

procedure TZAbstractCallableStatement.SetDouble(ParameterIndex: Integer;
  const Value: Double);
begin
  BindDouble({$IFNDEF GENERIC_INDEX}ParameterIndex-1{$ENDIF}, stDouble, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  BindDouble({$IFNDEF GENERIC_INDEX}ParameterIndex-1{$ENDIF}, stFloat, Value);
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetInt(ParameterIndex, Value: Integer);
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
procedure TZAbstractCallableStatement.SetLong(ParameterIndex: Integer;
  const Value: Int64);
{$IFDEF CPU64}
begin
  BindSignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLong, Value)
{$ELSE}
var Bind: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Bind := FBindList[ParameterIndex];
  {Registered Param ? }
  if (Bind.SQLType <> stULong) and (Bind.ParamType <> pctUnknown) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    case Bind.SQLType of
      stBoolean: FBindList.Put(ParameterIndex, Value <> 0);
      stByte, stWord, stLongWord: BindUnsignedOrdinal(ParameterIndex, Bind.SQLType, Value);
      stShort, stSmall, stInteger: BindSignedOrdinal(ParameterIndex, Bind.SQLType, Value);
      stLong: FBindList.Put(ParameterIndex, stlong, P8Bytes(@Value));
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
      stULong: SetULong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      stFloat, stDouble, stDate, stTime, stTimeStamp: SetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stCurrency: SetCurrency(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stBigDecimal: begin
          ZSysUtils.ScaledOrdinal2Bcd(Value, 0, fBCDTemp);
          BindList.Put(ParameterIndex, fBCDTemp);
        end;
      else FBindList.Put(ParameterIndex, stLong, P8Bytes(@Value));
    end;
  end else FBindList.Put(ParameterIndex, stLong, P8Bytes(@Value));
  {$ENDIF}
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractCallableStatement.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
var Bind: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Bind := FBindList[ParameterIndex];
  if Boolean(Bind.ParamType) and Boolean(Bind.SQLType) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    BindList.SetNull(ParameterIndex, BindList.SQLTypes[ParameterIndex])
  end else BindList.SetNull(ParameterIndex, SQLType);
end;

{**
  Sets the designated parameter to a Java <code>unsigned 16bit int</code> value.
  The driver converts this
  to an SQL <code>WORD</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetShort(ParameterIndex: Integer;
  Value: ShortInt);
begin
  BindSignedOrdinal(ParameterIndex, stShort, Value);
end;


{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetSmall(ParameterIndex: Integer;
  Value: SmallInt);
begin
  BindSignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stSmall, Value);
end;

procedure TZAbstractCallableStatement.SetTime(ParameterIndex: Integer;
  const Value: TZTime);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  FBindList.Put(ParameterIndex, Value);
end;

procedure TZAbstractCallableStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TZTimeStamp);
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  FBindList.Put(ParameterIndex, Value);
end;

{**
  Sets the designated parameter to a Java <code>usigned 32bit int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLongWord, Value);
end;

{**
  Sets the designated parameter to a Java <code>unsigned long long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
{$IFDEF CPU64}
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stULong, Value)
{$ELSE}
var Bind: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Bind := FBindList[ParameterIndex];
  {Registered Param ? }
  if (Bind.SQLType <> stULong) and (Bind.ParamType <> pctUnknown) then begin
    if (Bind.ParamType = pctOut) and not Connection.UseMetadata then
      Bind.ParamType := pctInOut;
    case Bind.SQLType of
      stBoolean: FBindList.Put(ParameterIndex, Value <> 0);
      stByte, stWord, stLongWord: BindUnsignedOrdinal(ParameterIndex, Bind.SQLType, Value);
      stShort, stSmall, stInteger: BindSignedOrdinal(ParameterIndex, Bind.SQLType, Value);
      stLong: SetLong(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stULong: FBindList.Put(ParameterIndex, stUlong, P8Bytes(@Value));
      stFloat, stDouble, stDate, stTime, stTimeStamp: SetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stCurrency: SetCurrency(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value);
      stBigDecimal: begin
          ZSysUtils.ScaledOrdinal2Bcd(Value, 0, fBCDTemp, False);
          BindList.Put(ParameterIndex, fBCDTemp);
        end;
      else FBindList.Put(ParameterIndex, stULong, P8Bytes(@Value));
    end;
  end else FBindList.Put(ParameterIndex, stULong, P8Bytes(@Value));
{$ENDIF}
end;


{**
  Sets the designated parameter to a Java <code>unsigned 16bit int</code> value.
  The driver converts this
  to an SQL <code>WORD</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractCallableStatement.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stWord, Value);
end;

procedure TZAbstractCallableStatement.Unprepare;
begin
  FResults.Clear;
  FActiveResultIndex := -1;
  FParamsRegistered := False;
  if FExecStatement <> nil then begin
    FExecStatement.Close;
    FExecStatement._Release;
    FExecStatement := nil
  end;
  inherited Unprepare;
end;

{ TZAbstractCallableStatement_A }

procedure TZAbstractCallableStatement_A.AfterConstruction;
begin
  inherited;
  FCharRec.CP := FClientCP;
end;

procedure TZAbstractCallableStatement_A.BindRawStr(Index: Integer;
  Buf: PAnsiChar; Len: LengthInt);
var Bind: PZBindValue;
begin
  CheckParameterIndex(Index);
  Bind := BindList[Index];
  if (Bind.SQLType <> stString) and (Bind.ParamType <> pctUnknown)
  then BindList.Put(Index, Bind.SQLType, Buf, Len, FClientCP)
  else BindList.Put(Index, stString, Buf, Len, FClientCP);
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
{$IFDEF FPC}
  {$PUSH}
  {$WARN 5093 off : Function result variable of a managed type does not seem to be initialized} //cpu 32
  {$WARN 5094 off : Function result variable of a managed type does not seem to be initialized} //cpu 64
{$ENDIF} // ZSetString does the job even if NOT required
function TZAbstractCallableStatement_A.GetAnsiString(
  ParameterIndex: Integer): AnsiString;
var
  P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatement.GetPChar(ParameterIndex, Pointer(P), L, FClientCP);
  if (FClientCP = ZOSCodePage) then
    ZSetString(P, L, Result)
  else begin
    FUniTemp := PRawToUnicode(P, L, FClientCP);
    Result := ZUnicodeToRaw(FUniTemp, ZOSCodePage);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF}
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
{$IFDEF FPC}
  {$PUSH}
  {$WARN 5093 off : Function result variable of a managed type does not seem to be initialized} //cpu 32
  {$WARN 5094 off : Function result variable of a managed type does not seem to be initialized} //cpu 64
{$ENDIF} // ZSetString does the job even if NOT required
function TZAbstractCallableStatement_A.GetRawByteString(
  ParameterIndex: Integer): RawByteString;
var
  P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatement.GetPChar(ParameterIndex, Pointer(P), L, FClientCP);
  ZSetString(P, L, Result)
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
    if (FClientCP = ConSettings.CTRL_CP) then
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
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatement.GetPChar(ParameterIndex, FCharRec.P, L, FClientCP);
  Result := PRawToUnicode(FCharRec.P, L, FClientCP);
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
{$IFDEF FPC}
  {$PUSH}
  {$WARN 5093 off : Function result variable of a managed type does not seem to be initialized} //cpu 32
  {$WARN 5094 off : Function result variable of a managed type does not seem to be initialized} //cpu 64
{$ENDIF} // ZSetString does the job even if NOT required
function TZAbstractCallableStatement_A.GetUTF8String(
  ParameterIndex: Integer): UTF8String;
var
  P: PAnsiChar;
  L: LengthInt;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  FExecStatement.GetPChar(ParameterIndex, Pointer(P), L, FClientCP);
  if (FClientCP = zCP_UTF8) then
    ZSetString(P, L, Result)
  else begin
    FUniTemp := PRawToUnicode(P, L, FClientCP);
    Result := ZUnicodeToRaw(FUniTemp, ZOSCodePage);
  end;
end;
{$IFDEF FPC} {$POP} {$ENDIF} // ZSetString does the job even if NOT required

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
  if (zOSCodePage = FClientCP)
  then IZPreparedStatement(FWeakIntfPtrOfIPrepStmt).SetRawByteString(ParameterIndex, Value)
  else begin
    fUniTemp := PRawToUnicode(Pointer(Value), Length(Value), ZOSCodePage);
    fRawTemp := PUnicodeToRaw(Pointer(FUniTemp), Length(fUniTemp), FClientCP);
  end;
end;
{$ENDIF}

procedure TZAbstractCallableStatement_A.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  if (Value.CP = ConSettings^.ClientCodePage.CP) then
    BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value.P, Value.Len)
  else begin
    FUniTemp := PRawToUnicode(Value.P, Value.Len, Value.CP);
    SetRawByteString(ParameterIndex, ZUnicodeToRaw(FUniTemp, FClientCP));
  end;
end;

procedure TZAbstractCallableStatement_A.SetRawByteString(
  ParameterIndex: Integer; const Value: RawByteString);
var Bind: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}Dec(ParameterIndex);{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  Bind := BindList[ParameterIndex];
  if (Bind.SQLType <> stString) and (Bind.ParamType <> pctUnknown)
  then BindList.Put(ParameterIndex, Bind.SQLType, Value, FClientCP)
  else BindList.Put(ParameterIndex, stString, Value, FClientCP);
end;

procedure TZAbstractCallableStatement_A.SetString(ParameterIndex: Integer;
  const Value: String);
begin
  {$IFDEF UNICODE}
  SetRawByteString(ParameterIndex, ZUnicodeToRaw(Value, FClientCP));
  {$ELSE}
  SetRawByteString(ParameterIndex, ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, FClientCP));
  {$ENDIF}
end;

procedure TZAbstractCallableStatement_A.SetUnicodeString(
  ParameterIndex: Integer; const Value: ZWideString);
begin
  SetRawByteString(ParameterIndex, ZUnicodeToRaw(Value, FClientCP));
end;

{$IFNDEF NO_UTF8STRING}
procedure TZAbstractCallableStatement_A.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  if (zCP_UTF8 = FClientCP)
  then SetRawByteString(ParameterIndex, Value)
  else begin
    fUniTemp := PRawToUnicode(Pointer(Value), Length(Value), zCP_UTF8);
    fRawTemp := PUnicodeToRaw(Pointer(FUniTemp), Length(fUniTemp), FClientCP);
  end;
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
begin
  {$IFNDEF GENERIC_INDEX}Index := Index-1;{$ENDIF}
  FExecStatement.GetPChar(Index, FCharRec.P, Len, zCP_UTF16);
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
    if (ConSettings.ClientCodePage.CP = ConSettings.CTRL_CP) then
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

