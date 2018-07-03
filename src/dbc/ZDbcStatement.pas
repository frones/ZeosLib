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
  ZDbcIntfs, ZTokenizer, ZCompatibility, ZVariant, ZDbcLogging, ZClasses,
  ZDbcUtils;

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
    FCursorName: AnsiString;
    FBatchQueries: TStrings;
    FConnection: IZConnection;
    FInfo: TStrings;
    FChunkSize: Integer; //size of buffer chunks for large lob's related to network settings
    FClosed: Boolean;
    FCachedLob: Boolean;
    procedure SetLastResultSet(const ResultSet: IZResultSet); virtual;
  protected
    fWBuffer: array[Byte] of WideChar;
    fABuffer: array[Byte] of AnsiChar;
    FWSQL: ZWideString;
    FaSQL: RawByteString;
    FStatementId : Integer;
    FOpenResultSet: Pointer; //weak reference to avoid memory-leaks and cursor issues
    procedure ToBuff(const Value: ZWideString; var Result: ZWideString); overload;
    procedure ToBuff(const Value: RawByteString; var Result: RawByteString); overload;
    procedure FlushBuff(var Result: ZWideString); overload;
    procedure FlushBuff(var Result: RawByteString); overload;
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
    property CursorName: AnsiString read FCursorName write FCursorName;
    property BatchQueries: TStrings read FBatchQueries;
    property Connection: IZConnection read FConnection;
    property Info: TStrings read FInfo;
    property Closed: Boolean read FClosed write FClosed;

    property SQL: String read {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
    property WSQL: ZWideString read FWSQL write SetWSQL;
    property ASQL: RawByteString read FaSQL write SetASQL;
    property ChunkSize: Integer read FChunkSize;
    property CachedLob: Boolean read FCachedLob;
    function CreateStmtLogEvent(Category: TZLoggingCategory;
      const Msg: RawByteString=''): TZLoggingEvent;
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; overload; virtual;
    function ExecuteUpdate(const SQL: ZWideString): Integer; overload; virtual;
    function Execute(const SQL: ZWideString): Boolean; overload; virtual;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; overload; virtual;
    function ExecuteUpdate(const SQL: RawByteString): Integer; overload; virtual;
    function Execute(const SQL: RawByteString): Boolean; overload; virtual;

    function GetSQL : String;

    procedure Close; virtual;
    function IsClosed: Boolean;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); virtual;

    function GetMaxFieldSize: Integer; virtual;
    procedure SetMaxFieldSize(Value: Integer); virtual;
    function GetMaxRows: Integer; virtual;
    procedure SetMaxRows(Value: Integer); virtual;
    procedure SetEscapeProcessing(Value: Boolean); virtual;
    function GetQueryTimeout: Integer; virtual;
    procedure SetQueryTimeout(Value: Integer); virtual;
    procedure Cancel; virtual;
    procedure SetCursorName(const Value: AnsiString); virtual;

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

  TZBindType = (zbtNull, zbt8Byte, zbtRawString, zbtUTF8String, zbtAnsiString,
    zbtUniString, zbtCharByRef, zbtBinByRef, zbtGUID, zbtBytes, zbtArray,
    zbtLob, zbtPointer, zbtBCD, zbtTimeStamp);

  PZBindValue = ^TZBindValue;
  TZBindValue = record
    Value:      Pointer;
    BindType:   TZBindType;
    ParamType:  TZParamType;
    SQLType:    TZSQLType;
    ParamSize:  LengthInt;
  end;
  PZBufRec = ^TZBufRec;
  TZBufRec = record
    Buf: Pointer;
    Len: LengthInt;
  end;
  P8Bytes = PInt64;

  PZBcd = ^TZBcd;
  PZTimeStamp = ^TZTimeStamp;

  PZTParamValueArray = ^TZTParamValueArray;
  {** Defines a static array of bind values. }
  TZTParamValueArray = array[0..High(Word)] of TZBindValue;

  TZBindList = class
  private
    FValues: PZTParamValueArray;
    FCount: Integer;
    FCapacity: Integer;
    FConSettings: PZConSettings;
    procedure Grow;
    {$IFOPT R+}
    class procedure Error(const Msg: string; Data: Integer);
    {$ENDIF}
    function AquireBuffer(Index: Integer; SQLType: TZSQLType; BindType: TZBindType): PZBindValue; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure SetCapacity(NewCapacity: Integer);
    function Get(Index: Integer): PZBindValue; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetBindType(Index: Integer): TZBindType; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetSQLType(Index: Integer): TZSQLType; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetType(Index: Integer): TZParamType; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetVariant(Index: Integer): TZVariant;
    function GetArray(Index: Integer): PZArray; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function Get8Byte(Index: Integer): P8Bytes; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetSize(Index: Integer): LengthInt; {$IFDEF WITH_INLINE}inline;{$ENDIF}
  public
    constructor Create(ConSettings: PZConSettings);
    destructor Destroy; override;
  public
    procedure Clear;
    procedure ClearValue(Index: Integer); {$IFDEF WITH_INLINE}inline;{$ENDIF}
    procedure Delete(Index: Integer);

    procedure Put(Index: Integer; Value: Boolean); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; _8Byte: P8Bytes); overload;
    procedure Put(Index: Integer; const Value: TZBCD); overload;
    procedure Put(Index: Integer; const Value: TZTimeStamp); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: TBytes); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: RawByteString; CP: Word); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt; CP: Word); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: ZWideString); overload;
    procedure Put(Index: Integer; const Value: TZArray); overload;
    procedure Put(Index: Integer; SQLType: TZSQLType; const Value: IZBLob); overload;

    procedure SetCount(NewCount: Integer);
    procedure SetNull(Index: Integer; SQLType: TZSQLType);
    procedure FlushAll;
  public
    property Count: Integer read FCount write SetCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Bindings[Index: Integer]: PZBindValue read Get; default;
    property ParamTypes[Index: Integer]: TZParamType read GetType;
    property Variants[Index: Integer]: TZVariant read GetVariant;
    property SQLTypes[Index: Integer]: TZSQLType read GetSQLType;
    property BindTypes[Index: Integer]: TZBindType read GetBindType;
    property Arrays[Index: Integer]: PZArray read GetArray;
    property _8Bytes[Index: Integer]: P8Bytes read Get8Byte;
    property Sizes[Index: Integer]: LengthInt read GetSize;
  end;

  {** Implements Abstract Prepared SQL Statement. }

  { TZAbstractPreparedStatement }

  TZAbstractPreparedStatement2 = class(TZAbstractStatement, IImmediatelyReleasable)
  private
    FInitialArrayCount: ArrayLenInt;
    FPrepared : Boolean;
    FExecCount: Integer;
    FSupportsDMLBatchArrays: Boolean;
    FBindList: TZBindList;
  protected
    FTokenMatchIndex, //did we match a token to indicate if Prepare makes sense?
    FCountOfQueryParams, //how many params did we found to prepvent mem-reallocs?
    FMinExecCount2Prepare: Integer; //how many executions must be done to fall into a real prepared mode?
    FGUIDAsString: Boolean; //How should a GUID value be treaded?
    property TokenMatchIndex: Integer read FTokenMatchIndex;
    procedure CheckParameterIndex(Value: Integer); virtual;
    procedure PrepareInParameters; virtual;
    procedure BindInParameters; virtual;
    procedure UnPrepareInParameters; virtual;

    procedure ValidateArraySizeAndType(const Value: Pointer; SQLType: TZSQLType;
      VariantType: TZVariantType; ParamIndex: Integer);

    procedure SetParamCount(NewParamCount: Integer); virtual;
    procedure SetBindCapacity(Capacity: Integer); virtual;

    procedure LogPrepStmtMessage(Category: TZLoggingCategory; const Msg: RawByteString = '');
    function GetInParamLogValue(ParamIndex: Integer): RawByteString; virtual;
    function GetCompareFirstKeywordStrings: PPreparablePrefixTokens; virtual;
  protected //Properties
    property ArrayCount: ArrayLenInt read FInitialArrayCount;
    property ExecutionCount: Integer read FExecCount;
    property MinExecCount2Prepare: Integer read FMinExecCount2Prepare;
    property SupportsDMLBatchArrays: Boolean read FSupportsDMLBatchArrays;
    property BindList: TZBindList read FBindList;
  protected //the sql conversions
    procedure SetASQL(const Value: RawByteString); override;
    procedure SetWSQL(const Value: ZWideString); override;
  protected //binding
    procedure BindArray(Index: Integer; const Value: TZArray); virtual;
    procedure BindBinary(Index: Integer; SQLType: TZSQLType; Buf: Pointer; Len: LengthInt); virtual;
    procedure BindBoolean(Index: Integer; Value: Boolean); virtual;
    procedure BindDateTime(Index: Integer; SQLType: TZSQLType; const Value: TDateTime); virtual;
    procedure BindDouble(Index: Integer; SQLType: TZSQLType; const Value: Double); virtual;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); virtual;
    procedure BindNull(Index: Integer; SQLType: TZSQLType); virtual;
    procedure BindSignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: Int64); virtual;
    procedure BindUnsignedOrdinal(Index: Integer; SQLType: TZSQLType; const Value: UInt64); virtual;

    procedure IsNull(Index: Integer; out Result: Boolean); overload; virtual;
    procedure GetBoolean(Index: Integer; out Result: Boolean); overload; virtual;
    procedure GetOrdinal(Index: Integer; out Result: Int64); overload; virtual;
    procedure GetOrdinal(Index: Integer; out Result: UInt64); overload; virtual;
    procedure GetDouble(Index: Integer; out Result: Double); overload; virtual;
    procedure GetBigDecimal(Index: Integer; var Result: TZBCD); overload; virtual;
    procedure GetBytes(Index: Integer; out nBytes: LengthInt; out Result: Pointer); overload; virtual;
    procedure GetDateTime(Index: Integer; out Result: TDateTime); overload; virtual;
    procedure GetLob(Index: Integer; var Result: IZBlob); overload; virtual;
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

    procedure Close; override;
    function GetSQL : String;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function IsPrepared: Boolean; virtual;
    property Prepared: Boolean read IsPrepared;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); virtual; abstract;

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
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: Extended); virtual;
    procedure SetPChar(ParameterIndex: Integer; Value: PChar); virtual;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); virtual; abstract;
    procedure SetString(ParameterIndex: Integer; const Value: String); virtual; abstract;
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); virtual; abstract;
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); virtual; abstract;
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); virtual; abstract;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);  virtual; abstract;
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

    procedure RegisterParameter(ParameterIndex: Integer;
      SQLType: TZSQLType; ParamType: TZParamType; ParamSize: LengthInt = -1);

    function IsNull(ParameterIndex: Integer): Boolean; overload;virtual;
    function GetPChar(ParameterIndex: Integer): PChar; virtual; abstract;
    function GetString(ParameterIndex: Integer): String; virtual; abstract;
    function GetAnsiString(ParameterIndex: Integer): AnsiString; virtual; abstract;
    function GetUTF8String(ParameterIndex: Integer): UTF8String; virtual; abstract;
    function GetRawByteString(ParameterIndex: Integer): RawByteString; virtual; abstract;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString; virtual; abstract;
    function GetBoolean(ParameterIndex: Integer): Boolean; overload; virtual;
    function GetByte(ParameterIndex: Integer): Byte; virtual;
    function GetShort(ParameterIndex: Integer): ShortInt; virtual;
    function GetWord(ParameterIndex: Integer): Word; virtual;
    function GetSmall(ParameterIndex: Integer): SmallInt; virtual;
    function GetUInt(ParameterIndex: Integer): Cardinal; virtual;
    function GetInt(ParameterIndex: Integer): Integer; virtual;
    function GetULong(ParameterIndex: Integer): UInt64; virtual;
    function GetLong(ParameterIndex: Integer): Int64; virtual;
    function GetFloat(ParameterIndex: Integer): Single; virtual;
    function GetDouble(ParameterIndex: Integer): Double; overload; virtual;
    function GetCurrency(ParameterIndex: Integer): Currency; virtual;
    function GetBigDecimal(ParameterIndex: Integer): Extended; overload; virtual;
    function GetBytes(ParameterIndex: Integer): TBytes; overload; virtual;
    function GetDate(ParameterIndex: Integer): TDateTime; virtual;
    function GetTime(ParameterIndex: Integer): TDateTime; virtual;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; virtual;
    function GetValue(ParameterIndex: Integer): TZVariant; virtual;

    procedure ClearParameters; virtual;

    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; override;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); override;
    procedure SetResultSetType(Value: TZResultSetType); override;
  end;

  TZRawPreparedStatement = class(TZAbstractPreparedStatement2)
  private
    FUniTemp: ZWideString;
  protected
    FInParamDefaultValues: TRawByteStringDynArray;
    procedure BindRawStr(Index: Integer; const Value: RawByteString); overload; virtual;
    procedure BindRawStr(Index: Integer; Buf: PAnsiChar; Len: LengthInt); overload; virtual;
    procedure BindLob(Index: Integer; SQLType: TZSQLType; const Value: IZBlob); override;
    procedure SetBindCapacity(Capacity: Integer); override;
    property InParamDefaultValues: TRawByteStringDynArray read FInParamDefaultValues;

    procedure GetPAnsiChar(Index: Integer; out nBytes: LengthInt; var Result: PAnsiChar); virtual;
  public
    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); override;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); override;
    procedure SetString(ParameterIndex: Integer; const Value: String); override;
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); override;
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); override;
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); override;

    function GetPChar(ParameterIndex: Integer): PChar; override;
    function GetString(ParameterIndex: Integer): String; override;
    function GetAnsiString(ParameterIndex: Integer): AnsiString; override;
    function GetUTF8String(ParameterIndex: Integer): UTF8String; override;
    function GetRawByteString(ParameterIndex: Integer): RawByteString; override;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString; override;
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

  TZUCS2PreparedStatement = class(TZAbstractPreparedStatement2)
  protected
    FInParamDefaultValues: TUnicodeStringDynArray;
    procedure BindUniStr(Index: Integer; const Value: ZWideString); overload; virtual;
    procedure BindUniStr(Index: Integer; Buf: PWideChar; CodePoints: LengthInt); overload; virtual;
    property InParamDefaultValues: TUnicodeStringDynArray read FInParamDefaultValues;
  public
    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string); override;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); override;
    procedure SetString(ParameterIndex: Integer; const Value: String); override;
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); override;
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); override;
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); override;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString); override;
  end;

  TZUCS2ParamDetectPreparedStatement = class(TZUCS2PreparedStatement)
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

  {** Implements Abstract Prepared SQL Statement. }

  { TZAbstractPreparedStatement }

  TZAbstractPreparedStatement = class(TZAbstractStatement, IZPreparedStatement,
    IImmediatelyReleasable)
  private
    FInParamValues: TZVariantDynArray;
    FInitialArrayCount: ArrayLenInt;
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
    FMinExecCount2Prepare, FInParamCount: Integer;
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
    procedure LogPrepStmtMessage(Category: TZLoggingCategory; const Msg: RawByteString = '');
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
    property ArrayCount: ArrayLenInt read FInitialArrayCount;
    property ExecutionCount: Integer read FExecCount;
    property MinExecCount2Prepare: Integer read FMinExecCount2Prepare;
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

    procedure Close; override;
    function GetSQL : String;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function IsPrepared: Boolean; virtual;
    property Prepared: Boolean read IsPrepared;
    procedure ReleaseImmediat(const Sender: IImmediatelyReleasable); override;

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
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: Extended); virtual;
    procedure SetPChar(ParameterIndex: Integer; Value: PChar); virtual;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); virtual;
    procedure SetString(ParameterIndex: Integer; const Value: String); virtual;
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); virtual;
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); virtual;
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
    FDBParamTypes: array of ShortInt;
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
    procedure Close; override;

    function IsFunction: Boolean;
    function HasOutParameter: Boolean;
    function GetFirstResultSet: IZResultSet; virtual;
    function GetPreviousResultSet: IZResultSet; virtual;
    function GetNextResultSet: IZResultSet; virtual;
    function GetLastResultSet: IZResultSet; virtual;
    function BOR: Boolean; virtual;
    function EOR: Boolean; virtual;
    function GetResultSetByIndex(const {%H-}Index: Integer): IZResultSet; virtual;
    function GetResultSetCount: Integer; virtual;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SQLType: Integer); virtual;
    procedure RegisterParamType(ParameterIndex:integer;ParamType:Integer);virtual;
    function WasNull: Boolean; virtual;

    function IsNull(ParameterIndex: Integer): Boolean; virtual;
    function GetPChar(ParameterIndex: Integer): PChar; virtual;
    function GetString(ParameterIndex: Integer): String; virtual;
    function GetAnsiString(ParameterIndex: Integer): AnsiString; virtual;
    function GetUTF8String(ParameterIndex: Integer): UTF8String; virtual;
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
    function GetBigDecimal(ParameterIndex: Integer): Extended; virtual;
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
  ZEncoding, ZDbcProperties;

{$IFOPT R+}
  {$DEFINE RangeCheckEnabled}
{$ENDIF}
var
{**
  Holds the value of the last assigned statement ID.
  Only Accessible using TZAbstractStatement.GetNextStatementId.
}
  GlobalStatementIdCounter : integer;

{ TZAbstractStatement }

{**
  Constructs this class and defines the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters;
}
constructor TZAbstractStatement.Create(const Connection: IZConnection; Info: TStrings);
begin
  { Sets the default properties. }
  inherited Create;
  Self.ConSettings := Connection.GetConSettings;
  FLastUpdateCount := -1;

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
  if Assigned(FBatchQueries) then
    FreeAndNil(FBatchQueries);
  FConnection.DeregisterStatement(Self);
  FConnection := nil;
  if Assigned(FInfo) then
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
  if L <= (SizeOf(fABuffer)-fABufferIndex) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> no move
    then fABuffer[fABufferIndex] := P^
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
  if L <= ((SizeOf(fWBuffer) shr 1)-fWBufferIndex) then begin
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

procedure TZAbstractStatement.ReleaseImmediat(const Sender: IImmediatelyReleasable);
var ImmediatelyReleasable: IImmediatelyReleasable;
begin
  if not FClosed then begin
    FClosed := True;
    if (FOpenResultSet <> nil) and Supports(IZResultSet(FOpenResultSet), IImmediatelyReleasable, ImmediatelyReleasable) and
       (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender);
    if Assigned(FLastResultSet) and Supports(FLastResultSet, IImmediatelyReleasable, ImmediatelyReleasable) and
       (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender);
    if Assigned(Connection) and Supports(Connection, IImmediatelyReleasable, ImmediatelyReleasable) and
       (ImmediatelyReleasable <> Sender) then
      ImmediatelyReleasable.ReleaseImmediat(Sender);
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
  if FLastResultSet = ResultSet then
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
begin
  if LastResultSet <> nil then begin
    LastResultSet.Close;
    LastResultSet := nil;
  end;
  FClosed := True;
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
  Sets escape processing on or off.
  If escape scanning is on (the default), the driver will do
  escape substitution before sending the SQL to the database.

  Note: Since prepared statements have usually been parsed prior
  to making this call, disabling escape processing for prepared
  statements will have no effect.

  @param enable <code>true</code> to enable; <code>false</code> to disable
}
procedure TZAbstractStatement.SetEscapeProcessing(Value: Boolean);
begin
  FEscapeProcessing := Value;
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
    Result := ''; //init for FPC
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
  const Msg: RawByteString = ''): TZLoggingEvent;
begin
  if msg <> '' then
    result := TZLoggingEvent.Create(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId)+' : '+ Msg, 0, '')
  else
    result := TZLoggingEvent.Create(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId), 0, '');
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
procedure TZAbstractStatement.SetCursorName(const Value: AnsiString);
begin
  FCursorName := Value;
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
  Result := LastResultSet;
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
  //JDBC prepares after 4th execution
  FMinExecCount2Prepare := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, DSProps_MinExecCntBeforePrepare, '2'), 2);
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

procedure TZAbstractPreparedStatement.ReleaseImmediat(const Sender: IImmediatelyReleasable);
begin
  FPrepared := False;
  FExecCount := 0;
  inherited ReleaseImmediat(Sender);
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
    stString: if not (VariantType in [vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stUnicodeString: if not (VariantType in [vtUnicodeString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stArray, stDataSet:
          raise Exception.Create(sUnsupportedOperation);
  end;
  Len := {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
  if (ParamIndex = 0) then
    FInitialArrayCount := Len
  else if (FInitialArrayCount <> 0) and (Len <> FInitialArrayCount) and (SQLType <> stDataSet) then
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
  const Msg: RawByteString = '');
begin
  if DriverManager.HasLoggingListener then
    if msg <> '' then
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
      vtFloat : result := FloatToRaw(VFloat);
      vtString,
      vtAnsiString,
      vtUTF8String,
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
{$WARNINGS OFF}
function TZAbstractPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Logging Execution }
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecPrepStmt,Self);
  Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
end;
{$WARNINGS ON}
{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
{$WARNINGS OFF}
function TZAbstractPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
  Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
end;
{$WARNINGS ON}
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
  ParameterIndex: Integer; const Value: Extended);
begin
  InternalSetDouble(ParameterIndex, stBigDecimal, Value);
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
procedure TZAbstractPreparedStatement.SetAnsiString(ParameterIndex: Integer;
   const Value: AnsiString);
begin
  SetInParam(ParameterIndex, stString, EncodeAnsiString(Value));
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
procedure TZAbstractPreparedStatement.SetUTF8String(ParameterIndex: Integer;
   const Value: UTF8String);
begin
  SetInParam(ParameterIndex, stString, EncodeUTF8String(Value));
end;

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
    vtFloat: SQLType := stBigDecimal;
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
  SetInParam(ParameterIndex, SQLType, EncodeFloat(Value));
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
  FInitialArrayCount := 0;
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
  Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
end;

procedure TZAbstractPreparedStatement.Close;
begin
  if Prepared then
    Unprepare;
  inherited Close;
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
  Self.FInitialArrayCount := 0;
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
    Result := ''; //init Result
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
  FProcSql := ''; //Init -> FPC
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
    if (FDBParamTypes[i] in [2, 4]) then //[ptResult, ptOutput]
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
procedure TZAbstractCallableStatement.Close;
begin
  ClearResultSets;
  inherited Close;
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
function TZAbstractCallableStatement.GetResultSetByIndex(const Index: Integer): IZResultSet;
begin
  Result := nil;
end;

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

procedure TZAbstractCallableStatement.RegisterParamType(ParameterIndex,
  ParamType: Integer);
begin
  if ({$IFDEF GENERIC_INDEX}High{$ELSE}Length{$ENDIF}(FDBParamTypes) < ParameterIndex) then
    SetLength(FDBParamTypes, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FDBParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := ParamType;
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
function TZAbstractCallableStatement.GetAnsiString(ParameterIndex: Integer): AnsiString;
begin
  Result := ClientVarManager.GetAsAnsiString(GetOutParam(ParameterIndex));
end;

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
function TZAbstractCallableStatement.GetUTF8String(ParameterIndex: Integer): UTF8String;
begin
  Result := ClientVarManager.GetAsUTF8String(GetOutParam(ParameterIndex));
end;

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
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
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
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
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
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
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
function TZAbstractCallableStatement.GetBigDecimal(ParameterIndex: Integer):
  Extended;
begin
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
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
  if (Value <> '') and ( not Prepared ) then Prepare;
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

procedure TZBindList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TZBindList.ClearValue(Index: Integer);
var BindValue: PZBindValue;
begin
  BindValue := Get(Index);
  if BindValue.Value <> nil then
    case BindValue.BindType of
      zbtRawString,
      zbtUTF8String,
      zbtAnsiString:RawByteString(BindValue.Value) := ''; //dec refcnt
      zbtUniString: ZWideString(BindValue.Value) := '';
      zbtBytes:     TBytes(BindValue.Value) := nil;
      zbtLob:       IZBlob(BindValue.Value) := nil;
      zbtPointer:   BindValue.Value := nil;
      zbt8Byte:     begin
                      {$IFNDEF CPU64}
                      FreeMem(BindValue.Value);
                      {$ENDIF}
                      BindValue.Value := nil;
                    end;
      else begin
        FreeMem(BindValue.Value);
        BindValue.Value := nil;
      end;
    end;
end;

constructor TZBindList.Create(ConSettings: PZConSettings);
begin
  inherited Create;
  FConSettings := ConSettings;
end;

procedure TZBindList.Delete(Index: Integer);
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  ClearValue(Index);
  Dec(FCount);
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
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

{$IFOPT R+}
class procedure TZBindList.Error(const Msg: string; Data: Integer);
{$IFNDEF FPC}
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
{$ENDIF}

begin
  {$IFDEF FPC}
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
  {$ELSE}
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
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
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
  Result := @FValues^[Index];
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.Get8Byte(Index: Integer): P8Bytes;
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
  if FValues^[Index].BindType = zbt8Byte
  then Result := {$IFDEF CPU64}@{$ENDIF}FValues^[Index].Value
  else raise EZSQLException.Create(SUnsupportedDataType);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetArray(Index: Integer): PZArray;
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
  if FValues^[Index].BindType = zbtArray
  then Result := FValues^[Index].Value
  else raise EZSQLException.Create(SUnsupportedDataType);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetBindType(Index: Integer): TZBindType;
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
  Result := FValues^[Index].BindType
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetSize(Index: Integer): LengthInt;
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
  Result := FValues^[Index].ParamSize
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetSQLType(Index: Integer): TZSQLType;
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
  Result := FValues^[Index].SQLType
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZBindList.GetType(Index: Integer): TZParamType;
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
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
    zbt8Byte: case BindValue.SQLType of
                stBoolean:
                  Result := EncodeBoolean(PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^ <> 0);
                stByte, stWord, stLongWord, stULong:
                  Result := EncodeUInteger(PUInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                stShort, stSmall, stInteger, stLong:
                  Result := EncodeInteger(PInt64({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                stCurrency:
                  Result := EncodeFloat(PCurrency({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                stFloat, stDouble, stBigDecimal:
                  Result := EncodeFloat(PDouble({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
                else
                  Result := EncodeDateTime(PDateTime({$IFDEF CPU64}@{$ENDIF}BindValue.Value)^);
              end;
    zbtRawString: Result := EncodeRawByteString(RawByteString(BindValue.Value));
    zbtUTF8String:Result := EncodeUTF8String(UTF8String(BindValue.Value));
    zbtAnsiString:Result := EncodeAnsiString(AnsiString(BindValue.Value));
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

function TZBindList.AquireBuffer(Index: Integer; SQLType: TZSQLType;
  BindType: TZBindType): PZBindValue;
begin
{$IFOPT R+}
  if (Index < 0) or (Index > High(Word)) then
    Error(SListIndexError, Index);
{$ENDIF}
  while (Index+1 > FCapacity) do
    Grow;
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
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

procedure TZBindList.Put(Index: Integer; const Value: TZArray);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stArray, zbtArray);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TZArray));
  PZArray(BindValue.Value)^ := Value;
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

procedure TZBindList.Put(Index: Integer; const Value: TZBCD);
var BindValue: PZBindValue;
begin
  BindValue := AquireBuffer(Index, stArray, zbtBCD);
  if BindValue.Value = nil then
    GetMem(BindValue.Value, SizeOf(TZBCD));
  PZBCD(BindValue.Value)^ := Value;
end;

procedure TZBindList.Put(Index: Integer; SQLType: TZSQLType;
  const Value: RawByteString; CP: Word);
var BindValue: PZBindValue;
begin
  if CP = zCP_UTF8 then
    BindValue := AquireBuffer(Index, SQLType, zbtUTF8String)
  else if CP = FConSettings.ClientCodePage^.CP then
    BindValue := AquireBuffer(Index, SQLType, zbtRawString)
  else
    BindValue := AquireBuffer(Index, SQLType, zbtAnsiString);

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
  NativeInt(AquireBuffer(Index, stBoolean, zbtPointer).Value) := Ord(Value);
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

procedure TZBindList.SetCapacity(NewCapacity: Integer);
var
  I: Integer;
begin
{$IFOPT R+}
  if (NewCapacity < 0) or (NewCapacity > High(Word)) then
    Error(SListCapacityError, NewCapacity);
{$ENDIF}
  if NewCapacity < FCount then begin
    for I := FCount - 1 downto NewCapacity do
      ClearValue(I);
    FCount := NewCapacity;
  end;
  {$IFDEF RangeCheckEnabled} {$R-} {$ENDIF}
  if NewCapacity <> FCapacity then begin
    ReallocMem(FValues, NewCapacity * SizeOf(TZBindValue));
    if NewCapacity > FCapacity then
      FillChar(FValues^[FCount], (NewCapacity - FCapacity) * SizeOf(TZBindValue), #0);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    FCapacity := NewCapacity;
  end;
end;

procedure TZBindList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
{$IFOPT R+}
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

{**
  Binds a binary value
}
procedure TZAbstractPreparedStatement2.BindArray(Index: Integer;
  const Value: TZArray);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, Value);
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
  Binds a bool value
}
procedure TZAbstractPreparedStatement2.BindBoolean(Index: Integer;
  Value: Boolean);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index,Value);
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
  Binds a null value
}
procedure TZAbstractPreparedStatement2.BindNull(Index: Integer;
  SQLType: TZSQLType);
begin
  CheckParameterIndex(Index);
  FBindList.SetNull(Index, SQLType);
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
  FInitialArrayCount := 0;
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
procedure TZAbstractPreparedStatement2.Close;
begin
  if Prepared then
    Unprepare;
  inherited Close;
end;

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractPreparedStatement2.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FSupportsDMLBatchArrays := Connection.GetMetadata.GetDatabaseInfo.SupportsArrayBindings;
  //JDBC prepares after 4th execution
  FMinExecCount2Prepare := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, DSProps_MinExecCntBeforePrepare, '2'), 2);
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
  Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
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
  Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
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
  Inc(FExecCount, Ord((FMinExecCount2Prepare > 0) and (FExecCount < FMinExecCount2Prepare)));
end;

procedure TZAbstractPreparedStatement2.GetBigDecimal(Index: Integer;
  var Result: TZBCD);
begin
  CheckParameterIndex(Index);
end;

procedure TZAbstractPreparedStatement2.GetBoolean(Index: Integer;
  out Result: Boolean);
begin
  CheckParameterIndex(Index);
  Result := BindList[Index].Value <> nil
end;

procedure TZAbstractPreparedStatement2.GetBytes(Index: Integer;
  out nBytes: LengthInt; out Result: Pointer);
var BindValue: PZBindValue;
begin
  CheckParameterIndex(Index);
  BindValue := BindList[Index];
  case BindValue.BindType of
    zbtNull:  begin
                nBytes := 0;
                Result := nil;
              end;
    zbt8Byte: begin
                nBytes := 8;
                Result := Bindlist._8Bytes[Index];
              end;

  end;
end;

function TZAbstractPreparedStatement2.GetCompareFirstKeywordStrings: PPreparablePrefixTokens;
begin
  Result := nil;
end;

function TZAbstractPreparedStatement2.GetCurrency(
  ParameterIndex: Integer): Currency;
var D: Double;
begin
  GetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, D);
  Result := d;
end;

function TZAbstractPreparedStatement2.GetDate(
  ParameterIndex: Integer): TDateTime;
begin
  GetDatetime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
  Result := Int(Result);
end;

procedure TZAbstractPreparedStatement2.GetDateTime(Index: Integer;
  out Result: TDateTime);
begin
  CheckParameterIndex(Index);
  Result := PDateTime(BindList._8Bytes[Index])^;
end;

function TZAbstractPreparedStatement2.GetDouble(
  ParameterIndex: Integer): Double;
begin
  GetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

procedure TZAbstractPreparedStatement2.GetDouble(Index: Integer;
  out Result: Double);
begin
  CheckParameterIndex(Index);
  Result := PDouble(BindList._8Bytes[Index])^;
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
      vtFloat : result := FloatToRaw(VFloat);
      {$IFNDEF UNICODE}vtString,{$ENDIF}
      vtAnsiString,
      vtUTF8String,
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

function TZAbstractPreparedStatement2.GetInt(ParameterIndex: Integer): Integer;
var I: Int64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, I);
  Result := Integer(I);
end;

procedure TZAbstractPreparedStatement2.GetLob(Index: Integer;
  var Result: IZBlob);
begin
  CheckParameterIndex(Index);
  Result := IZBlob(BindList[Index].Value);
end;

function TZAbstractPreparedStatement2.GetLong(ParameterIndex: Integer): Int64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

procedure TZAbstractPreparedStatement2.GetOrdinal(Index: Integer;
  out Result: UInt64);
begin
  CheckParameterIndex(Index);
  Result := PUInt64(BindList._8Bytes[Index])^;
end;

procedure TZAbstractPreparedStatement2.GetOrdinal(Index: Integer;
  out Result: Int64);
begin
  CheckParameterIndex(Index);
  Result := PInt64(BindList._8Bytes[Index])^;
end;

function TZAbstractPreparedStatement2.GetBigDecimal(
  ParameterIndex: Integer): Extended;
var D: Double;
begin
  GetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, D);
  Result := D;
end;

function TZAbstractPreparedStatement2.GetBoolean(
  ParameterIndex: Integer): Boolean;
begin
  GetBoolean(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement2.GetByte(ParameterIndex: Integer): Byte;
var U: Uint64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, U);
  Result := Byte(U);
end;

function TZAbstractPreparedStatement2.GetBytes(ParameterIndex: Integer): TBytes;
var P: Pointer;
  L: LengthInt;
begin
  GetBytes(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, P);
  Result := BufferToBytes(P, L);
end;

function TZAbstractPreparedStatement2.GetFloat(ParameterIndex: Integer): Single;
var D: Double;
begin
  GetDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, D);
  Result := D;
end;

function TZAbstractPreparedStatement2.GetShort(
  ParameterIndex: Integer): ShortInt;
var I: Int64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, I);
  Result := ShortInt(I);
end;

function TZAbstractPreparedStatement2.GetSmall(
  ParameterIndex: Integer): SmallInt;
var I: Int64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, I);
  Result := SmallInt(I);
end;

{**
  get the current SQL string
}
function TZAbstractPreparedStatement2.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

function TZAbstractPreparedStatement2.GetTime(
  ParameterIndex: Integer): TDateTime;
begin
  GetDatetime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
  Result := Frac(Result);
end;

function TZAbstractPreparedStatement2.GetTimestamp(
  ParameterIndex: Integer): TDateTime;
begin
  GetDatetime(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement2.GetUInt(
  ParameterIndex: Integer): Cardinal;
var U: UInt64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, U);
  Result := Cardinal(U);
end;

function TZAbstractPreparedStatement2.GetULong(ParameterIndex: Integer): UInt64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
end;

function TZAbstractPreparedStatement2.GetValue(
  ParameterIndex: Integer): TZVariant;
begin

end;

function TZAbstractPreparedStatement2.GetWord(ParameterIndex: Integer): Word;
var U: UInt64;
begin
  GetOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, U);
  Result := Word(U);
end;

procedure TZAbstractPreparedStatement2.IsNull(Index: Integer;
  out Result: Boolean);
begin
  CheckParameterIndex(Index);
  Result := BindList[Index].BindType = zbtNull
end;

function TZAbstractPreparedStatement2.IsNull(ParameterIndex: Integer): Boolean;
begin
  IsNull(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Result);
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

procedure TZAbstractPreparedStatement2.RegisterParameter(ParameterIndex: Integer;
  SQLType: TZSQLType; ParamType: TZParamType; ParamSize: LengthInt);
var BindValue: PZBindValue;
begin
  {$IFNDEF GENERIC_INDEX}ParameterIndex := ParameterIndex-1;{$ENDIF}
  CheckParameterIndex(ParameterIndex);
  BindValue := BindList[ParameterIndex];
  BindValue^.ParamType := ParamType;
  BindValue^.SQLType   := SQLType;
  BindValue^.ParamSize := ParamSize;
end;

procedure TZAbstractPreparedStatement2.ReleaseImmediat(
  const Sender: IImmediatelyReleasable);
begin
  FPrepared := False;
  FExecCount := 0;
  inherited ReleaseImmediat(Sender);
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
  const Value: Extended);
begin
  BindDouble(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stBigDecimal, Value);
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
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  BindBoolean(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value);
end;

{**
  Sets the designated parameter to a Java <code>byte</code> value.
  The driver converts this
  to an SQL <code>Byte</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetByte(ParameterIndex: Integer;
  Value: Byte);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stByte, Value);
end;

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

procedure TZAbstractPreparedStatement2.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType);
var aArray: TZArray;
begin
  if FSupportsDMLBatchArrays then begin
    ValidateArraySizeAndType(Pointer(Value), SQLType, VariantType, ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
    aArray.VArray := Pointer(Value);
    aArray.VArrayVariantType := VariantType;
    aArray.VArrayType := Ord(SQLType);
    aArray.VIsNullArray := nil;
    aArray.VIsNullArrayType := 0;
    aArray.VIsNullArrayVariantType := vtNull;
    BindArray(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, aArray);
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

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractPreparedStatement2.SetNull(ParameterIndex: Integer;
  SQLType: TZSQLType);
begin
  BindNull(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, SQLType);
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
  Sets the designated parameter to a Java <code>usigned int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetUInt(ParameterIndex: Integer;
  Value: Cardinal);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stLongWord, Value);
end;

{**
  Sets the designated parameter to a Java <code>unsigned long</code> value.
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
  {$IFDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex-1;
  {$ENDIF}
  case Value.VType of
    vtBoolean: BindBoolean(ParameterIndex, Value.VBoolean);
    vtInteger: BindSignedOrdinal(ParameterIndex, stLong, Value.VInteger);
    vtUInteger: BindSignedOrdinal(ParameterIndex, stULong, Value.VUInteger);
    vtFloat: BindDouble(ParameterIndex, stDouble, Value.VFloat);
    vtUnicodeString: SetUnicodeString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value.VUnicodeString);
    vtRawByteString: SetRawByteString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value.VRawByteString);
    vtAnsiString:    SetAnsiString(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value.VAnsiString);
    vtUTF8String:    SetUTF8String(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value.VUTF8String);
    vtCharRec:       SetCharRec(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value.VCharRec);
    vtDateTime:      BindDateTime(ParameterIndex, stTimeStamp, Value.VDateTime);
    vtBytes:         BindBinary(ParameterIndex, stBytes, Pointer(Value.VBytes), Length(Value.VBytes));
    vtArray:         BindArray(ParameterIndex,Value.VArray);
    vtInterface:
      if Supports(Value.VInterface, IZBlob, TempBlob) then begin
        if TempBlob.IsClob
        then BindLob(ParameterIndex, stAsciiStream, TempBlob)
        else BindLob(ParameterIndex, stBinaryStream, TempBlob);
        TempBlob := nil;
      end else
        raise EZSQLException.Create(sUnsupportedOperation);
    else BindNull(ParameterIndex, stUnknown);
  end;
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement2.SetWord(ParameterIndex: Integer;
  Value: Word);
begin
  BindUnsignedOrdinal(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, stWord, Value);
end;

procedure TZAbstractPreparedStatement2.SetWSQL(const Value: ZWideString);
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
procedure TZAbstractPreparedStatement2.Unprepare;
begin
  if Assigned(FOpenResultSet) then begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
  LastResultSet := nil;
  UnPrepareInParameters;
  FPrepared := False;
  FExecCount := 0;
  FInitialArrayCount := 0;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement2.UnPrepareInParameters;
begin
  FBindList.Clear;
end;

procedure TZAbstractPreparedStatement2.ValidateArraySizeAndType(
  const Value: Pointer; SQLType: TZSQLType; VariantType: TZVariantType;
  ParamIndex: Integer);
var Len: ArrayLenInt;
begin
  if Value = nil then Exit;
  case SQLType of
    stUnknown: raise Exception.Create('Invalid SQLType for Array binding!');
    stString: if not (VariantType in [vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stUnicodeString: if not (VariantType in [vtUnicodeString, vtCharRec]) then
          raise Exception.Create('Invalid Variant-Type for String-Array binding!');
    stArray, stDataSet:
          raise Exception.Create(sUnsupportedOperation);
  end;
  Len := {%H-}PArrayLenInt({%H-}NativeUInt(Value) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
  if (ParamIndex = 0) then
    FInitialArrayCount := Len
  else if (FInitialArrayCount <> 0) and (Len <> FInitialArrayCount) and (SQLType <> stDataSet) then
    raise Exception.Create('Array count does not equal with initial count!')
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
    if Value.IsClob then
      Value.GetPAnsiChar(ConSettings^.ClientCodePage.CP)
    else begin
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

function TZRawPreparedStatement.GetAnsiString(
  ParameterIndex: Integer): AnsiString;
var P: PAnsiChar;
  L: LengthInt;
begin
  GetPAnsichar(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, P);
  if ZCompatibleCodePages(zOSCodePage, ConSettings^.ClientCodePage.CP) then
    ZSetString(P, L, Result)
  else begin
    FUniTemp := PRawToUnicode(p, l, ConSettings^.ClientCodePage.CP);
    Result := ZUnicodeToRaw(FUniTemp, zOSCodePage);
  end;
end;

procedure TZRawPreparedStatement.GetPAnsiChar(Index: Integer;
  out nBytes: LengthInt; var Result: PAnsiChar);
var
  BindValue: PZBindValue;
begin
  CheckParameterIndex(Index);
  BindValue := BindList[Index];
  if BindValue.ParamType in [zptOutput, zptInputOutput, zptResult] then
  if BindValue.BindType = zbtRawString then begin
    Result := BindValue.Value;
    nBytes := Length(RawByteString(BindValue.Value));
  end else if BindValue.BindType = zbtCharByRef then begin
    Result := PZCharRec(BindValue.Value).P;
    nBytes := PZCharRec(BindValue.Value).Len;
  end;
end;

function TZRawPreparedStatement.GetPChar(ParameterIndex: Integer): PChar;
var
  {$IFDEF UNICODE}
  P: PAnsiChar;
  {$ENDIF}
  L: LengthInt;
begin
  {$IFDEF UNICODE}
  GetPAnsichar(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, P);
  FUniTemp := PRawToUnicode(P, L, ConSettings^.ClientCodePage.CP);
  Result := Pointer(FUniTemp);
  {$ELSE}
  GetPAnsichar(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, Result);
  {$ENDIF}
end;

function TZRawPreparedStatement.GetRawByteString(
  ParameterIndex: Integer): RawByteString;
var P: PAnsiChar;
  L: LengthInt;
begin
  GetPAnsichar(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, P);
  ZSetString(P, L, Result);
end;

function TZRawPreparedStatement.GetString(ParameterIndex: Integer): String;
var P: PAnsiChar;
  L: LengthInt;
begin
  GetPAnsichar(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, P);
  {$IFDEF UNICODE}
  Result := PRawToUnicode(P, L, ConSettings^.ClientCodePage.CP);
  {$ELSE}
  ZSetString(P, L, Result);
  {$ENDIF}
end;

function TZRawPreparedStatement.GetUnicodeString(
  ParameterIndex: Integer): ZWideString;
var P: PAnsiChar;
  L: LengthInt;
begin
  GetPAnsichar(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, P);
  Result := PRawToUnicode(P, L, ConSettings^.ClientCodePage.CP);
end;

function TZRawPreparedStatement.GetUTF8String(
  ParameterIndex: Integer): UTF8String;
var P: PAnsiChar;
  L: LengthInt;
begin
  GetPAnsichar(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, L, P);
  if ZCompatibleCodePages(zCP_UTF8, ConSettings^.ClientCodePage.CP) then
    ZSetString(P, L, Result)
  else begin
    FUniTemp := PRawToUnicode(p, l, ConSettings^.ClientCodePage.CP);
    Result := ZUnicodeToRaw(FUniTemp, zCP_UTF8);
  end;
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
procedure TZRawPreparedStatement.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  if ZCompatibleCodePages(ZOSCodePage, ConSettings^.ClientcodePage.CP)
  then BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Value)
  else BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF},
    ConSettings^.ConvFuncs.ZAnsiToRaw(Value, ConSettings.ClientcodePage.CP));
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
procedure TZRawPreparedStatement.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  if ZCompatibleCodepages(zCP_UTF8, ConSettings^.ClientCodePage.CP)
  then BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Value)
  else BindRawStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    ConSettings^.ConvFuncs.ZUTF8ToRaw(Value, ConSettings.ClientcodePage.CP));
end;

{ TZUCS2PreparedStatement }

procedure TZUCS2PreparedStatement.BindUniStr(Index: Integer;
  const Value: ZWideString);
begin
  CheckParameterIndex(Index);
  FBindList.Put(Index, stUnicodeString, Value);
end;

procedure TZUCS2PreparedStatement.BindUniStr(Index: Integer;
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
procedure TZUCS2PreparedStatement.SetAnsiString(ParameterIndex: Integer;
  const Value: AnsiString);
begin
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    PRawToUnicode(Pointer(Value), Length(Value), zOSCodePage))
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
procedure TZUCS2PreparedStatement.SetCharRec(ParameterIndex: Integer;
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
procedure TZUCS2PreparedStatement.SetDefaultValue(ParameterIndex: Integer;
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
procedure TZUCS2PreparedStatement.SetRawByteString(
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
procedure TZUCS2PreparedStatement.SetString(ParameterIndex: Integer;
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
procedure TZUCS2PreparedStatement.SetUnicodeString(
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
procedure TZUCS2PreparedStatement.SetUTF8String(ParameterIndex: Integer;
  const Value: UTF8String);
begin
  BindUniStr(ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF},
    PRawToUnicode(Pointer(Value), Length(Value), zCP_UTF8))
end;

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

{ TZUCS2ParamDetectPreparedStatement }

function TZUCS2ParamDetectPreparedStatement.GetUnicodeEncodedSQL(const SQL:
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
procedure TZUCS2ParamDetectPreparedStatement.Unprepare;
begin
  inherited UnPrepare;
  SetLength(FCachedQueryUni, 0);
end;

end.

