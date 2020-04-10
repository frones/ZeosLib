{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Core classes and interfaces                 }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZClasses;

interface

{$I ZCore.inc}

uses
  SysUtils, Classes, SyncObjs, FmtBCD,
  ZCompatibility, ZSysUtils
  {$IF defined(MSWINDOWS) and not defined(FPC)}, Windows{$IFEND} //some old comp. -> INFINITE
  {$IFDEF NO_UNIT_CONTNRS},System.Generics.Collections{$ENDIF};

const
  ZEOS_MAJOR_VERSION = 7;
  ZEOS_MINOR_VERSION = 3;
  ZEOS_SUB_VERSION = 0;
  ZEOS_STATUS = 'alpha';
  ZEOS_VERSION = Char(48+ZEOS_MAJOR_VERSION)+'.'+
                 Char(48+ZEOS_MINOR_VERSION)+'.'+
                 Char(48+ZEOS_SUB_VERSION)+'-'+ZEOS_STATUS;

type
  {$IFDEF OLDFPC}
  PDateTime = ^TDateTime;

  TAggregatedObject = class(TObject)
  private
    FController: Pointer;
    function GetController: IInterface;
  protected
    {$IFDEF FPC2_5UP}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$ELSE}
    function QueryInterface(const iid : tguid;out obj) : longint;stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
    {$ENDIF}
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  TContainedObject = class(TAggregatedObject, IInterface)
  protected
    {$IFDEF FPC2_5UP}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; override;
    {$ELSE}
    function QueryInterface(const iid : tguid;out obj) : longint;stdcall;
    {$ENDIF}

  end;
  {$ENDIF}

  {** Replacement for generic interface type. }
  IZInterface = IUnknown;

  {** Represents an interface for all abstract object. }
  IZObject = interface(IZInterface)
    ['{EF46E5F7-00CF-4DDA-BED0-057D6686AEE0}']
    function Equals(const Value: IZInterface): Boolean;
    function GetHashCode: LongInt;
    function Clone: IZInterface;
    function ToString: string;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

  {** Represents a fake interface for coparable objects. }
  IZComparable = interface(IZObject)
    ['{04112081-F07B-4BBF-A757-817816EB67C1}']
  end;

  {** Represents an interface to clone objects. }
  IZClonnable = interface(IZObject)
    ['{ECB7F3A4-7B2E-4130-BA66-54A2D43C0149}']
  end;

  {** Represents a generic collection iterator interface. }
  IZIterator = interface(IZObject)
    ['{D964DDD0-2308-4D9B-BD36-5810632512F7}']
    function HasNext: Boolean;
    function Next: IZInterface;
  end;

  {** Represents a collection of object interfaces. }
  IZCollection = interface(IZClonnable)
    ['{51417C87-F992-4CAD-BC53-CF3925DD6E4C}']

    function Get(Index: Integer): IZInterface;
    procedure Put(Index: Integer; const Item: IZInterface);
    function IndexOf(const Item: IZInterface): Integer;
    function GetCount: Integer;
    function GetIterator: IZIterator;

    function First: IZInterface;
    function Last: IZInterface;

    function Add(const Item: IZInterface): Integer;
    procedure Insert(Index: Integer; const Item: IZInterface);
    function Remove(const Item: IZInterface): Integer;

    procedure Exchange(Index1, Index2: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;

    function Contains(const Item: IZInterface): Boolean;
    function ContainsAll(const Col: IZCollection): Boolean;
    function AddAll(const Col: IZCollection): Boolean;
    function RemoveAll(const Col: IZCollection): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IZInterface read Get write Put; default;
  end;

  {** Represents a hash map interface. }
  IZHashMap = interface(IZClonnable)
    ['{782C64F4-AD09-4F56-AF2B-E4193A05BBCE}']

    function Get(const Key: IZInterface): IZInterface;
    procedure Put(const Key: IZInterface; const Value: IZInterface);
    function GetKeys: IZCollection;
    function GetValues: IZCollection;
    function GetCount: Integer;

    function Remove(const Key: IZInterface): Boolean;
    procedure Clear;

    property Count: Integer read GetCount;
    property Keys: IZCollection read GetKeys;
    property Values: IZCollection read GetValues;
  end;

  {** Represents a stack interface. }
  IZStack = interface(IZClonnable)
    ['{8FEA0B3F-0C02-4E70-BD8D-FB0F42D4497B}']

    function Peek: IZInterface;
    function Pop: IZInterface;
    procedure Push(const Value: IZInterface);
    function GetCount: Integer;

    property Count: Integer read GetCount;
  end;

  {** Implements an abstract interfaced object. }
  // New TObject contains some methods with the same names but it has different
  // result/parameter types so we just hide the inherited methods
  TZAbstractObject = class(TInterfacedObject, IZObject)
  public
    // Parameter type differs from base (TObject)
    function Equals(const Value: IZInterface): Boolean; {$IFDEF WITH_NEWTOBJECT} reintroduce; {$ENDIF} virtual;
    // Result type differs from base (PtrInt @ FPC, Integer @ Delphi)
    function GetHashCode: LongInt; {$IFDEF WITH_NEWTOBJECT} reintroduce; {$ENDIF} virtual;
    function Clone: IZInterface; virtual;
    // Result type differs from base (ansistring/shortstring @ FPC, string @ Delphi)
    function ToString: string; {$IFDEF WITH_NEWTOBJECT} reintroduce; {$ENDIF} virtual;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

  {$IFDEF NO_UNIT_CONTNRS}
  TObjectList = class(TObjectList<TObject>);
  {$ENDIF}

  {** EH:
    implements a threaded timer which does not belong to the
    windows message queue nor VCL/FMX}
  TZThreadTimer = class(TObject)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TThreadMethod;
    FThread: TThread;
    FSignal: TEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(Value: TThreadMethod);
  public
    constructor Create; overload;
    constructor Create(OnTimer: TThreadMethod;
      Interval: Cardinal; Enabled: Boolean); overload;
    destructor Destroy; override;
    procedure Reset;
  public
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TThreadMethod read FOnTimer write SetOnTimer;
  end;

  TCallbackPatch = packed record   //does the job nice .. with stdcall
    popEax          : byte;     // $58 pop EAX
    pushSelf_opcode : byte;     // $B8
    pushSelf_self   : Pointer;  // self
    pushEax         : byte;     // $50 push EAX
    jump_opcode     : byte;     // $E9
    jump_target     : Pointer;  // @TObject.DummyCallback
  end;

  {** implements a dispatcher to map C-DLL callbacks
      to a pascal TMethod of Object}
  TZMethodToDllCallbackDispatcher = class(TInterfacedObject)
  private
    FProcedure: TCallbackPatch;
  protected
    function GetProcedureAddress: Pointer;
  public
    constructor Create(const Instance: TObject; methodAddr: pointer);
  end;

  {** EH: implements a buffered raw encoded writer }
  TZRawSQLStringWriter = class(TObject)
  private
    function FlushBuff(Var Dest: RawByteString; ReservedLen: LengthInt): PAnsiChar; overload;
    procedure AddOrd32(Value: Cardinal; Digits: Byte; Negative: Boolean; var Result: RawByteString);
    procedure AddOrd64(const Value: UInt64; Digits: Byte; Negative: Boolean; var Result: RawByteString);
  protected
    FBuf, //the buffer we use as temporary storage
    FPos, //the current position of the buffer. Points always to the first writeable char
    FEnd: PAnsiChar; //the end of the buffer
  public
    constructor Create(AnsiCharCapacity: Integer);
    destructor Destroy; override;
  public
    procedure IncreaseCapacityTo(AnsiCharCapacity: Integer; var Result: RawByteString);
    procedure AddChar(Value: AnsiChar;      var Result: RawByteString);
    procedure AddText(Value: PAnsiChar; L: LengthInt; var Result: RawByteString); overload;
    procedure AddText(const Value: RawByteString; var Result: RawByteString); overload;
    procedure AddHexBinary(Value: PByte; L: LengthInt; ODBC: Boolean; var Result: RawByteString); overload;
    procedure AddHexBinary(const Value: TBytes; ODBC: Boolean; var Result: RawByteString); overload;
    procedure AddAscii7UTF16Text(const AsciiValue: UnicodeString; var Result: RawByteString);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    procedure AddText(const AsciiValue: UnicodeString; var Result: RawByteString); overload;
    {$ENDIF}
    procedure AddTextQuoted(Value: PAnsiChar; L: LengthInt; QuoteChar: AnsiChar; var Result: RawByteString); overload;
    procedure AddTextQuoted(const Value: RawByteString; QuoteChar: AnsiChar; var Result: RawByteString); overload;
    procedure AddOrd(Value: Byte;           var Result: RawByteString); overload;
    procedure AddOrd(Value: ShortInt;       var Result: RawByteString); overload;
    procedure AddOrd(Value: Word;           var Result: RawByteString); overload;
    procedure AddOrd(Value: SmallInt;       var Result: RawByteString); overload;
    procedure AddOrd(Value: Cardinal;       var Result: RawByteString); overload;
    procedure AddOrd(Value: Integer;        var Result: RawByteString); overload;
    procedure AddOrd(const Value: UInt64;   var Result: RawByteString); overload;
    procedure AddOrd(const Value: Int64;    var Result: RawByteString); overload;
    procedure AddOrd(Value: Pointer;        var Result: RawByteString); overload;
    procedure AddFloat(Value: Single;       var Result: RawByteString); overload;
    procedure AddFloat(const Value: Double; var Result: RawByteString); overload;
    procedure AddDecimal(const Value: Currency;   var Result: RawByteString); overload;
    procedure AddDecimal(const Value: TBCD; var Result: RawByteString); overload;
    procedure AddDate(const Value: TDateTime; const Format: String; var Result: RawByteString); overload;
    procedure AddDate(const Value: TZDate; const Format: String; var Result: RawByteString); overload;
    procedure AddTime(const Value: TDateTime; const Format: String; var Result: RawByteString); overload;
    procedure AddTime(const Value: TZTime; const Format: String; var Result: RawByteString); overload;
    procedure AddDateTime(const Value: TDateTime; const Format: String; var Result: RawByteString);
    procedure AddTimeStamp(const Value: TZTimeStamp; const Format: String; var Result: RawByteString);
    procedure AddGUID(const Value: TGUID; Options: TGUIDConvOptions; var Result: RawByteString);
    procedure Finalize(var Result: RawByteString);
    procedure CancelLastComma(var Result: RawByteString);
    procedure CancelLastCharIfExists(Value: AnsiChar; var Result: RawByteString);
    procedure ReplaceOrAddLastChar(OldChar, NewChar: AnsiChar; var Result: RawByteString);
    procedure AddLineFeedIfNotEmpty(var Result: RawByteString);
  end;

  {** EH: implements a buffered UTF16 encoded writer }
  TZUnicodeSQLStringWriter = class(TObject)
  private
    function FlushBuff(Var Dest: UnicodeString; ReservedLen: LengthInt): PWideChar; overload;
    procedure AddOrd32(Value: Cardinal; Digits: Byte; Negative: Boolean; var Result: UnicodeString);
    procedure AddOrd64(const Value: UInt64; Digits: Byte; Negative: Boolean; var Result: UnicodeString);
  protected
    FBuf, //the buffer we use as temporary storage
    FPos, //the current position of the buffer. Points always to the first writeable char
    FEnd: PWideChar; //the end of the buffer
  public
    constructor Create(WideCharCapacity: Integer);
    destructor Destroy; override;
  public
    procedure IncreaseCapacityTo(WideCharCapacity: Integer; var Result: UnicodeString);
    procedure AddText(Value: PWideChar; L: LengthInt; var Result: UnicodeString); overload;
    procedure AddText(const Value: UnicodeString; var Result: UnicodeString); overload;
    procedure AddAscii7Text(Value: PAnsiChar; L: LengthInt; var Result: UnicodeString); overload;
    procedure AddHexBinary(Value: PByte; L: LengthInt; ODBC: Boolean; var Result: UnicodeString); overload;
    procedure AddHexBinary(const Value: TBytes; ODBC: Boolean; var Result: UnicodeString); overload;
    procedure AddChar(Value: WideChar;      var Result: UnicodeString);
    procedure AddOrd(Value: Byte;           var Result: UnicodeString); overload;
    procedure AddOrd(Value: ShortInt;       var Result: UnicodeString); overload;
    procedure AddOrd(Value: Word;           var Result: UnicodeString); overload;
    procedure AddOrd(Value: SmallInt;       var Result: UnicodeString); overload;
    procedure AddOrd(Value: Cardinal;       var Result: UnicodeString); overload;
    procedure AddOrd(Value: Integer;        var Result: UnicodeString); overload;
    procedure AddOrd(const Value: UInt64;   var Result: UnicodeString); overload;
    procedure AddOrd(const Value: Int64;    var Result: UnicodeString); overload;
    procedure AddFloat(Value: Single;       var Result: UnicodeString); overload;
    procedure AddFloat(const Value: Double; var Result: UnicodeString); overload;
    procedure AddDecimal(const Value: Currency; var Result: UnicodeString); overload;
    procedure AddDecimal(const Value: TBCD; var Result: UnicodeString); overload;
    procedure AddDate(const Value: TDateTime; const Format: String; var Result: UnicodeString); overload;
    procedure AddDate(const Value: TZDate; const Format: String; var Result: UnicodeString); overload;
    procedure AddTime(const Value: TDateTime; const Format: String; var Result: UnicodeString); overload;
    procedure AddTime(const Value: TZTime; const Format: String; var Result: UnicodeString); overload;
    procedure AddDateTime(const Value: TDateTime; const Format: String; var Result: UnicodeString);
    procedure AddTimeStamp(const Value: TZTimeStamp; const Format: String; var Result: UnicodeString);
    procedure AddGUID(const Value: TGUID; Options: TGUIDConvOptions; var Result: UnicodeString);
    procedure Finalize(var Result: UnicodeString);
    procedure CancelLastComma(var Result: UnicodeString);
    procedure CancelLastCharIfExists(Value: WideChar; var Result: UnicodeString);
    procedure ReplaceOrAddLastChar(OldChar, NewChar: WideChar; var Result: UnicodeString);
    procedure AddLineFeedIfNotEmpty(var Result: UnicodeString);
  end;

  TZSQLStringWriter = {$IFDEF UNICODE}TZUnicodeSQLStringWriter{$ELSE}TZRawSQLStringWriter{$ENDIF};
  SQLString = {$IFDEF UNICODE}UnicodeString{$ELSE}RawByteString{$ENDIF};

  {** Modified comparison function. }
  TZListSortCompare = function (Item1, Item2: Pointer): Integer of object;

  {** Modified list of pointers. }
  TZSortedList = class({$IFDEF TLIST_IS_DEPRECATED}TObject{$ELSE}TList{$ENDIF})
  {$IFDEF TLIST_IS_DEPRECATED}
  private
    FList: TPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Item: Pointer): Pointer; inline;
    function First: Pointer; inline;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    function Remove(Item: Pointer): Integer; inline;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: TPointerList read FList;
    property Capacity: Integer read FCapacity write SetCapacity;
  {$ENDIF}
    procedure Sort(Compare: TZListSortCompare);
  end;

  {$IF NOT DECLARED(EArgumentException)}
  type
    EArgumentException = Class(Exception);
  {$IFEND}

implementation

uses ZMessages, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS},AnsiStrings{$ENDIF}; //need for inlined FloatToText;

{$IFDEF oldFPC}

{ TAggregatedObject }

constructor TAggregatedObject.Create(const Controller: IInterface);
begin
  FController := Pointer(Controller);
end;

function TAggregatedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

{$IFDEF FPC2_5UP}
function TAggregatedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
{$ELSE}
function TAggregatedObject.QueryInterface(const iid : tguid;out obj) : longint;stdcall;
{$ENDIF}
begin
  Result := IInterface(FController).QueryInterface(IID, Obj);
end;

function TAggregatedObject._AddRef: longint;
begin
  Result := IInterface(FController)._AddRef;
end;

function TAggregatedObject._Release : longint;
begin
  Result := IInterface(FController)._Release;
end;

{ TContainedObject }

{$IFDEF FPC2_5UP}
function TContainedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TContainedObject.QueryInterface(const iid : tguid;out obj) : longint;stdcall;
{$ENDIF}

begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{$ENDIF}

{ TZAbstractObject }

{**
  Checks is the specified value equals to this object.
  @param Value an interface to some object.
  @return <code>True</code> if the objects are identical.
}
function TZAbstractObject.Equals(const Value: IZInterface): Boolean;
begin
  if Value <> nil then
  begin
    Result := (IZInterface(Self) = Value)
      or ((Self as IZInterface) = (Value as IZInterface));
  end else
   Result := False;
end;

{**
  Gets a unique hash for this object.
  @return a unique hash for this object.
}
function TZAbstractObject.GetHashCode: LongInt;
begin
  Result := LongInt(Self);
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAbstractObject.Clone: IZInterface;
begin
  raise Exception.Create(SClonningIsNotSupported);
  result := nil;
end;

{**
  Checks is this object implements a specified interface.
  @param IId an interface id.
  @return <code>True</code> if this object support the interface.
}
function TZAbstractObject.InstanceOf(const IId: TGUID): Boolean;
begin
  Result := GetInterfaceEntry(IId) <> nil;
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAbstractObject.ToString: string;
begin
  Result := Format('%s <%p>', [ClassName, Pointer(Self)])
end;

type
  TZIntervalThread = class(TThread)
  private
    FSignal: TEvent;
    FInterval: Cardinal;
    FOnTimer: TThreadMethod;
    FActive: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Signal: TEvent);
  end;

{ TZThreadTimer }

constructor TZThreadTimer.Create;
begin
  inherited Create;
  FSignal := TSimpleEvent.Create;
end;

constructor TZThreadTimer.Create(OnTimer: TThreadMethod;
  Interval: Cardinal; Enabled: Boolean);
begin
  Create;
  FInterval := Interval;
  FOnTimer := OnTimer;
  FEnabled := Enabled;
  FThread := TZIntervalThread.Create(FSignal);
  TZIntervalThread(FThread).FOnTimer := FOnTimer;
  TZIntervalThread(FThread).FInterval := FInterval;
  TZIntervalThread(FThread).Suspended := False; //start thread
  Reset;
end;

destructor TZThreadTimer.Destroy;
begin
  FThread.Terminate;
  FSignal.SetEvent; //signal to break the waittime
  FThread.WaitFor;
  FreeAndNil(FThread);
  FreeAndNil(FSignal);
  inherited;
end;

procedure TZThreadTimer.Reset;
  procedure SignalThread;
  begin
    if FThread <> nil then begin
      FSignal.SetEvent; //signal thread should Start now
      while FSignal.WaitFor(1) = wrSignaled do; //wait until thread confirms event
    end;
  end;
begin
  SignalThread; //change active state
  TZIntervalThread(FThread).FOnTimer := FOnTimer;
  if FEnabled and Assigned(FOnTimer) and (FInterval > 0)
  then TZIntervalThread(FThread).FInterval := FInterval
  else TZIntervalThread(FThread).FInterval := INFINITE;
  SignalThread; //change active state
end;

procedure TZThreadTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    Reset;
  end;
end;

procedure TZThreadTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then begin
    FInterval := Value;
    Reset;
  end;
end;

procedure TZThreadTimer.SetOnTimer(Value: TThreadMethod);
begin
  if @FOnTimer <> @Value then begin
    FOnTimer := Value;
    Reset;
  end;
end;

{ TZIntervalThread }

constructor TZIntervalThread.Create(Signal: TEvent);
begin
  inherited Create(True); //suspended
  FActive := True;
  FSignal := Signal;
end;

procedure TZIntervalThread.Execute;
begin
  while not Terminated do
    case FSignal.WaitFor(FInterval) of
      wrTimeout:  if FActive and Assigned(FOnTimer) and (FInterval <> INFINITE) then
                    FOnTimer;
      wrSignaled: begin
                    FActive := not FActive;
                    FSignal.ResetEvent;
                  end;
      else        Break;
    end;
end;

constructor TZMethodToDllCallbackDispatcher.Create(const Instance: TObject;
  methodAddr: Pointer);
begin
  inherited Create;
  with FProcedure do begin
    popEax          := $58;
    pushSelf_opcode := $68;
    pushSelf_self   := Instance;
    pushEax         := $50;
    jump_opcode     := $E9;
    jump_target     := {%H-}PAnsiChar(PAnsiChar(methodAddr)-PAnsiChar(@FProcedure))-SizeOf(TCallbackPatch);
  end;
end;

{** Returns an address which should be registered to the DLL interface }
function TZMethodToDllCallbackDispatcher.GetProcedureAddress: Pointer;
begin
  Result := @FProcedure;
end;

{ TZRawSQLStringWriter }

procedure TZRawSQLStringWriter.AddHexBinary(Value: PByte; L: LengthInt;
  ODBC: Boolean; var Result: RawByteString);
var P: PAnsiChar;
  LTotal: LengthInt;
begin
  LTotal := ((L+1) shl 1) + Ord(not ODBC);
  if (FPos+LTotal < FEnd) then begin
    P := FPos;
    Inc(FPos, LTotal);
  end else
    P := FlushBuff(Result, LTotal);
  If ODBC then begin
    PByte(P  )^ := Ord('0');
    PByte(P+1)^ := Ord('x');
  end else begin
    PByte(P  )^ := Ord('x');
    PByte(P+1)^ := Ord(#39);
  end;
  if Value <> nil then
    ZBinToHex(PAnsiChar(Value), P+2, L);
  if not ODBC then
    PByte(P+LTotal-1)^ := Ord(#39);
end;

procedure TZRawSQLStringWriter.AddHexBinary(const Value: TBytes; ODBC: Boolean;
  var Result: RawByteString);
begin
  AddHexBinary(Pointer(Value), Length(Value), ODBC, Result);
end;

procedure TZRawSQLStringWriter.AddLineFeedIfNotEmpty(var Result: RawByteString);
begin
  if (Pointer(Result) <> nil) or (FPos > FBuf) then
    AddText(LineEnding, Result);
end;

procedure TZRawSQLStringWriter.AddAscii7UTF16Text(
  const AsciiValue: UnicodeString; var Result: RawByteString);
var PW: PWidechar;
  PA: PAnsiChar;
  L: LengthInt;
begin
  PW := Pointer(AsciiValue);
  if PW = nil then Exit;
  L := Length(AsciiValue);
  if L < (FEnd-FPos) then begin
    PA := FPos;
    Inc(FPos, L);
  end else
    PA := FlushBuff(Result, L);
  while L > 0 do begin
    PByte(PA)^ := PWord(PW)^;
    Inc(PA);
    Inc(PW);
    Dec(L);
  end;
end;

procedure TZRawSQLStringWriter.AddChar(Value: AnsiChar; var Result: RawByteString);
var P: PAnsiChar;
begin
  if FPos < FEnd then begin
    PByte(FPos)^ := Byte(Value);
    Inc(FPos);
  end else begin
    P := FlushBuff(Result, 1);
    PByte(P)^ := Byte(Value);
  end;
end;

procedure TZRawSQLStringWriter.AddText(Value: PAnsiChar; L: LengthInt;
  var Result: RawByteString);
var P: PAnsiChar;
begin
  if (Value = nil) or (L = 0) then Exit;
  if (FPos + L < FEnd) then begin
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, FPos^, L);
    Inc(FPos, L);
  end else begin
    P := FlushBuff(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, P^, L);
  end;
end;

procedure TZRawSQLStringWriter.AddOrd(Value: Cardinal; var Result: RawByteString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd32(Value, Digits, False, Result);
end;

procedure TZRawSQLStringWriter.AddOrd(Value: Integer; var Result: RawByteString);
var Digits: Byte;
  C: Cardinal;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, C, Negative);
  AddOrd32(C, Digits, Negative, Result);
end;

procedure TZRawSQLStringWriter.CancelLastCharIfExists(Value: AnsiChar;
  var Result: RawByteString);
var P: PAnsichar;
  L: LengthInt;
begin
  if (FPos > FBuf) then begin
    if PByte(FPos-1)^ = Byte(Value) then
      Dec(FPos);
  end else begin
    P := Pointer(Result);
    L := Length(Result)-{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}2{$ELSE}1{$ENDIF};
    Inc(P, L);
    if (L >= 1) and (PByte(P)^ = Byte(Value)) then
      SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
  end;
end;

procedure TZRawSQLStringWriter.CancelLastComma(var Result: RawByteString);
var P: PAnsichar;
  L: LengthInt;
begin
  if (FPos > FBuf) then begin
    if PByte(FPos-1)^ = Ord(',') then
      Dec(FPos);
  end else begin
    P := Pointer(Result);
    L := Length(Result)-{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}2{$ELSE}1{$ENDIF};
    Inc(P, L);
    if (L >= 1) and (PByte(P)^ = Ord(',')) then
      SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
  end;
end;

constructor TZRawSQLStringWriter.Create(AnsiCharCapacity: Integer);
begin
  if AnsiCharCapacity < High(Byte) then
    AnsiCharCapacity := High(Byte);
  GetMem(FBuf, AnsiCharCapacity);
  FPos := FBuf;
  FEnd := FBuf+AnsiCharCapacity;
end;

destructor TZRawSQLStringWriter.Destroy;
begin
  if FBuf <> nil then
    FreeMem(FBuf);
end;

procedure TZRawSQLStringWriter.Finalize(var Result: RawByteString);
begin
  if FPos > FBuf then
    FlushBuff(Result, 0);
end;

function TZRawSQLStringWriter.FlushBuff(var Dest: RawByteString;
  ReservedLen: LengthInt): PAnsiChar;
var LRes, L2: LengthInt;
begin
  LRes := Length(Dest);
  L2 := (FPos-FBuf);
  L2 := L2 + LRes;
  L2 := L2 + ReservedLen;
  SetLength(Dest, L2{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
  Result := Pointer(Dest);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  PByte(Result+L2)^ := Ord(#0);
  {$ENDIF}
  Inc(Result, LRes);
  if FPos > FBuf then begin
    LRes := (FPos-FBuf);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FBuf^, Result^, LRes);
    Inc(Result, LRes);
    FPos := FBuf;
  end;
end;

procedure TZRawSQLStringWriter.IncreaseCapacityTo(AnsiCharCapacity: Integer;
  var Result: RawByteString);
begin
  if AnsiCharCapacity < FEnd-FBuf then
    Exit;
  if FPos > FBuf then
    FlushBuff(Result,0);
  FreeMem(FBuf);
  GetMem(FBuf, AnsiCharCapacity);
  FPos := FBuf;
  FEnd := FBuf+AnsiCharCapacity;
end;

procedure TZRawSQLStringWriter.ReplaceOrAddLastChar(OldChar, NewChar: AnsiChar;
  var Result: RawByteString);
var P: PAnsichar;
  L: LengthInt;
  label setp;
begin
  if (FPos > FBuf) then begin
    P := FPos-1;
    if PByte(P)^ = Byte(OldChar)
    then PByte(P)^ := Ord(NewChar)
    else goto setp
  end else begin
    P := Pointer(Result);
    L := Length(Result)-{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}2{$ELSE}1{$ENDIF};
    Inc(P, L);
    if (L >= 1) and (PByte(P)^ = Byte(OldChar)) then
      PByte(P)^ := Byte(NewChar)
    else begin
setp: PByte(FPos)^ := Byte(NewChar);
      Inc(FPos);
    end;
  end;
end;

procedure TZRawSQLStringWriter.AddOrd32(Value: Cardinal; Digits: Byte;
  Negative: Boolean; var Result: RawByteString);
var P: PAnsiChar;
  D2: Byte;
begin
  D2 := Digits+Ord(Negative);
  if (FPos+D2 < FEnd) then begin
    P := FPos;
    Inc(FPos, D2);
  end else
    P := FlushBuff(Result, D2);
  if Negative then begin
    PByte(P)^ := Ord('-');
    Inc(P);
  end;
  IntToRaw(Value, P, Digits);
end;

procedure TZRawSQLStringWriter.AddOrd64(const Value: UInt64; Digits: Byte;
  Negative: Boolean; var Result: RawByteString);
var P: PAnsiChar;
  D2: Byte;
begin
  D2 := Digits+Ord(Negative);
  if (FPos+D2 < FEnd) then begin
    P := FPos;
    Inc(FPos, D2);
  end else
    P := FlushBuff(Result, D2);
  if Negative then begin
    PByte(P)^ := Ord('-');
    Inc(P);
  end;
  IntToRaw(Value, P, Digits);
end;

procedure TZRawSQLStringWriter.AddText(const Value: RawByteString;
  var Result: RawByteString);
begin
  AddText(Pointer(Value), Length(Value), Result);
end;

procedure TZRawSQLStringWriter.AddTextQuoted(const Value: RawByteString;
  QuoteChar: AnsiChar; var Result: RawByteString);
begin
  AddTextQuoted(Pointer(Value), Length(Value){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, QuoteChar, Result);
end;

procedure TZRawSQLStringWriter.AddTime(const Value: TZTime;
  const Format: String; var Result: RawByteString);
var L: LengthInt;
  P: PAnsiChar;
  Buffer: Array[0..cMaxTimeLenQuoted] of AnsiChar;
begin
  if (FPos + cMaxTimeLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := TimeToRaw(Value.Hour, Value.Minute, Value.Second,
    Value.Fractions, P, Format, True, Value.IsNegative);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddTimeStamp(const Value: TZTimeStamp;
  const Format: String; var Result: RawByteString);
var L: LengthInt;
  P: PAnsiChar;
  Buffer: Array[0..cMaxTimeStampLenQuoted] of AnsiChar;
begin
  if (FPos + cMaxTimeStampLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := ZSysUtils.DateTimeToRaw(Value.Year, Value.Month, Value.Day,
    Value.Hour, Value.Minute, Value.Second, Value.Fractions, P, Format, True, Value.IsNegative);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddTextQuoted(Value: PAnsiChar; L: LengthInt;
  QuoteChar: AnsiChar; var Result: RawByteString);
var
  P, Dest, PEnd, PFirst: PAnsiChar;
begin
  Dest := nil;
  P := Value;
  PEnd := P + L;
  PFirst := nil;
  while P < PEnd do begin
    if (AnsiChar(P^)=QuoteChar) then begin
      if Dest = nil then
        PFirst := P;
      Inc(Dest);
    end;
    Inc(P);
  end;
  if Dest = nil then begin //no quoteChars found?
    if (FPos+L+2 < FEnd) then begin
      Dest := FPos;
      Inc(FPos, L+2);
    end else
      Dest := FlushBuff(Result, L+2);
    AnsiChar(Dest^) := QuoteChar;
    if L > 0 then begin
      System.Move(Value^, (Dest+1)^, L);
      Inc(Dest, L+1);
    end else
      Inc(Dest);
    AnsiChar(Dest^) := QuoteChar;
    Exit;
  end;
  if (FPos+L+{%H-}NativeInt(Dest) +2 < FEnd) then begin
    Dest := FPos;
    Inc(FPos, L+{%H-}NativeInt(Dest)+2);
  end else
    Dest := FlushBuff(Result, L+{%H-}NativeInt(Dest)+2);
  AnsiChar(Dest^) := QuoteChar;
  Inc(Dest);
  P := PFirst;
  repeat
    Inc(P);
    Move(Value^, Dest^, (P - Value));
    Inc(Dest, P - Value);
    AnsiChar(Dest^) := QuoteChar;
    Inc(Dest);
    Value := P;
    while (P<PEnd) do if AnsiChar(P^)=QuoteChar
      then Break
      else Inc(P);
  until P = PEnd;
  Move(Value^, Dest^, (PEnd - Value));
  Inc(Dest, PEnd - Value);
  AnsiChar(Dest^) := QuoteChar;
end;

procedure TZRawSQLStringWriter.AddTime(const Value: TDateTime; const Format: String;
  var Result: RawByteString);
var H, M, S, MS: Word;
  L: LengthInt;
  P: PAnsiChar;
  Buffer: Array[0..cMaxTimeLenQuoted] of AnsiChar;
begin
  if (FPos + cMaxTimeLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  DecodeTime(Value, H, M, S, MS);
  L := TimeToRaw(H, M, S, MS*NanoSecsPerMSec, P, Format, True, False);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddOrd(const Value: UInt64; var Result: RawByteString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd64(Value, Digits, False, Result);
end;

procedure TZRawSQLStringWriter.AddOrd(const Value: Int64; var Result: RawByteString);
var Digits: Byte;
  U: UInt64;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, U, Negative);
  AddOrd64(U, Digits, Negative, Result);
end;

procedure TZRawSQLStringWriter.AddOrd(Value: Byte; var Result: RawByteString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd32(Value, Digits, False, Result);
end;

procedure TZRawSQLStringWriter.AddOrd(Value: ShortInt; var Result: RawByteString);
var B, Digits: Byte;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, B, Negative);
  AddOrd32(B, Digits, Negative, Result);
end;

procedure TZRawSQLStringWriter.AddOrd(Value: Word; var Result: RawByteString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd32(Value, Digits, False, Result);
end;

procedure TZRawSQLStringWriter.AddOrd(Value: SmallInt; var Result: RawByteString);
var Digits: Byte;
  W: Word;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, W, Negative);
  AddOrd32(W, Digits, Negative, Result);
end;

procedure TZRawSQLStringWriter.AddFloat(Value: Single; var Result: RawByteString);
var D: Double;
begin
  D := Value;
  AddFloat(D, Result);
end;

procedure TZRawSQLStringWriter.AddDate(const Value: TDateTime; const Format: String;
  var Result: RawByteString);
var Y, M, D: Word;
  L: LengthInt;
  P: PAnsiChar;
  Buffer: Array[0..cMaxDateLenQuoted] of AnsiChar;
begin
  if (FPos + cMaxDateLenQuoted < FEnd) //'-20199-09-11'
  then P := FPos
  else P := @Buffer[0];
  DecodeDate(Value, Y, M, D);
  L := DateToRaw(Y, M, D, P, Format, True, False);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddDate(const Value: TZDate; const Format: String;
  var Result: RawByteString);
var
  L: LengthInt;
  P: PAnsiChar;
  Buffer: Array[0..cMaxDateLenQuoted] of AnsiChar;
begin
  if (FPos + cMaxDateLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := DateToRaw(Value.Year, Value.Month, Value.Day, P,
    Format, True, Value.IsNegative);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddDateTime(const Value: TDateTime;
  const Format: String; var Result: RawByteString);
var Y, MO, D, H, M, S, MS: Word;
  L: LengthInt;
  P: PAnsiChar;
  Buffer: Array[0..cMaxTimeStampLenQuoted] of AnsiChar;
begin
  if (FPos + cMaxTimeStampLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  DecodeDate(Value, Y, MO, D);
  DecodeTime(Value, H, M, S, MS);
  L := DateTimeToRaw(Y, MO, D, H, M, S, MS*NanoSecsPerMSec, P, Format, True, False);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddDecimal(const Value: TBCD;
  var Result: RawByteString);
var L: LengthInt;
  P: PAnsiChar;
  Buffer: array[0..MaxFMTBcdFractionSize+2] of AnsiChar;
begin
  if (FPos+ MaxFMTBcdFractionSize+2 < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := ZSysUtils.BcdToRaw(Value, P, '.');
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddFloat(const Value: Double; var Result: RawByteString);
var L: LengthInt;
  P: PAnsiChar;
  Buffer: array[0..63] of AnsiChar;
begin
  if (FPos+64 < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := ZSysUtils.FloatToSqlRaw(Value, P);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZRawSQLStringWriter.AddGUID(const Value: TGUID;
  Options: TGUIDConvOptions; var Result: RawByteString);
var
  L: LengthInt;
  P: PAnsiChar;
begin
  L := 36;
  if guidWithBrackets in Options then
    Inc(L, 2);
  if guidQuoted in Options then
    Inc(L, 2);
  if (FPos + L < FEnd) then begin
    P := FPos;
    Inc(FPos, L);
  end else
    P := FlushBuff(Result, L);
  ZSysUtils.GUIDToBuffer(@Value.D1, P, Options);
end;

procedure TZRawSQLStringWriter.AddDecimal(const Value: Currency; var Result: RawByteString);
var L: LengthInt;
  P, P2: PAnsiChar;
  Buffer: array[0..23] of AnsiChar;
begin
  if (FPos + 24 < FEnd)
  then P := FPos
  else P := @Buffer[0];
  CurrToRaw(Value, P, @P2);
  L := P2-P;
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
procedure TZRawSQLStringWriter.AddText(const AsciiValue: UnicodeString;
  var Result: RawByteString);
begin
  AddAscii7UTF16Text(AsciiValue, Result);
end;
{$ENDIF}

procedure TZRawSQLStringWriter.AddOrd(Value: Pointer;
  var Result: RawByteString);
var X: {$IFDEF CPU64}UInt64{$ELSE}Cardinal{$ENDIF} absolute Value;
begin
  AddOrd(X, Result);
end;

{ TZUnicodeSQLStringWriter }

procedure TZUnicodeSQLStringWriter.AddChar(Value: WideChar;
  var Result: UnicodeString);
var P: PWideChar;
begin
  if (FPos < FEnd) then begin
    FPos^ := Value;
    Inc(FPos);
  end else begin
    P := FlushBuff(Result, 1);
    P^ := Value;
  end;
end;

procedure TZUnicodeSQLStringWriter.AddDate(const Value: TDateTime;
  const Format: String; var Result: UnicodeString);
var Y, M, D: Word;
  L: LengthInt;
  P: PWideChar;
  Buffer: Array[0..cMaxDateLenQuoted] of WideChar;
begin
  if (FPos+cMaxDateLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  DecodeDate(Value, Y, M, D);
  L := DateToUni(Y, M, D, P, Format, True, False);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddDate(const Value: TZDate;
  const Format: String; var Result: UnicodeString);
var
  L: LengthInt;
  P: PWideChar;
  Buffer: Array[0..cMaxDateLenQuoted] of WideChar;
begin
  if (FPos + cMaxDateLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := DateToUni(Value.Year, Value.Month, Value.Day, P, Format, True, Value.IsNegative);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddDateTime(const Value: TDateTime;
  const Format: String; var Result: UnicodeString);
var Y, MO, D, H, M, S, MS: Word;
  L: LengthInt;
  P: PWideChar;
  Buffer: Array[0..cMaxTimeStampLenQuoted] of WideChar;
begin
  if (FPos+cMaxTimeStampLenQuoted < FEnd) //'2019-09-11 00:00:00.999'
  then P := FPos
  else P := @Buffer[0];
  DecodeDate(Value, Y, MO, D);
  DecodeTime(Value, H, M, S, MS);
  L := ZSysUtils.DateTimeToUni(Y, MO, D, H, M, S, MS*NanoSecsPerMSec, P, Format, True, False);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddDecimal(const Value: TBCD;
  var Result: UnicodeString);
var L: LengthInt;
  P: PWideChar;
  Buffer: array[0..MaxFMTBcdFractionSize+2] of WideChar;
begin
  if (FPos+MaxFMTBcdFractionSize+2 < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := ZSysUtils.BcdToUni(Value, P, '.');
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddDecimal(const Value: Currency;
  var Result: UnicodeString);
var L: LengthInt;
  P, P2: PWideChar;
  Buffer: array[0..23] of WideChar;
begin
  if (FPos+24 < FEnd)
  then P := FPos
  else P := @Buffer[0];
  CurrToUnicode(Value, P, @P2);
  L := P2-P;
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddFloat(Value: Single;
  var Result: UnicodeString);
var D: Double;
begin
  D := Value;
  AddFloat(D, Result);
end;

procedure TZUnicodeSQLStringWriter.AddFloat(const Value: Double;
  var Result: UnicodeString);
var L: LengthInt;
  P: PWideChar;
  Buffer: array[0..63] of WideChar;
begin
  if (FPos+64 < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := ZSysUtils.FloatToSqlUnicode(Value, P);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddGUID(const Value: TGUID;
  Options: TGUIDConvOptions; var Result: UnicodeString);
var
  L: LengthInt;
  P: PWideChar;
begin
  L := 36;
  if guidWithBrackets in Options then
    Inc(L, 2);
  if guidQuoted in Options then
    Inc(L, 2);
  if (FPos + L < FEnd) then begin
    P := FPos;
    Inc(FPos, L);
  end else
    P := FlushBuff(Result, L);
  ZSysUtils.GUIDToBuffer(@Value.D1, P, Options);
end;

procedure TZUnicodeSQLStringWriter.AddHexBinary(Value: PByte; L: LengthInt;
  ODBC: Boolean; var Result: UnicodeString);
var P: PWideChar;
  LTotal: LengthInt;
begin
  LTotal := ((L+1) shl 1) + Ord(not ODBC);
  if (FPos+LTotal < FEnd) then begin
    P := FPos;
    Inc(FPos, LTotal);
  end else
    P := FlushBuff(Result, LTotal);
  If ODBC then begin
    PWord(P  )^ := Ord('0');
    PWord(P+1)^ := Ord('x');
  end else begin
    PWord(P  )^ := Ord('x');
    PWord(P+1)^ := Ord(#39);
  end;
  if Value <> nil then
    ZBinToHex(PAnsiChar(Value), P+2, L);
  if not ODBC then
    PWord(P+LTotal-1)^ := Ord(#39);
end;

procedure TZUnicodeSQLStringWriter.AddHexBinary(const Value: TBytes;
  ODBC: Boolean; var Result: UnicodeString);
begin
  AddHexBinary(Pointer(Value), Length(Value), ODBC, Result);
end;

procedure TZUnicodeSQLStringWriter.AddLineFeedIfNotEmpty(
  var Result: UnicodeString);
begin
  if (Pointer(Result) <> nil) or (FPos > FBuf) then
    AddText(LineEnding, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(Value: ShortInt;
  var Result: UnicodeString);
var B, Digits: Byte;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, B, Negative);
  AddOrd32(B, Digits, Negative, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(Value: Byte;
  var Result: UnicodeString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd32(Value, Digits, False, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(Value: Word;
  var Result: UnicodeString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd32(Value, Digits, False, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(const Value: Int64;
  var Result: UnicodeString);
var Digits: Byte;
  U: UInt64;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, U, Negative);
  AddOrd64(U, Digits, Negative, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(const Value: UInt64;
  var Result: UnicodeString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd64(Value, Digits, False, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(Value: Integer;
  var Result: UnicodeString);
var Digits: Byte;
  C: Cardinal;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, C, Negative);
  AddOrd32(C, Digits, Negative, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(Value: SmallInt;
  var Result: UnicodeString);
var Digits: Byte;
  W: Word;
  Negative: Boolean;
begin
  Digits := GetOrdinalDigits(Value, W, Negative);
  AddOrd32(W, Digits, Negative, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd(Value: Cardinal;
  var Result: UnicodeString);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  AddOrd32(Value, Digits, False, Result);
end;

procedure TZUnicodeSQLStringWriter.AddOrd32(Value: Cardinal; Digits: Byte;
  Negative: Boolean; var Result: UnicodeString);
var P: PWideChar;
  D2: Byte;
begin
  D2 := Digits+Ord(Negative);
  if (FPos+D2 < FEnd) then begin
    P := FPos;
    Inc(FPos, D2);
  end else
    P := FlushBuff(Result, D2);
  if Negative then begin
    PWord(P)^ := Ord('-');
    Inc(P);
  end;
  IntToUnicode(Value, P, Digits);
end;

procedure TZUnicodeSQLStringWriter.AddOrd64(const Value: UInt64; Digits: Byte;
  Negative: Boolean; var Result: UnicodeString);
var P: PWideChar;
  D2: Byte;
begin
  D2 := Digits+Ord(Negative);
  if (FPos+D2 < FEnd) then begin
    P := FPos;
    Inc(FPos, D2);
  end else
    P := FlushBuff(Result, D2);
  if Negative then begin
    PWord(P)^ := Ord('-');
    Inc(P);
  end;
  IntToUnicode(Value, P, Digits);
end;

procedure TZUnicodeSQLStringWriter.AddAscii7Text(Value: PAnsiChar; L: LengthInt;
  var Result: UnicodeString);
var P: PWideChar;
    I: LengthInt;
begin
  if (Value = nil) or (L = 0) then Exit;
  if (FPos + L < FEnd) then begin
    for I := 1 to L do begin
      PWord(FPos)^ := PByte(Value)^;
      Inc(FPos);
      Inc(Value);
    end;
  end else begin
    P := FlushBuff(Result, L);
    for I := 1 to L do begin
      PWord(P)^ := PByte(Value)^;
      Inc(P);
      Inc(Value);
    end;
  end;
end;

procedure TZUnicodeSQLStringWriter.AddText(Value: PWideChar; L: LengthInt;
  var Result: UnicodeString);
var P: PWideChar;
begin
  if (Value = nil) or (L = 0) then Exit;
  if (FPos + L < FEnd) then begin
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, FPos^, L shl 1);
    Inc(FPos, L);
  end else begin
    P := FlushBuff(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, P^, L shl 1);
  end;
end;

procedure TZUnicodeSQLStringWriter.AddText(const Value: UnicodeString;
  var Result: UnicodeString);
begin
  AddText(Pointer(Value), Length(Value), Result);
end;

procedure TZUnicodeSQLStringWriter.AddTime(const Value: TZTime;
  const Format: String; var Result: UnicodeString);
var L: LengthInt;
  P: PWideChar;
  Buffer: Array[0..cMaxTimeLenQuoted] of WideChar;
begin
  if (FPos + cMaxTimeLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := TimeToUni(Value.Hour, Value.Minute, Value.Second, Value.Fractions, P,
    Format, True, Value.IsNegative);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddTimeStamp(const Value: TZTimeStamp;
  const Format: String; var Result: UnicodeString);
var L: LengthInt;
  P: PWideChar;
  Buffer: Array[0..cMaxTimeStampLenQuoted] of WideChar;
begin
  if (FPos + cMaxTimeStampLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  L := DateTimeToUni(Value.Year, Value.Month, Value.Day, Value.Hour,
    Value.Minute, Value.Second, Value.Fractions, P, Format, True, Value.IsNegative);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.AddTime(const Value: TDateTime;
  const Format: String; var Result: UnicodeString);
var H, M, S, MS: Word;
  L: LengthInt;
  P: PWideChar;
  Buffer: Array[0..cMaxTimeLenQuoted] of WideChar;
begin
  if (FPos+cMaxTimeLenQuoted < FEnd)
  then P := FPos
  else P := @Buffer[0];
  DecodeTime(Value, H, M, S, MS);
  L := TimeToUni(H, M, S, MS*NanoSecsPerMSec, P, Format, True, False);
  if P = FPos
  then Inc(FPos, L)
  else AddText(P, L, Result);
end;

procedure TZUnicodeSQLStringWriter.CancelLastCharIfExists(Value: WideChar;
  var Result: UnicodeString);
var P: PWideChar;
  L: LengthInt;
begin
  if (FPos > FBuf) then begin
    if PWord(FPos-1)^ = Word(Value) then
      Dec(FPos);
  end else begin
    P := Pointer(Result);
    L := Length(Result)-1;
    Inc(P, L);
    if (L >= 1) and (PWord(P)^ = Word(',')) then
      SetLength(Result, L);
  end;
end;

procedure TZUnicodeSQLStringWriter.CancelLastComma(var Result: UnicodeString);
var P: PWideChar;
  L: LengthInt;
begin
  if (FPos > FBuf) then begin
    if PWord(FPos-1)^ = Ord(',') then
      Dec(FPos);
  end else begin
    P := Pointer(Result);
    L := Length(Result)-1;
    Inc(P, L);
    if (L >= 1) and (PWord(P)^ = Ord(',')) then
      SetLength(Result, L);
  end;
end;

constructor TZUnicodeSQLStringWriter.Create(WideCharCapacity: Integer);
begin
  if WideCharCapacity < High(Byte) then
    WideCharCapacity := High(Byte);
  GetMem(FBuf, WideCharCapacity shl 1);
  FPos := FBuf;
  FEnd := FBuf+WideCharCapacity;
end;

destructor TZUnicodeSQLStringWriter.Destroy;
begin
  FreeMem(FBuf);
  inherited;
end;

procedure TZUnicodeSQLStringWriter.Finalize(var Result: UnicodeString);
begin
  if FPos > FBuf then
    FlushBuff(Result, 0);
end;

function TZUnicodeSQLStringWriter.FlushBuff(var Dest: UnicodeString;
  ReservedLen: LengthInt): PWideChar;
var LRes: LengthInt;
begin
  LRes := Length(Dest);
  SetLength(Dest, LRes+(FPos-FBuf)+ReservedLen);
  Result := Pointer(Dest);
  Inc(Result, LRes);
  if FPos > FBuf then begin
    LRes := (PAnsiChar(FPos)-PAnsiChar(FBuf)); //get size in bytes
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FBuf^, Result^, LRes);
    Inc(PAnsiChar(Result), LRes);
    FPos := FBuf;
  end;
end;

procedure TZUnicodeSQLStringWriter.IncreaseCapacityTo(WideCharCapacity: Integer;
  var Result: UnicodeString);
begin
  if WideCharCapacity < FEnd-FBuf then
    Exit;
  if FPos > FBuf then
    FlushBuff(Result,0);
  FreeMem(FBuf);
  GetMem(FBuf, WideCharCapacity shl 1);
  FPos := FBuf;
  FEnd := FBuf+WideCharCapacity;
end;

procedure TZUnicodeSQLStringWriter.ReplaceOrAddLastChar(OldChar,
  NewChar: WideChar; var Result: UnicodeString);
var P: PWidechar;
  L: LengthInt;
  label setp;
begin
  if (FPos > FBuf) then begin
    P := FPos-1;
    if PWord(P)^ = Ord(OldChar)
    then PWord(P)^ := Ord(NewChar)
    else goto setp
  end else begin
    P := Pointer(Result);
    L := Length(Result)-{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}2{$ELSE}1{$ENDIF};
    Inc(P, L);
    if (L >= 1) and (PWord(P)^ = Ord(OldChar)) then
      PWord(P)^ := Ord(NewChar)
    else begin
setp: PWord(FPos)^ := Ord(NewChar);
      Inc(FPos);
    end;
  end;
end;

{ TZSortedList }

{**
  Origial Autor: Aleksandr Sharahov
  see http://guildalfa.ru/alsha/
  Performs hybrid sort algorithm for the list.
  changes by EgonHugeist:
  Replace cardinal casts by using our NativeUInt to make it 64Bit compatible too
  Note Alexandr wrote: For max of speed it is very impotant to use procedures
    QuickSort_0AA and HybridSort_0AA as is (not in class, not included
    in other procedure, and not changed parameters and code).
}
//~1.57 times faster than Delphi QuickSort on E6850
{$Q-}
{$R-}
const
  InsCount = 35; //33..49;
  InsLast = InsCount-1;
  SOP = SizeOf(pointer);
  MSOP = NativeUInt(-SOP);

{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF} // uses pointer maths
procedure QuickSortSha_0AA(L, R: NativeUInt; Compare: TZListSortCompare);
var
  I, J, P, T: NativeUInt;
begin;
  while true do begin
    I := L;
    J := R;
    if J-I <= InsLast * SOP then break;
    T := (J-I) shr 1 and MSOP + I;

    if Compare(PPointer(J)^, PPointer(I)^)<0 then begin
      P := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(J)^;
      PNativeUInt(J)^ := P;
    end;
    P := PNativeUInt(T)^;
    if Compare(Pointer(P), PPointer(I)^)<0 then
    begin
      P := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(T)^;
      PNativeUInt(T)^ := P;
    end
    else
      if Compare(PPointer(J)^, Pointer(P)) < 0 then
      begin
        P := PNativeUInt(J)^;
        PNativeUInt(J)^ := PNativeUInt(T)^;
        PNativeUInt(T)^ := P;
      end;

    repeat
      Inc(I,SOP);
    until not (Compare(PPointer(I)^, Pointer(P)) < 0);
    repeat
      Dec(J,SOP)
    until not (Compare(pointer(P), PPointer(J)^) < 0);
    if I < J then
      repeat
        T := PNativeUInt(I)^;
        PNativeUInt(I)^ := PNativeUInt(J)^;
        PNativeUInt(J)^ := T;
        repeat
          Inc(I,SOP);
        until not (Compare(PPointer(I)^, pointer(P)) < 0 );
        repeat
          Dec(J,SOP);
        until not (Compare(pointer(P), PPointer(J)^) < 0);
      until I >= J;
    Dec(I,SOP); Inc(J,SOP);

    if I-L <= R-J then
    begin
      if L + InsLast * SOP < I then
        QuickSortSha_0AA(L, I, Compare);
      L := J;
    end
    else
    begin
      if J + InsLast * SOP < R
        then QuickSortSha_0AA(J, R, Compare);
      R := I;
    end;
  end;
end;

procedure HybridSortSha_0AA(List: PPointerList; Count: integer; Compare: TZListSortCompare);
var
  I, J, {$IFDEF WITH_IE200706094}J2,{$ENDIF} L, R: NativeUInt;
begin;
  if (List<>nil) and (Count>1) then
  begin
    L := NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[0]);
    R := NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[Count-1]);
    J := R;
    if Count-1 > InsLast then
    begin
      J:=NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[InsLast]);
      QuickSortSha_0AA(L, R, Compare);
    end;

    I := L;
    repeat;
      if Compare(PPointer(J)^, PPointer(I)^) < 0 then I:=J;
      dec(J,SOP);
    until J <= L;

    if I > L then
    begin
      J := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(L)^;
      PNativeUInt(L)^ := J;
    end;

    J := L + SOP;
    while true do
    begin
      repeat;
        if J >= R then exit;
        inc(J,SOP);
      {$IFDEF WITH_IE200706094} //FPC 64Bit raises an internal Error 200706094!
        J2 := J+MSOP;
      until Compare(PPointer(J)^,PPointer(J2)^) < 0;
      {$ELSE}
      until Compare(PPointer(J)^,PPointer(J+MSOP)^) < 0;
      {$ENDIF}
      I := J - SOP;
      L := PNativeUInt(J)^;
      repeat;
        PNativeUInt(I+SOP)^ := PNativeUInt(I)^;
        dec(I,SOP);
      until not (Compare(Pointer(L),PPointer(I)^) < 0);
      PNativeUInt(I + SOP)^ := L;
    end;
  end;
end;

{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{$IFDEF TLIST_IS_DEPRECATED}
function TZSortedList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TZSortedList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TZSortedList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(Pointer));
end;

destructor TZSortedList.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TZSortedList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;

class procedure TZSortedList.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
end;

procedure TZSortedList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

function TZSortedList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    FList[I] := nil;
    Delete(I);
  end;
end;

function TZSortedList.First: Pointer;
begin
  Result := Get(0);
end;

function TZSortedList.Get(Index: Integer): Pointer;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index];
end;

procedure TZSortedList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TZSortedList.IndexOf(Item: Pointer): Integer;
var
  P: PAnsiChar;
begin
  P := Pointer(FList);
  for Result := 0 to FCount - 1 do begin
    if PPointer(P)^ = Item then
      Exit;
    Inc(P, SizeOf(Pointer));
  end;
  Result := -1;
end;

procedure TZSortedList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  Inc(FCount);
end;

function TZSortedList.Last: Pointer;
begin
  if FCount > 0 then
    Result := FList[Count - 1]
  else
  begin
    Error(@SListIndexError, 0);
    Result := nil;
  end;
end;

procedure TZSortedList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  if Item <> FList[Index] then begin
    FList[Index] := Item;
  end;
end;

function TZSortedList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TZSortedList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TZSortedList.SetCount(NewCount: Integer);
begin
  if NewCount < 0 then
    Error(@SListCountError, NewCount);
  if NewCount <> FCount then begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0);
    FCount := NewCount;
  end;
end;
{$ENDIF TLIST_IS_DEPRECATED}
{**
  Performs sorting for this list.
  @param Compare a comparison function.
}
procedure TZSortedList.Sort(Compare: TZListSortCompare);
begin
  {$IFDEF TLIST_ISNOT_PPOINTERLIST}
  HybridSortSha_0AA(@List, Count, Compare);
  {$ELSE}
  HybridSortSha_0AA(List, Count, Compare);
  {$ENDIF}
end;


end.


