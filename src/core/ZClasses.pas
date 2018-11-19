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
  SysUtils, Classes
  {$IFDEF NO_UNIT_CONTNRS},System.Generics.Collections{$ENDIF};

const
  ZEOS_MAJOR_VERSION = 7;
  ZEOS_MINOR_VERSION = 3;
  ZEOS_SUB_VERSION = 0;
  ZEOS_STATUS = 'alpha';
  ZEOS_VERSION = Char(48+ZEOS_MAJOR_VERSION)+'.'+
                 Char(48+ZEOS_MINOR_VERSION)+'.'+
                 Char(48+ZEOS_SUB_VERSION)+'-'+ZEOS_STATUS;
{$IFDEF ENABLE_POOLED}
  {Pooled Protocol Prefix, including final dot}
  PooledPrefix = 'pooled.'+ZEOS_VERSION;
{$ENDIF}


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

// Exceptions
type
  TZExceptionSpecificData = class
  public
    function Clone: TZExceptionSpecificData; virtual; abstract;
  end;

  {** Abstract SQL exception. }
  EZSQLThrowable = class(Exception)
  private
    FErrorCode: Integer;
    FStatusCode: String;
  protected
    FSpecificData: TZExceptionSpecificData;
  public
    constructor Create(const Msg: string);
    constructor CreateWithCode(const ErrorCode: Integer; const Msg: string);
    constructor CreateWithStatus(const StatusCode: String; const Msg: string);
    constructor CreateWithCodeAndStatus(ErrorCode: Integer; const StatusCode: String; const Msg: string);
    constructor CreateClone(const E:EZSQLThrowable);
    destructor Destroy; override;

    property ErrorCode: Integer read FErrorCode;
    property StatusCode: string read FStatuscode; // The "String" Errocode // FirmOS
    property SpecificData: TZExceptionSpecificData read FSpecificData; // Engine-specific data
  end;

  {** Generic SQL exception. }
  EZSQLException = class(EZSQLThrowable);

  {** Generic connection lost exception. }
  EZSQLConnectionLost = class(EZSQLException);

  {** Generic SQL warning. }
  EZSQLWarning = class(EZSQLThrowable);

  {$IFDEF NO_UNIT_CONTNRS}
  TObjectList = class(TObjectList<TObject>);
  {$ENDIF}

implementation

uses ZMessages, ZCompatibility;

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

{ EZSQLThrowable }

constructor EZSQLThrowable.CreateClone(const E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  FErrorCode:=E.ErrorCode;
  FStatusCode:=E.Statuscode;
  if E.SpecificData <> nil then
    FSpecificData := E.SpecificData.Clone;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
}
constructor EZSQLThrowable.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := -1;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
  @param ErrorCode a native server error code.
}
constructor EZSQLThrowable.CreateWithCode(const ErrorCode: Integer;
  const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

{**
  Creates an exception with message string.
  @param ErrorCode a native server error code.
  @param StatusCode a server status code.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateWithCodeAndStatus(ErrorCode: Integer;
  const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
  FStatusCode := StatusCode;
end;

{**
  Creates an exception with message string.
  @param StatusCode a server status code.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateWithStatus(const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FStatusCode := StatusCode;
end;

destructor EZSQLThrowable.Destroy;
begin
  FreeAndNil(FSpecificData);
  inherited;
end;

end.

