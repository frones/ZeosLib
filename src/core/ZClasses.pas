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
  SysUtils, Classes;

const
  ZEOS_MAJOR_VERSION = 7;
  ZEOS_MINOR_VERSION = 2;
  ZEOS_SUB_VERSION = 4;
  ZEOS_STATUS = 'stable';
  ZEOS_VERSION = Char(48+ZEOS_MAJOR_VERSION)+'.'+
                 Char(48+ZEOS_MINOR_VERSION)+'.'+
                 Char(48+ZEOS_SUB_VERSION)+'-'+ZEOS_STATUS;
{$IFDEF ENABLE_POOLED}
  {Pooled Protocol Prefix, including final dot}
  PooledPrefix = 'pooled.';
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

{$IFDEF WITH_NEWTOBJECT} // to suppress the overload warning of the Equals overload, Marco. (overload a non overload-declared funtion)
  {$WARNINGS OFF}
{$ENDIF}
  {** Implements an abstract interfaced object. }
  TZAbstractObject = class(TInterfacedObject, IZObject)
  public
    function Equals(const Value: IZInterface): Boolean; {$IFDEF WITH_NEWTOBJECT}overload;{$ENDIF} virtual;
    function GetHashCode: LongInt;
    function Clone: IZInterface; virtual;
    function ToString: string;{$IFDEF WITH_NEWTOBJECT}override{$ELSE} virtual{$ENDIF} ;
    function InstanceOf(const IId: TGUID): Boolean;
  end;
{$IFDEF WITH_NEWTOBJECT}
  {$WARNINGS ON}
{$ENDIF}

  TZCharReaderStream = Class(TStream)
  private
    fEnd, fStart, fCurrent: PChar;
  protected
    function GetSize: Int64; override;
  public
    procedure SetBuffer(const Buffer: String);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  End;

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

{ TZCharReaderStream }

function TZCharReaderStream.GetSize: Int64;
begin
  Result := Int64(fEnd-fStart){%H-}-1
end;

function TZCharReaderStream.Read(var Buffer; Count: Integer): Longint;
begin
  if (Count = SizeOf(Char)) and (fCurrent < fEnd) then begin
    //just a little byte/dword inline move instead of Move()
    //skip all possible compiler magic
    {$IFDEF UNICODE}
    Word(Buffer) := PWord(fCurrent)^;
    {$ELSE}
    Byte(Buffer) := PByte(fCurrent)^;
    {$ENDIF}
    Inc(fCurrent);
    Result := SizeOf(Char);
  end else
    Result := 0
end;

function TZCharReaderStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: fCurrent := {%H-}Pointer({%H-}NativeInt(fStart)+Offset);
    soFromCurrent:   fCurrent := {%H-}Pointer({%H-}NativeInt(fCurrent)+Offset);
    soFromEnd:       fCurrent := {%H-}Pointer({%H-}NativeInt(fEnd-1)+Offset);
  end;
  Result := origin; //make compiler happy: a true positoned processing is nowhere used in our code
  //Result := LongInt(fCurrent-fStart);
end;

function TZCharReaderStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Ord(Origin) of
    soFromBeginning: fCurrent := {%H-}Pointer({%H-}NativeInt(fStart)+Offset);
    soFromCurrent:   fCurrent := {%H-}Pointer({%H-}NativeInt(fCurrent)+Offset);
    soFromEnd:       fCurrent := {%H-}Pointer({%H-}NativeInt(fEnd-1)+Offset);
  end;
  Result := Ord(origin); //make compiler happy: a true positoned processing is nowhere used in our code
  //Result := Int64(fCurrent-fStart);
end;

procedure TZCharReaderStream.SetBuffer(const Buffer: String);
begin
  fStart := Pointer(Buffer);
  fCurrent := fStart;
  fEnd := fStart+Length(Buffer);
end;

function TZCharReaderStream.Write(const Buffer; Count: Integer): Longint;
begin
  //satisfy FPC:
  {$IFDEF FPC}
  Result := 0;
  {$ENDIF}
  raise Exception.Create(SUnsupportedOperation);
end;

end.

