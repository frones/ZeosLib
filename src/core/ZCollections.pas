{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Core collection and map classes               }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZCollections;

interface

{$I ZCore.inc}

uses Classes, ZClasses, ZCompatibility;

type

  /// <summary>Implements an iterator for regular TZCollection collection.</summary>
  TZIterator = class (TZAbstractObject, IZIterator)
  private
    FCollection: IZCollection;
    FCurrentIndex: Integer;
  public
    /// <summary>Creates this iterator for the specified interface list.</summary>
    /// <param>"List" a list of interfaces.</param>
    constructor Create(const Col: IZCollection);
    /// <summary>Checks has the iterated collection more elements.</summary>
    /// <returns>a next iterated element from the collection or <c>nil</c>
    ///  if no more elements.</returns>
    function HasNext: Boolean;
    /// <summary>Gets a next iterated element from the collection.</summary>
    /// <returns><c>True</c> if iterated collection has more elements.</returns>
    function Next: IZInterface;
  end;

  /// <summary>Interface list types.</summary>
  TZInterfaceList = array[0..{$IFDEF WITH_MAXLISTSIZE_DEPRECATED}Maxint div 16{$ELSE}MaxListSize{$ENDIF} - 1] of IZInterface;
  /// <summary>a reference to Interface list types.</summary>
  PZInterfaceList = ^TZInterfaceList;

  /// <summary>Implements a collection of interfaces.</summary>
  TZCollection = class(TZAbstractObject, IZCollection, IZClonnable)
  private
    FList: PZInterfaceList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    /// <summary>Raises a collection error.</summary>
    /// <param>"Msg" an error message.</param>
    /// <param>"Data" a integer value to describe an error.</param>
    class procedure Error(const Msg: string; Data: Integer);
    /// <summary>Increases an element capacity.</summary>
    procedure Grow;
    /// <summary>Sets a new list capacity.</summary>
    /// <param>"NewCapacity" a new list capacity.</param>
    procedure SetCapacity(NewCapacity: Integer);
    /// <summary>Sets a new element count.</summary>
    /// <param>"NewCount" a new element count.</param>
    procedure SetCount(NewCount: Integer);
  public
    /// <summary>Destroys this object.</summary>
    destructor Destroy; override;
    /// <summary>Clones an object instance.</summary>
    /// <returns> a cloned object interface.</returns>
    function Clone: IZInterface; override;
    /// <summary>Converts this object into the string representation.</summary>
    /// <returns>a string representation for this object.</returns>
    function ToString: string; override;
    /// <summary>Gets a collection element from the specified position.</summary>
    /// <param>"Index" a position index of the element.</param>
    /// <returns>a requested element.</returns>
    function Get(Index: Integer): IZInterface;
    /// <summary>Puts a specified object into defined position.</summary>
    /// <param>"Index" a position index.</param>
    /// <param>"Item" an object to be put.</param>
    procedure Put(Index: Integer; const Item: IZInterface);
    /// <summary>Defines an index of the specified object inside this colleciton.</summary>
    /// <param>"Item" an object to be found.</param>
    /// <returns>an object position index or -1 if it was not found.</returns>
    function IndexOf(const Item: IZInterface): Integer;
    /// <summary>Gets a number of the stored element in this collection.</summary>
    /// <returns>a number of stored elements.</returns>
    function GetCount: Integer;
    /// <summary>Gets a created iterator for this collection.</summary>
    /// <returns>a created iterator for this collection.</returns>
    function GetIterator: IZIterator;
    /// <summary>Gets the first element from this collection.</summary>
    /// <returns>the first element.</returns>
    function First: IZInterface;
    /// <summary>Gets the last object from this collection.</summary>
    /// <returns>return the last object.</returns>
    function Last: IZInterface;
    /// <summary>Adds a new object at the and of this collection.</summary>
    /// <param>"Item" an object to be added.</param>
    /// <returns>a position of the added object.</returns>
    function Add(const Item: IZInterface): Integer;
    /// <summary>Inserts an object into specified position.</summary>
    /// <param>"Index" a position index.</param>
    /// <param>"Item" an object to be inserted.</param>
    procedure Insert(Index: Integer; const Item: IZInterface);
    /// <summary>Removes an existed object which equals to the specified one.</summary>
    /// <param>"Item" an object to be removed.</param>
    /// <returns>an index of the removed object.</returns>
    function Remove(const Item: IZInterface): Integer;
    /// <summary>Exchanges two element in the collection.</summary>
    /// <param>"Index1" an index of the first element.</param>
    /// <param>"Index2" an index of the second element.</param>
    procedure Exchange(Index1, Index2: Integer);
    /// <summary>Deletes an object from the specified position.</summary>
    /// <param>"Index" the index of the object to be deleted.</param>
    procedure Delete(Index: Integer);
    /// <summary>Clears the content of this collection.</summary>
    procedure Clear;
    /// <summary>Checks is the specified object is stored in this collection.</summary>
    /// <param>"Item" an object to be searched.</param>
    /// <returns><c>True</c> if the object was found in the collection.</returns>
    function Contains(const Item: IZInterface): Boolean;
    /// <summary>Checks are all the object in this collection.</summary>
    /// <param>"Col" a collection of objects to be checked.</param>
    /// <returns><c>True</c> if all objects are in this collection.</returns>
    function ContainsAll(const Col: IZCollection): Boolean;
    /// <summary>Adds all elements from the specified collection into this collection.</summary>
    /// <param>"Col" a collection of objects to be added.</param>
    /// <returns><c>True</c> if this collection was changed</returns>
    function AddAll(const Col: IZCollection): Boolean;
    /// <summary>Removes all the elements from the specified collection.</summary>
    /// <param>"Col" a collection of objects to be removed.</param>
    /// <returns><c>True</c> if this collection was changed</returns>
    function RemoveAll(const Col: IZCollection): Boolean;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IZInterface read Get write Put; default;
  end;

  {** Implements an unmodifiable collection of interfaces. }
  TZUnmodifiableCollection = class(TZAbstractObject, IZCollection, IZClonnable)
  private
    FCollection: IZCollection;
  private
    /// <summary>Raises invalid operation exception.</summary>
    procedure RaiseException;
  public
    /// <summary>Constructs this object and assignes main properties.</summary>
    /// <param>"Collection" an initial modifiable list of interfaces.</param>
    constructor Create(const Collection: IZCollection);
    /// <summary>Destroys this object and frees the memory.</summary>
    destructor Destroy; override;
    /// <summary>Clones an object instance.</summary>
    /// <returns> a cloned object interface.</returns>
    function Clone: IZInterface; override;
    /// <summary>Converts this object into the string representation.</summary>
    /// <returns>a string representation for this object.</returns>
    function ToString: string; override;
    /// <summary>Gets a collection element from the specified position.</summary>
    /// <param>"Index" a position index of the element.</param>
    /// <returns>a requested element.</returns>
    function Get(Index: Integer): IZInterface;
    /// <summary>Puts a specified object into defined position.</summary>
    /// <param>"Index" a position index.</param>
    /// <param>"Item" an object to be put.</param>
    procedure Put(Index: Integer; const Item: IZInterface);
    /// <summary>Defines an index of the specified object inside this colleciton.</summary>
    /// <param>"Item" an object to be found.</param>
    /// <returns>an object position index or -1 if it was not found.</returns>
    function IndexOf(const Item: IZInterface): Integer;
    /// <summary>Gets a number of the stored element in this collection.</summary>
    /// <returns>a number of stored elements.</returns>
    function GetCount: Integer;
    /// <summary>Gets a created iterator for this collection.</summary>
    /// <returns>a created iterator for this collection.</returns>
    function GetIterator: IZIterator;
    /// <summary>Gets the first element from this collection.</summary>
    /// <returns>the first element.</returns>
    function First: IZInterface;
    /// <summary>Gets the last object from this collection.</summary>
    /// <returns>return the last object.</returns>
    function Last: IZInterface;
    /// <summary>Adds a new object at the and of this collection.</summary>
    /// <param>"Item" an object to be added.</param>
    /// <returns>a position of the added object.</returns>
    function Add(const Item: IZInterface): Integer;
    /// <summary>Inserts an object into specified position.</summary>
    /// <param>"Index" a position index.</param>
    /// <param>"Item" an object to be inserted.</param>
    procedure Insert(Index: Integer; const Item: IZInterface);
    /// <summary>Removes an existed object which equals to the specified one.</summary>
    /// <param>"Item" an object to be removed.</param>
    /// <returns>an index of the removed object.</returns>
    function Remove(const Item: IZInterface): Integer;
    /// <summary>Exchanges two element in the collection.</summary>
    /// <param>"Index1" an index of the first element.</param>
    /// <param>"Index2" an index of the second element.</param>
    procedure Exchange(Index1, Index2: Integer);
    /// <summary>Deletes an object from the specified position.</summary>
    /// <param>"Index" the index of the object to be deleted.</param>
    procedure Delete(Index: Integer);
    /// <summary>Clears the content of this collection.</summary>
    procedure Clear;
    /// <summary>Checks is the specified object is stored in this collection.</summary>
    /// <param>"Item" an object to be searched.</param>
    /// <returns><c>True</c> if the object was found in the collection.</returns>
    function Contains(const Item: IZInterface): Boolean;
    /// <summary>Checks are all the object in this collection.</summary>
    /// <param>"Col" a collection of objects to be checked.</param>
    /// <returns><c>True</c> if all objects are in this collection.</returns>
    function ContainsAll(const Col: IZCollection): Boolean;
    /// <summary>Adds all elements from the specified collection into this collection.</summary>
    /// <param>"Col" a collection of objects to be added.</param>
    /// <returns><c>True</c> if this collection was changed</returns>
    function AddAll(const Col: IZCollection): Boolean;
    /// <summary>Removes all the elements from the specified collection.</summary>
    /// <param>"Col" a collection of objects to be removed.</param>
    /// <returns><c>True</c> if this collection was changed</returns>
    function RemoveAll(const Col: IZCollection): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IZInterface read Get write Put; default;
  end;

  /// <summary>Implements a hash map of interfaces.</summary>
  TZHashMap = class(TZAbstractObject, IZHashMap, IZClonnable)
  private
    FKeys: IZCollection;
    FReadOnlyKeys: IZCollection;
    FValues: IZCollection;
    FReadOnlyValues: IZCollection;
  public
    /// <summary>Creates this hash map and assignes main properties.</summary>
    constructor Create;
    /// <summary>Destroys this object and frees the memory.</summary>
    destructor Destroy; override;
    /// <summary>Clones an object instance.</summary>
    /// <returns> a cloned object interface.</returns>
    function Clone: IZInterface; override;
    /// <summary>Gets a interface by it's key.</summary>
    /// <param>"Key" a key interface.</param>
    /// <returns> found value interface or <c>nil</c> otherwise.</returns>
    function Get(const Key: IZInterface): IZInterface;
    /// <summary>Put a new key/value pair interfaces.</summary>
    /// <param>"Key" a key interface.</param>
    /// <param>"Value" a value interface.</param>
    procedure Put(const Key: IZInterface; const Value: IZInterface);
    /// <summary>Gets a readonly collection of keys.</summary>
    /// <returns>a readonly collection of keys.</returns>
    function GetKeys: IZCollection;
    /// <summary>Gets a readonly collection of values.</summary>
    /// <returns>a readonly collection of values.</returns>
    function GetValues: IZCollection;
    /// <summary>Gets a number of elements in this hash map.</summary>
    /// <returns>a number of elements in this hash map.</returns>
    function GetCount: Integer;
    /// <summary>Removes the element from the map by it's key.</summary>
    /// <param>"Key" a key interface of the element.</param>
    /// <returns><c>true</c> if the hash map was changed.</returns>
    function Remove(const Key: IZInterface): Boolean;
    /// <summary>Clears this hash map and removes all elements.</summary>
    procedure Clear;

    property Count: Integer read GetCount;
    property Keys: IZCollection read GetKeys;
    property Values: IZCollection read GetValues;
  end;

  /// <summary>Implements a stack of interfaces.</summary>
  TZStack = class(TZAbstractObject, IZStack, IZClonnable)
  private
    FValues: IZCollection;
  public
    /// <summary>Constructs this object and assignes the main properties.</summary>
    constructor Create;
    /// <summary>Destroys this object and cleanups the memory.</summary>
    destructor Destroy; override;
    /// <summary>Clones an object instance.</summary>
    /// <returns>a cloned object interface.</returns>
    function Clone: IZInterface; override;
    /// <summary>Converts this object into the string representation.</summary>
    /// <returns>a string representation for this object.</returns>
    function ToString: string; override;
    /// <summary>Gets an element from the top this stack without removing it.</summary>
    /// <returns>an element from the top of the stack.</returns>
    function Peek: IZInterface;
    /// <summary>Gets an element from the top this stack and remove it.</summary>
    /// <returns>an element from the top of the stack.</returns>
    function Pop: IZInterface;
    /// <summary>Puts a new element to the top of this stack.</summary>
    /// <param>"Value" a new element to be put.</param>
    procedure Push(const Value: IZInterface);
    /// <summary>Gets a count of the stored elements.</summary>
    /// <returns>an elements count.</returns>
    function GetCount: Integer;

    property Count: Integer read GetCount;
  end;

implementation

uses SysUtils, ZMessages {$IFDEF FAST_MOVE}, ZFastCode{$ENDIF};

{ TZIterator }

constructor TZIterator.Create(const Col: IZCollection);
begin
  FCollection := Col;
  FCurrentIndex := 0;
end;

function TZIterator.HasNext: Boolean;
begin
  Result := FCurrentIndex < FCollection.Count;
end;

function TZIterator.Next: IZInterface;
begin
  if FCurrentIndex < FCollection.Count then
  begin
    Result := FCollection[FCurrentIndex];
    Inc(FCurrentIndex);
  end else
    Result := nil;
end;

{ TZCollection }

destructor TZCollection.Destroy;
begin
  Clear;
end;

class procedure TZCollection.Error(const Msg: string; Data: Integer);
begin
  {$IFDEF FPC}
  raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame);
  {$ELSE}
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
  {$ENDIF}
end;

procedure TZCollection.Grow;
var Delta: Integer;
begin
  if FCapacity > 64
  then Delta := FCapacity div 4
  else if FCapacity > 8
    then Delta := 16
    else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TZCollection.SetCapacity(NewCapacity: Integer);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (NewCapacity < FCount) or (NewCapacity > {$IFDEF WITH_MAXLISTSIZE_DEPRECATED}Maxint div 16{$ELSE}MaxListSize{$ENDIF}) then
    Error(SListCapacityError, NewCapacity);
  {$ENDIF}
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(IZInterface));
    if NewCapacity > FCapacity then
      System.FillChar(FList^[FCount], (NewCapacity - FCapacity) *
            SizeOf(IZInterface), 0);
    FCapacity := NewCapacity;
  end;
end;

procedure TZCollection.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (NewCount < 0) or (NewCount > {$IFDEF WITH_MAXLISTSIZE_DEPRECATED}Maxint div 16{$ELSE}MaxListSize{$ENDIF}) then
    Error(SListCountError, NewCount);
  {$ENDIF}
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount < FCount then
    for I := FCount - 1 downto NewCount do
      FList^[I] := nil;
  FCount := NewCount;
end;

function TZCollection.Clone: IZInterface;
var
  I: Integer;
  Collection: IZCollection;
  Clonnable: IZClonnable;
begin
  Collection := TZCollection.Create;
  for I := 0 to FCount - 1 do
    if FList^[I].QueryInterface(IZClonnable, Clonnable) = 0
    then Collection.Add(Clonnable.Clone)
    else Collection.Add(FList^[I]);
  Result := Collection;
end;

function TZCollection.Add(const Item: IZInterface): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

function TZCollection.AddAll(const Col: IZCollection): Boolean;
var
  I: Integer;
begin
  Result := Col.Count > 0;
  for I := 0 to Col.Count - 1 do
    Add(Col[I]);
end;

procedure TZCollection.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TZCollection.Contains(const Item: IZInterface): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TZCollection.ContainsAll(const Col: IZCollection): Boolean;
var
  I: Integer;
begin
  Result := Col.Count > 0;
  for I := 0 to Col.Count - 1 do
    if IndexOf(Col[I]) < 0 then begin
      Result := False;
      Break;
    end;
end;

procedure TZCollection.Delete(Index: Integer);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  FList^[Index] := nil;
  Dec(FCount);
  if Index < FCount then
  begin
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(IZInterface));
    {now nil pointer or on replacing the entry we'll get a bad interlockdecrement}
    Pointer(FList^[FCount]) := nil; //see http://sourceforge.net/p/zeoslib/tickets/100/
  end;
end;

procedure TZCollection.Exchange(Index1, Index2: Integer);
var
  Item: IZInterface;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SListIndexError, Index2);
  {$ENDIF}
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TZCollection.First: IZInterface;
begin
  Result := Get(0);
end;

function TZCollection.Get(Index: Integer): IZInterface;
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  Result := FList^[Index];
end;

function TZCollection.GetCount: Integer;
begin
  Result := FCount;
end;

function TZCollection.GetIterator: IZIterator;
begin
  Result := TZIterator.Create(Self);
end;

function TZCollection.IndexOf(const Item: IZInterface): Integer;
var
  I: Integer;
  Comparable: IZComparable;
  Unknown: IZInterface;
begin
  Result := -1;
  if (FCount = 0) or (Item = nil) then
    Exit;

  { Find IComparable objects }
  if Item.QueryInterface(IZComparable, Comparable) = 0 then begin
    for I := 0 to FCount - 1 do begin
      if Comparable.Equals(FList^[I]) then begin
        Result := I;
        Break;
      end;
    end;
    Comparable := nil;
  end else begin { Find ordinary objects }
    Unknown := Item;
    for I := 0 to FCount - 1 do begin
      if Unknown = FList^[I] then begin
        Result := I;
        Break;
      end;
    end;
    Unknown := nil;
  end;
end;

procedure TZCollection.Insert(Index: Integer; const Item: IZInterface);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  if FCount = FCapacity then
    Grow;
  if Index < FCount then begin
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(IZInterface));
    {now nil pointer or on replacing the entry we'll get a bad interlockdecrement}
    Pointer(Flist^[Index]) := nil; //see http://sourceforge.net/p/zeoslib/tickets/100/
  end;
  FList^[Index] := Item;
  Inc(FCount);
end;

function TZCollection.Last: IZInterface;
begin
  Result := Get(FCount - 1);
end;

procedure TZCollection.Put(Index: Integer; const Item: IZInterface);
begin
  {$IFNDEF DISABLE_CHECKING}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
  {$ENDIF}
  FList^[Index] := Item;
end;

function TZCollection.Remove(const Item: IZInterface): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

function TZCollection.RemoveAll(const Col: IZCollection): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Col.Count - 1 do
    Result := (Remove(Col[I]) >= 0) or Result;
end;

function TZCollection.ToString: string;
var
  I: Integer;
  TempObject: IZObject;
begin
  Result := '';
  for I := 0 to FCount - 1 do begin
    if I > 0 then
      Result := Result + ',';
    if FList^[I].QueryInterface(IZObject, TempObject) = 0
    then Result := Result + TempObject.ToString
    else Result := Result + Format('<%p>', [Pointer(FList^[I])]);
  end;
  Result := '[' + Result + ']';
end;

{ TZUnmodifiableCollection }

constructor TZUnmodifiableCollection.Create(const Collection: IZCollection);
begin
  inherited Create;
  FCollection := Collection;
end;

destructor TZUnmodifiableCollection.Destroy;
begin
  FCollection := nil;
  inherited Destroy;
end;

function TZUnmodifiableCollection.Clone: IZInterface;
begin
  Result := TZUnmodifiableCollection.Create(FCollection);
end;

procedure TZUnmodifiableCollection.RaiseException;
begin
  raise EInvalidOperation.Create(SImmutableOpIsNotAllowed);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
function TZUnmodifiableCollection.Add(const Item: IZInterface): Integer;
begin
  Result := -1;
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF} // unmodifyable - parameters not used intentionally

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
function TZUnmodifiableCollection.AddAll(const Col: IZCollection): Boolean;
begin
  Result := False;
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF} // unmodifyable - parameters not used intentionally

procedure TZUnmodifiableCollection.Clear;
begin
  RaiseException;
end;

function TZUnmodifiableCollection.Contains(const Item: IZInterface): Boolean;
begin
  Result := FCollection.Contains(Item);
end;

function TZUnmodifiableCollection.ContainsAll(const Col: IZCollection): Boolean;
begin
  Result := FCollection.ContainsAll(Col);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
procedure TZUnmodifiableCollection.Delete(Index: Integer);
begin
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF} // unmodifyable - parameters not used intentionally

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
procedure TZUnmodifiableCollection.Exchange(Index1, Index2: Integer);
begin
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF} // unmodifyable - parameters not used intentionally

function TZUnmodifiableCollection.First: IZInterface;
begin
  Result := FCollection.First;
end;

function TZUnmodifiableCollection.Get(Index: Integer): IZInterface;
begin
  Result := FCollection[Index];
end;

function TZUnmodifiableCollection.GetCount: Integer;
begin
  Result := FCollection.Count;
end;

function TZUnmodifiableCollection.GetIterator: IZIterator;
begin
  Result := TZIterator.Create(Self);
end;

function TZUnmodifiableCollection.IndexOf(const Item: IZInterface): Integer;
begin
  Result := FCollection.IndexOf(Item);
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
procedure TZUnmodifiableCollection.Insert(Index: Integer; const Item: IZInterface);
begin
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZUnmodifiableCollection.Last: IZInterface;
begin
  Result := FCollection.Last;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
procedure TZUnmodifiableCollection.Put(Index: Integer; const Item: IZInterface);
begin
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
function TZUnmodifiableCollection.Remove(const Item: IZInterface): Integer;
begin
  Result := -1;
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // unmodifyable - parameters not used intentionally
function TZUnmodifiableCollection.RemoveAll(const Col: IZCollection): Boolean;
begin
  Result := False;
  RaiseException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function TZUnmodifiableCollection.ToString: string;
begin
  Result := FCollection.ToString;
end;

{ TZHashMap }

constructor TZHashMap.Create;
begin
  inherited Create;
  FKeys := TZCollection.Create;
  FValues := TZCollection.Create;
  FReadOnlyKeys := TZUnmodifiableCollection.Create(FKeys);
  FReadOnlyValues := TZUnmodifiableCollection.Create(FValues);
end;

destructor TZHashMap.Destroy;
begin
  FReadOnlyKeys := nil;
  FReadOnlyValues := nil;
  FKeys := nil;
  FValues := nil;
  inherited Destroy;
end;

function TZHashMap.Clone: IZInterface;
var
  HashMap: TZHashMap;
begin
  HashMap := TZHashMap.Create;
  HashMap.FKeys := FKeys.Clone as IZCollection;
  HashMap.FReadOnlyKeys := FReadOnlyKeys.Clone as IZCollection;
  HashMap.FValues := FValues.Clone as IZCollection;
  HashMap.FReadOnlyValues := FReadOnlyValues.Clone as IZCollection;
  Result := HashMap;
end;

function TZHashMap.Get(const Key: IZInterface): IZInterface;
var
  Index: Integer;
begin
  Index := FKeys.IndexOf(Key);
  if Index >= 0
  then Result := FValues[Index]
  else Result := nil;
end;

procedure TZHashMap.Put(const Key: IZInterface; const Value: IZInterface);
var
 Index: Integer;
begin
  Index := FKeys.IndexOf(Key);
  if Index >= 0 then
    FValues[Index] := Value
  else begin
    FKeys.Add(Key);
    FValues.Add(Value);
  end;
end;

function TZHashMap.GetKeys: IZCollection;
begin
  Result := FReadOnlyKeys;
end;

function TZHashMap.GetValues: IZCollection;
begin
  Result := FReadOnlyValues;
end;

function TZHashMap.GetCount: Integer;
begin
  Result := FKeys.Count;
end;

function TZHashMap.Remove(const Key: IZInterface): Boolean;
var
  Index: Integer;
begin
  Index := FKeys.IndexOf(Key);
  if Index >= 0 then begin
    FKeys.Delete(Index);
    FValues.Delete(Index);
    Result := True;
  end else
    Result := False;
end;

procedure TZHashMap.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
end;

{ TZStack }

constructor TZStack.Create;
begin
  FValues := TZCollection.Create;
end;

destructor TZStack.Destroy;
begin
  FValues := nil;
  inherited Destroy;
end;

function TZStack.Clone: IZInterface;
var
  Stack: TZStack;
begin
  Stack := TZStack.Create;
  Stack.FValues := FValues.Clone as IZCollection;
  Result := Stack;
end;

function TZStack.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TZStack.Peek: IZInterface;
begin
  if FValues.Count > 0
  then Result := FValues[FValues.Count - 1]
  else Result := nil;
end;

function TZStack.Pop: IZInterface;
begin
  if FValues.Count > 0 then begin
    Result := FValues[FValues.Count - 1];
    FValues.Delete(FValues.Count - 1);
  end else
    Result := nil;
end;

procedure TZStack.Push(const Value: IZInterface);
begin
  FValues.Add(Value);
end;

function TZStack.ToString: string;
begin
  Result := FValues.ToString;
end;

end.
