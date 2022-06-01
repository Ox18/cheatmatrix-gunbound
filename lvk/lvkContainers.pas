{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains various reference-counted interface containers.
}
unit lvkContainers;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 28.04.03 0:30 $
// $Archive: /Components/LVK/Source/lvkContainers.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Dialogs, SysUtils, Classes, SyncObjs, lvkStandardQueueItems,
  lvkVersion;

type
  IKeyedItem = interface;

  { Description:
      This interface is supported by the list objects. Look at the
      IKeyedItem.AddSubscriber and IKeyedItem.RemoveSubscriber methods
      for some more information.
    See also:
      IKeyedItem, IKeyedInterfaceList
  }
  IKeyedItemSubscriber = interface
    ['{B5C00F60-7508-49ED-95B1-17F4E75FAA90}']

    procedure BeforeKeyChange(const Item: IKeyedItem);
    procedure AfterKeyChange(const Item: IKeyedItem);
  end;

  { Description:
      This interface has to be supported by all objects that are going to be
      added to a list of type IKeyedInterfaceList or IHashedInterfaceList.

      This interface provides the key to the list, which is used to
      calculate or find the correct spot in the list to store the item.
  }
  IKeyedItem = interface
    ['{7F146C5E-3383-4BD4-AFE6-0017461E3040}']

    // <COMBINE Key>
    function GetKey: string;
    { Description:
        This property returns the current key of the item. The key is simply
        a string used to identify the object. The lists does not support
        multiple items with the same keys, and any items inserted will
        overwrite any existing items with the same key.
    }
    property Key: string read GetKey;

    { Description:
        Whenever the item is added to a list, a subscriber is added to the
        item. The subscriber is a proxy for the list.

        If the key of the item changes, the item must probably be relocated
        in the list, so the item has the responsibility of notifying the
        list of this change when it happens.

        The way to change the key of an item is like this:

          1. call Subscriber.BeforeKeyChange@IKeyedItem
          2. change the key
          3. call Subscriber.AfterKeyChange@IKeyedItem

        This will relocate (if necessary) then item in the list so it can
        be found afterwards.

        The item is responsible for keeping a list of subscribers internally,
        and call the notification methods on all subscribers.

        If the item is programmed to not allow the key to change, this
        method, along with RemoveSubscriber@IKeyedItemSubscriber, can be empty
        to avoid having the overhead of storing the subscriber list for each
        item.

      Parameters:
        Subscriber - The subscriber to notify when the key changes.
      See also:
        RemoveSubscriber@IKeyedItemSubscriber
    }
    procedure AddSubscriber(const Subscriber: IKeyedItemSubscriber);

    { Description:
        This item will remove the subscriber from the items list, and is
        called when the item is removed from the list or the list is
        destroyed.
      Parameters:
        Subscriber - The subscriber to remove from the item.
      See also:
        AddSubscriber@IKeyedItemSubscriber
    }
    procedure RemoveSubscriber(const Subscriber: IKeyedItemSubscriber);
  end;

  { Description:
      This interface is returned from the CreateIterator function and can
      be used to iterate quickly through all the items in the list. Notice that
  }
  IItemIterator = interface
    ['{C3CB4CE6-9818-4110-A9AF-C7D958270B5E}']

    { Description:
        This function returns True if calling Next would return another item
        from the list.
      See also:
        Next
    }
    function HasNext: Boolean;

    { Description:
        This function returns the next available item from the list, or nil
        if no more items in the list.
      See also:
        HasNext
    }
    function Next: IUnknown;
  end;

  { Description:
      This interface list stores the items in sorted order, according to their
      key.
  }
  IKeyedInterfaceList = interface
    ['{878CE7C4-B040-46AC-BD1C-C44A579C0B46}']

    // <COMBINE Capacity>
    function GetCapacity: Integer;
    // <COMBINE Capacity>
    procedure SetCapacity(const NewCapacity: Integer);
    { Description:
        This property returns the current capacity of the interface list. The
        capacity controls how many items the list can take before it has to
        resize its internal data structures. The less times it has to
        resize those the better since it has to allocate a new data structure
        and then copy the data from the old to the new, which takes time.

        The program can also set the capacity through this property if it
        knows how many items it will need to insert, to avoid such costly
        resizes.
      Summary:
        The number of items the list can hold before it needs to resize its
        internal data structures.
    }
    property Capacity: Integer read GetCapacity write SetCapacity;

    // <COMBINE Count>
    function GetCount: Integer;
    { Description:
        This property returns the current number of items in the list.
    }
    property Count: Integer read GetCount;

    { Description:
        This method clears the list by removing all the items from it.

        Note: The capacity is left as it is.
      See also:
        Delete
    }
    procedure Clear;

    { Description:
        Deletes an item from the list by its numeric index, where 0 is the
        first item in the list.
      Parameters:
        Index - The index of the item to delete.
      See also:
        Delete@string, Delete@IKeyedItem
    }
    procedure Delete(const Index: Integer); overload;

    { Description:
        Deletes an item from the list by its string key.

        Note: If there is no item in the list with the specified key,
          no action will be taken.
      Parameters:
        Key - The string key of the item to delete.
      See also:
        Delete@Integer, Delete@IKeyedItem
    }
    procedure Delete(const Key: string); overload;

    { Description:
        Deletes an item from the list by its interface reference.

        Note: If the specified item is not in the list, no action will be taken.
      Parameters:
        Item - The item to delete from the list.
      See also:
        Delete@Integer, Delete@string
    }
    procedure Delete(const Item: IKeyedItem); overload;

    { Description:
        This function returns the index of a given item in the list, or -1 if
        the item is not present in the list.
      Parameters:
        Item - The item to find in the list.
      See also:
        IndexOf@string
    }
    function IndexOf(const Item: IKeyedItem): Integer; overload;

    { Description:
        This function returns the index of the item with the given string key
        in the list, or -1 if no such item is found in the list.
      Parameters:
        Key - The key of the item to locate.
      See also:
        IndexOf@IKeyedItem
    }
    function IndexOf(const Key: string): Integer; overload;

    { Description:
        This function returns True if the given item is present in the list,
        False if not. It's equivalent to calling IndexOf and checking if the
        index is 0 or higher.
      Parameters:
        Item - The item to check for in the list.
      See also:
        IndexOf@Item, Exists@string
    }
    function Exists(const Item: IKeyedItem): Boolean; overload;

    { Description:
        This function returns True if an item with the given key is present
        in the list, False if not. It's equivalent to calling IndexOf and
        checking if the index is 0 or higher.
      Parameters:
        Key - The key of the item to check for in the list.
      See also:
        IndexOf@string, Exists@IKeyedItem
    }
    function Exists(const Key: string): Boolean; overload;

    // <COMBINE Keys>
    function GetKeys(const Index: Integer): string;
    { Description:
        This property retrieves the key of a given item by its index.
      Parameters:
        Index - The index of the item to return the key for.
    }
    property Keys[const Index: Integer]: string read GetKeys;

    function Add(const Item: IKeyedItem): IKeyedItem;

    procedure Lock;
    procedure Unlock;

    function GetItems(const Index: Integer): IKeyedItem;
    property Items[const Index: Integer]: IKeyedItem read GetItems; default;

    function GetItemsByKey(const Key: string): IKeyedItem;
    property ItemsByKey[const Key: string]: IKeyedItem read GetItemsByKey;

    function First: IKeyedItem;
    function Next(const Item: IKeyedItem): IKeyedItem;
    function Previous(const Item: IKeyedItem): IKeyedItem;
    function Last: IKeyedItem;

    procedure CreateSnapshot(out Snapshot: IInterfaceList);
  end;

  { Description:
      This interface list stores the items in a hash table, indexed by their
      key.
  }
  IHashedInterfaceList = interface(IKeyedInterfaceList)
    ['{3D9E2E5F-90F8-4414-9069-63792AE0878C}']

    procedure Grow;
    procedure Reduce;
    function FillFactor: Double;

    function GetAutoGrowFillFactor: Double;
    procedure SetAutoGrowFillFactor(const NewFillFactor: Double);
    property AutoGrowFillFactor: Double read GetAutoGrowFillFactor write SetAutoGrowFillFactor;

    function GetAutoReduceFillFactor: Double;
    procedure SetAutoReduceFillFactor(const NewFillFactor: Double);
    property AutoReduceFillFactor: Double read GetAutoReduceFillFactor write SetAutoReduceFillFactor;
  end;

  { Description:
      This class can be used as the base class for key-based classes. It
      implements the necessary interfaces needed to support the interface
      lists stored in this unit.
  }
  TKeyedItemBase = class(TInterfacedObject, IKeyedItem)
  private
    FKey          : string;
    FSubscribers  : IInterfaceList;

  protected
    // IKeyedItem interface
    function GetKey: string; virtual;
    procedure SetKey(const NewKey: string); virtual;
    property Key: string read GetKey write SetKey;
    procedure AddSubscriber(const Subscriber: IKeyedItemSubscriber); virtual;
    procedure RemoveSubscriber(const Subscriber: IKeyedItemSubscriber); virtual;
    function GetPackageVersion: TPackageVersion;

  public
    constructor Create(const Key: string=''; const TrackSubscribers: Boolean=True);
  end;

  { Description:
      This interface is returned from the CreateKeyedItemWrapper@string@IUnknown
      function.
    See also:
      CreateKeyedItemWrapper@string@IUnknown
  }
  IKeyedItemWrapper = interface
    ['{6733637A-D53A-4A92-A171-9F5A6CBFFA87}']

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion;

    function GetItem: IUnknown;
    procedure SetItem(const NewItem: IUnknown);
    property Item: IUnknown read GetItem write SetItem;

    function GetKey: string;
    procedure SetKey(const NewKey: string);
    property Key: string read GetKey write SetKey;
  end;

  TItemSortFunc = function(const Item1, Item2: IUnknown): Integer of object;
  TItemFilterFunc = function(const Item: IUnknown): Boolean of object;

const
  DEFAULT_INITIAL_CAPACITY        = 0;                // Grow as needed
  DEFAULT_AUTO_REDUCE_FILL_FACTOR = 1.0/8.0;          // Reduce when list is one eigth full
  DEFAULT_AUTO_GROW_FILL_FACTOR   = 1.0/2.0;          // Grow when list is half full

{ Description:
    This function returns an iterator for the interface list. This iterator
    contains a snapshot of the contents of the interface list at the time
    it was created. Even if items are removed from the list, the iterator
    will still run through them.
  Parameters:
    List - The list to create an iterator for.
    Reverse - Set to True to produce an iterator that runs from the last item
      towards the first item, instead of from the first to the last.
    SortFunc - The sort function to use to sort the items, set to nil to leave
      the items in the order found in the list.
    FilterFunc - A function that will filter the items so that only items
      that returns True from this filter function will be included in the
      iterator.
}
function CreateIterator(const List: IKeyedInterfaceList; const Reverse: Boolean=False;
  const SortFunc: TItemSortFunc=nil; const FilterFunc: TItemFilterFunc=nil): IItemIterator;

{ Description:
    This function wraps an interface for an object that doesn't support
    the IKeyedItem interface inside an object that supports it.
  Parameters:
    Key - The key to store.
    Item - The item to wrap.
}
function CreateKeyedItemWrapper(const Key: string; const Item: IUnknown): IKeyedItemWrapper;

{ Description:
    This function creates and configures an instance of an object that supports
    the IKeyedInterfaceList interface.
  Parameters:
    InitialCapacity - How many items the list has space for initially.
  See also:
    IKeyedInterfaceList
}
function CreateKeyedInterfaceList(
  const InitialCapacity: Integer=DEFAULT_INITIAL_CAPACITY
  ): IKeyedInterfaceList;

{ Description:
    This function creates and configures an instance of an object that supports
    the IHashedInterfaceList interface.
  Parameters:
    InitialCapacity - How many items the list has space for initially.
    AutoGrowFillFactor - A value between 0.0 and 1.0. A value of .75 means that
      whenever the fill factor (number of filled buckets vs. total buckets)
      raises above 75%, the list grows upwards in size.
    AutoReduceFillFactor - A value between 0.0 and 1.0. A value of .25 means
      that whenever the fill factor reduces below 25%, the list shrinks
      downwards in size.
  See also:
    IHashedInterfaceList
}
function CreateHashedInterfaceList(
  const InitialCapacity: Integer=DEFAULT_INITIAL_CAPACITY;
  const AutoGrowFillFactor: Double=DEFAULT_AUTO_GROW_FILL_FACTOR;
  const AutoReduceFillFactor: Double=DEFAULT_AUTO_REDUCE_FILL_FACTOR): IHashedInterfaceList;

{ Description:
    This function produces a hash value from the key.
  Parameters:
    -
  See also:
    -
}
function KeyHash(const Key: string; const Modulo: Cardinal): Cardinal;

implementation

type
  TKeyedItemWrapper = class(TKeyedItemBase, IUnknown, IKeyedItemWrapper,
    IPackageVersion)
  private
    FItem : IUnknown;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

    // IKeyedItemWrapper interface
    function GetItem: IUnknown; virtual;
    procedure SetItem(const NewItem: IUnknown); virtual;

  public
    constructor Create(const Key: string; const Item: IUnknown);
  end;

  TItemIterator = class(TInterfacedObject, IItemIterator, IPackageVersion)
  private
    FList       : IInterfaceList;
    FIndex      : Integer;
    FDirection  : Integer;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IItemIterator interface
    function HasNext: Boolean;
    function Next: IUnknown;

    // Internal
    procedure Snapshot(const List: IUnknown; const FilterFunc: TItemFilterFunc); virtual;
    procedure Sort(const SortFunc: TItemSortFunc); virtual;

  public
    constructor Create(const List: IUnknown; const Reverse: Boolean;
      const SortFunc: TItemSortFunc; const FilterFunc: TItemFilterFunc);
  end;

  TItemSubscriber = class;
  TKeyedInterfaceListSubscriber = class(TInterfacedObject, IKeyedItemSubscriber)
  private
    FList : TItemSubscriber;

  protected
    // IKeyedItemSubscriber interface
    procedure BeforeKeyChange(const Item: IKeyedItem);
    procedure AfterKeyChange(const Item: IKeyedItem);

  public
    constructor Create(const List: TItemSubscriber);
  end;

  TItemSubscriber = class(TInterfacedObject, IKeyedItemSubscriber)
  protected
    // IKeyedItemSubscriber interface
    procedure BeforeKeyChange(const Item: IKeyedItem); virtual;
    procedure AfterKeyChange(const Item: IKeyedItem); virtual;
  end;

  TKeyedInterfaceList = class(TItemSubscriber, IKeyedInterfaceList,
    IPackageVersion)
  private
    FList       : IInterfaceList;
    FSubscriber : IKeyedItemSubscriber;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // Internal
    procedure InsertIntoList(const Item: IKeyedItem; const AddSubscriber: Boolean); virtual;
    procedure DeleteFromList(const Key: string; const RemoveSubscriber: Boolean); virtual;

    // IKeyedInterfaceList interface
    function GetCapacity: Integer; virtual;
    procedure SetCapacity(const NewCapacity: Integer); virtual;
    function GetCount: Integer; virtual;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload; virtual;
    procedure Delete(const Key: string); overload; virtual;
    procedure Delete(const Item: IKeyedItem); overload; virtual;
    function IndexOf(const Item: IKeyedItem): Integer; overload; virtual;
    function IndexOf(const Key: string): Integer; overload; virtual;
    function Exists(const Item: IKeyedItem): Boolean; overload; virtual;
    function Exists(const Key: string): Boolean; overload; virtual;
    function GetKeys(const Index: Integer): string; virtual;
    function Add(const Item: IKeyedItem): IKeyedItem; virtual;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    function GetItems(const Index: Integer): IKeyedItem; virtual;
    function GetItemsByKey(const Key: string): IKeyedItem; virtual;
    function First: IKeyedItem; virtual;
    function Next(const Item: IKeyedItem): IKeyedItem; virtual;
    function Previous(const Item: IKeyedItem): IKeyedItem; virtual;
    function Last: IKeyedItem; virtual;
    procedure CreateSnapshot(out Snapshot: IInterfaceList); virtual;

    // IKeyedItemSubscriber interface
    procedure BeforeKeyChange(const Item: IKeyedItem); override;
    procedure AfterKeyChange(const Item: IKeyedItem); override;

  public
    constructor Create(const InitialCapacity: Integer);
    destructor Destroy; override;
  end;

  PHashRec = ^THashRec;
  THashRec = record
    Item  : IKeyedItem;
    Next  : PHashRec;
    Prev  : PHashRec;
  end;

  THashedInterfaceList = class(TItemSubscriber, IHashedInterfaceList,
    IKeyedInterfaceList, IPackageVersion)
  private
    FList                 : TList;
    FLock                 : TCriticalSection;
    FCount                : Integer;
    FSubscriber           : IKeyedItemSubscriber;

    FFill                 : Integer;
    FAutoGrowFillFactor   : Double;
    FAutoReduceFillFactor : Double;
    FReachedCapacity      : Boolean;

  protected
    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // Internal
    function AdjustCapacity(const Capacity: Integer): Integer;
    procedure InsertIntoList(const Item: IKeyedItem; const AddSubscriber: Boolean); virtual;
    procedure DeleteFromList(const Key: string; const RemoveSubscriber: Boolean); virtual;
    procedure RecomputeStatistics;
    function GetAutoGrowFillFactor: Double;
    procedure SetAutoGrowFillFactor(const NewFillFactor: Double);
    function GetAutoReduceFillFactor: Double;
    procedure SetAutoReduceFillFactor(const NewFillFactor: Double);
    procedure CheckAutoGrowReduce;

    // IKeyedInterfaceList interface
    function GetCapacity: Integer; virtual;
    procedure SetCapacity(const NewCapacity: Integer); virtual;
    function GetCount: Integer; virtual;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload; virtual;
    procedure Delete(const Key: string); overload; virtual;
    procedure Delete(const Item: IKeyedItem); overload; virtual;
    function IndexOf(const Item: IKeyedItem): Integer; overload; virtual;
    function IndexOf(const Key: string): Integer; overload; virtual;
    function Exists(const Item: IKeyedItem): Boolean; overload; virtual;
    function Exists(const Key: string): Boolean; overload; virtual;
    function GetKeys(const Index: Integer): string; virtual;
    function Add(const Item: IKeyedItem): IKeyedItem; virtual;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    function GetItems(const Index: Integer): IKeyedItem; virtual;
    function GetItemsByKey(const Key: string): IKeyedItem; virtual;
    function First: IKeyedItem; virtual;
    function Next(const Item: IKeyedItem): IKeyedItem; virtual;
    function Previous(const Item: IKeyedItem): IKeyedItem; virtual;
    function Last: IKeyedItem; virtual;
    procedure CreateSnapshot(out Snapshot: IInterfaceList); virtual;

    // IHashedInterfaceList interface
    procedure Grow; virtual;
    procedure Reduce; virtual;
    function FillFactor: Double; virtual;

    // IKeyedItemSubscriber interface
    procedure BeforeKeyChange(const Item: IKeyedItem); override;
    procedure AfterKeyChange(const Item: IKeyedItem); override;

  public
    constructor Create(const InitialCapacity: Integer;
      const AutoGrowFillFactor, AutoReduceFillFactor: Double);
    destructor Destroy; override;
  end;

function CreateKeyedItemWrapper(const Key: string; const Item: IUnknown): IKeyedItemWrapper;
begin
  Result := TKeyedItemWrapper.Create(Key, Item) as IKeyedItemWrapper;
end;

function CreateKeyedInterfaceList(
  const InitialCapacity: Integer): IKeyedInterfaceList;
begin
  Result := TKeyedInterfaceList.Create(InitialCapacity) as IKeyedInterfaceList;
end;

function CreateHashedInterfaceList(
  const InitialCapacity: Integer;
  const AutoGrowFillFactor, AutoReduceFillFactor: Double): IHashedInterfaceList;
begin
  Result := THashedInterfaceList.Create(InitialCapacity, AutoGrowFillFactor,
    AutoReduceFillFactor) as IHashedInterfaceList;
end;

function CreateIterator(const List: IKeyedInterfaceList; const Reverse: Boolean=False;
  const SortFunc: TItemSortFunc=nil; const FilterFunc: TItemFilterFunc=nil): IItemIterator;
begin
  Result := TItemIterator.Create(List, Reverse, SortFunc, FilterFunc) as IItemIterator;
end;

{$IFOPT R+}
  {$DEFINE RANGEON}
{$ENDIF}
{$IFOPT Q+}
  {$DEFINE OVERFLOWON}
{$ENDIF}
{$R-,Q-}
function KeyHash(const Key: string; const Modulo: Cardinal): Cardinal;
var
  Hash  : Cardinal;
  Index : Integer;
  a,b   : Cardinal;
begin
  Assert(Key <> '');
  Assert(Modulo > 0);

  a := 31415;
  b := 27183;
  Hash := 0;
  for Index := 1 to Length(Key) do
  begin
    Hash := (a*Hash + Ord(Key[Index])) mod Modulo;
    a := (a * b) mod (Modulo-1);
  end;

  Result := Hash mod Modulo;
end;
{$IFDEF RANGEON}
  {$R+}
{$ENDIF}
{$IFDEF OVERFLOWON}
  {$Q+}
{$ENDIF}

{ TKeyedItemBase }

procedure TKeyedItemBase.AddSubscriber(
  const Subscriber: IKeyedItemSubscriber);
begin
  if Assigned(FSubscribers) then
    FSubscribers.Add(Subscriber);
end;

constructor TKeyedItemBase.Create(const Key: string; const TrackSubscribers: Boolean);
begin
  inherited Create;

  if Key = '' then
    FKey := IntToHex(Integer(Self), 8)
  else
    FKey := Key;
  if TrackSubscribers then
    FSubscribers := TInterfaceList.Create as IInterfaceList
  else
    FSubscribers := nil;
end;

function TKeyedItemBase.GetKey: string;
begin
  Result := FKey;
end;

function TKeyedItemBase.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TKeyedItemBase.RemoveSubscriber(
  const Subscriber: IKeyedItemSubscriber);
begin
  if Assigned(FSubscribers) then
    FSubscribers.Remove(Subscriber);
end;

procedure TKeyedItemBase.SetKey(const NewKey: string);
var
  Index : Integer;
begin
  if NewKey <> FKey then
  begin
    for Index := 0 to FSubscribers.Count-1 do
      (FSubscribers[Index] as IKeyedItemSubscriber).BeforeKeyChange(Self as IKeyedItem);

    FKey := NewKey;

    for Index := 0 to FSubscribers.Count-1 do
      (FSubscribers[Index] as IKeyedItemSubscriber).AfterKeyChange(Self as IKeyedItem);
  end;
end;

{ TKeyedItemWrapper }

constructor TKeyedItemWrapper.Create(const Key: string;
  const Item: IUnknown);
begin
  inherited Create(Key);

  FItem := Item;
end;

function TKeyedItemWrapper.GetItem: IUnknown;
begin
  Result := FItem;
end;

function TKeyedItemWrapper.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TKeyedItemWrapper.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := FItem.QueryInterface(IID, Obj);
end;

procedure TKeyedItemWrapper.SetItem(const NewItem: IUnknown);
begin
  FItem := NewItem;
end;

{ TKeyedInterfaceList }

function TKeyedInterfaceList.Add(const Item: IKeyedItem): IKeyedItem;
begin
  Assert(Assigned(Item));

  InsertIntoList(Item, True);
  Result := Item;
end;

procedure TKeyedInterfaceList.AfterKeyChange(const Item: IKeyedItem);
begin
  InsertIntoList(Item, False);
end;

procedure TKeyedInterfaceList.BeforeKeyChange(const Item: IKeyedItem);
var
  Index : Integer;
begin
  Lock;
  try
    Index := IndexOf(Item.Key);
    FList.Delete(Index);
  finally
    Unlock;
  end;
end;

procedure TKeyedInterfaceList.Clear;
var
  Item  : IKeyedItem;
  Index : Integer;
begin
  Lock;
  try
    for Index := 0 to FList.Count-1 do
    begin
      Item := IKeyedItem(FList[Index]);
      Item.RemoveSubscriber(FSubscriber);
    end;

    FList.Clear;
  finally
    Unlock;
  end;
end;

constructor TKeyedInterfaceList.Create(const InitialCapacity: Integer);
begin
  inherited Create;

  FList := TInterfaceList.Create as IInterfaceList;
  FList.Capacity := InitialCapacity;
  FSubscriber := TKeyedInterfaceListSubscriber.Create(Self) as IKeyedItemSubscriber;
end;

procedure TKeyedInterfaceList.Delete(const Key: string);
begin
  DeleteFromList(Key, True);
end;

procedure TKeyedInterfaceList.Delete(const Index: Integer);
begin
  Lock;
  try
    Assert((Index >= 0) and (Index < FList.Count));
    Delete(IKeyedItem(FList[Index]).Key);
  finally
    Unlock;
  end;
end;

procedure TKeyedInterfaceList.CreateSnapshot(out Snapshot: IInterfaceList);
var
  Index : Integer;
begin
  Lock;
  try
    Snapshot := TInterfaceList.Create as IInterfaceList;
    Snapshot.Capacity := FList.Count;

    for Index := 0 to FList.Count-1 do
      Snapshot.Add(FList[Index]);
  finally
    Unlock;
  end;
end;

procedure TKeyedInterfaceList.Delete(const Item: IKeyedItem);
begin
  Delete(Item.Key);
end;

procedure TKeyedInterfaceList.DeleteFromList(const Key: string;
  const RemoveSubscriber: Boolean);
var
  Index : Integer;
  Item  : IKeyedItem;
begin
  Assert(Key <> '');
  
  Lock;
  try
    Index := IndexOf(Key);
    if Index >= 0 then
    begin
      Item := IKeyedItem(FList[Index]);
      FList.Delete(Index);

      if RemoveSubscriber then
        Item.RemoveSubscriber(FSubscriber);
    end;
  finally
    Unlock;
  end;
end;

destructor TKeyedInterfaceList.Destroy;
begin
  Clear;

  inherited;
end;

function TKeyedInterfaceList.Exists(const Item: IKeyedItem): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

function TKeyedInterfaceList.Exists(const Key: string): Boolean;
begin
  Result := IndexOf(Key) >= 0;
end;

function TKeyedInterfaceList.First: IKeyedItem;
begin
  Lock;
  try
    if FList.Count = 0 then
      Result := nil
    else
      Result := IKeyedItem(FList[0]);
  finally
    Unlock;
  end;
end;

function TKeyedInterfaceList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TKeyedInterfaceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TKeyedInterfaceList.GetItems(const Index: Integer): IKeyedItem;
begin
  Lock;
  try
    Assert((Index >= 0) and (Index < FList.Count));
    Result := IKeyedItem(FList[Index]);
  finally
    Unlock;
  end;
end;

function TKeyedInterfaceList.GetItemsByKey(const Key: string): IKeyedItem;
var
  Index : Integer;
begin
  Assert(Key <> '');

  Lock;
  try
    Index := IndexOf(Key);
    if Index < 0 then
      Result := nil
    else
      Result := IKeyedItem(FList[Index]);
  finally
    Unlock;
  end;
end;

function TKeyedInterfaceList.GetKeys(const Index: Integer): string;
begin
  Lock;
  try
    Assert((Index >= 0) and (Index < FList.Count));
    Result := IKeyedItem(FList[Index]).Key;
  finally
    Unlock;
  end;
end;

function TKeyedInterfaceList.IndexOf(const Key: string): Integer;
var
  i1, i2, im    : Integer;
  KeyedItem     : IKeyedItem;
  CompareResult : Integer;
begin
  Assert(Key <> '');

  Lock;
  try
    i1 := 0;
    i2 := FList.Count-1;

    Result := -1;
    while i1 <= i2 do
    begin
      im := (i1 + i2) div 2;

      KeyedItem := IKeyedItem(FList[im]);
      CompareResult := CompareText(KeyedItem.Key, Key);
      if CompareResult < 0 then
        i1 := im+1
      else if CompareResult > 0 then
        i2 := im-1
      else begin
        Result := im;
        Break;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TKeyedInterfaceList.IndexOf(const Item: IKeyedItem): Integer;
begin
  Assert(Assigned(Item));

  Result := IndexOf(Item.Key);
end;

procedure TKeyedInterfaceList.InsertIntoList(const Item: IKeyedItem; const AddSubscriber: Boolean);
var
  CompareResult : Integer;
  KeyedItem     : IKeyedItem;
  i1, i2, im    : Integer;
  Index         : Integer;
begin
  Assert(Assigned(Item));

  Lock;
  try
    if FList.Count = 0 then
    begin
      FList.Add(Item);
      if AddSubscriber then
        Item.AddSubscriber(FSubscriber);
    end else if FList.Count = 1 then
    begin
      KeyedItem := IKeyedItem(FList[0]);
      CompareResult := CompareText(Item.Key, KeyedItem.Key);
      if CompareResult < 0  then
        FList.Insert(0, Item)
      else if CompareResult > 0 then
        FList.Add(Item)
      else begin
        KeyedItem.RemoveSubscriber(FSubscriber);
        FList[0] := Item;
      end;

      if AddSubscriber then
        Item.AddSubscriber(FSubscriber);
    end else begin
      i1 := 0;
      i2 := FList.Count-1;

      Index := -1;
      while i1 <= i2 do
      begin
        im := (i1 + i2) div 2;

        KeyedItem := IKeyedItem(FList[im]);
        CompareResult := CompareText(KeyedItem.Key, Item.Key);
        if CompareResult < 0 then
          i1 := im+1
        else if CompareResult > 0 then
          i2 := im-1
        else begin
          Index := im;
          Break;
        end;
      end;

      if Index <> -1 then
      begin
        KeyedItem := IKeyedItem(FList[Index]);
        FList[Index] := Item;

        KeyedItem.RemoveSubscriber(FSubscriber);
        if AddSubscriber then
          Item.AddSubscriber(FSubscriber);
      end else begin
        FList.Insert(i1, Item);
        if AddSubscriber then
          Item.AddSubscriber(FSubscriber);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TKeyedInterfaceList.Last: IKeyedItem;
begin
  Lock;
  try
    if FList.Count = 0 then
      Result := nil
    else
      Result := IKeyedItem(FList[FList.Count-1]);
  finally
    Unlock;
  end;
end;

procedure TKeyedInterfaceList.Lock;
begin
  FList.Lock;
end;

function TKeyedInterfaceList.Next(const Item: IKeyedItem): IKeyedItem;
var
  Index : Integer;
begin
  Assert(Assigned(Item));

  Lock;
  try
    Index := IndexOf(Item.Key);
    if (Index >= 0) and (Index < FList.Count-1) then
      Result := IKeyedItem(FList[Index+1])
    else
      Result := nil;
  finally
    Unlock;
  end;
end;

function TKeyedInterfaceList.Previous(const Item: IKeyedItem): IKeyedItem;
var
  Index : Integer;
begin
  Assert(Assigned(Item));

  Lock;
  try
    Index := IndexOf(Item.Key);
    if Index > 0 then
      Result := IKeyedItem(FList[Index-1])
    else
      Result := nil;
  finally
    Unlock;
  end;
end;

procedure TKeyedInterfaceList.SetCapacity(const NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

procedure TKeyedInterfaceList.Unlock;
begin
  FList.Unlock;
end;

function TKeyedInterfaceList.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

{ TKeyedInterfaceListSubscriber }

procedure TKeyedInterfaceListSubscriber.AfterKeyChange(
  const Item: IKeyedItem);
begin
  FList.AfterKeyChange(Item);
end;

procedure TKeyedInterfaceListSubscriber.BeforeKeyChange(
  const Item: IKeyedItem);
begin
  FList.BeforeKeyChange(Item);
end;

constructor TKeyedInterfaceListSubscriber.Create(
  const List: TItemSubscriber);
begin
  Assert(Assigned(List));

  inherited Create;

  FList := List;
end;

{ THashedInterfaceList }

function THashedInterfaceList.Add(const Item: IKeyedItem): IKeyedItem;
begin
  Assert(Assigned(Item));

  InsertIntoList(Item, True);
  Result := Item;
end;

function THashedInterfaceList.AdjustCapacity(
  const Capacity: Integer): Integer;
const
  PrimeTable  : array[0..28] of Integer = (
    7, 13, 31, 61, 127,
    251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521, 131071, 262139,
    524287, 1048573, 2097143, 4194301, 8388593, 16777213, 33554393,
    67108859, 134217689, 268435399, 536870909, 1073741789, 2147483647
  );
var
  Index : Integer;
begin
  Index := Low(PrimeTable);
  if Capacity > PrimeTable[Index] then
    while (Index < High(PrimeTable)) and (PrimeTable[Index] < Capacity) do
      Inc(Index);

  Result := PrimeTable[Index];
end;

procedure THashedInterfaceList.AfterKeyChange(const Item: IKeyedItem);
begin
  Assert(Assigned(Item));
  InsertIntoList(Item, False);
end;

procedure THashedInterfaceList.BeforeKeyChange(const Item: IKeyedItem);
begin
  Assert(Assigned(Item));
  DeleteFromList(Item.Key, False);
end;

procedure THashedInterfaceList.CheckAutoGrowReduce;
var
  CurrentCapacity : Integer;
begin
  CurrentCapacity := GetCapacity;

  Lock;
  try
    if not FReachedCapacity then
      if CurrentCapacity >= GetCapacity then
        FReachedCapacity := True;

    if FReachedCapacity then
    begin
      repeat
        CurrentCapacity := GetCapacity;

        if FillFactor >= FAutoGrowFillFactor then
          Grow
        else if FillFactor < FAutoReduceFillFactor then
          Reduce;

      until (CurrentCapacity = GetCapacity);
    end;
  finally
    Unlock;
  end;
end;

procedure THashedInterfaceList.Clear;
var
  h1, h2          : PHashRec;
  Index           : Integer;
  OldReduceFactor : Double;
begin
  Lock;
  try
    // Leave out auto-reduce until we've cleared the whole list
    OldReduceFactor := FAutoReduceFillFactor;
    FAutoReduceFillFactor := 0.0;

    try
      for Index := 0 to FList.Capacity-1 do
      begin
        if Assigned(FList) then
        begin
          h1 := FList[Index];
          FList[Index] := nil;
          while Assigned(h1) do
          begin
            h2 := h1;
            h1 := h1^.Next;

            h2^.Item.RemoveSubscriber(FSubscriber);
            Dispose(h2);
          end;
        end;
      end;
      FCount := 0;
    finally
      // Re-enable auto-reduce and check if we need to reduce now
      FAutoReduceFillFactor := OldReduceFactor;
      CheckAutoGrowReduce;
    end;
  finally
    Unlock;
  end;
end;

constructor THashedInterfaceList.Create(
  const InitialCapacity: Integer;
  const AutoGrowFillFactor, AutoReduceFillFactor: Double);
var
  Index : Integer;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FList := TList.Create;
  FSubscriber := TKeyedInterfaceListSubscriber.Create(Self) as IKeyedItemSubscriber;
  FAutoGrowFillFactor := AutoGrowFillFactor;
  FAutoReduceFillFactor := AutoReduceFillFactor;
  FReachedCapacity := False;

  FList.Capacity := AdjustCapacity(1);
  for Index := 0 to FList.Capacity-1 do
    FList.Add(nil);
end;

procedure THashedInterfaceList.Delete(const Index: Integer);
begin
  Lock;
  try
    Assert((Index >= 0) and (Index < FCount));
    Delete(GetKeys(Index));
  finally
    Unlock;
  end;
end;

procedure THashedInterfaceList.Delete(const Key: string);
begin
  DeleteFromList(Key, True);
end;

procedure THashedInterfaceList.CreateSnapshot(
  out Snapshot: IInterfaceList);
var
  Index   : Integer;
  HashRec : PHashRec;
begin
  Lock;
  try
    Snapshot := TInterfaceList.Create as IInterfaceList;
    Snapshot.Capacity := FCount;
    for Index := 0 to FList.Capacity-1 do
    begin
      HashRec := FList[Index];
      while Assigned(HashRec) do
      begin
        Snapshot.Add(HashRec^.Item);
        HashRec := HashRec^.Next;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure THashedInterfaceList.Delete(const Item: IKeyedItem);
begin
  Assert(Assigned(Item));

  Delete(Item.Key);
end;

procedure THashedInterfaceList.DeleteFromList(const Key: string;
  const RemoveSubscriber: Boolean);
var
  Index   : Integer;
  Item    : IKeyedItem;
  HashRec : PHashRec;
begin
  Lock;
  try
    Index := KeyHash(Key, FList.Capacity);
    HashRec := FList[Index];

    if Assigned(HashRec) then
    begin
      while Assigned(HashRec) do
      begin
        if CompareText(HashRec^.Item.Key, Key) = 0 then
        begin
          Item := HashRec^.Item;
          if HashRec^.Prev = nil then
            FList[Index] := HashRec^.Next;
          if Assigned(HashRec^.Prev) then
            HashRec^.Prev^.Next := HashRec^.Next
          else
            FList[Index] := HashRec^.Next;

          if Assigned(HashRec^.Next) then
            HashRec^.Next^.Prev := HashRec^.Prev;

          Dispose(HashRec);

          if RemoveSubscriber then
            Item.RemoveSubscriber(FSubscriber);
          Dec(FCount);

          if not Assigned(FList[Index]) then
            Dec(FFill);

          CheckAutoGrowReduce;
          Break;
        end else
          HashRec := HashRec^.Next;
      end;
    end;
  finally
    Unlock;
  end;
end;

destructor THashedInterfaceList.Destroy;
begin
  Clear;

  FList.Free;
  FLock.Free;

  inherited;
end;

function THashedInterfaceList.Exists(const Item: IKeyedItem): Boolean;
begin
  Result := Exists(Item.Key);
end;

function THashedInterfaceList.Exists(const Key: string): Boolean;
begin
  Result := Assigned(GetItemsByKey(Key));
end;

function THashedInterfaceList.FillFactor: Double;
begin
  if FCount = 0 then
    Result := 0
  else
    Result := FFill / FList.Capacity;
end;

function THashedInterfaceList.First: IKeyedItem;
var
  Index   : Integer;
  HashRec : PHashRec;
begin
  Lock;
  try
    Index := 0;
    while (Index < FList.Count) and (not Assigned(FList[Index])) do
      Inc(Index);

    if Index < FList.Count then
    begin
      HashRec := FList[Index];
      Result := HashRec^.Item;
    end else
      Result := nil
  finally
    Unlock;
  end;
end;

function THashedInterfaceList.GetAutoGrowFillFactor: Double;
begin
  Result := FAutoGrowFillFactor;
end;

function THashedInterfaceList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function THashedInterfaceList.GetCount: Integer;
begin
  Result := FCount;
end;

function THashedInterfaceList.GetItems(const Index: Integer): IKeyedItem;
var
  ListIndex : Integer;
  Left      : Integer;
  HashRec   : PHashRec;
begin
  Assert((Index >= 0) and (Index < FCount));
  Lock;
  try
    ListIndex := 0;
    Left := Index + 1;

    while Left > 0 do
    begin
      if Assigned(FList[ListIndex]) then
      begin
        HashRec := FList[ListIndex];
        while Assigned(HashRec) and (Left > 1) do
        begin
          HashRec := HashRec^.Next;
          Dec(Left);
        end;

        if Assigned(HashRec) and (Left = 1) then
        begin
          Result := HashRec^.Item;
          Break;
        end else
          Inc(ListIndex);
      end else
        Inc(ListIndex);
    end;
  finally
    Unlock;
  end;
end;

function THashedInterfaceList.GetItemsByKey(const Key: string): IKeyedItem;
var
  Index   : Integer;
  HashRec : PHashRec;
begin
  Lock;
  try
    Result := nil;

    Index := KeyHash(Key, FList.Capacity);
    HashRec := FList[Index];
    if Assigned(HashRec) then
    begin
      while Assigned(HashRec) do
      begin
        if CompareText(HashRec^.Item.Key, Key) = 0 then
        begin
          Result := HashRec^.Item;
          Break;
        end;

        HashRec := HashRec^.Next;
      end;
    end;
  finally
    Unlock;
  end;
end;

function THashedInterfaceList.GetKeys(const Index: Integer): string;
var
  Item  : IKeyedItem;
begin
  Assert((Index >= 0) and (Index < FCount));

  Item := GetItems(Index);
  Assert(Assigned(Item));
  Result := Item.Key;
end;

procedure THashedInterfaceList.Grow;
begin
  // This will automatically adjust the capacity to the next prime upwards
  SetCapacity(GetCapacity + 1);
end;

function THashedInterfaceList.IndexOf(const Key: string): Integer;
var
  Index   : Integer;
  HashRec : PHashRec;

  function CountPrevious(const Index: Integer): Integer;
  var
    ListIndex : Integer;
    HashRec   : PHashRec;
  begin
    Result := 0;
    for ListIndex := 0 to Index do
    begin
      HashRec := FList[ListIndex];
      while Assigned(HashRec) do
      begin
        Inc(Result);
        HashRec := HashRec^.Next;
      end;
    end;
  end;

begin
  Assert(Key <> '');

  Result := -1;
  Lock;
  try
    Index := KeyHash(Key, FList.Capacity);
    HashRec := FList[Index];

    if Assigned(HashRec) then
    begin
      if CompareText(HashRec^.Item.Key, Key) = 0 then
        Result := CountPrevious(Index-1)
      else begin
        while Assigned(HashRec) do
        begin
          if CompareText(HashRec^.Item.Key, Key) = 0 then
          begin
            Result := 0;
            while Assigned(HashRec^.Prev) do
            begin
              Inc(Result);
              HashRec := HashRec^.Prev;
            end;
            Result := Result + CountPrevious(Index-1);
            Break;
          end;

          HashRec := HashRec^.Next;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function THashedInterfaceList.IndexOf(const Item: IKeyedItem): Integer;
begin
  Result := IndexOf(Item.Key);
end;

procedure THashedInterfaceList.InsertIntoList(const Item: IKeyedItem;
  const AddSubscriber: Boolean);
var
  Index   : Integer;
  HashRec : PHashRec;
begin
  Assert(Assigned(Item));

  Lock;
  try
    Index := KeyHash(Item.Key, FList.Capacity);

    if Assigned(FList[Index]) then
    begin
      HashRec := FList[Index];

      while True do
      begin
        if CompareText(HashRec^.Item.Key, Item.Key) = 0 then
        begin
          HashRec^.Item.RemoveSubscriber(FSubscriber);
          HashRec^.Item := Item;
          Break;
        end else begin
          if Assigned(HashRec^.Next) then
            HashRec := HashRec^.Next
          else begin
            New(HashRec^.Next);
            HashRec^.Next^.Prev := HashRec;
            HashRec := HashRec^.Next;
            HashRec^.Item := Item;
            HashRec^.Next := nil;
            Inc(FCount);
            Break;
          end;
        end;
      end;
    end else begin
      New(HashRec);
      HashRec^.Item := Item;
      HashRec^.Next := nil;
      HashRec^.Prev := nil;
      FList[Index] := HashRec;
      Inc(FCount);
      Inc(FFill);
    end;

    if AddSubscriber then
      Item.AddSubscriber(FSubscriber);
    CheckAutoGrowReduce;
  finally
    Unlock;
  end;
end;

function THashedInterfaceList.Last: IKeyedItem;
var
  Index   : Integer;
  HashRec : PHashRec;
begin
  Lock;
  try
    Index := FList.Count-1;
    while (Index >= 0) and (not Assigned(FList[Index])) do
      Dec(Index);

    if Index < 0 then
      Result := nil
    else begin
      HashRec := FList[Index];
      while Assigned(HashRec^.Next) do
        HashRec := HashRec^.Next;
      Result := HashRec^.Item;
    end;
  finally
    Unlock;
  end;
end;

procedure THashedInterfaceList.Lock;
begin
  FLock.Acquire;
end;

function THashedInterfaceList.Next(const Item: IKeyedItem): IKeyedItem;
var
  Index   : Integer;
  HashRec : PHashRec;
begin
  Result := nil;

  if Assigned(Item) then
  begin
    Lock;
    try
      Index := KeyHash(Item.Key, FList.Capacity);
      HashRec := FList[Index];

      if Assigned(HashRec) then
      begin
        while Assigned(HashRec) do
        begin
          if CompareText(HashRec^.Item.Key, Item.Key) = 0 then
          begin
            HashRec := HashRec^.Next;
            if Assigned(HashRec) then
              Result := HashRec^.Item
            else begin
              Inc(Index);
              while Index < FList.Capacity do
              begin
                if Assigned(FList[Index]) then
                begin
                  HashRec := FList[Index];
                  Result := HashRec^.Item;
                  Break;
                end;

                Inc(Index);
              end;
            end;
            Break;
          end;

          HashRec := HashRec^.Next;
        end;
      end;
    finally
      Unlock;
    end;
  end;
end;

function THashedInterfaceList.Previous(const Item: IKeyedItem): IKeyedItem;
var
  Index   : Integer;
  HashRec : PHashRec;
begin
  Result := nil;

  if Assigned(Item) then
  begin
    Lock;
    try
      Index := KeyHash(Item.Key, FList.Capacity);
      HashRec := FList[Index];

      if Assigned(HashRec) then
      begin
        // The item found could be the first in the chain in that bucket,
        // in that case we must go towards the beginning and find the
        // previous one
        if CompareText(HashRec^.Item.Key, Item.Key) = 0 then
        begin
          Dec(Index);
          while Index >= 0 do
          begin
            HashRec := FList[Index];
            if Assigned(HashRec) then
            begin
              while Assigned(HashRec^.Next) do
                HashRec := HashRec^.Next;
              Result := HashRec^.Item;
              Break;
            end;

            Dec(Index);
          end;
        end else begin
          // It's not the first so try to locate it in the chain
          while Assigned(HashRec) do
          begin
            if CompareText(HashRec^.Item.Key, Item.Key) = 0 then
            begin
              // Was found in the chain, so return the previous one in the
              // same chain
              HashRec := HashRec^.Prev;
              Result := HashRec^.Item;
              Break;
            end;

            HashRec := HashRec^.Next;
          end;
        end;
      end;
    finally
      Unlock;
    end;
  end;
end;

procedure THashedInterfaceList.RecomputeStatistics;
var
  Index : Integer;
begin
  Lock;
  try
    FFill := 0;

    for Index := 0 to FList.Count-1 do
      if Assigned(FList[Index]) then
        Inc(FFill);
  finally
    Unlock;
  end;
end;

procedure THashedInterfaceList.SetAutoGrowFillFactor(
  const NewFillFactor: Double);
begin
  Assert((NewFillFactor > FAutoReduceFillFactor) and (NewFillFactor <= 1.0));

  if Abs(NewFillFactor - FAutoGrowFillFactor) > 1E-5 then
  begin
    FAutoGrowFillFactor := NewFillFactor;

    CheckAutoGrowReduce;
  end;
end;

procedure THashedInterfaceList.SetCapacity(const NewCapacity: Integer);
var
  NewList   : TList;
  Index     : Integer;
  HashRec   : PHashRec;
  NewIndex  : Integer;
begin
  Lock;
  try
    NewList := TList.Create;
    try
      // Create new list with the specified capacity
      NewList.Capacity := AdjustCapacity(NewCapacity);
      if NewList.Capacity = FList.Capacity then
        Exit;

      for Index := 0 to NewList.Capacity-1 do
        NewList.Add(nil);

      // Re-insert all items into this new list
      for Index := 0 to FList.Capacity-1 do
      begin
        while Assigned(FList[Index]) do
        begin
          HashRec := FList[Index];
          FList[Index] := HashRec^.Next;

          NewIndex := KeyHash(HashRec^.Item.Key, NewList.Capacity);
          HashRec^.Next := NewList[NewIndex];
          if Assigned(HashRec^.Next) then
            HashRec^.Next^.Prev := HashRec;
          HashRec^.Prev := nil;
          NewList[NewIndex] := HashRec;
        end;
      end;

      // Replace current bucket list with new one
      FList.Free;
      FList := NewList;
      RecomputeStatistics;
    except
      NewList.Free;
      raise;
    end;
  finally
    Unlock;
  end;
end;

procedure THashedInterfaceList.Unlock;
begin
  FLock.Release;
end;

function THashedInterfaceList.GetAutoReduceFillFactor: Double;
begin
  Result := FAutoReduceFillFactor;
end;

procedure THashedInterfaceList.Reduce;
begin
  SetCapacity(GetCapacity div 3);
end;

procedure THashedInterfaceList.SetAutoReduceFillFactor(
  const NewFillFactor: Double);
begin
  Assert((NewFillFactor < FAutoReduceFillFactor) and (NewFillFactor >= 0.0));

  if Abs(NewFillFactor - FAutoReduceFillFactor) > 1E-5 then
  begin
    FAutoReduceFillFactor := NewFillFactor;

    CheckAutoGrowReduce;
  end;
end;

function THashedInterfaceList.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

{ TItemIterator }

constructor TItemIterator.Create(const List: IUnknown;
  const Reverse: Boolean; const SortFunc: TItemSortFunc;
  const FilterFunc: TItemFilterFunc);
begin
  inherited Create;

  FList := TInterfaceList.Create as IInterfaceList;

  Snapshot(List, FilterFunc);
  if Assigned(SortFunc) then
    Sort(SortFunc);

  if Reverse then
  begin
    FIndex := FList.Count;
    FDirection := -1;
  end else begin
    FIndex := -1;
    FDirection := +1;
  end;
end;

function TItemIterator.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TItemIterator.HasNext: Boolean;
begin
  if FDirection = +1 then
    Result := FIndex < FList.Count-1
  else
    Result := FIndex > 0;
end;

function TItemIterator.Next: IUnknown;
begin
  if FDirection = +1 then
  begin
    if FIndex < FList.Count-1 then
    begin
      Inc(FIndex);
      Result := FList[FIndex];
    end else
      Result := nil;
  end else begin
    if FIndex > 0 then
    begin
      Dec(FIndex);
      Result := FList[FIndex];
    end else
      Result := nil;
  end;
end;

procedure TItemIterator.Snapshot(const List: IUnknown;
  const FilterFunc: TItemFilterFunc);
var
  InterfaceList       : IInterfaceList;
  HashedInterfaceList : IHashedInterfaceList;
  KeyedInterfaceList  : IKeyedInterfaceList;
  Index               : Integer;
begin
  if List.QueryInterface(IInterfaceList, InterfaceList) = S_OK then
  begin
    FList.Capacity := InterfaceList.Count;
  end else if List.QueryInterface(IHashedInterfaceList, HashedInterfaceList) = S_OK then
    HashedInterfaceList.CreateSnapshot(InterfaceList)
  else if List.QueryInterface(IKeyedInterfaceList, KeyedInterfaceList) = S_OK then
    KeyedInterfaceList.CreateSnapshot(InterfaceList)
  else
    raise Exception.Create('Don''t know how to handle this list type');

  if Assigned(FilterFunc) then
  begin
    for Index := 0 to InterfaceList.Count-1 do
      if FilterFunc(InterfaceList[Index]) then
        FList.Add(InterfaceList[Index]);
  end else begin
    for Index := 0 to InterfaceList.Count-1 do
      FList.Add(InterfaceList[Index]);
  end;
end;

procedure TItemIterator.Sort(const SortFunc: TItemSortFunc);

  procedure QuickSort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while SortFunc(FList[I], FList[P]) < 0 do Inc(I);
        while SortFunc(FList[J], FList[P]) > 0 do Dec(J);
        if I <= J then
        begin
          FList.Exchange(I, J);
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  QuickSort(0, FList.Count-1);
end;

{ TItemSubscriber }

procedure TItemSubscriber.AfterKeyChange(const Item: IKeyedItem);
begin
  // Do nothing
end;

procedure TItemSubscriber.BeforeKeyChange(const Item: IKeyedItem);
begin
  // Do nothing
end;

end.
