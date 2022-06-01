{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation of a thread communications queue.

  See also:
    TlvkThreadCommunicationsQueue
}
unit lvkThreadCommunicationsQueue;

// $Author: Lasse V. Karlsen $
// $Revision: 12 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkThreadCommunicationsQueue.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFNDEF DELPHI6UP}
  Forms,
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, SyncObjs, lvkComponents;

const
  CM_ITEM_ADDED       = WM_USER + 1;
  CM_ITEM_REMOVED     = WM_USER + 2;
  CM_QUEUE_EMPTY      = WM_USER + 3;

  DEFAULT_QUEUE_SIZE  = 0;

type
{ Description:
    Pointer to ^TQueueNode. Internally used in TQueueNode and to hold
    references inside TlvkThreadCommunicationsQueue
  See also:
    TQueueNode
}
  PQueueNode = ^TQueueNode;

{ Description:
    Holds one item in the queue as well as a reference to the next item
    in the queue.
  See also:
    PQueueNode
}
  TQueueNode = record
    // Reference to Item
    Item      : IUnknown;
    // Next node in the queue
    Next      : PQueueNode;
    // Previous node in the queue
    Prev      : PQueueNode;
    // Used for a prioritized queue
    Priority  : Integer;
  end;

  { Description:
      This is the base interface that all thread communication queues should
      support. Other classes and components in this package will use this
      interface.
  }
  IThreadCommunicationsQueue = interface
    ['{DB21DD01-9D18-11D5-B27F-0004761A6377}']

    { Description:
        This method pushes a new item into the queue. If the queue is full,
        the thread calling Push will be suspended until one of the following
        conditions are true:
         * The terminationevent is signaled
         * The wait times out
         * The queue gains space for at least one more item
      Parameters:
        Item - The new item to push into the queue. This value can be nil,
          for instance to signal end of data stream or similar. It's up to
          the thread that pops off the value to determine what it means.
        PushToFront - If this parameter is True (or you have called PushFront),
          the new item will be pushed into the front of the queue, otherwise
          it will be pushed into the end of the queue.
        Timeout - How long to wait for the queue to gain space for the item,
          in milliseconds. You can specify INFINITE if you wish the thread to
          not time out.
        TerminationEvent - A handle to a event synchronization object that
          should become signaled when the thread is terminating. You use this
          to make sure that a terminating thread doesn't stay suspended due
          to a wait for pushing data into the queue.
      Returns:
        True if the item could be pushed into the queue, False if the Push
        operation timed out or the termination event was signaled.
      See also:
        Push, PushFront, PushBack, Pop
    }
    function Push(const Item: IUnknown; const PushToFront: Boolean=True;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;

    // <ALIAS Push@IUnknown@Boolean@LongWord@THandle>
    function PushFront(const Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;
    // <ALIAS Push@IUnknown@Boolean@LongWord@THandle>
    function PushBack(const Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;

    { Description:
        This method locks the queue. When locked, only the thread that locked it
        can access the queue, all other threads that try to use it will be
        suspended while it is locked. Typically you will lock a queue when
        you want to push data into it in a specific sequence without
        worrying if a thread might manage to pop off an item in mid-sequence

        Note: The programmer must make sure that the queue is unlocked after
          the code has finished accessing the queue.
      See also:
        Unlock
    }
    procedure Lock;

    { Description:
        This method unlocks the queue after a call to Lock. The queue then
        becomes available again for other threads.
      See also:
        Lock
    }
    procedure Unlock;

    { Description:
        This method pops off a single item from the queue. If the queue is
        empty, the thread calling Pop will be suspended until one of the
        following conditions are true:
         * The terminationevent is signaled
         * The wait times out
         * The queue gains at least one more item
      Parameters:
        Item - Upon exit of the Pop method, and the function returns True,
          Item holds the next value popped off from the queue. This value can
          be nil, for instance to signal end of data stream or similar.
        PopFromFront - If this parameter is True (or you have called PopFront),
          the item will be popped off from the front of the queue, otherwise
          it will be popped off from the end of the queue.
        Timeout - How long to wait for the queue to gain another item,
          in milliseconds. You can specify INFINITE if you wish the thread to
          not time out.
        TerminationEvent - A handle to a event synchronization object that
          should become signaled when the thread is terminating. You use this
          to make sure that a terminating thread doesn't stay suspended due
          to a wait for popping data off of the queue.
      Returns:
        True if an item could be popped off the queue, False if the wait
        timed out or the termination event was signaled.
      See also:
        Pop, PopFront, PopBack, Push
    }
    function Pop(out Item: IUnknown; const PopFromFront: Boolean=False;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;

    // <ALIAS Pop@IUnknown@Boolean@LongWord@THandle>
    function PopFront(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;
    // <ALIAS Pop@IUnknown@Boolean@LongWord@THandle>
    function PopBack(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;
  end;

  { Description
      This is the base interface used for the prioritized communications queue.
  }
  IPrioritizedThreadCommunicationsQueue = interface(IThreadCommunicationsQueue)
    ['{2DF630F8-4171-43A4-BBAC-D935781F1B40}']

    { Description:
        This method pushes an item into the queue with the given priority.
        The priority is used to determine the order in which items are popped
        off from the queue. The item with the highest priority (priority 1 is
        considered to be a higher priority than priority 2) will be popped
        off first, then the next will follow.

        If the queue is full, the thread calling Push will be suspended until
        one of the following conditions are true:
         * The terminationevent is signaled
         * The wait times out
         * The queue gains space for at least one more item
      Parameters:
        Item - The item to push into the queue.
        Priority - The priority to assign to the item.
        Timeout - How long to wait for the queue to gain space for the item,
          in milliseconds. You can specify INFINITE if you wish the thread to
          not time out.
        TerminationEvent - A handle to a event synchronization object that
          should become signaled when the thread is terminating. You use this
          to make sure that a terminating thread doesn't stay suspended due
          to a wait for pushing data into the queue.
      See also:
        IThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle,
        IThreadCommunicationsQueue.PushFront@IUnknown@LongWord@THandle,
        IThreadCommunicationsQueue.PushBack@IUnknown@LongWord@THandle,
    }
    function PrioritizedPush(const Item: IUnknown; const Priority: Integer;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;
  end;

  { Description:
      This is a common class ancestor for all thread communication queue
      components and classes. Other components and classes in this package
      will rely on this class.
  }
  TlvkCustomCommunicationsQueue = class(TlvkComponent,
    IThreadCommunicationsQueue)
  public
    // <ALIAS IThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function Push(const Item: IUnknown; const PushToFront: Boolean=True;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=
        INVALID_HANDLE_VALUE): Boolean; virtual; abstract;
    // <ALIAS IThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function PushFront(const Item: IUnknown;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=
        INVALID_HANDLE_VALUE): Boolean; virtual; abstract;
    // <ALIAS IThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function PushBack(const Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=
        INVALID_HANDLE_VALUE): Boolean; virtual; abstract;
    // <ALIAS IThreadCommunicationsQueue.Lock>
    procedure Lock; virtual; abstract;
    // <ALIAS IThreadCommunicationsQueue.Unlock>
    procedure Unlock; virtual; abstract;
    // <ALIAS IThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function Pop(out Item: IUnknown; const PopFromFront: Boolean=False;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=
        INVALID_HANDLE_VALUE): Boolean; virtual; abstract;
    // <ALIAS IThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function PopFront(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=
        INVALID_HANDLE_VALUE): Boolean; virtual; abstract;
    // <ALIAS IThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function PopBack(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=
        INVALID_HANDLE_VALUE): Boolean; virtual; abstract;
  end;

{ Description:
    This class implements a simple thread communications queue. Threads will
    use the queue to communicate between themselves. Threads can push items
    of data (in the form of classes that implement the IUnknown interface)
    into one end of the queue and threads can pop off items from the
    other end.

    If a thread tries to push a new item into a full queue, then the
    thread will be suspended until there's room for another item in the
    queue, or the thread is terminated, whichever comes first.

    If a thread tries to pop off a item from the queue and the queue is empty,
    then the thread will be suspended until more items get pushed into the
    queue, or the thread is terminated, whichever comes first.
}
  TlvkThreadCommunicationsQueue = class(TlvkCustomCommunicationsQueue,
    IPrioritizedThreadCommunicationsQueue)
  private
    // Holds a reference to the first item in the queue (used during Pop)
    FHead           : PQueueNode;
    // Holds a reference to the last item in the queue (used during Push)
    FTail           : PQueueNode;
    // Holds the number of items in the queue
    FCount          : Integer;
    // Holds the maximum number of items in the queue
    FQueueSize      : Integer;
    // Critical section to safeguard against race conditions
    FLock           : TCriticalSection;
    // Event used to signal an empty queue (event will be signaled when queue
    // is not empty)
    FEmptyEvent     : TEvent;
    // Event used to signal a full queue (event will be signaled when the queue
    // is not full)
    FFullEvent      : TEvent;
    // Handle for events
    FHandle         : HWND;
    // Event used when a new item is added
    FItemAdded      : TNotifyEvent;
    // Event used when an item is removed
    FItemRemoved    : TNotifyEvent;
    // Event used when queue becomes empty
    FQueueEmpty     : TNotifyEvent;

    procedure WindProc(var Message: TMessage);

  protected
    { Description:
        This method is internally used to set the size of the queue. The size
        cannot be set lower than the number of items in the queue.
      Parameters:
        Value - The new number of maximum items in the queue. If 0, then no
          limit is imposed on the size of the queue.
      See also:
        QueueSize
    }
    procedure SetQueueSize(const Value: Integer);

    { Description:
        This method is internally used to update the status of the two events
        that are used to signal full and empty queue. The current status of
        the queue data is used to signal or reset each event.

        Note: This method does no locking of the critical section by itself
          so it must be called from within a locked region.
    }
    procedure UpdateEvents;

  public
    { Description:
        This constructor creates and initializes the communications object
        and the internal queue. All the critical sections and events are
        created here.
      Parameters:
        AOwner - Owner of component.
      See also:
        Destroy
    }
    constructor Create(AOwner: TComponent); override;

    { Description:
        This destructor clears out the queue and destroys the communications
        object, the internal queue and all synchronization objects related
        to the queue.
      See also:
        Create
    }
    destructor Destroy; override;

    // <ALIAS IThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function Push(const Item: IUnknown; const PushToFront: Boolean=True;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS IThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function PushFront(const Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS IThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function PushBack(const Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;

    // <ALIAS IPrioritizedThreadCommunicationsQueue.PrioritizedPush@IUnknown@Integer@LongWord@THandle>
    function PrioritizedPush(const Item: IUnknown; const Priority: Integer;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; virtual;

    // <ALIAS IThreadCommunicationsQueue.Lock>
    procedure Lock; override;
    // <ALIAS IThreadCommunicationsQueue.Unlock>
    procedure Unlock; override;

    // <ALIAS IThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function Pop(out Item: IUnknown; const PopFromFront: Boolean=False;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS IThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function PopFront(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS IThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function PopBack(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;

    { Description:
        This method will clear the queue.

        Note: If you simultaneously run a separate thread that keeps pushing new
          items into the queue, then Clear will clear out all items currently
          in the queue and then exit. The other thread will then resume
          pushing items into the queue.
      See also:
        Push, Pop
    }
    procedure Clear;

  published
    { Description:
        This property holds the size of the queue. Please see Push and Pop
        for descriptions on how this affects the queue and its operations.

        Note: If you set QueueSize to 0, no maximum size is imposed on the
          queue and it may grow limited only by available memory.
      See also:
        Push, Pop
    }
    property QueueSize: Integer read FQueueSize write SetQueueSize
      default DEFAULT_QUEUE_SIZE;

    { Description:
        This event will be called whenever an item is pushed into the queue.
        For each item pushed into the queue, this event will be called once.
        The event handler code will have to manually pop the item out of the
        queue on its own.

        Note: The event handler will be called from the context of the thread
          that created the queue component instance. If you dropped this
          component onto a form, the event handler will be called from the
          context of the main application thread. In that case it is safe to
          access VCL components.
        Note: If you have other threads popping items from the same thread,
          please note that the item that was added may have been popped off
          the queue before the event handler gets a chance to access it. You
          should therefore call the Pop method and use the result to verify
          that you actually got an item.
      See also:
        ItemRemoved
    }
    property ItemAdded: TNotifyEvent read FItemAdded write FItemAdded;

    { Description:
        This event will be called whenever an item is popped off the queue.
        For each item popped off the queue, this event will be called once.

        Note: The event handler will be called from the context of the thread
          that created the queue component instance. If you dropped this
          component onto a form, the event handler will be called from the
          context of the main application thread. In that case it is safe to
          access VCL components.
      See also:
        ItemAdded
    }
    property ItemRemoved: TNotifyEvent read FItemRemoved write FItemRemoved;

    { Description:
        This event handler will be called whenever the last item is popped
        off the queue. Note that depending on how the threads are scheduled,
        if you have one thread pushing data into the queue, and one thread
        popping data off from the queue, this event handler will possibly
        be called several times.
    }
    property OnQueueEmpty: TNotifyEvent read FQueueEmpty write FQueueEmpty;
  end;

  { Description:
      This property can be used as a reference "holder" to a real queue. You
      can drop one of these on a data module, and drop a real queue on a form,
      and then route the proxy to this real queue. This way you don't have
      to tie the datamodule queue to a specific form.
  }
  TlvkThreadCommunicationsQueueProxy = class(TlvkCustomCommunicationsQueue)
  private
    FQueue            : TlvkThreadCommunicationsQueue;
    FIgnoreNoInstance : Boolean;

    function GetItemAdded: TNotifyEvent;
    function GetItemRemoved: TNotifyEvent;
    function GetQueueSize: Integer;
    procedure ErrorNoInstance;
    procedure SetItemAdded(const Value: TNotifyEvent);
    procedure SetItemRemoved(const Value: TNotifyEvent);

  protected
    // <ALIAS TlvkThreadCommunicationsQueue.SetQueueSize@Integer>
    procedure SetQueueSize(const Value: Integer);

  public
    // <ALIAS TlvkThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function Push(const Item: IUnknown; const PushToFront: Boolean=True;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS TlvkThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function PushFront(const Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS TlvkThreadCommunicationsQueue.Push@IUnknown@Boolean@LongWord@THandle>
    function PushBack(const Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS IPrioritizedThreadCommunicationsQueue.PrioritizedPush@IUnknown@Integer@LongWord@THandle>
    function PrioritizedPush(const Item: IUnknown; const Priority: Integer;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; virtual;
    // <ALIAS TlvkThreadCommunicationsQueue.Lock>
    procedure Lock; override;
    // <ALIAS TlvkThreadCommunicationsQueue.Unlock>
    procedure Unlock; override;
    // <ALIAS TlvkThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function Pop(out Item: IUnknown; const PopFromFront: Boolean=False;
      const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS TlvkThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function PopFront(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS TlvkThreadCommunicationsQueue.Pop@IUnknown@Boolean@LongWord@THandle>
    function PopBack(out Item: IUnknown; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; override;
    // <ALIAS TlvkThreadCommunicationsQueue.Clear>
    procedure Clear;

  published
    // <ALIAS TlvkThreadCommunicationsQueue.QueueSize>
    property QueueSize: Integer read GetQueueSize write SetQueueSize;
    // <ALIAS TlvkThreadCommunicationsQueue.ItemAdded>
    property ItemAdded: TNotifyEvent read GetItemAdded write SetItemAdded;
    // <ALIAS TlvkThreadCommunicationsQueue.ItemRemoved>
    property ItemRemoved: TNotifyEvent read GetItemRemoved write SetItemRemoved;

    { Description:
        This property is used to set which queue instance this proxy refers to.
    }
    property Queue: TlvkThreadCommunicationsQueue read FQueue write FQueue;
    { Description:
        Set this property to True to have the component ignore all method
        calls and property readings/settings while the component is not
        referring to a queue instance. If this property is set to False,
        any such access will result in an exception being raised.
    }
    property IgnoreNoInstance: Boolean read FIgnoreNoInstance
      write FIgnoreNoInstance;
  end;

  { Description:
      This exception class is used for all exceptions raised in one of the
      communications queue components.
  }
  ElvkCommunicationsQueue = class(Exception);

  { Description:
      This exception class is used in the proxy queue component, whenever a
      method call is made or property is read/written that needs a queue
      instance, and the component does not refer to a queue instance.
  }
  ElvkThreadCommunicationsQueueProxy = class(ElvkCommunicationsQueue);

implementation

const
  PRIORITY_FRONT  = Low(Integer);
  PRIORITY_BACK   = PRIORITY_FRONT + 1;

{ TlvkThreadCommunicationsQueue }

procedure TlvkThreadCommunicationsQueue.Clear;
var
  Dummy : IUnknown;
begin
  FLock.Acquire;
  try
    while Pop(Dummy, False, 0, INVALID_HANDLE_VALUE) do
      ;
  finally
    FLock.Release;
  end;
end;

constructor TlvkThreadCommunicationsQueue.Create(AOwner: TComponent);
begin
  inherited;

  FHead := nil;
  FTail := nil;
  FCount := 0;
  FQueueSize := DEFAULT_QUEUE_SIZE;

  FLock := TCriticalSection.Create;
  FFullEvent := TEvent.Create(nil, False, True, '');
  FEmptyEvent := TEvent.Create(nil, False, False, '');

  FHandle := AllocateHWnd(WindProc);
end;

destructor TlvkThreadCommunicationsQueue.Destroy;
begin
  DeallocateHWnd(FHandle);

  Clear;
  FFullEvent.Free;
  FEmptyEvent.Free;
  FLock.Free;

  inherited;
end;

procedure TlvkThreadCommunicationsQueue.Lock;
begin
  FLock.Acquire;
end;

function TlvkThreadCommunicationsQueue.Pop(out Item: IUnknown;
  const PopFromFront: Boolean; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
var
  Temp    : PQueueNode;
  Handles : array[0..1] of THandle;
  rc      : Integer;
  wr      : TWaitResult;
begin
  Result := False;
  Item := nil;

  repeat
    if TerminationEvent <> INVALID_HANDLE_VALUE then
    begin
      Handles[0] := TerminationEvent;
      Handles[1] := FEmptyEvent.Handle;
      rc := WaitForMultipleObjects(2, @Handles, False, Timeout);

      if (rc = WAIT_TIMEOUT) or (rc = WAIT_OBJECT_0) then
        Exit
      else if rc <> WAIT_OBJECT_0+1 then
        RaiseLastWin32Error;
    end else begin
      wr := FEmptyEvent.WaitFor(Timeout);
      if wr = wrTimeout then
        Exit
      else if wr <> wrSignaled then
        RaiseLastWin32Error;
    end;

    Lock;
    try
      if Assigned(FHead) then
        Break
      else
        UpdateEvents;
    finally
      Unlock;
    end;
  until False;

  Lock;
  try
    if PopFromFront then
    begin
      if Assigned(FHead) then
      begin
        Item := FHead.Item;
        Temp := FHead;
        FHead := FHead^.Next;
        Dispose(Temp);

        if FHead = nil then
          FTail := nil
        else
          FHead^.Prev := nil;
      end else
        raise Exception.Create('No more items in queue');
    end else begin
      if Assigned(FTail) then
      begin
        Item := FTail.Item;
        Temp := FTail;
        FTail := FTail^.Prev;
        Dispose(Temp);

        if FTail = nil then
          FHead := nil
        else
          FTail^.Next := nil;
      end else
        raise Exception.Create('No more items in queue');
    end;

    Dec(FCount);
    UpdateEvents;
    Result := True;

    if Assigned(FItemRemoved) then
      PostMessage(FHandle, CM_ITEM_REMOVED, 0, 0);
    if FCount = 0 then
      PostMessage(FHandle, CM_QUEUE_EMPTY, 0, 0);
  finally
    Unlock;
  end;
end;

function TlvkThreadCommunicationsQueue.PopBack(out Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  Result := Pop(Item, False, Timeout, TerminationEvent);
end;

function TlvkThreadCommunicationsQueue.PopFront(out Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  Result := Pop(Item, True, Timeout, TerminationEvent);
end;

function TlvkThreadCommunicationsQueue.PrioritizedPush(
  const Item: IUnknown; const Priority: Integer;
  const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
var
  Handles : array[0..1] of THandle;
  rc      : Integer;
  wr      : TWaitResult;

  procedure AddToEmpty(const Item: IUnknown; const Priority: Integer);
  begin
    New(FHead);
    FHead^.Item := Item;
    FHead^.Next := nil;
    FHead^.Prev := nil;

    if (Priority = PRIORITY_FRONT) or (Priority = PRIORITY_BACK) then
      FHead^.Priority := 0
    else
      FHead^.Priority := Priority;

    FTail := FHead;
  end;

  procedure AddToFront(const Item: IUnknown; const Priority: Integer);
  var
    NewNode : PQueueNode;
  begin
    if Assigned(FHead) then
    begin
      New(NewNode);
      NewNode^.Item := Item;
      if (Priority = PRIORITY_FRONT) then
        NewNode^.Priority := FHead^.Priority - 1
      else
        NewNode^.Priority := Priority;
      NewNode^.Next := FHead;
      NewNode^.Prev := nil;

      FHead^.Prev := NewNode;
      FHead := NewNode;
    end else
      AddToEmpty(Item, Priority)
  end;

  procedure AddToBack(const Item: IUnknown; const Priority: Integer);
  var
    NewNode : PQueueNode;
  begin
    if Assigned(FTail) then
    begin
      New(NewNode);
      NewNode^.Item := Item;
      if (Priority = PRIORITY_BACK) then
        NewNode^.Priority := FTail^.Priority + 1
      else
        NewNode^.Priority := Priority;
      NewNode^.Next := nil;
      NewNode^.Prev := FTail;

      FTail^.Next := NewNode;
      FTail := NewNode;
    end else
      AddToEmpty(Item, Priority);
  end;

  procedure AddBefore(const Node: PQueueNode; const Item: IUnknown;
    const Priority: Integer);
  var
    NewNode : PQueueNode;
  begin
    if Node^.Prev = nil then
      AddToFront(Item, Priority)
    else begin
      New(NewNode);
      NewNode^.Item := Item;
      NewNode^.Priority := Priority;
      NewNode^.Prev := Node^.Prev;
      NewNode^.Next := Node;

      NewNode^.Prev^.Next := NewNode;
      NewNode^.Next^.Prev := NewNode;
    end;
  end;

  procedure AddPrioritized(const Item: IUnknown; const Priority: Integer);
  var
    Node  : PQueueNode;
    Added : Boolean;
  begin
    Node := FHead;
    Added := False;

    while Assigned(Node) and (not Added) do
    begin
      if Priority < Node^.Priority then
      begin
        AddBefore(Node, Item, Priority);
        Added := True;
      end else
        Node := Node^.Next;
    end;

    if not Added then
      AddToBack(Item, Priority);
  end;

begin
  Result := False;

  if TerminationEvent <> INVALID_HANDLE_VALUE then
  begin
    Handles[0] := TerminationEvent;
    Handles[1] := FFullEvent.Handle;
    rc := WaitForMultipleObjects(2, @Handles, False, Timeout);

    if (rc = WAIT_TIMEOUT) or (rc = WAIT_OBJECT_0) THEN
      Exit
    else if rc <> WAIT_OBJECT_0+1 then
      RaiseLastWin32Error;
  end else begin
    wr := FFullEvent.WaitFor(Timeout);
    if wr = wrTimeout then
      Exit
    else if wr <> wrSignaled then
      RaiseLastWin32Error;
  end;

  Lock;
  try
    if Priority = PRIORITY_FRONT then
      AddToFront(Item, Priority)
    else if Priority = PRIORITY_BACK then
      AddToBack(Item, Priority)
    else
      AddPrioritized(Item, Priority);

    Inc(FCount);
    UpdateEvents;
    Result := True;

    if Assigned(FItemAdded) then
      PostMessage(FHandle, CM_ITEM_ADDED, 0, 0);
  finally
    Unlock;
  end;
end;

function TlvkThreadCommunicationsQueue.Push(const Item: IUnknown;
  const PushToFront: Boolean; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
const
  Priorities  : array[Boolean] of Integer = (PRIORITY_BACK, PRIORITY_FRONT);
begin
  Result := PrioritizedPush(Item, Priorities[PushToFront], Timeout,
    TerminationEvent);
end;

function TlvkThreadCommunicationsQueue.PushBack(const Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  Result := PrioritizedPush(Item, PRIORITY_BACK, Timeout, TerminationEvent);
end;

function TlvkThreadCommunicationsQueue.PushFront(const Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  Result := PrioritizedPush(Item, PRIORITY_FRONT, Timeout, TerminationEvent);
end;

procedure TlvkThreadCommunicationsQueue.SetQueueSize(const Value: Integer);
begin
  FLock.Acquire;
  try
    if Value < 0 then
      raise Exception.Create('The size of the queue must be 0 or a positive ' +
        'value');
    if (Value > 0) and (Value < FCount) then
      raise Exception.Create('More than that number of items in the queue');
    FQueueSize := Value;
  finally
    FLock.Release;
  end;
end;

procedure TlvkThreadCommunicationsQueue.Unlock;
begin
  FLock.Release;
end;

procedure TlvkThreadCommunicationsQueue.UpdateEvents;
begin
  if FQueueSize <> 0 then
  begin
    if FCount >= FQueueSize then
      FFullEvent.ResetEvent
    else
      FFullEvent.SetEvent;
  end else
    FFullEvent.SetEvent;

  if FCount = 0 then
    FEmptyEvent.ResetEvent
  else
    FEmptyEvent.SetEvent;
end;

procedure TlvkThreadCommunicationsQueue.WindProc(var Message: TMessage);
begin
  case Message.Msg of
    CM_ITEM_ADDED:
      if Assigned(FItemAdded) then
        FItemAdded(Self);

    CM_ITEM_REMOVED:
      if Assigned(FItemRemoved) then
        FItemRemoved(Self);

    CM_QUEUE_EMPTY:
      if Assigned(FQueueEmpty) then
        FQueueEmpty(Self);
  else
    DefaultHandler(Message);
  end;
end;

{ TlvkThreadCommunicationsQueueProxy }

procedure TlvkThreadCommunicationsQueueProxy.Clear;
begin
  if Assigned(FQueue) then
    FQueue.Clear
  else
    ErrorNoInstance;
end;

procedure TlvkThreadCommunicationsQueueProxy.ErrorNoInstance;
begin
  if ([csDesigning, csLoading] * ComponentState = []) and
    (not FIgnoreNoInstance) then
  begin
    raise ElvkThreadCommunicationsQueueProxy.Create('Proxy does not refer ' +
      'to a queue instance');
  end;
end;

function TlvkThreadCommunicationsQueueProxy.GetItemAdded: TNotifyEvent;
begin
  if Assigned(FQueue) then
    Result := FQueue.FItemAdded
  else begin
    Result := nil;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.GetItemRemoved: TNotifyEvent;
begin
  if Assigned(FQueue) then
    Result := FQueue.FItemRemoved
  else begin
    Result := nil;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.GetQueueSize: Integer;
begin
  if Assigned(FQueue) then
    Result := FQueue.QueueSize
  else begin
    Result := 0;
    ErrorNoInstance;
  end;
end;

procedure TlvkThreadCommunicationsQueueProxy.Lock;
begin
  if Assigned(FQueue) then
    FQueue.Lock
  else
    ErrorNoInstance;
end;

function TlvkThreadCommunicationsQueueProxy.Pop(out Item: IUnknown;
  const PopFromFront: Boolean; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
begin
  if Assigned(FQueue) then
    Result := FQueue.Pop(Item, PopFromFront, Timeout, TerminationEvent)
  else begin
    Result := False;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.PopBack(out Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  if Assigned(FQueue) then
    Result := FQueue.PopBack(Item, Timeout, TerminationEvent)
  else begin
    Result := False;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.PopFront(out Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  if Assigned(FQueue) then
    Result := FQueue.PopFront(Item, Timeout, TerminationEvent)
  else begin
    Result := False;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.PrioritizedPush(
  const Item: IUnknown; const Priority: Integer; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
begin
  if Assigned(FQueue) then
    Result := FQueue.PrioritizedPush(Item, Priority, Timeout, TerminationEvent)
  else begin
    Result := False;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.Push(const Item: IUnknown;
  const PushToFront: Boolean; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
begin
  if Assigned(FQueue) then
    Result := FQueue.Push(Item, PushToFront, Timeout, TerminationEvent)
  else begin
    Result := False;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.PushBack(
  const Item: IUnknown; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
begin
  if Assigned(FQueue) then
    Result := FQueue.PushBack(Item, Timeout, TerminationEvent)
  else begin
    Result := False;
    ErrorNoInstance;
  end;
end;

function TlvkThreadCommunicationsQueueProxy.PushFront(
  const Item: IUnknown; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
begin
  if Assigned(FQueue) then
    Result := FQueue.PushFront(Item, Timeout, TerminationEvent)
  else begin
    Result := False;
    ErrorNoInstance;
  end;
end;

procedure TlvkThreadCommunicationsQueueProxy.SetItemAdded(
  const Value: TNotifyEvent);
begin
  if Assigned(FQueue) then
    FQueue.FItemAdded := Value
  else
    ErrorNoInstance;
end;

procedure TlvkThreadCommunicationsQueueProxy.SetItemRemoved(
  const Value: TNotifyEvent);
begin
  if Assigned(FQueue) then
    FQueue.FItemRemoved := Value
  else
    ErrorNoInstance;
end;

procedure TlvkThreadCommunicationsQueueProxy.SetQueueSize(
  const Value: Integer);
begin
  if Assigned(FQueue) then
    FQueue.QueueSize := Value
  else
    ErrorNoInstance;
end;

procedure TlvkThreadCommunicationsQueueProxy.Unlock;
begin
  if Assigned(FQueue) then
    FQueue.Unlock
  else
    ErrorNoInstance;
end;

end.
