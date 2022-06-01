{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains components for different synchronization objects.
}
unit lvkSyncObjs;

// $Author: Lasse V. Karlsen $
// $Revision: 10 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSyncObjs.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, SyncObjs, lvkComponents;

type
  { Description:
      This interface is implemented by objects that can be waited upon.
      Basically it just returns a handle to the waitable object. You can use
      this handle with WaitForSingleObject/WaitForMultipleObjects, or you can
      pass the interface to lvkWaitFor.
    See also:
      lvkWaitFor
  }
  IWaitableObject = interface
    ['{D897BB19-04B7-4977-92CD-CB2A06056A33}']

    // <COMBINE Handle>
    function GetHandle: THandle;
    { Description:
        Returns the handle to the waitable object. Useful with
        WaitForSingleObject/WaitForMultipleObjects.
    }
    property Handle: THandle read GetHandle;
  end;

{ Description:
    This component is the abstract base for all the synchronization objects
    in this unit. It implements the code related to activating and deactivating
    the component.
}
  TlvkSyncObj = class(TlvkComponent, IUnknown)
  private
    // Is the component active or not
    FActive       : Boolean;
    // DId we load an active component (use by Loaded)
    FLoadedActive : Boolean;

    // Internal method used for setting the Active property
    procedure SetActive(const Value: Boolean);

  protected
    { Description:
        This method is called when a synchronization object is activated,
        and is used to create the internal synchronization object for the
        component.
      See also:
        TlvkSyncObj.DestroySyncObj, TlvkSyncObj.Active
    }
    procedure CreateSyncObj; virtual; abstract;

    { Description:
        This methos is called when a synchronization object is deactivated,
        and is used to destroy the internal synchronization object for the
        component.
      See also:
        TlvkSyncObj.CreateSyncObj, TlvkSyncObj.Active
    }
    procedure DestroySyncObj; virtual; abstract;

    { Description:
        This method is called when the component (and the form it is on) has
        been completely loaded. It's used to set the component Active if it
        was streamed out in an active state.
      See also:
        TlvkSyncObj.Active
    }
    procedure Loaded; override;

    { Description:
        This method will make a check if the component is active or not. If the
        component is active, the method simply returns. If the component is
        not active, an exception is raised.
      See also:
        RequireDeactivated
    }
    procedure RequireActivated;

    { Description:
        This method will make a check if the component is active or not. If the
        component is active, an exception is raised. If the component is not
        active, the method simply returns.
      See also:
        RequireActivated
    }
    procedure RequireDeactivated;

  public
    { Description:
        This constructor creates and initializes the synchronization object.
      See also:
        TlvkSyncObj.Destroy
    }
    constructor Create(AOwner: TComponent); override;

    { Description:
        This destructor deactivates and destroys the synchronization object.
      See also:
        Active, Create
    }
    destructor Destroy; override;

  published
    { Description:
        This property is used to activate and deactivate the synchronization
        object. When the object is activated, the internal Win32 synchronization
        object is created and initialized. When the object is deactivated,
        the internal Win32 synchronization object is destroyed.
      See also:
        CreateSyncObj, DestroySyncObj
    }
    property Active: Boolean read FActive write SetActive default False;
  end;

  { Description:
      This exception class is used for all exceptions raised by the
      synchronization objects.
  }
  ElvkSyncObj = class(Exception);

  { Description:
      A critical section is a basic synchronization object. A critical
      section can only be owned by a single thread at a time.

      Common use for a critical section is to protect a critical resource that
      cannot be safely shared between threads. The programmer acquires the
      critical section before accessing the resource and releases it afterwards.

      If the critical section is taken by a different thread when the thread
      tries to acquire it, it will be suspended until the critical section
      is available.

      It is not possible to check if the critical section is currently
      available or not. Also, the critical section is unnamed which means
      that it only lives within a single process.
    See also:
      TlvkMutex
  }
  TlvkCriticalSection = class(TlvkSyncObj)
  private
    // Internal synchronization object
    FSection  : TRTLCriticalSection;

  protected
    // <ALIAS TlvkSyncObj.CreateSyncObj>
    procedure CreateSyncObj; override;

    // <ALIAS TlvkSyncObj.DestroySyncObj>
    procedure DestroySyncObj; override;

  public
    { Description:
        This method acquires the critical section. If the critical section is
        taken by a different thread, the thread that tries to acquire the
        section will be suspended until the section is released by the other
        thread. Only a single thread can hold a critical section at any
        given time.

        See Release for an example of usage.

        Note: You should always encapsulate calls to Acquire and Release in a
          try/finally block to make sure you release the section when you're
          finished with it.
        Note: Enter is an alias for the Acquire method.
      See also:
        Release, Enter, Leave
    }
    procedure Acquire;

    { Description:
        This method releases the critical section. You should always

        Note: You should always encapsulate calls to Acquire and Release in a
          try/finally block to make sure you release the section when you're
          finished with it.
        Note: Leave is an alias for the Release method.
      See also:
        Acquire, Enter, Leave
      Example:
<code><b>procedure</b> UseCritSect;
<b>begin</b>
  SomeCriticalSection.Acquire;
  <b>try</b>
    ... the code that uses some protected resource here
  <b>finally</b>
    SomeCriticalSection.Release;
  <b>end</b>;
<b>end</b>;</code>
    }
    procedure Release;

    // <ALIAS Acquire>
    procedure Enter;

    // <ALIAS Release>
    procedure Leave;

    { Description:
        The TryEnter method tries to acquire the critical section. If it
        manages to acquire it, it will return True and the caller is responsible
        for releasing the critical section when its done with it. If the
        critical section is already taken by a different thread, TryEnter will
        not wait for the critical section to be released but will return with
        a value of False immediately.
      Returns:
        True if the critical section was available and was acquired for the
        thread, False if it was not available.
      See also:
        Acquire, Release
    }
    function TryEnter: Boolean;
  end;

  { Description:
      This enumerated type is used as a return value type for calls to the
      WaitFor methods defined in TlvkHandleObject.

      The possible values are:
        * ewrSignaled  - The object was signaled, set
        * ewrTimeout - The timeout interval elapsed before the object was able
          to acquire the synchronization object
        * ewrAbandoned - The synchronization object was destroyed while it
          was being waited upon.
        * ewrTerminated - The thread that waited for the object was terminated.
        * ewrError - An error occured while the object was being waited upon.

      Note: You should check the help pages for the synchronization objects
        that descends from TlvkHandleObject to see what effect and meaning
        waiting upon the object has for that particular synchronization object.
    See also:
      TlvkHandleObject, TlvkEvent, TlvkMutex, TlvkSemaphore
  }
  TlvkWaitResult = (ewrSignaled, ewrTimeout, ewrAbandoned, ewrTerminated, ewrError);

  { Description:
      This is a base component for all the synchronization objects that rely
      on a handle to the Win32 synchronization object internally.
    See also:
      TlvkEvent, TlvkMutex, TlvkSemaphore
  }
  TlvkHandleObject = class(TlvkSyncObj, IWaitableObject)
  private
    // Internal member to hold the handle
    FHandle : THandle;

    function GetHandle: THandle;

  protected
    { Description:
        This handle property is internally used by the descendant components
        to store the handle to the Win32 synchronization object in.
    }
    property Handle: THandle read FHandle write FHandle;

    // <ALIAS TlvkSyncObj.DestroySyncObj>
    procedure DestroySyncObj; override;

  public
    { Description:
        All the synchronization objects that uses handles will be able to be
        waited upon. Exactly what it means that you wait upon the object is
        described in the text for the object.
    See also:
      TlvkEvent, TlvkMutex, TlvkSemaphore
    }
    function WaitFor(Timeout: LongWord=INFINITE; const TerminationEvent: THandle=INVALID_HANDLE_VALUE): TlvkWaitResult;
  end;

  { Description:
      This component encapsulates the Win32 synchronization object Event.
      An event is an object that has two states: Signaled and Unsignaled.

      When you Set the event, you Signal it. When you Reset the event, you
      Unsignal it.

      You can wait for the event object and this will suspend the thread
      calling WaitFor until the event is signaled. If the Manual reset
      property is False (default value), then the event will become unsignaled
      in the process of waiting upon it.

      You can think of an event as a door. When the door is closed (unsignaled)
      everyone that wants to get through (waiting for it) will have to wait
      until it opens (becomes signaled). When the door is opened (becomes
      signaled), the first person is let through. If the manual reset property
      is True, everyone is let through. If the manual reset property is
      False, only the first person in the line is let through and then the
      door is closed behind him.

      If you use manual reset, you must at some point call ResetEvent to
      unsignal it.
    See also:
      TlvkHandleObject.WaitFor, TlvkEvent.SetEvent, TlvkEvent.ResetEvent
  }
  TlvkEvent = class(TlvkHandleObject)
  private
    // Internal member variable for the manual reset property
    FManualReset  : Boolean;
    // Internal member variable for the event name property
    FEventName    : string;
    // Internal member variable for the initial state property
    FInitialState : Boolean;

    // Internal property specifier for ManualReset
    procedure SetManualReset(const Value: Boolean);
    // Internal property specifier for InitialState
    procedure SetInitialState(const Value: Boolean);
    // Internal property specifier for EventName
    procedure SetEventName(const Value: string);
    // Internal property specifier for AutoReset
    function GetAutoReset: Boolean;
    // Internal property specifier for AutoReset
    procedure SetAutoReset(const Value: Boolean);

  protected
    // <ALIAS TlvkSyncObj.CreateSyncObj>
    procedure CreateSyncObj; override;

  public
    // <ALIAS TlvkSyncObj.Create@TComponent>
    constructor Create(AOwner: TComponent); override;

    { Description:
        This signals the event.

        See TlvkEvent for a more detailed description.
      See also:
        ResetEvent
    }
    procedure SetEvent;

    { Description:
        This unsignals the event.

        See TlvkEvent for a more detailed description.
      See also:
        SetEvent
    }
    procedure ResetEvent;

  published
    { Description:
        If the ManualReset property is True, once the event becomes signaled,
        all threads waiting for it will be resumed. If the ManualReset
        property is False, once the event becomes signaled, only one of the
        threads waiting for it will be resumed and then the event will be
        unsignaled again.
      See also:
        AutoReset, SetEvent, ResetEvent
    }
    property ManualReset: Boolean read FManualReset write SetManualReset default False;

    { Description:
        This property is the exact opposite of ManualReset. Setting either
        ManualReset or AutoReset will change the other property as well. Thus
        they are simple aliases to each other with the opposite value.
      See also:
        ManualReset, SetEvent, ResetEvent
    }
    property AutoReset: Boolean read GetAutoReset write SetAutoReset default True;

    { Description:
        This property tells the component wether the event should be created in
        a signaled or unsignaled state when you activate it.
      See also:
        SetEvent, ResetEvent
    }
    property InitialState: Boolean read FInitialState write SetInitialState default False;

    { Description:
        If you give the event a name, then the event can be shared between
        processes. Creating the event in two different processes with the
        same name allows one process to trigger the other. The default value
        is a blank name which makes the event a single-process event.
    }
    property EventName: string read FEventName write SetEventName;
  end;

  { Description:
      A Mutex is a kind of critical section in which only one thread can
      own the mutex at any given time. A mutex has some additional properties
      though:
        - You wait on a mutex and you can thus give a timeout. When you
          try to acquire a critical section the thread is suspended until
          it is available no matter how long.
        - A mutex can be named, which means that it can be shared between
          multiple processes. A critical section is a single-process
          synchronization object.

      When you wait for a mutex, the result from the WaitFor method can be
      one of the following values:
        * ewrSignaled  - the mutex was taken by the thread, remember to
          release it when you're finished with it
        * ewrTimeout - The timeout interval elapsed before the mutex could
          be taken by the thread. The thread does not own the mutex.
        * ewrAbandoned - The mutex object was destroyed while it
          was being waited upon.
        * ewrTerminated - The thread that waited for the mutex was terminated.
        * ewrError - An error occured while the mutex was being waited upon.

      You can think of a mutex as a critical section that can be shared and has
      a timeout feature.
    See also:
      TlvkCriticalSection
  }
  TlvkMutex = class(TlvkHandleObject)
  private
    // Internal member variable for the mutex name property
    FMutexName    : string;
    // Internal member variable for the initial owner property
    FInitialOwner : Boolean;

    // Internal property specifier for InitialOwner
    procedure SetInitialOwner(const Value: Boolean);
    // Internal property specifier for MutexName
    procedure SetMutexName(const Value: string);

  protected
    // <ALIAS TlvkSyncObj.CreateSyncObj>
    procedure CreateSyncObj; override;

  public
    // <ALIAS TlvkSyncObj.Create@TComponent>
    constructor Create(AOwner: TComponent); override;

    { Description:
        This method releases the mutex and it becomes available for another
        thread to wait for and take it.
      See also:
        TlvkHandleObject.WaitFor@LongWord@THandle
    }
    procedure Release;

  published
    { Description:
        If this property is True, then the thread that activates the mutex
        component becomes the initial owner automatically. The thread must then
        subsequently release the mutex. The property is default False and
        the mutex is thus available once activated.
      See also:
        TlvkHandleObject.WaitFor@LongWord@THandle, Release
    }
    property InitialOwner: Boolean read FInitialOwner write SetInitialOwner default False;

    { Description:
        If you give the mutex a name, then the mutex can be shared between
        processes. Creating the mutex in two different processes with the
        same name allows one process to take the mutex and it will become
        unavailable in the other process as well. You can do this to
        protect external critical resources that is used by two or more
        applications.
    }
    property MutexName: string read FMutexName write SetMutexName;
  end;

  { Description:
      A semaphore is a kind of mutex, but it can be owned by more than one
      thread/process at the same time. You can control how many can own it
      at the same time and if more than that number of threads try to
      take the semaphore at the same time, some of them will be suspended
      until other threads release their hold on it.

      To take the semaphore, use the WaitFor method. The possible return
      values and outcomes from that method are as follows:

        * ewrSignaled  - The semaphore was taken for the thread and the
          available "slots" was reduced by one. Remember to release the
          semaphore when you're finished with it.
        * ewrTimeout - The timeout interval elapsed before the semaphore could
          be taken by the thread.
        * ewrAbandoned - The semaphore object was destroyed while it
          was being waited upon.
        * ewrTerminated - The thread that waited for the semaphore was
          terminated.
        * ewrError - An error occured while the semaphore was being waited upon.

      Note: Windows does not keep track of who actually managed to take a
        slot in the semaphore. Thus, it's possible to call Release to
        increase the number of slots available in the semaphore without
        actually having to call WaitFor first, though this is not advisable.

      You can think of a semaphore as a pile of tokens. To access a resource,
      you need one of the tokens so you pick it up from the pile. If the pile
      is empty, you must wait for someone to return a token to the pile.
    See also:
      TlvkHandleObject.WaitFor@LongWord@THandle
  }
  TlvkSemaphore = class(TlvkHandleObject)
  private
    // Internal member variable for the maximum count property
    FMaximumCount   : LongWord;
    // Internal member variable for the semaphore name property
    FSemaphoreName  : string;

    // Internal property specifier for MaximumCount
    procedure SetMaximumCount(const Value: LongWord);
    // Internal property specifier for SemaphoreName
    procedure SetSemaphoreName(const Value: string);

  protected
    // <ALIAS TlvkSyncObj.CreateSyncObj>
    procedure CreateSyncObj; override;

  public
    // <ALIAS TlvkSyncObj.Create@TComponent>
    constructor Create(AOwner: TComponent); override;

    { Description:
        This method releases the semaphore and increases the number of
        available slots in it. If the semaphore was exhausted before the call
        to Release and threads are waiting for a slot, then one thread will
        be resumed and will take the slot.
      See also:
        TlvkHandleObject.WaitFor@LongWord@THandle
    }
    procedure Release;

  published
    { Description:
        A semaphore has a maximum number of slots available to take. Once
        that many threads has taken a slot any threads that tries to take one
        will be suspended until one of the slots is returned through a call
        to Release.
      See also:
        TlvkHandleObject.WaitFor@LongWord@THandle, Release
    }
    property MaximumCount: LongWord read FMaximumCount write SetMaximumCount default 2;

    { Description:
        If you give the semaphore a name, then the semaphore can be shared
        between processes. Creating the semaphore in two different processes
        with the same name and maximum count allows one process to take a slot
        from the semaphore and there will be one less slot available in both
        processes. You can do this to protect external critical resources that
        can take a certain load before becoming unusuable or slow, or
        other such tasks.
    }
    property SemaphoreName: string read FSemaphoreName write SetSemaphoreName;
  end;

  { Description:
      A "Single Writer Multiple Readers" synchronization object is kind of a
      hybrid object between two of the other synchronization objects: Mutex
      and critical section.

      This synchronization object is usually used to protect a resource that
      can take any number of threads that only read from it (and thus does not
      change its contents), but only one thread that writes to it at a time
      (and while it writes, no other threads can read from it).

      You can think of this synchronization object as a sheet of paper.
      Several people can stand around and read from the same sheet of paper.
      While there are people reading it, nobody can write on the paper. If
      a person wants to write to the paper, he has to wait until people
      leave, and while he waits, he will block other people from starting
      to read from the paper, thus the paper will eventually become available.
      Then the person takes the paper and writes on it. While he does that,
      noone else can read or write on the paper and has to wait until he is
      finished with it. Once he is finished with it, others can gain access to
      it, either to read or write.
    See also:
      TlvkSingleWriterMultipleReaders.BeginRead,
      TlvkSingleWriterMultipleReaders.EndRead,
      TlvkSingleWriterMultipleReaders.BeginWrite,
      TlvkSingleWriterMultipleReaders.EndWrite
  }
  TlvkSingleWriterMultipleReaders = class(TlvkSyncObj)
  private
    // Internal member variable for the synchronization object
    FSyncObj  : TMultiReadExclusiveWriteSynchronizer;

  protected
    // <ALIAS TlvkSyncObj.CreateSyncObj>
    procedure CreateSyncObj; override;
    // <ALIAS TlvkSyncObj.DestroySyncObj>
    procedure DestroySyncObj; override;

  public
    { Description:
        This method will try to gain access to the object in a read-fashion.

        Here's the behaviour of this method:
          * If the object is not taken by any other threads, it becomes
            blocked for writing and the thread has read access to the data
            it protects.
          * If other threads has taken the object for read access to the data,
            and no thread is trying to get write access to the data, the thread
            is also granted read access to the data.
          * If other threads has taken the object for read access to the data,
            but a thread is trying to get write access to the data, the thread
            is suspended until that other thread has gained write access, done
            its job and released the object.
          * If a different thread has taken the object for write access to the
            data it protects, the thread will be suspended until that other
            thread releases the object.
      See also:
        EndRead, BeginWrite, EndWrite
    }
    procedure BeginRead;

    { Description:
        This method releases the object for a thread that gained read access to
        the data that the object protects.
      See also:
        BeginRead, BeginWrite, EndWrite
    }
    procedure EndRead;

    { Description:
        This method will try to gain access to the object in a write-fashion.

        Here's the behaviour of this method:
          * If the object is not taken by any other threads, it becomes
            blocked for reading and the thread has full read/write access to the
            data it protects.
          * If other threads has taken the object for read access to the data,
            the thread is suspended until all those threads has released the
            object. While the thread is waiting, other threads can not gain
            read access to the object and are also suspended.
      See also:
        BeginRead, EndRead, EndWrite
    }
    procedure BeginWrite;

    { Description:
        This method releases the object for a thread that gained read/write
        access to the data that the object protects.
      See also:
        BeginRead, EndRead, BeginWrite
    }
    procedure EndWrite;
  end;

{ Description:
    This function returns a pchar-pointer to the string contents, or nil if
    the string is empty.
}
function PCharOf(const s: string): PChar;

type
  ElvkWaitFor = class(Exception);

{ Description:
    This is a specialized version of WaitForMultipleObjects. It will take an
    array of IWaitableObject interface references, and set up a wait on all
    of them. The return value will match the one you would get from
    WaitForMultipleObjects.
  Parameters:
    WaitableObjects - Array of objects supporting the IWaitableObject interface.
    WaitableHandles - Array of handles to normal synchronization kernel
      objects.
    Timeout - The timeout to wait for, can be INFINITE.
    AllRequired - Set to True to wait for all the handles/objects to become
      signaled, or set to False to wait for any of them to become signaled.
  See also:
    IWaitableObject
}
function lvkWaitFor(const WaitableObjects: array of IWaitableObject;
  const WaitableHandles: array of THandle;
  const Timeout: Cardinal; const AllRequired: Boolean=False): Integer; overload;

implementation

function lvkWaitFor(const WaitableObjects: array of IWaitableObject;
  const WaitableHandles: array of THandle;
  const Timeout: Cardinal; const AllRequired: Boolean): Integer;
var
  Handles : packed array of THandle;
  Index   : Integer;
begin
  if (Length(WaitableObjects) = 0) and (Length(WaitableHandles) = 0) then
    Result := WAIT_TIMEOUT
  else begin
    SetLength(Handles, Length(WaitableObjects) + Length(WaitableHandles));
    for Index := 0 to Length(WaitableObjects)-1 do
    begin
      if not Assigned(WaitableObjects[Index]) then
        raise ElvkWaitFor.Create('List of objects contain nil values');
      if (WaitableObjects[Index].Handle = INVALID_HANDLE_VALUE) or
        (WaitableObjects[Index].Handle = 0) then
        raise ElvkWaitFor.Create('List of objects contain invalid handles');

      Handles[Index] := WaitableObjects[Index].Handle;
    end;
    for Index := 0 to Length(WaitableHandles)-1 do
    begin
      if (WaitableHandles[Index] = INVALID_HANDLE_VALUE) or (WaitableHandles[Index] = 0) then
        raise ElvkWaitFor.Create('List of handles contain invalid values');

      Handles[Length(WaitableObjects) + Index] := WaitableHandles[Index];
    end;

    Result := WaitForMultipleObjects(Length(Handles), @Handles[0], AllRequired,
      Timeout);
  end;
end;

function PCharOf(const s: string): PChar;
begin
  if s='' then
    Result := nil
  else
    Result := PChar(s);
end;

{ TlvkSyncObj }

constructor TlvkSyncObj.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FActive := False;
end;

destructor TlvkSyncObj.Destroy;
begin
  Active := False;

  inherited;
end;

procedure TlvkSyncObj.Loaded;
begin
  inherited;

  if FLoadedActive then
    Active := True;
end;

procedure TlvkSyncObj.RequireActivated;
begin
  if csDesigning in ComponentState then
    Exit;
  if not FActive then
    raise ElvkSyncObj.Create('This operation can not be done on a disabled synchronization object');
end;

procedure TlvkSyncObj.RequireDeactivated;
begin
  if csDesigning in ComponentState then
    Exit;
  if FActive then
    raise ElvkSyncObj.Create('This operation can not be done on a enabled synchronization object');
end;

procedure TlvkSyncObj.SetActive(const Value: Boolean);
begin
  if csLoading in ComponentState then
    FLoadedActive := Value
  else if Value <> FActive then
  begin
    if Value then
      CreateSyncObj
    else
      DestroySyncObj;
    FActive := Value;
  end;
end;

{ TlvkCriticalSection }

procedure TlvkCriticalSection.Acquire;
begin
  RequireActivated;

  EnterCriticalSection(FSection);
end;

procedure TlvkCriticalSection.CreateSyncObj;
begin
  InitializeCriticalSection(FSection);
end;

procedure TlvkCriticalSection.DestroySyncObj;
begin
  DeleteCriticalSection(FSection);
end;

procedure TlvkCriticalSection.Enter;
begin
  Acquire;
end;

procedure TlvkCriticalSection.Leave;
begin
  Release;
end;

procedure TlvkCriticalSection.Release;
begin
  RequireActivated;
  LeaveCriticalSection(FSection);
end;

function TlvkCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FSection);
end;

{ TlvkEvent }

constructor TlvkEvent.Create(AOwner: TComponent);
begin
  inherited;

  FManualReset := False;
  FInitialState := False;
  FEventName := '';
end;

procedure TlvkEvent.CreateSyncObj;
begin
  Handle := CreateEvent(nil, FManualReset, FInitialState, PCharOf(FEventName));
end;

function TlvkEvent.GetAutoReset: Boolean;
begin
  Result := not ManualReset;
end;

procedure TlvkEvent.ResetEvent;
begin
  RequireActivated;
  Windows.ResetEvent(Handle);
end;

procedure TlvkEvent.SetAutoReset(const Value: Boolean);
begin
  ManualReset := not Value;
end;

procedure TlvkEvent.SetEvent;
begin
  RequireActivated;
  Windows.SetEvent(Handle);
end;

procedure TlvkEvent.SetEventName(const Value: string);
begin
  if Value <> FEventName then
  begin
    if Active then
      raise ElvkSyncObj.Create('Cannot change the eventname of an active event');
    FEventName := Value;
  end;
end;

procedure TlvkEvent.SetInitialState(const Value: Boolean);
begin
  if Value <> FInitialState then
  begin
    RequireDeactivated;
    FInitialState := Value;
  end;
end;

procedure TlvkEvent.SetManualReset(const Value: Boolean);
begin
  if Value <> FManualReset then
  begin
    RequireDeactivated;
    FManualReset := Value;
  end;
end;

{ TlvkMutex }

constructor TlvkMutex.Create(AOwner: TComponent);
begin
  inherited;

  FMutexName := '';
  FInitialOwner := True;
end;

procedure TlvkMutex.CreateSyncObj;
begin
  Handle := CreateMutex(nil, FInitialOwner, PCharOf(FMutexName));
end;

procedure TlvkMutex.Release;
begin
  ReleaseMutex(Handle);
end;

procedure TlvkMutex.SetInitialOwner(const Value: Boolean);
begin
  if Value <> FInitialOwner then
  begin
    RequireDeactivated;
    FInitialOwner := Value;
  end;
end;

procedure TlvkMutex.SetMutexName(const Value: string);
begin
  if Value <> FMutexName then
  begin
    RequireDeactivated;
    FMutexName := Value;
  end;
end;

{ TlvkHandleObject }

procedure TlvkHandleObject.DestroySyncObj;
begin
  CloseHandle(Handle);
  Handle := INVALID_HANDLE_VALUE;
end;

function TlvkHandleObject.GetHandle: THandle;
begin
  Result := FHandle;
end;

function TlvkHandleObject.WaitFor(Timeout: LongWord; const TerminationEvent: THandle): TlvkWaitResult;
var
  rc      : LongWord;
  Handles : array[0..1] of THandle;
begin
  RequireActivated;

  if TerminationEvent <> INVALID_HANDLE_VALUE then
  begin
    Handles[0] := Handle;
    Handles[1] := TerminationEvent;
    rc := WaitForMultipleObjects(2, @Handles, False, Timeout);
  end else
    rc := WaitForSingleObject(Handle, Timeout);

  if rc = WAIT_OBJECT_0+1 then
    Result := ewrTerminated
  else if rc = WAIT_OBJECT_0 then
    Result := ewrSignaled
  else if rc = WAIT_ABANDONED then
    Result := ewrAbandoned
  else if rc = WAIT_FAILED then
    Result := ewrError
  else if rc = WAIT_TIMEOUT then
    Result := ewrTimeout
  else
    raise ElvkSyncObj.Create('Invalid result from WaitForSingleObject');
end;

{ TlvkSemaphore }

constructor TlvkSemaphore.Create(AOwner: TComponent);
begin
  inherited;

  FMaximumCount := 2;
  FSemaphoreName := '';  
end;

procedure TlvkSemaphore.CreateSyncObj;
begin
  Handle := CreateSemaphore(nil, FMaximumCount, FMaximumCount, PCharOf(FSemaphoreName));
end;

procedure TlvkSemaphore.Release;
begin
  ReleaseSemaphore(Handle, 1, nil);
end;

procedure TlvkSemaphore.SetMaximumCount(const Value: LongWord);
begin
  if Value <> FMaximumCount then
  begin
    RequireDeactivated;
    FMaximumCount := Value;
  end;
end;

procedure TlvkSemaphore.SetSemaphoreName(const Value: string);
begin
  if Value <> FSemaphoreName then
  begin
    RequireDeactivated;
    FSemaphoreName := Value;
  end;
end;

{ TlvkSingleWriterMultipleReaders }

procedure TlvkSingleWriterMultipleReaders.BeginRead;
begin
  RequireActivated;
  FSyncObj.BeginRead;
end;

procedure TlvkSingleWriterMultipleReaders.BeginWrite;
begin
  RequireActivated;
  FSyncObj.BeginWrite;
end;

procedure TlvkSingleWriterMultipleReaders.CreateSyncObj;
begin
  FSyncObj := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure TlvkSingleWriterMultipleReaders.DestroySyncObj;
begin
  FreeAndNil(FSyncObj);
end;

procedure TlvkSingleWriterMultipleReaders.EndRead;
begin
  RequireActivated;
  FSyncObj.EndRead;
end;

procedure TlvkSingleWriterMultipleReaders.EndWrite;
begin
  RequireActivated;
  FSyncObj.EndWrite;
end;

end.
