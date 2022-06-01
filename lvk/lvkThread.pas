{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the implementation for components for threads and
    futures.
}
unit lvkThread;

// $Author: Lasse V. Karlsen $
// $Revision: 15 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkThread.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Forms, ActiveX, ComObj, Windows, SysUtils, Classes, lvkVersion,
  lvkSyncObjs, lvkComponents;

type
  TlvkCustomThreadWrapper = class;
  TlvkThread = class;

{ Description:
    This interface should be supported by all classes that wish to
    cooperate with the thread. Cooperation with a thread means that when
    external code asks the thread to terminate, all cooperating classes will
    be informed of this. This means that even if you call a method of a class
    that executes a lengthy calculation, it is possible to abort that if the
    class supports the IThreadCooperate interface.

    Note: You need to register the cooperating class with the thread by
      calling TlvkThread.RegisterCoopObject and unregister it with
      TlvkThread.UnRegisterCoopObject before you destroy the object.
  See also:
    TlvkThread.RegisterCoopObject@IThreadCooperate,
    TlvkThread.UnRegisterCoopObject@IThreadCooperate
}
  IThreadCooperate = interface
    ['{144BA141-7A85-11D5-B23C-0004761A6377}']

    { Description:
        This method will be called when the thread is asked to terminate.
        The method currently executing in the class should exit as soon
        as possible without leaving the system in an unstable state.
    }
    procedure Terminate;
  end;

{ Description:
    This method definition is used when calling SynchronizeWithData to
    call a method in the context of the main application thread and pass
    some data to it as well. The normal Synchronize method of TThread
    does not allow any data to be passed, but this does.
  Parameters:
    Data - The data to pass to the method, can be nil.
  See also:
    TlvkThread.SynchronizeWithData@TThreadDataMethod@IUnknown
}
  TThreadDataMethod = procedure(const Data: IUnknown) of object;

  { Description:
      Used internally with external synchronization
  }
  PSynchronizeWithThreadRecord = ^TSynchronizeWithThreadRecord;
  TSynchronizeWithThreadRecord = record
    PassData    : Boolean;
    DataMethod  : TThreadDataMethod;
    Method      : TThreadMethod;
    ID          : Byte;
    Data        : IUnknown;
    DoneEvent   : THandle;
  end;

  TFilter = set of Byte;

  TSleepUntilOption = (suoTerminated, suoSynchronized);
  TSleepUntilOptions = set of TSleepUntilOption;

{ Description:
    This class implements a layer on top of TThread to make it easier to
    cooperate with TlvkThreadWrapper and TlvkThreadCommunicationsQueue.
  See also:
    TlvkThreadWrapper, TlvkThreadCommunicationsQueue
}
  TlvkThread = class(TThread, IWaitableObject)
  private
    // Internal termination event handle
    FTerminationEvent     : THandle;
    // Data to pass to synchronization method
    FData                 : IUnknown;
    // Synchronization method
    FMethod               : TThreadDataMethod;
    // Internal destroying property value holder
    FDestroying           : Boolean;
    // Cooperating objects
    FCoopObjects          : IInterfaceList;
    // Used internally
    FSynchronizeEvent     : THandle;
    FSynchronizeList      : TList;
    FSynchronizeListLock  : TRTLCriticalSection;

    // Internal method to call synchronization method
    procedure DoSynchronizeWithData;

  protected
    // IUnknown interface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;

    // IWaitableObject interface
    function GetHandle: THandle;

    // Internal methods
    procedure InternalSynchronizeWithThread(
      const SyncRec: TSynchronizeWithThreadRecord;
      const TerminationEvent: THandle; const Timeout: Cardinal);

  public
    { Description:
        This constructor creates the thread object and initializes the
        termination event.
      See also:
        Destroy
    }
    constructor Create(const CreateSuspended: Boolean);

    { Description:
        This destructor destroys the event and the thread object.
      See also:
        Create
    }
    destructor Destroy; override;

    { Description:
        This method registers a cooperating class instance with the thread.
        If external code asks the thread to terminate, all cooperating
        objects will be asked to terminate as well. This means that even if
        the code is currently executing a lengthy operation inside a class,
        it is possible to abort it sooner than just waiting for the operation
        to complete.
      See also:
        IThreadCooperate, UnRegisterCoopObject
    }
    procedure RegisterCoopObject(const CoopObject: IThreadCooperate);

    { Description:
        Before the cooperating object can be destroyed, the programmer must
        unregister it with the thread so it no longer will be called if the
        thread is asked to terminate.
      See also:
        IThreadCooperate, RegisterCoopObject
    }
    procedure UnRegisterCoopObject(const CoopObject: IThreadCooperate);

    { Description:
        This method simply surfaces the Synchronize method from the TThread
        object. You use Synchronize when you want to call a object method
        that needs to run in the main application thread (for instance, it
        needs to manipulate objects on a form).

        See <EXTLINK borland://TThread_Synchronize>TThread.Synchronize</EXTLINK>
        for more information.
      Parameters:
        Method - The method to execute in the main application thread.
    }
    procedure Synchronize(const Method: TThreadMethod);

    { Description:
        This method functions like Synchronize, except that you can pass
        a block of data along with it as well. This data is then passed down
        to the method that is called to direct its operations more accurately,
        thus you don't need to set up global variables to pass information to
        the method.

        See <EXTLINK borland://TThread_Synchronize>TThread.Synchronize</EXTLINK>
        for more information.
      Parameters:
        Method - The method to execute in the main application thread.
        Data - The data to pass to the method.
      See also:
        TThreadDataMethod
    }
    procedure SynchronizeWithData(const Method: TThreadDataMethod;
      const Data: IUnknown);

    { Description:
        This method suspends the thread object. The method is overridden to
        make sure that the event handlers in TlvkThreadWrapper is called
        accoordingly when the thread suspends itself.

        See <EXTLINK borland://TThread_Suspend>TThread.Suspend</EXTLINK>
        for more information.
      See also:
        TlvkCustomThreadWrapper.Suspend
    }
    procedure Suspend; virtual;

    { Description:
        This method terminates the thread. The method is overridden to
        make sure that the internal event flag is signaled.

        Note: The thread is not forcibly killed. Terminate simply sets a flag
          on the thread, named Terminated, to True to let the thread know that
          it should terminate and exit. The thread execution handler should
          check this flag periodically and exit if this flag is ever True.

        See <EXTLINK borland://TThread_Terminate>TThread.Terminate</EXTLINK>
        for more information.
      See also:
        Terminated,
        TlvkCustomThreadWrapper.Terminate
    }
    procedure Terminate; virtual;

    { Description
        The Sleep method is used to perform a delay for the specified number
        of milliseconds. The difference between this Sleep method and the
        Sleep function defined in Windows.pas is that this method will
        abort the delay if the thread is asked to terminate itself.
      Parameters:
        Milliseconds  - The number of milliseconds to wait before
          continuing.
      Returns:
        True if the sleep was performed and False if the sleep was aborted
        by the termination event.
      See also:
        Terminated
    }
    function Sleep(const Milliseconds: Cardinal): Boolean; virtual;

    { TODO 2 -oLVK -cDocumentation : Document SleepUntil }
    function SleepUntil(const Milliseconds: Cardinal;
      const Options: TSleepUntilOptions=
        [suoTerminated, suoSynchronized]): Boolean; virtual;

    { Description:
        This property returns the return value of the thread. Since it's
        protected in TThread it is just surfaced here.

        See <EXTLINK borland://TThread_ReturnValue>TThread.ReturnValue</EXTLINK>
        for more information.
      See also:
        TlvkCustomThreadWrapper.ReturnValue
    }
    property ReturnValue;

    { Description:
        This property returns True if the thread has been asked to terminate.
        Since it's protected in TThread it is just surfaced here.

        See <EXTLINK borland://TThread_Terminated>TThread.Terminated</EXTLINK>
        for more information.
      See also:
        Terminate,
        TlvkCustomThreadWrapper.Running
    }
    property Terminated;

    { Description:
        When this property returns True it means that the owner
        TlvkCustomThreadWrapper component is in the process of being destroyed.
        The thread code can use this property to determine if its safe to
        synchronize or do other actions that might require the component
        or the form the component is dropped on to be alive and kicking.
      Returns:
        True if the owner component is being destroyed, False if not.
    }
    property Destroying: Boolean read FDestroying;

    { Description:
        This property gives access to the termination event of the thread
        object. When Terminate is called and asks the thread to terminate,
        this event becomes signaled. You can use this inside the thread's
        execute handler in combination with WaitForMultipleObject/WaitForSingleObject
        to wait for an external resource but allow for ending the wait if the
        thread should terminate.
      Returns:
        Handle to thread termination event synchronization object.
      See also:
        Terminate, Terminated
    }
    property TerminationEvent: THandle read FTerminationEvent;

    { Description:
        This method is used to add a synchronization request to the thread.
        The thread will never execute these requests by itself, but rely on the
        thread code to periodically call the CheckSynchronize method of the
        thread object.
      Parameters:
        SyncRec - A record containing information about the synchronization
          request to execute in the context of the thread.
      See also:
        CheckSynchronize@TFilter@Boolean
    }
    procedure AddSynchronizeWithThread(const SyncRec: TSynchronizeWithThreadRecord); virtual;

    { Description:
        This method is used by the thread code to check if there are pending
        synchronization requests on this thread. Pending requests will then be
        executed in the context of the thread.

        You can control what requests to handle by:
          * Using a ID filter. This way you can avoid calling long-running
            requests in the middle of a tight loop, but allow fast requests
            to be handled
          * Executing only one request and then return control to the thread.
      Parameters:
        Filter - Set of filter ID's to execute. Use [0..255] to execute any
          pending request.
        OnlyOne - Set to True to execute only one request before returning. If
          set to False, all pending requests that pass the filter test will
          be executed before control is returned.
      See also:
        AddSynchronizeWithThread@TSynchronizeWithThreadRecord
    }
    procedure CheckSynchronize(const Filter: TFilter; const OnlyOne: Boolean=False); overload; virtual;
    // <COMBINE CheckSynchronize@TFilter@Boolean>
    procedure CheckSynchronize(const OnlyOne: Boolean=False); overload; virtual;

    { Description:
        This method requests a synchronization. The method specified in the
        Method parameter will be called in the context of the thread. This is
        a blocking call so the calling code will wait until the request
        has been fulfilled. The Synchronize method on normal thread objects
        allow for executing a piece of code in the context of the main thread,
        this method allows for executing a piece of code in the context of an
        arbitry thread.
      Parameters:
        Method - The method to execute in the context of the thread.
        Data - The data to pass to the method when calling it.
        ID - The ID to use for this request. Is used in conjunction with the
          filtering with CheckSynchronize to allow for choosing which requests
          to honor at which time.
        TerminationEvent - Since the calling code will block, you can optionally
          specify a termination event object. If this object gets signalled
          while the code is waiting for the request to be handled, it will
          return control to the calling code.
        Timeout - Since the calling code will block, you can optionally
          specify a timeout interval. If the timeout elapses before the
          request has been handled, it will return control to the calling
          code.
    }
    procedure SynchronizeWithThread(const Method: TThreadDataMethod;
      const Data: IUnknown; const ID: Byte=0;
      const TerminationEvent: THandle=0;
      const Timeout: Cardinal=INFINITE); overload; virtual;
    // <COMBINE SynchronizeWithThread@TThreadDataMethod@IUnknown@Byte@THandle@Cardinal>
    procedure SynchronizeWithThread(const Method: TThreadMethod;
      const ID: Byte=0;
      const TerminationEvent: THandle=0;
      const Timeout: Cardinal=INFINITE); overload; virtual;
  end;

{ Description:
    This is the event handler definition for a thread's execute
    handler.
  Parameters:
    Sender - The instance of TlvkCustomThreadWrapper that handles this
      thread.
    Thread - The thread object that wraps around the windows thread object.
      Use methods on this object from inside the thread execute handler to
      suspend the thread, terminate it, check termination status, etc.
    Data - Any parameters or other information passed to the thread to
      direct its execution
    ReturnValue - The return value of the thread execution handler.
  See also:
    TlvkCustomThreadWrapper.OnExecute
}
  TThreadExecuteEvent = procedure(const Sender: TlvkCustomThreadWrapper;
    const Thread: TlvkThread; const Data: IUnknown;
    var ReturnValue: LongWord) of object;

{ Description:
    This enumerated type is used for the ThreadingModel property of the
    TlvkCustomThreadWrapper component. It dictates how the threads threading
    model is initialized when the thread is created.

    The following values are available:
      * tmNone - CoInitializeEx will not be called and the thread can not
        use any COM(+) objects at all, unless it calls CoInitializeEx itself.
      * tmApartment - All calls to objects created by the thread is handled in
        a serialized manner. No concurrent calls are done to objects created
        by the thread.
      * tmMultiThreader - Calls to objects created by the thread can happen in
        a concurrent manner. Any use of global or critical resources must be
        protected by critical sections or similar synchronization objects.
  See also:
    TlvkCustomThreadWrapper, TlvkCustomThreadWrapper.ThreadingModel
}
  TlvkThreadingModel = (tmNone, tmApartment, tmMultiThreaded);

{ Description:
    This component is the core thread wrapper component in this package. It
    maintains all the logic related to creating the thread object, starting it,
    suspending it, resuming it, checking its status, etc.
  See also:
    TlvkThread, TlvkThreadWrapper
}
  TlvkCustomThreadWrapper = class(TlvkComponent)
  private
    // Data to pass to thread
    FData                   : IUnknown;
    // Return value of last execution, or 0 if not yet executed or still running
    FReturnValue            : LongWord;
    // The currently running thread object
    FThread                 : TlvkThread;
    // The priority of the running thread, or the next thread to create
    FPriority               : TThreadPriority;
    // If set to True, when the component has loaded the thread will start
    FAutoStart              : Boolean;
    // Event is set when thread terminates
    FTerminatedEvent        : THandle;
    // Threading model
    FThreadingModel         : TlvkThreadingModel;
    // FThread variable lock object
    FThreadLock             : TRTLCriticalSection;

    // Main thread execution handler
    FOnExecute              : TThreadExecuteEvent;
    // Called before the thread is started
    FOnStart                : TNotifyEvent;
    // Called after the thread has terminated
    FOnTerminate            : TNotifyEvent;
    // Called when the thread is suspended
    FOnSuspend              : TNotifyEvent;
    // Called when the thread is resumed
    FOnResume               : TNotifyEvent;

    // Internal method that is called when the thread object termnates
    procedure ThreadTerminated(Owner: TObject);

    // Internal method that returns True if the thread is still running
    function GetRunning: Boolean;
    // Internal method that returns wether or not the thread is currently suspended
    function GetSuspended: Boolean;
    // Internal method that is used to set the suspended flag of the thread
    procedure SetSuspended(const Value: Boolean);

    // Internal method that returns the current thread priority
    function GetPriority: TThreadPriority;
    // Internal method used to set the new thread priority
    procedure SetPriority(const Value: TThreadPriority);
    // Internal method used to set the threading model
    procedure SetThreadingModel(const Value: TlvkThreadingModel);

    // Internal method to set OnExecute event handler
    procedure SetOnExecute(const Value: TThreadExecuteEvent);
    // Internal method to set OnStart event handler
    procedure SetOnStart(const Value: TNotifyEvent);
    // Internal method to set OnTerminate event handler
    procedure SetOnTerminate(const Value: TNotifyEvent);
    // Internal method to set OnSuspend event handler
    procedure SetOnSuspend(const Value: TNotifyEvent);
    // Internal method to set OnResume event handler
    procedure SetOnResume(const Value: TNotifyEvent);

    // Internal method to call the OnSuspend event handler
    procedure DoSuspend;
    // Internal method to call the OnResume event handler
    procedure DoResume;
    // Internal method to call the OnStart event handler
    procedure DoStart;
    // Internal method to call the OnTerminate event handler
    procedure DoTerminate;

  protected
    // Internal method that starts the thread if AutoStart is True
    procedure Loaded; override;

    { Description:
        This property gives the value True if the thread is currently running.
        Running simply means that it has been started and hasn't yet
        terminated. If the thread is suspended, it's still considered to be
        running. Use the Suspended property to check if the thread has been
        suspended or not.
      See also:
        Suspended, Start, Terminate
    }
    property Running: Boolean read GetRunning;

    { Description:
        This property gives the value True if the thread is Running, but it
        has been suspended. If the thread is not running, then this property
        will give the value True.
      See also:
        Running, Start, Terminate, Suspend, Resume
    }
    property Suspended: Boolean read GetSuspended write SetSuspended;

    { Description:
        This property holds the current thread priority value.

        Note: If no thread is running when you change the priority then the
          next thread you start will have the given priority.
      See also:
        Start, Terminate
    }
    property Priority: TThreadPriority read GetPriority write SetPriority
      default tpNormal;

    { Description:
        This property gives the return value of the last thread execution,
        or 0 if not yet started/finished.
      See also:
        Start, Terminate, WaitFor
    }
    property ReturnValue: LongWord read FReturnValue;

    { Description:
        This property describes what threading model the thread should operate
        in. See TlvkThreadingModel for more information.
      See also:
        TlvkThreadingModel
    }
    property ThreadingModel: TlvkThreadingModel read FThreadingModel
      write SetThreadingModel default tmNone;

    { Description:
        This event handler is used to execute the core code that is to be
        executed in the thread. To terminate the thread, simply exit the
        event handler.

        Note: This event handler will run solely in a different thread
          context. This means that you should not directly access anything
          related to the VCL since the VCL components are not deemed
          thread-safe in all cases.
      See also:
        TThreadExecuteEvent, Start, Terminate
    }
    property OnExecute: TThreadExecuteEvent read FOnExecute write SetOnExecute;

    { Description:
        This event handler is called just before the thread is started from
        a call to Start.
      See also:
        Start
    }
    property OnStart: TNotifyEvent read FOnStart write SetOnStart;

    { Description:
        This event handler is called just after the thread has terminated
        because of the thread execution handler exiting. It's safe to restart
        the thread from this event handler.
      See also:
        Terminate
    }
    property OnTerminate: TNotifyEvent read FOnTerminate write SetOnTerminate;

    { Description:
        This event handler is called when the thread is suspended, either by
        a different thread or the thread suspends itself. It's safe to
        resume the thread from this event handler.
      See also:
        Suspend, Resume
    }
    property OnSuspend: TNotifyEvent read FOnSuspend write SetOnSuspend;

    { Description:
        This event handler is called when the thread is resumed. It's safe to
        suspend the thread from this event handler.
      See also:
        Suspend, Resume
    }
    property OnResume: TNotifyEvent read FOnResume write SetOnResume;

    { Description:
        Use this property to make the component start the thread immediately
        after the component has been loaded. Typical use will be to set this
        to True if you want the thread to start executing once the form the
        component is dropped onto has loaded.

        See <EXTLINK borland://TComponent_Loaded>TComponent.Loaded</EXTLINK>
        for more information.
    }
    property AutoStart: Boolean read FAutoStart write FAutoStart
      default False;

  public
    { Description:
        This constructor creates and initializes the thread wrapper
        component instance. The thread is not created yet though.
      Parameters:
        AOwner - The owner of the component.
      See also:
        Destroy
    }
    constructor Create(AOwner: TComponent); override;

    { Description:
        This destructor terminates and wait for the thread (if it's
        running) and then destroys the thread wrapper object instance.
      See also:
        Create
    }
    destructor Destroy; override;

    { Description:
        This method starts the thread. You must assign an event handler to
        OnExecute before calling Start or there will be raised an exception.

        Note: It's illegal to call Start if there's already a thread running.
      Parameters:
        Data - Any setup information and data to pass to the thread to direct
          its execution.
      See also:
        Suspend, Resume, Terminate, WaitFor
    }
    procedure Start(const Data: IUnknown=nil); virtual;

    { Description:
        This method will suspend the currently running thread. If the
        thread is not running, nothing will be done.

        Note: Even though a thread can suspend itself, it cannot (for
          obvious reasons) resume itself. Use the Resume method of this object
          to resume a suspended thread.
      See also:
        Resume
    }
    procedure Suspend; virtual;

    { Description:
        This method will resume a suspended running thread. If no thread
        is running, nothing will be done.
      See also:
        Suspend
    }
    procedure Resume; virtual;

    { Description:
        This method will ask the running thread to terminate itself. If no
        thread is running, nothing will be done.
      See also:
        TlvkThread.Terminate, TlvkThread.Terminated
    }
    procedure Terminate; virtual;

    { Description:
        This method will wait for the thread execution handler to exit
        and then return the return value the thread returned. If the thread
        is not running then the last return value is simply returned directly.
      Parameters:
        TerminationEvent  - The termination event of the thread that calls
          TerminateAndWaitFor. This allows the thread that wants to terminate
          and wait for a different thread to be terminated without it hanging.
        ThreadReturnValue - For the function types that returns True if it
          managed to terminate and wait for the thread, this parameter returns
          the thread return value.
        Timeout           - How many milliseconds to wait for the thread to
          terminate. Use INFINITE to wait until either thread (the one
          calling TerminateAndWaitFor or the one being waited for) terminates.
      See also:
        TlvkThread.Terminated, ReturnValue,
        WaitFor@THandle@LongWord@Cardinal, WaitFor@THandle@Cardinal
    }
    function WaitFor: LongWord; overload; virtual;

    // <COMBINE WaitFor>
    function WaitFor(const TerminationEvent: THandle;
      out ThreadReturnValue: LongWord; const Timeout: Cardinal): Boolean; overload; virtual;
    // <COMBINE WaitFor>
    function WaitFor(const TerminationEvent: THandle;
      const Timeout: Cardinal): Boolean; overload; virtual;

    { Description:
        This method terminates the thread and waits for it to terminate, it
        then returns the returnvalue of the thread. It's a combination of
        Terminate and WaitFor into one procedure.
      Parameters:
        TerminationEvent  - The termination event of the thread that calls
          TerminateAndWaitFor. This allows the thread that wants to terminate
          and wait for a different thread to be terminated without it hanging.
        ThreadReturnValue - For the function types that returns True if it
          managed to terminate and wait for the thread, this parameter returns
          the thread return value.
        Timeout           - How many milliseconds to wait for the thread to
          terminate. Use INFINITE to wait until either thread (the one
          calling TerminateAndWaitFor or the one being waited for) terminates.

      See also:
        TlvkThread.Terminated, Terminate, WaitFor
    }
    function TerminateAndWaitFor: LongWord; overload; virtual;

    // <COMBINE TerminateAndWaitFor>
    function TerminateAndWaitFor(const TerminationEvent: THandle;
      out ThreadReturnValue: LongWord; const Timeout: Cardinal): Boolean; overload; virtual;
    // <COMBINE TerminateAndWaitFor>
    function TerminateAndWaitFor(const TerminationEvent: THandle;
      const Timeout: Cardinal): Boolean; overload; virtual;

    // <ALIAS TlvkThread.SynchronizeWithThread@TThreadDataMethod@IUnknown@Byte@THandle@Cardinal>
    procedure SynchronizeWithThread(const Method: TThreadDataMethod;
      const Data: IUnknown; const ID: Byte=0;
      const TerminationEvent: THandle=0;
      const Timeout: Cardinal=INFINITE); overload; virtual;
    // <ALIAS TlvkThread.SynchronizeWithThread@TThreadMethod@Byte@THandle@Cardinal>
    procedure SynchronizeWithThread(const Method: TThreadMethod;
      const ID: Byte=0;
      const TerminationEvent: THandle=0;
      const Timeout: Cardinal=INFINITE); overload; virtual;
  end;

{ Description:
    This component is simply the component that surfaces all the necessary
    properties from TlvkCustomThreadWrapper.
  See also:
    TlvkCustomThreadWrapper, TlvkThread
}
  TlvkThreadWrapper = class(TlvkCustomThreadWrapper)
  public
    // <ALIAS TlvkCustomThreadWrapper.Running>
    property Running;
    // <ALIAS TlvkCustomThreadWrapper.Suspended>
    property Suspended;
    // <ALIAS TlvkCustomThreadWrapper.ReturnValue>
    property ReturnValue;

  published
    // <ALIAS TlvkCustomThreadWrapper.Priority>
    property Priority;
    // <ALIAS TlvkCustomThreadWrapper.AutoStart>
    property AutoStart;
    // <ALIAS TlvkCustomThreadWrapper.ThreadingModel>
    property ThreadingModel;

    // <ALIAS TlvkCustomThreadWrapper.OnExecute>
    property OnExecute;
    // <ALIAS TlvkCustomThreadWrapper.OnStart>
    property OnStart;
    // <ALIAS TlvkCustomThreadWrapper.OnTerminate>
    property OnTerminate;
    // <ALIAS TlvkCustomThreadWrapper.OnSuspend>
    property OnSuspend;
    // <ALIAS TlvkCustomThreadWrapper.OnResume>
    property OnResume;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

{ Description:
    This event handler definition is used for the TlvkFuture component.
  Parameters:
    Sender - The instance of TlvkCustomThreadWrapper that handles this
      thread.
    Thread - The thread object that wraps around the windows thread object.
      Use methods on this object from inside the thread execute handler to
      suspend the thread, terminate it, check termination status, etc.
    ReturnValue - The return value of the calculation event handler.
  See also:
    TlvkFuture.OnCalculate
}
  TFutureCalculateEvent = function(const Sender: TlvkCustomThreadWrapper;
    const Thread: TlvkThread; const Data: IUnknown): Variant of object;

{ Description:
    This class implements a future. A future is a thread class that is used
    to calculate a given value. The whole point of this is that you can start
    the calculation and at some point in the future (hence the name), the
    result of the calculation is made ready.
  See also:
    TlvkFuture.Start, TlvkFuture.Value
}
  TlvkFuture = class(TlvkCustomThreadWrapper)
  private
    // Event handler that is to be called to calculate
    FOnCalculate  : TFutureCalculateEvent;
    // Last returned value
    FValue        : Variant;
    // Is the value calculated or not
    FReady        : Boolean;

    // Internal method to handle the thread execution
    procedure ThreadExecute(const Sender: TlvkCustomThreadWrapper;
      const Thread: TlvkThread; const Data: IUnknown;
      var ReturnValue: LongWord);
    // Internal method to set the calculation event handler
    procedure SetOnCalculate(const Value: TFutureCalculateEvent);

  public
    // <ALIAS TlvkCustomThreadWrapper.Create@TComponent>
    constructor Create(AOwner: TComponent); override;

    // <ALIAS TlvkCustomThreadWrapper.Start@IUnknown>
    procedure Start(const Data: IUnknown=nil); override;

    { Description:
        This method clears the future by resetting the Ready flag.

        Note: If the thread is currently running, then an exception will
          be raised.
      See also:
        Ready, Value
    }
    procedure Clear;

    { Description:
        This method returns the value returned by the calculation event
        handler. If the thread is still running, Value will wait for the
        calculation to end, thus making sure that the correct value is
        returned.

        If the thread has not been started yet (Ready and Running are both False)
        then the thread is started, then waited upon.
      Returns:
        Value from calculation.
      See also:
        Ready
    }
    function Value: Variant;

    // <ALIAS TlvkCustomThreadWrapper.Running>
    property Running;
    // <ALIAS TlvkCustomThreadWrapper.Suspended>
    property Suspended;

    { Description:
        This property gives the value True if the future calculation has
        executed and finished with a result.
      See also:
        Value
    }
    property Ready: Boolean read FReady;

  published
    // <ALIAS TlvkCustomThreadWrapper.Priority>
    property Priority;
    // <ALIAS TlvkCustomThreadWrapper.AutoStart>
    property AutoStart;
    // <ALIAS TlvkCustomThreadWrapper.ThreadingModel>
    property ThreadingModel;

    { Description:
        This event handler is called to do the main calculation.
      See also:
        TFutureCalculateEvent
    }
    property OnCalculate: TFutureCalculateEvent read FOnCalculate write SetOnCalculate;

    // <ALIAS TlvkCustomThreadWrapper.OnStart>
    property OnStart;
    // <ALIAS TlvkCustomThreadWrapper.OnTerminate>
    property OnTerminate;
    // <ALIAS TlvkCustomThreadWrapper.OnSuspend>
    property OnSuspend;
    // <ALIAS TlvkCustomThreadWrapper.OnResume>
    property OnResume;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  { Description:
      This is the base exception class for all exceptions raised from the
      lvkThread classes.
  }
  ElvkThreadWrapper = class(Exception);
  { Description:
      This is the base exception class for all exceptions raised from the
      future component.
  }
  ElvkFuture = class(ElvkThreadWrapper);

resourcestring
  SCannotClear        = 'Cannot clear future while thread is running';
  SCannotChange       = 'Cannot change property %s when thread is running';
  SNoExecute          = 'Cannot start thread, no OnExecute event';
  SNoCalculate        = 'Cannot calculate future, no OnCalculate event';
  SAlreadyRunning     = 'Cannot start thread, already running';
  SNotRunning         = 'Cannot synchronize with thread, thread is not running';
  SCannotResume       = 'Cannot resume thread, not yet started';
  SCannotSuspend      = 'Cannot suspend thread, not yet started';
  SCannotTerminate    = 'Cannot terminate thread, not yet started';
  SCannotSetSuspended = 'Cannot set suspended flag, thread not yet started';

implementation

uses
  Messages, SyncObjs;

const
  CM_SYNCHRONIZE = WM_USER + 1;

type
  TWrapperThread = class(TlvkThread)
  private
    FOwner      : TlvkCustomThreadWrapper;
    FException  : Exception;

  protected
    procedure Execute; override;
    procedure HandleException; virtual;

  public
    constructor Create(const Owner: TlvkCustomThreadWrapper);

    procedure Suspend; override;
  end;

{ TlvkCustomThreadWrapper }

constructor TlvkCustomThreadWrapper.Create(AOwner: TComponent);
begin
  inherited;

  FPriority := tpNormal;
  FAutoStart := False;
  FReturnValue := 0;
  FOnExecute := nil;

  FOnStart := nil;
  FOnSuspend := nil;
  FOnResume := nil;
  FOnTerminate := nil;

  FTerminatedEvent := CreateEvent(nil, True, True, nil);
  InitializeCriticalSection(FThreadLock);
end;

destructor TlvkCustomThreadWrapper.Destroy;
begin
  EnterCriticalSection(FThreadLock);
  try
    if Running then
    begin
      FThread.FDestroying := True;
      Terminate;
      WaitFor;
    end;
  finally
    LeaveCriticalSection(FThreadLock);
  end;
  CloseHandle(FTerminatedEvent);
  DeleteCriticalSection(FThreadLock);

  inherited;
end;

procedure TlvkCustomThreadWrapper.DoResume;
begin
  if Assigned(FOnResume) and (not (csDestroying in ComponentState)) then
    FOnResume(Self);
end;

procedure TlvkCustomThreadWrapper.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TlvkCustomThreadWrapper.DoSuspend;
begin
  if Assigned(FOnSuspend) and (not (csDestroying in ComponentState)) then
    FOnSuspend(Self);
end;

procedure TlvkCustomThreadWrapper.DoTerminate;
begin
  if Assigned(FOnTerminate) and (not (csDestroying in ComponentState)) then
    FOnTerminate(Self);
end;

function TlvkCustomThreadWrapper.GetPriority: TThreadPriority;
begin
  Result := FPriority;
end;

function TlvkCustomThreadWrapper.GetRunning: Boolean;
begin
  Result := Assigned(FThread);
end;

function TlvkCustomThreadWrapper.GetSuspended: Boolean;
begin
  EnterCriticalSection(FThreadLock);
  try
    if Assigned(FThread) then
      Result := FThread.Suspended
    else
      Result := True;
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

procedure TlvkCustomThreadWrapper.Loaded;
begin
  inherited;

  if FAutoStart and (not (csDesigning in ComponentState)) then
    Start;
end;

procedure TlvkCustomThreadWrapper.Resume;
begin
  EnterCriticalSection(FThreadLock);
  try
    if Assigned(FThread) then
    begin
      DoResume;
      FThread.Resume;
    end;
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

procedure TlvkCustomThreadWrapper.SetOnExecute(
  const Value: TThreadExecuteEvent);
begin
  if Running then
    raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnExecute']);

  FOnExecute := Value;
end;

procedure TlvkCustomThreadWrapper.SetOnResume(const Value: TNotifyEvent);
begin
  if Running then
    raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnResume']);

  FOnResume := Value;
end;

procedure TlvkCustomThreadWrapper.SetOnStart(const Value: TNotifyEvent);
begin
  if Running then
    raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnStart']);

  FOnStart := Value;
end;

procedure TlvkCustomThreadWrapper.SetOnSuspend(const Value: TNotifyEvent);
begin
  if Running then
    raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnSuspend']);

  FOnSuspend := Value;
end;

procedure TlvkCustomThreadWrapper.SetOnTerminate(
  const Value: TNotifyEvent);
begin
  if Running then
    raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['OnTerminate']);

  FOnTerminate := Value;
end;

procedure TlvkCustomThreadWrapper.SetPriority(
  const Value: TThreadPriority);
begin
  if Value <> FPriority then
  begin
    FPriority := Value;
    EnterCriticalSection(FThreadLock);
    try
      if Running then
        FThread.Priority := FPriority;
    finally
      LeaveCriticalSection(FThreadLock);
    end;
  end;
end;

procedure TlvkCustomThreadWrapper.SetSuspended(const Value: Boolean);
begin
  if Value then
    Suspend
  else
    Resume;
end;

procedure TlvkCustomThreadWrapper.SetThreadingModel(
  const Value: TlvkThreadingModel);
begin
  if Running then
    raise ElvkThreadWrapper.CreateFmt(SCannotChange, ['ThreadingModel']);

  FThreadingModel := Value;
end;

procedure TlvkCustomThreadWrapper.Start(const Data: IUnknown=nil);
begin
  if not Assigned(FOnExecute) then
    raise ElvkThreadWrapper.Create(SNoExecute);
  if Assigned(FThread) then
    raise ElvkThreadWrapper.Create(SAlreadyRunning);
  FData := Data;
  FThread := TWrapperThread.Create(Self);
  ResetEvent(FTerminatedEvent);

  DoStart;
  FThread.Resume;
end;

procedure TlvkCustomThreadWrapper.Suspend;
begin
  EnterCriticalSection(FThreadLock);
  try
    if Assigned(FThread) then
    begin
      DoSuspend;
      TThread(FThread).Suspend;
    end;
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

procedure TlvkCustomThreadWrapper.SynchronizeWithThread(
  const Method: TThreadDataMethod; const Data: IUnknown; const ID: Byte;
  const TerminationEvent: THandle; const Timeout: Cardinal);
begin
  if not Assigned(FThread) then
    raise ElvkThreadWrapper.Create(SNotRunning);

  FThread.SynchronizeWithThread(Method, Data, ID, TerminationEvent, Timeout);
end;

procedure TlvkCustomThreadWrapper.SynchronizeWithThread(
  const Method: TThreadMethod; const ID: Byte; const TerminationEvent: THandle;
  const Timeout: Cardinal);
begin
  if not Assigned(FThread) then
    raise ElvkThreadWrapper.Create(SNotRunning);

  FThread.SynchronizeWithThread(Method, ID, TerminationEvent, Timeout);
end;

procedure TlvkCustomThreadWrapper.Terminate;
begin
  EnterCriticalSection(FThreadLock);
  try
    if Assigned(FThread) then
      FThread.Terminate;
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

function TlvkCustomThreadWrapper.TerminateAndWaitFor: LongWord;
begin
  if Running then
  begin
    Terminate;
    Result := WaitFor;
  end else
    Result := ReturnValue;
end;

function TlvkCustomThreadWrapper.TerminateAndWaitFor(
  const TerminationEvent: THandle; out ThreadReturnValue: LongWord;
  const Timeout: Cardinal): Boolean;
begin
  if Running then
  begin
    Terminate;
    Result := WaitFor(TerminationEvent, ThreadReturnValue, Timeout);
  end else begin
    Result := True;
    ThreadReturnValue := ReturnValue;
  end;
end;

function TlvkCustomThreadWrapper.TerminateAndWaitFor(
  const TerminationEvent: THandle; const Timeout: Cardinal): Boolean;
var
  ThreadReturnValue : LongWord;
begin
  Result := TerminateAndWaitFor(TerminationEvent, ThreadReturnValue, Timeout);
end;

procedure TlvkCustomThreadWrapper.ThreadTerminated(Owner: TObject);
begin
  DoTerminate;
end;

function TlvkCustomThreadWrapper.WaitFor: LongWord;
begin
  if Running then
    WaitForSingleObject(FTerminatedEvent, INFINITE);

  Result := FReturnValue;
end;

function TlvkCustomThreadWrapper.WaitFor(const TerminationEvent: THandle;
  out ThreadReturnValue: LongWord; const Timeout: Cardinal): Boolean;
var
  Handles : array[0..1] of THandle;
  rc  : DWORD;
begin
  if Running then
  begin
    Handles[0] := FTerminatedEvent;
    Handles[1] := TerminationEvent;
    rc := WaitForMultipleObjects(2, @Handles, False, Timeout);

    case rc of
      WAIT_OBJECT_0:
        Result := True;

      WAIT_OBJECT_0+1, WAIT_TIMEOUT:
        Result := False;
    else
      raise ElvkThreadWrapper.Create('Internal error in TlvkCustomThreadWrapper.WaitFor');
    end;
  end else
    Result := True;

  if Result then
    ThreadReturnValue := FReturnValue
  else
    ThreadReturnValue := 0;
end;

function TlvkCustomThreadWrapper.WaitFor(const TerminationEvent: THandle;
  const Timeout: Cardinal): Boolean;
var
  ThreadReturnValue : LongWord;
begin
  Result := WaitFor(TerminationEvent, ThreadReturnValue, Timeout);
end;

{ TWrapperThread }

constructor TWrapperThread.Create(const Owner: TlvkCustomThreadWrapper);
begin
  Assert(Assigned(Owner));
  Assert(Assigned(Owner.OnExecute));

  inherited Create(True);

  FOwner := Owner;
  Priority := Owner.Priority;
  OnTerminate := Owner.ThreadTerminated;
  FreeOnTerminate := True;
end;

procedure TWrapperThread.Execute;
var
  rc  : HResult;
begin
  try
    rc := 0;
    case FOwner.FThreadingModel of
      tmNone          : rc := 0;
      tmApartment     : rc := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      tmMultiThreaded : rc := CoInitializeEx(nil, COINIT_MULTITHREADED);
    end;
    if rc <> 0 then
      raise EOleException.Create(SysErrorMessage(rc), rc, '', '', 0);

    FOwner.FReturnValue := 0;
    try
      FOwner.OnExecute(FOwner, Self, FOwner.FData, FOwner.FReturnValue);
    except
      on E: EAbort do
        ; // swallow this one

      on E: Exception do
      begin
        FException := E;
        Synchronize(HandleException);
      end;
    end;
  finally
    EnterCriticalSection(FOwner.FThreadLock);
    try
      FOwner.FThread := nil;
    finally
      LeaveCriticalSection(FOwner.FThreadLock);
    end;
    SetEvent(FOwner.FTerminatedEvent);
  end;
end;

procedure TWrapperThread.HandleException;
begin
  Application.ShowException(FException);
end;

procedure TWrapperThread.Suspend;
begin
  if Assigned(FOwner.FOnSuspend) then
    Synchronize(FOwner.DoSuspend);
  inherited Suspend;
end;

{ TlvkThread }

procedure TlvkThread.AddSynchronizeWithThread(
  const SyncRec: TSynchronizeWithThreadRecord);
begin
  EnterCriticalSection(FSynchronizeListLock);
  try
    SetEvent(FSynchronizeEvent);
    FSynchronizeList.Add(@SyncRec);
  finally
    LeaveCriticalSection(FSynchronizeListLock);
  end;
end;

procedure TlvkThread.CheckSynchronize(
  const Filter: TFilter; const OnlyOne: Boolean);
var
  Index : Integer;
  Item  : PSynchronizeWithThreadRecord;
begin
  EnterCriticalSection(FSynchronizeListLock);
  try
    ResetEvent(FSynchronizeEvent);
    
    for Index := FSynchronizeList.Count-1 downto 0 do
    begin
      Item := FSynchronizeList[Index];
      if Item.ID in Filter then
      begin
        FSynchronizeList.Delete(Index);
        if Item.PassData then
          Item.DataMethod(Item.Data)
        else
          Item.Method;

        SetEvent(Item.DoneEvent);
        if OnlyOne then
          Break;
      end;
    end;
  finally
    LeaveCriticalSection(FSynchronizeListLock);
  end;
end;

procedure TlvkThread.CheckSynchronize(const OnlyOne: Boolean);
begin
  CheckSynchronize([0..255]);
end;

constructor TlvkThread.Create(const CreateSuspended: Boolean);
begin
  inherited Create(True);

  FTerminationEvent := CreateEvent(nil, True, False, nil);
  FSynchronizeEvent := CreateEvent(nil, True, False, nil);
  FCoopObjects := TInterfaceList.Create as IInterfaceList;
  FSynchronizeList := TList.Create;
  InitializeCriticalSection(FSynchronizeListLock);
end;

destructor TlvkThread.Destroy;
begin
  DeleteCriticalSection(FSynchronizeListLock);
  FSynchronizeList.Free;
  CloseHandle(FTerminationEvent);
  CloseHandle(FSynchronizeEvent);
  FCoopObjects := nil;

  inherited;
end;

procedure TlvkThread.DoSynchronizeWithData;
begin
  FMethod(FData);
end;

function TlvkThread.GetHandle: THandle;
begin
  Result := FTerminationEvent;
end;

procedure TlvkThread.InternalSynchronizeWithThread(
  const SyncRec: TSynchronizeWithThreadRecord;
  const TerminationEvent: THandle; const Timeout: Cardinal);
var
  TempRec : TSynchronizeWithThreadRecord;
begin
  TempRec := SyncRec;
  TempRec.DoneEvent := CreateEvent(nil, True, False, nil);
  try
    AddSynchronizeWithThread(TempRec);

    if TerminationEvent = 0 then
      lvkWaitFor([], [TempRec.DoneEvent], Timeout, False)
    else
      lvkWaitFor([], [TempRec.DoneEvent, TerminationEvent], Timeout, False);
  finally
    CloseHandle(TempRec.DoneEvent);
  end;
end;

function TlvkThread.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TlvkThread.RegisterCoopObject(
  const CoopObject: IThreadCooperate);
begin
  FCoopObjects.Add(CoopObject);
end;

function TlvkThread.Sleep(const Milliseconds: Cardinal): Boolean;
var
  rc  : Integer;
begin
  rc := WaitForSingleObject(FTerminationEvent, Milliseconds);
  Result := (rc <> WAIT_OBJECT_0);
end;

function TlvkThread.SleepUntil(const Milliseconds: Cardinal;
  const Options: TSleepUntilOptions): Boolean;
var
  rc  : Integer;
  Events  : array[0..1] of THandle;
  EventCount  : Integer;
begin
  EventCount := 0;
  if suoTerminated in Options then
  begin
    Events[EventCount] := FTerminationEvent;
    Inc(EventCount);
  end;
  if suoSynchronized in Options then
  begin
    Events[EventCount] := FSynchronizeEvent;
    Inc(EventCount);
  end;

  if EventCount = 0 then
  begin
    Windows.Sleep(Milliseconds);
    Result := True;
  end else if EventCount = 0 then
  begin
    rc := WaitForSingleObject(Events[0], Milliseconds);
    Result := (rc = WAIT_TIMEOUT);
  end else
  begin
    rc := WaitForMultipleObjects(EventCount, @Events, False, Milliseconds);
    Result := (rc = WAIT_TIMEOUT);
  end;
end;

procedure TlvkThread.Suspend;
begin
  inherited Suspend;
end;

procedure TlvkThread.Synchronize(const Method: TThreadMethod);
begin
  inherited Synchronize(Method);
end;

procedure TlvkThread.SynchronizeWithData(const Method: TThreadDataMethod;
  const Data: IUnknown);
begin
  Assert(Assigned(Method));

  FData := Data;
  FMethod := Method;

  Synchronize(DoSynchronizeWithData);
end;

procedure TlvkThread.SynchronizeWithThread(const Method: TThreadDataMethod;
  const Data: IUnknown; const ID: Byte; const TerminationEvent: THandle;
  const Timeout: Cardinal);
var
  SyncRec : TSynchronizeWithThreadRecord;
begin
  SyncRec.PassData := True;
  SyncRec.Method := nil;
  SyncRec.DataMethod := Method;
  SyncRec.Data := Data;
  SyncRec.ID := ID;
  InternalSynchronizeWithThread(SyncRec, TerminationEvent, Timeout);
end;

procedure TlvkThread.SynchronizeWithThread(const Method: TThreadMethod;
  const ID: Byte; const TerminationEvent: THandle;
  const Timeout: Cardinal);
var
  SyncRec : TSynchronizeWithThreadRecord;
begin
  SyncRec.PassData := False;
  SyncRec.Method := Method;
  SyncRec.DataMethod := nil;
  SyncRec.Data := nil;
  SyncRec.ID := ID;
  InternalSynchronizeWithThread(SyncRec, TerminationEvent, Timeout);
end;

procedure TlvkThread.Terminate;
var
  Index : Integer;
begin
  inherited Terminate;
  for Index := 0 to FCoopObjects.Count-1 do
    (FCoopObjects[Index] as IThreadCooperate).Terminate; 

  SetEvent(FTerminationEvent);
end;

procedure TlvkThread.UnRegisterCoopObject(
  const CoopObject: IThreadCooperate);
begin
  FCoopObjects.Remove(CoopObject);
end;

function TlvkThread._AddRef: Integer;
begin
  Result := -1;
end;

function TlvkThread._Release: Integer;
begin
  Result := -1;
end;

{ TlvkFuture }

procedure TlvkFuture.Clear;
begin
  if Running then
    raise ElvkFuture.Create(SCannotClear);
  FReady := False;
end;

constructor TlvkFuture.Create(AOwner: TComponent);
begin
  inherited;

  inherited OnExecute := ThreadExecute;
end;

procedure TlvkFuture.SetOnCalculate(const Value: TFutureCalculateEvent);
begin
  if Running then
    raise ElvkFuture.CreateFmt(SCannotChange, ['OnCalculate']);

  FOnCalculate := Value;
end;

procedure TlvkFuture.Start(const Data: IUnknown=nil);
begin
  if not Assigned(FOnCalculate) then
    raise ElvkFuture.Create(SNoCalculate);
  inherited;
end;

procedure TlvkFuture.ThreadExecute(const Sender: TlvkCustomThreadWrapper;
  const Thread: TlvkThread; const Data: IUnknown; var ReturnValue: LongWord);
begin
  FValue := FOnCalculate(Sender, Thread, Data);
  FReady := True;
  ReturnValue := 0;
end;

function TlvkFuture.Value: Variant;
begin
  if (not FReady) and (not Running) then
    Start;
  if Running then
    WaitFor;

  Result := FValue;
end;

end.
