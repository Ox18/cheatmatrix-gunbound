{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a basic thread pool component. Basically it
    replaces dropping multiple thread components and dealing with the
    queue.
}
unit lvkThreadPool;

// $Author: Lasse V. Karlsen $
// $Revision: 11 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkThreadPool.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages, SysUtils, Classes, lvkSyncObjs, lvkThread,
  lvkThreadCommunicationsQueue, lvkStandardQueueItems,
  lvkComponents;

const
  DEFAULT_POOL_SIZE = 4;
  DEFAULT_ACTIVE    = False;

type
  TlvkCustomThreadPool = class;

{ Description:
    This event handler type is used as the main processing event handler of the
    TlvkThreadPool component. The event handler will be called in the context
    of a worker thread with the work to be done.

    Note: Instead of using the Thread.Terminated flag to decide wether to
      abort sooner-than-completed or not, the code in the event handler should
      check ThreadPool.Terminated instead. Thread.Terminated is used
      to ask the thread to release itself when it has processed the current
      item, not to forcibly terminate the thread.
  Parameters:
    ThreadPool - The owning thread pool component that this event handler is
      called from.
    Thread - The thread that the code runs in.
    Work - The work to process, a single item of work that came from the
      WorkQueue queue of the thread pool.
    Result - The result of the processing should be returned back to the thread.
      The work item + the result item will be pushed into the ResultQueue (if
      one is provided). If no resultqueue is used, the event handler should
      just return nil (the default value).
  See also:
    TlvkCustomThreadPool, TlvkThreadPool
}
  TlvkProcessWorkEvent = procedure(const ThreadPool: TlvkCustomThreadPool;
    const Thread: TlvkThread; const Work: IUnknown;
    var Result: IUnknown) of object;

{ Description:
    This is the base, custom, thread pool component. It implements the core
    code that deals with processing work items in worker threads. Work items
    must be passed to the pool through a work queue.
  See also:
    TlvkThreadPool
}
  TlvkCustomThreadPool = class(TlvkComponent)
  private
    FActive           : Boolean;
    FSetActive        : Boolean;

    FConfigCritSec    : TlvkCriticalSection;
    FPoolSize         : Integer;
    FThreads          : array of TlvkThread;
    FWorkQueue        : TlvkThreadCommunicationsQueue;
    FResultQueue      : TlvkThreadCommunicationsQueue;
    FThreadsInUse     : Integer;
    FOnProcessWork    : TlvkProcessWorkEvent;
    FTerminated       : Boolean;

    FSynchronization  : TlvkCriticalSection;

    procedure SetActive(const Value: Boolean);
    procedure SetPoolSize(const Value: Integer);
    procedure SetWorkQueue(const Value: TlvkThreadCommunicationsQueue);
    procedure SetResultQueue(const Value: TlvkThreadCommunicationsQueue);
    procedure SetProcessWork(const Value: TlvkProcessWorkEvent);

    procedure DoActivate;
    procedure DoDeactivate;

    procedure AdjustPool;
    procedure CreateThreads;
    procedure DestroyThreads;

  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;

    { Description:
        This property is used to activate or deactivate the thread pool.
        By activating the thread pool, the threads will be created and will
        immediately start processing any pending work.

        Note: By deactivating the pool, all currently running threads in the
          pool will be asked to terminate. The Terminated property of the pool
          component will be set to True in this case.
        Note: Before you can activate the thread pool you must make sure that:
          * You have a ProcessWorkEvent event handler set.
          * You have a WorkQueue provided.
      See also:
        Terminated
    }
    property Active: Boolean read FActive write SetActive
      default DEFAULT_ACTIVE;

    { Description:
        This property specifies how many threads will be kept in the pool.
        At all times, this number of threads will be kept in the pool, even
        if just one item of work is provided at a time. The threads will
        be suspended when they aren't processing any work items though so
        they shouldn't use any cpu cycles.

        Note: Increasing this property while the thread pool is activated will
          create more threads and if there's work available they will
          immediately start processing work.
        Note: Decreasing this property while the thread pool is activated will
          release threads until this many threads are kept in the pool. If
          threads that are to be released are currently processing work,
          the program will wait for the processing to complete. This could mean
          that if there's lengthy processing involved, the main thread could
          become unresponsive. Unless absolutely necessary, don't decrease the
          PoolSize property while the thread pool is active.
    }
    property PoolSize: Integer read FPoolSize write SetPoolSize
      default DEFAULT_POOL_SIZE;

    { Description:
        This property allows the programmer to provide the thread pool with a
        queue. This queue will provide the thread pool with work items to
        process. A thread pool must have a work queue available, otherwise
        it cannot be activated. Work items in the queue will be processed
        inside worker threads. If the pool contains 4 worker threads, up to
        4 items will be processed simultaneously.
      See also:
        Active, OnProcessWork, ResultQueue
    }
    property WorkQueue: TlvkThreadCommunicationsQueue read FWorkQueue
      write SetWorkQueue;

    { Description:
        If the processing event handler returns a result and this result should
        be used in some way, the programmer can provide a result queue to the
        thread pool.

        The initial work item that was processed + the result that the
        processing yielded will be pushed into this queue as a single item of
        type IArrayItem. The first item in the array will be the work item,
        and the second item will be the result.
      See also:
        WorkQueue, IArrayItem, OnProcessWork
    }
    property ResultQueue: TlvkThreadCommunicationsQueue read FResultQueue
      write SetResultQueue;

    { Description:
        This event handler is called from within the worker threads to process
        one work item. The event handler can optionally return a result as well
      See also:
        Active, TlvkProcessWorkEvent
    }
    property OnProcessWork: TlvkProcessWorkEvent read FOnProcessWork
      write SetProcessWork;

  public
    { Description:
        This will create the thread pool component and initialize the pool
        size to 4 threads.
      See also:
        Destroy
    }
    constructor Create(AOwner: TComponent); override;

    { Description:
        This will destroy the thread pool. All threads currently running will
        be terminated.
      See also:
        Create
    }
    destructor Destroy; override;

    { Description:
        This method will kill all threads in the same manner as the destructor
        Destroy will do and the thread pool will become deactivated.

        Reactivating the pool will create the threads. Any work items they
        were currently processing will have become lost however, unless
        the process event handlers saved them away.
      See also:
        Destroy
    }
    procedure KillThreads;

    { Description:
        This property simply returns how many threads are currently running.
        A program can use this to monitor the workload of the thread pool.
        If the thread pool is running at full capacity most of the time,
        logic could be added to provide more threads.
    }
    property ThreadsInUse: Integer read FThreadsInUse;

    { Description:
        This property will return True whenever the destructor Destroy or
        the KillThreads method have been called. Threads should check this
        property periodically and abort as soon as possible if it is set.
      See also:
        TlvkProcessWorkEvent, Destroy, KillThreads
    }
    property Terminated: Boolean read FTerminated;
  end;

  { Description:
      This is the thread pool component that the programmer will want to use.
      It simply descends from TlvkCustomThreadPool and publishes the properties
      of it.
    See also:
      TlvkCustomThreadPool
  }
  TlvkThreadPool = class(TlvkCustomThreadPool)
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
    // <ALIAS TlvkCustomThreadPool.Active>
    property Active;
    // <ALIAS TlvkCustomThreadPool.PoolSize>
    property PoolSize;
    // <ALIAS TlvkCustomThreadPool.WorkQueue>
    property WorkQueue;
    // <ALIAS TlvkCustomThreadPool.ResultQueue>
    property ResultQueue;
    // <ALIAS TlvkCustomThreadPool.OnProcessWork>
    property OnProcessWork;
  end;

implementation

type
  TThreadWorker = class(TlvkThread)
  private
    FOwner  : TlvkCustomThreadPool;

  protected
    procedure Execute; override;

  public
    constructor Create(const Owner: TlvkCustomThreadPool);
  end;

{ TlvkCustomThreadPool }

procedure TlvkCustomThreadPool.AdjustPool;
var
  OldSize : Integer;
  Index   : Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  if Active then
  begin
    if Length(FThreads) < FPoolSize then
    begin
      OldSize := Length(FThreads);
      SetLength(FThreads, FPoolSize);
      for Index := OldSize to FPoolSize-1 do
        FThreads[Index] := TThreadWorker.Create(Self);
    end else if Length(FThreads) > FPoolSize then
    begin
      OldSize := Length(FThreads);
      for Index := FPoolSize to OldSize-1 do
        FThreads[Index].Terminate;
      for Index := FPoolSize to OldSize-1 do
        FThreads[Index].WaitFor;
      for Index := FPoolSize to OldSize-1 do
        FThreads[Index].Free;
      SetLength(FThreads, FPoolSize);
    end;
  end;
end;

constructor TlvkCustomThreadPool.Create(AOwner: TComponent);
begin
  inherited;

  FPoolSize := DEFAULT_POOL_SIZE;
  FActive := DEFAULT_ACTIVE;

  SetLength(FThreads, 0);
  FConfigCritSec := TlvkCriticalSection.Create(Self);
  FConfigCritSec.Active := True;
end;

procedure TlvkCustomThreadPool.CreateThreads;
var
  i : Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  SetLength(FThreads, FPoolSize);
  for i := 0 to FPoolSize-1 do
    FThreads[i] := TThreadWorker.Create(Self);
end;

destructor TlvkCustomThreadPool.Destroy;
begin
  Active := False;
  FSynchronization.Free;

  inherited;
end;

procedure TlvkCustomThreadPool.DestroyThreads;
var
  i : Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  for i := 0 to Length(FThreads)-1 do
    FThreads[i].Terminate;
  for i := 0 to Length(FThreads)-1 do
  begin
    FThreads[i].WaitFor;
    FThreads[i].Free;
  end;
  SetLength(FThreads, 0);
end;

procedure TlvkCustomThreadPool.DoActivate;
begin
  if not Assigned(FWorkQueue) then
    raise ElvkThreadWrapper.Create('Cannot activate a thread pool without a work queue');
  if not Assigned(FOnProcessWork) then
    raise ElvkThreadWrapper.Create('Cannot activate a thread pool without a work event handler');

  if csDesigning in ComponentState then
    Exit;
  FTerminated := False;
  CreateThreads;
end;

procedure TlvkCustomThreadPool.DoDeactivate;
begin
  if csDesigning in ComponentState then
    Exit;

  FTerminated := True;
  DestroyThreads;
end;

procedure TlvkCustomThreadPool.KillThreads;
var
  Index : Integer;
begin
  if not Active then
    raise ElvkThreadWrapper.Create('Can only kill threads while thread pool is active');
  if csDesigning in ComponentState then
    Exit;

  FTerminated := True;
  for Index := Low(FThreads) to High(FThreads) do
    FThreads[Index].Terminate;
  for Index := Low(FThreads) to High(FThreads) do
  begin
    FThreads[Index].WaitFor;
    FThreads[Index].Free;
  end;
  SetLength(FThreads, 0);
end;

procedure TlvkCustomThreadPool.Loaded;
begin
  inherited;

  if FSetActive then
    Active := True;
end;

procedure TlvkCustomThreadPool.Notification(AComponent: TComponent;
  Operation: TOperation); 
begin
  case Operation of
    opRemove:
      begin
        if AComponent = WorkQueue then
        begin
          Active := False;
          WorkQueue := nil;
        end;
        if AComponent = ResultQueue then
        begin
          Active := False;
          ResultQueue := nil;
        end;
      end;
  end;
end;

procedure TlvkCustomThreadPool.SetActive(const Value: Boolean);
begin
  if csLoading in ComponentState then
    FSetActive := Value
  else if Value <> FActive then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Value then
        DoActivate
      else
        DoDeactivate;
    end;

    FActive := Value;
  end;
end;

procedure TlvkCustomThreadPool.SetPoolSize(const Value: Integer);
begin
  if Value <> FPoolSize then
  begin
    FConfigCritSec.Acquire;
    try
      FPoolSize := Value;
      AdjustPool;
    finally
      FConfigCritSec.Release;
    end;
  end;
end;

procedure TlvkCustomThreadPool.SetProcessWork(
  const Value: TlvkProcessWorkEvent);
begin
  if Active and (not (csDesigning in ComponentState)) then
    raise Exception.Create('Can not change work event handler while thread pool is active');

  FOnProcessWork := Value;

  if Active and (not Assigned(FOnProcessWork)) then
    Active := False;
end;

procedure TlvkCustomThreadPool.SetResultQueue(
  const Value: TlvkThreadCommunicationsQueue);
begin
  if Active and (not (csDesigning in ComponentState)) then
    raise Exception.Create('Can not change result queue while thread pool is active');

  FResultQueue := Value;
end;

procedure TlvkCustomThreadPool.SetWorkQueue(
  const Value: TlvkThreadCommunicationsQueue);
begin
  if Active and (not (csDesigning in ComponentState)) then
    raise Exception.Create('Can not change work queue while thread pool is active');

  FWorkQueue := Value;

  if Active and (not Assigned(FWorkQueue)) then
    Active := False;
end;

{ TThreadWorker }

constructor TThreadWorker.Create(
  const Owner: TlvkCustomThreadPool);
begin
  inherited Create(True);

  FOwner := Owner;

  Resume;
end;

procedure TThreadWorker.Execute;
var
  Work    : IUnknown;
  Result  : IUnknown;
begin
  while not Terminated do
  begin
    if not FOwner.FWorkQueue.Pop(Work, False, INFINITE, TerminationEvent) then
      Break;

    InterlockedIncrement(FOwner.FThreadsInUse);
    try
      Result := nil;
      FOwner.FOnProcessWork(FOwner, Self, Work, Result);

      if Assigned(FOwner.FResultQueue) then
        FOwner.FResultQueue.Push(NewArrayItem([Work, Result]));
    finally
      InterlockedDecrement(FOwner.FThreadsInUse);
    end;
  end;
end;

end.
