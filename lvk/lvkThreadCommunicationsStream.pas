{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a communications stream object for use between two
    threads in the same process. It supports byte-for-byte writing and reading
    insitead of item-for-item reading and writing.
}
unit lvkThreadCommunicationsStream;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkThreadCommunicationsStream.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, SyncObjs, lvkComponents, 
  lvkThreadCommunicationsQueue, lvkStandardQueueItems, lvkVersion;

const
  DEFAULT_BLOCK_SIZE  = 2048;
  DEFAULT_MAX_BLOCKS  = 0;

type
  { Description:
      This is the base interface that all thread communication streams should
      support. Other classes and components in this package will use this
      interface.
  }
  IThreadCommunicationsStream = interface
    ['{5A260EF1-A298-11D5-B290-0004761A6377}']

    { Description:
        This method locks the stream. When locked, only the thread that locked it
        can access the stream, all other threads that try to use it will be
        suspended while it is locked. Typically you will lock a stream when
        you want to write data into it in a specific sequence without
        worrying if a thread might manage to read out data in mid-sequence.

        Note: The programmer must make sure that the stream is unlocked after
          the code has finished accessing the stream.
      See also:
        Unlock
    }
    procedure Lock;

    { Description:
        This method unlocks the stream after a call to Lock. The stream then
        becomes available again for other threads.
      See also:
        Lock
    }
    procedure Unlock;

    { Description:
        The Flush method writes out any pending data in the write block to the
        internal queue so that it will be picked up by the read end of the
        stream. Remember that data is buffered into "blocks" and if you write
        less data into the buffer than a block can take, the data will linger
        until more data is written. With Flush you can send the data into the
        queue at once.
      Parameters:
        Timeout - How long to wait for the queue to gain space for the item,
          in milliseconds. You can specify INFINITE if you wish the thread to
          not time out.
        TerminationEvent - A handle to a event synchronization object that
          should become signaled when the thread is terminating. You use this
          to make sure that a terminating thread doesn't stay suspended due
          to a wait for pushing data into the queue.
      Return value:
        True if it managed to flush the data, or False if it timed out before
        there was space in the queue.
      See also:
        Read, Write
    }
    function Flush(const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;

    { Description:
        The Read method will read data out from the stream. If not enough data
        is available, the method will suspend the active thread until either
        data becomes available or it times out. Note that if it has to
        wait for multiple blocks to arrive to fill the entire read buffer, then
        it will wait for a timeout for each block. This means that the
        Timeout will not be the maximum timeout to wait for, but rather the
        minimum.
      Parameters:
        Buffer - The buffer to fill with data.
        BufferSize - The amount of bytes to read into the buffer.
        AmountRead - Output parameter that will contain the number of bytes
          actually read into the buffer.
        Timeout - How long to wait for the stream to gain the necessary data,
          in milliseconds. You can specify INFINITE if you wish the thread to
          not time out.
        TerminationEvent - A handle to a event synchronization object that
          should become signaled when the thread is terminating. You use this
          to make sure that a terminating thread doesn't stay suspended due
          to a wait for reading data from the stream.
      Return value:
        True if the method managed to read all the requested data, False if
          not. Note that even if the method returns False, it might still have
          been able to read an amount of bytes from the stream.
      See also:
        Flush, Write
    }
    function Read(var Buffer; const BufferSize: LongWord;
      out AmountRead: LongWord; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;

    { Description:
        The Read method will read data out from the stream. If not enough data
        is available, the method will suspend the active thread until either
        data becomes available or it times out. Note that if it has to
        wait for multiple blocks to arrive to fill the entire read buffer, then
        it will wait for a timeout for each block. This means that the
        Timeout will not be the maximum timeout to wait for, but rather the
        minimum.
      Parameters:
        Buffer - The buffer to write data from.
        BufferSize - The amount of bytes to write from the buffer.
        AmountWritten - Output parameter that will contain the number of bytes
          actually written from the buffer.
        Timeout - How long to wait for the stream to gain space for the data,
          in milliseconds. You can specify INFINITE if you wish the thread to
          not time out.
        TerminationEvent - A handle to a event synchronization object that
          should become signaled when the thread is terminating. You use this
          to make sure that a terminating thread doesn't stay suspended due
          to a wait for space in the stream.
      Return value:
        True if the method managed to write all the requested data, False if
          not. Note that even if the method returns False, it might still have
          been able to write an amount of bytes to the stream.
      See also:
        Flush, Read
    }
    function Write(const Buffer; const BufferSize: LongWord;
      out AmountWritten: LongWord; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean;

    { Description:
        This closes the connection so that no more data is sent to it. The
        other end will be able to determine that no more data will come through
        the stream.
      See also:
        Open
    }
    procedure Close;

    { Description:
        This opens up the stream so that more data can be sent through it.
      See also:
        Close
    }
    procedure Open;
  end;

  { Description:
      This is a common class ancestor for all thread communication stream
      components and classes. Other components and classes in this package
      will rely on this class.
  }
  TlvkCustomThreadCommunicationsStream = class(TlvkComponent,
    IThreadCommunicationsStream)
  private
    FLock         : TCriticalSection;
    FQueue        : TlvkThreadCommunicationsQueue;

    FWriteSize    : LongWord;
    FWriteBlock   : PChar;
    FWriteTail    : PChar;
    FWriteHead    : PChar;
    FWriteLock    : TCriticalSection;
    FWriteClosed  : Boolean;

    FReadSize     : LongWord;
    FReadBlock    : PChar;
    FReadTail     : PChar;
    FReadHead     : PChar;
    FReadLock     : TCriticalSection;
    FReadClosed   : Boolean;

    procedure SetBlockSize(const Value: LongWord);
    function GetMaxBlocks: LongWord;
    procedure SetMaxBlocks(const Value: LongWord);

  protected
    { Description:
        This property controls how large each block is in the queue. The stream
        is divided into blocks so that the component can use the
        TlvkThreadCommunicationsQue internally. Notice that if you need to
        send a block before you've filled it up completely, use the Flush
        method.
      See also:
        MaxBlocks
    }
    property BlockSize: LongWord read FWriteSize write SetBlockSize
      default DEFAULT_BLOCK_SIZE;

    { Description:
        This property controls how many blocks that is allowed to be "in flight"
        in the communications queue before the component starts suspending
        the thread when it tries to push more data into the stream.
      See also:
        BlockSize
    }
    property MaxBlocks: LongWord read GetMaxBlocks write SetMaxBlocks
      default DEFAULT_MAX_BLOCKS;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // <ALIAS IThreadCommunicationsStream.Lock>
    procedure Lock; virtual;
    // <ALIAS IThreadCommunicationsStream.Unlock>
    procedure Unlock; virtual;

    // <ALIAS IThreadCommunicationsStream.Read@@LongWord@LongWord@LongWord@THandle>
    function Read(var Buffer; const BufferSize: LongWord;
      out AmountRead: LongWord; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; virtual;

    // <ALIAS IThreadCommunicationsStream.Write@@LongWord@LongWord@LongWord@THandle>
    function Write(const Buffer; const BufferSize: LongWord;
      out AmountWritten: LongWord; const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; virtual;

    // <ALIAS IThreadCommunicationsStream.Flush@LongWord@THandle>
    function Flush(const Timeout: LongWord=INFINITE;
      const TerminationEvent: THandle=INVALID_HANDLE_VALUE): Boolean; virtual;

    // <ALIAS IThreadCommunicationsStream.Close>
    procedure Close; virtual;

    // <ALIAS IThreadCommunicationsStream.Open>
    procedure Open; virtual;
  end;

{ Description:
    This class implements a simple thread communications stream. Threads will
    use the stream to communicate between themselves. Threads can write binary
    data into one end of the stream and threads can read out data from the
    other end.

    If a thread tries to write data into a full stream, then the
    thread will be suspended until there's room for more data in the
    stream, or the thread is terminated, whichever comes first.

    If a thread tries to read data from the stream and the stream is empty,
    then the thread will be suspended until more data get written into the
    stream, or the thread is terminated, whichever comes first.
}
  TlvkThreadCommunicationsStream = class(TlvkCustomThreadCommunicationsStream)
  published
    // <ALIAS TlvkCustomThreadCommunicationsStream.BlockSize>
    property BlockSize;
    // <ALIAS TlvkCustomThreadCommunicationsStream.MaxBlocks>
    property MaxBlocks;
  end;

implementation

uses
  Math;

{ TlvkCustomThreadCommunicationsStream }

procedure TlvkCustomThreadCommunicationsStream.Close;
begin
  FWriteLock.Acquire;
  try
    if not FWriteClosed then
    begin
      FQueue.Push(NewBooleanItem(True), True, INFINITE);
      FWriteClosed := True;
    end;
  finally
    FWriteLock.Release;
  end;
end;

constructor TlvkCustomThreadCommunicationsStream.Create(
  AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLock := TCriticalSection.Create;

  FReadSize := DEFAULT_BLOCK_SIZE;
  GetMem(FReadBlock, FReadSize);
  FReadTail := FReadBlock;
  FReadHead := FReadBlock;
  FReadLock := TCriticalSection.Create;

  FWriteSize := DEFAULT_BLOCK_SIZE;
  GetMem(FWriteBlock, FWriteSize);
  FWriteTail := FWriteBlock + FWriteSize;
  FWriteHead := FWriteBlock;
  FWriteLock := TCriticalSection.Create;

  FQueue := TlvkThreadCommunicationsQueue.Create(Self);
  FQueue.QueueSize := DEFAULT_MAX_BLOCKS;
end;

destructor TlvkCustomThreadCommunicationsStream.Destroy;
begin
  FLock.Free;
  FReadLock.Free;
  FWriteLock.Free;
  FQueue.Free;

  inherited;
end;

function TlvkCustomThreadCommunicationsStream.Flush(const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
var
  Block : IUnknown;
begin
  FWriteLock.Acquire;
  try
    if FWriteBlock = FWriteHead then
      Result := True
    else begin
      Block := NewDataItem(FWriteBlock, FWriteHead-FWriteBlock, rtMakeCopy);
      Result := FQueue.Push(Block, True, Timeout, TerminationEvent);
    end;

    if Result then
    begin
      FWriteHead := FWriteBlock;
      FWriteTail := FWriteBlock + FWriteSize;
    end;
  finally
    FWriteLock.Release;
  end;
end;

function TlvkCustomThreadCommunicationsStream.GetMaxBlocks: LongWord;
begin
  Result := FQueue.QueueSize;
end;

procedure TlvkCustomThreadCommunicationsStream.Lock;
begin
  FLock.Acquire;
end;

procedure TlvkCustomThreadCommunicationsStream.Open;
begin
  FWriteLock.Acquire;
  try
    if FWriteClosed then
    begin
      FWriteClosed := False;
      if FReadClosed then
      begin
        FReadLock.Acquire;
        try
          FReadClosed := False;
        finally
          FReadLock.Release;
        end;
      end;
    end;
  finally
    FWriteLock.Release;
  end;
end;

function TlvkCustomThreadCommunicationsStream.Read(var Buffer;
  const BufferSize: LongWord; out AmountRead: LongWord;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
var
  ToCopy    : Integer;
  BufferPtr : PChar;
  Block     : IUnknown;
  Dummy     : IUnknown;
begin
  FReadLock.Acquire;
  try
    BufferPtr := @Buffer;
    AmountRead := 0;

    while AmountRead < BufferSize do
    begin
      if FReadClosed then
      begin
        if not FWriteClosed then
          FReadClosed := False
        else begin
          Result := False;
          Exit;
        end;
      end;

      if FReadHead < FReadTail then
      begin
        ToCopy := Min(FReadTail-FReadHead, BufferSize-AmountRead);
        Move(FReadHead^, BufferPtr^, ToCopy);

        Inc(AmountRead, ToCopy);
        Inc(FReadHead, ToCopy);
        Inc(BufferPtr, ToCopy);
      end else begin
        if not FQueue.Pop(Block, False, Timeout, TerminationEvent) then
          Break;

        if Block.QueryInterface(IBooleanItem, Dummy) = S_OK then
          FReadClosed := True
        else try
          FreeMem(FReadBlock);
          FReadSize := (Block as IDataItem).Size;
          GetMem(FReadBlock, FReadSize);
          Move((Block as IDataItem).Data^, FReadBlock^, FReadSize);
          FReadHead := FReadBlock;
          FReadTail := FReadBlock + FReadSize;
        finally
          Block := nil;
        end;
      end;
    end;

    Result := (AmountRead = BufferSize);
  finally
    FReadLock.Release;
  end;
end;

procedure TlvkCustomThreadCommunicationsStream.SetBlockSize(
  const Value: LongWord);
var
  NewWriteBlock : PChar;
begin
  if Value <> FWriteSize then
  begin
    FWriteLock.Acquire;
    try
      if Value > FWriteSize then
      begin
        GetMem(NewWriteBlock, Value);
        Move(FWriteBlock^, NewWriteBlock^, (FWriteHead - FWriteBlock));
        FWriteHead := NewWriteBlock + (FWriteHead - FWriteBlock);
        FWriteTail := NewWriteBlock + Value;

        FreeMem(FWriteBlock);
        FWriteBlock := NewWriteBlock;
        FWriteSize := Value;
      end else begin
        Flush;

        FreeMem(FWriteBlock);
        GetMem(FWriteBlock, Value);
        FWriteSize := Value;
        FWriteHead := FWriteBlock;
        FWriteTail := FWriteBlock + FWriteSize;
      end;
    finally
      FWriteLock.Release;
    end;
  end;
end;

procedure TlvkCustomThreadCommunicationsStream.SetMaxBlocks(
  const Value: LongWord);
begin
  FQueue.QueueSize := Value;
end;

procedure TlvkCustomThreadCommunicationsStream.Unlock;
begin
  FLock.Release;
end;

function TlvkCustomThreadCommunicationsStream.Write(const Buffer;
  const BufferSize: LongWord; out AmountWritten: LongWord;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
var
  ToCopy    : Integer;
  BufferPtr : PChar;
begin
  FWriteLock.Acquire;
  try
    BufferPtr := @Buffer;
    AmountWritten := 0;

    while AmountWritten < BufferSize do
    begin
      if FWriteHead < FWriteTail then
      begin
        ToCopy := Min(FWriteTail-FWriteHead, BufferSize-AmountWritten);
        Move(BufferPtr^, FWriteHead^, ToCopy);

        Inc(AmountWritten, ToCopy);
        Inc(FWriteHead, ToCopy);
        Inc(BufferPtr, ToCopy);
      end else begin
        if not Flush(Timeout, TerminationEvent) then
          Break;
      end;
    end;

    if FWriteTail = FWriteHead then
      Flush(Timeout, TerminationEvent);

    Result := (AmountWritten = BufferSize);
  finally
    FWriteLock.Release;
  end;
end;

end.
