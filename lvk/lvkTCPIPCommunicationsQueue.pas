{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit implements an tcp/ip communications queue client and server
    component.
}
unit lvkTCPIPCommunicationsQueue;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkTCPIPCommunicationsQueue.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, ComObj, ActiveX, Classes, SysUtils,
  lvkPersistence, lvkThreadCommunicationsQueue, lvkThread, lvkVersion,
  lvkSocket, lvkThreadPool, lvkStandardQueueItems, lvkComponents;

const
  { Description:
      This is the default port that the TCPIP Communications Queue employs for
      the server socket.
    See also:
      TlvkTCPIPCommunicationsQueueServer,
      TlvkTCPIPCommunicationsQueueClient
  }
  DEFAULT_PORT  = 2001;

  { Description:
      This is the default host that the TCPIP Communications Queue connect to.
      It specifies the local machine, meaning inter-process communications.
    See also:
      TlvkTCPIPCommunicationsQueueServer,
      TlvkTCPIPCommunicationsQueueClient
  }
  DEFAULT_HOST  = '127.0.0.1';

type
  { Description:
      This is the client component for connecting to a TCPIPCommunicationsQueue.
      It allows the programmer to push new items into the queue and pop items
      off of the queue. It has the same functionality as the
      TlvkThreadCommunicationsQueue commponent, except that this queue uses
      TCPIP sockets to communicate and can thus be used between processes and
      even over a network.
    See also:
      TlvkTCPIPCommunicationsQueueServer,
      TlvkThreadCommunicationsQueue
  }
  TlvkTCPIPCommunicationsQueueClient = class(TlvkCustomCommunicationsQueue)
  private
    // Internal variable to hold host property value
    FHost : string;
    // Internal variable to hold port value
    FPort : Integer;

  public
    { Description:
        This constructor creates and initializes the queue client
        component. Note that a connection to the queue server is not
        opened until we actually push or pop an item.
    }
    constructor Create(AOwner: TComponent); override;

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

    // <ALIAS IThreadCommunicationsQueue.Lock>
    procedure Lock; override;
    // <ALIAS IThreadCommunicationsQueue.Unlock>
    procedure Unlock; override;

  published
    { Description:
        This property specifies the hostname or ip address of the computer
        hosting the server side of the tcpip queue. If you wish to connect to
        the same machine you're executing the client program on, use
        127.0.0.1 (default).
      See also:
        Port,
        TlvkTCPIPCommunicationsQueueServer.Port
    }
    property Host: string read FHost write FHost;

    { Description:
        This is the port that the component connects on to reach the server
        side of the queue. It must match the port setting on the server
        component.
      See also:
        Host,
        TlvkTCPIPCommunicationsQueueServer.Port
    }
    property Port: Integer read FPort write FPort default DEFAULT_PORT;
  end;

  { Description:
      This component is used as the server side of the tcpip communications queue.
      It uses a TlvkThreadCommunicationsQueue to hold the actual items, and
      this queue can be provided by the programmer, or an internal queue
      is created when needed if none is provided.
    See also:
      TlvkTCPIPCommunicationsQueueClient
  }
  TlvkTCPIPCommunicationsQueueServer = class(TlvkComponent)
  private
    // Internal variable to hold port property
    FPort       : Integer;
    // Server thread
    FThread     : TlvkThread;
    // Internal variable to server socket
    FActive     : Boolean;
    // Internal variable to hold active property while loading from stream
    FSetActive  : Boolean;
    // Internal variable to hold queue property
    FQueue      : TlvkThreadCommunicationsQueue;
    // Internal variable to signal wether the queue is owned or not
    FOwnsQueue  : Boolean;

    // Internal property write specifier for Active
    procedure SetActive(const Value: Boolean);
    // Internal property write specifier for Port
    procedure SetPort(const Value: Integer);
    // Internal property write specifier for Queue
    procedure SetQueue(const Value: TlvkThreadCommunicationsQueue);

  protected
    // Internal method to clean up after loading the object
    procedure Loaded; override;
    // Internal method to create and activate the socket
    procedure DoOpen;
    // Internal method to shut down the socket
    procedure DoClose;
    // Internal method to clean up component references
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    { Description:
        This method initializes the component and the property values but it
        does not open the queue. The programmer must use the Active property
        or the Open method to do that.
      See also:
        Destroy, Open, Close, Active
    }
    constructor Create(AOwner: TComponent); override;
    { Description:
        This destructor closes the tcpip queue and destroys all internal
        values.
      See also:
        Create, Close
    }
    destructor Destroy; override;

    { Description:
        This method opens the queue and makes the socket active. It is
        functional the same as Active:=True;
      See also:
        Close, Active
    }
    procedure Open;
    { Description:
        This method closes the queue and closes the socket, making the
        queue unavailable for any clients. It is functional the same as
        Active := False;
      See also:
        Open, Active
    }
    procedure Close;

  published
    { Description:
        This is the port that the server socket will be listening on. The
        programmer must use the same port setting on both the server
        and the client components. The component will listen on all
        available network interfaces on this port, so the programmer must make
        sure the port is available before the queue is opened.
      See also:
        TlvkTCPIPCommunicationsQueueClient.Port
    }
    property Port: Integer read FPort write SetPort;

    { Description:
        This property activates or deactivates the queue and the socket. By
        setting the value to True, the queue will become opened, and setting
        it to False will close it. It is functional the same as the methods
        Open and Close.
      See also:
        Open, Close
    }
    property Active: Boolean read FActive write SetActive;

    { Description:
        The programmer can optionally provide a queue to hold the items. This
        is useful if he/she wishes to use a descendant of the base queue
        component, or wish to configure it (for instance, to set maximum
        queue size). The queue has to be provided before the tcpip queue
        is opened. If no queue is provided when the tcpip queue is opened,
        an internal queue will be created and used, and this queue can be
        reached by this property while the tcpip queue is still open.
    }
    property Queue: TlvkThreadCommunicationsQueue read FQueue write SetQueue;
  end;

implementation

const
  cmdSendItemToServer   = 1;
  cmdGetItemFromServer  = 2;

type
  TServerThread = class(TlvkThread)
  private
    FServer : TlvkTCPIPCommunicationsQueueServer;

  protected
    procedure Execute; override;
    procedure HandleConnection(const ThreadPool: TlvkCustomThreadPool;
      const Thread: TlvkThread; const Work: IUnknown; var Result: IUnknown);

  public
    constructor Create(const Server: TlvkTCPIPCommunicationsQueueServer);
  end;
  
{ TlvkTCPIPCommunicationsQueueClient }

constructor TlvkTCPIPCommunicationsQueueClient.Create(AOwner: TComponent);
begin
  inherited;

  FHost := DEFAULT_HOST;
  FPort := DEFAULT_PORT;
end;

procedure TlvkTCPIPCommunicationsQueueClient.Lock;
begin
  // Do nothing, this queue cannot be explicitly locked due to
  // its distributed nature
end;

function TlvkTCPIPCommunicationsQueueClient.Pop(out Item: IUnknown;
  const PopFromFront: Boolean; const Timeout: LongWord;
    const TerminationEvent: THandle): Boolean;
var
  Connection    : IlvkClientSocket;
  CommandByte   : Byte;
  Stream        : TMemoryStream;
  stm           : IStream;
  DataLength    : LongWord;
  Ok            : Boolean;
begin
  Assert(FHost <> '', 'No hostname set');
  Assert(FPort <> 0, 'No port set');

  Connection := NewClientSocket(FHost, FPort);
  CommandByte := cmdGetItemFromServer;
  Connection.WriteBuffer(CommandByte, 1);
  Connection.WriteBuffer(Timeout, 4);
  Connection.WriteBuffer(PopFromFront, 1);

  Connection.ReadBuffer(DataLength, 4);

  if DataLength=0 then
  begin
    Result := False;
    Item := nil;
    Exit;
  end;

  Stream := TMemoryStream.Create;
  try
    Stream.Size := DataLength;
    Connection.ReadBuffer(Stream.Memory^, Stream.Size);
    Stream.Position := 0;
    stm := TStreamAdapter.Create(Stream);
    try
      Ok := LoadFromStream(stm, Item) = S_OK;
    finally
      stm := nil;
    end;

    Connection.WriteBuffer(Ok, 1);

    Result := Ok;
  finally
    Stream.Free;
  end;
end;

function TlvkTCPIPCommunicationsQueueClient.PopBack(out Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  Result := Pop(Item, False, Timeout);
end;

function TlvkTCPIPCommunicationsQueueClient.PopFront(out Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  Result := Pop(Item, True, Timeout);
end;

function TlvkTCPIPCommunicationsQueueClient.Push(const Item: IUnknown;
  const PushToFront: Boolean; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
var
  Connection    : IlvkClientSocket;
  CommandByte   : Byte;
  PersistStream : IPersistStream;
  Stream        : TMemoryStream;
  stm           : IStream;
  DataLength    : LongWord;
  Ok            : Boolean;
begin
  Assert(Assigned(Item), 'No item to push, cannot push a nil item over a ' +
    'tcp/ip connection');
  Assert(FHost <> '', 'No hostname set');
  Assert(FPort <> 0, 'No port set');
  if Item.QueryInterface(IPersistStream, PersistStream) <> S_OK then
    raise ElvkCommunicationsQueue.Create('Item cannot be streamed');

  Connection := NewClientSocket(FHost, FPort);
  CommandByte := cmdSendItemToServer;
  Connection.WriteBuffer(CommandByte, 1);
  Connection.WriteBuffer(Timeout, 4);
  Connection.WriteBuffer(PushToFront, 1);

  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(DataLength, 4);
    stm := TStreamAdapter.Create(Stream);
    try
      SaveToStream(stm, Item);
    finally
      stm := nil;
    end;

    DataLength := Stream.Size-4;
    Stream.Position := 0;
    Stream.WriteBuffer(DataLength, 4);

    Connection.WriteBuffer(Stream.Memory^, Stream.Size);
    Connection.ReadBuffer(Ok, 1);

    Result := Ok;
  finally
    Stream.Free;
  end;
end;

function TlvkTCPIPCommunicationsQueueClient.PushBack(const Item: IUnknown;
  const Timeout: LongWord; const TerminationEvent: THandle): Boolean;
begin
  Result := Push(Item, False, Timeout);
end;

function TlvkTCPIPCommunicationsQueueClient.PushFront(
  const Item: IUnknown; const Timeout: LongWord;
  const TerminationEvent: THandle): Boolean;
begin
  Result := Push(Item, True, Timeout);
end;

procedure TlvkTCPIPCommunicationsQueueClient.Unlock;
begin
  // Do nothing, this queue cannot be explicitly locked due
  // to its distributed nature
end;

{ TlvkTCPIPCommunicationsQueueServer }

procedure TlvkTCPIPCommunicationsQueueServer.Close;
begin
  Active := False;
end;

constructor TlvkTCPIPCommunicationsQueueServer.Create(AOwner: TComponent);
begin
  inherited;

  FPort := DEFAULT_PORT;
end;

destructor TlvkTCPIPCommunicationsQueueServer.Destroy;
begin
  Close;

  inherited;
end;

procedure TlvkTCPIPCommunicationsQueueServer.DoClose;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
  FThread := nil;

  if FOwnsQueue then
    FreeAndNil(FQueue);
end;

procedure TlvkTCPIPCommunicationsQueueServer.DoOpen;
begin
  Assert(FPort <> 0, 'No port set');

  if not Assigned(FQueue) then
  begin
    FQueue := TlvkThreadCommunicationsQueue.Create(Self);
    FOwnsQueue := True;
  end else
    FOwnsQueue := False;

  FThread := TServerThread.Create(Self);
end;

procedure TlvkTCPIPCommunicationsQueueServer.Loaded;
begin
  inherited;

  if FSetActive then
    Active := True;
end;

procedure TlvkTCPIPCommunicationsQueueServer.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FQueue) then
  begin
    Active := False;
    FQueue := nil;
  end;
end;

procedure TlvkTCPIPCommunicationsQueueServer.Open;
begin
  Active := True;
end;

procedure TlvkTCPIPCommunicationsQueueServer.SetActive(
  const Value: Boolean);
begin
  if csDesigning in ComponentState then
    FActive := Value
  else if csLoading in ComponentState then
    FSetActive := Value
  else if Value <> FActive then
  begin
    if Value then
      DoOpen
    else
      DoClose;
    FActive := Value;
  end;
end;

procedure TlvkTCPIPCommunicationsQueueServer.SetPort(const Value: Integer);
begin
  if Value <> FPort then
  begin
    if Active and (not (csDesigning in ComponentState)) then
      raise Exception.Create('Cannot change port property while tcp/ip queue ' +
        'is active');

    FPort := Value;
  end;
end;

procedure TlvkTCPIPCommunicationsQueueServer.SetQueue(
  const Value: TlvkThreadCommunicationsQueue);
begin
  if Value <> FQueue then
  begin
    if Active and (not (csDesigning in ComponentState)) then
      raise Exception.Create('Cannot change queue property while tcp/ip queue ' +
        'is active');

    FQueue := Value;
  end;
end;

{ TServerThread }

constructor TServerThread.Create(
  const Server: TlvkTCPIPCommunicationsQueueServer);
begin
  inherited Create(True);

  FServer := Server;
  FreeOnTerminate := False;

  Resume;
end;

procedure TServerThread.Execute;
var
  ServerSocket  : IlvkServerSocket;
  ThreadPool    : TlvkThreadPool;
  Connections   : TlvkThreadCommunicationsQueue;
  Connection    : IlvkSocket;
begin
  ThreadPool := nil;
  Connections := nil;
  try
    Connections := TlvkThreadCommunicationsQueue.Create(nil);
    ThreadPool := TlvkThreadPool.Create(nil);
    ThreadPool.WorkQueue := Connections;
    ThreadPool.PoolSize := 5;
    ThreadPool.OnProcessWork := HandleConnection;
    ThreadPool.Active := True;
    ServerSocket := NewServerSocket(FServer.Port);
    RegisterCoopObject(ServerSocket as IThreadCooperate);
    try
      while not Terminated do
      begin
        Connection := ServerSocket.Accept;
        if Assigned(Connection) then
          Connections.Push(Connection);
      end;
    finally
      UnRegisterCoopObject(ServerSocket as IThreadCooperate);
    end;
  finally
    Connections.Clear;
    ThreadPool.KillThreads;
    ThreadPool.Active := False;
    ThreadPool.Free;
    Connections.Free;
  end;
end;

procedure TServerThread.HandleConnection(
  const ThreadPool: TlvkCustomThreadPool; const Thread: TlvkThread;
  const Work: IUnknown; var Result: IUnknown);
var
  Socket      : IlvkSocket;
  CommandByte : Byte;

  procedure AcceptItem;
  var
    DataLength  : LongWord;
    Stream      : TMemoryStream;
    stm         : IStream;
    Item        : IUnknown;
    Timeout     : LongWord;
    PushToFront : Boolean;
    Ok          : Boolean;
  begin
    Socket.ReadBuffer(Timeout, SizeOf(Timeout));
    Socket.ReadBuffer(PushToFront, SizeOf(PushToFront));
    Socket.ReadBuffer(DataLength, SizeOf(DataLength));

    Stream := TMemoryStream.Create;
    try
      Stream.Size := DataLength;

      stm := TStreamAdapter.Create(Stream);
      try
        Socket.ReadBuffer(Stream.Memory^, DataLength);
        Stream.Position := 0;

        OleCheck(LoadFromStream(stm, Item));

        if FServer.Queue.Push(Item, PushToFront, Timeout) then
          Ok := True
        else
          Ok := False;
        Socket.WriteBuffer(Ok, SizeOf(Ok));
      finally
        stm := nil;
      end;
    finally
      Stream.Free;
    end;
  end;

  procedure SendItem;
  var
    DataLength    : LongWord;
    Stream        : TMemoryStream;
    stm           : IStream;
    Item          : IUnknown;
    Timeout       : LongWord;
    PopFromFront  : Boolean;
    Ok            : Boolean;
  begin
    Socket.ReadBuffer(Timeout, SizeOf(Timeout));
    Socket.ReadBuffer(PopFromFront, SizeOf(PopFromFront));

    if not FServer.Queue.Pop(Item, PopFromFront, Timeout, INVALID_HANDLE_VALUE) then
    begin
      DataLength := 0;
      Socket.WriteBuffer(DataLength, 4);
      Exit;
    end;

    Stream := TMemoryStream.Create;
    try
      Stream.WriteBuffer(DataLength, 4);

      stm := TStreamAdapter.Create(Stream);
      try
        SaveToStream(stm, Item);
      finally
        stm := nil;
      end;

      DataLength := Stream.Size-4;
      Stream.Position := 0;
      Stream.WriteBuffer(DataLength, 4);

      Socket.WriteBuffer(Stream.Memory^, Stream.Size);
      Socket.ReadBuffer(Ok, 1);
    finally
      Stream.Free;
    end;
  end;

begin
  Socket := Work as IlvkSocket;

  Thread.RegisterCoopObject(Socket as IThreadCooperate);
  try
    while not Thread.Terminated do
    begin
      try
        Socket.ReadBuffer(CommandByte, 1);

        case CommandByte of
          cmdSendItemToServer   : AcceptItem;
          cmdGetItemFromServer  : SendItem;
        end;
      except
        on ElvkSocket do
          Exit;
      end;
    end;
  finally
    Thread.UnRegisterCoopObject(Socket as IThreadCooperate);
  end;
end;

end.
