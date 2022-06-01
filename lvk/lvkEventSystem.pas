{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the components necessary to create a network event
    system.
}
unit lvkEventSystem;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 6 $
// $Archive: /Components/LVK/source/lvkEventSystem.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, ActiveX, ComObj,
  lvkSocket, lvkComponents, lvkThread, lvkSafeMem,
  lvkThreadCommunicationsQueue;

const
  DEFAULT_PORT    = 1971;

type
  { Description:
      This event type is used for events in the TlvkEventDispatcher component.
    Parameters:
      Sender - The component that called the event handler.
      SourceClient - The client id of the client that raised the original event.
        Clients can use this to singel out clients, or see if the message
        was raised by itself.
      Channel - The channel the event was raised in.
      EventName - The name of the event that was raised in the channel.
      ChannelEvent - Channel and EventName combined as Channel\EventName.
      Data - Any binary data that was sent along with the event. Can be nil if
        no data was sent. Please note that each client gets its own copy of
        the data so changing this block of data in a event handler will only
        change it in that single application.
    See also:
      TlvkEventDispatcher, TlvkEventItem.OnEvent
  }
  TOnEventEvent = procedure(const Sender: TObject;
    const SourceClient, Channel, EventName, ChannelEvent: string; const Data: ISafeMem) of object;

  { Description:
      This component class forms the basis of both the server and the client
      components. See these components for details.
    See also:
      TlvkEventServer, TlvkEventClient
  }
  TlvkCustomEventComponent = class(TlvkComponent)
  private
    FPort       : Word;
    FActive     : Boolean;
    FSetActive  : Boolean;

  protected
    procedure DoActivate; virtual; abstract;
    procedure DoDeactivate; virtual; abstract;

    procedure SetPort(const Value: Word); virtual;
    procedure SetActive(const Value: Boolean); virtual;

    { Description:
        This property controls which port the server operates on. Make sure
        you select a port which is available on the machine you're going to
        run the server on.
    }
    property Port: Word read FPort write SetPort default DEFAULT_PORT;

    { Description:
        This property controls wether the component is active or not. The server
        will not listen for connections until it becomes active, and the client
        will not connect to the server until it becomes active.
    }
    property Active: Boolean read FActive write SetActive default False;

    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        This method will simply do the same as setting the value of the Active
        property to True.
      See also:
        Active, Deactivate
    }
    procedure Activate; virtual;

    { Description:
        This method will simply do the same as setting the value of the Active
        property to False.
      See also:
        Active, Activate
    }
    procedure Deactivate; virtual;
  end;

  { Description:
      This component class forms the basis for the server component. It
      adds support for connections and maintains a listening thread and
      list of connected clients.
    See also:
      TlvkEventServer
  }
  TlvkCustomEventServer = class(TlvkCustomEventComponent)
  private
    FMainThread   : TlvkThread;
    FConnections  : TThreadList;
    FOnEvent      : TOnEventEvent;
    FEventQueue   : TlvkThreadCommunicationsQueue;

  protected
    procedure DoActivate; override;
    procedure DoDeactivate; override;

    property Connections: TThreadList read FConnections;
    procedure DispatchEvent(const SourceClient, ChannelEvent: string; const Data: ISafeMem); virtual;

    { Description:
        This event handler will be raised on the server-side for each event
        that passes through the server. You can use this if you wish to log
        events or otherwise look at the events as they flow past the server.

        Note that you cannot stop an event from being dispatched this way,
        you can simply examine the events as they flow past the server.
      See also:
        TOnEventEvent
    }
    property OnEvent: TOnEventEvent read FOnEvent write FOnEvent;
    procedure EventRaised(Sender: TObject); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This is the actual event server component. It inherits from
      TlvkCustomEventServer and simply publishes the properties relevant to the
      server.

      The event server is the component that listens for incoming connections
      from event clients elsewhere in the network. Typically you will have one
      instance of this component running in a server application on a server
      somewhere to be able to handle clients running on workstations in your
      network.

      The event server uses normal TCP/IP connections.
    See also:
      TlvkCustomEventServer, TlvkCustomEventComponent, TlvkEventClient
  }
  TlvkEventServer = class(TlvkCustomEventServer)
  published
    // <ALIAS TlvkCustomEventComponent.Port>
    property Port;
    // <ALIAS TlvkCustomEventComponent.Active>
    property Active;
    // <ALIAS TlvkCustomEventServer.OnEvent>
    property OnEvent;
  end;

  { Description:
      This component class forms the basis for the client component. It
      adds support for connecting to a server and dispatching events to
      connected dispatcher components.
    See also:
      TlvkEventClient, TlvkEventDispatcher
  }
  TlvkCustomEventClient = class(TlvkCustomEventComponent)
  private
    FServer       : string;
    FEvents       : TList;
    FThread       : TlvkThread;
    FEventQueue   : TlvkThreadCommunicationsQueue;
    FClientID     : string;
    FOnConnect    : TNotifyEvent;
    FOnDisconnect : TNotifyEvent;

  protected
    procedure DoActivate; override;
    procedure DoDeactivate; override;

    procedure SetServer(const Value: string);

    { Description:
        This property holds the ip address or hostname of the server to
        connect to.
      See also:
        Port
    }
    property Server: string read FServer write SetServer;

    procedure DoConnect;
    procedure DoDisconnect;

    { Description:
        This event handler is called when the client has disconnected from the
        server.
      See also:
        OnConnect
    }
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;

    { Description:
        This event handler is called when the client has successfully connected
        to the server.
      See also:
        OnDisconnect
    }
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;

    procedure Unsubscribe; virtual;
    procedure Subscribe; virtual;
    procedure Resubscribe; virtual;

    procedure SendCommand(const Command, ChannelEvent: string; const Data: ISafeMem=nil); virtual;
    procedure EventRaised(Sender: TObject); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        These two methods allows a client program to raise an event. By raising
        the event a message will be sent to the server which in turn will be
        dispatched to all connected clients, including the one that
        raised it in the first place. The program must supply a channel and
        event name, and can optionally provide a block of binary data to send
        along with the message, which can contain any type of data that the
        clients might need for handling the event.
      Parameters:
        Channel - The channel to raise the event in
        EventName - The event name to raise in the channel
        Data - Data to send along with the event
      See also:
        -
    }
    procedure RaiseEvent(const Channel, EventName: string; const Data: ISafeMem=nil); overload;
    // <COMBINE RaiseEvent@string@string@ISafeMem
    procedure RaiseEvent(const Channel, EventName: string; const Data: string); overload;

    { Description:
        This property gives the application a chance to learn its own id. When
        the client connects to the server, the id will be sent to the server.
        All events raised from the client will be tagged with the id of the
        client that raised it.

        Additionally, a special channel is created for each client that has
        the same name as the client id, which means it's possible to send
        messages to a single client if that is necessary. Read more about
        this feature in the help for the TlvkEventItem.Channel property.

        The client id is in the form of a GUID.

        Note: This id will be created when the component is created at runtime
        and will thus not be the same from one application session to the next,
        and should not be stored or otherwise associated with data from or for
        this client, it is only usable while the application is connected
        to the server.
      See also:
        TlvkEventItem.Channel
    }
    property ClientID: string read FClientID;
  end;

  { Description:
      This is the actual event client component. It inherits from
      TlvkCustomEventClient and simply publishes the properties relevant to the
      client.

      The client component connects to a server running somewhere on the
      network. A client can raise an event which will be sent to the server
      and dispatched to all connected clients that are interested in that
      kind of event.
    See also:
      TlvkCustomEventClient, TlvkCustomEventComponent, TlvkEventServer
  }
  TlvkEventClient = class(TlvkCustomEventClient)
  published
    // <ALIAS TlvkCustomEventComponent.Port>
    property Port;
    // <ALIAS TlvkCustomEventComponent.Active>
    property Active;
    // <ALIAS TlvkCustomEventClient.Server>
    property Server;
    // <ALIAS TlvkCustomEventClient.OnConnect>
    property OnConnect;
    // <ALIAS TlvkCustomEventClient.OnDisconnect>
    property OnDisconnect;
  end;

  TlvkCustomEventDispatcher = class;

  { Description:
      This collection item class holds information relevant to a single
      event that an event dispatcher is interested in, including the name
      of the event, the channel it's going to be raised in, and the event
      handler that will be called when this event is raised.
    See also:
      TlvkEventCollection, TlvkEventDispatcher
  }
  TlvkEventItem = class(TCollectionItem)
  private
    FChannel    : string;
    FEventName  : string;
    FOnEvent    : TOnEventEvent;

  protected
    procedure SetEventName(const Value: string); virtual;
    procedure SetChannel(const Value: string); virtual;
    function GetChannelEvent: string; virtual;
    procedure SetChannelEvent(const Value: string); virtual;

    procedure DispatchEvent(const SourceClient, ChannelEvent: string; const Data: ISafeMem); virtual;
    function GetDisplayName: string; override;

  published
    { Description:
        This property holds the name of the event to watch for. Event names
        are case insensitive and can contain the wildcard character * which
        will match zero or more characters. Thus, an application can watch
        for several events with a single event item.
      See also:
        Channel, ChannelEvent
    }
    property EventName: string read FEventName write SetEventName;

    { Description:
        This property holds the name of the channel that the event is going
        to be raised in. Channel names are case insensitive and can contain
        the wildcard character * which will match zero or more characters. Thus,
        an application can watch several channels for events with a single item.

        Note: A special channel will be created for each client with the same
        name as the client id for that client. The TlvkEventClient.ClientID
        property returns the client id for a given client connection in the
        form of a GUID in a string.

        To watch for events being sent to this special channel, a client
        must watch a channel using a wildcard * to specify it, otherwise
        the programmer must add code to construct the event items after the
        id is created for the client.
      See also:
        EventName, ChannelEvent
    }
    property Channel: string read FChannel write SetChannel;

    { Description:
        This event handler will be called once for each event matching the
        event name and channel name provided in the item, including wildcard
        matching. The parameters to the event will identify the origin of
        the event as well as the name of the channel and event that was
        raised.
      See also:
        TOnEventEvent
    }
    property OnEvent: TOnEventEvent read FOnEvent write FOnEvent;

    { Description:
        This property holds the combined values of EventName and Channel in the
        form of Channel\EventName. You can set the individual values or this
        one.
      See also:
        EventName, Channel
    }
    property ChannelEvent: string read GetChannelEvent write SetChannelEvent
      stored False;
  end;

  { Description:
      This collection class holds a list of event items associated with a single
      event dispatcher component.
    See also:
      TlvkEventItem, TlvkEventDispatcher
  }
  TlvkEventCollection = class(TOwnedCollection)
  protected
    function GetItem(const Index: Integer): TlvkEventItem;
    procedure SetItem(const Index: Integer; const Value: TlvkEventItem);

  public
    constructor Create(AOwner: TlvkCustomEventDispatcher);

    function Add: TlvkEventItem;
    function FindItemID(const ID: Integer): TlvkEventItem;
    function Insert(const Index: Integer): TlvkEventItem;
    property Items[const Index: Integer]: TlvkEventItem read GetItem
      write SetItem; default;
  end;

  { Description:
      This is the basis component for the event dispatcher component. It
      must be connected to an event client
    See also:
      TlvkEventItem, TlvkEventCollection, TlvkEventClient
  }
  TlvkCustomEventDispatcher = class(TlvkComponent)
  private
    FEventClient  : TlvkCustomEventClient;
    FEvents       : TlvkEventCollection;

  protected
    procedure SetEvents(const Value: TlvkEventCollection); virtual;
    procedure SetEventClient(const Value: TlvkCustomEventClient); virtual;

    { Description:
        This property controls which client is used for this event dispatcher.
        An event dispatcher relies on a event client to stay connected to
        a server. The dispatcher receives events from the client which will
        be handled by the items associated with the dispatcher.
      See also:
        TlvkEventClient
    }
    property EventClient: TlvkCustomEventClient read FEventClient
      write SetEventClient;

    { Description:
        This collection property holds a list of event items. When an event
        is raised in a client, sent to the server, and dispatched to connected
        clients, an event dispatcher will be given a copy of the event and will
        then iterate through all events associated with it, that is, listed
        in this collection. If any of the event items matches the channel
        and event names of the event being handled, the respective event
        handler will be called for the event.
      See also:
        TlvkEventCollection, TlvkEventItem
    }
    property Events: TlvkEventCollection read FEvents write SetEvents;

    procedure Resubscribe; virtual;
    procedure DispatchEvent(const SourceClient, ChannelEvent: string; const Data: ISafeMem); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This is the actual event dispatcher component. It inherits from
      TlvkCustomEventDispatcher and simply publishes the properties relevant
      for the dispatcher component.
    See also:
      TlvkCustomEventDispatcher
  }
  TlvkEventDispatcher = class(TlvkCustomEventDispatcher)
  published
    // <ALIAS TlvkCustomEventDispatcher.EventClient>
    property EventClient;
    // <ALIAS TlvkCustomEventDispatcher.Events>
    property Events;
  end;

  { Description:
      This exception class is used for exceptions raised in the event
      classes.
    Parameters:
      -
    See also:
      -
  }
  ElvkEvent = class(Exception);

resourcestring
  SERR_CANNOT_CHANGE_PORT_WHILE_ACTIVE    = 'Cannot change port while component is Active';
  SERR_CANNOT_CHANGE_SERVER_WHILE_ACTIVE  = 'Cannot change server while component is Active';

const
  SERVER_CHANNEL      = 'SERVER';
  CLIENT_CHANNEL      = 'CLIENT';
  CONNECTED_EVENT     = 'CLIENT CONNECTED';
  DISCONNECTED_EVENT  = 'CLIENT DISCONNECTED';

implementation

uses
  lvkRegExp, lvkSyncObjs, Windows, lvkStandardQueueItems;

const
  CMD_SUBSCRIBE     = 'SUBSCRIBE';
  CMD_UNSUBSCRIBE   = 'UNSUBSCRIBE';
  CMD_EVENT         = 'EVENT';
  CMD_CLIENTNAME    = 'CLIENTNAME';

  CMD_CONNECTED     = 1;
  CMD_DISCONNECTED  = 2;

type
  TlvkServerThread = class(TlvkThread)
  private
    FServer : TlvkCustomEventServer;

  protected
    procedure Execute; override;

  public
    constructor Create(const Server: TlvkCustomEventServer);
  end;

  TlvkServerConnectionThread = class(TlvkThread)
  private
    FServer     : TlvkCustomEventServer;
    FSocket     : IlvkSocket;
    FEvents     : TStringList;
    FClientID   : string;
    FRemoveSelf : Boolean;

  protected
    procedure Execute; override;
    procedure HandleCommand(const SourceClient, Command, ChannelEvent: string;
      const Data: ISafeMem); virtual;

    procedure Connect; virtual;
    procedure Disconnect; virtual;

    procedure DispatchEvent(const SourceClient, ChannelEvent: string; const Data: ISafeMem); virtual;
    procedure SendCommand(const SourceClient, Command, ChannelEvent: string; const Data: ISafeMem=nil); virtual;

  public
    constructor Create(const Server: TlvkCustomEventServer; const Socket: IlvkSocket);
    destructor Destroy; override;
  end;

  TlvkClientThread = class(TlvkThread)
  private
    FClient     : TlvkCustomEventClient;
    FSocket     : IlvkClientSocket;
    FSendEvent  : TlvkEvent;
    FSendQueue  : TlvkThreadCommunicationsQueue;

  protected
    procedure Execute; override;
    procedure HandleCommand(const SourceClient, Command, ChannelEvent: string;
      const Data: ISafeMem); virtual;

  public
    constructor Create(const Client: TlvkCustomEventClient);
    destructor Destroy; override;
  end;

function ChannelEventToRegExp(const ChannelEvent: string): IRegExp;
var
  RegExp  : string;
begin
  RegExp := StringReplace(ChannelEvent, '\', '\\', [rfReplaceAll]);
  RegExp := StringReplace(RegExp, '?', '[^\\]', [rfReplaceAll]);
  RegExp := StringReplace(RegExp, '*', '[^\\]*', [rfReplaceAll]);
  RegExp := '^' + RegExp + '$';
  Result := NewRegExp(RegExp);
end;

{ TlvkCustomEventComponent }

procedure TlvkCustomEventComponent.Activate;
begin
  Active := True;
end;

constructor TlvkCustomEventComponent.Create(AOwner: TComponent);
begin
  inherited;

  FPort := DEFAULT_PORT;
  FActive := False;
end;

procedure TlvkCustomEventComponent.Deactivate;
begin
  Active := False;
end;

destructor TlvkCustomEventComponent.Destroy;
begin
  Deactivate;

  inherited;
end;

procedure TlvkCustomEventComponent.Loaded;
begin
  inherited;

  Active := FSetActive;
end;

procedure TlvkCustomEventComponent.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if csLoading in ComponentState then
      FSetActive := Value
    else begin
      if Value then
      begin
        FActive := True;
        try
          if not (csDesigning in ComponentState) then
            DoActivate;
        except
          FActive := False;
          raise;
        end;
      end else begin
        try
          if not (csDesigning in ComponentState) then
            DoDeactivate;
        except
          FActive := True;
          raise;
        end;
        FActive := False;
      end;
    end;
  end;
end;

procedure TlvkCustomEventComponent.SetPort(const Value: Word);
begin
  if Value <> FPort then
  begin
    if FActive and (not (csDesigning in ComponentState)) then
      raise ElvkEvent.Create(SERR_CANNOT_CHANGE_PORT_WHILE_ACTIVE);

    FPort := Value;
  end;
end;

{ TlvkCustomEventServer }

constructor TlvkCustomEventServer.Create(AOwner: TComponent);
begin
  inherited;

  FEventQueue := TlvkThreadCommunicationsQueue.Create(Self);
  FEventQueue.ItemAdded := EventRaised;

  FConnections := TThreadList.Create;
end;

destructor TlvkCustomEventServer.Destroy;
begin
  Deactivate;
  FConnections.Free;
  FEventQueue.Free;

  inherited;
end;

procedure TlvkCustomEventServer.DispatchEvent(const SourceClient, ChannelEvent: string;
  const Data: ISafeMem);
var
  Index : Integer;
  List  : TList;
begin
  FEventQueue.Push(NewArrayItem([
    NewStringItem(SourceClient),
    NewStringItem(ChannelEvent),
    Data]));

  List := FConnections.LockList;
  try
    for Index := 0 to List.Count-1 do
      TlvkServerConnectionThread(List[Index]).DispatchEvent(SourceClient,
        ChannelEvent, Data);
  finally
    FConnections.UnlockList;
  end;
end;

procedure TlvkCustomEventServer.DoActivate;
begin
  FMainThread := TlvkServerThread.Create(Self);
end;

procedure TlvkCustomEventServer.DoDeactivate;
var
  Index   : Integer;
  Thread  : TlvkServerConnectionThread;
  List    : TList;
begin
  List := FConnections.LockList;
  try
    for Index := List.Count-1 downto 0 do
    begin
      Thread := TlvkServerConnectionThread(List[Index]);
      Thread.FRemoveSelf := False;
      Thread.FreeOnTerminate := False;
      Thread.Terminate;
      Thread.WaitFor;
      Thread.Disconnect;
      Thread.Free;
    end;
    List.Clear;
  finally
    FConnections.UnlockList;
  end;

  FMainThread.Terminate;
  FMainThread.WaitFor;
  FreeAndNil(FMainThread);
end;

procedure TlvkCustomEventServer.EventRaised(Sender: TObject);
var
  Item          : IUnknown;
  ClientID      : string;
  ChannelEvent  : string;
  DataItem      : IUnknown;
  Data          : ISafeMem;
  NewChannel    : string;
  NewEventName  : string;
begin
  while FEventQueue.Pop(Item, False, 0) do
  begin
    ClientID := ((Item as IArrayItem)[0] as IStringItem).Value;
    ChannelEvent := ((Item as IArrayItem)[1] as IStringItem).Value;
    DataItem := (Item as IArrayItem)[2];
    if Assigned(DataItem) then
      Data := DataItem as ISafeMem
    else
      Data := nil;

    if Assigned(FOnEvent) then
    begin
      if not RegExpScanf('^([^\\]+)\\([^\\]+)$', ChannelEvent, ['STRING', @NewChannel, 'STRING', @NewEventName]) then
      begin
        NewChannel := '';
        NewEventName := '';
      end;

      FOnEvent(Self, ClientID, NewChannel, NewEventName, ChannelEvent, Data);
    end;
  end;
end;

{ TlvkServerThread }

constructor TlvkServerThread.Create(const Server: TlvkCustomEventServer);
begin
  inherited Create(True);

  FServer := Server;
  FreeOnTerminate := False;

  Resume;
end;

procedure TlvkServerThread.Execute;
var
  ServerSocket  : IlvkServerSocket;
  Connection    : IlvkSocket;
begin
  ServerSocket := NewServerSocket(FServer.Port, True);
  try
    ServerSocket.Timeout := INFINITE;

    RegisterCoopObject(ServerSocket as IThreadCooperate);

    while not Terminated do
    begin
      Connection := ServerSocket.Accept;
      if Assigned(Connection) then
      begin
        Connection.Timeout := INFINITE;
        TlvkServerConnectionThread.Create(FServer, Connection);
      end;
    end;
  finally
    UnRegisterCoopObject(ServerSocket as IThreadCooperate);
  end;
end;

{ TlvkServerConnectionThread }

procedure TlvkServerConnectionThread.Connect;
var
  Data  : ISafeMem;
begin
  Data := AllocateSafeMem(Length(FClientID));
  Move(PChar(FClientID)^, Data.Pointer^, Length(FClientID));
  FServer.DispatchEvent(FClientID, Format('%s\%s', [SERVER_CHANNEL, CONNECTED_EVENT]), Data);
end;

constructor TlvkServerConnectionThread.Create(
  const Server: TlvkCustomEventServer; const Socket: IlvkSocket);
begin
  inherited Create(True);

  FRemoveSelf := True;
  FServer := Server;
  FSocket := Socket;
  FreeOnTerminate := True;

  FEvents := TStringList.Create;
  FEvents.Sorted := True;

  FServer.Connections.Add(Self);

  Resume;
end;

destructor TlvkServerConnectionThread.Destroy;
begin
  if FRemoveSelf then
    FServer.Connections.Remove(Self);
  FEvents.Free;

  inherited;
end;

procedure TlvkServerConnectionThread.Disconnect;
var
  Data  : ISafeMem;
begin
  Data := AllocateSafeMem(Length(FClientID));
  Move(PChar(FClientID)^, Data.Pointer^, Length(FClientID));
  FServer.DispatchEvent(FClientID, Format('%s\%s', [SERVER_CHANNEL, DISCONNECTED_EVENT]), Data);
end;

procedure TlvkServerConnectionThread.DispatchEvent(
  const SourceClient, ChannelEvent: string; const Data: ISafeMem);
var
  Index       : Integer;
  re          : IRegExp;
  Subscribed  : Boolean;
begin
  Subscribed := False;
  for Index := 0 to FEvents.Count-1 do
  begin
    re := ChannelEventToRegExp(FEvents[Index]);
    if re.MatchAgainst(ChannelEvent) then
    begin
      Subscribed := True;
      Break;
    end;
  end;

  if Subscribed then
    SendCommand(SourceClient, CMD_EVENT, ChannelEvent, Data);
end;

procedure TlvkServerConnectionThread.Execute;
var
  Temp          : Integer;
  SourceClient  : string;
  Command       : string;
  ChannelEvent  : string;
  Data          : ISafeMem;
begin

  RegisterCoopObject(FSocket as IThreadCooperate);
  try
    try
      try
        while not Terminated do
        begin
          // Get sourceclient
          FSocket.ReadBuffer(Temp, SizeOf(Temp));
          SetLength(SourceClient, Temp);
          FSocket.ReadBuffer(PChar(SourceClient)^, Temp);

          // Get command
          FSocket.ReadBuffer(Temp, SizeOf(Temp));
          SetLength(Command, Temp);
          FSocket.ReadBuffer(PChar(Command)^, Temp);

          // Get channel\event
          FSocket.ReadBuffer(Temp, SizeOf(Temp));
          SetLength(ChannelEvent, Temp);
          FSocket.ReadBuffer(PChar(ChannelEvent)^, Temp);

          // Get data
          FSocket.ReadBuffer(Temp, SizeOf(Temp));
          if Temp > 0 then
          begin
            Data := AllocateSafeMem(Temp);
            FSocket.ReadBuffer(Data.Pointer^, Temp);
          end else
            Data := nil;

          // Handle command
          HandleCommand(SourceClient, Command, ChannelEvent, Data);
        end;
      finally
        if not Terminated then
          Disconnect;
      end;
    except
      on E: ElvkSocket do
      begin
        if E.ErrorCode <> 3 then
          raise;
      end;
    end;
  finally
    UnRegisterCoopObject(FSocket as IThreadCooperate);
  end;
end;

procedure TlvkServerConnectionThread.HandleCommand(const SourceClient, Command,
  ChannelEvent: string; const Data: ISafeMem);
var
  re    : IRegExp;
  Index : Integer;
begin
  if Command = CMD_SUBSCRIBE then
  begin
    if FEvents.IndexOf(ChannelEvent) < 0 then
      FEvents.Add(ChannelEvent);
  end else if Command = CMD_UNSUBSCRIBE then
  begin
    re := ChannelEventToRegExp(ChannelEvent);
    for Index := FEvents.Count-1 downto 0 do
      if re.MatchAgainst(FEvents[Index]) then
        FEvents.Delete(Index);
  end else if Command = CMD_EVENT then
  begin
    FServer.DispatchEvent(SourceClient, ChannelEvent, Data);
  end else if Command = CMD_CLIENTNAME then
  begin
    FClientID := ChannelEvent;
    Connect;
  end;
end;

procedure TlvkServerConnectionThread.SendCommand(const SourceClient, Command,
  ChannelEvent: string; const Data: ISafeMem);
var
  Packet  : ISafeMem;
  Temp    : Integer;
begin
  Assert(Command <> '');
  Assert(ChannelEvent <> '');

  Packet := AllocateSafeMem;

  Temp := Length(SourceClient);
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  Packet.Stream.WriteBuffer(PChar(SourceClient)^, Temp);

  Temp := Length(Command);
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  Packet.Stream.WriteBuffer(PChar(Command)^, Temp);

  Temp := Length(ChannelEvent);
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  Packet.Stream.WriteBuffer(PChar(ChannelEvent)^, Temp);

  if Assigned(Data) then
    Temp := Data.Size
  else
    Temp := 0;
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  if Temp > 0 then
    Packet.Stream.Write(Data.Pointer^, Data.Size);

  FSocket.WriteBuffer(Packet.Pointer^, Packet.Size);
  Sleep(1);
end;

{ TlvkCustomEventClient }

constructor TlvkCustomEventClient.Create(AOwner: TComponent);
var
  GUID  : TGUID;
begin
  inherited;

  FEvents := TList.Create;
  FEventQueue := TlvkThreadCommunicationsQueue.Create(Self);
  FEventQueue.ItemAdded := EventRaised;

  CoCreateGuid(GUID);
  FClientID := GUIDToString(GUID);
end;

destructor TlvkCustomEventClient.Destroy;
begin
  FreeAndNil(FEvents);
  FEventQueue.Free;

  inherited;
end;

procedure TlvkCustomEventClient.DoActivate;
begin
  FThread := TlvkClientThread.Create(Self);
  FThread.Resume;

  Subscribe;
end;

procedure TlvkCustomEventClient.DoDeactivate;
begin
  Unsubscribe;

  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;

procedure TlvkCustomEventClient.RaiseEvent(const Channel,
  EventName: string; const Data: ISafeMem);
begin
  SendCommand(CMD_EVENT, Format('%s\%s', [Channel, EventName]), Data);
end;

procedure TlvkCustomEventClient.EventRaised(Sender: TObject);
var
  Item          : IUnknown;
  SourceClient  : string;
  ChannelEvent  : string;
  DataItem      : IUnknown;
  Data          : ISafeMem;
  Index         : Integer;
  Event         : TlvkCustomEventDispatcher;
  IntItem       : IIntegerItem;
  ArrItem       : IArrayItem;
begin
  while FEventQueue.Pop(Item, False, 0) do
  begin
    if Item.QueryInterface(IIntegerItem, IntItem) = 0 then
    begin
      case IntItem.Value of
        CMD_CONNECTED:
          DoConnect;

        CMD_DISCONNECTED:
          if Assigned(FThread) then
            DoDisconnect;
      end;
    end else if Item.QueryInterface(IArrayItem, ArrItem) = 0 then
    begin
      SourceClient := ((Item as IArrayItem)[0] as IStringItem).Value;
      ChannelEvent := ((Item as IArrayItem)[1] as IStringItem).Value;
      DataItem := (Item as IArrayItem)[2];
      if Assigned(DataItem) then
        Data := DataItem as ISafeMem
      else
        Data := nil;

      for Index := 0 to FEvents.Count-1 do
      begin
        Event := TlvkCustomEventDispatcher(FEvents[Index]);
        Event.DispatchEvent(SourceClient, ChannelEvent, Data);
      end;
    end;
  end;
end;

procedure TlvkCustomEventClient.RaiseEvent(const Channel, EventName,
  Data: string);
var
  DataSafeMem : ISafeMem;
begin
  if Data <> '' then
  begin
    DataSafeMem := AllocateSafeMem(Length(Data));
    DataSafeMem.Stream.WriteBuffer(PChar(Data)^, Length(Data));
  end else
    DataSafeMem := nil;

  RaiseEvent(Channel, EventName, DataSafeMem);
end;

procedure TlvkCustomEventClient.Resubscribe;
begin
  if Active then
  begin
    Unsubscribe;
    Subscribe;
  end;
end;

procedure TlvkCustomEventClient.SendCommand(const Command,
  ChannelEvent: string; const Data: ISafeMem);
var
  Packet  : ISafeMem;
  Temp    : Integer;
begin
  Assert(Active);
  Assert(Command <> '');
  Assert(ChannelEvent <> '');

  Packet := AllocateSafeMem;

  Temp := Length(FClientID);
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  Packet.Stream.WriteBuffer(PChar(FClientID)^, Temp);

  Temp := Length(Command);
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  Packet.Stream.WriteBuffer(PChar(Command)^, Temp);

  Temp := Length(ChannelEvent);
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  Packet.Stream.WriteBuffer(PChar(ChannelEvent)^, Temp);

  if Assigned(Data) then
    Temp := Data.Size
  else
    Temp := 0;
  Packet.Stream.WriteBuffer(Temp, SizeOf(Temp));
  if Temp > 0 then
    Packet.Stream.CopyFrom(Data.Stream, 0);

  try
    (FThread as TlvkClientThread).FSendQueue.Push(Packet);
  except
    on EAccessViolation do
      asm int 3 end;
  end;
  (FThread as TlvkClientThread).FSendEvent.SetEvent;
end;

procedure TlvkCustomEventClient.SetServer(const Value: string);
begin
  if Value <> FServer then
  begin
    if FActive and (not (csDesigning in ComponentState)) then
      raise ElvkEvent.Create(SERR_CANNOT_CHANGE_SERVER_WHILE_ACTIVE);

    FServer := Value;
  end;
end;

procedure TlvkCustomEventClient.Subscribe;
var
  Subscriptions : TStringList;
  Index         : Integer;
  EventIndex    : Integer;
  EventClient   : TlvkCustomEventDispatcher;
  EventItem     : TlvkEventItem;
  ChannelEvent  : string;
begin
  Subscriptions := TStringList.Create;
  try
    Subscriptions.Sorted := True;
    for Index := 0 to FEvents.Count-1 do
    begin
      EventClient := TlvkCustomEventDispatcher(FEvents[Index]);
      for EventIndex := 0 to EventClient.FEvents.Count-1 do
      begin
        EventItem := EventClient.FEvents[EventIndex];
        if (EventItem.FChannel <> '') and (EventItem.FEventName <> '') then
        begin
          ChannelEvent := Format('%s\%s', [EventItem.FChannel, EventItem.FEventName]);
          if Subscriptions.IndexOf(ChannelEvent) < 0 then
            Subscriptions.Add(ChannelEvent);
        end;
      end;
    end;
    { TODO 2 -oLVK -cSource : Get a list of subscriptions here }

    for Index := 0 to Subscriptions.Count-1 do
      SendCommand(CMD_SUBSCRIBE, Subscriptions[Index]);
  finally
    Subscriptions.Free;
  end;
end;

procedure TlvkCustomEventClient.Unsubscribe;
begin
  SendCommand(CMD_UNSUBSCRIBE, '*\*');
end;

procedure TlvkCustomEventClient.DoConnect;
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TlvkCustomEventClient.DoDisconnect;
begin
  FThread.WaitFor;
  FreeAndNil(FThread);
  FActive := False;
  
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

{ TlvkCustomEventDispatcher }

constructor TlvkCustomEventDispatcher.Create(AOwner: TComponent);
var
  Item  : TlvkEventItem;
begin
  inherited;

  FEvents := TlvkEventCollection.Create(Self);

  // Add default items
  Item := FEvents.Add;
  Item.Channel := SERVER_CHANNEL;
  Item.EventName := CONNECTED_EVENT;

  Item := FEvents.Add;
  Item.Channel := SERVER_CHANNEL;
  Item.EventName := DISCONNECTED_EVENT;
end;

destructor TlvkCustomEventDispatcher.Destroy;
begin
  FEvents.Free;

  inherited;
end;

procedure TlvkCustomEventDispatcher.DispatchEvent(const SourceClient, ChannelEvent: string;
  const Data: ISafeMem);
var
  Index : Integer;
  re    : IRegExp;
  Item  : TlvkEventItem;
begin
  for Index := 0 to FEvents.Count-1 do
  begin
    Item := FEvents[Index];
    if Assigned(Item.OnEvent) then
    begin
      re := ChannelEventToRegExp(Item.ChannelEvent);
      if re.MatchAgainst(ChannelEvent) then
        Item.DispatchEvent(SourceClient, ChannelEvent, Data);
    end;
  end;
end;

procedure TlvkCustomEventDispatcher.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  
  if (Operation = opRemove) and (AComponent = FEventClient) then
    FEventClient := nil;
end;

procedure TlvkCustomEventDispatcher.Resubscribe;
begin
  if Assigned(FEventClient) and FEventClient.Active then
    FEventClient.Resubscribe;
end;

procedure TlvkCustomEventDispatcher.SetEventClient(
  const Value: TlvkCustomEventClient);
begin
  if FEventClient <> Value then
  begin
    if Assigned(FEventClient) and Assigned(FEventClient.FEvents) then
      FEventClient.FEvents.Remove(Self);

    FEventClient := Value;

    if Assigned(FEventClient) and Assigned(FEventClient.FEvents) then
      FEventClient.FEvents.Add(Self);
  end;
end;

procedure TlvkCustomEventDispatcher.SetEvents(const Value: TlvkEventCollection);
begin
  if Assigned(Value) then
    FEvents.Assign(Value)
  else
    FEvents.Clear;
end;

{ TlvkClientThread }

constructor TlvkClientThread.Create(const Client: TlvkCustomEventClient);
begin
  inherited Create(True);

  FClient := Client;
  FreeOnTerminate := False;

  FSocket := NewClientSocket(FClient.Server, FClient.Port, True, False);
  FSocket.Timeout := INFINITE;

  FSendEvent := TlvkEvent.Create(nil);
  FSendEvent.Active := True;
  FSendQueue := TlvkThreadCommunicationsQueue.Create(nil);
end;

destructor TlvkClientThread.Destroy;
begin
  FreeAndNil(FSendQueue);
  FreeAndNil(FSendEvent);
  
  inherited;
end;

procedure TlvkClientThread.Execute;
var
  rc  : Integer;

  procedure ReadPacket;
  var
    Temp          : Integer;
    SourceClient  : string;
    Command       : string;
    ChannelEvent  : string;
    Data          : ISafeMem;
  begin
    // Get source client
    FSocket.ReadBuffer(Temp, SizeOf(Temp));
    SetLength(SourceClient, Temp);
    FSocket.ReadBuffer(PChar(SourceClient)^, Temp);

    // Get command
    FSocket.ReadBuffer(Temp, SizeOf(Temp));
    SetLength(Command, Temp);
    FSocket.ReadBuffer(PChar(Command)^, Temp);

    // Get channel\event
    FSocket.ReadBuffer(Temp, SizeOf(Temp));
    SetLength(ChannelEvent, Temp);
    FSocket.ReadBuffer(PChar(ChannelEvent)^, Temp);

    // Get data
    FSocket.ReadBuffer(Temp, SizeOf(Temp));
    if Temp > 0 then
    begin
      Data := AllocateSafeMem(Temp);
      FSocket.ReadBuffer(Data.Pointer^, Temp);
    end else
      Data := nil;

    // Handle command
    HandleCommand(SourceClient, Command, ChannelEvent, Data);
  end;

  procedure WritePackets;
  var
    Item  : IUnknown;
    Data  : ISafeMem;
  begin
    while FSocket.ReadyToWrite(0) and FSendQueue.Pop(Item, False, 0) do
    begin
      Data := Item as ISafeMem;
      FSocket.WriteBuffer(Data.Pointer^, Data.Size);
    end;
  end;

begin
  try
    FSocket.Events := [seReadyToRead, seReadyToWrite];

    RegisterCoopObject(FSocket as IThreadCooperate);
    try
      FClient.FEventQueue.Push(NewIntegerItem(CMD_CONNECTED));
      FClient.SendCommand(CMD_CLIENTNAME, FClient.ClientID);

      while not Terminated do
      begin
        rc := lvkWaitFor([FSocket as IWaitableObject, FSendEvent], [TerminationEvent], INFINITE);

        if (not Terminated) and (rc = WAIT_OBJECT_0) then
        begin
          if FSocket.ReadyToRead(0) then
            ReadPacket
          else if FSocket.ReadyToWrite(0) then
            WritePackets;
        end else if rc = WAIT_OBJECT_0 + 1 then
        begin
          WritePackets;
        end;
      end;

    finally
      UnRegisterCoopObject(FSocket as IThreadCooperate);
    end;
  finally
    FClient.FEventQueue.Push(NewIntegerItem(CMD_DISCONNECTED));
  end;
end;

procedure TlvkClientThread.HandleCommand(const SourceClient, Command,
  ChannelEvent: string; const Data: ISafeMem);
begin
  if Command = CMD_EVENT then
  begin
    FClient.FEventQueue.Push(NewArrayItem([
      NewStringItem(SourceClient),
      NewStringItem(ChannelEvent),
      Data]));
  end;
end;

{ TlvkEventCollection }

function TlvkEventCollection.Add: TlvkEventItem;
begin
  Result := inherited Add as TlvkEventItem;
end;

constructor TlvkEventCollection.Create(AOwner: TlvkCustomEventDispatcher);
begin
  inherited Create(AOwner, TlvkEventItem);
end;

function TlvkEventCollection.FindItemID(const ID: Integer): TlvkEventItem;
begin
  Result := inherited FindItemID(ID) as TlvkEventItem;
end;

function TlvkEventCollection.GetItem(const Index: Integer): TlvkEventItem;
begin
  Result := inherited Items[Index] as TlvkEventItem;
end;

function TlvkEventCollection.Insert(const Index: Integer): TlvkEventItem;
begin
  Result := inherited Insert(Index) as TlvkEventItem;
end;

procedure TlvkEventCollection.SetItem(const Index: Integer;
  const Value: TlvkEventItem);
begin
  inherited Items[Index] := Value;
end;

{ TlvkEventItem }

procedure TlvkEventItem.DispatchEvent(const SourceClient, ChannelEvent: string;
  const Data: ISafeMem);
begin
  if Assigned(FOnEvent) then
    FOnEvent(((Collection as TlvkEventCollection).GetOwner as TlvkCustomEventDispatcher),
      SourceClient, FChannel, FEventName, ChannelEvent, Data);
end;

function TlvkEventItem.GetChannelEvent: string;
begin
  if (FChannel <> '') and (FEventName <> '') then
    Result := Format('%s\%s', [FChannel, FEventName])
  else
    Result := '';
end;

function TlvkEventItem.GetDisplayName: string;
begin
  if (FChannel <>'') and (FEventName <> '') then
    Result := GetChannelEvent
  else
    Result := inherited GetDisplayName;
end;

procedure TlvkEventItem.SetChannel(const Value: string);
begin
  if Value <> FChannel then
  begin
    FChannel := Value;
    ((Collection as TlvkEventCollection).GetOwner as TlvkCustomEventDispatcher).Resubscribe;
  end;
end;

procedure TlvkEventItem.SetChannelEvent(const Value: string);
var
  NewChannel    : string;
  NewEventName  : string;
begin
  if RegExpScanf('^([^\\]+)\\([^\\]+)$', Value, ['STRING', @NewChannel, 'STRING', @NewEventName]) then
  begin
    Channel := NewChannel;
    EventName := NewEventName;
  end;
end;

procedure TlvkEventItem.SetEventName(const Value: string);
begin
  if Value <> FEventName then
  begin
    FEventName := Value;
    ((Collection as TlvkEventCollection).GetOwner as TlvkCustomEventDispatcher).Resubscribe;
  end;
end;

end.
