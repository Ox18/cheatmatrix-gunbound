{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the client and server socket classes.
}
unit lvkSocket;

// $Author: Lasse V. Karlsen $
// $Revision: 11 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSocket.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Sockets,
  {$ENDIF}
  Windows, Classes, SysUtils, WinSock;

type
  { Description:
      See IlvkSocket.Events for more information.
  }
  TSocketEvent = (seReadyToRead, seReadyToWrite);

  { Description:
      See IlvkSocket.Events for more information.
  }
  TSocketEvents = set of TSocketEvent;

  { Description:
      This type is used in the IlvkSocket.Options property to control what
      options are enabled on the socket. If one of the members are present
      in the set, that option is enabled on the socket. Remove a member to
      disable the option.
    See also:
      IlvkSocket.Options
  }
  TSocketOption = (
    // If this option is enabled, the Nagle algorithm is enabled.
    soNagleAlgorithm);
  // <COMBINE TSocketOption>
  TSocketOptions = set of TSocketOption;

  { Description:
      This interface is the basis for the client and the server interfaces for
      the lvkSocket class. It defines the common properties and methods.
    See also:
      IlvkClientSocket, IlvkServerSocket
  }
  IlvkSocket = interface
    ['{73CAA812-4D0E-4211-A256-7AFCFCAEDE11}']

    { Description:
        This method will close the socket connection. If this is a client
        socket then the connection will be broken. If it's a server
        socket then it will stop listening and be closed.
    }
    procedure Close;

    // <COMBINE Timeout>
    function GetTimeout: Cardinal;
    // <COMBINE Timeout>
    procedure SetTimeout(const Value: Cardinal);

    { Description:
        This property holds the timeout value for the socket. A socket
        initially starts with a value of INFINITE which means that any read
        or writes on the socket, as well as calls to Accept on the server
        socket will block for as long as is needed.

        If a socket is given a specific timeout value, in milliseconds, then
        a call to Read, Write or Accept will time out after the given
        interval has elapsed.

        Read this property to get the current timeout value and write to it
        to change the timeout value.
      See also:
        GetTimeout, SetTimeout
    }
    property Timeout: Cardinal read GetTimeout write SetTimeout;

    // <COMBINE RemotePort>
    function GetRemotePort: Word;

    { Description:
        This property returns the remote port for the socket. The local end
        is inside the current application, and the remote end is connected
        to the server, wherever that might be.

        For a client socket, this will be the port that the server is listening
        on and the client was told to connect to.

        For a server socket, this will be 0.

        For a socket returned from Accept, this will be the port that the
        client at the other end used when connecting.

        In IlvkSocket, this property is readonly. In IlvkClientSocket this
        is both read and write.
      See also:
        RemoteHostName, RemoteAddress
    }
    property RemotePort: Word read GetRemotePort;

    // <COMBINE RemoteHostName>
    function GetRemoteHostName: string;

    { Description:
        This property returns the remote hostname for the socket. The local end
        is inside the current application, and the remote end is connected
        to the server, wherever that might be.

        For a client socket, this will be the hostname that the server is
        listening on and the client was told to connect to.

        For a server socket, this will be ''.

        For a socket returned from Accept, this will be the hostname that the
        client at the other end used when connecting.

        In IlvkSocket, this property is readonly. In IlvkClientSocket this
        is both read and write.
      See also:
        RemotePort, RemoteAddress
    }
    property RemoteHostName: string read GetRemoteHostName;

    // <COMBINE RemoteAddress>
    function GetRemoteAddress: string;

    { Description:
        This property returns the remote address for the socket. The local end
        is inside the current application, and the remote end is connected
        to the server, wherever that might be.

        For a client socket, this will be the address that the server is
        listening on and the client was told to connect to.

        For a server socket, this will be ''.

        For a socket returned from Accept, this will be the address that the
        client at the other end used when connecting.

        In IlvkSocket, this property is readonly. In IlvkClientSocket this
        is both read and write.
      See also:
        RemotePort, RemoteHostName
    }
    property RemoteAddress: string read GetRemoteAddress;

    // <COMBINE LocalPort>
    function GetLocalPort: Word;

    { Description:
        This property returns the local port for the socket. The local end
        is inside the current application, and the remote end is connected
        to the server, wherever that might be.

        For a client socket, this will be the port for the client
        connection / computer, a random number assigned by the WinSock
        subsystem when the connection was made.

        For a server socket, this will be port that the server is listening
        on.

        For a socket returned from Accept, this will be the port that the
        server is listening on.

        In IlvkSocket, this property is readonly. In IlvkServerSocket this
        is both read and write.
      See also:
        LocalPort, LocalAddress
    }
    property LocalPort: Word read GetLocalPort;

    // <COMBINE LocalHostName>
    function GetLocalHostName: string;

    { Description:
        This property returns the local hostname for the socket. The local end
        is inside the current application, and the remote end is connected
        to the server, wherever that might be.

        For a client socket, this will be the hostname for the client
        connection / computer.

        For a server socket, this will be hostname that the socket is listening
        on.

        For a socket returned from Accept, this will be the hostname that the
        server accepted the connection on.

        In IlvkSocket, this property is readonly. In IlvkServerSocket this
        is both read and write.
      See also:
        LocalHostName, LocalAddress
    }
    property LocalHostName: string read GetLocalHostName;

    // <COMBINE LocalAddress>
    function GetLocalAddress: string;

    { Description:
        This property returns the local address for the socket. The local end
        is inside the current application, and the remote end is connected
        to the server, wherever that might be.

        For a client socket, this will be the address for the client
        connection / computer.

        For a server socket, this will be address that the socket is listening
        on.

        For a socket returned from Accept, this will be the address that the
        server accepted the connection on.

        In IlvkSocket, this property is readonly. In IlvkServerSocket this
        is both read and write.
      See also:
        LocalPort, LocalHostName
    }
    property LocalAddress: string read GetLocalAddress;

    { Description:
        This method will return True if the socket is ready to read data from.
        This means that there is data waiting to be read from the socket. You
        can optionally specify a timeout to have the check wait for the
        given time to see if the socket becomes available for reading in
        that time.
      Parameters:
        Timeout - The number of milliseconds to wait for the socket to become
          available for reading.
      See also:
        Read, ReadBuffer
    }
    function ReadyToRead(const Timeout: Cardinal=0): Boolean;

    { Description:
        This method will read a block of data from the socket, up to the
        specified size. Less data can also be read if there is less data
        available from the socket. If the function returns 0, then the
        socket connection was disconnected.

        If no data is available in the socket when a call to Read is made, then
        Read will block until data becomes available or the connection is
        closed.

        The method returns the number of bytes read, or 0 if the connection
        was disconnected.
      Parameters:
        Data - The buffer to read data into.
        Size - The amount of bytes to read.
      See also:
        ReadBuffer
    }
    function Read(var Data; const Size: Integer): Integer;

    { Description:
        This method wraps a call to Read inside a check to see if it was
        able to read all the data specified.

        If Read was not able to read the given amount of bytes, ReadBuffer
        will raise an exception.
      Parameters:
        Data - The buffer to read data into.
        Size - The amount of bytes to read.
      See also:
        Read
    }
    procedure ReadBuffer(var Data; const Size: Integer);

    { Description:
        This method will return True if the socket is ready to write data to.
        This means that the buffers are not full and the socket is connected.
        You can optionally specify a timeout to have the check wait for the
        given time to see if the socket becomes available for writing in
        that time.
      Parameters:
        Timeout - The number of milliseconds to wait for the socket to become
          available for writing.
      See also:
        Write, WriteBuffer, WriteString
    }
    function ReadyToWrite(const Timeout: Cardinal=0): Boolean;

    { Description:
        This method will write a block of data to the socket, up to the
        specified size. Less data can also be written if there is less space
        available in the socket buffers. If the function returns 0, then the
        socket connection was disconnected.

        If no space is available in the socket when a call to Write is made,
        then Write will block until it becomes available or the connection is
        closed.

        The method returns the number of bytes written, or 0 if the connection
        was disconnected.
      Parameters:
        Data - The buffer to write data from.
        Size - The amount of bytes to write.
      See also:
        WriteBuffer, WriteString
    }
    function Write(const Data; const Size: Integer): Integer;

    { Description:
        This method wraps a call to Write inside a check to see if it was
        able to write all the data specified.

        If Write was not able to write the given amount of bytes, WriteBuffer
        will raise an exception.
      Parameters:
        Data - The buffer to write data from.
        Size - The amount of bytes to write.
      See also:
        Write, WriteString
    }
    procedure WriteBuffer(const Data; const Size: Integer);

    { Description:
        This procedure is similar to WriteBuffer in that it will write the
        data given and raise an exception if not all the data could be
        written.

        The difference is that WriteString is given a string to write.

        Note: If the string is empty, no data will be written.
        Note: Only the contents of the string is written. The length of the
          string is not so to read it back you need a sure way of finding out
          where the string ends.
      Parameters:
        s - The string to write.
      See also:
        Write, WriteBuffer
    }
    procedure WriteString(const s: string);

    // <COMBINE Events>
    function GetEvents: TSocketEvents;
    // <COMBINE Events>
    procedure SetEvents(const Value: TSocketEvents);
    { Description:
        This property controls what kind of events this object supports. Events
        in this context is socket events, not delphi or windows events. The
        socket is a waitable object (see IWaitableObject), and this property
        controls what kind of events you can wait for.

        Note that setting this property to anything but an empty set turns the
        socket into an asynchronous socket. This means that reads and writes
        will return sooner, possibly with unfinished work, than if you're
        leaving the set empty and using the socket as a blocking socket.

        Available values in this set are:

          * seReadyToRead - Wait will be satisfied if data can be read from the
            socket.
          * seReadyToWrite - Wait will be satisfied if data can be written to
            the socket.
      See also:
        IWaitableObject, lvkWaitFor@array of IWaitableObject@Cardinal@Boolean
    }
    property Events: TSocketEvents read GetEvents write SetEvents;

    // <COMBINE Options>
    function GetOptions: TSocketOptions;
    // <COMBINE Options>
    procedure SetOptions(const Value: TSocketOptions);
    { Description:
        This property controls what options are enabled on the socket. Include
        items in the set to enable those options, remove them to disable
        those options.
      See also:
        TSocketOptions
    }
    property Options: TSocketOptions read GetOptions write SetOptions;
  end;

  { Description:
      This is the interface used to operate on a client socket. It inherits
      from IlvkSocket so it contains the same code. It also opens up some of
      the properties for writing and adds some new properties and methods
      specifically for a client socket.
    See also:
      IlvkSocket, IlvkServerSocket
  }
  IlvkClientSocket = interface(IlvkSocket)
    ['{0A3808AF-F4FF-4794-9F9D-B0E7B9FECC1C}']

    { Description:
        This method connects the client socket to the other end, typically
        a server. Before you can call Connect, you must configure the client
        socket with a hostname or address, and a port number to use
        when connecting.
      See also:
        Disconnect
    }
    procedure Connect;

    { Description:
        This method will disconnect the client. Unsent data will not be
        sent.
    }
    procedure Disconnect;

    // <COMBINE IsConnected>
    function GetIsConnected: Boolean;
    // <COMBINE IsConnected>
    procedure SetIsConnected(const Value: Boolean);

    { Description:
        This property holds wether or not the client socket is connected or not.
        Set it to True to connect and False to disconnect, or read it to get
        the current connected state.
    }
    property IsConnected: Boolean read GetIsConnected write SetIsConnected;

    // <COMBINE IlvkSocket.RemotePort>
    procedure SetRemotePort(const Value: Word);
    // <ALIAS IlvkSocket.RemotePort>
    property RemotePort: Word read GetRemotePort write SetRemotePort;

    // <COMBINE IlvkSocket.RemoteHostName>
    procedure SetRemoteHostName(const Value: string);
    // <ALIAS IlvkSocket.RemoteHostName>
    property RemoteHostName: string read GetRemoteHostName write SetRemoteHostName;

    // <COMBINE IlvkSocket.RemoteAddress>
    procedure SetRemoteAddress(const Value: string);
    // <ALIAS IlvkSocket.RemoteAddress>
    property RemoteAddress: string read GetRemoteAddress write SetRemoteAddress;
  end;

  { Description:
      This is the interface used to operate on a server socket. It inherits
      from IlvkSocket so it contains the same code. It also opens up some of
      the properties for writing and adds some new properties and methods
      specifically for a server socket.
    See also:
      IlvkSocket, IlvkClientSocket
  }
  IlvkServerSocket = interface(IlvkSocket)
    ['{EA20849A-8EE5-4B66-A65A-CF133E3A62D3}']

    // <COMBINE IsListening>
    function GetIsListening: Boolean;
    // <COMBINE IsListening>
    procedure SetIsListening(const Value: Boolean);
    { Description:
        This property holds the current status of the server socket, listening
        or not, as a boolean value.

        Read the property to get the current listening status.
        Write to the property to start the socket listening or stop
        listening.
      See also:
        Listen, StopListening
    }
    property IsListening: Boolean read GetIsListening write SetIsListening;

    { Description:
        This method will start the socket listening on the specified port
        for connections.

        If the hostname/address properties are blank strings, then the
        socket will listen on all ip addresses available on the server
        machine. If a specified address or hostname is given, only that
        ip address will be listened on.
      See also:
        IsListening, StopListening
    }
    procedure Listen;

    { Description:
        This method will stop the socket listening on the specified port
        for connections.
      See also:
        IsListening, Listen
    }
    procedure StopListening;

    { Description:
        This method will wait for a incoming connection and "accept" the
        connection. The connection will be handled in a separate socket
        object which will deal with reading from and writing to the
        connected client. This socket object will be returned.

        If you register the socket object as a thread cooperation object
        with TlvkThread, then if the thread is currently waiting for a
        connection when the thread is asked to terminate, then Accept
        will return nil.

        Usually, you'll call this method as part of a thread. If you terminate
        the thread, the usual way to cancel the Accept from blocking is to
        simply close the socket. In this case, Accept will return NIL.
      See also:
        Listen, StopListening
    }
    function Accept: IlvkSocket;

    // <COMBINE IlvkSocket.LocalPort>
    procedure SetLocalPort(const Value: Word);
    // <ALIAS IlvkSocket.LocalPort>
    property LocalPort: Word read GetLocalPort write SetLocalPort;

    // <COMBINE IlvkSocket.LocalHostName>
    procedure SetLocalHostName(const Value: string);
    // <ALIAS IlvkSocket.LocalHostName>
    property LocalHostName: string read GetLocalHostName write SetLocalHostName;

    // <COMBINE IlvkSocket.LocalAddress>
    procedure SetLocalAddress(const Value: string);
    // <ALIAS IlvkSocket.LocalAddress>
    property LocalAddress: string read GetLocalAddress write SetLocalAddress;
  end;

  { Description:
      This class inherits from TStream and wraps around a IlvkSocket object
      so that the socket object can be used for purposes specifically available
      to streams.

      Note that all operations which involves getting the current position,
      getting the current size and seeking will not work as in a normal
      TStream.
  }
  TlvkSocketStream = class(TStream)
  private
    FSocket   : IlvkSocket;
    FPosition : Integer;

  public
    { Description:
        This constructor creates an instance of TlvkSocketStream and
        configures it to use the specified socket object.
      Parameters:
        Socket - The socket object to use. Can be a client socket object or
          a socket object returned from Accept.
    }
    constructor Create(const Socket: IlvkSocket);

    { Description:
        This method functions like described for
        <EXTLINK borland://TStream_Read>TStream.Read</EXTLINK>.

        It wraps a call to IlvkSocket.Read.
    }
    function Read(var Buffer; Count: Longint): Longint; override;
    { Description:
        This method functions like described for
        <EXTLINK borland://TStream_Write>TStream.Write</EXTLINK>.

        It wraps a call to IlvkSocket.Write.
    }
    function Write(const Buffer; Count: Longint): Longint; override;
    { Description:
        This method functions like described for
        <EXTLINK borland://TStream_Seek>TStream.Seek</EXTLINK>, mostly.

        As a position it will return the number of bytes written/read from
        the socket was created. If you reposition, it will just adjust
        the "position" value and return that, but no actual seeking is done.
    }
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;

    {$IFDEF DELPHI6UP}
    { Description:
        This method functions like described for
        <EXTLINK borland://TStream_Seek>TStream.Seek</EXTLINK>, mostly.

        As a position it will return the number of bytes written/read from
        the socket was created. If you reposition, it will just adjust
        the "position" value and return that, but no actual seeking is done.
    }
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {$ENDIF}
  end;

  { Description:
      This exception is used for all exceptions raised from within the lvkSocket
      classes and support code.
  }
  ElvkSocket = class(Exception)
  private
    FErrorCode  : Integer;

  public
    { Description:
        Specialized form of Create constructor that takes an additional
        ErrorCode parameter which is stored for future reference in the
        exception object.
      Parameters:
        ErrorCode - The socket error code.
        Message - the text of the exception message.
      See also:
        CreateFmt, ErrorCode
    }
    constructor Create(const ErrorCode: Integer; const Message: string);

    { Description:
        Specialized form of CreateFmt constructor that takes an additional
        ErrorCode parameter which is stored for future reference in the
        exception object.
      Parameters:
        ErrorCode - The socket error code.
        Fmt - The format string.
        Args - List of arguments for the format string.
      See also:
        Create, ErrorCode
    }
    constructor CreateFmt(const ErrorCode: Integer; const Fmt: string; const Args: array of const);

    { Description:
        This property returns the socket error code that was the base for this
        exception being raised.
      See also:
        Create, CreateFmt
    }
    property ErrorCode: Integer read FErrorCode;
  end;

{ Description:
    This function creates a new instance of the lvkSocket object, prepared for
    a client connection. It will configure the object with the parameters given
    and optionally also try to connect the object at once.
  Parameters:
    HostName - The hostname or ip address of the server to connect to.
    Port - The port to connect on.
    AutoConnect - Set this to True to connect during the call to
      NewClientSocket. Set it to False so that you can configure the object
      more before you call .Connect.
  See also:
    NewServerSocket
}
function NewClientSocket(const HostName: string; const Port: Word;
  const AutoConnect: Boolean=True;
  const UseEvent: Boolean=False): IlvkClientSocket;

{ Description:
    This function creates a new instance of the lvkSocket object, prepared for
    a server connection. It will configure the object with the parameters given
    and optionally also try to start the object listening at once.
  Parameters:
    Port - The port to listen on.
    AutoListen - Set this to True to start listening during the call to
      NewServerSocket. Set it to False so that you can configure the object
      more before you call .Listen.
  See also:
    NewClientSocket
}
function NewServerSocket(const Port: Word;
  const AutoListen: Boolean=True): IlvkServerSocket;

{ Description:
    This function takes an address in the form of a 32-bit network-format
    value and returns the address as a dotted string.
  Parameters:
    Address - The address to convert to a dotted string.
}
function AddressToString(const Address: Cardinal): string;

{ Description:
    This function returns True if the address given is a dotted ip address.
    Any invalid ip address (not 4 byte-values with dots between) will be
    rejected and the function will return False.

    A valid ip address is of the form aaa.bbb.ccc.ddd, where aaa, bbb, ccc
    and ddd each is a value of 0 through 255, inclusive.
  Parameters:
    Address - The address to check.
}
function IsDottedAddress(const Address: string): Boolean;

{ Description:
    This function will take a hostname or a dotted ip address and return a
    address value in the form of a 32-bit network-format value. If the
    address is a dotted ip address it will simply convert it, otherwise it will
    do a dns lookup to find the ip address for the hostname and then convert
    the result.
  Parameters:
    Address - The address to convert.
}
function ResolveAddress(const Address: string): Cardinal;

{ Description:
    This function will take a hostname and do a normal dns lookup on it to
    get the ip address that it corresponds to.
  Parameters:
    HostName - The hostname to lookup.
  See also:
    ReverseDNSLookup
}
function ForwardDNSLookup(const HostName: string): Cardinal;

{ Description:
    This function will take a address in the form of a 32-bit network-format
    value and do a reverse dns lookup to get the hostname that it corresponds
    to.
  Parameters:
    Address - The address to lookup.
  See also:
    ForwardDNSLookup
}
function ReverseDNSLookup(const Address: Cardinal): string;

resourcestring
  // This string is used for exceptions
  SErrConnected     = 'Invalid operation while connected (%0:s)';
  // This string is used for exceptions
  SErrUnknown       = 'Unknown error #%0:d in call to %1:s';
  // This string is used for exceptions
  SErrCannotSeek    = 'Cannot seek on a TlvkSocketStream';
  // This string is used for exceptions
  SErrNotListening  = 'You can only do this operation on a listening socket(%0:s)';
  // This string is used for exceptions
  SErrNotConnected  = 'You can only do this operation on a connected socket (%0:s)';
  // Error string used for WSA exceptions
  SErrWSA           = '%0:s in call to %1:s';

type
  TWSAEvent = THandle;

function WSAEventSelect(s: TSocket; hEventObject: TWSAEvent;
  lNetworkEvents: LongInt): Integer; stdcall;

implementation

uses
  lvkThread, lvkSyncObjs, lvkVersion;

type
  TlvkSocket = class(TInterfacedObject, IlvkSocket, IlvkClientSocket,
    IlvkServerSocket, IThreadCooperate, IPackageVersion, IWaitableObject)
  private
    FSocket         : TSocket;
    FIsConnected    : Boolean;
    FIsListening    : Boolean;
    FTimeout        : Cardinal;
    FEvents         : TSocketEvents;
    FEvent          : TlvkEvent;

    FRemoteHostName : string;
    FRemotePort     : Word;
    FRemoteAddress  : string;
    FLocalHostName  : string;
    FLocalPort      : Word;
    FLocalAddress   : string;

    procedure DoConnect;
    procedure DoDisconnect;
    procedure DoListen;

    procedure CreateEvent;
    procedure DestroyEvent;

  protected
    // IWaitableObject interface
    function GetHandle: THandle;

    // IPackageVersion interface
    function GetPackageVersion: TPackageVersion;

    // IThreadCooperate interface
    procedure Terminate;

    // IlvkClientSocket interface
    procedure Connect;
    procedure Disconnect;
    function GetIsConnected: Boolean;
    procedure SetIsConnected(const Value: Boolean);
    procedure SetRemotePort(const Value: Word);
    procedure SetRemoteHostName(const Value: string);
    procedure SetRemoteAddress(const Value: string);

    // IlvkServerSocket interface
    function GetIsListening: Boolean;
    procedure SetIsListening(const Value: Boolean);
    procedure Listen;
    procedure StopListening;
    function Accept: IlvkSocket;
    procedure SetLocalPort(const Value: Word);
    procedure SetLocalHostName(const Value: string);
    procedure SetLocalAddress(const Value: string);

    // IlvkSocket interface
    procedure IlvkSocket.Close = Disconnect;
    procedure IlvkClientSocket.Close = Disconnect;
    procedure IlvkServerSocket.Close = Disconnect;
    function ReadyToRead(const Timeout: Cardinal): Boolean;
    function ReadyToWrite(const Timeout: Cardinal): Boolean;
    function GetTimeout: Cardinal;
    procedure SetTimeout(const Value: Cardinal);
    function GetRemotePort: Word;
    function GetRemoteHostName: string;
    function GetRemoteAddress: string;
    function GetLocalPort: Word;
    function GetLocalHostName: string;
    function GetLocalAddress: string;
    function Read(var Data; const Size: Integer): Integer;
    function Write(const Data; const Size: Integer): Integer;
    procedure ReadBuffer(var Data; const Size: Integer);
    procedure WriteBuffer(const Data; const Size: Integer);
    procedure WriteString(const s: string);
    function GetEvents: TSocketEvents;
    procedure SetEvents(const Value: TSocketEvents);
    function GetOptions: TSocketOptions;
    procedure SetOptions(const Value: TSocketOptions);

  public
    constructor CreateClient;
    constructor CreateServer;
    constructor CreateAccepted(const RemoteSocket: TSocket; const RemoteAddress: string;
      const RemotePort: Word; const Timeout: Cardinal);
    destructor Destroy; override;
  end;

{ Initialization code }

const
  VERSION_REQUIRED  = $0101;  // Version 1.1

var
  WSAData : TWSAData;

procedure WSACheck(const ResultCode: Integer; const MethodName: string);
var
  TempCode  : Integer;
begin
  // Convert a WinSock error code (<> 0) to an exception.
  if ResultCode <> 0 then
  begin
    if ResultCode = SOCKET_ERROR then
    begin
      TempCode := WSAGetLastError;
      if TempCode <> 0 then
        if TempCode <> SOCKET_ERROR then
          WSACheck(TempCode, MethodName)
        else
          raise ElvkSocket.CreateFmt(TempCode, SErrUnknown, [TempCode, MethodName]);
    end else
      raise ElvkSocket.CreateFmt(ResultCode, SErrWSA, [SysErrorMessage(ResultCode), MethodName]);
  end;
end;

{ WinSock 2 }

function WSAEventSelect; stdcall; external 'ws2_32.dll';

{ Support functions }

function CreateSockInAddr(const Address: Cardinal; const Port: Word): TSockAddrIn;
begin
  ZeroMemory(@Result, SizeOf(Result));
  Result.sin_family := AF_INET;
  Result.sin_port := htons(Port);
  Result.sin_addr.S_addr := Address;
end;

function AddressToString(const Address: Cardinal): string;
begin
  // Each byte of the 32-bit address is a single value in the ip address
  Result := Format('%d.%d.%d.%d', [
    Address and $ff,
    (Address and $ff00) shr 8,
    (Address and $ff0000) shr 16,
    (Address and $ff000000) shr 24]);
end;

function IsDottedAddress(const Address: string): Boolean;
var
  Index : Integer;
  Value : Integer;
  Count : Integer;
begin
  Assert(Address <> '');
  Assert(Address = Trim(Address));

  Result := False;

  Index := 1;
  Value := 0;
  Count := 0;
  while Index <= Length(Address) do
  begin
    case Address[Index] of
      '.':
        begin
          Value := 0;
          Inc(Count);
          if Count > 3 then
            Exit;
        end;

      '0'..'9':
        begin
          Value := Value * 10 + Ord(Address[Index])-48;
          if Value > 255 then
            Exit;
        end;
    else
      Exit;
    end;

    Inc(Index);
  end;

  Result := (Value <= 255) and (Count = 3);
end;

function ResolveAddress(const Address: string): Cardinal;
begin
  Assert(Address <> '');
  Assert(Address = Trim(Address));

  if CompareText(Address, 'localhost') = 0 then
    Result := ResolveAddress('127.0.0.1')
  else if IsDottedAddress(Address) then
  begin
    Result := inet_addr(PChar(Address));
    if Result = Cardinal(INADDR_NONE) then
    begin
      if Address = '0.0.0.0' then
        Result := Cardinal(INADDR_ANY)
      else if Address = '255.255.255.255' then
        Result := Cardinal(INADDR_BROADCAST)
      else
        raise ElvkSocket.Create(1, 'Unable to resolve address: invalid address');
    end;
  end else
    Result := ForwardDNSLookup(Address);
end;

function ForwardDNSLookup(const HostName: string): Cardinal;
var
  HostEnt : PHostEnt;
begin
  Assert(HostName <> '');
  Assert(HostName = Trim(HostName));

  HostEnt := WinSock.gethostbyname(PChar(HostName));
  if Assigned(HostEnt) then
    Move(HostEnt^.h_addr^[0], Result, SizeOf(Result))
  else
    raise ElvkSocket.Create(1, 'Unable to resolve address: ' + HostName);
end;

function ReverseDNSLookup(const Address: Cardinal): string;
var
  HostEnt : PHostEnt;
begin
  if Address = $0100007f then
    Result := 'localhost'
  else if Address = Cardinal(INADDR_ANY) then
    Result := '0.0.0.0'
  else if Address = Cardinal(INADDR_BROADCAST) then
    Result := '255.255.255.255'
  else begin
    HostEnt := WinSock.gethostbyaddr(@Address, SizeOf(Address), AF_INET);
    if Assigned(HostEnt) then
      Result := HostEnt^.h_name
    else
      raise ElvkSocket.Create(1, 'Unable to resolve address: ' + AddressToString(Address));
  end;
end;

{ ElvkSocket }

constructor ElvkSocket.Create(const ErrorCode: Integer;
  const Message: string);
begin
  Assert(ErrorCode <> 0);
  Assert(Message <> '');

  FErrorCode := ErrorCode;

  inherited Create(Message);
end;

constructor ElvkSocket.CreateFmt(const ErrorCode: Integer;
  const Fmt: string; const Args: array of const);
begin
  Assert(ErrorCode <> 0);
  Assert(Fmt <> '');

  FErrorCode := ErrorCode;

  inherited CreateFmt(Fmt, Args);
end;

{ Support functions }

function NewClientSocket(const HostName: string; const Port: Word;
  const AutoConnect: Boolean; const UseEvent: Boolean): IlvkClientSocket;
begin
  Assert(HostName <> '');
  Assert(Port > 0);

  Result := TlvkSocket.CreateClient as IlvkClientSocket;

  if IsDottedAddress(HostName) then
    Result.RemoteAddress := HostName
  else
    Result.RemoteHostName := HostName;
  Result.RemotePort := Port;

  if UseEvent then
    Result.Events := [];

  if AutoConnect then
    Result.Connect;
end;

function NewServerSocket(const Port: Word;
  const AutoListen: Boolean=True): IlvkServerSocket;
begin
  Assert(Port > 0);

  Result := TlvkSocket.CreateServer as IlvkServerSocket;
  Result.LocalPort := Port;
  if AutoListen then
    Result.Listen;
end;

{ TlvkSocket }

procedure TlvkSocket.Connect;
begin
  SetIsConnected(True);
end;

constructor TlvkSocket.CreateAccepted(const RemoteSocket: TSocket; const RemoteAddress: string;
  const RemotePort: Word; const Timeout: Cardinal);
var
  LocalAddr     : TSockAddr;
  LocalAddrSize : Integer;
begin
  Assert(FSocket <> INVALID_SOCKET);
  FSocket := RemoteSocket;
  FTimeout := Timeout;

  LocalAddrSize := SizeOf(LocalAddr);
  WSACheck(WinSock.getsockname(FSocket, LocalAddr, LocalAddrSize), 'TlvkSocket.CreateAccepted');
  FLocalPort := LocalAddr.sin_port;
  FLocalAddress := AddressToString(LocalAddr.sin_addr.S_addr);
  FLocalHostName := '';

  FIsConnected := True;
  FIsListening := False;

  FRemoteHostName := '';
  FRemoteAddress := RemoteAddress;
  FRemotePort := RemotePort;
end;

destructor TlvkSocket.Destroy;
begin
  DestroyEvent;
  Disconnect;

  inherited;
end;

procedure TlvkSocket.Disconnect;
begin
  SetIsListening(False);
  SetIsConnected(False);
end;

procedure TlvkSocket.DoConnect;
var
  LocalAddr     : TSockAddr;
  RemoteAddr    : TSockAddr;
  LocalAddrSize : Integer;
begin
  SetIsListening(False);

  // Create socket handle
  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    WSACheck(WSAGetLastError, 'TlvkSocket.DoConnect');

  // Bind to local address and port
  LocalAddr := CreateSockInAddr(0, 0);
  WSACheck(bind(FSocket, LocalAddr, SizeOf(LocalAddr)), 'TlvkSocket.DoConnect');

  // Connect to remote socket
  RemoteAddr := CreateSockInAddr(ResolveAddress(GetRemoteAddress), GetRemotePort);
  WSACheck(WinSock.connect(FSocket, RemoteAddr, SizeOf(LocalAddr)), 'TlvkSocket.DoConnect');

  // Get local address
  LocalAddrSize := SizeOf(LocalAddr);
  WSACheck(WinSock.getsockname(FSocket, LocalAddr, LocalAddrSize), 'TlvkSocket.DoConnect');
  FLocalPort := LocalAddr.sin_port;
  FLocalAddress := AddressToString(LocalAddr.sin_addr.S_addr);
  FLocalHostName := '';
end;

procedure TlvkSocket.DoDisconnect;
var
  Socket  : TSocket;
begin
  Socket := FSocket;
  FSocket := INVALID_SOCKET;
  SetEvent(Socket);
  WSACheck(closesocket(Socket), 'TlvkSocket.DoDisconnect');
end;

function TlvkSocket.GetRemoteHostName: string;
begin
  if (FRemoteHostName = '') and (FRemoteAddress <> '') then
  begin
    FRemoteHostName := ReverseDNSLookup(ResolveAddress(FRemoteAddress));
    Result := FRemoteHostName
  end else
    Result := FRemoteHostName;
end;

function TlvkSocket.GetRemoteAddress: string;
begin
  if (FRemoteAddress = '') and (FRemoteHostName <> '') then
  begin
    FRemoteAddress := AddressToString(ForwardDNSLookup(FRemoteHostName));
    Result := FRemoteAddress;
  end else
    Result := FRemoteAddress;
end;

function TlvkSocket.GetIsConnected: Boolean;
begin
  Result := FIsConnected;
end;

function TlvkSocket.GetRemotePort: Word;
begin
  Result := FRemotePort;
end;

procedure TlvkSocket.SetRemoteHostName(const Value: string);
begin
  if GetIsConnected or GetIsListening then
    raise ElvkSocket.CreateFmt(1, SErrConnected, ['Set RemoteHostName']);

  FRemoteHostName := Value;
  FRemoteAddress := '';
end;

procedure TlvkSocket.SetRemoteAddress(const Value: string);
begin
  if GetIsConnected or GetIsListening then
    raise ElvkSocket.CreateFmt(1, SErrConnected, ['Set RemoteAddress']);

  FRemoteHostName := '';
  FRemoteAddress := Value;
end;

procedure TlvkSocket.SetIsConnected(const Value: Boolean);
begin
  if Value <> FIsConnected then
  begin
    FIsConnected := Value;
    try
      if Value then
        DoConnect
      else
        DoDisconnect;
    except
      FIsConnected := not Value;
      raise;
    end;
  end;
end;

procedure TlvkSocket.SetRemotePort(const Value: Word);
begin
  if Value <> FRemotePort then
  begin
    if GetIsConnected or GetIsListening then
      raise ElvkSocket.CreateFmt(1, SErrConnected, ['Set RemotePort']);

    FRemotePort := Value;
  end;
end;

function TlvkSocket.Read(var Data; const Size: Integer): Integer;
var
  TimeOut   : TTimeVal;
  rc        : Integer;
  SocketSet : TFDSet;
begin
  if not FIsConnected then
    raise ElvkSocket.CreateFmt(5, SErrNotConnected, ['Read']);

  if FTimeout <> INFINITE then
  begin
    Timeout.tv_usec := FTimeout mod 1000;
    Timeout.tv_sec := FTimeout div 1000;

    SocketSet.fd_count := 1;
    SocketSet.fd_array[0] := FSocket;
    rc := WinSock.select(0, @SocketSet, nil, nil, @TimeOut);
    if rc = 0 then
    begin
      Result := 0;
      Exit;
    end;

    if rc = SOCKET_ERROR then
      WSACheck(WSAGetLastError, 'TlvkSocket.Read');
  end;

  Result := WinSock.recv(FSocket, Data, Size, 0);
  if Result = SOCKET_ERROR then
  begin
    rc := WSAGetLastError;
    if rc = WSAEWOULDBLOCK then
      Result := 0
    else
      WSACheck(rc, 'TlvkSocket.Read');
  end;
end;

function TlvkSocket.Write(const Data; const Size: Integer): Integer;
var
  TimeOut   : TTimeVal;
  rc        : Integer;
  SocketSet : TFDSet;
begin
  if not FIsConnected then
    raise ElvkSocket.CreateFmt(5, SErrNotConnected, ['Write']);

  if FTimeout <> INFINITE then
  begin
    Timeout.tv_usec := FTimeout mod 1000;
    Timeout.tv_sec := FTimeout div 1000;

    SocketSet.fd_count := 1;
    SocketSet.fd_array[0] := FSocket;
    rc := WinSock.select(0, nil, @SocketSet, nil, @TimeOut);
    if rc = 0 then
    begin
      Result := 0;
      Exit;
    end;

    if rc = SOCKET_ERROR then
      WSACheck(WSAGetLastError, 'TlvkSocket.Write');
  end;

  Result := WinSock.send(FSocket, (@Data)^, Size, 0);
  if Result = SOCKET_ERROR then
  begin
    rc := WSAGetLastError;
    if rc = WSAEWOULDBLOCK then
      Result := 0
    else
      WSACheck(rc, 'TlvkSocket.Write');
  end;
end;

procedure TlvkSocket.ReadBuffer(var Data; const Size: Integer);
var
  Amount  : Integer;
  DataPtr : PChar;
  Left    : Integer;
begin
  if not FIsConnected then
    raise ElvkSocket.CreateFmt(5, SErrNotConnected, ['ReadBuffer']);

  DataPtr := @Data;
  Left := Size;
  while Left > 0 do
  begin
    Amount := Read(DataPtr^, Left);
    if Amount > 0 then
    begin
      Inc(DataPtr, Amount);
      Dec(Left, Amount);
    end else
      Break;
  end;

  if Left > 0 then
    raise ElvkSocket.Create(3, 'Read-error');
end;

procedure TlvkSocket.WriteBuffer(const Data; const Size: Integer);
var
  Amount  : Integer;
  DataPtr : PChar;
  Left    : Integer;
begin
  if not FIsConnected then
    raise ElvkSocket.CreateFmt(5, SErrNotConnected, ['WriteBuffer']);

  DataPtr := @Data;
  Left := Size;
  while Left > 0 do
  begin
    Amount := Write(DataPtr^, Left);
    if Amount > 0 then
    begin
      Inc(DataPtr, Amount);
      Dec(Left, Amount);
    end else
      Break;
  end;

  if Left > 0 then
    raise ElvkSocket.Create(3, 'Write-error');
end;

procedure TlvkSocket.WriteString(const s: string);
var
  Temp  : string;
begin
  if not FIsConnected then
    raise ElvkSocket.CreateFmt(5, SErrNotConnected, ['WriteString']);

  if s <> '' then
  begin
    Temp := s;
    WriteBuffer(Temp[1], Length(Temp));
  end;
end;

function TlvkSocket.Accept: IlvkSocket;
var
  Socket    : TSocket;
  Addr      : TSockAddr;
  Size      : Integer;
  Timeout   : TTimeVal;
  rc        : Integer;
  SocketSet : TFDSet;
begin
  if not FIsListening then
    raise ElvkSocket.CreateFmt(5, SErrNotListening, ['Accept']);

  if FTimeout <> INFINITE then
  begin
    Timeout.tv_usec := FTimeout mod 1000;
    Timeout.tv_sec := FTimeout div 1000;

    SocketSet.fd_count := 1;
    SocketSet.fd_array[0] := FSocket;
    rc := WinSock.select(0, @SocketSet, nil, nil, @TimeOut);
    if rc = 0 then
    begin
      Result := nil;
      Exit;
    end;

    if rc = SOCKET_ERROR then
      WSACheck(WSAGetLastError, 'TlvkSocket.Accept');
  end;

  ZeroMemory(@Addr, SizeOf(Addr));
  Size := SizeOf(Addr);
  Socket := WinSock.accept(FSocket, @Addr, @Size);
  if Socket <> INVALID_SOCKET then
  begin
    Result := TlvkSocket.CreateAccepted(Socket,
      AddressToString(Cardinal(Addr.sin_addr.S_addr)), Addr.sin_port,
      FTimeout) as IlvkSocket;
  end else begin
    rc := WSAGetLastError;
    if (rc = WSAEINTR) and (FSocket = INVALID_SOCKET) then
      Result := nil
    else
      WSACheck(rc, 'TlvkSocket.Accept');
  end;
end;

procedure TlvkSocket.Listen;
begin
  SetIsListening(True);
end;

function TlvkSocket.GetLocalAddress: string;
begin
  if (FLocalAddress = '') and (FLocalHostName <> '') then
  begin
    FLocalAddress := AddressToString(ForwardDNSLookup(FLocalHostName));
    Result := FLocalAddress;
  end else
    Result := FLocalAddress;
end;

function TlvkSocket.GetLocalHostName: string;
begin
  if (FLocalHostName = '') and (FLocalAddress <> '') then
  begin
    FLocalHostName := ReverseDNSLookup(ResolveAddress(FLocalAddress));
    Result := FLocalHostName
  end else
    Result := FLocalHostName;
end;

function TlvkSocket.GetLocalPort: Word;
begin
  Result := FLocalPort;
end;

procedure TlvkSocket.SetLocalPort(const Value: Word);
begin
  if Value <> FLocalPort then
  begin
    if GetIsConnected or GetIsListening then
      raise ElvkSocket.CreateFmt(1, SErrConnected, ['Set RemotePort']);

    FLocalPort := Value;
  end;
end;

procedure TlvkSocket.SetLocalAddress(const Value: string);
begin
  if GetIsConnected or GetIsListening then
    raise ElvkSocket.CreateFmt(1, SErrConnected, ['Set LocalAddress']);

  FLocalHostName := '';
  FLocalAddress := Value;
end;

procedure TlvkSocket.SetLocalHostName(const Value: string);
begin
  if GetIsConnected or GetIsListening then
    raise ElvkSocket.CreateFmt(1, SErrConnected, ['Set LocalHostName']);

  FLocalHostName := Value;
  FLocalAddress := '';
end;

function TlvkSocket.GetIsListening: Boolean;
begin
  Result := FIsListening;
end;

procedure TlvkSocket.SetIsListening(const Value: Boolean);
begin
  if Value <> FIsListening then
  begin
    FIsListening := Value;
    try
      if Value then
        DoListen
      else
        DoDisconnect;
    except
      FIsListening := not Value;
      raise;
    end;
  end;
end;

procedure TlvkSocket.StopListening;
begin
  SetIsListening(False);
end;

procedure TlvkSocket.DoListen;
var
  LocalAddr   : TSockAddr;
begin
  SetIsConnected(False);

  // Create socket handle
  FSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    WSACheck(WSAGetLastError, 'TlvkSocket.DoListen');

  // Bind to local address and port
  if GetLocalAddress = '' then
    LocalAddr := CreateSockInAddr(ResolveAddress('0.0.0.0'), GetLocalPort)
  else
    LocalAddr := CreateSockInAddr(ResolveAddress(GetLocalAddress), GetLocalPort);
  WSACheck(bind(FSocket, LocalAddr, SizeOf(LocalAddr)), 'TlvkSocket.DoListen');

  // Connect to remote socket
  WSACheck(WinSock.listen(FSocket, 10), 'TlvkSocket.DoListen');
end;

constructor TlvkSocket.CreateClient;
begin
  FSocket := INVALID_SOCKET;
  FTimeout := INFINITE;

  FLocalPort := 0;
  FLocalAddress := '127.0.0.1';
  FLocalHostName := 'localhost';

  FRemotePort := 0;
  FRemoteAddress := '127.0.0.1';
  FRemoteHostName := 'Remotehost';

  FIsListening := False;
  FIsConnected := False;
end;

constructor TlvkSocket.CreateServer;
begin
  FSocket := INVALID_SOCKET;
  FTimeout := INFINITE;

  FLocalPort := 0;
  FLocalAddress := '';
  FLocalHostName := '';

  FRemotePort := 0;
  FRemoteAddress := '127.0.0.1';
  FRemoteHostName := 'Remotehost';

  FIsListening := False;
  FIsConnected := False;
end;

procedure TlvkSocket.Terminate;
begin
  SetIsConnected(False);
  SetIsListening(False);
end;

function TlvkSocket.GetTimeout: Cardinal;
begin
  Result := FTimeout;
end;

procedure TlvkSocket.SetTimeout(const Value: Cardinal);
begin
  FTimeout := Value;
end;

function TlvkSocket.ReadyToRead(const Timeout: Cardinal): Boolean;
var
  SocketSet : TFDSet;
  rc        : Integer;
  TimeVal   : TTimeVal;
begin
  TimeVal.tv_sec := Timeout div 1000;
  TimeVal.tv_usec := Timeout mod 1000;

  SocketSet.fd_count := 1;
  SocketSet.fd_array[0] := FSocket;
  rc := WinSock.select(0, @SocketSet, nil, nil, @TimeVal);
  Result := (rc > 0);
end;

function TlvkSocket.ReadyToWrite(const Timeout: Cardinal): Boolean;
var
  SocketSet : TFDSet;
  rc        : Integer;
  TimeVal   : TTimeVal;
begin
  TimeVal.tv_sec := Timeout div 1000;
  TimeVal.tv_usec := Timeout mod 1000;

  SocketSet.fd_count := 1;
  SocketSet.fd_array[0] := FSocket;
  rc := WinSock.select(0, nil, @SocketSet, nil, @TimeVal);
  Result := (rc > 0);
end;

function TlvkSocket.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkSocket.GetHandle: THandle;
begin
  if Assigned(FEvent) then
    Result := (FEvent as IWaitableObject).Handle
  else
    Result := INVALID_HANDLE_VALUE;
end;

function TlvkSocket.GetEvents: TSocketEvents;
begin
  Result := FEvents;
end;

procedure TlvkSocket.SetEvents(const Value: TSocketEvents);
begin
  if Value <> FEvents then
  begin
    DestroyEvent;

    FEvents := Value;

    CreateEvent;
  end;
end;

procedure TlvkSocket.CreateEvent;
var
  Mask  : LongInt;
  Event : TSocketEvent;
const
  EventMasks  : array[TSocketEvent] of LongInt = (
    FD_READ,      // seReadyToRead
    FD_WRITE      // seReadyToWrite
  );
begin
  if FEvents <> [] then
  begin
    FEvent := TlvkEvent.Create(nil);
    FEvent.Active := True;

    Mask := 0;
    for Event := Low(TSocketEvent) to High(TSocketEvent) do
      if Event in FEvents then
        Mask := Mask or EventMasks[Event];

    WSACheck(WSAEventSelect(FSocket, (FEvent as IWaitableObject).Handle, Mask),
      'CreateEvent');
  end;
end;

procedure TlvkSocket.DestroyEvent;
begin
  if Assigned(FEvent) then
  begin
    if GetIsConnected then
      WSACheck(WSAEventSelect(FSocket, 0, 0), 'DestroyEvent');

    FEvent.Active := False;
    FreeAndNil(FEvent);
  end;
end;

function TlvkSocket.GetOptions: TSocketOptions;
var
  BoolValue : LongBool;
  OptLen    : Integer;
begin
  Result := [];

  // Nagle algorithm
  OptLen := SizeOf(BoolValue);
  WSACheck(getsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, @BoolValue,
    OptLen), 'GetOptions');
  if not BoolValue then
    Result := Result + [soNagleAlgorithm];
end;

procedure TlvkSocket.SetOptions(const Value: TSocketOptions);
var
  BoolValue : LongBool;
begin
  // Nagle algorithm
  BoolValue := not (soNagleAlgorithm in Value);
  WSACheck(setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, @BoolValue,
    SizeOf(BoolValue)), 'SetOptions');
end;

{ TlvkSocketStream }

constructor TlvkSocketStream.Create(const Socket: IlvkSocket);
begin
  inherited Create;

  FSocket := Socket;
  FPosition := 0;
end;

function TlvkSocketStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FSocket.Read(Buffer, Count);
  Inc(FPosition, Result);
end;

function TlvkSocketStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      FPosition := Offset;

    soFromCurrent, soFromEnd:
      Inc(FPosition, Offset);

  else
    FPosition := FPosition;
  end;

  Result := FPosition;
end;

{$IFDEF DELPHI6UP}
function TlvkSocketStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      FPosition := Offset;

    soCurrent, soEnd:
      Inc(FPosition, Offset);
  end;

  Result := FPosition;
end;
{$ENDIF}

function TlvkSocketStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FSocket.Write(Buffer, Count);
  Inc(FPosition, Result);
end;

initialization
  ZeroMemory(@WSAData, SizeOf(WSAData));
  WSACheck(WSAStartup(VERSION_REQUIRED, WSAData), 'lvkSocket.initialization');
finalization
  WSACheck(WSACleanup, 'lvkSocket.finalization');
end.
