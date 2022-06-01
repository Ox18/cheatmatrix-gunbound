{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains mailslot components for easy and simple network
    communications.
}
unit lvkMailslots;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkMailslots.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Classes, SysUtils, ExtCtrls, lvkVersion;

const
  DEFAULT_ACTIVE            = False;
  DEFAULT_MAX_MESSAGE_SIZE  = 0;
  
type
  { Description:
      This event handler type is used for the OnBinary event handler of
      TlvkCustomMailslotServer.
    Parameters:
      Sender - The mailslot server component that called the event handler.
      Data - Pointer to the data received.
      Size - The number of bytes received.
    See also:
      TlvkStringEvent, TlvkClassEvent, TlvkLongWordEvent, TlvkStreamEvent
  }
  TlvkBinaryEvent   = procedure(const Sender: TObject; const Data: PChar;
    const Size: Integer) of object;

  { Description:
      This event handler type is used for the OnString event handler of
      TlvkCustomMailslotserver.
    Parameters:
      Sender - The mailslot server component that called the event handler.
      s - The string received.
    See also:
      TlvkBinaryEvent, TlvkClassEvent, TlvkLongWordEvent, TlvkStreamEvent
  }
  TlvkStringEvent   = procedure(const Sender: TObject;
    const s: string) of object;

  { Description:
      This event handler type is used for the OnClass event handler of
      TlvkCustomMailslotServer.
    Parameters:
      Sender - The mailslot server component that called the event handler.
      Obj - The component object received.
    See also:
      TlvkBinaryEvent, TlvkStringEvent, TlvkLongWordEvent, TlvkStreamEvent
  }
  TlvkClassEvent    = procedure(const Sender: TObject;
    var Obj: TComponent) of object;

  { Description:
      This event handler type is used for the OnLongWord event handler of
      TlvkCustomMailslotServer.
    Parameters:
      Sender - The mailslot server component that called the event handler.
      l - The LongWord value received.
    See also:
      TlvkBinaryEvent, TlvkStringEvent, TlvkClassEvent, TlvkStreamEvent
  }
  TlvkLongWordEvent = procedure(const Sender: TObject;
    const l: LongWord) of object;

  { Description:
      This event handler type is used for the OnStream event handler of
      TlvkCustomMailslotServer.
    Parameters:
      Sender - The mailslot server component that called the event handler.
      Strm - The TStream value received.
    See also:
      TlvkBinaryEvent, TlvkStringEvent, TlvkClassEvent, TlvkLongWordEvent
  }
  TlvkStreamEvent   = procedure(const Sender: TObject;
    var Strm: TStream) of object;

  { Description:
      When data is sent from the client to the server, the data is tagged
      as being of a specific type.

        * dtString - The value is a string value and will be handled by the
          OnString event handler.
        * dtBinary - The value is pure binary and will be handled by the
          OnBinary event handler.
        * dtClass - The value is the contents of a object instance and will
          be handled by the OnClass event handler.
        * dtLongWord - The value is a LongWord value and will be handled by
          the OnLongWord event handler.
        * dtStream - The value is the contents of a TStream and will be
          handled by the OnStream event handler.
  }
  TlvkDataType  = (dtString, dtBinary, dtClass, dtLongWord, dtStream);

  { Description:
      This is a special kind of object that can be sent through the mailslot.
      When an object that descends from TlvkMailslotAction is received at
      the server, it will be executed at once instead of passed on to the
      OnClass event handler. This is done internally in the receiving
      code.
  }
  TlvkMailslotAction  = class(TComponent)
  protected
    { Description:
        This method will be called once when the object is received at the
        server end of the mailslot. You will need to descend from
        TlvkMailslotAction and override Execute in order to provide a working
        object.
    }
    procedure Execute; virtual; abstract;
  end;

  { Description:
      This is the internal mailslot class definition. The server and client
      components both descend from this class. This class implements the
      core code related to mailslots.
    See also:
      TlvkMailslotServer, TlvkMailslotClient
  }
  TlvkiMailslot = class(TComponent)
  private
    FMailslotName : string;
    FActive       : Boolean;
    FHandle       : THandle;
    FSetActive    : Boolean;

    procedure SetMailslotName(const Value: string);
    procedure SetActive(const Value: Boolean);
    procedure SetPackageVersion(const Value: TPackageVersion);

  protected
    { Description:
        This property holds the name of the mailslot. The allowed format differs
        from the client to the server end.

        The client format has to be one of these three formats:
          \\.\mailslot\<name of mailslot>
          \\*\mailslot\<name of mailslot>
          \\<server>\mailslot\<name of mailslot>

        The server format can only be this format:
          \\.\mailslot\<name of mailslot>
    }
    property MailslotName: string read FMailslotName write SetMailslotName;

    { Description:
        Whenever this property is set to True, the mailslot component is
        activated. You can only send messages through an activated client,
        and only an active server will receive the messages.
      See also:
        TlvkiMailslot.OpenMailslot, TlvkiMailslot.CloseMailslot
    }
    property Active: Boolean read FActive write SetActive
      default DEFAULT_ACTIVE;

    { Description:
        This method will open the mailslot. The method will be used internally
        by the code that handles the Active property change.
      See also:
        TlvkiMailslot.Active, TlvkiMailslot.CloseMailslot
    }
    procedure OpenMailslot; virtual; abstract;

    { Description:
        This method will close the mailslot. The method will be used internally
        by the code that handles the Active property change.
      See also:
        TlvkiMailslot.Active, TlvkiMailslot.OpenMailslot
    }
    procedure CloseMailslot; virtual;

    procedure Loaded; override;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This is the custom version of the mailslot server component. It implements
      all the code related to receiving and handling messages in a mailslot.
    See also:
      TlvkiMailslot, TlvkMailslotServer
  }
  TlvkCustomMailslotServer  = class(TlvkiMailslot)
  private
    FMaxMessageSize : Integer;
    FThread         : TThread;

    FOnBinary       : TlvkBinaryEvent;
    FOnClass        : TlvkClassEvent;
    FOnString       : TlvkStringEvent;
    FOnInvalid      : TlvkBinaryEvent;
    FOnLongWord     : TlvkLongWordEvent;
    FOnStream       : TlvkStreamEvent;

    procedure SetMaxMessageSize(const Value: Integer);
    procedure HandleMessage(const Message: PChar; const Size: Integer);
    procedure Check; 

  protected
    { Description:
        This property holds the maximum message size that can be passed through
        the mailslot. It will be used internally to set the size of the
        receiving buffer.
    }
    property MaxMessageSize: Integer read FMaxMessageSize
      write SetMaxMessageSize default DEFAULT_MAX_MESSAGE_SIZE;

    { Description:
        This event handler will be called when a binary data block is
        received by the server.
      See also:
        OnInvalid, OnClass, OnString, OnLongWord, OnStream
    }
    property OnBinary: TlvkBinaryEvent read FOnBinary write FOnBinary;

    { Description:
        This event handler will be called when a invalid data block
        is received, one that the component cannot determine the type of.
      See also:
        OnBinary, OnClass, OnString, OnLongWord, OnStream
    }
    property OnInvalid: TlvkBinaryEvent read FOnInvalid write FOnInvalid;

    { Description:
        This event handler will be called when a object instance is received
        by the server.
      See also:
        OnBinary, OnInvalid, OnString, OnLongWord, OnStream
    }
    property OnClass: TlvkClassEvent read FOnClass write FOnClass;

    { Description:
        This event handler will be called when a string value is received
        by the server.
      See also:
        OnBinary, OnInvalid, OnClass, OnLongWord, OnStream
    }
    property OnString: TlvkStringEvent read FOnString write FOnString;

    { Description:
        This event handler will be called when a LongWord value is received
        by the server.
      See also:
        OnBinary, OnInvalid, OnString, OnClass, OnStream
    }
    property OnLongWord: TlvkLongWordEvent read FOnLongWord write FOnLongWord;

    { Description:
        This event handler will be called when a TStream value is received
        by the server.
      See also:
        OnBinary, OnInvalid, OnString, OnLongWord, OnClass
    }
    property OnStream: TlvkStreamEvent read FOnStream write FOnStream;

    // <ALIAS TlvkiMailslot.OpenMailslot>
    procedure OpenMailslot; override;
    // <ALIAS TlvkiMailslot.CloseMailslot>
    procedure CloseMailslot; override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This is the non-custom mailslot server component. This is the component
      you will drop on your form to create a usable mailslot server.
    See also:
      TlvkCustomMailslotServer
  }
  TlvkMailslotServer  = class(TlvkCustomMailslotServer)
  published
    // <ALIAS TlvkiMailslot.MailslotName>
    property MailslotName;
    // <ALIAS TlvkiMailslot.Active>
    property Active;
    // <ALIAS TlvkCustomMailslotServer.MaxMessageSize>
    property MaxMessageSize;
    // <ALIAS TlvkCustomMailslotServer.OnBinary>
    property OnBinary;
    // <ALIAS TlvkCustomMailslotServer.OnClass>
    property OnClass;
    // <ALIAS TlvkCustomMailslotServer.OnString>
    property OnString;
    // <ALIAS TlvkCustomMailslotServer.OnLongWord>
    property OnLongWord;
    // <ALIAS TlvkCustomMailslotServer.OnStream>
    property OnStream;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  { Description:
      This is the custom version of the mailslot client component. It implements
      all the code related to sending messages to a mailslot.
    See also:
      TlvkiMailslot, TlvkMailslotClient
  }
  TlvkCustomMailslotClient  = class(TlvkiMailslot)
  protected
    // <ALIAS TlvkiMailslot.OpenMailslot>
    procedure OpenMailslot; override;

  public
    { Description:
        This method is used to write a block of binary data to the
        server mailslot.
      Parameters:
        Data - Pointer to the data to send.
        Size - The amount of data to send, in bytes.
        DataType - The data type to send the data as.
      See also:
        TlvkDataType,
        Write@string, Write@LongWord,
        Send@TComponent, Send@TStream
    }
    procedure Write(const Data; const Size: Integer; const DataType: TlvkDataType=dtBinary); overload; virtual;

    { Description:
        This method is used to write a string value to the server mailslot.
      Parameters:
        Str - The string value to send to the server.
      See also:
        Write@@Integer@TlvkDataType, Write@LongWord,
        Send@TComponent, Send@TStream
    }
    procedure Write(const Str: string); overload; virtual;

    { Description:
        This method is used to write a LongWord value to the server mailslot.
      Parameters:
        l - The LongWord value to send to the server.
      See also:
        Write@@Integer@TlvkDataType, Write@string
        Send@TComponent, Send@TStream
    }
    procedure Write(const l: LongWord); overload; virtual;

    { Description:
        This method is used to send the contents of a object instance to the
        server mailslot.
      Parameters:
        Obj - The object instance to send. The object must descend from
          TComponent in order for the Send method to be able to stream
          the published properties of the object out to a stream.
      See also:
        Write@@Integer@TlvkDataType, Write@string, Write@LongWord,
        Send@TStream
    }
    procedure Send(const Obj: TComponent); overload; virtual;

    { Description:
        This method is used to send the contents of a TStream object to the
        server mailslot.
      Parameters:
        Strm - The stream object instance to send the contents of.
      See also:
        Write@@Integer@TlvkDataType, Write@string, Write@LongWord,
        Send@TComponent
    }
    procedure Send(const Strm: TStream); overload; virtual;
  end;


  { Description:
      This is the non-custom mailslot client component. This is the component
      you will drop on your form to create a usable mailslot client.
    See also:
      TlvkCustomMailslotClient
  }
  TlvkMailslotClient  = class(TlvkCustomMailslotClient)
  published
    // <ALIAS TlvkiMailslot.MailslotName>
    property MailslotName;
    // <ALIAS TlvkiMailslot.Active>
    property Active;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end; // class TlvkMailslotClient

  { Description:
      This exception class is used when exceptions are raised in any of the
      mailslot components.
    See also:
      TlvkMailslotClient, TlvkMailslotServer
  }
  ElvkMailslot  = class(Exception);

resourcestring
  { Description:
      This text message will be used for exceptions when the code tries to
      read from a mailslot component that hasn't yet been set Active.
  }
  SReadNoActive         = 'Cannot read from an inactive mailslot';
  { Description:
      This text message will be used for exceptions when the code tries to
      write to a mailslot component that hasn't yet been set Active.
  }
  SWriteNoActive        = 'Cannot write to an inactive mailslot';

  { Description:
      This text message will be used for exceptions when the code tries to
      read from a mailslot component and it fails to completely read the
      requested data.
  }
  SReadIncomplete       = 'Unable to complete read request';
  { Description:
      This text message will be used for exceptions when the code tries to
      write to a mailslot component and it fails to completely write the
      requested data.
  }
  SWriteIncomplete      = 'Unable to complete write request';

const
  { Description:
      This is the default mailslot name used when a mailslot component is
      created.
  }
  SDefaultMailslotName  = '\\.\mailslot\name';

implementation

{ TlvkiWatchThread }

type
  TlvkiWatchThread = class(TThread)
  private
    FOwner  : TlvkCustomMailslotServer;
    FHandle : THandle;
    FByte   : Byte;

  protected
    procedure Execute; override;
    procedure HandleByte;

  public
    constructor Create(const AOwner: TlvkCustomMailslotServer;
      const AHandle: THandle);
  end;

constructor TlvkiWatchThread.Create(const AOwner: TlvkCustomMailslotServer;
  const AHandle: THandle);
begin
  inherited Create(True);

  FOwner := AOwner;
  FHandle := AHandle;
  FreeOnTerminate := True;

  Resume;
end;

procedure TlvkiWatchThread.Execute;
var
  rc            : Boolean;
  b             : Byte;
  BytesRead     : Cardinal;
  WakeupReason  : LongWord;
begin
  repeat
    rc := ReadFile(FHandle, b, 1, BytesRead, nil);
    if rc then
    begin
      FByte := b;
      Synchronize(HandleByte);
    end;

    WakeupReason := GetLastError;

    if WakeupReason = ERROR_HANDLE_EOF then
      Break;

    if WakeupReason = ERROR_INSUFFICIENT_BUFFER then
    begin
      Synchronize(FOwner.Check);
      Continue;
    end;

    raise Exception.Create(SysErrorMessage(WakeupReason));
  until Terminated;
end;

procedure TlvkiWatchThread.HandleByte;
begin
  FOwner.HandleMessage(@FByte, 1);
end;

{ TlvkiMailslot }

procedure TlvkiMailslot.CloseMailslot;
begin
  if not (csDesigning in ComponentState) then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

constructor TlvkiMailslot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMailslotName := SDefaultMailslotName;
  FActive := DEFAULT_ACTIVE;
  FSetActive := False;
  FHandle := INVALID_HANDLE_VALUE;
end;

destructor TlvkiMailslot.Destroy;
begin
  Active := False;

  inherited Destroy;
end;

function TlvkiMailslot.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkiMailslot.Loaded;
begin
  inherited;

  Active := FSetActive;
end;

procedure TlvkiMailslot.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if csLoading in ComponentState then
      FSetActive := Value
    else begin
      if Value then
        OpenMailslot
      else
        CloseMailslot;
      FActive := Value;
    end;
  end;
end;

procedure TlvkiMailslot.SetMailslotName(const Value: string);
begin
  if FMailslotName <> Value then
  begin
    Active := False;
    FMailslotName := Value;

    if Copy(FMailslotName, 1, 2) <> '\\' then
    begin
      if Copy(FMailslotName, 1, 1) <> '\' then
        FMailslotName := '\' + FMailslotName;
      FMailslotName := '\\.\mailslot' + FMailslotName;
    end;
  end;
end; 

procedure TlvkiMailslot.SetPackageVersion(const Value: TPackageVersion);
begin
  // Do nothing
end;

{ TlvkCustomMailslotServer }

procedure TlvkCustomMailslotServer.Check;
var
  NextSize  : Cardinal;
  BytesRead : Cardinal;
  Message   : PChar;
begin
  if not Active then
    raise ElvkMailslot.Create(SReadNoActive);

  repeat
    if not GetMailslotInfo(FHandle, nil, NextSize, nil, nil) then
      RaiseLastWin32Error;
    if NextSize <> MAILSLOT_NO_MESSAGE then
    begin
      GetMem(Message, NextSize);
      try
        if not ReadFile(FHandle, Message^, NextSize, BytesRead, nil) then
          RaiseLastWin32Error;

        if BytesRead <> NextSize then
          raise ElvkMailslot.Create(SReadIncomplete);

        HandleMessage(Message, NextSize);
      finally
        FreeMem(Message);
      end;
    end;
  until NextSize = MAILSLOT_NO_MESSAGE;
end;

procedure TlvkCustomMailslotServer.CloseMailslot;
begin
  if not (csDesigning in ComponentState) then
  begin
    FThread.Terminate;
    FThread := nil;
  end;

  inherited;
end;

constructor TlvkCustomMailslotServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHandle := INVALID_HANDLE_VALUE;
  FMaxMessageSize := DEFAULT_MAX_MESSAGE_SIZE;
  FThread := nil;
end;

procedure TlvkCustomMailslotServer.HandleMessage(const Message: PChar;
  const Size: Integer);
var
  DataType  : TlvkDataType;
  s         : string;
  Text      : TStream;
  Binary    : TStream;
  Obj       : TComponent;
  l         : LongWord;
begin
  Move(Message^, DataType, 1);
  case DataType of
    dtString:
      if Assigned(FOnString) then
      begin
        SetLength(s, Size - 1);
        Move((Message + 1)^, s[1], Size - 1);
        FOnString(Self, s);
      end;

    dtBinary:
      if (Assigned(FOnBinary)) then
        FOnBinary(Self, Message + 1, Size - 1);

    dtClass:
      begin
        Text := TMemoryStream.Create;
        try
          Text.WriteBuffer((Message + 1)^, Size-1);

          Binary := TMemoryStream.Create;
          try
            Text.Position := 0;
            ObjectTextToBinary(Text, Binary);

            Binary.Position := 0;
            Obj := Binary.ReadComponent(nil);
            try
              if Obj is TlvkMailslotAction then
                TlvkMailslotAction(Obj).Execute
              else if Assigned(FOnClass) then
                FOnClass(Self, Obj);
            finally
              FreeAndNil(Obj);
            end;
          finally
            FreeAndNil(Binary);
          end;
        finally
          FreeAndNil(Text);
        end;
      end;

    dtLongWord:
      if Assigned(FOnLongWord) and (Size = 5) then
      begin
        Move((Message + 1)^, l, 4);
        FOnLongWord(Self, l);
      end;

    dtStream:
      if Assigned(FOnStream) then
      begin
        Binary := TMemoryStream.Create;
        try
          Binary.WriteBuffer((Message + 1)^, Size - 1);
          Binary.Position := 0;

          FOnStream(Self, Binary);
        finally
          FreeAndNil(Binary);
        end;
      end;
  else
    if Assigned(FOnInvalid) then
      FOnInvalid(Self, Message, Size);
  end; 
end;

procedure TlvkCustomMailslotServer.OpenMailslot;
begin
  if not (csDesigning in ComponentState) then
  begin
    FHandle := CreateMailSlot(PChar(FMailslotName), FMaxMessageSize, MAILSLOT_WAIT_FOREVER, nil);
    if FHandle = INVALID_HANDLE_VALUE then
      RaiseLastWin32Error;

    FThread := TlvkiWatchThread.Create(Self, FHandle);
  end;
end;

procedure TlvkCustomMailslotServer.SetMaxMessageSize(const Value: Integer);
begin
  if Value <> FMaxMessageSize then
  begin
    Active := False;
    FMaxMessageSize := Value;
  end;
end;

{ TlvkCustomMailslotClient }

procedure TlvkCustomMailslotClient.OpenMailslot;
begin
  if not (csDesigning in ComponentState) then
  begin
    FHandle := CreateFile(PChar(FMailslotName), GENERIC_WRITE, FILE_SHARE_READ,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if FHandle = INVALID_HANDLE_VALUE then
      RaiseLastWin32Error;
  end;
end;

procedure TlvkCustomMailslotClient.Send(const Obj: TComponent);
var
  Binary  : TMemoryStream;
  Text    : TMemoryStream;
  s       : string;
begin
  Assert(Assigned(Obj));

  Binary := TMemoryStream.Create;
  try
    Binary.WriteComponent(Obj);

    Text := TMemoryStream.Create;
    try
      Binary.Position := 0;
      ObjectBinaryToText(Binary, Text);

      Text.Position := 0;
      SetLength(s, Text.Size);
      Text.ReadBuffer(s[1], Text.Size);
      Write(s[1], Text.Size, dtClass);
    finally
      FreeAndNil(Text);
    end;
  finally
    FreeAndNil(Binary);
  end;
end;

procedure TlvkCustomMailslotClient.Send(const Strm: TStream);
var
  p : Pointer;
begin
  Strm.Position := 0;
  if Strm.Size > 0 then
  begin
    GetMem(p, Strm.Size);
    try
      Strm.ReadBuffer(p^, Strm.Size);
      Write(p^, Strm.Size, dtStream);
    finally
      FreeMemory(p);
    end;
  end;
end; 

procedure TlvkCustomMailslotClient.Write(const Data; const Size: Integer; const DataType: TlvkDataType);
var
  BytesWritten  : Cardinal;
  Temp          : PChar;
begin
  if not Active then
    raise ElvkMailslot.Create(SWriteNoActive);

  GetMem(Temp, Size + 1);
  try
    Temp^ := Char(DataType);
    Move(Data, (Temp + 1)^, Size);
    if not WriteFile(FHandle, Temp^, Size+1, BytesWritten, nil) then
      RaiseLastWin32Error;
    if BytesWritten <> Cardinal(Size+1) then
      raise ElvkMailslot.Create(SWriteIncomplete);
  finally
    FreeMem(Temp);
  end;
end;

procedure TlvkCustomMailslotClient.Write(const l: LongWord);
begin
  Write(l, 4, dtLongWord);
end;

procedure TlvkCustomMailslotClient.Write(const Str: string);
begin
  if Str <> '' then
    Write(Str[1], Length(Str), dtString);
end;

end.

