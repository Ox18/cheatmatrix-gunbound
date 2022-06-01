{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains code that can be used on Windows XP. Some of the code
    might not be XP-specific, but the code was designed for use with XP.
}
unit lvkXP;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkXP.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
{$IFDEF DELPHI6UP}
  Types,
{$ELSE}
  Forms,
{$ENDIF}
  Windows, Messages, Classes, SysUtils, lvkVersion;

type
  { Description:
      This filter is used in the TlvkCustomWTSSessionChange and
      TlvkWTSSessionChange components to filter out messages that will be
      handled.

        * sfCurrentSession - Only messages related to the session that the
          program is running in will be handled.
        * sfAllSessions - Messages related to any and all sessions on the
          system will be handled.
    See also:
      TlvkCustomWTSSessionChange.SessionFilter
  }
  TSessionFilter = (sfCurrentSession, sfAllSessions);

  { Description:
      This enumerated data type is used when calling the event handler for a
      session change notification.

        * ntConsoleConnect - Someone connected to the console (re-logged in to
          the existing session)
        * ntConsoleDisconnect - Someone disconnected from the console (this
          means going back to the welcome screen)
        * ntRemoteConnect - Same as ntConsoleConnect but this connection came
          from a remote location.
        * ntRemoteDisconnect - Same as ntConsoleDisconnect but this connection
          came from a remote location.
        * ntSessionLogon - Someone logged on to a new session.
        * ntSessionLogoff - Someone logged off from an existing session.
        * ntSessionLock - A session was locked by the user.
        * ntSessionUnlock - A session was unlocked by the user.
    See also:
      TSessionChangeEvent
  }
  TNotificationType = (ntConsoleConnect, ntConsoleDisconnect,
    ntRemoteConnect, ntRemoteDisconnect, ntSessionLogon, ntSessionLogoff,
    ntSessionLock, ntSessionUnlock);

  { Description:
      This event handler type is used for the event handlers of the
      TlvkCustomWTSSessionChange and TlvkWTSSessionChange components.
    Parameters:
      Sender - The component that sent the message.
      NotificationType - What kind of session change event took place.
      Session - The handle to the session involved.
    See also:
      TlvkCustomWTSSessionChange
  }
  TSessionChangeEvent = procedure(const Sender: TObject;
    const NotificationType: TNotificationType;
    const Session: LongWord) of object;

const
  DEFAULT_ACTIVE          = False;
  DEFAULT_SESSION_FILTER  = sfCurrentSession;

type
  { Description:
      This is the custom version of the TlvkWTSSessionChange component. It
      implements all the code related to the session change notification.
    See also:
      TlvkWTSSessionChange
  }
  TlvkCustomWTSSessionChange = class(TComponent)
  private
    FHandle               : THandle;
    FActive               : Boolean;
    FSetActive            : Boolean;
    FSessionFilter        : TSessionFilter;

    FOnConsoleConnect     : TSessionChangeEvent;
    FOnConsoleDisconnect  : TSessionChangeEvent;
    FOnRemoteConnect      : TSessionChangeEvent;
    FOnRemoteDisconnect   : TSessionChangeEvent;
    FOnSessionLogon       : TSessionChangeEvent;
    FOnSessionLogoff      : TSessionChangeEvent;
    FOnSessionLock        : TSessionChangeEvent;
    FOnSessionUnlock      : TSessionChangeEvent;

    procedure WindProc(var Message: TMessage);
    procedure SetActive(const Value: Boolean);
    procedure SetSessionFilter(const Value: TSessionFilter);

    procedure DoActivate;
    procedure DoDeactivate;

    procedure DoConsoleConnect(const Message: TMessage);
    procedure DoConsoleDisconnect(const Message: TMessage);
    procedure DoRemoteConnect(const Message: TMessage);
    procedure DoRemoteDisconnect(const Message: TMessage);
    procedure DoSessionLogon(const Message: TMessage);
    procedure DoSessionLogoff(const Message: TMessage);
    procedure DoSessionLock(const Message: TMessage);
    procedure DoSessionUnlock(const Message: TMessage);
    procedure SetPackageVersion(const Value: TPackageVersion);

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

    procedure Loaded; override;

    { Description:
        This property describes the current state of the component. Only when
        this property is True does the component react to notification
        messages.
    }
    property Active: Boolean read FActive write SetActive
      default DEFAULT_ACTIVE;

    { Description:
        This specifies the filter for the events. Check out the TSessionFilter
        data type for possible values.
    }
    property SessionFilter: TSessionFilter read FSessionFilter
      write SetSessionFilter default DEFAULT_SESSION_FILTER;

    { Description:
        This event handler is called whenever a ntConsoleConnect message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleDisconnect, OnRemoteConnect, OnRemoteDisconnect,
        OnSessionLogon, OnSessionLogoff, OnSessionLock, OnSessionUnlock
    }
    property OnConsoleConnect: TSessionChangeEvent read FOnConsoleConnect
      write FOnConsoleConnect;

    { Description:
        This event handler is called whenever a ntConsoleDisconnect message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleConnect, OnRemoteConnect, OnRemoteDisconnect, OnSessionLogon,
        OnSessionLogoff, OnSessionLock, OnSessionUnlock
    }
    property OnConsoleDisconnect: TSessionChangeEvent read FOnConsoleDisconnect
      write FOnConsoleDisconnect;

    { Description:
        This event handler is called whenever a ntRemoteConnect message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleConnect, OnConsoleDisconnect, OnRemoteDisconnect,
        OnSessionLogon, OnSessionLogoff, OnSessionLock, OnSessionUnlock
    }
    property OnRemoteConnect: TSessionChangeEvent read FOnRemoteConnect
      write FOnRemoteConnect;

    { Description:
        This event handler is called whenever a ntRemoteDisconnect message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleConnect, OnConsoleDisconnect, OnRemoteConnect,
        OnSessionLogon, OnSessionLogoff, OnSessionLock, OnSessionUnlock
    }
    property OnRemoteDisconnect: TSessionChangeEvent read FOnRemoteDisconnect
      write FOnRemoteDisconnect;

    { Description:
        This event handler is called whenever a ntSessionLogon message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleConnect, OnConsoleDisconnect, OnRemoteConnect,
        OnRemoteDisconnect, OnSessionLogoff, OnSessionLock, OnSessionUnlock
    }
    property OnSessionLogon: TSessionChangeEvent read FOnSessionLogon
      write FOnSessionLogon;

    { Description:
        This event handler is called whenever a ntSessionLogoff message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleConnect, OnConsoleDisconnect, OnRemoteConnect,
        OnRemoteDisconnect, OnSessionLogon, OnSessionLock, OnSessionUnlock
    }
    property OnSessionLogoff: TSessionChangeEvent read FOnSessionLogoff
      write FOnSessionLogoff;

    { Description:
        This event handler is called whenever a ntSessionLock message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleConnect, OnConsoleDisconnect, OnRemoteConnect,
        OnRemoteDisconnect, OnSessionLogon, OnSessionLogoff, OnSessionUnlock
    }
    property OnSessionLock: TSessionChangeEvent read FOnSessionLock
      write FOnSessionLock;

    { Description:
        This event handler is called whenever a ntSessionUnlock message
        is handled.
      See also:
        TSessionChangeEvent,
        OnConsoleConnect, OnConsoleDisconnect, OnRemoteConnect,
        OnRemoteDisconnect, OnSessionLogon, OnSessionLogoff, OnSessionLock
    }
    property OnSessionUnlock: TSessionChangeEvent read FOnSessionUnlock
      write FOnSessionUnlock;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This is the usable component version of TlvkCustomWTSSessionChange. See
      TlvkCustomWTSSessionChange for details.
    See also:
      TlvkCustomWTSSessionChange
  }
  TlvkWTSSessionChange = class(TlvkCustomWTSSessionChange)
  published
    // <ALIAS TlvkCustomWTSSessionChange.Active>
    property Active;
    // <ALIAS TlvkCustomWTSSessionChange.SessionFilter>
    property SessionFilter;
    // <ALIAS TlvkCustomWTSSessionChange.OnConsoleConnect>
    property OnConsoleConnect;
    // <ALIAS TlvkCustomWTSSessionChange.OnConsoleDisconnect>
    property OnConsoleDisconnect;
    // <ALIAS TlvkCustomWTSSessionChange.OnRemoteConnect>
    property OnRemoteConnect;
    // <ALIAS TlvkCustomWTSSessionChange.OnRemoteDisconnect>
    property OnRemoteDisconnect;
    // <ALIAS TlvkCustomWTSSessionChange.OnSessionLogon>
    property OnSessionLogon;
    // <ALIAS TlvkCustomWTSSessionChange.OnSessionLogoff>
    property OnSessionLogoff;
    // <ALIAS TlvkCustomWTSSessionChange.OnSessionLock>
    property OnSessionLock;
    // <ALIAS TlvkCustomWTSSessionChange.OnSessionUnlock>
    property OnSessionUnlock;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  EWTSSessionChange = class(Exception);

resourcestring
  ERR_WTS_NOT_DETECTED          = 'Terminal Services could not be located';
  ERR_WTS_UNABLE_TO_UNREGISTER  = 'Could not unregister from Terminal Services session change notification';
  ERR_WTS_UNABLE_TO_REGISTER    = 'Could not register for Terminal Services session change notification';

implementation

{ TlvkCustomWTSSessionChange }

const
  WM_WTSSESSION_CHANGE    = $02B1;

  WTS_CONSOLE_CONNECT     = $1;
  WTS_CONSOLE_DISCONNECT  = $2;
  WTS_REMOTE_CONNECT      = $3;
  WTS_REMOTE_DISCONNECT   = $4;
  WTS_SESSION_LOGON       = $5;
  WTS_SESSION_LOGOFF      = $6;
  WTS_SESSION_LOCK        = $7;
  WTS_SESSION_UNLOCK      = $8;

  NOTIFY_FOR_ALL_SESSIONS = 1;
  NOTIFY_FOR_THIS_SESSION = 0;

type
  TWTSRegisterSessionNotification = function(hWnd: HWND; dwFlags: DWORD): BOOL; stdcall;
  TWTSUnRegisterSessionNotification = function(hWnd: HWND): BOOL; stdcall;

var
  WTSRegisterSessionNotification    : TWTSRegisterSessionNotification;
  WTSUnRegisterSessionNotification  : TWTSUnRegisterSessionNotification;
  WTSDLL                            : THandle;

constructor TlvkCustomWTSSessionChange.Create(AOwner: TComponent);
begin
  inherited;

  FActive := False;
  FHandle := 0;

  if not (csDesigning in ComponentState) then
    FHandle := AllocateHWnd(WindProc)
  else
    FHandle := 0;
end;

destructor TlvkCustomWTSSessionChange.Destroy;
begin
  Active := False;
  if not (csDesigning in ComponentState) then
    DeallocateHWnd(FHandle);

  inherited;
end;

procedure TlvkCustomWTSSessionChange.DoActivate;
var
  rc  : Boolean;
begin
  if csDesigning in ComponentState then
    Exit;

  if csLoading in Componentstate then
  begin
    FSetActive := True;
    Exit;
  end;

  if not Assigned(WTSRegisterSessionNotification) then
    raise EWTSSessionChange.Create(ERR_WTS_NOT_DETECTED);
  if not Assigned(WTSUnRegisterSessionNotification) then
    raise EWTSSessionChange.Create(ERR_WTS_NOT_DETECTED);

  case FSessionFilter of
    sfCurrentSession  : rc := WTSRegisterSessionNotification(FHandle, NOTIFY_FOR_THIS_SESSION);
    sfAllSessions     : rc := WTSRegisterSessionNotification(FHandle, NOTIFY_FOR_ALL_SESSIONS);
  else
    rc := False;
  end;

  if rc then
    FActive := True
  else
    raise EWTSSessionChange.Create(ERR_WTS_UNABLE_TO_REGISTER);
end;

procedure TlvkCustomWTSSessionChange.DoConsoleConnect(
  const Message: TMessage);
begin
  if Assigned(FOnConsoleConnect) then
    FOnConsoleConnect(Self, ntConsoleConnect, Message.LParam);
end;

procedure TlvkCustomWTSSessionChange.DoConsoleDisconnect(
  const Message: TMessage);
begin
  if Assigned(FOnConsoleDisconnect) then
    FOnConsoleDisconnect(Self, ntConsoleDisconnect, Message.LParam);
end;

procedure TlvkCustomWTSSessionChange.DoDeactivate;
var
  rc  : Boolean;
begin
  if csDesigning in ComponentState then
    Exit;

  if csLoading in Componentstate then
  begin
    FSetActive := False;
    Exit;
  end;

  if not Assigned(WTSRegisterSessionNotification) then
    raise EWTSSessionChange.Create(ERR_WTS_NOT_DETECTED);
  if not Assigned(WTSUnRegisterSessionNotification) then
    raise EWTSSessionChange.Create(ERR_WTS_NOT_DETECTED);

  rc := WTSUnRegisterSessionNotification(FHandle);

  if rc then
    FActive := False
  else
    raise EWTSSessionChange.Create(ERR_WTS_UNABLE_TO_UNREGISTER);
end;

procedure TlvkCustomWTSSessionChange.DoRemoteConnect(
  const Message: TMessage);
begin
  if Assigned(FOnRemoteConnect) then
    FOnRemoteConnect(Self, ntRemoteConnect, Message.LParam);
end;

procedure TlvkCustomWTSSessionChange.DoRemoteDisconnect(
  const Message: TMessage);
begin
  if Assigned(FOnRemoteDisconnect) then
    FOnRemoteDisconnect(Self, ntRemoteDisconnect, Message.LParam);
end;

procedure TlvkCustomWTSSessionChange.DoSessionLock(
  const Message: TMessage);
begin
  if Assigned(FOnSessionLock) then
    FOnSessionLock(Self, ntSessionLock, Message.LParam);
end;

procedure TlvkCustomWTSSessionChange.DoSessionLogoff(
  const Message: TMessage);
begin
  if Assigned(FOnSessionLogoff) then
    FOnSessionLogoff(Self, ntSessionLogoff, Message.LParam);
end;

procedure TlvkCustomWTSSessionChange.DoSessionLogon(
  const Message: TMessage);
begin
  if Assigned(FOnSessionLogon) then
    FOnSessionLogon(Self, ntSessionLogon, Message.LParam);
end;

procedure TlvkCustomWTSSessionChange.DoSessionUnlock(
  const Message: TMessage);
begin
  if Assigned(FOnSessionUnlock) then
    FOnSessionUnlock(Self, ntSessionUnlock, Message.LParam);
end;

function TlvkCustomWTSSessionChange.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomWTSSessionChange.Loaded;
begin
  inherited;

  if FSetActive then
    Active := True;
end;

procedure TlvkCustomWTSSessionChange.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if Value then
      DoActivate
    else
      DoDeactivate;
  end;
end;

procedure TlvkCustomWTSSessionChange.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomWTSSessionChange.SetSessionFilter(
  const Value: TSessionFilter);
var
  OldActive : Boolean;
begin
  if FSessionFilter <> Value then
  begin
    OldActive := Active;
    Active := True;

    FSessionFilter := Value;

    Active := OldActive;
  end;
end;

procedure TlvkCustomWTSSessionChange.WindProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_QUERYENDSESSION:
      Message.Result := 1;
      
    WM_WTSSESSION_CHANGE:
      begin
        case Message.WParam of
          WTS_CONSOLE_CONNECT     : DoConsoleConnect(Message);
          WTS_CONSOLE_DISCONNECT  : DoConsoleDisconnect(Message);
          WTS_REMOTE_CONNECT      : DoRemoteConnect(Message);
          WTS_REMOTE_DISCONNECT   : DoRemoteDisconnect(Message);
          WTS_SESSION_LOGON       : DoSessionLogon(Message);
          WTS_SESSION_LOGOFF      : DoSessionLogoff(Message);
          WTS_SESSION_LOCK        : DoSessionLock(Message);
          WTS_SESSION_UNLOCK      : DoSessionUnlock(Message);
        end;
      end;
  else
    DefaultHandler(Message);
  end;
end;

initialization
  WTSDLL := LoadLibrary('wtsapi32.dll');
  if WTSDLL <> 0 then
  begin
    WTSRegisterSessionNotification := GetProcAddress(WTSDLL, 'WTSRegisterSessionNotification');
    WTSUnRegisterSessionNotification := GetProcAddress(WTSDLL, 'WTSUnRegisterSessionNotification');
  end else begin
  end;
finalization
  WTSRegisterSessionNotification := nil;
  WTSUnRegisterSessionNotification := nil;
  if WTSDLL <> 0 then
    FreeLibrary(WTSDLL);
end.
