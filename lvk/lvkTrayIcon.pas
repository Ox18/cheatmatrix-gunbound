{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains components for adding tray icon support to your
    application.
}
unit lvkTrayIcon;

// $Author: Lasse V. Karlsen $
// $Date: 28.04.03 0:29 $
// $Revision: 9 $
// $Archive: /Components/LVK/Source/lvkTrayIcon.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Messages, Menus, Graphics, ExtCtrls, ShellApi, Forms,
  Windows, Controls, lvkVersion, lvkSubclass;

var
  TrayIconMessage : Cardinal;

type
  { Description:
      This is the custom base component for the tray icon components in this
      unit.
    See also:
      TlvkTrayIcon, TlvkAnimatedTrayIcon
  }
  TlvkCustomTrayIcon = class(TlvkSubClass)
  private
    FActive             : Boolean;
    FOnClick            : TNotifyEvent;
    FOnDblClick         : TNotifyEvent;
    FOnHideToTray       : TNotifyEvent;
    FOnRestoreFromTray  : TNotifyEvent;
    FPopupMenu          : TPopupMenu;
    FIcon               : TIcon;
    FTimer              : TTimer;
    FHint               : string;
    FNID                : TNotifyIconData;
    FDefaultIcon        : THandle;
    FCloseToTray        : Boolean;
    FMinimizeToTray     : Boolean;
    FAutoRestore        : Boolean;
    FHidden             : Boolean;

    procedure DoActivate;
    procedure DoDeactivate;
    procedure DoClick;
    procedure DoDblClick;
    procedure DoRestoreFromTray;
    procedure DoHideToTray;
    function GetCurrentIcon: THandle;

    procedure SetActive(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetIcon(const Value: TIcon);
    procedure SetHidden(const Value: Boolean);

    procedure OnLeftClickTimer(Sender: TObject);
    procedure IconChanged(Sender: TObject);

    procedure SendTrayMessage(const Msg, Flags: Cardinal);

  protected
    procedure Loaded; override;

    procedure ReCreate;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    { Description:
        This property determines wether or not the tray icon shows up in the
        tray area.
      See also:
        Activate, Deactivate
    }
    property Active: Boolean read FActive write SetActive default True;

    { Description:
        This property controls which popup menu component to use for the right-
        click popup menu of the tray icon.
    }
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;

    { Description:
        This property controls the tooltip hint of the tray icon.
    }
    property Hint: string read FHint write SetHint;

    { Description:
        This property controls the icon appearance of the tray icon. Use a
        16x16 icon for tray icons.
    }
    property Icon: TIcon read FIcon write SetIcon;

    { Description:
        Set this property to True to have the [x] button in the top right
        corner of the window close the application to the tray instead of
        closing it completely. Set it to False to have the application close
        as normal.

        Default is True.
      See also:
        MinimizeToTray, AutoRestore
    }
    property CloseToTray: Boolean read FCloseToTray write FCloseToTray
      default True;

    { Description:
        Set this property to True to have the [_] button in the top right
        corner of the window minimize the application to the tray instead of
        to the task bar. Set it to False to have the application minimize as
        normal.

        Default is True.
      See also:
        CloseToTray, AutoRestore
    }
    property MinimizeToTray: Boolean read FMinimizeToTray write FMinimizeToTray
      default True;

    { Description:
        Set this property to True to have double-clicks on the tray icon
        restore the application automatically.
      See also:
        MinimizeToTray, CloseToTray
    }
    property AutoRestore: Boolean read FAutoRestore write FAutoRestore
      default True;

    { Description:
        This property controls wether the application is currently hidden to
        the tray. Set it to True to hide it, set it to False to show/restore
        it.
      See also:
        HideToTray, RestoreFromTray
    }
    property HiddenToTray: Boolean read FHidden write SetHidden;

    { Description:
        Event handler will be called when the user clicks once on the
        tray icon.
      See also:
        OnDblClick
    }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    { Description:
        Event handler will be called when the user double-clicks on the
        tray icon.
      See also:
        OnClick
    }
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;

    { Description:
        Event handler will be called whenever the application hides to the
        tray.
      See also:
        OnRestoreFromTray
    }
    property OnHideToTray: TNotifyEvent read FOnHideToTray write FOnHideToTray;

    { Description:
        Event handler will be called whenever the application restores from
        the tray.
      See also:
        OnHideToTray
    }
    property OnRestoreFromTray: TNotifyEvent read FOnRestoreFromTray
      write FOnRestoreFromTray;

    procedure BeforeTargetWindowProc(var Message: TMessage;
      var PassToTarget: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        This method activates (shows) the tray icon in the tray area.

        It is functionally equivalent to setting Active to True.
      See also:
        Active, Deactivate
    }
    procedure Activate;

    { Description:
        This methods deactivates (hides) the tray icon from the tray area.

        It is functionally equivalent to setting Active to False.
      See also:
        Active, Activate
    }
    procedure Deactivate;

    { Description:
        This method hides the application to the tray.
      See also:
        RestoreFromTray, HiddenToTray
    }
    procedure HideToTray;

    { Description:
        This method restores the application from the tray.
      See also:
        HideToTray, HiddenToTray
    }
    procedure RestoreFromTray;
  end;

  { Description:
      This is a simple tray-icon component that
      shows a icon in the bottom right tray area of
      the task bar. It provides events for clicking on it,
      along with a popup menu and the ability to minimize
      or close the application to the tray.
  }
  TlvkTrayIcon = class(TlvkCustomTrayIcon)
  public
    // <ALIAS TlvkCustomTrayIcon.HiddenToTray>
    property HiddenToTray;

  published
    // <ALIAS TlvkCustomTrayIcon.Active>
    property Active;
    // <ALIAS TlvkCustomTrayIcon.OnClick>
    property OnClick;
    // <ALIAS TlvkCustomTrayIcon.OnDblClick>
    property OnDblClick;
    // <ALIAS TlvkCustomTrayIcon.PopupMenu>
    property PopupMenu;
    // <ALIAS TlvkCustomTrayIcon.Hint>
    property Hint;
    // <ALIAS TlvkCustomTrayIcon.Icon>
    property Icon;
    // <ALIAS TlvkCustomTrayIcon.CloseToTray>
    property CloseToTray;
    // <ALIAS TlvkCustomTrayIcon.MinimizeToTray>
    property MinimizeToTray;
    // <ALIAS TlvkCustomTrayIcon.AutoRestore>
    property AutoRestore;
    // <ALIAS TlvkCustomTrayIcon.OnHideToTray>
    property OnHideToTray;
    // <ALIAS TlvkCustomTrayIcon.OnRestoreFromTray>
    property OnRestoreFromTray;
  end;

  { Description:
      This component provides an animated tray icon. In all other respects,
      it works like TlvkTrayIcon.
    See also:
      TlvkCustomTrayIcon, TlvkTrayIcon
  }
  TlvkCustomAnimatedTrayIcon = class(TlvkCustomTrayIcon)
  private
    FImageList              : TImageList;
    FInterval               : Integer;
    FTimer                  : TTimer;
    FAnimated               : Boolean;
    FIconIndex              : Integer;
    FSkipFirstWhenAnimated  : Boolean;

    procedure UpdateIcon;

    procedure ChangeIcon(Sender: TObject);

    procedure SetImageList(const Value: TImageList);
    procedure SetAnimated(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
    procedure SetSkipFirstWhenAnimated(const Value: Boolean);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    { Description:
        This property controls wether to skip the first image in the image list
        when animating. This way you can use one icon when it's not animated,
        and the rest of the list when it's animated.

        Default is True.
    }
    property SkipFirstWhenAnimated: Boolean read FSkipFirstWhenAnimated
      write SetSkipFirstWhenAnimated default True;

    { Description:
        This property controls wether the tray icon shows an animation or not.
        If not, it will show the first image in the image list.
      See also:
        ImageList, Interval
    }
    property Animated: Boolean read FAnimated write SetAnimated;

    { Description:
        This property controls which image list that holds the different icons
        to use for the tray icon. The images should be 16x16.
      See also:
        Animated, Interval
    }
    property ImageList: TImageList read FImageList write SetImageList;

    { Description:
        This property controls how fast the tray icon will switch to a new
        image. The value is given in milliseconds, so a value of 250 will
        switch to the next image four times a second.
      See also:
        Animated, ImageList
    }
    property Interval: Integer read FInterval write SetInterval;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This is an animated version of TlvkTrayIcon. Provide
      it with a image list and it will loop through the
      images.
  }
  TlvkAnimatedTrayIcon = class(TlvkCustomAnimatedTrayIcon)
  public
    // <ALIAS TlvkCustomTrayIcon.HiddenToTray>
    property HiddenToTray;

  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;

    // <ALIAS TlvkCustomAnimatedTrayIcon.SkipFirstWhenAnimated>
    property SkipFirstWhenAnimated;
    // <ALIAS TlvkCustomTrayIcon.Icon>
    property Icon;
    // <ALIAS TlvkCustomTrayIcon.Active>
    property Active;
    // <ALIAS TlvkCustomTrayIcon.OnClick>
    property OnClick;
    // <ALIAS TlvkCustomTrayIcon.OnDblClick>
    property OnDblClick;
    // <ALIAS TlvkCustomTrayIcon.PopupMenu>
    property PopupMenu;
    // <ALIAS TlvkCustomTrayIcon.Hint>
    property Hint;
    // <ALIAS TlvkCustomTrayIcon.CloseToTray>
    property CloseToTray;
    // <ALIAS TlvkCustomTrayIcon.MinimizeToTray>
    property MinimizeToTray;
    // <ALIAS TlvkCustomTrayIcon.AutoRestore>
    property AutoRestore;
    // <ALIAS TlvkCustomTrayIcon.OnHideToTray>
    property OnHideToTray;
    // <ALIAS TlvkCustomTrayIcon.OnRestoreFromTray>
    property OnRestoreFromTray;

    // <ALIAS TlvkCustomAnimatedTrayIcon.Animated>
    property Animated;
    // <ALIAS TlvkCustomAnimatedTrayIcon.ImageList>
    property ImageList;
    // <ALIAS TlvkCustomAnimatedTrayIcon.Interval>
    property Interval;
  end;

implementation

type
  TTrayIconManager = class
  private
    FHandle : HWND;
    FIcons  : TList;

  protected
    procedure WndProc(var Message: TMessage);

    procedure RegisterIcon(const Icon: TlvkCustomTrayIcon);
    procedure UnRegisterIcon(const Icon: TlvkCustomTrayIcon);

  public
    constructor Create;
    destructor Destroy; override;

    property Handle: HWND read FHandle;
  end;

var
  TrayIconManager       : TTrayIconManager;
  TaskBarCreatedMessage : Cardinal;

{ TTrayIconManager }

constructor TTrayIconManager.Create;
begin
  inherited;

  FIcons := TList.Create;
  FHandle := AllocateHWnd(WndProc);
end;

destructor TTrayIconManager.Destroy;
begin
  DeallocateHWnd(FHandle);
  FIcons.Free;

  inherited;
end;

procedure TTrayIconManager.RegisterIcon(const Icon: TlvkCustomTrayIcon);
begin
  FIcons.Add(Icon);
end;

procedure TTrayIconManager.UnRegisterIcon(const Icon: TlvkCustomTrayIcon);
begin
  FIcons.Remove(Icon);
end;

procedure TTrayIconManager.WndProc(var Message: TMessage);
var
  TrayIcon  : TlvkTrayIcon;
  Point     : TPoint;
  Index     : Integer;
begin
  if Message.Msg = TaskBarCreatedMessage then
  begin
    for Index := 0 to FIcons.Count-1 do
      TlvkCustomTrayIcon(FIcons[Index]).ReCreate;
  end;

  if Message.Msg = TrayIconMessage then
  begin
    TrayIcon := TlvkTrayIcon(Message.wParam);
    case Message.lParam of
      WM_MOUSEMOVE:
        begin
          TrayIcon.FTimer.Enabled := True;
        end;

      WM_LBUTTONDOWN:
        begin
          TrayIcon.FTimer.Enabled := True;
        end;

      WM_LBUTTONUP:
        begin
        end;

      WM_LBUTTONDBLCLK:
        begin
          TrayIcon.FTimer.Enabled := False;
          TrayIcon.DoDblClick;
        end;

      WM_RBUTTONDOWN:
        begin
          if Assigned(TrayIcon.PopupMenu) then
          begin
            SetForegroundWindow(FHandle);
            GetCursorPos(Point);
            TrayIcon.PopupMenu.Popup(Point.X, Point.Y);

            // Hack to force task switch
            PostMessage(FHandle, WM_USER, 0, 0);
          end;
        end;

      WM_RBUTTONUP:
        begin
        end;

      WM_RBUTTONDBLCLK:
        begin
        end;
    end;
  end else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam,
      Message.LParam);
end;

{ TlvkCustomTrayIcon }

procedure TlvkCustomTrayIcon.Activate;
begin
  Active := True;
end;

procedure TlvkCustomTrayIcon.BeforeTargetWindowProc(var Message: TMessage;
  var PassToTarget: Boolean);
begin
  inherited;

  case Message.Msg of
    WM_WINDOWPOSCHANGED:
      begin
        if TForm(Owner).WindowState = wsMinimized then
        begin
          if FMinimizeToTray then
            HideToTray;
        end;
      end;

    WM_SYSCOMMAND:
      begin
        if ((Message.WParam = SC_MINIMIZE) and FMinimizeToTray) or
          ((Message.WParam = SC_CLOSE) and FCloseToTray) then
        begin
          HideToTray;
          PassToTarget := False;
        end;
      end;
  end;
end;

constructor TlvkCustomTrayIcon.Create(AOwner: TComponent);
begin
  inherited;

  if not (AOwner is TForm) then
    raise Exception.Create('A tray icon component must be dropped on a form');

  SubClassTarget := AOwner as TWinControl;
  SubClassed := True;

  TrayIconManager.RegisterIcon(Self);

  FDefaultIcon := LoadIcon(0, IDI_WINLOGO);

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := GetDoubleClickTime;
  FTimer.OnTimer := OnLeftClickTimer;

  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;

  FActive := True;
  FMinimizeToTray := True;
  FCloseToTray := True;
  FAutoRestore := True;
end;

procedure TlvkCustomTrayIcon.Deactivate;
begin
  Active := False;
end;

destructor TlvkCustomTrayIcon.Destroy;
begin
  Deactivate;
  FTimer.Free;
  FIcon.Free;

  TrayIconManager.UnRegisterIcon(Self);

  inherited;
end;

procedure TlvkCustomTrayIcon.DoActivate;
begin
  SendTrayMessage(NIM_ADD, NIF_MESSAGE or NIF_ICON or NIF_TIP);
end;

procedure TlvkCustomTrayIcon.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TlvkCustomTrayIcon.DoDblClick;
begin
  if FAutoRestore then
  begin
    RestoreFromTray;
    SetForegroundWindow(Application.Handle);
  end;
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TlvkCustomTrayIcon.DoDeactivate;
begin
  SendTrayMessage(NIM_DELETE, NIF_MESSAGE or NIF_ICON or NIF_TIP);
end;

procedure TlvkCustomTrayIcon.DoHideToTray;
begin
  if Assigned(FOnHideToTray) then
    FOnHideToTray(Self);
end;

procedure TlvkCustomTrayIcon.DoRestoreFromTray;
begin
  if Assigned(FOnRestoreFromTray) then
    FOnRestoreFromTray(Self);
end;

function TlvkCustomTrayIcon.GetCurrentIcon: THandle;
begin
  if FIcon.Handle <> 0 then
    Result := FIcon.Handle
  else
    Result := FDefaultIcon;
end;

procedure TlvkCustomTrayIcon.HideToTray;
begin
  if not FHidden then
  begin
    DoHideToTray;

    ShowWindow(Application.Handle, SW_HIDE);
    TForm(Owner).Hide;
    FHidden := True;
  end;
end;

procedure TlvkCustomTrayIcon.IconChanged(Sender: TObject);
begin
  if Active then
    SendTrayMessage(NIM_MODIFY, NIF_ICON);
end;

procedure TlvkCustomTrayIcon.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    if FActive then
      DoActivate;
  end;
end;

procedure TlvkCustomTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FPopupMenu then
      FPopupMenu := nil;
  end;
end;

procedure TlvkCustomTrayIcon.OnLeftClickTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  DoClick;
end;

procedure TlvkCustomTrayIcon.ReCreate;
begin
  if Active then
  begin
    DoDeactivate;
    DoActivate;
  end;
end;

procedure TlvkCustomTrayIcon.RestoreFromTray;
begin
  if FHidden then
  begin
    DoRestoreFromTray;
    
    ShowWindow(Application.Handle, SW_NORMAL);
    TForm(Owner).Show;
    FHidden := False;
  end;
end;

procedure TlvkCustomTrayIcon.SendTrayMessage(const Msg, Flags: Cardinal);
begin
  if csDesigning in ComponentState then
    Exit;

  FNID.cbSize := SizeOf(FNID);
  StrPLCopy(FNID.szTip, PChar(FHint), SizeOf(FNID.szTip));
  FNID.uCallbackMessage := TrayIconMessage;
  FNID.Wnd := TrayIconManager.Handle;
  FNID.uID := Cardinal(Self);
  FNID.uFlags := Flags;
  FNID.hIcon := GetCurrentIcon;

  Shell_NotifyIcon(Msg, @FNID);
end;

procedure TlvkCustomTrayIcon.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if not (csLoading in ComponentState) then
    begin
      if Value then
        DoActivate
      else
        DoDeactivate;
    end;

    FActive := Value;
  end;
end;

procedure TlvkCustomTrayIcon.SetHidden(const Value: Boolean);
begin
  if Value <> FHidden then
  begin
    if Value then
      HideToTray
    else
      RestoreFromTray;
  end;
end;

procedure TlvkCustomTrayIcon.SetHint(const Value: string);
begin
  if Value <> FHint then
  begin
    FHint := Value;

    if Active then
      SendTrayMessage(NIM_MODIFY, NIF_TIP);
  end;
end;

procedure TlvkCustomTrayIcon.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
end;

{ TlvkCustomAnimatedTrayIcon }

procedure TlvkCustomAnimatedTrayIcon.ChangeIcon(Sender: TObject);
begin
  if Assigned(FImageList) then
  begin
    Inc(FIconIndex);
    if FIconIndex >= FImageList.Count then
    begin
      FIconIndex := 0;
      if FSkipFirstWhenAnimated and (FImageList.Count > 1) then
        Inc(FIconIndex);
    end;

    UpdateIcon;
  end;
end;

constructor TlvkCustomAnimatedTrayIcon.Create(AOwner: TComponent);
begin
  inherited;

  FInterval := 500;
  FAnimated := True;
  FSkipFirstWhenAnimated := True;
  
  if not (csDesigning in ComponentState) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.Enabled := False;
    FTimer.OnTimer := ChangeIcon;
  end;

  if (not (csLoading in ComponentState)) and Assigned(FTimer) then
  begin
    FTimer.Interval := FInterval;
    FTimer.Enabled := True;
  end;
end;

destructor TlvkCustomAnimatedTrayIcon.Destroy;
begin
  FTimer.Free;

  inherited;
end;

procedure TlvkCustomAnimatedTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FImageList then
      SetImageList(nil);
  end;
end;

procedure TlvkCustomAnimatedTrayIcon.SetAnimated(const Value: Boolean);
begin
  if Value <> FAnimated then
  begin
    FAnimated := Value;

    if not FAnimated then
    begin
      FIconIndex := 0;
      UpdateIcon;
    end;

    if Assigned(FTimer) then
    begin
      FTimer.Enabled := False;
      FTimer.Enabled := FAnimated;

      if FAnimated and FSkipFirstWhenAnimated and Assigned(FImageList) and (FImageList.Count > 1) and (FIconIndex = 0) then
      begin
        FIconIndex := 1;
        UpdateIcon;
      end;
    end;
  end;
end;

procedure TlvkCustomAnimatedTrayIcon.SetImageList(const Value: TImageList);
begin
  if Value <> FImageList then
  begin
    FImageList := Value;
    FIconIndex := 0;
    UpdateIcon;
  end;
end;

procedure TlvkCustomAnimatedTrayIcon.SetInterval(const Value: Integer);
begin
  if Value <> FInterval then
  begin
    if Value < 0 then
      FInterval := 0
    else
      FInterval := Value;

    if Assigned(FTimer) then
      FTimer.Interval := FInterval;
  end;
end;

procedure TlvkCustomAnimatedTrayIcon.SetSkipFirstWhenAnimated(
  const Value: Boolean);
begin
  if Value <> FSkipFirstWhenAnimated then
  begin
    FSkipFirstWhenAnimated := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FAnimated and (FIconIndex = 0) and Assigned(FImageList) and (FImageList.Count > 1) then
      begin
        Inc(FIconIndex);
        UpdateIcon;
      end;
    end;
  end;
end;

procedure TlvkCustomAnimatedTrayIcon.UpdateIcon;
var
  NewIcon : TIcon;
begin
  if Assigned(FImageList) and (FIconIndex < FImageList.Count) then
  begin
    NewIcon := TIcon.Create;
    try
      FImageList.GetIcon(FIconIndex, NewIcon);
      inherited Icon := NewIcon;
    finally
      NewIcon.Free;
    end;
  end else begin
    (inherited Icon).Assign(nil);
  end;
end;

initialization
  TrayIconMessage := RegisterWindowMessage('lvk.TrayIconMessage');
  TaskBarCreatedMessage := RegisterWindowMessage('TaskbarCreated');

  TrayIconManager := TTrayIconManager.Create;
finalization
  TrayIconManager.Free;
end.
