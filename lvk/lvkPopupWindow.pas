{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the popup window control used for the drop down
    edit control.
}
unit lvkPopupWindow;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkPopupWindow.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Types,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Messages;

type
  TCloseUpEvent = procedure(Sender: TObject) of object;

  TlvkCustomPopupWindow = class(TCustomControl)
  private
    FOnCloseUp    : TCloseUpEvent;
    FFramed       : Boolean;
    FHasSizeGrip  : Boolean;

    procedure SetFramed(const Value: Boolean);

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure RequestAlign; override;

    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    property OnCloseUp: TCloseUpEvent read FOnCloseUp write FOnCloseUp;
    property Framed: Boolean read FFramed write SetFramed default True;
    property HasSizeGrip: Boolean read FHasSizeGrip write FHasSizeGrip default False;

  public
    constructor Create(AOwner: TComponent); override;
    function CanFocus: Boolean; override;

    procedure Place(const Left, Top, Width, Height: Integer);
    procedure CloseUp;
  end;

implementation

uses
  lvkDropDownEdit, Windows;

{ TlvkCustomPopupWindow }

function TlvkCustomPopupWindow.CanFocus: Boolean;
begin
  Result := True;
end;

procedure TlvkCustomPopupWindow.CloseUp;
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
  Hide;
end;

constructor TlvkCustomPopupWindow.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csDesignInteractive,
    csClickEvents, csFramed, csDoubleClicks]; 
  Visible := False;
  Color := clBtnFace;

  FFramed := True;
end;

procedure TlvkCustomPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := WS_POPUP or WS_CLIPCHILDREN;
  if FFramed then
    Params.Style := Params.Style or WS_BORDER;
  Params.ExStyle := WS_EX_TOOLWINDOW;
end;

procedure TlvkCustomPopupWindow.CreateWnd;
begin
  inherited;

  if csDesigning in ComponentState then
    SetParent(nil);
end;

procedure TlvkCustomPopupWindow.Place(const Left, Top, Width, Height: Integer);
begin
  SetWindowPos(Handle, HWND_TOP, Left, Top, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  SetBounds(Left, Top, Width, Height);

  Visible := True;
end;

procedure TlvkCustomPopupWindow.RequestAlign;
begin
  // Do nothing here, to avoid adding scrollbars to underlying form
end;

procedure TlvkCustomPopupWindow.SetFramed(const Value: Boolean);
begin
  if FFramed <> Value then
  begin
    FFramed := Value;
    RecreateWnd;
  end;
end;

procedure TlvkCustomPopupWindow.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active = WA_INACTIVE then
    CloseUp
  else if (Message.Active <> WA_INACTIVE) and (Message.ActiveWindow <> 0) then
    SendMessage(Message.ActiveWindow, WM_NCACTIVATE, 1, 0);
end;

procedure TlvkCustomPopupWindow.WMActivateApp(var Message: TWMActivateApp);
begin
  inherited;
  if not Message.Active then
    CloseUp;
end;

procedure TlvkCustomPopupWindow.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TlvkCustomPopupWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  p : TPoint;
begin
  if HasSizeGrip then
  begin
    p := ScreenToClient(Point(Message.XPos, Message.YPos));
    if (p.X >= Width-3) and (p.X < Width+3) and (p.Y >= Height-3) and (p.Y < Height+3) then
      Message.Result := HTBOTTOMRIGHT
    else
      inherited;
  end else
    inherited;
end;

end.
