{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkSizeGrip sub-control used for resizable controls
    in the LVK library.
}
unit lvkSizeGrip;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSizeGrip.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Forms, Controls, Messages, Windows, Graphics;

type
  TlvkSizeGrip = class(TGraphicControl)
  private
    FResizingControl  : TWinControl;

    function GetResizingControl: TWinControl;

  protected
    procedure WMMouseActivate(var Message: TMessage);
      message WM_MOUSEACTIVATE;

    property ResizingControl: TWinControl read GetResizingControl;

    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    procedure ClickGrip(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  public
    constructor Create(AOwner: TComponent); override;
    procedure BeginResize;

    procedure PlaceOnControl(const Control: TWinControl);
  end;

const
  SIZEGRIP_SIZE = 13;

implementation

uses
  lvkPopupWindow;
  
{ TlvkSizeGrip }

procedure TlvkSizeGrip.BeginResize;
begin
  if Assigned(ResizingControl) then
  begin
    ReleaseCapture;
    ResizingControl.Perform(WM_SYSCOMMAND, $F008, 0);
  end;
end;

procedure TlvkSizeGrip.ClickGrip(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BeginResize;
end;

constructor TlvkSizeGrip.Create(AOwner: TComponent);
begin
  inherited;

  Align := alRight;
  Width := 13;
  OnMouseDown := ClickGrip;
  Cursor := crSizeNWSE;
end;

function TlvkSizeGrip.GetResizingControl: TWinControl;
begin
  if not Assigned(FResizingControl) then
  begin
    FResizingControl := Parent;
    while Assigned(FResizingControl) do
    begin
      if (FResizingControl is TCustomForm) or
        (FResizingControl is TlvkCustomPopupWindow) then
      begin
        Break;
      end else
        FResizingControl := FResizingControl.Parent;
    end;
  end;

  if Assigned(FResizingControl) then
    Result := FResizingControl
  else
    Result := nil;
end;

procedure TlvkSizeGrip.Paint;
const
  Grip  : array[1..13] of string = (
    '           wl',
    '          wdl',
    '         wddl',
    '        wddll',
    '       wddlwl',
    '      wddlwdl',
    '     wddlwddl',
    '    wddlwddll',
    '   wddlwddlwl',
    '  wddlwddlwdl',
    ' wddlwddlwddl',
    'wddlwddlwddll',
    'lllllllllllll'
  );
var
  x, y  : Integer;
begin
  inherited;

  if not Assigned(ResizingControl) then
    Exit;

  if ((FResizingControl is TCustomForm) and
    (TCustomForm(FResizingControl).WindowState <> wsMaximized)) or
    (not (FResizingControl is TCustomForm)) then
  begin
    for y := Low(Grip) to High(Grip) do
      for x := 1 to Length(Grip[y]) do
        case Grip[y][x] of
          ' ':
            ;

          'w':
            Canvas.Pixels[Width-13+x, Height-13+y] := clWhite;

          'l':
            Canvas.Pixels[Width-13+x, Height-13+y] := clBtnFace;

          'd':
            Canvas.Pixels[Width-13+x, Height-13+y] := clBtnShadow;
        end;
  end;
end;

procedure TlvkSizeGrip.PlaceOnControl(const Control: TWinControl);
begin
  Parent := Control;
  Align := alNone;
  SetBounds(
    Control.ClientWidth - SIZEGRIP_SIZE,
    Control.ClientHeight - SIZEGRIP_SIZE,
    SIZEGRIP_SIZE, SIZEGRIP_SIZE);
end;

procedure TlvkSizeGrip.SetParent(AParent: TWinControl);
begin
  inherited;

  FResizingControl := nil;
end;

procedure TlvkSizeGrip.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

end.
 