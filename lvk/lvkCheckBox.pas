{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkCheckBox component.
}
unit lvkCheckBox;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkCheckBox.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Windows, Controls, StdCtrls, Messages, lvkVersion;

type
  TlvkCustomCheckBox = class(TCustomCheckBox)
  protected
    function CanAutoSize(var NewWidth: Integer;
      var NewHeight: Integer): Boolean; override;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure Loaded; override;
    {$IFDEF DELPHI6UP}
    procedure SetAutoSize(Value: Boolean); override;
    {$ELSE}
    function GetAutoSize: Boolean;
    procedure SetAutoSize(Value: Boolean);
    property AutoSize: Boolean read GetAutoSize write SetAutoSize;
    {$ENDIF}

    function BoxSize: TPoint;
    procedure SetParent(AParent: TWinControl); override;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    procedure SetPackageVersion(const NewValue: TPackageVersion);
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component works exactly like TCheckBox, part of the Delphi VCL,
      except that it publishes the AutoSize property just like a TLabel
      does. Change the size of the caption property and the check box control
      will resize itself.
    Parameters:
      -
    See also:
      -
  }
  TlvkCheckBox = class(TlvkCustomCheckBox)
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;

    property Align;    
    property AutoSize;
    property Action;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TlvkCustomCheckBox }

function TlvkCustomCheckBox.BoxSize: TPoint;
begin
  Result.X := GetSystemMetrics(SM_CXMENUCHECK);
  Result.Y := GetSystemMetrics(SM_CYMENUCHECK);
end;

function TlvkCustomCheckBox.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
var
  OldFont : HFont;
  r       : TRect;
  dc      : HDC;
  h       : HWnd;
  Box     : TPoint;
begin
  if Assigned(Parent) and AutoSize then
  begin
    Box := BoxSize;
    r := ClientRect;
    r.Right := 0;
    h := Handle;
    dc := GetDeviceContext(h);
    try
      OldFont := SelectObject(dc, Font.Handle);
      try
        DrawText(dc, PChar(Caption), Length(Caption), r, DT_NOCLIP or
          DT_EXPANDTABS or DT_CALCRECT);
      finally
        SelectObject(dc, OldFont);
      end;
    finally
      ReleaseDC(h, dc);
    end;

    NewWidth := r.Right + Box.X + 6;

    if r.Bottom > NewHeight then
      NewHeight := r.Bottom;
      
    Result := True;
  end else
    Result := False;
end;

procedure TlvkCustomCheckBox.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  AdjustSize;
end;


constructor TlvkCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited;

  if csLoading in ComponentState then
    AutoSize := False;
end;

{$IFNDEF DELPHI6UP}
function TlvkCustomCheckBox.GetAutoSize: Boolean;
begin
  Result := inherited AutoSize;
end;
{$ENDIF}

function TlvkCustomCheckBox.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomCheckBox.Loaded;
begin
  inherited;
  AutoSize := True;
end;

procedure TlvkCustomCheckBox.SetAutoSize(Value: Boolean);
begin
  {$IFDEF DELPHI6UP}
  inherited;
  {$ELSE}
  inherited AutoSize := Value;
  {$ENDIF}
  AdjustSize;
end;

procedure TlvkCustomCheckBox.SetPackageVersion(
  const NewValue: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomCheckBox.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Parent) then
    AdjustSize;
end;

end.
