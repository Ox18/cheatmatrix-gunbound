{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the customized groupbox components.
}
unit lvkGroupBox;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkGroupBox.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Types,
  {$ENDIF}
  Windows, Classes, lvkVersion, StdCtrls, Controls, Graphics, Menus;

const
  DEFAULT_WIDTH     = 185;
  DEFAULT_HEIGHT    = 105;

type
  TlvkCustomGroupBox = class(TCustomGroupBox)
  private
    function GetPackageVersion: TPackageVersion;
    procedure SetPackageVersion(const Value: TPackageVersion);
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
  end;

  TDisabledControl = record
    Control : TControl;
    Enabled : Boolean;
  end;

  TlvkCustomExpandableGroupBox = class(TlvkCustomGroupBox)
  private
    FExpanded         : Boolean;
    FExpanding        : Boolean;
    FHeight           : Integer;
    FSetExpanded      : Boolean;
    FDisabledControls : array of TDisabledControl;

  protected
    procedure SetExpanded(const Value: Boolean); virtual;
    procedure DoExpand; virtual;
    procedure DoContract; virtual;
    procedure SetHeight(const Value: Integer); virtual;
    procedure DisableControls; virtual;
    procedure EnableControls; virtual;
    function CanDisableControl(const Control: TControl): Boolean; virtual;

    function DefaultHeight: Integer; virtual;
    procedure Paint; override;
    procedure Loaded; override;

    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property Height: Integer read FHeight write SetHeight default DEFAULT_HEIGHT;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function CanFocus: Boolean; override;
  end;

  TlvkExpandableGroupBox = class(TlvkCustomExpandableGroupBox)
  private
  published
    // From TlvkCustomExpandableGroupBox
    property Expanded;

    // From TCustomGroupBox
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
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
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TlvkExpandableCheckBox = class(TPersistent)
  private
    FCheckBox : TCheckBox;
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    function GetColor: TColor;
    function GetFont: TFont;
    function GetParentColor: Boolean;
    function GetParentFont: Boolean;
    function GetParentShowHint: Boolean;
    function GetPopupMenu: TPopupMenu;
    function GetShowHint: Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetParentColor(const Value: Boolean);
    procedure SetParentFont(const Value: Boolean);
    procedure SetParentShowHint(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetShowHint(const Value: Boolean);

  protected
    function BoxSize: TPoint; virtual;
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(const CheckBox: TCheckBox);
    procedure AdjustBounds; virtual;

  published
    property Caption: TCaption read GetCaption write SetCaption;
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor;
    property ShowHint: Boolean read GetShowHint write SetShowHint default False;
    property ParentColor: Boolean read GetParentColor write SetParentColor default True;
    property ParentFont: Boolean read GetParentFont write SetParentFont default True;
    property ParentShowHint: Boolean read GetParentShowHint write SetParentShowHint default True;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
  end;

  TlvkCustomCheckBoxExpandableGroupBox = class(TlvkCustomExpandableGroupBox)
  private
    FCheckBox           : TCheckBox;
    FExpandableCheckBox : TlvkExpandableCheckBox;

    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
    procedure CheckBoxClick(Sender: TObject);
    procedure SetExpandableCheckBox(const Value: TlvkExpandableCheckBox);
    procedure DrawBlankArea;
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);

  protected
    procedure SetExpanded(const Value: Boolean); override;
    procedure Paint; override;
    function CanDisableControl(const Control: TControl): Boolean; override;
    function DefaultHeight: Integer; override;
    property Checked: Boolean read GetChecked write SetChecked default True;
    procedure SetParent(AParent: TWinControl); override;
    property CheckBox: TlvkExpandableCheckBox read FExpandableCheckBox
      write SetExpandableCheckBox;
    procedure SetEnabled(Value: Boolean); override;
    property Caption: TCaption read GetCaption write SetCaption stored False;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TlvkCheckBoxExpandableGroupBox = class(TlvkCustomCheckBoxExpandableGroupBox)
  published
    // From TlvkCustomExpandableGroupBox
    property Expanded;
    property Checked;
    property CheckBox;

    // From TCustomGroupBox
    property Caption;
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
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
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  Math, lvkScope;

{ TlvkCustomGroupBox }

function TlvkCustomGroupBox.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomGroupBox.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

{ TlvkCustomExpandableGroupBox }

function TlvkCustomExpandableGroupBox.CanDisableControl(
  const Control: TControl): Boolean;
begin
  Result := True;
end;

function TlvkCustomExpandableGroupBox.CanFocus: Boolean;
begin
  if FExpanded then
    Result := inherited CanFocus
  else
    Result := False;
end;

constructor TlvkCustomExpandableGroupBox.Create(AOwner: TComponent);
begin
  inherited;

  FExpanded := True;
  FSetExpanded := True;
  Width := DEFAULT_WIDTH;
  Height := DEFAULT_HEIGHT;
end;

function TlvkCustomExpandableGroupBox.DefaultHeight: Integer;
begin
  if HandleAllocated and not IsIconic(inherited Handle) then
  begin
    Canvas.Font := Font;
    Result := Canvas.TextHeight('0');
  end else
    Result := 13;
end;

procedure TlvkCustomExpandableGroupBox.DisableControls;
var
  Index   : Integer;
  Index2  : Integer;
  Control : TControl;
begin
  SetLength(FDisabledControls, ControlCount);
  Index2 := 0;
  for Index := 0 to ControlCount-1 do
  begin
    Control := Controls[Index];

    if CanDisableControl(Control) then
    begin
      FDisabledControls[Index2].Control := Control;
      FDisabledControls[Index2].Enabled := Control.Enabled;
      Control.Enabled := False;
      Inc(Index2);
    end;
  end;

  SetLength(FDisabledControls, Index2);
end;

procedure TlvkCustomExpandableGroupBox.DoContract;
begin
  Scope.BooleanFlag(FExpanding, True);
  inherited Height := DefaultHeight;
  DisableControls;
end;

procedure TlvkCustomExpandableGroupBox.DoExpand;
begin
  Scope.BooleanFlag(FExpanding, True);
  EnableControls;
  inherited Height := FHeight;
end;

procedure TlvkCustomExpandableGroupBox.EnableControls;
var
  Index : Integer;
begin
  for Index := 0 to Length(FDisabledControls)-1 do
    FDisabledControls[Index].Control.Enabled := FDisabledControls[Index].Enabled;
  SetLength(FDisabledControls, 0);
end;

procedure TlvkCustomExpandableGroupBox.Loaded;
begin
  inherited;

  Expanded := FSetExpanded;
end;

procedure TlvkCustomExpandableGroupBox.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index : Integer;
begin
  inherited;

  if Operation = opRemove then
  begin
    for Index := 0 to Length(FDisabledControls)-1 do
      if FDisabledControls[Index].Control = AComponent then
      begin
        if Index < Length(FDisabledControls)-1 then
          FDisabledControls[Index] := FDisabledControls[Length(FDisabledControls)-1];
        SetLength(FDisabledControls, Length(FDisabledControls)-1);
        Break;
      end;
  end;
end;

procedure TlvkCustomExpandableGroupBox.Paint;
var
  H     : Integer;
  R     : TRect;
  Flags : Longint;

  procedure DrawFrame;
  begin
    if FExpanded or (csDesigning in ComponentState) then
      Canvas.FrameRect(R)
    else begin
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.MoveTo(R.Left+1, R.Top);
      Canvas.LineTo(R.Right-1, R.Top);
    end;
  end;

begin
  with Canvas do
  begin
    Font := Self.Font;
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width, Height);

    if Ctl3D then
    begin
      Inc(R.Left);
      Inc(R.Top);
      Brush.Color := clBtnHighlight;
      DrawFrame;
      OffsetRect(R, -1, -1);
      Brush.Color := clBtnShadow;
    end else
      Brush.Color := clWindowFrame;
    DrawFrame;

    if Text <> '' then
    begin
      if not UseRightToLeftAlignment then
        R := Rect(8, 0, 0, H)
      else
        R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
      DrawText(Handle, PChar(Text), Length(Text), R, Flags or DT_CALCRECT);
      Brush.Color := Color;
      DrawText(Handle, PChar(Text), Length(Text), R, Flags);
    end;
  end;
end;

procedure TlvkCustomExpandableGroupBox.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  if FExpanding then
    inherited SetBounds(ALeft, ATop, AWidth, AHeight)
  else begin
    if Expanded or (csDesigning in ComponentState) then
    begin
      FHeight := AHeight;
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    end else begin
      if AHeight <> DefaultHeight then
        FHeight := AHeight;
      inherited SetBounds(ALeft, ATop, AWidth, DefaultHeight);
    end;
  end;
end;

procedure TlvkCustomExpandableGroupBox.SetExpanded(const Value: Boolean);
begin
  if csLoading in ComponentState then
    FSetExpanded := Value
  else begin
    if Value <> FExpanded then
    begin
      if not (csDesigning in ComponentState) then
      begin
        if Value then
          DoExpand
        else
          DoContract;
      end;

      FExpanded := Value;
    end;
  end;
end;

procedure TlvkCustomExpandableGroupBox.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    if FHeight <> DefaultHeight then
      FHeight := Value;

    if Expanded or (csDesigning in ComponentState) then
      inherited Height := Value;
  end;
end;

{ TlvkCustomCheckBoxExpandableGroupBox }

function TlvkCustomCheckBoxExpandableGroupBox.CanDisableControl(
  const Control: TControl): Boolean;
begin
  Result := Control <> FCheckBox;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.CheckBoxClick(
  Sender: TObject);
begin
  Expanded := FCheckBox.Checked;
end;

constructor TlvkCustomCheckBoxExpandableGroupBox.Create(
  AOwner: TComponent);
begin
  inherited;

  FCheckBox := TCheckBox.Create(Self);
  FCheckBox.Left := 12;
  FCheckBox.Top := 0;
  FCheckBox.OnClick := CheckBoxClick;
  FCheckBox.Checked := True;
  FCheckBox.Parent := Self;
  Expanded := True;

  ControlStyle := ControlStyle - [csSetCaption];
  FCheckBox.Caption := 'Expandable Groupbox';
  FExpandableCheckBox := TlvkExpandableCheckBox.Create(FCheckBox);

  inherited Caption := '';
end;

function TlvkCustomCheckBoxExpandableGroupBox.DefaultHeight: Integer;
begin
  if Assigned(FCheckBox) then
    Result := FCheckBox.Height
  else
    Result := inherited DefaultHeight;
end;

destructor TlvkCustomCheckBoxExpandableGroupBox.Destroy;
begin
  FExpandableCheckBox.Free;

  inherited;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.DrawBlankArea;
begin
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Rectangle(FCheckBox.Left - 2, 0, FCheckBox.Left + FCheckBox.Width + 2,
    FCheckBox.Height);
end;

function TlvkCustomCheckBoxExpandableGroupBox.GetCaption: TCaption;
begin
  Result := FCheckBox.Caption;
end;

function TlvkCustomCheckBoxExpandableGroupBox.GetChecked: Boolean;
begin
  Result := FCheckBox.Checked;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.Paint;
begin
  inherited;
  DrawBlankArea;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.SetCaption(
  const Value: TCaption);
begin
  FCheckBox.Caption := Value;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.SetChecked(
  const Value: Boolean);
begin
  FCheckBox.Checked := Value;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.SetEnabled(Value: Boolean);
begin
  inherited;
  FCheckBox.Enabled := Value;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.SetExpandableCheckBox(
  const Value: TlvkExpandableCheckBox);
begin
  FExpandableCheckBox.Assign(Value);
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.SetExpanded(
  const Value: Boolean);
begin
  inherited;
  FCheckBox.Checked := Value;
end;

procedure TlvkCustomCheckBoxExpandableGroupBox.SetParent(
  AParent: TWinControl);
begin
  inherited;

  if Assigned(AParent) and Assigned(FExpandableCheckBox) then
    FExpandableCheckBox.AdjustBounds;
end;

{ TlvkExpandableCheckBox }

procedure TlvkExpandableCheckBox.AdjustBounds;
var
  b   : Graphics.TBitmap;
  Box : TPoint;
begin
  b := Graphics.TBitmap.Create;
  Scope.DestroyObject(b);

  b.PixelFormat := pf24bit;
  b.Width := 1;
  b.Height := 1;
  b.Canvas.Font := FCheckBox.Font;

  Box := BoxSize;
  FCheckBox.Width := b.Canvas.TextWidth(FCheckBox.Caption) + 6 + Box.X;
  FCheckBox.Height := Max(b.Canvas.TextHeight(FCheckBox.Caption), Box.Y);
end;

procedure TlvkExpandableCheckBox.AssignTo(Dest: TPersistent);
begin
  if Dest is TlvkExpandableCheckBox then
  begin
    TlvkExpandableCheckBox(Dest).Caption := Caption;
  end else
    inherited;
end;

function TlvkExpandableCheckBox.BoxSize: TPoint;
begin
  Result.X := GetSystemMetrics(SM_CXMENUCHECK);
  Result.Y := GetSystemMetrics(SM_CYMENUCHECK);
end;

constructor TlvkExpandableCheckBox.Create(const CheckBox: TCheckBox);
begin
  inherited Create;

  FCheckBox := CheckBox;
end;

function TlvkExpandableCheckBox.GetCaption: TCaption;
begin
  Result := FCheckBox.Caption;
end;

function TlvkExpandableCheckBox.GetColor: TColor;
begin
  Result := FCheckBox.Color;
end;

function TlvkExpandableCheckBox.GetFont: TFont;
begin
  Result := FCheckBox.Font;
end;

function TlvkExpandableCheckBox.GetParentColor: Boolean;
begin
  Result := FCheckBox.ParentColor;
end;

function TlvkExpandableCheckBox.GetParentFont: Boolean;
begin
  Result := FCheckBox.ParentFont;
end;

function TlvkExpandableCheckBox.GetParentShowHint: Boolean;
begin
  Result := FCheckBox.ParentShowHint;
end;

function TlvkExpandableCheckBox.GetPopupMenu: TPopupMenu;
begin
  Result := FCheckBox.PopupMenu;
end;

function TlvkExpandableCheckBox.GetShowHint: Boolean;
begin
  Result := FCheckBox.ShowHint;
end;

procedure TlvkExpandableCheckBox.SetCaption(const Value: TCaption);
begin
  FCheckBox.Caption := Value;
end;

procedure TlvkExpandableCheckBox.SetColor(const Value: TColor);
begin
  FCheckBox.Color := Value;
end;

procedure TlvkExpandableCheckBox.SetFont(const Value: TFont);
begin
  FCheckBox.Font := Value;
end;

procedure TlvkExpandableCheckBox.SetParentColor(const Value: Boolean);
begin
  FCheckBox.ParentColor := Value;
end;

procedure TlvkExpandableCheckBox.SetParentFont(const Value: Boolean);
begin
  FCheckBox.ParentFont := Value;
end;

procedure TlvkExpandableCheckBox.SetParentShowHint(const Value: Boolean);
begin
  FCheckBox.ParentShowHint := Value;
end;

procedure TlvkExpandableCheckBox.SetPopupMenu(const Value: TPopupMenu);
begin
  FCheckBox.PopupMenu := Value;
end;

procedure TlvkExpandableCheckBox.SetShowHint(const Value: Boolean);
begin
  FCheckBox.ShowHint := Value;
end;

end.
