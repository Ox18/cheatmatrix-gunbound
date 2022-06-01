{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the various status bar components and panels.
}
unit lvkStatusBar;

// $Author: Lasse V. Karlsen $
// $Revision: 16 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkStatusBar.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  lvkVersion, lvkEdits, lvkSizeGrip, lvkComboBox, AppEvnts;

const
  iCapsLock         = 0;
  iScrollLock       = 1;
  iNumLock          = 2;

  iClickEvent       = 0;
  iDoubleClickEvent = 1;

  DEFAULT_STATUSBAR_HEIGHT  = 21;
  DEFAULT_SIZEGRIP          = False;
  DEFAULT_ENABLED           = True;

type
  { Description:
      This is the custom version of the status bar component.

    See also:
      TlvkStatusBar
  }
  TlvkCustomStatusBar = class(TCustomPanel)
  private
    procedure SetPackageVersion(const Value: TPackageVersion);

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This is the main status bar component. Typically you will drop one of
      these onto the form and let it align to the bottom of it. Then you will
      drop one of the other status panel components onto this status bar
      and configure them.

    See also:
      TlvkCustomStatusBar
  }
  TlvkStatusBar = class(TlvkCustomStatusBar)
  published
    property Anchors;
    property Align default alBottom;
    property Height default DEFAULT_STATUSBAR_HEIGHT;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  { Description:
      This is the custom, base version, of the status bar panel components.
      All other status bar panels should inherit from this component.
  }
  TlvkCustomStatusBarPanel = class(TCustomPanel)
  private
    FClientPanel    : TPanel;
    FSizeGrip       : TGraphicControl;

    FOnClick        : TNotifyEvent;
    FOnDblClick     : TNotifyEvent;

    function GetEvent(const Index: Integer): TNotifyEvent;
    procedure SetEvent(const Index: Integer; const Value: TNotifyEvent);

    function GetSizeGrip: Boolean;
    procedure SetSizeGrip(const Value: Boolean);
    procedure AddSizeGrip;
    procedure RemoveSizeGrip;
    procedure SetPackageVersion(const Value: TPackageVersion);

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

    procedure Paint; override;
    procedure RequestAlign; override;

    { Description:
        Descendant classes must place all their components inside the
        ClientPanel panel. The base component will then make sure there is
        enough spacing between panels by adjusting the size and position of
        these client panels.
    }
    property ClientPanel: TPanel read FClientPanel;
    procedure SetParent(AParent: TWinControl); override;
    procedure AdjustSizeOfClientPanel; virtual;
    procedure Resize; override;

    { Description:
        If the sub-components that the descendant panels add needs to handle
        the events of the surrounding panel (like OnClick), use the SetEvents
        method to copy the event handlers from the panel to the embedded
        component.
    }
    procedure SetEvents; virtual;

    property OnClick: TNotifyEvent index iClickEvent read GetEvent
      write SetEvent;
    property OnDblClick: TNotifyEvent index iDoubleClickEvent read GetEvent
      write SetEvent;

    { Description:
        Set SizeGrip to True to display a size grip at the right end of the
        panel. This grip can be used to resize the underlying form.
    }
    property SizeGrip: Boolean read GetSizeGrip write SetSizeGrip
      default DEFAULT_SIZEGRIP;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TlvkCustomStatusBarPanelClass = class of TlvkCustomStatusBarPanel;

  { Description:
      This is the basic custom class for all label-based status bar panels.
  }
  TlvkCustomLabelStatusBarPanel = class(TlvkCustomStatusBarPanel)
  private
    FLabel  : TLabel;

    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    function GetFont: TFont;
    procedure SetFont(const Font: TFont);
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);

  protected
    procedure SetEvents; override;
    procedure AdjustSizeOfClientPanel; override;

    property Caption: TCaption read GetCaption write SetCaption;
    property Font: TFont read GetFont write SetFont;
    property Alignment: TAlignment read GetAlignment write SetAlignment;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This is a simple label-based status bar panel. It allows you to display
      a label in the status bar.

      The Caption, Font and Alignment properties match the properties on the
      TLabel component.
  }
  TlvkLabelStatusBarPanel = class(TlvkCustomLabelStatusBarPanel)
  published
    property Anchors default [akLeft, akTop];
    property Align default alLeft;
    property ShowHint default False;
    property ParentShowHint default True;
    property Hint;
    property PopupMenu;
    property Visible;

    property Caption;
    property Font;
    property Alignment default taLeftJustify;
    // <ALIAS TlvkCustomStatusBarPanel.SizeGrip>
    property SizeGrip default False;
    property OnClick;
    property OnDblClick;
    property OnResize;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  TlvkCustomClockStatusBarPanel = class(TlvkCustomLabelStatusBarPanel)
  private
    FDisplayFormat  : string;
    FTimer          : TTimer;

    procedure SetDisplayFormat(const Value: string);
    procedure TimerTick(Sender: TObject);

  protected
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This status bar panel component implements a simple clock panel.
      The DisplayFormat property matches the format string for FormatDateTime.
  }
  TlvkClockStatusBarPanel = class(TlvkCustomClockStatusBarPanel)
  published
    property Anchors default [akLeft, akTop];
    property Align default alLeft;
    property ShowHint default False;
    property ParentShowHint default True;
    property Visible;
    property Hint;
    property PopupMenu;

    property Alignment default taCenter;
    property Font;
    // <ALIAS TlvkCustomStatusBarPanel.SizeGrip>
    property SizeGrip default False;
    property OnClick;
    property OnDblClick;
    property OnResize;

    property DisplayFormat;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  TKeyType = (ktCapsLock, ktScrollLock, ktNumLock);

  TlvkCustomKeyStatusBarPanel = class(TlvkCustomLabelStatusBarPanel)
  private
    FKey                : TKeyType;
    FKeyNames           : array[TKeyType] of string;
    FApplicationEvents  : TApplicationEvents;

    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure SetKey(const Value: TKeyType);
    function GetKeyNames(const Index: Integer): string;
    procedure SetKeyNames(const Index: Integer; const Value: string);

  protected
    property Key: TKeyType read FKey write SetKey;
    property ScrollLock: string index iScrollLock read GetKeyNames write SetKeyNames;
    property CapsLock: string index iCapsLock read GetKeyNames write SetKeyNames;
    property NumLock: string index iNumLock read GetKeyNames write SetKeyNames;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This component allows you to show the status of the Caps-Lock, Num-Lock
      and Scroll-Lock toggle keys in the status bar. If the key is toggled on,
      the component will show the text in the status bar. If the key is
      toggled off, the component will show a blank panel.
  }
  TlvkKeyStatusBarPanel = class(TlvkCustomKeyStatusBarPanel)
  published
    property Anchors default [akLeft, akTop];
    property Align default alLeft;
    property ShowHint default False;
    property ParentShowHint default True;
    property Visible;
    property Hint;
    property PopupMenu;

    property Alignment default taLeftJustify;
    property Font;
    // <ALIAS TlvkCustomStatusBarPanel.SizeGrip>
    property SizeGrip default False;
    property OnClick;
    property OnDblClick;
    property OnResize;

    { Description:
        This property determines what key to show the status for.
    }
    property Key default ktCapsLock;

    { Description:
        These properties determine the actual text to show for the three
        keys.
    }
    property ScrollLock;
    // <COMBINE ScrollLock>
    property CapsLock;
    // <COMBINE ScrollLock>
    property NumLock;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  TlvkCustomProgressBarStatusPanel = class(TlvkCustomStatusBarPanel)
  private
    FProgressBar  : TProgressBar;

    function GetMax: Integer;
    function GetMin: Integer;
    function GetPosition: Integer;
    function GetSmooth: Boolean;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetSmooth(const Value: Boolean);

  protected
    procedure SetParent(AParent: TWinControl); override;

    property Min: Integer read GetMin write SetMin;
    property Max: Integer read GetMax write SetMax;
    property Position: Integer read GetPosition write SetPosition;
    property Smooth: Boolean read GetSmooth write SetSmooth;

  public
    constructor Create(AOwner: TComponent); override;

    procedure StepIt;
    procedure StepBy(const Delta: Integer);
  end;

  { Description:
      This componet allows you to display a progress bar in the status bar.
      Most of the properties have been simply copied from TProgressBar.
  }
  TlvkProgressBarStatusBarPanel = class(TlvkCustomProgressBarStatusPanel)
  published
    property Anchors default [akLeft, akTop];
    property Align default alLeft;
    property ShowHint default False;
    property ParentShowHint default True;
    property Visible;
    property Hint;
    property PopupMenu;

    property Min default 0;
    property Max default 100;
    property Position default 0;
    property Smooth default False;

    property OnResize;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

  TlvkCustomEditStatusBarPanel = class(TlvkCustomStatusBarPanel)
  private
    FEdit : TlvkEdit;

    function GetFont: TFont;
    function GetReadOnly: Boolean;
    function GetText: string;
    procedure SetFont(const Value: TFont);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetText(const Value: string);
    function GetMaxLength: Integer;
    function GetPasswordChar: Char;
    procedure SetMaxLength(const Value: Integer);
    procedure SetPasswordChar(const Value: Char);
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetEnabledColor: TColor;
    procedure SetDisabledColor(const Value: TColor);
    procedure SetEnabledColor(const Value: TColor);
    function GetDisabledColor: TColor;

  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(Value: Boolean); override;

    property Enabled: Boolean read GetEnabled write SetEnabled
      default DEFAULT_ENABLED;
    property EnabledColor: TColor read GetEnabledColor write SetEnabledColor
      default DEFAULT_ENABLED_COLOR;
    property DisabledColor: TColor read GetDisabledColor write SetDisabledColor
      default DEFAULT_DISABLED_COLOR;
    property Text: string read GetText write SetText;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Font: TFont read GetFont write SetFont;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This status bar panel allows you to display a TlvkEdit in the
      status bar. Most of the properties are simply copied from TEdit.
  }
  TlvkEditStatusBarPanel = class(TlvkCustomEditStatusBarPanel)
  published
    property Anchors default [akLeft, akTop];
    property Align default alLeft;
    property ShowHint default False;
    property ParentShowHint default True;
    property Visible;
    property Hint;
    property PopupMenu;
    property Enabled;

    property Text;
    property ReadOnly default False;
    property Font;
    property PasswordChar default #0;
    property MaxLength default 0;
    property OnChange;

    property OnClick;
    property OnDblClick;
    property OnResize;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;

    // <ALIAS TlvkEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkEdit.DisabledColor>
    property DisabledColor;
  end;

  TlvkCustomComboBoxStatusBarPanel = class(TlvkCustomStatusBarPanel)
  private
    FComboBox : TlvkComboBox;

    {$IFDEF DELPHI6UP}
    function GetAutoComplete: Boolean;
    function GetAutoDropDown: Boolean;
    {$ENDIF}
    function GetFont: TFont;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetMaxLength: Integer;
    function GetSorted: Boolean;
    function GetStyle: TComboBoxStyle;
    function GetText: string;
    {$IFDEF DELPHI6UP}
    procedure SetAutoComplete(const Value: Boolean);
    procedure SetAutoDropDown(const Value: Boolean);
    {$ENDIF}
    procedure SetFont(const Value: TFont);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetMaxLength(const Value: Integer);
    procedure SetSorted(const Value: Boolean);
    procedure SetStyle(const Value: TComboBoxStyle);
    procedure SetText(const Value: string);
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetOnDropDown: TNotifyEvent;
    procedure SetOnDropDown(const Value: TNotifyEvent);
    function GetDisabledColor: TColor;
    function GetEnabledColor: TColor;
    procedure SetDisabledColor(const Value: TColor);
    procedure SetEnabledColor(const Value: TColor);

  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(Value: Boolean); override;

    property EnabledColor: TColor read GetEnabledColor write SetEnabledColor
      default DEFAULT_ENABLED_COLOR;
    property DisabledColor: TColor read GetDisabledColor write SetDisabledColor
      default DEFAULT_DISABLED_COLOR;
    {$IFDEF DELPHI6UP}
    property AutoComplete: Boolean read GetAutoComplete write SetAutoComplete;
    property AutoDropDown: Boolean read GetAutoDropDown write SetAutoDropDown;
    {$ENDIF}
    property Enabled: Boolean read GetEnabled write SetEnabled
      default DEFAULT_ENABLED;
    property Font: TFont read GetFont write SetFont;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read GetItems write SetItems;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property Sorted: Boolean read GetSorted write SetSorted;
    property Style: TComboBoxStyle read GetStyle write SetStyle;
    property Text: string read GetText write SetText;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnDropDown: TNotifyEvent read GetOnDropDown write SetOnDropDown;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This status bar panel allows you to display a combo box in the
      status bar. Most of the properties are simply copied from TlvkComboBox.
  }
  TlvkComboBoxStatusBarPanel = class(TlvkCustomComboBoxStatusBarPanel)
  published
    property Anchors default [akLeft, akTop];
    property Align default alLeft;
    property ShowHint default False;
    property ParentShowHint default True;
    property Visible;
    property Hint;
    property PopupMenu;
    property Enabled;

    {$IFDEF DELPHI6UP}
    property AutoComplete default True;
    property AutoDropDown default False;
    {$ENDIF}
    property Font;
    property ItemIndex default -1;
    property Items;
    property MaxLength;
    property Sorted default False;
    property Style default csDropDown;
    property Text;
    property OnChange;

    property OnClick;
    property OnDblClick;
    property OnResize;
    property OnDropDown;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;

    // <ALIAS TlvkComboBox.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkComboBox.DisabledColor>
    property DisabledColor;
  end;

  TlvkCustomHintStatusBarPanel = class(TlvkCustomLabelStatusBarPanel)
  private
    FApplicationEvents  : TApplicationEvents;

    procedure DoHint(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This is a simple label-like status bar panel that will
      show the current hint at all times. This means that if
      you use this panel, you don't need to enable ShowHint
      on any components, as the hint text will be visible in
      the status bar at all times.
  }
  TlvkHintStatusBarPanel = class(TlvkCustomHintStatusBarPanel)
  published
    property Anchors default [akLeft, akTop];
    property Align default alLeft;
    property ShowHint default False;
    property ParentShowHint default True;
    property Visible;
    property Hint;
    property PopupMenu;

    property Alignment default taLeftJustify;
    property Font;
    // <ALIAS TlvkCustomStatusBarPanel.SizeGrip>
    property SizeGrip default False;
    property OnClick;
    property OnDblClick;
    property OnResize;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

implementation

uses
  Forms, Math;

type
  TSizeGrippedPanel = class(TPanel)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
      override;
  end;

{ TlvkCustomStatusBar }

constructor TlvkCustomStatusBar.Create(AOwner: TComponent);
begin
  inherited;

  Align := alBottom;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csSetCaption];

  Height := DEFAULT_STATUSBAR_HEIGHT;
end;

function TlvkCustomStatusBar.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomStatusBar.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

{ TlvkCustomStatusBarPanel }

procedure TlvkCustomStatusBarPanel.AddSizeGrip;
begin
  FSizeGrip := TlvkSizeGrip.Create(Self);
  FSizeGrip.Parent := ClientPanel;
end;

procedure TlvkCustomStatusBarPanel.AdjustSizeOfClientPanel;
var
  x1, x2  : Integer;
begin
  if Align in [alLeft, alTop, alBottom, alClient] then
    x1 := 0
  else if akLeft in Anchors then
    x1 := 0
  else
    x1 := 2;

  if Align in [alRight, alTop, alBottom, alClient] then
    x2 := ClientWidth
  else if akRight in Anchors then
    x2 := ClientWidth
  else
    x2 := ClientWidth-2;

  FClientPanel.SetBounds(x1, 0, x2-x1, ClientHeight);
end;

constructor TlvkCustomStatusBarPanel.Create(AOwner: TComponent);
begin
  inherited;

  Align := alLeft;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Width := 100;
  ControlStyle := ControlStyle - [csSetCaption];

  FClientPanel := TSizeGrippedPanel.Create(Self);
  FClientPanel.Anchors := [akLeft, akRight, akTop, akBottom];
end;

destructor TlvkCustomStatusBarPanel.Destroy;
begin
  FreeAndNil(FClientPanel);
  
  inherited;
end;

function TlvkCustomStatusBarPanel.GetEvent(
  const Index: Integer): TNotifyEvent;
begin
  case Index of
    iClickEvent:
      Result := FOnClick;

    iDoubleClickEvent:
      Result := FOnDblClick;
  end;
end;

function TlvkCustomStatusBarPanel.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkCustomStatusBarPanel.GetSizeGrip: Boolean;
begin
  Result := Assigned(FSizeGrip);
end;

procedure TlvkCustomStatusBarPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FSizeGrip) then
    inherited
  else begin
    if (X >= Width-3) and (X < Width+3) and (Y >= Height-SIZEGRIP_SIZE) and (Y < Height+3) then
      TlvkSizeGrip(FSizeGrip).BeginResize
    else if (Y >= Height-3) and (Y < Height+3) and (X >= Width-SIZEGRIP_SIZE) and (X < Width+3) then
      TlvkSizeGrip(FSizeGrip).BeginResize
    else
      inherited;
  end;
end;

procedure TlvkCustomStatusBarPanel.Paint;
begin
  AdjustSizeOfClientPanel;
  inherited;
end;

procedure TlvkCustomStatusBarPanel.RemoveSizeGrip;
begin
  FreeAndNil(FSizeGrip);
end;

procedure TlvkCustomStatusBarPanel.RequestAlign;
begin
  inherited;
  if Assigned(Parent) then
    AdjustSizeOfClientPanel;
end;

procedure TlvkCustomStatusBarPanel.Resize;
begin
  inherited;
  AdjustSizeOfClientPanel;
end;

procedure TlvkCustomStatusBarPanel.SetEvent(const Index: Integer;
  const Value: TNotifyEvent);
begin
  case Index of
    iClickEvent:
      FOnClick := Value;

    iDoubleClickEvent:
      FOnDblClick := Value;
  end;

  SetEvents;
end;

procedure TlvkCustomStatusBarPanel.SetEvents;
begin
  inherited OnClick := FOnClick;
  inherited OnDblClick := FOnDblClick;

  FClientPanel.OnClick := FOnClick;
  FClientPanel.OnDblClick := FOnDblClick;
end;

procedure TlvkCustomStatusBarPanel.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomStatusBarPanel.SetParent(AParent: TWinControl);
begin
  if Assigned(AParent) then
  begin
    inherited;
    FClientPanel.Parent := Self;
    FClientPanel.BevelInner := bvLowered;
    FClientPanel.BevelOuter := bvNone;
    AdjustSizeOfClientPanel;
  end else
    inherited;
end;

procedure TlvkCustomStatusBarPanel.SetSizeGrip(const Value: Boolean);
begin
  if Value <> GetSizeGrip then
  begin
    if Value then
      AddSizeGrip
    else
      RemoveSizeGrip;
  end;
end;

{ TlvkCustomLabelStatusBarPanel }

procedure TlvkCustomLabelStatusBarPanel.AdjustSizeOfClientPanel;
var
  MaxWidth  : Integer;
begin
  inherited;

  if Assigned(ClientPanel.Parent) then
  begin
    MaxWidth := ClientPanel.ClientWidth - FLabel.Left - 3;

    if Assigned(FSizeGrip) then
      MaxWidth := Max(0, MaxWidth - FSizeGrip.Width);
    FLabel.Width := MaxWidth;
    FLabel.Height := ClientPanel.ClientHeight - FLabel.Top - 1;
  end;
end;

constructor TlvkCustomLabelStatusBarPanel.Create(AOwner: TComponent);
begin
  inherited;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := ClientPanel;
  FLabel.Left := 2;
  FLabel.Top := 4;
  FLabel.Width := 0;
end;

function TlvkCustomLabelStatusBarPanel.GetAlignment: TAlignment;
begin
  Result := FLabel.Alignment;
end;

function TlvkCustomLabelStatusBarPanel.GetCaption: TCaption;
begin
  Result := FLabel.Caption;
end;

function TlvkCustomLabelStatusBarPanel.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

procedure TlvkCustomLabelStatusBarPanel.SetAlignment(
  const Value: TAlignment);
begin
  FLabel.Alignment := Value;
end;

procedure TlvkCustomLabelStatusBarPanel.SetCaption(const Value: TCaption);
begin
  FLabel.Caption := Value;
end;

procedure TlvkCustomLabelStatusBarPanel.SetEvents;
begin
  inherited;

  FLabel.OnClick := OnClick;
  FLabel.OnDblClick := OnDblClick;
end;

procedure TlvkCustomLabelStatusBarPanel.SetFont(const Font: TFont);
begin
  FLabel.Font := Font;
end;

{ TlvkCustomClockStatusBarPanel }

constructor TlvkCustomClockStatusBarPanel.Create(AOwner: TComponent);
begin
  inherited;

  Width := 62;
  Alignment := taCenter;

  FDisplayFormat := 'hh:nn.ss';
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 250;
  FTimer.OnTimer := TimerTick;
end;

destructor TlvkCustomClockStatusBarPanel.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TlvkCustomClockStatusBarPanel.SetDisplayFormat(
  const Value: string);
begin
  if FDisplayFormat <> '' then
  begin
    FDisplayFormat := Value;
    if Assigned(Parent) then
      TimerTick(Self);
  end;
end;

procedure TlvkCustomClockStatusBarPanel.TimerTick(Sender: TObject);
begin
  if csDesigning in ComponentState then
    inherited Caption := FDisplayFormat
  else
    inherited Caption := FormatDateTime(FDisplayFormat, Now);
end;

{ TlvkCustomProgressBarStatusPanel }

constructor TlvkCustomProgressBarStatusPanel.Create(AOwner: TComponent);
begin
  inherited;

  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Parent := ClientPanel;
  FProgressBar.Align := alClient;
  FProgressBar.BorderWidth := 0;
end;

function TlvkCustomProgressBarStatusPanel.GetMax: Integer;
begin
  Result := FProgressBar.Max;
end;

function TlvkCustomProgressBarStatusPanel.GetMin: Integer;
begin
  Result := FProgressBar.Min;
end;

function TlvkCustomProgressBarStatusPanel.GetPosition: Integer;
begin
  Result := FProgressBar.Position;
end;

function TlvkCustomProgressBarStatusPanel.GetSmooth: Boolean;
begin
  Result := FProgressBar.Smooth;
end;

procedure TlvkCustomProgressBarStatusPanel.SetMax(const Value: Integer);
begin
  FProgressBar.Max := Value;
end;

procedure TlvkCustomProgressBarStatusPanel.SetMin(const Value: Integer);
begin
  FProgressBar.Min := Value;
end;

procedure TlvkCustomProgressBarStatusPanel.SetParent(AParent: TWinControl);
begin
  inherited;

  if not (csDestroying in ComponentState) then
  begin
    ClientPanel.BevelInner := bvNone;
    ClientPanel.BevelOuter := bvNone;
  end;
end;

procedure TlvkCustomProgressBarStatusPanel.SetPosition(
  const Value: Integer);
begin
  FProgressBar.Position := Value;
end;

procedure TlvkCustomProgressBarStatusPanel.SetSmooth(const Value: Boolean);
begin
  FProgressBar.Smooth := Value;
end;

procedure TlvkCustomProgressBarStatusPanel.StepBy(const Delta: Integer);
begin
  FProgressBar.StepBy(Delta);
end;

procedure TlvkCustomProgressBarStatusPanel.StepIt;
begin
  FProgressBar.StepIt;
end;

{ TlvkCustomKeyStatusBarPanel }

constructor TlvkCustomKeyStatusBarPanel.Create(AOwner: TComponent);
begin
  inherited;

  Width := 40;
  FKeyNames[ktCapsLock] := 'CAPS';
  FKeyNames[ktNumLock] := 'NUM';
  FKeyNames[ktScrollLock] := 'SCRL';

  if not (csDesigning in ComponentState) then
  begin
    FApplicationEvents := TApplicationEvents.Create(Self);
    FApplicationEvents.OnIdle := DoIdle;
  end;
end;

destructor TlvkCustomKeyStatusBarPanel.Destroy;
begin
  FApplicationEvents.Free;
  
  inherited;
end;

procedure TlvkCustomKeyStatusBarPanel.DoIdle(Sender: TObject;
  var Done: Boolean);
const
  KeyCodes  : array[TKeyType] of Integer = (VK_CAPITAL, VK_SCROLL, VK_NUMLOCK);
begin
  if csDesigning in ComponentState then
    inherited Caption := FKeyNames[FKey]
  else if (GetKeyState(KeyCodes[FKey]) and 1) <> 0 then
    inherited Caption := FKeyNames[FKey]
  else
    inherited Caption := '';
end;

function TlvkCustomKeyStatusBarPanel.GetKeyNames(
  const Index: Integer): string;
begin
  Result := FKeyNames[TKeyType(Index)];
end;

procedure TlvkCustomKeyStatusBarPanel.SetKey(const Value: TKeyType);
var
  Done  : Boolean;
begin
  if Value <> FKey then
  begin
    FKey := Value;
    DoIdle(Self, Done);
  end;
end;

procedure TlvkCustomKeyStatusBarPanel.SetKeyNames(const Index: Integer;
  const Value: string);
begin
  FKeyNames[TKeyType(Index)] := Value;
end;

{ TlvkCustomEditStatusBarPanel }

constructor TlvkCustomEditStatusBarPanel.Create(AOwner: TComponent);
begin
  inherited;

  ClientPanel.BevelInner := bvNone;
  ClientPanel.BevelOuter := bvNone;

  FEdit := TlvkEdit.Create(Self);
  FEdit.Parent := ClientPanel;
  FEdit.Align := alClient;
  FEdit.Font.Name := 'Arial';
  FEdit.Font.Size := 8;
end;

function TlvkCustomEditStatusBarPanel.GetDisabledColor: TColor;
begin
  Result := FEdit.DisabledColor;
end;

function TlvkCustomEditStatusBarPanel.GetEnabled: Boolean;
begin
  Result := FEdit.Enabled;
end;

function TlvkCustomEditStatusBarPanel.GetEnabledColor: TColor;
begin
  Result := FEdit.EnabledColor;
end;

function TlvkCustomEditStatusBarPanel.GetFont: TFont;
begin
  Result := FEdit.Font;
end;

function TlvkCustomEditStatusBarPanel.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

function TlvkCustomEditStatusBarPanel.GetOnChange: TNotifyEvent;
begin
  Result := FEdit.OnChange;
end;

function TlvkCustomEditStatusBarPanel.GetPasswordChar: Char;
begin
  Result := FEdit.PasswordChar;
end;

function TlvkCustomEditStatusBarPanel.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

function TlvkCustomEditStatusBarPanel.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TlvkCustomEditStatusBarPanel.SetDisabledColor(
  const Value: TColor);
begin
  FEdit.DisabledColor := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetEnabled(Value: Boolean);
begin
  FEdit.Enabled := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetEnabledColor(
  const Value: TColor);
begin
  FEdit.EnabledColor := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetFont(const Value: TFont);
begin
  FEdit.Font := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetMaxLength(const Value: Integer);
begin
  FEdit.MaxLength := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetOnChange(
  const Value: TNotifyEvent);
begin
  FEdit.OnChange := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetPasswordChar(const Value: Char);
begin
  FEdit.PasswordChar := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetReadOnly(const Value: Boolean);
begin
  FEdit.ReadOnly := Value;
end;

procedure TlvkCustomEditStatusBarPanel.SetText(const Value: string);
begin
  FEdit.Text := Value;
end;

{ TlvkCustomComboBoxStatusBarPanel }

constructor TlvkCustomComboBoxStatusBarPanel.Create(AOwner: TComponent);
begin
  inherited;

  ClientPanel.BevelInner := bvNone;
  ClientPanel.BevelOuter := bvNone;
  Width := 121;
  
  FComboBox := TlvkComboBox.Create(Self);
  FComboBox.Align := alClient;
  FComboBox.Parent := ClientPanel;
  FComboBox.Font.Name := 'Arial';
  FComboBox.Font.Size := 8;
end;

{$IFDEF DELPHI6UP}
function TlvkCustomComboBoxStatusBarPanel.GetAutoComplete: Boolean;
begin
  Result := FComboBox.AutoComplete;
end;

function TlvkCustomComboBoxStatusBarPanel.GetAutoDropDown: Boolean;
begin
  Result := FComboBox.AutoDropDown;
end;
{$ENDIF}

function TlvkCustomComboBoxStatusBarPanel.GetDisabledColor: TColor;
begin
  Result := FComboBox.DisabledColor;
end;

function TlvkCustomComboBoxStatusBarPanel.GetEnabled: Boolean;
begin
  Result := FComboBox.Enabled;
end;

function TlvkCustomComboBoxStatusBarPanel.GetEnabledColor: TColor;
begin
  Result := FComboBox.EnabledColor;
end;

function TlvkCustomComboBoxStatusBarPanel.GetFont: TFont;
begin
  Result := FComboBox.Font;
end;

function TlvkCustomComboBoxStatusBarPanel.GetItemIndex: Integer;
begin
  Result := FComboBox.ItemIndex;
end;

function TlvkCustomComboBoxStatusBarPanel.GetItems: TStrings;
begin
  Result := FComboBox.Items;
end;

function TlvkCustomComboBoxStatusBarPanel.GetMaxLength: Integer;
begin
  Result := FComboBox.MaxLength;
end;

function TlvkCustomComboBoxStatusBarPanel.GetOnChange: TNotifyEvent;
begin
  Result := FComboBox.OnChange;
end;

function TlvkCustomComboBoxStatusBarPanel.GetOnDropDown: TNotifyEvent;
begin
  Result := FComboBox.OnDropDown;
end;

function TlvkCustomComboBoxStatusBarPanel.GetSorted: Boolean;
begin
  Result := FComboBox.Sorted;
end;

function TlvkCustomComboBoxStatusBarPanel.GetStyle: TComboBoxStyle;
begin
  Result := FComboBox.Style;
end;

function TlvkCustomComboBoxStatusBarPanel.GetText: string;
begin
  Result := FComboBox.Text;
end;

{$IFDEF DELPHI6UP}
procedure TlvkCustomComboBoxStatusBarPanel.SetAutoComplete(
  const Value: Boolean);
begin
  FComboBox.AutoComplete := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetAutoDropDown(
  const Value: Boolean);
begin
  FComboBox.AutoDropDown := Value;
end;
{$ENDIF}

procedure TlvkCustomComboBoxStatusBarPanel.SetDisabledColor(
  const Value: TColor);
begin
  FComboBox.DisabledColor := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetEnabled(Value: Boolean);
begin
  FComboBox.Enabled := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetEnabledColor(
  const Value: TColor);
begin
  FComboBox.EnabledColor := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetFont(const Value: TFont);
begin
  FComboBox.Font := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetItemIndex(
  const Value: Integer);
begin
  FComboBox.ItemIndex := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetItems(const Value: TStrings);
begin
  FComboBox.Items := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetMaxLength(
  const Value: Integer);
begin
  FComboBox.MaxLength := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetOnChange(
  const Value: TNotifyEvent);
begin
  FComboBox.OnChange := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetOnDropDown(
  const Value: TNotifyEvent);
begin
  FComboBox.OnDropDown := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetSorted(const Value: Boolean);
begin
  FComboBox.Sorted := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetStyle(
  const Value: TComboBoxStyle);
begin
  FComboBox.Style := Value;
end;

procedure TlvkCustomComboBoxStatusBarPanel.SetText(const Value: string);
begin
  FComboBox.Text := Value;
end;

{ TlvkCustomHintStatusBarPanel }

constructor TlvkCustomHintStatusBarPanel.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FApplicationEvents := TApplicationEvents.Create(Self);
    FApplicationEvents.OnHint := DoHint;
  end;
end;

destructor TlvkCustomHintStatusBarPanel.Destroy;
begin
  FApplicationEvents.Free;

  inherited;
end;

procedure TlvkCustomHintStatusBarPanel.DoHint(Sender: TObject);
begin
  inherited Caption := Application.Hint;
end;

{ TSizeGrippedPanel }

procedure TSizeGrippedPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(TlvkCustomStatusBarPanel(Owner).FSizeGrip) then
    inherited
  else begin
    if (X >= Width-3) and (X < Width+3) and (Y >= Height-SIZEGRIP_SIZE) and (Y < Height+3) then
      TlvkSizeGrip(TlvkCustomStatusBarPanel(Owner).FSizeGrip).BeginResize
    else if (Y >= Height-3) and (Y < Height+3) and (X >= Width-SIZEGRIP_SIZE) and (X < Width+3) then
      TlvkSizeGrip(TlvkCustomStatusBarPanel(Owner).FSizeGrip).BeginResize
    else
      inherited;
  end;
end;

procedure TSizeGrippedPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if not Assigned(TlvkCustomStatusBarPanel(Owner).FSizeGrip) then
    Cursor := crDefault
  else begin
    if (X >= Width-3) and (X < Width+3) and (Y >= Height-SIZEGRIP_SIZE) and (Y < Height+3) then
      Cursor := crSizeNWSE
    else if (Y >= Height-3) and (Y < Height+3) and (X >= Width-SIZEGRIP_SIZE) and (X < Width+3) then
      Cursor := crSizeNWSE
    else
      Cursor := crDefault
  end;
end;

end.
