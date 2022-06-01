{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

unit lvk3DGradientLabel;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 8 $
// $Archive: /Components/LVK/source/lvk3DGradientLabel.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Windows, Graphics, Controls, Messages,
  lvkControls;

type
  { Description:
      Used for the HorizontalAlignment property of TlvkCustom3DGradientLabel
    See also:
      TlvkCustom3DGradientLabel.HorizontalAlignment
  }
  THorizAlignment = (haLeft, haCenter, haRight);

  { Description:
      User for the VerticalAlignment property of TlvkCustom3DGradientLabel
    See also:
      TlvkCustom3DGradientLabel.VerticalAlignment
  }
  TVertAlignment  = (vaTop, vaCenter, vaBottom);

  { Description:
      User for the GradientDirection property of TlvkCustom3DGradientLabel
    See also:
      TlvkCustom3DGradientLabel.GradientDirection
  }
  TGradientDirection = (gdRight, gdDown, gdLeft, gdUp);

  TlvkCustom3DGradientLabel = class;

  { Description:
      This class is used to provide sub-properties for the Margins property
      of TlvkCustom3DGradientLabel.
    See also:
      TlvkCustom3DGradientLabel.Margins
  }
  TMargins = class(TPersistent)
  private
    FLabel    : TlvkCustom3DGradientLabel;
    FMargins  : array[0..3] of Integer;

    function GetMargin(const Index: Integer): Integer;
    procedure SetMargin(const Index, Value: Integer);

  public
    { Description:
        Creates a new instance of the TMargins class and attaches it to the
        given instance of TlvkCustom3DGradientLabel. It needs a valid instance
        in order to inform the owning label component about changes so that it
        can redraw itself.
      See also:
        TlvkCustom3DGradientLabel
    }
    constructor Create(const ALabel: TlvkCustom3DGradientLabel);

    { Description:
        Copies the properties from a different TMargins object instance into
        this instance. Only handles TMargins.
    }
    procedure Assign(Source: TPersistent); override;

  published
    { Description:
        This property defines how many pixels margin to use on the left side
        of the gradient label. A value of 0 is the default value, meaning
        no margin.
      See also:
        Top, Bottom, Right, TlvkCustom3DGradientLabel.Margins
    }
    property Left: Integer index 0 read GetMargin write SetMargin default 0;

    { Description:
        This property defines how many pixels margin to use on the top side
        of the gradient label. A value of 0 is the default value, meaning
        no margin.
      See also:
        Left, Bottom, Right, TlvkCustom3DGradientLabel.Margins
    }
    property Top: Integer index 1 read GetMargin write SetMargin default 0;

    { Description:
        This property defines how many pixels margin to use on the bottom side
        of the gradient label. A value of 0 is the default value, meaning
        no margin.
      See also:
        Left, Top, Right, TlvkCustom3DGradientLabel.Margins
    }
    property Bottom: Integer index 2 read GetMargin write SetMargin default 0;

    { Description:
        This property defines how many pixels margin to use on the right side
        of the gradient label. A value of 0 is the default value, meaning
        no margin.
      See also:
        Left, Top, Bottom, TlvkCustom3DGradientLabel.Margins
    }
    property Right: Integer index 3 read GetMargin write SetMargin default 0;
  end;

  { Description:
      This is the custom version of the Tlvk3DGradientLabel component. It
      provides a label component which features margins, a gradient background,
      3D bevels, 3D shadow, etc.
    See also:
      Tlvk3DGradientLabel
  }
  TlvkCustom3DGradientLabel = class(TlvkCustomControl)
  private
    FColor1             : TColor;
    FColor2             : TColor;
    FHorizAlignment     : THorizAlignment;
    FVertAlignment      : TVertAlignment;
    FMargins            : TMargins;
    FAutoSize           : Boolean;
    FGradientDirection  : TGradientDirection;
    FShadowColor        : TColor;
    FOnMouseEnter       : TNotifyEvent;
    FOnMouseLeave       : TNotifyEvent;
    FShadowDX           : Integer;
    FShadowDY           : Integer;

    procedure DrawGradient(const Rect: TRect);
    procedure DrawText(const Rect: TRect);
    procedure SetColor1(const Value: TColor);
    procedure SetColor2(const Value: TColor);
    procedure SetHorizAlignment(const Value: THorizAlignment);
    procedure SetVertAlignment(const Value: TVertAlignment);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure CalculateSize;
    procedure SetGradientDirection(const Value: TGradientDirection);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowDX(const Value: Integer);
    procedure SetShadowDY(const Value: Integer);
    function GetBevelInner: TBevelCut;
    function GetBevelOuter: TBevelCut;
    procedure SetBevelInner(const Value: TBevelCut);
    procedure SetBevelOuter(const Value: TBevelCut);
    function GetBevelWidth: Integer;
    function GetBorderWidth: Integer;
    procedure SetBevelWidth(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetMargins(const Value: TMargins);
    function GetBorderColor: TColor;
    procedure SetBorderColor(const Value: TColor);

  protected
    { Description:
        Does all the painting of this specialized label component.
    }
    procedure Paint; override;

    { Description:
        This property holds the current caption of the label component.
    }
    property Caption: TCaption read GetCaption write SetCaption stored true;

    { Description:
        This property holds one of the two gradient colors of the label
        component. Specifically, the GradientDirection property uses this
        as the "from" color, ie. a left-to-right gradient will flow from this
        color on the left to Color2 on the right.
      See also:
        Color2
    }
    property Color1: TColor read FColor1 write SetColor1 default clBtnFace;

    { Description:
        This property holds the other of the two gradient colors of the label
        component. Specifically, the GradientDirection property uses this
        as the "to" color, ie. a left-to-right gradient will flow from Color1
        on the left to this color on the right.
      See also:
        Color1
    }
    property Color2: TColor read FColor2 write SetColor2 default clAppWorkspace;

    { Description:
        This property controls what kind of horizontal alignment to use for the
        text in the label. The possible values are:

          * haLeft - All the way to the left, next to any possible margins. This
            is the default value.
          * haCenter - In the center.
          * haRight - All the way to the right, next to any possible margins.
      See also:
        Margins, VerticalAlignment, THorizAlignment
    }
    property HorizontalAlignment: THorizAlignment read FHorizAlignment
      write SetHorizAlignment default haLeft;

    { Description:
        This property controls what kind of vertical alignment to use for the
        text in the label. The possible values are:

          * vaTop - All the way to the top, below any possible margins. This
            is the default value.
          * vaCenter - In the center.
          * vaBottom - All the way to the bottom, above any possible margins.
      See also:
        Margins, HorizontalAlignment, TVertAlignment
    }
    property VerticalAlignment: TVertAlignment read FVertAlignment
      write SetVertAlignment default vaTop;

    { Description:
        This property controls wether the component autosizes or not. If you
        set this property to True, it means the component will adjust its own
        size to contain just the label, margins, bevels, etc. Only the width
        and height properties are adjusted in this way. If you set this
        property to False, you must set the width and height properties
        yourself.
    }
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;

    { Description:
        This property defines which direction the gradient flows in. The
        gradient will flow from Color1 to Color2. The possible values are:

          * gdRight - Color1 will be on the left side, Color2 will be on the
            right side. This is the default value.
          * gdDown - Color1 will be on the top, Color2 will be on the bottom.
          * gdLeft - Color1 will be on the right side, Color2 will be on the
            left side.
          * gdUp - Color1 will be on the bottom, Color2 will be on the top.
      See also:
        Color1, Color2, TGradientDirection
    }
    property GradientDirection: TGradientDirection read FGradientDirection
      write SetGradientDirection default gdRight;

    { Description:
        This property controls the color of the shadow of the text. By default
        this value is set to clNone, which means no shadow will be drawn.

        The shadow is painted by default one pixel to the right and one down
        from the text.
      See also:
        ShadowDeltaX, ShadowDeltaY
    }
    property ShadowColor: TColor read FShadowColor write SetShadowColor
      default clNone;

    { Description:
        This property is one of two that controls where the shadow will be
        painted in relation to the normal caption. This property controls the
        left/right distance in pixels, and a positive value is to the right of
        the normal caption.
      See also:
        ShadowDeltaY, ShadowColor
    }
    property ShadowDeltaX: Integer read FShadowDX write SetShadowDX default 1;

    { Description:
        This property is one of two that controls where the shadow will be
        painted in relation to the normal caption. This property controls the
        up/down distance in pixels, and a positive value is downwards from the
        normal caption.
      See also:
        ShadowDeltaX, ShadowColor
    }
    property ShadowDeltaY: Integer read FShadowDY write SetShadowDY default 1;

    { Description:
        This property controls what kind of bevel is used as the inner
        bevel. There are two bevels available for the component, an outer
        bevel, and an inner bevel.
      See also:
        BevelOuter, BevelWidth,
    }
    property BevelInner: TBevelCut read GetBevelInner write SetBevelInner
      default bvNone;

    { Description:
        This property controls what kind of bevel is used as the outer
        bevel. There are two bevels available for the component, an outer
        bevel, and an inner bevel.
      See also:
        BevelInner, BevelWidth
    }
    property BevelOuter: TBevelCut read GetBevelOuter write SetBevelOuter
      default bvNone;

    { Description:
        This property controls the width of the bevels you set for this
        component.
      See also:
        BevelInner, BevelOuter
    }
    property BevelWidth: Integer read GetBevelWidth write SetBevelWidth
      default 1;

    { Description:
        This property controls the width of the border for this component.
      See also:
        BorderColor
    }
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth
      default 0;

    { Description:
        This property controls the amount of margins you want to use for
        this component. The margin is added right next to the caption and
        is inside any bevels or borders.
      See also:
        TMargins
    }
    property Margins: TMargins read FMargins write SetMargins;

    { Description:
        This property controls the color of the border of the component.
      See also:
        BorderWidth
    }
    property BorderColor: TColor read GetBorderColor write SetBorderColor
      default clBtnFace;

    procedure SetName(const NewName: TComponentName); override;

    { Description:
        This event is fired whenever the mouse pointer enters the area
        controlled by this component.
      See also:
        OnMouseLeave
    }
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;

    { Description:
        This event is fired whenever the mouose pointer leaves the area
        controlled by this component.
      See also:
        OnMouseEnter
    }
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This is the non-custom version of the 3D gradient label component.
    See also:
      TlvkCustom3DGradientLabel
  }
  Tlvk3DGradientLabel = class(TlvkCustom3DGradientLabel)
  published
    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property Font;
    property Caption;
    property BiDiMode;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnUnDock;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;

    // <ALIAS TlvkCustom3DGradientLabel.BorderColor>
    property BorderColor;
    // <ALIAS TlvkCustom3DGradientLabel.Color1>
    property Color1;
    // <ALIAS TlvkCustom3DGradientLabel.Color2>
    property Color2;
    // <ALIAS TlvkCustom3DGradientLabel.HorizontalAlignment>
    property HorizontalAlignment;
    // <ALIAS TlvkCustom3DGradientLabel.VerticalAlignment>
    property VerticalAlignment;
    // <ALIAS TlvkCustom3DGradientLabel.Margins>
    property Margins;
    // <ALIAS TlvkCustom3DGradientLabel.AutoSize>
    property AutoSize;
    // <ALIAS TlvkCustom3DGradientLabel.GradientDirection>
    property GradientDirection;
    // <ALIAS TlvkCustom3DGradientLabel.ShadowColor>
    property ShadowColor;
    // <ALIAS TlvkCustom3DGradientLabel.OnMouseEnter>
    property OnMouseEnter;
    // <ALIAS TlvkCustom3DGradientLabel.OnMouseLeave>
    property OnMouseLeave;
    // <ALIAS TlvkCustom3DGradientLabel.ShadowDeltaX>
    property ShadowDeltaX;
    // <ALIAS TlvkCustom3DGradientLabel.ShadowDeltaY>
    property ShadowDeltaY;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

implementation

// Copied from ExtCtrls.pas
procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

{ TlvkCustom3DGradientLabel }

procedure TlvkCustom3DGradientLabel.CalculateSize;
var
  w, h  : Integer;
begin
  Canvas.Font.Assign(Font);

  // Calculate width from Caption, adjust for margins, borders and bevels
  w := Canvas.TextWidth(Caption) + Margins.Left + FMargins.Right + BorderWidth*4;
  if BevelInner <> bvNone then
    Inc(w, BevelWidth*2);
  if BevelOuter <> bvNone then
    Inc(w, BevelWidth*2);

  // Calculate height from Caption, adjust for margins, borders and bevels
  h := Canvas.TextHeight(Caption) + FMargins.Top + FMargins.Bottom + BorderWidth*4;
  if BevelInner <> bvNone then
    Inc(h, BevelWidth*2);
  if BevelOuter <> bvNone then
    Inc(h, BevelWidth*2);

  // Adjust control height and width
  Width := w;
  Height := h;
end;

constructor TlvkCustom3DGradientLabel.Create(AOwner: TComponent);
begin
  inherited;

  // Set default property values
  FShadowDX := 1;
  FShadowDY := 1;
  DoubleBuffered := True;
  FShadowColor := clNone;
  Width := 96;
  Height := 13;
  FColor1 := clBtnFace;
  FColor2 := clAppWorkSpace;
  FMargins := TMargins.Create(Self);
  FHorizAlignment := haLeft;
  FVertAlignment := vaTop;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelWidth := 1;
  // Caption := '';
  FAutoSize := False;
end;

destructor TlvkCustom3DGradientLabel.Destroy;
begin
  FreeAndNil(FMargins);
  
  inherited;
end;

procedure TlvkCustom3DGradientLabel.DrawGradient(const Rect: TRect);
var
  c           : Integer;
  r1, g1, b1  : Double;
  r2, g2, b2  : Double;

  procedure HorizontalGradient(const Base, Direction: Double);
  var
    x       : Integer;
    r, g, b : Double;
    Factor  : Double;
  begin
    // This procedure will draw a horizontal gradient
    // Factory is calculated to be from 0.0 to 1.0 and is used to calculate the
    // proper color to draw with. Note that this procedure can draw a gradient
    // both from left to right and from right to left. In the latter case,
    // the factor is simply reversed to start from 1.0 (base=1.0) and
    // go towards 0.0 (direction=-1.0)
    for x := Rect.Left to (Rect.Right-1) do
    begin
      Factor := Base + Direction * ((x-Rect.Left)/(Rect.Right-Rect.Left-1));

      r := r1 + (r2-r1)*Factor;
      g := g1 + (g2-g1)*Factor;
      b := b1 + (b2-b1)*Factor;

      Canvas.Pen.Color := RGB(Trunc(r), Trunc(g), Trunc(b));
      Canvas.MoveTo(x, Rect.Top);
      Canvas.LineTo(x, Rect.Bottom);
    end;
  end;

  procedure VerticalGradient(const Base, Direction: Double);
  var
    y       : Integer;
    r, g, b : Double;
    Factor  : Double;
  begin
    // Vertical version of HorizontalGradient, otherwise exactly the same
    for y := Rect.Top to Rect.Bottom-1 do
    begin
      Factor := Base + Direction * ((y-Rect.Top)/(Rect.Bottom-Rect.Top-1));

      r := r1 + (r2-r1)*Factor;
      g := g1 + (g2-g1)*Factor;
      b := b1 + (b2-b1)*Factor;

      Canvas.Pen.Color := RGB(Trunc(r), Trunc(g), Trunc(b));
      Canvas.MoveTo(Rect.Left, y);
      Canvas.LineTo(Rect.Right, y);
    end;
  end;

begin
  if not Assigned(Parent) then
    Exit
  else if FColor1 = FColor2 then
  begin
    Canvas.Pen.Color := FColor1;
    Canvas.Brush.Color := FColor1;
    Canvas.Rectangle(Rect);
  end else if Rect.Left = Rect.Right then
    Exit
  else if Rect.Left+1 = Rect.Right then
  begin
    Canvas.MoveTo(Rect.Left, Rect.Top);
    Canvas.Pen.Color := FColor1;
    Canvas.LineTo(Rect.Left, Rect.Bottom);
  end else begin
    // Extract r, g and b values of the first color
    c := ColorToRGB(FColor1);
    r1 := (c and $0000FF);
    g1 := (c and $00FF00) shr 8;
    b1 := (c and $FF0000) shr 16;

    // Extract r, g and b values of the second color
    c := ColorToRGB(FColor2);
    r2 := (c and $0000FF);
    g2 := (c and $00FF00) shr 8;
    b2 := (c and $FF0000) shr 16;

    case GradientDirection of
      gdRight:
        HorizontalGradient(0.0, +1.0);

      gdLeft:
        HorizontalGradient(1.0, -1.0);

      gdDown:
        VerticalGradient(0.0, +1.0);

      gdUp:
        VerticalGradient(1.0, -1.0);
    end;
  end;
end;

procedure TlvkCustom3DGradientLabel.DrawText(const Rect: TRect);
var
  x, y  : Integer;
begin
  Canvas.Font.Assign(Font);
  Canvas.Brush.Style := bsClear;

  // Determine left position of text
  case FHorizAlignment of
    haLeft:
      // just get it inside the margin, from the left edge
      x := Rect.Left + FMargins.Left;

    haCenter:
      // Calculate the center horizontal position
      x := Rect.Left + FMargins.Left + (Rect.Right-Rect.Left - Canvas.TextWidth(Caption) - FMargins.Left - FMargins.Right) div 2;

    haRight:
      // just get it inside the margin, from the right edge
      x := Rect.Left + (Rect.Right-Rect.Left - Canvas.TextWidth(Caption) - FMargins.Right);

  else
    // Default, shouldn't get here, but make sure it's drawn correctly
    x := Rect.Left + FMargins.Left;
  end;

  case FVertAlignment of
    vaTop:
      // just get it inside the margin, from the top edge
      y := Rect.Top + FMargins.Top;

    vaCenter:
      // Calculate the center vertical position
      y := Rect.Top + FMargins.Top + (Rect.Bottom-Rect.Top - Canvas.TextHeight(Caption) - FMargins.Top - FMargins.Bottom) div 2;

    vaBottom:
      // just get it inside the margin, from the bottom edge
      y := Rect.Top + (Rect.Bottom-Rect.Top - Canvas.TextHeight(Caption) - FMargins.Bottom);

  else
    // Default, shouldn't get here, but make sure it's drawn correctly
    y := Rect.Top + FMargins.Top;
  end;

  // Draw shadow
  if (FShadowColor <> clNone) and ((FShadowDX <> 0) or (FShadowDY <> 0)) then
  begin
    Canvas.Font.Color := FShadowColor;
    Canvas.TextRect(Rect, x+FShadowDX, y+FShadowDY, Caption);
    Canvas.Font.Color := Font.Color;
  end;
  Canvas.TextRect(Rect, x, y, Caption);
end;

function TlvkCustom3DGradientLabel.GetBevelInner: TBevelCut;
begin
  Result := inherited BevelInner;
end;

function TlvkCustom3DGradientLabel.GetBevelOuter: TBevelCut;
begin
  Result := inherited BevelOuter;
end;

function TlvkCustom3DGradientLabel.GetBevelWidth: Integer;
begin
  Result := inherited BevelWidth;
end;

function TlvkCustom3DGradientLabel.GetBorderWidth: Integer;
begin
  Result := inherited BorderWidth;
end;

function TlvkCustom3DGradientLabel.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

function TlvkCustom3DGradientLabel.GetBorderColor: TColor;
begin
  Result := inherited Color;
end;

procedure TlvkCustom3DGradientLabel.MouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TlvkCustom3DGradientLabel.MouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TlvkCustom3DGradientLabel.Paint;
var
  Rect                  : TRect;
  TopColor, BottomColor : TColor;

  procedure AdjustColors(Bevel: TBevelCut);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  // Get control rectangle
  Rect := GetClientRect;

  // Draw and adjust for outer bevel
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;

  // Draw border
  Frame3D(Canvas, Rect, BorderColor, BorderColor, BorderWidth);

  // Draw and adjust for inner bevel
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;

  // Draw gradient area
  DrawGradient(Rect);

  // Draw text on top
  DrawText(Rect);
end;

procedure TlvkCustom3DGradientLabel.SetAutoSize(const Value: Boolean);
begin
  // This is a standard property accessor. Since we need to redraw the
  // control if this property changes, we only do the painting if the
  // value actually changes. So if you do AutoSize := True; and the
  // AutoSize property is already True, nothing will happen.
  // The rest of the property accessor functions are built in the same
  // way.
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    if FAutoSize then
    begin
      CalculateSize;
      Invalidate;
    end;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetBevelInner(const Value: TBevelCut);
begin
  if GetBevelInner <> Value then
  begin
    inherited BevelInner := Value;
    if FAutoSize then
      CalculateSize;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetBevelOuter(const Value: TBevelCut);
begin
  if GetBevelOuter <> Value then
  begin
    inherited BevelOuter := Value;
    if FAutoSize then
      CalculateSize;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetBevelWidth(const Value: Integer);
begin
  if GetBevelWidth <> Value then
  begin
    inherited BevelWidth := Value;
    if FAutoSize then
      CalculateSize;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetBorderWidth(const Value: Integer);
begin
  if GetBorderWidth <> Value then
  begin
    inherited BorderWidth := Value;
    if FAutoSize then
      CalculateSize;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetCaption(const Value: TCaption);
begin
  if Value <> GetCaption then
  begin
    inherited Caption := Value;
    if FAutoSize then
      CalculateSize;

    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetBorderColor(const Value: TColor);
begin
  if Value <> GetBorderColor then
  begin
    inherited Color := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetColor1(const Value: TColor);
begin
  if Value <> FColor1 then
  begin
    FColor1 := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetColor2(const Value: TColor);
begin
  if Value <> FColor2 then
  begin
    FColor2 := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetGradientDirection(
  const Value: TGradientDirection);
begin
  if Value <> FGradientDirection then
  begin
    FGradientDirection := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetHorizAlignment(
  const Value: THorizAlignment);
begin
  if Value <> FHorizAlignment then
  begin
    FHorizAlignment := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetMargins(const Value: TMargins);
begin
  FMargins.Assign(Value);
end;

procedure TlvkCustom3DGradientLabel.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetShadowDX(const Value: Integer);
begin
  if FShadowDX <> Value then
  begin
    FShadowDX := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetShadowDY(const Value: Integer);
begin
  if FShadowDY <> Value then
  begin
    FShadowDY := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetVertAlignment(
  const Value: TVertAlignment);
begin
  if Value <> FVertAlignment then
  begin
    FVertAlignment := Value;
    Invalidate;
  end;
end;

procedure TlvkCustom3DGradientLabel.SetName(const NewName: TComponentName);
begin
  inherited;
  Invalidate;
end;

{ TMargins }

procedure TMargins.Assign(Source: TPersistent);
begin
  if Source is TMargins then
    FMargins := (Source as TMargins).FMargins
  else
    inherited;
end;

constructor TMargins.Create(const ALabel: TlvkCustom3DGradientLabel);
begin
  inherited Create;

  FLabel := ALabel;
end;

function TMargins.GetMargin(const Index: Integer): Integer;
begin
  Result := FMargins[Index];
end;

procedure TMargins.SetMargin(const Index, Value: Integer);
begin
  if FMargins[Index] <> Value then
  begin
    if Value < 0 then
      FMargins[Index] := 0
    else
      FMargins[Index] := Value;

    if FLabel.AutoSize then
      FLabel.CalculateSize;
    FLabel.Invalidate;
  end;
end;

end.
