{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkPaintBox component.
}
unit lvkPaintBox;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkPaintBox.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Classes, Forms, Graphics, Controls, Messages, SysUtils,
  lvkVersion, lvkControls;

type
  TlvkCustomPaintBox = class;

  { Description:
      This event handler is used when the TlvkPaintbox component requires
      a portion of the image to be redrawn.
    Parameters:
      Sender - The paintbox component that calls the event handler.
      Contents - The bitmap to draw into. Use the Canvas property, as well as
        Width, Height, PixelFormat, etc. to do the job properly.
      Region - The region to redraw.
    See also:
      TlvkCustomPaintBox, TlvkPaintBox
  }
  TPaintRegionEvent = procedure(const Sender: TlvkCustomPaintBox; const Contents: TBitmap; const Region: TRect) of object;

  { Description:
      This is the custom version of the TlvkPaintBox component. This is the
      component class that implements all the code.

      This paintbox differs from the built-in Delphi TPaintBox as follows:
        * The component caches its contents. It will not require any code to
          maintain its contents when you reveal part of the paintbox, like
          when you move a window on top of your application and then away
          again.
        * The component only calls the paint event handler for new regions.
          For instance, if you resize the control to a bigger size, only
          the new area will be repainted. The existing area will be left
          as is. Likewise, if you call the ScrollBy method, only the
          area which were scrolled into view will be repainted.
    See also:
      TlvkPaintBox
  }
  TlvkCustomPaintBox = class(TlvkCustomControl)
  private
    FContents       : TBitmap;
    FOnPaintRegion  : TPaintRegionEvent;
    FUpdateCount    : Integer;
    FChanged        : Boolean;
    FScrollDX       : Integer;
    FScrollDY       : Integer;

    procedure OnContentsChange(Sender: TObject);
    procedure SetOnPaintRegion(const NewHandler: TPaintRegionEvent);

    procedure DoPaintRegion(const Region: TRect);

  protected
    procedure Resize; override;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;

    { Description:
        This event handler will be called whenever a portion of the paintbox
        requires updating. It might be due to resizing the paintbox, a call
        to InvalidateAll, or the results of a scroll.
    }
    property OnPaintRegion: TPaintRegionEvent read FOnPaintRegion write SetOnPaintRegion;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        This property returns the bitmap that is used as contents of the
        paintbox. You can draw on the bitmap at all times, even outside of the
        OnPaintRegion event handler. Doing so will force an immediate update
        of the paintbox visuals.
      Returns:
        TBitmap class instance that contains the contents of the paintbox.
    }
    property Contents: TBitmap read FContents;

    { Description:
        Use this method to lock the paintbox. When the paintbox is locked,
        no updating will take place. When you unlock the paintbox with a call
        to EndUpdate, and the paintbox contents was changed in the meantime,
        the paintbox visuals will be updated as well.
      See also:
        EndUpdate@Boolean
    }
    procedure BeginUpdate;

    { Description:
        Use this method to unlock the paintbox. When the paintbox is locked,
        no updating will take place. When you unlock the paintbox , and the
        paintbox contents was changed in the meantime, the paintbox visuals
        will be updated as well.
      Parameters:
        OwnerPaints - Set this to True if the calling code will take care of
          updating the paintbox visuals itself. This is useful when the
          programmer knows that only a small portion of the paintbox has
          been updated.
      See also:
        BeginUpdate
    }
    procedure EndUpdate(const OwnerPaints: Boolean=False);

    { Description:
        This method can be used to invalidate and force a repaint of a given
        area of the paintbox. The paint event handler will be called once to
        repaint the given area. After this, a update of the paintbox
        visuals will be performed.
      Parameters:
        Rect - The region of the paintbox to redraw.
      See also:
        InvalidateAll
    }
    procedure InvalidateRect(const Rect: TRect);

    { Description:
        This method can be used to invalidate the contents of the paintbox
        and force a repaint. The paint event handler will be called once to
        repaint the whole paintbox. After this, a update of the paintbox
        visuals will be performed.
      See also:
        InvalidateRect
    }
    procedure InvalidateAll;

    { Description:
        This method is called to paint the paintbox on the form. It has been
        overridden from TCustomControl.
    }
    procedure Paint; override;

    { Description:
        Use this method to repaint a given area of the paintbox on the form.
        You can use this if you wish to show parts of the paintbox.
      Parameters:
        x1, y1, x2, y2 - The area to paint.
      See also:
        PaintRegion@TRect
    }
    procedure PaintRegion(const x1, y1, x2, y2: Integer); overload;

    { Description:
        Use this method to repaint a given area of the paintbox on the form.
        You can use this if you wish to show parts of the paintbox.
      Parameters:
        Region - The area to paint.
      See also:
        PaintRegion@Integer@Integer@Integer@Integer
    }
    procedure PaintRegion(const Region: TRect); overload;

    { Description:
        This method can be used to scroll the contents of the paintbox in
        a given direction by a given amount of pixels. The new area which
        thus "scrolls into view" will be repainted by the event handler. The
        existing bitmap data that is scrolled around on the paintbox will
        simply be moved around, no repainting required.
      Parameters:
        DX, DY - The amount of pixels to scroll the contents by. -15,0 will
          scroll the contents 15 pixels to the left, +15, 0 will scroll
          15 pixels to the right. 0, -15 will scroll it 15 pixels up and
          0, +15 will scroll it 15 pixels down. You can scroll in both
          left/right and up/down directions at the same time.
    }
    procedure ScrollBy(const DX, DY: Integer);
  end;

  { Description:
      This is the usable component version of TlvkCustomPaintBox. See
      TlvkCustomPaintBox for details.
    See also:
      TlvkCustomPaintBox
  }
  TlvkPaintBox = class(TlvkCustomPaintBox)
  published
    // <ALIAS TlvkCustomPaintBox.OnPaintRegion>
    property OnPaintRegion;
    
    property Constraints;
    property Enabled;
    property OnClick;
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
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property Action;
    property Anchors;
    property BiDiMode;
    property Caption;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Align;
    property AutoSize;
    property OnDblClick;

    property OnCanResize;
    property OnConstrainedResize;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;

implementation

uses
  Math;

{ TlvkCustomPaintBox }

procedure TlvkCustomPaintBox.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

constructor TlvkCustomPaintBox.Create(AOwner: TComponent);
begin
  inherited;

  FContents := TBitmap.Create;
  FContents.Width := Width;
  FContents.Height := Height;
  FContents.PixelFormat := pf24bit;
  FContents.OnChange := OnContentsChange;
end;

destructor TlvkCustomPaintBox.Destroy;
begin
  FContents.Free;

  inherited;
end;

procedure TlvkCustomPaintBox.DoPaintRegion(const Region: TRect);
begin
  if Assigned(FOnPaintRegion) then
    FOnPaintRegion(Self, Contents, Rect(Region.Left, Region.Top, Region.Right+1, Region.Bottom+1));
end;

procedure TlvkCustomPaintBox.EndUpdate(const OwnerPaints: Boolean);
var
  DX, DY  : Integer;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and FChanged then
  begin
    if (FScrollDX <> 0) or (FScrollDY <> 0) then
    begin
      DX := FScrollDX;
      DY := FScrollDY;
      FScrollDX := 0;
      FScrollDY := 0;

      ScrollBy(DX, DY);
    end;

    if not OwnerPaints then
      Refresh;
  end;
end;

procedure TlvkCustomPaintBox.InvalidateAll;
begin
  InvalidateRect(Rect(0, 0, FContents.Width-1, FContents.Height-1));
end;

procedure TlvkCustomPaintBox.InvalidateRect(const Rect: TRect);
begin
  BeginUpdate;
  try
    OnPaintRegion(Self, FContents, Rect);

    if FUpdateCount = 0 then
      Paint;
  finally
    EndUpdate;
  end;
end;

procedure TlvkCustomPaintBox.OnContentsChange(Sender: TObject);
begin
  if FUpdateCount = 0 then
    Paint
  else
    FChanged := True;
end;

procedure TlvkCustomPaintBox.Paint;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.Rectangle(0, 0, Width, Height);
  end else begin
    Canvas.Draw(0, 0, FContents);
    FChanged := False;
  end;
end;

procedure TlvkCustomPaintBox.PaintRegion(const x1, y1, x2, y2: Integer);
begin
  PaintRegion(Rect(x1, y1, x2, y2));
end;

procedure TlvkCustomPaintBox.PaintRegion(const Region: TRect);
var
  DC      : HDC;
  Handle  : HWND;
  UseRect : TRect;
begin
  DC := GetDeviceContext(Handle);
  try
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        TControlCanvas(Canvas).UpdateTextFlags;
        UseRect := Rect(Max(Region.Left-1, 0), Max(Region.Top-1, 0), Region.Right+1, Region.Bottom+1);
        Canvas.CopyRect(UseRect, FContents.Canvas, UseRect);
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TlvkCustomPaintBox.Resize;
var
  OldWidth, OldHeight : Integer;
begin
  inherited;

  BeginUpdate;
  try
    OldWidth := FContents.Width;
    OldHeight := FContents.Height;

    FContents.Width := Width;
    FContents.Height := Height;

    if Width > OldWidth then
      DoPaintRegion(Rect(OldWidth, 0, Width-1, Height-1));

    if Height > OldHeight then
      DoPaintRegion(Rect(0, OldHeight, OldWidth-1, Height-1));
  finally
    EndUpdate;
  end;
end;

procedure TlvkCustomPaintBox.ScrollBy(const DX, DY: Integer);
begin
  if (DY = 0) and (DX = 0) then
    Exit;

  if FUpdateCount > 0 then
  begin
    FScrollDX := FScrollDX + DX;
    FScrollDY := FScrollDY + DY;
    FChanged := True;
    Exit;
  end;

  BeginUpdate;
  try
    if (Abs(DX) >= Width) or (Abs(DY) >= Height) then
    begin
      DoPaintRegion(Rect(0, 0, FContents.Width-1, FContents.Height-1));
    end else if DY = 0 then
    begin
      if DX < 0 then
      begin
        // Scroll left, repaint right border
        FContents.Canvas.CopyRect(
          Rect(0, 0, FContents.Width+DX, FContents.Height),
          FContents.Canvas,
          Rect(-DX, 0, FContents.Width, FContents.Height));
        DoPaintRegion(Rect(FContents.Width+DX, 0, FContents.Width-1, FContents.Height-1));
      end else begin
        // Scroll right, repaint left border
        FContents.Canvas.CopyRect(
          Rect(DX, 0, FContents.Width, FContents.Height),
          FContents.Canvas,
          Rect(0, 0, FContents.Width-DX, FContents.Height));
        DoPaintRegion(Rect(0, 0, DX-1, FContents.Height-1));
      end;
    end else if DX = 0 then
    begin
      if DY < 0 then
      begin
        // Scroll up, repaint bottom border
        FContents.Canvas.CopyRect(
          Rect(0, 0, FContents.Width, FContents.Height+DY),
          FContents.Canvas,
          Rect(0, -DY, FContents.Width, FContents.Height));
        DoPaintRegion(Rect(0, FContents.Height+DY, FContents.Width-1, FContents.Height-1));
      end else begin
        // Scroll down, repaint top border
        FContents.Canvas.CopyRect(
          Rect(0, DY, FContents.Width, FContents.Height),
          FContents.Canvas,
          Rect(0, 0, FContents.Width, FContents.Height-DY));
        DoPaintRegion(Rect(0, 0, FContents.Width-1, DY-1));
      end;
    end else begin
      if DY < 0 then
      begin
        // Upwards
        if DX < 0 then
        begin
          // Up left
          FContents.Canvas.CopyRect(
            Rect(0, 0, FContents.Width+DX, FContents.Height+DY),
            FContents.Canvas,
            Rect(-DX, -DY, FContents.Width, FContents.Height));
          DoPaintRegion(Rect(0, FContents.Height+DY, FContents.Width-1, FContents.Height-1));
          DoPaintRegion(Rect(FContents.Width+DX, 0, FContents.Width-1, FContents.Height+DY-1));
        end else begin
          FContents.Canvas.CopyRect(
            Rect(Dx, 0, FContents.Width, FContents.Height+DY),
            FContents.Canvas,
            Rect(0, -DY, FContents.Width-DX, FContents.Height));
          DoPaintRegion(Rect(0, FContents.Height+DY, FContents.Width-1, FContents.Height-1));
          DoPaintRegion(Rect(0, 0, DX-1, FContents.Height+DY-1));
        end;
      end else begin
        // Downwards
        if DX < 0 then
        begin
          // Down left
          FContents.Canvas.CopyRect(
            Rect(0, DY, FContents.Width+DX, FContents.Height),
            FContents.Canvas,
            Rect(-DX, 0, FContents.Width, FContents.Height-DY));
          DoPaintRegion(Rect(0, 0, FContents.Width-1, DY-1));
          DoPaintRegion(Rect(FContents.Width+DX,DY, FContents.Width-1, FContents.Height-1));
        end else begin
          // Down right
          FContents.Canvas.CopyRect(
            Rect(DX, DY, FContents.Width, FContents.Height),
            FContents.Canvas,
            Rect(0, 0, FContents.Width-DX, FContents.Height-DY));
          DoPaintRegion(Rect(0, 0, FContents.Width-1, DY-1));
          DoPaintRegion(Rect(0, DY, DX-1, FContents.Height-1));
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TlvkCustomPaintBox.SetOnPaintRegion(
  const NewHandler: TPaintRegionEvent);
begin
  FOnPaintRegion := NewHandler;
  DoPaintRegion(Rect(0, 0, FContents.Width-1, FContents.Height-1));
end;

procedure TlvkCustomPaintBox.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if csDesigning in ComponentState then
    inherited;
end;

end.
