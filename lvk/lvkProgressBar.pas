{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a couple of custom progress bar components.
}
unit lvkProgressBar;

// $Author: Lasse V. Karlsen $
// $Revision: 10 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkProgressBar.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  {$IFDEF DELPHI6UP}
  Types,
  {$ENDIF}
  SysUtils, Classes, Windows, Graphics, Messages, Controls, ExtCtrls,
  lvkControls;

const
  DEFAULT_MIN_VALUE             = 0;
  DEFAULT_MAX_VALUE             = 100;
  DEFAULT_VALUE                 = 50;
  DEFAULT_BACKGROUND_COLOR      = clWindow;
  DEFAULT_FOREGROUND_COLOR      = clHighlight;
  DEFAULT_TEXT_FORMAT           = '%0:0.0f %%';
  DEFAULT_BORDER_WIDTH          = 1;
  DEFAULT_INNER_BORDER_WIDTH    = 0;
  DEFAULT_BORDER_COLOR          = clBlack;
  DEFAULT_TEXT_FOREGROUND_COLOR = clWindow;
  DEFAULT_TEXT_BACKGROUND_COLOR = clHighlight;
  DEFAULT_UPDATE_INTERVAL       = 0;

  DEFAULT_TOP_LEFT_COLOR        = clDkGray;
  DEFAULT_TOP_RIGHT_COLOR       = clBlack;
  DEFAULT_BOTTOM_LEFT_COLOR     = clSilver;
  DEFAULT_BOTTOM_RIGHT_COLOR    = clDkGray;

type
  IlvkProgress = interface
    ['{DD67A77D-631B-4C6E-98B4-C1C112B96BB1}']

    function GetMinValue: Integer;
    procedure SetMinValue(const Value: Integer);
    property MinValue: Integer read GetMinValue write SetMinValue;

    function GetMaxValue: Integer;
    procedure SetMaxValue(const Value: Integer);
    property MaxValue: Integer read GetMaxValue write SetMaxValue;

    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
    property Value: Integer read GetValue write SetValue;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Reset;

    procedure UpdateNow;
  end;

  TlvkCustomProgressBar = class(TlvkGraphicControl)
  private
    FInnerBorderWidth     : Word;
    FBorderWidth          : Word;
    FBackgroundColor      : TColor;
    FForegroundColor      : TColor;
    FBorderColor          : TColor;
    FTextFormat           : string;
    FMinValue             : Integer;
    FMaxValue             : Integer;
    FValue                : Integer;
    FUpdateLevel          : Integer;
    FChanged              : Boolean;
    FThreadSafe           : Boolean;
    FUpdateInterval       : Integer;
    FUpdateTimer          : TTimer;
    FHandle               : THandle;
    FTextBackgroundColor  : TColor;
    FTextForegroundColor  : TColor;
    FFont                 : TFont;

    function GetMaxValue: Integer;
    function GetMinValue: Integer;
    function GetValue: Integer;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    procedure SetValue(const Value: Integer);

    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetTextFormat(const Value: string);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Word);
    procedure SetTextBackgroundColor(const Value: TColor);
    procedure SetTextForegroundColor(const Value: TColor);
    procedure SetInnerBorderWidth(const Value: Word);
    procedure SetFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);
    procedure SetUpdateInterval(const Value: Integer);

    procedure UpdateByTimer(Sender: TObject);

  protected
    procedure UpdateInformation(var Min, Max, Value: Integer); virtual;
    procedure MessageHandler(var Message: TMessage); virtual;
    procedure Changed; virtual;

    property BackgroundColor: TColor read FBackgroundColor
      write SetBackgroundColor default DEFAULT_BACKGROUND_COLOR;
    property ForegroundColor: TColor read FForegroundColor
      write SetForegroundColor default DEFAULT_FOREGROUND_COLOR;
    property TextFormat: string read FTextFormat write SetTextFormat;
    property MinValue: Integer read FMinValue write SetMinValue
      default DEFAULT_MIN_VALUE;
    property MaxValue: Integer read FMaxValue write SetMaxValue
      default DEFAULT_MAX_VALUE;
    property Value: Integer read FValue write SetValue
      default DEFAULT_VALUE;
    property BorderWidth: Word read FBorderWidth write SetBorderWidth
      default DEFAULT_BORDER_WIDTH;
    property BorderColor: TColor read FBorderColor write SetBorderColor
      default DEFAULT_BORDER_COLOR;
    property TextForegroundColor: TColor read FTextForegroundColor
      write SetTextForegroundColor default DEFAULT_TEXT_FOREGROUND_COLOR;
    property TextBackgroundColor: TColor read FTextBackgroundColor
      write SetTextBackgroundColor default DEFAULT_TEXT_BACKGROUND_COLOR;
    property InnerBorderWidth: Word read FInnerBorderWidth
      write SetInnerBorderWidth default DEFAULT_INNER_BORDER_WIDTH;
    property Font: TFont read FFont write SetFont;
    property UpdateInterval: Integer read FUpdateInterval
      write SetUpdateInterval default DEFAULT_UPDATE_INTERVAL;

    function CalcRectangle(const Left: Boolean; const Rect: TRect): Trect;

    procedure Paint; override;
    procedure PaintBackground(const Bitmap: TBitmap; var Rect: TRect); virtual;
    procedure PaintForeground(const Bitmap: TBitmap; var Rect: TRect); virtual;
    procedure PaintText(const Bitmap: TBitmap; var Rect: TRect); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure UpdateNow;
    procedure Reset; virtual;
  end;

  TlvkSegmentedProgressBar = class;

  TlvkProgressBar = class(TlvkCustomProgressBar, IlvkProgress)
  published
    property PopupMenu;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DELPHI6UP}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
    property OnResize;

    property Align;
    property Anchors;
    property Constraints;
    property Visible;

    property MinValue;
    property MaxValue;
    property Value;
    property ForegroundColor;
    property BackgroundColor;
    property TextFormat;
    property BorderWidth;
    property InnerBorderWidth;
    property BorderColor;
    property Font;
    property UpdateInterval;
  end;

  TlvkProgressBarSegment = class(TCollectionItem, IUnknown, IlvkProgress)
  private
    FMinValue     : Integer;
    FMaxValue     : Integer;
    FValue        : Integer;
    FUpdateLevel  : Integer;
    FChanged      : Boolean;
    FProgressBar  : TlvkCustomProgressBar;
    FName: TCaption;

    function GetMaxValue: Integer;
    function GetMinValue: Integer;
    function GetValue: Integer;
    procedure SetMaxValue(const Value: Integer);
    procedure SetMinValue(const Value: Integer);
    procedure SetValue(const Value: Integer);

    procedure NotifyChanged;

  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function OwnerProgressBar: TlvkSegmentedProgressBar;
    procedure UpdateInformation(var Min, Max, Value: Integer);

  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateNow;

    procedure Reset;

  published
    property Name: TCaption read FName write FName;
    property ProgressBar: TlvkCustomProgressBar read FProgressBar
      write FProgressBar;
    property MinValue: Integer read FMinValue write SetMinValue
      default DEFAULT_MIN_VALUE;
    property MaxValue: Integer read FMaxValue write SetMaxValue
      default DEFAULT_MAX_VALUE;
    property Value: Integer read FValue write SetValue
      default DEFAULT_VALUE;
  end;

  TlvkProgressBarSegmentCollection = class(TOwnedCollection)
  private
    procedure SetItem(Index: Integer;
      const Value: TlvkProgressBarSegment);
    function GetItem(Index: Integer): TlvkProgressBarSegment;

  public
    constructor Create(AOwner: TPersistent);

    function Add: TlvkProgressBarSegment;
    function FindItemID(ID: Integer): TlvkProgressBarSegment;
    function Insert(Index: Integer): TlvkProgressBarSegment;
    property Items[Index: Integer]: TlvkProgressBarSegment read GetItem
      write SetItem; default;
    function SegmentByName(const Name: string): TlvkProgressBarSegment;
  end;

  TlvkCustomSegmentedProgressBar = class(TlvkCustomProgressBar)
  private
    FSegments : TlvkProgressBarSegmentCollection;
    procedure SetSegments(const Value: TlvkProgressBarSegmentCollection);

  protected
    property Segments: TlvkProgressBarSegmentCollection read FSegments
      write SetSegments;
    procedure UpdateInformation(var Min, Max, Value: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset; override;
  end;

  TlvkSegmentedProgressBar = class(TlvkCustomSegmentedProgressBar)
  published
    property Segments;

    property PopupMenu;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DELPHI6UP}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
    property OnResize;

    property Align;
    property Anchors;
    property Constraints;
    property Visible;

    property ForegroundColor;
    property BackgroundColor;
    property TextFormat;
    property BorderWidth;
    property InnerBorderWidth;
    property BorderColor;
    property Font;
    property UpdateInterval;
  end;

implementation

uses
  {$IFNDEF DELPHI6UP}
  Forms,
  {$ENDIF}
  Math;

{ TlvkCustomProgressBar }

procedure TlvkCustomProgressBar.BeginUpdate;
begin
  InterlockedIncrement(FUpdateLevel);
end;

function TlvkCustomProgressBar.CalcRectangle(const Left: Boolean;
  const Rect: TRect): Trect;
var
  p : Double;
begin
  Result := Rect;
  if FMaxValue = FMinValue then
    p := 100.0
  else
    p := (FValue - FMinValue) / (FMaxValue - FMinValue);

  Result.Right := Trunc(Rect.Left + (Rect.Right - Rect.Left) * p);

  if not Left then
  begin
    Result.Left := Result.Right + 1;
    Result.Right := Rect.Right;
  end;
end;

procedure TlvkCustomProgressBar.Changed;
begin
  if FUpdateLevel = 0 then
  begin
    if FUpdateInterval = 0 then
    begin
      if FThreadSafe and (not (csDesigning in ComponentState)) then
        PostMessage(FHandle, CM_CHANGED, 0, 0)
      else
        Invalidate;
    end;
  end else
    FChanged := True;
end;

constructor TlvkCustomProgressBar.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];
  FMinValue := DEFAULT_MIN_VALUE;
  FMaxValue := DEFAULT_MAX_VALUE;
  FValue := DEFAULT_VALUE;
  FBackgroundColor := DEFAULT_BACKGROUND_COLOR;
  FForegroundColor := DEFAULT_FOREGROUND_COLOR;
  FTextFormat := DEFAULT_TEXT_FORMAT;
  FTextForegroundColor := DEFAULT_TEXT_FOREGROUND_COLOR;
  FTextBackgroundColor := DEFAULT_TEXT_BACKGROUND_COLOR;
  FInnerBorderWidth := DEFAULT_INNER_BORDER_WIDTH;

  FFont := TFont.Create;
  FFont.OnChange := FontChanged;

  Width := 100;
  Height := 17;

  if not (csDesigning in ComponentState) then
    FHandle := AllocateHWnd(MessageHandler)
  else
    FHandle := INVALID_HANDLE_VALUE;
end;

destructor TlvkCustomProgressBar.Destroy;
begin
  if not (csDesigning in ComponentState) then
    DeallocateHWnd(FHandle);

  FFont.Free;

  inherited;
end;

procedure TlvkCustomProgressBar.EndUpdate;
begin
  if FUpdateLevel > 0 then
  begin
    InterlockedDecrement(FUpdateLevel);

    if (FUpdateLevel = 0) and FChanged then
    begin
      if FThreadSafe and (not (csDesigning in ComponentState)) then
        PostMessage(FHandle, CM_CHANGED, 0, 0)
      else
        Invalidate;
    end;
  end;
end;

procedure TlvkCustomProgressBar.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TlvkCustomProgressBar.GetMaxValue: Integer;
begin
  Result := FMaxValue;
end;

function TlvkCustomProgressBar.GetMinValue: Integer;
begin
  Result := FMinValue;
end;

function TlvkCustomProgressBar.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TlvkCustomProgressBar.MessageHandler(var Message: TMessage);
begin
  if Message.Msg = CM_INVALIDATE then
    Invalidate
  else
    DefaultHandler(Message);
end;

procedure TlvkCustomProgressBar.Paint;
var
  Bitmap  : TBitmap;
  r       : TRect;
begin
  Bitmap := TBitmap.Create;
  try
    UpdateInformation(FMinValue, FMaxValue, FValue);
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    r := Rect(0, 0, Width, Height);
    PaintBackground(Bitmap, r);
    PaintForeground(Bitmap, r);
    PaintText(Bitmap, r);

    Canvas.Draw(0, 0, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TlvkCustomProgressBar.PaintBackground(const Bitmap: TBitmap;
  var Rect: TRect);

  procedure AdjustRect(var Rect: TRect; const Amount: Word);
  begin
    if Amount > 0 then
    begin
      Inc(Rect.Left, Amount);
      Inc(Rect.Top, Amount);
      Dec(Rect.Right, Amount);
      Dec(Rect.Bottom, Amount);
    end;
  end;

begin
  Bitmap.Canvas.Brush.Color := FBackgroundColor;
  Bitmap.Canvas.Brush.Style := bsSolid;
  if FBorderWidth > 0 then
  begin
    Bitmap.Canvas.Pen.Color := FBorderColor;
    Bitmap.Canvas.Pen.Width := FBorderWidth * 2 - 1;
  end else
    Bitmap.Canvas.Pen.Color := FBackgroundColor;
  Bitmap.Canvas.Rectangle(Rect);

  AdjustRect(Rect, FBorderWidth);
  AdjustRect(Rect, FInnerBorderWidth);
end;

procedure TlvkCustomProgressBar.PaintForeground(const Bitmap: TBitmap;
  var Rect: TRect);
var
  r : TRect;
begin
  Bitmap.Canvas.Brush.Color := FForegroundColor;
  Bitmap.Canvas.Brush.Style := bsSolid;
  Bitmap.Canvas.Pen.Color := FForegroundColor;
  Bitmap.Canvas.Pen.Width := 0;

  r := CalcRectangle(True, Rect);
  Bitmap.Canvas.Rectangle(r);
end;

procedure TlvkCustomProgressBar.PaintText(const Bitmap: TBitmap;
  var Rect: TRect);
var
  s       : string;
  r       : TRect;
  x, y    : Integer;
  Percent : Double;
begin
  if FTextFormat <> '' then
  begin
    if FMinValue = FMaxValue then
      Percent := 100.0
    else
      Percent := 100.0 * ((FValue - FMinValue) / (FMaxValue - FMinValue));

    s := Format(FTextFormat, [Percent, FMinValue, FMaxValue, FValue, FMaxValue-FMinValue]);
    x := Rect.Left + (Rect.Right - Rect.Left + 1 - Canvas.TextWidth(s)) div 2;
    y := Rect.Top + (Rect.Bottom - Rect.Top + 1 - Canvas.TextHeight(s)) div 2;

    Bitmap.Canvas.Font.Assign(FFont);

    r := CalcRectangle(True, Rect);
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.Font.Color := FTextForegroundColor;
    Bitmap.Canvas.TextRect(r, x, y, s);

    r := CalcRectangle(False, Rect);
    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.Font.Color := FTextBackgroundColor;
    Bitmap.Canvas.TextRect(r, x, y, s);
  end;
end;

procedure TlvkCustomProgressBar.Reset;
begin
  FValue := FMinValue;
end;

procedure TlvkCustomProgressBar.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TlvkCustomProgressBar.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TlvkCustomProgressBar.SetBorderWidth(const Value: Word);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TlvkCustomProgressBar.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TlvkCustomProgressBar.SetForegroundColor(const Value: TColor);
begin
  if FForegroundColor <> Value then
  begin
    FForegroundColor := Value;
    Changed;
  end;
end;

procedure TlvkCustomProgressBar.SetInnerBorderWidth(const Value: Word);
begin
  FInnerBorderWidth := Value;
end;

procedure TlvkCustomProgressBar.SetMaxValue(const Value: Integer);
begin
  if FMaxValue <> Value then
  begin
    if Value < FMinValue then
      FMaxValue := FMinValue
    else
      FMaxValue := Value;

    if FValue > FMaxValue then
      FValue := FMaxValue;

    Changed;
  end;
end;

procedure TlvkCustomProgressBar.SetMinValue(const Value: Integer);
begin
  if FMinValue <> Value then
  begin
    if Value > FMaxValue then
      FMinValue := FMaxValue
    else
      FMinValue := Value;

    if FValue < FMinValue then
      FValue := FMinValue;

    Changed;
  end;
end;

procedure TlvkCustomProgressBar.SetTextBackgroundColor(
  const Value: TColor);
begin
  FTextBackgroundColor := Value;
end;

procedure TlvkCustomProgressBar.SetTextForegroundColor(
  const Value: TColor);
begin
  FTextForegroundColor := Value;
end;

procedure TlvkCustomProgressBar.SetTextFormat(const Value: string);
begin
  if FTextFormat <> Value then
  begin
    FTextFormat := Value;
    Changed;
  end;
end;

procedure TlvkCustomProgressBar.SetUpdateInterval(const Value: Integer);
begin
  if FUpdateInterval <> Value then
  begin
    FUpdateInterval := Value;

    if not (csDesigning in ComponentState) then
    begin
      if FUpdateInterval = 0 then
        FreeAndNil(FUpdateTimer)
      else begin
        if not Assigned(FUpdateTimer) then
        begin
          FUpdateTimer := TTimer.Create(nil);
          FUpdateTimer.OnTimer := UpdateByTimer;
        end;

        FUpdateTimer.Interval := FUpdateInterval;
      end;
    end;
  end;
end;

procedure TlvkCustomProgressBar.SetValue(const Value: Integer);
begin
  if FValue <> Value then
  begin
    if Value < FMinValue then
      FValue := FMinValue
    else if Value > FMaxValue then
      FValue := FMaxValue
    else
      FValue := Value;
    Changed;
  end;
end;

procedure TlvkCustomProgressBar.UpdateByTimer(Sender: TObject);
begin
  if FUpdateLevel = 0 then
    Invalidate;
end;

procedure TlvkCustomProgressBar.UpdateInformation(var Min, Max, Value: Integer);
begin
  // Do nothing in the default progress bar
end;

procedure TlvkCustomProgressBar.UpdateNow;
begin
  PostMessage(FHandle, CM_INVALIDATE, 0, 0);
end;

{ TlvkCustomSegmentedProgressBar }

constructor TlvkCustomSegmentedProgressBar.Create(AOwner: TComponent);
begin
  inherited;

  FSegments := TlvkProgressBarSegmentCollection.Create(Self);
end;

destructor TlvkCustomSegmentedProgressBar.Destroy;
begin
  FSegments.Free;

  inherited;
end;

procedure TlvkCustomSegmentedProgressBar.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  Index : Integer;
begin
  inherited;
  if Operation = opRemove then
    for Index := 0 to FSegments.Count-1 do
      if FSegments[Index].ProgressBar = AComponent then
        FSegments[Index].ProgressBar := nil;
end;

procedure TlvkCustomSegmentedProgressBar.Reset;
var
  Index : Integer;
begin
  inherited;

  for Index := 0 to FSegments.Count-1 do
    FSegments[Index].Reset;
end;

procedure TlvkCustomSegmentedProgressBar.SetSegments(
  const Value: TlvkProgressBarSegmentCollection);
begin
  FSegments.Assign(Value);
end;

procedure TlvkCustomSegmentedProgressBar.UpdateInformation(var Min, Max, Value: Integer);
var
  Index     : Integer;
  SubMin    : Integer;
  SubMax    : Integer;
  SubValue  : Integer;
  Segment   : TlvkProgressBarSegment;
begin
  Min := 0;
  Max := 0;
  Value := 0;

  for Index := 0 to FSegments.Count-1 do
  begin
    Segment := FSegments[Index];
    Segment.UpdateInformation(SubMin, SubMax, SubValue);
    Inc(Max, SubMax-SubMin);
    Inc(Value, SubValue-SubMin);
  end;

  if FSegments.Count = 0 then
  begin
    Max := 100;
    Value := 50;
  end;
end;

{ TlvkProgressBarSegmentCollection }

function TlvkProgressBarSegmentCollection.Add: TlvkProgressBarSegment;
begin
  Result := inherited Add as TlvkProgressBarSegment;
end;

constructor TlvkProgressBarSegmentCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TlvkProgressBarSegment);
end;

function TlvkProgressBarSegmentCollection.FindItemID(
  ID: Integer): TlvkProgressBarSegment;
begin
  Result := inherited FindItemID(ID) as TlvkProgressBarSegment;
end;

function TlvkProgressBarSegmentCollection.GetItem(
  Index: Integer): TlvkProgressBarSegment;
begin
  Result := inherited Items[Index] as TlvkProgressBarSegment;
end;

function TlvkProgressBarSegmentCollection.Insert(
  Index: Integer): TlvkProgressBarSegment;
begin
  Result := inherited Insert(Index) as TlvkProgressBarSegment;
end;

function TlvkProgressBarSegmentCollection.SegmentByName(
  const Name: string): TlvkProgressBarSegment;
var
  Index : Integer;
begin
  Result := nil;

  for Index := 0 to Count-1 do
    if CompareText(Items[Index].Name, Name) = 0 then
    begin
      Result := Items[Index];
      Break;
    end;
end;

procedure TlvkProgressBarSegmentCollection.SetItem(Index: Integer;
  const Value: TlvkProgressBarSegment);
begin
  inherited Items[Index] := Value;
end;

{ TlvkProgressBarSegment }

procedure TlvkProgressBarSegment.BeginUpdate;
begin
  InterlockedIncrement(FUpdateLevel);
end;

procedure TlvkProgressBarSegment.EndUpdate;
begin
  if FUpdateLevel > 0 then
  begin
    InterlockedDecrement(FUpdateLevel);

    if (FUpdateLevel = 0) and FChanged then
      OwnerProgressBar.Changed;
  end;
end;

function TlvkProgressBarSegment.GetMaxValue: Integer;
begin
  Result := FMaxValue;
end;

function TlvkProgressBarSegment.GetMinValue: Integer;
begin
  Result := FMinValue;
end;

function TlvkProgressBarSegment.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TlvkProgressBarSegment.NotifyChanged;
begin
  if FUpdateLevel = 0 then
    OwnerProgressBar.Changed
  else
    FChanged := True;
end;

function TlvkProgressBarSegment.OwnerProgressBar: TlvkSegmentedProgressBar;
begin
  Result := (Collection as TlvkProgressBarSegmentCollection).GetOwner as
    TlvkSegmentedProgressBar;
end;

function TlvkProgressBarSegment.QueryInterface(const IID: TGUID;
  out Obj): HRESULT;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TlvkProgressBarSegment.Reset;
begin
  FValue := FMinValue;
end;

procedure TlvkProgressBarSegment.SetMaxValue(const Value: Integer);
begin
  if FMaxValue <> Value then
  begin
    if Value < FMinValue then
      FMaxValue := FMinValue
    else
      FMaxValue := Value;

    if FValue > FMaxValue then
      FValue := FMaxValue;

    NotifyChanged;
  end;
end;

procedure TlvkProgressBarSegment.SetMinValue(const Value: Integer);
begin
  if FMinValue <> Value then
  begin
    if Value > FMaxValue then
      FMinValue := FMaxValue
    else
      FMinValue := Value;

    if FValue < FMinValue then
      FValue := FMinValue;

    NotifyChanged;
  end;
end;

procedure TlvkProgressBarSegment.SetValue(const Value: Integer);
begin
  if FValue <> Value then
  begin
    if Value < FMinValue then
      FValue := FMinValue
    else if Value > FMaxValue then
      FValue := FMaxValue
    else
      FValue := Value;

    NotifyChanged;
  end;
end;

procedure TlvkProgressBarSegment.UpdateInformation(var Min, Max,
  Value: Integer);
begin
  if Assigned(FProgressBar) then
    FProgressBar.UpdateInformation(Min, Max, Value)
  else begin
    Min := FMinValue;
    Max := FMaxValue;
    Value := FValue;
  end;
end;

procedure TlvkProgressBarSegment.UpdateNow;
begin
  OwnerProgressBar.UpdateNow;
end;

function TlvkProgressBarSegment._AddRef: Integer;
begin
  Result := -1;
end;

function TlvkProgressBarSegment._Release: Integer;
begin
  Result := -1;
end;

end.
