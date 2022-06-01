{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a component for showing validation error messages. It
    will show as a triangle with an exclamation mark in it if there are
    validation messages, or nothing at all if no validation messages exists.
}
unit lvkValidationImage;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkValidationImage.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{$R ErrorImages.res}

uses
  SysUtils, Classes, Controls, lvkValidators, AppEvnts, lvkControls,
  Messages, Dialogs;

type
  { Description:
      This custom control works like the TlvkValidationLabel control, except it
      will only show a triangle with an exclamation mark in it if there are
      validation messages. If no validation messages exists, the triangle
      will not show at all. The control has mouse-over effects and a tooltip
      that will show the validation messages.
  }
  TlvkCustomValidationImage = class(TlvkCustomControl)
  private
    FValidators   : TValidatorCollection;
    FAppEvents    : TApplicationEvents;
    FHighlighted  : Boolean;
    FErrors       : TList;
    FOnMouseLeave : TNotifyEvent;
    FOnMouseEnter : TNotifyEvent;
    FShowAll      : Boolean;
    FOnBeforeShow : TNotifyEvent;
    FOnAfterShow  : TNotifyEvent;
    FOnBeforeHide : TNotifyEvent;
    FOnAfterHide  : TNotifyEvent;

    procedure DoOnBeforeShow;
    procedure DoOnAfterShow;
    procedure DoOnBeforeHide;
    procedure DoOnAfterHide;

    procedure SetValidators(const Value: TValidatorCollection);
    procedure AppIdle(Sender: TObject; var Done: Boolean);

    procedure ShowImage(const HintLines: TStrings);
    procedure HideImage;

    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetHighlighted(const Value: Boolean);
    procedure SetShowAll(const Value: Boolean);

  protected

    // <ALIAS TlvkCustomValidationLabel.Validators>
    property Validators: TValidatorCollection read FValidators
      write SetValidators;

    { Description:
        Set this property to True to show all validation messages, instead
        of just the first one found.
    }
    property ShowAll: Boolean read FShowAll write SetShowAll default False;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Paint; override;

    property Highlighted: Boolean read FHighlighted write SetHighlighted;
    procedure Click; override;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnBeforeShow: TNotifyEvent read FOnBeforeShow write FOnBeforeShow;
    property OnAfterShow: TNotifyEvent read FOnAfterShow write FOnAfterShow;
    property OnBeforeHide: TNotifyEvent read FOnBeforeHide write FOnBeforeHide;
    property OnAfterHide: TNotifyEvent read FOnAfterHide write FOnAfterHide;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This control works like the TlvkValidationLabel control, except it
      will only show a triangle with an exclamation mark in it if there are
      validation messages. If no validation messages exists, the triangle
      will not show at all. The control has mouse-over effects and a tooltip
      that will show the validation messages.
  }
  TlvkValidationImage = class(TlvkCustomValidationImage)
  published
    // <ALIAS TlvkCustomValidationLabel.Validators>
    property Validators;
    // <ALIAS TlvkCustomValidationImage.ShowAll>
    property ShowAll;

    property OnBeforeShow;
    property OnAfterShow;
    property OnBeforeHide;
    property OnAfterHide;

    // Inherited from TCustomLabel
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Windows, Graphics;

type
  TValidatorCracker = class(TlvkCustomValidator);
  TControlValidatorCracker = class(TlvkCustomControlValidator);

var
  Images  : TImageList  = nil;

procedure LoadImages;
var
  Bitmap  : Graphics.TBitmap;
begin
  Images := TImageList.Create(nil);
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'ERROR_NORMAL');
    Images.Width := Bitmap.Width;
    Images.Height := Bitmap.Height;
    Images.AddMasked(Bitmap, Bitmap.Canvas.Pixels[0, Bitmap.Height-1]);

    Bitmap.LoadFromResourceName(HInstance, 'ERROR_HIGHLIGHTED');
    Images.AddMasked(Bitmap, Bitmap.Canvas.Pixels[0, Bitmap.Height-1]);
  finally
    Bitmap.Free;
  end;
end;

{ TlvkCustomValidationImage }

procedure TlvkCustomValidationImage.AppIdle(Sender: TObject;
  var Done: Boolean);
var
  Index     : Integer;
  Validator : TlvkCustomValidator;
  Messages  : TStrings;
begin
  if Assigned(FValidators) then
  begin
    FErrors.Clear;

    Messages := TStringList.Create;
    try
      for Index := 0 to FValidators.Count-1 do
      begin
        Validator := FValidators[Index].Validator;
        if Assigned(Validator) then
        begin
          if not Validator.Validate then
          begin
            FErrors.Add(Validator);
            if (Messages.Count = 0) or FShowAll then
              Messages.Add(TValidatorCracker(Validator).InvalidMessage);
          end;
        end;
      end;

      if Messages.Count > 0 then
        ShowImage(Messages)
      else
        HideImage;
    finally
      Messages.Free;
    end;
  end;
end;

procedure TlvkCustomValidationImage.Click;
var
  Done      : Boolean;
  Validator : TlvkCustomValidator;
  Index     : Integer;
begin
  inherited;

  AppIdle(Self, Done);
  for Index := 0 to FErrors.Count-1 do
  begin
    Validator := TlvkCustomValidator(FErrors[Index]);
    if Validator.SetFocus then
      Break;
  end;
end;

constructor TlvkCustomValidationImage.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FAppEvents := TApplicationEvents.Create(Self);
    FAppEvents.OnIdle := AppIdle;
    Visible := False;
  end;

  FValidators := TValidatorCollection.Create(Self);
  FErrors := TList.Create;

  ShowHint := True;
  Width := 15;
  Height := 15;
end;

destructor TlvkCustomValidationImage.Destroy;
begin
  FreeAndNil(FErrors);
  FreeAndNil(FAppEvents);
  FreeAndNil(FValidators);

  inherited;
end;

procedure TlvkCustomValidationImage.DoOnAfterHide;
begin
  if Assigned(FOnAfterHide) then
    FOnAfterHide(Self);
end;

procedure TlvkCustomValidationImage.DoOnAfterShow;
begin
  if Assigned(FOnAfterShow) then
    FOnAfterShow(Self);
end;

procedure TlvkCustomValidationImage.DoOnBeforeHide;
begin
  if Assigned(FOnBeforeHide) then
    FOnBeforeHide(Self);
end;

procedure TlvkCustomValidationImage.DoOnBeforeShow;
begin
  if Assigned(FOnBeforeShow) then
    FOnBeforeShow(Self);
end;

procedure TlvkCustomValidationImage.HideImage;
begin
  if Visible then
  begin
    DoOnBeforeHide;
    Hide;
    DoOnAfterHide;
  end;
end;

procedure TlvkCustomValidationImage.MouseEnter(var Msg: TMessage);
begin
  Highlighted := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TlvkCustomValidationImage.MouseLeave(var Msg: TMessage);
begin
  Highlighted := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TlvkCustomValidationImage.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index : Integer;
begin
  inherited;

  if Assigned(FValidators) then
  begin
    Index := 0;
    while Index < FValidators.Count do
    begin
      if FValidators[Index].Validator = AComponent then
        FValidators.Delete(Index)
      else
        Inc(Index);
    end;
  end;
end;

procedure TlvkCustomValidationImage.Paint;
var
  Index : Integer;
begin
  if csDesigning in ComponentState then
    Index := 0
  else
    Index := Ord(FHighlighted);

  Images.Draw(Canvas, (Width - Images.Width) div 2,
    (Height - Images.Height) div 2, Index, True);
end;

procedure TlvkCustomValidationImage.SetHighlighted(const Value: Boolean);
begin
  if FHighlighted <> Value then
  begin
    FHighlighted := Value;

    if Visible then
      Invalidate;
  end;
end;

procedure TlvkCustomValidationImage.SetShowAll(const Value: Boolean);
begin
  if FShowAll <> Value then
  begin
    FShowAll := Value;
    Invalidate;
  end;
end;

procedure TlvkCustomValidationImage.SetValidators(
  const Value: TValidatorCollection);
begin
  FValidators.Assign(Value);
end;

procedure TlvkCustomValidationImage.ShowImage(const HintLines: TStrings);
var
  s : string;
begin
  s := HintLines.Text;
  while (s <> '') and (s[Length(s)] in [#13, #10]) do
    Delete(s, Length(s), 1);

  if Hint <> s then
    Hint := s;

  if not Visible then
  begin
    DoOnBeforeShow;
    Show;
    DoOnAfterShow;
  end;
end;

initialization
  LoadImages;
finalization
  FreeAndNil(Images);
end.
