{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the drop down edit control.
}
unit lvkDropDownEdit;

// $Author: Lasse V. Karlsen $
// $Revision: 12 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkDropDownEdit.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, Graphics, Dialogs, Forms, lvkSpeedEdit,
  lvkSizeGrip, lvkPopupWindow, Messages;

const
  CM_HIDEDROPDOWN = WM_USER + 1;

type
  TlvkCustomDropDownEdit = class;
  TCreateDropDownFrameEvent = procedure(Sender: TObject;
    const AOwner: TComponent; out Frame: TFrame;
    var EditOwnsFrame: Boolean) of object;
  TDropDownNotifyEvent = procedure(Sender: TObject;
    const Frame: TFrame) of object;
  TPositionSizeGripEvent = procedure(Sender: TObject;
    const SizeGrip: TlvkSizeGrip; const Frame: TFrame) of object;

  TlvkDropDownEditOption = (deoCloseOnEsc, deoCloseOnEnter);
  TlvkDropDownEditOptions = set of TlvkDropDownEditOption;
  { Description:
      This component implements a combobox-like component, but instead of
      dropping down a list of items, it will drop down a frame so you can
      drop down anything you want.

      Note: This component requires you to override the OnCreateDropDownFrame
        event, otherwise nothing will drop down.
  }
  TlvkCustomDropDownEdit = class(TlvkCustomSpeedEdit)
  private
    FOnBeforeDropDown             : TNotifyEvent;
    FOnCreateDropDownFrame        : TCreateDropDownFrameEvent;
    FOnAfterDropDown              : TDropDownNotifyEvent;

    FOnBeforeCloseUp              : TDropDownNotifyEvent;
    FOnAfterCloseUp               : TNotifyEvent;

    FOnPositionSizeGrip           : TPositionSizeGripEvent;

    FDropDownForm                 : TlvkCustomPopupWindow;
    FDropDownFrame                : TFrame;
    FOwnsFrame                    : Boolean;
    FOriginalHeight               : Integer;
    FOriginalWidth                : Integer;

    FDropDownSizeGrip             : TlvkSizeGrip;
    FSizeGrip                     : Boolean;
    FAutoConfigureFrame           : Boolean;
    FAutoWidth                    : Boolean;
    FAutoDropDown                 : Boolean;
    FSavedCursor                  : TCursor;
    FOptions                      : TlvkDropDownEditOptions;

    FOnShortCut                   : TShortCutEvent;

    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure HookOwnerForm;
    procedure UnhookOwnerForm;

    procedure DoBeforeDropDown;
    procedure DoCreateDropDownFrame(out Frame: TFrame);
    procedure DoAfterDropDown(const Frame: TFrame);
    procedure DoBeforeCloseUp(const Frame: TFrame);
    procedure DoPositionSizeGrip;
    procedure DoAfterCloseUp;
    procedure ShowDropDown;
    procedure HideDropDown;
    procedure DropDownCloseUp(Sender: TObject);
    procedure ResizeDropDownForm(Sender: TObject);
    procedure DropDownClick(Sender: TObject);
    procedure CMHideDropDown(var Msg: TMessage); message CM_HIDEDROPDOWN;
    function GetDroppedDown: Boolean;
    procedure SetDroppedDown(const Value: Boolean);

  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    { Description:
        This event will be called just before the dropdown box is created.
    }
    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown
      write FOnBeforeDropDown;

    { Description:
        This event handler will be called once to create the frame to show in
        the dropdown box. If this event is not set, or it returns the frame
        parameter as nil, no dropdown will show.

        Set the owner of the frame to the specified owner.
    }
    property OnCreateDropDownFrame: TCreateDropDownFrameEvent
      read FOnCreateDropDownFrame write FOnCreateDropDownFrame;

    { Description:
        This event will be called just after the dropdown box has been
        shown.
    }
    property OnAfterDropDown: TDropDownNotifyEvent read FOnAfterDropDown
      write FOnAfterDropDown;

    { Description:
        This event will be called just before the dropdown box rolls back up.
        You can use this to read data from the shown frame and configure
        the application accordingly.
    }
    property OnBeforeCloseUp: TDropDownNotifyEvent read FOnBeforeCloseUp
      write FOnBeforeCloseUp;

    { Description:
        This event will be called just after the dropdown box has rolled up
        and been destroyed.
    }
    property OnAfterCloseUp: TNotifyEvent read FOnAfterCloseUp
      write FOnAfterCloseUp;

    { Description:
        Override this event if you want the size grip to show in a different
        spot. The default spot is the lower right corner of the dropdown box.

        You can use this event to place the sizegrip inside other controls,
        such as in the lower right corner of a TListBox. If the sizegrip
        is placed on the dropdown box itself, it will be shown behind all
        other controls placed on the frame.
      See also:
        SizeGrip
    }
    property OnPositionSizeGrip: TPositionSizeGripEvent
      read FOnPositionSizeGrip write FOnPositionSizeGrip;

    { Description:
        This property returns the current state for the dropdown control.
        You can set this property to control the state.
      See also:
        DropDown, CloseUp
    }
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;

    { Description:
        Set this property to True to get a sizegrip in the lower right corner
        of the dropdown box. Leave it to False to have no such grip. A dropdown
        box without a sizegrip cannot be resized by normal means.
      See also:
        OnPositionSizeGrip
    }
    property SizeGrip: Boolean read FSizeGrip write FSizeGrip default False;

    { Description:
        This property controls wether the frame used will be configured by the
        dropdown edit component or not. Configuring the frame will do the
        following:

        - Remove scrollbars if a sizegrip will be added
    }
    property AutoConfigureFrame: Boolean read FAutoConfigureFrame
      write FAutoConfigureFrame default True;

    { Description:
        Leave this property to True to have the dropdown form/frame
        automatically resize itself to be the same width as the edit
        control. Set it to False to have the form/frame use the preconfigured
        width of the frame.
    }
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;

    { Description:
        Set this property to True to have the control drop down the frame
        immediately when it gets focus. Leave it at False to have the user
        manually click the button to drop it down.
    }
    property AutoDropDown: Boolean read FAutoDropDown write FAutoDropDown
      default False;

    { Description:
        This property configures the dropdown on how the user can close the
        dropdown window. It's a set, which can contain none, one or both of
        the following two values:

          * deoCloseOnEsc - If user presses the Esc key, close the window.
          * deoCloseOnEnter - If user presses the Enter key, close the
            window.
    }
    property Options: TlvkDropDownEditOptions read FOptions write FOptions;

    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        This method does the same as setting the DroppedDown property to True.
      See also:
        DroppedDown, CloseUp
    }
    procedure DropDown;

    { Description:
        This method does the same as setting the DroppedDown property to False.
      See also:
        DroppedDown, DropDown
    }
    procedure CloseUp;
  end;

  // <ALIAS TlvkCustomDropDownEdit>
  TlvkDropDownEdit = class(TlvkCustomDropDownEdit)
  public
    // <ALIAS TlvkCustomDropDownEdit.DroppedDown>
    property DroppedDown;
  published
    // From TEdit
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    // <ALIAS TlvkCustomEdit.Color>
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
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

    // From TlvkCustomEdit
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment;
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.ReadOnlyColor>
    property ReadOnlyColor;

    // From TlvkCustomSpeedEdit
    // <ALIAS TlvkCustomSpeedEdit.Button>
    property Button;

    // From TlvkCustomDropDownEdit
    // <ALIAS TlvkCustomDropDownEdit.Options>
    property Options;
    // <ALIAS TlvkCustomDropDownEdit.OnBeforeDropDown>
    property OnBeforeDropDown;
    // <ALIAS TlvkCustomDropDownEdit.OnCreateDropDownFrame>
    property OnCreateDropDownFrame;
    // <ALIAS TlvkCustomDropDownEdit.OnAfterDropDown>
    property OnAfterDropDown;
    // <ALIAS TlvkCustomDropDownEdit.OnBeforeCloseUp>
    property OnBeforeCloseUp;
    // <ALIAS TlvkCustomDropDownEdit.OnAfterCloseUp>
    property OnAfterCloseUp;
    // <ALIAS TlvkCustomDropDownEdit.OnPositionSizeGrip>
    property OnPositionSizeGrip;
    // <ALIAS TlvkCustomDropDownEdit.SizeGrip>
    property SizeGrip;
    // <ALIAS TlvkCustomDropDownEdit.AutoConfigureFrame>
    property AutoConfigureFrame;
    // <ALIAS TlvkCustomDropDownEdit.AutoWidth>
    property AutoWidth;
    // <ALIAS TlvkCustomDropDownEdit.AutoDropDown>
    property AutoDropDown;
  end;

function GetOwnerForm(const Component: TComponent): TForm;

implementation

uses
  Windows, ExtCtrls;

type
  TlvkDropDownPopup = class(TlvkCustomPopupWindow)
  published
    property OnCloseUp;
    property OnResize;
    property HasSizeGrip;
  end;

function GetOwnerForm(const Component: TComponent): TForm;
var
  Owner : TComponent;
begin
  if Assigned(Component) then
  begin
    Owner := Component.Owner;
    while Assigned(Owner) and (not (Owner is TForm)) do
      Owner := Owner.Owner;

    if Assigned(Owner) then
      Result := TForm(Owner)
    else
      Result := nil;
  end else
    Result := nil;
end;

{ TlvkCustomDropDownEdit }

procedure TlvkCustomDropDownEdit.CMHideDropDown(var Msg: TMessage);
begin
  if Assigned(FDropDownFrame) then
    HideDropDown;
  SpeedButton.Enabled := Enabled;
  Cursor := FSavedCursor;
end;

constructor TlvkCustomDropDownEdit.Create(AOwner: TComponent);
begin
  inherited;

  FAutoWidth := True;
  Button.Kind := lbkComboBoxLookalike;
  OnButtonClick := DropDownClick;
  SpeedButton.GroupIndex := 1;
  SpeedButton.AllowAllUp := True;
  FSizeGrip := False;
  FAutoConfigureFrame := True;
  FOptions := [deoCloseOnEsc, deoCloseOnEnter];
end;

destructor TlvkCustomDropDownEdit.Destroy;
begin
  UnhookOwnerForm;
  if Assigned(FDropDownFrame) then
    HideDropDown;
  inherited;
end;

procedure TlvkCustomDropDownEdit.DoAfterDropDown(const Frame: TFrame);
begin
  if Assigned(FOnAfterDropDown) then
    FOnAfterDropDown(Self, Frame);
end;

procedure TlvkCustomDropDownEdit.DoAfterCloseUp;
begin
  if Assigned(FOnAfterCloseUp) then
    FOnAfterCloseUp(Self);
end;

procedure TlvkCustomDropDownEdit.DoBeforeDropDown;
begin
  if Assigned(FOnBeforeDropDown) then
    FOnBeforeDropDown(Self);
end;

procedure TlvkCustomDropDownEdit.DoBeforeCloseUp(const Frame: TFrame);
begin
  if Assigned(FOnBeforeCloseUp) then
    FOnBeforeCloseUp(Self, Frame);
end;

procedure TlvkCustomDropDownEdit.DoCreateDropDownFrame(out Frame: TFrame);
begin
  if Assigned(FOnCreateDropDownFrame) then
  begin
    FOwnsFrame := True;
    FOnCreateDropDownFrame(Self, Self, Frame, FOwnsFrame);
  end else begin
    Frame := nil;
    FOwnsFrame := False;
  end;
end;

procedure TlvkCustomDropDownEdit.DoPositionSizeGrip;
begin
  if Assigned(FOnPositionSizeGrip) then
    FOnPositionSizeGrip(Self, FDropDownSizeGrip, FDropDownFrame)
  else begin
    FDropDownSizeGrip.Parent := FDropDownFrame;
    FDropDownSizeGrip.Align := alNone;
    FDropDownSizeGrip.SetBounds(FDropDownFrame.Width - 13,
      FDropDownFrame.Height - 13, 13, 13);
  end;
end;

procedure TlvkCustomDropDownEdit.DropDown;
begin
  DroppedDown := True;
end;

procedure TlvkCustomDropDownEdit.DropDownClick(Sender: TObject);
begin
  if SpeedButton.Down then
    ShowDropDown
  else
    HideDropDown;
end;

function TlvkCustomDropDownEdit.GetDroppedDown: Boolean;
begin
  Result := Assigned(FDropDownFrame);
end;

procedure TlvkCustomDropDownEdit.HideDropDown;
begin
  UnhookOwnerForm;
  DoBeforeCloseUp(FDropDownFrame);
  FreeAndNil(FDropDownSizeGrip);
  if FOwnsFrame then
    FreeAndNil(FDropDownFrame)
  else if Assigned(FDropDownFrame) then
  begin
    FDropDownFrame.Width := FOriginalWidth;
    FDropDownFrame.Height := FOriginalHeight;
    FDropDownFrame.Parent := nil;
  end;
  FreeAndNil(FDropDownForm);
  SpeedButton.Down := False;
  DoAfterCloseUp;
end;

procedure TlvkCustomDropDownEdit.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  Msg : TMsg;
begin
  if (Key = VK_DOWN) and (ssAlt in Shift) then
  begin
    SpeedButton.Down := True;
    SpeedButton.Click;
    PeekMessage(Msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
  end else
    inherited KeyDown(Key, Shift);
end;

procedure TlvkCustomDropDownEdit.ResizeDropDownForm(Sender: TObject);
begin
  if Assigned(FDropDownSizeGrip) then
    DoPositionSizeGrip;
end;

procedure TlvkCustomDropDownEdit.CloseUp;
begin
  SetFocus;
  DroppedDown := False;
end;

procedure TlvkCustomDropDownEdit.SetDroppedDown(const Value: Boolean);
begin
  if Value <> GetDroppedDown then
    if Value then
      ShowDropDown
    else
      PostMessage(Handle, CM_HIDEDROPDOWN, 0, 0);
end;

procedure TlvkCustomDropDownEdit.ShowDropDown;
var
  DropDownPoint : TPoint;
  TooHigh       : Boolean;
  WorkingArea   : TRect;
  UseWidth      : Integer;
  OriginalWidth : Integer;
begin
  if Assigned(FDropDownFrame) then
    HideDropDown;

  DoBeforeDropDown;
  DoCreateDropDownFrame(FDropDownFrame);
  if Assigned(FDropDownFrame) then
  begin
    SetFocus;

    if FAutoConfigureFrame then
    begin
      if FSizeGrip then
      begin
        FDropDownFrame.HorzScrollBar.Visible := False;
        FDropDownFrame.VertScrollBar.Visible := False;
        FDropDownFrame.DoubleBuffered := True;
      end;
    end;

    // Create and configure the popup window
    FDropDownForm := TlvkDropDownPopup.Create(Self);
    FDropDownForm.Parent := Self;
    FDropDownForm.DoubleBuffered := True;
    TlvkDropDownPopup(FDropDownForm).OnCloseUp := DropDownCloseUp;
    TlvkDropDownPopup(FDropDownForm).OnResize := ResizeDropDownForm;

    if FAutoWidth then
    begin
      UseWidth := Width;
      if UseWidth < FDropDownFrame.Width + 2 then
        UseWidth := FDropDownFrame.Width + 2;
    end else
      UseWidth := FDropDownFrame.Width + 2;

    DropDownPoint := Parent.ClientToScreen(Point(Left, Top + Height));

    // Configure the frame
    FDropDownFrame.Parent := FDropDownForm;
    FDropDownFrame.Align := alClient;

    // Finalize form
    if SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkingArea, 0) then
      TooHigh := (DropDownPoint.Y + FDropDownFrame.Height + 2 > WorkingArea.Bottom)
    else
      TooHigh := (DropDownPoint.Y + FDropDownFrame.Height + 2 > Screen.Height);

    OriginalWidth := FDropDownFrame.Width + 2;
    if TooHigh then
    begin
      DropDownPoint := Parent.ClientToScreen(Point(Left, Top - FDropDownFrame.Height - 2));
      FDropDownForm.Place(DropDownPoint.X, DropDownPoint.Y,
        UseWidth, FDropDownFrame.Height + 2);
    end else
      FDropDownForm.Place(DropDownPoint.X, DropDownPoint.Y,
        UseWidth, FDropDownFrame.Height + 2);

    FOriginalHeight := FDropDownFrame.Height;
    FOriginalWidth := OriginalWidth;

    FDropDownForm.Constraints.MinHeight := FDropDownFrame.Height+2;
    FDropDownForm.Constraints.MinWidth := OriginalWidth;

    if FSizeGrip then
    begin
      FDropDownSizeGrip := TlvkSizeGrip.Create(FDropDownForm);
      DoPositionSizeGrip;
      TlvkDropDownPopup(FDropDownForm).HasSizeGrip := True;
    end;

    // Transfer focus to popup window
    SpeedButton.Down := True;
    SelectNext(FDropDownFrame, True, True);

    // Events
    DoAfterDropDown(FDropDownFrame);
    HookOwnerForm;
  end else
    SpeedButton.Down := False;
end;

procedure TlvkCustomDropDownEdit.DropDownCloseUp(Sender: TObject);
begin
  SpeedButton.Enabled := False;
  FSavedCursor := Cursor;
  Cursor := crArrow;
  PostMessage(Handle, CM_HIDEDROPDOWN, 0, 0);
end;

procedure TlvkCustomDropDownEdit.DoEnter;
begin
  inherited;
  if FAutoDropDown then
    DroppedDown := True;
end;

procedure TlvkCustomDropDownEdit.HookOwnerForm;
var
  OwnerForm : TForm;
begin
  OwnerForm := GetOwnerForm(Self);
  if Assigned(OwnerForm) then
  begin
    FOnShortCut := OwnerForm.OnShortCut;
    OwnerForm.OnShortCut := FormShortCut;
  end;
end;

procedure TlvkCustomDropDownEdit.UnhookOwnerForm;
var
  OwnerForm : TForm;
begin
  OwnerForm := GetOwnerForm(Self);
  if Assigned(OwnerForm) and (not (csDestroying in ComponentState)) then
    OwnerForm.OnShortCut := FOnShortCut;
end;

procedure TlvkCustomDropDownEdit.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
var
  State : TShiftState;
begin
  State := KeyDataToShiftState(Msg.KeyData);

  if (Msg.CharCode = VK_DOWN) and (State = [ssAlt]) then
  begin
    Handled := True;
    CloseUp;
  end else if (Msg.CharCode = VK_RETURN) and (State = []) then
  begin
    if deoCloseOnEnter in FOptions then
    begin
      Handled := True;
      CloseUp;
    end;
  end else if (Msg.CharCode = VK_ESCAPE) and (State = []) then
  begin
    if deoCloseOnEsc in FOptions then
    begin
      Handled := True;
      CloseUp;
    end;
  end;

  if (not Handled) and Assigned(FOnShortCut) then
    FOnShortCut(Msg, Handled);
end;

end.
