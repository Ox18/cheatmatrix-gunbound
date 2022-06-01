{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the drop down edit control.
}
unit lvkPanelDropDownEdit;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:51 $
// $Revision: 2 $
// $Archive: /Components/LVK/source/lvkPanelDropDownEdit.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, Graphics, Dialogs, Forms, ExtCtrls, lvkSpeedEdit,
  lvkSizeGrip, lvkPopupWindow, Messages;

const
  CM_HIDEDROPDOWN = WM_USER + 1;

type
  TlvkCustomPanelDropDownEdit = class;
  TPanelDropDownNotifyEvent = procedure(Sender: TObject;
    const Panel: TCustomPanel) of object;
  TPanelPositionSizeGripEvent = procedure(Sender: TObject;
    const SizeGrip: TlvkSizeGrip; const Panel: TCustomPanel) of object;

  TlvkPanelDropDownEditOption = (deoCloseOnEsc, deoCloseOnEnter);
  TlvkPanelDropDownEditOptions = set of TlvkPanelDropDownEditOption;
  { Description:
      This component implements a combobox-like component, but instead of
      dropping down a list of items, it will drop down a panel so you can
      drop down anything you want.
  }
  TlvkCustomPanelDropDownEdit = class(TlvkCustomSpeedEdit)
  private
    FDropDownPanel                : TCustomPanel;
    FOnBeforeDropDown             : TNotifyEvent;
    FOnAfterDropDown              : TPanelDropDownNotifyEvent;

    FOnBeforeCloseUp              : TPanelDropDownNotifyEvent;
    FOnAfterCloseUp               : TNotifyEvent;

    FOnPositionSizeGrip           : TPanelPositionSizeGripEvent;

    FDropDownForm                 : TlvkCustomPopupWindow;
    FOriginalHeight               : Integer;
    FOriginalWidth                : Integer;
    FOriginalParent               : TWinControl;

    FDropDownSizeGrip             : TlvkSizeGrip;
    FSizeGrip                     : Boolean;
    FAutoConfigurePanel           : Boolean;
    FAutoWidth                    : Boolean;
    FAutoDropDown                 : Boolean;
    FSavedCursor                  : TCursor;
    FOptions                      : TlvkPanelDropDownEditOptions;

    FPreventClose                 : Boolean;
    FDropDownFramed               : Boolean;

    FOnShortCut                   : TShortCutEvent;

    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure HookOwnerForm;
    procedure UnhookOwnerForm;

    procedure DoBeforeDropDown;
    procedure DoAfterDropDown(const Panel: TCustomPanel);
    procedure DoBeforeCloseUp(const Panel: TCustomPanel);
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
    procedure SetDropDownPanel(const Value: TCustomPanel);

  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    { Description:
        This property controls wether the dropdown box is framed or not.
    }
    property DropDownFramed: Boolean read FDropDownFramed write FDropDownFramed
      default True;

    { Description:
        This property decides which panel to use when dropping down then
        edit box.
    }
    property DropDownPanel: TCustomPanel read FDropDownPanel write
      SetDropDownPanel;

    { Description:
        This event will be called just before the dropdown box is created.
    }
    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown
      write FOnBeforeDropDown;

    { Description:
        This event will be called just after the dropdown box has been
        shown.
    }
    property OnAfterDropDown: TPanelDropDownNotifyEvent read FOnAfterDropDown
      write FOnAfterDropDown;

    { Description:
        This event will be called just before the dropdown box rolls back up.
        You can use this to read data from the shown panel and configure
        the application accordingly.
    }
    property OnBeforeCloseUp: TPanelDropDownNotifyEvent read FOnBeforeCloseUp
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
        other controls placed on the panel.
      See also:
        SizeGrip
    }
    property OnPositionSizeGrip: TPanelPositionSizeGripEvent
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
        This property controls wether the panel used will be configured by the
        dropdown edit component or not. Configuring the panel will do the
        following:

        - Remove scrollbars if a sizegrip will be added
    }
    property AutoConfigurePanel: Boolean read FAutoConfigurePanel
      write FAutoConfigurePanel default True;

    { Description:
        Leave this property to True to have the dropdown form/panel
        automatically resize itself to be the same width as the edit
        control. Set it to False to have the form/panel use the preconfigured
        width of the panel.
    }
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;

    { Description:
        Set this property to True to have the control drop down the panel
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
    property Options: TlvkPanelDropDownEditOptions read FOptions write FOptions;

    procedure DoEnter; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Description:
        This property controls wether the dropdown can be closed up or not.
        Typically used when a dropdown panel shows secondary forms and
        want the dropdown to stay in place.
    }
    property PreventClose: Boolean read FPreventClose write FPreventClose;

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

  // <ALIAS TlvkCustomPanelDropDownEdit>
  TlvkPanelDropDownEdit = class(TlvkCustomPanelDropDownEdit)
  public
    // <ALIAS TlvkCustomPanelDropDownEdit.DroppedDown>
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

    // From TlvkCustomPanelDropDownEdit
    // <ALIAS TlvkCustomPanelDropDownEdit.Options>
    property Options;
    // <ALIAS TlvkCustomPanelDropDownEdit.OnBeforeDropDown>
    property OnBeforeDropDown;
    // <ALIAS TlvkCustomPanelDropDownEdit.OnAfterDropDown>
    property OnAfterDropDown;
    // <ALIAS TlvkCustomPanelDropDownEdit.OnBeforeCloseUp>
    property OnBeforeCloseUp;
    // <ALIAS TlvkCustomPanelDropDownEdit.OnAfterCloseUp>
    property OnAfterCloseUp;
    // <ALIAS TlvkCustomPanelDropDownEdit.OnPositionSizeGrip>
    property OnPositionSizeGrip;
    // <ALIAS TlvkCustomPanelDropDownEdit.SizeGrip>
    property SizeGrip;
    // <ALIAS TlvkCustomPanelDropDownEdit.AutoConfigurePanel>
    property AutoConfigurePanel;
    // <ALIAS TlvkCustomPanelDropDownEdit.AutoWidth>
    property AutoWidth;
    // <ALIAS TlvkCustomPanelDropDownEdit.AutoDropDown>
    property AutoDropDown;
    // <ALIAS TlvkCustomPanelDropDownEdit.DropDownPanel>
    property DropDownPanel;
    // <ALIAS TlvkCustomPanelDropDownEdit.DropDownFramed>
    property DropDownFramed;
  end;

function GetOwnerForm(const Component: TComponent): TForm;

implementation

uses
  Windows;

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

{ TlvkCustomPanelDropDownEdit }

procedure TlvkCustomPanelDropDownEdit.CMHideDropDown(var Msg: TMessage);
begin
  //SetFocus;
  if DroppedDown then
    HideDropDown;
  SpeedButton.Enabled := Enabled;
  Cursor := FSavedCursor;
end;

constructor TlvkCustomPanelDropDownEdit.Create(AOwner: TComponent);
begin
  inherited;

  FDropDownFramed := True;
  FAutoWidth := True;
  Button.Kind := lbkComboBoxLookalike;
  OnButtonClick := DropDownClick;
  SpeedButton.GroupIndex := 1;
  SpeedButton.AllowAllUp := True;
  FSizeGrip := False;
  FAutoConfigurePanel := True;
  FOptions := [deoCloseOnEsc, deoCloseOnEnter];
end;

destructor TlvkCustomPanelDropDownEdit.Destroy;
begin
  UnhookOwnerForm;
  if DroppedDown then
    HideDropDown;
  inherited;
end;

procedure TlvkCustomPanelDropDownEdit.DoAfterDropDown(const Panel: TCustomPanel);
begin
  if Assigned(FOnAfterDropDown) then
    FOnAfterDropDown(Self, Panel);
end;

procedure TlvkCustomPanelDropDownEdit.DoAfterCloseUp;
begin
  if Assigned(FOnAfterCloseUp) then
    FOnAfterCloseUp(Self);
end;

procedure TlvkCustomPanelDropDownEdit.DoBeforeDropDown;
begin
  if Assigned(FOnBeforeDropDown) then
    FOnBeforeDropDown(Self);
end;

procedure TlvkCustomPanelDropDownEdit.DoBeforeCloseUp(const Panel: TCustomPanel);
begin
  if Assigned(FOnBeforeCloseUp) then
    FOnBeforeCloseUp(Self, Panel);
end;

procedure TlvkCustomPanelDropDownEdit.DoPositionSizeGrip;
begin
  if Assigned(FOnPositionSizeGrip) then
    FOnPositionSizeGrip(Self, FDropDownSizeGrip, FDropDownPanel)
  else begin
    FDropDownSizeGrip.Parent := FDropDownPanel;
    FDropDownSizeGrip.Align := alNone;
    FDropDownSizeGrip.SetBounds(FDropDownPanel.Width - 13,
      FDropDownPanel.Height - 13, 13, 13);
  end;
end;

procedure TlvkCustomPanelDropDownEdit.DropDown;
begin
  DroppedDown := True;
end;

procedure TlvkCustomPanelDropDownEdit.DropDownClick(Sender: TObject);
begin
  if SpeedButton.Down then
    ShowDropDown
  else
    HideDropDown;
end;

function TlvkCustomPanelDropDownEdit.GetDroppedDown: Boolean;
begin
  Result := Assigned(FDropDownForm);
end;

procedure TlvkCustomPanelDropDownEdit.HideDropDown;
begin
  UnhookOwnerForm;
  //SetFocus;
  DoBeforeCloseUp(FDropDownPanel);
  FreeAndNil(FDropDownSizeGrip);

  if Assigned(FDropDownPanel) then
  begin
    FDropDownPanel.Align := alNone;
    FDropDownPanel.Width := FOriginalWidth;
    FDropDownPanel.Height := FOriginalHeight;
    FDropDownPanel.Parent := FOriginalParent;
    FDropDownPanel.Visible := False;
  end;
  FreeAndNil(FDropDownForm);
  SpeedButton.Down := False;
  DoAfterCloseUp;
end;

procedure TlvkCustomPanelDropDownEdit.KeyDown(var Key: Word;
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

procedure TlvkCustomPanelDropDownEdit.ResizeDropDownForm(Sender: TObject);
begin
  if Assigned(FDropDownSizeGrip) then
    DoPositionSizeGrip;
end;

procedure TlvkCustomPanelDropDownEdit.CloseUp;
begin
  SetFocus;
  DroppedDown := False;
end;

procedure TlvkCustomPanelDropDownEdit.SetDroppedDown(const Value: Boolean);
begin
  if Value <> GetDroppedDown then
    if Value then
      ShowDropDown
    else
      PostMessage(Handle, CM_HIDEDROPDOWN, 0, 0);
end;

procedure TlvkCustomPanelDropDownEdit.ShowDropDown;
var
  DropDownPoint : TPoint;
  TooHigh       : Boolean;
  WorkingArea   : TRect;
  UseWidth      : Integer;
  OriginalWidth : Integer;
begin
  if DroppedDown then
    HideDropDown;

  DoBeforeDropDown;
  if Assigned(FDropDownPanel) then
  begin                               
    //SetFocus;

    if FAutoConfigurePanel then
    begin
      if FSizeGrip then
        FDropDownPanel.DoubleBuffered := True;
    end;

    // Create and configure the popup window
    FDropDownForm := TlvkDropDownPopup.Create(Self);
    FDropDownForm.Parent := Self;
    FDropDownForm.DoubleBuffered := True;
    TlvkDropDownPopup(FDropDownForm).Framed := FDropDownFramed;
    TlvkDropDownPopup(FDropDownForm).OnCloseUp := DropDownCloseUp;
    TlvkDropDownPopup(FDropDownForm).OnResize := ResizeDropDownForm;

    if FAutoWidth then
    begin
      UseWidth := Width;
      if UseWidth < FDropDownPanel.Width + 2 then
        UseWidth := FDropDownPanel.Width + 2;
    end else
      UseWidth := FDropDownPanel.Width + 2;

    DropDownPoint := Parent.ClientToScreen(Point(Left, Top + Height));

    // Configure the panel
    FOriginalParent := FDropDownPanel.Parent;
    FDropDownPanel.Parent := FDropDownForm;

    // Finalize form
    if SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkingArea, 0) then
      TooHigh := (DropDownPoint.Y + FDropDownPanel.Height + 2 > WorkingArea.Bottom)
    else
      TooHigh := (DropDownPoint.Y + FDropDownPanel.Height + 2 > Screen.Height);

    OriginalWidth := FDropDownPanel.Width;
    if TooHigh then
    begin
      DropDownPoint := Parent.ClientToScreen(Point(Left, Top - FDropDownPanel.Height - 2));
      FDropDownForm.Place(DropDownPoint.X, DropDownPoint.Y,
        UseWidth, FDropDownPanel.Height + 2);
    end else
      FDropDownForm.Place(DropDownPoint.X, DropDownPoint.Y,
        UseWidth, FDropDownPanel.Height + 2);

    FOriginalHeight := FDropDownPanel.Height;
    FOriginalWidth := OriginalWidth;
    FDropDownPanel.Align := alClient;
    FDropDownPanel.SetBounds(0, 0, FDropDownForm.ClientWidth, FDropDownForm.ClientHeight);
    FDropDownPanel.Visible := True;

    FDropDownForm.Constraints.MinHeight := FDropDownPanel.Height+2;
    FDropDownForm.Constraints.MinWidth := OriginalWidth;

    if FSizeGrip then
    begin
      FDropDownSizeGrip := TlvkSizeGrip.Create(FDropDownForm);
      DoPositionSizeGrip;
      TlvkDropDownPopup(FDropDownForm).HasSizeGrip := True;
    end;

    // Transfer focus to popup window
    SpeedButton.Down := True;
    SelectNext(FDropDownPanel, True, True);

    // Events
    DoAfterDropDown(FDropDownPanel);
    HookOwnerForm;
  end else
    SpeedButton.Down := False;
end;

procedure TlvkCustomPanelDropDownEdit.DropDownCloseUp(Sender: TObject);
begin
  if FPreventClose then
    Abort;
    
  SpeedButton.Enabled := False;
  FSavedCursor := Cursor;
  Cursor := crArrow;
  Parent.Invalidate;
  PostMessage(Handle, CM_HIDEDROPDOWN, 0, 0);
end;

procedure TlvkCustomPanelDropDownEdit.DoEnter;
begin
  inherited;
  if FAutoDropDown then
    DroppedDown := True;
end;

procedure TlvkCustomPanelDropDownEdit.HookOwnerForm;
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

procedure TlvkCustomPanelDropDownEdit.UnhookOwnerForm;
var
  OwnerForm : TForm;
begin
  OwnerForm := GetOwnerForm(Self);
  if Assigned(OwnerForm) and (not (csDestroying in ComponentState)) then
    OwnerForm.OnShortCut := FOnShortCut;
end;

procedure TlvkCustomPanelDropDownEdit.FormShortCut(var Msg: TWMKey;
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

procedure TlvkCustomPanelDropDownEdit.SetDropDownPanel(
  const Value: TCustomPanel);
begin
  if DroppedDown then
    CloseUp;

  if Assigned(FDropDownPanel) and (not (csDesigning in ComponentState)) then
    FDropDownPanel.Visible := True;
  FDropDownPanel := Value;
  if Assigned(FDropDownPanel) and (not (csDesigning in ComponentState)) then
    FDropDownPanel.Visible := False;
end;

procedure TlvkCustomPanelDropDownEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FDropDownPanel then
    begin
      if DroppedDown then
        CloseUp;
      FDropDownPanel := nil;
    end;
  end;
end;

end.
