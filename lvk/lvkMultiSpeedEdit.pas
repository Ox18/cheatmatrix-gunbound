{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This file contains a component for producing a edit-like component with
    buttons at both ends.
}
unit lvkMultiSpeedEdit;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkMultiSpeedEdit.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

{$R lvkMultiSpeedEdit.RES}

uses
  Classes, Controls, Menus, Windows, Graphics, StdCtrls, Buttons, ExtCtrls,
  Messages, Forms,
  lvkSpeedEdit, lvkDropDownEdit, lvkSpeedEditGlyphs, lvkEdits,
  lvkPopupWindow, lvkSizeGrip;

type
  TButtonSide = (bsLeft, bsRight);
  TButtonType = (btClick, btRepeatClick, btSpin, btDropDown);

const
  iVisible  = 0;
  iHint     = 1;

  CM_CREATEBUTTONS  = WM_USER + 123;

  DEFAULT_REPEAT_DELAY    = 250;
  DEFAULT_REPEAT_INTERVAL = 25;
  DEFAULT_BUTTON_SIDE     = bsRight;
  DEFAULT_SIZE_GRIP       = False;

type
  TlvkCustomMultiSpeedEdit = class;

  { Description:
      This component is used as the spin button for spin-edits and similar
      controls. You can also use it as a separate button control if you want.
  }
  TlvkSpinButton = class(TWinControl)
  private
    FSpinUp         : TSpeedButton;
    FSpinDown       : TSpeedButton;
    FActiveButton   : TSpeedButton;
    FRepeatTimer    : TTimer;
    FRepeatDelay    : Word;
    FRepeatInterval : Word;

    FOnUpClick      : TNotifyEvent;
    FOnDownClick    : TNotifyEvent;

    procedure DoUpClick;
    procedure DoDownClick;

    procedure ButtonRepeatClick(Sender: TObject);
    procedure ButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    function GetTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);

  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure AdjustButtons; virtual;

  public
    constructor Create(AOwner: TComponent); override;

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure SetFocus; override;

  published
    { Description:
        This event handler is called once when the user clicks on the up
        button, and if the user holds down the button, it will be called
        repeatedly until the user releases the button.
      See also:
        OnDownClick
    }
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;

    { Description:
        This event handler is called once when the user clicks on the down
        button, and if the user holds down the button, it will be called
        repeatedly until the user releases the button.
      See also:
        OnUpClick
    }
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;

    { Description:
        This property controls how long you have to hold down one of the buttons
        before it will start repeating. The delay is given in milliseconds,
        and the default value, 250, is equivalent to a quarter of a second.
      See also:
        RepeatInterval
    }
    property RepeatDelay: Word read FRepeatDelay write FRepeatDelay
      default DEFAULT_REPEAT_DELAY;

    { Description:
        Once the button control has started repeating, it will start calling the
        OnXYZClick event handler repeatedly. The delay between each call is
        the value of this property, given in milliseconds.
      See also:
        RepeatDelay
    }
    property RepeatInterval: Word read FRepeatInterval write FRepeatInterval
      default DEFAULT_REPEAT_INTERVAL;

    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property Transparent: Boolean read GetTransparent write SetTransparent;
  end;

  { Description:
      This is the basic item contained in the TSpeedEditButtonCollection
      and controls how a single button in the TlvkCustomMultiSpeedEdit component
      looks and behaves.
    See also:
      TSpeedEditButtonCollection
  }
  TSpeedEditButtonItem = class(TCollectionItem)
  private
    FButton                 : TSpeedButton;
    FSpinButton             : TlvkSpinButton;

    FSide                   : TButtonSide;
    FWidth                  : Integer;
    FVisible                : Boolean;
    FHint                   : string;
    FShowHint               : Boolean;
    FParentShowHint         : Boolean;
    FParentFont             : Boolean;
    FPopupMenu              : TPopupMenu;
    FFlat                   : Boolean;
    FFont                   : TFont;
    FAction                 : TBasicAction;
    FTransparent            : Boolean;
    FMargin                 : Integer;
    FSpacing                : Integer;
    FLayout                 : TButtonLayout;
    FCaption                : TCaption;
    FKind                   : TlvkEditButtonKind;
    FNumGlyphs              : TNumGlyphs;
    FGlyph                  : TPicture;
    FParentBiDiMode         : Boolean;
    FButtonType             : TButtonType;
    FDefaultDropDown        : Boolean;

    FAutoConfigureFrame     : Boolean;
    FSizeGrip               : Boolean;
    FAutoWidth              : Boolean;
    FOptions                : TlvkDropDownEditOptions;

    FOnMouseUp              : TMouseEvent;
    FOnMouseDown            : TMouseEvent;
    FOnMouseMove            : TMouseMoveEvent;
    FOnBeginRepeat          : TNotifyEvent;
    FOnClick                : TNotifyEvent;
    FOnEndRepeat            : TNotifyEvent;
    FOnDblClick             : TNotifyEvent;
    FOnUpClick              : TNotifyEvent;
    FOnDownClick            : TNotifyEvent;
    FOnPositionSizeGrip     : TPositionSizeGripEvent;

    FRepeatTimer            : TTimer;
    FRepeatDelay            : Word;
    FRepeatInterval         : Word;
    FOnCreateDropDownFrame  : TCreateDropDownFrameEvent;
    FOnBeforeCloseUp        : TDropDownNotifyEvent;
    FOnAfterDropDown        : TDropDownNotifyEvent;
    FOnBeforeDropDown       : TNotifyEvent;
    FOnAfterCloseUp         : TNotifyEvent;

    procedure Invalidate;
    procedure AdjustLook;
    function Edit: TlvkCustomMultiSpeedEdit;

    procedure SpinDownClick(Sender: TObject);
    procedure SpinUpClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ButtonDblClick(Sender: TObject);
    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonRepeatTimer(Sender: TObject);

    procedure SetSide(const Value: TButtonSide);
    procedure SetWidth(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetShowHint(const Value: Boolean);
    procedure SetParentShowHint(const Value: Boolean);
    procedure SetParentFont(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetFlat(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetAction(const Value: TBasicAction);
    procedure SetCaption(const Value: TCaption);
    procedure SetGlyph(const Value: TPicture);
    procedure SetKind(const Value: TlvkEditButtonKind);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetNumGlyphs(const Value: TNumGlyphs);
    procedure SetSpacing(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetParentBiDiMode(const Value: Boolean);
    procedure SetButtonType(const Value: TButtonType);
    function GetDroppedDown: Boolean;
    procedure SetDroppedDown(const Value: Boolean);

  protected
    procedure DestroyButton;
    procedure CreateSpeedButton;
    procedure CreateSpinButton;
    procedure CreateButton;

    procedure StartRepeatClick;
    procedure StopRepeatClick;
    procedure CheckRepeatClick(const X, Y: Integer);

    procedure DoBeforeDropDown;
    procedure DoCreateDropDownFrame(const AOwner: TComponent;
      out Frame: TFrame);
    procedure DoAfterDropDown(const Frame: TFrame);
    procedure DoBeforeCloseUp(const Frame: TFrame);
    procedure DoAfterCloseUp;
    procedure DoPositionSizeGrip(const SizeGrip: TlvkSizeGrip;
      const Frame: TFrame);

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    procedure CloseUp; virtual;
    procedure DropDown; virtual;

  published
    // <ALIAS TlvkCustomDropDownEdit.SizeGrip>
    property SizeGrip: Boolean read FSizeGrip write FSizeGrip
      default DEFAULT_SIZE_GRIP;
    // <ALIAS TlvkCustomDropDownEdit.AutoConfigureFrame>
    property AutoConfigureFrame: Boolean read FAutoConfigureFrame
      write FAutoConfigureFrame default True;
    // <ALIAS TlvkCustomDropDownEdit.AutoWidth>
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;

    { Description:
        This property controls what kind of button will be used for this
        item. The value can be one of the following:

          * btClick - A normal clickable button
          * btRepeatClick - A normal clickable button, that will also start
            repeating if you hold it down
          * btSpin - A Spinner button
          * btDropDown - A combobox-like button
    }
    property ButtonType: TButtonType read FButtonType write SetButtonType
      default btClick;

    // <ALIAS TlvkSpinButton.RepeatDelay>
    property RepeatDelay: Word read FRepeatDelay write FRepeatDelay
      default DEFAULT_REPEAT_DELAY;
    // <ALIAS TlvkSpinButton.RepeatDelay>
    property RepeatInterval: Word read FRepeatInterval write FRepeatInterval
      default DEFAULT_REPEAT_INTERVAL;

    { Description:
        This property controls what side the button will be in the edit
        control. It can be either bsLeft or bsRight for the left or right
        side.

        The order of the items in the collection dictates the order of the
        buttons in the edit control. Topmost item will be leftmost in the
        edit control.
    }
    property Side: TButtonSide read FSide write SetSide
      default DEFAULT_BUTTON_SIDE;

    // <ALIAS TlvkEditButton.Width>
    property Width: Integer read FWidth write SetWidth default 16;

    property Visible: Boolean read FVisible write SetVisible default True;
    property Hint: string read FHint write SetHint;
    property ShowHint: Boolean read FShowHint write SetShowHint default False;
    property ParentShowHint: Boolean read FParentShowHint
      write SetParentShowHint default True;
    property ParentBiDiMode: Boolean read FParentBiDiMode
      write SetParentBiDiMode default True;
    property ParentFont: Boolean read FParentFont write SetParentFont
      default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font: TFont read FFont write SetFont;
    property Action: TBasicAction read FAction write SetAction;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs
      default 1;
    property Glyph: TPicture read FGlyph write SetGlyph;
    property Caption: TCaption read FCaption write SetCaption stored True;
    property Layout: TButtonLayout read FLayout write SetLayout
      default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent
      default True;
    property Kind: TlvkEditButtonKind read FKind write SetKind
      default lbkCaptionOnly;

    { Description:
        Set this property to True to have this item be the default dropdown
        button in the edit control. If you hit the Alt+Down keys, or have the
        TlvkCustomMultiSpeedEdit.AutoDropDown property set to True on the edit
        control, this item will be used to select which dropdown to use.
    }
    property DefaultDropDown: Boolean read FDefaultDropDown
      write FDefaultDropDown default False;

    { Description:
        This event is called once when the repeat-button starts repeating. This
        event handler is only used with btRepeatClick button types.
      See also:
        OnEndRepeat
    }
    property OnBeginRepeat: TNotifyEvent read FOnBeginRepeat write FOnBeginRepeat;

    { Description:
        This event handler is called once for normal clickable buttons, and
        once + repeating for repeating buttons. It is not called for dropdown
        or spinner buttons.
    }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    { Description:
        This event handler is called once when the repeat-button stops
        repeating. This event handler is only used with btRepeatClick
        button types.
      See also:
        OnBeginRepeat
    }
    property OnEndRepeat: TNotifyEvent read FOnEndRepeat write FOnEndRepeat;

    { Description:
        This event handler is called once for normal clickable buttons when
        the user double-clicks on the button. This event handler is only used
        with btClick button types.
      See also:
        OnClick
    }
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;

    { Description:
        This event handler is called once when the user clicks the mouse button
        down on a normal clickable button. This event handler is only used with
        btClick button types.
      See also:
        OnMouseUp
    }
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;

    { Description:
        This event handler is called once when the user moves the cursor around
        on top of a normal clickable button. This event handler is only used
        with btClick button types.
    }
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;

    { Description:
        This event handler is called once when the user releases the mouse
        button from a normal clickable button. This event handler is only used
        with btClick button types.
      See also:
        OnMouseDown
    }
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;

    // <ALIAS TlvkSpinButton.OnUpClick>
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
    // <ALIAS TlvkSpinButton.OnDownClick>
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;

    // <ALIAS TlvkCustomDropDownEdit.OnBeforeDropDown>
    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown
      write FOnBeforeDropDown;
    // <ALIAS TlvkCustomDropDownEdit.OnCreateDropDownFrame>
    property OnCreateDropDownFrame: TCreateDropDownFrameEvent
      read FOnCreateDropDownFrame write FOnCreateDropDownFrame;
    // <ALIAS TlvkCustomDropDownEdit.OnAfterDropDown>
    property OnAfterDropDown: TDropDownNotifyEvent read FOnAfterDropDown
      write FOnAfterDropDown;
    // <ALIAS TlvkCustomDropDownEdit.OnBeforeCloseUp>
    property OnBeforeCloseUp: TDropDownNotifyEvent read FOnBeforeCloseUp
      write FOnBeforeCloseUp;
    // <ALIAS TlvkCustomDropDownEdit.OnAfterCloseUp>
    property OnAfterCloseUp: TNotifyEvent read FOnAfterCloseUp
      write FOnAfterCloseUp;
    // <ALIAS TlvkCustomDropDownEdit.OnPositionSizeGrip>
    property OnPositionSizeGrip: TPositionSizeGripEvent
      read FOnPositionSizeGrip write FOnPositionSizeGrip;
  end;

  { Description:
      This collection class is used to hold a list of the buttons
      attached to the TlvkCustomSpeedEdit control.
    See also:
      TSpeedEditButtonItem
  }
  TSpeedEditButtonCollection = class(TOwnedCollection)
  private
    function GetItem(const Index: Integer): TSpeedEditButtonItem;
    procedure SetItem(const Index: Integer; const Value: TSpeedEditButtonItem);

  protected
    procedure Update(Item: TCollectionItem); override;

  public
    constructor Create(const AOwner: TPersistent);

    function Add: TSpeedEditButtonItem;
    function FindItemID(const ID: Integer): TSpeedEditButtonItem;
    function Insert(const Index: Integer): TSpeedEditButtonItem;
    property Items[const Index: Integer]: TSpeedEditButtonItem read GetItem
      write SetItem; default;
  end;

  { Description:
      This is the custom version of a "edit-with-a-button" control on steroids.
      It allows you to attach multiple buttons to the edit control, and allow
      them to be positioned on either the left or the right side. You can also
      choose what kind of buttons to attach.
  }
  TlvkCustomMultiSpeedEdit = class(TlvkCustomEdit)
  private
    FButtons          : TSpeedEditButtonCollection;
    FLeftMargin       : Integer;
    FRightMargin      : Integer;
    FUndoText         : string;
    FUndoStart        : Integer;
    FUndoLength       : Integer;
    FUndoState        : Boolean;
    FOptions          : TlvkDropDownEditOptions;

    FDropDownForm     : TlvkCustomPopupWindow;
    FDropDownFrame    : TFrame;
    FOwnsFrame        : Boolean;
    FOriginalHeight   : Integer;
    FOriginalWidth    : Integer;
    FDropDownSizeGrip : TlvkSizeGrip;

    FDropDownItem     : TSpeedEditButtonItem;
    FSavedCursor      : TCursor;
    FOnShortCut       : TShortCutEvent;
    FAutoDropDown     : Boolean;

    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure HookOwnerForm;
    procedure UnhookOwnerForm;
    procedure SetButtons(const Value: TSpeedEditButtonCollection);
    procedure ShowDropDown(const Item: TSpeedEditButtonItem);
    procedure HideDropDown;
    procedure CMHideDropDown(var Msg: TMessage); message CM_HIDEDROPDOWN;
    procedure DropDownCloseUp(Sender: TObject);
    procedure ResizeDropDownForm(Sender: TObject);
    function GetDroppedDown: Boolean;
    procedure SetDroppedDown(const Value: Boolean);

  protected
    procedure InvokeDefaultButton(const Default, Cancel: Boolean); virtual;
    procedure AdjustMargins; virtual;
    procedure AdjustEditRect; virtual;
    procedure RemoveButtons; virtual;
    procedure CreateButtons; virtual;

    { Description:
        This property contains a collection of button items to attach to the
        edit control.

        The order of the button items dictates the order in the edit control.'
        The topmost button item will be leftmost in the edit.
    }
    property Buttons: TSpeedEditButtonCollection read FButtons write SetButtons;

    { Description:
        Set this to True to have the edit control automatically drop down one
        of the frames attached to it. You must set the DefaultDropDown
        property on the button item to use.
    }
    property AutoDropDown: Boolean read FAutoDropDown write FAutoDropDown;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;

    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMUndo(var Message: TWMUndo); message WM_UNDO;
    procedure Change; override;
    procedure Loaded; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;

    procedure CMCreateButtons(var Message: TMessage); message CM_CREATEBUTTONS;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;

    { Description:
        This property controls wether there is a dropdown visible or not. It
        will return True if there is, False if not. If you set it to False,
        any visible dropdown will be closed up. If you set it to True, the
        dropdown that is the default dropdown will be dropped down.
    }
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;

    { Description:
        Calling this method has the same effect as setting the DroppedDown
        property to False.
    }
    procedure CloseUp; virtual;

    { Description:
        Calling this method has the same effect as setting the DroppedDown
        property to True.
    }
    procedure DropDown; virtual;

    procedure SetFocus; override;
  end;

  { Description:
      This is the "edit-with-a-button" control on steroids. It allows you to
      attach multiple buttons to the edit control, and allow them to be
      positioned on either the left or the right side. You can also choose what
      kind of buttons to attach.
  }
  TlvkMultiSpeedEdit = class(TlvkCustomMultiSpeedEdit)
  published
    // <ALIAS TlvkCustomMultiSpeedEdit.Buttons>
    property Buttons;
    // <ALIAS TlvkCustomMultiSpeedEdit.AutoDropDown>
    property AutoDropDown;

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
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.ReadOnlyColor>
    property ReadOnlyColor;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment;
  end;

implementation

uses
  {$IFDEF DELPHI6UP}
  Types,
  {$ENDIF}
  SysUtils, Clipbrd;

type
  TlvkDropDownPopup = class(TlvkCustomPopupWindow)
  published
    property OnCloseUp;
    property OnResize;
    property HasSizeGrip;
  end;

  TCrackSpinButton = class(TlvkSpinButton);

function GetParentForm(const Control: TControl): TCustomForm;
var
  Parent  : TControl;
begin
  if Assigned(Control) then
  begin
    Parent := Control.Parent;
    while Assigned(Parent) and (not (Parent is TCustomForm)) do
      Parent := Parent.Parent;

    if Assigned(Parent) then
      Result := Parent as TCustomForm
    else
      Result := nil;
  end else
    Result := nil;
end;

{ TSpeedEditButtonItem }

procedure TSpeedEditButtonItem.AdjustLook;
begin
  if Assigned(FButton) then
  begin
    case FKind of
      lbkCaptionOnly:
        begin
          FButton.Caption := FCaption;
          FButton.Glyph := nil;
        end;

      lbkGlyphOnly:
        begin
          FButton.Caption := '';
          if FGlyph.Width = 0 then
            FButton.Glyph := nil
          else
            FButton.Glyph.Assign(FGlyph);
        end;

      lbkCaptionAndGlyph:
        begin
          FButton.Caption := FCaption;
          if FGlyph.Width = 0 then
            FButton.Glyph := nil
          else
            FButton.Glyph.Assign(FGlyph);
        end;

      lbkComboBoxLookalike:
        begin
          FButton.Caption := '';
          FButton.Glyph := lvkSpeedEditGlyphs.GetGlyphFor(sebkComboBox);
        end;

      lbkEllipsis:
        begin
          FButton.Caption := '';
          FButton.Glyph := lvkSpeedEditGlyphs.GetGlyphFor(sebkEllipsis);
        end;
    else
      raise Exception.Create('Internal error in TSpeedEditButtonItem.AdjustLook');
    end;
  end;
end;

procedure TSpeedEditButtonItem.ButtonClick(Sender: TObject);
begin
  case FButtonType of
    btClick:
      if Assigned(FOnClick) then
        FOnClick(Self);

    btRepeatClick:
      ; // handled elsewhere

    btDropDown:
      begin
        if FButton.Down then
          Edit.ShowDropDown(Self)
        else
          Edit.HideDropDown;
      end;
  end;
end;

procedure TSpeedEditButtonItem.ButtonDblClick(Sender: TObject);
begin
  case FButtonType of
    btClick:
      if Assigned(FOnClick) then
        FOnDblClick(Self);

    btRepeatClick:
      ; // handled elsewhere

    btDropDown:
      ; // cannot double-click such a button
  end;
end;

procedure TSpeedEditButtonItem.ButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);

  if (Button = mbLeft) and (Shift * [ssShift, ssCtrl, ssAlt] = []) and (FButtonType = btRepeatClick) then
    StartRepeatClick;
end;

procedure TSpeedEditButtonItem.ButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);

  if FButtonType = btRepeatClick then
    CheckRepeatClick(X, Y);
end;

procedure TSpeedEditButtonItem.ButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FButtonType = btRepeatClick then
    StopRepeatClick;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TSpeedEditButtonItem.ButtonRepeatTimer(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);

  FRepeatTimer.Interval := FRepeatInterval;
end;

procedure TSpeedEditButtonItem.CheckRepeatClick(const X, Y: Integer);
begin
  if not Assigned(FRepeatTimer) then
    Exit;

  FRepeatTimer.Enabled := PtInRect(Rect(0, 0, FButton.Width, FButton.Height+1),
    Point(X, Y));
end;

procedure TSpeedEditButtonItem.CloseUp;
begin
  DroppedDown := False;
end;

constructor TSpeedEditButtonItem.Create(Collection: TCollection);
begin
  inherited;

  FAutoWidth := True;
  FSizeGrip := False;
  FAutoConfigureFrame := True;
  FOptions := [deoCloseOnEsc, deoCloseOnEnter];

  FRepeatDelay := DEFAULT_REPEAT_DELAY;
  FRepeatInterval := DEFAULT_REPEAT_INTERVAL;
  FSide := DEFAULT_BUTTON_SIDE;
  FWidth := 16;
  FVisible := True;
  FHint := '';
  FShowHint := False;
  FParentShowHint := True;
  FParentFont := True;
  FPopupMenu := nil;
  FFlat := False;
  FFont := TFont.Create;
  FAction := nil;
  FTransparent := True;
  FMargin := -1;
  FSpacing := 4;
  FLayout := blGlyphLeft;
  FCaption := '';
  FKind := lbkCaptionOnly;
  FNumGlyphs := 1;
  FGlyph := TPicture.Create;
  FParentBiDiMode := True;

  Invalidate;
end;

procedure TSpeedEditButtonItem.CreateButton;
begin
  DestroyButton;

  if FButtonType in [btClick, btRepeatClick, btDropDown] then
    CreateSpeedButton
  else if FButtonType = btSpin then
    CreateSpinButton;
end;

procedure TSpeedEditButtonItem.CreateSpeedButton;
begin
  FButton := TSpeedButton.Create(Edit);
  FButton.Parent := Edit;

  if FSide = bsLeft then
    FButton.Align := alLeft
  else
    FButton.Align := alRight;

  FButton.Cursor := crArrow;
  FButton.Width := FWidth;
  FButton.Hint := FHint;
  FButton.Action := FAction;

  FButton.AllowAllUp := (FButtonType = btDropDown);
  if FButton.AllowAllUp then
    FButton.GroupIndex := 1;

  FButton.ParentShowHint := FParentShowHint;
  FButton.ShowHint := FShowHint;
  FButton.ParentFont := FParentFont;
  FButton.PopupMenu := FPopupMenu;
  FButton.Flat := FFlat;
  FButton.Font := FFont;
  FButton.Transparent := FTransparent;
  FButton.Margin := FMargin;
  FButton.Spacing := FSpacing;
  FButton.Layout := FLayout;
  FButton.NumGlyphs := FNumGlyphs;
  FButton.ParentBiDiMode := FParentBiDiMode;

  AdjustLook;

  FButton.OnClick := ButtonClick;
  FButton.OnDblClick := ButtonDblClick;
  FButton.OnMouseDown := ButtonMouseDown;
  FButton.OnMouseMove := ButtonMouseMove;
  FButton.OnMouseUp := ButtonMouseUp;
end;

procedure TSpeedEditButtonItem.CreateSpinButton;
begin
  FSpinButton := TlvkSpinButton.Create(Edit);
  FSpinButton.Parent := Edit;

  if FSide = bsLeft then
    FSpinButton.Align := alLeft
  else
    FSpinButton.Align := alRight;

  FSpinButton.Transparent := FTransparent;
  FSpinButton.Flat := FFlat;
  FSpinButton.Cursor := crArrow;
  FSpinButton.Width := FWidth;
  FSpinButton.Hint := FHint;
  FSpinButton.ShowHint := FShowHint;
  FSpinButton.Action := FAction;
  FSpinButton.RepeatDelay := FRepeatDelay;
  FSpinButton.RepeatInterval := FRepeatInterval;

  FSpinButton.OnDownClick := SpinDownClick;
  FSpinButton.OnUpClick := SpinUpClick;

  FSpinButton.ParentShowHint := FParentShowHint;
  FSpinButton.PopupMenu := FPopupMenu;
end;

destructor TSpeedEditButtonItem.Destroy;
begin
  FGlyph.Free;
  FFont.Free;
  DestroyButton;

  inherited;
end;

procedure TSpeedEditButtonItem.DestroyButton;
begin
  if FButtonType = btDropDown then
  begin
    if Edit.FDropDownItem = Self then
      Edit.CloseUp;
  end;

  StopRepeatClick;
  FreeAndNil(FButton);
  FreeAndNil(FSpinButton);
end;

procedure TSpeedEditButtonItem.DoAfterCloseUp;
begin
  if Assigned(FOnAfterCloseUp) then
    FOnAfterCloseUp(Self);
end;

procedure TSpeedEditButtonItem.DoAfterDropDown(const Frame: TFrame);
begin
  if Assigned(FOnAfterDropDown) then
    FOnAfterDropDown(Self, Frame);
end;

procedure TSpeedEditButtonItem.DoBeforeCloseUp(const Frame: TFrame);
begin
  if Assigned(FOnBeforeCloseUp) then
    FOnBeforeCloseUp(Self, Frame);
end;

procedure TSpeedEditButtonItem.DoBeforeDropDown;
begin
  if Assigned(FOnBeforeDropDown) then
    FOnBeforeDropDown(Self);
end;

procedure TSpeedEditButtonItem.DoCreateDropDownFrame(
  const AOwner: TComponent; out Frame: TFrame);
begin
  if Assigned(FOnCreateDropDownFrame) then
  begin
    Edit.FOwnsFrame := True;
    FOnCreateDropDownFrame(Self, AOwner, Frame, Edit.FOwnsFrame);
  end else begin
    Frame := nil;
    Edit.FOwnsFrame := False;
  end;
end;

procedure TSpeedEditButtonItem.DoPositionSizeGrip(const SizeGrip: TlvkSizeGrip;
  const Frame: TFrame);
begin
  if Assigned(FOnPositionSizeGrip) then
    FOnPositionSizeGrip(Self, SizeGrip, Frame)
  else begin
    SizeGrip.Parent := Frame;
    SizeGrip.Align := alNone;
    SizeGrip.SetBounds(Frame.Width - 13, Frame.Height - 13, 13, 13);
  end;
end;

procedure TSpeedEditButtonItem.DropDown;
begin
  if ButtonType = btDropDown then
    DroppedDown := True;
end;

function TSpeedEditButtonItem.Edit: TlvkCustomMultiSpeedEdit;
begin
  Result := (Collection as TSpeedEditButtonCollection).GetOwner as
    TlvkCustomMultiSpeedEdit;
end;

function TSpeedEditButtonItem.GetDroppedDown: Boolean;
begin
  Result := Assigned(Edit.FDropDownFrame);
end;

procedure TSpeedEditButtonItem.Invalidate;
begin
  if (Collection as TSpeedEditButtonCollection).UpdateCount = 0 then
    Edit.CreateButtons;
end;

procedure TSpeedEditButtonItem.SetAction(const Value: TBasicAction);
begin
  if FAction <> Value then
  begin
    FAction := Value;
    if Assigned(FButton) then
      FButton.Action := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetButtonType(const Value: TButtonType);
var
  OldIsSpin : Boolean;
  NewIsSpin : Boolean;
begin
  if FButtonType <> Value then
  begin
    if Assigned(Edit.FDropDownFrame) then
      Edit.CloseUp;

    OldIsSpin := FButtonType = btSpin;
    FButtonType := Value;
    NewIsSpin := FButtonType = btSpin;

    if OldIsSpin <> NewIsSpin then
      Invalidate
    else begin
      if Assigned(FButton) then
      begin
        FButton.AllowAllUp := (FButtonType = btDropDown);
        if FButton.AllowAllUp then
          FButton.GroupIndex := 1
        else
          FButton.GroupIndex := 0;
      end;
      AdjustLook;
    end;
  end;
end;

procedure TSpeedEditButtonItem.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    AdjustLook;
  end;
end;

procedure TSpeedEditButtonItem.SetDroppedDown(const Value: Boolean);
begin
  if FButtonType = btDropDown then
  begin
    if Value <> GetDroppedDown then
      if Value then
        Edit.ShowDropDown(Self)
      else
        PostMessage(Edit.Handle, CM_HIDEDROPDOWN, 0, 0);
  end else
    raise Exception.Create('Can only drop down btDropDown buttons');
end;

procedure TSpeedEditButtonItem.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if Assigned(FButton) then
      FButton.Flat := Value
    else if Assigned(FSpinButton) then
      FSpinButton.Flat := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetFont(const Value: TFont);
begin
  if Assigned(Value) then
    FFont.Assign(Value)
  else begin
    FFont.Free;
    FFont := TFont.Create;
  end;

  if Assigned(FButton) then
    FButton.Font := FFont;
end;

procedure TSpeedEditButtonItem.SetGlyph(const Value: TPicture);
begin
  FGlyph.Free;
  FGlyph := TPicture.Create;
  FGlyph.Assign(Value);
  AdjustLook;
end;

procedure TSpeedEditButtonItem.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    if Assigned(FButton) then
      FButton.Hint := Value
    else if Assigned(FSpinButton) then
      FSpinButton.Hint := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetKind(const Value: TlvkEditButtonKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    AdjustLook;
  end;
end;

procedure TSpeedEditButtonItem.SetLayout(const Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if Assigned(FButton) then
      FButton.Layout := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetMargin(const Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    if Assigned(FButton) then
      FButton.Margin := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetNumGlyphs(const Value: TNumGlyphs);
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    if Assigned(FButton) then
      FButton.NumGlyphs := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetParentBiDiMode(const Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    if Assigned(FButton) then
      FButton.ParentBiDiMode := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetParentFont(const Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if Assigned(FButton) then
      FButton.ParentFont := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetParentShowHint(const Value: Boolean);
begin
  if FParentShowHint <> Value then
  begin
    FParentShowHint := Value;
    if FParentShowHint then
      FShowHint := False;
    if Assigned(FButton) then
    begin
      FButton.ParentShowHint := FParentShowHint;
      FButton.ShowHint := FShowHint;
    end else if Assigned(FSpinButton) then
    begin
      FSpinButton.ParentShowHint := FParentShowHint;
      FSpinButton.ShowHint := FShowHint;
    end;
  end;
end;

procedure TSpeedEditButtonItem.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if Assigned(FButton) then
      FButton.PopupMenu := Value
    else if Assigned(FSpinButton) then
      FSpinButton.PopupMenu := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetShowHint(const Value: Boolean);
begin
  if FShowHint <> Value then
  begin
    FShowHint := Value;
    if FShowHint then
      FParentShowHint := False;

    if Assigned(FButton) then
    begin
      FButton.ParentShowHint := FParentShowHint;
      FButton.ShowHint := FShowHint;
    end else if Assigned(FSpinButton) then
    begin
      FSpinButton.ParentShowHint := FParentShowHint;
      FSpinButton.ShowHint := FShowHint;
    end;
  end;
end;

procedure TSpeedEditButtonItem.SetSide(const Value: TButtonSide);
begin
  if FSide <> Value then
  begin
    FSide := Value;
    Invalidate;
  end;
end;

procedure TSpeedEditButtonItem.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    if Assigned(FButton) then
      FButton.Spacing := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if Assigned(FButton) then
      FButton.Transparent := Value
    else if Assigned(FSpinButton) then
      FSpinButton.Transparent := Value;
  end;
end;

procedure TSpeedEditButtonItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Invalidate;
  end;
end;

procedure TSpeedEditButtonItem.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Invalidate;
  end;
end;

procedure TSpeedEditButtonItem.SpinDownClick(Sender: TObject);
begin
  if Assigned(FOnDownClick) then
    FOnDownClick(Self);
end;

procedure TSpeedEditButtonItem.SpinUpClick(Sender: TObject);
begin
  if Assigned(FOnUpClick) then
    FOnUpClick(Self);
end;

procedure TSpeedEditButtonItem.StartRepeatClick;
begin
  if Assigned(FButton) and Assigned(FOnClick) then
  begin
    FRepeatTimer := TTimer.Create(FButton);
    FRepeatTimer.Enabled := False;
    FRepeatTimer.OnTimer := ButtonRepeatTimer;
    FRepeatTimer.Interval := FRepeatDelay;
    FRepeatTimer.Enabled := True;

    if Assigned(FOnBeginRepeat) then
      FOnBeginRepeat(Self);
    FOnClick(Self);
  end;
end;

procedure TSpeedEditButtonItem.StopRepeatClick;
begin
  FreeAndNil(FRepeatTimer);
end;

{ TSpeedEditButtonCollection }

function TSpeedEditButtonCollection.Add: TSpeedEditButtonItem;
begin
  Result := inherited Add as TSpeedEditButtonItem;
end;

constructor TSpeedEditButtonCollection.Create(const AOwner: TPersistent);
begin
  inherited Create(AOwner, TSpeedEditButtonItem);
end;

function TSpeedEditButtonCollection.FindItemID(
  const ID: Integer): TSpeedEditButtonItem;
begin
  Result := inherited FindItemID(ID) as TSpeedEditButtonItem;
end;

function TSpeedEditButtonCollection.GetItem(
  const Index: Integer): TSpeedEditButtonItem;
begin
  Result := inherited Items[Index] as TSpeedEditButtonItem;
end;

function TSpeedEditButtonCollection.Insert(
  const Index: Integer): TSpeedEditButtonItem;
begin
  Result := inherited Insert(Index) as TSpeedEditButtonItem;
end;

procedure TSpeedEditButtonCollection.SetItem(const Index: Integer;
  const Value: TSpeedEditButtonItem);
begin
  inherited Items[Index] := Value;
end;

procedure TSpeedEditButtonCollection.Update(Item: TCollectionItem);
begin
  inherited;

  (GetOwner as TlvkCustomMultiSpeedEdit).CreateButtons;
end;

{ TlvkCustomMultiSpeedEdit }

procedure TlvkCustomMultiSpeedEdit.AdjustEditRect;
var
  r : TRect;
begin
  if HandleAllocated and (not IsIconic(Handle)) then
  begin
    SendMessage(Handle, EM_GETRECT, 0, LongInt(@r));

    r.Bottom := ClientHeight;
    r.Right := ClientWidth - FRightMargin;
    r.Top := 0;
    r.Left := FLeftMargin;

    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@r));
  end;
end;

procedure TlvkCustomMultiSpeedEdit.AdjustMargins;
var
  Index : Integer;
begin
  FLeftMargin := 0;
  FRightMargin := 0;

  for Index := 0 to FButtons.Count-1 do
  begin
    if Assigned(FButtons[Index].FButton) then
    begin
      if FButtons[Index].Side = bsLeft then
        Inc(FLeftMargin, FButtons[Index].FButton.Width)
      else
        Inc(FRightMargin, FButtons[Index].FButton.Width);
    end else if Assigned(FButtons[Index].FSpinButton) then
    begin
      if FButtons[Index].Side = bsLeft then
        Inc(FLeftMargin, FButtons[Index].FSpinButton.Width)
      else
        Inc(FRightMargin, FButtons[Index].FSpinButton.Width);
    end;
  end;

  AdjustEditRect;
end;

procedure TlvkCustomMultiSpeedEdit.Change;
begin
  inherited;
  FUndoState := False;
end;

procedure TlvkCustomMultiSpeedEdit.CloseUp;
begin
  if ComponentState * [csDestroying, csDesigning] <> [] then
    Exit;
    
  SetFocus;
  if Assigned(FDropDownFrame) then
    PostMessage(Handle, CM_HIDEDROPDOWN, 0, 0);
end;

procedure TlvkCustomMultiSpeedEdit.CMCreateButtons(var Message: TMessage);
var
  Side  : TButtonSide;
  Index : Integer;
begin
  for Side := bsLeft to bsRight do
    for Index := FButtons.Count-1 downto 0 do
      if (FButtons[Index].Side = Side) and FButtons[Index].Visible then
        FButtons[Index].CreateButton;

  AdjustMargins;
  Invalidate;
end;

procedure TlvkCustomMultiSpeedEdit.CMHideDropDown(var Msg: TMessage);
begin
  if Assigned(FDropDownItem) then
  begin
    FDropDownItem.FButton.Enabled := Enabled;
    if Assigned(FDropDownFrame) then
      HideDropDown;
    Cursor := FSavedCursor;
  end;
end;

constructor TlvkCustomMultiSpeedEdit.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle - [csAcceptsControls, csNoStdEvents] + [csOpaque];
  FButtons := TSpeedEditButtonCollection.Create(Self);
  DoubleBuffered := True;
end;

procedure TlvkCustomMultiSpeedEdit.CreateButtons;
begin
  if ComponentState * [csLoading, csDestroying] <> [] then
    Exit;

  RemoveButtons;
  SendMessage(Handle, CM_CREATEBUTTONS, 0, 0);
end;

procedure TlvkCustomMultiSpeedEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := (Params.Style or ES_MULTILINE) and (not ES_WANTRETURN);
end;

destructor TlvkCustomMultiSpeedEdit.Destroy;
begin
  UnhookOwnerForm;
  if Assigned(FDropDownFrame) then
    HideDropDown;
  RemoveButtons;
  FButtons.Free;

  inherited;
end;

procedure TlvkCustomMultiSpeedEdit.DoEnter;
begin
  inherited;
  if FAutoDropDown then
    DroppedDown := True;
end;

procedure TlvkCustomMultiSpeedEdit.DropDown;
begin
  DroppedDown := True;
end;

procedure TlvkCustomMultiSpeedEdit.DropDownCloseUp(Sender: TObject);
begin
  if Assigned(FDropDownItem) then
  begin
    FDropDownItem.FButton.Enabled := False;
    FSavedCursor := Cursor;
    Cursor := crArrow;
    PostMessage(Handle, CM_HIDEDROPDOWN, 0, 0);
  end;
end;

procedure TlvkCustomMultiSpeedEdit.FormShortCut(var Msg: TWMKey;
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

function TlvkCustomMultiSpeedEdit.GetDroppedDown: Boolean;
begin
  Result := Assigned(FDropDownFrame);
end;

procedure TlvkCustomMultiSpeedEdit.HideDropDown;
begin
  UnhookOwnerForm;
  Assert(Assigned(FDropDownItem));
  try
    FDropDownItem.DoBeforeCloseUp(FDropDownFrame);
  finally
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
    FDropDownItem.FButton.Down := False;
    FDropDownItem.DoAfterCloseUp;
    FDropDownItem := nil;
  end;
end;

procedure TlvkCustomMultiSpeedEdit.HookOwnerForm;
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

procedure TlvkCustomMultiSpeedEdit.InvokeDefaultButton(const Default,
  Cancel: Boolean);
var
  Form  : TCustomForm;
  Found : Boolean;

  procedure Traverse(const Control: TWinControl);
  var
    Index   : Integer;
    Button  : TButton;
  begin
    for Index := 0 to Control.ControlCount-1 do
    begin
      if Control.Controls[Index] is TWinControl then
        Traverse(Control.Controls[Index] as TWinControl);

      if Found then
        Break;

      if Control.Controls[Index] is TButton then
      begin
        Button := TButton(Control.Controls[Index]);

        if (Button.Default = Default) and (Button.Cancel = Cancel) then
        begin
          (Control.Controls[Index] as TButton).Click;
          Found := True;
          Break;
        end;
      end;
    end;
  end;

begin
  Form := GetParentForm(Self);

  if Assigned(Form) then
  begin
    Found := False;
    Traverse(Form as TWinControl);
  end;
end;

procedure TlvkCustomMultiSpeedEdit.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  Msg     : TMsg;
  Index   : Integer;
  Handled : Boolean;
begin
  if (Key = VK_DOWN) and (ssAlt in Shift) then
  begin
    Handled := False;
    for Index := 0 to FButtons.Count-1 do
    begin
      if (FButtons[Index].FButtonType = btDropDown) and
        FButtons[Index].FDefaultDropDown and
        Assigned(FButtons[Index].FButton) then
      begin
        FButtons[Index].FButton.Down := True;
        FButtons[Index].FButton.Click;
        PeekMessage(Msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
        Handled := True;
        Break;
      end;
    end;

    if not Handled then
      inherited KeyDown(Key, Shift);
  end else
    inherited KeyDown(Key, Shift);
end;

procedure TlvkCustomMultiSpeedEdit.KeyPress(var Key: Char);
begin
  case Key of
    #10:
      Key := #0; // Ignore Ctrl+Enter

    #13:
      begin
        Key := #0; // Ignore Enter
        InvokeDefaultButton(True, False);
      end;

    #27:
      begin
        Key := #0;
        InvokeDefaultButton(False, True);
      end;
  end;

  inherited;
end;

procedure TlvkCustomMultiSpeedEdit.Loaded;
var
  Index : Integer;
begin
  inherited;

  CreateButtons;
  for Index := 0 to FButtons.Count-1 do
  begin
    if Assigned(FButtons[Index].FButton) then
      FButtons[Index].FButton.Enabled := Enabled;
    if Assigned(FButtons[Index].FSpinButton) then
      FButtons[Index].FSpinButton.Enabled := Enabled;
  end;
end;

procedure TlvkCustomMultiSpeedEdit.RemoveButtons;
var
  Index : Integer;
begin
  for Index := 0 to FButtons.Count-1 do
    if Assigned(FButtons.Items[Index].FButton) or Assigned(FButtons.Items[Index].FSpinButton) then
      FButtons.Items[Index].DestroyButton;
end;

procedure TlvkCustomMultiSpeedEdit.ResizeDropDownForm(Sender: TObject);
begin
  if Assigned(FDropDownSizeGrip) then
    FDropDownItem.DoPositionSizeGrip(FDropDownSizeGrip, FDropDownFrame);
end;

procedure TlvkCustomMultiSpeedEdit.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

  if HandleAllocated and (not IsIconic(Handle)) then
    AdjustEditRect;
end;

procedure TlvkCustomMultiSpeedEdit.SetButtons(
  const Value: TSpeedEditButtonCollection);
begin
  if Assigned(Value) then
    FButtons.Assign(Value)
  else
    FButtons.Clear;
end;

procedure TlvkCustomMultiSpeedEdit.SetDroppedDown(const Value: Boolean);
var
  Index : Integer;
begin
  if Value <> GetDroppedDown then
    if Value then
    begin
      for Index := 0 to FButtons.Count-1 do
      begin
        if (FButtons[Index].FButtonType = btDropDown) and
          FButtons[Index].FDefaultDropDown and
          Assigned(FButtons[Index].FButton) then
        begin
          FButtons[Index].FButton.Down := True;
          FButtons[Index].FButton.Click;
          Break;
        end;
      end;
    end else
      PostMessage(Handle, CM_HIDEDROPDOWN, 0, 0);
end;

procedure TlvkCustomMultiSpeedEdit.SetEnabled(Value: Boolean);
var
  Index : Integer;
begin
  inherited;

  for Index := 0 to FButtons.Count-1 do
  begin
    if Assigned(FButtons[Index].FButton) then
      FButtons[Index].FButton.Enabled := Value;
    if Assigned(FButtons[Index].FSpinButton) then
      FButtons[Index].FSpinButton.Enabled := Value;
  end;
end;

procedure TlvkCustomMultiSpeedEdit.SetFocus;
begin
  if not (csDesigning in ComponentState) then
    inherited;
end;

procedure TlvkCustomMultiSpeedEdit.ShowDropDown(
  const Item: TSpeedEditButtonItem);
var
  DropDownPoint : TPoint;
  TooHigh       : Boolean;
  WorkingArea   : TRect;
  UseWidth      : Integer;
  OriginalWidth : Integer;
begin
  if ComponentState * [csDestroying, csDesigning] <> [] then
    Exit;
    
  if Assigned(FDropDownFrame) then
    HideDropDown;
  if not Assigned(Item) then
    Exit;

  FDropDownItem := Item;
  Item.DoBeforeDropDown;
  Item.DoCreateDropDownFrame(Self, FDropDownFrame);
  if Assigned(FDropDownFrame) then
  begin
    SetFocus;

    if Item.FAutoConfigureFrame then
    begin
      if Item.FSizeGrip then
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

    if Item.FAutoWidth then
      UseWidth := Width
    else
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

    if Item.FSizeGrip then
    begin
      FDropDownSizeGrip := TlvkSizeGrip.Create(FDropDownForm);
      Item.DoPositionSizeGrip(FDropDownSizeGrip, FDropDownFrame);
      TlvkDropDownPopup(FDropDownForm).HasSizeGrip := True;
    end;

    // Transfer focus to popup window
    Item.FButton.Down := True;
    SelectNext(FDropDownFrame, True, True);

    // Events
    Item.DoAfterDropDown(FDropDownFrame);
    HookOwnerForm;
  end else
    Item.FButton.Down := False;
end;

procedure TlvkCustomMultiSpeedEdit.UnhookOwnerForm;
var
  OwnerForm : TForm;
begin
  OwnerForm := GetOwnerForm(Self);
  if Assigned(OwnerForm) and (not (csDestroying in ComponentState)) then
    OwnerForm.OnShortCut := FOnShortCut;
end;

procedure TlvkCustomMultiSpeedEdit.WMPaste(var Message: TWMPaste);

  function FirstClipboardLine: string;
  var
    Index : Integer;
  begin
    Result := Clipboard.AsText;

    Index := Pos(#13, Result);
    if Index > 0 then
      Delete(Result, Index, Length(Result) - Index + 1);

    Index := Pos(#10, Result);
    if Index > 0 then
      Delete(Result, Index, Length(Result) - Index + 1);
  end;

begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    FUndoText := Text;
    FUndoStart := SelStart;
    FUndoLength := SelLength;

    SelText := FirstClipboardLine;
    FUndoState := True;
  end;
end;

procedure TlvkCustomMultiSpeedEdit.WMUndo(var Message: TWMUndo);
begin
  if FUndoState then
  begin
    Text := FUndoText;
    SelStart := FUndoStart;
    SelLength := FUndoLength;
    FUndoState := False;
  end else
    inherited;
end;

{ TlvkSpinButton }

procedure TlvkSpinButton.AdjustButtons;
begin
  if Assigned(Parent) then
  begin
    FSpinUp.Top := 0;
    FSpinUp.Height := ClientHeight div 2;

    FSpinDown.Top := ClientHeight div 2;
    FSpinDown.Height := ClientHeight - (ClientHeight div 2);
  end;
end;

procedure TlvkSpinButton.ButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FActiveButton := Sender as TSpeedButton;

  if FActiveButton.Tag = 0 then
    DoUpClick
  else
    DoDownClick;

  FRepeatTimer := TTimer.Create(Self);
  FRepeatTimer.Enabled := False;
  FRepeatTimer.Interval := FRepeatDelay;
  FRepeatTimer.OnTimer := ButtonRepeatClick;
  FRepeatTimer.Enabled := True;
end;

procedure TlvkSpinButton.ButtonMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FRepeatTimer) then
  begin
    FRepeatTimer.Enabled := PtInRect(Rect(0, 0, FActiveButton.Width,
      FActiveButton.Height+1), Point(X, Y));
  end;
end;

procedure TlvkSpinButton.ButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FreeAndNil(FRepeatTimer);
  FActiveButton := nil;
end;

procedure TlvkSpinButton.ButtonRepeatClick(Sender: TObject);
begin
  if FActiveButton.Tag = 0 then
    DoUpClick
  else
    DoDownClick;
  FRepeatTimer.Interval := FRepeatInterval;
end;

constructor TlvkSpinButton.Create(AOwner: TComponent);
var
  Glyph : TBitmap;
begin
  inherited;

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption];// + [csOpaque];

  Width := 23;
  Height := 22;

  DoubleBuffered := True;
  FSpinUp := TSpeedButton.Create(Self);
  FSpinUp.Transparent := False;
  FSpinUp.Parent := Self;
  FSpinUp.Align := alTop;
  FSpinUp.Tag := 0;
  FSpinUp.OnMouseDown := ButtonMouseDown;
  FSpinUp.OnMouseMove := ButtonMouseMove;
  FSpinUp.OnMouseUp := ButtonMouseUp;
  Glyph := TBitmap.Create;
  try
    Glyph.LoadFromResourceName(HInstance, 'LVK_SPIN_UP');
    FSpinUp.Glyph.Assign(Glyph);
  finally
    Glyph.Free;
  end;

  FSpinDown := TSpeedButton.Create(Self);
  FSpinDown.Transparent := False;
  FSpinDown.Parent := Self;
  FSpinDown.Align := alClient;
  FSpinDown.Tag := 1;
  FSpinDown.OnMouseDown := ButtonMouseDown;
  FSpinDown.OnMouseMove := ButtonMouseMove;
  FSpinDown.OnMouseUp := ButtonMouseUp;
  Glyph := TBitmap.Create;
  try
    Glyph.LoadFromResourceName(HInstance, 'LVK_SPIN_DOWN');
    FSpinDown.Glyph.Assign(Glyph);
  finally
    Glyph.Free;
  end;

  FRepeatDelay := DEFAULT_REPEAT_DELAY;
  FRepeatInterval := DEFAULT_REPEAT_INTERVAL;
end;

procedure TlvkSpinButton.DoDownClick;
begin
  if Assigned(FOnDownClick) then
    FOnDownClick(Self);
end;

procedure TlvkSpinButton.DoUpClick;
begin
  if Assigned(FOnUpClick) then
    FOnUpClick(Self);
end;

function TlvkSpinButton.GetFlat: Boolean;
begin
  Result := FSpinUp.Flat;
end;

function TlvkSpinButton.GetTransparent: Boolean;
begin
  Result := FSpinUp.Transparent;
end;

procedure TlvkSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  AdjustButtons;
end;

procedure TlvkSpinButton.SetEnabled(Value: Boolean);
begin
  inherited;

  FSpinDown.Enabled := Value;
  FSpinUp.Enabled := Value;
end;

procedure TlvkSpinButton.SetFlat(const Value: Boolean);
begin
  FSpinUp.Flat := Value;
  FSpinDown.Flat := Value;
end;

procedure TlvkSpinButton.SetFocus;
begin
  // Do nothing
  // inherited;
end;

procedure TlvkSpinButton.SetParent(AParent: TWinControl);
begin
  inherited;

  AdjustButtons;
end;

procedure TlvkSpinButton.SetTransparent(const Value: Boolean);
begin
  FSpinUp.Transparent := Value;
  FSpinDown.Transparent := Value;
end;

end.
