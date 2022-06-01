{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkSpeedEdit control.
}
unit lvkSpeedEdit;

// $Author: Lasse V. Karlsen $
// $Revision: 9 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSpeedEdit.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, SysUtils, Classes, Controls, Graphics, lvkEdits, Buttons,
  Messages, Menus;

type
  { Description:
      This enumerated type is used for the Kind property on the button of a
      TlvkSpeedEdit. It controls what kind of graphical display the button
      will have:

      * lbkCaptionOnly - Only the given caption will be displayed, the glyph
        property will be disregarded.
      * lbkGlyphOnly - Only the given glyph will be displayed. The caption will
        be disregarded.
      * lbkCaptionAndGlyph - Both the caption and the glyph given will be used.
      * lbkComboBoxLookalike - The down-arrow used on a combo box will be
        used. The caption and glyph will be disregarded.
      * lbkEllipsis - three single dots will be used, on a normal sized
        edit they will be in the middle of the button. The caption and
        glyph will be disregarded.
    Parameters:
      -
    See also:
      -
  }
  TlvkEditButtonKind = (lbkCaptionOnly, lbkGlyphOnly, lbkCaptionAndGlyph,
    lbkComboBoxLookalike, lbkEllipsis);

  TlvkCustomSpeedEdit = class;
  { Description:
      This class is used for the Button property on the TlvkCustomSpeedEdit
      and TlvkSpeedEdit controls. The properties here are mapped from the
      TSpeedButton control, so if you need more information then look up the
      corresponding property on the TSpeedButton control in the VCL help.
    See also:
      TlvkCustomSpeedEdit, TlvkSpeedEdit
  }
  TlvkEditButton = class(TPersistent)
  private
    FOwner        : TlvkCustomSpeedEdit;
    FOnClick      : TNotifyEvent;
    FCaption      : TCaption;
    FGlyph        : TPicture;
    FKind         : TlvkEditButtonKind;
    FGlyphs       : array[TlvkEditButtonKind] of TBitmap;
    FWidth        : Integer;

    procedure SetWidth(const Value: Integer);
    procedure SetKind(const Value: TlvkEditButtonKind);
    procedure AdjustLook;
    procedure SetCaption(const Value: TCaption);
    procedure SetGlyph(const Value: TPicture);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(const Value: TNumGlyphs);
    function GetAction: TBasicAction;
    procedure SetAction(const Value: TBasicAction);
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetHint: string;
    function GetParentBiDiMode: Boolean;
    function GetParentFont: Boolean;
    function GetParentShowHint: Boolean;
    function GetPopupMenu: TPopupMenu;
    function GetShowHint: Boolean;
    function GetVisible: Boolean;
    procedure SetHint(const Value: string);
    procedure SetParentBiDiMode(const Value: Boolean);
    procedure SetParentFont(const Value: Boolean);
    procedure SetParentShowHint(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetShowHint(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    function GetLayout: TButtonLayout;
    function GetMargin: Integer;
    function GetSpacing: Integer;
    function GetTransparent: Boolean;
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);

  public
    constructor Create(const Owner: TlvkCustomSpeedEdit);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    { Description:
        This controls the width of the button.
    }
    property Width: Integer read FWidth write SetWidth default 16;

    property Visible: Boolean read GetVisible write SetVisible default True;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShowHint write SetShowHint default False;
    property ParentShowHint: Boolean read GetParentShowHint
      write SetParentShowHint default True;
    property ParentBiDiMode: Boolean read GetParentBiDiMode
      write SetParentBiDiMode default True;
    property ParentFont: Boolean read GetParentFont write SetParentFont
      default True;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Font: TFont read GetFont write SetFont;
    property Action: TBasicAction read GetAction write SetAction;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs
      default 1;
    property Glyph: TPicture read FGlyph write SetGlyph;
    property Caption: TCaption read FCaption write SetCaption stored True;
    property Layout: TButtonLayout read GetLayout write SetLayout
      default blGlyphLeft;
    property Margin: Integer read GetMargin write SetMargin default -1;
    property Spacing: Integer read GetSpacing write SetSpacing default 4;
    property Transparent: Boolean read GetTransparent write SetTransparent
      default True;

    { Description:
        Use this property to control what properties to use and wether to load
        default values into the button.
    }
    property Kind: TlvkEditButtonKind read FKind write SetKind
      default lbkEllipsis;
  end;

  { Description:
      This component derives from TlvkCustomEdit and adds a button at the
      right end of the component. This button can be used to pop up dialog boxes
      or similar related to the value in the edit.
    See also:
      TlvkSpeedEdit
  }
  TlvkCustomSpeedEdit = class(TlvkCustomEdit)
  private
    FButton     : TSpeedButton;
    FEditButton : TlvkEditButton;

    function GetOnButtonClick: TNotifyEvent;
    procedure SetEditButton(const Value: TlvkEditButton);
    procedure SetOnButtonClick(const Value: TNotifyEvent);

    procedure AdjustButton;
    procedure ButtonClick(Sender: TObject);

  protected
    procedure SetAlignment(const Value: TAlignment); override;
    property SpeedButton: TSpeedButton read FButton;
    procedure SetParent(AParent: TWinControl); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure SetEnabled(Enabled: Boolean); override;

    { Description:
        This property gives access to properties related to the button.
    }
    property Button: TlvkEditButton read FEditButton write SetEditButton;

    { Description:
        This event handler is called whenever the button is clicked by the
        user or the ClickButton method is called.
    }
    property OnButtonClick: TNotifyEvent read GetOnButtonClick
      write SetOnButtonClick;
    procedure DoEnter; override;

    procedure WindowsXPWorkaround;
    procedure Change; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
      override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    { Description:
        This method simulates a button click on the button at the right end of
        the edit control.
      See also:
        OnButtonClick
    }
    procedure ClickButton; virtual;
  end;

  { Description:
      This control combines a TEdit and a TSpeedButton into one control,
      with the button to the right of the edit box. The button will generate
      an event when clicked and can thus be used to show dialogs, set
      values, check values, etc.
  }
  TlvkSpeedEdit = class(TlvkCustomSpeedEdit)
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
    // <ALIAS TlvkCustomSpeedEdit.OnButtonClick>
    property OnButtonClick;
  end;

implementation

uses
  lvkSpeedEditGlyphs;

{ TlvkCustomSpeedEdit }

procedure TlvkCustomSpeedEdit.AdjustButton;
begin
  if Assigned(Parent) then
  begin
    FButton.SetBounds(ClientWidth - FEditButton.Width, 0, FEditButton.Width,
      ClientHeight);
    if FButton.Visible then
      SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN or EC_LEFTMARGIN,
        MakeLong(0, FEditButton.Width))
    else begin
      SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN or EC_LEFTMARGIN, 0);
    end;
  end;
end;

procedure TlvkCustomSpeedEdit.ButtonClick(Sender: TObject);
begin
  if Assigned(FEditButton.FOnClick) then
    FEditButton.FOnClick(Sender);
end;

procedure TlvkCustomSpeedEdit.Change;
begin
  inherited;
  WindowsXPWorkaround;
end;

procedure TlvkCustomSpeedEdit.Click;
begin
  inherited;
  WindowsXPWorkaround;
end;

procedure TlvkCustomSpeedEdit.ClickButton;
begin
  FButton.Click;
end;

constructor TlvkCustomSpeedEdit.Create(AOwner: TComponent);
begin
  inherited;

  Width := 145;
  
  FButton := TSpeedButton.Create(Self);
  FButton.Parent := Self;
  FButton.Cursor := crArrow;
  FButton.OnClick := ButtonClick;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignVisible];
  FEditButton := TlvkEditButton.Create(Self);
end;

destructor TlvkCustomSpeedEdit.Destroy;
begin
  FEditButton.Free;
  
  inherited;
end;

procedure TlvkCustomSpeedEdit.DoEnter;
begin
  inherited;

  WindowsXPWorkaround;
end;

function TlvkCustomSpeedEdit.GetOnButtonClick: TNotifyEvent;
begin
  Result := FEditButton.FOnClick;
end;

procedure TlvkCustomSpeedEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Msg : TMsg;
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
  begin
    ButtonClick(Self);
    PeekMessage(Msg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
  end else
    inherited KeyDown(Key, Shift);
end;

procedure TlvkCustomSpeedEdit.Loaded;
begin
  inherited;

  AdjustButton;
  FEditButton.AdjustLook;
end;

procedure TlvkCustomSpeedEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  WindowsXPWorkaround;
end;

procedure TlvkCustomSpeedEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  WindowsXPWorkaround;
end;

procedure TlvkCustomSpeedEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  WindowsXPWorkaround;
end;

procedure TlvkCustomSpeedEdit.Resize;
begin
  inherited;
  AdjustButton;
end;

procedure TlvkCustomSpeedEdit.SetAlignment(const Value: TAlignment);
begin
  inherited;
  AdjustButton;
end;

procedure TlvkCustomSpeedEdit.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  AdjustButton;
end;

procedure TlvkCustomSpeedEdit.SetEditButton(const Value: TlvkEditButton);
begin
  FButton.Assign(Value);
end;

procedure TlvkCustomSpeedEdit.SetEnabled(Enabled: Boolean);
begin
  inherited;

  FButton.Enabled := Enabled;
end;

procedure TlvkCustomSpeedEdit.SetOnButtonClick(const Value: TNotifyEvent);
begin
  FEditButton.FOnClick := Value;
end;

procedure TlvkCustomSpeedEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  AdjustButton;
end;

procedure TlvkCustomSpeedEdit.WindowsXPWorkaround;
begin
  // On Windows XP, the button keeps being overpainted by the background
  // color.
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if (Win32MajorVersion = 5) and (Win32MinorVersion >= 1) then
      FButton.Invalidate;
  end;
end;

{ TlvkEditButton }

procedure TlvkEditButton.AdjustLook;
begin
  case FKind of
    lbkCaptionOnly:
      begin
        FOwner.FButton.Caption := FCaption;
        FOwner.FButton.Glyph := nil;
      end;

    lbkGlyphOnly:
      begin
        FOwner.FButton.Caption := '';
        if FGlyph.Width = 0 then
          FOwner.FButton.Glyph := nil
        else
          FOwner.FButton.Glyph.Assign(FGlyph);
      end;

    lbkCaptionAndGlyph:
      begin
        FOwner.FButton.Caption := FCaption;
        if FGlyph.Width = 0 then
          FOwner.FButton.Glyph := nil
        else
          FOwner.FButton.Glyph.Assign(FGlyph);
      end;

    lbkComboBoxLookalike:
      begin
        FOwner.FButton.Caption := '';
        FOwner.FButton.Glyph := lvkSpeedEditGlyphs.GetGlyphFor(sebkComboBox);
      end;

    lbkEllipsis:
      begin
        FOwner.FButton.Caption := '';
        FOwner.FButton.Glyph := lvkSpeedEditGlyphs.GetGlyphFor(sebkEllipsis);
      end;
  else
    raise Exception.Create('Internal error in TlvkEditButton.AdjustLook');
  end;
end;

procedure TlvkEditButton.Assign(Source: TPersistent);
begin
  if Source is TlvkEditButton then
  begin
    FOnClick := TlvkEditButton(Source).FOnClick;
    Caption := TlvkEditButton(Source).Caption;
    Visible := TlvkEditButton(Source).Visible;
    Hint := TlvkEditButton(Source).Hint;
    ShowHint := TlvkEditButton(Source).ShowHint;
    ParentShowHint := TlvkEditButton(Source).ParentShowHint;
    ParentBiDiMode := TlvkEditButton(Source).ParentBiDiMode;
    ParentFont := TlvkEditButton(Source).ParentFont;

    PopupMenu := TlvkEditButton(Source).PopupMenu;
    Flat := TlvkEditButton(Source).Flat;
    Font := TlvkEditButton(Source).Font;
    Action := TlvkEditButton(Source).Action;
    NumGlyphs := TlvkEditButton(Source).NumGlyphs;
    Layout := TlvkEditButton(Source).Layout;
    Margin := TlvkEditButton(Source).Margin;
    Spacing := TlvkEditButton(Source).Spacing;
    Transparent := TlvkEditButton(Source).Transparent;

    Glyph := TlvkEditButton(Source).Glyph;
    Kind := TlvkEditButton(Source).Kind;
  end else
    inherited;
end;

constructor TlvkEditButton.Create(const Owner: TlvkCustomSpeedEdit);
begin
  inherited Create;

  FCaption := '...';
  FWidth := 16;
  FKind := lbkEllipsis;
  FGlyph := TPicture.Create;
  FOwner := Owner;
  AdjustLook;
end;

destructor TlvkEditButton.Destroy;
var
  ButtonKind  : TlvkEditButtonKind;
begin
  FGlyph.Free;
  for ButtonKind := Low(TlvkEditButtonKind) to High(TlvkEditButtonKind) do
    FGlyphs[ButtonKind].Free;

  inherited;
end;

function TlvkEditButton.GetAction: TBasicAction;
begin
  Result := FOwner.FButton.Action;
end;

function TlvkEditButton.GetFlat: Boolean;
begin
  Result := FOwner.FButton.Flat;
end;

function TlvkEditButton.GetFont: TFont;
begin
  Result := FOwner.FButton.Font;
end;

function TlvkEditButton.GetHint: string;
begin
  Result := FOwner.FButton.Hint;
end;

function TlvkEditButton.GetLayout: TButtonLayout;
begin
  Result := FOwner.FButton.Layout;
end;

function TlvkEditButton.GetMargin: Integer;
begin
  Result := FOwner.FButton.Margin;
end;

function TlvkEditButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FOwner.FButton.NumGlyphs;
end;

function TlvkEditButton.GetParentBiDiMode: Boolean;
begin
  Result := FOwner.FButton.ParentBiDiMode;
end;

function TlvkEditButton.GetParentFont: Boolean;
begin
  Result := FOwner.FButton.ParentFont;
end;

function TlvkEditButton.GetParentShowHint: Boolean;
begin
  Result := FOwner.FButton.ParentShowHint;
end;

function TlvkEditButton.GetPopupMenu: TPopupMenu;
begin
  Result := FOwner.FButton.PopupMenu;
end;

function TlvkEditButton.GetShowHint: Boolean;
begin
  Result := FOwner.FButton.ShowHint;
end;

function TlvkEditButton.GetSpacing: Integer;
begin
  Result := FOwner.FButton.Spacing;
end;

function TlvkEditButton.GetTransparent: Boolean;
begin
  Result := FOwner.FButton.Transparent;
end;

function TlvkEditButton.GetVisible: Boolean;
begin
  Result := FOwner.FButton.Visible;
end;

procedure TlvkEditButton.SetAction(const Value: TBasicAction);
begin
  FOwner.FButton.Action := Value;
end;

procedure TlvkEditButton.SetCaption(const Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    AdjustLook;
  end;
end;

procedure TlvkEditButton.SetFlat(const Value: Boolean);
begin
  FOwner.FButton.Flat := Value;
end;

procedure TlvkEditButton.SetFont(const Value: TFont);
begin
  FOwner.FButton.Font := Value;
end;

procedure TlvkEditButton.SetGlyph(const Value: TPicture);
begin
  FGlyph.Free;
  FGlyph := TPicture.Create;
  FGlyph.Assign(Value);
  AdjustLook;
end;

procedure TlvkEditButton.SetHint(const Value: string);
begin
  FOwner.FButton.Hint := Value;
end;

procedure TlvkEditButton.SetKind(const Value: TlvkEditButtonKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    AdjustLook;
  end;
end;

procedure TlvkEditButton.SetLayout(const Value: TButtonLayout);
begin
  FOwner.FButton.Layout := Value;
end;

procedure TlvkEditButton.SetMargin(const Value: Integer);
begin
  FOwner.FButton.Margin := Value;
end;

procedure TlvkEditButton.SetNumGlyphs(const Value: TNumGlyphs);
begin
  FOwner.FButton.NumGlyphs := Value;
end;

procedure TlvkEditButton.SetParentBiDiMode(const Value: Boolean);
begin
  FOwner.FButton.ParentBiDiMode := Value;
end;

procedure TlvkEditButton.SetParentFont(const Value: Boolean);
begin
  FOwner.FButton.ParentFont := Value;
end;

procedure TlvkEditButton.SetParentShowHint(const Value: Boolean);
begin
  FOwner.FButton.ParentShowHint := Value;
end;

procedure TlvkEditButton.SetPopupMenu(const Value: TPopupMenu);
begin
  FOwner.FButton.PopupMenu := Value;
end;

procedure TlvkEditButton.SetShowHint(const Value: Boolean);
begin
  FOwner.FButton.ShowHint := Value;
end;

procedure TlvkEditButton.SetSpacing(const Value: Integer);
begin
  FOwner.FButton.Spacing := Value;
end;

procedure TlvkEditButton.SetTransparent(const Value: Boolean);
begin
  FOwner.FButton.Transparent := Value;
end;

procedure TlvkEditButton.SetVisible(const Value: Boolean);
begin
  if FOwner.FButton.Visible <> Value then
  begin
    FOwner.FButton.Visible := Value;
    FOwner.AdjustButton;
  end;
end;

procedure TlvkEditButton.SetWidth(const Value: Integer);
begin
  if (Value > 1) and (Value <> FWidth) then
  begin
    FWidth := Value;
    FOwner.AdjustButton;
  end;
end;

end.
