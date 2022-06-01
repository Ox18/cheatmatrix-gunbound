{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains components that implement some of the editbox-like
    components, with new features that I use.
}
unit lvkEdits;

// $Author: Lasse V. Karlsen $
// $Revision: 15 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkEdits.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Graphics, Controls, SysUtils, Classes, Messages, StdCtrls, Buttons,
  Menus, lvkVersion, Dialogs, Forms, ExtCtrls, lvkSizeGrip;

{$R lvkEdits.RES}

const
  INDEX_ENABLED_COLOR     = 0;
  INDEX_DISABLED_COLOR    = 1;
  INDEX_CURRENT_COLOR     = 2;
  INDEX_READONLY_COLOR    = 3;

  DEFAULT_ENABLED_COLOR   = clWindow;
  DEFAULT_DISABLED_COLOR  = clBtnFace;
  DEFAULT_READONLY_COLOR  = clWindow;

  CM_ADJUSTBUTTON         = WM_USER + 1;

type
  { Description:
      This is the base component deriving from TCustomEdit. It adds two
      properties, EnabledColor and DisabledColor, in addition to changing
      the functionality of the Color property.
  }
  TlvkCustomEdit = class(TCustomEdit)
  private
    FEnabledColor   : TColor;
    FDisabledColor  : TColor;
    FReadonlyColor  : TColor;
    FAlignment      : TAlignment;

    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const NewValue: TColor);
    procedure AdjustColor;
    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;

  protected
    procedure SetAlignment(const Value: TAlignment); virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;

    { Description:
        This property controls wether the memo should be readonly or not. The
        default is not.
    }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    { Description:
        This property controls wether the edit is left-aligned or right-aligned.
    }
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;

    { Description:
        This property controls what color the edit component has when it is
        enabled. Changing this property while the control is enabled will
        change the color of the component.
      See also:
        DisabledColor, Color
    }
    property EnabledColor: TColor index INDEX_ENABLED_COLOR read GetColor
      write SetColor default DEFAULT_ENABLED_COLOR;

    { Description:
        This property controls what color the edit component has when it is
        disabled. Changing this property while the control is disabled will
        change the color of the component.
      See also:
        EnabledColor, Color
    }
    property DisabledColor: TColor index INDEX_DISABLED_COLOR read GetColor
      write SetColor default DEFAULT_DISABLED_COLOR;

    { Description:
        This property controls what color the edit component currently has. If
        the control is enabled, setting this property will set the
        EnabledColor property, and if the control is disabled it will set the
        DisabledColor property. The same effect is used when reading from the
        property.
      See also:
        EnabledColor, DisabledColor
    }
    property Color: TColor index INDEX_CURRENT_COLOR read GetColor
      write SetColor stored False;

    { Description:
        This property controls which color to use when the component is in
        readonly mode.
      See also:
        ReadOnly
    }
    property ReadOnlyColor: TColor index INDEX_READONLY_COLOR read GetColor
      write SetColor default DEFAULT_READONLY_COLOR;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    procedure SetPackageVersion(const NewValue: TPackageVersion);

  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
  end;

  { Description:
      This is a control that derives from TlvkCustomEdit which derives from
      TEdit. It adds the EnabledColor and DisabledColor properties, in addition
      to changing the functionality of the Color property slightly.
    See also:
      TlvkCustomEdit
  }
  TlvkEdit = class(TlvkCustomEdit)
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
    // <ALIAS TlvkCustomEdit.ReadOnly>
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
    // <ALIAS TlvkCustomEdit.ReadOnlyColor
    property ReadOnlyColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment;
  end;

implementation

type
  TCollectionCracker = class(TOwnedCollection);

{ TlvkCustomEdit }

procedure TlvkCustomEdit.AdjustColor;
begin
  if Enabled then
  begin
    if ReadOnly then
      inherited Color := FReadonlyColor
    else
      inherited Color := FEnabledColor;
  end else
    inherited Color := FDisabledColor;
end;

constructor TlvkCustomEdit.Create(AOwner: TComponent);
begin
  inherited;

  FEnabledColor := DEFAULT_ENABLED_COLOR;
  FDisabledColor := DEFAULT_DISABLED_COLOR;
  FReadonlyColor := DEFAULT_READONLY_COLOR;
  
  AdjustColor;
end;

procedure TlvkCustomEdit.CreateParams(var Params: TCreateParams);
const
  AlignmentStyles : array[TAlignment] of Word = (
    ES_LEFT,
    ES_RIGHT,
    ES_CENTER
  );
begin
  inherited;

  Params.Style := (Params.Style and (not (ES_LEFT or ES_RIGHT or ES_CENTER))) or
    AlignmentStyles[FAlignment];
end;

function TlvkCustomEdit.GetColor(const Index: Integer): TColor;
begin
  case Index of
    INDEX_ENABLED_COLOR:
      Result := FEnabledColor;

    INDEX_DISABLED_COLOR:
      Result := FDisabledColor;

    INDEX_CURRENT_COLOR:
      Result := inherited Color;

    INDEX_READONLY_COLOR:
      Result := FReadonlyColor;
  else
    raise Exception.Create('Internal error in TlvkCustomEdit.GetColor');
  end;
end;

function TlvkCustomEdit.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkCustomEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TlvkCustomEdit.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TlvkCustomEdit.SetColor(const Index: Integer;
  const NewValue: TColor);
begin
  if NewValue <> GetColor(Index) then
  begin
    case Index of
      INDEX_ENABLED_COLOR:
        begin
          FEnabledColor := NewValue;
          if Enabled and (not ReadOnly) then
            AdjustColor;
        end;

      INDEX_DISABLED_COLOR:
        begin
          FDisabledcolor := NewValue;
          if not Enabled then
            AdjustColor;
        end;

      INDEX_READONLY_COLOR:
        begin
          FReadonlyColor := NewValue;
          if Enabled and ReadOnly then
            AdjustColor;
        end;

      INDEX_CURRENT_COLOR:
        if Enabled then
        begin
          if ReadOnly then
            ReadOnlyColor := NewValue
          else
            EnabledColor := NewValue;
        end else
          DisabledColor := NewValue;
    else
      raise Exception.Create('Internal error in TlvkCustomEdit.SetColor');
    end;
  end;
end;

procedure TlvkCustomEdit.SetEnabled(Value: Boolean);
begin
  inherited;

  AdjustColor;
end;

procedure TlvkCustomEdit.SetPackageVersion(
  const NewValue: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomEdit.SetReadOnly(const Value: Boolean);
begin
  inherited ReadOnly := Value;
  AdjustColor;
end;

end.
