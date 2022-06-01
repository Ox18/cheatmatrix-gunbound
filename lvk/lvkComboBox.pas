{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the lvkComboBox control.
}
unit lvkComboBox;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkComboBox.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, Graphics, lvkEdits, lvkVersion, StdCtrls;

type
  { Description:
      This is the base component deriving from TCustomComboBox. It adds two
      properties, EnabledColor and DisabledColor, in addition to changing
      the functionality of the Color property.
  }
  TlvkCustomComboBox = class(TCustomComboBox)
  private
    FEnabledColor   : TColor;
    FDisabledColor  : TColor;

    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const NewValue: TColor);
    procedure AdjustColor;

  protected
    procedure SetEnabled(Value: Boolean); override;

    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor: TColor index INDEX_ENABLED_COLOR read GetColor
      write SetColor default DEFAULT_ENABLED_COLOR;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor: TColor index INDEX_DISABLED_COLOR read GetColor
      write SetColor default DEFAULT_DISABLED_COLOR;
    // <ALIAS TlvkCustomEdit.Color>
    property Color: TColor index INDEX_CURRENT_COLOR read GetColor
      write SetColor stored False;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    procedure SetPackageVersion(const NewValue: TPackageVersion);
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This is a control that derives from TlvkCustomComboBox which derives from
      TCustomComboBox. It adds the EnabledColor and DisabledColor properties, in
      addition to changing the functionality of the Color property slightly.
    See also:
      TlvkCustomListBox
  }
  TlvkComboBox = class(TlvkCustomComboBox)
  published
    // From TCustomComboBox
    {$IFDEF DELPHI6}
    property OnCloseUp;
    property AutoComplete default True;
    property AutoDropDown default False;
    property OnSelect;
    {$ENDIF}
    property Align;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    // <ALIAS TlvkCustomEdit.Color>
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
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
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }

    // From TlvkCustomComboBox
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

implementation

uses
  Windows;
  
{ TlvkCustomComboBox }

procedure TlvkCustomComboBox.AdjustColor;
begin
  if Enabled then
    inherited Color := FEnabledColor
  else
    inherited Color := FDisabledColor;
end;

constructor TlvkCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;

  FEnabledColor := DEFAULT_ENABLED_COLOR;
  FDisabledColor := DEFAULT_DISABLED_COLOR;
  AdjustColor;
end;

function TlvkCustomComboBox.GetColor(const Index: Integer): TColor;
begin
  case Index of
    INDEX_ENABLED_COLOR:
      Result := FEnabledColor;

    INDEX_DISABLED_COLOR:
      Result := FDisabledColor;

    INDEX_CURRENT_COLOR:
      Result := inherited Color;
  else
    raise Exception.Create('Internal error in TlvkCustomComboBox.GetColor');
  end;
end;

function TlvkCustomComboBox.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomComboBox.SetColor(const Index: Integer;
  const NewValue: TColor);
begin
  if NewValue <> GetColor(Index) then
  begin
    case Index of
      INDEX_ENABLED_COLOR:
        begin
          FEnabledColor := NewValue;
          if Enabled then
            AdjustColor;
        end;

      INDEX_DISABLED_COLOR:
        begin
          FDisabledcolor := NewValue;
          if not Enabled then
            AdjustColor;
        end;

      INDEX_CURRENT_COLOR:
        if Enabled then
          EnabledColor := NewValue
        else
          DisabledColor := NewValue;
    else
      raise Exception.Create('Internal error in TlvkCustomComboBox.SetColor');
    end;
  end;
end;

procedure TlvkCustomComboBox.SetEnabled(Value: Boolean);
begin
  inherited;

  AdjustColor;
end;

procedure TlvkCustomComboBox.SetPackageVersion(
  const NewValue: TPackageVersion);
begin
  // Do nothing
end;

end.
