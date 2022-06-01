{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkListBox component.
}
unit lvkListBox;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 5 $
// $Archive: /Components/LVK/source/lvkListBox.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, Graphics, lvkEdits, lvkVersion, StdCtrls;

type
  { Description:
      This is the base component deriving from TCustomListBox. It adds two
      properties, EnabledColor and DisabledColor, in addition to changing
      the functionality of the Color property.

  }
  TlvkCustomListBox = class(TCustomListBox)
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
      This is a control that derives from TlvkCustomListBox which derives from
      TCustomListBox. It adds the EnabledColor and DisabledColor properties, in
      addition to changing the functionality of the Color property slightly.
    See also:
      TlvkCustomListBox
  }
  TlvkListBox = class(TlvkCustomListBox)
  published
    // From TCustomListBox
    property Style;
    {$IFDEF DELPHI6UP}
    property AutoComplete;
    property ScrollWidth;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    {$ENDIF}
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    // <ALIAS TlvkCustomEdit.Color>
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
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
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    // From TlvkCustomListBox
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

implementation

{ TlvkCustomListBox }

procedure TlvkCustomListBox.AdjustColor;
begin
  if Enabled then
    inherited Color := FEnabledColor
  else
    inherited Color := FDisabledColor;
end;

constructor TlvkCustomListBox.Create(AOwner: TComponent);
begin
  inherited;

  FEnabledColor := DEFAULT_ENABLED_COLOR;
  FDisabledColor := DEFAULT_DISABLED_COLOR;
  AdjustColor;
end;

function TlvkCustomListBox.GetColor(const Index: Integer): TColor;
begin
  case Index of
    INDEX_ENABLED_COLOR:
      Result := FEnabledColor;

    INDEX_DISABLED_COLOR:
      Result := FDisabledColor;

    INDEX_CURRENT_COLOR:
      Result := inherited Color;
  else
    raise Exception.Create('Internal error in TlvkCustomListBox in .GetColor');
  end;
end;

function TlvkCustomListBox.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomListBox.SetColor(const Index: Integer;
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
      raise Exception.Create('Internal error in TlvkCustomListBox.SetColor');
    end;
  end;
end;

procedure TlvkCustomListBox.SetEnabled(Value: Boolean);
begin
  inherited;

  AdjustColor;
end;

procedure TlvkCustomListBox.SetPackageVersion(
  const NewValue: TPackageVersion);
begin
  // Do nothing
end;

end.
