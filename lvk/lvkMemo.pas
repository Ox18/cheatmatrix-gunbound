{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkMemo control.
}
unit lvkMemo;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkMemo.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, Graphics, lvkEdits, lvkVersion, StdCtrls;

type
  { Description:
      This is the base component deriving from TCustomMemo. It adds two
      properties, EnabledColor and DisabledColor, in addition to changing
      the functionality of the Color property.
  }
  TlvkCustomMemo = class(TCustomMemo)
  private
    FEnabledColor   : TColor;
    FDisabledColor  : TColor;
    FReadOnlyColor  : TColor;

    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const NewValue: TColor);
    procedure AdjustColor;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);

  protected
    procedure SetEnabled(Value: Boolean); override;

    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor: TColor index INDEX_ENABLED_COLOR read GetColor
      write SetColor default DEFAULT_ENABLED_COLOR;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor: TColor index INDEX_DISABLED_COLOR read GetColor
      write SetColor default DEFAULT_DISABLED_COLOR;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property ReadOnlyColor: TColor index INDEX_READONLY_COLOR read GetColor
      write SetColor default DEFAULT_READONLY_COLOR;
    // <ALIAS TlvkCustomEdit.Color>
    property Color: TColor index INDEX_CURRENT_COLOR read GetColor
      write SetColor stored False;

    // <ALIAS TlvkCustomEdit.ReadOnly>
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    
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
      This is a control that derives from TlvkCustomMemo which derives from
      TCustomMemo. It adds the EnabledColor and DisabledColor properties, in
      addition to changing the functionality of the Color property slightly.
    See also:
      TlvkCustomListBox
  }
  TlvkMemo = class(TlvkCustomMemo)
  published
    // From TCustomMemo
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
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
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
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

    // From TlvkCustomMemo
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor;
    // <ALIAS TlvkCustomEdit.ReadOnlyColor>
    property ReadOnlyColor;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;
  end;

implementation

uses
  Windows;
  
{ TlvkCustomMemo }

procedure TlvkCustomMemo.AdjustColor;
begin
  if Enabled then
  begin
    if ReadOnly then
      inherited Color := FReadOnlyColor
    else
      inherited Color := FEnabledColor;
  end else
    inherited Color := FDisabledColor;
end;

constructor TlvkCustomMemo.Create(AOwner: TComponent);
begin
  inherited;

  FEnabledColor := DEFAULT_ENABLED_COLOR;
  FDisabledColor := DEFAULT_DISABLED_COLOR;
  FReadOnlyColor := DEFAULT_READONLY_COLOR;
  ControlStyle := ControlStyle - [csSetCaption];
  AdjustColor;
end;

function TlvkCustomMemo.GetColor(const Index: Integer): TColor;
begin
  case Index of
    INDEX_ENABLED_COLOR:
      Result := FEnabledColor;

    INDEX_DISABLED_COLOR:
      Result := FDisabledColor;

    INDEX_READONLY_COLOR:
      Result := FReadOnlyColor;

    INDEX_CURRENT_COLOR:
      Result := inherited Color;
  else
    raise Exception.Create('Internal error in TlvkCustomMemo.GetColor');
  end;
end;

function TlvkCustomMemo.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkCustomMemo.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TlvkCustomMemo.SetColor(const Index: Integer;
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
          FDisabledColor := NewValue;
          if not Enabled then
            AdjustColor;
        end;

      INDEX_READONLY_COLOR:
        begin
          FReadOnlyColor := NewValue;
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
      raise Exception.Create('Internal error in TlvkCustomMemo.SetColor');
    end;
  end;
end;

procedure TlvkCustomMemo.SetEnabled(Value: Boolean);
begin
  inherited;

  AdjustColor;
end;

procedure TlvkCustomMemo.SetPackageVersion(
  const NewValue: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomMemo.SetReadOnly(const Value: Boolean);
begin
  if Value <> (inherited ReadOnly) then
  begin
    inherited ReadOnly := Value;
    AdjustColor;
  end;
end;

end.
