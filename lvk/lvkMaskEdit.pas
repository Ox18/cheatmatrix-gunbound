{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkMaskEdit component.
}
unit lvkMaskEdit;

{$I VERSIONS.INC}

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkMaskEdit.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Controls, lvkVersion, Graphics, lvkEdits, Mask;

type
  { Description:
      This component inherits from the TMaskEdit component and adds
      color adjustment and text alignment.
  }
  TlvkMaskEdit = class(TMaskEdit)
  private
    FEnabledColor   : TColor;
    FDisabledColor  : TColor;
    FReadOnlyColor  : TColor;
    FAlignment      : TAlignment;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    procedure SetPackageVersion(const NewValue: TPackageVersion);

    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const NewValue: TColor);
    procedure AdjustColor;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);

  protected
    procedure SetAlignment(const Value: TAlignment); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetEnabled(Value: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property Align;

    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    // <ALIAS TlvkCustomEdit.EnabledColor>
    property EnabledColor: TColor index INDEX_ENABLED_COLOR read GetColor
      write SetColor default DEFAULT_ENABLED_COLOR;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property DisabledColor: TColor index INDEX_DISABLED_COLOR read GetColor
      write SetColor default DEFAULT_DISABLED_COLOR;
    // <ALIAS TlvkCustomEdit.DisabledColor>
    property ReadOnlyColor: TColor index INDEX_READONLY_COLOR read GetColor
      write SetColor default DEFAULT_READONLY_COLOR;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    // <ALIAS TlvkCustomEdit.Color>
    property Color: TColor index INDEX_CURRENT_COLOR read GetColor
      write SetColor stored False;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
  end;

implementation

uses
  Windows;

{ TlvkMaskEdit }

procedure TlvkMaskEdit.AdjustColor;
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

constructor TlvkMaskEdit.Create(AOwner: TComponent);
begin
  inherited;

  FEnabledColor := DEFAULT_ENABLED_COLOR;
  FDisabledColor := DEFAULT_DISABLED_COLOR;
  FReadOnlyColor := DEFAULT_READONLY_COLOR;
  AdjustColor;
end;

procedure TlvkMaskEdit.CreateParams(var Params: TCreateParams);
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

function TlvkMaskEdit.GetColor(const Index: Integer): TColor;
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
    raise Exception.Create('Internal error in TlvkMaskEdit.GetColor');
  end;
end;

function TlvkMaskEdit.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

function TlvkMaskEdit.GetReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TlvkMaskEdit.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TlvkMaskEdit.SetColor(const Index: Integer;
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
          FReadOnlyColor := NewValue;
          if ReadOnly and Enabled then
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
      raise Exception.Create('Internal error in TlvkMaskEdit.SetColor');
    end;
  end;
end;

procedure TlvkMaskEdit.SetEnabled(Value: Boolean);
begin
  inherited;

  AdjustColor;
end;

procedure TlvkMaskEdit.SetPackageVersion(const NewValue: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkMaskEdit.SetReadOnly(const Value: Boolean);
begin
  if Value <> (inherited ReadOnly) then
  begin
    inherited ReadOnly := Value;
    AdjustColor;
  end;
end;

end.
