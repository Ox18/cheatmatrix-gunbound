{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{* Portions contributed by Wim van der Vegt                                   *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkSpinEdit control.
}
unit lvkSpinEdit;

// $Author: Lasse V. Karlsen $
// $Revision: 1 $
// $Date: 8.03.02 18:53 $
// $Archive: /Components/LVK/Source/lvkSpinEdit.pas $

{$I VERSIONS.INC}

interface

uses
  SysUtils, Classes, Controls, lvkEdits, lvkVersion, Graphics, Spin;

type
  { Description:
      This control inherits from the Sample control provided by Borland,
      TSpinEdit, and adds color adjustment and alignment.
  }
  TlvkSpinEdit = class(TSpinEdit)
  private
    FEnabledColor   : TColor;
    FDisabledColor  : TColor;
    FAlignment      : TlvkAlignment;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    procedure SetPackageVersion(const NewValue: TPackageVersion);

    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const NewValue: TColor);
    procedure AdjustColor;

  protected
    procedure SetAlignment(const Value: TlvkAlignment); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetEnabled(Value: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;

  published
    // <ALIAS TlvkCustomEdit.Alignment>
    property Alignment: TlvkAlignment read FAlignment write SetAlignment
      default ltaLeft;
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
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
  end;

implementation

uses
  Windows;
  
{ TlvkSpinEdit }

procedure TlvkSpinEdit.AdjustColor;
begin
  if Enabled then
    inherited Color := FEnabledColor
  else
    inherited Color := FDisabledColor;
end;

constructor TlvkSpinEdit.Create(AOwner: TComponent);
begin
  inherited;

  FEnabledColor := DEFAULT_ENABLED_COLOR;
  FDisabledColor := DEFAULT_DISABLED_COLOR;
  AdjustColor;
end;

procedure TlvkSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;

  case FAlignment of
    ltaLeft:
      Params.ExStyle := (Params.ExStyle and (not WS_EX_RIGHT)) or WS_EX_LEFT;

    ltaRight:
      Params.ExStyle := (Params.ExStyle and (not WS_EX_LEFT)) or WS_EX_RIGHT;
  end;
end;

function TlvkSpinEdit.GetColor(const Index: Integer): TColor;
begin
  case Index of
    INDEX_ENABLED_COLOR:
      Result := FEnabledColor;

    INDEX_DISABLED_COLOR:
      Result := FDisabledColor;

    INDEX_CURRENT_COLOR:
      Result := inherited Color;
  else
    raise Exception.Create('Internal error in TlvkSpinEdit.GetColor');
  end;
end;

function TlvkSpinEdit.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkSpinEdit.SetAlignment(const Value: TlvkAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TlvkSpinEdit.SetColor(const Index: Integer;
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
      raise Exception.Create('Internal error in TlvkSpinEdit.SetColor');
    end;
  end;
end;

procedure TlvkSpinEdit.SetEnabled(Value: Boolean);
begin
  inherited;

  AdjustColor;
end;

procedure TlvkSpinEdit.SetPackageVersion(const NewValue: TPackageVersion);
begin
  // Do nothing
end;

end.
