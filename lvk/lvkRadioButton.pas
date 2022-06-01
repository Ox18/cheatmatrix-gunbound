{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkRadioButton component.
}
unit lvkRadioButton;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkRadioButton.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Windows, Controls, StdCtrls, Messages, lvkVersion,
  ActnList;

type
  TlvkCustomRadioButton = class(TRadioButton)
  private
    FGroupIndex : Integer;
    FChecked    : Boolean;
    FSetChecked : Boolean;

    procedure SetGroupIndex(const Value: Integer);
    procedure TurnSiblingsOff;

  protected
    function GetChecked: Boolean; override;
    procedure CreateWnd; override;
    procedure SetChecked(Value: Boolean); override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure Loaded; override;
    {$IFDEF DELPHI6UP}
    procedure SetAutoSize(Value: Boolean); override;
    {$ELSE}
    function GetAutoSize: Boolean;
    procedure SetAutoSize(Value: Boolean);
    property AutoSize: Boolean read GetAutoSize write SetAutoSize;
    {$ENDIF}

    function BoxSize: TPoint;
    procedure SetParent(AParent: TWinControl); override;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;

    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    procedure SetPackageVersion(const NewValue: TPackageVersion);
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;
    function CanAutoSize(var NewWidth: Integer;
      var NewHeight: Integer): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Description:
      This component works exactly like TRadioButton, part of the Delphi VCL,
      except that it publishes the AutoSize property just like a TLabel
      does. Change the size of the caption property and the check box control
      will resize itself.
    Parameters:
      -
    See also:
      -
  }
  TlvkRadioButton = class(TlvkCustomRadioButton)
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property Align;
    property PackageVersion;
    property AutoSize default True;
    property GroupIndex;
  end;

implementation

type
  TRadioButtonCracker = class(TRadioButton);

{ TlvkCustomRadioButton }

function TlvkCustomRadioButton.BoxSize: TPoint;
begin
  Result.X := GetSystemMetrics(SM_CXMENUCHECK);
  Result.Y := GetSystemMetrics(SM_CYMENUCHECK);
end;

function TlvkCustomRadioButton.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
var
  OldFont : HFont;
  r       : TRect;
  dc      : HDC;
  h       : HWnd;
  Box     : TPoint;
begin
  if Assigned(Parent) and AutoSize then
  begin
    Box := BoxSize;
    r := ClientRect;
    r.Right := 0;
    h := Handle;
    dc := GetDeviceContext(h);
    try
      OldFont := SelectObject(dc, Font.Handle);
      try
        DrawText(dc, PChar(Caption), Length(Caption), r, DT_NOCLIP or
          DT_EXPANDTABS or DT_CALCRECT);
      finally
        SelectObject(dc, OldFont);
      end;
    finally
      ReleaseDC(h, dc);
    end;

    NewWidth := r.Right + Box.X + 6;

    if r.Bottom > NewHeight then
      NewHeight := r.Bottom;
      
    Result := True;
  end else
    Result := False;
end;

procedure TlvkCustomRadioButton.CMTextChanged(var Message: TMessage);
begin
  AdjustSize;
end;

constructor TlvkCustomRadioButton.Create(AOwner: TComponent);
begin
  inherited;

  AutoSize := True;
end;

procedure TlvkCustomRadioButton.CreateWnd;
begin
  inherited;
  SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
end;

{$IFNDEF DELPHI6UP}
function TlvkCustomRadioButton.GetAutoSize: Boolean;
begin
  Result := inherited AutoSize;
end;
{$ENDIF}

function TlvkCustomRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TlvkCustomRadioButton.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomRadioButton.Loaded;
begin
  inherited;
  AdjustSize;

  if FSetChecked then
    Checked := True;
end;

procedure TlvkCustomRadioButton.SetAutoSize(Value: Boolean);
begin
  {$IFDEF DELPHI6UP}
  inherited;
  {$ELSE}
  inherited AutoSize := Value;
  {$ENDIF}
  AdjustSize;
end;

procedure TlvkCustomRadioButton.SetChecked(Value: Boolean);
begin
  if csLoading in ComponentState then
    FSetChecked := Value
  else if FChecked <> Value then
  begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);
    if Value then
    begin
      TurnSiblingsOff;
      inherited Changed;
      if not ClicksDisabled then Click;
    end;
  end;
end;

procedure TlvkCustomRadioButton.SetGroupIndex(const Value: Integer);
begin
  if Value <> FGroupIndex then
  begin
    FGroupIndex := Value;
    if Checked then
      TurnSiblingsOff;
  end;
end;

procedure TlvkCustomRadioButton.SetPackageVersion(
  const NewValue: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomRadioButton.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Parent) then
    AdjustSize;
end;

procedure TlvkCustomRadioButton.TurnSiblingsOff;
var
  I       : Integer;
  Sibling : TControl;
  DoSet   : Boolean;
begin
  if Assigned(Parent) then
    with Parent do
      for I := 0 to ControlCount - 1 do
      begin
        Sibling := Controls[I];
        if (Sibling <> Self) and (Sibling is TRadioButton) then
        begin
          DoSet := True;
          if Sibling is TlvkCustomRadioButton then
          begin
            if (FGroupIndex <> 0) and
              (TlvkCustomRadioButton(Sibling).FGroupIndex <> FGroupIndex) then
            begin
              DoSet := False;
            end;
          end;

          if DoSet then
            with TRadioButtonCracker(Sibling) do
            begin
              {$IFDEF DELPHI6UP}
              if Assigned(Action) and
                 (Action is TCustomAction) and
                 TCustomAction(Action).AutoCheck then
                TCustomAction(Action).Checked := False;
              {$ENDIF}
              SetChecked(False);
            end;
        end;
      end;
end;

end.
