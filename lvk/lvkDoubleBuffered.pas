{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a simple component that sets the DoubleBuffered property
    on all wincontrols from a common root.
}
unit lvkDoubleBuffered;

// $Author: Lasse V. Karlsen $
// $Revision: 6 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkDoubleBuffered.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, Forms, Controls, lvkComponents;

type
  { Description:
      Drop this component on a form or frame, set the root to point to
      the part of the form/frame (including controls on top of it) that should
      have the DoubleBuffered property set to True, and that's it.

      The default is to target the owning form or frame, which means all
      controls on the form/frame.
  }
  TlvkDoubleBuffered = class(TlvkComponent)
  private
    FDoubleBuffered : Boolean;
    FRoot           : TWinControl;

    procedure SetDoubleBuffered(const Value: Boolean);
    procedure SetRoot(const Value: TWinControl);

  protected
    procedure Loaded; override;
    procedure PropagateDoubleBuffered; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function IsNotOwner: Boolean;

  public
    constructor Create(AOwner: TComponent); override;

  published
    { Description:
        When you set this property to a new value, all the controls which
        are targetted will have their DoubleBuffered properties adjusted
        to match.

        The default is True.
    }
    property DoubleBuffered: Boolean read FDoubleBuffered
      write SetDoubleBuffered default True;

    { Description:
        This property controls which target control will have its DoubleBuffered
        property adjusted. All controls on top of that control will also
        have their properties adjusted, and controls on top of them, etc.

        Default is to target the owning form or frame (if owner is a form or
        frame that is) so that all other controls on the form/frame will have
        their properties adjusted.
    }
    property Root: TWinControl read FRoot write SetRoot stored IsNotOwner;
  end;

implementation

{ TlvkDoubleBuffered }

constructor TlvkDoubleBuffered.Create(AOwner: TComponent);
begin
  inherited;

  FDoubleBuffered := True;
  FRoot := nil;
end;

function TlvkDoubleBuffered.IsNotOwner: Boolean;
begin
  Result := (FRoot <> Owner) and (Owner is TWinControl);
end;

procedure TlvkDoubleBuffered.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
    if FDoubleBuffered then
      PropagateDoubleBuffered;
end;

procedure TlvkDoubleBuffered.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRoot) then
    FRoot := nil;
end;

procedure TlvkDoubleBuffered.PropagateDoubleBuffered;

  procedure Traverse(const Control: TWinControl);
  var
    Index : Integer;
  begin
    Control.DoubleBuffered := FDoubleBuffered;

    for Index := 0 to Control.ControlCount-1 do
    begin
      if Control.Controls[Index] is TWinControl then
        Traverse(TWinControl(Control.Controls[Index]));
    end;
  end;

begin
  if Assigned(FRoot) then
    Traverse(FRoot)
  else if Owner is TWinControl then
    Traverse(TWinControl(Owner));
end;

procedure TlvkDoubleBuffered.SetDoubleBuffered(const Value: Boolean);
begin
  if FDoubleBuffered <> Value then
  begin
    FDoubleBuffered := Value;

    if not (Owner is TCustomFrame) then
      if ComponentState * [csLoading, csDesigning] = [] then
        PropagateDoubleBuffered;
  end;
end;

procedure TlvkDoubleBuffered.SetRoot(const Value: TWinControl);
begin
  if FRoot <> Value then
  begin
    FRoot := Value;

    if not (Owner is TCustomFrame) then
      if ComponentState * [csLoading, csDesigning] = [] then
        PropagateDoubleBuffered;
  end;
end;

end.
