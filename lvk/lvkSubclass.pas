{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a class that is used when subclassing other components
    and handling messages on their behalf.
}
unit lvkSubclass;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkSubclass.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, Controls, Messages,
  lvkComponents;

type
  TlvkSubClass = class(TlvkComponent)
  private
    FOldWindowProc  : TWndMethod;
    FSubClassed     : Boolean;
    FSubClassTarget : TWinControl;
    FSetSubClassed  : Boolean;

    procedure WindowProc(var Message: TMessage);
    procedure SetSubClassTarget(const Value: TWinControl);
    procedure SetSubClassed(const Value: Boolean);

  protected
    procedure BeforeTargetWindowProc(var Message: TMessage;
      var PassToTarget: Boolean); virtual;
    procedure AfterTargetWindowProc(var Message: TMessage); virtual;
    property SubClassTarget: TWinControl read FSubClassTarget
      write SetSubClassTarget;
    property SubClassed: Boolean read FSubClassed write SetSubClassed;
    procedure Loaded; override;

  public
    destructor Destroy; override;
  end;

implementation

{ TlvkSubClass }

procedure TlvkSubClass.AfterTargetWindowProc(
  var Message: TMessage);
begin
  // Do nothing by default
end;

procedure TlvkSubClass.BeforeTargetWindowProc(
  var Message: TMessage; var PassToTarget: Boolean);
begin
  // Do nothing by default
end;

destructor TlvkSubClass.Destroy;
begin
  SubClassed := False;

  inherited;
end;

procedure TlvkSubClass.Loaded;
begin
  inherited;

  if FSetSubClassed then
    SubClassed := True;
end;

procedure TlvkSubClass.SetSubClassed(const Value: Boolean);
begin
  if Value <> FSubClassed then
  begin
    if Value then
    begin
      Assert(FSubClassTarget is TWinControl, 'Sub-class target is not a TWinControl');

      if csLoading in ComponentState then
      begin
        FSetSubClassed := True; 
      end else if not (csDesigning in ComponentState) then
      begin
        FOldWindowProc := FSubClassTarget.WindowProc;
        FSubClassTarget.WindowProc := WindowProc;
        FSubClassed := True;
      end;
    end else begin
      if not (csDestroying in ComponentState) then
      begin
        FSubClassTarget.WindowProc := FOldWindowProc;
        FOldWindowProc := nil;
      end;
      FSubclassed := False;
    end;
  end;
end;

procedure TlvkSubClass.SetSubClassTarget(const Value: TWinControl);
var
  OldSubClassed : Boolean;
begin
  OldSubClassed := SubClassed;
  try
    SubClassed := False;
    FSubClassTarget := Value;
  finally
    if Assigned(FSubClassTarget) then
      SubClassed := OldSubClassed;
  end;
end;

procedure TlvkSubClass.WindowProc(var Message: TMessage);
var
  PassToTarget  : Boolean;
begin
  PassToTarget := True;
  BeforeTargetWindowProc(Message, PassToTarget);

  if PassToTarget and Assigned(FOldWindowProc) then
    FOldWindowProc(Message);

  AfterTargetWindowProc(Message);
end;

end.
