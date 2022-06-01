{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains an enabler component that uses validators to set its
    status.
  See also:
    TlvkCustomValidatorEnabler, TlvkValidatorEnabler
}
unit lvkValidatorEnabler;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkValidatorEnabler.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkEnabler, lvkValidators, lvkVersion;

type
  { Description:
      This is the base class for the TlvkValidatorEnabler component.
  }
  TlvkCustomValidatorEnabler = class(TlvkIdleEnabler)
  private
    FValidators : TValidatorCollection;
    FNeedAll    : Boolean;

    procedure SetValidators(const Value: TValidatorCollection);
    procedure SetPackageVersion(const Value: TPackageVersion);

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

    function GetCurrentStatus: Boolean; override;

    { Description:
        This property lists all the validators to check.
    }
    property Validators: TValidatorCollection read FValidators
      write SetValidators;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    { Description:
        This property tells the enabler component wether all the validators
        must be valid or just one (any one) is enough.
    }
    property NeedAll: Boolean read FNeedAll write FNeedAll default True;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This component is used to enable controls, based on the result of a
      validation check against several validator components.

      List up all the validators to check in the Validators collection
      property.
  }
  TlvkValidatorEnabler = class(TlvkCustomValidatorEnabler)
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;

    // <ALIAS TlvkCustomValidatorEnabler.Validators>
    property Validators;
    // <ALIAS TlvkCustomValidatorEnabler.NeedAll>
    property NeedAll;

    // <ALIAS TlvkCustomEnabler.Controls>
    property Controls;
    // <ALIAS TlvkCustomEnabler.Enablers>
    property Enablers;
  end;

implementation

{ TlvkCustomValidatorEnabler }

constructor TlvkCustomValidatorEnabler.Create(AOwner: TComponent);
begin
  inherited;

  FValidators := TValidatorCollection.Create(Self);
  FNeedAll := True;
end;

destructor TlvkCustomValidatorEnabler.Destroy;
begin
  FValidators.Free;

  inherited;
end;

function TlvkCustomValidatorEnabler.GetCurrentStatus: Boolean;
var
  Index     : Integer;
  Validator : TlvkCustomValidator;
  Any, All  : Boolean;
begin
  Index := 0;

  All := True;
  Any := False;
  while Index < FValidators.Count do
  begin
    Validator := FValidators[Index].Validator;
    if Assigned(Validator) then
    begin
      if Validator.Validate then
        Any := True
      else
        All := False;
    end;

    Inc(Index);
  end;

  if FValidators.Count = 0 then
    Result := True
  else if FNeedAll then
    Result := All
  else
    Result := Any;
end;

function TlvkCustomValidatorEnabler.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomValidatorEnabler.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index : Integer;
begin
  inherited;

  if Assigned(FValidators) then
  begin
    Index := 0;
    while Index < FValidators.Count do
    begin
      if FValidators[Index].Validator = AComponent then
        FValidators.Delete(Index)
      else
        Inc(Index);
    end;
  end;
end;

procedure TlvkCustomValidatorEnabler.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomValidatorEnabler.SetValidators(
  const Value: TValidatorCollection);
begin
  FValidators.Assign(Value);
end;

end.
 