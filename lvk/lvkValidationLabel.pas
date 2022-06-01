{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkValidationLabel component, a component that
    shows validation messages from TlvkCustomValidator descendants.
}
unit lvkValidationLabel;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkValidationLabel.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes,
  AppEvnts, StdCtrls, lvkValidators, lvkVersion;

type
  { Description:
      This component forms the basis of a validation label component. The
      purpose of it is to perform validation through a set of linked
      validator components, and show any error messages in itself. If all
      the validators succeed, ie. no errors, the label stays blank.
  }
  TlvkCustomValidationLabel = class(TCustomLabel)
  private
    FValidators : TValidatorCollection;
    FAppEvents  : TApplicationEvents;

    procedure SetCaption(const Source: TlvkCustomValidator);
    procedure SetValidators(const Value: TValidatorCollection);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure SetPackageVersion(const Value: TPackageVersion);

  protected
    // <ALIAS TlvkRegExp.PackageVersion>
    function GetPackageVersion: TPackageVersion;
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion: TPackageVersion read GetPackageVersion
      write SetPackageVersion stored False;

    { Description:
        This property is used to list the validators that will be used to
        control the text of this validation label. The order matters, so
        the first one that fails validation will be used to show an
        error message.
    }
    property Validators: TValidatorCollection read FValidators
      write SetValidators;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Description:
      This label is used to show a validation error message. Typical use will
      be to drop one next to a TEdit or similar component, drop a number
      of validator components along with it, link the validators to the
      TEdit or whatever you dropped it next to, and finally link the
      validation label to the validators. This way, if one of the validators
      fail validation (the contents of the TEdit is invalid), the label
      will show the error message associated with the validator. If all the
      validators pass validation, the label will be blank.
  }
  TlvkValidationLabel = class(TlvkCustomValidationLabel)
  published
    // <ALIAS TlvkRegExp.PackageVersion>
    property PackageVersion;

    // <ALIAS TlvkCustomValidationLabel.Validators>
    property Validators;

    // Inherited from TCustomLabel
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DELPHI6UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Graphics;

type
  TValidatorCracker = class(TlvkCustomValidator);

{ TlvkCustomValidationLabel }

procedure TlvkCustomValidationLabel.AppIdle(Sender: TObject;
  var Done: Boolean);
var
  Valid     : Boolean;
  Index     : Integer;
  Validator : TlvkCustomValidator;
begin
  if Assigned(FValidators) then
  begin
    Valid := True;
    for Index := 0 to FValidators.Count-1 do
    begin
      Validator := FValidators[Index].Validator;
      if Assigned(Validator) then
      begin
        if not Validator.Validate then
        begin
          Valid := False;
          SetCaption(Validator);
          Show;
          Break;
        end;
      end;
    end;

    if Valid then
    begin
      SetCaption(nil);
      Hide;
    end;
  end;
end;

constructor TlvkCustomValidationLabel.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FAppEvents := TApplicationEvents.Create(Self);
    FAppEvents.OnIdle := AppIdle;
    Visible := False;
  end;

  FValidators := TValidatorCollection.Create(Self);
  Font.Color := clRed;
  Font.Style := [fsBold];
end;

destructor TlvkCustomValidationLabel.Destroy;
begin
  FreeAndNil(FAppEvents);
  FreeAndNil(FValidators);

  inherited;
end;

function TlvkCustomValidationLabel.GetPackageVersion: TPackageVersion;
begin
  Result := lvkVersion.PackageVersion;
end;

procedure TlvkCustomValidationLabel.Loaded;
begin
  inherited;
  if csDesigning in ComponentState then
    Caption := Name;
end;

procedure TlvkCustomValidationLabel.Notification(AComponent: TComponent;
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

procedure TlvkCustomValidationLabel.SetCaption(
  const Source: TlvkCustomValidator);
var
  NewCaption  : string;
begin
  if Assigned(Source) then
    NewCaption := TValidatorCracker(Source).InvalidMessage
  else
    NewCaption := '';

  if Caption <> NewCaption then
    Caption := NewCaption;
end;

procedure TlvkCustomValidationLabel.SetPackageVersion(
  const Value: TPackageVersion);
begin
  // Do nothing
end;

procedure TlvkCustomValidationLabel.SetValidators(
  const Value: TValidatorCollection);
begin
  FValidators.Assign(Value);
end;

end.
