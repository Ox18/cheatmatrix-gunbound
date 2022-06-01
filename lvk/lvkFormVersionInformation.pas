{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the TlvkFormVersionInformation control.
}
unit lvkFormVersionInformation;

// $Author: Lasse V. Karlsen $
// $Revision: 8 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkFormVersionInformation.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  lvkVersionInformation,
  SysUtils, Classes, Controls, Forms;

type
  { Description:
      This component is a simple descendant of TlvkVersionInformation. In
      addition to providing the same properties as its ancestor, it will
      automatically update the texts and captions on any components on the
      form you drop the component on, if they contain the text \%VERSION\%.

      For instance, if you set the caption of the form to "Test App v\%VERSION\%"
      then when the program has loaded, the caption will be "Test App v1.0.0.1"
      instead. This applies to all components that has a caption or text
      property and contains \%VERSION\% somewhere in there.
    See also:
      TlvkVersionInformation
  }
  TlvkFormVersionInformation = class(TlvkVersionInformation)
  private
    procedure Traverse(const Component: TComponent);
    function OwnerForm: TComponent;
    
  protected
    procedure Loaded; override;
  end;

implementation

uses
  TypInfo;
  
{ TlvkFormVersionInformation }

procedure TlvkFormVersionInformation.Loaded;
begin
  inherited;

  Traverse(OwnerForm);
end;

function TlvkFormVersionInformation.OwnerForm: TComponent;
begin
  Result := Self;
  while Assigned(Result) and (not (Result is TCustomForm)) and Assigned(Result.Owner) do
    Result := Result.Owner;
end;

procedure TlvkFormVersionInformation.Traverse(const Component: TComponent);
var
  Index1    : Integer;
  Index2    : Integer;
  Count     : Integer;
  PropList  : PPropList;
const
  PropertyNames : array[1..2, 0..1] of string = (
    ('Caption', 'TCaption'),
    ('Text', 'string')
  );

  procedure AdjustProperty(const Component: TComponent; const PropertyName: string);
  var
    Value : string;
  begin
    Value := GetStrProp(Component, PropertyName);
    if Pos('%VERSION%', UpperCase(Value)) > 0 then
    begin
      Value := StringReplace(Value, '%VERSION%', FileVersion, [rfReplaceAll, rfIgnoreCase]);
      SetStrProp(Component, PropertyName, Value);
    end;
  end;

begin
  if not Assigned(Component) then
    Exit;
  if csDesigning in ComponentState then
    Exit;

  for Index1 := 0 to Component.ComponentCount-1 do
    Traverse(Component.Components[Index1]);

  if Component is TWinControl then
    for Index1 := 0 to TWinControl(Component).ControlCount-1 do
      Traverse(TWinControl(Component).Controls[Index1]);

  {$IFDEF DELPHI6UP}
  Count := GetPropList(Component.ClassInfo, PropList);
  if Count > 0 then
  try
  {$ELSE}
  New(PropList);
  try
    Count := GetPropList(Component.ClassInfo, [Low(TTypeKind)..High(TTypeKind)],
      PropList);
    if Count > 0 then
  {$ENDIF}
    for Index1 := 0 to Count-1 do
    begin
      for Index2 := Low(PropertyNames) to High(PropertyNames) do
      begin
        if CompareText(PropList^[Index1].Name, PropertyNames[Index2, 0]) = 0 then
          if PropList^[Index1].PropType^.Name = PropertyNames[Index2, 1] then
            AdjustProperty(Component, PropertyNames[Index2, 0]);
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

end.
