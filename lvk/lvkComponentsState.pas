{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains a component for saving property values for individual
    components on a form, and restoring them when the form is loaded later.
}
unit lvkComponentsState;

// $Author: Lasse V. Karlsen $
// $Revision: 7 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkComponentsState.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  SysUtils, Classes, lvkState;

type
  { Description:
      this component saves and restores state of property values of components
      on the form. See the Properties property for more information.
  }
  TlvkComponentsState = class(TlvkCustomState)
  private
    FProperties : TStrings;
    FComponent  : TComponent;

    function SplitName(const Name: string; out ComponentName,
      PropertyName: string): Boolean;
    function LocatePropertyParent(const Name: string;
      out Obj: TObject; out PropertyName: string): Boolean;
    procedure RestoreProperty(const Name: string);
    procedure SaveProperty(const Name: string);
    procedure SetProperties(const Value: TStrings);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SaveState; override;
    procedure RestoreState; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    // <ALIAS TlvkCustomState.SectionName>
    property SectionName;
    // <ALIAS TlvkCustomState.Storage>
    property Storage;
    // <ALIAS TlvkCustomState.Active>
    property Active;
    // <ALIAS TlvkCustomState.OnBeforeSave>
    property OnBeforeSave;
    // <ALIAS TlvkCustomState.OnAfterSave>
    property OnAfterSave;
    // <ALIAS TlvkCustomState.OnBeforeRestore>
    property OnBeforeRestore;
    // <ALIAS TlvkCustomState.OnAfterRestore>
    property OnAfterRestore;

    { Description:
        This property holds a simple TStrings list of the properties to
        save and restore. Each line of the list contains the name of a single
        property of a single component, on the form ComponentName.PropertyName.

        At design-time there is a property editor that can be used to edit
        the list and gives easy access to the available properties of the
        existing components on the form. Note that only components on the
        form itself can be saved in this current version of the component.

        In the future, this component will hopefully handle components on
        frames dropped on the form as well.
    }
    property Properties: TStrings read FProperties write SetProperties;
  end;

implementation

uses
  TypInfo;

{ TlvkComponentsState }

constructor TlvkComponentsState.Create(AOwner: TComponent);
begin
  FProperties := TStringList.Create;

  inherited;
end;

destructor TlvkComponentsState.Destroy;
begin
  inherited;

  FreeAndNil(FProperties);
end;

function TlvkComponentsState.LocatePropertyParent(const Name: string;
  out Obj: TObject; out PropertyName: string): Boolean;
var
  Index   : Integer;
  SubName : string;
begin
  PropertyName := Name;
  Obj := nil;

  repeat
    Index := Pos('.', PropertyName);
    if Index > 0 then
    begin
      SubName := Copy(PropertyName, 1, Index-1);
      Delete(PropertyName, 1, Index);

      if Assigned(Obj) then
      begin
        Obj := GetObjectProp(Obj, SubName);
        Assert(Assigned(Obj), 'Invalid state, ' + Obj.ClassName + ' does not have a property with the name ' + SubName);
      end else begin
        Obj := Form.FindComponent(SubName);
        Assert(Assigned(Obj), 'Invalid state, form does not have a component with the name ' + SubName);
      end;
    end;
  until Index = 0;

  Result := Assigned(Obj);
end;

procedure TlvkComponentsState.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Index         : Integer;
  ComponentName : string;
  PropertyName  : string;
begin
  inherited;

  if Operation = opRemove then
  begin
    Index := 0;
    while Index < FProperties.Count do
    begin
      if SplitName(FProperties[Index], ComponentName, PropertyName) then
      begin
        if CompareText(ComponentName, AComponent.Name) = 0 then
          FProperties.Delete(Index)
        else
          Inc(Index);
      end else
        Inc(Index);
    end;
  end;
end;

procedure TlvkComponentsState.RestoreProperty(const Name: string);
var
  Obj           : TObject;
  PropertyName  : string;
  PropInfo      : PPropInfo;
begin
  if LocatePropertyParent(Name, Obj, PropertyName) then
  begin
    PropInfo := GetPropInfo(Obj, PropertyName, STATE_PROPERTY_KINDS);
    if Assigned(PropInfo) then
    begin
      case PropInfo^.PropType^.Kind of
        tkInteger, tkChar, tkWChar:
          SetOrdProp(Obj, PropInfo, ReadInteger(Name,
            GetOrdProp(Obj, PropInfo)));

        tkInt64:
          SetInt64Prop(Obj, PropInfo, ReadInteger(Name,
            GetInt64Prop(Obj, PropInfo)));

        tkEnumeration:
          SetEnumProp(Obj, PropInfo, ReadString(Name,
            GetEnumProp(Obj, PropInfo)));

        tkFloat:
          SetFloatProp(Obj, PropInfo, ReadFloat(Name,
            GetFloatProp(Obj, PropInfo)));

        tkString, tkLString, tkWString:
          SetStrProp(Obj, PropInfo, ReadString(Name,
            GetStrProp(Obj, PropInfo)));

        tkSet:
          SetSetProp(Obj, PropInfo, ReadString(Name,
            GetSetProp(Obj, PropInfo, True)));

        tkClass:
          begin
            Obj := GetObjectProp(Obj, PropInfo);
            if Obj is TStrings then
              TStrings(Obj).Text := ReadString(Name, TStrings(Obj).Text);
          end;
      end;
    end;
  end;
end;

procedure TlvkComponentsState.RestoreState;
var
  Index : Integer;
begin
  if not Assigned(FProperties) then
    Exit;

  FComponent := nil;
  for Index := 0 to FProperties.Count-1 do
    RestoreProperty(FProperties[Index]);
end;

procedure TlvkComponentsState.SaveProperty(const Name: string);
var
  PropertyName  : string;
  PropInfo      : PPropInfo;
  Obj           : TObject;
begin
  if LocatePropertyParent(Name, Obj, PropertyName) then
  begin
    PropInfo := GetPropInfo(Obj, PropertyName, STATE_PROPERTY_KINDS);
    if Assigned(PropInfo) then
    begin

      case PropInfo^.PropType^.Kind of
        tkInteger, tkChar, tkWChar:
          WriteInteger(Name, GetOrdProp(Obj, PropInfo));

        tkInt64:
          WriteInteger(Name, GetInt64Prop(Obj, PropInfo));

        tkEnumeration:
          WriteString(Name, GetEnumProp(Obj, PropInfo));

        tkFloat:
          WriteFloat(Name, GetFloatProp(Obj, PropInfo));

        tkString, tkLString, tkWString:
          WriteString(Name, GetStrProp(Obj, PropInfo));

        tkSet:
          WriteString(Name, GetSetProp(Obj, PropInfo, True));

        tkClass:
          begin
            Obj := GetObjectProp(Obj, PropInfo);
            if Obj is TStrings then
              WriteString(Name, TStrings(Obj).Text);
          end;
      end;
    end;
  end;
end;

procedure TlvkComponentsState.SaveState;
var
  Index : Integer;
begin
  if not Assigned(FProperties) then
    Exit;

  FComponent := nil;
  for Index := 0 to FProperties.Count-1 do
    SaveProperty(FProperties[Index]);
end;

procedure TlvkComponentsState.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

function TlvkComponentsState.SplitName(const Name: string;
  out ComponentName, PropertyName: string): Boolean;
var
  Index : Integer;
begin
  Result := False;

  if Trim(Name) = '' then
    Exit;
  Index := Pos('.', Name);
  if Index = 0 then
    Exit;

  ComponentName := Trim(Copy(Name, 1, Index-1));
  PropertyName := Trim(Copy(Name, Index+1, Length(Name)));
  Result := (ComponentName <> '') and (PropertyName <> '');
end;

end.
 