{ TODO 2 -oLVK -cDocumentation : Write documentation for this unit }
{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains functions for registering the relationships between
    forms and datamodules, and producing forms and datamodules upon request.
}
unit lvkFormDependencies;

// $Author: Lasse V. Karlsen $
// $Revision: 10 $
// $Date: 16.04.03 10:50 $
// $Archive: /Components/LVK/source/lvkFormDependencies.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Classes, Forms;

type
  TDataModuleClass = class of TDataModule;

procedure RegisterDependency(const FormClass: TFormClass; const DependsOn: TDataModuleClass); overload;
procedure RegisterDependency(const FormClass: TFormClass; const DependsOn: array of TDataModuleClass); overload;
procedure RegisterDependency(const DataModuleClass: TDataModuleClass; const DependsOn: TDataModuleClass); overload;
procedure RegisterDependency(const DataModuleClass: TDataModuleClass; const DependsOn: array of TDataModuleClass); overload;

procedure RegisterVariable(const FormClass: TFormClass; const Variable); overload;
procedure RegisterVariable(const DataModuleClass: TDataModuleClass; const Variable); overload;

function CreateInstance(const FormClass: TFormClass): TForm; overload;
function CreateInstance(const DataModuleClass: TDataModuleClass): TDataModule; overload;

function GetInstance(const FormClass: TFormClass; const AllowCreate: Boolean=True): TForm; overload;
function GetInstance(const DataModuleClass: TDataModuleClass; const AllowCreate: Boolean=True): TDataModule; overload;

procedure AssertInstance(const FormClass: TFormClass); overload;
procedure AssertInstance(const DataModuleClass: TDataModuleClass); overload;

procedure CleanUpDataModules;

implementation

type
  TDependencyType = (dtForm, dtDataModule);

  TDependencyRec = record
    Dependencies    : array of TDataModuleClass;
    VariablePtr     : Pointer;
    FormClass       : TFormClass;
    DataModuleClass : TDataModuleClass;
  end;

var
  DependencyList  : array of TDependencyRec = nil;

function GenericFindDependency(const FormClass: TFormClass;
  const DataModuleClass: TDataModuleClass): Integer;
var
  Index : Integer;
begin
  Assert(Assigned(FormClass) or Assigned(DataModuleClass));

  Result := -1;
  for Index := Low(DependencyList) to High(DependencyList) do
  begin
    if (Assigned(FormClass) and (FormClass=DependencyList[Index].FormClass)) or (Assigned(DataModuleClass) and (DataModuleClass=DependencyList[Index].DataModuleClass)) then
    begin
      Result := Index;
      Break;
    end;
  end;
end;

function NewDependency(const FormClass: TFormClass;
  const DataModuleClass: TDataModuleClass): Integer;
begin
  Assert(Assigned(FormClass) or Assigned(DataModuleClass));

  SetLength(DependencyList, Length(DependencyList)+1);
  Result := High(DependencyList);
  DependencyList[Result].FormClass := FormClass;
  DependencyList[Result].DataModuleClass := DataModuleClass;
  DependencyList[Result].Dependencies := nil;
  DependencyList[Result].VariablePtr := nil;
end;

procedure GenericRegisterDependency(const FormClass: TFormClass;
  const DataModuleClass: TDataModuleClass;
  const DependsOn: array of TDataModuleClass);
var
  Index     : Integer;
  Index2    : Integer;
  DepIndex  : Integer;
  Found     : Boolean;
begin
  Assert(Assigned(FormClass) or Assigned(DataModuleClass));

  DepIndex := GenericFindDependency(FormClass, DataModuleClass);
  if DepIndex = -1 then
    DepIndex := NewDependency(FormClass, DataModuleClass);

  for Index := Low(DependsOn) to High(DependsOn) do
  begin
    Found := False;
    for Index2 := Low(DependencyList[DepIndex].Dependencies) to High(DependencyList[DepIndex].Dependencies) do
    begin
      if DependencyList[DepIndex].Dependencies[Index2] = DependsOn[Index] then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
    begin
      SetLength(DependencyList[DepIndex].Dependencies, Length(DependencyList[DepIndex].Dependencies)+1);
      DependencyList[DepIndex].Dependencies[High(DependencyList[DepIndex].Dependencies)] := DependsOn[Index];
    end;
  end;
end;

procedure GenericRegisterVariable(const FormClass: TFormClass; const DataModuleClass: TDataModuleClass; const Variable);
var
  DepIndex  : Integer;
begin
  Assert(Assigned(FormClass) or Assigned(DataModuleClass));

  DepIndex := GenericFindDependency(FormClass, DataModuleClass);
  if DepIndex = -1 then
    DepIndex := NewDependency(FormClass, DataModuleClass);
  DependencyList[DepIndex].VariablePtr := @Variable;
end;

function GenericGetInstance(const FormClass: TFormClass; const DataModuleClass: TDataModuleClass; const AllowCreate: Boolean): TObject; forward;
function GenericCreateInstance(const FormClass: TFormClass; const DataModuleClass: TDataModuleClass): TObject;
var
  DepIndex  : Integer;
  Index     : Integer;
  Dummy     : TObject;
begin
  Assert(Assigned(FormClass) or Assigned(DataModuleClass));

  DepIndex := GenericFindDependency(FormClass, DataModuleClass);
  if DepIndex = -1 then
  begin
    if Assigned(FormClass) then
      Result := FormClass.Create(Application)
    else
      Result := DataModuleClass.Create(Application);
    Exit;
  end;

  for Index := Low(DependencyList[DepIndex].Dependencies) to High(DependencyList[DepIndex].Dependencies) do
  begin
    Dummy := GenericGetInstance(nil, DependencyList[DepIndex].Dependencies[Index], True);
    Assert(Assigned(Dummy));
  end;

  if Assigned(FormClass) then
    Result := FormClass.Create(Application)
  else
    Result := DataModuleClass.Create(Application);
  if Assigned(DependencyList[DepIndex].VariablePtr) then
    Move(Result, DependencyList[DepIndex].VariablePtr^, 4);
end;

function GenericGetInstance(const FormClass: TFormClass; const DataModuleClass: TDataModuleClass; const AllowCreate: Boolean): TObject;
var
  Index   : Integer;
  ClsType : TClass;
begin
  Assert(Assigned(FormClass) or Assigned(DataModuleClass));

  if Assigned(FormClass) then
    ClsType := FormClass
  else
    ClsType := DataModuleClass;

  for Index := 0 to Application.ComponentCount-1 do
  begin
    if ClsType = Application.Components[Index].ClassType then
    begin
      Result := Application.Components[Index];
      Exit;
    end;
  end;

  if AllowCreate then
    Result := GenericCreateInstance(FormClass, DataModuleClass)
  else
    Result := nil;
end;

procedure RegisterDependency(const FormClass: TFormClass; const DependsOn: TDataModuleClass);
begin
  GenericRegisterDependency(FormClass, nil, [DependsOn]);
end;

procedure RegisterDependency(const FormClass: TFormClass; const DependsOn: array of TDataModuleClass);
begin
  GenericRegisterDependency(FormClass, nil, DependsOn);
end;

procedure RegisterDependency(const DataModuleClass: TDataModuleClass; const DependsOn: TDataModuleClass);
begin
  GenericRegisterDependency(nil, DataModuleClass, [DependsOn]);
end;

procedure RegisterDependency(const DataModuleClass: TDataModuleClass; const DependsOn: array of TDataModuleClass); overload;
begin
  GenericRegisterDependency(nil, DataModuleClass, DependsOn);
end;

procedure RegisterVariable(const FormClass: TFormClass; const Variable); overload;
begin
  GenericRegisterVariable(FormClass, nil, Variable);
end;

procedure RegisterVariable(const DataModuleClass: TDataModuleClass; const Variable); overload;
begin
  GenericRegisterVariable(nil, DataModuleClass, Variable);
end;

function GetInstance(const FormClass: TFormClass; const AllowCreate: Boolean=True): TForm;
var
  Obj : TObject;
begin
  Obj := TForm(GenericGetInstance(FormClass, nil, AllowCreate));
  if Assigned(Obj) then
    Result := Obj as TForm
  else
    Result := nil;
end;

function GetInstance(const DataModuleClass: TDataModuleClass; const AllowCreate: Boolean): TDataModule;
var
  Obj : TObject;
begin
  Obj := GenericGetInstance(nil, DataModuleClass, AllowCreate);
  if Assigned(Obj) then
    Result := Obj as TDataModule
  else
    Result := nil;
end;

function CreateInstance(const FormClass: TFormClass): TForm;
begin
  Result := GenericCreateInstance(FormClass, nil) as TForm;
end;

function CreateInstance(const DataModuleClass: TDataModuleClass): TDataModule;
begin
  Result := GenericCreateInstance(nil, DataModuleClass) as TDataModule;
end;

procedure CleanUpDataModules;
var
  ToDestroy : array of TObject;
  Index     : Integer;

  function Unnecessary(const dm: TDataModule): Boolean;
  var
    Index1, Index2  : Integer;
    DepIndex        : Integer;
  begin
    Result := False;
    DepIndex := GenericFindDependency(nil, TDataModuleClass(dm.ClassType));
    if DepIndex = -1 then
      Exit;

    for Index1 := 0 to Application.ComponentCount-1 do
    begin
      DepIndex := -1;

      if Application.Components[Index1] is TForm then
        DepIndex := GenericFindDependency(TFormClass(Application.Components[Index1].ClassType), nil)
      else if Application.Components[Index1] is TDataModule then
        if TDataModule(Application.Components[Index1]) <> dm then
          DepIndex := GenericFindDependency(nil, TDataModuleClass(Application.Components[Index1].ClassType));

      if DepIndex <> -1 then
      begin
        for Index2 := Low(DependencyList[DepIndex].Dependencies) to High(DependencyList[DepIndex].Dependencies) do
        begin
          if DependencyList[DepIndex].Dependencies[Index2] = dm.ClassType then
            Exit;
        end;
      end;
    end;

    Result := True;
  end;

begin
  repeat
    SetLength(ToDestroy, 0);

    for Index := 0 to Application.ComponentCount-1 do
    begin
      if Application.Components[Index] is TDataModule then
        if Unnecessary(Application.Components[Index] as TDataModule) then
        begin
          SetLength(ToDestroy, Length(ToDestroy)+1);
          ToDestroy[High(ToDestroy)] := Application.Components[Index];
        end;
    end;

    for Index := Low(ToDestroy) to High(ToDestroy) do
      ToDestroy[Index].Free;
  until Length(ToDestroy) = 0;
end;

procedure AssertInstance(const FormClass: TFormClass);
begin
  Assert(Assigned(GetInstance(FormClass, True)));
end;

procedure AssertInstance(const DataModuleClass: TDataModuleClass);
begin
  Assert(Assigned(GetInstance(DataModuleClass, True)));
end;

end.
