{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}
unit ComponentsStateEditorFM;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 5 $
// $Archive: /Components/LVK/source/ComponentsStateEditorFM.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, lvkListBox, ActnList, Buttons, lvkState,
  lvkComboBox, Menus, lvkINIFileStateStorage, lvkFormState, lvkComponentsState,
  lvkEnabler, lvkComponents;

type
  TfmComponentsStateEditor = class(TForm)
    paAvailableProperties: TPanel;
    lblAvailableProperties: TLabel;
    lbAvailableProperties: TlvkListBox;
    Splitter1: TSplitter;
    paSavedProperties: TPanel;
    lblSavedProperties: TLabel;
    lbSavedProperties: TlvkListBox;
    btAdd: TButton;
    btRemove: TButton;
    btMoveUp: TButton;
    btMoveDown: TButton;
    paBottom: TPanel;
    btOk: TBitBtn;
    btCancel: TBitBtn;
    alComponentsStateEditor: TActionList;
    acAdd: TAction;
    acRemove: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    cbComponents: TlvkComboBox;
    Label1: TLabel;
    stIniFile: TlvkINIFileStateStorage;
    stForm: TlvkFormState;
    stComponents: TlvkComponentsState;
    pmAvailableProperties: TPopupMenu;
    pmSavedProperties: TPopupMenu;
    Add1: TMenuItem;
    acClear: TAction;
    Remove1: TMenuItem;
    Clear1: TMenuItem;
    PropertyEnabler: TlvkEventBasedEnabler;
    procedure alComponentsStateEditorUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acAddExecute(Sender: TObject);
    procedure acRemoveExecute(Sender: TObject);
    procedure lbAvailablePropertiesDblClick(Sender: TObject);
    procedure lbSavedPropertiesDblClick(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure cbComponentsChange(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure PropertyEnablerUpdate(Sender: TObject; var Enabled: Boolean);
  private
    { Private declarations }
    FComponentsState  : TlvkComponentsState;

    procedure SetComponentsState(const Value: TlvkComponentsState);
    procedure UpdateUI;
    function GetProperties(const Component: TComponent;
      const Properties: TStrings;
      const ExceptProperties: TStrings): Boolean;

  public
    { Public declarations }
    property ComponentsState: TlvkComponentsState read FComponentsState
      write SetComponentsState;
  end;

var
  fmComponentsStateEditor: TfmComponentsStateEditor;

implementation

uses
  TypInfo;

{$R *.dfm}

{ TfmComponentsStateEditor }

procedure TfmComponentsStateEditor.SetComponentsState(
  const Value: TlvkComponentsState);
begin
  if FComponentsState <> Value then
  begin
    FComponentsState := Value;
    if Assigned(FComponentsState) then
      UpdateUI;
  end;
end;

procedure TfmComponentsStateEditor.UpdateUI;
var
  Form            : TCustomForm;
  ComponentIndex  : Integer;
  Component       : TComponent;
  List            : TStringList;
begin
  Form := FComponentsState.Owner as TCustomForm;
  List := TStringList.Create;
  try
    List.Add(Form.Name);
    for ComponentIndex := 0 to Form.ComponentCount-1 do
    begin
      Component := Form.Components[ComponentIndex];
      if (Component is TlvkCustomState) or (Component is TlvkCustomStateStorage) then
        Continue;

      if GetProperties(Component, nil, nil) then
        List.Add(Component.Name);
    end;

    List.Sort;
    cbComponents.Items.Assign(List);
  finally
    List.Free;
  end;

  lbSavedProperties.Items.Text := FComponentsState.Properties.Text;

  if cbComponents.Items.Count > 0 then
  begin
    cbComponents.ItemIndex := 0;
    cbComponentsChange(Self);
  end else begin
    cbComponents.ItemIndex := -1;
    cbComponents.Text := '';
  end;

  Caption := FComponentsState.Owner.Name + '.' + FComponentsState.Name;
end;

procedure TfmComponentsStateEditor.alComponentsStateEditorUpdate(
  Action: TBasicAction; var Handled: Boolean);
begin
  acAdd.Enabled := lbAvailableProperties.SelCount > 0;
  acRemove.Enabled := lbSavedProperties.SelCount > 0;

  acMoveUp.Enabled := (lbSavedProperties.SelCount > 0) and
    (not lbSavedProperties.Selected[0]);
  acMoveDown.Enabled := (lbSavedProperties.SelCount > 0) and
    (not lbSavedProperties.Selected[lbSavedProperties.Items.Count-1]);

  acClear.Enabled := lbSavedProperties.Items.Count > 0;

  Handled := True;
end;

procedure TfmComponentsStateEditor.acAddExecute(Sender: TObject);
var
  Index : Integer;
begin
  lbSavedProperties.Items.BeginUpdate;
  try
    lbAvailableProperties.Items.BeginUpdate;
    try
      Index := lbAvailableProperties.Items.Count-1;
      while Index >= 0 do
      begin
        if lbAvailableProperties.Selected[Index] then
        begin
          lbSavedProperties.Items.Add(cbComponents.Text + '.' + lbAvailableProperties.Items[Index]);
          lbAvailableProperties.Items.Delete(Index);
        end;

        Dec(Index);
      end;
    finally
      lbAvailableProperties.Items.EndUpdate;
    end;
  finally
    lbSavedProperties.Items.EndUpdate;
  end;
end;

procedure TfmComponentsStateEditor.acRemoveExecute(Sender: TObject);
var
  Index : Integer;
begin
  lbSavedProperties.Items.BeginUpdate;
  try
    lbAvailableProperties.Items.BeginUpdate;
    try
      Index := lbSavedProperties.Items.Count-1;
      while Index >= 0 do
      begin
        if lbSavedProperties.Selected[Index] then
          lbSavedProperties.Items.Delete(Index);
        Dec(Index);
      end;

      cbComponentsChange(Self);
    finally
      lbAvailableProperties.Items.EndUpdate;
    end;
  finally
    lbSavedProperties.Items.EndUpdate;
  end;
end;

procedure TfmComponentsStateEditor.lbAvailablePropertiesDblClick(
  Sender: TObject);
begin
  acAdd.Execute;
end;

procedure TfmComponentsStateEditor.lbSavedPropertiesDblClick(
  Sender: TObject);
begin
  acRemove.Execute;
end;

procedure TfmComponentsStateEditor.acMoveUpExecute(Sender: TObject);
var
  Index : Integer;
begin
  lbSavedProperties.Items.BeginUpdate;
  try
    Index := 0;
    while Index < lbSavedProperties.Items.Count-1 do
    begin
      if lbSavedProperties.Selected[Index+1] then
      begin
        lbSavedProperties.Items.Exchange(Index, Index+1);
        lbSavedProperties.Selected[Index] := True;
      end;
      Inc(Index);
    end;
  finally
    lbSavedProperties.Items.EndUpdate;
  end;
end;

procedure TfmComponentsStateEditor.acMoveDownExecute(Sender: TObject);
var
  Index : Integer;
begin
  lbSavedProperties.Items.BeginUpdate;
  try
    Index := lbSavedProperties.Items.Count-1;
    while Index > 0 do
    begin
      if lbSavedProperties.Selected[Index-1] then
      begin
        lbSavedProperties.Items.Exchange(Index, Index-1);
        lbSavedProperties.Selected[Index] := True;
      end;
      Dec(Index);
    end;
  finally
    lbSavedProperties.Items.EndUpdate;
  end;
end;

procedure TfmComponentsStateEditor.btOkClick(Sender: TObject);
begin
  if Assigned(FComponentsState) then
    FComponentsState.Properties.Text := lbSavedProperties.Items.Text;
end;

procedure TfmComponentsStateEditor.cbComponentsChange(Sender: TObject);
var
  Form      : TCustomForm;
  Component : TComponent;
begin
  Form := FComponentsState.Owner as TCustomForm;

  lbAvailableProperties.Items.BeginUpdate;
  try
    lbAvailableProperties.Items.Clear;

    if Form.Name = cbComponents.Text then
      Component := Form
    else
      Component := Form.FindComponent(cbComponents.Text);
    if Assigned(Component) then
      GetProperties(Component, lbAvailableProperties.Items,
        lbSavedProperties.Items);
  finally
    lbAvailableProperties.Items.EndUpdate;
  end;

  lbAvailableProperties.ItemIndex := -1;
end;

procedure TfmComponentsStateEditor.acClearExecute(Sender: TObject);
begin
  lbSavedProperties.Items.Clear;
  cbComponentsChange(Self);
end;

procedure TfmComponentsStateEditor.PropertyEnablerUpdate(Sender: TObject;
  var Enabled: Boolean);
begin
  Enabled := lbAvailableProperties.Items.Count > 0;
end;

function TfmComponentsStateEditor.GetProperties(
  const Component: TComponent; const Properties: TStrings;
  const ExceptProperties: TStrings): Boolean;
var
  CheckList : TList;

  function Traverse(const Obj: TObject; const BaseName: string): Boolean;
  var
    PropList      : PPropList;
    Count         : Integer;
    PropertyIndex : Integer;
    PropClass     : TClass;
    Available     : Boolean;
    FullName      : string;
  begin
    Result := False;

    if not Assigned(Obj) then
      Exit;

    if CheckList.IndexOf(Obj) >= 0 then
      Exit;

    CheckList.Add(Obj);
    try
      {$IFNDEF DELPHI6UP}
      New(PropList);
      try
      {$ENDIF}
        {$IFDEF DELPHI6UP}
        Count := GetPropList(Obj.ClassInfo, PropList);
        {$ELSE}
        Count := GetPropList(Obj.ClassInfo, STATE_PROPERTY_KINDS, PropList);
        {$ENDIF}
        try
          for PropertyIndex := 0 to Count-1 do
          begin
            if PropList^[PropertyIndex]^.PropType^.Kind in STATE_PROPERTY_KINDS then
            begin
              if PropList^[PropertyIndex]^.PropType^.Kind = tkClass then
              begin
                {$IFDEF DELPHI6UP}
                PropClass := GetObjectPropClass(PropList^[PropertyIndex]);
                {$ELSE}
                PropClass := GetObjectPropClass(Obj, PropList^[PropertyIndex]);
                {$ENDIF}
                Available := PropClass.InheritsFrom(TStrings) or (PropClass = TStrings);

                if not Available then
                begin
                  if Traverse(GetObjectProp(Obj, PropList^[PropertyIndex]),
                    BaseName + PropList^[PropertyIndex].Name + '.') then
                  begin
                    Result := True;
                  end;
                end;
              end else
                Available := True;

              if Available then
              begin
                if Assigned(Properties) then
                begin
                  FullName := Component.Name + '.' + BaseName +
                    PropList^[PropertyIndex]^.Name;
                  if ExceptProperties.IndexOf(FullName) < 0 then
                  begin
                    Result := True;
                    Properties.Add(BaseName + PropList^[PropertyIndex]^.Name);
                  end;
                end else
                  Result := True;
              end;
            end;
          end;
        finally
          {$IFDEF DELPHI6UP}
          FreeMem(PropList);
          {$ENDIF}
        end;
      {$IFNDEF DELPHI6UP}
      finally
        Dispose(PropList);
      end;
      {$ENDIF}
    finally
      CheckList.Remove(Obj);
    end;
  end;

begin
  CheckList := TList.Create;
  try
    Result := Traverse(Component, '');
  finally
    CheckList.Free;
  end;
end;

end.
