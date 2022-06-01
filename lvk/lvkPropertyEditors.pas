{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit registers property editors into the IDE.
}
unit LVKPropertyEditors;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 7 $
// $Archive: /Components/LVK/source/lvkPropertyEditors.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

procedure Register;

implementation

uses
{$IFDEF DELPHI6UP}
  DesignIntf, DesignEditors, VCLEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  lvkDataSetToExcel, Windows, ShellApi, lvkBinaryCompare, lvkRegExp,
  lvkStatusBar, Dialogs, Forms, 
  SysUtils, Classes, lvkVersion, Controls,
  lvkFormBinaryFile, AboutLVKComponentsFM, lvkState, lvkComponentsState,
  lvkValidators, ComponentsStateEditorFM, lvkGlobalHotKeys,
  EditRegularExpressionFM;

type
  TVersionPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TEditFilename = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TEditInputFilename = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TEditOutputFilename = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TEditDataSetToExcel = class(TComponentEditor)
  public
    procedure Edit; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TEditTemplateFilename = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TEditlvkRegExp = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TEditlvkRegularExpressionValidator = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TEditStatusBar = class(TComponentEditor)
  public
    //procedure Edit; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TComponentsStateProperties = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
  end;

  TComponentsStateComponent = class(TComponentEditor)
  public
    procedure Edit; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TlvkFileItem, 'Filename',
    TEditFilename);

  RegisterPropertyEditor(TypeInfo(TStrings), TlvkComponentsState, 'Properties',
    TComponentsStateProperties);
  RegisterComponentEditor(TlvkComponentsState, TComponentsStateComponent);

  RegisterPropertyEditor(TypeInfo(TPackageVersion), nil, 'PackageVersion',
    TVersionPropertyEditor);

  RegisterComponentEditor(TlvkDataSetToExcel, TEditDataSetToExcel);

  RegisterPropertyEditor(TypeInfo(string), TlvkRegExp, 'Pattern', TEditlvkRegExp);
  RegisterPropertyEditor(TypeInfo(string), TlvkRegularExpressionValidator,
    'RegularExpression', TEditlvkRegularExpressionValidator);

  RegisterPropertyEditor(TypeInfo(TShortCut), THotKeyItem, 'HotKey',
    TShortCutProperty);

  RegisterPropertyEditor(TypeInfo(string), TlvkDataSetToExcel,
    'TemplateFilename', TEditTemplateFilename);

  RegisterPropertyEditor(TypeInfo(string), TlvkPatchBuilder, 'OldFilename',
    TEditInputFilename);
  RegisterPropertyEditor(TypeInfo(string), TlvkPatchBuilder, 'NewFilename',
    TEditInputFilename);
  RegisterPropertyEditor(TypeInfo(string), TlvkPatchBuilder, 'PatchFilename',
    TEditOutputFilename);
  RegisterPropertyEditor(TypeInfo(string), TlvkPatchApplier, 'OldFilename',
    TEditInputFilename);
  RegisterPropertyEditor(TypeInfo(string), TlvkPatchApplier, 'NewFilename',
    TEditOutputFilename);
  RegisterPropertyEditor(TypeInfo(string), TlvkPatchApplier, 'PatchFilename',
    TEditInputFilename);

  RegisterComponentEditor(TlvkStatusBar, TEditStatusBar);
end;

{ TVersionPropertyEditor }

procedure TVersionPropertyEditor.Edit;
begin
  with TfmAboutLVKComponents.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

function TVersionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure EditFilename(const ds2x: TlvkDataSetToExcel);
var
  Dialog    : TOpenDialog;
  Filename  : string;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Filename := ds2x.TemplateFilename;

    Dialog.Filter := 'Excel Workbooks (*.xls)|*.xls|All Files (*.*)|*.*';
    Dialog.FilterIndex := 1;
    Dialog.DefaultExt := 'xls';
    Dialog.InitialDir := ExtractFilePath(Filename);
    Dialog.FileName := ExtractFileName(Filename);
    Dialog.Title := 'Select spreadsheet template';
    Dialog.Options := [ofHideReadOnly, ofEnableSizing, ofPathMustExist, ofFileMustExist];

    if Dialog.Execute then
      ds2x.TemplateFilename := Dialog.FileName;
  finally
    Dialog.Free;
  end;
end;

{ TEditTemplateFilename }

procedure TEditTemplateFilename.Edit;
begin
  EditFilename(GetComponent(0) as TlvkDataSetToExcel);
end;

function TEditTemplateFilename.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TEditDataSetToExcel }

procedure TEditDataSetToExcel.Edit;
begin
  EditFilename(Component as TlvkDataSetToExcel);
end;

procedure TEditDataSetToExcel.ExecuteVerb(Index: Integer);
var
  Filename  : string;
begin
  case Index of
    0:
      (Component as TlvkDataSetToExcel).Execute;

    1:
      begin
        Filename := (Component as TlvkDataSetToExcel).TemplateFilename;
        if Filename <> '' then
          ShellExecute(0, 'OPEN', PChar(Filename), nil, nil, SW_NORMAL)
        else
          ShowMessage('No template filename');
      end;
  else
    inherited;
  end;
end;

function TEditDataSetToExcel.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'Execute';
    1 : Result := 'Edit template file';
  else
    Result := '';
  end;
end;

function TEditDataSetToExcel.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TEditStatusBar }

{
procedure TEditStatusBar.Edit;
begin
  inherited;
  // Do nothing here
end;
}

type
  TPanelType = record
    PanelClass  : TlvkCustomStatusBarPanelClass;
    Caption     : string;
  end;

const
  PanelTypes  : array[0..6] of TPanelType = (
    (PanelClass: TlvkLabelStatusBarPanel;       Caption: 'Label'),
    (PanelClass: TlvkClockStatusBarPanel;       Caption: 'Clock'),
    (PanelClass: TlvkKeyStatusBarPanel;         Caption: 'Key'),
    (PanelClass: TlvkProgressBarStatusBarPanel; Caption: 'Progress bar'),
    (PanelClass: TlvkEditStatusBarPanel;        Caption: 'Edit'),
    (PanelClass: TlvkComboBoxStatusBarPanel;    Caption: 'Combo box'),
    (PanelClass: TlvkHintStatusBarPanel;        Caption: 'Hint')
  );

procedure TEditStatusBar.ExecuteVerb(Index: Integer);
begin
  if Index in [Low(PanelTypes)..High(PanelTypes)] then
    Designer.CreateComponent(PanelTypes[Index].PanelClass, Component, 0, 0, 0, 0);
end;

function TEditStatusBar.GetVerb(Index: Integer): string;
begin
  if Index in [Low(PanelTypes)..High(PanelTypes)] then
    Result := 'Add ' + PanelTypes[Index].Caption + ' status bar panel'
  else
    Result := '';
end;

function TEditStatusBar.GetVerbCount: Integer;
begin
  Result := Length(PanelTypes);
end;

{ TEditInputFilename }

procedure TEditInputFilename.Edit;
var
  Dialog  : TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Application);
  try
    Dialog.InitialDir := ExtractFilePath(GetValue);
    Dialog.FileName := ExtractFileName(GetValue);
    Dialog.Title := 'Select value for ' + GetName;
    Dialog.Options := [ofExtensionDifferent, ofPathMustExist,
      ofFileMustExist, ofEnableSizing{$IFDEF DELPHI6UP}, ofDontAddToRecent{$ENDIF}];

    if Dialog.Execute then
    begin
      SetValue(Dialog.Filename);
      Modified;
    end;
  finally
    Dialog.Free;
  end;
end;

function TEditInputFilename.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TEditOutputFilename }

procedure TEditOutputFilename.Edit;
var
  Dialog  : TSaveDialog;
begin
  Dialog := TSaveDialog.Create(Application);
  try
    Dialog.InitialDir := ExtractFilePath(GetValue);
    Dialog.FileName := ExtractFileName(GetValue);
    Dialog.Title := 'Select value for ' + GetName;
    Dialog.Options := [ofOverwritePrompt, ofExtensionDifferent, ofPathMustExist,
      ofCreatePrompt, ofEnableSizing{$IFDEF DELPHI6UP}, ofDontAddToRecent{$ENDIF}];

    if Dialog.Execute then
    begin
      SetValue(Dialog.Filename);
      Modified;
    end;
  finally
    Dialog.Free;
  end;
end;

function TEditOutputFilename.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TEditFilename }

procedure TEditFilename.Edit;
var
  Dialog  : TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Application);
  try
    Dialog.InitialDir := ExtractFilePath(GetValue);
    Dialog.FileName := ExtractFileName(GetValue);
    Dialog.Title := 'Select file to load';
    Dialog.Options := [ofExtensionDifferent, ofPathMustExist,
      ofFileMustExist, ofEnableSizing{$IFDEF DELPHI6UP}, ofDontAddToRecent{$ENDIF}];

    if Dialog.Execute then
    begin
      SetValue(Dialog.Filename);
      Modified;
    end;
  finally
    Dialog.Free;
  end;
end;

function TEditFilename.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TComponentsStateProperties }

procedure TComponentsStateProperties.Edit;
var
  cse : TfmComponentsStateEditor;
begin
  cse := TfmComponentsStateEditor.Create(Application);
  try
    cse.ComponentsState := TlvkComponentsState(GetComponent(0));
    if cse.ShowModal = mrOk then
      Modified;
  finally
    cse.Free;
  end;
end;

function TComponentsStateProperties.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TComponentsStateProperties.GetValue: string;
const
  Plural  : array[Boolean] of string = ('y', 'ies');
begin
  Result := Format('(%d propert%s)', [
    TlvkComponentsState(GetComponent(0)).Properties.Count,
    Plural[TlvkComponentsState(GetComponent(0)).Properties.Count <> 1]]);
end;

{ TComponentsStateComponent }

procedure TComponentsStateComponent.Edit;
begin
  ExecuteVerb(0);
end;

procedure TComponentsStateComponent.ExecuteVerb(Index: Integer);
var
  cse : TfmComponentsStateEditor;
begin
  case Index of
    0:
      begin
        cse := TfmComponentsStateEditor.Create(Application);
        try
          cse.ComponentsState := TlvkComponentsState(Component);
          if cse.ShowModal = mrOk then
            Designer.Modified;
        finally
          cse.Free;
        end;
      end;
  end;
end;

function TComponentsStateComponent.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Select properties';
  end;
end;

function TComponentsStateComponent.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TEditlvkRegExp }

procedure TEditlvkRegExp.Edit;
var
  re  : TlvkRegExp;
begin
  re := GetComponent(0) as TlvkRegExp;
  with TfmEditRegularExpression.Create(Application) do
  try
    Caption := re.Name;
    RegularExpression := re.Pattern;
    Options := re.Options;

    if ShowModal = mrOk then
    begin
      re.Pattern := RegularExpression;
      re.Options := Options;
      Modified;
    end;
  finally
    Free;
  end;
end;

function TEditlvkRegExp.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TEditlvkRegularExpressionValidator }

procedure TEditlvkRegularExpressionValidator.Edit;
var
  re  : TlvkRegularExpressionValidator;
begin
  re := GetComponent(0) as TlvkRegularExpressionValidator;
  with TfmEditRegularExpression.Create(Application) do
  try
    Caption := re.Name;
    RegularExpression := re.RegularExpression;
    Options := re.Options;

    if ShowModal = mrOk then
    begin
      re.RegularExpression := RegularExpression;
      re.Options := Options;
      Modified;
    end;
  finally
    Free;
  end;
end;

function TEditlvkRegularExpressionValidator.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
