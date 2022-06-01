{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the property editor for the script components.
}
unit lvkActiveScriptCodeEditorFM;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 3 $
// $Archive: /Components/LVK/source/lvkActiveScriptCodeEditorFM.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
{$IFDEF DELPHI6UP}
  DesignIntf, DesignEditors, Variants,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterJScript, lvkStatusBar,
  ExtCtrls, SynEdit, SynHighlighterGeneral, SynHighlighterPython,
  SynHighlighterVBScript, StdCtrls, ActnList,
  lvkActiveScriptComponent, lvkActiveScript, SynMemo, ImgList, Menus,
  StdActns;

type
  TlvkActiveScriptCodeEditor = class(TForm)
    StatusBar: TlvkStatusBar;
    paFilename: TlvkLabelStatusBarPanel;
    paClock: TlvkClockStatusBarPanel;
    paCapsLock: TlvkKeyStatusBarPanel;
    paNumLock: TlvkKeyStatusBarPanel;
    paScrollLock: TlvkKeyStatusBarPanel;
    paCaretPosition: TlvkLabelStatusBarPanel;
    paToolbar: TPanel;
    btCompile: TButton;
    hlJScript: TSynJScriptSyn;
    hlVBScript: TSynVBScriptSyn;
    hlPython: TSynPythonSyn;
    hlGeneral: TSynGeneralSyn;
    alScriptEditor: TActionList;
    acCompile: TAction;
    lblError: TLabel;
    btCancel: TButton;
    btOk: TButton;
    eScriptCode: TSynMemo;
    ilScriptEditor: TImageList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    pmScriptCode: TPopupMenu;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Undo1: TMenuItem;
    SelectAll1: TMenuItem;
    lvkLabelStatusBarPanel1: TlvkLabelStatusBarPanel;
    paLanguage: TlvkComboBoxStatusBarPanel;
    procedure alScriptEditorUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acCompileExecute(Sender: TObject);
    procedure paLanguageDropDown(Sender: TObject);
    procedure paLanguageChange(Sender: TObject);
  private
    FLanguage: string;
    function GetSourceCode: string;
    procedure SetLanguage(const Value: string);
    procedure SetSourceCode(const Value: string);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    { Private declarations }
  public
    { Public declarations }

    property Language: string read FLanguage write SetLanguage;
    property SourceCode: string read GetSourceCode write SetSourceCode;
    property Caption: string read GetCaption write SetCaption;
  end;

  TScriptCodeEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
  end;

  TScriptCodeModuleEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
  end;

  TActiveScriptEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

{$R *.dfm}

procedure Register;
begin
  RegisterComponentEditor(TlvkActiveScriptWrapper, TActiveScriptEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TlvkActiveScriptWrapper,
    'ScriptCode', TScriptCodeEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TlvkActiveScriptModule,
    'ScriptCode', TScriptCodeModuleEditor);
end;

function TlvkActiveScriptCodeEditor.GetSourceCode: string;
begin
  Result := eScriptCode.Text;
end;

procedure TlvkActiveScriptCodeEditor.SetLanguage(const Value: string);
begin
  FLanguage := Value;
  if paLanguage.Text <> Value then
    paLanguage.Text := Value;

  if CompareText(Copy(FLanguage, 1, 8), 'VBSCRIPT') = 0 then
    eScriptCode.Highlighter := hlVBScript
  else if CompareText(Copy(FLanguage, 1, 7), 'JSCRIPT') = 0 then
    eScriptCode.Highlighter := hlJScript
  else if CompareText(Copy(FLanguage, 1, 10), 'JAVASCRIPT') = 0 then
    eScriptCode.Highlighter := hlJScript
  else if CompareText(Copy(FLanguage, 1, 6), 'PYTHON') = 0 then
    eScriptCode.Highlighter := hlPython
  else
    eScriptCode.Highlighter := hlGeneral;
end;

procedure TlvkActiveScriptCodeEditor.SetSourceCode(const Value: string);
begin
  eScriptCode.Text := Value;
end;

procedure TlvkActiveScriptCodeEditor.alScriptEditorUpdate(
  Action: TBasicAction; var Handled: Boolean);
begin
  paCaretPosition.Caption := Format('%d:%d', [eScriptCode.CaretX, eScriptCode.CaretY]);
  acCompile.Enabled := eScriptCode.Lines.Count > 0;

  Handled := True;
end;

procedure TlvkActiveScriptCodeEditor.acCompileExecute(Sender: TObject);
var
  Script  : IlvkActiveScript;
begin
  Script := TlvkActiveScript.Create;
  Script.Language := FLanguage;
  Script.ScriptCode.Text := eScriptCode.Text;
  try
    Script.Compile;
  except
    on E: ElvkActiveScriptCompile do
    begin
      lblError.Caption := E.Message;
      eScriptCode.CaretX := E.CharPos;
      eScriptCode.CaretY := E.LineNo;
      eScriptCode.SetFocus;
    end;
  end;
end;

{ TScriptCodeEditor }

procedure TScriptCodeEditor.Edit;
var
  EditorForm  : TlvkActiveScriptCodeEditor;
begin
  EditorForm := TlvkActiveScriptCodeEditor.Create(nil);
  try
    EditorForm.Language := TlvkActiveScriptWrapper(GetComponent(0)).Language;
    EditorForm.SourceCode := TlvkActiveScriptWrapper(GetComponent(0)).ScriptCode.Text;
    EditorForm.Caption := TlvkActiveScriptWrapper(GetComponent(0)).Name + '.ScriptCode';
    if EditorForm.ShowModal = mrOk then
    begin
      TlvkActiveScriptWrapper(GetComponent(0)).ScriptCode.Text := EditorForm.SourceCode;
      TlvkActiveScriptWrapper(GetComponent(0)).Language := EditorForm.Language;
    end;
  finally
    EditorForm.Free;
  end;
end;

function TScriptCodeEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TlvkActiveScriptCodeEditor.paLanguageDropDown(Sender: TObject);
var
  Script  : IlvkActiveScript;
begin
  if paLanguage.Items.Count = 0 then
  begin
    Screen.Cursor := crHourGlass;
    try
      Script := TlvkActiveScript.Create;
      Script.GetInstalledLanguages(paLanguage.Items, False);
      paLanguage.ItemIndex := paLanguage.Items.IndexOf(FLanguage);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TlvkActiveScriptCodeEditor.paLanguageChange(Sender: TObject);
begin
  SetLanguage(paLanguage.Text);
end;

function TScriptCodeEditor.GetValue: string;
const
  Plural  : array[Boolean] of string = ('', 's');
begin
  Result := Format('(%d line%s)', [
    TlvkActiveScriptWrapper(GetComponent(0)).ScriptCode.Count,
    Plural[TlvkActiveScriptWrapper(GetComponent(0)).ScriptCode.Count <> 1]]);
end;

{ TScriptCodeModuleEditor }

procedure TScriptCodeModuleEditor.Edit;
var
  EditorForm  : TlvkActiveScriptCodeEditor;
begin
  EditorForm := TlvkActiveScriptCodeEditor.Create(nil);
  try
    EditorForm.Language := TlvkActiveScriptModuleCollection(
      TlvkActiveScriptModule(GetComponent(0)).Collection).ActiveScript.Language;
    EditorForm.SourceCode := TlvkActiveScriptModule(GetComponent(0)).ScriptCode.Text;
    EditorForm.Caption := TlvkActiveScriptModule(GetComponent(0)).DisplayName + '.ScriptCode';
    if EditorForm.ShowModal = mrOk then
    begin
      TlvkActiveScriptModule(GetComponent(0)).ScriptCode.Text := EditorForm.SourceCode;
      TlvkActiveScriptModuleCollection(
        TlvkActiveScriptModule(GetComponent(0)).Collection).ActiveScript.Language := EditorForm.Language;
    end;
  finally
    EditorForm.Free;
  end;
end;

function TScriptCodeModuleEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TScriptCodeModuleEditor.GetValue: string;
const
  Plural  : array[Boolean] of string = ('', 's');
begin
  Result := Format('(%d line%s)', [
    TlvkActiveScriptModule(GetComponent(0)).ScriptCode.Count,
    Plural[TlvkActiveScriptModule(GetComponent(0)).ScriptCode.Count <> 1]]);
end;

function TlvkActiveScriptCodeEditor.GetCaption: string;
begin
  Result := paFilename.Caption;
end;

procedure TlvkActiveScriptCodeEditor.SetCaption(const Value: string);
begin
  paFilename.Caption := Value;
  inherited Caption := 'Script Code Editor [' + Value + ']';
end;

{ TActiveScriptEditor }

procedure TActiveScriptEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TActiveScriptEditor.ExecuteVerb(Index: Integer);
var
  EditorForm  : TlvkActiveScriptCodeEditor;
begin
  case Index of
    0:
      begin
        EditorForm := TlvkActiveScriptCodeEditor.Create(nil);
        try
          EditorForm.Language := TlvkActiveScriptWrapper(Component).Language;
          EditorForm.SourceCode := TlvkActiveScriptWrapper(Component).ScriptCode.Text;
          EditorForm.Caption := Component.Name + '.ScriptCode';
          if EditorForm.ShowModal = mrOk then
          begin
            TlvkActiveScriptWrapper(Component).ScriptCode.Text := EditorForm.SourceCode;
            TlvkActiveScriptWrapper(Component).Language := EditorForm.Language;
          end;
        finally
          EditorForm.Free;
        end;
      end;
  end;
end;

function TActiveScriptEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Edit script code';
  else
    Result := '';
  end;
end;

function TActiveScriptEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
