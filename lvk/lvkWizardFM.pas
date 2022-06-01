{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}

{ Description:
    This unit contains the form used by the wizard component.
  See also:
    TlvkWizard
}
unit lvkWizardFM;

// $Author: Lasse V. Karlsen $
// $Revision: 5 $
// $Date: 16.04.03 10:51 $
// $Archive: /Components/LVK/source/lvkWizardFM.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ActnList, lvkWizard;

const
  CM_FIRSTFOCUS   = WM_USER + 1;

type
  TPageData = record
    Parent      : TWinControl;
    Left        : Integer;
    Top         : Integer;
    Width       : Integer;
    Height      : Integer;
    Align       : TAlign;
    BorderStyle : TBorderStyle;
    BevelOuter  : TBevelCut;
    BevelInner  : TBevelCut;
  end;

  TfmlvkWizard = class(TForm)
    alWizard: TActionList;
    acBack: TAction;
    acNext: TAction;
    acFinish: TAction;
    acCancel: TAction;
    WizardPanel: TPanel;
    paWizardArea: TPanel;
    paWizardHeader: TPanel;
    lblPageTitle: TLabel;
    lblPageSubtitle: TLabel;
    imgHeader: TImage;
    paWatermark: TPanel;
    paWizardButtons: TPanel;
    btCancel: TBitBtn;
    btFinish: TBitBtn;
    btNext: TBitBtn;
    btBack: TBitBtn;
    imgWatermark: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    stWatermark: TStaticText;
    stWizardArea: TStaticText;
    stWizard: TStaticText;
    procedure acFinishExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure alWizardUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure acBackExecute(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure paWizardAreaResize(Sender: TObject);
    procedure paWatermarkResize(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    FWizard       : TlvkWizard;
    FPage         : Integer;
    FPageData     : TPageData;

    FActionLists  : TList;

    procedure PreparePage(const PageIndex: Integer);
    procedure RestorePage(const PageIndex: Integer);

    procedure FindActionLists(const Panel: TPanel);

    procedure OnFirstFocus(var Msg: TMessage); message CM_FIRSTFOCUS;

  public
    property Wizard: TlvkWizard read FWizard write FWizard;
    function SetCurrentPage(const Page: Integer): Boolean;
  end;

var
  fmlvkWizard: TfmlvkWizard;

implementation

{$R *.dfm}

type
  TWizardPageCrack = class(TWizardPage);
  TWizardCrack = class(TlvkCustomWizard);

procedure TfmlvkWizard.acFinishExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfmlvkWizard.acCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmlvkWizard.FormShow(Sender: TObject);
var
  Index : Integer;
begin
  TWizardCrack(Wizard).DoShowWizard;

  Index := 0;
  while Index < Wizard.Pages.Count do
  begin
    if TWizardPageCrack(Wizard.Pages.Items[Index]).GetUsePage then
    begin
      SetCurrentPage(Index);
      Break;
    end;

    Inc(Index);
  end;
end;

procedure TfmlvkWizard.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  FActionLists := TList.Create;
  FPage := -1;

  FormResize(Sender);
end;

procedure TfmlvkWizard.FormDestroy(Sender: TObject);
begin
  SetCurrentPage(-1);
  FreeAndNil(FActionLists);
end;

procedure TfmlvkWizard.FormHide(Sender: TObject);
begin
  SetCurrentPage(-1);
  TWizardCrack(Wizard).DoCloseWizard;
end;

function TfmlvkWizard.SetCurrentPage(const Page: Integer): Boolean;
var
  AllowNext   : Boolean;
  AllowEnter  : Boolean;
begin
  if Page = FPage then
    Result := True
  else begin
    Result := False;

    if (FPage <> -1) and (Page > FPage) then
    begin
      TWizardPageCrack(Wizard.Pages.Items[FPage]).DoAllowNext(Wizard, FPage, AllowNext);
      if not AllowNext then
        Exit;
    end;
    if Page <> -1 then
    begin
      TWizardPageCrack(Wizard.Pages.Items[Page]).DoAllowEnter(Wizard, Page, AllowEnter);
      if not AllowEnter then
        Exit;
    end;

    if FPage <> -1 then
    begin
      TWizardPageCrack(Wizard.Pages.Items[FPage]).DoBeforeHide(Wizard, FPage);
      RestorePage(FPage);
      TWizardPageCrack(Wizard.Pages.Items[FPage]).DoAfterHide(Wizard, FPage);
    end;

    FPage := Page;

    if FPage <> -1 then
    begin
      TWizardPageCrack(Wizard.Pages.Items[FPage]).DoBeforeShow(Wizard, Page);
      PreparePage(FPage);
      TWizardPageCrack(Wizard.Pages.Items[FPage]).DoAfterShow(Wizard, Page);
    end;

    PostMessage(Handle, CM_FIRSTFOCUS, 0, 0);
  end;
end;

procedure TfmlvkWizard.PreparePage(const PageIndex: Integer);
var
  Panel : TPanel;
begin
  Panel := Wizard.Pages.Items[PageIndex].Page;

  FPageData.Parent := Panel.Parent;
  FPageData.Left := Panel.Left;
  FPageData.Top := Panel.Top;
  FPageData.Width := Panel.Width;
  FPageData.Height := Panel.Height;
  FPageData.Align := Panel.Align;
  FPageData.BorderStyle := Panel.BorderStyle;
  FPageData.BevelInner := Panel.BevelInner;
  FPageData.BevelOuter := Panel.BevelOuter;

  paWatermark.Visible := Wizard.Pages.Items[PageIndex].ShowWatermark;
  paWizardHeader.Visible := Wizard.Pages.Items[PageIndex].ShowHeader;

  Panel.BorderStyle := bsNone;
  Panel.BevelInner := bvNone;
  Panel.BevelOuter := bvNone;
  Panel.Align := alClient;
  FindActionLists(Panel);
  Panel.Parent := paWizardArea;
  TWizardCrack(Wizard).SetCurrentPage(PageIndex, Panel);

  lblPageTitle.Caption := Wizard.Pages.Items[PageIndex].PageTitle;
  lblPageSubtitle.Caption := Wizard.Pages.Items[PageIndex].PageSubtitle;
end;

procedure TfmlvkWizard.RestorePage(const PageIndex: Integer);
var
  Panel : TPanel;
begin
  FActionLists.Clear;

  Panel := Wizard.Pages.Items[PageIndex].Page;

  Panel.Align := FPageData.Align;
  Panel.Parent := FPageData.Parent;
  Panel.BorderStyle := FPageData.BorderStyle;
  Panel.BevelInner := FPageData.BevelInner;
  Panel.BevelOuter := FPageData.BevelOuter;
  Panel.SetBounds(FPageData.Left, FPageData.Top, FPageData.Width, FPageData.Height);
  TWizardCrack(Wizard).SetCurrentPage(PageIndex, nil);
end;

procedure TfmlvkWizard.alWizardUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  AllowNext   : Boolean;
  AllowFinish : Boolean;
  Index       : Integer;
begin
  for Index := 0 to FActionLists.Count-1 do
    TActionList(FActionLists[Index]).UpdateAction(nil);

  TWizardPageCrack(Wizard.Pages.Items[FPage]).DoAllowFinish(Wizard, FPage, AllowFinish);
  TWizardPageCrack(Wizard.Pages.Items[FPage]).DoAllowNext(Wizard, FPage, AllowNext);

  acBack.Enabled := (TWizardCrack(Wizard).GetNextPage(-1) <> -1);
  acNext.Enabled := AllowNext and (TWizardCrack(Wizard).GetNextPage(+1) <> -1);
  acFinish.Enabled := AllowNext and AllowFinish;

  Handled := True;
end;

procedure TfmlvkWizard.acBackExecute(Sender: TObject);
var
  Index : Integer;
begin
  Index := TWizardCrack(Wizard).GetNextPage(-1);

  if Index >= 0 then
    SetCurrentPage(Index);
end;

procedure TfmlvkWizard.acNextExecute(Sender: TObject);
var
  Index : Integer;
begin
  Index := TWizardCrack(Wizard).GetNextPage(+1);

  if Index >= 0 then
    SetCurrentPage(Index);
end;

procedure TfmlvkWizard.FindActionLists(const Panel: TPanel);
var
  Comp    : TComponent;
  Control : TControl;
  fm      : TForm;
  Index   : Integer;
begin
  Control := Panel.Parent;

  while Assigned(Control) and (not (Control is TForm)) do
    Control := Control.Parent;

  if Assigned(Control) then
  begin
    fm := TForm(Control);
    for Index := 0 to fm.ComponentCount-1 do
    begin
      Comp := fm.Components[Index];
      if Comp is TActionList then
        FActionLists.Add(Comp);
    end;
  end;
end;

procedure TfmlvkWizard.OnFirstFocus(var Msg: TMessage);
var
  Control : TWinControl;
begin
  Control := FindNextControl(nil, True, True, False);
  if Assigned(Control) then
    Control.SetFocus;
end;

procedure TfmlvkWizard.paWizardAreaResize(Sender: TObject);
begin
  if paWatermark.Visible then
    stWizardArea.Left := paWatermark.Width + 1
  else
    stWizardArea.Left := 1;

  if paWizardHeader.Visible then
    stWizardArea.Top := paWizardHeader.Height + 1
  else
    stWizardArea.Top := 1;
  stWizardArea.Caption := Format('%d x %d', [paWizardArea.Width, paWizardArea.Height]);
end;

procedure TfmlvkWizard.paWatermarkResize(Sender: TObject);
begin
  stWatermark.Caption := Format('%d x %d', [paWatermark.Width, paWatermark.Height]);
end;

procedure TfmlvkWizard.FormResize(Sender: TObject);
begin
  stWizard.Caption := Format('%d x %d', [Width, Height]);
end;

end.
