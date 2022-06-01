{******************************************************************************}
{*                                                                            *}
{* (C) Copyright 1999-2002, Lasse Vågsæther Karlsen                           *}
{*                                                                            *}
{******************************************************************************}
unit EditRegularExpressionFM;

// $Author: Lasse V. Karlsen $
// $Date: 16.04.03 10:50 $
// $Revision: 6 $
// $Archive: /Components/LVK/source/EditRegularExpressionFM.pas $

interface

{$I VERSIONS.INC}
{$I DIRECTIVES.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, lvkRegExp, StdCtrls, Buttons, lvkEdits, lvkSpeedEdit,
  lvkDropDownEdit, TypInfo, ComCtrls, lvkSizeGrip, ActnList, lvkState,
  lvkFormState, lvkINIFileStateStorage;

type
  TfmEditRegularExpression = class(TForm)
    lblRegExp: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    lblOptions: TLabel;
    eOptions: TlvkDropDownEdit;
    pcTest: TPageControl;
    tsMatch: TTabSheet;
    lblMatch: TLabel;
    eMatch: TMemo;
    lblMatchResult: TLabel;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    eFind: TMemo;
    btFindFirst: TButton;
    btFindNext: TButton;
    lblFindResult: TLabel;
    eRegExp: TlvkDropDownEdit;
    alEditRegularExpression: TActionList;
    acFindFirst: TAction;
    acFindNext: TAction;
    lvkINIFileStateStorage1: TlvkINIFileStateStorage;
    lvkFormState1: TlvkFormState;
    procedure eOptionsAfterDropDown(Sender: TObject; const Frame: TFrame);
    procedure eOptionsBeforeCloseUp(Sender: TObject; const Frame: TFrame);
    procedure eMatchChange(Sender: TObject);
    procedure eOptionsAfterCloseUp(Sender: TObject);
    procedure alEditRegularExpressionUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure acFindFirstExecute(Sender: TObject);
    procedure acFindNextExecute(Sender: TObject);
    procedure eRegExpExit(Sender: TObject);
    procedure eRegExpCreateDropDownFrame(Sender: TObject;
      const AOwner: TComponent; out Frame: TFrame;
      var EditOwnsFrame: Boolean);
    procedure eOptionsCreateDropDownFrame(Sender: TObject;
      const AOwner: TComponent; out Frame: TFrame;
      var EditOwnsFrame: Boolean);
  private
    { Private declarations }
    FOptions  : TlvkRegExpOptions;

    function GetRegularExpression: string;
    procedure SetRegularExpression(const Value: string);
    function GetOptions: TlvkRegExpOptions;
    procedure SetOptions(const Value: TlvkRegExpOptions);
    function TrimMemo(const s: string): string;
    procedure UpdateState;
    procedure Find(const StartPosition: Integer);

  public
    { Public declarations }
    property RegularExpression: string read GetRegularExpression
      write SetRegularExpression;

  published
    property Options: TlvkRegExpOptions read GetOptions write SetOptions;
  end;

var
  fmEditRegularExpression: TfmEditRegularExpression;

implementation

uses RegularExpressionOptionsFRM, PresetRegularExpressionFRM;

{$R *.dfm}

{ TfmEditRegularExpression }

function TfmEditRegularExpression.GetOptions: TlvkRegExpOptions;
begin
  Result := FOptions;
end;

function TfmEditRegularExpression.GetRegularExpression: string;
begin
  Result := eRegExp.Text;
end;

procedure TfmEditRegularExpression.SetOptions(
  const Value: TlvkRegExpOptions);
begin
  FOptions := Value;
  eOptions.Text := GetSetProp(Self, 'Options', False);
end;

procedure TfmEditRegularExpression.SetRegularExpression(
  const Value: string);
begin
  eRegExp.Text := Value;
end;

procedure TfmEditRegularExpression.eOptionsAfterDropDown(Sender: TObject;
  const Frame: TFrame);
begin
  TfrmRegularExpressionOptions(Frame).Options := Options;
end;

procedure TfmEditRegularExpression.eOptionsBeforeCloseUp(Sender: TObject;
  const Frame: TFrame);
begin
  Options := TfrmRegularExpressionOptions(Frame).Options;
end;

function TfmEditRegularExpression.TrimMemo(const s: string): string;
begin
  Result := s;
  while (Result <> '') and (Result[Length(Result)] in [#13, #10]) do
    Delete(Result, Length(Result), 1);
end;

procedure TfmEditRegularExpression.eMatchChange(Sender: TObject);
begin
  if RegExpMatch(eRegExp.Text, TrimMemo(eMatch.Lines.Text), FOptions) then
    lblMatchResult.Caption := 'Matches!'
  else
    lblMatchResult.Caption := 'Does not match!';
end;

procedure TfmEditRegularExpression.UpdateState;
begin
  eMatchChange(Self);
  lblFindResult.Caption := '';
end;

procedure TfmEditRegularExpression.eOptionsAfterCloseUp(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmEditRegularExpression.Find(const StartPosition: Integer);
var
  re  : IRegExp;
begin
  if eFind.Text <> '' then
  begin
    re := NewRegExp(eRegExp.Text, FOptions);
    if re.MatchAgainst(eFind.Text, StartPosition, StartPosition) then
    begin
      lblFindResult.Caption := 'Found!';
      eFind.SelStart := re.Groups[0].StartPosition;
      eFind.SelLength := re.Groups[0].EndPosition - re.Groups[0].StartPosition;
    end else begin
      lblFindResult.Caption := 'Not found!';
      eFind.SelStart := 0;
      eFind.SelLength := 0;
    end;

    eFind.SetFocus;
  end;
end;

procedure TfmEditRegularExpression.alEditRegularExpressionUpdate(
  Action: TBasicAction; var Handled: Boolean);
begin
  acFindFirst.Enabled := (eRegExp.Text <> '') and (eFind.Lines.Count > 0);
  acFindNext.Enabled := (eRegExp.Text <> '') and (eFind.Lines.Count > 0) and
    (eFind.SelStart < Length(eFind.Lines.Text));

  Handled := True;
end;

procedure TfmEditRegularExpression.acFindFirstExecute(Sender: TObject);
begin
  Find(0);
end;

procedure TfmEditRegularExpression.acFindNextExecute(Sender: TObject);
begin
  Find(eFind.SelStart+1);
end;

procedure TfmEditRegularExpression.eRegExpExit(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmEditRegularExpression.eRegExpCreateDropDownFrame(
  Sender: TObject; const AOwner: TComponent; out Frame: TFrame;
  var EditOwnsFrame: Boolean);
begin
  Frame := TfrmPresetRegularExpression.Create(AOwner);
end;

procedure TfmEditRegularExpression.eOptionsCreateDropDownFrame(
  Sender: TObject; const AOwner: TComponent; out Frame: TFrame;
  var EditOwnsFrame: Boolean);
begin
  Frame := TfrmRegularExpressionOptions.Create(AOwner);
end;

end.
