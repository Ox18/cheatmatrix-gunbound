unit PresetRegularExpressionFRM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, lvkComponents, lvkDoubleBuffered;

type
  TfrmPresetRegularExpression = class(TFrame)
    lbExpressions: TListBox;
    Label1: TLabel;
    lvkDoubleBuffered1: TlvkDoubleBuffered;
    procedure lbExpressionsDblClick(Sender: TObject);
  private
    { Private declarations }
    FExpressions  : TStrings;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  lvkDropDownEdit, lvkValidators;

{$R *.dfm}

{ TfrmPresetRegularExpression }

constructor TfrmPresetRegularExpression.Create(AOwner: TComponent);
var
  Kind  : TlvkRegularExpressionValidatorKind;
begin
  inherited;

  FExpressions := TStringList.Create;
  for Kind := Low(TlvkRegularExpressionValidatorKind) to High(TlvkRegularExpressionValidatorKind) do
  begin
    if Kind <> rkCustom then
    begin
      FExpressions.Add(RegularExpressionValidatorKindExpressions(Kind));
      lbExpressions.Items.Add(RegularExpressionValidatorKindNames[Kind]);
    end;
  end;
  lbExpressions.ItemIndex := -1;
end;

destructor TfrmPresetRegularExpression.Destroy;
begin
  FExpressions.Free;

  inherited;
end;

procedure TfrmPresetRegularExpression.lbExpressionsDblClick(
  Sender: TObject);
begin
  TlvkDropDownEdit(Owner).Text := FExpressions[lbExpressions.ItemIndex];
  TlvkDropDownEdit(Owner).CloseUp;
end;

end.
