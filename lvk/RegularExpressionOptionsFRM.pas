unit RegularExpressionOptionsFRM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, lvkRegExp, TypInfo, lvkCheckBox;

type
  TfrmRegularExpressionOptions = class(TFrame)
    chkGreedy: TlvkCheckBox;
    chkCaseSensitive: TlvkCheckBox;
    chkMultiLine: TlvkCheckBox;
    chkDotAll: TlvkCheckBox;
    chkDollarEndOnly: TlvkCheckBox;
    chkBOL: TlvkCheckBox;
    chkEOL: TlvkCheckBox;
    chkEmptyValid: TlvkCheckBox;
    chkAnchored: TlvkCheckBox;
  private
    { Private declarations }
    function GetOptions: TlvkRegExpOptions;
    procedure SetOptions(const Value: TlvkRegExpOptions);

  public
    { Public declarations }
    property Options: TlvkRegExpOptions read GetOptions write SetOptions;
  end;

implementation

{$R *.dfm}

{ TfrmRegularExpressionOptions }

function TfrmRegularExpressionOptions.GetOptions: TlvkRegExpOptions;
var
  Option    : TlvkRegExpOption;
  CheckBox  : TCheckBox;
begin
  Result := [];
  for Option := Low(TlvkRegExpOption) to High(TlvkRegExpOption) do
  begin
    CheckBox := TCheckBox(FindComponent('chk' + Copy(
      GetEnumName(TypeInfo(TlvkRegExpOption), Ord(Option)), 3, 255)));

    Assert(Assigned(CheckBox), 'No checkbox for ' + GetEnumName(TypeInfo(TlvkRegExpOption), Ord(Option)));

    if CheckBox.Checked then
      Include(Result, Option);
  end;
end;

procedure TfrmRegularExpressionOptions.SetOptions(
  const Value: TlvkRegExpOptions);
var
  Option    : TlvkRegExpOption;
  CheckBox  : TCheckBox;
begin
  for Option := Low(TlvkRegExpOption) to High(TlvkRegExpOption) do
  begin
    CheckBox := TCheckBox(FindComponent('chk' + Copy(
      GetEnumName(TypeInfo(TlvkRegExpOption), Ord(Option)), 3, 255)));

    Assert(Assigned(CheckBox), 'No checkbox for ' + GetEnumName(TypeInfo(TlvkRegExpOption), Ord(Option)));

    CheckBox.Checked := Option in Value;
  end;
end;

end.
