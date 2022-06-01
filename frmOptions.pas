
unit frmOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ButtonWithColor, common, configsLoader, constantes,
  Spin;

type
  TframeOptions = class(TFrame)
    GroupBox2: TGroupBox;
    edtConPort: TSpinEdit;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    AutoGetUpd: TCheckBox;
    AutoUpd: TRadioButton;
    NOUpd: TRadioButton;
    ShowUpd: TRadioButton;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    cbShowHidden: TCheckBox;
    boxSelectAuto: TCheckBox;
    boxProtegido: TCheckBox;
    procedure btnCancelaClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure AutoGetUpdClick(Sender: TObject);
    procedure AutoUpdClick(Sender: TObject);
    procedure ShowUpdClick(Sender: TObject);
    procedure NOUpdClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SaveOptions;
    procedure LoadOptions;
    Procedure Reload;
  end;

  var tmpUpdMd: integer;

implementation

{$R *.dfm}

procedure TframeOptions.LoadOptions;
var i:integer;
begin
{
  if not FileExists(CMDir+Constante(0066, false)) then
  begin
     SaveOptions;
     exit;
  end;   }

  try
    //boxProtegido.Checked := LoadConfig('ModoProtegido','true').AsBoolean;
    //AutoGetUpd.Checked := LoadConfig('AutoCheckUpdate','false').AsBoolean;
    //boxSelectAuto.Checked := LoadConfig('AutoSelect').AsBoolean;
    edtConPort.value := 8080; //LoadConfig('connectionport','8080').AsInteger;
    AutoGetUpd.Enabled := AutoGetUpd.Checked;
    ShowUpd.Enabled := AutoGetUpd.Checked;
    NoUpd.Enabled := AutoGetUpd.Checked;

    AutoGetUpd.Checked := AutoGetUpd.Checked;
    NoUpd.Checked := AutoGetUpd.Checked;
    ShowUpd.Checked := AutoGetUpd.Checked;

    AutoCheckUpdate := AutoGetUpd.Checked; 
    if AutoCheckUpdate then
    begin
       { i := LoadConfig('UpdateMode').AsInteger;
        case i of
          1 : }AutoUpd.Checked := true;
          {2 : ShowUpd.Checked := true;
          3 : NoUpd.Checked := true;
        end;              }
    end;

  except
    on e:exception do
  end;

  Reload;
end;

procedure TframeOptions.Reload;
begin
    AutoCheckUpdate := AutoGetUpd.Checked;
    if AutoUpd.Checked then UpdateMode := 1;
    if ShowUpd.Checked then UpdateMode := 2;
    if NoUpd.Checked then UpdateMode := 3;
end;

procedure TframeOptions.SaveOptions;
begin
{
  WriteConfig('AutoCheckUpdate', BoolToStr( AutoGetUpd.Checked ));
  WriteConfig('UpdateMode', intToStr( tmpUpdMd ));
  WriteConfig('ConnectionPort', IntToStr( edtConPort.Value ));
  WriteConfig('ShowHidden', BoolToStr( cbShowHidden.Checked ));
  WriteConfig('AutoSelect', BoolToStr( boxSelectAuto.Checked ));
  WriteConfig('ModoProtegido', BoolToStr( boxProtegido.Checked ));
 }
  AutoCheckUpdate := true; // AutoGetUpd.Checked;
  UpdateMode := tmpUpdMd;
  ConnectionPort := edtConPort.Value;
end;

procedure TframeOptions.btnCancelaClick(Sender: TObject);
begin
  Hide;
end;

procedure TframeOptions.btnOKClick(Sender: TObject);
begin
  SaveOptions;
  Reload;
  Hide;
end;

procedure TframeOptions.AutoGetUpdClick(Sender: TObject);
begin
   NoUpd.Enabled := AutoGetUpd.Checked;
   AutoUpd.Enabled := AutoGetUpd.Checked;
   ShowUpd.Enabled := AutoGetUpd.Checked;

   NoUpd.Checked := AutoGetUpd.Checked;
   AutoUpd.Checked := AutoGetUpd.Checked;
   ShowUpd.Checked := AutoGetUpd.Checked;

   if not AutoGetUpd.Checked then tmpUpdMd := 0;
end;

procedure TframeOptions.AutoUpdClick(Sender: TObject);
begin
  tmpUpdMd := 1;
end;

procedure TframeOptions.ShowUpdClick(Sender: TObject);
begin
   tmpUpdMd := 2;
end;

procedure TframeOptions.NOUpdClick(Sender: TObject);
begin
  tmpUpdMd := 3;
end;

end.
