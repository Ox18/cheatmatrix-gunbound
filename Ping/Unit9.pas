unit Unit9;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, IdIntercept, IdBaseComponent, IdAntiFreezeBase,
  IdAntiFreeze, IdComponent, IdTCPConnection, IdTCPClient, IdFinger, IdRawBase,
  IdRawClient, IdIcmpClient, ping;

type
  TForm9 = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    IdAntiFreeze1: TIdAntiFreeze;
    Timer1: TTimer;
    Timer2: TTimer;
    Button1: TButton;
    IdIcmpClient1: TIdIcmpClient;
    TrayIcon1: TTrayIcon;
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure OnMinimize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form9: TForm9;
  totalFalhas: integer = 0;
  iniciado: boolean = false;

const
  ADP_IP = 'www.google.com';

implementation

{$R *.dfm}

procedure TForm9.Button1Click(Sender: TObject);
begin
  Windows.Beep(1000, 800);
end;

procedure TForm9.OnMinimize;
begin
  hide;
end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  hide;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  ListBox1.Items.Add('Pingando ' + ADP_IP);
  Application.OnMinimize := OnMinimize;
end;

procedure TForm9.FormHide(Sender: TObject);
begin
  TrayIcon1.Visible := true;

end;

procedure addLog(texto: String);
var
  SomeTxtFile: TextFile;
  arquivo: string;
begin
  try
    arquivo := ExtractFilePath(ParamStr(0)) + 'CMPing.log';
    AssignFile(SomeTxtFile, arquivo);
    if (FileExists(arquivo)) then
      Append(SomeTxtFile)
    else
      Rewrite(SomeTxtFile);
    WriteLn(SomeTxtFile, texto);
  finally
    CloseFile(SomeTxtFile);
  end;
end;

procedure TForm9.Timer1Timer(Sender: TObject);
var
  res, s: String;
  status: boolean;
  // '64.233.163.104';//'208.185.127.40'; (* http://delphi.about.com *)
begin
  try
    IdIcmpClient1.Host := ADP_IP;
    IdIcmpClient1.ping();
    status := (IdIcmpClient1.ReplyStatus.ReplyStatusType = rsEcho);
  except
    on e: Exception do
      status := false;
  end;

  res := FormatDateTime('dd/mm/yy hh:nn:ss', NOW);
  If { Pingar(ADP_IP) } status then
  begin
    // ListBox1.Items.Add(res + ' - Ping OK');
    totalFalhas := 0;
  end
  else
  begin
    addLog(res + ' - Erro no ping');
    ListBox1.Items.Add(res + ' - Erro no ping');
    inc(totalFalhas);
  end;

  { IdIcmpClient1.Host := 'www.googlefer.com';
    IdIcmpClient1.Ping();
    res := IdIcmpClient1.ReplyData;
    ListBox1.Items.Add(res); }
end;

procedure TForm9.Timer2Timer(Sender: TObject);
begin
  if totalFalhas > 5 then
  begin
    Panel1.Caption := 'FALHA';
    Panel1.Font.Color := clWhite;
    Panel1.Color := clRed;
    Windows.Beep(1000, 800);
  end
  else if totalFalhas > 0 then
  begin
    Panel1.Caption := 'ALERTA';
    Panel1.Font.Color := clRed;
    Panel1.Color := $0004D5CF;
  end
  else
  begin
    Panel1.Caption := 'OK';
    Panel1.Font.Color := clWhite;
    Panel1.Color := clGreen;
  end;
end;

procedure TForm9.TrayIcon1DblClick(Sender: TObject);
begin
  Application.Restore;
  ShowWindow(Application.Handle, SW_RESTORE);
  show;
  BringToFront;
  Application.BringToFront;
end;

end.
