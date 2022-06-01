unit SplashScreen;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	ShellApi, ScktComp, ExtCtrls, jpeg, Gauges, Menus,
	IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, StdCtrls;

type
	TSplash = class(TForm)
		painelAtivacao: TPanel;
		campoSenha: TEdit;
		Label3: TLabel;
		Label2: TLabel;
		campoUser: TEdit;
		Label1: TLabel;
		campoCodigo: TEdit;
		Panel2: TPanel;
		Shape1: TShape;
		Panel4: TPanel;
		Shape2: TShape;
		lblSplashStatus: TLabel;
		Panel1: TPanel;
		Image1: TImage;
		pParcial: TGauge;
		geralBar: TGauge;
		Timer1: TTimer;
		PopupMenu1: TPopupMenu;
		Cancelar1: TMenuItem;
		TimerAnime: TTimer;
		Label4: TLabel;
		Panel3: TPanel;
		IdHTTP2: TIdHTTP;
		Panel5: TPanel;
		pTotal: TGauge;
		procedure Cancelar1Click(Sender: TObject);
		// procedure Timer1Timer(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure TimerAnimeTimer(Sender: TObject);
		procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure Panel2Click(Sender: TObject);
		procedure Label4Click(Sender: TObject);
		procedure IdHTTP2Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
		procedure IdHTTP2WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
		procedure IdHTTP2WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Panel3Click(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		tini, tfim, tres: Integer;
		procedure SocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
	end;

Procedure ChangeSplash(Texto: AnsiString);
procedure SetProgress(index: Integer; valor: cardinal);
procedure telaAtivacao;

var
	Splash: TSplash;
	SplashAtivar: TSplash;
	totalBaixados, totalAtual, totalGeral: Int64;

implementation

{$R *.dfm}

uses constantes, Inicializador;

procedure Delay(dwMilliseconds: Longint);
var
	iStart, iStop: DWORD;
begin
	iStart := GetTickCount;
	repeat
		iStop := GetTickCount;
		Application.ProcessMessages;
		Sleep(1); // addition from Christian Scheffler to avoid high CPU last
	until (iStop - iStart) >= dwMilliseconds;
end;

procedure TSplash.SocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
	//ChangeSplash(constante(0009,false));
	ErrorCode := 0;
    //delay(3000);
   // ExitProcess(0);
end;

procedure SetProgress(index: Integer; valor: cardinal);
begin
	if index = 1 then
		Splash.pParcial.Progress := valor;
	if index = 2 then
		Splash.pTotal.Progress := valor;
end;

// uses UnitMatriz;

{ procedure TSplash.Timer1Timer(Sender: TObject);
  begin
  lblSplashStatus.Caption := SplashStatus;
  lblSplashStatus.Left:= (Splash.Width-Label1.Width) div 2;
  lblSplashStatus.Refresh;
  Update;
  end; }

Procedure ChangeSplash(Texto: AnsiString);
begin
	Splash.lblSplashStatus.Caption := Texto;
	Splash.lblSplashStatus.Left := (Splash.Width - Splash.lblSplashStatus.Width) div 2;
	Splash.lblSplashStatus.Refresh;
	Splash.Update;
end;

Procedure ExitCM;
var
    SelfProc: THandle;
begin
    try
        SelfProc := OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId);
        TerminateProcess(SelfProc, 0);
        closehandle(SelfProc);
    except
        on e: exception do
        begin
        end;
    end;
end;

procedure TSplash.Cancelar1Click(Sender: TObject);
begin
	ExitCM;
	//ExitProcess(0);
end;

procedure TSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caFree;
end;

procedure TSplash.FormCreate(Sender: TObject);
var
	diretorio: AnsiString;
begin
	DoubleBuffered := true;
	painelAtivacao.DoubleBuffered := true;
	Panel4.DoubleBuffered := true;
	tini := GetTickCount;
end;

procedure TSplash.IdHTTP2Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
	if (AWorkMode = wmRead) and (totalGeral > 0) then
	begin
		totalBaixados := AWorkCount;
		Splash.pTotal.Progress := AWorkCount + totalAtual;
		Splash.pParcial.Progress := AWorkCount;
	end;
end;

procedure TSplash.IdHTTP2WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
	if (AWorkMode = wmRead) and (totalGeral > 0) then
	begin
		Splash.pParcial.Progress := 0;
		Splash.pParcial.MaxValue := AWorkCountMax;
		Splash.pTotal.MaxValue := totalGeral;
	end;
end;

procedure TSplash.IdHTTP2WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
	if (AWorkMode = wmRead) and (totalGeral > 0) then
	begin
		Splash.pTotal.Progress := totalBaixados + totalAtual;
		totalAtual := totalAtual + Splash.pParcial.MaxValue;
		Splash.pParcial.Progress := Splash.pParcial.MaxValue;
	end;
end;

procedure TSplash.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
	SC_DragMove = $F012; { a magic number }
begin
	ReleaseCapture;
	Splash.Perform(WM_SysCommand, SC_DragMove, 0);
end;

procedure TSplash.Label4Click(Sender: TObject);
var
	p: pchar;
	pansi: AnsiString;
begin
	pansi := (Constante(0064, false)) + (Constante(0090, false));
	p := CMPchar(pansi);
	ShellExecute(0, nil, p, '', '', 0);
end;

procedure TSplash.Panel2Click(Sender: TObject);
begin
	// Enviar dados
	ChangeSplash((Constante(0012, true)));
	ativarCMX(campoUser.Text, campoSenha.Text, campoCodigo.Text);
end;

procedure TSplash.Panel2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	TPanel(Sender).BevelInner := bvLowered;
end;

procedure TSplash.Panel2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	TPanel(Sender).BevelInner := bvRaised;
end;

procedure TSplash.Panel3Click(Sender: TObject);
var
    SelfProc: THandle;
    nNome: AnsiString;
    s: AnsiString;
begin
    try
        // MatrizInfo.Ligado := false;
        //ExitProcess(0);
        SelfProc := OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId);
        TerminateProcess(SelfProc, 0);
        closehandle(SelfProc);
    except
        on e: exception do
        begin
        end;
    end;
end;

var
	mostrarTela: boolean = false;

const
	ativacaoATop: Integer = 144;
	ativacaoDTop: Integer = 4;
	ativacaoLeft: Integer = 12;

procedure telaAtivacao;
begin
	mostrarTela := not mostrarTela;
end;

procedure TSplash.TimerAnimeTimer(Sender: TObject);
begin
	if mostrarTela then
	begin

		if (not painelAtivacao.Visible) then
		begin
			Height := 300;
			painelAtivacao.Visible := true;
		end
		else
		begin
			if (painelAtivacao.Top < ativacaoATop) then
			begin
				painelAtivacao.Top := painelAtivacao.Top + 4;
			end;
		end;
	end
	else
	begin
		if (painelAtivacao.Top > ativacaoDTop) then
		begin
			painelAtivacao.Top := painelAtivacao.Top - 4;
		end
		else
		begin
			if painelAtivacao.Visible then
				painelAtivacao.Visible := false;
			Height := 150;
		end;
	end;
end;

end.

