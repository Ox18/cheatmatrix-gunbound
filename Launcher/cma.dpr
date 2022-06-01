library cma;

(* **********************************************
  *                                             *
  *  Modulo de conexão / autenticação	        *
  *                                             *
  ********************************************** *)

uses
	SysUtils,
	windows,
	Classes,
	Inicializador in 'Inicializador.pas',
	functions in 'functions.pas',
	SplashScreen in 'SplashScreen.pas' { Splash } ,
	constantes in '..\constantes.pas';
{$R *.res}

function f1(valor: PDWORD; lpDiretorioBase: PAnsiChar; tempoVar: PPAnsiChar): POINTER; stdcall;
var
	i, n: integer;
	s: AnsiString;
begin
	result := nil;
	if valor <> nil then
	begin
		pDiretorioPlugin := StrNew(lpDiretorioBase);
		diretorioPlugin := AnsiString(pDiretorioPlugin);
		@Receptor := valor;
		n := 2;
		while n = 2 do
		begin
			n := inicializar;
			Sleep(100);
		end;

		if (n = 1) then
		begin
			tempoVar^ := GetMemory(Length(stringTempo) + 1);
			CopyMemory(tempoVar^, PAnsiChar(stringTempo), Length(stringTempo) + 1);

			for i := 0 to Length(PluginsLiberados) - 1 do
			begin
				s := s + PluginsLiberados[i];
				if (i < (Length(PluginsLiberados) - 1)) then
					s := s + AnsiString('|');
			end;
			result := GetMemory(Length(s) + 1);
			CopyMemory(result, PAnsiChar(s), Length(s) + 1);
		end;
	end;
end;

exports f1;

var
	nSize: integer;

begin

	{ pDiretorioPlugin := GetMemory($104);
	  nSize := GetModuleFileNameA(HInstance, pDiretorioPlugin, $104);
	  GetModuleFileNameA(HInstance, pDiretorioPlugin, nSize + 2);
	  pathPlugin := AnsiString(pDiretorioPlugin);
	  diretorioPlugin := ExtractFilePath(pathPlugin);
	  OutputDebugStringA(PAnsiChar(AnsiString('F1 em ' + IntToHex(cardinal(@f1), 8))));
	  }
end.
