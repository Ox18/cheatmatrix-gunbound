/* *********************************************
 *                                             *
 *  Modulo central de execução do CMX          *
 *                                             *
 ********************************************** */

#pragma hdrstop
#include "suporte.h"
// ---------------------------------------------------------------------------
char *keyMutex = "mdlcmmtx";
char *cmxON = "CMX";
HINSTANCE g_hInstance = NULL;
HBRUSH g_splashbrush = NULL;

HWND CMDOK;
HANDLE closeFlag;
HANDLE *threads;
char *DLLPath;
char *diretorioRaiz;
char *DLLPathAnterior;
char *DLLPath1;

void inicializarDLL();
DWORD WINAPI ThreadAtalhos(LPVOID lpParam);

BYTE diferencial = 0;

void mutacao(char *valor) {
	if (diferencial == 0) {
		diferencial = random(50) + 30;
	}

	if (valor != NULL && strlen(valor) > 0) {
		DWORD oldProtection;
		VirtualProtect(valor, strlen(valor), PAGE_EXECUTE_READWRITE, &oldProtection);
		for (int i = 0; i < (strlen(valor)); i++)
			valor[i] = (((BYTE)valor[i]) & 0xFF) ^ diferencial;
		VirtualProtect(valor, strlen(valor), oldProtection, &oldProtection);
	}
}

bool MyExtractFilePath(char* valor, char *buffer) {
	if (strlen(valor) == 0)
		return false;

	int i = 0;
	for (i = (strlen(valor) - 2); i > 0; i--) {
		if (char(valor[i]) == 92) {
			// buffer = (char *)malloc(i+2);
			memcpy(buffer, valor, i + 1);
			buffer[i + 1] = 0;
			return true;
		}
	}
	memcpy(buffer, valor, i + 1);
	return true;
}

/*
void getDiretorioAnterior(char *origem, char**destino) {
int indice = 0;
if (origem && strlen(origem) > 3) {
for (int i = (strlen(origem) - 2); i > 0; i--) {
if (origem[i] == '\\') {
int len = i + 1;
if (destino) {
 *destino = (char*)malloc(len + 1);
memset(*destino, 0, len + 1);
memcpy(*destino, origem, len);
return;
}
}
}
}
} */

#pragma argsused

void inicializarDLL() {
	static bool inicializado;
	if (!inicializado) {
		inicializado = true;

		closeFlag = CreateEvent(NULL, TRUE, FALSE, NULL);

		threads = new HANDLE[1];
		threads[0] = CreateThread(NULL, 0, ThreadAtalhos, NULL, 0, NULL);
		SetThreadContext(threads[0], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
	}
}

void ExitCM(){
	HANDLE SelfProc;
	__try{
		SelfProc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
        TerminateProcess(SelfProc, 0);
		CloseHandle(SelfProc);
	}__except(1){
	}
}

DWORD WINAPI ThreadAtalhos(LPVOID lpParam) {
	DWORD ultimoAtalho = 0;
	DWORD momentoUltimoAtalho = 0;

	while (WaitForSingleObject(closeFlag, 50) != WAIT_OBJECT_0) {
		__try {
			int modAC = (((GetKeyState(18) & 0x80) == 0x80) << 1) + ((GetKeyState(17) & 0x80) == 0x80);
			int mod = (modAC << 1) + ((GetKeyState(16) & 0x80) == 0x80);
			bool encontrado = false;
			ATALHO atalho;
			atalho.mods = 2;
			atalho.tecla = 46;
			if (atalho.mods == mod && (GetKeyState(atalho.tecla) & 0x80) == 0x80) {
				encontrado = true;
				int numeroAtalho = ((mod << 13) + atalho.tecla);

				if (ultimoAtalho != numeroAtalho)
					momentoUltimoAtalho = GetTickCount();

				if (ultimoAtalho != numeroAtalho || (GetTickCount() - momentoUltimoAtalho) > 800) {
					ultimoAtalho = numeroAtalho;
					__try {
						ExitCM();
					} __except (1) {
					}
				}
			}
			if (!encontrado) {
				ultimoAtalho = 0;
				momentoUltimoAtalho = GetTickCount();
			}
		} __except (1) {
		}
	}
}

LRESULT CALLBACK WndProc(HWND hwnd,
	// "handle" to the window that this message is for
	UINT message,
	// TYPE of message (e.g. WM_PAINT is a message asking to paint the window)
	WPARAM wparam, // information about the actual message
	LPARAM lparam) // MORE info about the message
{
	switch(message) {
	case WM_CREATE: {
			if (CMDOK) {
				break;
			}

			// CMDOK = CreateWindow("Button", "&sair", WS_CHILD | WS_VISIBLE, 25, 35, 40, 25, hwnd, (HMENU)IDOK, g_hInstance, 0);
			break;
		}

	case WM_PAINT: {
			// we would place our Windows painting code here.
			HDC hdc;
			PAINTSTRUCT ps;
			hdc = BeginPaint(hwnd, &ps);

			int bk = GetBkMode(hdc);
			SetBkMode(hdc, TRANSPARENT);

			HFONT Font;
			HFONT oldFont;
			// Font = CreateFont(36, 10, 0, 0, FW_DONTCARE, FALSE, TRUE, FALSE, DEFAULT_CHARSET, OUT_OUTLINE_PRECIS, CLIP_DEFAULT_PRECIS, ANTIALIASED_QUALITY, VARIABLE_PITCH, TEXT("Arial"));
			CreateFont(14, 7, 0, 0, 4, 0, FALSE, FALSE, 0, 0, 0, DEFAULT_QUALITY, 0, "Arial");
			oldFont = (HFONT)SelectObject(hdc, Font);

			DWORD cor;
			cor = GetTextColor(hdc);
			SetTextColor(hdc, 0xFFFFFF);

			__try {
				mutacao(cmxON);
				TextOut(hdc, 25, 12, cmxON, 3);
				mutacao(cmxON);
			}__finally {
				SelectObject(hdc, oldFont);
				SetTextColor(hdc, cor);
				DeleteObject(oldFont);
				DeleteObject(Font);
				SetBkMode(hdc, bk);
			}

			// draw a circle and a 2 squares
			// Ellipse(hdc, 20, 20, 160, 160);
			// Rectangle(hdc, 50, 50, 90, 90);
			// Rectangle(hdc, 100, 50, 140, 90);

			EndPaint(hwnd, &ps);
		}return 0;
		break;
		/* case WM_COMMAND: {

		switch(HIWORD(wparam)) {
		case BN_CLICKED:
		switch(LOWORD(wparam)) {
		case IDOK: {
		// THIS IS THE CODE THAT DOES THE RESHAPING !!!!!
		HRGN WinRgn; // Create RGN
		WinRgn = CreateRoundRectRgn(0, 0, 278, 378, 363, 363); // Specify shape
		SetWindowRgn(hwnd, WinRgn, true); // Apply it to our main window

		// MessageBox(hwnd, "Window RGN Set, To exit, click on the window.", sAppName, MB_OK);
		ShowWindow(CMDOK, 0); // Hide The "Set Window RGN" Command BUtton
		UpdateWindow(hwnd); // Redraw (Just in  case!)
		break;
		}
		}
		break;
		}
		} */

	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
		break;

	}
	return DefWindowProc(hwnd, message, wparam, lparam);
}

char* gerarNomeRandomico(int len) {
	randomize();
	char *nome = (char*)malloc(len + 1);
	memset(nome, 0, len + 1);
	for (int i = 0; i < len; i++) {
		nome[i] = random(23) + (((random(50) % 2) == 0) ? 'a' : 'A');
	}
	return nome;
}

void ProcessarComandos(DWORD* valor, DWORD size) {
	//debugar("Processando %d [%d]", valor, size);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR szCmdLine, int iCmdShow) {
	mutacao(cmxON);
	mutacao(keyMutex);

	char *nomeAutenticador = 0;
	char *nomeMatriz = 0;
	char *nomeMonitor = 0;
	char *nomeConfigurador = 0;

	__try {
		HMODULE dll = 0;
		mutacao(keyMutex);
		HANDLE hMutex = OpenMutex(MUTEX_ALL_ACCESS, 0, keyMutex);
		mutacao(keyMutex);

		if (!hMutex) {
			mutacao(keyMutex);
			hMutex = CreateMutex(0, 0, keyMutex);
			mutacao(keyMutex);
		} else {
			ExitCM();
		}
	} __except (1) {
		ExitCM();
	}

	LPSTR linha = GetCommandLine();
	//debugar("Param - %s", linha);
	linha = strstr(linha,"||") + 2;

	char* pch;
	int i = -1;
	while((pch = strchr(linha,'|')) != NULL){
		i++;
		//pch = strchr(pch+1,'|');
		int len = (pch - linha + 1);

		char* temp = (char*)malloc(len);
		memset(temp, 0, len);
		memcpy(temp, linha, len-1);

        switch(i) {
			case 0:
				diretorioRaiz = temp;
				//debugar("Param %d - %s", i , temp);
				break;
			case 1:
				nomeAutenticador = temp;
				//debugar("Param %d - %s", i , temp);
				break;
			case 2:
				nomeMatriz = temp;
				//debugar("Param %d - %s", i , temp);
				break;
			case 3:
				nomeMonitor = temp;
				//debugar("Param %d - %s", i , temp);
				break;
			case 4:
				nomeConfigurador = temp;
				//debugar("Param %d - %s", i , temp);
				break;
			default:
				break;
		}

		linha += len;
	}

   /*
	debugarArray(linha, strlen(linha));
	char *parametro = strtok(linha, "\34 ");
	int numeroParametros = 0;
	for (int i = 0; (parametro != NULL && i < 100); i++, numeroParametros++) {
		parametro = strtok(NULL, "|");
		switch(i) {
		case 0:
			diretorioRaiz = strdup(parametro);
			debugar("Param %d - %s", i , parametro);
			break;
		case 1:
			nomeAutenticador = strdup(parametro);
			debugar("Param %d - %s", i , parametro);
			break;
		case 2:
			nomeMatriz = strdup(parametro);
			debugar("Param %d - %s", i , parametro);
			break;
		case 3:
			nomeMonitor = strdup(parametro);
			debugar("Param %d - %s", i , parametro);
			break;
		case 4:
			nomeConfigurador = strdup(parametro);
			debugar("Param %d - %s", i , parametro);
			break;
		default:
			break;
		}
	}
	*/

	if (i < 4)
		ExitCM();
	/*
	int len = GetCurrentDirectoryA(NULL, NULL);
	DLLPath = (char*)malloc(len + 3);
	GetCurrentDirectoryA(len + 1, DLLPath);
	strcat(DLLPath, "\\");
	debugar("Diretorio atual: %s", DLLPath);   */

	char *DLLPath1 = (char *)malloc(MAX_PATH);
	DLLPath = (char *)malloc(MAX_PATH);
	GetModuleFileNameA(hInstance, &DLLPath1[0], MAX_PATH);
	MyExtractFilePath(DLLPath1, DLLPath);
	free(DLLPath1);
	//debugar("Diretorio atual: %s", DLLPath);

	void *callbackMonitor = 0;


	//
	// Carrega o modulo de monitoramento/injeção. Retorna o ponteiro para
	// o metodo Processar, para que o Autenticador passe os dados
	char *pathMonitor = strdup(DLLPath);
	__try {
		//debugar("Monitor: %s", pathMonitor);
		pathMonitor = (char*)realloc(pathMonitor, strlen(pathMonitor) + strlen(nomeMonitor) + 1);
		strcat(pathMonitor, nomeMonitor);
		HMODULE moduloMonitor = LoadLibraryA(pathMonitor);
		if (moduloMonitor) {
			typedef void*(__stdcall PASCAL * TCallBack)(char*);
			TCallBack callBack = (TCallBack)GetProcAddress(moduloMonitor, "f1");
			if (callBack) {
				//debugar("Step01");
				callbackMonitor = callBack(diretorioRaiz);
				//debugar("Step02");
			}
		} else {
			ExitCM();
		}
	}__finally {
		free(pathMonitor);
	}

	//debugar("Step2");
	if (!callbackMonitor) {
		ExitCM();
	}
	//debugar("Step3");
	//--------------------------------------------------------------
 /*
	char *nomeSpi = "plugins\\spi.pl";
	char *pathSpi = strdup(DLLPath);
	pathSpi = (char*)realloc(pathSpi, strlen(pathSpi) + strlen(nomeSpi) + 1);
	strcat(pathSpi, nomeSpi);

	//
	// Carrega o modulo autenticador e Autentica
	HMODULE moduloSpi = LoadLibraryA(pathSpi);
	__try {
		if (moduloSpi) {
			typedef struct METODOS {
				PROC iniciar;
				PROC processarComando;
				PROC atualizarEnderecos;
				PROC processarAtalho;
				PROC draw;
				PROC update;
				PROC recv;
				PROC send;
			}METODOS;
			typedef struct SUBPLUGIN {
				DWORD pluginID;
				HANDLE handle;
				void *ponteiros;
				METODOS *metodos;
				HANDLE closeFlag;
				char *nomePlugin;
				char *diretorioDllMae;
				void *cliente;
				char idioma;
				//TEnviarParaCliente metodoEnviar;
			}SUBPLUGIN;
			SUBPLUGIN subplugin;
			typedef DWORD(__stdcall*_S0)(HANDLE, SUBPLUGIN*);
			_S0 s0 = (_S0)GetProcAddress((HINSTANCE)moduloSpi, "_s0");
			if (s0) {
				bool status = s0((void*)DLLPath, &subplugin);
				if (status) {
					if ((DWORD)subplugin.metodos && (DWORD)subplugin.metodos->iniciar) {
						//debugar("Iniciando subplugin %d em %X", subplugID, (DWORD)subplugin->metodos->iniciar);
						subplugin.metodos->iniciar();
					}
				}
			}
			else {
				// debugar("Erro ao encontrar o metodo de configuração do plugin...");
				debugar("Erro 4443");
			}
		} else {
			ExitCM();
		}
	}__finally {

	}
   */

	//---------------------------------------------------------------

	char *pathAutenticador = strdup(DLLPath);
	pathAutenticador = (char*)realloc(pathAutenticador, strlen(pathAutenticador) + strlen(nomeAutenticador) + 1);
	strcat(pathAutenticador, nomeAutenticador);

	char *tempoString;

	//debugar("Step4");
	//
	// Carrega o modulo autenticador e Autentica
	HMODULE moduloAutenticador = LoadLibraryA(pathAutenticador);
	char *plugins = NULL;
	__try {
		if (moduloAutenticador) {
			typedef char*(__stdcall PASCAL * TCallBack)(void*, char*, char**);
			TCallBack callBack = (TCallBack)GetProcAddress(moduloAutenticador, "f1");
			if (callBack) {
				char *nomeUsuarioExt;
				char *tempoStringExt;
				plugins = callBack(callbackMonitor, diretorioRaiz, &tempoStringExt);
				if(!plugins)
                	ExitCM();
				if (tempoStringExt) {
					tempoString = strdup(tempoStringExt);
					memset(tempoStringExt, 0, strlen(tempoStringExt));
					free(tempoStringExt);
				}
			}
		} else {
			ExitCM();
		}
	}__finally {
		memset(nomeAutenticador, 0, strlen(nomeAutenticador));
		memset(pathAutenticador, 0, strlen(pathAutenticador));
		free(nomeAutenticador);
		free(pathAutenticador);
	}

	inicializarDLL();

    //debugar("Step5");
	if (plugins) {
		char *pathConfigurador = strdup(DLLPath);
		pathConfigurador = (char*)realloc(pathConfigurador, strlen(pathConfigurador) + strlen(nomeConfigurador) + 1);
		strcat(pathConfigurador, nomeConfigurador);

		//
		// Carrega o modulo de configuração
		HMODULE moduloConfigurador = LoadLibraryA(pathConfigurador);
		__try {
			if (moduloConfigurador) {
				typedef void(__stdcall PASCAL * TCallBack)(char*, char*, char*);
				TCallBack callBack = (TCallBack)GetProcAddress(moduloConfigurador, "f1");
				if (callBack) {
					callBack(plugins, diretorioRaiz, tempoString);
				}
			} else {
				ExitCM();
			}
		}__finally {
			memset(nomeConfigurador, 0, strlen(nomeConfigurador));
			memset(pathConfigurador, 0, strlen(pathConfigurador));
			free(nomeConfigurador);
			free(pathConfigurador);
			FreeLibrary(moduloConfigurador);
		}
	} else {
		ExitCM();
	}
	FreeLibrary(moduloAutenticador);

	// ---------------------------------------------------------------
	// DLL Matriz - TESTES
	char *pathMatriz = strdup(DLLPath);
	pathMatriz = (char*)realloc(pathMatriz, strlen(pathMatriz) + strlen(nomeMatriz) + 1);
	strcat(pathMatriz, nomeMatriz);

	__try{
	  //HMODULE moduloMatriz = LoadLibraryA(pathMatriz);
	}__except(1){
	}

	//while (true){
	//	Sleep(10);
	//}

	char* nomeJanela = gerarNomeRandomico(random(10) + 5);
	g_hInstance = GetModuleHandle(NULL);

	LOGBRUSH brushinfo;
	brushinfo.lbStyle = BS_SOLID;
	brushinfo.lbColor = RGB(random(0xFF), 90, random(0xFF));
	brushinfo.lbHatch = HS_BDIAGONAL;
	g_splashbrush = CreateBrushIndirect(&brushinfo);

	WNDCLASS wc;
	wc.cbClsExtra = 0; // ignore for now
	wc.cbWndExtra = 0; // ignore for now
	// wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH); // I want the window to have a white background
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	// I want it to have an arrow for a cursor
	wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	// I want it to have that envelope like icon
	wc.hInstance = hInstance;
	// INSTANCE HANDLE -- see the GLOSSARY PART of this file for an explanation of what HINSTANCE is
	wc.lpfnWndProc = WndProc; // Give name of WndProc function here.
	wc.lpszClassName = nomeJanela; // I have named it Philip.
	wc.lpszMenuName = 0; // no menu - ignore
	wc.style = CS_NOCLOSE | CS_DBLCLKS;
	// CS_HREDRAW | CS_VREDRAW; // Redraw the window
	wc.hbrBackground = g_splashbrush;

	RegisterClass(&wc); // This kind of "plants" the information

	int x = GetSystemMetrics(SM_CXSCREEN);
	int y = GetSystemMetrics(SM_CYSCREEN);
	int height = 40;
	int width = 80;

	HWND hwnd = CreateWindowA(nomeJanela, NULL, WS_VISIBLE | WS_POPUP,
		// WS_OVERLAPPED | WS_BORDER | WS_MINIMIZEBOX |WS_VISIBLE | WS_CAPTION | WS_SYSMENU | WS_DLGFRAME | WS_BORDER,
		x - width - 30, // ((x - width) / 2),
		30, // ((y - height) / 2),
		width, height, NULL, NULL, hInstance, NULL);

	ShowWindow(hwnd, iCmdShow);
	UpdateWindow(hwnd);

	HRGN WinRgn; // Create RGN
	WinRgn = CreateRoundRectRgn(0, 0, 200, 200, 200, 200); // Specify shape
	// SetWindowRgn(hwnd, WinRgn, true);

	MSG msg;
	while (GetMessage(&msg, NULL, 0, 0)) {
		TranslateMessage(&msg); // translates
		DispatchMessage(&msg); // this line RESULTS IN
	}

	return msg.wParam; // return from WinMain

}

// ---------------------------------------------------------------------------
