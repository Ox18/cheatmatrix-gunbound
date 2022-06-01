// ---------------------------------------------------------------------------

#ifndef baseCentralH
#define baseCentralH

#pragma comment(lib,"ws2_32.lib")
#pragma comment(lib,"imagehlp.lib")

#define TOTALHOOKS 5

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <ddraw.h>

//#include "server.h"
#include "client.h"
#include "variaveis.h"
#include "..\debugutils.h"
#include "..\CMStrUtils.h"
#include "..\CMIniFiles.h"
#include "..\CMUtils.h"
#include "..\drawUtils.h"
#include "..\tiposBase.h"
#include "CMIniFiles.h"
#include "suporte.h"
#include "constantes.h"

#include <math.h>
#include <graphics.hpp>
#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"

extern HANDLE closeFlag;
extern CRITICAL_SECTION flagConnect;
extern HANDLE *threads;
extern LISTA *subplugs;
extern LISTA *ponteiros;
extern LISTA *atalhos;
extern char *processoJogo;
extern bool inicializado;
extern WSADATA dataWSA;
extern Cliente *cliente;
extern char *DLLPath;
extern char *DLLPathAnterior;
extern char *DLLPath1;
extern HINSTANCE hInstanceDll;
extern char *NomeMatriz;
extern char *diretorioBase;
extern BYTE indiceProcesso;

// ---------------------------------------------------------------------------

void inicializarDLL();
void finalizarDLL();

DWORD WINAPI ThreadConexaoMonitorador(LPVOID lpParam);
DWORD WINAPI ThreadEncontraProcessoMonitorador(LPVOID lpParam);
DWORD WINAPI ThreadAtalhosMonitorador(LPVOID lpParam);
void PerformError(char*msg, char*titulo = NULL);
void __fastcall processar(PACOTE *pacote);
void __stdcall processarCliente(PACOTE * pacote);
void __stdcall processarCliente(PACOTE * pacote);
void __stdcall PASCAL OnClientDisconnect(ContextoCliente * contexto);
void __stdcall enviar(int socket, TIPO_PACOTE tipo, char*buffer, int size);

#endif
