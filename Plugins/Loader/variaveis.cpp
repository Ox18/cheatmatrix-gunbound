// ---------------------------------------------------------------------------

#pragma hdrstop

#include "variaveis.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)

HANDLE closeFlag = 0;
HANDLE *threads;
CRITICAL_SECTION flagDraw;
CRITICAL_SECTION flagUpdate;
CRITICAL_SECTION flagEnderecador;
CRITICAL_SECTION flagSubplugins;
CRITICAL_SECTION flagAtalho;
// SUBPLUGIN *subplugins;
bool mostrarStatus = true;
char *DLLPath = NULL;
GdiplusStartupInput gdiplusStartupInput;
ULONG_PTR gdiplusToken;
HMODULE cmlib = 0;
wchar_t *ImageName;
char *ImageNameA;
bool canDrawNow = false;

MyCreateSurface_Type OldCreateSurface;
MyCreateDevice_Type OldCreateDevice;

void *glpdd;
bool inicializado = false;
Servidor *servidor = NULL;
LISTA *subplugs = new LISTA(0);
LISTA *atalhos = new LISTA(0);

bool isGunbound = false;
bool flagGunbound = false;
char *nomeArquivo;
char *nomeDLL;
HMODULE instanceDLL;
HMODULE moduloDLL;
WSADATA dataWSA;

BITMAP bmp;
HBITMAP hBmp = NULL;

LPDIRECTINPUT lpdi;
LPDIRECTINPUTDEVICE m_keyboard;

