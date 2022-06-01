//---------------------------------------------------------------------------

#ifndef variaveisH
#define variaveisH

#pragma comment(lib,"ws2_32.lib")
#pragma comment(lib,"imagehlp.lib")

#define TOTALHOOKS 5

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <ddraw.h>
#include <dinput.h>

#include "server.h"
#include "..\client.h"
#include "tipos.h"
#include "..\debugutils.h"
#include "..\CMStrUtils.h"
#include "..\CMIniFiles.h"
#include "..\CMUtils.h"
#include "..\drawUtils.h"
#include "..\tiposBase.h"

#include <math.h>
//#include <graphics.hpp>
#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"
//#pragma link "dinput8.lib"
//#pragma link "dxguid.lib"
//#pragma comment (lib, "dxguid.lib")

//---------------------------------------------------------------------------
//extern SUBPLUGIN *subplugins;
extern HANDLE closeFlag;
extern CRITICAL_SECTION flagDraw;
extern CRITICAL_SECTION flagUpdate;
extern CRITICAL_SECTION flagEnderecador;
extern CRITICAL_SECTION flagSubplugins;
extern CRITICAL_SECTION flagAtalho;
extern HANDLE *threads;
extern bool mostrarStatus;
extern char *DLLPath;
extern GdiplusStartupInput gdiplusStartupInput;
extern ULONG_PTR gdiplusToken;
extern HMODULE cmlib;
extern wchar_t *ImageName;
extern char *ImageNameA;
extern bool canDrawNow;

extern MyCreateSurface_Type OldCreateSurface;
extern MyCreateDevice_Type OldCreateDevice;

extern void *glpdd;
extern bool inicializado;
extern Servidor *servidor;

extern bool isGunbound;
extern char *nomeArquivo;
extern HMODULE instanceDLL;
extern HMODULE moduloDLL;
extern bool flagGunbound;
extern char *nomeDLL;
extern WSADATA dataWSA;

extern LISTA *subplugs;
extern LISTA *atalhos;

extern BITMAP bmp;
extern HBITMAP hBmp;

Tfunctionhook hooks[TOTALHOOKS];

//extern LPDIRECTINPUT8 lpdi;
extern LPDIRECTINPUT lpdi;
extern LPDIRECTINPUTDEVICE m_keyboard;

#endif
