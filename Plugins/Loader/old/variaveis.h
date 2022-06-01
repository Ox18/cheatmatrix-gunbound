//---------------------------------------------------------------------------

#ifndef variaveisH
#define variaveisH

#include "tipos.h"
#include "common.h"
#include "mathUtils.h"
#include <math.h>
#include <graphics.hpp>
#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"

#define TOTALHOOKS 5
const DWORD SPY_MESSAGE = WM_USER + 4934;
GdiplusStartupInput gdiplusStartupInput;
ULONG_PTR gdiplusToken;
char *DLLPath;
HMODULE DriverLib;

/*
extern HANDLE HandleThread1;
extern HANDLE HandleThread2;
extern HANDLE HandleThreadAS;
extern int ParamsThread1;
extern int ParamsThread2;
extern int ParamsThreadAS;
*/

HANDLE HandleThread1 = 0;
HANDLE HandleThread2 = 0;
HANDLE HandleThreadAS = 0;
int ParamsThread1 = 1;
int ParamsThread2 = 1;
int ParamsThreadAS = 1;

static HBITMAP hBmp = NULL;
BITMAP bmp;
Tfunctionhook hooks[TOTALHOOKS];
wchar_t *ImageName;
char *ImageNameA;
char *ImageTest1;
// Flags para desenhar na tela
bool CanDrawNow = false;
DWORD LastDraw = 0;
TcmOpenFileMappingA cmOpenFileMappingA;
TcmMapViewOfFile cmMapViewOfFile;
TcmVirtualProtect CMVirtualProtect;
void *glpdd;
MyCreateSurface_Type OldCreateSurface;

// Variavel de suporte para desinject do npggnt
const PCHAR MapName = "AD02992CEB206FF90";
HANDLE hMemFile;
const float Tempo = 0.05f;

bool debugmode = false;

extern f_CMOpenProcess CMOpenProcess;
extern f_CMGetDC CMGetDC;
extern f_CMGetDC CMGetWindowDC;
//extern f_CMReadProcessMemory CMReadProcessMemory;
//extern f_CMWriteProcessMemory CMWriteProcessMemory;
//extern f_CMVirtualProtect CMVirtualProtect;

//---------------------------------------------------------------------------
#endif
