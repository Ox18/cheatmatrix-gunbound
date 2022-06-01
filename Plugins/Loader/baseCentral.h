//---------------------------------------------------------------------------

#ifndef baseCentralH
#define baseCentralH

#pragma comment(lib,"ws2_32.lib")
#pragma comment(lib,"imagehlp.lib")

#define TOTALHOOKS 5

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <ddraw.h>

#include "server.h"
#include "variaveis.h"
#include "..\debugutils.h"
#include "..\CMStrUtils.h"
#include "..\CMIniFiles.h"
#include "..\CMUtils.h"
#include "..\drawUtils.h"
#include "..\tiposBase.h"

#include <math.h>
#include <graphics.hpp>
#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"

// A Screen represents one display that images can be drawn to.
struct Screen
{
    GUID guid;
    TCHAR szDesc[200];
    HMONITOR hmon;
    LPDIRECTDRAW7 pdd;
    LPDIRECTDRAWSURFACE7 pddsFront;
    LPDIRECTDRAWSURFACE7 pddsBack;
	LPDIRECTDRAWSURFACE7 pddsOff[5];
	Screen* pScreenNext; // For linked list
};

struct EnumInfo
{
    BOOL bMultimonSupported;
    HRESULT hr;
};
extern Screen* s_pScreenFirst;
extern HWND s_hwnd; // Main app focus HWND
extern BOOL g_bActive;// = TRUE; // Whether app is actively drawing


typedef struct DDRAW_TABLE_BASE {
    long* QueryInterface;
	long* AddRef;
	long* Release;
	long* Compact;
	long* CreateClipper;
	long* CreatePalette;
	long* CreateSurface;
	long* DuplicateSurface;
	long* EnumDisplayModes;
	long* EnumSurfaces;
	long* FlipToGDISurface;
	long* GetCaps;
	long* GetDisplayMode;
	long* GetFourCCCodes;
	long* GetGDISurface;
	long* GetMonitorFrequency;
	long* GetScanLine;
	long* GetVerticalBlankStatus;
	long* Initialize;
	long* RestoreDisplayMode;
	long* SetCooperativeLevel;
	long* SetDisplayMode;
	long* WaitForVerticalBlank;
	long* GetAvailableVidMem;
	long* GetSurfaceFromDC;
	long* RestoreAllSurfaces;
	long* TestCooperativeLevel;
	long* GetDeviceIdentifier;
	long* StartModeTest;
	long* EvaluateMode;
	//unsigned reservado: 40;
}DDRAW_TABLE_BASE;

typedef struct DDRAW_TABLE {
   DDRAW_TABLE_BASE novaTabela;
   DDRAW_TABLE_BASE tabelaAntiga;
}DDRAW_TABLE;

typedef struct SURFACE_TABLE_BASE {
	long* QueryInterface;
	long* AddRef;
	long* Release;
	long* AddAttachedSurface;
	long* AddOverlayDirtyRect;
	long* Blt;
	long* BltBatch;
	long* BltFast;
	long* DeleteAttachedSurface;
	long* EnumAttachedSurfaces;
	long* EnumOverlayZOrders;
	long* Flip;
	long* GetAttachedSurface;
	long* GetBltStatus;
	long* GetCaps;
	long* GetClipper;
	long* GetColorKey;
	long* GetDC;
	long* GetFlipStatus;
	long* GetOverlayPosition;
	long* GetPalette;
	long* GetPixelFormat;
	long* GetSurfaceDesc;
	long* Initialize;
	long* IsLost;
	long* Lock;
	long* ReleaseDC;
	long* Restore;
	long* SetClipper;
	long* SetColorKey;
	long* SetOverlayPosition;
	long* SetPalette;
	long* Unlock;
	long* UpdateOverlay;
	long* UpdateOverlayDisplay;
	long* UpdateOverlayZOrder;
	long* GetDDInterface;
	long* PageLock;
	long* PageUnlock;
	long* SetSurfaceDesc;
	long* SetPrivateData;
	long* GetPrivateData;
	long* FreePrivateData;
	long* GetUniquenessValue;
	long* ChangeUniquenessValue;
	long* SetPriority;
	long* GetPriority;
	long* SetLOD;
	long* GetLOD;
}SURFACE_TABLE_BASE;

typedef struct SURFACE_TABLE {
   SURFACE_TABLE_BASE novaTabela;
   SURFACE_TABLE_BASE *tabelaAntiga;
}SURFACE_TABLE;

typedef struct INPUT_DEVICE_TABLE_BASE{
	long* QueryInterface;
	long* AddRef;
	long* Release;
	long* GetCapabilities;
	long* EnumObjects;
	long* GetProperty;
	long* SetProperty;
	long* Acquire;
	long* Unacquire;
	long* GetDeviceState;
	long* GetDeviceData;
	long* SetDataFormat;
	long* SetEventNotification;
	long* SetCooperativeLevel;
	long* GetObjectInfo;
	long* GetDeviceInfo;
	long* RunControlPanel;
	long* Initialize;
	long* CreateEffect;
	long* EnumEffects;
	long* GetEffectInfo;
	long* GetForceFeedbackState;
	long* SendForceFeedbackCommand;
	long* EnumCreatedEffectObjects;
	long* Escape;
	long* Poll;
	long* SendDeviceData;
	long* EnumEffectsInFile;
	long* WriteEffectToFile;
	long* BuildActionMap;
	long* SetActionMap;
	long* GetImageInf;
}INPUT_DEVICE_TABLE_BASE;

typedef struct INPUT_DEVICE_TABLE {
   INPUT_DEVICE_TABLE_BASE novaTabela;
   INPUT_DEVICE_TABLE_BASE *tabelaAntiga;
}INPUT_DEVICE_TABLE;


//---------------------------------------------------------------------------

void chamarDraw(HDC dc, char tipoDesenho);
void chamarUpdate();
void chamarEnderecador();

void inicializarDLL();
void finalizarDLL();

DWORD WINAPI ThreadDesenha( LPVOID lpParam );
DWORD WINAPI ThreadUpdate( LPVOID lpParam );
DWORD WINAPI ThreadEnderecos( LPVOID lpParam );
DWORD WINAPI ThreadAtalhos( LPVOID lpParam );
void PerformError(char *msg, char *titulo = NULL);
void __stdcall processar(ContextoCliente *contexto, PACOTE *pacote);
void __stdcall processarCliente(PACOTE *pacote);
void __stdcall PASCAL OnClientDisconnect(ContextoCliente *contexto);
void __stdcall enviar(int socket, TIPO_PACOTE tipo, char* buffer, int size);


#endif
