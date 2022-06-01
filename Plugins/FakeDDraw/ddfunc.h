//---------------------------------------------------------------------------

#ifndef ddfuncH
#define ddfuncH

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <string.h>
#include <dinput.h>
#include "debugutils.h"
#include "client.h"
#include "..\VMProtectSDK.h"

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

typedef HRESULT(__stdcall PASCAL *MyRelease_Type)(LPVOID *param1);
typedef HRESULT(__stdcall PASCAL *MyBlt_Type)(LPVOID *param1, LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx);
typedef HRESULT(__stdcall PASCAL *MyCreateSurface_Type)(LPVOID *param1, LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter);
typedef HRESULT(__stdcall PASCAL *MyCreateDevice_Type)(LPVOID *, REFGUID, LPDIRECTINPUTDEVICE *, LPUNKNOWN);
typedef HRESULT(__stdcall PASCAL *MyGetDeviceState_Type)(LPVOID *param1 , DWORD cbData, LPVOID lpvData);
typedef HRESULT(__stdcall PASCAL *MyDirectDrawCreateEx_Type)(DWORD*,DWORD*,DWORD*,DWORD*);//GUID FAR *, LPVOID  *, REFIID , IUnknown FAR *);

extern MyBlt_Type ddBLT;
extern MyCreateSurface_Type OldCreateSurface;
extern MyCreateDevice_Type OldCreateDevice;

extern void *glpdd;
extern PROC AcquireDDThreadLockVAR;
extern PROC CheckFullscreenVAR;
extern PROC CompleteCreateSysmemSurfaceVAR;
extern PROC D3DParseUnknownCommandVAR;
extern PROC DDGetAttachedSurfaceLclVAR;
extern PROC DDInternalLockVAR;
extern PROC DDInternalUnlockVAR;
extern PROC DirectDrawCreateVAR;
extern PROC DirectDrawCreateClipperVAR;
extern MyDirectDrawCreateEx_Type DirectDrawCreateExVAR;
extern PROC DirectDrawEnumerateAVAR;
extern PROC DirectDrawEnumerateExAVAR;
extern PROC DirectDrawEnumerateExWVAR;
extern PROC DirectDrawEnumerateWVAR;
extern PROC DllCanUnloadNowVAR;
extern PROC DllGetClassObjectVAR;
extern PROC DSoundHelpVAR;
extern PROC GetDDSurfaceLocalVAR;
extern PROC GetOLEThunkDataVAR;
extern PROC GetSurfaceFromDCVAR;
extern PROC RegisterSpecialCaseVAR;
extern PROC ReleaseDDThreadLockVAR;
extern HMODULE dllMae;

extern Cliente *cliente;

void __stdcall processar(PACOTE *pacote);
int getPorta();


HRESULT WINAPI H_DirectDrawCreateEx(DWORD* lpGuid, DWORD* lplpDD, DWORD* iid, DWORD* pUnkOuter);
//---------------------------------------------------------------------------
#endif
