//---------------------------------------------------------------------------

#ifndef tiposH
#define tiposH
   /*
#include "Unit6.h"
#include "CMUtils.h"
#include "CMIniFiles.h"
#include "CMStrUtils.h"

#include "estruturaBot.h" */
#include "windows.h"
#include <stdio.h>
#include <stdlib.h>
#include <ddraw.h>

typedef HRESULT(__stdcall PASCAL *MyCreateSurface_Type)(LPVOID *param1, LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter);//(  LPDDSURFACEDESC lpDDSurfaceDesc, LPDIRECTDRAWSURFACE * lplpDDSurface, IUnknown * pUnkOuter);
typedef HRESULT(__stdcall PASCAL *MyFlip_Type)(LPVOID *param1, LPVOID lpDDSurfaceTargetOverride, DWORD dwFlags );//( LPDIRECTDRAWSURFACE lpDDSurfaceTargetOverride, DWORD dwFlags );
typedef HRESULT(__stdcall PASCAL *MyBlt_Type)(LPVOID *param1, LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx);
typedef HRESULT(__stdcall PASCAL *MyReleaseDC_Type)(LPVOID *param1, HDC dc );
typedef HRESULT(__stdcall PASCAL *MyBltFast_Type)(LPVOID *param1, DWORD dwX, DWORD dwY, LPDIRECTDRAWSURFACE7 lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwTrans );
typedef ULONG(__stdcall PASCAL *MyReleaseSurface_Type)(PVOID param1);
typedef ULONG(__stdcall PASCAL *MyReleaseSurface2_Type)();

typedef BOOL (__stdcall *f_CMReadProcessMemory)(DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
typedef BOOL (__stdcall *f_CMWriteProcessMemory)(DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
typedef DWORD (__stdcall *f_CMOpenProcess)(DWORD pid, PVOID procBase);
typedef HDC (__stdcall *f_CMGetDC)(HWND hnd);
typedef BOOL (__stdcall *f_CMVirtualProtect)(HANDLE hProcess, LPVOID lpAddress, DWORD dwSize, DWORD flNewProtect, PDWORD lpflOldProtect);

//------------ CM Apis types declatarion ----------
typedef HANDLE(__stdcall *TcmOpenFileMappingA)(DWORD dwDesiredAccess, BOOL bInheritHandle, char * lpName);
typedef HANDLE(__stdcall *TcmMapViewOfFile)( HANDLE hFileMappingObject, DWORD dwDesiredAccess, DWORD dwFileOffsetHigh, DWORD dwFileOffsetLow, DWORD dwNumberOfBytesToMap);
typedef BOOL(__stdcall *TcmVirtualProtect)(HANDLE hProcess, LPVOID lpAddress, DWORD dwSize, DWORD flNewProtect, PDWORD lpflOldProtect);

typedef struct _hookJMP {
    UCHAR instruction;
    DWORD offset;
}ThookJMP;

typedef struct _PhoenixCFG {
/*
    _PhoenixCFG(float _raio, float _fase2, float _tempo){
		raio = _raio;
		fase2 = _fase2;
        tempo = _tempo;
	}    */
	float raio;
	float fase2;
	float tempo;
	float tempo2;
} TPhoenixCFG;

//Inline Hook support
typedef struct _functionhook {
	BYTE newdata[5];
	BYTE olddata[5];
	BYTE tramp[5];
	unsigned char* original;
	unsigned char* callback;
	unsigned char* tranpolin;
}Tfunctionhook;

typedef struct _surfaces {
	DWORD surfaceID;
	MyBlt_Type bltPointer;
	MyReleaseSurface2_Type ReleaseSurface;
	MyFlip_Type Flip;
} TSurfaces;

#pragma pack(push, 1)

typedef struct {
	BYTE    IDLength;
	BYTE    ColorMapType;
	BYTE    ImageType;
	WORD    CMapStart;
	WORD    CMapLength;
	BYTE    CMapDepth;
	WORD    XOffset;
	WORD    YOffset;
	WORD    Width;
	WORD    Height;
	BYTE    PixelDepth;
	BYTE    ImageDescriptor;
}   TGA_Header;

typedef struct _CM_RGB {
	BYTE r;
	BYTE g;
	BYTE b;
    BYTE x;
} TCM_RGB, *PCM_RGB;

typedef struct _DrawPoint {
	float time;
	bool drawed;
} TDrawPoint;

typedef struct _Projetil {
	_Projetil(int _anguloInicial, double _x, double _y, double _speedX, double _speedY){
		x = _x;
		y = _y;
		xInicial = 0;
		yInicial = 0;
		speedX = _speedX;
		speedY = _speedY;
		anguloInicial = _anguloInicial;
		inverter = false;
		ultimoSpeedX = 0;
		ultimoSpeedY = 0;
	}

	_Projetil(){
	}

	int anguloInicial;
	double x;
	double y;
	double xInicial;
	double yInicial;
	double speedX;  //Velocidade inicial
	double speedY;
	double inverter;
	double ultimoSpeedX;
	double ultimoSpeedY;
}TProjetil;

typedef struct _mobile{
	int indice;
	int quantidadeTiros;
}TMobile;

typedef struct _reta{
	double pa;
	double pb;
	double pc;
}TReta;

typedef struct _temporal{
	bool flag1;
	bool flag2;
	DWORD tempo;
} Temporal;


typedef struct _Slots{
	bool slotScore;
	bool slotNascer;
	bool var3;
	DWORD tempoSlot[4];
	bool jogadorMorto;
	bool var6;
} Slots;

typedef struct _aviso{
	bool ativo;
	int len;
	char valor[150];
	DWORD inicio;
	DWORD tempo;
}TAviso;

typedef struct _socketBuf{
	SOCKET* sock;
	BYTE* buffer;
    int len;
}TSocketBuf;

#pragma pack(pop)
//---------------------------------------------------------------------------
#endif
