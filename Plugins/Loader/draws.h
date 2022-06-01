//---------------------------------------------------------------------------

#ifndef drawsH
#define drawsH

#include "windows.h"
#include <stdio.h>
#include <stdlib.h>
//#include "../png/png.h"

#define DLL_EXPORT __declspec(dllexport)
//---------------------------------------------------------------------------

typedef struct tagPNGINFO
{
	int nWidth;					//width
	int nHeight;				//height
	unsigned char **ppbyRow;	//data:rgba
}PNGINFO;

typedef struct tagPNGBLEND
{
	BOOL bBlend;				//blend flag: true - blend image with background
	COLORREF cr;				//blend color
}PNGBLEND;

void Png32_Show(HDC hdc, int xDest,int yDest,int nWidth,int nHeight,PNGINFO *pPngInfo,int xSour,int ySour);

extern "C" DLL_EXPORT DWORD WINAPI DirectDrawCreateExTramp( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter );

void DrawStatus(HDC hdc);
HRESULT __stdcall BltHooked(LPVOID *param1 , LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx);
HRESULT __stdcall PASCAL CreateSurfaceHooked( LPVOID *param1 ,  LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter);
HRESULT WINAPI DirectDrawCreateExHooked( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter );
void __fastcall FillPoints(HDC dc);


#endif
