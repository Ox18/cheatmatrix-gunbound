//---------------------------------------------------------------------------

#ifndef tiposH
#define tiposH

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <string.h>
#include <dinput.h>

//---------------------------------------------------------------------------
typedef struct _functionhook {
	char newdata[5];
	char olddata[5];
	char tramp[5];
	unsigned char* original;
	unsigned char* callback;
	unsigned char* tranpolin;
}Tfunctionhook;

typedef HRESULT(__stdcall PASCAL *MyRelease_Type)(LPVOID *param1);
typedef HRESULT(__stdcall PASCAL *MyBlt_Type)(LPVOID *param1, LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx);
typedef HRESULT(__stdcall PASCAL *MyCreateSurface_Type)(LPVOID *param1, LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter);
typedef HRESULT(__stdcall PASCAL *MyCreateDevice_Type)(LPVOID *, REFGUID, LPDIRECTINPUTDEVICE *, LPUNKNOWN);
typedef HRESULT(__stdcall PASCAL *MyGetDeviceState_Type)(LPVOID *param1 , DWORD cbData, LPVOID lpvData);

#endif
