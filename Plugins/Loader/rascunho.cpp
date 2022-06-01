//---------------------------------------------------------------------------

#pragma hdrstop

#include "rascunho.h"

/*

/*******************************************************************************/
/*                    Flip
 * /*******************************************************************************/

 /*
extern "C" DLL_EXPORT DWORD __stdcall OldFlipX( LPDIRECTDRAWSURFACE lpDDSurfaceTargetOverride, DWORD dwFlags ) {
    _asm {
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        ret
    }
}

HRESULT __stdcall MyFlip(LPVOID *param1 , LPVOID lpDDSurfaceTargetOverride, DWORD dwFlags ) {
	HRESULT retval = 0;
	SendMessage(HWND_BROADCAST, SPY_MESSAGE, 9, 1 );
	int i = GetIdOfInterface( (DWORD) param1 );
    if(surfaces.size() > 0 && i < surfaces.size() && i >= 0) {
        SendMessage(HWND_BROADCAST, SPY_MESSAGE, 9, 1 );
        retval = surfaces[i].Flip(param1, lpDDSurfaceTargetOverride, dwFlags);
    }
    else {
        SendMessage(HWND_BROADCAST, SPY_MESSAGE, 10, 1 );
    }
	return retval;
}
*/

//******************************************************************************
//                      CreateRemoteThread Hook
//******************************************************************************
/*
HANDLE  __stdcall MyCRT(
HANDLE hProcess,
		LPSECURITY_ATTRIBUTES lpThreadAttributes,
		SIZE_T dwStackSize,
		LPTHREAD_START_ROUTINE lpStartAddress,
		LPVOID lpParameter,
		DWORD dwCreationFlags,
		LPDWORD lpThreadId  ) {
	HANDLE retval = 0;
	return OldCRT(hProcess, lpThreadAttributes, dwStackSize, lpStartAddress, lpParameter, dwCreationFlags, lpThreadId);
	//return retval;
}

extern "C" DLL_EXPORT HANDLE __stdcall OldCRT(
HANDLE hProcess,
        LPSECURITY_ATTRIBUTES lpThreadAttributes,
        SIZE_T dwStackSize,
        LPTHREAD_START_ROUTINE lpStartAddress,
        LPVOID lpParameter,
        DWORD dwCreationFlags,
        LPDWORD lpThreadId) {
    _asm {
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        ret
    }
}
*/

//------------------------------------------------------------------------------

/*******************************************************************************/
/*                          LoadLibrary Hook
 * /*******************************************************************************/
/*
extern "C" DLL_EXPORT HMODULE __stdcall OldLoadLib(LPCSTR lpLibFileName) {

    _asm {
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        ret
    }
}

HMODULE __stdcall MyLoadLib(LPCSTR lpLibFileName) {
    //Sleep(100);
    //SendMSg( (char *)lpLibFileName );
	HMODULE retval = OldLoadLib(lpLibFileName);
    return retval;
}
*/


/*
 *
 * unsigned long GetTargetThreadIdFromProcname(char *procName)
 * {
 * PROCESSENTRY32 pe;
 * HANDLE thSnapshot, hProcess;
 * BOOL retval, ProcFound = false;
 * unsigned long pTID, threadID;
 *
 * //VMPROTECTBEGIN
 *
 * thSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
 *
 * if(thSnapshot == INVALID_HANDLE_VALUE)
 * {
 * //MessageBox(NULL, "Error: unable to create toolhelp snapshot", "Loader", NULL);
 * return 0;
 * }
 *
 * pe.dwSize = sizeof(PROCESSENTRY32);
 *
 * retval = Process32First(thSnapshot, &pe);
 *
 * while(retval)
 * {
 * if(  LowerCase(StrPas(PCHAR(pe.szExeFile))) == LowerCase(StrPas(PCHAR(procName))) )
 * {
 * ProcFound = true;
 * break;
 * }
 *
 * retval    = Process32Next(thSnapshot,&pe);
 * pe.dwSize = sizeof(PROCESSENTRY32);
 * }
 *
 * CloseHandle(thSnapshot);
 *
 * _asm {
 * mov eax, fs:[0x18]
 * add eax, 36
 * mov [pTID], eax
 * }
 *
 * hProcess = OpenProcess(PROCESS_VM_READ, false, pe.th32ProcessID);
 * ReadProcessMemory(hProcess, (const void *)pTID, &threadID, 4, NULL);
 * CloseHandle(hProcess);
 *
 * if(ProcFound)
 * return threadID;
 * else
 * return 0;
 *
 * //VMPROTECTEND
 * }         */

 //--------------------------------------------------------------------------
 // Botar no inicio do callback do CreateSurface :

 //Desinjeta a NPSC.DES
	/*if(!finishedLib)
	 * {
	 * finishedLib = true;
	 * FreeLibrary(npsch);
	 * SendMSg( (char *)"Ejetado" );
	 * }  */

	//HRESULT retval = 0;

//---------------------------------------------------------------------------
/*
HRESULT __stdcall MyReleaseDC(LPVOID *param1 , HDC dc) {
	HRESULT retval = 0;

	__try{
		DWORD* pvtbl = (DWORD*) *param1;
		DWORD* pReleaseDC = (DWORD*)( DWORD(pvtbl) + 44 );
		DWORD*	g_pReleaseDC = NULL;
		g_pReleaseDC = (DWORD*)*pReleaseDC;
		MyReleaseDC_Type dReleaseDC = (MyReleaseDC_Type)g_pReleaseDC;

		debug(70, (DWORD)dc);
		retval =  dReleaseDC(param1, dc);

	}
	__except(1)
	{
		debug(1000, 50);
		return DD_OK;
	}

	return retval;
   //DrawStatus(hdc);

}

ULONG __stdcall MyReleaseSurface(PVOID param1) {
	ULONG retval = 0;

    int i = GetIdOfInterface( (DWORD) param1 );
    if(surfaces.size() > 0 && i < surfaces.size() && i >= 0) {
        retval = surfaces[i].ReleaseSurface();
        surfaces[i].surfaceID = 0;
        surfaces.erase( surfaces.begin()+i);
        return retval;
    }

    return retval;
}
*/

//---------------------------------------------------------------------------
	/*
	int i = GetIdOfInterface( (DWORD) param1 );

    if(surfaces.size() > 0 && i < surfaces.size() && i >= 0) {
		if(BotInfos) {
			if(BotInfos->ShowStatus) {
				HDC hdc;
				((LPDIRECTDRAWSURFACE7)(param1))->GetDC(&hdc);
				DrawStatus(hdc);
				((LPDIRECTDRAWSURFACE7)(param1))->ReleaseDC(hdc);
			}
		}

		__try {
			debug(10, (DWORD)surfaces[i].bltPointer);
			retval =  surfaces[i].bltPointer(param1, lpDestRect, lpDDSrcSurface, lpSrcRect, dwFlags, lpDDBltFx);
		}
        __except(1) {
            return DD_OK;
        }

		if(BotInfos) {
            if(BotInfos->ShowStatus) {
                HDC hdc;
				((LPDIRECTDRAWSURFACE7)(param1))->GetDC(&hdc);
                DrawStatus(hdc);
                ((LPDIRECTDRAWSURFACE7)(param1))->ReleaseDC(hdc);
            }
        }

		//((LPDIRECTDRAW7)(glpdd))->EnumSurfaces(DDWAITVB_BEGIN, NULL);
		//CanDrawNow = true;
		//FillPoints(0);
    }
    else {
        //SendMessage(HWND_BROADCAST, SPY_MESSAGE, 10, 1 );
	}   */


//---------------------------------------------------------------------------
/*IDirectDrawSurface * CreateOffScreenSurface(IDirectDraw *pdd, int dx, int dy) {
    DDSURFACEDESC ddsd;
    IDirectDrawSurface *pdds;

    //
    // create a DirectDrawSurface for this bitmap
    //
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT |DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth = dx;
    ddsd.dwHeight = dy;

    if (pdd->CreateSurface(&ddsd, &pdds, NULL) != DD_OK) {
        return NULL;
    } else {
        return pdds;
    }
} */


//-------------------------------------------------------------------------
//    Pega o ID da interface no array
//-------------------------------------------------------------------------
/*
int GetIdOfInterface(DWORD interf) {
    for(int i = 0; i < surfaces.size(); i++) {
        if(surfaces[i].surfaceID == interf) {
            return i;
        }
    }
    return -1;
}

int RemoveSurface(DWORD interf) {
	for(int i = 0; i < surfaces.size(); i++) {
        if(surfaces[i].surfaceID == interf) {
            int j = surfaces[i].surfaceID;
            surfaces[i].surfaceID = 0;
            return j;
        }
    }
    return -1;
}
*/

//---------------------------------------------------------------------------
#pragma package(smart_init)
