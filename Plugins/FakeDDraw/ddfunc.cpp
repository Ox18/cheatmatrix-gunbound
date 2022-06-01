//---------------------------------------------------------------------------

#pragma hdrstop

#include "ddfunc.h"
#include <ddraw.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)

MyBlt_Type ddBLT = 0;
MyCreateSurface_Type OldCreateSurface;
MyCreateDevice_Type OldCreateDevice;
void *glpdd;
HMODULE dllMae;

PROC AcquireDDThreadLockVAR;
PROC CheckFullscreenVAR;
PROC CompleteCreateSysmemSurfaceVAR;
PROC D3DParseUnknownCommandVAR;
PROC DDGetAttachedSurfaceLclVAR;
PROC DDInternalLockVAR;
PROC DDInternalUnlockVAR;
PROC DirectDrawCreateVAR;
PROC DirectDrawCreateClipperVAR;
MyDirectDrawCreateEx_Type DirectDrawCreateExVAR;
PROC DirectDrawEnumerateAVAR;
PROC DirectDrawEnumerateExAVAR;
PROC DirectDrawEnumerateExWVAR;
PROC DirectDrawEnumerateWVAR;
PROC DllCanUnloadNowVAR;
PROC DllGetClassObjectVAR;
PROC DSoundHelpVAR;
PROC GetDDSurfaceLocalVAR;
PROC GetOLEThunkDataVAR;
PROC GetSurfaceFromDCVAR;
PROC RegisterSpecialCaseVAR;
PROC ReleaseDDThreadLockVAR;

Cliente *cliente;

int getPorta(){
	VMProtectBegin("getVersao");
	__try{
		HKEY hKey = 0;
		char buf[255] = {0};
		DWORD dwType = 0;
		DWORD dwBufSize = 0;
		const char* subkey = "SOFTWARE\\SoftX";
		const char* key = "Version";
		int porta = 0;

		if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, subkey, 0, KEY_ALL_ACCESS, &hKey) == ERROR_SUCCESS) {
			dwType = REG_DWORD;
			dwBufSize = 4;
			RegQueryValueEx(hKey, key, 0, &dwType, (unsigned char*)&porta, (ULONG*)&dwBufSize);
			RegCloseKey(hKey);
			return porta;
		}
		return porta;
	}__except(1){
	}
	VMProtectEnd();
}

void __stdcall processar(PACOTE *pacote) {
	VMProtectBegin("prcPac");
	typedef PVOID(__stdcall PASCAL *t_SearchBlt)(void);
	t_SearchBlt searchBlt = 0;

	if (pacote != NULL) {
		switch (pacote->tipo) {
		case TP_MENSAGEM: {
				char *msg = (char*)&pacote->buffer;
				debugar("dd: %s",msg);
				__try{
					dllMae = LoadLibraryA(msg);
					if(dllMae){
						searchBlt = (t_SearchBlt)GetProcAddress(dllMae,"_s0");
						if(searchBlt){
							ddBLT = (MyBlt_Type)searchBlt();
						}else{
							FreeLibrary(dllMae);
							dllMae = 0;
						}
					}else{
						dllMae = 0;
					}
				}__except(1){
					dllMae = 0;
                   	debugar("FLM");
				}
			} break;
		case TP_DISCONNECT: {
			 if(dllMae){
				__try{
					FreeLibrary(dllMae);
				}__except(1){
				}
				dllMae = 0;
				ddBLT = 0;
			 }
		} break;
		default:
			break;
		}
	}
	VMProtectEnd();
}


HRESULT __stdcall H_Blt(LPVOID *param1, LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx) {
	HRESULT retval = 0;

	bool usarOriginal = true;

	if (ddBLT) {
		__try{
			retval = ddBLT(param1, lpDestRect, lpDDSrcSurface, lpSrcRect, dwFlags, lpDDBltFx);
			usarOriginal = false;
		}__except(1){
			usarOriginal = true;
		}
	}

	if(usarOriginal){
		__try {
			// Pega o ponteiro do método original, armazenado no recipiente
			SURFACE_TABLE *tabela = (SURFACE_TABLE*)*param1;
			MyBlt_Type myBlt = (MyBlt_Type)tabela->tabelaAntiga->Blt;
			retval = myBlt(param1, lpDestRect, lpDDSrcSurface, lpSrcRect, dwFlags, lpDDBltFx);
		}
		__except (1) {
			return DD_FALSE;
		}
	}

	return retval;
}

HRESULT __stdcall H_Release(LPVOID *param1) {
	HRESULT retval = 0;
	__try {
		SURFACE_TABLE *tabela = (SURFACE_TABLE *)*param1;
		MyRelease_Type myRelease = (MyRelease_Type)tabela->tabelaAntiga->Release;
		memcpy((PVOID)&tabela->novaTabela, (PVOID)tabela->tabelaAntiga, sizeof tabela->novaTabela);
		retval = myRelease(param1);
		free(tabela->tabelaAntiga);
	} __except(1) {
		debugar("Releasing Error");
		return DD_FALSE;
	}
	return retval;
}

//
//  CreateSurface Hook
//
HRESULT __stdcall PASCAL H_CreateSurface( LPVOID *param1 ,  LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter) {
	__try{
		//debugar("CreateSurface");
		LPDIRECTDRAWSURFACE7 *surface;
		HRESULT retval = OldCreateSurface(param1 , lpDDSurfaceDesc, (void**)&surface, pUnkOuter);
		SURFACE_TABLE *tabela = (SURFACE_TABLE *)*surface;
		if((DWORD)tabela->novaTabela.Blt != (DWORD)&H_Blt) {
			tabela->tabelaAntiga = (SURFACE_TABLE_BASE*) malloc(sizeof tabela->novaTabela);
			memset(tabela->tabelaAntiga, 0, sizeof tabela->novaTabela);
			memcpy(tabela->tabelaAntiga, &tabela->novaTabela, sizeof tabela->novaTabela);
		}
		DWORD flOldProtect, flDontCare;
		VirtualProtect((PVOID)&tabela->novaTabela.Blt, 4, PAGE_EXECUTE_READWRITE, &flOldProtect);
		tabela->novaTabela.Blt = (long*)&H_Blt;
		VirtualProtect((PVOID)&tabela->novaTabela.Blt, 4, flOldProtect, &flDontCare);
		VirtualProtect((PVOID)&tabela->novaTabela.Release, 4, PAGE_EXECUTE_READWRITE, &flOldProtect);
		tabela->novaTabela.Release = (long*)&H_Release;
		VirtualProtect((PVOID)&tabela->novaTabela.Release, 4, flOldProtect, &flDontCare);
		*lplpDDSurface = surface;
		return retval;
	}__except(1){
    	debugar("Erro csfc");
    	return DD_FALSE;
	}
}

HRESULT __stdcall H_GetDeviceState(LPVOID *param1 , DWORD cbData, LPVOID lpvData){
	HRESULT retval = 0;
	//debugar("GetDeviceState");
	__try {
		INPUT_DEVICE_TABLE *tabela = (INPUT_DEVICE_TABLE *)*param1;
		MyGetDeviceState_Type myGDS = (MyGetDeviceState_Type)tabela->tabelaAntiga->GetDeviceState;
		retval = myGDS(param1, cbData, lpvData);
	} __except(1) {
		debugar("Erro dvc");
		return DD_FALSE;
	}
	return retval;
}

//-------------------------------------------------------------------------
//   CallBack do DirectDrawCreateEx
//-------------------------------------------------------------------------
HRESULT WINAPI H_DirectDrawCreateEx(DWORD* lpGuid, DWORD* lplpDD, DWORD* iid, DWORD* pUnkOuter) {
	VMProtectBegin("prcDDCr");
	__try {
		// Chama o DirectDrawCreateEx original
		HRESULT retval = DirectDrawCreateExVAR((DWORD*)lpGuid, (DWORD*)lplpDD, (DWORD*)iid, (DWORD*)pUnkOuter);
		//debugar("DirectDrawCreateEx start: %x - %x - %x", retval, lplpDD, *(PDWORD*)lplpDD);
		glpdd = (void*)*lplpDD;

		__try {
			// Pega o pontero da interface
			DWORD* ppvtbl = (DWORD*)*lplpDD; // Endereço da variavel
			DWORD* pvtbl = (DWORD*)*ppvtbl; // Endereço da interface

			// Variaveis para desproteger a memoria
			DWORD flOldProtect, flNewProtect, flDontCare;
			MEMORY_BASIC_INFORMATION mbi;

			VirtualProtect((PVOID)pvtbl, 38, PAGE_EXECUTE_READWRITE, &flOldProtect);

			DWORD* g_pCreateSurface = NULL;
			DWORD* pCreateSurface;

			pCreateSurface = (DWORD*)(DWORD(pvtbl) + 24);
			g_pCreateSurface = (DWORD*)*pCreateSurface;

			OldCreateSurface = (MyCreateSurface_Type)g_pCreateSurface;
			DWORD* pMyCreateSurface = (DWORD*)&H_CreateSurface;
			*pCreateSurface = (DWORD)pMyCreateSurface;
			VirtualProtect((PVOID)pvtbl, 30, flOldProtect, &flDontCare);
		}
		__except (1) {
			debugar("Erro n° 33");
		}
		return retval;
	}
	__except (1) {
		debugar("Error n° 0099");
	}
    VMProtectEnd();
}
