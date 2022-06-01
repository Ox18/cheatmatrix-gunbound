//---------------------------------------------------------------------------

#pragma hdrstop
#include "baseCentral.h"
#include "..\secret.h"
#include "hooks.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "minIni.h"
#include "..\Md5.h"
#include "..\client.h"
#include "..\VMProtectSDK.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

#define CREATE_THREAD_ACCESS (PROCESS_CREATE_THREAD | PROCESS_QUERY_INFORMATION | PROCESS_VM_OPERATION | PROCESS_VM_WRITE | PROCESS_VM_READ)
#define DLL_EXPORT __declspec(dllexport)
#define sizearray(a)  (sizeof(a) / sizeof((a)[0]))

//#define MODO_DEBUG

typedef HRESULT(__stdcall PASCAL *MyDirectDrawCreateEx_Type)(DWORD*,DWORD*,DWORD*,DWORD*);//GUID FAR *, LPVOID  *, REFIID , IUnknown FAR *);
//MyBlt_Type ddBLT = 0;

void onExit();
Cliente *cliente;
////
////  Callback do Blt
////
HRESULT __stdcall H_Blt(LPVOID *param1 , LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx) {
	HRESULT retval = 0;

	__try {
    	// Pega o ponteiro do método original, armazenado no recipiente
		SURFACE_TABLE *tabela = (SURFACE_TABLE *)*param1;

		MyBlt_Type myBlt = (MyBlt_Type)tabela->tabelaAntiga->Blt;

		if(lpDDSrcSurface != NULL){
			HDC dc = 0;
			__try {
				HRESULT rdc = 0;
                ((LPDIRECTDRAWSURFACE7)(lpDDSrcSurface))->Restore();
				rdc = ((LPDIRECTDRAWSURFACE7)(lpDDSrcSurface))->GetDC(&dc);


					//char teste[100];
					//int n = sprintf(teste, "teste 2");
					//TextOut(dc, 10, 400, teste, n);

					//DesenharStatus(dc);
				if(rdc == DD_OK){
					chamarDraw(dc, TD_DDRAW);
				}
				((LPDIRECTDRAWSURFACE7)(lpDDSrcSurface))->ReleaseDC(dc);
			} __except(1) {
			}
		}else{
			if(mostrarStatus) {
				chamarDraw(0, TD_DDRAW);
			}
		}

		retval = myBlt(param1, lpDestRect, lpDDSrcSurface, lpSrcRect, dwFlags, lpDDBltFx);

		canDrawNow = true;
	} __except(1) {
		return DD_FALSE;
	}
	return retval;
}
//
//HRESULT __stdcall H_Release(LPVOID *param1) {
//	HRESULT retval = 0;
//
//	__try {
//    	// Pega o ponteiro do método original, armazenado no recipiente
//		SURFACE_TABLE *tabela = (SURFACE_TABLE *)*param1;
//		MyRelease_Type myRelease = (MyRelease_Type)tabela->tabelaAntiga->Release;
//		//debugar("Releasing surface...");
//		memcpy((PVOID)&tabela->novaTabela, (PVOID)tabela->tabelaAntiga, sizeof tabela->novaTabela);
//		retval = myRelease(param1);
//		free(tabela->tabelaAntiga);
//	} __except(1) {
//		debugar("Releasing Error");
//		return DD_FALSE;
//	}
//	return retval;
//}
//
////
////  CreateSurface Hook
////
//HRESULT __stdcall PASCAL H_CreateSurface( LPVOID *param1 ,  LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter) {
//	// Chama o CreateSurface original
//	//debugar("Creating surface...");
//	//debugar("sfc start");
//	__try{
//		LPDIRECTDRAWSURFACE7 *surface;
//		//debugar("sfc 1");
//		HRESULT retval = OldCreateSurface(param1 , lpDDSurfaceDesc, (void**)&surface, pUnkOuter);
//		//debugar("sfc 2 %x",surface);
//		SURFACE_TABLE *tabela = (SURFACE_TABLE *)*surface;
//		//debugar("sfc 3 %x", tabela);
//		//debugar("sfc 3.0 %x", tabela->novaTabela.Blt);
//		//debugar("sfc 3.0.1 %x", &H_Blt);
//		if((DWORD)tabela->novaTabela.Blt != (DWORD)&H_Blt) {
//			//debugar("sfc 3.1");
//			tabela->tabelaAntiga = (SURFACE_TABLE_BASE*) malloc(sizeof tabela->novaTabela);
//			//debugar("sfc 3.2");
//			memset(tabela->tabelaAntiga, 0, sizeof tabela->novaTabela);
//			//debugar("sfc 3.3");
//			memcpy(tabela->tabelaAntiga, &tabela->novaTabela, sizeof tabela->novaTabela);
//			//debugar("sfc 4");
//		}
//		//debugar("sfc 5");
//		DWORD flOldProtect, flDontCare;
//		VirtualProtect((PVOID)&tabela->novaTabela.Blt, 4, PAGE_EXECUTE_READWRITE, &flOldProtect);
//		//debugar("sfc 6");
//		//tabela->novaTabela.Flip = tabela->novaTabela.Blt
//		tabela->novaTabela.Blt = (long*)&H_Blt;
//		VirtualProtect((PVOID)&tabela->novaTabela.Blt, 4, flOldProtect, &flDontCare);
//		//debugar("sfc 7");
//		VirtualProtect((PVOID)&tabela->novaTabela.Release, 4, PAGE_EXECUTE_READWRITE, &flOldProtect);
//		//tabela->novaTabela.Flip = tabela->novaTabela.Blt
//		tabela->novaTabela.Release = (long*)&H_Release;
//		VirtualProtect((PVOID)&tabela->novaTabela.Release, 4, flOldProtect, &flDontCare);
//        //debugar("sfc 8");
//		*lplpDDSurface = surface;
//		return retval;
//	}__except(1){
//    	debugar("Erro csfc");
//    	return DD_FALSE;
//	}
//}

//HRESULT __stdcall H_GetDeviceState(LPVOID *param1 , DWORD cbData, LPVOID lpvData){
//	HRESULT retval = 0;
//	//debugar("GetDeviceState");
//	__try {
//		INPUT_DEVICE_TABLE *tabela = (INPUT_DEVICE_TABLE *)*param1;
//		MyGetDeviceState_Type myGDS = (MyGetDeviceState_Type)tabela->tabelaAntiga->GetDeviceState;
//		retval = myGDS(param1, cbData, lpvData);
//	} __except(1) {
//		debugar("Erro dvc");
//		return DD_FALSE;
//	}
//	return retval;
//}

//HRESULT __stdcall PASCAL H_CreateDevice(LPVOID *param1 , REFGUID rguid, LPDIRECTINPUTDEVICE * lplpDirectInputDevice, LPUNKNOWN pUnkOuter){
//	__try{
//	/*
//		LPDIRECTINPUT8 objeto = (LPDIRECTINPUT8)param1;
//		LPDIRECTINPUTDEVICE *device;
//		//debugar("CreateDevice - Old(original): %X %X %X %X", (DWORD*)OldCreateDevice, param1, *param1, &objeto->CreateDevice);
//		//HRESULT retval = OldCreateDevice(param1, rguid, (LPDIRECTINPUTDEVICE*)&device, pUnkOuter);
//		HRESULT retval = OldCreateDevice(param1, rguid, (LPDIRECTINPUTDEVICE*)lplpDirectInputDevice, pUnkOuter);
//		//debugar("armazenado em device");
//       */
//		/*
//		INPUT_DEVICE_TABLE *tabela = (INPUT_DEVICE_TABLE *)*device;
//		if((DWORD)tabela->novaTabela.GetDeviceState != (DWORD)&H_GetDeviceState) {
//			tabela->tabelaAntiga = (INPUT_DEVICE_TABLE_BASE*) malloc(sizeof tabela->novaTabela);
//			memset(tabela->tabelaAntiga, 0, sizeof tabela->novaTabela);
//			memcpy(tabela->tabelaAntiga, &tabela->novaTabela, sizeof tabela->novaTabela);
//		}
//
//		if(rguid == GUID_SysKeyboard){
//			debugar("Instalando hook em GetDeviceState %X", tabela->novaTabela.GetDeviceState);
//			DWORD flOldProtect, flDontCare;
//			VirtualProtect((PVOID)&tabela->novaTabela.GetDeviceState, 4, PAGE_EXECUTE_READWRITE, &flOldProtect);
//			//tabela->novaTabela.GetDeviceState = (long*)&H_GetDeviceState;
//			VirtualProtect((PVOID)&tabela->novaTabela.GetDeviceState, 4, flOldProtect, &flDontCare);
//			debugar("Create Device Hookado!");
//		}   */
//		//*lplpDirectInputDevice = (LPDIRECTINPUTDEVICE)device;
//		//debugar("CreateDevice retornando %d", retval);
//		//return retval;
//	}__except(1){
//    	return DD_FALSE;
//	}
//}
//
////-------------------------------------------------------------------------
////   Trampolim do DirectDrawCreateEx
////-------------------------------------------------------------------------
//extern "C" DLL_EXPORT DWORD WINAPI O_DirectDrawCreateEx( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter ) {
//    _asm {
//        nop
//        nop
//		nop
//		nop
//        nop
//        nop
//		nop
//        nop
//        nop
//        nop
//		nop
//        ret
//	}
//}

////-------------------------------------------------------------------------
////   CallBack do DirectDrawCreateEx
////-------------------------------------------------------------------------
//HRESULT WINAPI H_DirectDrawCreateEx( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter ) {
//	__try {
//
//		// Chama o DirectDrawCreateEx original
//		HRESULT retval = O_DirectDrawCreateEx(lpGuid, lplpDD, iid, pUnkOuter);
//		//debugar("ddr start %x - %x - %x", lpGuid, lplpDD, *(PDWORD*)lplpDD);
//		glpdd = (void*)*lplpDD;
//
//		// Precisamos do driver para cuidar da proteção da memória
//		//BOOL DrvLoad = IsDriverLoaded();
//
//		__try {
//			//Pega o pontero da interface
//			DWORD* ppvtbl = (DWORD*)*lplpDD; // Endereço da variavel
//			DWORD* pvtbl = (DWORD*) *ppvtbl; // Endereço da interface
//
//			//Variaveis para desproteger a memoria
//			DWORD flOldProtect, flNewProtect, flDontCare;
//			MEMORY_BASIC_INFORMATION mbi;
//
//			//Se o driver não estiver carregado não hooka, evitando crash do sistema
//			//if(DrvLoad)
//			//	CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 30, PAGE_EXECUTE_READWRITE, &flOldProtect);
//			//else
//				VirtualProtect((PVOID)pvtbl, 38, PAGE_EXECUTE_READWRITE, &flOldProtect);
//
//			DWORD*	g_pCreateSurface = NULL;
//			DWORD*  pCreateSurface;
//
//			pCreateSurface = (DWORD*)( DWORD(pvtbl) + 24 );
//			g_pCreateSurface = (DWORD*)*pCreateSurface;
//
//			OldCreateSurface = (MyCreateSurface_Type)g_pCreateSurface;
//
//			DWORD* pMyCreateSurface = (DWORD*)&H_CreateSurface;
//
//			//replace CreateDevice entry with my own function
//			*pCreateSurface = (DWORD)pMyCreateSurface;
//
//
//
//			//if(DrvLoad)
//			//	CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 30, flOldProtect, &flDontCare);
//			//else
//				VirtualProtect((PVOID)pvtbl, 30, flOldProtect, &flDontCare);
//		} __except(1) {
//			PerformError("Erro n° 33");
//		}
//		//SendMessage(HWND_BROADCAST, SPY_MESSAGE, 5, 1 );
//		RemoverHookPorDetour(0);
//		return retval;
//	}__except(1){
//    	debugar("Error n° 0099");
//	}
//
//}

//-------------------------------------------------------------------------
//   Trampolim do DirectDrawCreateEx
//-------------------------------------------------------------------------
extern "C" DLL_EXPORT DWORD WINAPI O_DirectInput8Create(HINSTANCE hinst, DWORD dwVersion, REFIID riidltf, LPVOID *ppvOut, LPUNKNOWN punkOuter){
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
        nop
        ret
    }
}

//-------------------------------------------------------------------------
//   CallBack do DirectDrawCreateEx
//-------------------------------------------------------------------------
HRESULT WINAPI H_DirectInput8Create(HINSTANCE hinst, DWORD dwVersion, REFIID riidltf, LPVOID *ppvOut, LPUNKNOWN punkOuter){
	 __try {
		// Chama o DirectDrawCreateEx original
		HRESULT retval = O_DirectInput8Create(hinst, dwVersion, riidltf, ppvOut, punkOuter);
	  /*
		lpdi = *(LPDIRECTINPUT*)ppvOut;
		debugar("teste: %X", &lpdi->CreateDevice);
		debugar("DirectInput8 %X", ppvOut);
		__try {
			//Pega o pontero da interface
			DWORD* ppvtbl = (DWORD*)*ppvOut; // Endereço da variavel
			DWORD* pvtbl = (DWORD*) *ppvtbl; // Endereço da interface

			DWORD flOldProtect, flNewProtect, flDontCare;
			MEMORY_BASIC_INFORMATION mbi;

			DWORD*	g_pCreateDevice = NULL;
			DWORD*  pCreateDevice;

			pCreateDevice = (DWORD*)( DWORD(pvtbl) + 12 );
			VirtualProtect((PVOID)pCreateDevice, 4, PAGE_EXECUTE_READWRITE, &flOldProtect);
			g_pCreateDevice = (DWORD*)*pCreateDevice;
			OldCreateDevice = (MyCreateDevice_Type)g_pCreateDevice;
			debugar("CreateDevice original em %X", OldCreateDevice);
			DWORD* pMyCreateDevice = (DWORD*)&H_CreateDevice;
			debugar("CreateDevice substituto em %X",pMyCreateDevice);
			//*pCreateDevice = (DWORD)pMyCreateDevice;
			VirtualProtect((PVOID)pCreateDevice, 4, flOldProtect, &flDontCare);
		} __except(1) {
			PerformError("Erro n° 331");
		}                    */
		RemoverHookPorDetour(1);
		return retval;
	}__except(1){
		debugar("Erro 9929");
	}
}

//
//  Pega endereço de uma função na dll informada
//
PVOID GetAddress(char *lib, char *func) {
	__try{
		HANDLE libH = LoadLibrary(lib);
		PVOID address = (PVOID)GetProcAddress((HMODULE)libH, func);
		return address;
	}__except(1){
	}
}

//
//  Lê o arquivo de configurações e pega o prefixo do nome das funções
//  da dll de exports de nomes mutantes
//
char *CompletaPrefixo(char *index) {
	__try{
		char *buf = (char *)malloc(MAX_PATH);
		CM_StrCat(DLLPath, "configs.ini", buf);
		TCMIniFile *ini = new TCMIniFile(buf);
		char *prefixo = ini->ReadString("configs", "Prefix", "F");
		CM_StrCat(prefixo, index, buf);
		free(ini);
		return buf;
	}__except(1){
	}
}

bool GravaPorta(int porta) {
	__try{
		char *buf = (char *)malloc(MAX_PATH);
		CM_StrCat(DLLPath, "configs.ini", buf);
		TCMIniFile *ini = new TCMIniFile(buf);
		ini->WriteInteger("configs", "ses", porta * 9269 + 37);
		free(ini);
		return buf;
	}__except(1){
	}
}

int nSize = 0;

/*
extern "C" DLL_EXPORT bool __stdcall m1(DWORD ProcessID) {
   HANDLE Proc;
   char buf[50]={0};
   LPVOID RemoteString, LoadLibAddy;

   ProcessID -= 5436;
   debugar("Calling inject from Matrix for ID %d at %d", ProcessID, GetTickCount());

   if(!ProcessID)
      return false;

   Proc = OpenProcess(CREATE_THREAD_ACCESS, FALSE, ProcessID);

   if(!Proc){
      //sprintf(buf, "OpenProcess() failed: %d", GetLastError());
      //MessageBox(NULL, buf, "Loader", NULL);
      return false;
   }

   LoadLibAddy = (LPVOID)GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");

   RemoteString = (LPVOID)VirtualAllocEx(Proc, NULL, strlen(nomeDLL), MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
 DWORD written = 0;
 WriteProcessMemory(Proc,(LPVOID)RemoteString, nomeDLL, strlen(nomeDLL), &written);
 DWORD tid = 0;
 CreateRemoteThread(Proc, NULL, NULL, (LPTHREAD_START_ROUTINE)LoadLibAddy, (LPVOID)RemoteString, NULL, &tid);
 CloseHandle(Proc);
 //debugar("dll: %s [%d]", nomeDLL, strlen(nomeDLL));
 return true;
 }
 */

const char *mystristr(const char *haystack, const char *needle) {
	if (!*needle) {
		return haystack;
	}
	for (; *haystack; ++haystack) {
		if (toupper(*haystack) == toupper(*needle)) {
			const char *h, *n;
			for (h = haystack, n = needle; *h && *n; ++h, ++n) {
				if (toupper(*h) != toupper(*n)) {
					break;
				}
			}
			if (!*n) {
				return haystack;
			}
		}
	}
	return 0;
}

void onExit() {
	// MessageBoxA(0,"Saido...","saindo",0);
	// debugar("Saindo...");
}

// DWORD WINAPI ThreadAliveCM(LPVOID lpParam) {
// int total = 0;
// while(true){
// Sleep(5);
// total++;
// if(total > 200){
// total = 0;
// debugar("Ainda rodando...");
// }
// }
// }

extern "C" __stdcall __declspec(dllexport) bool WINAPI _s0(int porta) {
	// ddBLT = (MyBlt_Type)param1;
	// return (DWORD)H_Blt;
	__try {
		if (!cliente->conectado)
			cliente->conectar(porta);
	}
	__except (1) {
		return false;
	}
	return cliente->conectado;
}

int inicializaDllMae() {
	VMProtectBegin("inDMae");

	VMProtectEnd();
}

#pragma argsused

int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved) {
	// debugar("teste");
	switch (reason) {
	case DLL_PROCESS_ATTACH: {
			DisableThreadLibraryCalls(hinst);
			instanceDLL = hinst;
			moduloDLL = GetModuleHandleA(0);
			// return inicializaDllMae();

			char str[100];
			char outStr[100];
			char hashNomeArquivo[100];
			int i = 0;
			char *nomeIni = NULL;
			char cCurrentPath[FILENAME_MAX];

			__try {
				WSAStartup(0x101, &dataWSA);
			}
			__except (1) {
			}
			cliente = new Cliente((PROC)processar);
			cliente->inicializar();

			atexit(onExit);
			//
			// Inicializa funcao de debug
			fOutputDebugStringA = (hOutputDebugStringA)GetProcAddress(GetModuleHandleA("kernel32.dll"), "OutputDebugStringA");
			if (!fOutputDebugStringA)
				MessageBox(0, "Erro no output!", "Bad Boy", 0);

			if (!GetCurrentDirectoryA(sizeof(cCurrentPath), &cCurrentPath[0])) {
				return errno;
			}
			GetCurrentProcessName(&nomeArquivo);
			const char *sistema = mystristr(cCurrentPath, "\\windows\\system32");
			if (!sistema) {

				debugar("Iniciando...");

				//
				// Diretorio da dll que está sendo injetada
				char *DLLPath1 = (char *)malloc(MAX_PATH);
				DLLPath = (char *)malloc(MAX_PATH);
				GetModuleFileNameA(instanceDLL, &DLLPath1[0], MAX_PATH);
				MyExtractFilePath(DLLPath1, DLLPath);
				free(DLLPath1);

				__try {
					wchar_t x;
					int totlen = (CM_StrLen(DLLPath) + 9) * (sizeof x);
					ImageName = new wchar_t[totlen];
					memset(ImageName, 0x00, totlen);
					mbstowcs(ImageName, DLLPath, CM_StrLen(DLLPath));

					nomeDLL = (char*)malloc(0x104);
					nSize = GetModuleFileNameA(instanceDLL, nomeDLL, 0x104);
					GetModuleFileNameA(instanceDLL, nomeDLL, nSize + 2);
					//
					// Nome do arquivo de imagem TGA do menu (WCHAR)
					wcscat(ImageName, L"stat.dat");

					// Nome do arquivo de imagem TGA do menu (PCHAR)
					ImageNameA = new char[CM_StrLen(DLLPath) + 9];
					CM_StrCat(DLLPath, "stat.dat", ImageNameA);

					nomeArquivo = (char*)malloc(0x104);
					GetCurrentProcessName(&nomeArquivo);

					strcat(nomeArquivo, "-");
					for (i = 0; i < strlen(nomeArquivo); i++) {
						nomeArquivo[i] = tolower(nomeArquivo[i]);
					}
					MDString(nomeArquivo, hashNomeArquivo);
					nomeIni = (char *)malloc(MAX_PATH);
					CM_StrCat(DLLPath, "configs.ini", nomeIni);
					for (i = 0; ini_getkey("other", i, str, sizearray(str), nomeIni) > 0; i++) {
						ini_gets("other", str, "", outStr, sizearray(outStr), nomeIni);
						isGunbound = (stricmp(hashNomeArquivo, outStr) == 0);
						if (isGunbound)
							break;
					}

					// debugar("Iniciando hook...");
					HookFunctions(0);

					// Aguarda uns instantes...
					Sleep(800);
				}
				__except (1) {
					debugar("Erro 6226");
					MessageBoxA(0, "Erro n° 138.", "Erro", MB_ICONERROR);
					HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
					TerminateProcess(proc, 0);
					CloseHandle(proc);
				}

				if (isGunbound) {
					Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
					__try {
						Sleep(300);
					}
					__except (1) {
					}

					// __try {
					// //
					// // WSACleanup();
					// WSAStartup(0x101, &dataWSA);
					// }
					// __except (1) {
					// }

					//
					// Inicializa as threads de processamento
					inicializarDLL();

					servidor = new Servidor((PROC)processar);
					servidor->onClientDisconnect = (PROC)OnClientDisconnect;

					//
					// Procura uma porta livre para conexão do servidor
					for (int i = 1313, j = 0; i < 30999; i++) {
						if (servidor->conectar(i)) {
							break;
						}
						debugar("Erro 9222. Nº %d", i);
						if (i == 30999) {
							return 0;
						}
					}
					GravaPorta(servidor->getPorta());
					inicializado = true;
					int total = 0;
				}
			}
			else {
			}
			return 1;
		} break;
	case DLL_PROCESS_DETACH: {
			//debugar("descarregando...");
			delete servidor;
			Gdiplus::GdiplusShutdown(gdiplusToken);
			debugar("Descarregado!");
		}
        break;

        default:  break;
    }
    return true;

}
