// ---------------------------------------------------------------------------

#pragma hdrstop

#include "hooks.h"
#include "secret.h"
#include "acesso.h"
#include "funcoes.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)

FuncList Functions[] = {

	{"kernel32.dll", "LoadLibraryA", LoadLibraryAHooked, 0, false, 0}, // 0
	{"kernel32.dll", "LoadLibraryW", LoadLibraryWHooked, 0, false, 0},
	{"kernel32.dll", "FreeLibrary", FreeLibraryHooked, 0, false, 0}, // 1
	{"kernel32.dll", "LoadLibraryExA", LoadLibraryExAHooked, 0, false, 0}, // 2
	{"kernel32.dll", "LoadLibraryExW", LoadLibraryExWHooked, 0, false, 0}, // 3
	{"user32.dll", "MessageBoxA", MessageBoxAHook, 0, false, 0}, // 4
	// {"wsock32.dll"	,"recv",			RecvHooked, 			0, false}, //5
	// {"wsock32.dll"	,"send",			SendHooked, 			0, false}, //6
	// {"wsock32.dll"	,"recvfrom",		RecvFromHooked, 		0, false}, //7
	// {"wsock32.dll"	,"sendto",  		SendToHooked, 			0, false},  //8
	{"kernel32.dll", "CreateProcessA", CreateProcessAHooked, 0, true, 0}, // 9
	{"kernel32.dll", "CreateProcessW", CreateProcessWHooked, 0, true, 0},
	{"kernel32.dll", "UnmapViewOfFile", UnmapViewOfFileHooked, 0, false, 0}, // 10
	{"msvbvm60.dll ", "rtcShell", rtcShellHooked, 0, true, 0}, // 10

	// #ifdef MODO_DEBUG
	// {"ws2_32.dll"	,"recv",			RecvHooked, 			0, true, &isGunbound},  //11
	// {"ws2_32.dll"	,"send",			SendHooked, 			0, true, &isGunbound},  //12
	// #else
	// {"ws2_32.dll"	,"recv",			RecvHooked, 			0, false, &isGunbound},  //11
	// {"ws2_32.dll"	,"send",			SendHooked, 			0, false, &isGunbound},
	// #endif
	// {"ws2_32.dll"	,"recvfrom",		RecvFromHooked, 		0, false, &isGunbound},  //13
	// {"ws2_32.dll"	,"sendto",			SendToHooked, 			0, false, 0},  //14
	// {"ws2_32.dll"	,"WSARecv",			WSARecvHooked, 			0, false, 0}  //15

};

HMODULE WINAPI LoadLibraryExAHooked(PCSTR lpszModuleName, HANDLE hFile, DWORD dwFlags) {
	debugar("calling LoadLibraryExA");
	if (!Functions[iLoadLibraryExA].lpOldAddress) {
		Functions[iLoadLibraryExA].lpOldAddress = (PROC)LoadLibraryExA;
	}

	bool existeDLL = (GetModuleHandleA(lpszModuleName) != 0);

	typedef HMODULE(__stdcall *hLoadLibraryExA)(LPCSTR);
	hLoadLibraryExA originalLoadLibraryExA = (hLoadLibraryExA)Functions[iLoadLibraryExA].lpOldAddress;
	HMODULE resultado = originalLoadLibraryExA(lpszModuleName);

	PFUNCLIST pFuncList;
	DWORD dwNumOfFunctions = sizeof(Functions) / sizeof(struct FuncList);
	DWORD i = 0;

	// debugar("Total funcoes: %d", dwNumOfFunctions);
	if (!existeDLL)
		for (pFuncList = Functions, i = 0; i < dwNumOfFunctions; pFuncList++, i++) {
			if (pFuncList->habilitado && (!pFuncList->habilitarCondicional || *pFuncList->habilitarCondicional == true)) {
				// debugar("Hooking %s de %s ",pFuncList->lpszFunctionName, lpszModuleName);
				__try {
					ReRouteAPI((HMODULE)resultado, (char*)pFuncList->lpszModuleName, (char*)pFuncList->lpszFunctionName, (DWORD)pFuncList->lpNewAddress);
					if (!stricmp(pFuncList->lpszModuleName, lpszModuleName)) {
						// debugar("hooking Exports on LoadLibrary!");
						HookExports((HMODULE)resultado, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
					}
				}
				__except (1) {
					// debugar("erro 0001");
				}
			}
		}
	// HMODULE resultado = LoadLibraryA(lpszModuleName);
	// HookFunctions();
	debugar("LoadLibraryExA: %s [%X]", lpszModuleName, resultado);
	return resultado;

	return 0;
}

//
// ----------------------------------
// Callback para LoadLibraryExW
// ----------------------------------
//
HMODULE WINAPI LoadLibraryExWHooked(PCWSTR lpszModuleName, HANDLE hFile, DWORD dwFlags) {
	debugar("calling LoadLibraryExW");
	if(!Functions[iLoadLibraryExW].lpOldAddress) {
		Functions[iLoadLibraryExW].lpOldAddress = (PROC)LoadLibraryExW;
	}

	//debugar("LoadLibraryWHooked");
    bool existeDLL = (GetModuleHandleW(lpszModuleName) != 0);

	typedef HMODULE (__stdcall *hLoadLibraryExW)(PCWSTR);
	hLoadLibraryExW originalLoadLibraryExW = (hLoadLibraryExW)Functions[iLoadLibraryExW].lpOldAddress;
	HMODULE resultado = originalLoadLibraryExW(lpszModuleName);
	//HMODULE resultado = LoadLibraryW(lpszModuleName);
	//HookFunctionsIAT();
	LPSTR nomeDll, comandLine;
	UnicodeToAnsi(lpszModuleName,&nomeDll);
	debugar("LoadLibraryExW: %s [%X]", nomeDll, resultado);

	PFUNCLIST pFuncList;
	DWORD dwNumOfFunctions = sizeof(Functions) / sizeof(struct FuncList);
	DWORD i = 0;

	//debugar("Total funcoes: %d", dwNumOfFunctions);
	if(!existeDLL)
	for (pFuncList = Functions, i = 0; i < dwNumOfFunctions; pFuncList++, i++) {
		if (pFuncList->habilitado && (!pFuncList->habilitarCondicional || *pFuncList->habilitarCondicional == true)) {
			//debugar("Hooking %s de %s ",pFuncList->lpszFunctionName, lpszModuleName);
			__try{
				ReRouteAPI((HMODULE)resultado, (char*)pFuncList->lpszModuleName, (char*)pFuncList->lpszFunctionName, (DWORD)pFuncList->lpNewAddress );
				if (!stricmp(pFuncList->lpszModuleName, nomeDll)) {
					//debugar("hooking Exports on LoadLibrary!");
					HookExports((HMODULE)resultado, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
				}
			}__except(1){
				//debugar("erro 0001");
			}
		}
	}

	return resultado;
}

//
// ----------------------------------
// Callback para LoadLibraryA
// ----------------------------------
//
HMODULE WINAPI LoadLibraryAHooked(LPCSTR lpszModuleName) {

	if(!Functions[iLoadLibraryA].lpOldAddress) {
		Functions[iLoadLibraryA].lpOldAddress = (PROC)LoadLibraryA;
	}

	bool existeDLL = (GetModuleHandleA(lpszModuleName) != 0);

	typedef HMODULE (__stdcall *hLoadLibraryA)(LPCSTR);
	hLoadLibraryA originalLoadLibraryA = (hLoadLibraryA)Functions[iLoadLibraryA].lpOldAddress;
	HMODULE resultado = originalLoadLibraryA(lpszModuleName);

	PFUNCLIST pFuncList;
	DWORD dwNumOfFunctions = sizeof(Functions) / sizeof(struct FuncList);
	DWORD i = 0;

	//debugar("Total funcoes: %d", dwNumOfFunctions);
	if(!existeDLL)
	for (pFuncList = Functions, i = 0; i < dwNumOfFunctions; pFuncList++, i++) {
		if (pFuncList->habilitado && (!pFuncList->habilitarCondicional || *pFuncList->habilitarCondicional == true)) {
			//debugar("Hooking %s de %s ",pFuncList->lpszFunctionName, lpszModuleName);
			__try{
				ReRouteAPI((HMODULE)resultado, (char*)pFuncList->lpszModuleName, (char*)pFuncList->lpszFunctionName, (DWORD)pFuncList->lpNewAddress );
				if (!stricmp(pFuncList->lpszModuleName, lpszModuleName)) {
					//debugar("hooking Exports on LoadLibrary!");
					HookExports((HMODULE)resultado, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
				}
			}__except(1){
				//debugar("erro 0001");
			}
		}
	}
	//HMODULE resultado = LoadLibraryA(lpszModuleName);
	//HookFunctions();
	debugar("LoadLibraryA: %s [%X]", lpszModuleName, resultado);
	return resultado;
}

//
// ----------------------------------
// Callback para LoadLibraryW
// ----------------------------------
//
HMODULE WINAPI LoadLibraryWHooked(PCWSTR lpszModuleName) {

	if(!Functions[iLoadLibraryW].lpOldAddress) {
		Functions[iLoadLibraryW].lpOldAddress = (PROC)LoadLibraryW;
	}

	//debugar("LoadLibraryWHooked");
    bool existeDLL = (GetModuleHandleW(lpszModuleName) != 0);

	typedef HMODULE (__stdcall *hLoadLibraryW)(PCWSTR);
	hLoadLibraryW originalLoadLibraryW = (hLoadLibraryW)Functions[iLoadLibraryW].lpOldAddress;
	HMODULE resultado = originalLoadLibraryW(lpszModuleName);
	//HMODULE resultado = LoadLibraryW(lpszModuleName);
	//HookFunctionsIAT();
	LPSTR nomeDll, comandLine;
	UnicodeToAnsi(lpszModuleName,&nomeDll);
	debugar("LoadLibraryW: %s [%X]", nomeDll, resultado);

	PFUNCLIST pFuncList;
	DWORD dwNumOfFunctions = sizeof(Functions) / sizeof(struct FuncList);
	DWORD i = 0;

	//debugar("Total funcoes: %d", dwNumOfFunctions);
	if(!existeDLL)
	for (pFuncList = Functions, i = 0; i < dwNumOfFunctions; pFuncList++, i++) {
		if (pFuncList->habilitado && (!pFuncList->habilitarCondicional || *pFuncList->habilitarCondicional == true)) {
			//debugar("Hooking %s de %s ",pFuncList->lpszFunctionName, lpszModuleName);
			__try{
				ReRouteAPI((HMODULE)resultado, (char*)pFuncList->lpszModuleName, (char*)pFuncList->lpszFunctionName, (DWORD)pFuncList->lpNewAddress );
				if (!stricmp(pFuncList->lpszModuleName, nomeDll)) {
					//debugar("hooking Exports on LoadLibrary!");
					HookExports((HMODULE)resultado, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
				}
			}__except(1){
				//debugar("erro 0001");
			}
		}
	}

	return resultado;
}

char *ExtractFilePath(char *valor){
	for(char* i = (char*)(valor+strlen(valor)-1); i >= valor; i--){
		if(*i == '\\'){
			return ++i;
		}
	}
}

int HookFunctions(HMODULE lpModule) {
	ULONG_PTR ldr_addr;
	PEB_LDR_DATA_SECRET* ldr_data;
	LDR_MODULE_SECRET *modulo, *prec, *next;
	PFUNCLIST pFuncList;
	DWORD dwNumOfFunctions = sizeof(Functions) / sizeof(struct FuncList);
	DWORD i = 0;
	char moduleName[1024];
//	debugar("Modulo 0: %X / %X", GetModuleHandleA(0), instanceDLL);
	for (pFuncList = Functions, i = 0; i < dwNumOfFunctions; pFuncList++, i++) {
		if (pFuncList->habilitado && (!pFuncList->habilitarCondicional || *pFuncList->habilitarCondicional == true)) {
			HMODULE moduleH = GetModuleHandleA(pFuncList->lpszModuleName);
			HookExports(moduleH, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);

			if (pFuncList->habilitado && (!pFuncList->habilitarCondicional || *pFuncList->habilitarCondicional == true)) {
//				debugar("Hookando %s", pFuncList->lpszFunctionName);
				__asm mov eax, fs : [0x30] // get il PEB ADDR
				__asm add eax, 0xc
				__asm mov eax, [eax] // get LoaderData ADDR
				__asm mov ldr_addr, eax

				ldr_data = (PEB_LDR_DATA_SECRET*)ldr_addr; // init PEB_LDR_DATA struct.
				modulo = (LDR_MODULE_SECRET*)ldr_data->InLoadOrderModuleList.Flink;

				while (modulo->BaseAddress != 0) {
					// debugar("modulo: %X", modulo->BaseAddress);
					// debugar("instanceDLL: %X",instanceDLL);
					if ((ULONG_PTR)modulo->BaseAddress != (DWORD)instanceDLL) {
						// debugar("Hookando em %s",modulo->FullDllName);
						__try {
							memset(&moduleName[0], 0, 1024);
							GetModuleFileNameA((HMODULE)modulo->BaseAddress, &moduleName[0], 1024);
							char* nomeDLL = ExtractFilePath(&moduleName[0]);
							// debugar("Dll [%X] %s", modulo->BaseAddress,&nomeDLL[0]);

							ReRouteAPI((HMODULE)modulo->BaseAddress, (char*)pFuncList->lpszModuleName, (char*)pFuncList->lpszFunctionName, (DWORD)pFuncList->lpNewAddress);

							// if (!stricmp(pFuncList->lpszModuleName, &nomeDLL[0])) {
							// //debugar("Hooking exports of %s",pFuncList->lpszModuleName);
							// HookExports((HMODULE)modulo->BaseAddress, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
							// }
						}
						__except (1) {
//							debugar("Erro 0109");
						}
						// ReRouteAPI(modulo->BaseAddress, "kernel32.dll", "LoadLibraryA", (DWORD)LoadLibraryWHooked );
						// Get the precedent and the successive struct according to the initialization order
						// prec = (LDR_MODULE_SECRET*)(ULONG_PTR)((ULONG_PTR)modulo->InInitializationOrderModuleList.Blink - 16);
						// next = (LDR_MODULE_SECRET*)(ULONG_PTR)((ULONG_PTR)modulo->InInitializationOrderModuleList.Flink - 16);
					}

					modulo = (LDR_MODULE_SECRET*)modulo->InLoadOrderModuleList.Flink;
				}
				// debugar("Solicitando hook em %s de %s", nomeArquivo, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
				// PDWORD old1 = HookImports(0, pFuncList->lpszModuleName, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
				// PDWORD old2 = HookExports(GetModuleHandleA(pFuncList->lpszModuleName), pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
				// if ((DWORD)old1 != (DWORD)pFuncList->lpNewAddress) {
				// Functions[i].lpOldAddress = (PROC)old1;
				// pFuncList->lpOldAddress = (PROC)old1;
				// }
			}
		}
	}
	/*
	 PFUNCLIST pFuncList;
	 DWORD dwNumOfFunctions = sizeof(Functions) / sizeof(struct FuncList);
	 DWORD i = 0;
	 for (pFuncList = Functions, i = 0; i < dwNumOfFunctions; pFuncList++, i++) {
	 if (pFuncList->habilitado && (!pFuncList->habilitarCondicional || *pFuncList->habilitarCondicional == true)) {
	 // debugar("Solicitando hook em %s de %s", nomeArquivo, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
	 PDWORD old1 = HookImports(modulo, pFuncList->lpszModuleName, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
	 PDWORD old2 = HookExports(pFuncList->lpszModuleName, pFuncList->lpszFunctionName, pFuncList->lpNewAddress);
	 if ((DWORD)old1 != (DWORD)pFuncList->lpNewAddress) {
	 Functions[i].lpOldAddress = (PROC)old1;
	 pFuncList->lpOldAddress = (PROC)old1;
	 }
	 }
	 } */
}

int UnHookFunctions() {
	/*
	 PFUNCLIST pFuncList;
	 DWORD dwNumOfFunctions = sizeof(Functions) / sizeof(struct FuncList);
	 DWORD i = 0;
	 for(pFuncList = Functions, i = 0; i < dwNumOfFunctions;  pFuncList++, i++) {
	 if(pFuncList->habilitado){
	 if(!pFuncList->lpOldAddress){
	 HookImports(0, pFuncList->lpszModuleName, pFuncList->lpszFunctionName, pFuncList->lpOldAddress);
	 PDWORD old = 0;
	 pFuncList->lpOldAddress = (PROC)old;
	 }
	 }
	 } */
}

//
IMAGE_IMPORT_DESCRIPTOR* GetImportDescriptor(HMODULE hMod, char* pszDllName) {
	IMAGE_DOS_HEADER* pDOSHeader = (IMAGE_DOS_HEADER*)hMod;
	IMAGE_OPTIONAL_HEADER* pOptionHeader = (IMAGE_OPTIONAL_HEADER*)((BYTE*)hMod + pDOSHeader->e_lfanew + 24);
	IMAGE_IMPORT_DESCRIPTOR* pImportDesc = (IMAGE_IMPORT_DESCRIPTOR*)((BYTE*)hMod + pOptionHeader->DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
	char* pszImpAddr = 0;
	while (pImportDesc->FirstThunk) {
		pszImpAddr = (char*)((BYTE*)hMod + pImportDesc->Name);
		if (stricmp(pszDllName, pszImpAddr)) {
			pImportDesc++;
			continue;
		}
		else {
			return pImportDesc;
		}
	}
	return NULL;
}

IMAGE_THUNK_DATA* GetOriginalFirstThunk(HMODULE hMod, IMAGE_IMPORT_DESCRIPTOR* pImportDesc) {
	return (IMAGE_THUNK_DATA*)((BYTE*)hMod + pImportDesc->OriginalFirstThunk);
}

IMAGE_THUNK_DATA* GetFirstThunk(HMODULE hMod, IMAGE_IMPORT_DESCRIPTOR* pImportDesc) {
	return (IMAGE_THUNK_DATA*)((BYTE*)hMod + pImportDesc->FirstThunk);
}

// This function gets the address of the LoadLibraryA function.
DWORD* GetCurrentFunctAddr(HMODULE hMod, IMAGE_THUNK_DATA* pOriginalFirstThunk, IMAGE_THUNK_DATA* pFirstThunk, char* pszFunctionName) {
	// From the IMAGE thunk datastructure we
	// get the address of the imported function.
	char* szTest;
	while (pOriginalFirstThunk->u1.Function) {
		// Get the import load address of the function,
		// in our case LoadLibraryA.
		szTest = (char*)((BYTE*)hMod + (DWORD)pOriginalFirstThunk->u1.AddressOfData + 2);
		if (stricmp(pszFunctionName, szTest) == 0) {
			return &pFirstThunk->u1.Function;
		}
		pOriginalFirstThunk++;
		pFirstThunk++;
	}
	return NULL;
}

// Once address is found, we have to redirect using the VirtualProtect WinAPI
// that enables write permission to third party EXE.

bool ChangeAddress(DWORD* dwOldAddress, DWORD dwNewAddress) {
	// Change the old address of the function with the new address.
	// Firstly this address is changed.
	DWORD dwOld;
	if (!(VirtualProtect(dwOldAddress, 4, PAGE_READWRITE, &dwOld))) {
		return false;
	}
	*dwOldAddress = dwNewAddress;
	// Once changed it updated in the executable.
	if (!(VirtualProtect(dwOldAddress, 4, PAGE_EXECUTE, &dwOld))) {
		return false;
	}
	else {
		//OutputDebugString("Change Address Final..");
		return true;
	}
}

bool ReRouteAPI(HMODULE hMod, char* pszDllName, char* pszFunctionName, DWORD dwNewAddress) {
	//debugar("ReRouteAPI is entered.....");
	IMAGE_IMPORT_DESCRIPTOR* IID = GetImportDescriptor(hMod, pszDllName);
	if (IID == NULL)
		return false;
	IMAGE_THUNK_DATA* OriginalFirstThunk = GetOriginalFirstThunk(hMod, IID);
	IMAGE_THUNK_DATA* FirstThunk = GetFirstThunk(hMod, IID);
	//debugar("Change Address start..");
	DWORD* dwOldFunctionAddress = GetCurrentFunctAddr(hMod, OriginalFirstThunk, FirstThunk, pszFunctionName);
	if (dwOldFunctionAddress == NULL)
		return false;
	return ChangeAddress(dwOldFunctionAddress, dwNewAddress);
}

LPDWORD HookImports(HMODULE lpmodulo, LPCSTR lpszModuleName, LPCSTR lpszFunctionName, LPVOID lpNewAddress) {
	LPDWORD resultado = (LPDWORD)0;

	if (!Functions[iLoadLibraryA].lpOldAddress) {
		Functions[iLoadLibraryA].lpOldAddress = (PROC)LoadLibraryA;
	}/**/

	if (!lpmodulo)
		lpmodulo = GetModuleHandleA(0);
	//
   //	typedef HMODULE(__stdcall *hLoadLibraryA)(LPCSTR);
   //	hLoadLibraryA originalLoadLibraryA = (hLoadLibraryA)(hLoadLibraryA)Functions[iLoadLibraryA].lpOldAddress;

	LPDWORD enderecoFuncao = (LPDWORD)GetProcAddress(GetModuleHandleA(lpszModuleName), lpszFunctionName); // var_4

	if (!enderecoFuncao)
		return resultado;

	DWORD dwSize = 0;
	PIMAGE_IMPORT_DESCRIPTOR entryData = (PIMAGE_IMPORT_DESCRIPTOR)ImageDirectoryEntryToData(lpmodulo, true, IMAGE_DIRECTORY_ENTRY_IMPORT, &dwSize);

	if (!entryData)
		return resultado;

	while (entryData->Name) {
		PIMAGE_THUNK_DATA pThunk = (PIMAGE_THUNK_DATA)((DWORD)lpmodulo + entryData->FirstThunk);
		while (pThunk->u1.Function) {
			if ((DWORD)pThunk->u1.Function == (DWORD)enderecoFuncao) {
				VirtualProtect(pThunk, 4, 4, &dwSize);
				if (lpNewAddress && !IsBadCodePtr((PROC)lpNewAddress) && !IsBadWritePtr(pThunk, 4)) {
					// debugar("HKI - %s - %X", lpszFunctionName,	pThunk->u1.Function);
					pThunk->u1.Function = (DWORD)lpNewAddress;
				}
			}
			pThunk++;
		}
		entryData++;
	}
	resultado = enderecoFuncao;

	return resultado;
}

LPDWORD HookExports(HMODULE lpszModule, LPCSTR lpszFunctionName, LPVOID lpNewAddress) {
   /*	LPDWORD resultado = (LPDWORD)0;

	if (!Functions[iLoadLibraryA].lpOldAddress)
		Functions[iLoadLibraryA].lpOldAddress = (PROC)LoadLibraryA;

	typedef HMODULE(__stdcall *hLoadLibraryA)(LPCSTR);
	hLoadLibraryA originalLoadLibraryA = (hLoadLibraryA)Functions[iLoadLibraryA].lpOldAddress;

	HMODULE mLib = originalLoadLibraryA(lpszModuleName); // var_8
	 */
	 LPDWORD resultado = (LPDWORD)0;

	 HMODULE mLib = lpszModule;
	if (!mLib)
		return resultado;

	DWORD dwSize = 0;
	PIMAGE_EXPORT_DIRECTORY entryData = (PIMAGE_EXPORT_DIRECTORY)ImageDirectoryEntryToData(mLib, true, IMAGE_DIRECTORY_ENTRY_EXPORT, &dwSize);

	if (!entryData)
		return resultado;

	PDWORD AddressOfFunctions = (PDWORD)((DWORD)mLib + entryData->AddressOfFunctions); // var_18
	PWORD AddressOfNameOrdinals = (PWORD)((DWORD)mLib + (DWORD)entryData->AddressOfNameOrdinals);
	// var_30
	PDWORD AddressOfNames = (PDWORD)((DWORD)mLib + entryData->AddressOfNames); // var_1C
	DWORD NumberOfNames = entryData->NumberOfNames; // var_20
	DWORD NumberOfFunctions = entryData->NumberOfFunctions; // var_24
	DWORD indice = 0;

	// --- recomeça
	for (int i = 0; i < entryData->NumberOfNames; i++) { // i = var_28

		indice = AddressOfNameOrdinals[i]; // var_2C

		if (indice > NumberOfFunctions)
			return resultado;

		PCHAR nome = (PCHAR)((DWORD)mLib + AddressOfNames[i]);
		if (nome && stricmp(nome, lpszFunctionName) == 0) {
			resultado = (LPDWORD)((DWORD)mLib + AddressOfFunctions[indice]);

			VirtualProtect((PDWORD)((DWORD)AddressOfFunctions + indice*4), 4, PAGE_READWRITE, &dwSize);
//			debugar("HKE - %s - %X", lpszFunctionName, resultado);
			if (lpNewAddress && !IsBadCodePtr((PROC)lpNewAddress) && !IsBadWritePtr((PDWORD)((DWORD)AddressOfFunctions + indice * 4), 4))
				AddressOfFunctions[indice] = (DWORD)((DWORD)lpNewAddress - (DWORD)mLib);
		}
	}
	return resultado;
}

//
// Zera os dados de um item no array de hook
//
void ZeroValue(int index) {
	__try {
		for (int i = 0; i < 5; i++) {
			hooks[index].newdata[i] = 0;
			hooks[index].olddata[i] = 0;
			hooks[index].tramp[i] = 0;
		}
	}
	__except (1) {
	}
}

//
// Gera uma mensagem de erro
//
void PerformError(char *msg, char *titulo) {
	if (!titulo)
		MessageBoxA(NULL, msg, "Erro", NULL);
	else
		MessageBoxA(NULL, msg, titulo, NULL);
	HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
	TerminateProcess(proc, 0);
	CloseHandle(proc);
}

//
// Insere hook de CodeOverwrite
//
bool HookarPorDetour(int index, PVOID func, PVOID callback, PVOID tranpolin) {
	bool result = false;
	__try {
		ZeroValue(index);
		unsigned char* FuncHookPtr = (unsigned char*)func;
		unsigned char* HookTramp = (unsigned char*)tranpolin;
		hooks[index].original = (unsigned char*)FuncHookPtr;
		hooks[index].callback = (unsigned char*)callback;
		hooks[index].tranpolin = (unsigned char*)tranpolin;
		DWORD OldProtect;
		int i;

		hooks[index].newdata[0] = 0xE9;
		*(PDWORD) & (hooks[index].newdata[1]) = (DWORD)((DWORD)callback - ((DWORD)FuncHookPtr + 5));

		hooks[index].tramp[0] = 0x68; // Push
		*(PDWORD) & (hooks[index].tramp[1]) = ((DWORD)FuncHookPtr + 5);

		VirtualProtect(FuncHookPtr, 10, PAGE_EXECUTE_READWRITE, &OldProtect);
		// Unprotect the target memory. 10 bytes for good measure.

		// Copia os bytes originais
		for (i = 0; i < 5; i++) {
			hooks[index].olddata[i] = FuncHookPtr[i];
			FuncHookPtr[i] = hooks[index].newdata[i];
		}

		VirtualProtect(FuncHookPtr, 10, OldProtect, NULL);
		// Reprotect the memory.
		VirtualProtect(HookTramp, 25, PAGE_EXECUTE_READWRITE, &OldProtect);
		// Reprotect the memory.

		for (i = 0; i < 5; i++) {
			HookTramp[i] = hooks[index].olddata[i];
		}

		for (i = 0; i < 50; i++) {
			if (HookTramp[i] == 0x90 && HookTramp[i + 1] == 0x90 && HookTramp[i + 2] == 0x90 && HookTramp[i + 3] == 0x90 && HookTramp[i + 4] == 0x90) {
				HookTramp[i] = hooks[index].tramp[0];
				HookTramp[i + 1] = hooks[index].tramp[1];
				HookTramp[i + 2] = hooks[index].tramp[2];
				HookTramp[i + 3] = hooks[index].tramp[3];
				HookTramp[i + 4] = hooks[index].tramp[4];
				result = true;
				break;
			}
		}

		VirtualProtect(HookTramp, 25, OldProtect, NULL);
	}
	__except (1) {
		PerformError("Erro n° 24", "Erro");
	}
	return result;
}

//
// Remove hook de CodeOverwrite
//
void RemoverHookPorDetour(int index) {
	__try {
		unsigned char* FuncHookPtr = (unsigned char*)hooks[index].original;
		DWORD OldProtect;
		VirtualProtect(FuncHookPtr, 10, PAGE_EXECUTE_READWRITE, &OldProtect);
		// Restaura os 5 primeiros bytes
		for (int i = 0; i < 5; i++) {
			FuncHookPtr[i] = hooks[index].olddata[i];
		}
		VirtualProtect(FuncHookPtr, 10, OldProtect, NULL);
	}
	__except (1) {
	}
}

 __declspec(naked) loadDll(void) {
	_asm {
		// Placeholder for the return address
		push 0xDEADBEEF
		// Save the flags and registers
		pushfd
		pushad
		// Placeholder for the string address and LoadLibrary
		push 0xDEADBEEF
		mov eax, 0xDEADBEEF
		// Call LoadLibrary with the string parameter
		call eax
		// Restore the registers and flags
		popad
		popfd
		// Return control to the hijacked thread
		retn
	}
}

loadDll_end(void) {
}

bool InjetarDLL(HANDLE processo, int threeadID, char *dllName) {
	HANDLE hThread;
	CONTEXT contexto;
	DWORD oldIP, oldProtection;

	// debugar("Iniciando etapa Inject");

	if (!processo || !dllName) {
//		debugar("ir0");
		return false;
	}

#ifdef MODO_DEBUG
	// debugar("Injetando em %s", nomeArquivo);
#endif

	typedef int(__stdcall*hLoadLibraryA)(LPCSTR);
	PROC loadLib = 0;//(PROC)HookExports("kernel32.dll", "LoadLibraryA", 0);
	if (!loadLib) {
//		debugar("ir1");
		return false;
	}

	// debugar("Tentando injetar %d em %s", processo, dllName);
	PCHAR RemoteString = (PCHAR)VirtualAllocEx(processo, 0, strlen(dllName) + 1, MEM_COMMIT, PAGE_READWRITE);
	if (!RemoteString) {
//		debugar("ir2");
		return false;
	}
	WriteProcessMemory(processo, (LPVOID)RemoteString, dllName, strlen(dllName), NULL);
	DWORD stubLen = (unsigned long) & loadDll_end - (unsigned long) & loadDll;
	PBYTE stub = (PBYTE)VirtualAllocEx(processo, NULL, stubLen, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	if (!stub) {
		VirtualFreeEx(processo, RemoteString, strlen(dllName), 0x4000);
//		debugar("ir3");
		return false;
	}
	hThread = OpenThread((THREAD_GET_CONTEXT | THREAD_SET_CONTEXT | THREAD_SUSPEND_RESUME | THREAD_QUERY_INFORMATION), false, threeadID);
	if (!hThread) {
		VirtualFreeEx(processo, stub, stubLen, 0x4000);
		VirtualFreeEx(processo, RemoteString, strlen(dllName), 0x4000);
//		debugar("ir4");
		return false;
	}
	SuspendThread(hThread);
	contexto.ContextFlags = CONTEXT_CONTROL;
	GetThreadContext(hThread, &contexto);
	oldIP = contexto.Eip;
	contexto.Eip = (DWORD)stub;
	contexto.ContextFlags = CONTEXT_CONTROL;

	VirtualProtect(loadDll, stubLen, PAGE_EXECUTE_READWRITE, &oldProtection);
	memcpy((void *)((unsigned long)loadDll + 1), &oldIP, 4);
	memcpy((void *)((unsigned long)loadDll + 8), &RemoteString, 4);
	memcpy((void *)((unsigned long)loadDll + 13), &loadLib, 4);

	WriteProcessMemory(processo, stub, loadDll, stubLen, NULL);
	SetThreadContext(hThread, &contexto);
	ResumeThread(hThread);

	// Sleep(5000);
	// VirtualFreeEx(procInfo->hProcess, RemoteString, strlen(dllName), MEM_DECOMMIT);
	// VirtualFreeEx(procInfo->hProcess, stub, stubLen, MEM_DECOMMIT);
	CloseHandle(hThread);
	return true;
}
