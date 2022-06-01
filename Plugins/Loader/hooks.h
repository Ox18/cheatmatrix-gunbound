//---------------------------------------------------------------------------

#ifndef hooksH
#define hooksH

#include "windows.h"
#include <stdio.h>
#include <stdlib.h>
#include <imagehlp.h>
#include <tlhelp32.h>
//#include <Winternl.h>
#include "variaveis.h"

typedef struct FuncList
{
   LPCSTR lpszModuleName;
   LPCSTR lpszFunctionName;
   LPVOID lpNewAddress;
   PROC lpOldAddress;
   bool habilitado;
   bool *habilitarCondicional;
}* PFUNCLIST;

typedef enum _HOOK_INDEX{
   iLoadLibraryA,
   iLoadLibraryW,
   iFreeLibrary,
   iLoadLibraryExA,
   iLoadLibraryExW,
   iMessageBoxA,
   iCreateProcessA,
   iCreateProcessW,
   iUnmapViewOfFile,
   iRtcShell,
   iRecv,
   iSend,
   iRecvFrom,
   iSendTo ,
   iWSARecv
} HOOK_INDEX;

extern FuncList Functions[];

HMODULE WINAPI LoadLibraryAHooked(LPCSTR lpszModuleName);
HMODULE WINAPI LoadLibraryWHooked(PCWSTR lpszModuleName);
HMODULE WINAPI LoadLibraryExAHooked(PCSTR lpszModuleName, HANDLE hFile, DWORD dwFlags);
HMODULE WINAPI LoadLibraryExWHooked(PCWSTR lpszModuleName, HANDLE hFile, DWORD dwFlags);

LPDWORD HookImports(HMODULE lpmodulo, LPCSTR lpszModuleName, LPCSTR lpszFunctionName, LPVOID lpNewAddress);
LPDWORD HookExports(HMODULE lpszModule, LPCSTR lpszFunctionName, LPVOID lpNewAddress);

bool HookarPorDetour(int index, PVOID func, PVOID callback, PVOID tranpolin);
void RemoverHookPorDetour(int index);
int UnHookFunctions();
int HookFunctions(HMODULE modulo);
bool InjetarDLL(HANDLE processo, int threeadID, char *dllName);
bool ReRouteAPI(HMODULE hMod, char* pszDllName, char* pszFunctionName, DWORD dwNewAddress);
//---------------------------------------------------------------------------
#endif


