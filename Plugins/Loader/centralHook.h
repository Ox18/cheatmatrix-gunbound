//---------------------------------------------------------------------------

#ifndef centralHookH
#define centralHookH

#include "windows.h"
//#include <stdio.h>
//#include <stdlib.h>
#include <imagehlp.h>
#include <tlhelp32.h>

//---------------------------------------------------------------------------

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
   iLoadLibraryExA,
   iLoadLibraryExW,
   iMessageBoxA,
   iCreateProcessA,
   iCreateProcessW,
   iRtcShell,
   iRecv,
   iSend,
   iRecvFrom,
   iSendTo ,
   iWSARecv
} HOOK_INDEX;

extern FuncList Functions[];

LPDWORD HookImports(HMODULE lpmodulo, LPCSTR lpszModuleName, LPCSTR lpszFunctionName, LPVOID lpNewAddress);
LPDWORD HookExports(LPCSTR lpszModuleName, LPCSTR lpszFunctionName, LPVOID lpNewAddress);

bool HookarPorDetour(int index, PVOID func, PVOID callback, PVOID tranpolin);
void RemoverHookPorDetour(int index);
int UnHookFunctions();
int HookFunctions(HMODULE modulo);
bool InjetarDLL(HANDLE processo, int threeadID, char *dllName);

#endif
