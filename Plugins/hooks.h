//---------------------------------------------------------------------------

#ifndef hooksH
#define hooksH

#include "windows.h"
#include <stdio.h>
#include <stdlib.h>
#include <imagehlp.h>
#include <tlhelp32.h>
#include <Winternl.h>
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
   iRecv,
   iSend,
   iRecvFrom,
   iSendTo ,
   iWSARecv
} HOOK_INDEX;

//---------------------------------------------------------------------------
extern FuncList Functions[];

void HookDetour(int index, PVOID func, PVOID callback, PVOID tranpolin);
void UnhookDetour(int index);

int HookFunctions(HMODULE modulo);
int UnHookFunctions();

LPDWORD HookImports(HMODULE lpmodulo, LPCSTR lpszModuleName, LPCSTR lpszFunctionName, LPVOID lpNewAddress);
LPDWORD HookExports(LPCSTR lpszModuleName, LPCSTR lpszFunctionName, LPVOID lpNewAddress);
int HookFunctionsIAT();


#endif
