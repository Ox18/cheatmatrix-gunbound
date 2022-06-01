//---------------------------------------------------------------------------

#ifndef secretH
#define secretH

#include "windows.h"
#include <stdio.h>
#include <stdlib.h>
#include <Winternl.h>
//---------------------------------------------------------------------------

typedef struct _LDR_DATA_TABLE_ENTRY_SECRET {
	LIST_ENTRY InLoadOrderLinks;
	LIST_ENTRY InMemoryOrderModuleList;
	LIST_ENTRY InInitializationOrderModuleList;
	PVOID DllBase;
	PVOID EntryPoint;
	ULONG SizeOfImage;
	UNICODE_STRING FullDllName;
	UNICODE_STRING BaseDllName;
	ULONG Flags;
	USHORT LoadCount;
	USHORT TlsIndex;
	union{
		LIST_ENTRY HashLinks;
		PVOID SectionPointer;
	};
	ULONG CheckSum;
	union {
		ULONG TimeDateStamp;
		PVOID LoadedImports;
	};
	PVOID EntryPointActivationContext;
	PVOID PatchInformation;
} LDR_DATA_TABLE_ENTRY_SECRET, *PLDR_DATA_TABLE_ENTRY_SECRET;


typedef struct _LDR_MODULE_SECRET {

  LIST_ENTRY              InLoadOrderModuleList;
  LIST_ENTRY              InMemoryOrderModuleList;
  LIST_ENTRY              InInitializationOrderModuleList;
  PVOID                   BaseAddress;
  PVOID                   EntryPoint;
  ULONG                   SizeOfImage;
  UNICODE_STRING          FullDllName;
  UNICODE_STRING          BaseDllName;
  ULONG                   Flags;
  SHORT                   LoadCount;
  SHORT                   TlsIndex;
  LIST_ENTRY              HashTableEntry;
  ULONG                   TimeDateStamp;

} LDR_MODULE_SECRET, *PLDR_MODULE_SECRET;


typedef struct _PEB_LDR_DATA_SECRET{
  ULONG                   Length;
  BOOLEAN                 Initialized;
  PVOID                   SsHandle;
  LIST_ENTRY              InLoadOrderModuleList;
  LIST_ENTRY              InMemoryOrderModuleList;
  LIST_ENTRY              InInitializationOrderModuleList;
} PEB_LDR_DATA_SECRET, *PPEB_LDR_DATA_SECRET;

//----------------------------------------------------------------------

int GetCurrentProcessName(char **nome);
bool HideDll(HINSTANCE instanceDLL);


#endif
