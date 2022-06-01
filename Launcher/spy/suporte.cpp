// ---------------------------------------------------------------------------

#pragma hdrstop

#include "suporte.h"
#include "debugutils.h"
#include <tlhelp32.h>
// ---------------------------------------------------------------------------
#pragma package(smart_init)

#define CREATE_THREAD_ACCESS (PROCESS_CREATE_THREAD | PROCESS_QUERY_INFORMATION | PROCESS_VM_OPERATION | PROCESS_VM_WRITE | PROCESS_VM_READ)

unsigned long GetTargetProcessIdFromProcname(char *processo, int id) { // char *procName
	PROCESSENTRY32 pe;
	HANDLE thSnapshot;
	BOOL retval, ProcFound = false;

	if(id <= 0)
    	id = 1;

	unsigned long pid = 0;
	thSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	if (thSnapshot == INVALID_HANDLE_VALUE) {
		return 0;
	}

	pe.dwSize = sizeof(PROCESSENTRY32);

	retval = Process32First(thSnapshot, &pe);
	int i = 0;
	while (retval) {
		__try {
			if (stricmp(pe.szExeFile, processo) == 0) {
				i++;
				if(i == id)
					pid = pe.th32ProcessID;
			}
		} __except (1) {
			debugar("Erro!");
		}

		retval = Process32Next(thSnapshot, &pe);
		pe.dwSize = sizeof(PROCESSENTRY32);
	}
	CloseHandle(thSnapshot);
	return pid;
}

BOOL InjectDLL(DWORD ProcessID, char *dllName) {
	HANDLE Proc;
	char buf[50] = {
		0
	};
	LPVOID RemoteString, LoadLibAddy;
	if (!ProcessID)
		return false;
	Proc = OpenProcess(CREATE_THREAD_ACCESS, FALSE, ProcessID);
	if (!Proc) {
		return false;
	}
	LoadLibAddy = (LPVOID)GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
	RemoteString = (LPVOID)VirtualAllocEx(Proc, NULL, strlen(dllName), MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
	DWORD written = 0;
	WriteProcessMemory(Proc, (LPVOID)RemoteString, dllName, strlen(dllName), &written);
	DWORD tid = 0;
	CreateRemoteThread(Proc, NULL, NULL, (LPTHREAD_START_ROUTINE)LoadLibAddy, (LPVOID)RemoteString, NULL, &tid);
	CloseHandle(Proc);
	return true;
}
