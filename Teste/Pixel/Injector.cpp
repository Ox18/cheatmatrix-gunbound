/*
		hAx Injecter/Loader v1.0 by Zak Farrington alias fritz <fritz@hax-studios.net>
		A Generic DLL Injector and Loader
		Copyright (C) 2005 hAx Studios Ltd.
		All rights reserved.

		http://hax-studios.net
*/

#include <windows.h>
#include <tlhelp32.h>
#include <cstdio>
#include "Injector.h"


//** Injector.cpp: Inject function
BOOL InjectDLL(DWORD dwPID, char* strHookDLL)
{
	char strMessage[1024];

	//** Check if the hook dll exists
	DWORD dwAttr = GetFileAttributes(strHookDLL);
	if(dwAttr == 0xFFFFFFFF)
	{
		DWORD dwError = GetLastError();

		switch(dwError)
		{
			case ERROR_FILE_NOT_FOUND:
				//sprintf(strMessage, "Error: Could not find \"%s\".", strHookDLL);
				MessageBox(GetActiveWindow(), strMessage, "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
				return FALSE;
			break;
			
			case ERROR_PATH_NOT_FOUND:
				MessageBox(GetActiveWindow(), "Error: Invalid path to Hook DLL.", "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
				return FALSE;
			break;
			
			case ERROR_ACCESS_DENIED:
				//sprintf(strMessage, "Error: Access denied to '%s'.", strHookDLL//);
				MessageBox(GetActiveWindow(),  strMessage, "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
				return FALSE;
			break;

			default:
				MessageBox(GetActiveWindow(), "Error: A unknown error has occured.", "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
				return FALSE;
			break;
		}
	}
	else
	{
		if(dwAttr & FILE_ATTRIBUTE_DIRECTORY)
		{
			//sprintf(strMessage, "Error: '%s' is a directory.", strHookDLL);
			MessageBox(GetActiveWindow(), strMessage, "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
			return FALSE;
		}
	}

	if(dwPID == NULL)
	{
		MessageBox(GetActiveWindow(), "Error: Could not retrieve a valid process ID.", "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
		return FALSE;
	}

	HANDLE hProcess;								//** Will be the process we inject
	HMODULE hKernel;								//** Will hold Kernel module
	LPVOID lpExecString, lpLoadLibraryAddr;			//** Remote string and LoadLibrary() address holder

	hProcess = OpenProcess(PROCESS_ALL_ACCESS, FALSE, dwPID); //** Attempt to gain access to user-defined process

	if(hProcess == INVALID_HANDLE_VALUE) //** Error
	{
		MessageBox(GetActiveWindow(), "Error: Could not OpenProcess(PROCESS_ALL_ACCESS, FALSE, ...);", "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
		return FALSE;
	}

	hKernel = LoadLibrary("kernel32.dll");	//** Attempt to load the kernel

	if(hKernel == NULL)
	{
		MessageBox(GetActiveWindow(), "Error: Could not LoadLibrary(\"kernel32.dll\");", "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
		CloseHandle(hProcess);
		return FALSE;
	}

	lpLoadLibraryAddr = (LPVOID)GetProcAddress(hKernel, "LoadLibraryA");	//** Attempt to get LoadLibrary address

	lpExecString = (LPVOID)VirtualAllocEx(hProcess, NULL, strlen(strHookDLL), MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE); 

	if(WriteProcessBytes(hProcess, (LPVOID)lpExecString, strHookDLL, strlen(strHookDLL)) == FALSE)
	{
		MessageBox(GetActiveWindow(), "Error: Could not WriteProcessBytes()", "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
		CloseHandle(hProcess);

		return FALSE;
	}
	
	HANDLE hRemoteThread = CreateRemoteThread(hProcess, NULL, NULL, (LPTHREAD_START_ROUTINE)lpLoadLibraryAddr, (LPVOID)lpExecString, NULL, NULL);

	if(hRemoteThread == INVALID_HANDLE_VALUE)
	{
		MessageBox(GetActiveWindow(), "Error: Could not CreateRemoteThread()", "[:: hAx DLL Injector ::]", MB_OK + MB_ICONERROR);
		CloseHandle(hRemoteThread);
		CloseHandle(hProcess);
		return FALSE;
	}

	CloseHandle(hProcess);

	return TRUE;
}

//**Injector.cpp: GetProcessID function
DWORD GetProcessID(char* strProcessName)
{
	HANDLE hProcessSnap;
	PROCESSENTRY32 pe32;

	hProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

	if(hProcessSnap == INVALID_HANDLE_VALUE)
	{
		return FALSE;
	}
	else
	{	
		pe32.dwSize = sizeof(PROCESSENTRY32);

		if(Process32First(hProcessSnap, &pe32) == NULL)
		{
			CloseHandle(hProcessSnap);
			return FALSE;
		}
		else
		{
			do
			{
				if(stricmp(pe32.szExeFile, strProcessName) == 0)
				{
					CloseHandle(hProcessSnap);
					return pe32.th32ProcessID;	//** Return Process ID	
				}

			} while(Process32Next(hProcessSnap, &pe32));
		}
	}

	CloseHandle(hProcessSnap);

	return FALSE; //** If we make it here, error process not found
}

BOOL WriteProcessBytes(HANDLE hProcess, LPVOID lpBaseAddress, LPCVOID lpBuffer, SIZE_T nSize)
{
	DWORD dwOldProtect; 
	BOOL boolReturn = FALSE; 

	if(hProcess == NULL) 
	{ 
		VirtualProtect(lpBaseAddress, nSize, PAGE_EXECUTE_READWRITE, &dwOldProtect); 
		boolReturn = ((memcpy(lpBaseAddress, lpBuffer, nSize))? 1 : 0); 
		VirtualProtect(lpBaseAddress, nSize, dwOldProtect, &dwOldProtect); 
	} 
	else 
	{ 
		VirtualProtectEx(hProcess, lpBaseAddress, nSize, PAGE_EXECUTE_READWRITE, &dwOldProtect); 
		boolReturn = WriteProcessMemory(hProcess, lpBaseAddress, (LPVOID)lpBuffer, nSize, 0); 
		VirtualProtectEx(hProcess, lpBaseAddress, nSize, dwOldProtect, &dwOldProtect); 
	} 

	VirtualFreeEx(hProcess, lpBaseAddress, nSize, MEM_RELEASE);

	return boolReturn;
}
