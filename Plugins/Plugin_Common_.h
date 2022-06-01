//---------------------------------------------------------------------------
#ifndef PluginCommonH

#include "PluginClasses.h"
#include "estruturaBot.h"

//******************************************************************************
//Mapping


extern HANDLE MapHandle;
extern HMODULE cmlib;

TBotInfos * __fastcall StartMapping();
TBotInfos * __fastcall StartMapping2();
//******************************************************************************

typedef BOOL (__stdcall *f_CMReadProcessMemory)(DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
typedef BOOL (__stdcall *f_CMWriteProcessMemory)(DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
typedef DWORD (__stdcall *f_CMOpenProcess)(DWORD pid, PVOID procBase);
typedef HDC (__stdcall *f_CMGetDC)(HWND hnd);
typedef HANDLE (__stdcall *f_CMOpenFileMappingA)(DWORD dwDesiredAccess, BOOL bInheritHandle, char * lpName);
typedef HANDLE (__stdcall *f_CMMapViewOfFile)( HANDLE hFileMappingObject, DWORD dwDesiredAccess, DWORD dwFileOffsetHigh, DWORD dwFileOffsetLow, DWORD dwNumberOfBytesToMap);
typedef BOOL (__stdcall *f_CMVirtualProtect)(HANDLE hProcess, LPVOID lpAddress, DWORD dwSize, DWORD flNewProtect, PDWORD lpflOldProtect);

extern f_CMReadProcessMemory CMReadProcessMemory;
extern f_CMWriteProcessMemory CMWriteProcessMemory;
extern f_CMOpenProcess CMOpenProcess;
extern f_CMGetDC CMGetDC;
extern f_CMOpenFileMappingA CMOpenFileMappingA;
extern f_CMMapViewOfFile CMMapViewOfFile;
extern TMatrizData *MatrizInfo;
extern f_CMVirtualProtect CMVirtualProtect;
extern f_CMGetDC CMGetWindowDC;
/*
extern BOOL *CMReadProcessMemory (DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
extern BOOL *CMWriteProcessMemory (DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
extern BOOL *CMOpenProcess (DWORD pid, PVOID procBase);
extern BOOL *CMGetDC (HWND hnd);  */


extern HANDLE hMemFile;
//extern TMatrizData *PMatrizData;


//void StartMapping();

#define PluginCommonH
//---------------------------------------------------------------------------
#endif


  
