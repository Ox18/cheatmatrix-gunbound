//---------------------------------------------------------------------------

#ifndef Unit6H
#define Unit6H

#include <windows.h>

typedef BOOL (__stdcall *f_CMReadProcessMemory)(DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
typedef BOOL (__stdcall *f_CMWriteProcessMemory)(DWORD lpProcessID, PVOID lpProcessBase, PVOID lpAddress, PVOID lpBuffer, DWORD lpsize);
typedef DWORD (__stdcall *f_CMOpenProcess)(DWORD pid, PVOID procBase);
typedef HDC (__stdcall *f_CMGetDC)(HWND hnd);
//typedef BOOL (__stdcall *f_CMVirtualProtect)(HANDLE hProcess, LPVOID lpAddress, DWORD dwSize, DWORD flNewProtect, PDWORD lpflOldProtect);

extern f_CMOpenProcess CMOpenProcess;
extern f_CMGetDC CMGetDC;
extern f_CMGetDC CMGetWindowDC;
//extern f_CMReadProcessMemory CMReadProcessMemory;
//extern f_CMWriteProcessMemory CMWriteProcessMemory;
//extern f_CMVirtualProtect CMVirtualProtect;

unsigned long GetTargetProcessIdFromProcname(char *procName);
char * GetProcessN(HANDLE valor);
void SendMSg(char *valor);
//---------------------------------------------------------------------------
#endif
