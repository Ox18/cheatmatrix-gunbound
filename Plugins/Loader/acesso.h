//---------------------------------------------------------------------------

#ifndef acessoH
#define acessoH
//---------------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#include <windows.h>

#define CREATE_THREAD_ACCESS (PROCESS_CREATE_THREAD | PROCESS_QUERY_INFORMATION | PROCESS_VM_OPERATION | PROCESS_VM_WRITE | PROCESS_VM_READ)


BOOL WINAPI FreeLibraryHooked(HMODULE hModule);
int WINAPI SendHooked(SOCKET, const char *, int, int);
int WINAPI SendToHooked(SOCKET sock, const char * buffer, int  len, int flags, struct sockaddr *to, int tolen);
int WINAPI RecvHooked(SOCKET, char *, int, int);
int WINAPI RecvFromHooked(SOCKET sock, char * buffer, int  len, int flags, struct sockaddr *from, int *fromlen);


int WINAPI MessageBoxAHook(HWND hWnd,LPCTSTR lpText,LPCTSTR lpCaption,UINT uType);

int WINAPI WSARecvHooked(
  SOCKET s,
  LPWSABUF lpBuffers,
  DWORD dwBufferCount,
  LPDWORD lpNumberOfBytesRecvd,
  LPDWORD lpFlags,
  LPWSAOVERLAPPED lpOverlapped,
  LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
);

BOOL WINAPI CreateProcessAHooked(
  LPCTSTR lpApplicationName,
  LPTSTR lpCommandLine,
  LPSECURITY_ATTRIBUTES lpProcessAttributes,
  LPSECURITY_ATTRIBUTES lpThreadAttributes,
  BOOL bInheritHandles,
  DWORD dwCreationFlags,
  LPVOID lpEnvironment,
  LPCTSTR lpCurrentDirectory,
  LPSTARTUPINFO lpStartupInfo,
  LPPROCESS_INFORMATION lpProcessInformation
);

BOOL WINAPI CreateProcessWHooked(
   LPCWSTR lpApplicationName,
   LPWSTR lpCommandLine,
   LPSECURITY_ATTRIBUTES lpProcessAttributes,
   LPSECURITY_ATTRIBUTES lpThreadAttributes,
   BOOL bInheritHandles,
   DWORD dwCreationFlags,
   LPVOID lpEnvironment,
   LPCWSTR lpCurrentDirectory,
   LPSTARTUPINFOW lpStartupInfo,
   LPPROCESS_INFORMATION lpProcessInformation
);

BOOL WINAPI UnmapViewOfFileHooked(LPCVOID lpBaseAddress);

typedef struct SHELL_STRUCT{
	int count;
	int param2;
	char *nome;
}SHELL_STRUCT;

int WINAPI rtcShellHooked(SHELL_STRUCT *cchMultiByte, LPSTR lpCommandLine);

#endif
