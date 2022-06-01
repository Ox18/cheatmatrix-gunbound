//---------------------------------------------------------------------------

#pragma hdrstop

#include "Unit6.h"
#include "messages.hpp"
#include <tlhelp32.h>

#include <stdio.h>
#include <string.h>

//#include   <shellapi.h>
//  #include   <psapi.hpp>   


//f_CMReadProcessMemory CMReadProcessMemory = 0;
//f_CMWriteProcessMemory CMWriteProcessMemory = 0;
f_CMOpenProcess CMOpenProcess = 0;
f_CMGetDC CMGetDC = 0;
f_CMGetDC CMGetWindowDC = 0;

typedef BOOL (WINAPI * PFNENUMPROCESSES)(
DWORD * lpidProcess,
DWORD cb,
DWORD * cbNeeded
);

typedef BOOL (WINAPI * PFNENUMPROCESSMODULES)(
HANDLE hProcess,
HMODULE *lphModule,
DWORD cb,
LPDWORD lpcbNeeded
);

typedef DWORD (WINAPI * PFNGETMODULEFILENAMEEXA)(
HANDLE hProcess,
HMODULE hModule,
LPSTR lpFilename,
DWORD nSize
);

typedef DWORD (WINAPI * PFGetModuleBaseName)(
  HANDLE hProcess,
  HMODULE hModule,
  LPTSTR lpBaseName,
  DWORD nSize
);





//unsigned int i;
static HMODULE hModPSAPI = 0;
static PFNENUMPROCESSES EnumProcesses = 0;
static PFNENUMPROCESSMODULES EnumProcessModules = 0;
static PFNGETMODULEFILENAMEEXA GetModuleFileNameExA = 0;
static PFGetModuleBaseName GetModuleBaseName = 0;

unsigned long GetTargetProcessIdFromProcname(char *procName)
{
   PROCESSENTRY32 pe;
   HANDLE thSnapshot;
	 BOOL retval, ProcFound = false;

	 DWORD pid = 0;
	 int total = 0;

   thSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

   if(thSnapshot == INVALID_HANDLE_VALUE)
   {
      //MessageBox(NULL, "Error: unable to create toolhelp snapshot", "Loader", NULL);
      return 0;
   }

   pe.dwSize = sizeof(PROCESSENTRY32);

    retval = Process32First(thSnapshot, &pe);

   while(retval)
	 {
   
			if( strcmp((char *)pe.szExeFile, (char *)procName) == 0 )
      {
				 ProcFound = true;
				 total++;
				 pid = pe.th32ProcessID;
			}

			retval    = Process32Next(thSnapshot,&pe);
			pe.dwSize = sizeof(PROCESSENTRY32);
	 }

	 CloseHandle(thSnapshot);

	 if(ProcFound)
		 return pid;
	 else
	   return 0;

}

char * GetProcessN(HANDLE valor)
{
				if ( !hModPSAPI )
						hModPSAPI = LoadLibrary( "PSAPI.DLL" );

				if ( !hModPSAPI )
				    return FALSE ;

				EnumProcesses = (PFNENUMPROCESSES)
				GetProcAddress( hModPSAPI,"EnumProcesses" );

				EnumProcessModules = (PFNENUMPROCESSMODULES)
				GetProcAddress( hModPSAPI, "EnumProcessModules" );

				GetModuleFileNameExA = (PFNGETMODULEFILENAMEEXA)
				GetProcAddress( hModPSAPI, "GetModuleFileNameExA" );

				GetModuleBaseName = (PFGetModuleBaseName)
				GetProcAddress( hModPSAPI, "GetModuleBaseNameA" );

				if ( !EnumProcesses
				|| !EnumProcessModules
				|| !GetModuleFileNameExA || !GetModuleBaseName)
				return "erro1";




				char   title[110];
        char   classname[95];  
        char   totalstr[256];  
        HANDLE   hProcess;  
        unsigned   long   *pPid;  
        unsigned   long   result;  
        void   *hg;  
				unsigned   long   id;

              int len;
							hProcess   =   valor;
							char *buf = (char *)malloc(MAX_PATH+1);
							char *buf1 = (char *)malloc(MAX_PATH+1);
              //char   buf1[MAX_PATH+1]="unknown";   
							if(   NULL   !=   hProcess   )
              {  
                    HMODULE   hMod;  
                    DWORD   cbNeeded;  
                    if( EnumProcessModules(   hProcess,   &hMod,   sizeof( hMod ), &cbNeeded   )   )  
                    {  
													len=GetModuleFileNameExA( hProcess,  hMod, &buf[0], MAX_PATH+1 );
													//buf[len]="";
													len=GetModuleBaseName( hProcess, hMod, &buf1[0], MAX_PATH+1 );
													//buf1[len]="";
													return buf1;
										}
                    //All   Code   here  
							}

				FreeLibrary(hModPSAPI);

				return   "erro2";
}

HANDLE GetHostWindow()
{
   return FindWindowA(NULL, "lalalalala");
}

int GetSize(char *valor)
{
		for(int i = 0; i < 256; i++)
		{
			 if(valor[i] == 0)
			   return i+1;
		}
		return 255;
}

void SendMSg(char *valor)
{
	 COPYDATASTRUCT cds;   // declare a variable with type copy-data-struct" (windows API)
	// Fill the members of the COPYDATASTRUCT.
	// Input: pCmd = pointer to a T_YHF_COMMAND struct
	int j = GetSize(valor);
	char *nome = (char *)malloc(j+1);
	memcpy(nome, valor, j+1);

	// First copy the 4 characters for module + command into dwData
	cds.dwData = 0;  // combine 4 characters in a single WORD
	cds.cbData = (j+1); // count of bytes in data block
	cds.lpData = nome;  // pointer to data block
	 // now SEND the message ("SEND", not "POST" !)
	SendMessage(	(HWND)GetHostWindow(), WM_COPYDATA, 0, (LPARAM)&cds );

	free(nome);

}
//---------------------------------------------------------------------------
#pragma package(smart_init)
