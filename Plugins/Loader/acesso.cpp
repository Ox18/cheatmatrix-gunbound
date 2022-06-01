// ---------------------------------------------------------------------------

#pragma hdrstop

#include <stdio.h>
#include <string.h>
#include <windows.h>
#include "acesso.h"
// #include "DLL.h"
// #include "injunit.h"
#include "..\funcoes.h"
// #include "..\sysUtils.h"
#include "hooks.h"
#include "variaveis.h"
#include "..\debugutils.h"
#include "processador.h"
#include <winbase.h>
// ---------------------------------------------------------------------------

#pragma package(smart_init)

#define MODO_DEBUG

//
// ----------------------------------
// Callback para Recv
// ----------------------------------
//
int WINAPI RecvHooked(SOCKET lpSock, char * lpBuffer, int lpLen, int lpFlags) {
	// ULTIMO: loc_1357303F
	int resultado = 0;
	/*
	if(!Functions[iRecv].lpOldAddress){
	Functions[iRecv].lpOldAddress = (PROC)recv;
	}

	typedef int (__stdcall *hRecv)(SOCKET,char*,int,int);
	hRecv originalRecv = (hRecv)Functions[iRecv].lpOldAddress;

	char * buffer;
	while(!(buffer = (char*)malloc(lpLen)))
	Sleep(1);

	__try{
	PDWORD socket = 0;
	if(instanceDLL && (socket = SalvaSock(lpSock)) != 0){
	if(socket){
	if(!socket[2]){
	do {
	resultado = originalRecv(lpSock,buffer,lpLen,lpFlags);
	if(resultado <= 0)
	break;
	CopiarMemoriaSocket(socket, buffer, resultado);
	} while( ProcessarBuffer(socket[0], socket[1], socket[2], 1) );
	SalvarPacote(socket, buffer, lpLen, resultado);
	}
	}
	} else {
	resultado = originalRecv(lpSock,lpBuffer,lpLen,lpFlags);
	}
	memcpy(lpBuffer, buffer, lpLen);
	}__finally{
	free(buffer);
	}
	 */ return resultado;

	/* __try{
	if(instanceDLL){
	// Se não houver um pipe alocado, aloca um
	PDWORD pipe = SalvaSock(sock);
	if(pipe){
	DWORD totalDisponivel;
	if(PeekNamedPipe( (PVOID)pipe[2], 0, 0, 0, &totalDisponivel, 0)){

	if(totalDisponivel == 0){
	// Recebe o pacote do socket
	resultado = originalRecv(sock,buffer,len,flags);

	// Grava no pipe, o buffer recebido
	if(resultado > 0)
	WriteFile( (PVOID)pipe[1], buffer, resultado, &totalDisponivel, 0);

	// Uma vez recebido o pacote, processa-o
	while( ProcessarBuffer((PDWORD*)pipe, 0, 0, 1, sock) ){

	int bytesRecebidos = 0xFFFFFFFF;
	// Continua a receber os dados e processar
	while( bytesRecebidos == 0xFFFFFFFF ){
	DWORD erro = WSAGetLastError();
	if(erro == 10038 || erro == 10035)
	break;
	#ifdef MODO_DEBUG
	debugar("erro ao receber - %X - %s", erro, nomeArquivo);
	#endif
	Sleep(10);
	bytesRecebidos = originalRecv(sock, buffer, len, flags);
	}

	// Grava no pipe o buffer recebido
	if(bytesRecebidos > 0)
	WriteFile( (PVOID)pipe[1], buffer, resultado, &totalDisponivel, 0);
	else
	break;



	}
	}

	// Coloca no buffer os dados processados no pipe
	if(resultado > 0)
	ReadFile((PDWORD)pipe[2], buffer, len, (PDWORD)&resultado, 0);

	return resultado;
	}
	}
	}

	resultado = originalRecv(sock,buffer,len,flags);
	return resultado;
	}__finally{
	//debugar("[%X / %X] Recebido: %X - %X", originalRecv, recv, resultado, flags);
	} */
}

//
// ----------------------------------
// Callback para Send
// ----------------------------------
//
int WINAPI SendHooked(SOCKET sock, const char * buffer, int len, int flags) {
	int resultado = 1;
	/*
	if(!Functions[iSend].lpOldAddress){
	Functions[iSend].lpOldAddress = (PROC)send;
	}
	typedef int (__stdcall *hSend)(SOCKET,const char*,int,int);
	hSend originalSend = (hSend)Functions[iSend].lpOldAddress;
	if(instanceDLL)
	ProcessarBuffer(0, (PBYTE)buffer, len, 2, sock);
	resultado = originalSend(sock, buffer, len, flags);
	return resultado; */

}

BOOL WINAPI FreeLibraryHooked(HMODULE hModule){
    char moduleName[1024];
	memset(&moduleName[0],0,1024);
	GetModuleFileNameA((HMODULE)hModule,&moduleName[0],1024);
	//char* nomeDLL = ExtractFilePath(&moduleName[0]);
	debugar("FreeLibrary: %X - %s", hModule, &moduleName[0]);
	return FreeLibrary(hModule);
}


BOOL WINAPI UnmapViewOfFileHooked(LPCVOID lpBaseAddress){
	if(!Functions[iUnmapViewOfFile].lpOldAddress){
		Functions[iUnmapViewOfFile].lpOldAddress = (PROC)UnmapViewOfFile;
	}

	char moduleName[1024];
	memset(&moduleName[0],0,1024);
	GetModuleFileNameA((HMODULE)lpBaseAddress,&moduleName[0],1024);
	//char* nomeDLL = ExtractFilePath(&moduleName[0]);
    debugar("trying to unmap module %X - %s", lpBaseAddress, &moduleName[0]);

	typedef int (__stdcall *hUnmapViewOfFile)(LPCVOID);
	hUnmapViewOfFile originalUnmapViewOfFile = (hUnmapViewOfFile)Functions[iUnmapViewOfFile].lpOldAddress;
	BOOL resultado = originalUnmapViewOfFile(lpBaseAddress);


	return true;
}

//
// ----------------------------------
// Callback para RecvFrom
// ----------------------------------
//
int WINAPI RecvFromHooked(SOCKET sock, char * buffer, int len, int flags,
	struct sockaddr *from, int *fromlen) {

	int resultado = 0;
	/* __try{
	if(!Functions[iRecvFrom].lpOldAddress) {
	Functions[iRecvFrom].lpOldAddress = (PROC)recvfrom;
	}

	typedef int (__stdcall *hRecvFrom)(SOCKET,char*,int,int,struct sockaddr*,int*);
	hRecvFrom originalRecvFrom = (hRecvFrom)Functions[iRecvFrom].lpOldAddress;
	resultado = originalRecvFrom(sock,buffer,len,flags,from,fromlen);

	char* ip = inet_ntoa(*(PIN_ADDR)&from->sa_data[0]);
	if(stricmp(ip, "127.0.0.1")){
	resultado = 0xFFFFFFFF;
	}else{
	debugar("[RecvFrom] Recebido: %X",resultado);
	}
	}__except(1){
	debugar("erro em RecvFrom!");
	}
	 */
	return resultado;

}

//
// ----------------------------------
// Callback para SendTo
// ----------------------------------
//
int WINAPI SendToHooked(SOCKET sock, const char * buffer, int len, int flags,
	struct sockaddr *to, int tolen) {

	struct sockaddr_in addr;
	int bytes = 0, size = 0;
	char * IP;
	FILE * fp;

	/*
	bytes = sendto(sock,buffer,len,flags,to,tolen);

	if(bytes == -1)
	return -1;

	if(getpeername(sock,(struct sockaddr*)&addr,&size) == 0)
	{

	//if(addr.sin_addr.s_addr != inet_addr("127.0.0.1"))
	{
	fp = fopen("c:\\log.txt","a+");

	if(fp)
	{
	IP = inet_ntoa(addr.sin_addr);
	fputs("**********************************************************\n",fp);
	fprintf(fp,"->Dados enviados (SendTo)\n\n[+] Destino: %s\n",IP);
	fputs("**********************************************************\n\n",fp);
	fwrite(buffer,1,bytes,fp);
	fputs("\n\n",fp);
	fclose(fp);
	}
	}

	}

	 */
	return bytes;
}

//
// ----------------------------------
// Callback para LoadLibraryExA
// ----------------------------------
//
HMODULE WINAPI LoadLibraryExAHooked(PCSTR lpszModuleName, HANDLE hFile,
	DWORD dwFlags) {
	/*
	HMODULE hmod = LoadLibraryExA(lpszModuleName,hFile,dwFlags);
	HookFunctions(0);
	return (hmod);
	 */
}

//
// ----------------------------------
// Callback para LoadLibraryExW
// ----------------------------------
//
HMODULE WINAPI LoadLibraryExWHooked(PCWSTR lpszModuleName, HANDLE hFile,
	DWORD dwFlags) {
	/*
	HMODULE hmod = LoadLibraryExW(lpszModuleName,hFile,dwFlags);
	HookFunctions(0);
	return (hmod);
	 */
}

int WINAPI MessageBoxAHook(HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption,
	UINT uType) {
	/*
	//typedef int (__stdcall *hMessageBoxA)(HWND hWnd,LPCTSTR lpText,LPCTSTR lpCaption,UINT uType);
	//hMessageBoxA fMessageBoxA =  (hMessageBoxA)GetProcAddress(GetModuleHandleA("user32"),"MessageBoxA");
	debugar("Acesso a MessageBoxA");
	if(!Functions[iMessageBoxA].lpOldAddress) {
	Functions[iMessageBoxA].lpOldAddress = (PROC)MessageBoxA;
	}

	typedef int (__stdcall *hMessageBoxA)(HWND,LPCTSTR,LPCTSTR,UINT);
	hMessageBoxA originalMessageBoxA = (hMessageBoxA)Functions[iMessageBoxA].lpOldAddress;
	int resultado = originalMessageBoxA(hWnd, lpText, "HOOKADO!", uType);
	debugar("MessageBoxA");
	return resultado;
	 */
}

// -------------------------------------------------------------

int WINAPI WSARecvHooked(SOCKET s, LPWSABUF lpBuffers, DWORD dwBufferCount,
	LPDWORD lpNumberOfBytesRecvd, LPDWORD lpFlags,
	LPWSAOVERLAPPED lpOverlapped, LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine) {
	/*
	struct sockaddr_in addr;
	int bytes, size;
	char * IP;
	FILE * fp;

	bytes = WSARecv(s,lpBuffers,dwBufferCount,lpNumberOfBytesRecvd,lpFlags,lpOverlapped,lpCompletionRoutine);

	if(bytes == -1)
	return -1;


	if(getpeername(s,(struct sockaddr*)&addr,&size) == 0)
	{

	//if(addr.sin_addr.s_addr != inet_addr("127.0.0.1"))
	{
	fp = fopen("g:\\log.txt","a+");

	if(fp)
	{
	IP = inet_ntoa(addr.sin_addr);
	fputs("**********************************************************\n",fp);
	fprintf(fp,"-> Dados recebidos (WSARecv)\n\n[+] Origem: %s\n",IP);
	fputs("**********************************************************\n\n",fp);


	fwrite(lpBuffers,1,bytes,fp);
	fputs("\n\n",fp);
	fclose(fp);
	}
	}
	}

	return bytes;
	 */
	return 0;

}

const char *stristr(const char *haystack, const char *needle) {
	if (!*needle) {
		return haystack;
	}
	for (; *haystack; ++haystack) {
		if (toupper(*haystack) == toupper(*needle)) {
			const char *h, *n;
			for (h = haystack, n = needle; *h && *n; ++h, ++n) {
				if (toupper(*h) != toupper(*n)) {
					break;
				}
			}
			if (!*n) {
				return haystack; /* return the start of the match */
			}
		}
	}
	return 0;
}

DWORD ThreadIdFromPID(DWORD dwOwnerPID){
    HANDLE        hThreadSnap = NULL;
    BOOL          bRet        = FALSE;
    THREADENTRY32 te32        = {0};

    // Take a snapshot of all threads currently in the system.

    hThreadSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if (hThreadSnap == INVALID_HANDLE_VALUE)
        return (FALSE);

    // Fill in the size of the structure before using it.

    te32.dwSize = sizeof(THREADENTRY32);

    // Walk the thread snapshot to find all threads of the process.
    // If the thread belongs to the process, add its information
    // to the display list.

	if (Thread32First(hThreadSnap, &te32)){
		do{
			if (te32.th32OwnerProcessID == dwOwnerPID){
				CloseHandle (hThreadSnap);
				return te32.th32ThreadID;
			}
		}
        while (Thread32Next(hThreadSnap, &te32));
		bRet = TRUE;
	}
	else
		bRet = FALSE;          // could not walk the list of threads

	// Do not forget to clean up the snapshot object.
	CloseHandle (hThreadSnap);

    return (bRet);
}


//#define BUFFER_SIZE 255
int WINAPI rtcShellHooked(SHELL_STRUCT *cchMultiByte, LPSTR lpCommandLine){
	int resultado = 0;
    char    *pMBBuffer = (char *)malloc( 255 );
	wcstombs(pMBBuffer, (wchar_t *)cchMultiByte->nome, 255 );

    debugar("-----------------------------");
	debugar("rtcShell: %s", pMBBuffer);
	//debugar("teste rtc 0.1");

	if (Functions[iRtcShell].lpOldAddress) {
		 //debugar("teste rtc 1");
		 //LPPROCESS_INFORMATION lpProcessInformation;
		 //memset(lpProcessInformation, 0, sizeof *lpProcessInformation);
		 //debugar("teste rtc 1.2");
		 typedef int(__stdcall*hRtcShell)(SHELL_STRUCT *, LPSTR);
			hRtcShell oRtcShell = (hRtcShell)Functions[iRtcShell].lpOldAddress;
			resultado = oRtcShell(cchMultiByte, lpCommandLine);
			double pid;
			__asm{
				fst pid
			}

			int processId = (int)pid;
			debugar("Processo criado: %d",processId);
			int threadId = ThreadIdFromPID(processId);
			if(threadId){
				HANDLE hThread = OpenThread(THREAD_SUSPEND_RESUME, FALSE, threadId);
				if(hThread){
					__try{
						__try{
							HANDLE hProcess = OpenProcess(CREATE_THREAD_ACCESS, FALSE, processId);
							debugar("Processo aberto: %d", hProcess);
							if(hProcess){
								__try{
									//SuspendThread(lpProcessInformation->hThread);
									// Injeta a DLL
									debugar("Chamando inject...");
									bool injetado = InjetarDLL(hProcess, threadId, nomeDLL);
									debugar("Inject finalizado. Status: %d", injetado);
									//ResumeThread(lpProcessInformation->hThread);
								}__finally{
									CloseHandle(hProcess);
								}
							}
						}__except(1){
							debugar("Erro 9928");
						}
					}__finally{
						CloseHandle(hThread);
                    }
				}
			}

	}else{
		debugar("Ponteiro nao encontrado para Shell");
	}
	debugar("rtcShellHooked acionado em %s / %x %x", nomeArquivo, cchMultiByte, lpCommandLine);
	debugar("-----------------------------");
	return resultado;
}

char *ExtractFilePath(char *valor){
	for(char* i = (char*)(valor+strlen(valor)-1); i >= valor; i--){
		if(*i == '\\'){
			return ++i;
		}
	}
}

BOOL WINAPI CreateProcessAHooked(LPCTSTR lpApplicationName,
	LPTSTR lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes,
	LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles,
	DWORD dwCreationFlags, LPVOID lpEnvironment, LPCTSTR lpCurrentDirectory,
	LPSTARTUPINFO lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation) {
	// String nomeApp = lpApplicationName;
	bool criandoGB = !(lpApplicationName == NULL || stristr(lpApplicationName,"gunbound.gme") == NULL);
	bool resultado = false;
	int indice = iCreateProcessA;

	debugar("-----------------------------");
   /*	char *nomeProcesso = ExtractFilePath((char*)lpApplicationName);
	if(!stricmp(nomeProcesso, "LoadSysControl.dat")){
		debugar("is LoadSysControl");
		lpProcessInformation->dwThreadId = 1000;
		lpProcessInformation->dwProcessId = 1000;
		lpProcessInformation->hProcess = (void*)1000;
		lpProcessInformation->hThread = (void*)1000;
     	return true;
	}

	if(!stricmp(nomeProcesso, "ApexProgress.dat")){
		debugar("is ApexProgress");
		lpProcessInformation->dwThreadId = 1000;
		lpProcessInformation->dwProcessId = 1000;
		lpProcessInformation->hProcess = (void*)1000;
		lpProcessInformation->hThread = (void*)1000;
     	return true;
	}    */
	//debugar("executando CreateProcessA em %s", nomeArquivo);
//	debugar("CreateProcessA Application Name: %s", lpApplicationName);

	// debugar(strcat(nomeArquivo," - CreateProcessA acionado"));
	#ifdef MODO_DEBUG
//	debugar("CreateProcessA acionado em %s", nomeArquivo);
//	debugar("CreateProcessA Command Line: %s", lpCommandLine);
	#endif
	// debugar("\n");

	if (!Functions[iCreateProcessA].lpOldAddress) {
		Functions[iCreateProcessA].lpOldAddress = (PROC)CreateProcessA;
		// return false;
	}

	// debugar("CreateProcessA em %X / %X", Functions[iCreateProcessA].lpOldAddress, CreateProcessA);
	//if (criandoGB) {
//		if (isGunbound && !flagGunbound) {
//
//			#ifdef MODO_DEBUG
//			debugar("Iniciando hardcode em %s", nomeArquivo);
//			#endif
//			flagGunbound = !flagGunbound;
//
//			/* // Unhooka o Send
//			if(Functions[iSend].lpOldAddress){
//			HookExports(Functions[iSend].lpszModuleName, Functions[iSend].lpszFunctionName, Functions[iSend].lpOldAddress);
//			}
//
//			// Unhooka o Recv
//			if(Functions[iRecv].lpOldAddress){
//			HookExports(Functions[iRecv].lpszModuleName, Functions[iRecv].lpszFunctionName, Functions[iRecv].lpOldAddress);
//			}
//
//			// Unhooka o CreateProcessA
//			if(Functions[iCreateProcessA].lpOldAddress){
//			HookExports(Functions[iCreateProcessA].lpszModuleName, Functions[iCreateProcessA].lpszFunctionName, Functions[iCreateProcessA].lpOldAddress);
//			}
//
//			// Unhooka o CreateProcessW
//			if(Functions[iCreateProcessW].lpOldAddress){
//			HookExports(Functions[iCreateProcessW].lpszModuleName, Functions[iCreateProcessW].lpszFunctionName, Functions[iCreateProcessW].lpOldAddress);
//			}
//
//			HardCodeMemoria(); */
//
//		}
//		else
{
			#ifdef MODO_DEBUG
			// debugar("Não é o GB! continuando...");
			#endif
			bool suspendido = false;
			if (((dwCreationFlags >> 2) & 1) == 0) {
				dwCreationFlags |= CREATE_SUSPENDED;
				suspendido = true;
			}

			typedef int(__stdcall*hCreateProcessA)(LPCTSTR, LPTSTR,
				LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD,
				LPVOID, LPCTSTR, LPSTARTUPINFO, LPPROCESS_INFORMATION);
			hCreateProcessA oCreateProcessA = (hCreateProcessA)
			Functions[iCreateProcessA].lpOldAddress;

			// debugar("continuando criação do processo...");
			resultado = oCreateProcessA(lpApplicationName, lpCommandLine,
				lpProcessAttributes, lpThreadAttributes, bInheritHandles,
				dwCreationFlags, lpEnvironment, lpCurrentDirectory,
				lpStartupInfo, lpProcessInformation);
			// debugar("resultado CPA: %X", resultado);
			// Injeta a DLL
			// debugar("chamando inject...");
			InjetarDLL(lpProcessInformation->hProcess, lpProcessInformation->dwThreadId, nomeDLL);
			// debugar("inject finalizado");
			// InjectDLLBase(lpProcessInformation, nomeDLL);

			if (suspendido) {
				ResumeThread(lpProcessInformation->hThread);
			}

			return resultado;
		}
	//}
     debugar("-----------------------------");
	#ifdef MODO_DEBUG
	// debugar("gb processado...");
	#endif
	typedef int(__stdcall*hCreateProcessA)(LPCTSTR, LPTSTR,
		LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD, LPVOID,
		LPCTSTR, LPSTARTUPINFO, LPPROCESS_INFORMATION);
	hCreateProcessA oCreateProcessA = (hCreateProcessA)
	Functions[iCreateProcessA].lpOldAddress;
	return oCreateProcessA(lpApplicationName, lpCommandLine,
		lpProcessAttributes, lpThreadAttributes, bInheritHandles, dwCreationFlags,
		lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation);

}

BOOL WINAPI CreateProcessWHooked(LPCWSTR lpApplicationName,
	LPWSTR lpCommandLine, LPSECURITY_ATTRIBUTES lpProcessAttributes,
	LPSECURITY_ATTRIBUTES lpThreadAttributes, BOOL bInheritHandles,
	DWORD dwCreationFlags, LPVOID lpEnvironment, LPCWSTR lpCurrentDirectory,
	LPSTARTUPINFOW lpStartupInfo, LPPROCESS_INFORMATION lpProcessInformation) {
	bool resultado = false;
	//#ifdef MODO_DEBUG
//	debugar("-----------------------------");
//	debugar("executando CreateProcessW em %s", nomeArquivo);
	//#endif

	if (!Functions[iCreateProcessW].lpOldAddress) {
		Functions[iCreateProcessW].lpOldAddress = (PROC)CreateProcessW;
	}
	// debugar(strcat(nomeArquivo," - CreateProcessW acionado"));

	LPSTR nomeApp, comandLine;
	UnicodeToAnsi(lpApplicationName, &nomeApp);
	UnicodeToAnsi(lpCommandLine, &comandLine);
	bool criandoGB = !(lpApplicationName == NULL || stristr
		(nomeApp, "gunbound.gme") == NULL);
	/*
	if(lpApplicationName){
	char gbs[13];
	int poss = getPosicaoBarra(nomeApp);
	int len = strlen(nomeApp)-poss;
	strncpy(&gbs[0], &nomeApp[poss], len);
	if(stricmp(gbs,"gunbound.gme") == 0){
	isGunbound = false;
	UnHookFunctions();
	}
	}
	 */

	//debugar("[%d] Não é o gb certo. Desconectando...", GetCurrentProcessId());
	isGunbound = false;
	servidor->terminar();

	UnHookFunctions();

	//if (criandoGB) {
		#ifdef MODO_DEBUG
//		 debugar("CreateProcessW Application Name: %s", nomeApp);
//		 debugar("CreateProcessW Command Line: %s \n", comandLine);
		#endif

		bool suspendido = false;
		if (((dwCreationFlags >> 2) & 1) == 0) {
			dwCreationFlags |= CREATE_SUSPENDED;
			suspendido = true;
		}

		typedef int(__stdcall*hCreateProcessW)(LPCWSTR, LPCWSTR,
			LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD, LPVOID,
			LPCWSTR, LPSTARTUPINFOW, LPPROCESS_INFORMATION);
		hCreateProcessW oCreateProcessW = (hCreateProcessW)
		Functions[iCreateProcessW].lpOldAddress;
		resultado = oCreateProcessW(lpApplicationName, lpCommandLine,
			lpProcessAttributes, lpThreadAttributes, bInheritHandles,
			dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo,
			lpProcessInformation);

		// Injeta a DLL
		InjetarDLL(lpProcessInformation->hProcess, lpProcessInformation->dwThreadId, nomeDLL);

		if (suspendido)
			ResumeThread(lpProcessInformation->hThread);
   /*	}
	else {
		typedef int(__stdcall*hCreateProcessW)(LPCWSTR, LPCWSTR,
			LPSECURITY_ATTRIBUTES, LPSECURITY_ATTRIBUTES, BOOL, DWORD, LPVOID,
			LPCWSTR, LPSTARTUPINFOW, LPPROCESS_INFORMATION);
		hCreateProcessW oCreateProcessW = (hCreateProcessW)
			Functions[iCreateProcessW].lpOldAddress;
		resultado = oCreateProcessW(lpApplicationName, lpCommandLine,
			lpProcessAttributes, lpThreadAttributes, bInheritHandles,
			dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo,
			lpProcessInformation);
	}     */
	debugar("-----------------------------");
	return resultado;
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// GetIndicePeloPacote
