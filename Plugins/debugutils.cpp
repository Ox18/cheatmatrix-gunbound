//---------------------------------------------------------------------------

#pragma hdrstop

#include "debugutils.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

hOutputDebugStringA fOutputDebugStringA;

//########################################################
// DEBUG UTILS
//########################################################

void getError(LPTSTR lpszFunction) {
	// Retrieve the system error message for the last-error code

	LPVOID lpMsgBuf;
	LPVOID lpDisplayBuf;
	DWORD dw = GetLastError();

	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) & lpMsgBuf, 0, NULL);

	// Display the error message and exit the process

	lpDisplayBuf = (LPVOID)LocalAlloc(LMEM_ZEROINIT, (lstrlen((LPCTSTR)lpMsgBuf) + lstrlen((LPCTSTR)lpszFunction) + 40)*sizeof(TCHAR));
	debugar("%s failed with error %d: %s", lpszFunction, dw, lpMsgBuf);

	LocalFree(lpMsgBuf);
	LocalFree(lpDisplayBuf);
	// ExitProcess(dw);
}


void debugarString(char* valor){
	__try{
		if(!fOutputDebugStringA){
			fOutputDebugStringA = (hOutputDebugStringA)OutputDebugStringA;
			if(!fOutputDebugStringA){
				fOutputDebugStringA = (hOutputDebugStringA)GetProcAddress(GetModuleHandleA("kernel32.dll"), "OutputDebugStringA");
				if(!fOutputDebugStringA)
					MessageBox(0, "Erro no output!", "Bad Boy", 0);
			}
		}

		if(!valor || strlen(valor) == 0){
			fOutputDebugStringA("-");
		}else{
            fOutputDebugStringA(valor);
        }
	}__except(1){
		return;
	}
}

void debugar(char* formato, ... ){
	if(!formato)
		debugarString("\n");

	va_list args;
	va_start( args, formato );
	int total = strlen(formato)+2048; //vprintf(formato, args);
	char *buffer = (char *)malloc(total+1);
	if(buffer){
		__try{
			vsprintf(buffer, formato, args);
			debugarString(buffer);
		}__finally{
			free(buffer);
		}
	}
	va_end(listPointer);
}

void gerarAviso(char* formato, ... ){
	va_list args;
	va_start( args, formato );

	// Debug --------------------
	if(!formato)
		debugarString("\n");

	int total = strlen(formato) + 2048; //vprintf(formato, args);
	char *buffer = (char *)malloc(total+1);
	if(buffer){
		__try{
			vsprintf(buffer, formato, args);
			debugarString(buffer);
		}__finally{
			free(buffer);
		}
	}

	//char *buffer = (char *)malloc(total+1);
	for(int i = 0; i < 10; i++){
		if(!avisos[i].ativo){
		   memset(&avisos[i].valor[0],0,150);
		   vsprintf(avisos[i].valor, formato, args);
		   avisos[i].len = strlen(avisos[i].valor);
		   avisos[i].ativo = true;
		   avisos[i].inicio = GetTickCount();
		   avisos[i].tempo = tempoAviso; //tempo;
		   break;
		}
	}
	va_end(listPointer);
}

void debugarArray(BYTE* buffer, int len){
	 PBYTE bytes = bufferToArrayBytes(buffer,len);
	 debugarString(bytes);
	 free(bytes);
}

void debugarInt(DWORD valor){
	PCHAR resultado = (PCHAR)malloc(7);
	itoa(valor, resultado, 16);
	debugarString(resultado);
	free(resultado);
}

PCHAR bufferToArrayBytes(BYTE* buffer, int len){
	const char* letras = "0123456789ABCDEF";
	PBYTE resultado = (PBYTE)malloc(len*2+1);
	resultado[len*2] = 0;
    for(int i = 0; i < len; i++){
		BYTE b = buffer[i];
		BYTE b1 = b & 0x0F;
		BYTE b2 = b >> 0x04;
		char c1 = letras[b1];
		char c2 = letras[b2];
		resultado[i*2] = c2;
		resultado[i*2+1] = c1;
	}
	return resultado;
}
