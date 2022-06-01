//---------------------------------------------------------------------------

#ifndef debugutilsH
#define debugutilsH

#include <stdio.h>
#include <string.h>
#include <windows.h>
//---------------------------------------------------------------------------
typedef struct _aviso{
	bool ativo;
	int len;
	char valor[150];
	DWORD inicio;
	DWORD tempo;
}TAviso;

extern TAviso avisos[10];
extern DWORD tempoAviso;
extern char nomeAviso[7][150];

typedef int (__stdcall *hOutputDebugStringA)(LPCTSTR);
extern hOutputDebugStringA fOutputDebugStringA;

void debugarString(char* valor);
void debugar(char* formato, ... );
void debugarArray(BYTE* buffer, int len);
void debugarInt(DWORD valor);
PCHAR bufferToArrayBytes(BYTE* buffer, int len);
void gerarAviso(char* formato, ... );
void getError(LPTSTR lpszFunction);
//void gerarAvisoTempo(DWORD tempo, char* formato, ... );


#endif
