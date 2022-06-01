//---------------------------------------------------------------------------

#ifndef clientH
#define clientH
//---------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <winsock2.h>

typedef struct PACOTE {
   WORD len;
   WORD tipo;
   BYTE *buffer;
} PACOTE;

typedef struct TesteStruct {
	DWORD valor1;
	int valor2;
	WORD valor3;
	char valor4[50];
} TesteStruct;

extern SOCKET clienteSock;
extern HANDLE closeFlag;
extern CRITICAL_SECTION flagConsole;
extern CRITICAL_SECTION flagListaClientes;
extern HANDLE threadProcessamento;
extern bool conectado;

bool enviar(int tipo, char* buffer, int len);
void desinicializar();
void inicializar();
void conectar(int porta);
void logar(char *szFormat, ...);
void terminar();
void limpar();
void desconectar();
DWORD WINAPI ThreadProcessamento( LPVOID lpParam );

#endif
