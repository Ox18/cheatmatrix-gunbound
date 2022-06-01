//---------------------------------------------------------------------------

#ifndef serverH
#define serverH

#pragma link "ws2_32.lib"

#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <winsock2.h>
#include <vector>

#define WAIT_TIMEOUT_INTERVAL 100
#define THREADS_POR_PROCESSADOR 2
#define MAX_BUFFER_LEN 256

typedef struct TesteStruct {
	DWORD valor1;
	int valor2;
	WORD valor3;
	char valor4[50];
} TesteStruct;

typedef struct ContextoCliente {
	SOCKET      socket;  //accepted socket
	HANDLE 		thread;
	bool 		finalizado;
	DWORD 		pluginID;
	HANDLE 		subPlugin;
	DWORD		threadID;
} ContextoCliente;

typedef struct CLIENTE {
	LIST_ENTRY header;
	ContextoCliente *cliente;
} CLIENTE;

typedef struct PACOTE {
   WORD len;
   WORD tipo;
   BYTE *buffer;
} PACOTE;

extern WSADATA wsaData;
extern SOCKET ListenSocket;
extern struct sockaddr_in ServerAddress, ClientAddress;
extern int nPortNo;
extern int nClientLength;
extern HANDLE closeFlag;
extern HANDLE clienteFlag;
extern HANDLE completionPort;
extern CRITICAL_SECTION flagConsole;
extern CRITICAL_SECTION flagListaClientes;
extern CLIENTE *clientes;
extern int numeroThreads;
extern HANDLE threadConexao;

void inicializar();
void desinicializar();
void conectar(int porta);
void logar(char *szFormat, ...);
void destruir();
bool removerCliente(SOCKET sock);
void adicionarCliente(ContextoCliente *cliente);
int getNoProcessadores();
void limparListaClientes();
void aceitarCliente(SOCKET listenSocket);
bool inicializarIOCP();
bool associarIOCP(ContextoCliente *contexto);
void limpar();
void terminar();

DWORD WINAPI ThreadProcessamento( LPVOID lpParam );
DWORD WINAPI ThreadConnect( LPVOID lpParam );
bool enviar(SOCKET sock, int tipo, char* buffer, int len);

//---------------------------------------------------------------------------
#endif
