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

class ContextoCliente {
private:
	WSABUF            *wsaBuffer;
	SOCKET             socket;  //accepted socket
	OVERLAPPED        *overlap;
	char               buffer[MAX_BUFFER_LEN];
	int               totalBytes;
	int               sentBytes;

public:

	ContextoCliente() {
		overlap = new OVERLAPPED;
		wsaBuffer = new WSABUF;

		ZeroMemory(overlap, sizeof(OVERLAPPED));

		socket =  SOCKET_ERROR;

		ZeroMemory(buffer, MAX_BUFFER_LEN);

		wsaBuffer->buf = buffer;
		wsaBuffer->len = MAX_BUFFER_LEN;

		totalBytes = 0;
		sentBytes = 0;
	}

	~ContextoCliente() {
		//Wait for the pending operations to complete
		while (!HasOverlappedIoCompleted(overlap)) {
			Sleep(0);
		}
		closesocket(socket);
		//Cleanup
		delete overlap;
		delete wsaBuffer;
	}

	void SetTotalBytes(int n) {
		totalBytes = n;
	}

	int GetTotalBytes() {
		return totalBytes;
	}

	void SetSentBytes(int n) {
		sentBytes = n;
	}

	void IncrSentBytes(int n) {
		sentBytes += n;
	}

	int GetSentBytes() {
		return sentBytes;
	}

	void SetSocket(SOCKET s) {
		socket = s;
	}

	SOCKET GetSocket() {
		return socket;
	}

	void SetBuffer(char *szBuffer) {
		strcpy(buffer, szBuffer);
	}

	void GetBuffer(char *szBuffer) {
		strcpy(szBuffer, buffer);
	}

	void ZeroBuffer() {
		ZeroMemory(buffer, MAX_BUFFER_LEN);
	}

	void SetWSABUFLength(int nLength) {
		wsaBuffer->len = nLength;
	}

	int GetWSABUFLength() {
		return wsaBuffer->len;
	}

	WSABUF* GetWSABUFPtr() {
		return wsaBuffer;
	}

	OVERLAPPED* GetOVERLAPPEDPtr() {
		return overlap;
	}

	void ResetWSABUF() {
		ZeroBuffer();
		wsaBuffer->buf = buffer;
		wsaBuffer->len = MAX_BUFFER_LEN;
	}
};

typedef struct CLIENTE {
	LIST_ENTRY header;
    ContextoCliente *cliente;
} CLIENTE;

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
extern HANDLE *threadsProcessamento;
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

typedef struct PACOTE {
	WORD tipo;
	BYTE *buffer;
} PACOTE;

//---------------------------------------------------------------------------
#endif
