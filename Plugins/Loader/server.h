//---------------------------------------------------------------------------

#ifndef serverH
#define serverH

#pragma link "ws2_32.lib"

#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <winsock2.h>
//#include <vector>
#include "..\debugUtils.h"
#include "..\tiposBase.h"

#define WAIT_TIMEOUT_INTERVAL 100
#define THREADS_POR_PROCESSADOR 2
#define MAX_BUFFER_LEN 256

typedef struct ContextoCliente {
	SOCKET      socket;  //accepted socket
	HANDLE 		thread;
	bool 		finalizado;
	SUBPLUGIN   *subplugin;
	DWORD		threadID;
} ContextoCliente;

typedef struct CLIENTE {
	LIST_ENTRY header;
	ContextoCliente *cliente;
} CLIENTE;

typedef void (__stdcall *MetodoProcessarServer)( ContextoCliente *contexto, PACOTE *pacote );

class Servidor {
	private:
		WSADATA wsaData;
		struct sockaddr_in ServerAddress, ClientAddress;
		int nPortNo;
		int nClientLength;
		HANDLE completionPort;
		CRITICAL_SECTION flagConsole;
		CRITICAL_SECTION flagListaClientes;
		CLIENTE *clientes;
		int numeroThreads;
		HANDLE threadConexao;

	public:
		Servidor(PROC callback){
			ListenSocket = NULL;
			nPortNo = 1313;
			nClientLength = 0;
			closeFlag = NULL;
			clienteFlag = NULL;
			clientes = NULL;
			completionPort = NULL;
			numeroThreads = 0;
			threadConexao = 0;
			clienteSock = 0;
			processar = (MetodoProcessarServer)callback;
		}
		~Servidor(){
        	terminar();
		}

		SOCKET ListenSocket;
		SOCKET clienteSock;
		HANDLE closeFlag;
		HANDLE clienteFlag;
		MetodoProcessarServer processar;
		PROC onClientDisconnect;
		PROC onClientConnect;

        int getPorta(){
			return nPortNo;
		}

        void __fastcall inicializar();
		void __fastcall desinicializar();
		bool __fastcall conectar(int porta);
		void logar(char *szFormat, ...);
		bool __fastcall removerCliente(SOCKET sock);
		void __fastcall adicionarCliente(ContextoCliente *cliente);
		int __fastcall getNoProcessadores();
		void __fastcall limparListaClientes();
		void __fastcall aceitarCliente();
		bool __fastcall inicializarIOCP();
		bool __fastcall associarIOCP(ContextoCliente *contexto);
		void __fastcall limpar();
		void __fastcall terminar();
		bool __fastcall enviar(SOCKET sock, int tipo, char* buffer, int len);
		bool __fastcall ClienteConectar(int porta);
		bool __fastcall ClienteEnviar(int tipo, char* buffer, int len);
		void __fastcall ClienteDesconectar();
};

typedef struct PacoteTratamento {
	ContextoCliente *contexto;
	Servidor *servidor;
}PacoteTratamento;

DWORD WINAPI ThreadRecebedor(LPVOID lpParam);
DWORD WINAPI ThreadConnect( LPVOID lpParam );
bool enviar(SOCKET sock, int tipo, char* buffer, int len);

//---------------------------------------------------------------------------
#endif
