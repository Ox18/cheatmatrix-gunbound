//---------------------------------------------------------------------------


#pragma hdrstop

#include "server.h"
#include "unitServidor.h"
#include "list.h"

SOCKET ListenSocket = NULL;
struct sockaddr_in ServerAddress, ClientAddress;
int nPortNo = 1313;
int nClientLength = 0;
HANDLE closeFlag = NULL;
HANDLE clienteFlag = NULL;
CRITICAL_SECTION flagConsole;
CRITICAL_SECTION flagListaClientes;
CLIENTE *clientes = NULL;
HANDLE completionPort = NULL;
int numeroThreads = 0;
HANDLE *threadsProcessamento;
HANDLE threadConexao = 0;

void inicializar(){
	clientes = (CLIENTE*)malloc(sizeof *clientes);
	clientes->header.Flink = (LIST_ENTRY*)clientes;
	clientes->header.Blink = (LIST_ENTRY*)clientes;

	closeFlag = CreateEvent(NULL, TRUE, FALSE, NULL);
	InitializeCriticalSection(&flagConsole);
	InitializeCriticalSection(&flagListaClientes);

	WSADATA wsaData;
	int nResult = WSAStartup(MAKEWORD(2,2), &wsaData);

	if (nResult != NO_ERROR){
    	// Erro ao executar WSAStartup()
		logar("Erro 1241");
		return;
	}
}


void adicionarCliente(ContextoCliente *cliente){
	if(cliente == NULL || cliente->socket == 0)
		return;

	__try{
		EnterCriticalSection(&flagListaClientes);

		__try{
			CLIENTE *novoCliente = (CLIENTE*)malloc(sizeof *clientes);
			novoCliente->header.Blink = (LIST_ENTRY*)clientes->header.Blink;
			novoCliente->header.Flink = (LIST_ENTRY*)clientes;
			if(clientes->header.Flink == (LIST_ENTRY*)clientes)
			   clientes->header.Flink = (LIST_ENTRY*)novoCliente;
			clientes->header.Blink = (LIST_ENTRY*)novoCliente;
			novoCliente->cliente = cliente;
		}__finally{
			LeaveCriticalSection(&flagListaClientes);
		}
	}__except(1){
	}
}

bool removerCliente(SOCKET sock){
	if(sock == 0)
		return false;

	__try{
		EnterCriticalSection(&flagListaClientes);

		__try{
			CLIENTE *item = clientes;
			do{

				ContextoCliente *contexto = item->cliente;
				if(contexto != NULL && contexto->socket == sock){
					 item->header.Blink->Flink = item->header.Flink;
					 item->header.Flink->Blink = item->header.Blink;
					 if(item->cliente != NULL)
						free(item->cliente);
					 free(item);
					 return true;
				}

				item = (CLIENTE*)item->header.Flink;
			} while((CLIENTE*)item != clientes);
		}__finally{
			LeaveCriticalSection(&flagListaClientes);
		}

		return false;
	}__except(1){
    	return false;
    }
}

void limparListaClientes(){
	__try{
		EnterCriticalSection(&flagListaClientes);
		__try{
			CLIENTE *item = clientes;
			do{

				ContextoCliente *contexto = item->cliente;
				CLIENTE *novoitem = (CLIENTE*)item->header.Flink;

				if(contexto != NULL && contexto->socket != 0) {
					 item->header.Blink->Flink = item->header.Flink;
					 item->header.Flink->Blink = item->header.Blink;
					 free(item->cliente);
					 free(item);
				}

				item = novoitem;
			} while((CLIENTE*)item != clientes);
		}__finally{
			LeaveCriticalSection(&flagListaClientes);
		}
	}__except(1){
    }
}

void limpar(){
	__try{
		SetEvent(closeFlag);
		WaitForSingleObject(threadConexao, INFINITE);

		CLIENTE *lista = clientes;
		while( (DWORD)(lista->header.Flink) != (DWORD)clientes  ){
			if( (DWORD)lista != (DWORD)clientes ){
				WaitForSingleObject(lista->cliente->thread, INFINITE);
			}
			lista = (CLIENTE *)lista->header.Flink;
		}

		limparListaClientes();
	}__except(1){
	}
}

void desinicializar(){
	__try{
		DeleteCriticalSection(&flagConsole);
		DeleteCriticalSection(&flagListaClientes);
		CloseHandle(closeFlag);
		WSACleanup();
    }__except(1){
	}
}

void terminar(){
    __try{
		closesocket(ListenSocket);
		WSACleanup();
		SetEvent(closeFlag);
		limpar();
		//Close open sockets
		//closesocket(ListenSocket);
		desinicializar();
	}__except(1){
	}
}

void conectar(int porta){

	inicializar();

	//Create a socket
	ListenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

	if (ListenSocket == INVALID_SOCKET) {
		logar("Error occurred while opening socket: %d", WSAGetLastError());
		WSACleanup();
		return;
	}

	//Cleanup and Init with 0 the ServerAddress
	ZeroMemory((char *)&ServerAddress, sizeof(ServerAddress));

	//Port number will be supplied as a commandline argument
	nPortNo = porta;

	//Fill up the address structure
	ServerAddress.sin_family = AF_INET;
	ServerAddress.sin_addr.s_addr = INADDR_ANY; //WinSock will supply address
	ServerAddress.sin_port = htons(nPortNo);    //comes from commandline

	//Assign local address and port number
	if (bind(ListenSocket, (struct sockaddr *) &ServerAddress, sizeof(ServerAddress)) == SOCKET_ERROR) {
		closesocket(ListenSocket);
		logar("Error occurred while binding.");
		WSACleanup();
		return;
	}

	//Make the socket a listening socket
	if (SOCKET_ERROR == listen(ListenSocket,SOMAXCONN)) {
		closesocket(ListenSocket);
		logar("Error occurred while listening.");
		WSACleanup();
		return;
	}

	/*clienteFlag = WSACreateEvent();

	if (clienteFlag == WSA_INVALID_EVENT) {
		logar("Error occurred while WSACreateEvent().");
		closesocket(ListenSocket);
		desinicializar();
	}

	if (WSAEventSelect(ListenSocket, clienteFlag, FD_ACCEPT) == SOCKET_ERROR) {
		logar("Error occurred while WSAEventSelect().");
		WSACloseEvent(clienteFlag);
		closesocket(ListenSocket);
		desinicializar();
	}*/

	//
	//	Cria thread de conexão
	DWORD threadID;
	threadConexao = CreateThread(0, 0, ThreadConnect, (void *)ListenSocket, 0, &threadID);

	logar("Servidor conectado!");
}

void logar(char *szFormat, ...) {
	EnterCriticalSection(&flagConsole);

	va_list args;
	va_start(args, szFormat);

	char buffer[256];
	vsprintf(buffer, szFormat, args );
	Form15->Memo1->Lines->Add(buffer);

	va_end(args);

	LeaveCriticalSection(&flagConsole);
}

//---------------------------------------------------------------------------

//
//	Aceita a conexão de um cliente e o adiciona na lista
//
void aceitarCliente(SOCKET listenSocket){
	sockaddr_in clienteAddress;
	int clienteLength = sizeof(clienteAddress);

	//Accept remote connection attempt from the client
	SOCKET cliente = accept(listenSocket, (sockaddr*)&clienteAddress, &clienteLength);

	if (cliente == INVALID_SOCKET) {
		//logar("\nError occurred while accepting socket: %ld.", WSAGetLastError());
    	return;
	}

	DWORD threadID;
	ContextoCliente *contexto = (ContextoCliente*)malloc(sizeof *contexto);
	contexto->socket = cliente;
	contexto->thread = CreateThread(0, 0, ThreadProcessamento, (void *)contexto, 0, &threadID);
	contexto->threadID = threadID;
	adicionarCliente(contexto);
	logar("Cliente conectado em: %d", (int)inet_ntoa(ClientAddress.sin_addr));
}

//
//	Thread gerenciadora de conexões
//
DWORD WINAPI ThreadConnect( LPVOID lpParam ) {
	SOCKET listenSocket = (SOCKET)lpParam;   //(SOCKET)ListenSocket;
	while(WaitForSingleObject(closeFlag, 0) != WAIT_OBJECT_0) {
		aceitarCliente(listenSocket);
	}
	return 1;
}

void processar(ContextoCliente *contexto, PACOTE *pacote){
	if(pacote != NULL){
		switch(pacote->tipo){
			case 0: {
				TesteStruct *teste = (TesteStruct *)&pacote->buffer;
				logar("Cliente %d: %s", contexto->threadID, teste->valor4);
			} break;
			default: break;
		}
	}
}

void enviarTeste(SOCKET sock){
  static int contador = 0;
  TesteStruct teste;
  teste.valor1 = contador++;
  teste.valor2 = contador;
  teste.valor3 = contador;
  enviar(sock, 0, (char*)&teste, sizeof teste);
}

//
//	Thread gerenciadora de pacotes
//
DWORD WINAPI ThreadProcessamento( LPVOID lpParam ) {
	int quantidadeBytes = 0;
	ContextoCliente *contexto = (ContextoCliente*)lpParam;

	if(contexto == NULL || contexto->socket == 0)
		return 0;

	while (WaitForSingleObject(closeFlag, 0)!= WAIT_OBJECT_0){
		int len = 0;
		PACOTE *pacote = NULL;
		quantidadeBytes = recv(contexto->socket, (char*)&len, 2, 0 );
		if(len > 0){
			pacote = (PACOTE*)malloc(len + 5);
			memset(pacote, 0, len + 5);
			pacote->len = len;
			__try {
				 quantidadeBytes = recv(contexto->socket, (char*)&pacote->tipo, len+2, 0 );
				 if (quantidadeBytes <= 0) {
					  closesocket(contexto->socket);
					  removerCliente(contexto->socket);
					  break;
				 }
				 processar(contexto, pacote);

				 enviarTeste(contexto->socket);
			}__finally{
				 free(pacote);
			}
		}
	}
	return 1;
}

bool enviar(SOCKET sock, int tipo, char* buffer, int len){
	if(sock > 0){
		int enviados;
		PACOTE *pacote = (PACOTE*)malloc(4+len);
		pacote->len = (WORD)len;
		pacote->tipo = (WORD)tipo;
		memcpy(&pacote->buffer, buffer, len);
		enviados = send(sock, (char*)pacote, len+4, 0);
		if (enviados == SOCKET_ERROR) {
			//closesocket(clienteSock);
			logar("Erro 7826");
			return false;
		}
		return true;
    }else{
		return false;
    }
}

//---------------------------------------------------------------------------

#pragma package(smart_init)
