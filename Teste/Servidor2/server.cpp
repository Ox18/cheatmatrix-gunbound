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
	numeroThreads = THREADS_POR_PROCESSADOR * getNoProcessadores();
	threadsProcessamento = new HANDLE[numeroThreads];

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

	if (!inicializarIOCP()) {
		// Erro ao inicializar o IOCP
		logar("Erro 9281");
	}
}

bool inicializarIOCP(){
	completionPort = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 0 );
	if ( completionPort == NULL) {
    	// Erro ao criar o IOCP
		logar("Erro 9213: %d.", WSAGetLastError());
		return false;
	}

	//
	// Cria threads de processamento
	DWORD nThreadID;
	for (int i = 0; i < numeroThreads; i++) {
		threadsProcessamento[i] = CreateThread(0, 0, ThreadProcessamento, (void *)(i+1), 0, &nThreadID);
	}
	return true;
}

void adicionarCliente(ContextoCliente *cliente){
	if(cliente == NULL || cliente->GetSocket() == 0)
		return;

	EnterCriticalSection(&flagListaClientes);

	__try{
		CLIENTE *novoCliente = (CLIENTE*)malloc(sizeof *clientes);
		novoCliente->header.Blink = (LIST_ENTRY*)clientes->header.Blink;
		novoCliente->header.Flink = (LIST_ENTRY*)clientes;
		clientes->header.Blink = (LIST_ENTRY*)novoCliente;
		novoCliente->cliente = cliente;
	}__finally{
		LeaveCriticalSection(&flagListaClientes);
	}

	LeaveCriticalSection(&flagListaClientes);
}

bool removerCliente(SOCKET sock){
	if(sock == 0)
		return false;

	EnterCriticalSection(&flagListaClientes);

	__try{
		CLIENTE *item = clientes;
		do{

			ContextoCliente *contexto = item->cliente;
			if(contexto != NULL && contexto->GetSocket() == sock){
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
}

void limparListaClientes(){
    EnterCriticalSection(&flagListaClientes);
	__try{
		CLIENTE *item = clientes;
		do{

			ContextoCliente *contexto = item->cliente;
			CLIENTE *novoitem = (CLIENTE*)item->header.Flink;

			if(contexto != NULL && contexto->GetSocket() != 0) {
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
}

bool associarIOCP(ContextoCliente *contexto) {
	HANDLE associacao = CreateIoCompletionPort((HANDLE)contexto->GetSocket(), completionPort, (DWORD)contexto, 0);
	if (associacao == NULL) {
		// Erro ao associar o cliente
		logar("Erro 1783");
		removerCliente(contexto->GetSocket());
		free(contexto);
		return false;
	}
	return true;
}

void limpar(){
	SetEvent(closeFlag);
	WaitForSingleObject(threadConexao, INFINITE);

	// Tira as threads do estado de blocking - GetQueuedCompletionStatus()
	for (int i = 0; i < numeroThreads; i++)
		PostQueuedCompletionStatus(completionPort, 0, (DWORD) NULL, NULL);

	//Aguarda até que as threads de processamento terminem
	WaitForMultipleObjects(numeroThreads, threadsProcessamento, TRUE, INFINITE);
	WSACloseEvent(clienteFlag);
    limparListaClientes();
}

void desinicializar(){
	DeleteCriticalSection(&flagConsole);
	DeleteCriticalSection(&flagListaClientes);
	CloseHandle(closeFlag);
	CloseHandle(completionPort);
	delete[] threadsProcessamento;
	WSACleanup();
}

void terminar(){
	limpar();
	//Close open sockets
	closesocket(ListenSocket);
	desinicializar();
}

int getNoProcessadores() {
	static int nProcessors = 0;
	if (nProcessors == 0) {
		SYSTEM_INFO si;
		GetSystemInfo(&si);
		nProcessors = si.dwNumberOfProcessors;
	}
	return nProcessors;
}

void conectar(int porta){

	inicializar();

	//Create a socket
	ListenSocket = WSASocket(AF_INET, SOCK_STREAM, 0, NULL, 0, WSA_FLAG_OVERLAPPED);//socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

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

	clienteFlag = WSACreateEvent();

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
	}

	nClientLength = sizeof(ClientAddress);

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
		logar("\nError occurred while accepting socket: %ld.", WSAGetLastError());
	}

	// Mostra o IP do cliente
	ContextoCliente *contexto = new ContextoCliente;//(ContextoCliente*)malloc(sizeof *contexto);
	contexto->SetSocket(cliente);
	adicionarCliente(contexto);
	logar("Cliente conectado em: %d", (int)inet_ntoa(ClientAddress.sin_addr));

	if(associarIOCP(contexto)){
		DWORD dwFlags = 0;
		DWORD dwBytes = 0;

		WSABUF *wsaBuffer = contexto->GetWSABUFPtr();
		OVERLAPPED *overlap = contexto->GetOVERLAPPEDPtr();

		//	Recv inicial para ativar a completionport
		int status = WSARecv(contexto->GetSocket(), wsaBuffer, 1, &dwBytes, &dwFlags, overlap, NULL);
		if ((status == SOCKET_ERROR) && (WSAGetLastError() != WSA_IO_PENDING)) {
			logar("Erro inicial na postagem");
		}
	}
}

//
//	Thread gerenciadora de conexões
//
DWORD WINAPI ThreadConnect( LPVOID lpParam ) {
	SOCKET listenSocket = (SOCKET)lpParam;   //(SOCKET)ListenSocket;
	WSANETWORKEVENTS WSAEvents;
	while(WaitForSingleObject(closeFlag, 0) != WAIT_OBJECT_0) {
    	if ( WSAWaitForMultipleEvents(1, &clienteFlag, FALSE, WAIT_TIMEOUT_INTERVAL, FALSE) != WSA_WAIT_TIMEOUT ) {
			WSAEnumNetworkEvents(listenSocket, clienteFlag, &WSAEvents);
			if ((WSAEvents.lNetworkEvents & FD_ACCEPT) && (WSAEvents.iErrorCode[FD_ACCEPT_BIT] == 0)) {
				 aceitarCliente(listenSocket);
			}
		}
	}
}

//
//	Thread gerenciadora de pacotes
//
DWORD WINAPI ThreadProcessamento( LPVOID lpParam ) {
	DWORD 			bytesTransferidos = 0;
	void 			*contexto = NULL;
	OVERLAPPED 		*overlap = NULL;
	ContextoCliente *cliente = NULL;
	DWORD           dwBytes = 0, dwFlags = 0;
	int 			bytesEnviados = 0;

	while (WaitForSingleObject(closeFlag, 0)!= WAIT_OBJECT_0){
		bool status = GetQueuedCompletionStatus(completionPort, &bytesTransferidos, (LPDWORD)&contexto, &overlap, INFINITE);
		if (contexto == NULL) {
			// Desligando...
			break;
		}

		cliente = (ContextoCliente *)contexto;
		if (!status || bytesTransferidos == 0) {
			// O cliente desconectou...
			removerCliente(cliente->GetSocket());
			continue;
		}

		WSABUF *wsaBuffer = cliente->GetWSABUFPtr();
		OVERLAPPED *overlap = cliente->GetOVERLAPPEDPtr();

		bytesEnviados = WSARecv(cliente->GetSocket(), wsaBuffer, 1, &dwBytes, (DWORD*)dwFlags, overlap, NULL);
		//logar("Qnt bytes: %d", cliente->wsaBuffer->len);

		if ((bytesEnviados == SOCKET_ERROR) && (WSAGetLastError() != WSA_IO_PENDING)) {
			removerCliente(cliente->GetSocket());
		}

		/*

		TesteStruct *teste = new TesteStruct();
		int sizeBuffer = (sizeof *teste);
		char *buffer = (char*)malloc(sizeBuffer + 1);
		 __try{
			 int totalBytes = 0;

			 //Cleanup and Init with 0 the szBuffer
			 memset(buffer, 0, sizeBuffer + 1);

			 int nBytesSent;
			 int nBytesRecv;

			 nBytesRecv = recv(RemoteSocket, buffer, sizeBuffer, 0 );

			 if (SOCKET_ERROR == nBytesRecv || nBytesRecv == 0) {
				  closesocket(RemoteSocket);
				  RemoteSocket = 0;
				  continue;
			 }
			 memcpy( &((char*)teste)[totalBytes], &buffer[0], nBytesRecv );

			 totalBytes += nBytesRecv;
			 while(totalBytes < sizeBuffer){
				 memset(buffer, 0, sizeBuffer + 1);
				 nBytesRecv = recv(RemoteSocket, buffer, 255, 0 );
				 if (SOCKET_ERROR == nBytesRecv || nBytesRecv == 0) {
					  closesocket(RemoteSocket);
					  RemoteSocket = 0;
					  continue;
				 }
				 memcpy( &((char*)teste)[totalBytes], &buffer[0], nBytesRecv );

				 totalBytes += nBytesRecv;
			 }

			 //Form15->SpinEdit1->Value = (int)teste->valor1;
			 //Form15->SpinEdit2->Value = (int)teste->valor2;
			 //Form15->SpinEdit3->Value = (int)teste->valor3;
			 //Form15->Edit1->Text = teste->valor4;
			 //logar("Valor recebido: %d", teste->valor1);
			 logar(teste->valor4);

		 }__finally{
			 free(buffer);
		 }
		 */
	}
}

//
//	Os dois primeiros Bytes são reservados
//	2º: Tipo de pacote
//	3º: Ação / Identificador
//
void formatarDado(WSABUF buffer){
   // buffer.len

}

//---------------------------------------------------------------------------

#pragma package(smart_init)
