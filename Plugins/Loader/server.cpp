// ---------------------------------------------------------------------------

#pragma hdrstop

#include "server.h"
#include "list.h"

void __fastcall Servidor::adicionarCliente(ContextoCliente *cliente) {
	if (cliente == NULL || cliente->socket == 0)
		return;

	__try {
		EnterCriticalSection(&flagListaClientes);

		__try {
			CLIENTE *novoCliente = (CLIENTE*)malloc(sizeof*clientes);
			novoCliente->header.Blink = (LIST_ENTRY*)clientes->header.Blink;
			novoCliente->header.Flink = (LIST_ENTRY*)clientes;
			if (clientes->header.Flink == (LIST_ENTRY*)clientes)
				clientes->header.Flink = (LIST_ENTRY*)novoCliente;
			clientes->header.Blink = (LIST_ENTRY*)novoCliente;
			novoCliente->cliente = cliente;

			typedef void(__stdcall PASCAL * TClientConnect)(ContextoCliente *contexto);
			TClientConnect onConnect = (TClientConnect)onClientConnect;
			onConnect(cliente);
		}
		__finally {
			LeaveCriticalSection(&flagListaClientes);
		}
	}
	__except (1) {
	}
}

bool __fastcall Servidor::removerCliente(SOCKET sock) {
	if (sock == 0)
		return false;

	__try {
		EnterCriticalSection(&flagListaClientes);

		__try {
			CLIENTE *item = clientes;
			do {

				ContextoCliente *contexto = item->cliente;
				if (contexto != NULL && contexto->socket == sock) {
					item->header.Blink->Flink = item->header.Flink;
					item->header.Flink->Blink = item->header.Blink;
					if (item->cliente != NULL)
						free(item->cliente);
					free(item);
					return true;
				}

				item = (CLIENTE*)item->header.Flink;
			}
			while ((CLIENTE*)item != clientes);
		}
		__finally {
			LeaveCriticalSection(&flagListaClientes);
		}

		return false;
	}
	__except (1) {
		return false;
	}
}

void __fastcall Servidor::limparListaClientes() {
	__try {
		EnterCriticalSection(&flagListaClientes);
		__try {
			CLIENTE *item = clientes;
			do {

				ContextoCliente *contexto = item->cliente;
				CLIENTE *novoitem = (CLIENTE*)item->header.Flink;

				if (contexto != NULL && contexto->socket != 0) {
					item->header.Blink->Flink = item->header.Flink;
					item->header.Flink->Blink = item->header.Blink;
					free(item->cliente);
					free(item);
				}

				item = novoitem;
			}
			while ((CLIENTE*)item != clientes);
		}
		__finally {
			LeaveCriticalSection(&flagListaClientes);
		}
	}
	__except (1) {
	}
}

void __fastcall Servidor::inicializar() {
	clientes = (CLIENTE*)malloc(sizeof*clientes);
	clientes->header.Flink = (LIST_ENTRY*)clientes;
	clientes->header.Blink = (LIST_ENTRY*)clientes;

	closeFlag = CreateEvent(NULL, TRUE, FALSE, NULL);
	InitializeCriticalSection(&flagConsole);
	InitializeCriticalSection(&flagListaClientes);

	WSADATA wsaData;
	int nResult = WSAStartup(MAKEWORD(2, 2), &wsaData);

	if (nResult != NO_ERROR) {
		// Erro ao executar WSAStartup()
		logar("Erro 1241");
		return;
	}
}

void __fastcall Servidor::limpar() {
	__try {
		SetEvent(closeFlag);
		TerminateThread(threadConexao, 0);
		CloseHandle(threadConexao);
		// WaitForSingleObject(threadConexao, INFINITE);

		CLIENTE *lista = clientes;
		while ((DWORD)(lista->header.Flink) != (DWORD)clientes) {
			if ((DWORD)lista != (DWORD)clientes) {
				WaitForSingleObject(lista->cliente->thread, INFINITE);
			}
			lista = (CLIENTE*)lista->header.Flink;
		}

		limparListaClientes();
	}
	__except (1) {
	}
}

void __fastcall Servidor::desinicializar() {
	__try {
		DeleteCriticalSection(&flagConsole);
		DeleteCriticalSection(&flagListaClientes);
		CloseHandle(closeFlag);
		WSACleanup();
	}
	__except (1) {
	}
}

void __fastcall Servidor::terminar() {
	__try {
		closesocket(ListenSocket);
		WSACleanup();
		SetEvent(closeFlag);
		limpar();
		// Close open sockets
		// closesocket(ListenSocket);
		desinicializar();
	}
	__except (1) {
	}
}

void __fastcall Servidor::ClienteDesconectar() {
	closesocket(clienteSock);
}

bool __fastcall Servidor::ClienteEnviar(int tipo, char* buffer, int len) {
	int enviados;
	PACOTE *pacote = (PACOTE*)malloc(4 + len);
	pacote->len = (WORD)len;
	pacote->tipo = (WORD)tipo;
	memcpy(pacote->bufferEx, buffer, len);
	enviados = send(clienteSock, (char*)pacote, len + 4, 0);
	if (enviados == SOCKET_ERROR) {
		// closesocket(clienteSock);
		logar("Erro 7826");
		return false;
	}
	return true;
}

bool __fastcall Servidor::ClienteConectar(int porta) {
	clienteSock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (clienteSock == INVALID_SOCKET) {
		// Error occurred while opening socket
		logar("Erro 8322: %d", WSAGetLastError());
		return false;
	}

	struct hostent *server = gethostbyname("localhost");

	if (server == NULL) {
		closesocket(clienteSock);
		logar("Erro 9283");
		return false;
	}

	struct sockaddr_in ServerAddress;
	ZeroMemory((char*) & ServerAddress, sizeof(ServerAddress));
	ServerAddress.sin_family = AF_INET;
	CopyMemory((char*) & ServerAddress.sin_addr.s_addr, (char*)server->h_addr,
		server->h_length);
	ServerAddress.sin_port = htons(porta);
	if (connect(clienteSock, reinterpret_cast<const struct sockaddr*>
			(&ServerAddress), sizeof(ServerAddress)) == SOCKET_ERROR) {
		closesocket(clienteSock);
		// Erro ao conectar
		logar("Erro 5444");
		return false;
	}

	return true;
}

bool __fastcall Servidor::conectar(int porta) {
	__try {
		inicializar();
		// Create a socket
		ListenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
		if (ListenSocket == INVALID_SOCKET) {
			// Error occurred while opening socket
			logar("Erro 7224: %d", WSAGetLastError());
			desinicializar();
			return false;
		}

		// Cleanup and Init with 0 the ServerAddress
		ZeroMemory((char*) & ServerAddress, sizeof(ServerAddress));

		// Port number will be supplied as a commandline argument
		nPortNo = porta;

		// Fill up the address structure
		ServerAddress.sin_family = AF_INET;
		ServerAddress.sin_addr.s_addr = INADDR_ANY;
		// WinSock will supply address
		ServerAddress.sin_port = htons(nPortNo); // comes from commandline

		// Assign local address and port number
		if (bind(ListenSocket, (struct sockaddr*) & ServerAddress, sizeof
				(ServerAddress)) == SOCKET_ERROR) {
			closesocket(ListenSocket);
			// Error occurred while binding
			logar("Erro 1131.");
			desinicializar();
			return false;
		}

		// Make the socket a listening socket
		if (SOCKET_ERROR == listen(ListenSocket, SOMAXCONN)) {
			closesocket(ListenSocket);
			// Error occurred while listening
			logar("Erro 9290.");
			desinicializar();
			return false;
		}

		//
		// Cria thread de conexão
		DWORD threadID;
		threadConexao = CreateThread(0, 0, ThreadConnect, (void*)this, 0, &threadID);
		// logar("Servidor conectado!");*/
		return true;
	}
	__except (1) {
		debugar("Error in conectting server!");
		return false;
	}

}

void Servidor::logar(char *szFormat, ...) {
	EnterCriticalSection(&flagConsole);

	va_list args;
	va_start(args, szFormat);

	char buffer[256];
	vsprintf(buffer, szFormat, args);
	debugar(buffer);

	va_end(args);

	LeaveCriticalSection(&flagConsole);
}

// ---------------------------------------------------------------------------

//
// Aceita a conexão de um cliente e o adiciona na lista
//
void __fastcall Servidor::aceitarCliente() {
	__try{
		sockaddr_in clienteAddress;
		int clienteLength = sizeof(clienteAddress);

		// Accept remote connection attempt from the client
		SOCKET cliente = accept(ListenSocket, (sockaddr*) & clienteAddress,
			&clienteLength);

		if (cliente == INVALID_SOCKET) {
			// logar("\nError occurred while accepting socket: %ld.", WSAGetLastError());
			return;
		}

		PacoteTratamento *pacote = (PacoteTratamento*)malloc(sizeof*pacote);

		DWORD threadID;
		ContextoCliente *contexto = (ContextoCliente*)malloc(sizeof*contexto);
		contexto->socket = cliente;

		pacote->contexto = contexto;
		pacote->servidor = this;

		contexto->thread = CreateThread(0, 0, ThreadRecebedor, (void*)pacote, 0, &threadID);
		contexto->threadID = threadID;
		adicionarCliente(contexto);

		contexto->subplugin = NULL;

		FILE_SUBPLUGIN fileSubplugin;
		memset(&fileSubplugin, 0, sizeof fileSubplugin);
		enviar(contexto->socket, TP_SUBPLUGIN, (char*) & fileSubplugin, sizeof fileSubplugin);
		// logar("Cliente conectado em: %d", (int)inet_ntoa(ClientAddress.sin_addr));
	}__except(1){
        debugar("Erro 5520");
	}
}

//
// Thread gerenciadora de conexões
//
DWORD WINAPI ThreadConnect(LPVOID lpParam) {
	Servidor *servidor = (Servidor*)lpParam; // (SOCKET)ListenSocket;
	while (WaitForSingleObject(servidor->closeFlag, 10) != WAIT_OBJECT_0) {
		servidor->aceitarCliente();
	}
	return 1;
}

DWORD WINAPI ThreadRecebedor(LPVOID lpParam) {
	int quantidadeBytes = 0;
	PacoteTratamento *pacote = (PacoteTratamento*)lpParam;
	ContextoCliente *contexto = pacote->contexto;
	Servidor *servidor = pacote->servidor;
	free(pacote);

	if (contexto == NULL || contexto->socket == 0)
		return 0;

	while (WaitForSingleObject(servidor->closeFlag, 10) != WAIT_OBJECT_0) {
		int len = 0;
		PACOTE *pacote = NULL;
		__try{
			quantidadeBytes = recv(contexto->socket, (char*) & len, 2, 0);
		}__except(1){
        	debugar("Erro 0019");
		}

		__try{
			//debugar("recebendo [1] %d / %d bytes", quantidadeBytes, len);
			if (len > 0) {
				pacote = (PACOTE*)malloc(len + 5);
				memset(pacote, 0, len + 5);
				pacote->len = len;
				__try {
					quantidadeBytes = recv(contexto->socket, (char*) & pacote->tipo, len + 2, 0);
					//debugar("recebendo [2] %d / %d bytes", quantidadeBytes, len);
					if (quantidadeBytes <= 0) {
						closesocket(contexto->socket);
						servidor->removerCliente(contexto->socket);
						break;
					}
					servidor->processar(contexto, pacote);
					// enviarTeste(contexto->socket);
				}
				__finally {
					free(pacote);
				}
			}else{
				//debugar("step6");
				//debugar("[Server] Cliente desconectado...");
				if((DWORD)servidor->onClientDisconnect){
					typedef void(__stdcall PASCAL * TClientDisconnect)(ContextoCliente *contexto);
					TClientDisconnect onDisconnect = (TClientDisconnect)servidor->onClientDisconnect;
					onDisconnect(contexto);
					servidor->removerCliente(contexto->socket);
				}
				return 0;
			}
        }__except(1){
        	debugar("Erro 0020");
		}
	}
	return 1;
}

bool __fastcall Servidor::enviar(SOCKET sock, int tipo, char* buffer, int len) {
	if (sock > 0) {
		int enviados;
		PACOTE *pacote = (PACOTE*)malloc(4 + len);
		pacote->len = (WORD)len;
		pacote->tipo = (WORD)tipo;
		memcpy(&pacote->buffer, buffer, len);
		enviados = send(sock, (char*)pacote, len + 4, 0);
		if (enviados == SOCKET_ERROR) {
			// closesocket(clienteSock);
			logar("Erro 7826");
			return false;
		}
		return true;
	}
	else {
		return false;
	}
}

// ---------------------------------------------------------------------------

#pragma package(smart_init)
