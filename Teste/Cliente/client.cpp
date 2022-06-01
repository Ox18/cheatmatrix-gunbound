//---------------------------------------------------------------------------


#pragma hdrstop

#include "client.h"
#include "unitCliente.h"

SOCKET clienteSock = 0;
HANDLE closeFlag = 0;
CRITICAL_SECTION flagConsole;
CRITICAL_SECTION flagListaClientes;
HANDLE threadProcessamento = 0;
bool conectado = false;

//---------------------------------------------------------------------------

void inicializar(){
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

	DWORD threadID;
	threadProcessamento = CreateThread(0, 0, ThreadProcessamento, (void *)clienteSock, 0, &threadID);
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

void limpar(){
	__try{
		SetEvent(closeFlag);
		WaitForSingleObject(threadProcessamento, INFINITE);
	}__except(1){
	}
}

void terminar(){
	__try{
		limpar();
		closesocket(clienteSock);
		desinicializar();
    }__except(1){
	}
}

void conectar(int porta){
	 ResetEvent(closeFlag);
	 clienteSock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	 if (clienteSock == INVALID_SOCKET) {
          // Error occurred while opening socket
		  logar("Erro 8322: %d", WSAGetLastError());
		  return;
	 }

	 struct hostent *server = gethostbyname("localhost");

	 if (server == NULL) {
		  closesocket(clienteSock);
		  logar("Erro 9283");
		  return;
	 }

	 struct sockaddr_in ServerAddress;
	 ZeroMemory((char *) &ServerAddress, sizeof(ServerAddress));
	 ServerAddress.sin_family = AF_INET;
	 CopyMemory((char *)&ServerAddress.sin_addr.s_addr, (char *)server->h_addr, server->h_length);
	 ServerAddress.sin_port = htons(porta);
	 if (connect(clienteSock, reinterpret_cast<const struct sockaddr *>(&ServerAddress),sizeof(ServerAddress)) == SOCKET_ERROR) {
		  closesocket(clienteSock);
		  // Erro ao conectar
		  logar("Erro 5444");
		  return;
	 }

	 conectado = true;
}

void desconectar(){
	 closesocket(clienteSock);
	 SetEvent(closeFlag);
	 WaitForSingleObject(threadProcessamento, INFINITE);
	 conectado = false;
}

bool enviar(int tipo, char* buffer, int len){
	if(conectado){
		int enviados;
		PACOTE *pacote = (PACOTE*)malloc(4+len);
		pacote->len = (WORD)len;
		pacote->tipo = (WORD)tipo;
		memcpy(&pacote->buffer, buffer, len);
		enviados = send(clienteSock, (char*)pacote, len+4, 0);
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

void logar(char *szFormat, ...) {
	EnterCriticalSection(&flagConsole);

	va_list args;
	va_start(args, szFormat);

	char buffer[256];
	vsprintf(buffer, szFormat, args );
	Form14->Memo1->Lines->Add(buffer);

	va_end(args);

	LeaveCriticalSection(&flagConsole);
}

void processar(PACOTE *pacote){
	if(pacote != NULL){
		switch(pacote->tipo){
			case 0: {
				TesteStruct *teste = (TesteStruct *)&pacote->buffer;
				logar("Servidor: %d", teste->valor1);
			} break;
			default: break;
		}
	}
}

DWORD WINAPI ThreadProcessamento( LPVOID lpParam ) {
	int quantidadeBytes = 0;
	SOCKET servidor = (SOCKET)lpParam;

	while (WaitForSingleObject(closeFlag, 0)!= WAIT_OBJECT_0){
		if(conectado && clienteSock > 0){
			int len = 0;
			PACOTE *pacote = NULL;
			quantidadeBytes = recv(clienteSock, (char*)&len, 2, 0 );
			if(len > 0){
				pacote = (PACOTE*)malloc(len + 5);
				memset(pacote, 0, len + 5);
				pacote->len = len;
				__try {
					 quantidadeBytes = recv(clienteSock, (char*)&pacote->tipo, len+2, 0 );
					 if (quantidadeBytes <= 0) {
						 conectado = false;
						 closesocket(clienteSock);
						 SetEvent(closeFlag);
						 break;
					 }
					 processar(pacote);
				}__finally{
					 free(pacote);
				}
			}
		}
	}
}

#pragma package(smart_init)
