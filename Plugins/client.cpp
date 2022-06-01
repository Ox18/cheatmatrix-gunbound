//---------------------------------------------------------------------------


#pragma hdrstop

#include "client.h"

//---------------------------------------------------------------------------

void Cliente::inicializar(){
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
	threadProcessamento = CreateThread(0, 0, ThreadProcessamentoClientUnique, (void *)this, 0, &threadID);
}

void Cliente::desinicializar(){
	__try{
		DeleteCriticalSection(&flagConsole);
		DeleteCriticalSection(&flagListaClientes);
		CloseHandle(closeFlag);
		WSACleanup();
    }__except(1){
    }
}

void Cliente::limpar(){
	__try{
		SetEvent(closeFlag);
		WaitForSingleObject(threadProcessamento, INFINITE);
	}__except(1){
	}
}

void Cliente::terminar(){
	__try{
		limpar();
		closesocket(clienteSock);
		desinicializar();
    }__except(1){
	}
}

bool Cliente::conectar(int porta){
	 ResetEvent(closeFlag);
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
	 ZeroMemory((char *) &ServerAddress, sizeof(ServerAddress));
	 ServerAddress.sin_family = AF_INET;
	 CopyMemory((char *)&ServerAddress.sin_addr.s_addr, (char *)server->h_addr, server->h_length);
	 ServerAddress.sin_port = htons(porta);
	 if (connect(clienteSock, reinterpret_cast<const struct sockaddr *>(&ServerAddress),sizeof(ServerAddress)) == SOCKET_ERROR) {
		  closesocket(clienteSock);
		  // Erro ao conectar
		  logar("Erro 5444");
		  return false;
	 }

	 conectado = true;
	 return true;
}

void Cliente::desconectar(){
	 closesocket(clienteSock);
	 SetEvent(closeFlag);
	 WaitForSingleObject(threadProcessamento, INFINITE);
	 conectado = false;
}

bool Cliente::enviar(int tipo, char* buffer, int len){
	if(conectado){
		int enviados;
		PACOTE *pacote = (PACOTE*)malloc(4+len);
		pacote->len = (WORD)len;
		pacote->tipo = (WORD)tipo;
		memcpy(pacote->bufferEx, buffer, len);
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

void Cliente::logar(char *szFormat, ...) {
	EnterCriticalSection(&flagConsole);

	va_list args;
	va_start(args, szFormat);

	char buffer[256];
	vsprintf(buffer, szFormat, args );
	//Form14->Memo1->Lines->Add(buffer);

	va_end(args);

	LeaveCriticalSection(&flagConsole);
}

/*
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
*/

DWORD WINAPI ThreadProcessamentoClientUnique( LPVOID lpParam ) {
	int quantidadeBytes = 0;
	Cliente *cliente = (Cliente*)lpParam;

	while (WaitForSingleObject(cliente->closeFlag, 1)!= WAIT_OBJECT_0){
		if(cliente->conectado && cliente->clienteSock > 0){
			int len = 0;
			PACOTE *pacote = NULL;
			quantidadeBytes = recv(cliente->clienteSock, (char*)&len, 2, 0 );
			if(len > 0){
				pacote = (PACOTE*)malloc(len + 5);
				memset(pacote, 0, len + 5);
				pacote->len = len;
				__try {
					 quantidadeBytes = recv(cliente->clienteSock, (char*)&pacote->tipo, len+2, 0 );
					 if (quantidadeBytes <= 0) {
						 cliente->conectado = false;
						 closesocket(cliente->clienteSock);
						 SetEvent(cliente->closeFlag);
						 break;
					 }
					 cliente->processar(pacote);
				}__finally{
					 free(pacote);
				}
			}else{
            	//debugar("Cliente desconectado!");
                closesocket(cliente->clienteSock);
				//SetEvent(cliente->closeFlag);
				cliente->conectado = false;
				//return 0;
            }
		}
	}
}

#pragma package(smart_init)
