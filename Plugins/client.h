//---------------------------------------------------------------------------

#ifndef clientH
#define clientH
//---------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <winsock2.h>
#include "tiposBase.h"

#pragma pack(push,1)

typedef void (__stdcall *MetodoProcessarClient)( PACOTE *pacote );

class Cliente {
	private:
        CRITICAL_SECTION flagConsole;
		CRITICAL_SECTION flagListaClientes;
	public:
		Cliente(PROC callback){
			clienteSock = 0;
			closeFlag = 0;
			threadProcessamento = 0;
			conectado = false;
			processar = (MetodoProcessarClient)callback;
		}
		~Cliente(){
        	terminar();
		}

		//PROC OnCarregarPlugin;
        SOCKET clienteSock;
		HANDLE closeFlag;
		HANDLE threadProcessamento;
		bool conectado;
		MetodoProcessarClient processar;

		bool enviar(int tipo, char* buffer, int len);
		void desinicializar();
		void inicializar();
		bool conectar(int porta);
		void logar(char *szFormat, ...);
		void terminar();
		void limpar();
		void desconectar();

};

DWORD WINAPI ThreadProcessamentoClientUnique( LPVOID lpParam );

#pragma pack(pop)

#endif
