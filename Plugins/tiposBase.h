//---------------------------------------------------------------------------

#ifndef tiposBaseH
#define tiposBaseH
//---------------------------------------------------------------------------
#include <windows.h>
#include "debugUtils.h"

typedef struct ATALHO {
	unsigned long pluginID;
	int id;
	char mods;
	short tecla;
} ATALHO;

enum LINGUAGEM {
   PT_BR,
   EN_US
};

typedef struct POINT_FLOAT{
	float x;
	float y;
}POINT_FLOAT;

typedef struct PACOTE {
   WORD len;
   WORD tipo;
   union{
		BYTE *buffer;
        BYTE bufferEx[];
   };
} PACOTE;

typedef struct COMANDO_SUBPLUGIN {
	int tipo;
	unsigned long pluginID;
	char valor[];
} COMANDO_SUBPLUGIN;

typedef struct FILE_SUBPLUGIN {
	unsigned long pluginID;
	char idioma;
	char nome[];
} FILE_SUBPLUGIN;

enum TIPO_PACOTE {
	TP_DISCONNECT,
	TP_MENSAGEM,
	TP_SUBPLUGIN,
	TP_PONTEIRO,
	TP_COMANDO,
	TP_ATALHO,
	TP_IDIOMA
};

enum TIPO_DESENHO {
	TD_DDRAW,
	TD_GDI,
	TD_UPDATE
};

enum HACK_TYPE {
	HT_Hack = 5,
	HT_Control = 10
};

enum IMAGE_STATUS {
	IS_Normal,
	IS_Main,
	IS_Running,
	IS_Open
};

enum MESSAGE_TYPE {
	MT_Close,
	MT_Hide,
	MT_Show,
	MT_Visible
};

typedef struct PLUGIN_INFO {
	BOOL PutInList;
	HMODULE PluginHandle;
	HANDLE WindowHandle;
	BOOL Registered;
	int Tipo;
	int Status;
	void* Offset;
	DWORD Codigo;
	char* Nome;
	char* Nick;
	char* Game;
	char* FileName;
	char* Key;
	char* Nulo;
} PLUGIN_INFO;

typedef struct PLUGIN_MEMORY_DATA {
	 int PacketID;
	 unsigned long Ponteiro;
	 unsigned long Offset;
	 unsigned long Range;
	 unsigned long Size;
	 unsigned long Endereco;
	 char* Valor;
} PLUGIN_MEMORY_DATA;

typedef struct DADOS_PONTEIRO {
	 unsigned long pluginID;
	 unsigned long packetID;
	 unsigned long ponteiro;
	 unsigned long offset;
	 unsigned long range;
	 unsigned long size;
	 unsigned long endereco;
	 union {
		char valorEx[];
		char *valor;
	 };
} DADOS_PONTEIRO;

typedef struct MENSAGEM_TEXTO {
	WORD length;
	char buffer[];
} MENSAGEM_TEXTO;

/*
typedef struct LISTA_PONTEIROS {
	int quantidade;
	int maiorID;
	DADOS_PONTEIRO **ponteiros;
} LISTA_PONTEIROS;
*/
typedef struct METODOS {
	PROC iniciar;
	PROC processarComando;
	PROC atualizarEnderecos;
	PROC processarAtalho;
	PROC draw;
	PROC update;
	PROC recv;
	PROC send;
} METODOS;

class LISTA {
	private:
		int total;
		CRITICAL_SECTION sessaoAdd;
		long*** tabela;
		int dimensao;
	public:
		LISTA(int tamanho){
			if(tamanho == 0)
				dimensao = 64;
			else
            	dimensao = tamanho;
			total = 0;
			tabela = new long**[dimensao];
			memset(&tabela[0], 0, dimensao);
			tabela[0] = new long*[dimensao];
			memset(tabela[0], 0, dimensao);
			InitializeCriticalSection(&sessaoAdd);
		}
		~LISTA(){
            DeleteCriticalSection(&sessaoAdd);
		}
    public:
		void __fastcall add(long* valor);
		void __fastcall clear();
		long* __fastcall get(int indice);
		void __fastcall set(int indice, long* valor);
		bool __fastcall exists(int indice);
		int __fastcall size();
		void __fastcall remove(int indice);
		void __fastcall addAt(int indice, long* valor);
		void __fastcall removeFromAndFree(int indice);
		void __fastcall removeFrom(int indice);
};

typedef void(__stdcall PASCAL * TEnviarParaCliente)(int socket, TIPO_PACOTE tipo, char* buffer, int size);

typedef struct SUBPLUGIN {
	DWORD pluginID;
	HANDLE handle;
	LISTA *ponteiros;
	METODOS *metodos;
	HANDLE closeFlag;
	char *nomePlugin;
	char *diretorioDllMae;
	void *cliente;
	char idioma;
	TEnviarParaCliente metodoEnviar;
} SUBPLUGIN;

#endif
