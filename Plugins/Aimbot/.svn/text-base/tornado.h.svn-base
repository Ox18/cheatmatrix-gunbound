//---------------------------------------------------------------------------

#ifndef tornadoH
#define tornadoH

#include "windows.h"

class TTornado {
	public:
	TTornado(){
		// Etapa 0 = Nao iniciado
		// Etapa 1 = Iniciado
		// Etapa 2 = Finalizado
     	etapa = 0;
	}

	public:
	int etapa;
	int inicio;
	int fim;
	bool espelho;

	void __fastcall limpar();
	void __fastcall seta(int x);
	void __fastcall seta(int x, bool isEspelho);
	bool __fastcall completo();
	void __fastcall cancelar();
};

class TGrupoTornados{
	public:

	TGrupoTornados(){
		ZeroMemory(&tornados[0], sizeof tornados);
		atual = 0;
		tornados[atual].limpar();
	}

	// Mostra Linha de suporte
	bool modoTornado;
	int atual;
	TTornado tornados[5];

	public:
		void __fastcall cancelar();
		void __fastcall setaLinha(int x);
		void __fastcall setaLinha(int x, bool espelho);
		void __fastcall deletar(int x);
		void __fastcall limpar();
		bool __fastcall temTornado();
		void __fastcall removerLinha();
};

//---------------------------------------------------------------------------
#endif
