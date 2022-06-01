//---------------------------------------------------------------------------

#ifndef statusH
#define statusH

#include "windows.h"

class TTelaStatus {
	public:
	TTelaStatus(int lpDuracao){
	   duracao = lpDuracao;
	   primeiroTick = GetTickCount();
	}

	public:
	int duracao;
	DWORD primeiroTick;

	void __fastcall inicia();
	bool __fastcall mostrar();
	void __fastcall setarDelay(int delay);
};
//---------------------------------------------------------------------------
#endif
