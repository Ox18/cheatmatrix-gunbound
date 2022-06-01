//---------------------------------------------------------------------------


#pragma hdrstop

#include "status.h"

void __fastcall TTelaStatus::inicia(){
	primeiroTick = GetTickCount();
}

bool __fastcall TTelaStatus::mostrar(){

	if(primeiroTick == 0 || duracao == 0){
    	return false;
	}

	if(duracao < 0){
    	return true;
	}

	DWORD atual = GetTickCount();
	DWORD final = (atual - primeiroTick);
	
	if(final > duracao){
    	return false;
	}
}

void __fastcall TTelaStatus::setarDelay(int delay){
	duracao = delay;
}

//---------------------------------------------------------------------------

#pragma package(smart_init)
