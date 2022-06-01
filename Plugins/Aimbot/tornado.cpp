//---------------------------------------------------------------------------


#pragma hdrstop

#include "tornado.h"

//---------------------------------------------------------------
//		TGrupoTornados
//---------------------------------------------------------------

	void __fastcall TGrupoTornados::deletar(int x){
		for(int i = 0; i < 5; i++){
			if(tornados[i].completo()){
				if( (x > tornados[i].inicio  && x < tornados[i].fim) || (x > tornados[i].fim  && x < tornados[i].inicio) ){
					tornados[i].limpar();
                    atual = i;
				}
			}
		}
	}

	void __fastcall TGrupoTornados::setaLinha(int x, bool espelho){
        if(tornados[atual].completo()){
        	atual = 0;
			for(int i = 0; i < 5; i++){
            	if(!tornados[i].completo()){
					atual = i;
					break;
				}
			}
			tornados[atual].limpar();
		}

		tornados[atual].seta(x, espelho);
	}

	void __fastcall TGrupoTornados::setaLinha(int x){
        setaLinha(x, false);
	}

	void __fastcall TGrupoTornados::removerLinha(){
		if(!tornados[atual].completo()){
			tornados[atual].limpar();
		}
	}

	void __fastcall TGrupoTornados::limpar(){
		for(int i = 0; i < 5; i++){
			tornados[i].limpar();
		}
		atual = 0;
	}

	void __fastcall TGrupoTornados::cancelar(){
    	tornados[atual].limpar();
	}

	bool __fastcall TGrupoTornados::temTornado(){
		for(int i = 0; i < 5; i++){
			if(tornados[i].completo()) return true;
		}
		return false;
	}

//---------------------------------------------------------------
//		TTornado
//---------------------------------------------------------------

	void __fastcall TTornado::limpar(){
		etapa = 0;
	}

	void __fastcall TTornado::seta(int x, bool isEspelho){
		espelho = isEspelho;
        seta(x);
	}

	void __fastcall TTornado::seta(int x){
		switch(etapa){
			case 0:
				etapa = 1;
				inicio = x;
				break;
			case 1:
				etapa = 2;
				if(x < inicio){
					fim = inicio;
					inicio = x;
				}else{
					fim = x;
				}
				break;
			default:
				etapa = 0;
				break;
		}
	}

	bool __fastcall TTornado::completo(){
		return (etapa == 2);
	}

//---------------------------------------------------------------------------

#pragma package(smart_init)
