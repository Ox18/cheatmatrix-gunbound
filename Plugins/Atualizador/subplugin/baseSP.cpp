#pragma hdrstop

#include "baseSP.h"
#include "controle.h"
#include "..\..\tiposBase.h"

#pragma package(smart_init)

#define QNT_METODOS 3

METODOS metodos;
int currentPackets = 0;
SUBPLUGIN* base;
HINSTANCE instancia;
OS_INFO *versaoOS;

bool pluginAtivo() {
	if(base && base->ponteiros){
		return base->ponteiros->size() == GetMinimumPackets();
	}else{
		return false;
    }
}

void inicializarMetodos() {
	__try{
		static bool inicializado;
		if (!inicializado) {
			inicializado = true;

		   	metodos.processarAtalho = (PROC)ProcessarAtalho;
			metodos.iniciar = (PROC)Iniciar;
			metodos.processarComando = (PROC)ProcessarComando;
			metodos.draw = (PROC)Desenhar;
			metodos.update = (PROC)AtualizarValores;
			metodos.atualizarEnderecos = (PROC)AtualizarEnderecos;
		}
	}__except(1){
		debugar("Erro 1112");
		getError("1112");
	}
}
