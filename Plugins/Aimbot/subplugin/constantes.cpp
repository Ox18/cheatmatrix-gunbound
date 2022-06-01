// ---------------------------------------------------------------------------

#pragma hdrstop

#include "constantes.h"
#include "baseSP.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)

char* getString(int id){
	return constante(id);
}

char* constante(int id){
	BYTE linguagem = base->idioma;
	if(linguagem == PT_BR){
		switch(id){
			case 1: return "Mobile: ";
			case 2: return "Alvo: ";
			case 3: return "BackShot: ";
			case 4: return "Modo: ";
			case 5: return "Efeito: ";
			case 6: return "AutoShot: ";
			case 7: return "Status: ";
			case 8: return "Sim";
			case 9: return "Nao";
			case 10: return "Mais Perto";
			case 11: return "Mais Longe";
			case 12: return "Slice";
			case 13: return "Mouse";
			case 14: return "Aleatorio";
			case 15: return "Personalizado";
			case 16: return "Espelho";
			case 17: return "Tornado";
			case 18: return "Ligado";
			case 19: return "Desligado";
			case 20: return "Falha ao gravar o AutoShot";
		}
	} else {
    	switch(id){
			case 1: return "Mobile: ";
			case 2: return "Target: ";
			case 3: return "BackShot: ";
			case 4: return "Mode: ";
			case 5: return "Effect: ";
			case 6: return "AutoShot: ";
			case 7: return "Status";
			case 8: return "Yes";
			case 9: return "No";
			case 10: return "Shorter";
			case 11: return "Further";
			case 12: return "Slice";
			case 13: return "Mouse";
			case 14: return "Random";
			case 15: return "Custom";
			case 16: return "Mirror";
			case 17: return "Tornado";
			case 18: return "ON";
			case 19: return "OFF";
			case 21: return "Failed to write AutoShot";
		}
	}

}
