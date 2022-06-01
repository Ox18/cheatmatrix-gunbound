// ---------------------------------------------------------------------------

#pragma hdrstop

#include "constantes.h"
//#include "baseSP.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)

int idioma = 0;

int diferencial = 0;
bool patched;

void patchConstantes() {
	if (!patched) {
		patched = true;
		char valor;
		DWORD oldProtection;

		randomize();
		diferencial = random(50) + 30;

		for (int j = 1; j < 300; j++) {
			__try {
				//mutacao(constanteX(j, 0));
				//mutacao(constanteX(j, 1));
				mutacao(constanteX(j, -1));
			} __except (1) {
			}
		}
	}
}

char* mutacao(char *valor) {
	if (diferencial == 0) {
		diferencial = random(50) + 30;
	}

	if (valor != NULL && strlen(valor) > 0) {
		DWORD oldProtection;
		VirtualProtect(valor, strlen(valor), PAGE_EXECUTE_READWRITE, &oldProtection);
		for (int i = 0; i < (strlen(valor)); i++)
			valor[i] = (((BYTE)valor[i]) & 0xFF) ^ diferencial;
		VirtualProtect(valor, strlen(valor), oldProtection, &oldProtection);
	}
	return valor;
}

char* constanteX(int id, int lpIdioma){
	BYTE idiomaAtual = 0;
	bool encontrado = true;

	if (lpIdioma == -2)
		idiomaAtual = ((!idioma)?0:idioma);

	if (lpIdioma == -2 || lpIdioma == -1) {
		switch(id){
			case 200: return "configs.ini";
			case 201: return "configs";
			case 202: return "ses";
			default: encontrado = false;
		}
	}else{
		idiomaAtual = lpIdioma;
		encontrado = false;
	}

	if (!encontrado && lpIdioma != -1){
		if(idiomaAtual == 0){
			switch(id){
				default: return 0;
			}
		} else {
			switch(id){
				default: return 0;
			}
		}
	}
	return 0;
}

char* constante(int id){
	return mutacao((char*)strdup(constanteX(id)));
}

char* constanteEx(int id, char* buffer){
	if(buffer){
		__try{
			char* result = constanteX(id);
			memset(buffer,0,strlen(result)+1);
			memcpy(&buffer[0],result,strlen(result));
			mutacao(buffer);
		}__except(1){
		}
	}
}
