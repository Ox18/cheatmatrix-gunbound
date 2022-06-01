//---------------------------------------------------------------------------

#pragma hdrstop

#include "tiposBase.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

int __fastcall LISTA::size(){
	return total;
}

void __fastcall LISTA::clear(){
	EnterCriticalSection(&sessaoAdd);
	__try{
		int novoTotal = total - 1;
		int index2 = novoTotal%dimensao;
		int index1 = ((novoTotal-index2)/dimensao) % dimensao;

		for(int n1 = index1; n1 >= 0; n1--){
			for(int n2 = index2; n2 >= 0; n2--){
				__try{
					if(tabela[n1][n2])
						free(tabela[n1][n2]);
                }__except(1){
                }
			}
			index2 = dimensao;
			__try{
				if(tabela[n1])
					free(tabela[n1]);
			}__except(1){
			}
		}

		tabela[0] = new long*[dimensao];
		memset(tabela[0], 0, dimensao);
        total = 0;
	}__finally{
		LeaveCriticalSection(&sessaoAdd);
	}
}

//
//	Formula do total: dimensao * n1 + n2
void __fastcall LISTA::add(long* valor){
	EnterCriticalSection(&sessaoAdd);
	__try{
		int totalInicial = total - 1;
		int index2 = totalInicial%dimensao;
		int index1 = ((totalInicial-index2)/dimensao) % dimensao;

		int novoTotal = total;
		int novoIndex2 = novoTotal%dimensao;
		int novoIndex1 = ((novoTotal-novoIndex2)/dimensao) % dimensao;

		if(novoIndex2 < index2){
			tabela[novoIndex1] = new long*[dimensao];
			memset(tabela[novoIndex1], 0, dimensao);
		}

		tabela[novoIndex1][novoIndex2] = (long*)valor;
		total++;// = novoTotal;
	}__finally{
		LeaveCriticalSection(&sessaoAdd);
	}
}

long* __fastcall LISTA::get(int indice){
	__try{
		if(indice < total){
			int index2 = indice%dimensao;
			int index1 = ((indice-index2)/dimensao) % dimensao;
			return (long*)tabela[index1][index2];
		}
	}__except(1){
	}
	return NULL;
}

void __fastcall LISTA::set(int indice, long* valor){
	__try{
		if(indice < total){
			int index2 = indice%dimensao;
			int index1 = ((indice-index2)/dimensao) % dimensao;
			tabela[index1][index2] = valor;
        }
	}__except(1){
	}
}

void __fastcall LISTA::addAt(int indice, long* valor){
	EnterCriticalSection(&sessaoAdd);
	__try{
		__try{
			if(indice < dimensao*dimensao){
				int novoTotal = indice + 1;
				if(novoTotal > total){
					for(int i = total; i < novoTotal; i++){
						add(0);
					}
				}
				set(indice, valor);
			}
		}__except(1){
		}
	}__finally{
		LeaveCriticalSection(&sessaoAdd);
	}
}

//
//	Realocar todos os items é complicado. Vamos então apenas
//	realocar o último para o buraco gerado
void __fastcall LISTA::remove(int indice){
	__try{
		if(indice <= total){
			total--;
        	int indiceFinal = total;
			int indexU2 = indiceFinal%dimensao;
			int indexU1 = ((indiceFinal-indexU2)/dimensao) % dimensao;
			long* ultimo = (long*)tabela[indexU1][indexU2];
			tabela[indexU1][indexU2] = 0;

			if(indice < total){
				int index2 = indice%dimensao;
				int index1 = ((indice-index2)/dimensao) % dimensao;
				__try{
					free(tabela[index1][index2]);
				}__except(1){
				}
				tabela[index1][index2] = ultimo;
			}

			//
			//	Verifica se diminuiu no nivel 1. Se sim, desaloca o ultimo
			int indiceFRecalc = total;
			int indexFR2 = indiceFRecalc%dimensao;
			int indexFR1 = ((indiceFRecalc-indexFR2)/dimensao) % dimensao;
			if(indexFR1 < indexU1){
				__try{
					free(tabela[indexU1]);
				}__except(1){
				}
			}
		}
	}__except(1){
	}
}

void __fastcall LISTA::removeFromAndFree(int indice){
	__try{
		if(indice < total){
			int index2 = indice%dimensao;
			int index1 = ((indice-index2)/dimensao) % dimensao;
			__try{
				free(tabela[index1][index2]);
			}__except(1){
			}
			tabela[index1][index2] = 0;
		}
	}__except(1){
	}
}

void __fastcall LISTA::removeFrom(int indice){
	__try{
		if(indice < total){
			int index2 = indice%dimensao;
			int index1 = ((indice-index2)/dimensao) % dimensao;
			tabela[index1][index2] = 0;
		}
	}__except(1){
	}
}

bool __fastcall LISTA::exists(int indice){
    return ((indice < total) && (get(indice) != 0));
}
