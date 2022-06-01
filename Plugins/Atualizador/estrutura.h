//---------------------------------------------------------------------------

#ifndef estruturaH
#define estruturaH

#include <windows.h>

enum TIPO_PACOTE_PLUGIN {
	TP_LIGAR,
	TP_RESULTADO
};

typedef struct _EnderecoMemoria {
	 DWORD Ponteiro;
	 DWORD Offset;
	 DWORD Size;
} TEnderecoMemoria, *PEnderecoMemoria;


//---------------------------------------------------------------------------
#endif
