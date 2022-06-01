//---------------------------------------------------------------------------

#ifndef commonH
#define commonH

#include "estruturaBot.h"

typedef struct _EfeitoVento {
	double efeito;
    double gravidade;
} TEfeitoVento;

extern TBotInfos *BotInfos;
extern TBotTeste *BotTeste;
extern const quantidadeMobiles = 24;
extern TEfeitoVento effectFixes[quantidadeMobiles][36];
extern TEfeitoVento effects[quantidadeMobiles];

int __fastcall MobileToIndex(int valor);



//---------------------------------------------------------------------------
#endif
