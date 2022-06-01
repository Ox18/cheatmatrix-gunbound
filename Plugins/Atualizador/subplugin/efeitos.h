//---------------------------------------------------------------------------

#ifndef efeitosH
#define efeitosH
//---------------------------------------------------------------------------

typedef struct _EfeitoVento {
	double efeito;
    double gravidade;
} TEfeitoVento;

const int quantidadeMobiles = 25;
extern TEfeitoVento effectFixes[quantidadeMobiles-1][36];
extern TEfeitoVento effects[quantidadeMobiles-1];

#endif
