//---------------------------------------------------------------------------

#ifndef estruturaBotH
#define estruturaBotH

//#include "tornado.h"
#include <windows.h>

enum MOBILES {
	MB_ARMOR,
	MB_MAGE,
	MB_NAK,
	MB_TRICO,
	MB_BIG_FOOT,
	MB_BOOMER,
	MB_RAON,
	MB_LIGHTNING,
	MB_JD,
	MB_ASATE,
	MB_TURTLE,
	MB_ICE,
	MB_GRUB,
	MB_ADUKA,
	MB_KASILON,
	MB_JFROG,
	MB_DRAGON,
	MB_KNIGHT,
	MB_PHOENIX,
	MB_MAYA,
	MB_WOLF,
	MB_TIBURON,
	MB_BLUE_WHALE,
	MB_FRANK,
	MB_RANDOM,
	MB_TODOS
};

enum TIPO_PACOTE_BOT {
	TP_LIGAR,
	TP_PERSONALIZAR_MODO_MIRA,
	TP_PROXIMO_MODO_MIRA,
	TP_MODO_MIRA_ANTERIOR,
	TP_MODO_JUDAS,
	TP_MODO_GDI,
	TP_LINHA_VERTICAL,
	TP_OPCAO_TIRO,
	TP_MODO_DEBUG,
	TP_OPCAO_COR,
	TP_TEMPO_MENU,
	TP_ESPESSURA_LINHA,
	TP_MENU_POS
};

enum MODO_MIRA {
    AM_Perto,
    AM_Longe,
    AM_Slice,
    AM_Mouse,
	AM_Random,
	AM_Auto//,
	//AM_Personalizado
};

typedef struct _EnderecoMemoria {
	 DWORD Ponteiro;
	 DWORD Offset;
	 DWORD Size;
} TEnderecoMemoria, *PEnderecoMemoria;

typedef struct OPCOES_TIRO{
   int mobile;
   bool tiroEspecial;
   bool marcarSS;
} OPCOES_TIRO;

typedef struct OPCAO_COR{
   int indice;
   DWORD cor;
} OPCAO_COR;

typedef struct _mensagemTeste{
   DWORD param1;
   DWORD param2;
   DWORD param3;
   DWORD param4;
} mensagemTeste;

typedef struct _melhorForca{
	int forca;
	POINT ponto;
}TMelhorForca;

/*
typedef struct _botinfos
{
	int interfaceJogo;
	int aceleracaoVento;
	int anguloVento;
	int forcaBot;
	int anguloBot;
	int framesPorSegundo;
	int mobile;
	int modoMira;
	int tiro;
	int modoEfeito;
	int indiceBot;
	int semiForcaBot;
	int alvoManualID;
	TMelhorForca melhorForca;
	bool modoEspelho;
	bool mostrarStatus;
	bool autoMobile;
	bool backShot;
	bool ligado;
	bool mostrarLinha;
	bool direita;
	bool linhaGraficoAntigo;
	bool modoDebug;
	bool semiDireita;
	bool inGame;
	WORD inclinacaoBot;
	DWORD corLinha1;
	DWORD corLinha2;
	DWORD corCirculos1;
	DWORD corCirculos2;
	DWORD corEspelho;
	BYTE espessuraLinha;
	BYTE alvoID;
	BYTE times[8];
	BYTE flags[8];
	TPoint posicaoBot;
	TPoint semiPosicaoBot[8];
	TPoint posicaoAlvo;
	TPoint camera;
	float efeitoVento;
	float gravidade;
	HANDLE processoAlvo;
	PVOID processoBase;
	HWND janela;
	char nomeAlvo[15];
	char nomeMobile[30];
	TGrupoTornados tornados;
	TOpcoesTiro trico;
	TOpcoesTiro bigFoot;
	TOpcoesTiro mage;
	TOpcoesTiro turtle;
	TOpcoesTiro nak;
	TTiroMaya maya;
	TOpcoesTiro grub;
	TOpcoesTiro kasildon;
	TOpcoesTiro dragao;
	TOpcoesTiro cavalo;
	TOpcoesTiro raon;
	TOpcoesTiro frank;
	DWORD enderecos[20];
	//mensagemTeste teste;
	TPoint posicaoBotC;
	TPointFloat teste1;
	//bool alvoManual;
} *PBotInfos, TBotInfos;
*/

/*
typedef struct BOT_INFO{
		bool modoDebug; 	  // Define modo debug indicado pela matriz
		bool ligado;          // Define se o aim está ligado
		bool autoMobile;	  // Selecionar o mobile automaticamente
		bool backShot;		  // Indica se a mira está para traz
		bool acertarAmigos;   // Indica se deve mirar nos amigos
		int mobile;           // Mobile selecionado
		int modoMira;		  // Modo de mira - Slice/Auto/Close/Far/Mouse/Random
		int framesPorSegundo; // Amostragem
		CFG_LINHA linhas[10]; // Configuração de cor/espessura das linhas
		BYTE alvoID;		  // ID do alvo
		POINT semiPosicaoBot[8]; //

		TTiroMaya maya;
		TOpcoesTiro trico;
		TOpcoesTiro bigFoot;
		TOpcoesTiro mage;
		TOpcoesTiro turtle;
		TOpcoesTiro nak;
		TOpcoesTiro grub;
		TOpcoesTiro kasildon;
		TOpcoesTiro dragao;
		TOpcoesTiro cavalo;
		TOpcoesTiro raon;
		TOpcoesTiro frank;
}BOT_INFO;     */

typedef struct _botTeste
{
    float raio;
}TBotTeste;
//---------------------------------------------------------------------------
#endif
