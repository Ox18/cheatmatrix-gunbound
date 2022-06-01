//---------------------------------------------------------------------------

#ifndef mobilesH
#define mobilesH

#include "..\pixelUnit.h"
#include "..\..\mathUtils.h"
#include "..\tornado.h"
#include "estruturaBot.h"
#include "efeitos.h"
//#include "..\..\VMProtectSDK.h"
#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"

#define MAX_LINHAS 10;

enum TIPO_LINHA {
	TL_LINHA1 = 0,
	TL_LINHA2 = 1,
	TL_ESPELHO = 2,
	TL_TORNADO = 3,
	TL_BRANCO = 5,
};

typedef struct _BotPos{
	WORD x;
	WORD y;
	WORD inclinacao;
} TBotPos;

typedef struct _PhoenixCFG {
	float raio;
	float fase2;
	float tempo;
	float tempo2;
} TPhoenixCFG;

typedef struct _tiroMaya{
   bool tiroEspecial;
   float raio;
   float velocidadeAngular;
   float faseBola1;
   float faseBola2;
   int delayBolas; //
   float pico;
} TTiroMaya;

typedef struct _Raio {
	float raioMax;
	float raioMin;
	float raioAtual;
	float tick;
	bool flag;
} TRaio;

typedef struct _Rotacao {
	float anguloAtual;
	int anguloReal;
	float limite;
	float tick;
	bool sentido;
} TRotacao;

typedef struct _DistanceComp {
    int index;
    double distancia;
} TDistanceComp;

typedef struct _reta {
	double pa;
	double pb;
	double pc;
}TReta;

typedef struct _Projetil {
	_Projetil(int _anguloInicial, double _x, double _y, double _speedX, double _speedY){
		x = _x;
		y = _y;
		xInicial = 0;
		yInicial = 0;
		speedX = _speedX;
		speedY = _speedY;
		anguloInicial = _anguloInicial;
		inverter = false;
		ultimoSpeedX = 0;
		ultimoSpeedY = 0;
	}

	_Projetil(){
	}

	int anguloInicial;
	double x;
	double y;
	double xInicial;
	double yInicial;
	double speedX;  //Velocidade inicial
	double speedY;
	double inverter;
	double ultimoSpeedX;
	double ultimoSpeedY;
}TProjetil;

typedef struct __pointFloatD{
	__pointFloatD(float _x1, float _y1){
		x1 = _x1;
		y1 = _y1;
		x2 = x1;
		y2 = y1;
	}

	__pointFloatD(float _x1, float _y1, float _x2, float _y2){
		x1 = _x1;
		y1 = _y1;
		x2 = _x2;
		y2 = _y2;
	}

	__pointFloatD(){
    }

	float x1;
	float y1;
	float x2;
	float y2;
} TPointFloatD;

typedef struct _CM_RGB {
	char r;
	char g;
	char b;
    char x;
} TCM_RGB, *PCM_RGB;

typedef struct _opcoesTiro{
   bool tiroEspecial;
   bool marcarSS;
} TOpcoesTiro;

typedef char NOME_BOT[20];

//
//	Configuração das Linhas
//
typedef struct CFG_LINHA{
	Gdiplus::Pen *pen;
	DWORD cor;
    DWORD espessura;
} CFG_LINHA;

Gdiplus::Color ToGdiColor(DWORD valor);

class TShMatrix {

	private:
		int posicaoTelaX;
		int posicaoTelaY;
		int posicaoTelaC;
		int diametroX;
		int diametroY;
		int raio;
		int x;
		int y;
		bool temTornado;
		int count;
		float fatorForca;
		bool flagEspelho;
		float gravidade;
		TReta reta;
		POINTFLOAT velocidadeProjetil;
		float novaGravidade;

		TProjetil Projetil[6];
		POINTFLOAT ultimoProjetil[6];

		POINT pontoEstatico[6];
		POINT ultimoPontoEstatico[6];

		bool tornadosFlag[5];

		double Alfa, Beta, Gama, Delta;
		double aceleracaoVentoX, aceleracaoVentoY;
		double velocidadeTiroX,  velocidadeTiroY;

		/*
		Gdiplus::Pen *whitePen;
		Gdiplus::Pen *corLinha1;
		Gdiplus::Pen *corLinha2;
		Gdiplus::Pen *corCirculo1;
		Gdiplus::Pen *corCirculo2;
		Gdiplus::Pen *redPen;
		Gdiplus::Pen *bluePen;
		Gdiplus::Pen *corEspelho;
		*/

		Gdiplus::Graphics *grafico;

	public:
		TShMatrix(){
		   temTornado = false;
		   x = 0;
		   y = 0;
		   forcaBot = 0;
		   anguloBot = 0;
		   posicaoTelaX = 0;
		   posicaoTelaY = 0;
		   diametroX = 0;
		   diametroY = 0;
		   raio = 36;
		   count = 1;
		   tiro = 0;
		   fatorForca = 0;
		   modoMira = 2;
		   ultimoAlvo = -1;
		   tempoMenu = 5;

		   flagAutoShot1 = false;
		   flagAutoShot2 = false;

		   autoMobile = true;
		   direita = false;
		   flagEspelho = false;
		   modoGDI = false;
		   linhaVertical = false;
		   mostrarMenu = true;
		   tiroLiberado = false;
		   inicioAmostraMenu = GetTickCount();

		   nomes = new NOME_BOT[8];
		   memset(nomes, 0, 20*8);
		   memset(&opcoesTiro, 1, sizeof opcoesTiro);
		   memset(&intVars[0], 0, 20);
		   memset(&floatVars[0], 0, (sizeof floatVars[0]) * 5);

		   posicaoMenu.x = 10;
		   posicaoMenu.y = 10;
		   movendoMenu = false;

		   //whitePen    = new Gdiplus::Pen(ToGdiColor(0xFFFFFF), 1);
		   //redPen      = new Gdiplus::Pen(ToGdiColor(clBlue), 1);
		   //bluePen     = new Gdiplus::Pen(ToGdiColor(clRed), 1);
		   //corLinha1   = new Gdiplus::Pen(ToGdiColor(0), 1);
		   //corLinha2   = new Gdiplus::Pen(ToGdiColor(0), 1);
		   //corCirculo1 = new Gdiplus::Pen(ToGdiColor(0), 1);
		   //corCirculo2 = new Gdiplus::Pen(ToGdiColor(0), 1);

		   setConfiguracaoLinha(5, 0xFFFFFF, 1);   // whitePen
		   setConfiguracaoLinha(0, 0x0000FF, 1);   // redPen
		   setConfiguracaoLinha(1, 0xFF0000, 1);    // bluePen
		   setConfiguracaoLinha(2, 0x008000, 1);    // espelho
		   setConfiguracaoLinha(3, 0x800000, 1);
		   //setConfiguracaoLinha(4, 0, 1);        // corLinha2
		   //setConfiguracaoLinha(5, 0, 1);        // corCirculo1
		   //setConfiguracaoLinha(6, 0, 1);        // corCirculo2
		   //setConfiguracaoLinha(7, 0xC00000, 2); // Espelho

		   grafico = new Gdiplus::Graphics((HDC)0);

		};

		POINT camera;        // Posição da camera na tela
		POINT posicaoAlvo;   //
		POINT posicaoBot;	  // Posição do bot
		POINTFLOAT aceleracaoVento;  // Velocidade do vento
		bool modoDebug; 	  // Define modo debug indicado pela matriz
		bool renderizar; 	  // Define se deve ou não renderizar na tela
		bool ligado;          // Define se o aim está ligado
		bool semiDireita;     // Indica se está virado para a direita
		bool direita;		  // Indica se está virado para a direita
		bool autoMobile;	  // Selecionar o mobile automaticamente
		bool backShot;		  // Indica se a mira está para traz
		bool autoShot;
		bool atirando;
		bool tiroLiberado;
		bool acertarAmigos;   // Indica se deve mirar nos amigos
		bool modoGDI;
		bool linhaVertical;
		bool mostrarMenu;
		bool modoEspelho;
		bool flagAutoShot1;
		bool flagAutoShot2;
		bool movendoMenu;
		int mobile;           // Mobile selecionado
		int indiceBot;		  // Indice do bot
		int indiceMobile;     // Indice do mobile
		int semiForcaBot;	  //
		int interfaceJogo;    // Interface do GB - NEW/OLD
		int tiro;			  // Tiro selecionado - T1/T2/SS
		int anguloBot;		  // Angulo do bot
		int modoMira;		  // Modo de mira - Slice/Auto/Close/Far/Mouse/Random
		int framesPorSegundo; // Amostragem
		int forcaBot;         // Força calculada para o Bot
		int forcaVento;		  // Força do vento
		int ultimoAlvo;
		int alvoManualID;
		float gravidadeGB;
		float efeitoVento;
		TMelhorForca melhorForca;
		CFG_LINHA linhas[10]; // Configuração de cor/espessura das linhas
		TGrupoTornados tornados;
		WORD anguloVento;	  // Angulo do vento
		WORD inclinacaoBot;   // Inclinação do Bot
		HANDLE processoAlvo;  // Processo alvo
		PVOID processoBase;   // Provavelmente nao mais usado
		DWORD enderecos[20];  // Endereços na memoria
		BYTE times[8];        // Time de cada jogador
		BYTE flags[8];        //
		BYTE alvoID;		  // ID do alvo
		char tempoMenu;
		char *nomeMobile;
		char *nomeAlvo;
		DWORD inicioAmostraMenu;
		POINT posicaoMenu;
		POINT semiPosicaoBot[8]; //
		NOME_BOT *nomes;
		int intVars[5];
		float floatVars[5];
		TTiroMaya maya;
		TOpcoesTiro opcoesTiro[30];

		void __fastcall fillPoints();
		void __fastcall fillPoints(HDC dc);
        void __fastcall calculaTornado();
		void __fastcall fillArmor();
		void __fastcall fillMage();
		void __fastcall fillNak();
		void __fastcall fillTrico();
		void __fastcall fillBigFoot();
		void __fastcall fillBoomer();
		void __fastcall fillRaon();
		void __fastcall fillLightning();
		void __fastcall fillJD();
		void __fastcall fillASate();
		void __fastcall fillIce();
		void __fastcall fillTurtle();
		void __fastcall fillGrub();
		void __fastcall fillAduka();
		void __fastcall fillKalsiddon();
		void __fastcall fillJFrog();
		void __fastcall fillPhoenix();
		void __fastcall fillMaya();
		void __fastcall fillDragon();
		void __fastcall fillKnight();
		void __fastcall fillWolf();
		void __fastcall fillBlueWhale();
		void __fastcall fillTiburon();
		void __fastcall fillFrank();
		void __fastcall fillMage2();
		void __fastcall fillPrincess();
		void __fastcall fillAssassin();
		void __fastcall fillShadowWalker();
		void __fastcall fillCarrior();
		//void __fastcall fixPos(int indice);
		void __fastcall calcular(int indice, float t);
		void __fastcall desenhaLinha(int indice, bool flag = false);
		void __fastcall processaRotacao(TRotacao *rotacao);
		void __fastcall desenhaRotacao(TRotacao rotacao, Gdiplus::Pen *caneta, int x, int y, int raio, int comprimentoLinha, int extra = 0);
		void __fastcall setConfiguracaoLinha(BYTE linha, DWORD cor, BYTE espessura);
		TPointFloatD __fastcall GetAimFix(int index);
		POINT __fastcall PosicaoBot();
		POINT __fastcall GetPosicaoDe(int index);
		POINT __fastcall PosicaoAlvo();
		double __fastcall GetDistance(POINT ponto1, POINT ponto2);
		int __fastcall GetBotAngle();
		POINT __fastcall GetCameraPos();
		void __fastcall ProximoMobile();
		void __fastcall MobileAnterior();
		void __fastcall ProximaMira();
		void __fastcall MiraAnterior();
		bool __fastcall MesmoTime(int indice);
		bool __fastcall AlguemNoLugar(int indice);
		void __fastcall AlvoAnterior();
		void __fastcall ProximoAlvo();
		int __fastcall IndexToMobile(int valor);
};

int __fastcall MobileToIndex(int valor);
extern int magePoints[40];


//---------------------------------------------------------------------------
#endif
