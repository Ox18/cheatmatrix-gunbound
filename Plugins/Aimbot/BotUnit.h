#define USEINJECT

#ifndef BotUnitH
#define BotUnitH

#include "selfdescription.h"
#include "PluginClasses.h"
#include "PluginCommon.h"
#include "PluginUtils.h"
#include "status.h"
#include "classesCB.h"
#include "common.h"

extern bool DebugFlag;

typedef struct _BotPos{
	WORD x;
	WORD y;
	WORD inclinacao;
} TBotPos;

typedef struct _DistanceComp {
    int index;
    double distancia;
} TDistanceComp;

typedef struct _AimState {
	int angulo;
    int forca;
}TAimState;

enum TModoMira {
    AM_Close,
    AM_Far,
    AM_Slice,
    AM_Mouse,
    AM_Random,
	AM_Auto//,
	//AM_Personalizado
};

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

typedef struct __pointFloatE{
    __pointFloatE(float _x, float _y){
		x = _x;
		y = _y;
	}

	__pointFloatE(){
    }

	float x;
	float y;
} TPointFloatE;

/*typedef struct _sstime
 * {
 * float t1;
 * float t2;
 * float t3;
 * float t4;
 * float t5;
 * } TSSTime; */

//1800
class TBot {
    
public:
 	/** Construtor do objeto **/
    TBot(){
		autoMobile = true;
        modoMira = AM_Far;
		corLinha1 = 0xFF0000;
        corLinha2 = 0x0000FF;
        ligado = false;
        autoEffect = true;
        pausado = false;
		backShot = false;
    };
	int quantidadePontos;
	int ouro;
	int indiceAlvoSelecionado;
	int tiro;
	bool direita;
	bool backShot;
	bool autoMobile;
	bool acertarAmigos;
	bool ligado;
    bool pausado;
	bool autoEffect;
	bool trajetoriaEspecial;
	DWORD corLinha1;
	DWORD corLinha2;
	DWORD corCirculo1;
	DWORD corCirculo2;
	BYTE mobile;
	BYTE indiceMobile;
	BYTE selfIndex;
	BYTE targetIndex;
	BYTE interfaceJogo;
    TAimState estadoBot;
    TAimState estadoVento;
    TEfeitoVento efeitoVento;
    TPoint alvo;
    TPoint bot;
	TPoint camera;
	TPoint alvoSelecionado;
	TModoMira modoMira;
	char nomeAlvo[15];
    char nomeMobile[30];
	TGrupoTornados tornados;
	TPoint bot2;
public:
	TPointFloat __fastcall GetEffect();
	BOOL __fastcall IsPaused();
	int __fastcall GetShot();
	void __fastcall PrepareAim();
	int __fastcall GetIndex();
	TPoint __fastcall GetCameraPos();
	TPoint __fastcall GetBotPos();
	int __fastcall GetWindAngle();
	int __fastcall GetWindPower();
	int __fastcall GetBotAngle();
	void __fastcall GetTargetPos();
	int __fastcall GetNextTarget();
	int __fastcall GetPrevTarget();
	int __fastcall GetMobile();
	void __fastcall GetNextMobile();
	void __fastcall GetPrevMobile();
	int __fastcall IndexToMobile(int valor);
	boolean __fastcall isFromSameTeam(int index);
	boolean __fastcall SomeOneInPlace(int index);
	TPoint __fastcall GetPositionOf(int index);
	bool __fastcall FaceToRight();
	void __fastcall SetTarget(int index);
	double __fastcall GetDistance(TPoint ponto1, TPoint ponto2);
	DWORD __fastcall getFreeColor(DWORD free1, DWORD free2);
    //double IntToRadian(int valor);
	void __fastcall FillPoints(HDC dc);
	void __fastcall GetPower();
	void __fastcall GetSlicePower();
	void __fastcall GetTornadoLine();
	void __fastcall GetTempoSS(int index);
	TPointFloatD __fastcall GetAimFix(int index);
    
};

extern float efeitoAx;
extern float efeitoAy;
extern float gravidadeA;
extern TTelaStatus status;
extern TFileCB *arquivoCM;

extern TEfeitoVento efeitoFixGlobal;
/*extern float snx;
 * extern float sny;
 * extern double eng;
 * extern double env;
 * extern float enx;
 * extern float eny;
 *
 * extern int exangle;
 * extern int ShotIndex; */
//---------------------------------------------------------------------------
#endif
