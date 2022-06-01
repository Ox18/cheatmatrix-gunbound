//---------------------------------------------------------------------------

#pragma hdrstop

#include "BotUnit.h"
#include "PluginUtils.h"
#include "PixelUnit.h"
#include "Unit2.h"
#include <forms.hpp>
#include <math.h>
#include <windows.h>
#include <controls.hpp>
#include <graphics.hpp>
#include "extras.h"
#include "Sharing.h"
#include "mathUtils.h"
#include "SMDrawUnit.h"

#define USEINJECT
//#define debug1
//#define debug2
TSMDraw *DrawTd;
TTelaStatus status(1000);
TFileCB *arquivoCM = NULL;

TEfeitoVento efeitoFixGlobal;

BYTE valor = 0;

float efeitoAx = 0;
float efeitoAy = 0;

float gravidadeA = 0;

/**
 * Pega o nome do móbile
 **/
AnsiString getMobileName(int valor) {
    switch(valor) {
        case 0: return "Armor";
        case 1: return "Mage";
        case 2: return "Nak";
        case 3: return "Trico";
        case 4: return "Big Foot";
        case 5: return "Boomer";
        case 6: return "Raon";
        case 7: return "Lightning";
        case 8: return "J.D";
        case 9: return "A.Sate";
        case 11: return "Turtle";
        case 10: return "Ice";
        case 12: return "Grub";
        case 13: return "Aduka";
        case 14: return "Kasilddon";
        case 15: return "J.Frog";
        case 16: case 125: return "Dragon";
		case 17: case 209: return "Knight";
		case 19: return "Phoenix";
		case 20: return "Maya";
		case 21: return "Wolf";
		case 22: return "Tiburon";
		case 23: return "BlueWhale";
		case 24: return "Frank";
        case 255: return "Random";
        default: return "Random";
    }
}

/**
 *  Constante para calculo do angulo de inclinação de cada bot
 */
TPointFloatD __fastcall TBot::GetAimFix(int index) {
	switch(index) {
		case 0: return TPointFloatD(46, 30, 42, 30); break; //Armor
		case 1: return (direita?TPointFloatD(59, 36):TPointFloatD(53, 38)); break; //Mage
		case 2: return TPointFloatD(140, 33); break;//Nak
		case 3: return TPointFloatD(58, 37); break; //Trico
		case 4: return TPointFloatD(56, 42); break; //Big Foot
		case 5: return TPointFloatD(75, 38); break; //Boomer
		case 6: return TPointFloatD(64, 34); break; //Raon ok
		case 7: return TPointFloatD(60, 36); break; //Lightning
		case 8: return TPointFloatD(60, 40); break; //J.D
		case 9: return TPointFloatD(42, 35); break; //A.Sate
        case 10: {
			//testPos = eng;
            int s = GetShotMode(interfaceJogo);
            if(s == 0)
				return TPointFloatD(54, 50);  //Ice
            else
				return TPointFloatD(40, 45);
        } break;
		case 11: return TPointFloatD(59, 29); break;  //Turtle ok
		case 12: return TPointFloatD(55, 38); break;  //Grub
		case 13: return TPointFloatD(136, 29); break; //aduka
		case 14: return TPointFloatD(56, 42); break;  //Kalsiddon
		case 15: return TPointFloatD(46, 30); break;  //JFrog
		case 16: case 125: return TPointFloatD(53, 47); break; //return TPointFloatD(52-BotInfos->maya.faseBola1, 46-BotInfos->maya.faseBola2); break; //Dragon
		case 17: case 209: return TPointFloatD(45, 35); break; //Knight
		case 18: return (direita?TPointFloatD(115, 42):TPointFloatD(109, 42)); break; //Phoenix
		case 19: return TPointFloatD(63, 35); break; //Maya
		case 20: {  //Wolf
            int s = GetShotMode(interfaceJogo);
			if(s == 2)
				return TPointFloatD(68, 40);
            else
				return TPointFloatD(63, 35);
		} break;
		case 21: return TPointFloatD(60, 36); break; //Tiburon
		case 22: return TPointFloatD(63, 36); break; //BlueWhale
		case 23: return TPointFloatD(41, 31); break; //Frank
		default: break;
    }
    return TPointFloatD(0, 0);
}

/**
* 	(ShotTime) Pega o tempo de estouro de ss ou tiro com mudaça de frequencia na ação
*/
/*
void TBot::GetTempoSS(int index) {
	return;
    switch(index) {
		case 0: {
			// Armor - SS
			TSSTime tmp;
			// Zera variavel e atribui de uma vez para evitar espaço de tempo em 0
			memset(&tmp, 0x00, sizeof tmp);
			tmp.t1 = 81;
			BotInfos->SSTime = tmp;
			BotInfos->ChangeColor = true;
		} break;
		case 3: {
			// Trico - Translação dos repolhos
			TSSTime tmp;
			memset(&tmp, 0x00, sizeof tmp);
			// Trico tem uma velocidade de translação diferente para cada lado
			if(Direita)
                tmp.t1 = 220;
            else
				tmp.t1 = 76;
			BotInfos->SSTime = tmp;
			BotInfos->ChangeColor = false;
		} break;
		case 5: {
			/// Boomer - SS
            TSSTime tmp;
			memset(&tmp, 0x00, sizeof tmp);
			tmp.t1 = 63;
			BotInfos->SSTime = tmp;
			BotInfos->ChangeColor = true;
        } break;
		case 11: {
			// Turtle - S2 e SS
            int s = GetShotMode();
			int f = 0;
            if(s == 0)
                f = 0;
            else if(s == 1)
				f = 119;
            else if(s == 2)
				f = 98;
			TSSTime tmp;
            memset(&tmp, 0x00, sizeof tmp);
            tmp.t1 = f;
			BotInfos->SSTime = tmp;
			BotInfos->ChangeColor = true;
        } break;
		case 14: {
			// Kasildon - S2 e SS
			TSSTime tmp;
			memset(&tmp, 0x00, sizeof tmp);
            tmp.t1 = 616;
            tmp.t2 = 1074;
			BotInfos->SSTime = tmp;
			BotInfos->ChangeColor = true;
        } break;
		default: {
			// Outros = 0 = Não faz nada
            TSSTime tmp;
            memset(&tmp, 0x00, sizeof tmp);
			BotInfos->SSTime = tmp;
			BotInfos->ChangeColor = false;
        } break;
    }
}
*/


TEfeitoVento __fastcall getFixVento(int indiceMobile){
	int i = (floor(BotInfos->anguloVento / 10) + 1);
	int j = (((i - 1) + quantidadeMobiles) % quantidadeMobiles);
	double difP = (effectFixes[indiceMobile][i].efeito - effectFixes[indiceMobile][j].efeito) / 10;
	double difG = (effectFixes[indiceMobile][i].gravidade - effectFixes[indiceMobile][j].gravidade) / 10;
	double nivel = BotInfos->anguloVento % 10;
	TEfeitoVento resultado;
	resultado.efeito = effectFixes[indiceMobile][j].efeito + (nivel * difP);
	resultado.gravidade = effectFixes[indiceMobile][j].gravidade + (nivel * difG);
	return resultado;
}


/*************************************************************************
 *	Prepara o aim para agir, pegando as forças, efeitos, etc...
 *************************************************************************/
void __fastcall TBot::PrepareAim() {

	if(!memoriaLida){
		return;
	}

	__try{
		BotInfos->autoMobile = autoMobile;
		selfIndex = BotInfos->indiceBot;

		bool semiDireita = BotInfos->semiDireita; //(i == 1);
		semiDireita = (backShot ? !semiDireita : semiDireita);
		semiDireita = ((mobile == 2 || mobile == 13) ? !semiDireita : semiDireita);
		direita = semiDireita;
		BotInfos->direita = direita;

		if(autoMobile)
			mobile = BotInfos->mobile;
		else
            BotInfos->mobile = mobile;

		indiceMobile = MobileToIndex(mobile);

		// Pega o tipo de tiro (S1, S2, SS) por pixel
		tiro = GetShotMode(interfaceJogo);
		BotInfos->tiro = tiro;

		interfaceJogo = GetInterfaceGB();
		BotInfos->interfaceJogo = interfaceJogo;

		GetBotPos();
		BotInfos->posicaoBot = bot;
		BotInfos->posicaoBotC = bot2;

		GetTargetPos();

		TEfeitoVento efeito = effects[indiceMobile];
		TEfeitoVento efeitoFix;// = getFixVento(indiceMobile);
		efeitoFixGlobal = efeitoFix;
		efeito.efeito += efeitoFix.efeito;
		efeito.gravidade += efeitoFix.gravidade;

		efeitoVento = efeito;
		BotInfos->gravidade = efeitoVento.gravidade + BotInfos->maya.velocidadeAngular;
		BotInfos->efeitoVento = efeitoVento.efeito + BotInfos->maya.raio;

		// Pega angulo do bot
		GetBotAngle();
		BotInfos->anguloBot = estadoBot.angulo; // - BotInfos->maya.faseBola1;

		// Se for slice pega a força de acordo com a barra
		if(modoMira == AM_Slice) {
			int k = (interfaceJogo == 1 ? 389 : 241);
			int power = 0;
			DWORD address = 0;
			estadoBot.forca = (BotInfos->semiForcaBot - k);
			BotInfos->forcaBot = estadoBot.forca;
		}else{
			estadoBot.forca = BotInfos->melhorForca.forca;
			BotInfos->forcaBot = estadoBot.forca;
		}

		BotInfos->posicaoAlvo = alvo;
		BotInfos->backShot = backShot;

		BotInfos->modoMira = modoMira;

		BotInfos->framesPorSegundo = 30;
		BotInfos->alvoID = MatrizInfo->TargetId;

		BotInfos->processoAlvo = MatrizInfo->TargetHandle;
		BotInfos->processoBase = MatrizInfo->TargetBase;

		camera = BotInfos->camera;

		/*HDC dc = CreateDC("DISPLAY", "", "", NULL);
		if(dc == 0) return;

		__try{
			char teste[100];
			int n = sprintf(teste, "%d   %d   %d",selfIndex, BotInfos->semiPosicaoBot[selfIndex].x , BotInfos->semiPosicaoBot[selfIndex].y);
			TextOut(dc, 10, 90, teste, n);
		}__finally{
			DeleteDC(dc);
		}     */
	}__except(1){
        //PRINTDLG;
	}
}


/*************************************************************************
 * Pega a posição bot
 **************************************************************************/
double v0 = 0.5;

TPoint __fastcall TBot::GetBotPos() {
	TBotPos posicao;
	TBotPos posicao2;
	TPointFloatD aimFix = GetAimFix(indiceMobile);

	int inclinacaoInicial1 = aimFix.x1 + BotInfos->maya.faseBola1;
	int raioVirtual1 = aimFix.y1 + BotInfos->maya.faseBola2;

	int inclinacaoInicial2 = aimFix.x2 + BotInfos->maya.faseBola1;
	int raioVirtual2 = aimFix.y2 + BotInfos->maya.faseBola2;

	posicao.x = BotInfos->semiPosicaoBot[selfIndex].x;
	posicao.y = BotInfos->semiPosicaoBot[selfIndex].y;

	posicao2.x = BotInfos->semiPosicaoBot[selfIndex].x;
	posicao2.y = BotInfos->semiPosicaoBot[selfIndex].y;

	posicao.inclinacao = BotInfos->inclinacaoBot;
	posicao2.inclinacao = BotInfos->inclinacaoBot;

	BOOL tdir = ( (indiceMobile == 2 || indiceMobile == 13 )? !direita : direita );

    if(backShot)
        tdir = !tdir;
    
	int angle1, angle2;
    if (tdir) {
		angle1 = (posicao.inclinacao + inclinacaoInicial1);
		angle2 = (posicao2.inclinacao + inclinacaoInicial2);
    }
    else {
		angle1 = (posicao.inclinacao + (180 - inclinacaoInicial1));
		angle2 = (posicao2.inclinacao + (180 - inclinacaoInicial2));
    }
    
	posicao.x += (int)(cos(intToRadian(angle1)) * (v0 + raioVirtual1));
	posicao.y -= (int)(sin(intToRadian(angle1)) * (v0 + raioVirtual1)) - 20;

	posicao2.x += (int)(cos(intToRadian(angle2)) * (v0 + raioVirtual2));
	posicao2.y -= (int)(sin(intToRadian(angle2)) * (v0 + raioVirtual2)) - 20;

	bot.x = posicao.x;
	bot.y = posicao.y;

	bot2.x = posicao2.x;
	bot2.y = posicao2.y;

	return bot;
}

/*************************************************************************
 * Pega angulo do Bot
 **************************************************************************/
int __fastcall TBot::GetBotAngle() {
	int k = 0;
    if(interfaceJogo == 1)
        k = getAngleOld();
    else
		k = getAngleNew();

    //Application->ProcessMessages();
    if(direita)
        k += 360;
    else
        k = 180 - k;
    estadoBot.angulo = (k % 360);
    
    return (k % 360);
}

/*************************************************************************
 * Verifica se esta pausado
 **************************************************************************/
BOOL __fastcall TBot::IsPaused() {
	return (BotInfos->flags[selfIndex] != 4);
}

/*************************************************************************
 * Pega a posição alvo
 **************************************************************************/

 void __fastcall TBot::GetTargetPos() {
    switch(modoMira) {
		case AM_Auto: {
			alvoSelecionado = GetPositionOf(targetIndex);
			alvo = alvoSelecionado;
		} break;

		/** Pega pela posição do mouse **/
		case AM_Mouse: {
			int estado = GetKeyState(VK_CONTROL);

			/** Se estiver pressionado CTRL, mantem a mira fixa no local **/
			if(estado <= 1 && estado >= -1){
				alvo.x = Mouse->CursorPos.x+camera.x;
				alvo.y = Mouse->CursorPos.y+camera.y;
				targetIndex = -1;
			}
        } break;
        
        case AM_Random: {
            randomize();
            int j = (random(4)+1);
            int i = 0, k = 0;
            
            while( j > 0 ) {
                Sleep(1);
                k++;
                
                if(SomeOneInPlace(i)) {
                    bool flag = isFromSameTeam(i);
                    if(acertarAmigos) flag = false;
                    if(!flag) {
                        j--;
                        if(j == 0)
                            break;
                    }
                }
                
                i = ((i > 7) ? 0 : (i+1) );
                if(k > 32) break;
            }
            
            targetIndex = i;
            alvo = GetPositionOf(i);
            
        } break;
        
		/** Pega pela posição do slice **/
        case AM_Slice: {
			alvo.x = 0;
            alvo.y = 0;
            targetIndex = -1;
        } break;
        
		/** Pega pelo mais perto **/
        case AM_Close: {
            TDistanceComp menor;
            menor.distancia = 9999;
            menor.index = 0;
            
            for(int i = 0; i < 8; i++) {
                Sleep(1);
                if(SomeOneInPlace(i)) {
                    BOOL flag = isFromSameTeam(i);
                    if(acertarAmigos) flag = false;
                    if(!flag) {
                        TPoint tmpTarget = GetPositionOf(i);
                        double dist = GetDistance(tmpTarget, bot);
                        if(dist < menor.distancia) {
                            menor.index = i;
                            menor.distancia = dist;
                        }
                    }
                }
            }
            targetIndex = menor.index;
            alvo = GetPositionOf(menor.index);
            
        }  break;
        
		/** Pega pelo mais longe **/
		case AM_Far: {
            TDistanceComp maior;
            maior.distancia = 0;
            maior.index = 0;
            
            for(int i = 0; i < 8; i++) {
                Sleep(1);
                if(SomeOneInPlace(i)) {
                    BOOL flag = isFromSameTeam(i);
                    if(acertarAmigos) flag = false;
                    if(!flag) {
                        TPoint tmpTarget = GetPositionOf(i);
                        double dist = GetDistance(tmpTarget, bot);
                        if(dist > maior.distancia) {
                            maior.index = i;
                            maior.distancia = dist;
                        }
                    }
                }
            }
            targetIndex = maior.index;
            alvo = GetPositionOf(maior.index);
        } break;
        
        default: break;
    }
    
    //target.y += 6;
}

/*************************************************************************
 * Pega proximo alvo
 **************************************************************************/
int __fastcall TBot::GetNextTarget() {
    for(int i = targetIndex; i < 8; i++) {
        //Application->ProcessMessages();
        if(i == targetIndex || i == selfIndex) continue;
        
        BOOL flag = isFromSameTeam(i);
		if(acertarAmigos) flag = false;
        if(!flag && SomeOneInPlace(i))
            return i;
    }
    
    //Se continua aqui recomeça do 0
    for(int i = 0; i < 8; i++) {
        //Application->ProcessMessages();
        if(i == targetIndex || i == selfIndex) continue;
        BOOL flag = isFromSameTeam(i);
        if(acertarAmigos) flag = false;
        if(!flag && SomeOneInPlace(i))
            return i;
    }
    
    //Não achou nada? oÕ
    return targetIndex;
}

/*************************************************************************
 * Pega alvo anterior
 **************************************************************************/
int __fastcall TBot::GetPrevTarget() {
    for(int i = targetIndex; i > 0; i--) {
        //Application->ProcessMessages();
        if(i == targetIndex || i == selfIndex) continue;
        BOOL flag = isFromSameTeam(i);
		if(acertarAmigos) flag = false;
        if(!flag && SomeOneInPlace(i))
            return i;
    }
    
    //Se continua aqui recomeça do 0
    for(int i = 8; i < 0; i++) {
        //Application->ProcessMessages();
        if(i == targetIndex || i == selfIndex) continue;
        BOOL flag = isFromSameTeam(i);
        if(acertarAmigos) flag = false;
        if(!flag && SomeOneInPlace(i))
            return i;
    }
    
    //Não achou nada? oÕ
    return targetIndex;
}

/*************************************************************************
 *  Proximo mobile
 **************************************************************************/
void __fastcall TBot::GetNextMobile() {
    autoMobile = false;
    int j = indiceMobile;
    j++;
	if(j > 23)
        j = 0;
    mobile = IndexToMobile(j);
}

/*************************************************************************
 *  Mobile Anterior
 **************************************************************************/
void __fastcall TBot::GetPrevMobile() {
    autoMobile = false;
    int j = indiceMobile;
    j--;
    if(j < 0)
		j = 23;
    mobile = IndexToMobile(j);
}

/*************************************************************************
 * Pega o Mobile de acordo com o index do array de Effects
 **************************************************************************/
int __fastcall TBot::IndexToMobile(int valor) {
    switch(valor) {
        case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
        case 8: case 9: case 10: case 11: case 12: case 13:
		case 14: case 15: case 16: case 17: return valor; break;
		case 18: case 19: case 20: case 21: case 22: case 23: return valor+1; break;
        default: return 0; break;
    }
    return 0;
}

/*************************************************************************
 * Verifica o time do jogador cujo index for informado
 **************************************************************************/
boolean __fastcall TBot::isFromSameTeam(int index) {
	return ((index == selfIndex) || (BotInfos->times[index] == BotInfos->times[selfIndex]));
}

/*************************************************************************/
/* Verifica se há jogador no Lugar
 * /*************************************************************************/
boolean __fastcall TBot::SomeOneInPlace(int index) {
	index &= 7;
    return (BotInfos->flags[index] != 0);
}

/*************************************************************************/
/* Pega posição X e Y do jogador do index especificado
 * /*************************************************************************/
TPoint __fastcall TBot::GetPositionOf(int index) {
	index &= 7;
	TPoint tmpTarget;
	tmpTarget.x = BotInfos->semiPosicaoBot[index].x;
    tmpTarget.y = BotInfos->semiPosicaoBot[index].y+15;//
    return tmpTarget;
}

/*************************************************************************/
/* Muda o alvo
 * /*************************************************************************/
void __fastcall TBot::SetTarget(int index) {
    targetIndex = index;
}

/*************************************************************************/
/* Pega a distancia entre dois pontos
 * /*************************************************************************/
double __fastcall TBot::GetDistance(TPoint ponto1, TPoint ponto2) {
    int px = (ponto1.x - ponto2.x);
    px *= px;
    int py = (ponto1.y - ponto2.y);
    py *= py;
    
    
    return Sqrt( modulo(px + py) );
}

/*************************************************************************/
/* Pega cor de linha extra para desenhar o Hook do tiro
 * /*************************************************************************/
DWORD __fastcall TBot::getFreeColor(DWORD free1, DWORD free2) {
    return ( corLinha2 == free1 ? free2 : free1 );
}


/*************************************************************************/
/* Inteiro para radiano
 * /*************************************************************************/
/*double TBot::intToRadian(int valor)
 * {
 * return valor*pi/180;
 * } */


//---------------------------------------------------------------------------

#pragma package(smart_init)


