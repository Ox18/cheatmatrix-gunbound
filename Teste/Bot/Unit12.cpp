//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit12.h"
#include "mobilesB.h"
#include "tiposBase.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Spin"
#pragma resource "*.dfm"
TForm12 *Form12;
TShMatrix *aimbot;

GdiplusStartupInput gdiplusStartupInput;
ULONG_PTR gdiplusToken;

//---------------------------------------------------------------------------
__fastcall TForm12::TForm12(TComponent* Owner)
	: TForm(Owner)
{
}

bool desenhando = false;

char* getMobileName(int valor) {
	//VMProtectBegin("nomes");
	switch(valor) {
	case 0:
		return "Armor";
	case 1:
		return "Mage";
	case 2:
		return "Nak";
	case 3:
		return "Trico";
	case 4:
		return "Big Foot";
	case 5:
		return "Boomer";
	case 6:
		return "Raon";
	case 7:
		return "Lightning";
	case 8:
		return "J.D";
	case 9:
		return "A.Sate";
	case 11:
		return "Turtle";
	case 10:
		return "Ice";
	case 12:
		return "Grub";
	case 13:
		return "Aduka";
	case 14:
		return "Kasilddon";
	case 15:
		return "J.Frog";
	case 16:
	case 125:
		return "Dragon";
	case 17:
	case 209:
		return "Knight";
	case 19:
		return "Phoenix";
	case 20:
		return "Maya";
	case 21:
		return "Wolf";
	case 22:
		return "Tiburon";
	case 23:
		return "BlueWhale";
	case 24:
		return "Frank";
	case 255:
		return "Random";
	default:
		return "Random";
	}
	return "";
	//VMProtectEnd();
}

void __fastcall TForm12::AtualizarValores() {
	//VMProtectBegin("update");
	// debugar("Atualizando valores no subplugin de %s",Hack_Name);
	__try {
		//
		// Indice do Jogador
		aimbot->indiceBot = 0;

		//
		// Força do Vento
		aimbot->forcaVento = trackFV->Position;
		aimbot->anguloVento = trackAV->Position;
		aimbot->mobile = comboMobiles->ItemIndex;
		aimbot->indiceMobile = MobileToIndex(aimbot->mobile);
		aimbot->nomeMobile = getMobileName(aimbot->mobile);
		if (aimbot->alvoID != aimbot->ultimoAlvo) {
			aimbot->ultimoAlvo = aimbot->alvoID;
			aimbot->nomeAlvo = "Fulano";
		}

		aimbot->inclinacaoBot = 0;
		for (int i = 0; i < 8; i++) {
			aimbot->semiPosicaoBot[i].x = Shape1->Left;
			aimbot->semiPosicaoBot[i].y = Shape1->Top;
		}


		char indexInGame = 0;
		aimbot->renderizar = true;
		aimbot->semiDireita = Shape1->Left < (Width / 2);
		aimbot->semiForcaBot = trackFB->Position;
		POINT p;
		p.x = 200;
		aimbot->camera = p;
	}
	__except (1) {
	}

	aimbot->modoMira = AM_Slice;
	aimbot->backShot = false;
	aimbot->modoDebug = false;

	__try {
		if (aimbot->mostrarMenu && aimbot->tempoMenu != 0 && (GetTickCount() - aimbot->inicioAmostraMenu) > aimbot->tempoMenu * 1000)
			aimbot->mostrarMenu = false;

		if (aimbot->movendoMenu) {
			GetCursorPos(&aimbot->posicaoMenu);
		}
		if (aimbot->ligado) {
			bool valorDireita = aimbot->semiDireita; // (i == 1);
			valorDireita = (aimbot->backShot ? !valorDireita : valorDireita);
			valorDireita = ((aimbot->mobile == 2 || aimbot->mobile == 13) ? !valorDireita : valorDireita);
			aimbot->direita = valorDireita;
			// if(mobileAnterior != aimbot->mobile){
			// mobileAnterior = aimbot->mobile;
			// debugar("mobile: %d", aimbot->mobile);
			// }

			// Pega o tipo de tiro (S1, S2, SS) por pixel
			aimbot->tiro = trackTiro->Position;
			// debugar("Tiro: %d", aimbot->tiro);
			aimbot->interfaceJogo = 1;
			aimbot->posicaoBot = aimbot->semiPosicaoBot[0];
			aimbot->posicaoAlvo = aimbot->PosicaoAlvo();

			TEfeitoVento efeito = effects[aimbot->indiceMobile];
			// TEfeitoVento efeitoFix; // = getFixVento(indiceMobile);
			// efeito.efeito += efeitoFix.efeito;
			// efeito.gravidade += efeitoFix.gravidade;
			// efeitoVento = efeito;
			#ifdef _DEBUG
			aimbot->gravidadeGB = efeito.gravidade;// +  ((aimbot->modoDebug)?aimbot->maya.faseBola2:0.0f);
			aimbot->efeitoVento = efeito.efeito;// + ((aimbot->modoDebug)?aimbot->maya.faseBola1:0.0f);
			#else
			aimbot->gravidadeGB = efeito.gravidade;
			aimbot->efeitoVento = efeito.efeito;
			#endif
			//debugar("%d - %d - %f - %f - %f",aimbot->modoDebug, aimbot->indiceMobile, efeito.gravidade, aimbot->gravidadeGB, aimbot->efeitoVento);
			////if(efeitoAnterior != efeito.efeito){
			// efeitoAnterior = efeito.efeito;
			// debugar("efeito: %f / %f", efeitoAnterior, aimbot->gravidadeGB);
			// debugar("gravidade: %f / %f", efeito.gravidade, aimbot->efeitoVento);
			// }

			// Pega angulo do bot
			aimbot->anguloBot = trackAB->Position;
			// Se for slice pega a força de acordo com a barra
			if (aimbot->modoMira == AM_Slice) {
				int k = 0;
				int power = 0;
				DWORD address = 0;
				// aimbot->estadoBot.forca = (aimbot->semiForcaBot - k);
				aimbot->forcaBot = (aimbot->semiForcaBot - k);

				// if(forcaAnterior != aimbot->forcaBot){
				// forcaAnterior = aimbot->forcaBot;
				// debugar("forca: %d / %d", aimbot->semiForcaBot, aimbot->forcaBot);
				// }
			}
			else {

				aimbot->forcaBot = aimbot->melhorForca.forca;
			}
			// HDC dc = CreateDC("DISPLAY", "", "", NULL);
			// if(dc == 0) return;
			//
			// __try{
			// char teste[100];
			// int n = sprintf(teste, "%d   %d   %d",selfIndex, BotInfos->semiPosicaoBot[selfIndex].x , BotInfos->semiPosicaoBot[selfIndex].y);
			// TextOut(dc, 10, 90, teste, n);
			// }__finally{
			// DeleteDC(dc);
			// }
		}
	}
	__except (1) {
		debugar("Erro no processamento! [2]");
	}
	//VMProtectEnd();
}

void __stdcall TForm12::Desenhar(HDC dc, char tipoDesenho) {
	if (!desenhando)
		desenhando = true;
	else
		return;

	__try {
		/*if (tipoDesenho == TD_DDRAW && (aimbot->mostrarMenu || aimbot->movendoMenu) ) {
			DesenharStatus(dc);
		}    */

		if ((bool)tipoDesenho == aimbot->modoGDI) {
			__try {
				if (aimbot->renderizar) {
					aimbot->camera.x = -40;
					aimbot->camera.y = 0; // = aimbot->GetCameraPos();
					if (dc == 0)
						aimbot->fillPoints(Left,Top);
					else
						aimbot->fillPoints(dc,Left-20,Top+30+(Shape1->Height/2));
				}
			}__except (1) {
				debugar("Erro 0148");
			}
		}
	}
	__finally {
		desenhando = false;
	}
}

//---------------------------------------------------------------------------
void __fastcall TForm12::FormCreate(TObject *Sender)
{
	Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
	aimbot = new TShMatrix();
}
//---------------------------------------------------------------------------

void __fastcall TForm12::Timer2Timer(TObject *Sender)
{
	AtualizarValores();
	valorFBot->Caption = IntToStr(trackFB->Position);
	valorABot->Caption = IntToStr(trackAB->Position);
	valorFVento->Caption = IntToStr(trackFV->Position);
	valorAVento->Caption = IntToStr(trackAV->Position);
	valorFator->Caption = FloatToStr((float)SpinEdit1->Value / 1000);
	aimbot->tickP = ((float)SpinEdit1->Value / 1000);
}
//---------------------------------------------------------------------------

void __fastcall TForm12::FormDestroy(TObject *Sender)
{
	Gdiplus::GdiplusShutdown(gdiplusToken);
}
//---------------------------------------------------------------------------

void __fastcall TForm12::Timer1Timer(TObject *Sender)
{
	HDC dc = GetDC(0);
	if(dc){
		__try{
			Desenhar(dc, TD_DDRAW);
		}__finally{
			ReleaseDC(0,dc);
        }

	}
}
//---------------------------------------------------------------------------

