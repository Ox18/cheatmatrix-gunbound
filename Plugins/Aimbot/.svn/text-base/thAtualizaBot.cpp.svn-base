//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "thAtualizaBot.h"
#include "common.h"
#include "SMDrawUnit.h"
#include "PixelUnit.h"
#include "sharing.h"
#pragma package(smart_init)


__fastcall thAtualizaBot::thAtualizaBot(bool CreateSuspended)
	: TThread(CreateSuspended)
{
}

void __fastcall thAtualizaBot::AtualizarDados(){

}

//---------------------------------------------------------------------------
void __fastcall thAtualizaBot::Execute()
{
   bool executando = false;
   while(true){
       Sleep(20);

	   if(executando){
			continue;
	   }else{
			executando = true;
	   }

	   __try{

			if(!memoriaLida){
				continue;
			}

			__try{
				BotInfos->autoMobile = ShotMatrix->autoMobile;

				if(ShotMatrix->autoMobile)
					ShotMatrix->mobile = BotInfos->mobile;

				//ShotMatrix->indiceMobile = MobileToIndex(ShotMatrix->mobile);

				ShotMatrix->interfaceJogo = GetInterfaceGB();
				BotInfos->interfaceJogo = ShotMatrix->interfaceJogo;

				// Pega o tipo de tiro (S1, S2, SS) por pixel
				ShotMatrix->tiro = GetShotMode(ShotMatrix->interfaceJogo);
				BotInfos->tiro = ShotMatrix->tiro;

				//ShotMatrix->GetBotPos();
				BotInfos->posicaoBot = ShotMatrix->bot;

				//ShotMatrix->GetTargetPos();

				ShotMatrix->efeitoVento = effects[ShotMatrix->indiceMobile];
				BotInfos->gravidade = ShotMatrix->efeitoVento.gravidade;
				BotInfos->efeitoVento = ShotMatrix->efeitoVento.efeito;

				bool semiDireita = BotInfos->semiDireita; //(i == 1);
				semiDireita = (ShotMatrix->backShot ? !semiDireita : semiDireita);
				semiDireita = ((ShotMatrix->mobile == 2 || ShotMatrix->mobile == 13) ? !semiDireita : semiDireita);
				ShotMatrix->direita = semiDireita;
				BotInfos->direita = ShotMatrix->direita;

				//Sleep(10);
				////Application->ProcessMessages();

				// Pega angulo do bot
				//ShotMatrix->GetBotAngle();
				BotInfos->anguloBot = ShotMatrix->estadoBot.angulo;

				// Se for slice pega a força de acordo com a barra
				if(ShotMatrix->modoMira == AM_Slice) {
					int k = (ShotMatrix->interfaceJogo == 1 ? 389 : 241);
					int power = 0;
					DWORD address = 0;
					ShotMatrix->estadoBot.forca = (BotInfos->semiForcaBot - k);
					BotInfos->forcaBot = ShotMatrix->estadoBot.forca;
				}

				//Sleep(10);
				//Application->ProcessMessages();

				BotInfos->posicaoAlvo = ShotMatrix->alvo;
				BotInfos->backShot = ShotMatrix->backShot;

				BotInfos->modoMira = ShotMatrix->modoMira;

				BotInfos->framesPorSegundo = 30;
				BotInfos->alvoID = MatrizInfo->TargetId;

				//Sleep(10);
				//Application->ProcessMessages();

				BotInfos->processoAlvo = MatrizInfo->TargetHandle;
				BotInfos->processoBase = MatrizInfo->TargetBase;

				ShotMatrix->camera = BotInfos->camera;
			}__except(1){
				//PRINTDLG;
				continue;
			}



	   }__finally{
           executando = false;
	   }
	   executando = false;
   }
}
//---------------------------------------------------------------------------
