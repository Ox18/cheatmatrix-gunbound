//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <math.h>
#include "Threads.h"
#include "Unit2.h"
#include "tipos.h"
#include "Sharing.h"
#pragma package(smart_init)

//---------------------------------------------------------------------------

//   Important: Methods and properties of objects in VCL can only be
//   used in a method called using Synchronize, for example:
//
//      Synchronize(&UpdateCaption);
//
//   where UpdateCaption could look like:
//
//      void __fastcall AimThread::UpdateCaption()
//      {
//        Form1->Caption = "Updated in a thread";
//      }
//---------------------------------------------------------------------------

__fastcall AimThread::AimThread(bool CreateSuspended)
	: TThread(CreateSuspended)
{
	executando = false;
}

double __fastcall Modulo(double valor)
{
	 return (valor >= 0) ? valor : -1*valor;
}

double __fastcall GetDistance(TPoint ponto1, TPoint ponto2)
{
		int px = (ponto1.x - ponto2.x);
		px *= px;
		int py = (ponto1.y - ponto2.y);
    py *= py;

		long sum = (px + py);
		return Sqrt( Modulo(sum) );
}
//---------------------------------------------------------------------------

float __fastcall radianToInt(float valor) {
    return valor*180/pi;
}

int __fastcall modulo(int valor){
	return (valor < 0)?(-1*valor):(valor);
}

void __fastcall AimThread::Execute()
{
	//---- Place thread code here ----
	/*
	return;
	while(!Terminated)
	{
		if(executando)
			continue;
		else
        	executando = true;

		__try{
			if(ShotMatrix->modoMira != AM_Slice){
				if(ShotMatrix->ligado)
				{
					double Alfa, Beta, Gama, Delta;
                    double velocidadeVentoX = 0;
					double velocidadeVentoY = 0;
					double velocidadeTiroX = 0,  velocidadeTiroY = 0;

					switch(mobile){
					 case 12:
						 Alfa = cos(intToRadian((float)BotInfos->anguloBot))-0.033;
						break;
					 default:
						 Alfa = cos(intToRadian((float)BotInfos->anguloBot)) + (BotInfos->direita?-0.002:0.002);
						 break;
					}

                    Beta = sin(intToRadian((float)BotInfos->anguloBot));
					Gama = cos(intToRadian(BotInfos->anguloVento));
					Delta = sin(intToRadian(BotInfos->anguloVento));

					int fatorForca = BotInfos->efeitoVento;
					
					velocidadeVentoX = (int)( ((float)BotInfos->velocidadeVento) * Gama  );
					velocidadeVentoY = (int)( ((float)BotInfos->velocidadeVento) * Delta );

                    int x = BotInfos->posicaoBot.x;
					int y = BotInfos->posicaoBot.y;
					int forca = BotInfos->forcaBot;
				    int angulo = BotInfos->anguloBot;

					int menor = 9999;
					int melhor = 0;
					int count = 0;

					bool tornadosFlag[4];
					bool temTornado = BotInfos->tornados.temTornado();

					for( int force = 1; force <= 400; force++ )
					{
						 velocidadeTiroX = ((Alfa * fatorForca) * i) / 100.0;
						 velocidadeTiroY = ((Beta  * fatorForca) * i) / 100.0;

						 TProjetil Projetil = TProjetil(angulo, x, y, velocidadeTiroX, velocidadeTiroY);
						 TPointFloat ultimoProjetil = TPointFloat(x, y);

						 // Desenha as linhas de tornado
						 if(temTornado){
							 for(int i = 0; i < 4; i++){
								if(BotInfos->tornados.tornados[i].completo() ){
									int x1 = (BotInfos->tornados.tornados[i].inicio - (float)camera.x);
									int x2 = (BotInfos->tornados.tornados[i].fim - (float)camera.x);
									if(x1 >= 0 && x1 <= 800){
										grafico.DrawLine(&corLinha2, x1, 0, x1, 510);
									}
									if(x2 >= 0 && x2 <= 800){
										grafico.DrawLine(&corLinha2, x2, 0, x2, 510);
									}
								}else{
									if(BotInfos->tornados.tornados[i].etapa == 1){
										int x1 = (BotInfos->tornados.tornados[i].inicio - (float)camera.x);
										if(x1 >= 0 && x1 <= 800){
											grafico.DrawLine(&corLinha2, x1, 0, x1, 510);
										}
									}
								}
							 }
						 }


						 if(velocidadeTiroX != 0 || velocidadeTiroY != 0){
							 while(Projetil.x < 1800 && Projetil.x > 0 && Projetil.y > -5000 && Projetil.y < 2000)
							 {
								Projetil.x = Projetil.x + Projetil.speedX * 0.05 + aceleracaoX*0.05*0.05*0.5;
								Projetil.y = Projetil.y - Projetil.speedY * 0.05 + aceleracaoY*0.05*0.05*0.5;
								Projetil.speedX = Projetil.speedX + aceleracaoX * 0.05;
								Projetil.speedY = Projetil.speedY + aceleracaoY * 0.05;

								if(temTornado){
									for(int i = 0; i < 10; i++){
										if(BotInfos->tornados.tornados[i].completo()){
											bool dentroDoTornado = (ultimoProjetil.x >= BotInfos->tornados.tornados[i].inicio  && ultimoProjetil.x <= BotInfos->tornados.tornados[i].fim);
											if(Projetil.x >= BotInfos->tornados.tornados[i].inicio  && Projetil.x <= BotInfos->tornados.tornados[i].fim && !tornadosFlag[i] && !dentroDoTornado) {
												tornadosFlag[i] = true;
												BOOL direita = ((Projetil.x - ultimoProjetil.x) > 0);

												// Calcula o angulo de inclinação da trajetória no momento do encontro com o tornado
												float distX = (float)((float)Projetil.x - (float)ultimoProjetil.x);
												float distY = (-1)*(float)((float)Projetil.y - (float)ultimoProjetil.y);

												//  Evita divisão por zero
												distX = (distX == 0.0)? 1.0 : distX;
												float angulo = ((float)distY/(float)distX);
												angulo = radianToInt(atan(angulo));

												if(!direita)
												   angulo = (180+angulo);

												if(angulo < 0) angulo += 360;

												//  Verifica se o tiro vem do lado da linha 1 ou 2 do tornado
												BOOL pertoDoPrimeiro =  ((BotInfos->tornados.tornados[i].inicio - ultimoProjetil.x) > 0);

												//  Calcula o ponto exato de encontro com o tornado
												float novoX = ((pertoDoPrimeiro)?(float)Modulo(BotInfos->tornados.tornados[i].inicio - ultimoProjetil.x):(float)Modulo(BotInfos->tornados.tornados[i].fim - ultimoProjetil.x));
												float tgAngulo = (distY/distX);
												float novoY = (-1)*(novoX*tgAngulo);

												//  corrige a inversao de sinal do inicio
												distY *= -1;

												Projetil.x = ((pertoDoPrimeiro)?BotInfos->tornados.tornados[i].inicio:BotInfos->tornados.tornados[i].fim);
												Projetil.y = Projetil.y - distY + novoY;
												//  Linha segue para a outra ponta do tornado
												float proximoX = ((pertoDoPrimeiro)?BotInfos->tornados.tornados[i].fim:BotInfos->tornados.tornados[i].inicio);

												//  Largura dp Tornado, para calcular o Y de subida do tiro
												float larguraTornado = (float)Modulo(BotInfos->tornados.tornados[i].fim - BotInfos->tornados.tornados[i].inicio);

												//  Angulo para a direita inverte o sinal
												if(angulo < 90.0 || angulo > 270.0){
													tgAngulo *= -1;
												}else if(angulo == 90.0 || angulo == 270.0){
													continue;
												}

												//  Tamanho da subida/descida de uma semi-volta no tornado
												float y = (float)((float)tgAngulo*(float)larguraTornado);

												//  Volta 2
												Projetil.y += y;
												Projetil.x = proximoX;
												proximoX = ((pertoDoPrimeiro)?BotInfos->tornados.tornados[i].inicio:BotInfos->tornados.tornados[i].fim);

												//  Volta 3
												Projetil.y += y;
												Projetil.x = proximoX;
												proximoX = ((pertoDoPrimeiro)?BotInfos->tornados.tornados[i].fim:BotInfos->tornados.tornados[i].inicio);

												Projetil.y += y;
												Projetil.x = proximoX;

												ultimoProjetil.x = Projetil.x;
												ultimoProjetil.y = Projetil.y;
											}else{
												tornadosFlag[i] = false;
											}
										}
									}
								 }
                            
								// Grava o ultimo ponto calculado da trajetoria
								ultimoProjetil.x = Projetil.x;
								ultimoProjetil.y = Projetil.y;

								TPoint tempp;
								tempp.x = Projetil.x;
								tempp.y = Projetil.y;

								int a = ((int)ShotMatrix->alvo.x - (int)Projetil.x);
								int b = ((int)ShotMatrix->alvo.y - (int)Projetil.y);
							
								double distanced = sqrt( Modulo(a*a) + Modulo(b*b) );
								int distance = (int)distanced;

								if( distance <= menor )
								{
									menor = distance;
									melhor = force;
								}

							 }
						 }
					}
					if(melhor > 1)
						ShotMatrix->estadoBot.forca = melhor;

				}
			}
		}__finally{
            executando = false;
		}

		Sleep(10);
		Application->ProcessMessages();

	}  */
}
//---------------------------------------------------------------------------
