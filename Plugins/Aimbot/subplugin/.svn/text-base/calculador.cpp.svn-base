//---------------------------------------------------------------------------


#pragma hdrstop

//#include "forms.hpp"
#include "calculador.h"
#include "controle.h"

double tick = 0.05f;
POINT pontoNak;
POINT posicaoMouse;

bool __fastcall teclaPressionada(int key){
	int status = GetKeyState(key);
	return ((status < -1) || (status > 1));
}

TMelhorForca __fastcall TCalculador::calcularMelhor(HANDLE closeFlag){

	 menorDistancia = 9999;

	 ZeroMemory(&tornadosFlag[0],4);
	 ZeroMemory(&Projetil[0],(sizeof Projetil[0])*6);
	 ZeroMemory(&ultimoProjetil[0],48);

	 int mobile = aimbot->mobile;
	 flagEspelho = false;

	 angulo = aimbot->anguloBot;

	 GetCursorPos(&posicaoMouse);
	 if(!teclaPressionada(VK_CONTROL)){
			pontoNak.x = posicaoMouse.x + camera.x;
			pontoNak.y = posicaoMouse.y + camera.y;
	 }

	 Alfa = cos(intToRadian((float)angulo)) + (aimbot->direita?-0.002:0.002);
	 Alfa = ((mobile == 12)?(cos(intToRadian((float)angulo))-(0.053-(0.0002*angulo))):Alfa);
	 Beta = sin(intToRadian((float)angulo));
	 Gama = (float)cos(intToRadian(aimbot->anguloVento)); // ( (cos(intToRadian(aimbot->anguloVento)) < 0)?0.041:-0.03) + aimbot->maya.faseBola1;
	 Delta = (float)sin(intToRadian(aimbot->anguloVento));

	 velocidadeTiroX = (Alfa * aimbot->forcaBot);
	 velocidadeTiroY = (Beta * aimbot->forcaBot);

	 x = aimbot->posicaoBot.x;
	 y = aimbot->posicaoBot.y;

	 gravidade = aimbot->gravidadeGB + ((mobile == 12)?(0.004*angulo - 0.01):0);// - ((mobile == 12)?(23.7-(0.2*angulo)):0) + ((mobile == 12 && angulo > 85)?3:0);
	 tiro = aimbot->tiro;
	 direita = aimbot->direita;

	 aceleracaoVentoX = ((int)((float)Gama * (float)aimbot->forcaVento + ((Gama < 0)?-1:1) * 0.0001f )) * aimbot->efeitoVento;
	 aceleracaoVentoY = ((int)((float)Delta * (float)aimbot->forcaVento + ((Delta < 0)?-1:1) * 0.0001f)) * (aimbot->efeitoVento);

	 temTornado = aimbot->tornados.temTornado();

	 Projetil[0] = TProjetil(angulo, x, y, velocidadeTiroX, velocidadeTiroY);
	 velocidadeProjetil.x = 0;
	 velocidadeProjetil.y = 0;
	 aceleracaoVento.x = aceleracaoVentoX;
	 aceleracaoVento.y = aceleracaoVentoY;
	 novaGravidade = gravidade;

	 alvo = aimbot->posicaoAlvo;

	 int k = 0;
     int n = 0;
	 bool flagCalcula = false;
	 fAtual = 0;

	 int j = 0;
	 while( WaitForSingleObject(closeFlag, ((j % 10) == 0)?3:0) != WAIT_OBJECT_0 ){
     	j++;
		if(!flagCalcula)
			fAtual += 20;
		else{
			k++;
			if(k > 40)
            	break;
			fAtual++;
		}

		if(fAtual > 400 && !flagCalcula){
        	flagCalcula = true;
			fAtual = (melhorForca.forca - 20);
			fAtual = ((fAtual < 0)?0:fAtual);
			continue;
		}else if(fAtual > 400 && flagCalcula){
            break;
		}

		velocidadeTiroX = (Alfa * fAtual);
		velocidadeTiroY = (Beta * fAtual);
        aceleracaoVento.x = aceleracaoVentoX;
		aceleracaoVento.y = aceleracaoVentoY;
		velocidadeProjetil.x = 0;
	 	velocidadeProjetil.y = 0;

		aceleracaoVentoX = ((int)((float)Gama * (float)aimbot->forcaVento + ((Gama < 0)?-1:1) * 0.0001f )) * aimbot->efeitoVento;
	 	aceleracaoVentoY = ((int)((float)Delta * (float)aimbot->forcaVento + ((Delta < 0)?-1:1) * 0.0001f)) * (aimbot->efeitoVento);

		x = aimbot->posicaoBot.x;
	 	y = aimbot->posicaoBot.y;

		Projetil[0] = TProjetil(angulo, x, y, velocidadeTiroX, velocidadeTiroY);

		ultimoProjetil[0].x = Projetil[0].x;
		ultimoProjetil[0].y = Projetil[0].y;
		Projetil[0].x = x;
		Projetil[0].y = y;
		Projetil[0].inverter = false;

        switch(mobile){
			case 0: calculaGenerico(); break;
			case 1: calculaGenerico(); break;
			case 2: calculaNak(); break;
			case 3: calculaTrico(); break;
			case 4: calculaBigFoot(); break;
			case 5: calculaBoomer(); break;
			case 6: calculaGenerico(); break;
			case 7: calculaLightning(); break;
			case 8: calculaGenerico(); break;
			case 9: calculaASate(); break;
			case 10: calculaGenerico(); break;
			case 11: calculaTurtle(); break;
			case 12: calculaGrub(); break;
			case 13: calculaGenerico(); break;
			case 14: calculaKalsiddon(); break;
			case 15: calculaJFrog(); break;
			case 16: calculaDragon(); break;
			case 17: calculaKnight(); break;
			case 19: calculaPhoenix(); break;
			case 20: calculaMaya(); break;
			case 21: calculaGenerico(); break;
			case 22: calculaGenerico(); break;
			case 23: calculaBlueWhale(); break;
			case 24: calculaGenerico(); break;
			case 25: calculaGenerico(); break;
			case 26: calculaGenerico(); break;
			case 27: calculaGenerico(); break;
			case 28: calculaGenerico(); break;


			default: break;
		}

	 }

	 return melhorForca;
}

void __fastcall TCalculador::analizaMelhor(int alvo_x, int alvo_y){
	float a = (alvo.x - alvo_x);
	float b = (alvo.y - alvo_y);
	float distancia = sqrt( modulo(a*a) + modulo(b*b) );
	if(distancia < menorDistancia){
		menorDistancia = distancia;
		melhorForca.forca = fAtual;
		melhorForca.ponto.x = alvo_x;
		melhorForca.ponto.y = alvo_y;
	}
}

void __fastcall TCalculador::analizaMelhor(int distancia){
	if(distancia < menorDistancia){
		menorDistancia = distancia;
        melhorForca.forca = fAtual;
	}
}

void __fastcall TCalculador::calculaTornado(bool analizar){
	if(temTornado){
		if(analizar)
    		analizaMelhor(Projetil[0].x, Projetil[0].y);
		__try{
			for(int i = 0; i < 5; i++){
				if(aimbot->tornados.tornados[i].completo()){
					for(int j = 0; j < count; j++){
						bool dentroDoTornado = (ultimoProjetil[j].x >= aimbot->tornados.tornados[i].inicio  && ultimoProjetil[j].x <= aimbot->tornados.tornados[i].fim);
						if(Projetil[j].x >= aimbot->tornados.tornados[i].inicio  && Projetil[j].x <= aimbot->tornados.tornados[i].fim && !tornadosFlag[i] && !dentroDoTornado) {
							if(!aimbot->tornados.tornados[i].espelho){
								tornadosFlag[i] = true;
								BOOL direita = ((Projetil[j].x - ultimoProjetil[j].x) > 0);

								// Calcula o angulo de inclinação da trajetória no momento do encontro com o tornado
								float distX = (float)((float)Projetil[j].x - (float)ultimoProjetil[j].x);
								float distY = (-1)*(float)((float)Projetil[j].y - (float)ultimoProjetil[j].y);

								//  Evita divisão por zero
								distX = (distX == 0.0)? 1.0 : distX;
								float angulo = ((float)distY/(float)distX);
								angulo = radianToInt(atan(angulo));

								if(!direita)
								   angulo = (180+angulo);

								if(angulo < 0) angulo += 360;

								//  Verifica se o tiro vem do lado da linha 1 ou 2 do tornado
								BOOL pertoDoPrimeiro =  ((aimbot->tornados.tornados[i].inicio - ultimoProjetil[j].x) > 0);

								//  Calcula o ponto exato de encontro com o tornado
								float novoX = ((pertoDoPrimeiro)?(float)modulo(aimbot->tornados.tornados[i].inicio - ultimoProjetil[j].x):(float)modulo(aimbot->tornados.tornados[i].fim - ultimoProjetil[j].x));
								float tgAngulo = (distY/distX);
								float novoY = (novoX*tgAngulo);

								//  corrige a inversao de sinal do inicio
								distY *= -1;

								Projetil[j].x = ((pertoDoPrimeiro)?aimbot->tornados.tornados[i].inicio:aimbot->tornados.tornados[i].fim);
								Projetil[j].y = Projetil[j].y - distY + (pertoDoPrimeiro?-novoY:novoY);

								//  Linha segue para a outra ponta do tornado
								float proximoX = ((pertoDoPrimeiro)?aimbot->tornados.tornados[i].fim:aimbot->tornados.tornados[i].inicio);

								//  Largura dp Tornado, para calcular o Y de subida do tiro
								float larguraTornado = (float)modulo(aimbot->tornados.tornados[i].fim - aimbot->tornados.tornados[i].inicio);

								//  Angulo para a direita inverte o sinal
								if(angulo < 90.0 || angulo > 270.0){
									tgAngulo *= -1;
								}else if(angulo == 90.0 || angulo == 270.0){
									continue;
								}

								//  Tamanho da subida/descida de uma semi-volta no tornado
								float y = (float)((float)tgAngulo*(float)larguraTornado);

								//  Volta 2
								Projetil[j].y += y;
								Projetil[j].x = proximoX;
								proximoX = ((pertoDoPrimeiro)?aimbot->tornados.tornados[i].inicio:aimbot->tornados.tornados[i].fim);

								//  Volta 3
								Projetil[j].y += y;
								Projetil[j].x = proximoX;
								proximoX = ((pertoDoPrimeiro)?aimbot->tornados.tornados[i].fim:aimbot->tornados.tornados[i].inicio);

								// Define variáveis da equação da reta (aplicação na formula da phoenix)
								reta.pa = Projetil[j].y - ((float)Projetil[j].y + (float)y);
								reta.pb = (float)proximoX - Projetil[j].x;
								reta.pc = Projetil[j].x * ((float)Projetil[j].y + (float)y) - Projetil[j].y * (float)proximoX;

								Projetil[j].y += y;
								Projetil[j].x  = proximoX;

								ultimoProjetil[j].x = Projetil[j].x;
								ultimoProjetil[j].y = Projetil[j].y;
							}else{
								Projetil[j].inverter = !Projetil[j].inverter;
								Projetil[j].ultimoSpeedX *= -1;

								tornadosFlag[i] = true;
								BOOL direita = ((Projetil[j].x - ultimoProjetil[j].x) > 0);

								// Calcula o angulo de inclinação da trajetória no momento do encontro com o espelho
								float distX = (float)((float)Projetil[j].x - (float)ultimoProjetil[j].x);
								float distY = (-1)*(float)((float)Projetil[j].y - (float)ultimoProjetil[j].y);

								//  Evita divisão por zero
								distX = (distX == 0.0)? 1.0 : distX;
								float angulo = ((float)distY/(float)distX);
								angulo = radianToInt(atan(angulo));

								if(!direita)
								   angulo = (180+angulo);

								if(angulo < 0) angulo += 360;

								//  Verifica se o tiro vem do lado da linha 1 ou 2 do espelho
								BOOL pertoDoPrimeiro =  ((aimbot->tornados.tornados[i].inicio - ultimoProjetil[j].x) > 0);

								//  Calcula o ponto exato de encontro com o espelho
								float novoX = ((pertoDoPrimeiro)?(float)modulo(aimbot->tornados.tornados[i].inicio - ultimoProjetil[j].x):(float)modulo(aimbot->tornados.tornados[i].fim - ultimoProjetil[j].x));
								float tgAngulo = (distY/distX);
								float novoY = (novoX*tgAngulo);
								float resto = (distX - novoX);

								//  corrige a inversao de sinal do inicio
								distY *= -1;

								if((Projetil[j].y - distY + novoY) > 360){
									Projetil[j].x = ((pertoDoPrimeiro)?aimbot->tornados.tornados[i].inicio:aimbot->tornados.tornados[i].fim);
									Projetil[j].y = Projetil[j].y - distY + (pertoDoPrimeiro?-novoY:novoY);
									flagEspelho = !flagEspelho;
								}

								double x2 = ultimoProjetil[j].x;
								double y2 = ((Projetil[j].y - ultimoProjetil[j].y) * 2 + ultimoProjetil[j].y);

								// Variáveis da equação da reta
								reta.pa = (Projetil[j].y - y2);
								reta.pb = (x2 - Projetil[j].x);
								reta.pb = ((reta.pb == 0)?1:reta.pb);
								reta.pc = (Projetil[j].x * y2 - Projetil[j].y * x2);

								ultimoProjetil[j].x = Projetil[j].x;
								ultimoProjetil[j].y = Projetil[j].y;

								// Termina o pedaço da trajetória após a inversão da mesma
								Projetil[j].x -= resto;
								Projetil[j].y = (((-1 * reta.pa * Projetil[j].x) - reta.pc) / reta.pb);

								ultimoProjetil[j].x = Projetil[j].x;
								ultimoProjetil[j].y = Projetil[j].y;
							}
						}else{
							tornadosFlag[i] = false;
						}
					}
				}
			}
		}__except(1){
		}
	}
	if(analizar)
		analizaMelhor(Projetil[0].x, Projetil[0].y);
}

void __fastcall TCalculador::calcular(int indice, float t, bool analizar){
	/*ultimoPontoEstatico[indice] = pontoEstatico[indice];

	pontoEstatico[indice].x = Projetil[indice].xInicial + x + Projetil[indice].speedX * t + aceleracaoVentoX * t * t;
	pontoEstatico[indice].y = Projetil[indice].yInicial + y - Projetil[indice].speedY * t - aceleracaoVentoY * t * t + 0.5 * gravidade * t * t;

	Projetil[indice].x += (pontoEstatico[indice].x - ultimoPontoEstatico[indice].x);
	Projetil[indice].y += (pontoEstatico[indice].y - ultimoPontoEstatico[indice].y);
	*/

	Projetil[indice].x += Projetil[indice].speedX * tick * (Projetil[indice].inverter?-1:1) + Projetil[indice].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5;
	Projetil[indice].y -= Projetil[indice].speedY * tick + Projetil[indice].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
	Projetil[indice].ultimoSpeedX += aceleracaoVentoX * tick;
	Projetil[indice].ultimoSpeedY += (aceleracaoVentoY - gravidade) * tick;
	calculaTornado(analizar);

	if(analizar)
		analizaMelhor(Projetil[indice].x, Projetil[indice].y);
}

void __fastcall TCalculador::desenhaLinha(int indice, bool flag){
	ultimoProjetil[indice].x = Projetil[indice].x;
	ultimoProjetil[indice].y = Projetil[indice].y;
}

void __fastcall TCalculador::calculaNak() {
	__try {
		count = 1;
		int tempo = 0;
		bool primeiro = true;
		bool flagNak = false;
		int descidaCount = 0;

		for (double t = 0; t <= 50; t += tick){
			tempo += 5;
			int novoY = pontoNak.y;
			//calcular(0,t);

			if((int)Projetil[0].y > (int)ultimoProjetil[0].y){
			   descidaCount++;
			}

			if(!flagNak){
				calcular(0,t);
				/*
				Projetil[0].x += Projetil[0].speedX * tick * (Projetil[0].inverter?-1:1) + Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5;
				Projetil[0].y -= Projetil[0].speedY * tick + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
				Projetil[0].ultimoSpeedX += aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY += (aceleracaoVentoY - gravidade) * tick;
				calculaTornado(false);     */
			}else{
                Projetil[0].x += Projetil[0].speedX * tick * (Projetil[0].inverter?-1:1) + Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5;
				Projetil[0].y -= Projetil[0].speedY * tick * 1.48 + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
				Projetil[0].ultimoSpeedX += aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY += - gravidade * -2.4 * tick;
				analizaMelhor(Projetil[0].x, Projetil[0].y);
				calculaTornado();
			}

			//calculaTornado();
			desenhaLinha(0);
			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
				break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaTrico() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaBigFoot() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}


void __fastcall TCalculador::calculaBoomer() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaLightning() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaJD() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaASate() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaTurtle() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaGrub() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaKalsiddon() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaJFrog() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaDragon() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaKnight() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaPhoenix() {

	// Mudanças de Raio, Fase e Tempo
	TPhoenixCFG phoenixCFG[10] = {
		{0,248,26,0}, {1,243,25,0},   {2,239,25,0}, {4,236,25,0}, {6,234,25,0},
		{7,232,26,0}, {7.8,230,27,0}, {8,228,28,0}, {8,227,30,0}, {8,227,32,0}
	};

	TPhoenixCFG phoenixCFGSS[10] = {
		{0,258,80,27},   {0,257,80,27},   {0,256,80,27}, {0,255,80,27}, {0,254,80,27},
		{0.7,253,90,28}, {1.5,253,80,28}, {2,252,80,28}, {1,252,80,28}, {1,252,60,28}
	};

	// Mudança de fase do Tiro 1 de acordo com a força
	float raiosCFGT1[5] = {0, 0.125, 0.250, 0.625, 1};
	float raiosCFGSS[5] = {0, 0, 0.33, 0.67, 1};
	float temposT2CFG[10] = {2, 2, 1, 1, 1, 0, -1, -2, -3, -5};

	__try {
		bool inverteTopo = false;

		count = 1;
		bool flagPhoenix = false;
		bool primeiro = true;
		int tempoPhoenix = 0;
		int tempo = 0;


		ultimoProjetil[0].x = Projetil[0].x;
		ultimoProjetil[0].y = Projetil[0].y;
		Projetil[0].x = x;
		Projetil[0].y = y;
		Projetil[0].inverter = false;
		//analizaMelhor(Projetil[0].x, Projetil[0].y);

		int anguloQuad = (angulo == 90)?90:(angulo % 90);
		anguloQuad = (angulo > 90)?(90-anguloQuad):anguloQuad;
		float indiceMod = (anguloQuad % 10);
		int indice =  (int)(anguloQuad / 10);
		int indice2 = ((indice+1) > 9)?indice:(indice+1);

		TPhoenixCFG cfg1 = phoenixCFG[indice];
		TPhoenixCFG cfg2 = phoenixCFG[indice2];
		TPhoenixCFG cfg;

		if(tiro == 2){
			cfg1 = phoenixCFGSS[indice];
			cfg2 = phoenixCFGSS[indice2];
		}

		cfg.raio  = cfg1.raio  + (((float)(cfg2.raio  - cfg1.raio ))/10 * indiceMod);
		cfg.fase2 = cfg1.fase2 + (((float)(cfg2.fase2 - cfg1.fase2))/10 * indiceMod);
		cfg.tempo = cfg1.tempo + (((float)(cfg2.tempo - cfg1.tempo))/10 * indiceMod);
		cfg.tempo2 = cfg1.tempo2 + (((float)(cfg2.tempo2 - cfg1.tempo2))/10 * indiceMod);

		float indiceModF = (fAtual % 100);
		int indiceF = floor(fAtual / 100);
		int indice2F = ((indiceF+1) > 4)?indiceF:(indiceF+1);

		cfg.fase2 += (tiro == 2)?(8 - (fAtual * 0.02)):(16 - (fAtual * 0.04));
		float raioTiro = 0;

		for (double t = 0; t <= 50; t += tick){
        	tempo += 5;
			if(flagPhoenix){
				Projetil[0].x += Projetil[0].speedX * tick * (Projetil[0].inverter?-1:1) + (Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5)*(Projetil[0].inverter?0:1);
				Projetil[0].y -= Projetil[0].speedY * tick + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
				Projetil[0].ultimoSpeedX -= aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY -= (aceleracaoVentoY - gravidade) * tick;
				calculaTornado(false);
			}else{
				calcular(0,t,false);
			}

			if(flagPhoenix){
				int semiTempo = 0;

				if(tiro == 0){
                	semiTempo = tempoPhoenix + ((int)cfg.tempo)*5;

					float porcentagem1 = raiosCFGT1[indiceF];
					float porcentagem2 = raiosCFGT1[indice2F];
					float porcentagem =  porcentagem1 + (porcentagem2 - porcentagem1)/100 * indiceModF;

					raioTiro = cfg.raio * porcentagem;
				} else if(tiro == 1){
                    semiTempo = tempoPhoenix + ((int)cfg.tempo)*5;

					float tempoT2CFG1 = temposT2CFG[indice];
					float tempoT2CFG2 = temposT2CFG[indice2];
					float tempoT2CFG = tempoT2CFG1 + ((tempoT2CFG2 - tempoT2CFG1)/10 * indiceMod);

					semiTempo += ((int)tempoT2CFG)*5;

				}else if(tiro == 2){
					semiTempo = tempoPhoenix + ((int)cfg.tempo2)*5;// + aimbot->teste1.y*5;

					float porcentagem1 = raiosCFGSS[indiceF];
					float porcentagem2 = raiosCFGSS[indice2F];
					float porcentagem =  porcentagem1 + (porcentagem2 - porcentagem1)/100 * indiceModF;

					raioTiro = cfg.raio * porcentagem;
				}

				if(tempo == semiTempo){
					analizaMelhor(Projetil[0].x, Projetil[0].y);

					if(tiro == 0){
						int fase = 80;
						int sweep = 200;
						float raioPhoenix = raioTiro;//aimbot->maya.faseBola2; //0.0175*forca * modulo(sin( intToRadian(aimbot->anguloBot) )) + aimbot->maya.faseBola2 - 0.86;
						float px = 0;
						float py = 0;
						float ux = Projetil[0].x;
						float uy = Projetil[0].y;

						int n = modulo(360-sweep);

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						// Desenha o semi-circulo (de 5 em 5 angulos para economizar tempo)
						for(int i = 0; i < n; i+= 5){
							Projetil[0].x = ultimoProjetil[0].x + ((float)raioPhoenix*(float)cos((float)intToRadian(i+fase))*(aimbot->direita?-1:1)) * ((Projetil[0].inverter)?-1:1) * ((inverteTopo)?-1:1);
							Projetil[0].y = ultimoProjetil[0].y - ((float)raioPhoenix*(float)sin((float)intToRadian(i+fase)));

							calculaTornado();

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}

						raioPhoenix = 68;
						fase = cfg.fase2;
						sweep = 200;

						for(float i = 0; i < n; i+= 0.5){
							Projetil[0].x = ultimoProjetil[0].x + ((float)raioPhoenix*(float)cos((float)intToRadian(i+fase))*(aimbot->direita?-1:1)) * ((Projetil[0].inverter)?-1:1) * ((inverteTopo)?-1:1);
							Projetil[0].y = ultimoProjetil[0].y - ((float)raioPhoenix*(float)sin((float)intToRadian(i+fase)));

							calculaTornado();

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}

						return;
					}else if(tiro == 1){ // T2
						int subida = 60+0.1*fAtual;
						int pontoFim = 1900;

						int pontoInicio = (int)(Projetil[0].y - subida);
						int g = (((pontoFim - pontoInicio) > 0)?2:-2);
						int dif = modulo(pontoFim - pontoInicio);

						//for (double t = 0; t <= 50; t += tick){
						for(int n = 0; n < dif; n++){
							pontoInicio += g;
							Projetil[0].y = pontoInicio;
							analizaMelhor(Projetil[0].x, Projetil[0].y);
						}

                        Projetil[0].y = 1900;
                        analizaMelhor(Projetil[0].x, Projetil[0].y);
						return;
					}else{  // SS
						int subida = cfg.tempo - (40 - (0.1*fAtual));//aimbot->maya.faseBola1; //60+0.1*forca;

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						Projetil[0].y = (Projetil[0].y - subida);
						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						//-----------------------------------
						int fase = -90;
						int sweep = 200;
						float raioPhoenix = raioTiro; //aimbot->teste1.x;//(forca < 200)?0:(0.01*(forca - 200)); //0.0175*forca;

						int n = modulo(360-sweep);

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						for(int i = 0; i < n; i+= 5){
							Projetil[0].x = ultimoProjetil[0].x + ((float)raioPhoenix*(float)cos((float)intToRadian(i+fase))*(aimbot->direita?-1:1)) * ((Projetil[0].inverter)?-1:1) * ((inverteTopo)?-1:1);
							Projetil[0].y = ultimoProjetil[0].y + ((float)raioPhoenix*(float)sin((float)intToRadian(i+fase)));

							calculaTornado();

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}

						raioPhoenix = 80;
						fase = cfg.fase2;
						sweep = 200;

						for(float i = 0; i < n; i+= 0.5){
							Projetil[0].x = ultimoProjetil[0].x - ((float)raioPhoenix*(float)cos((float)intToRadian(i+fase))*(aimbot->direita?-1:1)) * ((Projetil[0].inverter)?-1:1) * ((inverteTopo)?-1:1);
							Projetil[0].y = ultimoProjetil[0].y - ((float)raioPhoenix*(float)sin((float)intToRadian(i+fase)));

							calculaTornado();

							analizaMelhor(Projetil[0].x, Projetil[0].y);

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}

						return;
					}
				}
			} else {
				 int semiTempo = 210;

				 if(tempo == semiTempo){

					analizaMelhor(Projetil[0].x, Projetil[0].y);

					ultimoProjetil[0].x = Projetil[0].x;
					ultimoProjetil[0].y = Projetil[0].y;

					x = Projetil[0].x;
					y = Projetil[0].y;

					tempoPhoenix = tempo;
					flagPhoenix = true;

					int diferenca = (x - aimbot->posicaoBot.x);

					if( (direita && diferenca <= -2) || (!direita && diferenca >= 2) )
                    	inverteTopo = !inverteTopo;
					continue;
				 }
			}

			calculaTornado();

			for(int i = 0; i < count; i++){
				primeiro = false;
				ultimoProjetil[i].x = Projetil[i].x;
				ultimoProjetil[i].y = Projetil[i].y;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaMaya() {
    __try{
		count = 1;//((tiro == 1)?3:1);
		bool primeiro = true;
		bool flagMaya = false;
		int tempo = 0;

		for(int i = 0; i < count; i++){
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (double t = 0; t <= 50; t += tick){
        	tempo += 5;
			if(flagMaya){
				Projetil[0].x -= Projetil[0].speedX * tick * (Projetil[0].inverter?-1:1) + (Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5)*(Projetil[0].inverter?0:1);
				Projetil[0].y += Projetil[0].speedY * tick + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
				Projetil[0].ultimoSpeedX += aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY += (aceleracaoVentoY - gravidade) * tick;

				calculaTornado();
				//analizaMelhor(Projetil[0].x, Projetil[0].y);

				// O maxado volta para a mão do Maya e para
				if(modulo(Projetil[0].x - aimbot->posicaoBot.x) <= 2 && modulo(Projetil[0].y - aimbot->posicaoBot.y) <= 2){
					break;
				}
			}else{
				calcular(0,t);
				//analizaMelhor(Projetil[0].x, Projetil[0].y);
			}

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}

            if(aimbot->maya.tiroEspecial && !flagMaya){
				 if(tiro == 2){
					 double base = 0.0063;
 					 double parte = (angulo % 360);
					 int anguloTiro = angulo;
					 anguloTiro = (anguloTiro % 90) - 45;
					 anguloTiro = (anguloTiro < 0)?(anguloTiro * -1):anguloTiro;
					 parte = (cos(intToRadian((float)anguloTiro))- 0.008)*100;
					 parte -= (96 - (fAtual * 0.24));

					 double partePico = base;
					 int pico = ((int)(parte))*5;// + (aimbot->teste1.x*5); //(partePico*(double)forca) * 29.54399486*5;

					 if(tempo >= (int)(pico - 2) && tempo <= (int)(pico + 2)){
						 double ny = (int)modulo(Projetil[0].y - aimbot->posicaoBot.y);
						 double tg = tan(intToRadian(angulo));
						 tg = ((tg == 0)?1:tg);
						 int nx = (int)(ny/tg);
						 int rx = (Projetil[0].x - nx);

						 t = 0;
						 flagMaya = true;
						 x = Projetil[0].x;
						 y = Projetil[0].y;

						 Projetil[0].ultimoSpeedX = 0;
						 Projetil[0].ultimoSpeedY = 0;
						 //analizaMelhor(Projetil[0].x, Projetil[0].y);

						 ultimoProjetil[0].x = Projetil[0].x;
						 ultimoProjetil[0].y = Projetil[0].y;

						 if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
							break;
						 }

						 continue;
					 }
				 }else{
					 count = 1;
				 }
			}

			for(int i = 0; i < count; i++){
				desenhaLinha(i);
				analizaMelhor(Projetil[0].x, Projetil[0].y);
				if( Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000 ){
					break;
				}
			}
		}
	}__except(1){
		return;
	}

	/*__try {

		count = 1;
		bool flagMaya = false;

		for(int i = 0; i < count; i++){
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
		}

		for (double t = 0; t <= 12; t += 0.05f){
			if(flagMaya){
				Projetil[0].x = x - velocidadeTiroX * t - aceleracaoVentoX * t * t;
				Projetil[0].y = y + velocidadeTiroY * t + aceleracaoVentoY * t * t - 0.5 * gravidade * t * t;
                analizaMelhor(Projetil[0].x, Projetil[0].y);
			}else{
				Projetil[0].x = x + Projetil[0].speedX * t + aceleracaoVentoX * t * t;
				Projetil[0].y = y - Projetil[0].speedY * t - aceleracaoVentoY * t * t + 0.5 * gravidade * t * t;
                analizaMelhor(Projetil[0].x, Projetil[0].y);
			}

            if(aimbot->maya.tiroEspecial && !flagMaya){
				 if(tiro == 2){//if(aimbot->tiro == 2 && TotalTime >= aimbot->maya.pico){ // 4.65

					 double base = 0.0063;

					 double parte = (angulo % 360);
					 parte = parte - (floor((double)parte/90)*90);
					 parte = ((parte > 45)?modulo(parte - 90):parte);
					 parte = ((parte == 0 || parte == 90)?0:parte);
					 base = base + parte*0.00006;

					 double partePico = base;
					 double pico = (partePico*(double)forca);

					 if(t >= pico && t < (pico+0.05)){
						 double ny = (int)modulo(Projetil[0].y - aimbot->posicaoBot.y);
						 double tg = tan(intToRadian(angulo));
						 tg = ((tg == 0)?1:tg);
						 int nx = (int)(ny/tg);
						 int rx = (Projetil[0].x - nx);

						 t = 0;
						 flagMaya = true;
						 x = Projetil[0].x;
						 y = Projetil[0].y;

						 ultimoProjetil[0].x = Projetil[0].x;
						 ultimoProjetil[0].y = Projetil[0].y;

						 continue;
					 }
				 }else{
					 count = 1;
				 }
			 }

			if(temTornado)
				calculaTornado();

			for(int i = 0; i < count; i++){
				ultimoProjetil[i].x = Projetil[i].x;
				ultimoProjetil[i].y = Projetil[i].y;
			}
		}
	}__except(1){
		return;
	}  */
}


void __fastcall TCalculador::calculaBlueWhale() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
                break;
			}
		}
	}__except(1){
		return;
	}
}

void __fastcall TCalculador::calculaGenerico() {
	__try {
		count = 1;

		for (double t = 0; t <= 50; t += tick){
			calcular(0,t);
			desenhaLinha(0);

			if( Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000 ){
				break;
			}
		}
	}__except(1){
		return;
	}
}


//---------------------------------------------------------------------------

#pragma package(smart_init)
