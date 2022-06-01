// ---------------------------------------------------------------------------

#pragma hdrstop

#include "mobilesB.h"
#include "..\..\Plugins\Aimbot\estruturaBot.h"
#include "debugUtils.h"

#pragma package(smart_init)

// int magePoints[40] = { 20, 30, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35, 30, 35  };
int magePoints[40] = {
	15, 40, 45, 50, 45, 45, 45, 45, 45, 45, 50, 45, 45, 45, 50, 45, 45, 50, 45, 45, 45, 50, 45, 40, 50, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45
};

double tick = 0.05f;
POINT pontoNak;

int PontoInicialX = 0;
int PontoInicialY = 0;

int __fastcall MobileToIndex(int valor) {
	switch(valor) {
	case 0:
	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	case 9:
	case 10:
	case 11:
	case 12:
	case 13:
	case 14:
	case 15:
	case 16:
	case 17:
		return valor;
		break;
	case 19:
	case 20:
	case 21:
	case 22:
	case 23:
	case 24:
		return(valor - 1);
	case 125:
		return 16;
		break;
	case 209:
		return 17;
		break;
	default:
		return 0;
		break;
	}
	return 0;
}

int __fastcall TShMatrix::IndexToMobile(int valor) {
	if (valor >= 0 && valor <= 17)
		return valor;
	else if (valor >= 18 && valor <= 23)
		return valor + 1;
	else
		return 0;
}

void __fastcall TShMatrix::ProximoMobile() {
	int j = indiceMobile;
	j = (indiceMobile + 1) % quantidadeMobiles;
	mobile = IndexToMobile(j);
}

void __fastcall TShMatrix::MobileAnterior() {
	int j = indiceMobile;
	j = (indiceMobile - 1 + quantidadeMobiles) % quantidadeMobiles;
	mobile = IndexToMobile(j);
}

void __fastcall TShMatrix::ProximaMira() {
	modoMira = (modoMira + 1) % 6;
}

void __fastcall TShMatrix::MiraAnterior() {
	modoMira = (modoMira - 1 + 6) % 6;
}

bool __fastcall TShMatrix::MesmoTime(int indice) {
	return((indice == indiceBot) || (times[indice] == times[indiceBot]));
}

bool __fastcall TShMatrix::AlguemNoLugar(int indice) {
	indice = (indice + 8) % 8;
	return(flags[indice] != 0);
}

void __fastcall TShMatrix::AlvoAnterior() {
	for (int i = alvoID; i >= 0; i--) {
		if (i == alvoID || i == indiceBot)
			continue;
		bool flag = MesmoTime(i);
		if (acertarAmigos)
			flag = false;
		if (!flag && AlguemNoLugar(i)) {
			alvoID = i;
			return;
		}
	}
	// Se continua aqui recomeça do 0
	for (int i = 7; i >= 0; i--) {
		if (i == alvoID || i == indiceBot)
			continue;
		bool flag = MesmoTime(i);
		if (acertarAmigos)
			flag = false;
		if (!flag && AlguemNoLugar(i)) {
			alvoID = i;
			return;
		}
	}
}

void __fastcall TShMatrix::ProximoAlvo() {
	for (int i = alvoID; i < 8; i++) {
		if (i == alvoID || i == indiceBot)
			continue;
		bool flag = MesmoTime(i);
		if (acertarAmigos)
			flag = false;
		if (!flag && AlguemNoLugar(i)) {
			alvoID = i;
			return;
		}
	}
	// Se continua aqui recomeça do 0
	for (int i = 0; i < 8; i++) {
		if (i == alvoID || i == indiceBot)
			continue;
		bool flag = MesmoTime(i);
		if (acertarAmigos)
			flag = false;
		if (!flag && AlguemNoLugar(i)) {
			alvoID = i;
			return;
		}
	}
}

//
// Pega posição da câmera
//
POINT __fastcall TShMatrix::GetCameraPos() {
	POINT camera;
	__try {
		__try {
			if (enderecos[1] > 0 && enderecos[2] > 0) {
				camera.x = *((PWORD)(enderecos[1]));
				camera.y = *((PWORD)(enderecos[2]));
				camera.x -= 400;
				camera.y -= 240;
				if (camera.x > 60000)
					camera.x = (camera.x - 65535);
			}
		}
		__except (1) {
			return camera;
		}
		return camera;
	}
	__except (1) {
	}
	return camera;
}

//
// Pega a distancia entre dois pontos
//
double __fastcall TShMatrix::GetDistance(POINT ponto1, POINT ponto2) {
	int px = (ponto1.x - ponto2.x);
	px *= px;
	int py = (ponto1.y - ponto2.y);
	py *= py;

	return sqrt(modulo(px + py));
}

POINT __fastcall TShMatrix::GetPosicaoDe(int index) {
	index &= 7;
	POINT tmpTarget;
	tmpTarget.x = semiPosicaoBot[index].x;
	tmpTarget.y = semiPosicaoBot[index].y + 15; //
	return tmpTarget;
}

POINT __fastcall TShMatrix::PosicaoBot() {
	TBotPos posicao;
	TPointFloatD aimFix = GetAimFix(indiceMobile);
	double v0 = 0.5;

	int inclinacaoInicial1 = aimFix.x1 + maya.velocidadeAngular;
	int raioVirtual1 = aimFix.y1 + maya.raio;

	int inclinacaoInicial2 = aimFix.x2 + maya.velocidadeAngular;
	int raioVirtual2 = aimFix.y2 + maya.raio;

	posicao.x = semiPosicaoBot[indiceBot].x;
	posicao.y = semiPosicaoBot[indiceBot].y;

	posicao.inclinacao = inclinacaoBot;

	//
	// Mobiles nak e aduka atiram pra traz
	BOOL tdir = ((indiceMobile == 2 || indiceMobile == 13) ? !direita : direita);

	if (backShot)
		tdir = !tdir;

	int angle;
	angle = ((tdir) ? (posicao.inclinacao + inclinacaoInicial1) : (posicao.inclinacao + (180 - inclinacaoInicial1)));

	posicao.x += (int)(cos(intToRadian(angle)) * (v0 + raioVirtual1));
	posicao.y -= (int)(sin(intToRadian(angle)) * (v0 + raioVirtual1)) - 20;

	POINT retorno;
	retorno.x = posicao.x;
	retorno.y = posicao.y;
	return retorno;
}

int __fastcall TShMatrix::GetBotAngle() {
	__try {
		int k = 0;
		if (interfaceJogo == 1)
			k = getAngleOld();
		else
			k = getAngleNew();

		if (direita)
			k += 360;
		else
			k = 180 - k;

		return(k % 360);
	}
	__except (1) {
		debugar("Erro 8586");
	}
}

TPointFloatD __fastcall TShMatrix::GetAimFix(int index) {
	switch(index) {
	case 0:
		return TPointFloatD(46, 30, 42, 30);
		break; // Armor
	case 1:
		return(direita ? TPointFloatD(59, 36) : TPointFloatD(53, 38));
		break; // Mage
	case 2:
		return TPointFloatD(140, 33);
		break; // Nak
	case 3:
		return TPointFloatD(58, 37);
		break; // Trico
	case 4:
		return TPointFloatD(56, 42);
		break; // Big Foot
	case 5:
		return TPointFloatD(75, 38);
		break; // Boomer
	case 6:
		return TPointFloatD(64, 34);
		break; // Raon ok
	case 7:
		return TPointFloatD(60, 36);
		break; // Lightning
	case 8:
		return TPointFloatD(60, 40);
		break; // J.D
	case 9:
		return TPointFloatD(42, 35);
		break; // A.Sate
	case 10: {
			// testPos = eng;
			int s = GetShotMode(interfaceJogo);
			if (s == 0)
				return TPointFloatD(54, 50); // Ice
			else
				return TPointFloatD(40, 45);
		}break;
	case 11:
		return TPointFloatD(59, 29);
		break; // Turtle ok
	case 12:
		return TPointFloatD(55, 38);
		break; // Grub
	case 13:
		return TPointFloatD(136, 29);
		break; // aduka
	case 14:
		return TPointFloatD(56, 42);
		break; // Kalsiddon
	case 15:
		return TPointFloatD(46, 30);
		break; // JFrog
	case 16:
	case 125:
		return TPointFloatD(53, 47);
		break; // return TPointFloatD(52-BotInfos->maya.faseBola1, 46-BotInfos->maya.faseBola2); break; //Dragon
	case 17:
	case 209:
		return TPointFloatD(45, 35);
		break; // Knight
	case 18:
		return(direita ? TPointFloatD(115, 42) : TPointFloatD(109, 42));
		break; // Phoenix
	case 19:
		return TPointFloatD(63, 35);
		break; // Maya
	case 20: { // Wolf
			int s = GetShotMode(interfaceJogo);
			if (s == 2)
				return TPointFloatD(68, 40);
			else
				return TPointFloatD(63, 35);
		}break;
	case 21:
		return TPointFloatD(60, 36);
		break; // Tiburon
	case 22:
		return TPointFloatD(63, 36);
		break; // BlueWhale
	case 23:
		return TPointFloatD(41, 31);
		break; // Frank
	default:
		break;
	}
	return TPointFloatD(0, 0);
}

//
// Pega posição do alvo
//
POINT __fastcall TShMatrix::PosicaoAlvo() {
	POINT mouse;
	GetCursorPos(&mouse);
	POINT alvo;
	switch(modoMira) {
	case AM_Auto: {
			alvo = GetPosicaoDe(alvoID);
		}break;

		//
		// Pega pela posição do mouse
	case AM_Mouse: {
			int estado = GetKeyState(VK_CONTROL);

			// Se estiver pressionado CTRL, mantem a mira fixa no local
			if (estado <= 1 && estado >= -1) {
				alvo.x = mouse.x + camera.x;
				alvo.y = mouse.y + camera.y;
			}
		}break;

	case AM_Random: {
			randomize();
			int j = (random(4) + 1);
			int i = 0, k = 0;

			while (j > 0) {
				Sleep(1);
				k++;

				if (AlguemNoLugar(i)) {
					bool flag = MesmoTime(i);
					if (acertarAmigos)
						flag = false;
					if (!flag) {
						j--;
						if (j == 0)
							break;
					}
				}

				i = ((i > 7) ? 0 : (i + 1));
				if (k > 32)
					break;
			}

			alvoID = i;
			alvo = GetPosicaoDe(i);

		}break;

		// Pega pela posição do slice
	case AM_Slice: {
			alvo.x = 0;
			alvo.y = 0;
		}break;

		// Pega pelo mais perto
	case AM_Perto: {
			TDistanceComp menor;
			menor.distancia = 9999;
			menor.index = 0;

			for (int i = 0; i < 8; i++) {
				Sleep(1);
				if (AlguemNoLugar(i)) {
					BOOL flag = MesmoTime(i);
					if (acertarAmigos)
						flag = false;
					if (!flag) {
						POINT tmpTarget = GetPosicaoDe(i);
						double dist = GetDistance(tmpTarget, posicaoBot);
						if (dist < menor.distancia) {
							menor.index = i;
							menor.distancia = dist;
						}
					}
				}
			}
			alvoID = menor.index;
			alvo = GetPosicaoDe(menor.index);
		}break;

		// Pega pelo mais longe
	case AM_Longe: {
			TDistanceComp maior;
			maior.distancia = 0;
			maior.index = 0;

			for (int i = 0; i < 8; i++) {
				Sleep(1);
				if (AlguemNoLugar(i)) {
					BOOL flag = MesmoTime(i);
					if (acertarAmigos)
						flag = false;
					if (!flag) {
						POINT tmpTarget = GetPosicaoDe(i);
						double dist = GetDistance(tmpTarget, posicaoBot);
						if (dist > maior.distancia) {
							maior.index = i;
							maior.distancia = dist;
						}
					}
				}
			}
			alvoID = maior.index;
			alvo = GetPosicaoDe(maior.index);
		}break;

	default:
		break;
	}
	return alvo;
	// target.y += 6;
}

Gdiplus::Color ToGdiColor(DWORD valor) {
	__try {
		TCM_RGB result = *(PCM_RGB)(&valor);
		result.x = 0xFF;
		return Gdiplus::Color(result.x, result.r, result.g, result.b);
	}
	__except (1) {
	}
	return Gdiplus::Color(0, 0, 0, 0);
}

void __fastcall TShMatrix::fillPoints(int px, int py) {
	__try {
		HDC dc = 0;
		dc = CreateDC("DISPLAY", "", "", NULL);
		if (!dc)
			return;

		__try {
			fillPoints(dc, px, py);
		}
		__finally {
			DeleteDC(dc);
		}
	}
	__except (1) {
	}
}

POINT posicaoMouse;

bool __fastcall teclaPressionada(int key) {
	int status = GetKeyState(key);
	return((status < -1) || (status > 1));
}

void __fastcall TShMatrix::processaRotacao(TRotacao *rotacao) {
	randomize();
	if (rotacao->sentido) {
		rotacao->anguloAtual = (rotacao->anguloAtual + rotacao->tick);
	}
	else {
		rotacao->anguloAtual = (rotacao->anguloAtual - rotacao->tick);
	}

	if (rotacao->anguloAtual > 360)
		rotacao->anguloAtual = 0;

	if (rotacao->anguloAtual < 0)
		rotacao->anguloAtual = 360;

	rotacao->anguloReal = (int)rotacao->anguloAtual % 360;

	rotacao->limite -= rotacao->tick;
	if (rotacao->limite <= 0) {
		randomize();
		rotacao->limite = random(200);
		int aleatorio = random(1000);
		rotacao->sentido = ((aleatorio % 2) == 0);
	}
}

void __fastcall TShMatrix::desenhaRotacao(TRotacao rotacao, Gdiplus::Pen *caneta, int x, int y, int raio, int comprimentoLinha, int extra) {
	int angulo = rotacao.anguloReal;
	int intervalo = 360 / (extra * 2 + 4);

	for (int i = 0; i < 4; i++) {
		POINT ponto1;
		POINT ponto2;

		ponto1.x = raio * cos(intToRadian(angulo));
		ponto1.y = raio * sin(intToRadian(angulo));

		ponto2.x = (raio + comprimentoLinha) * cos(intToRadian(angulo));
		ponto2.y = (raio + comprimentoLinha) * sin(intToRadian(angulo));

		if ((y + ponto1.y) <= 515 && (y + ponto2.y) <= 515) {
			grafico->DrawLine(caneta, (int)(x + ponto1.x)+PontoInicialX, (int)(y + ponto1.y)+PontoInicialY, (int)(x + ponto2.x)+PontoInicialX, (int)(y + ponto2.y)+PontoInicialY);
		}
		angulo += 90;
	}
}

void processaTempo(TRaio *raio) {
	if (raio->raioAtual < 0)
		raio->raioAtual = raio->raioMax;

	if (raio->flag) {
		raio->raioAtual = raio->raioAtual + raio->tick;
		if (raio->raioAtual >= raio->raioMax) {
			raio->raioAtual = raio->raioMax;
			raio->flag = false;
		}
	}
	else {
		raio->raioAtual = raio->raioAtual - raio->tick;
		if (raio->raioAtual <= raio->raioMin) {
			raio->raioAtual = raio->raioMin;
			raio->flag = true;
		}
	}
}

// 0 - BotPos
const int quantidadeRaios = 2;
TRaio raios[quantidadeRaios];

const int quantidadeRotacoes = 4;
TRotacao rotacoes[quantidadeRotacoes];

//
// Tempos usados nos cálculos de velocidade de rotação do círculo de alvo
DWORD tempoAtual = 0;
DWORD ultimoTempo = 0;
int tempoProcessamento = 0;
float tempoVirtual = 1;
float raioAlvoManual = 50;

//
// Seta a configuração de uma linha (cor e espessura)
//
void __fastcall TShMatrix::setConfiguracaoLinha(BYTE linha, DWORD cor, BYTE espessura) {
	int maxLinhas = MAX_LINHAS;
	if (linha < maxLinhas) {
		if (linhas[linha].pen != NULL)
			delete linhas[linha].pen;

		linhas[linha].cor = cor;
		if (espessura)
			linhas[linha].espessura = espessura;
		linhas[linha].pen = new Gdiplus::Pen(ToGdiColor(cor), espessura);
	}
}

//
// Realiza os cálculos e desenha na tela
//
float c1 = 0.0f;
float c2 = 0.0f;
void __fastcall TShMatrix::fillPoints(HDC dc, int px, int py) {
	// VMProtectBegin("fill");
	__try {
		if (!dc)
			return;

		if (!ligado)
			return;

		PontoInicialX = px;
		PontoInicialY = py;

		//
		// Calculo do tempo/velocidade de rotação do circulo de alvo
		tempoAtual = GetTickCount();
		if (ultimoTempo == 0)
			ultimoTempo = tempoAtual;
		tempoProcessamento = (tempoAtual - ultimoTempo);
		tempoVirtual = (tempoProcessamento == 0) ? 1.0f : (1 / ((float)tempoProcessamento));
		ultimoTempo = tempoAtual;

		//
		// No topo da tela o contador de posição do GB surta. Corrijamos isso
		if (camera.y > 60000)
			camera.y = (camera.y - 65295 - 240);

		//
		// Precisamos da posição do mouse em algumas operações
		GetCursorPos(&posicaoMouse);

		ZeroMemory(&tornadosFlag[0], 4);
		ZeroMemory(&Projetil[0], (sizeof Projetil[0]) * 6);
		ZeroMemory(&ultimoProjetil[0], 48);

		//
		// Trava mira do nak ao precionar CTRL
		if (!teclaPressionada(VK_CONTROL)) {
			pontoNak.x = posicaoMouse.x + camera.x;
			pontoNak.y = posicaoMouse.y + camera.y;
		}

		// ---------------------------------------------------
		__try {
			grafico = new Gdiplus::Graphics(dc);
			__try {
				posicaoTelaX = posicaoBot.x - camera.x + px;
				posicaoTelaY = posicaoBot.y - camera.y + py;

				diametroX = raio * cos(intToRadian(anguloBot));
				diametroY = raio * sin(intToRadian(anguloBot));

				//
				// Seta o tempo de rotação do circulo de alvo
				rotacoes[0].tick = (random(10) * 0.01) / tempoVirtual;
				// random(15)*0.1;
				rotacoes[1].tick = (random(20) * 0.01) / tempoVirtual;
				// random(20)*0.1;
				rotacoes[2].tick = (random(15) * 0.01) / tempoVirtual;
				// random(10)*0.1;
				rotacoes[3].tick = (random(30) * 0.01) / tempoVirtual;
				// random(80)*0.1;

				//
				// Processa rotações do círculo de alvo
				processaRotacao(&rotacoes[0]);
				processaRotacao(&rotacoes[1]);
				processaRotacao(&rotacoes[2]);
				processaRotacao(&rotacoes[3]);

				if (posicaoTelaY < 500)
					desenhaRotacao(rotacoes[0], linhas[TL_BRANCO].pen, posicaoTelaX, posicaoTelaY, 35, 3);

				raios[0].raioMax = 15;
				raios[0].raioMin = 13;
				raios[0].tick = random(15) * 0.01;
				processaTempo(&raios[0]);

				if (posicaoTelaY < 500)
					grafico->DrawEllipse(linhas[TL_BRANCO].pen, posicaoTelaX - raios[0].raioAtual, posicaoTelaY - raios[0].raioAtual, 2 * raios[0].raioAtual, 2 * raios[0].raioAtual);

				posicaoTelaX = posicaoBot.x - camera.x;
				posicaoTelaY = posicaoBot.y - camera.y;

				if (posicaoTelaY <= 515)
					grafico->DrawLine(linhas[TL_BRANCO].pen, posicaoTelaX, posicaoTelaY, posicaoTelaX + diametroX, posicaoTelaY - diametroY);

				// linhaVertical

				if (modoMira < 2 || modoMira >= 3) {
					posicaoTelaX = posicaoAlvo.x - camera.x;
					posicaoTelaY = posicaoAlvo.y - camera.y;
					/*
					grafico->DrawEllipse(corCirculo1, posicaoTelaX - raio, posicaoTelaY - raio, 2*raio, 2*raio);
					grafico->DrawLine(linhas[TL_BRANCO].pen, posicaoTelaX, posicaoTelaY - raio, posicaoTelaX, posicaoTelaY + raio);
					grafico->DrawLine(linhas[TL_BRANCO].pen, posicaoTelaX - raio, posicaoTelaY, posicaoTelaX + raio, posicaoTelaY);
					 */
					// desenhaRotacao(rotacoes[2], linhas[TL_BRANCO].pen, posicaoTelaX, posicaoTelaY, 32, 2);
					int raioAlvo = (modoMira == 3) ? 10 : 20;
					desenhaRotacao(rotacoes[2], linhas[TL_BRANCO].pen, posicaoTelaX, posicaoTelaY, raioAlvo, 6);
					desenhaRotacao(rotacoes[1], linhas[TL_LINHA2].pen, posicaoTelaX, posicaoTelaY, 3, 8);
					grafico->DrawEllipse(linhas[TL_BRANCO].pen, posicaoTelaX - raios[0].raioAtual, posicaoTelaY - raios[0].raioAtual, 2 * raios[0].raioAtual, 2 * raios[0].raioAtual);
				}

				if (interfaceJogo == 1) {
					grafico->DrawLine(linhas[TL_BRANCO].pen, 389 + forcaBot, 565, 389 + forcaBot, 585);
				}
				else {
					grafico->DrawLine(linhas[TL_BRANCO].pen, 241 + forcaBot, 565, 241 + forcaBot, 585);
				}

				flagEspelho = false;

				// angulo += ((mobile == 24)?BotInfos->teste1.y:0);

				Alfa = cos(intToRadian((float)anguloBot)) + (direita ? -0.002 : 0.002);
				Alfa = ((mobile == 12) ? (cos(intToRadian((float)anguloBot)) - (0.053 - (0.0002 * anguloBot))) : Alfa);
				Beta = sin(intToRadian((float)anguloBot));
				Gama = (float)cos(intToRadian(anguloVento)); // ( (cos(intToRadian(BotInfos->anguloVento)) < 0)?0.041:-0.03) + BotInfos->maya.faseBola1;
				Delta = (float)sin(intToRadian(anguloVento));

				// TEfeitoVento efeitoFix = fixEfects();

				// fatorForca = BotInfos->efeitoVento; + efeitoFix.efeito;

				// velocidadeTiroX = ((Alfa * BotInfos->forcaBot * fatorForca) / 100.0);
				// velocidadeTiroY = ((Beta * BotInfos->forcaBot * fatorForca) / 100.0);

				// TCalculador *calculador = new TCalculador();

				velocidadeTiroX = (Alfa * forcaBot);
				velocidadeTiroY = (Beta * forcaBot);

				x = posicaoBot.x;
				y = posicaoBot.y;

				// debugar("gravidadeGB: %f", (float)gravidadeGB);
				// debugar("forcaBot: %f", (float)forcaBot);
				// debugar("forcaVento: %f", (float)forcaVento);
				// debugar("anguloBot: %d", anguloBot);
				// debugar("efeitoVento: %f", (float)efeitoVento);

				gravidade = gravidadeGB + ((mobile == 12) ? (0.004 * anguloBot - 0.01) : 0); // - ((mobile == 12)?(23.7-(0.2*angulo)):0) + ((mobile == 12 && angulo > 85)?3:0);

				aceleracaoVentoX = (int)((float)Gama  * (float)forcaVento);//+ ((Gama < 0) ? -1 : 1) * 0.0001f)) * efeitoVento;
				aceleracaoVentoY = (int)((float)Delta * (float)forcaVento);//+ ((Delta < 0) ? -1 : 1) * 0.0001f)) * (efeitoVento);

				if(efeitoVento == 0)
					efeitoVento = 1;

				c1 = (( velocidadeTiroX - aceleracaoVentoX ) / efeitoVento);
				c2 = ( velocidadeTiroY / efeitoVento ) - ((efeitoVento * aceleracaoVentoY - gravidade) / (efeitoVento * efeitoVento));

				// gravidade += efeitoFix.gravidade;

				// Se estiver no modo tornado, desenha a linha na posicao do mouse
				if (linhaVertical) { // tornados.modoTornado
					grafico->DrawLine(linhas[TL_BRANCO].pen, posicaoMouse.x, 0, posicaoMouse.x, 510);
				}

				temTornado = tornados.temTornado();

				// Desenha as linhas de tornado
				for (int i = 0; i < 4; i++) {
					if (tornados.tornados[i].completo()) {
						int x1 = (tornados.tornados[i].inicio - camera.x);
						int x2 = (tornados.tornados[i].fim - camera.x);
						if (x1 >= 0 && x1 <= 800) {
							if (tornados.tornados[i].espelho) {
								int resto = (660 - camera.y);
								if (resto <= 515)
									grafico->DrawLine(linhas[TL_ESPELHO].pen, x1, resto, x1, 515);
							}
							else
								grafico->DrawLine(linhas[TL_TORNADO].pen, x1, 0, x1, 510);
						}
						if (x2 >= 0 && x2 <= 800) {
							if (tornados.tornados[i].espelho) {
								int resto = (660 - camera.y);
								if (resto <= 515) {
									grafico->DrawLine(linhas[TL_ESPELHO].pen, x2, resto, x2, 515);
									grafico->DrawLine(linhas[TL_ESPELHO].pen, x2, resto + 10, x1, resto + 10);
								}
							}
							else {
								grafico->DrawLine(linhas[TL_TORNADO].pen, x2, 0, x2, 515);
								grafico->DrawLine(linhas[TL_TORNADO].pen, x1, 150, x2, 150);
							}
						}
					}
					else {
						if (tornados.tornados[i].etapa == 1) {
							int x1 = (tornados.tornados[i].inicio - camera.x);
							if (x1 >= 0 && x1 <= 800) {
								if (tornados.tornados[i].espelho) {
									int resto = (660 - camera.y);
									if (resto <= 515)
										grafico->DrawLine(linhas[TL_ESPELHO].pen, x1, resto, x1, 515);
								}
								else
									grafico->DrawLine(linhas[TL_TORNADO].pen, x1, 0, x1, 510);
							}
						}
					}
				}

				Projetil[0] = TProjetil(anguloBot, x, y, velocidadeTiroX, velocidadeTiroY);
				velocidadeProjetil.x = 0;
				velocidadeProjetil.y = 0;
				aceleracaoVento.x = aceleracaoVentoX;
				aceleracaoVento.y = aceleracaoVentoY;
				novaGravidade = gravidade;

				pontoEstatico[0].x = x;
				pontoEstatico[0].y = y;
				ultimoPontoEstatico[0] = pontoEstatico[0];

				if (teclaPressionada(VK_CONTROL) && teclaPressionada(VK_SHIFT)) {
					int indice = 0;
					int menorDistManual = 9999;
					for (int i = 0; i < 8; i++) {
						if (semiPosicaoBot[i].x > 0 && semiPosicaoBot[i].y > 0) {
							int a = (semiPosicaoBot[i].x - (posicaoMouse.x + camera.x));
							int b = (semiPosicaoBot[i].y - (posicaoMouse.y + camera.y));

							double distancia = sqrt(modulo(a * a) + modulo(b * b));
							if (distancia < menorDistManual) {
								menorDistManual = distancia;
								indice = i;
							}
						}
					}
					raioAlvoManual += 0.08f / tempoVirtual;
					if (raioAlvoManual >= 60)
						raioAlvoManual = 20;
					alvoManualID = indice;
					grafico->DrawEllipse(linhas[TL_BRANCO].pen, semiPosicaoBot[indice].x - camera.x - raioAlvoManual, semiPosicaoBot[indice].y - camera.y - raioAlvoManual, 2 * raioAlvoManual, 2 * raioAlvoManual);
				}

				ultimoProjetil[0].x = Projetil[0].x;
				ultimoProjetil[0].y = Projetil[0].y;
				Projetil[0].x = x;
				Projetil[0].y = y;
				Projetil[0].inverter = false;

				switch(mobile) {
				case 0:
					fillArmor();
					break;
				case 1:
					fillMage();
					break;
				case 2:
					fillNak();
					break;
				case 3:
					fillTrico();
					break;
				case 4:
					fillBigFoot();
					break;
				case 5:
					fillBoomer();
					break;
				case 6:
					fillRaon();
					break;
				case 7:
					fillLightning();
					break;
				case 8:
					fillJD();
					break;
				case 9:
					fillASate();
					break;
				case 10:
					fillIce();
					break;
				case 11:
					fillTurtle();
					break;
				case 12:
					fillGrub();
					break;
				case 13:
					fillAduka();
					break;
				case 14:
					fillKalsiddon();
					break;
				case 15:
					fillJFrog();
					break;
				case 16:
					fillDragon();
					break;
				case 17:
					fillKnight();
					break;
				case 19:
					fillPhoenix();
					break;
				case 20:
					fillMaya();
					break;
				case 21:
					fillWolf();
					break;
				case 22:
					fillTiburon();
					break;
				case 23:
					fillBlueWhale();
					break;
				case 24:
					fillFrank();
					break;
				default:
					break;
				}

				// grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(BotInfos->melhorForca.ponto.x - camera.x), (int)(BotInfos->melhorForca.ponto.y - camera.y), (int)(BotInfos->posicaoAlvo.x - camera.x), (int)(BotInfos->posicaoAlvo.y - camera.y));
				// grafico->DrawEllipse(linhas[TL_BRANCO].pen, BotInfos->posicaoAlvo.x - camera.x - 3, BotInfos->posicaoAlvo.y - camera.y - 3, 6, 6);

			}
			__finally {
				delete grafico;
			}

		}
		__except (1) {
        	debugar("Erro 1818");
			return;
		}
	}
	__except (1) {
	}
	// VMProtectEnd();
}

void __fastcall TShMatrix::calculaTornado() {
	// VMProtectBegin("tornado");
	if (!temTornado)
		return;

	__try {
		for (int i = 0; i < 5; i++) {
			if (tornados.tornados[i].completo()) {
				for (int j = 0; j < count; j++) {
					bool dentroDoTornado = (ultimoProjetil[j].x >= tornados.tornados[i].inicio && ultimoProjetil[j].x <= tornados.tornados[i].fim);
					if (Projetil[j].x >= tornados.tornados[i].inicio && Projetil[j].x <= tornados.tornados[i].fim && !tornadosFlag[i] && !dentroDoTornado) {
						if (!tornados.tornados[i].espelho) {
							tornadosFlag[i] = true;
							BOOL direita = ((Projetil[j].x - ultimoProjetil[j].x) > 0);

							// Calcula o angulo de inclinação da trajetória no momento do encontro com o tornado
							float distX = (float)((float)Projetil[j].x - (float)ultimoProjetil[j].x);
							float distY = (-1) * (float)((float)Projetil[j].y - (float)ultimoProjetil[j].y);

							// Evita divisão por zero
							distX = (distX == 0.0) ? 1.0 : distX;
							float angulo = ((float)distY / (float)distX);
							angulo = radianToInt(atan(angulo));

							if (!direita)
								angulo = (180 + angulo);

							if (angulo < 0)
								angulo += 360;

							// Verifica se o tiro vem do lado da linha 1 ou 2 do tornado
							BOOL pertoDoPrimeiro = ((tornados.tornados[i].inicio - ultimoProjetil[j].x) > 0);

							// Calcula o ponto exato de encontro com o tornado
							float novoX = ((pertoDoPrimeiro) ? (float)modulo(tornados.tornados[i].inicio - ultimoProjetil[j].x) : (float)modulo(tornados.tornados[i].fim - ultimoProjetil[j].x));
							float tgAngulo = (distY / distX);
							float novoY = (novoX * tgAngulo);

							// corrige a inversao de sinal do inicio
							distY *= -1;

							Projetil[j].x = ((pertoDoPrimeiro) ? tornados.tornados[i].inicio : tornados.tornados[i].fim);
							Projetil[j].y = Projetil[j].y - distY + (pertoDoPrimeiro ? -novoY : novoY);

							// Termina de desenhar a trajetória até a posição do tornado
							grafico->DrawLine(linhas[(modoEspelho) ? TL_ESPELHO : TL_TORNADO].pen, ultimoProjetil[j].x - camera.x, ultimoProjetil[j].y - camera.y, Projetil[j].x - camera.x, Projetil[j].y - camera.y);
							// grafico->DrawEllipse(linhas[TL_BRANCO].pen, Projetil[j].x - camera.x - 3, Projetil[j].y - camera.y - 3, 6, 6);
							desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (Projetil[j].x - camera.x), (Projetil[j].y - camera.y), 3, 2, 2);

							// Linha segue para a outra ponta do tornado
							float proximoX = ((pertoDoPrimeiro) ? tornados.tornados[i].fim : tornados.tornados[i].inicio);

							// Largura dp Tornado, para calcular o Y de subida do tiro
							float larguraTornado = (float)modulo(tornados.tornados[i].fim - tornados.tornados[i].inicio);

							// Angulo para a direita inverte o sinal
							if (angulo < 90.0 || angulo > 270.0) {
								tgAngulo *= -1;
							}
							else if (angulo == 90.0 || angulo == 270.0) {
								continue;
							}

							// Tamanho da subida/descida de uma semi-volta no tornado
							float y = (float)((float)tgAngulo * (float)larguraTornado);

							// Volta 1
							grafico->DrawLine(linhas[modoEspelho ? TL_ESPELHO : TL_TORNADO].pen, Projetil[j].x - camera.x, Projetil[j].y - camera.y, (float)proximoX - camera.x, (float)((float)Projetil[j].y - camera.y + (float)y));
							// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(proximoX-camera.x-3), (int)(Projetil[j].y - camera.y + y - 3), 6, 6);
							desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (proximoX - camera.x), (Projetil[j].y - camera.y + y), 3, 2, 2);

							// Volta 2
							Projetil[j].y += y;
							Projetil[j].x = proximoX;
							proximoX = ((pertoDoPrimeiro) ? tornados.tornados[i].inicio : tornados.tornados[i].fim);

							grafico->DrawLine(linhas[modoEspelho ? TL_ESPELHO : TL_TORNADO].pen, Projetil[j].x - camera.x, Projetil[j].y - camera.y, (float)proximoX - camera.x, (float)((float)Projetil[j].y - camera.y + (float)y));
							// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(proximoX - camera.x - 3), (int)(Projetil[j].y - camera.y + y - 3), 6, 6);
							desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (proximoX - camera.x), (Projetil[j].y - camera.y + y), 3, 2, 2);

							// Volta 3
							Projetil[j].y += y;
							Projetil[j].x = proximoX;
							proximoX = ((pertoDoPrimeiro) ? tornados.tornados[i].fim : tornados.tornados[i].inicio);

							// Define variáveis da equação da reta (aplicação na formula da phoenix)
							reta.pa = Projetil[j].y - ((float)Projetil[j].y + (float)y);
							reta.pb = (float)proximoX - Projetil[j].x;
							reta.pc = Projetil[j].x * ((float)Projetil[j].y + (float)y) - Projetil[j].y * (float)proximoX;

							grafico->DrawLine(linhas[modoEspelho ? TL_ESPELHO : TL_TORNADO].pen, Projetil[j].x - camera.x, Projetil[j].y - camera.y, (float)proximoX - camera.x, (float)((float)Projetil[j].y - camera.y + (float)y));
							// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(proximoX - camera.x-3), (int)(Projetil[j].y - camera.y+y-3), 6, 6);
							desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (proximoX - camera.x), (Projetil[j].y - camera.y + y), 3, 2, 2);

							Projetil[j].y += y;
							Projetil[j].x = proximoX;

							ultimoProjetil[j].x = Projetil[j].x;
							ultimoProjetil[j].y = Projetil[j].y;
						}
						else if (Projetil[j].y > 660) {
							Projetil[j].inverter = !Projetil[j].inverter;
							Projetil[j].ultimoSpeedX *= -1;

							tornadosFlag[i] = true;
							BOOL direita = ((Projetil[j].x - ultimoProjetil[j].x) > 0);

							// Calcula o angulo de inclinação da trajetória no momento do encontro com o espelho
							float distX = (float)((float)Projetil[j].x - (float)ultimoProjetil[j].x);
							float distY = (-1) * (float)((float)Projetil[j].y - (float)ultimoProjetil[j].y);

							// Evita divisão por zero
							distX = (distX == 0.0) ? 1.0 : distX;
							float angulo = ((float)distY / (float)distX);
							angulo = radianToInt(atan(angulo));

							if (!direita)
								angulo = (180 + angulo);

							if (angulo < 0)
								angulo += 360;

							// Verifica se o tiro vem do lado da linha 1 ou 2 do espelho
							BOOL pertoDoPrimeiro = ((tornados.tornados[i].inicio - ultimoProjetil[j].x) > 0);

							// Calcula o ponto exato de encontro com o espelho
							float novoX = ((pertoDoPrimeiro) ? (float)modulo(tornados.tornados[i].inicio - ultimoProjetil[j].x) : (float)modulo(tornados.tornados[i].fim - ultimoProjetil[j].x));
							float tgAngulo = (distY / distX);
							float novoY = (novoX * tgAngulo);
							float resto = (distX - novoX);

							// corrige a inversao de sinal do inicio
							distY *= -1;

							if ((Projetil[j].y - distY + novoY) > 360) {
								Projetil[j].x = ((pertoDoPrimeiro) ? tornados.tornados[i].inicio : tornados.tornados[i].fim);
								Projetil[j].y = Projetil[j].y - distY + (pertoDoPrimeiro ? -novoY : novoY);

								// Termina de desenhar a trajetória até a posição do espelho
								grafico->DrawLine(linhas[modoEspelho ? TL_ESPELHO : TL_TORNADO].pen, ultimoProjetil[j].x - camera.x, ultimoProjetil[j].y - camera.y, Projetil[j].x - camera.x, Projetil[j].y - camera.y);
								// grafico->DrawEllipse(linhas[TL_BRANCO].pen, Projetil[j].x - camera.x - 3, Projetil[j].y - camera.y - 3, 6, 6);
								desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (Projetil[j].x - camera.x), (Projetil[j].y - camera.y), 3, 2, 2);
								flagEspelho = !flagEspelho;
							}

							double x2 = ultimoProjetil[j].x;
							double y2 = ((Projetil[j].y - ultimoProjetil[j].y) * 2 + ultimoProjetil[j].y);

							// Variáveis da equação da reta
							reta.pa = (Projetil[j].y - y2);
							reta.pb = (x2 - Projetil[j].x);
							reta.pb = ((reta.pb == 0) ? 1 : reta.pb);
							reta.pc = (Projetil[j].x * y2 - Projetil[j].y * x2);

							ultimoProjetil[j].x = Projetil[j].x;
							ultimoProjetil[j].y = Projetil[j].y;

							// Termina o pedaço da trajetória após a inversão da mesma
							Projetil[j].x -= resto;
							Projetil[j].y = (((-1 * reta.pa * Projetil[j].x) - reta.pc) / reta.pb);

							grafico->DrawLine(linhas[modoEspelho ? TL_ESPELHO : TL_TORNADO].pen, ultimoProjetil[j].x - camera.x, ultimoProjetil[j].y - camera.y, Projetil[j].x - camera.x, Projetil[j].y - camera.y);

							ultimoProjetil[j].x = Projetil[j].x;
							ultimoProjetil[j].y = Projetil[j].y;
						}
					}
					else {
						tornadosFlag[i] = false;
					}
				}
			}
		}
	}
	__except (1) {
	}
	// VMProtectEnd();
}

/*
void __fastcall TAimBot::fixPos(int indice){
Projetil[indice].xInicial += (Projetil[indice].x - ultimoProjetil[indice].x);
Projetil[indice].yInicial += (Projetil[indice].y - ultimoProjetil[indice].y);
} */

void __fastcall TShMatrix::calcular(int indice, float t) {
	// VMProtectBegin("calc");
	__try {
		// ultimoPontoEstatico[indice] = pontoEstatico[indice];

		/*
		velocidadeProjetil.x += Projetil[indice].speedX * 0.05;
		velocidadeProjetil.y += Projetil[indice].speedY * 0.05;

		aceleracaoVento.x += aceleracaoVentoX * 0.05 * 0.05;
		aceleracaoVento.y += aceleracaoVentoY * 0.05 * 0.05;

		novaGravidade += gravidade * 0.05 * 0.05;

		pontoEstatico[indice].x += velocidadeProjetil.x + aceleracaoVento.x;
		pontoEstatico[indice].y -= velocidadeProjetil.y - aceleracaoVento.y + 0.5 * novaGravidade;
		 *//*
		aceleracaoVentoX += aceleracaoVentoX * 0.05 * 0.05;
		aceleracaoVentoY += aceleracaoVentoY * 0.05 * 0.05;

		gravidade += gravidade * 0.05 * 0.05; */

		// pontoEstatico[indice].x = Projetil[indice].xInicial + x + Projetil[indice].speedX * t + aceleracaoVentoX  * t * t;
		// pontoEstatico[indice].y = Projetil[indice].yInicial + y - Projetil[indice].speedY * t - aceleracaoVentoY * t * t + 0.5 * gravidade * t * t;
		/* if(0 == 0){
		Projetil[indice].x += (Projetil[indice].speedX * (Projetil[indice].inverter?-1:1) * tick) + 2 * aceleracaoVentoX * (Projetil[indice].inverter?0:1) * t * tick;
		Projetil[indice].y -= (Projetil[indice].speedY * tick) + (2) * aceleracaoVentoY * t * tick - (gravidade * t * tick);
		}else{ */
		/* Projetil[indice].x += Projetil[indice].speedX * tick;// + aceleracaoVentoX*tick*tick*0.5;
		Projetil[indice].y -= Projetil[indice].speedY * tick;// + aceleracaoVentoY*tick*tick*0.5;
		Projetil[indice].speedX += aceleracaoVentoX * tick;
		Projetil[indice].speedY += aceleracaoVentoY * tick; */

		// *(Projetil[indice].inverter?0:1)
		/*Projetil[indice].x += Projetil[indice].speedX * tick * (Projetil[indice].inverter ? -1 : 1) + Projetil[indice].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5;
		Projetil[indice].y -= Projetil[indice].speedY * tick + Projetil[indice].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
		// if(Projetil[indice].inverter)
		// Projetil[indice].ultimoSpeedX = 0;
		Projetil[indice].ultimoSpeedX += aceleracaoVentoX * tick;
		Projetil[indice].ultimoSpeedY += (aceleracaoVentoY - gravidade) * tick;
		calculaTornado();    */

		Projetil[indice].x = x + (aceleracaoVentoX * t + c1);  //Projetil[indice].speedX * tick;// + aceleracaoVentoX * tick * tick * 0.5;
		Projetil[indice].y = y - (((( (-1 * gravidade) + (efeitoVento * aceleracaoVentoY) ) * t) / efeitoVento) + c2);
        calculaTornado();

		// }

		// if(flagEspelho){
		// Projetil[indice].x -= (pontoEstatico[indice].x - ultimoPontoEstatico[indice].x);
		// Projetil[indice].y += (pontoEstatico[indice].y - ultimoPontoEstatico[indice].y);
		// }else{ */
		// Projetil[indice].x += (pontoEstatico[indice].x - ultimoPontoEstatico[indice].x);
		// Projetil[indice].y += (pontoEstatico[indice].y - ultimoPontoEstatico[indice].y);
		// }
	}
	__except (1) {
	}
	// VMProtectEnd();
}

void __fastcall TShMatrix::desenhaLinha(int indice, bool flag) {
	// VMProtectBegin("linha");
	if ((Projetil[indice].x > (camera.x - 20)) && (Projetil[indice].x < (camera.x + 820)) && (Projetil[indice].y > (camera.y - 20)) && (Projetil[indice].y < (camera.y + 515))) {
		if (indice == 0 && !flag) {
			grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[indice].x - camera.x)+PontoInicialX, (int)(ultimoProjetil[indice].y - camera.y)+PontoInicialY, (int)(Projetil[indice].x - camera.x)+PontoInicialX, (int)(Projetil[indice].y - camera.y)+PontoInicialY);
		}
		else {
			if (ultimoProjetil[indice].x != 0 && ultimoProjetil[indice].y != 0)
				grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(ultimoProjetil[indice].x - camera.x)+PontoInicialX, (int)(ultimoProjetil[indice].y - camera.y)+PontoInicialY, (int)(Projetil[indice].x - camera.x)+PontoInicialX, (int)(Projetil[indice].y - camera.y)+PontoInicialY);
		}
	}

	// for(int i = 0; i < count; i++){
	ultimoProjetil[indice].x = Projetil[indice].x;
	ultimoProjetil[indice].y = Projetil[indice].y;
	// VMProtectEnd();
	// }
}

void __fastcall TShMatrix::fillArmor() {
	__try {
		count = 1;
		int semiTempo = 405;
		int tempo = 0;
		bool flagSS = false;
		bool primeiro = true;
		int j = 0;

		for (double t = 0; t <= 50; t += tick) {
			j++;
			tempo += 5;

			calcular(0, j * tickP);

			if (tempo == semiTempo && tiro == 2) {
				// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(ultimoProjetil[0].x - camera.x - 3), (int)(ultimoProjetil[0].y - camera.y - 3), 6, 6);
				desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (ultimoProjetil[0].x - camera.x), (ultimoProjetil[0].y - camera.y), 3, 2, 2);
				flagSS = true;
			}

			desenhaLinha(0, flagSS);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
		debugar("Erro 7770");
		return;
	}
}

void __fastcall TShMatrix::fillMage() {
	__try {
		count = 1;
		int tempo = 0;
		int indiceTempo = 0;
		bool primeiro = true;
		bool flagSS = false;

		for (double t = 0; t <= 50; t += tick) {
			tempo += 5;

			calcular(0, t);

			// magePoints[(int)BotInfos->teste1.x] = BotInfos->teste1.y*5;

			int semiTempo = magePoints[indiceTempo];

			if (tiro == 1 && ((tempo - 5) == semiTempo)) {
				tempo = 0;
				if (indiceTempo >= 40)
					indiceTempo = 37;
				// if(indiceTempo == BotInfos->teste1.x)
				// grafico->DrawEllipse(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[0].x - camera.x - 3), (int)(ultimoProjetil[0].y - camera.y - 3), 6, 6);
				// else
				// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(ultimoProjetil[0].x - camera.x - 2), (int)(ultimoProjetil[0].y - camera.y - 2), 4, 4);
				desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (ultimoProjetil[0].x - camera.x), (ultimoProjetil[0].y - camera.y), 3, 2, 2);
				indiceTempo++;
			}
			desenhaLinha(0, flagSS);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7771");
		return;
	}
}

void __fastcall TShMatrix::fillMage2() {
	__try {

		count = 1;
		bool primeiro = true;
		bool flagMaya = false;

		for (double t = 0; t <= 12; t += tick) {
			// double fixX = (0.00023333 * BotInfos->anguloVento) + 0.958;
			// double fixY = 0.00133333 * (BotInfos->anguloVento - 90) + 0.958;
			Projetil[0].x += (Projetil[0].speedX * (Projetil[0].inverter ? -1 : 1) * tick) + 2 * aceleracaoVentoX * (t + tick * 0.5) * tick;
			// BotInfos->maya.faseBola1
			Projetil[0].y -= (Projetil[0].speedY * tick) + 2 * aceleracaoVentoY * (t + tick * 0.5) * tick - (gravidade * (t + tick * 0.5) * tick);

			if (temTornado)
				calculaTornado();

			for (int i = 0; i < count; i++) {
				if ((Projetil[i].x > (camera.x - 20)) && (Projetil[i].x < (camera.x + 820)) && (Projetil[i].y > (camera.y - 20)) && (Projetil[i].y < (camera.y + 525))) {
					if (i == 0) {
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
					}
					else {
						if (!primeiro)
							grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
					}
				}
			}

			for (int i = 0; i < count; i++) {
				primeiro = false;
				ultimoProjetil[i].x = Projetil[i].x;
				ultimoProjetil[i].y = Projetil[i].y;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7772");
		return;
	}

}

void __fastcall TShMatrix::fillNak() {
	__try {
		count = 1;
		int tempo = 0;
		bool primeiro = true;
		bool flagNak = false;
		int descidaCount = 0;

		for (double t = 0; t <= 50; t += tick) {
			tempo += 5;
			int novoY = pontoNak.y;
			// calcular(0,t);

			if ((int)Projetil[0].y > (int)ultimoProjetil[0].y) {
				descidaCount++;
			}

			if (!flagNak) {
				calcular(0, t);
			}
			else {
				Projetil[0].x += Projetil[0].speedX * tick * (Projetil[0].inverter ? -1 : 1) + Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5;
				Projetil[0].y -= Projetil[0].speedY * tick * (1.48) + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
				// Projetil[0].ultimoSpeedX += aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY += -gravidade * (-2.4) * tick;
				calculaTornado();
			}

			if (opcoesTiro[MB_NAK].tiroEspecial && tiro == 1) {
				if (!flagNak && (int)Projetil[0].y > (int)ultimoProjetil[0].y && Projetil[0].y >= novoY) {
					flagNak = true;
					// Projetil[0].speedY *= (1.55+BotInfos->teste1.y);

					if ((int)ultimoProjetil[0].x != (int)Projetil[0].x || (int)ultimoProjetil[0].y != (int)Projetil[0].y) {
						TReta segmento;
						segmento.pa = (ultimoProjetil[0].y - Projetil[0].y);
						segmento.pb = (Projetil[0].x - ultimoProjetil[0].x);
						segmento.pc = (ultimoProjetil[0].x * Projetil[0].y - ultimoProjetil[0].y * Projetil[0].x);
						segmento.pb = ((segmento.pb == 0) ? 1 : segmento.pb);
						int novoX = (-1 * segmento.pb * novoY - segmento.pc) / segmento.pa;
						Projetil[0].x = novoX;
						Projetil[0].y = novoY;
						desenhaLinha(0);
						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;
					}

					desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (Projetil[0].x - camera.x), (Projetil[0].y - camera.y), 3, 2, 2);
				}
			}

			// calculaTornado();
			desenhaLinha(0);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7773");
		return;
	}
}

void __fastcall TShMatrix::fillTrico() {
	__try {

		// debugar("Trico: %d %d", opcoesTiro[MB_TRICO].tiroEspecial, opcoesTiro[MB_TRICO].marcarSS);
		count = ((opcoesTiro[MB_TRICO].tiroEspecial && tiro == 1) ? 3 : 1);
		int tempo = 0;
		bool primeiro = true;
		int flagInicio = 0;
		bool flagDesenho = false;
		/*
		for(int i = 1; i < count; i++){
		pontoEstatico[i] = TPoint(x,y);
		ultimoPontoEstatico[i] = pontoEstatico[i];
		}
		 */
		// debugar("Calculando Trico...");
		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (double t = 0; t <= 50; t += tick) {
			tempo += 5;
			calcular(0, t);

			if (tempo == 105 && tiro == 1 && opcoesTiro[MB_TRICO].tiroEspecial) {
				flagDesenho = true;

				// grafico->DrawLine(linhas[TL_LINHA2].pen, (int) (BotInfos->posicaoBot.x - camera.x), (int)(BotInfos->posicaoBot.y - camera.y), (int)(Projetil[2].x - camera.x), (int)(ultimoProjetil[2].y - camera.y));

				TReta segmento;
				for (int n = 1; n <= 2; n++) {
					int ultimoX = ultimoProjetil[n].x;
					int ultimoY = ultimoProjetil[n].y;

					int difX = ultimoProjetil[n].x - posicaoBot.x;
					int countX = difX / 5;
					int posInicio = (ultimoProjetil[n].x < posicaoBot.x) ? ultimoProjetil[n].x : posicaoBot.x;
					int posFim = (ultimoProjetil[n].x < posicaoBot.x) ? posicaoBot.x : ultimoProjetil[n].x;

					segmento.pa = (ultimoProjetil[n].y - posicaoBot.y);
					segmento.pb = (posicaoBot.x - ultimoProjetil[n].x);
					segmento.pc = (ultimoProjetil[n].x * posicaoBot.y - ultimoProjetil[n].y * posicaoBot.x);
					segmento.pb = ((segmento.pb == 0) ? 1 : segmento.pb);

					for (int i = posInicio; i < posFim; i++) {
						if (i > posFim) {
							i = posFim;
						}
						int novoY = ((-1 * segmento.pa * i - segmento.pc) / segmento.pb);
						if ((novoY - camera.y) <= 515) {
							grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(i - camera.x), (int)(novoY - camera.y), (int)(ultimoX - camera.x), (int)(ultimoY - camera.y));
						}

						ultimoX = i;
						ultimoY = novoY;
					}
				}

			}

			if (opcoesTiro[MB_TRICO].tiroEspecial && tiro == 1) {
				int tricoRaio = 43;
				float velocidadeAngularTrico = ((direita) ? 7 : -7.3);
				int faseTrico = ((direita) ? 172 : -55);

				float tempoTrico = t * 10.0f;

				int anguloRepolho1 = faseTrico + ((direita) ? 180 : 0) + (int)(velocidadeAngularTrico * tempoTrico);
				int anguloRepolho2 = faseTrico + ((!direita) ? 180 : 0) + (int)(velocidadeAngularTrico * tempoTrico);

				POINT repolho1;
				POINT repolho2;
				repolho1.x = tricoRaio * cos(intToRadian(anguloRepolho1));
				repolho1.y = tricoRaio * sin(intToRadian(anguloRepolho1));
				repolho2.x = tricoRaio * cos(intToRadian(anguloRepolho2));
				repolho2.y = tricoRaio * sin(intToRadian(anguloRepolho2));

				repolho1.x = (Projetil[0].x + repolho1.x);
				repolho1.y = (Projetil[0].y + repolho1.y);
				repolho2.x = (Projetil[0].x + repolho2.x);
				repolho2.y = (Projetil[0].y + repolho2.y);

				// fixPos(1);
				// fixPos(2);

				Projetil[1].x = repolho1.x;
				Projetil[1].y = repolho1.y;
				Projetil[2].x = repolho2.x;
				Projetil[2].y = repolho2.y;
			}

			calculaTornado();
			/*
			if(i > 0 && tempo == 70){
			grafico->DrawLine(linhas[TL_LINHA2].pen, (int) (BotInfos->posicaoBot.x - camera.x), (int)(BotInfos->posicaoBot.y - camera.y), (int)(ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y));
			flagInicio++;
			primeiro = false;
			} */

			for (int i = 0; i < count; i++) {
				if (flagDesenho || i == 0) {
					desenhaLinha(i);
				}
				else {
					ultimoProjetil[i].x = Projetil[i].x;
					ultimoProjetil[i].y = Projetil[i].y;
				}
			}

			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7774");
		return;
	}
}

void __fastcall TShMatrix::fillBigFoot() {
	__try {
		bool flagBigFoot = opcoesTiro[MB_BIG_FOOT].tiroEspecial;

		if (flagBigFoot) {
			if (tiro == 1)
				count = 6;
			else
				count = 4;
		}
		else {
			count = 1;
		}

		bool primeiro = true;

		if (flagBigFoot) {
			if (tiro == 0) {
				for (int i = 0; i < count; i++) {
					float anguloEspecial = 0;
					float forcaBotBB = forcaBot;
					switch(i) {
					case 1:
						anguloEspecial = 1;
						forcaBotBB += 5.4;
						break;
					case 2:
						anguloEspecial = 0;
						forcaBotBB += 15.4;
						break;
					case 3:
						anguloEspecial = 0;
						forcaBotBB += -3.9;
						break;
					case 0:
						anguloEspecial = -1;
						forcaBotBB += 5.6;
						break;
					default:
						break;
					}

					float BAlfa = cos(intToRadian((float)anguloBot + anguloEspecial)) + (direita ? -0.002 : 0.002);
					float BBeta = sin(intToRadian((float)anguloBot + anguloEspecial));
					float BvelocidadeTiroX = BAlfa * forcaBotBB;
					float BvelocidadeTiroY = BBeta * forcaBotBB;

					Projetil[i] = TProjetil(anguloBot + anguloEspecial, x, y, BvelocidadeTiroX, BvelocidadeTiroY);
				}
			}
			else if (tiro == 1) {
				for (int i = 0; i < count; i++) {
					float anguloEspecial = 0;
					float forcaBotBB = forcaBot;
					switch(i) {
					case 0:
						anguloEspecial = -1;
						forcaBotBB += -9;
						break;
					case 1:
						anguloEspecial = -1;
						forcaBotBB += 5;
						break;
					case 2:
						anguloEspecial = -1;
						forcaBotBB += 20;
						break;
					case 3:
						anguloEspecial = 1;
						forcaBotBB += -9;
						break;
					case 4:
						anguloEspecial = 1;
						forcaBotBB += 5;
						break;
					case 5:
						anguloEspecial = 1;
						forcaBotBB += 20;
						break;
					default:
						break;
					}

					float BAlfa = cos(intToRadian((float)anguloBot + anguloEspecial)) + (direita ? -0.002 : 0.002);
					float BBeta = sin(intToRadian((float)anguloBot + anguloEspecial));

					float BvelocidadeTiroX = BAlfa * forcaBotBB;
					float BvelocidadeTiroY = BBeta * forcaBotBB;

					Projetil[i] = TProjetil(anguloBot + anguloEspecial, x, y, BvelocidadeTiroX, BvelocidadeTiroY);
				}
			}
			else {
				for (int i = 0; i < count; i++) {
					float anguloEspecial = 0;
					float forcaBotBB = forcaBot;
					switch(i) {
					case 0:
						anguloEspecial = 0;
						forcaBotBB += 14;
						break;
					case 1:
						anguloEspecial = 0;
						forcaBotBB += -4;
						break;
					case 2:
						anguloEspecial = -1;
						forcaBotBB += 4;
						break;
					case 3:
						anguloEspecial = 1;
						forcaBotBB += 5;
						break;
					default:
						break;
					}

					float BAlfa = cos(intToRadian((float)anguloBot + anguloEspecial)) + (direita ? -0.002 : 0.002);
					float BBeta = sin(intToRadian((float)anguloBot + anguloEspecial));

					float BvelocidadeTiroX = BAlfa * forcaBotBB;
					float BvelocidadeTiroY = BBeta * forcaBotBB;

					Projetil[i] = TProjetil(anguloBot + anguloEspecial, x, y, BvelocidadeTiroX, BvelocidadeTiroY);
				}
			}
		}

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i, flagBigFoot);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
		/*
		for (double t = 0; t <= 30; t += tick){
		calcular(0,t);

		if(flagBigFoot){
		for(int i = 1; i < count; i++){
		calcular(i,t);
		}
		}

		if(temTornado)
		calculaTornado();

		for(int i = 0; i < count; i++){
		if( (Projetil[i].x > (camera.x-20)) && (Projetil[i].x < (camera.x+820)) && (Projetil[i].y > (camera.y-20)) && (Projetil[i].y < (camera.y+525))  ) {
		if(i == 0 && !flagBigFoot){
		grafico->DrawLine(linhas[TL_LINHA1].pen, (int) (ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
		}else{
		if(!primeiro)
		grafico->DrawLine(linhas[TL_LINHA2].pen, (int) (ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
		}
		}
		}

		for(int i = 0; i < count; i++){
		primeiro = false;
		ultimoProjetil[i].x = Projetil[i].x;
		ultimoProjetil[i].y = Projetil[i].y;
		}
		}
		 */
	}
	__except (1) {
    	debugar("Erro 7775");
		return;
	}
}

void __fastcall TShMatrix::fillBoomer() {
	__try {

		count = 1;
		float semiTempo = 250;
		int tempo = 0;
		bool flagSS = false;
		bool flagBoomer = false;
		bool primeiro = true;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				tempo += 5;

				// if(!flagBoomer){
				calcular(0, t);
				/* }else{
				Projetil[0].x += Projetil[0].speedX * tick + Projetil[0].ultimoSpeedX + aceleracaoVentoX*tick*tick*0.5;
				Projetil[0].y -= Projetil[0].speedY * tick + Projetil[0].ultimoSpeedY + aceleracaoVentoY*tick*tick*0.5;
				Projetil[0].ultimoSpeedX += aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY += aceleracaoVentoY * tick;
				calculaTornado();

				aceleracaoVentoX += BotInfos->maya.faseBola1;
				aceleracaoVentoX += BotInfos->maya.faseBola2;
				}
				/*
				if(tempo == 250 && tiro == 2){
				grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(ultimoProjetil[0].x - camera.x - 3), (int)(ultimoProjetil[0].y - camera.y - 3), 6, 6);
				flagSS = true;
				}else if(tempo == (BotInfos->teste1.x*5)){
				flagBoomer = true;

				if(BotInfos->anguloBot > 0){
				float m = tan(intToRadian(BotInfos->anguloBot+BotInfos->teste1.y));
				m = ((m == 0)?1:m);
				int y0 = Projetil[0].y;
				int y1 = camera.y + 515;
				int x0 = Projetil[0].x;
				int x1 = (y1 - y0 + m*x0)/m;

				Projetil[0].x = x1;
				Projetil[0].y = y1;
				desenhaLinha(i,flagSS);
				break;
				}


				} */

				desenhaLinha(i, flagSS);
				if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
					break;
				}
			}
		}

	}
	__except (1) {
		debugar("Erro 7776");
		return;
	}
}

void __fastcall TShMatrix::fillRaon() {
	__try {

		bool primeiro = true;
		bool flagRaon = (tiro == 1 && opcoesTiro[MB_RAON].tiroEspecial);
		count = (flagRaon ? 2 : 1);
		/*
		for(int i = 1; i < count; i++){
		pontoEstatico[i] = TPoint(x,y);
		ultimoPontoEstatico[i] = pontoEstatico[i];
		}
		 */
		if (flagRaon) {
			for (int i = 0; i < count; i++) {
				float anguloEspecial = 0;
				float forcaBotBB = forcaBot;
				switch(i) {
				case 0:
					anguloEspecial = 0;
					forcaBotBB += 14.2;
					break;
				case 1:
					anguloEspecial = 0;
					forcaBotBB += -12.8;
					break;
				default:
					break;
				}

				float BAlfa = cos(intToRadian((float)anguloBot + anguloEspecial)) + (direita ? -0.002 : 0.002);
				float BBeta = sin(intToRadian((float)anguloBot + anguloEspecial));

				float BvelocidadeTiroX = Alfa * forcaBotBB;
				float BvelocidadeTiroY = BBeta * forcaBotBB;

				Projetil[i] = TProjetil(anguloBot + anguloEspecial, x, y, BvelocidadeTiroX, BvelocidadeTiroY);
			}
		}

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 30; t += tick) {
				calcular(i, t);
				desenhaLinha(i, flagRaon);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7777");
		return;
	}
}

void __fastcall TShMatrix::fillLightning() {
	__try {
		count = 1;
		bool primeiro = true;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (double t = 0; t <= 50; t += tick) {
			calcular(0, t);
			desenhaLinha(0);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7778");
		return;
	}
}

void __fastcall TShMatrix::fillJD() {
	__try {
		count = 1;
		bool primeiro = true;

		for (double t = 0; t <= 50; t += tick) {
			calcular(0, t);
			desenhaLinha(0);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7779");
		return;
	}
}

void __fastcall TShMatrix::fillASate() {
	__try {
		count = 1;
		bool primeiro = true;

		for (double t = 0; t <= 50; t += tick) {
			calcular(0, t);
			desenhaLinha(0);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
		debugar("Erro 7780");
		return;
	}
}

void __fastcall TShMatrix::fillIce() {
	__try {
		count = 1;
		bool primeiro = true;

		for (double t = 0; t <= 50; t += tick) {
			calcular(0, t);
			desenhaLinha(0);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7781");
		return;
	}
}

void __fastcall TShMatrix::fillTurtle() {
	__try {
		count = 1;
		int tempo = 0;
		bool primeiro = true;
		bool flagSS = false;
		bool flagTT = false;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			tempo = 0;
			for (double t = 0; t <= 50; t += tick) {
				tempo += 5;

				if (flagTT) {
					// gravidade += BotInfos->maya.faseBola1;
					Projetil[0].x += Projetil[0].speedX * tick * (Projetil[0].inverter ? -1 : 1) + Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5;
					Projetil[0].y -= Projetil[0].speedY * tick + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
					Projetil[0].ultimoSpeedX += aceleracaoVentoX * tick;
					Projetil[0].ultimoSpeedY += (aceleracaoVentoY - gravidade) * tick;
					calculaTornado();
				}
				else {
					calcular(i, t);
				}

				if (tiro == 2) {
					float semiTempo = 485;
					if (tempo == semiTempo) {
						// (BotInfos->teste1.x)
						count = 1;
						flagTT = true;
						// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(ultimoProjetil[0].x - camera.x - 3), (int)(ultimoProjetil[0].y - camera.y - 3), 6, 6);
						desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (ultimoProjetil[0].x - camera.x), (ultimoProjetil[0].y - camera.y), 3, 2, 2);
						flagSS = true;
					}
				}
				else if (tiro == 1) {
					// Fork de Turtle
					if ((tempo == 115) || (tempo == 165) || (tempo == 220) || (tempo == 275) || (tempo == 345) || (tempo == 430) || (tempo == 605)) {

						desenhaRotacao(rotacoes[0], linhas[TL_BRANCO].pen, (ultimoProjetil[0].x - camera.x), (ultimoProjetil[0].y - camera.y), 3, 2, 2);
						// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(ultimoProjetil[0].x - camera.x - 3), (int)(ultimoProjetil[0].y - camera.y - 3), 6, 6);
						if (tempo == 365)
							flagSS = true;
					}
				}

				desenhaLinha(i, flagSS && flagTT);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7782");
		return;
	}
}

void __fastcall TShMatrix::fillGrub() {
	__try {
		bool primeiro = true;
		bool flagGrub = (opcoesTiro[MB_GRUB].tiroEspecial && tiro == 1);

		count = 1;

		if (flagGrub) {
			count = 4;
			for (int i = 0; i < count; i++) {
				float anguloEspecial = 0;
				float forcaBotBB = forcaBot;
				switch(i) {
				case 0:
					anguloEspecial = -2.1; //
					forcaBotBB += -9.8; //
					break;
				case 1:
					anguloEspecial = -1.1; // + BotInfos->teste1.x;
					forcaBotBB += -5.8; // ;
					break;
				case 2:
					anguloEspecial = -0.1; // 0 + BotInfos->teste1.y;
					forcaBotBB += -1.8; // 4;
					break;
				case 3:
					anguloEspecial = 0.9; // -1;
					forcaBotBB += 1.6; // 8;
					break;
				default:
					break;
				}

				float BAlfa = cos(intToRadian(((float)anguloBot) + anguloEspecial)) + (direita ? -0.002 : 0.002);
				float BBeta = sin(intToRadian(((float)anguloBot) + anguloEspecial));

				float BvelocidadeTiroX = BAlfa * forcaBotBB;
				float BvelocidadeTiroY = BBeta * forcaBotBB;

				Projetil[i] = TProjetil(anguloBot + anguloEspecial, x, y, BvelocidadeTiroX, BvelocidadeTiroY);
			}
		}

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i, flagGrub);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7783");
		return;
	}
}

void __fastcall TShMatrix::fillAduka() {
	__try {

		count = 1;
		bool primeiro = true;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i);
				if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7784");
		return;
	}
}

void __fastcall TShMatrix::fillKalsiddon() {
	__try {

		count = 1;
		int tempo = 0;
		bool primeiro = true;
		bool flagSS = false;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				tempo += 5;
				calcular(i, t);

				int semiTempo = (int)(0.8 * forcaBot);
				int resto = (semiTempo % 5);
				semiTempo -= (resto - 25);

				if (tempo == semiTempo) {
					// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(ultimoProjetil[0].x - camera.x - 3), (int)(ultimoProjetil[0].y - camera.y - 3), 6, 6);
					desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (ultimoProjetil[0].x - camera.x), (ultimoProjetil[0].y - camera.y), 3, 2, 2);
					flagSS = !flagSS;
				}

				desenhaLinha(i, flagSS);
				if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7785");
		return;
	}
}

void __fastcall TShMatrix::fillJFrog() {
	__try {

		count = 1;
		bool primeiro = true;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i);
				if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7786");
		return;
	}
}

void __fastcall TShMatrix::fillDragon() {
	__try {

		count = 1;
		bool primeiro = true;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7787");
		return;
	}
}

void __fastcall TShMatrix::fillKnight() {
	__try {

		count = 1;
		bool primeiro = true;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (double t = 0; t <= 50; t += tick) {
			calcular(0, t);
			desenhaLinha(0);
			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7788");
		return;
	}
}

// Mudanças de Raio, Fase e Tempo
TPhoenixCFG phoenixCFG[10] = { {
		0, 248, 26, 0
	}, {
		1, 243, 25, 0
	}, {
		2, 239, 25, 0
	}, {
		4, 236, 25, 0
	}, {
		6, 234, 25, 0
	}, {
		7, 232, 26, 0
	}, {
		7.8, 230, 27, 0
	}, {
		8, 228, 28, 0
	}, {
		8, 227, 30, 0
	}, {
		8, 227, 32, 0
	}
};

TPhoenixCFG phoenixCFGSS[10] = { {
		0, 258, 80, 27
	}, {
		0, 257, 80, 27
	}, {
		0, 256, 80, 27
	}, {
		0, 255, 80, 27
	}, {
		0, 254, 80, 27
	}, {
		0.7, 253, 90, 28
	}, {
		1.5, 253, 80, 28
	}, {
		2, 252, 80, 28
	}, {
		1, 252, 80, 28
	}, {
		1, 252, 60, 28
	}
};

// Mudança de fase do Tiro 1 de acordo com a força
float raiosCFGT1[5] = {
	0, 0.125, 0.250, 0.625, 1
};
float raiosCFGSS[5] = {
	0, 0, 0.33, 0.67, 1
};
float temposT2CFG[10] = {
	2, 2, 1, 1, 1, 0, -1, -2, -3, -5
};

void __fastcall TShMatrix::fillPhoenix() {
	__try {
		bool inverteTopo = false;

		count = 1;
		bool flagPhoenix = false;
		bool primeiro = true;
		int tempoPhoenix = 0;
		int tempo = 0;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		int anguloQuad = (anguloBot == 90) ? 90 : (anguloBot % 90);
		anguloQuad = (anguloBot > 90) ? (90 - anguloQuad) : anguloQuad;
		float indiceMod = (anguloQuad % 10);
		int indice = (int)(anguloQuad / 10);
		int indice2 = ((indice + 1) > 9) ? indice : (indice + 1);

		TPhoenixCFG cfg1 = phoenixCFG[indice];
		TPhoenixCFG cfg2 = phoenixCFG[indice2];
		TPhoenixCFG cfg;

		if (tiro == 2) {
			cfg1 = phoenixCFGSS[indice];
			cfg2 = phoenixCFGSS[indice2];
		}

		cfg.raio = cfg1.raio + (((float)(cfg2.raio - cfg1.raio)) / 10 * indiceMod);
		cfg.fase2 = cfg1.fase2 + (((float)(cfg2.fase2 - cfg1.fase2)) / 10 * indiceMod);
		cfg.tempo = cfg1.tempo + (((float)(cfg2.tempo - cfg1.tempo)) / 10 * indiceMod);
		cfg.tempo2 = cfg1.tempo2 + (((float)(cfg2.tempo2 - cfg1.tempo2)) / 10 * indiceMod);

		float indiceModF = (forcaBot % 100);
		int indiceF = floor(forcaBot / 100);
		int indice2F = ((indiceF + 1) > 4) ? indiceF : (indiceF + 1);

		cfg.fase2 += (tiro == 2) ? (8 - (forcaBot * 0.02)) : (16 - (forcaBot * 0.04));
		float raioTiro = 0;

		for (double t = 0; t <= 50; t += tick) {
			tempo += 5;
			if (flagPhoenix) {
				Projetil[0].x += Projetil[0].speedX * tick * (Projetil[0].inverter ? -1 : 1) + (Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5) * (Projetil[0].inverter ? 0 : 1);
				Projetil[0].y -= Projetil[0].speedY * tick + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
				Projetil[0].ultimoSpeedX -= aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY -= (aceleracaoVentoY - gravidade) * tick;
				calculaTornado();
			}
			else {
				calcular(0, t);
			}

			if (flagPhoenix) {
				int semiTempo = 0;

				if (tiro == 0) {
					semiTempo = tempoPhoenix + ((int)cfg.tempo) * 5;

					float porcentagem1 = raiosCFGT1[indiceF];
					float porcentagem2 = raiosCFGT1[indice2F];
					float porcentagem = porcentagem1 + (porcentagem2 - porcentagem1) / 100 * indiceModF;

					raioTiro = cfg.raio * porcentagem;
				}
				else if (tiro == 1) {
					semiTempo = tempoPhoenix + ((int)cfg.tempo) * 5;

					float tempoT2CFG1 = temposT2CFG[indice];
					float tempoT2CFG2 = temposT2CFG[indice2];
					float tempoT2CFG = tempoT2CFG1 + ((tempoT2CFG2 - tempoT2CFG1) / 10 * indiceMod);

					semiTempo += ((int)tempoT2CFG) * 5;

				}
				else if (tiro == 2) {
					semiTempo = tempoPhoenix + ((int)cfg.tempo2) * 5;
					// + BotInfos->teste1.y*5;

					float porcentagem1 = raiosCFGSS[indiceF];
					float porcentagem2 = raiosCFGSS[indice2F];
					float porcentagem = porcentagem1 + (porcentagem2 - porcentagem1) / 100 * indiceModF;

					raioTiro = cfg.raio * porcentagem;
				}

				if (tempo == semiTempo) {
					if ((Projetil[0].y - camera.y) <= 515)
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

					if (tiro == 0) {
						// float anguloCurva = (0.03*forca + 27) + BotInfos->teste1.x + (-5 + (6.1 * modulo(sin(intToRadian(BotInfos->anguloBot)))));
						int fase = 80;
						int sweep = 200;
						float raioPhoenix = raioTiro; // BotInfos->maya.faseBola2; //0.0175*forca * modulo(sin( intToRadian(BotInfos->anguloBot) )) + BotInfos->maya.faseBola2 - 0.86;
						float px = 0;
						float py = 0;
						float ux = Projetil[0].x;
						float uy = Projetil[0].y;

						int n = modulo(360 - sweep);

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						// Desenha o semi-circulo (de 5 em 5 angulos para economizar tempo)
						for (int i = 0; i < n; i += 5) {
							Projetil[0].x = ultimoProjetil[0].x + ((float)raioPhoenix * (float)cos((float)intToRadian(i + fase)) * (direita ? -1 : 1)) * ((Projetil[0].inverter) ? -1 : 1) * ((inverteTopo) ? -1 : 1);
							Projetil[0].y = ultimoProjetil[0].y - ((float)raioPhoenix * (float)sin((float)intToRadian(i + fase)));

							if (temTornado)
								calculaTornado();

							if ((Projetil[0].y - camera.y) <= 515)
								grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y), (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y));

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}

						raioPhoenix = 68; // BotInfos->teste1.x;
						fase = cfg.fase2; // 226 + BotInfos->maya.faseBola1;
						sweep = 200;

						for (float i = 0; i < n; i += 0.5) {
							Projetil[0].x = ultimoProjetil[0].x + ((float)raioPhoenix * (float)cos((float)intToRadian(i + fase)) * (direita ? -1 : 1)) * ((Projetil[0].inverter) ? -1 : 1) * ((inverteTopo) ? -1 : 1);
							Projetil[0].y = ultimoProjetil[0].y - ((float)raioPhoenix * (float)sin((float)intToRadian(i + fase)));

							if (temTornado)
								calculaTornado();

							if ((Projetil[0].y - camera.y) <= 515)
								grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y), (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y));

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}

						/* Projetil[0].x = ux;
						Projetil[0].y = uy;
						//fixPos(0);
						ultimoProjetil[0].x = ux;
						ultimoProjetil[0].y = uy; */

						/*
						int x1 = 0;
						int y1 = 1900 - Projetil[0].y;
						x1 = tan(intToRadian(anguloCurva))*y1;
						x1 = (direita?x1:-x1);

						if((Projetil[0].y - camera.y) <= 515)
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						Projetil[0].x += x1;
						Projetil[0].y = 1900;
						x1 = Projetil[0].x;

						// Calcula as variáveis da equação da reta - invertido ou não
						int x2 = ((ultimoProjetil[0].x - Projetil[0].x) * 2) + Projetil[0].x;
						int y2 = Projetil[0].y;
						if( Projetil[0].inverter || inverteTopo ){
						reta.pa = (ultimoProjetil[0].y - y2);
						reta.pb = (x2 - ultimoProjetil[0].x);
						reta.pc = (ultimoProjetil[0].x * y2 - ultimoProjetil[0].y * x2);
						reta.pb = ((reta.pb == 0)?1:reta.pb);
						}else{
						reta.pa = (ultimoProjetil[0].y - Projetil[0].y);
						reta.pb = (Projetil[0].x - ultimoProjetil[0].x);
						reta.pc = (ultimoProjetil[0].x * Projetil[0].y - ultimoProjetil[0].y * Projetil[0].x);
						reta.pb = ((reta.pb == 0)?1:reta.pb);
						}

						// Desenha de 5 em 5 pixels horizontais pra calculo do espelho e tornado
						int parte = modulo((ultimoProjetil[0].x - Projetil[0].x) / 5);
						for(int i = 0; i < parte; i ++){
						int j = (direita?5:-5) * (Projetil[0].inverter?-1:1) * (inverteTopo?-1:1) ;

						Projetil[0].x = ultimoProjetil[0].x + j;
						Projetil[0].y = -1 * ((reta.pa * Projetil[0].x) + reta.pc) / reta.pb;

						if(temTornado)
						calculaTornado();

						if((Projetil[0].y - camera.y) <= 515)
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;
						}

						Projetil[0].x = x1;
						Projetil[0].y = 1900;

						if(temTornado)
						calculaTornado();

						if((Projetil[0].y - camera.y) <= 515)
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

						 */
						return;
					}
					else if (tiro == 1) { // T2
						int subida = 60 + 0.1 * forcaBot;
						int pontoFim = (1900 - camera.y);
						pontoFim = (pontoFim <= 515) ? pontoFim : 515;
						int pontoInicio = (int)(Projetil[0].y - camera.y - subida);
						if (pontoInicio <= 515 && pontoFim <= 515)
							grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y - subida), (int)(Projetil[0].x - camera.x), pontoFim);
						return;
					}
					else { // SS
						// float anguloCurva = BotInfos->maya.faseBola2;//0.01*forca + 9;
						int subida = cfg.tempo - (40 - (0.1 * forcaBot));
						// BotInfos->maya.faseBola1; //60+0.1*forca;

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						if ((Projetil[0].y - camera.y - subida) <= 515)
							grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y - subida), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

						Projetil[0].y = (Projetil[0].y - subida);
						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						// -----------------------------------
						int fase = -90;
						int sweep = 200;
						float raioPhoenix = raioTiro; // BotInfos->teste1.x;//(forca < 200)?0:(0.01*(forca - 200)); //0.0175*forca;
						/* float px = 0;
						float py = 0;
						float ux = Projetil[0].x;
						float uy = Projetil[0].y; */

						int n = modulo(360 - sweep);

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						for (int i = 0; i < n; i += 5) {
							Projetil[0].x = ultimoProjetil[0].x + ((float)raioPhoenix * (float)cos((float)intToRadian(i + fase)) * (direita ? -1 : 1)) * ((Projetil[0].inverter) ? -1 : 1) * ((inverteTopo) ? -1 : 1);
							Projetil[0].y = ultimoProjetil[0].y + ((float)raioPhoenix * (float)sin((float)intToRadian(i + fase)));

							if (temTornado)
								calculaTornado();

							if ((Projetil[0].y - camera.y) <= 515)
								grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y), (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y));

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}
						/*
						Projetil[0].x = ux;
						Projetil[0].y = uy;
						ultimoProjetil[0].x = ux;
						ultimoProjetil[0].y = uy; */

						raioPhoenix = 80; // BotInfos->teste1.x;
						fase = cfg.fase2; // BotInfos->maya.faseBola2;//cfg.fase2; // 226 + BotInfos->maya.faseBola1;
						sweep = 200;

						for (float i = 0; i < n; i += 0.5) {
							Projetil[0].x = ultimoProjetil[0].x - ((float)raioPhoenix * (float)cos((float)intToRadian(i + fase)) * (direita ? -1 : 1)) * ((Projetil[0].inverter) ? -1 : 1) * ((inverteTopo) ? -1 : 1);
							Projetil[0].y = ultimoProjetil[0].y - ((float)raioPhoenix * (float)sin((float)intToRadian(i + fase)));

							if (temTornado)
								calculaTornado();

							if ((Projetil[0].y - camera.y) <= 515)
								grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y), (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y));

							ultimoProjetil[0].x = Projetil[0].x;
							ultimoProjetil[0].y = Projetil[0].y;
						}

						/*
						//-----------------------------------

						int x1 = 0;
						int y1 = 1900 - Projetil[0].y;
						x1 = tan(intToRadian(anguloCurva))*y1;
						x1 = (direita?x1:-x1);

						//grafico.DrawEllipse(&linhas[TL_BRANCO].pen, Projetil[0].x - camera.x - 3, Projetil[0].y - camera.y - 3, 6, 6);
						//grafico.DrawLine(&linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

						Projetil[0].x -= x1;
						Projetil[0].y = 1900;
						x1 = Projetil[0].x;

						int x2 = ((ultimoProjetil[0].x - Projetil[0].x) * 2) + Projetil[0].x;
						int y2 = Projetil[0].y;
						if( Projetil[0].inverter || inverteTopo ){
						reta.pa = (ultimoProjetil[0].y - y2);
						reta.pb = (x2 - ultimoProjetil[0].x);
						reta.pc = (ultimoProjetil[0].x * y2 - ultimoProjetil[0].y * x2);
						reta.pb = ((reta.pb == 0)?1:reta.pb);
						}else{
						reta.pa = (ultimoProjetil[0].y - Projetil[0].y);
						reta.pb = (Projetil[0].x - ultimoProjetil[0].x);
						reta.pc = (ultimoProjetil[0].x * Projetil[0].y - ultimoProjetil[0].y * Projetil[0].x);
						reta.pb = ((reta.pb == 0)?1:reta.pb);
						}

						int parte = modulo((ultimoProjetil[0].x - Projetil[0].x) / 5);
						for(int i = 0; i < parte; i ++){
						int j = (direita?5:-5) * (Projetil[0].inverter?-1:1) * ((inverteTopo)?-1:1);

						Projetil[0].x = ultimoProjetil[0].x - j;
						Projetil[0].y = -1 * ((reta.pa * Projetil[0].x) + reta.pc) / reta.pb;

						if(temTornado)
						calculaTornado();

						if((Projetil[0].y - camera.y) <= 515)
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;
						}

						//if((Projetil[0].y - camera.y) <= 515)
						//grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));
						//else
						//	grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), 515, (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));
						 */
						return;
					}
				}
			}
			else {
				int semiTempo = 210;

				if (tempo == semiTempo) {

					if ((Projetil[0].y - camera.y) <= 515) {
						// grafico->DrawEllipse(linhas[TL_LINHA1].pen, Projetil[0].x - camera.x - 3, Projetil[0].y - camera.y - 3, 6, 6);
						desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (Projetil[0].x - camera.x), (Projetil[0].y - camera.y), 3, 2, 2);
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));
					}

					ultimoProjetil[0].x = Projetil[0].x;
					ultimoProjetil[0].y = Projetil[0].y;

					x = Projetil[0].x;
					y = Projetil[0].y;

					tempoPhoenix = tempo;
					flagPhoenix = true;

					int diferenca = (x - posicaoBot.x);

					// if( (direita && diferenca <= -10) || (!direita && diferenca >= 10) )
					if ((direita && diferenca <= -2) || (!direita && diferenca >= 2))
						inverteTopo = !inverteTopo;
					// Projetil[0].inverter = !Projetil[0].inverter;
					/*
					HDC dc = GetDC(0);
					__try{

					if(BotInfos != NULL && BotInfos->inGame){
					char teste1[100];
					int n = sprintf(teste1, "[%d, %d]", x, y);
					TextOut(dc, x - camera.x + 10, y - camera.y, teste1, n);

					char teste2[100];
					n = sprintf(teste2, "[%d, %d]", BotInfos->posicaoBot.x, BotInfos->posicaoBot.y);
					TextOut(dc, BotInfos->posicaoBot.x - camera.x + 10, BotInfos->posicaoBot.y - camera.y, teste2, n);
					}

					}__finally{
					ReleaseDC(0, dc);
					} */

					continue;
				}
			}

			if (temTornado)
				calculaTornado();

			for (int i = 0; i < count; i++) {
				if ((Projetil[i].x > (camera.x - 20)) && (Projetil[i].x < (camera.x + 820)) && (Projetil[i].y > (camera.y - 20)) && (Projetil[i].y < (camera.y + 515))) {
					if (i == 0) {
						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
					}
					else {
						if (!primeiro)
							grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
					}
				}
			}

			for (int i = 0; i < count; i++) {
				primeiro = false;
				ultimoProjetil[i].x = Projetil[i].x;
				ultimoProjetil[i].y = Projetil[i].y;
			}
		}
	}
	__except (1) {
    	debugar("Erro 7789");
		return;
	}
}

void __fastcall TShMatrix::fillMaya() {
	__try {

		count = 1; // ((tiro == 1)?3:1);
		bool primeiro = true;
		bool flagMaya = false;
		int tempo = 0;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (double t = 0; t <= 50; t += tick) {
			tempo += 5;
			if (flagMaya) {
				// Projetil[0].x = x - velocidadeTiroX * t - aceleracaoVentoX * t * t;
				// Projetil[0].y = y + velocidadeTiroY * t + aceleracaoVentoY * t * t - 0.5 * gravidade * t * t;

				Projetil[0].x -= Projetil[0].speedX * tick * (Projetil[0].inverter ? -1 : 1) + (Projetil[0].ultimoSpeedX + aceleracaoVentoX * tick * tick * 0.5) * (Projetil[0].inverter ? 0 : 1);
				Projetil[0].y += Projetil[0].speedY * tick + Projetil[0].ultimoSpeedY + (aceleracaoVentoY - gravidade) * tick * tick * 0.5;
				Projetil[0].ultimoSpeedX += aceleracaoVentoX * tick;
				Projetil[0].ultimoSpeedY += (aceleracaoVentoY - gravidade) * tick;
				calculaTornado();

				// O maxado volta para a mão do Maya e para
				if (modulo(Projetil[0].x - posicaoBot.x) <= 2 && modulo(Projetil[0].y - posicaoBot.y) <= 2) {
					grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(posicaoBot.x - camera.x), (int)(posicaoBot.y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));
					break;
				}
			}
			else {
				calcular(0, t);
				// Projetil[0].x = x + Projetil[0].speedX * t + aceleracaoVentoX * t * t;
				// Projetil[0].y = y - Projetil[0].speedY * t - aceleracaoVentoY * t * t + 0.5 * gravidade * t * t;
			}

			if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
				break;
			}

			if (opcoesTiro[MB_MAYA].tiroEspecial && !flagMaya) {
				if (tiro == 1) {
					if (tempo == 45) {
						count = 3;
						grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(ultimoProjetil[1].x - camera.x), (int)(ultimoProjetil[1].y - camera.y), (int)(posicaoBot.x - camera.x), (int)(posicaoBot.y - camera.y));
						grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(ultimoProjetil[2].x - camera.x), (int)(ultimoProjetil[2].y - camera.y), (int)(posicaoBot.x - camera.x), (int)(posicaoBot.y - camera.y));
					}

					int mayaRaio = 48;
					float velocidadeAngularMaya = (direita ? 11.2 : 11.2);
					int faseMaya = (direita ? 686.7 : 763);

					float tempoMaya = t * 10;
					int anguloRepolho1 = faseMaya + ((direita) ? 180 : 0) + (int)(velocidadeAngularMaya * tempoMaya);
					int anguloRepolho2 = faseMaya + ((!direita) ? 180 : 0) + (int)(velocidadeAngularMaya * tempoMaya);

					POINT repolho1;
					POINT repolho2;
					repolho1.x = mayaRaio * cos(intToRadian(anguloRepolho1));
					repolho2.x = mayaRaio * cos(intToRadian(anguloRepolho2));

					repolho1.x = (Projetil[0].x + repolho1.x);
					repolho1.y = (Projetil[0].y);
					repolho2.x = (Projetil[0].x + repolho2.x);
					repolho2.y = (Projetil[0].y);

					Projetil[1].x = repolho1.x;
					Projetil[1].y = repolho1.y;
					Projetil[2].x = repolho2.x;
					Projetil[2].y = repolho2.y;

					if (tempo < 45) {
						ultimoProjetil[1].x = Projetil[1].x;
						ultimoProjetil[1].y = Projetil[1].y;
						ultimoProjetil[2].x = Projetil[2].x;
						ultimoProjetil[2].y = Projetil[2].y;
					}

				}
				else if (tiro == 2) { // if(BotInfos->tiro == 2 && TotalTime >= BotInfos->maya.pico){ // 4.65

					double base = 0.0063;
					double parte = (anguloBot % 360);
					// parte = parte - (floor((double)parte/90)%90);
					// parte = ((parte > 45)?modulo(parte - 90):parte);
					// parte = ((parte == 0 || parte == 90)?0:parte);
					// base = base + parte*0.00006;

					int anguloTiro = anguloBot;
					anguloTiro = (anguloTiro % 90) - 45;
					anguloTiro = (anguloTiro < 0) ? (anguloTiro*-1) : anguloTiro;
					parte = (cos(intToRadian((float)anguloTiro)) - 0.008) * 100;
					parte -= (96 - (forcaBot * 0.24));

					double partePico = base;
					int pico = ((int)(parte)) * 5; // + (BotInfos->teste1.x*5); //(partePico*(double)forca) * 29.54399486*5;

					if (tempo >= (int)(pico - 2) && tempo <= (int)(pico + 2)) {
						// grafico->DrawEllipse(linhas[TL_BRANCO].pen, Projetil[0].x - camera.x - 3, Projetil[0].y - camera.y - 3, 6, 6);
						desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (Projetil[0].x - camera.x), (Projetil[0].y - camera.y), 3, 2, 2);

						double ny = (int)modulo(Projetil[0].y - posicaoBot.y);
						double tg = tan(intToRadian(anguloBot));
						tg = ((tg == 0) ? 1 : tg);
						int nx = (int)(ny / tg);
						int rx = (Projetil[0].x - nx);

						t = 0;
						flagMaya = true;
						x = Projetil[0].x;
						y = Projetil[0].y;

						Projetil[0].ultimoSpeedX = 0;
						Projetil[0].ultimoSpeedY = 0;

						grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

						ultimoProjetil[0].x = Projetil[0].x;
						ultimoProjetil[0].y = Projetil[0].y;

						if (Projetil[0].x > 1800 || Projetil[0].x < 0 || Projetil[0].y < -5000 || Projetil[0].y > 2000) {
							break;
						}

						continue;
					}
				}
				else {
					count = 1;
				}
			}

			for (int i = 0; i < count; i++) {
				desenhaLinha(i);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
		debugar("Erro 7790");
		return;
	}
}

void __fastcall TShMatrix::fillWolf() {
	__try {

		int tempo = 0;
		count = 1;
		bool primeiro = true;
		bool flagWolf = false;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				tempo += 5;

				if (flagWolf)
					gravidade += 5.8f;

				calcular(i, t);

				int inicio = 160;
				if (tiro == 2 && (tempo == inicio || tempo == (inicio + 80) || tempo == (inicio + 160) || tempo == (inicio + 240) || tempo == (inicio + 320))) {
					if ((Projetil[i].y - camera.y) <= 515) {
						// grafico->DrawEllipse(linhas[TL_BRANCO].pen, (int)(Projetil[0].x - camera.x - 3), (int)(Projetil[0].y - camera.y - 3), 6, 6);
						desenhaRotacao(rotacoes[3], linhas[TL_BRANCO].pen, (Projetil[0].x - camera.x), (Projetil[0].y - camera.y), 3, 2, 2);
						grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(Projetil[0].x - camera.x), (int)515);
					}
				}

				if (tempo == 600 && tiro == 2) { // (BotInfos->teste1.x*5)
					/* if( (Projetil[i].y - camera.y) <= 515){
					int x0 = ultimoProjetil[0].x;
					int y0 = ultimoProjetil[0].y;
					int x1 = Projetil[0].x;
					int y1 = Projetil[0].y;

					float fracao = ((x1 - x0) == 0)?1.0f:(float)(((float)(y1 - y0))/((float)(x1 - x0)));
					int angulo2 = radianToInt(atan(fracao));
					int angulo = angulo2+BotInfos->teste1.y; //(90 - angulo2)+BotInfos->teste1.y;


					y1 = 1900;
					//angulo = 78 + BotInfos->teste1.y;
					if(angulo > 0 && angulo != 90 && angulo != 270){
					float tgA = tan(intToRadian(angulo));
					if(tgA != 0){
					x1 = (tgA * x0 - y0 + y1)/tgA;

					if( Projetil[0].inverter ){
					x1 = x0 - (x1 - x0);
					}

					reta.pa = (y0 - y1);
					reta.pb = (x1 - x0);
					reta.pc = (x0 * y1 - y0 * x1);
					reta.pb = ((reta.pb == 0)?1:reta.pb);

					int parte = modulo((x0 - x1) / 5);
					for(int i = 0; i < parte; i++){
					int j = ((x1 < x0)?5:-5) * (Projetil[0].inverter?-1:1);

					Projetil[0].x = ultimoProjetil[0].x - j;
					Projetil[0].y = -1 * ((reta.pa * Projetil[0].x) + reta.pc) / reta.pb;

					if(temTornado)
					calculaTornado();

					if((Projetil[0].y - camera.y) <= 515)
					grafico->DrawLine(linhas[TL_LINHA1].pen, (int)(Projetil[0].x - camera.x), (int)(Projetil[0].y - camera.y), (int)(ultimoProjetil[0].x - camera.x), (int)(ultimoProjetil[0].y - camera.y));

					ultimoProjetil[0].x = Projetil[0].x;
					ultimoProjetil[0].y = Projetil[0].y;
					}

					desenhaLinha(i);
					//grafico->DrawLine(linhas[TL_LINHA2].pen, (int)(x0 - camera.x), (int)(y0 - camera.y), (int)(x1 - camera.x), (int)(y1 - camera.x) );
					}

					}
					}
					break; */
					flagWolf = true;
				}

				desenhaLinha(i);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7791");
		return;
	}
}

void __fastcall TShMatrix::fillBlueWhale() {
	__try {

		count = 1;
		bool primeiro = true;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		double velocidadeY = 0;

		/* for (double t = 0; t <= 12; t += tick){
		Projetil[0].x += (Projetil[0].speedX * (Projetil[0].inverter?-1:1) * tick) + 2 * aceleracaoVentoX * t * tick;
		Projetil[0].y -= (Projetil[0].speedY * tick) + 2 * aceleracaoVentoY * t * tick  - (gravidade * (t + tick * 0.5) * tick);
		velocidadeY = Projetil[0].speedY + 2 * aceleracaoVentoY * t - gravidade * (t + tick * 0.5);

		gravidade += BotInfos->maya.faseBola2;

		if(velocidadeY < 0){
		gravidade += BotInfos->maya.pico;
		}

		if(temTornado)
		calculaTornado();

		for(int i = 0; i < count; i++){
		if( (Projetil[i].x > (camera.x-20)) && (Projetil[i].x < (camera.x+820)) && (Projetil[i].y > (camera.y-20)) && (Projetil[i].y < (camera.y+525))  ) {
		if(i == 0){
		grafico->DrawLine(linhas[TL_LINHA1].pen, (int) (ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
		}else{
		if(!primeiro)
		grafico->DrawLine(linhas[TL_LINHA2].pen, (int) (ultimoProjetil[i].x - camera.x), (int)(ultimoProjetil[i].y - camera.y), (int)(Projetil[i].x - camera.x), (int)(Projetil[i].y - camera.y));
		}
		}
		}

		for(int i = 0; i < count; i++){
		primeiro = false;
		ultimoProjetil[i].x = Projetil[i].x;
		ultimoProjetil[i].y = Projetil[i].y;
		}
		} */

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
		debugar("Erro 7792");
		return;
	}
}

void __fastcall TShMatrix::fillTiburon() {
	__try {

		count = 1;
		bool primeiro = true;
		bool flagMaya = false;

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
    	debugar("Erro 7793");
		return;
	}
}

void __fastcall TShMatrix::fillFrank() {
	__try {

		count = 1;
		bool primeiro = true;
		bool flagFrank = opcoesTiro[MB_FRANK].tiroEspecial;

		if (flagFrank) {
			if (tiro == 2)
				count = 5;
		}

		if (flagFrank) {
			if (tiro == 2) {
				for (int i = 0; i < count; i++) {
					float anguloEspecial = 0;
					float forcaBotBB = forcaBot;
					anguloEspecial = i * 10;

					float BAlfa = cos(intToRadian((float)anguloBot - anguloEspecial)) + (direita ? -0.002 : 0.002);
					float BBeta = sin(intToRadian((float)anguloBot - anguloEspecial));

					float BvelocidadeTiroX = BAlfa * forcaBotBB;
					float BvelocidadeTiroY = BBeta * forcaBotBB;

					Projetil[i] = TProjetil(anguloBot - anguloEspecial, x, y, BvelocidadeTiroX, BvelocidadeTiroY);
				}
			}
		}

		for (int i = 0; i < count; i++) {
			ultimoProjetil[i].x = Projetil[i].x;
			ultimoProjetil[i].y = Projetil[i].y;
			Projetil[i].x = x;
			Projetil[i].y = y;
			Projetil[i].inverter = false;
		}

		for (int i = 0; i < count; i++) {
			for (double t = 0; t <= 50; t += tick) {
				calcular(i, t);
				desenhaLinha(i, flagFrank && tiro == 2);
				if (Projetil[i].x > 1800 || Projetil[i].x < 0 || Projetil[i].y < -5000 || Projetil[i].y > 2000) {
					break;
				}
			}
		}
	}
	__except (1) {
		return;
	}
}

// ---------------------------------------------------------------------------
