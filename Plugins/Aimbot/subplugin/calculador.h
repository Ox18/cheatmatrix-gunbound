//---------------------------------------------------------------------------

#ifndef calculadorH
#define calculadorH

#include "..\..\tiposBase.h"
//#include "variaveis.h"
#include "mathUtils.h"
#include "mobiles.h"
#include "estruturaBot.h"
#include <math.h>

class TCalculador {

	private:
		int fAtual;
		int posicaoTelaX;
		int posicaoTelaY;
		int diametroX;
		int diametroY;
		int raio;
		int x;
		int y;
		int angulo;
		float gravidade;
		bool temTornado;
		bool direita;
		int count;
		int tiro;
		float fatorForca;
		bool flagEspelho;

		TReta reta;

		POINT_FLOAT velocidadeProjetil;
		POINT_FLOAT aceleracaoVento;
		float novaGravidade;

		float menorDistancia;
		TMelhorForca melhorForca;

		int espessura;

		TProjetil Projetil[6];
		POINT_FLOAT ultimoProjetil[6];

		bool tornadosFlag[4];

		double Alfa, Beta, Gama, Delta;
		double aceleracaoVentoX, aceleracaoVentoY;
		double velocidadeTiroX,  velocidadeTiroY;
	public:
		TCalculador(){
		   temTornado = false;
		   x = 0;
		   y = 0;
		   //forca = 0;
		   angulo = 0;
		   posicaoTelaX = 0;
		   posicaoTelaY = 0;
		   diametroX = 0;
		   diametroY = 0;
		   raio = 36;
		   count = 1;
		   espessura = 1;
		   tiro = 0;
		   fatorForca = 0;
		   direita = false;
		};

		POINT camera;
		POINT alvo;

        void __fastcall calculaTornado(bool analizar = true);
//		void __fastcall calculaArmor();
//		void __fastcall calculaMage();
		void __fastcall calculaNak();
		void __fastcall calculaTrico();
		void __fastcall calculaBigFoot();
		void __fastcall calculaBoomer();
//		void __fastcall calculaRaon();
		void __fastcall calculaLightning();
		void __fastcall calculaJD();
		void __fastcall calculaASate();
//		void __fastcall calculaIce();
		void __fastcall calculaTurtle();
		void __fastcall calculaGrub();
//		void __fastcall calculaAduka();
		void __fastcall calculaKalsiddon();
		void __fastcall calculaJFrog();
		void __fastcall calculaPhoenix();
		void __fastcall calculaMaya();
		void __fastcall calculaDragon();
		void __fastcall calculaKnight();
//		void __fastcall calculaFrank();
//		void __fastcall calculaTiburon();
		void __fastcall calculaBlueWhale();
//		void __fastcall calculaWolf();
		void __fastcall calculaGenerico();
		void __fastcall analizaMelhor(int distancia);
		//void __fastcall fixPos(int indice);
		void __fastcall calcular(int indice, float t, bool analizar = true);
		TMelhorForca __fastcall calcularMelhor(HANDLE closeFlag);
		void __fastcall analizaMelhor(int x, int y);
		void __fastcall desenhaLinha(int indice, bool flag = false);
};


//---------------------------------------------------------------------------
#endif



