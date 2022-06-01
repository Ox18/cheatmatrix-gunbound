//---------------------------------------------------------------------------

#ifndef classesCBH
#define classesCBH

#include <math.h>
#include <vcl.h>
#include <windows.h>
#include "IniFiles.hpp"
#include "StrUtils.hpp"
#include <vector>
#include <tlhelp32.h>
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>

const numeroMobiles = 20;

typedef struct _Calibracao
{
	 double x;
	 double y;
	 double g;
	 double rx;
	 double ry;
} TCalibracoes;

typedef struct _quadrantes{
	 std::vector<BYTE> bytesQ1;
	 std::vector<BYTE> bytesQ2;
	 std::vector<BYTE> bytesQ3;
	 std::vector<BYTE> bytesQ4;
}TQuadrantes;

typedef struct _ForcasCB{
	std::vector<float> angulos;
}TForcasCB;

typedef struct _MobileCB{
		int indice;
		String nome;
		float gravidade;
		std::vector<TForcasCB> forcas;
}TMobileCB;


class TFileCB{
	public:
		TMemoryStream *arquivo;
		BYTE bytes[1430];
		TMobileCB mobile;
		PBYTE memoria;
	public:
		void __fastcall carregar(BYTE mobile);
		void __fastcall gravar();
		void __fastcall montaConfigMobile(int mobile);
		float __fastcall setSinal(float valor, bool positivo);
};



//---------------------------------------------------------------------------
#endif
