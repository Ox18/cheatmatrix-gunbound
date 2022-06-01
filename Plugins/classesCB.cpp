//---------------------------------------------------------------------------


#pragma hdrstop

#include "classesCB.h"
#include "Unit1.h"



const double pi = 3.141592654;

TCalibracoes decompoeCalibracaoFull(AnsiString valor){
	TCalibracoes resultado;

	int j = 0;
	AnsiString s = "";
	for(int i = 1; i <= valor.Length(); i++){
		if(valor[i] == '|' || i == valor.Length()){
			 if(valor.Length() == i){
				s = s + valor[i];
			 }
			 j++;
			 switch(j){
				case 1: resultado.y = StrToFloat(s); break;
				case 2: resultado.g = StrToFloat(s); break;
				case 3: resultado.ry = StrToFloat(s); break;
			 }
			 s = "";
			 continue;
		}
		s = s + valor[i];
	}
	return resultado;
}

bool isAnguloDeConvergencia(int angulo, int forca, bool Xflag = false){
	int i,j,k,m,n,r;
	float f, s = 0;

	float power = (float)forca;

	int ultimo = -99;

	for(i = 0; i <= 360; i++){
		int j = i;
		if(Xflag) j = ((i-90+360)%360); 

		if(j > angulo) return false;

		s = Sin(j*pi/180);
		f = s * power;
		k = (int)(f);

		if(ultimo != k && (ultimo != -99 || angulo == 0)){
			if(j == angulo)
				return true;
		}

		if(ultimo != k) ultimo = k;
	}

	return false;
}

double __fastcall Modulo(double valor)
{
	 if(valor < 0)
		 return valor*(-1);
	 return valor;
}

float __fastcall TFileCB::setSinal(float valor, bool positivo){
	if( (positivo && valor < 0) || (!positivo && valor > 0) )
		valor *= -1;
	return valor;
}

void __fastcall TFileCB::montaConfigMobile(int mobile){

	if(mobile > numeroMobiles || mobile < 0) return;

	this->mobile.forcas.clear();

	float ultimaCalibracao = 0;
	int k = -1;
	for(int i = 1; i <= 26; i++){
        TForcasCB forca;
		ultimaCalibracao = 0;
		for(int j = 0; j < 360; j++){
			if( isAnguloDeConvergencia(j, i) ){
				k++;
				int n = (k);
				ultimaCalibracao += ((float)setSinal(bytes[n], !(j > 90 && j <= 270))/100);
			}
			forca.angulos.push_back(ultimaCalibracao);
		}

		this->mobile.forcas.push_back(forca);
	}
}

void __fastcall TFileCB::carregar(BYTE mobile){
	if(mobile < 0 || mobile > 20) return;

	this->arquivo = new TMemoryStream();
	this->arquivo->Write(memoria, 28680);

	try{
		// Le gravidades
		int n = (4*numeroMobiles);
		if( this->arquivo->Size < (n+numeroMobiles*1430)) return;
		float gravidades[numeroMobiles];
		this->arquivo->Position = 0;
		this->arquivo->ReadBuffer(&gravidades[0], 80);
		this->mobile.gravidade = gravidades[mobile];
		this->mobile.indice = mobile;

		BYTE bytesLocal[1430];


        //this->arquivo->Seek(0, soFromBeginning);
		int qnt = (n+(mobile*1430));
		for(int i = 0; i <= mobile; i++){
             this->arquivo->Read(&bytesLocal[0],1430);
		}

		memcpy(bytes, bytesLocal, 1430);

		montaConfigMobile(mobile);
	}__finally{
		this->arquivo->Free();
	}
}




//---------------------------------------------------------------------------

#pragma package(smart_init)
