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

void __stdcall AddLog(AnsiString valor)
{
	Form10->Memo1->Lines->Add(valor);
}


void __fastcall TFileCB::gravar(){
	String dir = GetCurrentDir();
	String nome = dir+"\\data.dll";
	
	TStringList *lista = new TStringList();
	lista->SaveToFile(nome);

	this->arquivo = new TFileStream(nome, fmOpenWrite);
	__try{
		TIniFile *ini = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");

		for(int m = 0; m < numeroMobiles; m++){
			String s = ini->ReadString(IntToStr(m), "A0P26", "");
			if(s != NULL && s.Length() > 0){
				 TCalibracoes calibracao = decompoeCalibracaoFull(s);
				 float g = calibracao.g;
				 this->arquivo->WriteBuffer(&g,4);
			}else{
				 float g = 0;
				 this->arquivo->WriteBuffer(&g,4);
			}
		}

		for(int m = 0; m < numeroMobiles; m++){
			AddLog("Gravando mobile "+numeroMobiles);
			int count = 0;
			for(int f = 1; f <= 26; f++){
				AddLog("Gravando Força "+IntToStr(f));
				Application->ProcessMessages();
				for(int a = 0; a < 360; a++){
					if(isAnguloDeConvergencia(a,f)){
						count++;
						//AddLog("Gravando angulo "+IntToStr(a));
						String s = ini->ReadString(IntToStr(m), "A"+IntToStr(a)+"P"+IntToStr(f), "");
						if(s != NULL && s.Length() > 0){
							 TCalibracoes calibracao = decompoeCalibracaoFull(s);
							 BYTE resto = (BYTE)Modulo(calibracao.ry*100);
							 this->arquivo->WriteBuffer(&resto,1);
						}else{
							 BYTE resto = 0;
							 this->arquivo->WriteBuffer(&resto,1);
						}
					}
				}
			}
			AddLog("TOTAL: "+IntToStr(count));
		}
	}__finally{
		this->arquivo->Free();
	}

	/*for(int i = 0; i < this->mobiles.size(); i++){
		this->arquivo->WriteBuffer(&this->mobiles[i].indice,1);
		this->arquivo->WriteBuffer(&this->mobiles[i].gravidade,4);
		this->arquivo->WriteBuffer(&this->mobiles[i].diferencaMenor,4);
		this->arquivo->WriteBuffer(&this->mobiles[i].diferencaMaior,4);
		this->mobiles[i].getDobras();

		this->arquivo->WriteBuffer(&this->mobiles[i].primeiraCalibracaoX ,4);
        
		for(int j = (this->mobiles[i].forcas.size()-1); j > 0; j--){
			 WORD quantidade = ((this->mobiles[i].forcas[j].x.quantidadeDobras << 1) + this->mobiles[i].forcas[j].x.diferencaInicial);
			 this->arquivo->WriteBuffer(&quantidade ,1);
		}

		for(int j = (this->mobiles[i].forcas.size()-1); j > 0; j--){
			 for(int k = 0; k < this->mobiles[i].forcas[j].x.dobras.size(); k++){
              	  this->arquivo->WriteBuffer(&this->mobiles[i].forcas[j].x.dobras[k] ,1);
			 }
		}
	} */
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
	String dir = GetCurrentDir();
	String nome = dir+"\\data.dll";
	
	TStringList *lista = new TStringList();
	if(!FileExists(nome)){
		lista->SaveToFile(nome);
	}
	if(FileExists(nome))
		this->arquivo = new TFileStream(nome, fmOpenRead);
	else
		return;

	try{
		// Le gravidades
		int n = (4*numeroMobiles);
		if( this->arquivo->Size < (n+numeroMobiles*1430)) return;
		float gravidades[numeroMobiles];
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
