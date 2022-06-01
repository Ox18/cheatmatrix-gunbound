//---------------------------------------------------------------------------

//using namespace std;

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"

#include "Unit4.h"
#include "Unit7.h"
#include "calibracoes.h"
#include "Suporte.h"
#include "classesCB.h"

#include <tlhelp32.h>
#include "IniFiles.hpp"
#include <stdio.h>
#include <StrUtils.hpp>
#include "CMStrUtils.h"
#include <ddraw.h>
#include "extras.h"
#include <math.h>
#include <graphics.hpp>
#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Spin"
#pragma link "Spin"
#pragma resource "*.dfm"
TForm10 *Form10;

DWORD hotkeys[8];

typedef DWORD TPixelLine[800];
typedef TPixelLine * PPixelLine;

TFileCB *arquivoCM;

DWORD ShotPtr;
DWORD ShotOff;
DWORD CameraPtr;
DWORD CameraOff;
DWORD WindAnglePtr;
DWORD WindAngleOff;
DWORD WindPowerPtr;
DWORD WindPowerOff;
DWORD BotPosPtr;
DWORD BotPosOff;
DWORD IndexPtr;
DWORD IndexOff;
DWORD DireitaPtr;
DWORD DireitaOff;
DWORD DelayPtr;
DWORD DelayOff;

TPoint pontoChave(0,0);

DWORD WindAngleToSet = 0;
DWORD WindPowerToSet = 0;
DWORD BotAngleToSet = 0;
int GameInterface;
bool calibraGravidade = false;

short Mobile;
short selfIndex;
bool Direita;
DWORD BotAngle = 0;
double Gravity = 0;
DWORD WindAngle = 0;
DWORD WindPower = 0;
TPoint BotPos(0, 0);
bool TurnUsed = false;
bool TurnFlag = false;
int delay = 1;
int lastdelay = 0;
bool Ligado = false;
TIniFile *allini;
DWORD ShotTicks = 0;
TPoint CurTopo;
TPoint CurFim;
float distFim = 0;
bool RestartNow = false;
bool inGame = false;
int VentoReal = 0;
bool delayChanged = false;
int forcaReserva = 0;
int pontoMaisAlto = 0;

TPointList Shot;
int indiceTopoAim = 0;

TPoint _topoTrajetoria(0,0);
TPoint _ladoTrajetoria(0,0);
TPoint _topoAim(0,0);
TPoint _ladoAim(0,0);

int CurWindAngle = WindAngle;
int CurWindPower = WindPower;

//Lista de calibrações 
//float calibracaoFinal[360];
int ultimaForcaFinal = -1;
int ultimoMobileFinal = -1;

int YComp = 0;

double TG, TY, TX;


bool gbopen = false;

HANDLE Processo = 0;
DWORD gbpid = 0;
AnsiString LastMsg = "";

bool debugFlag = false;

int CurBot;

GdiplusStartupInput gdiplusStartupInput;
ULONG_PTR           gdiplusToken;  

const double pi = 3.141592654;
const double Tempo = 0.05;

std::vector<int> angulos;

typedef struct _Maximus
{
		TPoint topo;
		TPoint fim;
} TMaximus;

typedef struct _Projetil
{
	 double x;
	 double y;
	 double SpeedX;
	 double SpeedY;
}TProjetil;

typedef struct _PointFloatEx
{
	 double x;
	 double y;
	 double g;
} TPointFloatEx;

TPointFloatEx ultimaCalibracao, calibracao;

typedef enum _Posicao
{
	 pos_Acima,
	 pos_Abaixo,
	 pos_Antes,
	 pos_Depois,
	 pos_Igual
}TPosicao;

typedef char tcbits[7];
tcbits tnums[10] =
{
	{1,1,1,1,1,1,0}, //0
	{1,1,0,0,0,0,1},
	{1,1,0,1,1,0,1},
	{1,1,0,1,0,1,1},
	{0,0,1,1,0,1,1}, //4
	{1,1,1,0,0,1,1},
	{0,1,1,0,1,1,1},
	{1,0,0,1,0,1,0},
	{1,1,1,1,1,1,1}, //8
	{1,0,1,1,0,1,1}
};

void __stdcall AddLog(AnsiString valor)
{
	LastMsg = valor;
	Form10->Memo1->Lines->Add(valor);
}

String formataFloat(double valor){
 	return FloatToStrF(valor, ffFixed, 8, 4);
}

String CMIntToHex(INT64 valor, int formato)
{
	String resultado = "";
	String hexes = "0123456789ABCDEF";

	 for(int j = 0; j < formato; j++)
	 {
			 int i = valor & 0xF;
			 valor >>= 4;
			 String s = hexes[i+1];
			 resultado = s+resultado;
	 }

   return resultado;
}

INT64 CMHexToInt(AnsiString s)
{
  INT64 Result = 0;
  s = s.UpperCase();

  if( s.SubString(1,2) == "0X"){
	 s = s.SubString(3,s.Length());
  }

  for( BYTE i = 1; i <= s.Length(); i++ )
  {
    Result <<= 4;
	switch (s[i])
	{
	  case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
	  {
		 Result += (BYTE(s[i]) - BYTE('0'));
	  } break;

	  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	  {
	     Result += (BYTE(s[i]) - BYTE('A') + 10);
	  } break;

	  default: break;
	}
  }
  return Result;
}


int SafeWindPower()
{
		HDC dc = GetDC(0);
		int result = 0;
		__try
		{
			 DWORD p1 = CMGetPixel( dc, 395, 19); //cima
			 DWORD p2 = CMGetPixel( dc, 395, 33); //baixo
			 DWORD p3 = CMGetPixel( dc, 392, 21); //esquerda cima
			 DWORD p4 = CMGetPixel( dc, 398, 21); //direita cima
			 DWORD p5 = CMGetPixel( dc, 392, 31); //esquerda baixo
			 DWORD p6 = CMGetPixel( dc, 398, 31); //direita baixo
			 DWORD p7 = CMGetPixel( dc, 395, 26); //Meio

			 char bits[7];
			 bits[0] = (AnsiChar)((p1 & 0xFFFF00) == 0x9cba00);
			 bits[1] = (AnsiChar)((p2 & 0xFFFF00) == 0x9cba00);
			 bits[2] = (AnsiChar)((p3 & 0xFFFF00) == 0x9cba00);
			 bits[3] = (AnsiChar)((p4 & 0xFFFF00) == 0x9cba00);
			 bits[4] = (AnsiChar)((p5 & 0xFFFF00) == 0x9cba00);
			 bits[5] = (AnsiChar)((p6 & 0xFFFF00) == 0x9cba00);
			 bits[6] = (AnsiChar)((p7 & 0xFFFF00) == 0x9cba00);

			 int j = 0;
			 for(int i = 0; i < 10; i++)
			 {
				if( memcmp(&tnums[i][0], &bits[0], 7) == 0 )
				{
					j = i;
					break;
				}
			 }

			 p1 = CMGetPixel( dc, 395+11, 19); //cima
			 p2 = CMGetPixel( dc, 395+11, 33); //baixo
			 p3 = CMGetPixel( dc, 392+11, 21); //esquerda cima
			 p4 = CMGetPixel( dc, 398+11, 21); //direita cima
			 p5 = CMGetPixel( dc, 392+11, 31); //esquerda baixo
			 p6 = CMGetPixel( dc, 398+11, 31); //direita baixo
			 p7 = CMGetPixel( dc, 395+11, 26); //Meio

			 bits[0] = (AnsiChar)((p1 & 0xFFFF00) == 0x9cba00);
			 bits[1] = (AnsiChar)((p2 & 0xFFFF00) == 0x9cba00);
			 bits[2] = (AnsiChar)((p3 & 0xFFFF00) == 0x9cba00);
			 bits[3] = (AnsiChar)((p4 & 0xFFFF00) == 0x9cba00);
			 bits[4] = (AnsiChar)((p5 & 0xFFFF00) == 0x9cba00);
			 bits[5] = (AnsiChar)((p6 & 0xFFFF00) == 0x9cba00);
			 bits[6] = (AnsiChar)((p7 & 0xFFFF00) == 0x9cba00);

			 int k = 0;
			 for(int i = 0; i < 10; i++)
			 {
				if( memcmp(&tnums[i][0], &bits[0], 7) == 0 )
				{
					k = i;
					break;
				}
			 }
			 result = j*10+k;
		}
		__finally
		{
			if(dc)
			  ReleaseDC(0, dc);
		}

	 return result;
}

void WaitTime(DWORD valor)
{
   int j = (valor <= 10) ? 1 : 10;
	 DWORD inicio = GetTickCount();
	 while((GetTickCount() - inicio) < valor)
	 {
		Sleep(j);
		Application->ProcessMessages();
	 }
}

TPointFloatEx decompoeCalibracao(AnsiString valor){
	TPointFloatEx resultado;
	
	int j = 0;
	AnsiString s = "";
	for(int i = 1; i <= valor.Length(); i++){
		if(valor[i] == '|'){
			 j++;
			 switch(j){
				case 1: resultado.y = StrToFloat(s); break;
				case 2: resultado.g = StrToFloat(s); break;
			 }
			 s = "";
			 continue;
		}
		s = s + valor[i];
	}
	return resultado;
}

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

bool pegaProximoAngulo(){
	TIniFile *iniCal = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");

	__try{
		for(int i = 0; i < angulos.size(); i++){
			if((angulos[i]%360) > WindAngleToSet){
				 AnsiString valor = iniCal->ReadString(IntToStr(Mobile), "A"+IntToStr(angulos[i])+"P"+Form10->SpinEdit1->Text, "");
				 if(valor.Length() == 0){
					  WindAngleToSet = angulos[i];
					  WindPowerToSet = Form10->SpinEdit1->Value;
					  return true;
				 }
			}
		}

		AddLog("Calibração Finalizada!");
	}__finally{
		iniCal->Free();
	}
	return false;
}

TPointFloatEx pegaCalibracaoAnterior(int angulo){
		TIniFile *iniCal = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");
		TPointFloatEx ponto;

		ponto.x = 0;
		ponto.y = 0;
		ponto.g = 0;

		for(int i = (angulos.size()-1); i >= 0; i--){
			if(angulos[i] < angulo){
				 AnsiString nome =  ("A"+IntToStr(angulos[i])+"P"+Form10->SpinEdit1->Text);
				 AnsiString valor = iniCal->ReadString(IntToStr(Mobile), nome, "");
				 if(valor.Length() == 0){
					  ponto.x = 0;
					  ponto.y = 0;
					  ponto.g = 0;
				 }else{
					  ponto = decompoeCalibracao(valor);
					  break;
				 }
			}
		}
		return ponto;
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

void listaAngulos(){
	angulos.clear();

	for(int i = 0; i < 360; i++){
		//if(i <= 90 || (i > 180 && i <= 270) )
		if( isAnguloDeConvergencia(i, Form10->SpinEdit1->Value) ){
				angulos.push_back(i);
		}
	}

	// Ordena os angulos
	for(int i = 0; i < angulos.size(); i++){
		for(int j = 0; j < angulos.size(); j++){
			 if(angulos[j] > angulos[i]){
				int k = angulos[i];
				angulos[i] = angulos[j];
				angulos[j] = k;
			 }
		}
	}

}

bool isAnguloDeConvergencia(int angulo, bool Xflag = false){
	double power = (double)Form10->SpinEdit1->Value;
	return isAnguloDeConvergencia(angulo, power, Xflag);
}

int __fastcall GetBotAngle()
{
	 int j = GetInterfaceGB();
	 GameInterface = j;
	 int k = 0;
	 if(j == 1)
		 k = getAngleOld();
	 else
		 k = getAngleNew();

	 //Application->ProcessMessages();	 
	 if(Direita)
		 k += 360;
	 else
		 k = 180 - k;
	 BotAngle = (k % 360);

	 return (k % 360);
}

AnsiString ActiveCaption()
{
	HWND Handle;
	long Len;
	AnsiString Title;

	AnsiString Result = "";
	Handle = GetForegroundWindow();
	if(Handle)
	{
		Len = GetWindowTextLength(Handle) + 1;
		Title.SetLength(Len);
		GetWindowText(Handle, Title.c_str(), Len);
	}
	return Trim(Title);
}

unsigned long GetTargetProcessIdFromProcname(char *procName)
{
    PROCESSENTRY32 pe;
	HANDLE thSnapshot;
	BOOL retval, ProcFound = false;
	DWORD pid = 0;
	int total = 0;

	thSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
	if(thSnapshot == INVALID_HANDLE_VALUE)
	{
	   return 0;
	}

	pe.dwSize = sizeof(PROCESSENTRY32);
	retval = Process32First(thSnapshot, &pe);
	total = 0;
	
	while(retval)
	{
		if(  LowerCase(StrPas(PCHAR(pe.szExeFile))) == LowerCase(StrPas(PCHAR(procName))) )
		{
			ProcFound = true;
			total++;
			if(total > 1)
			return pe.th32ProcessID;
			pid = pe.th32ProcessID;
		}

		retval    = Process32Next(thSnapshot,&pe);
		pe.dwSize = sizeof(PROCESSENTRY32);
	 }

	 CloseHandle(thSnapshot);
     return 0;

}


//----------------------------------------------------------------------------

double __fastcall IntToRadian(int valor)
{
   return valor*pi/180;
}

int __fastcall GetIndex()
{
	__try
	{
		DWORD address = 0;
		ReadProcessMemory(Processo, (PVOID)(IndexPtr), &address, 4, NULL);
		WaitTime(1);
		ReadProcessMemory(Processo, (PVOID)(address+IndexOff), &selfIndex, 1, NULL);
		return selfIndex;
	}
	__except(1)
	{
		return 0;
	}
}

bool __fastcall FaceToRight()
{
	__try
	{
		DWORD address = 0;
		int i = 0;
		ReadProcessMemory(Processo, (PVOID)(DireitaPtr), &address, 4, NULL);
		WaitTime(1);
		ReadProcessMemory(Processo, (PVOID)(address+DireitaOff+(selfIndex*0x1C)), &i, 1, NULL);
		bool toRight = (i == 1);
		//toRight = (BackShot ? !toRight : toRight);
		toRight = ((Mobile == 2 || Mobile == 13) ? !toRight : toRight);
		Direita = toRight;
		return toRight;
	}
	__except(1)
	{
		return false;
	}
}

TPoint __fastcall GetAimFix(int index)
{
		 switch(index)
		 {
			 case 0: return TPoint(46, 30);  //Armor
			 case 1: return TPoint(60, 36);  //Mage
			 case 2: return TPoint(140, 33); //Nak
			 case 3: return TPoint(60, 38);  //Trico
			 case 4: return TPoint(56, 42);  //Big Foot
			 case 5: return TPoint(75, 38);  //Boomer
			 case 6: return TPoint(64, 34);  //Raon ok
			 case 7: return TPoint(60, 36);  //Lightning
			 case 8: return TPoint(60, 40);  //J.D
			 case 9: return TPoint(42, 35);  //A.Sate
			 case 10:
			 {
				 //testPos = eng;
				 int s = GetShotMode(2);
				 //ShotIndex = s;
						 
				 if(s == 0)
					 return TPoint(54, 50);  //Ice
				 else
					 return TPoint(40, 45);
						 
			 } 
			 case 11: return TPoint(60, 30); //Turtle ok
			 case 12: return TPoint(55, 38);  //Grub
			 case 13: return TPoint(135, 28); //Aduka  ok
			 case 14: return TPoint(56, 42);  //Kalsiddon
			 case 15: return TPoint(46, 30);  //JFrog
			 case 16: case 125: return TPoint(54, 46);  //Dragon
			 case 17: case 209: return TPoint(45, 35);  //Knight
         	 default: break;
		 }
		 return TPoint(0, 0);
}

double v0 = 0.5;
TPoint __fastcall GetBotPos()
{
  int j = 0;
	j = Mobile;

	int v1 = GetAimFix(j).x;
	int v2 = GetAimFix(j).y;
	
	DWORD addressX = 0;

	int botX = 0;
	int botY = 0;
	int InclinBot = 0;
	
	__try
	{
		ReadProcessMemory(Processo, (PVOID)(BotPosPtr), &addressX, 4, NULL);
		ReadProcessMemory(Processo, (PVOID)(addressX+BotPosOff+(0x1C*selfIndex)), &botX, 2, NULL);
		WaitTime(1);
		ReadProcessMemory(Processo, (PVOID)(addressX+BotPosOff+(0x1C*selfIndex)+4), &botY, 2, NULL);
		ReadProcessMemory(Processo, (PVOID)(addressX+BotPosOff+(0x1C*selfIndex)+8), &InclinBot, 2, NULL);
	}
	__except(1)
	{
		return TPoint(0,0);
	}
	//int originaly = botY;
	BOOL tdir = ( (j == 2 || j == 13 )? !Direita : Direita );
	//if(BackShot)
	//  tdir = !tdir;

	int angle;
	if (tdir)
		 angle = (InclinBot + v1);
	else
		 angle = (InclinBot + (180 - v1));

	botX += (int)(cos(IntToRadian(angle)) * (v0 + v2));
	botY -= (int)(sin(IntToRadian(angle)) * (v0 + v2)) - 20;

	BotPos = TPoint(botX, botY);
	//YComp = (originaly - botY);
	return TPoint(botX, botY);
}

void GetValues()
{
	if(!Form10->freezaVento->Checked){
		__try
		{
			DWORD Address = 0;
			ReadProcessMemory(Processo, (PDWORD)(WindAnglePtr), &Address, 4, NULL);
			ReadProcessMemory(Processo, (PDWORD)(Address+WindAngleOff), &WindAngle, 2, NULL);

			Address = 0;
			ReadProcessMemory(Processo, (PDWORD)(WindPowerPtr), &Address, 4, NULL);
			ReadProcessMemory(Processo, (PDWORD)(Address+WindPowerOff), &WindPower, 1, NULL);

			Address = 0;
			ReadProcessMemory(Processo, (PDWORD)(DelayPtr), &Address, 4, NULL);
			ReadProcessMemory(Processo, (PDWORD)(Address+DelayOff), &delay, 1, NULL);
			WaitTime(1);
		}
		__except(1)
		{
			return;
		}
	}else{
		WindAngle = Form10->SpinEdit5->Value;
		WindPower = Form10->SpinEdit4->Value;
	}
}

//Calibrar gravidade tmb
TMaximus __fastcall GetBestEffect(TPointFloatEx WindEffect, std::vector<TPoint>& linha, bool usearray = false)
{
  TMaximus result;
	__try
	{
			double Alfa,Beta,Gama,Delta; //Variaveis de controle de funcções aritméticas - sen, cos tg
			//float WindSpeedX = 0;
			//float WindSpeedY = 0;
			double ShotSpeedX = 0;
			double ShotSpeedY = 0;

			double AceleracaoX = 0;
			double AceleracaoY = 0;

			AceleracaoX = WindEffect.x;
			//if(WindEffect.y < 0.0001) WindEffect.y = 0;
			//if(WindEffect.g < 0.0001) WindEffect.g = 0;
			AceleracaoY = (double)(WindEffect.y - WindEffect.g);

			Alfa = cos(IntToRadian(BotAngle));
			Beta = sin(IntToRadian(BotAngle));
			Gama = cos(IntToRadian(CurWindAngle));
			Delta = sin(IntToRadian(CurWindAngle));

			//AceleracaoX = ( (int)(CurWindPower * Gama) );
			//AceleracaoY = ( (int)(CurWindPower * Delta) ) - WindEffect.g;

			TProjetil Projetil;

			ShotSpeedX = 400 * Alfa;
			ShotSpeedY = 400 * Beta;

			Projetil.x = BotPos.x;
			Projetil.y = BotPos.y;
			Projetil.SpeedX = ShotSpeedX;
			Projetil.SpeedY = ShotSpeedY;

			TPoint BestTop(0, 99999);
			TPoint BestEnd(0, 0);

			linha.clear();

			bool chdir = false;
			TPoint Ponto1 = TPoint(0,0);
			TPoint Ponto2 = TPoint(0,0);
			TPoint LastPoint = TPoint(0,0);
			TPoint penultimo = TPoint(0,0);
			int k = 0;
			TPosicao spos = pos_Igual;
			bool convergido = false;

			if(Projetil.SpeedX != 0 || Projetil.SpeedY != 0)
			while(Projetil.x < 1800 && Projetil.x > 0 && Projetil.y > -5000 && Projetil.y < 2000)
			{
				penultimo = BestEnd;

				Projetil.x += Projetil.SpeedX*Tempo;// + AceleracaoX*Tempo*Tempo*0.5;
				Projetil.y -= Projetil.SpeedY*Tempo;// + AceleracaoY*Tempo*Tempo*0.5;

				Projetil.SpeedX += AceleracaoX * Tempo;
				Projetil.SpeedY += AceleracaoY * Tempo;

				BestEnd = TPoint(Projetil.x, Projetil.y);
				//--- Verifica inversão da trajetoria (no inicio é sempre contínua pra cima)

				if(!convergido){
						k++;
						if(k == 1)
							 Ponto1 = BestEnd;
						else
						if(k == 2)
						{
							 Ponto2 = BestEnd;
							 if(Ponto2.y < Ponto1.y)
								 spos = pos_Acima;
							 else
							 if(Ponto2.y > Ponto1.y)
								 spos = pos_Abaixo;
							 else
							   k--;
						}
						else
						{
							 TPosicao cpos = pos_Igual;
							 if(BestEnd.y < LastPoint.y)
								 cpos = pos_Acima;
							 else
							 if(BestEnd.y > LastPoint.y)
									cpos = pos_Abaixo;
							 else
								 cpos = pos_Igual;

							 if(cpos != spos && cpos != pos_Igual)
							 {
									 chdir = true;
									 BestTop = TPoint(LastPoint.x, LastPoint.y);
									 convergido = true;
							 }
						}
					  
						//----------------------------------------------------------
						if(Projetil.y < BestTop.y)
							BestTop = TPoint(Projetil.x, Projetil.y);
				}
					
				if(usearray)
					 linha.push_back(TPoint(Projetil.x, Projetil.y));
				LastPoint = BestEnd;
			}

			if(chdir)
				result.topo = BestTop;
			else
				result.topo = TPoint(0, 99999);
			result.fim = BestEnd;

	}
	__except(1)
	{
	  return result;
	}
	return result;
}


HANDLE FindGBProcess(DWORD id)
{

    if(Processo != 0) CloseHandle(Processo);
		/*HWND Janela = FindWindow("Softnyx","Gunbound");
		if(!Janela) return 0;
		DWORD pid = 0;
		GetWindowThreadProcessId(Janela, &pid);
		if(pid == 0) return 0;  */
		DWORD pid = id;
		gbpid = pid;
		Processo = OpenProcess(PROCESS_ALL_ACCESS, false, pid);
		return Processo;
}

//Atira
void ShotNow()
{
		 TPoint Pt;

		 Pt.x = 100;
		 Pt.y = 100;
		 Pt.x = (int)(Pt.x * (65535 / Screen->Width));
		 Pt.y = (int)(Pt.y * (65535 / Screen->Height));
		 mouse_event(MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE, Pt.x, Pt.y, 0, 0);
		 mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0 ,0);
		 mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0 ,0);

		 int j = 100;
		 int k = 0;
		 for(int i = 1; i < j; i++)
		 {
		    k++;
				if(k > 10)
				  Sleep(1);
				mouse_event(MOUSEEVENTF_MOVE, i, 0, 0 ,0);
		 }
		 
		 mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0 ,0);
		 Mouse->CursorPos.x = 300;
		 Mouse->CursorPos.y = 300;
}

DWORD endTiro = 0;

TPoint __fastcall GetShotPos()
{
		DWORD Address = 0;
		short x = 0, y = 0;
		__try
		{
				if(endTiro == 0){
					ReadProcessMemory(Processo, (PDWORD)(ShotPtr), &Address, 4, NULL);
					endTiro = Address+ShotOff;
				//WaitTime(1);
				}
				ReadProcessMemory(Processo, (PDWORD)(endTiro), &x, 2, NULL);
				ReadProcessMemory(Processo, (PDWORD)(endTiro+4), &y, 2, NULL);
                WaitTime(1);
				ReadProcessMemory(Processo, (PDWORD)(endTiro), &x, 2, NULL);
				ReadProcessMemory(Processo, (PDWORD)(endTiro+4), &y, 2, NULL);
		}
		__except(1)
		{
                return TPoint(0,0);
		}
	//WaitTime(5);
	return TPoint(x+Form10->SpinEdit6->Value ,y+Form10->SpinEdit7->Value);//
}

double __fastcall Modulo(double valor)
{
	 if(valor < 0)
		 return valor*(-1);
	 return valor;
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%% GetShotLine %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TPointFloatEx GetShotLine()
{
		CurWindAngle = WindAngle;
		CurWindPower = WindPower;

		TurnUsed = true;
		TPointFloatEx pfnil;

		TG = ultimaCalibracao.g;
		TX = ultimaCalibracao.x;
		TY = ultimaCalibracao.y;

		pfnil.x = 0;
		pfnil.y = 0;
		pfnil.g = 0;

		Shot.Clear();
		TPointList Aim;
		int indiceTopo = 0;

		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		// Verifica o angulo do bot, para evitar que se acerte
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

		if(WindAngleToSet > 0 && WindAngleToSet <= 90){
			BotAngleToSet = 98 + (int)Modulo(cos(IntToRadian(WindAngleToSet))*WindPowerToSet*0.65);
		}else if(WindAngle > 90 && WindAngle <= 270){
            BotAngleToSet = 98 - (int)Modulo(cos(IntToRadian(WindAngleToSet))*WindPowerToSet*0.3);
		}else{
			BotAngleToSet = 98 + (int)Modulo(cos(IntToRadian(WindAngleToSet))*WindPowerToSet*0.65);
		}

		//Muda o angulo do bot
		int	ultimoAnguloBot = 0;
		int count = 0;
		if(BotAngle != BotAngleToSet)
		{
			AddLog("Mudando angulo do bot...");
			while(BotAngle != BotAngleToSet)
			{
				if(!Ligado){
					return pfnil;
				}
							
				Application->ProcessMessages();
						
				int key = 0;

				//Pressiona tecla pra baixo caso o angulo seja maior
				if(BotAngle > BotAngleToSet){
					key = 38;
				} else {
					//senão pressiona para cima, e assim vai até chegar ao angulo certo
					if(BotAngle < BotAngleToSet){
						key = 40;
					} else {
						break;
					}
				}
						
				keybd_event(key,MapVirtualKeyA(key, 0), 0 , 0);
				WaitTime(200);
				keybd_event(key,MapVirtualKeyA(key, 0), KEYEVENTF_KEYUP,0);
				WaitTime(200);

				if(BotAngle == ultimoAnguloBot && ultimoAnguloBot != 0){
					if(count > 5){
					   return pfnil;
					}
					count++;
				}

				ultimoAnguloBot = BotAngle;
			}
		}

		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		// Verifica vento por pixel, para ter certeza de que o tiro
		// esta no vento certo
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		int force = SafeWindPower();
		VentoReal = force;

		//__asm
		//	plb1:

		int voltas = 0;
		while(force != WindPowerToSet || WindAngle != WindAngleToSet){
			//Lmebando que neste momento tem um time freezando o vento, assim podemos
			//entrar em um loop para esperar o vento chegar no valor certo, pressioando
			//F8 enquanto não for o vento correto.
			if(force != WindPowerToSet || WindAngle != WindAngleToSet)
			{
					AddLog("Vento inválido...");
					while(force != WindPowerToSet || WindAngle != WindAngleToSet)
					{
						if(!Ligado){
							return pfnil;
						}
						
						force = SafeWindPower();
						VentoReal = force;
						keybd_event(VK_F8,0xc2,0 , 0);
						keybd_event(VK_F8,0xc2, KEYEVENTF_KEYUP,0);
						WaitTime(500);
						keybd_event(VK_F8,0xc2,0 , 0);
						keybd_event(VK_F8,0xc2, KEYEVENTF_KEYUP,0);
						WaitTime(500);
						voltas++;

						if(voltas > 20) return pfnil;
					}
			}

			if(!Ligado){
				return pfnil;
			}                 		

			WaitTime(150);
			//Se o vento continuar não sendo o valor desejado, volta e reinicia o loop
		}

		//Seta vento atual para mostrar na tela
		CurWindAngle = WindAngle;
		CurWindPower = WindPower;

		//Se não for a vez do nosso bot jogar, retorna da função e espera até que seje
		if(delay != selfIndex)
			return pfnil;

    	WaitTime(500);

		//Sei lah pra que eu coloquei isso, mas num to com cabeça agora pra lembrar
		//Verifica se o processo selecionado pelo bot é o certo
		if(!Processo)
		{
			 AddLog("Falha ao pegar o processo, saindo da verificação...");
			 DWORD pid = GetTargetProcessIdFromProcname("gunbound.gme");
			 if(pid > 0)
				FindGBProcess(pid);
				
			 return pfnil;
		}

		//Posição do tiro
		TPoint npos;
		//posição de origem do tiro
		TPoint opos;
		opos = GetShotPos();
		AddLog("Atirando.. Força: FULL");
		
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		// Tiro automatico
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		ShotNow();
		AddLog("Tiro completo.");
		WaitTime(100);
		int j = 0;
		npos = GetShotPos();
		
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		// Verifica se nao houve erro no tiro (aparentemente o tiro não saiu
		// do lugar, indicando ou erro ao atirar ou erro na leitura da memoria)
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		AddLog("Verificando validade do tiro");
		j = GetTickCount();
		while(npos.x == opos.x && npos.y == opos.y)
		{
			 //j++;
			 if(!Ligado){
				return pfnil;
			 }

			 if((GetTickCount() - j) > 1200)
			 {
				 AddLog("Falha ao iniciar o calculo da trajetoria.. ");
				 WaitTime(40);
				 return pfnil;
			 }

       		 Application->ProcessMessages();
			 npos = GetShotPos();
		}

		j = 0;
		npos = GetShotPos();
		TPoint LastPoint(0,0);
        DWORD inicio = 0;
		Application->ProcessMessages();
		
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		// Verifica se o tiro chegou ao fim de seu trajeto, terminando o
		// período da jogada, e guarda todos os pontos da trajetoria
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		AddLog("Aguardando fim da trajetoria do projetil...");
		while(true)
		{
			 if(!Ligado) return pfnil;

			 //lê posição do tiro
			 TPoint Ponto = GetShotPos();
			 if(Ponto.x == LastPoint.x && Ponto.y == LastPoint.y)
			 {
				 if(inicio == 0)
					 inicio = GetTickCount();
				 else
				 {
					 //Se o tiro parar de mover por 1 seg sai do loop
					 if((GetTickCount() - inicio) > 1200)
						 break;
				 }
			 }
			 else
			 {
				 //Adiciona posição à lista, caso não esteja repitido
				 Shot.Add(Ponto);
				 inicio = 0;
			 }

			 if(delay != selfIndex)
				 break;

			 Application->ProcessMessages();
			 LastPoint = Ponto;
		}
		AddLog("Tiro finalizado. Calculando trajetória...");

		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		//Ate aqui atirou e guardou todos os pontos da trajetoria 
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		TPoint ShotY(0,99999);
		TPoint ShotFim = Shot.Points[Shot.count()-1];//LastPoint;
		ShotTicks = Shot.count();
		//xii.. deu algum problema..
		if(ShotTicks == 0)
		{
			 AddLog("Falha ao pegar os pontos da trajetória do projétil! reiniciando...");
			 return pfnil;
		}

		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		//Pega o topo da trajetoria do projetil (menor Y)
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		for(int i = 0; i < Shot.count(); i++)
		{
			if(!Ligado){
				return pfnil;
			}
			
			 if(Shot[i].y < ShotY.y)
			 {
					ShotY = Shot[i];
					indiceTopo = i;
					pontoMaisAlto = Shot[i].y;
			 }
		}

		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		//Seta os efeitos iniciais 
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		TPointFloatEx effect;
		
		//ultimoCal = ultima calibração
		effect.y = ultimaCalibracao.y;
		effect.g = ultimaCalibracao.g;

		//pontos máximos da trajetoria, ou seja, topo e fim
		TMaximus UltimoAim, CurAim;
		std::vector<TPoint> emp;
		inicio = 0;

		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		// Acrescimo dos efeitos - quanto menor, mais preciso, porem mais
		// demorado.. 4 casas decimais quebra o galho
		//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
		double gdiv = 0.01f;
		double ydiv = 0.001f;
		int diverror = 0;

		int flagY = 0;
		bool calibrarGravidade = false;

		_topoAim = ShotY;

		CurAim = GetBestEffect(effect, emp);
		int diferenca = CurAim.topo.y - ShotY.y;
		flagY = (diferenca < 0 || diferenca > 9999)?-1:1;
		int ultimaDiferenca = diferenca;
		int ultimoE = 0;

		if(flagY == 0)
			return effect;

		// Calibra Gravidade
		if(effect.g == 0 || CurWindPower == 0){
			if(diferenca == 0) return effect;
			int xs = 0;
			while(true){
				Application->ProcessMessages();

				if(!Ligado){
					return pfnil;
				}

				CurAim = GetBestEffect(effect, emp);
				_topoTrajetoria = CurAim.topo;
				diferenca = (CurAim.topo.y - ShotY.y);
				int novaFlag = (diferenca < 0 || diferenca > 9999)?-1:1;

				if(diferenca == 0)
					return effect;

				if(novaFlag != flagY){
					xs++;
				}

				if((novaFlag != flagY && ultimaDiferenca < 9999) || (novaFlag != flagY && xs > 5)){
					if( Modulo(diferenca) > Modulo(ultimaDiferenca) )
						effect.g = ultimoE;
					return effect;
				}

				ultimaDiferenca = diferenca;
				ultimoE = effect.g;

				if(CurAim.topo.y > 9999){
					effect.g += gdiv;
				}else{
                    effect.g += (-1 * gdiv * novaFlag);
				}

				TG = effect.g;
				TY = effect.y;
			}
			return effect;
		// Calibra Y
		} else {
			if(isAnguloDeConvergencia(WindAngleToSet)){
				if(diferenca != 0){
					while(true){
						Application->ProcessMessages();

						if(!Ligado){
							return pfnil;
						}
					
						CurAim = GetBestEffect(effect, emp);
						_topoTrajetoria = CurAim.topo;
						diferenca = (CurAim.topo.y - ShotY.y);
						int novaFlag = (diferenca < 0)?-1:1;

						if(diferenca == 0) break;

						if(novaFlag != flagY){
							if( Modulo(diferenca) > Modulo(ultimaDiferenca) )
								effect.y = ultimoE;
							break;
						}

						ultimaDiferenca = diferenca;
						ultimoE = effect.y;

						effect.y += (gdiv * novaFlag);

						TG = effect.g;
						TY = effect.y;
					}
				}
			}
		}

		return effect;

}

//*****************************************************************************

AnsiString GetRandomStr()
{
	 AnsiString letras = "abcdefghijklmnopqrstuvwxyz0123456789";
	 AnsiString final = "";
	 Randomize();
	 for(int i = 1; i < 20; i++ )
	 {
			 int k = Random(35)+1;
			 final = final + letras[k];
	 }
	 return final;
}

//---------------------------------------------------------------------------
__fastcall TForm10::TForm10(TComponent* Owner)
	: TForm(Owner)
{

}
//---------------------------------------------------------------------------

void TForm10::WMHotKey(TMessage& Message)
{
	
  //Ctrl+Enter - Liga o aim
	if( Message.WParam == hotkeys[0] )
	{
			//SpinEdit1Change(NULL);
			Ligado = !Ligado;

	}

	if( Message.WParam == hotkeys[1] )
	{
			SpinEdit5->Value++;
	}

	if( Message.WParam == hotkeys[2] )
	{
			SpinEdit5->Value--;
	}
   /*
	if( Message.WParam == hotkeys[1] )
	{
			//RestartNow = true;
			SafeWindPower();
	}
	if( Message.WParam == hotkeys[2] )
	{
			//RestartNow = true;
			///===+
			int block = SpinEdit3->Value;

			int total = 0;
			keybd_event(0x20, 0, 0, 0);

			DWORD id = 0;
			HWND janela = FindWindowA(NULL, "gunbound");

			 TPoint Pt;

		 Pt.x = 100;
		 Pt.y = 100;
		 Pt.x = (int)(Pt.x * (65535 / Screen->Width));
		 Pt.y = (int)(Pt.y * (65535 / Screen->Height));
		 mouse_event(MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE, Pt.x, Pt.y, 0, 0);
		 mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0 ,0);
		 mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0 ,0);

		 int j = block;
		 int k = 0;
		 DWORD s1 = GetTickCount();
               
		 bool f = false;

		 for(int i = 1; i < j; i++)
		 {
				//k++;
				//if(k > 10)
				{
					 WaitTime(i);
					 mouse_event(MOUSEEVENTF_MOVE, SpinEdit3->Value, 0, 0 ,0);
				}
				//if((GetTickCount() - s1) >= block)
				{
           //break;
				}
		 }
		 DWORD s2 = GetTickCount();
		 Memo1->Lines->Add(IntToStr(s2-s1) + " >>> " + IntToStr(block));
		 mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0 ,0);
		 Mouse->CursorPos.x = 300;
		 Mouse->CursorPos.y = 300;
	}

	*/

	if( Message.WParam == hotkeys[3] )
	{
			SpinEdit4->Value = SpinEdit4->Value+1;
	}

	if( Message.WParam == hotkeys[4] )
	{
			SpinEdit4->Value = SpinEdit4->Value-1;
	}

	if( Message.WParam == hotkeys[5] )
	{
			Edit1->Text = formataFloat(StrToFloat(Edit1->Text)+0.01);
	}

	if( Message.WParam == hotkeys[6] )
	{
			Edit1->Text = formataFloat(StrToFloat(Edit1->Text)-0.01);
	}
	if( Message.WParam == hotkeys[7] )
	{
		   /*	pRGBArray pixels = getLinha();

			for(int i = 0; i < (sizeof (*pixels))/(sizeof (*PRGBArray)) ; i++){
				StringGrid1->Cells[0][i] = rgbToColor(pixels[i]);
			}
            */
			//Pixel = NULL;
			//Bmp->Free();
			//return result;
	}


}

void SetHotKey(DWORD index, DWORD modifier, DWORD valor)
{
   DWORD id = GlobalAddAtom(GetRandomStr().c_str());
	 RegisterHotKey(Form10->Handle, id, modifier, valor);
	 hotkeys[index] = id;
}

void UpdateOffsets()
{

}

void __fastcall TForm10::FormCreate(TObject *Sender)
{
		Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
		allini = new TIniFile(ExtractFilePath(ParamStr(0))+"data.ini");
		ShotPtr = HexToInt(allini->ReadString("S0", "ShotPtr", "0"));
		ShotOff = HexToInt(allini->ReadString("S0", "ShotOff", "0"));
		CameraPtr = HexToInt(allini->ReadString("geral", "CameraPtr", "0"));
		CameraOff = HexToInt(allini->ReadString("geral", "CameraOff", "0"));
		WindAnglePtr = HexToInt(allini->ReadString("geral", "WindAnglePtr", "0"));
		WindAngleOff = HexToInt(allini->ReadString("geral", "WindAngleOff", "0"));
		WindPowerPtr = HexToInt(allini->ReadString("geral", "WindPowerPtr", "0"));
		WindPowerOff = HexToInt(allini->ReadString("geral", "WindPowerOff", "0"));
		BotPosPtr = HexToInt(allini->ReadString("geral", "BotPosPtr", "0"));
		BotPosOff = HexToInt(allini->ReadString("geral", "BotPosOff", "0"));
		IndexPtr = HexToInt(allini->ReadString("geral", "IndexPtr", "0"));
		IndexOff = HexToInt(allini->ReadString("geral", "IndexOff", "0"));
		DireitaPtr = HexToInt(allini->ReadString("geral", "DireitaPtr", "0"));
		DireitaOff = HexToInt(allini->ReadString("geral", "DireitaOff", "0"));
		DelayPtr = HexToInt(allini->ReadString("geral", "DelayPtr", "0"));
		DelayOff = HexToInt(allini->ReadString("geral", "DelayOff", "0"));

		//---------------------------------------------
		// Carrega ultimas calibrações para agilização

		ultimaCalibracao.g = allini->ReadFloat("geral", "CG", 0);
		ultimaCalibracao.x = allini->ReadFloat("geral", "CX", 0);
		ultimaCalibracao.y = allini->ReadFloat("geral", "CY", 0);

		// Lista os angulos para a força em questao
		listaAngulos();

		if(angulos.size() > 0)
			WindAngleToSet = angulos[0];//allini->ReadInteger("geral", "LastWA", 0);
		else
			WindAngleToSet = allini->ReadInteger("geral", "LastWA", 0);
			
		WindPowerToSet = SpinEdit1->Value;//allini->ReadInteger("geral", "LastWF", 0);
		forcaReserva = WindPowerToSet;
		//---------------------------------------------
		//Atalhos

		SetHotKey(0, MOD_CONTROL, 'B' );
		SetHotKey(1, MOD_CONTROL, 'A');
		SetHotKey(2, MOD_CONTROL + MOD_SHIFT, 'A');
		SetHotKey(3, MOD_CONTROL, 'F');
		SetHotKey(4, MOD_CONTROL + MOD_SHIFT, 'F');
		SetHotKey(5, MOD_CONTROL, 'G');
		SetHotKey(6, MOD_CONTROL + MOD_SHIFT, 'G');
		SetHotKey(7, MOD_CONTROL, 'Q');
		/*SetHotKey(1, MOD_CONTROL, 48 );
		SetHotKey(2, MOD_CONTROL, 81);
		SetHotKey(3, MOD_CONTROL, 68);
		SetHotKey(4, MOD_CONTROL + MOD_SHIFT, 68);
		SetHotKey(5, MOD_CONTROL, 83);
		//SetHotKey(6, MOD_CONTROL + MOD_SHIFT, 83); */
		//SetHotKey(7, MOD_CONTROL + MOD_SHIFT, 80);

		// Pega proximo angulo a ser calibrado
		pegaProximoAngulo();
		//ultimaCalibracao = pegaCalibracaoAnterior(WindAngleToSet);

		//Button7Click(NULL);

		int k = 0;
		int n = 0;
		for(int i = 1; i < 26; i++){
			n = 0;
			for(int j = 0; j < 360; j++){
				if(isAnguloDeConvergencia(j,i)){
					k++;
					n++;
				}
			}
			Memo1->Lines->Add(IntToStr(n));
		}
		Memo1->Lines->Add(">>"+IntToStr(k));
		arquivoCM = new TFileCB();
}
//---------------------------------------------------------------------------

void __fastcall TForm10::ComboBox1Change(TObject *Sender)
{
   AnsiString val = "S0";
	 if(ComboBox1->ItemIndex == 1) val = "S1";
	 else if(ComboBox1->ItemIndex == 11) val = "S11";
	 ShotPtr = HexToInt(allini->ReadString(val, "ShotPtr", 0));
	 ShotOff = HexToInt(allini->ReadString(val, "ShotOff", 0));
	 CurBot = ComboBox1->ItemIndex;
}
//---------------------------------------------------------------------------

typedef struct _resto{
	int angulo;
	int forca;
	double resto;
} TResto;

double consertaResto(double valor){
	double nvalor = Modulo(valor);
	if(nvalor > 0.4 && nvalor < 0.7){
     	return 0.6;
	}else if(nvalor >= 0.7 /*&& nvalor <= 1.5*/){
		return 0.9;
	}else{
     	return 0;
	}
}

BYTE consertaRestoBinario(double valor){
 	return ( consertaResto(valor) > 0.7 )?1:0;
}

void __fastcall TForm10::Button1Click(TObject *Sender)
{
/*	TIniFile *ini = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");
	TIniFile *data = new TIniFile(ExtractFilePath(ParamStr(0))+"data.ini");
	erroLog->Clear();
	Memo3->Clear();

	//float menor = (SpinEdit8->Value/100);
	//float maior = menor+0.3;
	int maiorTotal = 0;

	String s = "";
    String bytesFinal = ""; 

	for(int f = 26; f > 0; f--){
		std::vector<BYTE> restos1;
		std::vector<BYTE> restos2;
		std::vector<BYTE> restos3;
		std::vector<BYTE> restos4;
		std::vector<BYTE> restos5;
		std::vector<float> calibracoes;

		String bytes = "";

		int total = 0;
		
		for(int i = 0; i <= 90; i++){
			if(isAnguloDeConvergencia(i, f)){
				String s = ini->ReadString(IntToStr(Mobile), "A"+IntToStr(i)+"P"+IntToStr(f), "");
				if(s != NULL && s.Length() > 0){
					 TCalibracoes calibracao = decompoeCalibracaoFull(s);
					 restos1.push_back( consertaRestoBinario( calibracao.ry ) );
				}else{
                 	erroLog->Lines->Add("Erro ao ler a calibração do angulo "+IntToStr(i)+" ("+IntToStr(f)+")");
				}
			}
		}

		for(int i = 181; i <= 270; i++){
			if(isAnguloDeConvergencia(i, f)){
				AnsiString s = ini->ReadString(IntToStr(Mobile), "A"+IntToStr(i)+"P"+IntToStr(f), "");
				if(s != NULL && s.Length() > 0){
					 TCalibracoes calibracao = decompoeCalibracaoFull(s);
					 restos3.push_back( consertaRestoBinario( calibracao.ry ) );
				}else{
                 	erroLog->Lines->Add("Erro ao ler a calibração do angulo "+IntToStr(i)+" ("+IntToStr(f)+")");
				}
			}
		}

		// Junta os dois quadrantes reais
		for(int i = 0; i < restos1.size(); i++){
         	restos5.push_back(restos1[i]);
		}

		for(int i = 0; i < restos3.size(); i++){
         	restos5.push_back(restos3[i]);
		}

		// Separa binarios
		int j = 0;
		int conjunto = 0;

		ULONG64 valor = 0;
		String g = "";
		j = 0;
		for(int i = 0; i < restos5.size(); i++){
			valor = ((valor << 1) + restos5[i]);
			j++;
			if(j > 8){
				g = g + CMIntToHex(conjunto,2);
				conjunto = 0;
				j = 1;
			}
			conjunto = ((conjunto << 1) + restos5[i]);
		}

		if(conjunto > 0){
			while(conjunto < 0x80){
				conjunto <<= 1;
			}
		}

		g = g + CMIntToHex(conjunto,2);

		bytes = bytes + "0x"+g; //IntToHex(valor,7);
		
		bytesFinal = bytesFinal + ((bytesFinal.Length() == 0)?"":",") + bytes;
	}

	Memo3->Text = Memo3->Text + s + "{"+bytesFinal+"}";
	if(CheckBox9->Checked)
		data->WriteString("csm", IntToStr(Mobile), "{"+bytesFinal+"}");

	for(int f = 26; f > 0; f--){
		for(int i = 0; i <= 270; i++){
			if( (i >= 0 && i <= 90) || (i > 180 && i <= 270) ){
				if( isAnguloDeConvergencia(i,f) ){
					String s = ini->ReadString(IntToStr(Mobile), "A"+IntToStr(i)+"P"+IntToStr(f), "");
					if(s != NULL && s.Length() > 0){
						 TCalibracoes calibracao = decompoeCalibracaoFull(s);
						 if(calibracao.g != 0){
							if(CheckBox9->Checked)
								data->WriteString("gsm", IntToStr(Mobile), AnsiReplaceStr( formataFloat(calibracao.g), ",", ".") );
							break;
						 }
					}else{
						erroLog->Lines->Add("Erro ao ler a calibração do angulo "+IntToStr(i)+" ("+IntToStr(f)+")");
					}
				}
			}
		}
	}
*/

	arquivoCM->gravar();
}
//---------------------------------------------------------------------------

bool intimer = false;
void __fastcall TForm10::Timer1Timer(TObject *Sender)
{
	 if(!intimer)
		 intimer = true;
	 else
		 return;

	 __try
	 {

		 int id = GetTargetProcessIdFromProcname("gunbound.gme");
		 if(id == 0)
		 {
			gbopen = false;
		 }
		 else
		 {
			if(!gbopen)
			{
				gbopen = true;
				FindGBProcess(id);
			}
		 }

		 if(!gbopen){
            GetValues();
			return;
		 }

		 Mobile = ComboBox1->ItemIndex;
		 GetValues();
		 GetIndex();
		 FaceToRight();
		 GetBotPos();
		 GetBotAngle();

		 if(delay != lastdelay)
		 {
			 TurnFlag = true;
			 if(TurnFlag && delay == selfIndex)
			 {
				TurnUsed = false;
				TurnFlag = false;
			 }
		 }
			 
	 }
	 __finally
	 {
       intimer = false;
	 }
}
//---------------------------------------------------------------------------

bool inTimer2 = false;
int rodadas = 0;
int ultimaForca = 0;
void __fastcall TForm10::Timer2Timer(TObject *Sender)
{
	 if(!inTimer2)
		 inTimer2 = true;
	 else
		 return;

	 if(gbopen)
		Panel1->Color = clLime;
	 else
		Panel1->Color = clRed;

	 //Mostra PID do processo
	 //Label4->Caption = IntToStr((int)gbpid)+" - "+IntToStr((int)Processo);

	 //Seta o status do calibrador
	 if(Ligado)
	 {
		 Label18->Caption = "Ligado";
		 Label18->Color = clLime;
	 }
	 else
	 {
		 Label18->Caption = "Desligado";
		 Label18->Color = clRed;
		 inTimer2 = false;

		 if(CheckBox7->Checked){
		 	WindAngleToSet = SpinEdit5->Value;
			WindPowerToSet = SpinEdit4->Value;
			/*if(CheckBox11->Checked)
				GSM[Mobile] = StrToFloat(Edit1->Text);*/
		 }

		 return;
	 }

	 __try
	 {
		 if(CheckBox7->Checked){
			 return;
		 }

		 TurnUsed = RestartNow;
		 if(!TurnUsed && delay == selfIndex && inGame)
		 {
			 WaitTime(500);
			 RestartNow = false;

			 if(modoDebug->Checked){
				  WindAngleToSet = SpinEdit5->Value;
				  WindPowerToSet = SpinEdit4->Value;
			 }else{
				 if(ultimaForca != WindPowerToSet){
					listaAngulos();
					if(angulos.size() > 0)
					   WindAngleToSet = angulos[0];
					else
					   WindAngleToSet = 0;
					ultimaForca = WindPowerToSet;
				 }

				 // Se o vento não for o devido, da F8
				 if(WindPowerToSet != WindPower || WindAngleToSet != WindAngle)
				 {
					keybd_event(VK_F8,0xc2,0 , 0);
					keybd_event(VK_F8,0xc2, KEYEVENTF_KEYUP,0);
					WaitTime(300);
					return;
				 }

				 //Vento certo, calibra o tiro
				 if(WindPowerToSet != 0 && ultimaCalibracao.g == 0){
					WindPowerToSet = 0;
					WindAngleToSet = 0;
					calibraGravidade = true;
				 }

			 }

			 TIniFile *iniData = new TIniFile(ExtractFilePath(ParamStr(0))+"data.ini");
			 int bot = iniData->ReadInteger("geral","BOT",0);
			 if(bot != Mobile && !modoDebug->Checked){
				ultimaCalibracao.y = 0;
				ultimaCalibracao.g = 0;
				TY = 0;
				TG = 0;
				WindAngleToSet = 0;
				calibraGravidade = true; //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
				if(CheckBox5->Checked){
					iniData->WriteInteger("geral", "BOT", Mobile);
					iniData->WriteFloat("geral", "CY", 0);
					iniData->WriteFloat("geral", "CG", 0);
					iniData->WriteFloat("geral", "LastWA", 0);
					iniData->WriteFloat("geral", "LastWF", 0);
				}
				WindPowerToSet = 0;
				return;
			 }

			 if(modoDebug->Checked){
                //ultimaCalibracao.g = StrToFloat(Edit1->Text);
			 }

			 TPointFloatEx ponto = GetShotLine();

			 if(!Ligado) return;

			 //if(!modoDebug->Checked){
				 if(ponto.g == 0 && WindPowerToSet > 0){
					 calibraGravidade = true;
					 WindPowerToSet = 0;
					 WindAngleToSet = 0;
					 AddLog("Erro ao executar o bot");
					 WaitTime(500);
					 TurnUsed = false;
				 }else{
					 if(modoDebug->Checked){
					 	 // Se estiver no modo debug, calibra e desliga
						 Ligado = !Ligado;
						 return;
					 }

					 AddLog("WF: "+formataFloat(WindPower)+"  WA: "+formataFloat(WindAngle)+" Y: "+formataFloat(ponto.y)+" ("+formataFloat(ponto.y - ultimaCalibracao.y)+")  G: "+formataFloat(ponto.g));
					 TIniFile *ini = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");
					 TIniFile *ini2 = new TIniFile(ExtractFilePath(ParamStr(0))+"data.ini");

					 __try
					 {
						ultimaCalibracao.g = ponto.g;
						ultimaCalibracao.y = ponto.y;

						TPointFloatEx ultima = pegaCalibracaoAnterior(WindAngleToSet);

						float resto = (ponto.y - ultima.y);
						if(WindAngleToSet > 180 && WindAngleToSet < 210){
							int anguloDoMeio = 0;
							for(int i = 0; i < angulos.size(); i++){
								if(angulos[i] > 180){
									anguloDoMeio = angulos[i];
									break;
								}
							}
							if(WindAngleToSet == anguloDoMeio){
                                resto = ponto.y;
							}
						}

						if(!isAnguloDeConvergencia(WindAngleToSet)) ponto.y = ultima.y;

						if(StrToFloat(formataFloat(ponto.y)) == StrToFloat(formataFloat(ultima.y)) && rodadas <= 2 && WindPowerToSet != 0){
							rodadas++;
							return;
						}

						rodadas = 0;


						if(CheckBox5->Checked){
							ini->WriteString(IntToStr(Mobile), "A"+IntToStr((int)WindAngle)+"P"+IntToStr((int)WindPower), formataFloat(ponto.y)+"|"+formataFloat(ponto.g)+"|"+formataFloat((resto > 0.2 || resto < -0.2)?resto:0));
						}

						if(calibraGravidade){
							calibraGravidade = false;
							WindPowerToSet = SpinEdit1->Value;
						}else
						if(!pegaProximoAngulo()){
							if(WindPowerToSet <= 1){
								Ligado = false;
								if(Form10->CheckBox1->Checked)
									tocarBeep();
							}else{
								if(Form10->CheckBox1->Checked)
								tocarBeep2();
								WindPowerToSet--;
								SpinEdit1->Value = WindPowerToSet;
								listaAngulos();
							}
						}

						WindAngleToSet = (WindAngleToSet % 360);

						calibraGravidade = false;
						WaitTime(500);

						if(CheckBox5->Checked){
							ini2->WriteInteger("geral", "LastWA", WindAngleToSet);
							ini2->WriteInteger("geral", "LastWF", WindPowerToSet);

							ini2->WriteString("geral", "CG", formataFloat(ponto.g));
							ini2->WriteString("geral", "CY", formataFloat(ponto.y));
						}
						
						keybd_event(VK_F8,0xc2,0 , 0);
						keybd_event(VK_F8,0xc2, KEYEVENTF_KEYUP,0);
						WaitTime(500);
						keybd_event(VK_F8,0xc2,0 , 0);
						keybd_event(VK_F8,0xc2, KEYEVENTF_KEYUP,0);
						WaitTime(500);

						TurnUsed = false;
					 }
					 __finally
					 {
						ini->Free();
						ini2->Free();
					 }
				 }
			 //}
		 }
	 }
	 __finally
	 {
        inTimer2 = false;
	 }
}
//---------------------------------------------------------------------------

bool inTimer3 = false;
void __fastcall TForm10::timerVentoTimer(TObject *Sender)
{
		if(!inTimer3)
			 inTimer3 = true;
		else
			 return;

		__try
		{
			if((!Ligado || !gbopen) && !(CheckBox7->Checked && modoDebug->Checked))
				return;

			DWORD addressA = 0, addressP = 0;
			__try
			{
				if(!CheckBox7->Checked || (CheckBox7->Checked && modoDebug->Checked)){
					ReadProcessMemory(Processo, (PDWORD)(WindAnglePtr), &addressA, 4, NULL);
					ReadProcessMemory(Processo, (PDWORD)(WindPowerPtr), &addressP, 4, NULL);
					WriteProcessMemory(Processo, (PDWORD)(addressA+WindAngleOff), &WindAngleToSet, 2, NULL);
					WriteProcessMemory(Processo, (PDWORD)(addressP+WindPowerOff), &WindPowerToSet, 1, NULL);
				}
			}__except(1){
				return;
			}
		}
		__finally
		{
			inTimer3 = false;
		}
}
//---------------------------------------------------------------------------

bool inTimer4 = false;
void __fastcall TForm10::writeTimerTimer(TObject *Sender)
{

		if(!inTimer4)
			 inTimer4 = true;
		else
			 return;



		/*HDC dc = GetDC(0);
		try
		{
			char sval2[101];
			sprintf(sval2, "Delay %d             ", SpinEdit2->Value);
			TextOutA(dc, 480, 25, sval2, 15);

			sprintf(sval2, "Sleep %d             ", SpinEdit3->Value);
			TextOutA(dc, 480, 40, sval2, 15);
		}
		__finally
		{
			ReleaseDC(0, dc);
		}  */

		__try
		{
        	if(!anyDC->Checked && !inGame) return;

			if((!Ligado || !gbopen) && !CheckBox6->Checked)
						return;

			HDC dc = GetDC(0);

			__try
			{
				if(TY < 0.0001 && TY > -0.0001) TY = 0;
				if(TG < 0.0001 && TG > -0.0001) TG = 0;

				TextOutA(dc, 480, 10, "Bot Ligado", 10);

				char sval[101];

				sprintf(sval, "WA to set: %G | WF to set: %G                                                 ", (double)(WindAngleToSet), (double)(WindPowerToSet) );
				TextOutA(dc, 480, 25, sval, 60);

				sprintf(sval, "Ult. G: %G | Ult Y: %G                          ", (double)(ultimaCalibracao.g), (double)(ultimaCalibracao.y) );
				TextOutA(dc, 480, 40, sval, 60);

				TextOutA(dc, 10, 70, LastMsg.c_str(), LastMsg.Length());

				sprintf(sval, "Cur. G: %G | Cur. Y: %G                         ", ((double)(TG)), ((double)(TY)) );
				TextOutA(dc, 480, 100, sval, 60);

				sprintf(sval, "Ang. Bot: %d/%d (%d) | Ang. Vento: %d                        ", BotAngle, BotAngleToSet, (180-BotAngleToSet), VentoReal);
				TextOutA(dc, 480, 115, sval, 60);

				/*sprintf(sval, "                                                                                ");
				sprintf(sval, "CurTopo: %d / %d | CurFim: %d / %d |  Distancia: %G  |  ShotTicks: %d                                                  ", CurTopo.x, CurTopo.y, CurFim.x, CurFim.y, distFim, ShotTicks);

				TextOutA(dc, 150, 130, sval, 80);
				sprintf(sval, "Vento Real: %d                                                 ", VentoReal);
				TextOutA(dc, 480, 145, sval, 25);  */
			}
			__finally
			{
				ReleaseDC(0, dc);
			}
		}
		__finally
		{
			 inTimer4 = false;
		}

}
//---------------------------------------------------------------------------

void __fastcall TForm10::FormClose(TObject *Sender, TCloseAction &Action)
{
	HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
	if(proc)
	{
		TerminateProcess(proc, 0);
		CloseHandle(proc);
	}
}
//---------------------------------------------------------------------------
TPoint GetCameraPos()
{
	DWORD address = 0, x = 0, y = 0;
	__try
	{
		ReadProcessMemory(Processo, (PVOID)(CameraPtr), &address, 4, NULL);
		ReadProcessMemory(Processo, (PVOID)(address+CameraOff), &x, 2, NULL);
		WaitTime(1);
		ReadProcessMemory(Processo, (PVOID)(address+CameraOff+4), &y, 2, NULL);
	}
	__except(1)
	{
		return TPoint(0,0);
	}
	return TPoint(x-400,y-240);
}

typedef struct _CM_RGB
{
	char r;
	char g;
	char b;
	char x;
} TCM_RGB, *PCM_RGB;

Gdiplus::Color ToGdiColor(DWORD valor)
{
	TCM_RGB result = *(PCM_RGB)(&valor);
	result.x = 0xFF;
	return Gdiplus::Color(result.x , result.r, result.g, result.b);
}

bool inTimer5 = false;
void __fastcall TForm10::drawTimerTimer(TObject *Sender)
{
	if(!inTimer5)
		 inTimer5 = true;
	else
		 return;


	__try
	{
    	if(!anyDC->Checked && !inGame) return;

		if(!CheckBox3->Checked && !CheckBox7->Checked) return;
		HDC dc = GetDC(0);

		if(!dc)
			return;

		__try
		{
			TPoint cam = GetCameraPos();
			int cameraX = cam.x;
			int cameraY = cam.y;
			if(cameraY > 60000)
				cameraY = (cameraY - 65295 - 240);

			if(debugFlag){
				char sval2[100];
				sprintf(sval2, "Debugging             ");
				TextOutA(dc, 5, 5, sval2, 9);
			}

		
			int espessura = 1;

			//Cria PENS com suas cores
			Gdiplus::Pen whitePen(ToGdiColor(0xFFFFFF), espessura);
			Gdiplus::Pen whitePen2(ToGdiColor(0xFFFFFF));
			Gdiplus::Pen customPen1(ToGdiColor(clBlue), espessura);
			Gdiplus::Pen customPen2(ToGdiColor(clRed), espessura);
			Gdiplus::Pen redPen(ToGdiColor(clRed), 1);
			Gdiplus::Pen bluePen(ToGdiColor(clBlue), 1);
			Gdiplus::Pen greenPen(ToGdiColor(clGreen), 1);

			__try
			{
				const int raio = 36;

				Gdiplus::Graphics grap(dc);


				for(int i = 1; i < Shot.count(); i++){
					if(Shot.count() > i){
						 grap.DrawLine(&whitePen2, (int)Shot[i-1].x - cameraX, (int)Shot[i-1].y - cameraY, (int)Shot[i].x - cameraX, (int)Shot[i].y - cameraY);
						 grap.DrawEllipse(&bluePen, (int)Shot[i-1].x - cameraX - 2, (int)Shot[i-1].y - cameraY - 2, 4, 4);

					}
				}

				if(!CheckBox7->Checked){
					if(indiceTopoAim > Shot.count()){
						grap.DrawLine(&redPen, (int)_topoAim.x - cameraX - 10, (int)_topoAim.y - cameraY, (int)_topoAim.x - cameraX + 10, (int)_topoAim.y - cameraY);
						grap.DrawLine(&redPen, (int)_topoAim.x - cameraX, (int)_topoAim.y - cameraY - 10, (int)_topoAim.x - cameraX, (int)_topoAim.y - cameraY + 10);

						grap.DrawLine(&redPen, (int)_ladoAim.x - cameraX - 10, (int)_ladoAim.y - cameraY, (int)_ladoAim.x - cameraX + 10, (int)_ladoAim.y - cameraY);
						grap.DrawLine(&redPen, (int)_ladoAim.x - cameraX, (int)_ladoAim.y - cameraY - 10, (int)_ladoAim.x - cameraX, (int)_ladoAim.y - cameraY + 10);

					   /*	grap.DrawLine(&whitePen2, (int)_topoTrajetoria.x - cameraX - 10, (int)_topoTrajetoria.y - cameraY, (int)_topoTrajetoria.x - cameraX + 10, (int)_topoTrajetoria.y - cameraY);
						grap.DrawLine(&whitePen2, (int)_topoTrajetoria.x - cameraX, (int)_topoTrajetoria.y - cameraY - 10, (int)_topoTrajetoria.x - cameraX, (int)_topoTrajetoria.y - cameraY + 10);

						grap.DrawLine(&whitePen2, (int)_ladoTrajetoria.x - cameraX - 10, (int)_ladoTrajetoria.y - cameraY, (int)_ladoTrajetoria.x - cameraX + 10, (int)_ladoTrajetoria.y - cameraY);
						grap.DrawLine(&whitePen2, (int)_ladoTrajetoria.x - cameraX, (int)_ladoTrajetoria.y - cameraY - 10, (int)_ladoTrajetoria.x - cameraX, (int)_ladoTrajetoria.y - cameraY + 10);
						*/
					}
				}
				

				int tx = BotPos.x - cameraX;
				int ty = BotPos.y - cameraY;

				int posx = raio*cos(IntToRadian(BotAngle));
				int posy = raio*sin(IntToRadian(BotAngle));

				grap.DrawEllipse(&redPen, tx - raio, ty - raio, 2*raio, 2*raio);
				grap.DrawLine(&whitePen2, tx, ty, tx+posx, ty-posy);

				if(!CheckBox7->Checked){
					grap.DrawLine(&whitePen, tx, ty, tx, pontoMaisAlto-cameraY);
					grap.DrawLine(&whitePen, tx-10, pontoMaisAlto-cameraY, tx+10, pontoMaisAlto-cameraY);
					grap.DrawLine(&redPen, (int)0, pontoMaisAlto-cameraY, tx, pontoMaisAlto-cameraY);

					if(GameInterface == 1)
					{
						grap.DrawLine(&whitePen, 389+400, 565, 389+400, 585);
					}
					else
					{
						grap.DrawLine(&whitePen, 241+400, 565, 241+400, 585);
					}
				}
				//------------------------------------

				double Alfa,Beta,Gama,Delta; //Variaveis de controle de funcções aritméticas - sen, cos tg
				double ShotSpeedX = 0;
				double ShotSpeedY = 0;

				Alfa = cos(IntToRadian(BotAngle));
				Beta = sin(IntToRadian(BotAngle));
				Gama = cos(IntToRadian(WindAngle));
				Delta = sin(IntToRadian(WindAngle));

				double AceleracaoX = ((WindPower > 0)?(arquivoCM->mobile.forcas[WindPower-1].angulos[(WindAngle+90+360)%360]):0) - StrToFloat(Edit1->Text);//+(0.0035*(float)WindPower); //TX;
				double AceleracaoY = ((WindPower > 0)?(arquivoCM->mobile.forcas[WindPower-1].angulos[WindAngle]):0) - arquivoCM->mobile.gravidade; //TY - TG;
				//if(AceleracaoX < 0.00001) AceleracaoX = 0;
				//if(AceleracaoY < 0.00001 ) AceleracaoY = 0;

                if(CheckBox7->Checked){
					char sval2[100];
					sprintf(sval2, "A: %s  F: %s  X: %s   Y: %s   G: %s    I: %d                ", IntToStr((int)WindAngle), IntToStr((int)WindPower), formataFloat(AceleracaoX), formataFloat(AceleracaoY), formataFloat(arquivoCM->mobile.gravidade), ((WindAngle+90+360)%360) );
					TextOutA(dc, 10, 10, sval2, 76);
				}

				AceleracaoY -= GSM[Mobile];

				TProjetil Projetil;

				ShotSpeedX = 400 * Alfa;
				ShotSpeedY = 400 * Beta;

				Projetil.x = BotPos.x;
				Projetil.y = BotPos.y;
				Projetil.SpeedX = ShotSpeedX;  //ShotSpeedX;
				Projetil.SpeedY = ShotSpeedY;  //ShotSpeedY;

				//Sleep(1);
				bool flag = false;
					 
				TPoint ultimo;
				ultimo.x = Projetil.x - cameraX;
				ultimo.y = Projetil.y - cameraY;

				BOOL ss = false;
				double TotalTime = 0;

				BOOL drawed = false;
				BOOL breaked = false;

				char sval[101];

				if(Form10->CheckBox7->Checked){
					if(Projetil.SpeedX != 0 || Projetil.SpeedY != 0)
					 while(Projetil.x < 1800 && Projetil.x > 0 && Projetil.y > -5000 && Projetil.y < 2000)
					 {
						Projetil.x += Projetil.SpeedX*Tempo; // + AceleracaoX*Tempo*Tempo*0.5;
						Projetil.y -= Projetil.SpeedY*Tempo; // + AceleracaoY*Tempo*Tempo*0.5;

						Projetil.SpeedX += AceleracaoX * Tempo;
						Projetil.SpeedY += AceleracaoY * Tempo;

						TotalTime += Tempo;

						BOOL inwind = false;

						if( (Projetil.x > (cameraX-20)) && (Projetil.x < (cameraX+820)) && (Projetil.y > (cameraY-20)) && (Projetil.y < (cameraY+525)) )
						{
							int cx = Projetil.x - cameraX;
							int cy = Projetil.y - cameraY;
							inwind = (cx < 390 || cx > 415 || (cx >= 390 && cx <= 415 && cy > 40));
							if(inwind)
							{
								drawed = true;

								//evita q a trajetoria "quebre" na tela
								if(breaked)
								{
									flag = false;
									breaked = false;
								}

								if(!flag)
								{
									flag = true;
									ultimo.x = Projetil.x - cameraX;
									ultimo.y = Projetil.y - cameraY;
								}
								else  
								{
									TPoint atual;
									atual.x = Projetil.x - cameraX;
									atual.y = Projetil.y - cameraY;

									//if(BotInfos->ChangeColor && pontos[0].time > 0 && TotalTime >= pontos[0].time)
									grap.DrawLine(&customPen2,(int) ultimo.x, (int)ultimo.y, (int)atual.x, (int)atual.y);

									ultimo = atual;
								}
							}
						}
						else
						{
							if(drawed)
								breaked = true;
						}
						//**********************************************
					}
				}

			}
			__finally                           
			{
				ReleaseDC(0, dc);
			}
		}
		__except(1)
		{
				return;
		}
	}
	__finally
	{
		inTimer5 = false;
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm10::Button2Click(TObject *Sender)
{

	 HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
	 if(proc)
	 {
		TerminateProcess(proc, 0);
		CloseHandle(proc);
	 }
}
//---------------------------------------------------------------------------


void __fastcall TForm10::Timer6Timer(TObject *Sender)
{
   inGame = (strcmp(LowerCase(ActiveCaption()).t_str(), "gunbound") == 0);
}
//---------------------------------------------------------------------------


void __fastcall TForm10::FormShow(TObject *Sender)
{
	ComboBox1->ItemIndex = 0;
	AnsiString val = "S0";
	if(ComboBox1->ItemIndex == 1)
		val = "S1";
	else
		if(ComboBox1->ItemIndex == 11)
			val = "S11";
	ShotPtr = HexToInt(allini->ReadString(val, "ShotPtr", 0));
	ShotOff = HexToInt(allini->ReadString(val, "ShotOff", 0));
	CurBot = ComboBox1->ItemIndex;
}
//---------------------------------------------------------------------------
// Pega
/*
	TIniFile *iniCal = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");

	listaAngulos();
	for(int i = 0; i < angulos.size(); i++){
		 AnsiString valor = iniCal->ReadString(IntToStr(Mobile), "A"+IntToStr(angulos[i])+"P"+SpinEdit1->Text, "");

         //O angulo ainda não foi calibrado
		 if(valor.Length() == 0){
             if(i == 0){
				ultimaCalibracao.x = 0;
				ultimaCalibracao.y = 0;
				ultimaCalibracao.g = 0;
				WindAngleToSet = angulos[i];
				WindPowerToSet = SpinEdit1->Value;
				return;
			 }else{
				ultimaCalibracao = decompoeCalibracao(valor);
				WindAngleToSet = angulos[i];
				WindPowerToSet = SpinEdit1->Value;
				return;
			 }
		 }
	}

	iniCal->Free();
	*/
//---------------------------------------------------------------------------


void SetPort(WORD address, WORD Value){
  Byte bValue;
  bValue = (int)(Value & 255);
  asm{
    mov dx, address
    mov al, bValue
    out dx, al
  }
}

WORD GetPort(WORD address){
  byte bValue;
  asm{
	mov dx, address
	in al, dx
	mov bValue, al
  }
  return bValue;
}

void DoSound(Word Freq){
	BYTE B;
	if( Freq > 18 ){
	  Freq = Word((int)(1193181 / (LONG)(Freq)));
	  B    = (Byte)(GetPort(0x61));

	  if ((B & 3) == 0){
		SetPort(0x61, (Word)(B | 3));
		SetPort(0x43, 0xB6);
	  }

	  SetPort(0x42, Freq);
	  SetPort(0x42, Freq >> 8);
	}
}

void Delay(int MSecs){
	LONG FirstTickCount;
	FirstTickCount = GetTickCount();
	while( ((GetTickCount() - FirstTickCount) >= (LONG)(MSecs)) ){
	  Sleep(1);
	  //or use Application.ProcessMessages instead of Sleep
	}
}

void Sound(int aFreq, int aDelay){
  if( Win32Platform == VER_PLATFORM_WIN32_NT){
	Beep(aFreq, aDelay);
  }else{
	DoSound(aFreq);
	Delay(aDelay);
  }
}

void NoSound(){
  Word Value;
  if( !(Win32Platform == VER_PLATFORM_WIN32_NT)){
	Value = GetPort(0x61) & 0xFC;
	SetPort(0x61, Value);
  }
}


void tocarBeep(){
	for(int j = 0; j < 5; j++){
	  Sound(900, 500);
	  Sound(500, 500);
  }
  NoSound();
}

void tocarBeep2(){
    for(int j = 0; j < 5; j++){
	  Sound(900, 300);
	  Sound(700, 300);
  }
  NoSound();
}

void __fastcall TForm10::Button4Click(TObject *Sender)
{
/*
if(angulos.size() == 0) listaAngulos();
	ListBox1->Clear();
	for(int i = 0; i < angulos.size(); i++){
     	ListBox1->Items->Add(IntToStr(angulos[i]));
	}	 */
}
//---------------------------------------------------------------------------

void atualizaCalibracao(int f = -1){
	double diferenca = 0.3;

	ultimoMobileFinal = Mobile;
	ultimaForcaFinal = WindPower;

	if(!Form10->CheckBox7->Checked){
		Form10->Memo2->Clear();
		Form7->Memo1->Clear();
	}

	/*int digitos = SpinEdit8->Text.Length();
	int qnt = 1;
	for(int i = 0; i < digitos; i++){
		qnt *= 10;
	}
	double menor = (((float)SpinEdit8->Value)/qnt);  */
	double menor = DSM[Mobile][0];
	double maior = DSM[Mobile][1];

	//for(int f = 26; f > 0; f--){
		std::vector<BYTE> restos1;
		std::vector<BYTE> restos2;
		std::vector<BYTE> restos3;
		std::vector<BYTE> restos4;
		std::vector<BYTE> restos5;
		std::vector<BYTE> restos6;
		std::vector<WORD> angulosGerais;
		std::vector<double> calibracoes;

		//for(int f = 0; f < 26; f++){
		if(f == -1){
			f = WindPower;
		}
		
		std::vector<BYTE> angulosValidos;

		int quadrante1 = 0;

		// Pega angulos de convergencia
		for(int i = 0; i <= 270; i++){
			if( (i >= 0 && i <= 90) || (i > 180 && i <= 270 ) ){
				if(isAnguloDeConvergencia(i,f)){
					if(i >= 0 && i <= 90) quadrante1++;
					angulosValidos.push_back(i);
				}
			}
		}

		//int quantidade = ((angulosValidos.size() % 8) == 0)?(int)(angulosValidos.size() / 8):((int)(angulosValidos.size() / 8) + 1);
		int sobra = (angulosValidos.size() % 8);
		int quantidade = angulosValidos.size() + ((sobra == 0)?0:(8-sobra));

		/*for(int i = 0; i < quantidade; i++){
			BYTE k = CSM[Mobile][26-f][i];
			for(int n = 0; n < 8; n++){
				BYTE bit = (k & 1);
				k >>= 1;
				restos5.push_back(bit);
			}
		} */

		//String v = "";
		ULONG64 k = CSM[Mobile][26-f];
		for(int i = 0; i < quantidade; i++){
            BYTE bit = (k & 1);
			k >>= 1;
			restos6.insert(restos6.begin() ,bit);
			//v = IntToStr(bit) + v;
		}
		//Form10->Memo1->Lines->Add(v);

		/*
		int n = 0;
		for(int i = 0; i < restos5.size(); i=i+8){
			if(i == (restos5.size()-8)){
				n = (8-(restos5.size()-angulosValidos.size()));
			}else{
             	n = 8;
			}
			for(int j = (n-1); j >= 0; j--){
				restos6.push_back( restos5[i+j] );
			}
		}   */

		// Quadrante 1 (independente)
		int angulosQ1 = 0;
		for(int i = 0; i <= 90; i++){
			if(isAnguloDeConvergencia(i,f)){
                angulosQ1++;
			}
		}

		restos1.clear();
		for(int i = 0; i < angulosQ1; i++){
			restos1.push_back(restos6[i]);
		}

		// Quadrante 3 (independente)
		int angulosQ3 = 0;
		for(int i = 181; i <= 270; i++){
			if(isAnguloDeConvergencia(i,f)){
                angulosQ3++;
			}
		}

		restos3.clear();
		for(int i = 0; i < angulosQ3; i++){
			restos3.push_back(restos6[angulosQ1+i]);
		}

		restos6.clear();

		// Quadrante 2 (dependente do 1)
		int angulosQ2 = 0;
		for(int i = 91; i <= 180; i++){
			if(isAnguloDeConvergencia(i,f)){
                angulosQ2++;
			}
		} 

		for(int i = (int)(restos1.size()-1); i >= (int)(restos1.size()-angulosQ2); i--){
			restos2.push_back(restos1[i]);
		}

		// Quadrante 4 (dependente do 3)
		int angulosQ4 = 0;
		for(int i = 271; i <= 360; i++){
			if(isAnguloDeConvergencia(i,f)){
                angulosQ4++;
			}
		}

		for(int i = (int)(restos3.size()-1); i >= (int)(restos3.size()-angulosQ4); --i){
			if(i >= 0)
				restos4.push_back(restos3[i]);
		}

		int count = 0;

		if(!Form10->CheckBox7->Checked){
			for(int i = 0; i < 360; i++){
				if(isAnguloDeConvergencia(i,f)){
					angulosGerais.push_back(i);
				}
			}
		}


		calibracoes.clear();
		double calibracao = 0;
		for(int i = 0; i < restos1.size(); i++){
			calibracao += ( ((i > 0)?1:0) * ((restos1[i] == 0)?menor:maior) );
			calibracoes.push_back(calibracao);
			restos5.push_back(restos1[i]);

			if(!Form10->CheckBox7->Checked)
				Form7->Memo2->Lines->Add(IntToStr(angulosGerais[count+i])+" - "+IntToStr(restos1[i]));
		}

		count += restos1.size();

		if(!Form10->CheckBox7->Checked)
			restos1.clear();
		for(int i = 0; i < restos2.size(); i++){
			calibracao -= ( ((restos2[i] == 0)?menor:maior) );
			calibracoes.push_back(calibracao);
			restos5.push_back(restos2[i]);

			if(!Form10->CheckBox7->Checked)
				Form7->Memo3->Lines->Add(IntToStr(angulosGerais[count+i])+" - "+IntToStr(restos2[i]));
		}

		count += restos2.size();

		if(!Form10->CheckBox7->Checked)
			restos2.clear();
		for(int i = 0; i < restos3.size(); i++){
			calibracao -= ( ((restos3[i] == 0)?menor:maior ));
			calibracoes.push_back(calibracao);
			restos5.push_back(restos3[i]);

			if(!Form10->CheckBox7->Checked)
				Form7->Memo4->Lines->Add(IntToStr(angulosGerais[count+i])+" - "+IntToStr(restos3[i]));
		}

		count += restos3.size();

		if(!Form10->CheckBox7->Checked)
			restos3.clear();
		for(int i = 0; i < restos4.size(); i++){
			calibracao += ( ((restos4[i] == 0)?menor:maior ));
			calibracoes.push_back(calibracao);
			restos5.push_back(restos4[i]);

			if(!Form10->CheckBox7->Checked)
				Form7->Memo5->Lines->Add(IntToStr(angulosGerais[count+i])+" - "+IntToStr(restos4[i]));
		}

		if(!Form10->CheckBox7->Checked)
			restos4.clear();
		angulosValidos.clear();

		int j = -1;
		TIniFile *ini = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");
		for(int i = 0; i < 360; i++){
			bool anguloConvergencia = isAnguloDeConvergencia(i,f);
			if(anguloConvergencia){
				j++;
				angulosValidos.push_back(i);
			}
			if(j < 0) j = 0;
			//arquivoCM->mobile.forcas[WindPower].angulos[i] = arquivoCM->mobile.forcas[WindPower].angulos[j];
			if(anguloConvergencia && !Form10->CheckBox7->Checked){

                int angulo = i;
				String adicional = "";
				if((angulo >= 0 && angulo <= 90) || (angulo > 180 && angulo <= 270)){
					String s = ini->ReadString(IntToStr(Mobile), "A"+IntToStr(angulo)+"P"+IntToStr(f), "");
                    if(s != NULL && s.Length() > 0){
						TCalibracoes calibracao = decompoeCalibracaoFull(s);
						double dif = Modulo(Modulo(calibracao.y) - Modulo(calibracoes[j]));
						if( dif > 0.2 ){
							adicional = " - Diferença acima do limite (~"+formataFloat(dif)+")";
							Form10->erroLog->Lines->Add("A: "+IntToStr(i)+" F: "+IntToStr(f)+" ::   "+formataFloat(calibracoes[j])+" ("+IntToStr(restos5[j])+")"+adicional);
						}else{
                            adicional = " - OK (~"+formataFloat(dif)+")";
						}
					}else{
                     	adicional = " - não encontrado";
					}
				}

				Form7->Memo1->Lines->Add( "A: "+IntToStr(i)+" F: "+IntToStr(f)+" ::   "+formataFloat(calibracoes[j])+" ("+IntToStr(restos5[j])+")"+adicional );
			}
		}

		if(!Form10->CheckBox7->Checked){
			for(int i = 0; i < calibracoes.size(); i++){
				Form10->Memo2->Lines->Add( "A: "+IntToStr(angulosValidos[i])+" F: "+IntToStr(f)+" ::   "+formataFloat(calibracoes[i]) );
				//Form7->Memo1->Lines->Add( "A: "+IntToStr(angulosValidos[i])+" F: "+IntToStr(f)+" ::   "+formataFloat(calibracoes[i]) );
			}
			if(!Form10->CheckBox10->Checked)
				Form7->Show();
		}

}

void atualizar(){
	if(Mobile != ultimoMobileFinal || WindPower != ultimaForcaFinal){
        atualizaCalibracao();
	}
}

void __fastcall TForm10::Button5Click(TObject *Sender)
{
	erroLog->Clear();
	if(CheckBox10->Checked){
		for(int i = 26; i > 0; i--)
			atualizaCalibracao(i);
	}else{
        atualizaCalibracao();
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm10::Button6Click(TObject *Sender)
{
	TIniFile *ini = new TIniFile(ExtractFilePath(ParamStr(0))+"calibration.ini");
	TIniFile *data = new TIniFile(ExtractFilePath(ParamStr(0))+"data.ini");

	int f = WindPower;
	int rg = 0, rp = 0;
	double restoMenor = 0;
	double restoMaior = 0;
	for(int i = 0; i <= 270; i++){
		if( (i >= 0 && i <= 90) || (i > 180 && i <= 270) ){
			if( isAnguloDeConvergencia(i,f) ){
				String s = ini->ReadString(IntToStr(Mobile), "A"+IntToStr(i)+"P"+IntToStr(f), "");
				if(s != NULL && s.Length() > 0){
					 TCalibracoes calibracao = decompoeCalibracaoFull(s);
					 double resto = Modulo(calibracao.ry);
				 
					 if(resto > 0.1){
						if(resto > 0.7){
							restoMaior += resto;
							rg++;
						}else{
							restoMenor += resto;
							rp++;
						}
					 }
				}else{
					erroLog->Lines->Add("Erro ao ler a calibração do angulo "+IntToStr(i)+" ("+IntToStr(f)+")");
				}
			}
		}
	}

	double mediaG = 0, mediaP = 0;

	if(rp != 0){
		mediaP = (restoMenor/rp);
	}

	if(rg != 0){
		mediaG = (restoMaior/rg);
	}

	Memo1->Lines->Add("Maior: "+formataFloat(mediaG)+"  Menor: "+formataFloat(mediaP) );
	if(CheckBox8->Checked){
		data->WriteString("dsm", IntToStr(Mobile), "{"+AnsiReplaceStr(formataFloat(mediaP),",",".")+","+AnsiReplaceStr(formataFloat(mediaG),",",".")+"}");
	}

}
//---------------------------------------------------------------------------

void __fastcall TForm10::Timer3Timer(TObject *Sender)
{
	if(CheckBox7->Checked)
    	atualizar();
}
//---------------------------------------------------------------------------

void __fastcall TForm10::Button7Click(TObject *Sender)
{
/*	TIniFile *ini = new TIniFile(ExtractFilePath(ParamStr(0))+"data.ini");
	String valor = ini->ReadString("csm",IntToStr(Mobile),"");

	String s = "";
	String log = "";
	Memo1->Lines->Add("Configurações carregadas");
	int j = 0;
	for(int i = 1; i <= valor.Length(); i++){
    	if(valor[i] == '{') continue;

		if(i == valor.Length() || valor[i] == ',' || valor[i] == '}'){
			 CSM[Mobile][j] = CMHexToInt(s);
			 s = "";
			 //log = log + ((log.Length() == 0)?"":",") + CMIntToHex(CSM[Mobile][j],14)
			 Memo1->Lines->Add( CMIntToHex(CSM[Mobile][j],14) );
			 j++;
			 continue;
		}

        s = s + valor[i];
	}

	s = "";
	j = 0;
	valor = ini->ReadString("dsm",IntToStr(Mobile),"");
	for(int i = 1; i <= valor.Length(); i++){
    	if(valor[i] == '{') continue;

		if(i == valor.Length() || valor[i] == ',' || valor[i] == '}'){
			 DSM[Mobile][j] = StrToFloat(AnsiReplaceStr(s,".",","));
			 s = "";
			 Memo1->Lines->Add("Diferença "+IntToStr(j+1)+": "+formataFloat(DSM[Mobile][j]) );
			 j++;
			 continue;
		}

		s = s + valor[i];
	}

	for(int i = 0; i < 20; i++){
		valor = ini->ReadString("gsm",IntToStr(Mobile),"0");
        GSM[i] = StrToFloat(AnsiReplaceStr(valor,".",","));
	}    */

	Form7->Memo1->Clear();
	arquivoCM->carregar(ComboBox1->ItemIndex);
	for(int i = 0; i < arquivoCM->mobile.forcas.size(); i++){
		Application->ProcessMessages();
		Form7->Memo1->Lines->Add("-------- Força: "+IntToStr(i+1));
		for(int j = 0; j < arquivoCM->mobile.forcas[i].angulos.size(); j++){
			Form7->Memo1->Lines->Add("Angulo "+IntToStr(j)+" - "+FloatToStr(arquivoCM->mobile.forcas[i].angulos[j]));
		}
	}
	Form7->Show();
}
//---------------------------------------------------------------------------




