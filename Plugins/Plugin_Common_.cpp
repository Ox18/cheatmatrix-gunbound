//---------------------------------------------------------------------------


#pragma hdrstop

#include "PluginCommon.h"
#include <IniFiles.hpp>
#include "CMStrUtils.h"
#include <windows.h>
//#pragma link "CMIniFiles.lib"

f_CMReadProcessMemory CMReadProcessMemory;
f_CMWriteProcessMemory CMWriteProcessMemory;
f_CMOpenProcess CMOpenProcess;
f_CMGetDC CMGetDC;
f_CMOpenFileMappingA CMOpenFileMappingA;
f_CMMapViewOfFile CMMapViewOfFile;
f_CMVirtualProtect CMVirtualProtect;
f_CMGetDC CMGetWindowDC;
TMatrizData *MatrizInfo;

HANDLE MapHandle;
HMODULE cmlib = 0;

const PCHAR MapNameX = "AD02992CEB206FF90";

AnsiString CompleteExportName(AnsiString index)
{
		 /*TIniFile *ini = new TCMIniFile( AnsiString(ExtractFilePath(ParamStr(0))+"Common\\Configs.ini").c_str() );
		 char *prefix = ini->ReadString("configs","Prefix",0);
		 int len = CM_StrLen(prefix);
		 AnsiString result;
		 result.LoadStr(prefix, len);
		 return AnsiString(result+index);  */
		 TIniFile* ini = new TIniFile(AnsiString(ExtractFilePath(ParamStr(0))+"Common\\Configs.ini"));
		 AnsiString prefixo = ini->ReadString("configs","Prefix","F");
		 ini->Free();
		 
		 return (prefixo+index);
}

PDWORD GetMutantAddress(HMODULE module, int valor)
{
  typedef DWORD (__stdcall *TF1)(int index);
	TF1 F1 = (TF1)(GetProcAddress(module, CompleteExportName("1").c_str()));
	PDWORD result = (PDWORD)( F1(valor) -456789);
	return result;
}

/*
TBotInfos * __fastcall StartMapping2(){
	__try{
		TBotTeste *objeto = new TBotTeste();
		AnsiString nome = AnsiString(ExtractFilePath(ParamStr(0))+"LangLib.dll");
		cmlib = LoadLibraryA(nome.c_str());

		CMOpenFileMappingA = f_CMOpenFileMappingA( GetMutantAddress(cmlib, 8) );
		CMMapViewOfFile = f_CMMapViewOfFile( GetMutantAddress(cmlib, 9) );
		CMGetWindowDC = f_CMGetDC( GetMutantAddress(cmlib, 10) );

		if(!CMOpenFileMappingA || !CMMapViewOfFile || !CMGetWindowDC)
		{
			 MessageBox(0,"Falha ao carregar os recursos de comunicação!", "Erro",0);
			 return NULL;
		}

		//BotInfos = 0;
		MapHandle = CMOpenFileMappingA(FILE_MAP_WRITE, False, MapNameX);
		objeto = (TBotInfos *)CMMapViewOfFile(MapHandle, FILE_MAP_WRITE, 0, 0, 0);
		if( !objeto )
		{
			MapHandle = (DWORD *)CreateFileMappingA((PVOID)MAXDWORD, NULL, PAGE_READWRITE, 0, sizeof (*objeto), (PCHAR)MapNameX);
			objeto = (TBotInfos *)CMMapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
			if(!objeto)
				 objeto = (TBotInfos *)CMMapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			if(!objeto)
			   objeto = (TBotInfos *)MapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			   ZeroMemory(objeto, sizeof (*objeto));

			if(!objeto)
				return new TBotInfos();
			else
				return objeto;
	  }else{
		  return objeto;
	  }
	}__except(1){
		return new TBotInfos();
	}
	return new TBotInfos();
}
*/

TBotInfos * __fastcall StartMapping2(){
	__try{
		TBotInfos *objeto = new TBotInfos();

		//BotInfos = 0;
		MapHandle = OpenFileMappingA(FILE_MAP_WRITE, False, MapNameX);
		objeto = (TBotInfos *)MapViewOfFile(MapHandle, FILE_MAP_WRITE, 0, 0, 0);
		if( !objeto )
		{
			MapHandle = (DWORD *)CreateFileMappingA((PVOID)MAXDWORD, NULL, PAGE_READWRITE, 0, sizeof (*objeto), (PCHAR)MapNameX);
			objeto = (TBotInfos *)MapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
			if(!objeto)
				 objeto = (TBotInfos *)MapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			if(!objeto)
			   objeto = (TBotInfos *)MapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			   ZeroMemory(objeto, sizeof (*objeto));

			if(!objeto)
				return new TBotInfos();
			else
				return objeto;
	  }else{
		  return objeto;
	  }
	}__except(1){
		return new TBotInfos();
	}
	return new TBotInfos();
}

TBotInfos * __fastcall StartMapping(){
	__try{
		TBotInfos *objeto = new TBotInfos();
		AnsiString nome = AnsiString(ExtractFilePath(ParamStr(0))+"LangLib.dll");

		if(!FileExists(nome)){
            MessageBox(0,"Falha ao carregar (4)!", "Erro",0);
		}

		cmlib = LoadLibrary(nome.c_str());
		//cmlib = (HMODULE)teste;

		if(cmlib == 0){
			MessageBox(0,"Falha ao carregar (2)!", "Erro",0);
			return new TBotInfos();
		}

		CMOpenFileMappingA = (f_CMOpenFileMappingA)( GetMutantAddress(cmlib, 8) );
		CMMapViewOfFile = (f_CMMapViewOfFile)( GetMutantAddress(cmlib, 9) );
		CMGetWindowDC = (f_CMGetDC)( GetMutantAddress(cmlib, 10) );

		if(!CMOpenFileMappingA || !CMMapViewOfFile || !CMGetWindowDC)
		{
			 MessageBox(0,"Falha ao carregar os recursos de comunicação!", "Erro",0);
			 return NULL;
		}

		//BotInfos = 0;
		MapHandle = CMOpenFileMappingA(FILE_MAP_WRITE, False, MapNameX);
		objeto = (TBotInfos *)CMMapViewOfFile(MapHandle, FILE_MAP_WRITE, 0, 0, 0);
		if( !objeto )
		{
			MapHandle = (DWORD *)CreateFileMappingA((PVOID)MAXDWORD, NULL, PAGE_READWRITE, 0, sizeof (*objeto), (PCHAR)MapNameX);
			objeto = (TBotInfos *)CMMapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
			if(!objeto)
				 objeto = (TBotInfos *)CMMapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			if(!objeto)
			   objeto = (TBotInfos *)MapViewOfFile(MapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);

			   ZeroMemory(objeto, sizeof (*objeto));

			if(!objeto)
				return new TBotInfos();
			else
				return objeto;
	  }else{
		  return objeto;
	  }
	}__except(1){
		MessageBox(0,"Falha ao carregar!", "Erro",0);
		return new TBotInfos();
	}
	return new TBotInfos();
}


//---------------------------------------------------------------------------

#pragma package(smart_init)
