#include <stdio.h>
#include <windows.h>
#include <System.hpp>
#include <classes.hpp>

//---------------------------------------------------------------------------

#ifndef PluginClassesH

//using namespace std;

#define fSep "|ØH®¼Ã|";
#define vSep "|©À½¥£|";

/*
typedef struct TSwLine {
		AnsiString Field;
        AnsiString Value;
} *PSwLine;


class TSwField {
	//private:
			//TSwField();
	public:
			TSwLine fValue;
			AnsiString AsString();
			PAnsiChar AsPointer();
			LONG AsInteger();
			BOOL AsBoolean();
	};

//
//                    Menssagem
//
class TSwMessage
	{

	private:
		TList *fValue;
		//std::vector<TSwLine> fValue;
	public:
			TSwMessage();
			~TSwMessage();
			void Clear();
			void Add(AnsiString Field, AnsiString Value);
			AnsiString ToText();
			void LoadFrom(AnsiString Value);
			TSwField FieldByName(AnsiString Name);
	};

class TPlugin {
			//TPlugin();
public:

			bool PutInList;
			HMODULE PluginHandle;
			HANDLE WindowHandle;
			BOOL Registered;
			int Tipo;
			int Status;
			PVOID Offset;
			PAnsiChar Codigo;
			PAnsiChar Nome;
			PAnsiChar Nick;
			PAnsiChar Game;
			PAnsiChar FileName;
			PAnsiChar Key;
			// Functions
			PAnsiChar SendMsg(TSwMessage Valor);
			void Show();
			void Hide();
			bool Visible();
			void Close();

}	*PPlugin;

typedef struct TPgRec {
	BOOL PutInList;
	HMODULE PluginHandle;
	HANDLE WindowHandle;
	BOOL Registered;
	int Tipo;
	int Status;
	PVOID Offset;
	DWORD Codigo;
	PAnsiChar Nome;
	PAnsiChar Nick;
	PAnsiChar Game;
	PAnsiChar FileName;
	PAnsiChar Key;
	PAnsiChar Nulo;
	
} *PPgRec;

typedef struct TMatrizData {
	 DWORD Calibracoes;
	 DWORD MatrizHandle;
	 DWORD DriverHandle;
	 //char MatrizDir[500];
	 int Opens;
	 HANDLE TargetHandle;
	 DWORD TargetId;
	 PVOID TargetBase;
	 bool Ligado;
}	*PTMatrizData;


typedef struct TScreenMsg {
	 AnsiString msg;
	 DWORD delay;
	 DWORD delaycount;
}	*PScreenMsg;
*/

#define PluginClassesH
//---------------------------------------------------------------------------
#endif




