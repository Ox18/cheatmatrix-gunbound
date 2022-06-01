//---------------------------------------------------------------------------

#ifndef sharingH
#define sharingH

//#include "BotUnit.h"
#include <windows.h>
#include "tornado.h"
#include "inifiles.hpp"
#include "..\client.h"
#include "..\VMProtectSDK.h"

//#define DEBUGAR


#define USEINJECT

typedef struct _Atalho {
	float key;
	float modificador;
	_Atalho(float _modificador, float _key) {
		key = _key;
        modificador = _modificador;
	}
	_Atalho(){}
} TAtalho;

//typedef TBot *PBot;
//extern PBot ShotMatrix;

extern bool DebugFlag;
extern bool atirar;
extern int codigoErroPlugin;
extern Cliente *cliente;
extern TIniFile *cmxConfig;
extern char* nomeSubplugin;
extern DWORD instancia;
extern String InjectorName;

//---------------------------------------------------------------------------
#endif
