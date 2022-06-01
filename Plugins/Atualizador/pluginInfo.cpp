//---------------------------------------------------------------------------


#pragma hdrstop

#include "pluginInfo.h"

int GetMinimumPackets() {
   return MinimumPackets;
}

int GetPluginID(){
	return Hack_Code;
}

bool AllFixed;
PLUGIN_MEMORY_DATA *pacotes = NULL;
int CurrentPackets;
int id1;
int *porta = NULL;
LINGUAGEM *idioma = NULL;

//---------------------------------------------------------------------------

#pragma package(smart_init)
