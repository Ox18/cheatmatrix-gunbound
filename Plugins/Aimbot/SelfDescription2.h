//#include "stdafx.h"
//#include "FormX.h"
#include <windows.h>
#include <classes.hpp>
#include <pluginclasses.h>
#include <vector>

#define Hack_Name "Mobile Hack"
#define Hack_Nick "Mobile Hack"
#define Hack_Game "GunBound"
#define Hack_Type HT_Hack
#define Hack_ToList 1
#define Hack_Code "8431518399"

BOOL AllFixed;
std::vector<TPluginMemoryDataRecord> Pacotes;
std::vector<TScreenMsg> ScreenMessages;
int CurrentPackets;
int id1;

/* Numero de Pacotes Requeridos  */
#define MinimumPackets 2;
