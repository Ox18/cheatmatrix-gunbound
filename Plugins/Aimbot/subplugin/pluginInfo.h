//---------------------------------------------------------------------------

#ifndef pluginInfoH
#define pluginInfoH

#include <windows.h>
#include "..\tiposBase.h"

//using namespace std;
//#define debug1

/*****************************************************************************

   As constantes e variaveis marcadas com dois asteriscos são específicas para
   cada hack, devendo ser editadas e acordo com o mesmo

******************************************************************************/

extern const char*	 		Hack_Name 	= " "; //ShotMatrix  //** Nome do Hack
extern const char*	 		Hack_Game 	= " "; //GunBound  //** Pacote do Hack
extern const char	 		Hack_Type 	= HT_Hack;      // Tipo - Hack ou Controle
extern const short 			Hack_ToList = 1;            // Mostra na lista da matriz ou não
extern const unsigned long	Hack_Code 	= 1598745666;   //** Codigo do Hack
extern const short			MinimumPackets = 20;        //** Quantidade de Pacotes de ponteiros
extern const char*	 		Hack_Nick 	= Hack_Name;    // Nome do Hack
extern const char*	 		Hack_Label 	= " "; //ShotMatrix 1.6

// Indica se todas as variáveis foram carregadas corretamente, para não dar crash no sistema
extern bool AllFixed;
// Pacotes de Ponteiros, ordenadas pelo PackID dele do banco de dados do servidor
extern PLUGIN_MEMORY_DATA *pacotes;
// Quantidade de pacotes de ponteiros recebida pela matriz
extern int CurrentPackets;
extern int *porta;
//** Codigos de atalhos dos hacks; deve-se mudar o length do array de atalhos
//ex: keys[x], onde x é a quantidade de atalhos do hack, e colocar entre as chaves
//os atalhos, separados por virgula, é óbvio
extern DWORD keys[1] = { 0 }; //**Atalhos
#ifdef debug1
extern DWORD hotkeys[31]; //**Atalhos
#else
extern DWORD hotkeys[32]; //**Atalhos
#endif

// Pega numero Pacotes Requeridos
int GetMinimumPackets();
int GetPluginID();
//---------------------------------------------------------------------------
#endif
