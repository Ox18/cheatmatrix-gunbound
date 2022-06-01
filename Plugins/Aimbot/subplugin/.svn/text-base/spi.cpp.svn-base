//---------------------------------------------------------------------------
#include "baseSP.h"
#include "..\pluginInfo.h"
#include "..\..\debugUtils.h"
#include "..\..\secret.h"
#include <windows.h>

#define DLL_EXPORT __declspec(dllexport)

//
//	Retorna o endereço do array com os endereços das funcoes
//

extern "C" DLL_EXPORT DWORD __stdcall _s0(char *diretorioDllMae, SUBPLUGIN *_base) {
   	if(diretorioDllMae != NULL && _base != NULL){
		base = _base;
		if(base != NULL){
			base->pluginID = GetPluginID();
			base->metodos = (METODOS*)&metodos;
			base->diretorioDllMae = diretorioDllMae;
		}
		return 1;
    }else{
        return 0;
    }
}


#pragma argsused
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fwdreason, LPVOID lpvReserved){
	 switch(fwdreason) {
		case DLL_PROCESS_ATTACH: {

			DisableThreadLibraryCalls(hinstDLL);
			/*fOutputDebugStringA = (hOutputDebugStringA)GetProcAddress(GetModuleHandleA("kernel32.dll"), "OutputDebugStringA");
			if(!fOutputDebugStringA)
				MessageBox(0, "Erro no output!", "Bad Boy", 0);   */
			__try{
				debugar("loading...");
				instancia = hinstDLL;
				inicializarMetodos();
			}__except(1){
				debugar("Erro %d", 8446);
				getError("8446");
			}

		}
		break;
		case DLL_PROCESS_DETACH: {
        	debugar("unloaded");
		}
		break;

		default:  break;
	 }
	return true;

}
//---------------------------------------------------------------------------
