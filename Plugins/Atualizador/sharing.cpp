// ---------------------------------------------------------------------------
// #define DEBUGAR

#pragma hdrstop

#include "sharing.h"

bool atirar = false;
int ultimoMobile = -1;
Cliente *cliente = NULL;
TIniFile *cmxConfig = NULL;
char* nomeSubplugin = 0;
DWORD instancia = 0;
String InjectorName = "";

// -----------------------------------------------------------
// Codigos de erro do plugin
// -----------------------------------------------------------
int codigoErroPlugin = 0;
/*
1 = Erro ao procurar janela
2 = Erro no objeto de compartilhamento com a matriz (MatrizInfo)
 */
// -----------------------------------------------------------
// Atalhos
// -----------------------------------------------------------

#pragma package(smart_init)
