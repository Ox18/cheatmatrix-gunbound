//---------------------------------------------------------------------------

#ifndef controleH
#define controleH

#include "mobiles.h"
#include "calculador.h"
#include "..\pluginInfo.h"
#include "..\..\tiposBase.h"
//---------------------------------------------------------------------------
extern TShMatrix *aimbot;
extern TCalculador *calculador;
extern int currentPackets;
extern HANDLE *threads;
extern BITMAP bmp;
extern HBITMAP hBmp;
extern char* imagemMenu;
extern CRITICAL_SECTION flagDraw;

void DesenharStatus(HDC hdc);
void __stdcall PASCAL Iniciar();
void __stdcall PASCAL Desenhar(HDC dc, char tipoDesenho);
void __stdcall PASCAL AtualizarValores();
DWORD WINAPI ThreadProcessamento( LPVOID lpParam );
void __stdcall PASCAL ProcessarComando(COMANDO_SUBPLUGIN *comando);
void __stdcall PASCAL AtualizarEnderecos();
void __stdcall PASCAL ProcessarAtalho(int id);
void pararTiro(int delay);
void mostrarMenu();

#endif
