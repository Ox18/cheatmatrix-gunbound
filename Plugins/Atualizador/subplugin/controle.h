//---------------------------------------------------------------------------

#ifndef controleH
#define controleH

#include "subplugin.h"
#include "..\pluginInfo.h"
#include "..\..\tiposBase.h"
//---------------------------------------------------------------------------
extern ESTRUTURA_PLG *estruturaBase;
extern int currentPackets;
extern HANDLE *threads;
extern BITMAP bmp;
extern HBITMAP hBmp;
extern char* imagemMenu;
extern CRITICAL_SECTION flagDraw;

DWORD EscanearMemoria(DWORD inicioModulo, DWORD fimModulo, BYTE ignorar, PBYTE arrayBytes, DWORD sequenciaLength, int lpOffset, PDWORD lpBuffer, DWORD quantidadeCopiar, BYTE lpFlag, BYTE indice);
void __stdcall HardCodeMemoria();
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
void __fastcall setarConfiguracao(int tipo, char* buffer, int len);

#endif
