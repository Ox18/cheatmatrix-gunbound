//---------------------------------------------------------------------------

#define TOTALHOOKS 5
#define TIROS_BIG_FOOT 6
#define DLL_EXPORT __declspec(dllexport)

#include "mobiles.h"
#include "variaveis.h"
#include "drawUtils.h"
#include <tlhelp32.h>
#include "debugUtils.h"
#include "server.h"

#pragma comment(lib,"ws2_32.lib")
#pragma comment(lib,"imagehlp.lib")

static HINSTANCE hApiHookDLL = 0;

void RemoverHK(int index);
TAimBot aimbot;
HMODULE cmlib = 0;
DWORD debugFlag;
bool renderizar = false;
bool finishedLib = false;
bool libEncontrada = false;
HMODULE npsch = 0;
HMODULE npggnt = 0;
HMODULE handleBot = 0;



//
//	Chama o método de draw de cada subplugin
//
void chamarDraw(HDC dc){
    for(int i = 0; i < )

}

//-------------------------------------------------------------------------
//  Gera uma mensagem de erro
//-------------------------------------------------------------------------
void PerformError(char *msg, char *titulo = NULL) {
	if(!titulo)
        MessageBoxA(NULL, msg, "Erro", NULL);
    else
        MessageBoxA(NULL, msg, titulo, NULL);
    HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
    TerminateProcess(proc, 0);
    CloseHandle(proc);
}

//-------------------------------------------------------------------------
//   Pega posição da câmera
//-------------------------------------------------------------------------
TPoint GetCameraPos() {
	__try{
		TPoint camera(0,0);
		if(BotInfos){
			__try {
				camera.x = *((PWORD)(BotInfos->enderecos[1]));
				camera.y = *((PWORD)(BotInfos->enderecos[2]));

				camera.x -= 400;
				camera.y -= 240;
				if(camera.x > 60000)
					camera.x = (camera.x - 65535);

			}__except(1) {
				return camera;
			}
		}
		return camera;
	}__except(1){
	}
}

void __fastcall FillPoints(HDC dc){
	__try{
		/*char teste[100];
		int n = sprintf(teste, "Handles: %x - %x - x", npsch, npggnt, handleBot);
		TextOut(dc, 10, 50, teste, n);        */

		if(!BotInfos->modoDebug && renderizar){
			aimbot.camera = GetCameraPos();
			if(dc == 0)
				aimbot.fillPoints();
			else
				aimbot.fillPoints(dc);
		}
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//    Transforma uma cor para gdiplus:color
//-------------------------------------------------------------------------
Gdiplus::Color ToGdiColor(DWORD valor) {
	__try{
		TCM_RGB result = *(PCM_RGB)(&valor);
		result.x = 0xFF;
		return Gdiplus::Color(result.x , result.r, result.g, result.b);
    }__except(1){
	}
}

//-------------------------------------------------------------------------
//    Length da string unicode
//-------------------------------------------------------------------------
int CM_WStrLen(wchar_t *valor) {
	__try{
		wchar_t j = 0;
		for(int i = 0; i < MAX_PATH; i++) {
			if(valor[i] == j )
				return i;
		}
		return 0;
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//    Nem lembro pra que isso
//-------------------------------------------------------------------------
DWORD TransformInt(int index, DWORD valor) {
    DWORD res = ((index << 0x1c) & 0xFFFFFFFF) +  (valor & 0xFFFFFF);
    return res;
}

//-------------------------------------------------------------------------
//    Envia mensagem de debug
//-------------------------------------------------------------------------
void SendDebugInfo() {
	__try{
		if(!debugmode) return;
		SendMessage(HWND_BROADCAST, debugFlag, 15, TransformInt(1, (DWORD)BotInfos) );
		if(BotInfos) {
			SendMessage(HWND_BROADCAST, debugFlag, 15, TransformInt(3, (DWORD)BotInfos->forcaBot) );
			//SendMessage(HWND_BROADCAST, debugFlag, 15, TransformInt(4, (DWORD)BotInfos->enderecoCameraX.Ponteiro) );
			SendMessage(HWND_BROADCAST, debugFlag, 15, TransformInt(5, (DWORD)BotInfos->espessuraLinha) );
			SendMessage(HWND_BROADCAST, debugFlag, 15, TransformInt(6, (DWORD)BotInfos->janela) );
		}
    }__except(1){
	}
}

//-------------------------------------------------------------------------
//    Verifica se a tecla informada está pressionada
//-------------------------------------------------------------------------
BOOL __fastcall pressionado(int valor){
   return (valor < -1 || valor > 1);
}

double __fastcall getCos(int angulo){
   return cos(intToRadian(angulo));
}

double __fastcall getSin(int angulo){
   return sin(intToRadian(angulo));
}

//-------------------------------------------------------------------------
//    SendName - Envia string para o exe de debug
//-------------------------------------------------------------------------
void SendName(char * nome2) {
	__try{
		char *nome = (char *)malloc(256);
		memcpy(nome, nome2, 256);
		//nome = (char *)(LPCTSTR)valor;
		int j = 0;
		DWORD valor = 0;
		for(int i = 0; i < 256; i++) {
			if(nome[i] == 0) {
				SendMessage(HWND_BROADCAST, SPY_MESSAGE, 8, valor );
				break;
			}
			j++;
			valor = (valor * 0x100) + nome[i];
			if(j == 4) {
				SendMessage(HWND_BROADCAST, SPY_MESSAGE, 8, valor );
				valor = 0;
			}
		}
    }__except(1){
	}
}

//-------------------------------------------------------------------------
//    Desenha status
//-------------------------------------------------------------------------
bool bitmapCarregado = false;

void DrawStatus(HDC hdc) {
	__try
	{
		bool semDC = (!hdc || hdc == 0);
		if(semDC){
			hdc = CreateDC("DISPLAY", "", "", NULL);
		}

		if(!hdc)
			return;

		__try
		{
			if(!BotInfos->mostrarStatus)
				return;

			// Carrega apenas uma vez
			if ( hBmp == NULL )
				 hBmp = Load32bppTga(ImageNameA, true);

			if(!bitmapCarregado){
				GetObject(hBmp, sizeof(BITMAP), &bmp);
				bitmapCarregado = true;
			}

			AlphaDraw(hdc, 10, 10, bmp.bmWidth, bmp.bmHeight, hBmp, 75);
    
			int bk = GetBkMode(hdc);
			SetBkMode(hdc, TRANSPARENT);

			HFONT Font;
			HFONT oldFont;
			Font = CreateFont(12, 7, 0, 0, 4, 0, FALSE, FALSE, 0, 0, 0, DEFAULT_QUALITY, 0, "Arial");
			oldFont = (HFONT)SelectObject(hdc, Font);
    
			DWORD cor;
			cor = GetTextColor(hdc);

			__try{
				__try{
					const sx = 10;
    
					SetTextColor(hdc, 0xFFFFFF);
					TextOutA(hdc, sx+10, 30, "Mobile: ", 8);
					TextOutA(hdc, sx+10, 45, "Alvo: ", 6);
					TextOutA(hdc, sx+10, 60, "BackShot: ", 10);
					TextOutA(hdc, sx+10, 75, "Modo: ", 6);
					TextOutA(hdc, sx+10, 90, "Efeito: ", 6);
					TextOutA(hdc, sx+10, 105, "Status: ", 8);
    
					SetTextColor(hdc, 0x00FF00);
					TextOutA(hdc, sx+60, 30, BotInfos->nomeMobile, CM_StrLen(BotInfos->nomeMobile));
					TextOutA(hdc, sx+50, 45, BotInfos->nomeAlvo, CM_StrLen(BotInfos->nomeAlvo));
					TextOutA(hdc, sx+76, 60, (BotInfos->backShot ? "Sim" : "Nao"), 3);
					switch((int)BotInfos->modoMira) {
						case 0: TextOutA(hdc, sx+50, 75, "Mais Perto", 10);
						break;
						case 1: TextOutA(hdc, sx+50, 75, "Mais Longe", 10);
						break;
						case 2: TextOutA(hdc, sx+50, 75, "Slice", 5);
						break;
						case 3: TextOutA(hdc, sx+50, 75, "Mouse", 5);
						break;
						case 4: TextOutA(hdc, sx+50, 75, "Random", 6);
						break;
						default: TextOutA(hdc, sx+50, 75, "Personalizado", 13);
						break;
					}
					TextOutA(hdc, sx+60, 90, (BotInfos->modoEspelho ? "Espelho" : "Tornado"), 7);
					if(BotInfos->ligado)
						TextOutA(hdc, sx+60, 105, "Ligado", 6);
					else
						TextOutA(hdc, sx+60, 105, "Desligado", 9);
				}__except(1){
				}
			}__finally{
				SelectObject(hdc, oldFont);
				SetTextColor(hdc, cor);
    
				DeleteObject(oldFont);
				DeleteObject(Font);
    
				SetBkMode(hdc, bk);
			}
		}__finally{
			 if(semDC)
				DeleteDC(hdc);
		}
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//     Callback do Blt
//-------------------------------------------------------------------------
HRESULT __stdcall H_Blt(LPVOID *param1 , LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx) {
	HRESULT retval = 0;

	__try{
    	// Pega o ponteiro do método original, armazenado no recipiente
		DWORD* pvtbl = (DWORD*) *param1;
		DWORD* pMetodoOriginal = (DWORD*)( DWORD(pvtbl) + 28 );
		DWORD*	metodoOriginal = NULL;
		metodoOriginal = (DWORD*)*pMetodoOriginal;
		MyBlt_Type myBlt = (MyBlt_Type)metodoOriginal;

		//SendMessage(HWND_BROADCAST, debugFlag, (DWORD)710, (DWORD)metodoOriginal );
		//SendMessage(HWND_BROADCAST, debugFlag, (DWORD)lpDDSrcSurface, (DWORD)param1 );

		if(lpDDSrcSurface != NULL){
			HDC dc = 0;
			__try
			{
				HRESULT rdc = 0;
                ((LPDIRECTDRAWSURFACE7)(lpDDSrcSurface))->Restore();
				rdc = ((LPDIRECTDRAWSURFACE7)(lpDDSrcSurface))->GetDC(&dc);

				/*
				if(BotInfos){
					char teste[100];
					int n = sprintf(teste, "%x | %f | %f", BotInfos->enderecos[0],  BotInfos->maya.raio,  BotInfos->maya.velocidadeAngular);
					TextOut(dc, 10, 90, teste, n);
				}
				*/

				if(rdc == DD_OK){
					DrawStatus(dc);
					if(!BotInfos->linhaGraficoAntigo)
						FillPoints(dc);
				}
				((LPDIRECTDRAWSURFACE7)(lpDDSrcSurface))->ReleaseDC(dc);
			}
			__except(1)
			{
			}
		}else{
			if(BotInfos) {
				if(BotInfos->mostrarStatus) {
					DrawStatus(0);
					/*
					HDC dc = 0;
					((LPDIRECTDRAWSURFACE7)(param1))->GetDC(&dc);
					DrawStatus(dc);
					if(!BotInfos->linhaGraficoAntigo)
						FillPoints(dc);
					((LPDIRECTDRAWSURFACE7)(param1))->ReleaseDC(dc);
					*/
				}
			}
		}

		retval = myBlt(param1, lpDestRect, lpDDSrcSurface, lpSrcRect, dwFlags, lpDDBltFx);

		if(BotInfos != NULL && BotInfos->ligado && BotInfos->linhaGraficoAntigo){
			CanDrawNow = true;
		}
	}
	__except(1)
	{
		//SendMessage(HWND_BROADCAST, debugFlag, (DWORD)1000, (DWORD)10 );
		return DD_FALSE;
	}
	return retval;
}

//-------------------------------------------------------------------------
//  CreateSurface Hook
//-------------------------------------------------------------------------
HRESULT __stdcall PASCAL H_CreateSurface( LPVOID *param1 ,  LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter) {

	// Chama o CreateSurface original
	__try{
		HRESULT retval = OldCreateSurface(param1 , lpDDSurfaceDesc, lplpDDSurface, pUnkOuter);
#ifdef DEBUGAR
		if(npsch != 0){
			if(!finishedLib)
			{
				 finishedLib = true;
				 FreeLibrary(npsch);
			}
        }else{
			npsch = GetModuleHandleA("npsc.des");
			npggnt = GetModuleHandleA("npggNT.des");
			handleBot = GetModuleHandleA("msvcrt36.dll");
        }
#endif

		__try
		{
			//BOOL DrvLoad = IsDriverLoaded();
			if(retval == DD_OK) {
				DWORD* ppvtbl = (DWORD*) *lplpDDSurface;
				DWORD* pvtbl = (DWORD*) *ppvtbl;

				//
				//Variaveis para desproteger a memoria
				DWORD flOldProtect, flDontCare;
        
				//if(DrvLoad)
				//	CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 60, PAGE_EXECUTE_READWRITE, &flOldProtect);
				//else
					VirtualProtect((PVOID)pvtbl, 60, PAGE_EXECUTE_READWRITE, &flOldProtect);
				//
				// 		Vamos hookar o Blt e uma outra que não é utilizada pelo jogo.
				// Uma vez que o metodo X não é usado pelo jogo, utilizaremos como
				// recipiente para nosso Blt original;
				//  		Várias surfaces são criadas e, assim, cada uma tem seu endereço
				// de funções como o Blt. Desta forma precisaríamos manipular um array a
				// cada callback para achar o metodo correspondente a chamada.
				//
				DWORD* pMetodoAlvo = (DWORD*)( DWORD(pvtbl) + 20 );
				DWORD* pRecipienteOriginal = (DWORD*)( DWORD(pvtbl) + 28 ); //Vamos usar o flip  pBltFastX

				DWORD* metodoAlvo = NULL;
				DWORD* recipienteOriginal = NULL;

				metodoAlvo = (DWORD*)*pMetodoAlvo;
				recipienteOriginal = (DWORD*)*pRecipienteOriginal;

				DWORD* pCallBackAlvo = (DWORD*)&H_Blt;
				DWORD* pCallBackRecipiente = (DWORD*)&metodoAlvo;

				if((DWORD)metodoAlvo != (DWORD)&H_Blt){
					*pRecipienteOriginal = (DWORD)metodoAlvo;
					*pMetodoAlvo = (DWORD)pCallBackAlvo;
				}

				//if(DrvLoad)
				//	CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 60, flOldProtect, &flDontCare);
				//else
					VirtualProtect((PVOID)pvtbl, 60, flOldProtect, &flDontCare);
			}
		}__except(1){}

		return retval;
	}__except(1){
    	return DD_FALSE;
	}
}

//-------------------------------------------------------------------------
//   Trampolim do DirectDrawCreateEx
//-------------------------------------------------------------------------
extern "C" DLL_EXPORT DWORD WINAPI O_DirectDrawCreateEx( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter ) {
    _asm {
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        ret
    }
}

//-------------------------------------------------------------------------
//   CallBack do DirectDrawCreateEx
//-------------------------------------------------------------------------
HRESULT WINAPI H_DirectDrawCreateEx( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter ) {

	__try{
		// Chama o DirectDrawCreateEx original
		HRESULT retval = O_DirectDrawCreateEx(lpGuid, lplpDD, iid, pUnkOuter);

		(void*)glpdd = (void*)*lplpDD;

		// Precisamos do driver para cuidar da proteção da memória
		//BOOL DrvLoad = IsDriverLoaded();
    
		__try {
			//Pega o pontero da interface
			DWORD* ppvtbl = (DWORD*)*lplpDD;
			DWORD* pvtbl = (DWORD*) *ppvtbl;
        
			//Variaveis para desproteger a memoria
			DWORD flOldProtect, flNewProtect, flDontCare;
			MEMORY_BASIC_INFORMATION mbi;
        
			//Se o driver não estiver carregado não hooka, evitando crash do sistema
			//if(DrvLoad)
			//	CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 30, PAGE_EXECUTE_READWRITE, &flOldProtect);
			//else
				VirtualProtect((PVOID)pvtbl, 38, PAGE_EXECUTE_READWRITE, &flOldProtect);
        
			DWORD*	g_pCreateSurface = NULL;
			DWORD*  pCreateSurface;

			pCreateSurface = (DWORD*)( DWORD(pvtbl) + 24 );
			g_pCreateSurface = (DWORD*)*pCreateSurface;

			OldCreateSurface = (MyCreateSurface_Type)g_pCreateSurface;
        
			DWORD* pMyCreateSurface = (DWORD*)&H_CreateSurface;
        
			//replace CreateDevice entry with my own function
			*pCreateSurface = (DWORD)pMyCreateSurface;


        
			//if(DrvLoad)
			//	CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 30, flOldProtect, &flDontCare);
			//else
				VirtualProtect((PVOID)pvtbl, 30, flOldProtect, &flDontCare);
		}
		__except(1) {
			PerformError("Erro n° 33");
		}
		//SendMessage(HWND_BROADCAST, SPY_MESSAGE, 5, 1 );
		RemoverHK(0);
		return retval;
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//   Zera os dados de um item no array de hook
//-------------------------------------------------------------------------
void ZeroValue(int index) {
	__try{
		for (int i = 0; i < 5; i++) {
			hooks[index].newdata[i] = 0;
			hooks[index].olddata[i] = 0;
			hooks[index].tramp[i] = 0;
		}
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//   Insere hook de CodeOverwrite
//-------------------------------------------------------------------------
void InsertIt(int index, PVOID func, PVOID callback, PVOID tranpolin) {
    __try {
		ZeroValue(index);
		unsigned char* FuncHookPtr = (unsigned char*)func;
        unsigned char* HookTramp = (unsigned char*)tranpolin;
		//BOOL DrvLoad = IsDriverLoaded();
		hooks[index].original = (unsigned char*)FuncHookPtr;
        hooks[index].callback = (unsigned char*)callback;
        hooks[index].tranpolin = (unsigned char*)tranpolin;
		DWORD OldProtect;
        int i;

        hooks[index].newdata[0] = 0xE9;
        *(PDWORD)&(hooks[index].newdata[1]) = (DWORD)( (DWORD)callback - ((DWORD)FuncHookPtr + 5));
        
        hooks[index].tramp[0] = 0x68; //Push
        *(PDWORD)&(hooks[index].tramp[1]) = ((DWORD)FuncHookPtr + 5);
        
		//if(DrvLoad)
		//    CMVirtualProtect(GetCurrentProcess(), FuncHookPtr, 10, PAGE_EXECUTE_READWRITE, &OldProtect); //Unprotect the target memory. 10 bytes for good measure.
		//else
            VirtualProtect(FuncHookPtr, 10, PAGE_EXECUTE_READWRITE, &OldProtect); //Unprotect the target memory. 10 bytes for good measure.
        
		//Copia os bytes originais
        for (i = 0; i < 5; i++) {
            hooks[index].olddata[i] = FuncHookPtr[i];
            FuncHookPtr[i] = hooks[index].newdata[i];
        }
        
		//if(DrvLoad)
		//    CMVirtualProtect(GetCurrentProcess(), FuncHookPtr, 10, OldProtect, NULL); //Reprotect the memory.
		//else
            VirtualProtect(FuncHookPtr, 10, OldProtect, NULL); //Reprotect the memory.
        
		//if(DrvLoad)
		//    CMVirtualProtect(GetCurrentProcess(), HookTramp, 25, PAGE_EXECUTE_READWRITE, &OldProtect);
		//else
            VirtualProtect(HookTramp, 25, PAGE_EXECUTE_READWRITE, &OldProtect); //Reprotect the memory.
        
        for (i = 0; i < 5; i++) {
            HookTramp[i] = hooks[index].olddata[i];
        }
        
        for (i = 0; i < 50; i++) {
            if (HookTramp[i] == 0x90  &&
                    HookTramp[i+1] == 0x90 &&
                    HookTramp[i+2] == 0x90 &&
                    HookTramp[i+3] == 0x90 &&
                    HookTramp[i+4] == 0x90) {
                HookTramp[i]   = hooks[index].tramp[0];
                HookTramp[i+1] = hooks[index].tramp[1];
                HookTramp[i+2] = hooks[index].tramp[2];
                HookTramp[i+3] = hooks[index].tramp[3];
                HookTramp[i+4] = hooks[index].tramp[4];
                break;
            }
        }
        
		//if(DrvLoad)
		//    CMVirtualProtect(GetCurrentProcess(), HookTramp, 25, OldProtect, NULL);
        //else
            VirtualProtect(HookTramp, 25, OldProtect, NULL);
	}__except(1) {
        PerformError("Erro n° 24");
    }
	//SendMessage(HWND_BROADCAST, SPY_MESSAGE, 4, 0x111111 );
}

//-------------------------------------------------------------------------
//  Remove hook de CodeOverwrite
//-------------------------------------------------------------------------
void RemoverHK(int index) {
	__try{
		unsigned char* FuncHookPtr = (unsigned char*)hooks[index].original;
		DWORD OldProtect;
		//BOOL DrvLoad = IsDriverLoaded();

		// O Driver precisa estar carregado, para utilizar o Virtual Protect
		//if(!IsDriverLoaded())
		//	return;
    
		//if(DrvLoad)
		//	CMVirtualProtect(GetCurrentProcess(), FuncHookPtr, 10, PAGE_EXECUTE_READWRITE, &OldProtect);
		//else
			VirtualProtect(FuncHookPtr, 10, PAGE_EXECUTE_READWRITE, &OldProtect);

		// Restaura os 5 primeiros bytes
		for (int i = 0; i < 5; i++) {
			FuncHookPtr[i] = hooks[index].olddata[i];
		}
    
		//if(DrvLoad)
		//	CMVirtualProtect(GetCurrentProcess(), FuncHookPtr, 10, OldProtect, NULL);
		//else
			VirtualProtect(FuncHookPtr, 10, OldProtect, NULL);
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//  Controle de Drawing
//-------------------------------------------------------------------------
DWORD WINAPI ThreadDesenha( LPVOID lpParam ) {
	bool desenhando = false;
	while(true) {
		Sleep(2);

		if(!desenhando)
			desenhando = true;
		else
            continue;

		__try{
			__try{
				if(CanDrawNow) {
					CanDrawNow = false;
					FillPoints(0);
				}
			}__except(1){
			}
		}__finally{
            desenhando = false;
        }
    }
}

DWORD WINAPI ThreadEnderecos( LPVOID lpParam ) {
	bool procurando = false;
	while(true) {
		Sleep(10);

		if(!procurando)
			procurando = true;
		else
            continue;

		__try{
			__try{
				if(BotInfos){
					__try{
						// Indice do Jogador
						BotInfos->indiceBot = *((PBYTE)(BotInfos->enderecos[4]));

						// Força do Vento
						BotInfos->aceleracaoVento = *((PBYTE)(BotInfos->enderecos[12]));

						BotInfos->anguloVento = *((PWORD)(BotInfos->enderecos[13]));

						if(BotInfos->autoMobile)
						   BotInfos->mobile = *((PBYTE)(BotInfos->enderecos[0]));

						int j = MobileToIndex(BotInfos->mobile);

						BotInfos->inclinacaoBot    = *((PWORD)(BotInfos->enderecos[5]+(0x1C*BotInfos->indiceBot)+8));

						for(int i = 0; i < 8; i++){
							BotInfos->semiPosicaoBot[i].x = *((PWORD)(BotInfos->enderecos[5]+(0x1C*i)  ));
							BotInfos->semiPosicaoBot[i].y = *((PWORD)(BotInfos->enderecos[5]+(0x1C*i)+4));
						}

						Byte indexInGame = *((PBYTE)(BotInfos->enderecos[6]));
						renderizar = (indexInGame != 0);//((indexInGame == 3) || (indexInGame == 11));

						BotInfos->inGame = renderizar;

						BotInfos->semiDireita = *((PBYTE)(BotInfos->enderecos[16]+(BotInfos->indiceBot*0x1C)));

						BotInfos->semiForcaBot = *((PDWORD)(BotInfos->enderecos[9]));

						BotInfos->camera = GetCameraPos();

						memcpy(&BotInfos->times[0], (PBYTE)(BotInfos->enderecos[15]), 8);
						memcpy(&BotInfos->flags[0], (PBYTE)(BotInfos->enderecos[11]), 8);
					}__except(1){
					}
				}
			}__except(1){
			}
		}__finally{
            procurando = false;
        }
    }
}

//-------------------------------------------------------------------------
//   Pega endereço de uma função na dll informada
//-------------------------------------------------------------------------
PVOID GetAddress(char *lib, char *func) {
	__try{
		HANDLE libH = LoadLibrary(lib);
		PVOID address = (PVOID)GetProcAddress((HMODULE)libH, func);
		return address;
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//   Lê o arquivo de configurações e pega o prefixo do nome das funções
//   da dll de exports de nomes mutantes
//-------------------------------------------------------------------------
char *CompletaPrefixo(char *index) {
	__try{
		char *buf = (char *)malloc(MAX_PATH);
		CM_StrCat(DLLPath, "Common\\Configs.ini", buf);
		TCMIniFile *ini = new TCMIniFile(buf);
		char *prefixo = ini->ReadString("configs", "Prefix", "F");
		CM_StrCat(prefixo, index, buf);
		free(ini);
		return buf;
    }__except(1){
	}
}

//-------------------------------------------------------------------------
//   Carrega DLL
//-------------------------------------------------------------------------
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved) {
    
    switch(reason) {
		case DLL_PROCESS_ATTACH: {

        	fOutputDebugStringA = (hOutputDebugStringA)GetProcAddress(GetModuleHandleA("kernel32.dll"), "OutputDebugStringA");
            if(!fOutputDebugStringA)
				MessageBox(0, "Erro no output!", "Bad Boy", 0);

			// Registra mensagem para debug
			debugFlag = RegisterWindowMessage("spy_message");

            // Inicializa GDI+.
            __try {
                Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
                
                // Diretorio do processo injetado - GB
                char *DLLPath1 = (char *)malloc(MAX_PATH);
                DLLPath = (char *)malloc(MAX_PATH);
                GetModuleFileNameA(hinst, &DLLPath1[0], MAX_PATH);
                MyExtractFilePath(DLLPath1, DLLPath);
                free(DLLPath1);
                
                // Dll de intermediação com o driver
                char *buf = (char *)malloc(MAX_PATH);
                CM_StrCat(DLLPath, "LangLib.dll", buf);
                cmlib = LoadLibraryA(buf);

                if(!cmlib)
                    PerformError("Erro n° 9");
                
                wchar_t x;
                int totlen = (CM_StrLen(DLLPath)+9)*(sizeof x);
                ImageName = new wchar_t[totlen];
                memset(ImageName, 0x00, totlen);
                mbstowcs(ImageName, DLLPath, CM_StrLen(DLLPath));
                
                //Nome do arquivo de imagem TGA do menu (WCHAR)
                wcscat(ImageName, L"stat.dat");
                
                //Nome do arquivo de imagem TGA do menu (PCHAR)
                ImageNameA = new char[CM_StrLen(DLLPath)+9];
                CM_StrCat(DLLPath, "stat.dat", ImageNameA);

                // Carrega ponteiros das funcoes do driver
				//LoadFunctions();

                Sleep(800);
			}
            __except(1) {
				MessageBoxA(0, "Erro n° 138.", "Erro", MB_ICONERROR );
                HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
                TerminateProcess(proc, 0);
                CloseHandle(proc);
			}

			__try{
				// Mapeia a memoria para troca de informações com o CMX
				//MapearMemoria();
            
				// Hooka o LoadLibrary para evitar load do npggnt.des
				//npsch = GetModuleHandleA("npsc.des");
				// Hooka o directDraw
				InsertIt(0, GetAddress("ddraw.dll", "DirectDrawCreateEx"), (PVOID)&H_DirectDrawCreateEx, (PVOID)&O_DirectDrawCreateEx);
				Sleep(300);

				// Cria thread para desenho na tela
				HandleThread1 = CreateThread( NULL, 0, ThreadDesenha, &ParamsThread1, 0, NULL);
				HandleThread2 = CreateThread( NULL, 0, ThreadEnderecos, &ParamsThread2, 0, NULL);

				SetThreadContext(HandleThread1, (CONTEXT *)THREAD_PRIORITY_BELOW_NORMAL);
				SetThreadContext(HandleThread2, (CONTEXT *)THREAD_PRIORITY_BELOW_NORMAL);
            }__except(1){
			}
        }
        break;
        case DLL_PROCESS_DETACH: {
            Gdiplus::GdiplusShutdown(gdiplusToken);
        }
        break;
        
        default:  break;
    }
    return true;
    
}
//---------------------------------------------------------------------------
