//---------------------------------------------------------------------------

#pragma hdrstop

#include "draws.h"
#include "debugutils.h"
#include "variaveis.h"
#include "drawUtils.h"
#include "shotmatrix.h"
#include "driverSupport.h"
#include "dll.h"
#include "hooks.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

void __fastcall FillPoints(HDC dc){
	__try{
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
/*
void Png32_Show(HDC hdc, int xDest,int yDest,int nWidth,int nHeight,PNGINFO *pPngInfo,int xSour,int ySour)
{
	if(xSour+nWidth>(int)pPngInfo->nWidth)
		nWidth=pPngInfo->nWidth-xSour;

	if(ySour+nHeight>(int)pPngInfo->nHeight)
		nHeight=pPngInfo->nHeight-ySour;

	if(nWidth>0 && nHeight>0){
		 HDC hmemdc=0;
		 LPBYTE pBitsDest=NULL;
		 HBITMAP hbmpDest=0;
		 HGDIOBJ hOldBmp=0;
		 BITMAPINFO bmi;
		 //sour memory
		 unsigned char ** row_pointers =
		   pPngInfo->ppbyRow+pPngInfo->nHeight-1-(pPngInfo->nHeight-ySour-nHeight);
		 //Do alpla blend
		 int nLineTailDest=WIDTHBYTES(24*nWidth)-3*nWidth;
		 // Initialize header to 0s.
		 ZeroMemory(&bmi, sizeof(bmi));
		 // Fill out the fields you care about.
		 bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
		 bmi.bmiHeader.biWidth = nWidth;
		 bmi.bmiHeader.biHeight = nHeight;
		 bmi.bmiHeader.biPlanes = 1;
		 bmi.bmiHeader.biBitCount = 24;
		 bmi.bmiHeader.biCompression = BI_RGB;
		 // Create the surface.
		 hmemdc=CreateCompatibleDC(hdc);
		 // Get Dest Rectangle memory
		 hbmpDest = CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS,
					(void **)&pBitsDest, NULL, 0);
		 hOldBmp=SelectObject(hmemdc,hbmpDest);
		 BitBlt(hmemdc,0,0,nWidth,nHeight,hdc,xDest,yDest,SRCCOPY);
		#ifdef ASM_CORE
		 __asm
		 {
		  push esi
		  push edi
		  push eax
		  push ebx
		  push ecx
		  push edx
		  push es
		  push ds

		  mov ax,ds
		  mov es,ax
		  mov edi,pBitsDest
		  mov ecx,nHeight
		@@beginy:
		  mov ebx, row_pointers
		  mov esi,[ebx]
		  //offset picture's left margin
		  mov eax,xSour
		  imul eax,4  ;//4 bytes make up one pixel
		  add esi,eax
		  mov eax,nWidth
		@@beginx:
		   //get alpla value
		   xor ebx,ebx
		   mov bl,[esi+3]

		   //blue
		   xor dx,dx
		   mov dl,[edi];
		   imul dx,bx
		   add dh,[esi+2]
		   mov [edi],dh ;//save result to *pBitsDest
		   inc edi
		   //green
		   xor dx,dx
		   mov dl,[edi];
		   imul dx,bx
		   add dh,[esi+1]
		   mov [edi],dh ;//save result to *pBitsDest
		   inc edi
		   //red
		   xor dx,dx
		   mov dl,[edi];
		   imul dx,bx
		   add dh,[esi]
		   mov [edi],dh ;//save result to *pBitsDest
		   inc edi
		   add esi,4

		   dec eax
		   cmp eax,0
		   jne @@beginx
		   add edi,nLineTailDest
		   sub row_pointers,4 ;//next line
		   loop @@beginy
		   pop ds
		   pop es
		   pop edx
		   pop ecx
		   pop ebx
		   pop eax
		   pop edi
		   pop esi
		 }
		#else//ASM_CORE
		 {
			  int i,j;
			  BYTE *p1=pBitsDest;
			  for(i=0;i<nHeight;i++){
				  BYTE *p2=*(row_pointers--);
				  for(j=0;j<nWidth;j++){
					 *p1++=((p2[3]*(*p1))>>8)+p2[2];
					 *p1++=((p2[3]*(*p1))>>8)+p2[1];
					 *p1++=((p2[3]*(*p1))>>8)+p2[0];
					 p2+=4;
				  }
				  p1+=nLineTailDest;
			  }
		 }
		#endif//ASM_CORE
		 //render
		 BitBlt(hdc,xDest,yDest,nWidth,nHeight,hmemdc,0,0,SRCCOPY);
		 SelectObject(hmemdc,hOldBmp);
		 //Free memory
		 DeleteObject(hbmpDest);
		 DeleteDC(hmemdc);
	}
}
*/

//-------------------------------------------------------------------------
//    Desenha status
//-------------------------------------------------------------------------
void DrawStatus(HDC hdc) {
	bool bitmapCarregado = false;

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
				 hBmp = Load32bppTga(fundoMenu, true);

			if(!bitmapCarregado){
				GetObject(hBmp, sizeof(BITMAP), &bmp);
				bitmapCarregado = true;
			}

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

					AlphaDraw(hdc, 10, 10, bmp.bmWidth, bmp.bmHeight, hBmp, 75);

					SetTextColor(hdc, 0xFFFFFF);
					TextOutA(hdc, sx+10, 30, "Mobile: ", 8);
					TextOutA(hdc, sx+10, 45, "Alvo: ", 6);
					TextOutA(hdc, sx+10, 60, "BackShot: ", 10);
					TextOutA(hdc, sx+10, 75, "Modo: ", 6);
					TextOutA(hdc, sx+10, 90, "Efeito: ", 6);
					TextOutA(hdc, sx+10, 105, "Status: ", 8);

					SetTextColor(hdc, 0x00FF00);
					TextOutA(hdc, sx+60, 30, BotInfos->nomeMobile, strlen(BotInfos->nomeMobile));
					TextOutA(hdc, sx+50, 45, BotInfos->nomes[BotInfos->alvoID], strlen(BotInfos->nomes[BotInfos->alvoID]));
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

void DrawAviso(HDC hdc) {
	bool bitmapCarregado = false;

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

					for(int i = 0; i < 10; i++){
						if(avisos[i].ativo){
							if((GetTickCount() - avisos[i].inicio) > avisos[i].tempo){
								memset(&avisos[i].valor,0,150);
								avisos[i].len = 0;
								avisos[i].ativo = false;
								avisos[i].inicio = 0;
							}else{
						   		TextOutA(hdc, 10, i*15, avisos[i].valor, avisos[i].len);
							}
						}
					}
					//SetTextColor(hdc, 0x00FF00);
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
bool primeiraAmostragemAviso = false;
HRESULT __stdcall BltHooked(LPVOID *param1 , LPRECT lpDestRect, LPVOID lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPVOID lpDDBltFx) {
	HRESULT retval = 0;

	__try{
    	// Pega o ponteiro do método original, armazenado no recipiente
		DWORD* pvtbl = (DWORD*) *param1;
		DWORD* pMetodoOriginal = (DWORD*)( DWORD(pvtbl) + 28 );
		DWORD*	metodoOriginal = NULL;
		metodoOriginal = (DWORD*)*pMetodoOriginal;
		MyBlt_Type myBlt = (MyBlt_Type)metodoOriginal;

		if(lpDDSrcSurface != NULL){
			HDC dc = 0;
			__try
			{
            	if(!primeiraAmostragemAviso){
					primeiraAmostragemAviso = true;
					gerarAviso("Cheat Matrix X");
				}

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
					DrawAviso(dc);
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
					DrawAviso(0);
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
HRESULT __stdcall PASCAL CreateSurfaceHooked( LPVOID *param1 ,  LPVOID lpDDSurfaceDesc, LPVOID * lplpDDSurface, DWORD * pUnkOuter) {

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
			BOOL DrvLoad = IsDriverLoaded();
			if(retval == DD_OK) {
				DWORD* ppvtbl = (DWORD*) *lplpDDSurface;
				DWORD* pvtbl = (DWORD*) *ppvtbl;

				//Variaveis para desproteger a memoria
				DWORD flOldProtect, flDontCare;

				if(DrvLoad)
					CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 60, PAGE_EXECUTE_READWRITE, &flOldProtect);
				else
					VirtualProtect((PVOID)pvtbl, 60, PAGE_EXECUTE_READWRITE, &flOldProtect);

				/* 		Vamos hookar o Blt e uma outra que não é utilizada pelo jogo.
				* Uma vez que o metodo X não é usado pelo jogo, utilizaremos como
				* recipiente para nosso Blt original;
				*  		Várias surfaces são criadas e, assim, cada uma tem seu endereço
				* de funções como o Blt. Desta forma precisaríamos manipular um array a
				* cada callback para achar o metodo correspondente a chamada.
				*/
				DWORD* pMetodoAlvo = (DWORD*)( DWORD(pvtbl) + 20 );
				DWORD* pRecipienteOriginal = (DWORD*)( DWORD(pvtbl) + 28 ); //Vamos usar o flip  pBltFastX

				DWORD* metodoAlvo = NULL;
				DWORD* recipienteOriginal = NULL;

				metodoAlvo = (DWORD*)*pMetodoAlvo;
				recipienteOriginal = (DWORD*)*pRecipienteOriginal;

				DWORD* pCallBackAlvo = (DWORD*)&BltHooked;
				DWORD* pCallBackRecipiente = (DWORD*)&metodoAlvo;

				//SendMessage(HWND_BROADCAST, debugFlag, (DWORD)910, (DWORD)metodoAlvo );
				//SendMessage(HWND_BROADCAST, debugFlag, (DWORD)920, (DWORD)recipienteOriginal );

				if((DWORD)metodoAlvo != (DWORD)&BltHooked){
					//*pCallBackRecipiente = (DWORD)metodoAlvo;
					*pRecipienteOriginal = (DWORD)metodoAlvo;
					*pMetodoAlvo = (DWORD)pCallBackAlvo;
				}

				if(DrvLoad)
					CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 60, flOldProtect, &flDontCare);
				else
					VirtualProtect((PVOID)pvtbl, 60, flOldProtect, &flDontCare);
			}
		}__except(1){}

		return retval;
	}__except(1){
    	return DD_FALSE;
	}
}

//-------------------------------------------------------------------------
//   CallBack do DirectDrawCreateEx
//-------------------------------------------------------------------------
HRESULT WINAPI DirectDrawCreateExHooked( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter ) {
	__try{
		// Chama o DirectDrawCreateEx original
		HRESULT retval = DirectDrawCreateExTramp(lpGuid, lplpDD, iid, pUnkOuter);

		(void*)glpdd = (void*)*lplpDD;

		// Precisamos do driver para cuidar da proteção da memória
		BOOL DrvLoad = IsDriverLoaded();

		__try {
			//Pega o pontero da interface
			DWORD* ppvtbl = (DWORD*)*lplpDD;
			DWORD* pvtbl = (DWORD*) *ppvtbl;

			//Variaveis para desproteger a memoria
			DWORD flOldProtect, flNewProtect, flDontCare;
			MEMORY_BASIC_INFORMATION mbi;

			//Se o driver não estiver carregado não hooka, evitando crash do sistema
			if(DrvLoad)
				CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 30, PAGE_EXECUTE_READWRITE, &flOldProtect);
			else
				VirtualProtect((PVOID)pvtbl, 38, PAGE_EXECUTE_READWRITE, &flOldProtect);

			DWORD*	g_pCreateSurface = NULL;
			DWORD*  pCreateSurface;

			pCreateSurface = (DWORD*)( DWORD(pvtbl) + 24 );
			g_pCreateSurface = (DWORD*)*pCreateSurface;

			OldCreateSurface = (MyCreateSurface_Type)g_pCreateSurface;

			DWORD* pMyCreateSurface = (DWORD*)&CreateSurfaceHooked;

			//replace CreateDevice entry with my own function
			*pCreateSurface = (DWORD)pMyCreateSurface;



			if(DrvLoad)
				CMVirtualProtect(GetCurrentProcess(), (PVOID)pvtbl, 30, flOldProtect, &flDontCare);
			else
				VirtualProtect((PVOID)pvtbl, 30, flOldProtect, &flDontCare);
		}
		__except(1) {
			PerformError("Erro n° 33");
		}
		UnhookDetour(0);
		return retval;
	}__except(1){
	}
}

//-------------------------------------------------------------------------
//   Trampolim do DirectDrawCreateEx
//-------------------------------------------------------------------------
extern "C" DLL_EXPORT DWORD WINAPI DirectDrawCreateExTramp( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid, IUnknown FAR *pUnkOuter ) {
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