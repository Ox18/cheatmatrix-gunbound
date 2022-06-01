#include "baseCentral.h"
#include "..\mathUtils.h"
#include "..\secret.h"

// #include "variaveis.h"
// #include <tlhelp32.h>

// static HINSTANCE hApiHookDLL = 0;

//
// Inicializa as variáveis e threads
//
void inicializarDLL() {
	static bool inicializado;
	if (!inicializado) {
		inicializado = true;

		closeFlag = CreateEvent(NULL, TRUE, FALSE, NULL);
		InitializeCriticalSection(&flagDraw);
		InitializeCriticalSection(&flagUpdate);
		InitializeCriticalSection(&flagEnderecador);
		InitializeCriticalSection(&flagSubplugins);
		InitializeCriticalSection(&flagAtalho);

		threads = new HANDLE[4];
		threads[0] = CreateThread(NULL, 0, ThreadDesenha, NULL, 0, NULL);
		threads[1] = CreateThread(NULL, 0, ThreadUpdate, NULL, 0, NULL);
		threads[2] = CreateThread(NULL, 0, ThreadEnderecos, NULL, 0, NULL);
		threads[3] = CreateThread(NULL, 0, ThreadAtalhos, NULL, 0, NULL);
		SetThreadContext(threads[0], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
		SetThreadContext(threads[1], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
		SetThreadContext(threads[2], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
		SetThreadContext(threads[3], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
	}
}

void __stdcall enviar(int socket, TIPO_PACOTE tipo, char* buffer, int size) {
	servidor->enviar(socket, tipo, buffer, size);
}

//
// Finaliza a DLL limpando as variáveis
//
void finalizarDLL() {
	static bool inicializado;
	if (!inicializado) {
		SetEvent(closeFlag);
		WaitForMultipleObjects(4, threads, true, INFINITE);

		DeleteCriticalSection(&flagDraw);
		DeleteCriticalSection(&flagUpdate);
		DeleteCriticalSection(&flagEnderecador);
		DeleteCriticalSection(&flagSubplugins);
		DeleteCriticalSection(&flagAtalho);
		CloseHandle(closeFlag);
	}
}

void ErrorExit(LPTSTR lpszFunction) {
	// Retrieve the system error message for the last-error code

	LPVOID lpMsgBuf;
	LPVOID lpDisplayBuf;
	DWORD dw = GetLastError();

	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, dw, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) & lpMsgBuf, 0, NULL);

	// Display the error message and exit the process

	lpDisplayBuf = (LPVOID)LocalAlloc(LMEM_ZEROINIT, (lstrlen((LPCTSTR)lpMsgBuf) + lstrlen((LPCTSTR)lpszFunction) + 40)*sizeof(TCHAR));
	debugar("%s failed with error %d: %s", lpszFunction, dw, lpMsgBuf);

	LocalFree(lpMsgBuf);
	LocalFree(lpDisplayBuf);
	// ExitProcess(dw);
}

void __stdcall PASCAL OnClientDisconnect(ContextoCliente *contexto) {
	if (contexto->subplugin && contexto->subplugin->pluginID > 0) {
		// debugar("Cliente %s desconectado", contexto->subplugin->nomePlugin);

		char *nome = strrchr(contexto->subplugin->nomePlugin, '\\');
		nome++;

		int plugID = codigoToIndice(contexto->subplugin->pluginID);
		subplugs->removeFrom(plugID);
		SetEvent(contexto->subplugin->closeFlag);
		Sleep(1000);

		// debugar("teste: %X %s", GetModuleHandleA(nome), nome);

		for (int i = 0; GetModuleHandleA(nome) != 0 && i < 10; i++) {
			__try {
				// debugar("Limpando plugin [%X - %X] %s da memoria...", contexto->subplugin->handle, GetModuleHandleA(nome), nome);
				if (FreeLibrary(GetModuleHandleA(nome))) {
					debugar("unloaded");
				}
				else {
					//ErrorExit(TEXT("FreeLibrary"));
					debugar("Erro 4444!");
				}
			}
			__except (1) {
				debugar("Erro 8828");
			}
		}
		CloseHandle(contexto->subplugin->closeFlag);
		free(contexto->subplugin->nomePlugin);
		free(contexto->subplugin);
	}
}

//
// Chama o método de draw de cada subplugin
//
void chamarDraw(HDC dc, char tipoDesenho) {
	if(inicializado){
		EnterCriticalSection(&flagDraw);
		__try {
			// debugar("blt 3 - %d", subplugs->size());
			for (int i = 0; i < subplugs->size(); i++) {
				__try {
					SUBPLUGIN *subplugin = (SUBPLUGIN*)subplugs->get(i);
					if (subplugin && subplugin->metodos && (DWORD)subplugin->metodos->draw) {
						typedef HRESULT(__stdcall PASCAL * TDrawFunction)(HDC, char);
						TDrawFunction drawFunction = (TDrawFunction)subplugin->metodos->draw;
						if (drawFunction) {
							__try {
								// debugar("chamando draw...");
								drawFunction(dc, tipoDesenho);
							}
							__except (1) {
								debugar("Erro 2421.");
							}
						}
					}
				}
				__except (1) {
					debugar("Erro 9097");
				}
			}
		}
		__finally {
			LeaveCriticalSection(&flagDraw);
		}
	}
}

//
// Chama o método de atualização de endereços de cada subplugin
//
void chamarUpdate() {
	EnterCriticalSection(&flagUpdate);
	__try {
		for (int i = 0; i < subplugs->size(); i++) {
			__try {
				SUBPLUGIN *subplugin = (SUBPLUGIN*)subplugs->get(i);
				if (subplugin != NULL && subplugin->metodos != NULL && (DWORD)subplugin->metodos->update != NULL) {
					if ((DWORD)subplugin->metodos->update) {
						__try {
							subplugin->metodos->update();
						}
						__except (1) {
							debugar("Erro 1251.");
						}
					}
				}
			}
			__except (1) {
				debugar("Erro 9098");
			}
		}
	}
	__finally {
		LeaveCriticalSection(&flagUpdate);
	}
}

void chamarEnderecador() {
	EnterCriticalSection(&flagEnderecador);
	__try {
		for (int i = 0; i < subplugs->size(); i++) {
			__try {
				SUBPLUGIN *subplugin = (SUBPLUGIN*)subplugs->get(i);
				if (subplugin && (DWORD)subplugin->metodos && (DWORD)subplugin->metodos->atualizarEnderecos) {
					__try {
						subplugin->metodos->atualizarEnderecos();
					}
					__except (1) {
						debugar("Erro 6233.");
					}
				}
			}
			__except (1) {
				debugar("Erro 9099");
			}
		}
	}
	__finally {
		LeaveCriticalSection(&flagEnderecador);
	}
}

void DesenharLinha(int x1, int y1, int x2, int y2) {

}

//
// Gera uma mensagem de erro
//
void PerformError(char *msg, char *titulo) {
	if (!titulo)
		MessageBoxA(NULL, msg, "Erro", NULL);
	else
		MessageBoxA(NULL, msg, titulo, NULL);
	HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
	TerminateProcess(proc, 0);
	CloseHandle(proc);
}

//
// Controle de Drawing - Modo GDI
//
DWORD WINAPI ThreadDesenha(LPVOID lpParam) {
	while (WaitForSingleObject(closeFlag, 10) != WAIT_OBJECT_0) {
		__try {
			if(inicializado){
				chamarDraw(0, TD_UPDATE);
				if (canDrawNow) {
					canDrawNow = false;
					chamarDraw(0, TD_GDI);
				}
			}
		}
		__except (1) {
		}
	}
}

//
// Atualiza os endereços pelos ponteiros
//
DWORD WINAPI ThreadEnderecos(LPVOID lpParam) {
	while (WaitForSingleObject(closeFlag, 500) != WAIT_OBJECT_0) {
		__try {
			if (inicializado) {
				chamarEnderecador();
			}
		}
		__except (1) {
			debugar("Erro 1001");
		}
	}
}

//
// Atualiza os endereços de acordo com os dados de Ponteiro informados
//
DWORD WINAPI ThreadUpdate(LPVOID lpParam) {
	while (WaitForSingleObject(closeFlag, 10) != WAIT_OBJECT_0) {
		__try {
			if (inicializado) {
				chamarUpdate();
			}
		}
		__except (1) {
		}
	}
}

struct D3DDevice
{
   D3DDevice* Next;

   GUID Guid;
   char* Name;
   BOOL IsHW;
};

struct DisplayMode
{
   DisplayMode* Next;

   int Width;
   int Height;
   int Depth;
};


struct DisplayDriver
{
   DisplayDriver* Next;

   GUID Guid;
   char* Description;

   D3DDevice* D3DDeviceList;
   DisplayMode* DisplayModeList;
};

DisplayDriver* DisplayDriverList;



HRESULT CALLBACK EnumDisplayDrivers( GUID* pGuid, LPSTR DriverDescription,
                                     LPSTR DriverName, LPVOID Context,
                                     HMONITOR HMonitor )
{
   DisplayDriver* NewDD = new DisplayDriver;
   if( NewDD )
   {
      ZeroMemory( NewDD, sizeof( DisplayDriver ));

      if( pGuid )
         CopyMemory( &( NewDD->Guid ), pGuid, sizeof( GUID ));

	  NewDD->Description = new char[strlen( DriverDescription ) + 1];
//	  debugar("ddraw s: %s",NewDD->Description);
      if( NewDD->Description )
      {
         strcpy( NewDD->Description, DriverDescription );
      }

      NewDD->DisplayModeList = NULL;
      NewDD->D3DDeviceList = NULL;
      NewDD->Next = NULL;
   }
  /*
   if( !DisplayDriverList )
      DisplayDriverList = NewDD;
   else
   {
      for( DisplayDriver* TheDD = DisplayDriverList; TheDD->Next; TheDD = TheDD->Next )
         ;

      TheDD->Next = NewDD;
   }     */

   return DDENUMRET_OK;
}

Screen* s_pScreenFirst = NULL; // Linked list of Screens
HWND s_hwnd = NULL;

//-----------------------------------------------------------------------------
// Name: DDEnumCallbackEx()
// Desc: This callback function is called by DirectDraw once for each
//       available DirectDraw device.  In this implementation, it saves the
//       GUID, device description, and hmon in a Screen structure for later use.
//-----------------------------------------------------------------------------
BOOL WINAPI DDEnumCallbackEx( GUID* pGuid, LPTSTR pszDesc, LPTSTR pszDriverName,
							  VOID* pContext, HMONITOR hmon )
{
//	debugar("DDraw object: %x %x %x %x", pGuid, pszDriverName, pContext, hmon);

	Screen* pScreenNew;
    EnumInfo* pEnumInfo = (EnumInfo*)pContext;
    GUID guidNull;
    ZeroMemory(&guidNull, sizeof(GUID));

	if (s_pScreenFirst != NULL && s_pScreenFirst->guid == guidNull){
        // We must be running on a multimon system, so get rid of the
        // guidNull Screen -- we want Screens with specific GUIDs.
        delete s_pScreenFirst;
        s_pScreenFirst = NULL;
    }

    // Store all the info in a Screen structure
	pScreenNew = new Screen;
	if (pScreenNew == NULL){
        pEnumInfo->hr = E_OUTOFMEMORY;
        return FALSE; // fatal error, stop enumerating
    }
	ZeroMemory(pScreenNew, sizeof(Screen));
    if (pGuid == NULL)
		pScreenNew->guid = guidNull;
    else
		pScreenNew->guid = *pGuid;
	lstrcpy(pScreenNew->szDesc, pszDesc);

    pScreenNew->hmon = hmon;

    // Insert Screen into global linked list
    if (s_pScreenFirst == NULL)
        s_pScreenFirst = pScreenNew;
    else
    {
        // Insert at end of list
        Screen* pScreen = s_pScreenFirst;
        while (pScreen->pScreenNext != NULL)
            pScreen = pScreen->pScreenNext;
		pScreen->pScreenNext = pScreenNew;
    }


	return TRUE; // Keep enumerating
}

HRESULT EnumerateScreens( VOID )
{
    HRESULT hr;
    HMODULE hModule = NULL;
    LPDIRECTDRAWENUMERATEEX pDDEnumEx = NULL;
    EnumInfo enumInfo;

    ZeroMemory(&enumInfo, sizeof(enumInfo));

    hModule = LoadLibrary( TEXT("ddraw.dll") );
    // If ddraw.dll doesn't exist in the search path,
    // then DirectX probably isn't installed, so fail.
    if (hModule == NULL)
        return E_FAIL;

    pDDEnumEx = (LPDIRECTDRAWENUMERATEEX) GetProcAddress(hModule,
#ifdef UNICODE
        "DirectDrawEnumerateExW"
#else
        "DirectDrawEnumerateExA"
#endif
        );

    if (pDDEnumEx == NULL)
    {
        // We must be running on an old version of DirectDraw.
        // Therefore MultiMon isn't supported. Fall back on
        // DirectDrawEnumerate to enumerate standard devices on a
        // single-monitor system.
        enumInfo.bMultimonSupported = FALSE;
        //hr = DirectDrawEnumerate(DDEnumCallback, &enumInfo);
    }
    else
    {
        enumInfo.bMultimonSupported = TRUE;
        hr = pDDEnumEx(DDEnumCallbackEx, &enumInfo, DDENUM_ATTACHEDSECONDARYDEVICES);
    }

    // If something failed inside the enumeration, be sure to return that HRESULT
    if (SUCCEEDED(hr) && FAILED(enumInfo.hr))
        hr = enumInfo.hr;

    FreeLibrary(hModule);
    return hr;
}

PVOID GetAddress(char *lib, char *func) {
	__try{
		HANDLE libH = LoadLibrary(lib);
		PVOID address = (PVOID)GetProcAddress((HMODULE)libH, func);
		return address;
	}__except(1){
	}
}

HRESULT InitScreens( VOID )
{
    HRESULT hr;
    Screen* pScreen;
    GUID* pGuid;
    DWORD dwFlags;
    DDSURFACEDESC2 ddsd;
    DDSCAPS2 ddsCaps;
    RECT rc;
    HRGN hrgn;
    BYTE rgnDataBuffer[1024];
    GUID guidNull;
    ZeroMemory(&guidNull, sizeof(GUID));

//	debugar("step1");
	typedef HRESULT(__stdcall PASCAL *tMyDirectDrawCreateEx)( GUID FAR * lpGuid, LPVOID  *lplpDD, REFIID  iid,IUnknown FAR *pUnkOuter );
	tMyDirectDrawCreateEx MyDirectDrawCreateEx = (tMyDirectDrawCreateEx)GetAddress("ddraw.dll", "DirectDrawCreateEx");
    for (pScreen = s_pScreenFirst; pScreen != NULL; pScreen = pScreen->pScreenNext)
	{
//    	debugar("step2");
        if (pScreen->guid == guidNull) pGuid = NULL;
        else pGuid = &pScreen->guid;

		if (FAILED(hr = MyDirectDrawCreateEx(pGuid, (VOID**)&(pScreen->pdd), IID_IDirectDraw7, NULL)))
		return hr;
//		debugar("step3 %x ",pScreen->pdd);
		/*
		if (pScreen == s_pScreenFirst)
		{
			debugar("step4");
            dwFlags = DDSCL_SETFOCUSWINDOW;
            if (FAILED(hr = pScreen->pdd->SetCooperativeLevel(s_hwnd, dwFlags)))return hr;

            dwFlags = DDSCL_ALLOWREBOOT | DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN;
            if (FAILED(hr = pScreen->pdd->SetCooperativeLevel(s_hwnd, dwFlags)))return hr;
        }
        else
		{
			debugar("step5");
            dwFlags = DDSCL_SETFOCUSWINDOW | DDSCL_CREATEDEVICEWINDOW |DDSCL_ALLOWREBOOT | DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN;
            if (FAILED(hr = pScreen->pdd->SetCooperativeLevel(s_hwnd, dwFlags)))return hr;
        }    */
		//debugar("step6");
		//if (FAILED(hr = pScreen->pdd->SetDisplayMode(100, 100,32,0,0))) return hr;
		//debugar("step7");
  // Note: It is recommended that programs call SetDisplayMode on all screens
  // before creating/acquiring any DirectDrawSurfaces.
	}
//	debugar("step8");
	if (s_pScreenFirst==NULL)
	{
//		debugar("step9");
		MessageBox(GetFocus(), "First Screen is Null. Something Wrong", "Restart", MB_OK);
		exit(1);
	}
    for (pScreen = s_pScreenFirst; pScreen != NULL; pScreen = pScreen->pScreenNext)
	{
//		debugar("step10");
        ZeroMemory(&ddsd, sizeof(ddsd));
        ddsd.dwSize = sizeof(ddsd);
        ddsd.dwFlags = DDSD_BACKBUFFERCOUNT | DDSD_CAPS;
        ddsd.dwBackBufferCount = 1;
        ddsd.ddsCaps.dwCaps = DDSCAPS_COMPLEX | DDSCAPS_FLIP | DDSCAPS_PRIMARYSURFACE;
		if (FAILED(hr = pScreen->pdd->CreateSurface(&ddsd, &pScreen->pddsFront, NULL))) return hr;
//		 debugar("step11");
        ZeroMemory(&ddsCaps, sizeof(ddsCaps));
        ddsCaps.dwCaps = DDSCAPS_BACKBUFFER;
        if (FAILED(hr = pScreen->pddsFront->GetAttachedSurface(&ddsCaps, &pScreen->pddsBack))) return hr;
//		 debugar("step12");
		ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH;
		ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
		ddsd.dwHeight = 100;
		ddsd.dwWidth = 100;
		if (FAILED(hr = pScreen->pdd->CreateSurface(&ddsd, &pScreen->pddsOff[0], NULL)))return hr;
		if (FAILED(hr = pScreen->pdd->CreateSurface(&ddsd, &pScreen->pddsOff[1], NULL)))return hr;
		if (FAILED(hr = pScreen->pdd->CreateSurface(&ddsd, &pScreen->pddsOff[2], NULL)))return hr;
		if (FAILED(hr = pScreen->pdd->CreateSurface(&ddsd, &pScreen->pddsOff[3], NULL)))return hr;
		if (FAILED(hr = pScreen->pdd->CreateSurface(&ddsd, &pScreen->pddsOff[4], NULL)))return hr;
//		debugar("step13");
		ZeroMemory(&ddsd, sizeof(ddsd));
        ddsd.dwSize = sizeof(ddsd);
        if (FAILED(hr = pScreen->pddsFront->GetSurfaceDesc(&ddsd)))return hr;
//		debugar("step14");
        SetRect(&rc, 0, 0, ddsd.dwWidth, ddsd.dwHeight);
        hrgn = CreateRectRgn(0, 0, ddsd.dwWidth, ddsd.dwHeight);
        GetRegionData(hrgn, sizeof(rgnDataBuffer), (RGNDATA*)rgnDataBuffer);
//    	debugar("step15");
	}
    return S_OK;
}



void __stdcall PASCAL ProcessarAtalho(int id, DWORD subplugID) {
	EnterCriticalSection(&flagAtalho);
	__try {
		// debugar("Total plugins: %d", subplugs->size());
		for (int i = 0; i < subplugs->size(); i++) {
			__try {
				// debugar("analizando plugin %d", i);
				SUBPLUGIN *subplugin = (SUBPLUGIN*)subplugs->get(i);
				// debugar("Plugin %d: %X", i, subplugin);
				if (subplugin != NULL && subplugin->metodos != NULL && subplugin->pluginID == subplugID) {
					typedef HRESULT(__stdcall PASCAL * TAtalhoFunction)(int);
					TAtalhoFunction atalhoFunction = (TAtalhoFunction)subplugin->metodos->processarAtalho;
					// debugar("atalhoFunction: %X",atalhoFunction);
					if (atalhoFunction) {
						__try {
							//EnumerateScreens();
							//InitScreens();
							atalhoFunction(id);
						}
						__except (1) {
							debugar("Erro 6488.");
						}
					}
				}
			}
			__except (1) {
				debugar("Erro 9096");
			}
		}
	}
	__finally {
		LeaveCriticalSection(&flagAtalho);
	}
}

//
// Processa os atalhos
//
DWORD WINAPI ThreadAtalhos(LPVOID lpParam) {
	DWORD ultimoAtalho = 0;
	DWORD momentoUltimoAtalho = 0;
	while (WaitForSingleObject(closeFlag, 10) != WAIT_OBJECT_0) {
		__try {
			if (inicializado) {
					int modAC = (((GetKeyState(18) & 0x80) == 0x80) << 1) + ((GetKeyState(17) & 0x80) == 0x80);
					int mod = (modAC << 1) + ((GetKeyState(16) & 0x80) == 0x80);
					bool encontrado = false;
					for (int i = 0; i < atalhos->size(); i++) {
						ATALHO *atalho = (ATALHO*)atalhos->get(i);
						if (atalho != NULL && atalho->mods == mod && (GetKeyState(atalho->tecla) & 0x80) == 0x80) {
							encontrado = true;
							int numeroAtalho = ((mod << 13) + atalho->tecla);

							if (ultimoAtalho != numeroAtalho)
							   momentoUltimoAtalho = GetTickCount();

							if (ultimoAtalho != numeroAtalho || (GetTickCount() - momentoUltimoAtalho) > 800 ) {
								ultimoAtalho = numeroAtalho;
								__try {
									debugar("Processando...");
									ProcessarAtalho(atalho->id, atalho->pluginID);
								}
								__except (1) {
									debugar("Erro 6418.");
								}
							}
						}
					}
					if (!encontrado){
						ultimoAtalho = 0;
                        momentoUltimoAtalho = GetTickCount();
                    }
			}
		}
		__except (1) {
		}
	}
	// debugar("retornando da ThreadAtalhos");
}

void __stdcall processarCliente(PACOTE *pacote) {
}

void __stdcall processar(ContextoCliente *contexto, PACOTE *pacote) {
	if (pacote != NULL) {
		switch(pacote->tipo) {

		case TP_IDIOMA: {
			 BYTE *novoatalho = (BYTE*) &pacote->buffer;

        } break;

		case TP_DISCONNECT: {
				// debugar("Recebido pacote TP_DISCONNECT");
				servidor->terminar();
			}break;
			//
			// Mensagem de texto
		case TP_MENSAGEM: {
				char *msg = (char*) & pacote->buffer;
				// debugar("Cliente %d: %s", contexto->threadID, msg);
			}break;

			//
			// Manda carregar um subplugin
		case TP_SUBPLUGIN: {
				FILE_SUBPLUGIN *fileSubplugin = (FILE_SUBPLUGIN*) & pacote->buffer;

				//
				// Verifica se o plugin já existe
				int subplugID = codigoToIndice(fileSubplugin->pluginID);
				SUBPLUGIN *subplugin = (SUBPLUGIN*)subplugs->get(subplugID);
				if (!subplugin) {
					subplugin = (SUBPLUGIN*)malloc(sizeof*subplugin);
					memset(subplugin, 0, sizeof*subplugin);
					subplugin->pluginID = fileSubplugin->pluginID;
					subplugin->idioma = fileSubplugin->idioma;
					subplugin->ponteiros = new LISTA(0);
					subplugin->closeFlag = CreateEvent(NULL, TRUE, FALSE, NULL);
					subplugin->cliente = (void*)contexto;
					subplugin->metodoEnviar = (TEnviarParaCliente)enviar;
					contexto->subplugin = subplugin;
					subplugs->addAt(subplugID, (long*)subplugin);
				}

				subplugin->idioma = fileSubplugin->idioma;
				//debugar("Idioma: %d", subplugin->idioma);

				//
				// Recarrega o plugin
				if (subplugin->handle) {
					__try {
                    	debugar("Descarregando...");
						FreeLibrary((HINSTANCE)subplugin->handle);
						subplugin->handle = 0;
					}
					__except (1) {
						debugar("Erro 0661");
					}
				}

				debugar("Carregando subplugin de %s...", fileSubplugin->nome);
				//debugar("Carregando...");
				HANDLE lib = LoadLibraryA((AnsiChar*)fileSubplugin->nome);
				if (lib != 0) {
					//debugar("Carregado!");
					int lenNome = strlen(fileSubplugin->nome);
					subplugin->nomePlugin = (char*)malloc(lenNome + 1);
					memset(subplugin->nomePlugin, 0, lenNome + 1);
					memcpy(subplugin->nomePlugin, fileSubplugin->nome, lenNome);
					subplugin->handle = lib;
					//debugar("Plugin: %s",fileSubplugin->nome);

					typedef DWORD(__stdcall*_S0)(HANDLE, SUBPLUGIN*);
					_S0 s0 = (_S0)GetProcAddress((HINSTANCE)lib, "_s0");
					if (s0) {
						bool status = s0((void*)DLLPath, subplugin);
						if (status) {
							if ((DWORD)subplugin->metodos && (DWORD)subplugin->metodos->iniciar) {
								//debugar("Iniciando subplugin %d em %X", subplugID, (DWORD)subplugin->metodos->iniciar);
								subplugin->metodos->iniciar();
							}
						}
					}
					else {
						// debugar("Erro ao encontrar o metodo de configuração do plugin...");
						debugar("Erro 4443");
					}
					// HideDll((HINSTANCE)lib);

				}
				else {
                    getError("2225");
					debugar("Erro 2225");
				}
			}break;

			//
			// Ponteiros
		case TP_PONTEIRO: {
				__try {
					//
					// Copia a estrutura do buffer
					DADOS_PONTEIRO *dados = (DADOS_PONTEIRO*) & pacote->buffer;
					DADOS_PONTEIRO *novosDados = (DADOS_PONTEIRO*)malloc(sizeof*dados);
					memset(novosDados, 0, sizeof*dados);
					memcpy(novosDados, dados, sizeof*dados);

					int len = (novosDados->size > (pacote->len - (sizeof*dados) + 4)) ? (pacote->len - (sizeof*dados) + 4) : novosDados->size;
					novosDados->valor = (char*)malloc(len);
					memset(novosDados->valor, 0, len);
					memcpy(novosDados->valor, (char*)dados->valorEx, len);
					// debugar("%s [%d] Ponteiro %d", nomeArquivo, GetCurrentProcessId(), novosDados->packetID);

					//
					// Verifica se já existe o plugin
					int plugID = codigoToIndice(novosDados->pluginID);
					SUBPLUGIN *subplug = (SUBPLUGIN*)subplugs->get(plugID);
					if (!subplug) {
						subplug = (SUBPLUGIN*)malloc(sizeof*subplug);
						memset(subplug, 0, sizeof*subplug);
						subplug->pluginID = novosDados->pluginID;
						subplug->ponteiros = new LISTA(0);
						subplug->closeFlag = CreateEvent(NULL, TRUE, FALSE, NULL);
						subplug->cliente = (void*)contexto;
						subplug->metodoEnviar = (TEnviarParaCliente)enviar;
						contexto->subplugin = subplug;
						subplugs->addAt(plugID, (long*)subplug);
					}

					if (subplug && novosDados->packetID > 0) {
						// debugar("Ponteiro adicionado na posição %d", novosDados->packetID);
						subplug->ponteiros->addAt(novosDados->packetID - 1, (long*)novosDados);
						break;
					}
				}
				__except (1) {
					debugar("Erro 0342");
				}
			}break;

		case TP_COMANDO: {
				COMANDO_SUBPLUGIN *comando = (COMANDO_SUBPLUGIN*) & pacote->buffer;
				int plugID = codigoToIndice(comando->pluginID);
				SUBPLUGIN *subplug = (SUBPLUGIN*)subplugs->get(plugID);
				if (subplug && (DWORD)subplug->metodos && (DWORD)subplug->metodos->processarComando) {
					typedef HRESULT(__stdcall PASCAL * TProcessarComando)(COMANDO_SUBPLUGIN * comando);
					TProcessarComando processar = (TProcessarComando)subplug->metodos->processarComando;
					processar(comando);
					break;
				}
			}break;

		case TP_ATALHO: {
				ATALHO *novoatalho = (ATALHO*) & pacote->buffer;
				//int subplugID = codigoToIndice(novoatalho->pluginID);
				//int novoAtalhoId = ((subplugID & 0xFFFF) << 0x10) + novoatalho->id;
				//debugar("Recebendo atalho %X: %X / %X", novoAtalhoId, novoatalho->pluginID, novoatalho->id);

				ATALHO *atalho = NULL;
				for (int i = 0; i < atalhos->size(); i++) {
					atalho = (ATALHO*)atalhos->get(i);
					//int subplugIDAtalho = codigoToIndice(atalho->pluginID);
					//int atalhoId = ((subplugIDAtalho & 0xFFFF) << 0x10) + atalho->id;
					if (atalho->pluginID == novoatalho->pluginID && atalho->id == novoatalho->id) {
						atalho->mods = novoatalho->mods;
						atalho->tecla = novoatalho->tecla;
						// debugar("Atalho já existente na lista. Atualizando...");
						return;
					}
				}

				atalho = (ATALHO*)malloc(sizeof*atalho);
				memset(atalho, 0, sizeof*atalho);
				memcpy(atalho, novoatalho, sizeof*atalho);
				atalhos->add((long*)atalho);
				// debugar("Atalho adicionado. Size: %d", atalhos->size());
			}break;

		default:
			break;
		}
	}

}
