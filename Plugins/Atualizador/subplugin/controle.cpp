// ---------------------------------------------------------------------------

#pragma hdrstop

#include "controle.h"
#include "baseSP.h"
#include "..\estrutura.h"
#include "..\..\debugUtils.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)

HANDLE *threads = NULL;
ESTRUTURA_PLG *estruturaBase = new ESTRUTURA_PLG();
int porcentagem = 0;
DWORD inicioAmostragem = 0;

//
// Atualiza os valores do plugin lendo da memoria
//
void __stdcall PASCAL AtualizarValores() {

}

void mostrarMenu() {
	inicioAmostragem = GetTickCount();
	/*if(aimbot->tempoMenu >= 0){
		aimbot->mostrarMenu = true;
		aimbot->inicioAmostraMenu = GetTickCount();
	} */
}

void __stdcall PASCAL ProcessarComando(COMANDO_SUBPLUGIN *comando) {
	__try {
		// debugar("Processando comando no subpl de %s", Hack_Name);
		if (comando != NULL) {
			switch(comando->tipo) {
				//
				// Ligar / Desligar
			case TP_LIGAR: {
            		debugar("Atualizador acionado!");
					if (comando->valor == NULL)
						estruturaBase->ligado = !estruturaBase->ligado;
					else
						estruturaBase->ligado = (bool)comando->valor[0];

					if(estruturaBase->ligado)
						HardCodeMemoria();

					mostrarMenu();

					estruturaBase->ligado = false;
					setarConfiguracao(TP_LIGAR, (char*) & estruturaBase->ligado, 1);
				}break;
			}
		}
	}
	__except (1) {
		debugar("Erro 7271");
	}
}

DWORD WINAPI ThreadProcessamento(LPVOID lpParam) {
	while (WaitForSingleObject(base->closeFlag, 50) != WAIT_OBJECT_0) {
		__try {
			//if (estruturaBase->tempoMenu < 0 || (estruturaBase->mostrarMenu && estruturaBase->tempoMenu != 0 && (GetTickCount() - aimbot->inicioAmostraMenu) > aimbot->tempoMenu * 1000))
			//	estruturaBase->mostrarMenu = false;

			if (estruturaBase->ligado) {
			
			}
		}
		__except (1) {
			debugar("Erro no processamento! [2]");
		}
	}
	debugar("exiting...");
	return 0;
	// //VMProtectEnd();
}

void DesenharStatus(HDC hdc) {
	//VMProtectBegin("status");
	if( (GetTickCount() - 5000) >= inicioAmostragem)
		return;

	__try {
		bool semDC = (!hdc || hdc == 0);
		if (semDC) {
			hdc = CreateDC("DISPLAY", "", "", NULL);
		}

		if (!hdc)
			return;

		__try {
			int bk = GetBkMode(hdc);
			SetBkMode(hdc, TRANSPARENT);

			HFONT Font;
			HFONT oldFont;
			Font = CreateFont(12, 7, 0, 0, 4, 0, FALSE, FALSE, 0, 0, 0, DEFAULT_QUALITY, 0, "Arial");
			oldFont = (HFONT)SelectObject(hdc, Font);

			DWORD cor;
			cor = GetTextColor(hdc);

			__try {
				__try {
					const sx = 10;
					const sy = 10;
					SetTextColor(hdc, 0xFFFFFF);
					TextOutA(hdc, sx + 10, sy + 30, "Atualizador: ", 8);
					SetTextColor(hdc, 0x00FF00);
					TextOutA(hdc, sx + 60, sy + 30, (estruturaBase->ligado ? "ON " : "OFF"), 3);
				}
				__except (1) {
				}
			}
			__finally {
				SelectObject(hdc, oldFont);
				SetTextColor(hdc, cor);

				DeleteObject(oldFont);
				DeleteObject(Font);

				SetBkMode(hdc, bk);
			}
		}
		__finally {
			if (semDC)
				DeleteDC(hdc);
		}
	}__except (1) {
		debugar("Erro 1212");
	}
	//VMProtectEnd();
}

//
// Escreve na tela
//
bool desenhando = false;

void __stdcall PASCAL Desenhar(HDC dc, char tipoDesenho) {
	if (!desenhando)
		desenhando = true;
	else
		return;

	__try {

		if (tipoDesenho == TD_DDRAW /*&& (aimbot->mostrarMenu || aimbot->movendoMenu)*/ ) {
			DesenharStatus(dc);
		}
		/*
		if ((bool)tipoDesenho == aimbot->modoGDI) {
			__try {
				if (aimbot->renderizar) {
					aimbot->camera = aimbot->GetCameraPos();
					if (dc == 0)
						aimbot->fillPoints();
					else
						aimbot->fillPoints(dc);
				}
			}__except (1) {
				debugar("Erro 0148");
			}
		}       */
	}
	__finally {
		desenhando = false;
	}

}

void __fastcall setarConfiguracao(int tipo, char* buffer, int len) {
	__try {
		COMANDO_SUBPLUGIN *comando = (COMANDO_SUBPLUGIN*)malloc(8 + len);
		__try {
			memset(comando, 0, 8 + len);
			comando->tipo = tipo;
			memcpy(&comando->valor, buffer, len);
			if ((DWORD)base->metodoEnviar && base->cliente)
				base->metodoEnviar(*(SOCKET*)base->cliente, TP_COMANDO, (char*)comando, 8 + len);
		}
		__finally {
			free(comando);
		}
	}
	__except (1) {
		debugar("Erro 1182");
	}
}

void __stdcall PASCAL ProcessarAtalho(int id) {
	__try {
		//debugar("ativado atalho %d", id);
		switch(id) {
		case 1: {
				estruturaBase->ligado = !estruturaBase->ligado;
				setarConfiguracao(TP_LIGAR, (char*) & estruturaBase->ligado, 1);
				mostrarMenu();
			}break;
		default:
			break;
		}
	}
	__except (1) {
		debugar("Erro 7272");
	}
}

//
// Atualiza os endereços de acordo com os ponteiros
//
static HMODULE dinput = 0;

void __stdcall PASCAL AtualizarEnderecos() {

}

DWORD EscanearMemoria(DWORD inicioModulo, DWORD fimModulo, BYTE ignorar, PBYTE arrayBytes, DWORD sequenciaLength, int lpOffset, PDWORD lpBuffer, DWORD quantidadeCopiar, BYTE lpFlag, BYTE indice){
	DWORD resultado = 0;
	WORD excecao = (WORD)ignorar;

	if(inicioModulo == fimModulo || !arrayBytes || !sequenciaLength || !lpBuffer){
		return resultado;
	}

	int i = 0, encontrados = 0;
	PBYTE memoryPos = (PBYTE)inicioModulo;
    bool encontrado = false;

	INT64 quantidadeScan = 0;
	INT64 totalScan = (DWORD)(fimModulo - inicioModulo);

	//debugar("Total: %X - Inicio: %X - Fim: %X", (DWORD)totalScan, (DWORD)inicioModulo, (DWORD)fimModulo);
	debugarArray(arrayBytes, sequenciaLength);

	while((DWORD)memoryPos <= (DWORD)fimModulo){
		quantidadeScan++;

		if(estruturaBase->ligado){
			if((quantidadeScan % 0x10000) == 0){
				porcentagem = (int)( ((INT64)quantidadeScan*100) / ((INT64)totalScan) );
				//debugar("[%X / %X] Porcentagem: %d", memoryPos, fimModulo, porcentagem);
			}

			for(int i = 0; i < sequenciaLength; i++){
				 if(/*!IsBadReadPtr(memoryPos,1) ||*/ (BYTE)arrayBytes[i] != (BYTE)memoryPos[i] ){
                 	if(ignorar == 0 || (BYTE)arrayBytes[i] != (BYTE)ignorar)
					 	break;
				 }
				 if(i == (sequenciaLength - 1)){
					 encontrados++;
					 if(encontrados >= indice){
                     	debugar("found on index %d", encontrados);
						 encontrado = true;
						 break;
					 }else{
						 break;
					 }
				 }
			}

			if(encontrado){
				break;
			}else{
				memoryPos++;
			}
		}else{
            return 0;
		}
	}
	//debugar("Original:");
	//debugarArray((PBYTE)0x480838, sequenciaLength);
	//debugar("Finalizado no endereço %X (%d)", memoryPos, encontrado);
	porcentagem = 100;

	if(encontrado){
		debugar("Encontrado em %X / O: %d / S: %d / I: %d", memoryPos, lpOffset, quantidadeCopiar, indice);
		// Aloca o espaço para copiar os bytes da memoria
		memset(lpBuffer, 0, quantidadeCopiar);
		switch(lpFlag){
			case 0:
				// copia para o buffer o conteúdo no endereço escaneado
				memcpy(lpBuffer, (PVOID)(((DWORD)memoryPos + (DWORD)lpOffset) & 0xFFFFFFFF), quantidadeCopiar);
				resultado = (DWORD)lpBuffer;
			break;
			case 1:
				// copia para o buffer o endereço escaneado
				lpBuffer[0] = (((DWORD)memoryPos + lpOffset) );
				resultado = (DWORD)lpBuffer;
			break;
			case 2:
				// Copia para o buffer a posição de memoria acrescida do offset
				// mais o valor lido naquela posição
				// Pega o deslocamento
				memcpy(lpBuffer, (memoryPos + lpOffset), quantidadeCopiar);
				// Calcula o deslocamento em cima do endereço
				lpBuffer[0] = ((DWORD)memoryPos + lpOffset + quantidadeCopiar + lpBuffer[0]);
				resultado = (DWORD)lpBuffer;
			break;
			default: break;
		}
	}
	return resultado;
}

bool escaneando = false;
void __stdcall HardCodeMemoria(){
	if(escaneando)
		return;
	else
		escaneando = true;

	__try{
		debugar("Iniciando hardcode...");
		DWORD moduloInicio = (DWORD)GetModuleHandleA(0);
		DWORD moduloFim = (moduloInicio + 0x200000);

		DWORD *encontrados = new DWORD[MinimumPackets];
		memset(encontrados, 0, 4*MinimumPackets);

		int quantidadePonteiros = base->ponteiros->size();
		for (int i = 0; i < quantidadePonteiros; i++) {
			DADOS_PONTEIRO *ponteiro = (DADOS_PONTEIRO*)base->ponteiros->get(i);
			BYTE ignorar = (ponteiro->ponteiro == 0)?0x90:(BYTE)ponteiro->ponteiro;
			DWORD quantidadeCopiar = (ponteiro->size & 0xFF);
			DWORD sequenciaLength = ((ponteiro->size & 0xFF00) >> 0x08);
			//debugar("Inicio: %X", moduloInicio);
			//debugar("Fim: %X", moduloFim);
			//debugar("Ignorar: %X", ignorar);
			//debugar("Length: %d", sequenciaLength);
			//debugar("Offset: %d", ponteiro->offset);
			//debugar("Qnde. Copiar: %d", quantidadeCopiar);
			//debugar("Inicio: %X - Fim: %X", moduloInicio, moduloFim);
            debugar("----------- Escaneando [%d] --------", i);
			EscanearMemoria(moduloInicio, moduloFim, ignorar, ponteiro->valor, sequenciaLength, ponteiro->offset, &encontrados[i], quantidadeCopiar, 0, ponteiro->range);
			debugar("Resultado[%d]: %X", i, encontrados[i]);
			debugar("------------------------------------");
		}
		setarConfiguracao(TP_RESULTADO, (char*) &encontrados[0], 4*MinimumPackets);
	}__finally{
		debugar("Hardcode concluído!");
		escaneando = false;
    }
}

void __stdcall PASCAL Iniciar() {
	if (!versaoOS) {
		versaoOS = new OS_INFO;
		*versaoOS = GetOS();
	}

	// debugar("Iniciando subpl de %s", Hack_Name);
	inicioAmostragem = GetTickCount();
	threads = new HANDLE[3];
	threads[0] = CreateThread(NULL, 0, ThreadProcessamento, NULL, 0, NULL);
	SetThreadContext(threads[0], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);

}
