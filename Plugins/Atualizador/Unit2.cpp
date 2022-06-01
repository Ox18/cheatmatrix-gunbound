#include <vcl.h>
#include <stdio.h>
#include <math.h>
#pragma hdrstop

#include "Unit2.h"
#include "pluginInfo.h"
#include "IniFiles.hpp"
#include "Sharing.h"
#include "mathutils.h"
#include "pluginUtils.h"
#include "drawUtils.h"
#include "estrutura.h"

#pragma package(smart_init)
#pragma link "Spin"
#pragma resource "*.dfm"

#define DEBUGAR

TForm2 *Form2;
TIniFile *iniCFG = NULL;
HANDLE threadConexao = NULL;
LISTA *atalhos = new LISTA(0);

void delay(DWORD ms) {
	DWORD ct = 0;
	while (ct < ms) {
		Sleep(5);
		Application->ProcessMessages();
		ct += 5;
	}
}

void traduzPlugin() {
}

void enviarAtalho(ATALHO *atalho) {
	if (atalho) {
		atalho->pluginID = Hack_Code;
		cliente->enviar(TP_ATALHO, (char*)atalho, sizeof*atalho);
	}
}

DWORD WINAPI ThreadConectar(LPVOID lpParam) {
	////VMProtectBegin("conectar");
	while (WaitForSingleObject(cliente->closeFlag, 800) != WAIT_OBJECT_0) {
		__try {
			if (!cliente->conectado && cmxConfig != NULL) {
				int portaServidor = cmxConfig->ReadInteger("configs", "ses", 0);
				// (porta != NULL)?*porta:1313;
				portaServidor = (int)((portaServidor - 37) / 9269);
				if (portaServidor >= 1313 && portaServidor <= 33999) {
					cliente->conectar(portaServidor);
					if (cliente->conectado) {
						// debugar("start [%d]", portaServidor + 10);
						__try {
							//
							// Após conectar, precisamos enviar os pacotes
							for (int i = 0; i < GetMinimumPackets(); i++) {
								DADOS_PONTEIRO *dados;
								int len = (pacotes[i].Size <= 4) ? (sizeof*dados) : ((sizeof*dados) + pacotes[i].Size - 4);
								dados = (DADOS_PONTEIRO*)malloc(len);
								memset(dados, 0, len);
								__try {
									dados->pluginID = Hack_Code;
									memcpy(&dados->packetID, &pacotes[i], sizeof pacotes[i]);
									memcpy(dados->valorEx, pacotes[i].Valor, pacotes[i].Size);
									cliente->enviar(TP_PONTEIRO, (char*)dados, len);
								}
								__finally {
									free(dados);
								}
							}

							Sleep(1500);

							for (int i = 0; i < atalhos->size(); i++) {
								__try {
									// debugar("Enviando atalho %d: %d", i, (atalhos->get(i)) ? ((ATALHO*)atalhos->get(i))->tecla : 0);
									ATALHO *atalho = (ATALHO*)atalhos->get(i);
									enviarAtalho(atalho);
								}
								__except (1) {
									debugar("Erro 1111");
								}
							}
						}
						__except (1) {
							debugar("Erro 9342");
						}

						//
						// Carregamos os subplugins no jogo
						FILE_SUBPLUGIN subplugin;
						subplugin.pluginID = GetPluginID();
					}
				}
				else {
					Sleep(2000);
				}
			}
		}
		__except (1) {
			debugar("Erro 7777");
		}
	}
	////VMProtectEnd();
}

void __fastcall setarConfiguracao(int tipo, char* buffer, int len) {
	COMANDO_SUBPLUGIN *comando = (COMANDO_SUBPLUGIN*)malloc(8 + len);
	__try {
		memset(comando, 0, 8 + len);
		comando->pluginID = Hack_Code;
		comando->tipo = tipo;
		memcpy(&comando->valor[0], buffer, len);
		cliente->enviar(TP_COMANDO, (char*)comando, 8 + len);
	}
	__finally {
		free(comando);
	}
}

bool __fastcall gravarConfiguracaoCheckBox(TObject *sender) {
	AnsiString arquivo = arquivoConfiguracoes();
	TIniFile *iniCFG = new TIniFile(arquivo);
	bool resultado = false;
	__try {
		__try {
			TCheckBox* check = dynamic_cast<TCheckBox*>(sender);
			if (check) {
				resultado = check->Checked;
				iniCFG->WriteBool("configs", check->Name, check->Checked);
			}
		}
		__except (1) {
		}
	}
	__finally {
		iniCFG->Free();
	}
	return resultado;
}

bool __fastcall gravarConfiguracao(String titulo, String valor) {
	AnsiString arquivo = arquivoConfiguracoes();
	TIniFile *iniCFG = new TIniFile(arquivo);
	bool resultado = false;
	__try {
		__try {
			iniCFG->WriteString("configs", titulo, valor);
			resultado = true;
		}
		__except (1) {
		}
	}
	__finally {
		iniCFG->Free();
	}
	return resultado;
}

String __fastcall lerConfiguracao(String titulo) {
	AnsiString arquivo = arquivoConfiguracoes();
	TIniFile *iniCFG = new TIniFile(arquivo);
	__try {
		__try {
			return iniCFG->ReadString("configs", titulo, "");
		}
		__except (1) {
		}
	}
	__finally {
		iniCFG->Free();
	}
	return "";
}

typedef struct _opcoesTiro {
	bool tiroEspecial;
	bool marcarSS;
} TOpcoesTiro;

TStrings *logList = new TStringList();
TStrings *sqlList = new TStringList();

void __fastcall gerarLogResultado(DWORD *encontrados) {

	DWORD ponteiroPrincipal = encontrados[0];
	DWORD ponteiroVento = encontrados[10];

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[12], 8) + "' WHERE id = 1;");

	logList->Add("---- Mobile ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[12], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[1], 8) + "' WHERE id = 6;");

	logList->Add("---- Location X ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[1], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[2], 8) + "' WHERE id = 17;");

	logList->Add("---- Direita-Esquerda ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[2], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[3], 8) + "' WHERE id = 9;");

	logList->Add("---- Nome ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[3], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[4], 8) + "' WHERE id = 16;");

	logList->Add("---- Time ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[4], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[5], 8) + "' WHERE id = 2;");

	logList->Add("---- Camera X ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[5], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[5] + 4, 8) + "' WHERE id = 3;");

	logList->Add("---- Camera Y ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[5] + 4, 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[6], 8) + "' WHERE id = 10;");

	logList->Add("---- Power ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[6], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '0', offset = '" + IntToHex((int)encontrados[7], 8) + "' WHERE id = 5;");

	logList->Add("---- Index ----");
	logList->Add("Ponteiro: " + IntToHex((int)0, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[7], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '0', offset = '" + IntToHex((int)encontrados[8], 8) + "' WHERE id = 7;");

	logList->Add("---- InGame ----");
	logList->Add("Ponteiro: " + IntToHex((int)0, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[8], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroPrincipal, 8) + "', offset = '" + IntToHex((int)encontrados[9], 8) + "' WHERE id = 12;");

	logList->Add("---- Status ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroPrincipal, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[9], 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroVento, 8) + "', offset = '" + IntToHex((int)encontrados[11] + 40, 8) + "' WHERE id = 13;");

	logList->Add("---- Força Vento ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroVento, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[11] + 40, 8));
	logList->Add(" ");

	sqlList->Add("UPDATE items_memoria SET ponteiro = '" + IntToHex((int)ponteiroVento, 8) + "', offset = '" + IntToHex((int)encontrados[11] + 41, 8) + "' WHERE id = 14;");

	logList->Add("---- Angulo Vento ----");
	logList->Add("Ponteiro: " + IntToHex((int)ponteiroVento, 8));
	logList->Add("Offset: " + IntToHex((int)encontrados[11] + 41, 8));
	logList->Add(" ");

	// listaLog->Lines = logList;
	// Memo5->Lines = sqlList;
	// Notebook1->Pages = logList;
	// listaLog->Lines = logList;

}

void __stdcall PASCAL ProcessarComando(COMANDO_SUBPLUGIN *comando) {
	__try {
		// debugar("Processando comando no plugin %s", Hack_Name);
		if (comando != NULL) {
			switch (comando->tipo) {
				//
				// Ligar / Desligar
			case TP_LIGAR: {
					if (comando->valor != NULL) {
						if ((bool)comando->valor[0])
							Form2->Ligado->Checked = true;
						else
							Form2->Desligado->Checked = true;
					}
				} break;
			case TP_RESULTADO: {
					debugar("Recebendo resultado...");
					if (comando->valor != NULL) {
						DWORD *encontrados = new DWORD[MinimumPackets];
						memset(encontrados, 0, 4 * MinimumPackets);
						memcpy(&encontrados[0], &comando->valor[0], MinimumPackets * 4);
						debugar("MinimumPackets: %d", MinimumPackets);
						gerarLogResultado(&encontrados[0]);
						// Form2->Memo5->Lines->Add("SkyW4rrior");

					}
				} break;
			}
		}
	}
	__except (1) {
		debugar("Erro 7271");
	}
}

void __stdcall processar(PACOTE *pacote) {
	if (pacote != NULL) {
		switch (pacote->tipo) {
			//
			// Mensagem de texto
		case TP_MENSAGEM: {
				__try {
					char *msg = (char*)&pacote->buffer;
					debugar("Servidor: %s", msg);
				}
				__except (1) {
					debugar("Erro 4748!");
				}
			} break;

		case TP_COMANDO: {
				COMANDO_SUBPLUGIN *comando = (COMANDO_SUBPLUGIN*)&pacote->buffer;
				ProcessarComando(comando);
			} break;

			//
			// Ao receber uma requisição de subplugin do servidor, envia o nome
			// do subplugin a ser carregado
		case TP_SUBPLUGIN: {
				// debugar("[C] Recebida solicitação de subplugin");
				__try {
					AnsiString diretorio = getDiretorioSubplugins();
					if (FileExists(ExtractFilePath(ParamStr(0)) + "\\plugins\\_")) {
						diretorio = ExtractFilePath(ParamStr(0)) + "plugins\\";
					}
					// getDiretorioSubplugins();
					AnsiString nomePlugin(nomeSubplugin);
					/* "Plugins\\subplugins\\spi.dll"; */
					diretorio = diretorio + nomePlugin;
					// debugar("dir: %s", diretorio.t_str());
					FILE_SUBPLUGIN *fileSubplugin = (FILE_SUBPLUGIN*)malloc(sizeof(*fileSubplugin) + diretorio.Length() + 1);
					__try {
						memset(fileSubplugin, 0, sizeof(*fileSubplugin) + diretorio.Length() + 1);
						fileSubplugin->pluginID = Hack_Code;
						memcpy(&fileSubplugin->nome, diretorio.c_str(), diretorio.Length());
						// debugar("enviando nome %s", diretorio.t_str());
						cliente->enviar(TP_SUBPLUGIN, (char*)fileSubplugin, sizeof(*fileSubplugin) + diretorio.Length());
						Form2->LigadoClick(Form2->Ligado);
					}
					__finally {
						free(fileSubplugin);
					}
				}
				__except (1) {
					debugar("Erro 4745!");
				}
			}
		default:
			break;
		}
	}
}

// -------------------------------------------------------------------
// Pega o nome do arquivo de configurações
// -------------------------------------------------------------------
AnsiString __fastcall arquivoConfiguracoes() {
	// VMProtectBegin("fileConfigs");
	TStringList *vazio = new TStringList();
	AnsiString arquivo = GetCurrentDir() + "\\" + Hack_Name + ".ini";
	if (!FileExists((AnsiString)arquivo)) {
		vazio->Text = "";
		vazio->SaveToFile((AnsiString)arquivo);
	}
	return arquivo;
	// VMProtectEnd();
}

AnsiString __fastcall arquivoConfigCMX() {
	// VMProtectBegin("configCMX");
	TStringList *vazio = new TStringList();
	AnsiString arquivo = GetCurrentDir() + "\\configs.ini";
	if (!FileExists((AnsiString)arquivo)) {
		vazio->Text = "";
		vazio->SaveToFile((AnsiString)arquivo);
	}
	return arquivo;
	// VMProtectEnd();
}

// ---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner) : TForm(Owner) {
}

//
// Função que retorna a string com o status do hack
//
AnsiString GetStatus() {
	// VMProtectBegin("getStatus");
	return (Form2->Ligado->Checked ? "ON" : "OFF");
	// VMProtectEnd();
}

// Timer de controle e amostragem do status do painel à direita
// ------------------------------------------------------------------
bool StatusTimerFlag = false;

void __fastcall TForm2::StatusTimerTimer(TObject *Sender) {
	// Mostra o nome do hack no painel
	sHack->Caption = Hack_Label;
}

// ------------------------------------------------------------------
// Formata um float
// ------------------------------------------------------------------
AnsiString formataFloat(double valor) {
	return FloatToStrF(valor, ffFixed, 8, 4);
}

AnsiString formataFloatEx(double valor) {
	AnsiString resultado = FloatToStrF(valor, ffFixed, 8, 4);
	resultado = StringReplace(resultado, ",", ".", TReplaceFlags() << rfReplaceAll);
	return resultado;
}

float potencia(int base, int expoente) {
	int temp = 1;

	if (expoente == 0)
		return 1;

	for (int x = 1; x <= expoente; x++) {
		temp *= base;
	}
	return (float)temp;
}

AnsiString SystemDir() {
	char *dir = new char[MAX_PATH];
	GetSystemDirectoryA(&dir[0], MAX_PATH);
	AnsiString result = Trim(StrPas(dir));
	if (result[result.Length()] != '\\')
		result = result + "\\";
	return result;
}

void ShortCutToHotKey(TShortCut hotKey, WORD *key, UINT *modifiers) {
	TShiftState shift;

	ShortCutToKey(hotKey, *key, shift);
	*modifiers = 0;

	if (shift.Contains(ssShift))
		* modifiers = (*modifiers | MOD_SHIFT);

	if (shift.Contains(ssAlt))
		* modifiers = (*modifiers | MOD_ALT);

	if (shift.Contains(ssCtrl))
		* modifiers = (*modifiers | MOD_CONTROL);
}

void __fastcall TForm2::FormCreate(TObject *Sender) {
	////VMProtectBegin("createfrm");

	AnsiString arquivo = arquivoConfiguracoes();
	iniCFG = new TIniFile(arquivo);
	cmxConfig = new TIniFile(arquivoConfigCMX());
	InjectorName = ExtractFilePath(ParamStr(0)) + "msvcrt36.dll";

	for (int i = 0; i < ComponentCount; i++) {
		__try {
			TCheckBox* check;
			check = dynamic_cast<TCheckBox*>(Components[i]);
			if (check) {
				bool flag = (check->Tag == 0);
				if (check->Tag < 2)
					check->Checked = iniCFG->ReadBool("configs", check->Name, check->Checked);
			}
		}
		__except (1) {
		}
	}

	TListItem *item = listaAtalhos->Items->Add();
	item->SubItems->Add("");
	item->SubItems->Add("");
	item->SubItems->Add(0);

	__try {
		TStringList *atalhosCFG = new TStringList();
		iniCFG->ReadSection("atalhos", atalhosCFG);
		for (int i = 0; i < atalhosCFG->Count; i++) {
			UnicodeString atalho = iniCFG->ReadString("atalhos", atalhosCFG->Strings[i], 0);
			bool encontrado = false;
			for (int j = 0; j < listaAtalhos->Items->Count; j++) {
				if (listaAtalhos->Items->Item[j]->SubItems->Count >= 2 && listaAtalhos->Items->Item[j]->SubItems->Strings[1] == atalhosCFG->Strings[i]) {
					listaAtalhos->Items->Item[j]->SubItems->Strings[0] = atalho;
					encontrado = true;
					break;
				}
			}
			if (!encontrado) {
				TListItem *item = listaAtalhos->Items->Add();
				item->Caption = "desconhecido";
				item->SubItems->Add(atalho);
				item->SubItems->Add(atalhosCFG->Strings[i]);
				item->SubItems->Add(0);
			}
		}
	}
	__except (1) {
		debugar("Erro 9910");
	}

	int qntAtalhosLista = listaAtalhos->Items->Count;
	for (int j = 1; j < qntAtalhosLista; j++) {
		__try {
			if (listaAtalhos->Items->Item[j]->SubItems->Count >= 2) {
				UnicodeString atalhoString = listaAtalhos->Items->Item[j]->SubItems->Strings[0];
				int atalhoInt = TextToShortCut(atalhoString);
				if (atalhoInt != 0) {
					listaAtalhos->Items->Item[j]->SubItems->Strings[2] = IntToStr(atalhoInt);
					int id = StrToInt(listaAtalhos->Items->Item[j]->SubItems->Strings[1]);

					ATALHO *atalho = (ATALHO*)malloc(sizeof*atalho);
					atalho->id = id;
					atalho->mods = atalhoInt >> 13;
					atalho->tecla = atalhoInt & 0xFFF;
					atalho->pluginID = Hack_Code;

					ATALHO *existente = (ATALHO*)atalhos->get(id);
					if (existente)
						free(existente);
					atalhos->addAt(id, (long*)atalho);
				}
			}
		}
		__except (1) {
			debugar("Erro 0010");
		}
	}

	listaAtalhos->Items->Delete(0);

	threadConexao = CreateThread(NULL, 0, ThreadConectar, NULL, 0, NULL);
	SetThreadContext(threadConexao, (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);

	////VMProtectEnd();
}

/** ************************************************************
 * Evento FORM-DESTROY - desregistra os atalhos
 ************************************************************ */
void __fastcall TForm2::FormDestroy(TObject *Sender) {
	__try {
		WaitForSingleObject(threadConexao, 2000);
		CloseHandle(threadConexao);
	}
	__except (1) {
	}
}

AnsiString CMFormatTime(DWORD valor) {
	// AnsiString sh,sm,ss;
	DWORD h, m, s, l;
	l = valor % 1000;
	s = valor / 1000;

	m = s / 60;
	s = s % 60;

	h = m / 60;
	m = m % 60;

	char res[12];
	sprintf(res, "%d:%d:%d.%d", h, m, s, l);
	return res;
}

bool tornadoTimerFlag = false;
bool ctrlPressionado = false;

void TForm2::WMProcMes(TMessage& Message) {
}

#define TIROS_BIG_FOOT 6

bool desenhando = false;

TStringList *lista = new TStringList();
bool intimer = false;

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

void __fastcall TForm2::timerAutoShotTimer(TObject *Sender) {
	sPS->Caption = (cliente->conectado) ? "ON" : "OFF";
	sPS->Font->Color = (cliente->conectado) ? clGreen : clRed;
}

// ---------------------------------------------------------------------------

void __fastcall TForm2::listaAtalhosSelectItem(TObject *Sender, TListItem *Item, bool Selected) {
	HotKey1->HotKey = TextToShortCut(Item->SubItems->Strings[0]);
	labelItem->Caption = Item->Caption;
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::Button6Click(TObject *Sender) {
	// VMProtectBegin("atalhos");
	if (listaAtalhos->ItemIndex >= 0 && listaAtalhos->ItemIndex < listaAtalhos->Items->Count) {
		// UnregisterHotKey(Handle, StrToInt(listaAtalhos->Items->Item[listaAtalhos->ItemIndex]->SubItems->Strings[2]));
		int atalhoInt = HotKey1->HotKey;
		for (int i = 0; i < listaAtalhos->Items->Count; i++) {
			int itemInt = StrToInt(listaAtalhos->Items->Item[i]->SubItems->Strings[2]);
			if (itemInt == atalhoInt) {
				MessageBox(0, "Este atalho já está em uso!", "Atenção", 0);
				return;
			}
		}

		UnicodeString hotkey = ShortCutToText(HotKey1->HotKey);
		int id = StrToInt(listaAtalhos->Items->Item[listaAtalhos->ItemIndex]->SubItems->Strings[1]);
		listaAtalhos->Items->Item[listaAtalhos->ItemIndex]->SubItems->Strings[0] = hotkey;
		listaAtalhos->Items->Item[listaAtalhos->ItemIndex]->SubItems->Strings[2] = IntToStr(atalhoInt);
		ATALHO *atalho = (ATALHO*)atalhos->get(id);
		if (!atalho) {
			atalho = new ATALHO;
		}

		atalho->id = id;
		atalho->mods = atalhoInt >> 13;
		atalho->tecla = atalhoInt & 0xFFF;
		atalho->pluginID = Hack_Code;
		enviarAtalho(atalho);

		AnsiString arquivo = arquivoConfiguracoes();
		TIniFile *iniCFG = new TIniFile(arquivo);
		__try {
			iniCFG->WriteString("atalhos", listaAtalhos->Items->Item[listaAtalhos->ItemIndex]->SubItems->Strings[1], hotkey);
		}
		__finally {
			iniCFG->Free();
		}
	}
	// VMProtectEnd();
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::ComboKeyClick(TObject *Sender) {
	// VMProtectBegin("mudaAtalho");
	if (ComboKey->ItemIndex > 0 && ComboKey->ItemIndex < ComboKey->Items->Count) {
		UnicodeString key = "";
		String tecla = "";

		int posicaoI = Pos("[", ComboKey->Items->Strings[ComboKey->ItemIndex]);
		int posicaoF = Pos("]", ComboKey->Items->Strings[ComboKey->ItemIndex]);
		if (posicaoI > 0 && posicaoF > 0) {
			tecla = ComboKey->Items->Strings[ComboKey->ItemIndex].SubString(posicaoI + 1, posicaoF - posicaoI - 1);
		}
		else {
			return;
		}

		bool someKey = false;
		int CTRLState = GetKeyState(VK_CONTROL);
		int Shifttate = GetKeyState(VK_SHIFT);
		int AltState = GetKeyState(VK_MENU);
		if (pressionado(CTRLState)) {
			key = "CTRL";
			someKey = true;
		}

		if (pressionado(Shifttate)) {
			if (someKey)
				key += "+";
			someKey = true;
			key += "SHIFT";
		}

		if (pressionado(AltState)) {
			if (someKey)
				key += "+";
			someKey = true;
			key += "ALT";
		}

		if (someKey) {
			key += "+";
			key += tecla;
			HotKey1->HotKey = TextToShortCut(key);
		}
	}
	// VMProtectEnd();
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::HotKey1Enter(TObject *Sender) {
	ComboKey->ItemIndex = 0;
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::Button4Click(TObject *Sender) {
	TSaveDialog *operador = new TSaveDialog(NULL);
	operador->Filter = "*.ini|*.ini";
	operador->DefaultExt = "ini";

	__try {
		String arquivo = arquivoConfiguracoes();
		if (FileExists(arquivo)) {
			if (operador->Execute(Handle)) {
				operador->Files->LoadFromFile(arquivo);
				operador->Files->SaveToFile(operador->FileName);
			}
		}
	}
	__finally {
		operador->Free();
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::Button5Click(TObject *Sender) {
	String arquivo = arquivoConfiguracoes();
	TIniFile *iniCFG = new TIniFile(arquivo);
	TOpenDialog *leitor = new TOpenDialog(NULL);
	leitor->Filter = "*.ini|*.ini";
	leitor->DefaultExt = "ini";

	__try {
		if (leitor->Execute(Handle)) {
			leitor->Files->LoadFromFile(leitor->FileName);
			leitor->Files->SaveToFile(arquivo);

			for (int i = 0; i < ComponentCount; i++) {
				__try {
					TCheckBox* check;
					check = dynamic_cast<TCheckBox*>(Components[i]);
					if (check) {
						bool flag = (check->Tag == 0);
						if (check->Tag < 2)
							if (iniCFG->ReadString("configs", check->Name, "").Length() > 0)
								check->Checked = iniCFG->ReadBool("configs", check->Name, flag);
					}
				}
				__except (1) {
				}
			}
		}
	}
	__finally {
		leitor->Free();
		iniCFG->Free();
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::Button7Click(TObject *Sender) {
	String arquivo = arquivoConfiguracoes();
	if (FileExists(arquivo)) {
		DeleteFileA(arquivo);
	}
	ShowMessage("Você deve reiniciar o CMX para que as modificações tenham efeito.");
}

void __fastcall TForm2::LigadoClick(TObject *Sender) {
	char valor = Ligado->Checked;
	setarConfiguracao(TP_LIGAR, &valor, 1);
}
// ---------------------------------------------------------------------------

bool listando = false;

void __fastcall TForm2::Timer1Timer(TObject *Sender) {
	if (listando)
		return;
	else
		listando = true;

	__try {
		if (logList->Count > 0) {
			listaLog->Clear();
			Memo5->Clear();
		}

		listaLog->Lines->AddStrings(logList);
		logList->Clear();

		Memo5->Lines->AddStrings(sqlList);
		sqlList->Clear();
	}
	__finally {
		listando = false;
	}

}
// ---------------------------------------------------------------------------
