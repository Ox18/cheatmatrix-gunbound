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
#include "pixelUnit.h"
#include "injUnit.h"
#include "constantes.h"

#pragma package(smart_init)
#pragma link "Spin"
#pragma resource "*.dfm"

//#define DEBUGAR

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

void enviarAtalho(ATALHO *atalho) {
	if (atalho) {
		// debugar("enviando atalho %d [%d / %d]", atalho->id, atalho->mods, atalho->tecla);
		atalho->pluginID = Hack_Code;
		cliente->enviar(TP_ATALHO, (char*)atalho, sizeof*atalho);
	}
}

DWORD WINAPI ThreadConectar(LPVOID lpParam) {
	VMProtectBegin("conectar");
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

							// Sleep(1000);
						}
						__except (1) {
							debugar("Erro 9342");
						}

						//
						// Carregamos os subplugins no jogo
						//FILE_SUBPLUGIN subplugin;
						//subplugin.pluginID = GetPluginID();
						//subplugin.idioma =
						//
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
	VMProtectEnd();
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

DWORD __fastcall gravarConfiguracaoCor(TObject *sender) {
	AnsiString arquivo = arquivoConfiguracoes();
	TIniFile *iniCFG = new TIniFile(arquivo);
	DWORD resultado = 0;
	__try {
		__try {
			TColorBox* colorBox = dynamic_cast<TColorBox*>(sender);
			if (colorBox) {
				resultado = colorBox->Selected;
				iniCFG->WriteInteger("configs", colorBox->Name, resultado);
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

DWORD __fastcall gravarConfiguracaoCombo(TObject *sender) {
	AnsiString arquivo = arquivoConfiguracoes();
	TIniFile *iniCFG = new TIniFile(arquivo);
	int resultado = 0;
	__try {
		__try {
			TComboBox* combo = dynamic_cast<TComboBox*>(sender);
			if (combo) {
				resultado = combo->ItemIndex;
				iniCFG->WriteInteger("configs", combo->Name, resultado);
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

void __fastcall TForm2::mudarModoMira(char indice) {
	switch(indice) {
	case AM_Perto:
		radioPerto->Checked = true;
		break;
	case AM_Longe:
		radioLonge->Checked = true;
		break;
	case AM_Slice:
		radioSlice->Checked = true;
		break;
	case AM_Mouse:
		radioMouse->Checked = true;
		break;
	case AM_Random:
		radioRandom->Checked = true;
		break;
	case AM_Auto:
		radioPersonalizado->Checked = true;
		break;
	default:
		break;
	}
	iniCFG->WriteInteger("configs", "mira", (int)indice);
}

typedef struct _opcoesTiro {
	bool tiroEspecial;
	bool marcarSS;
}TOpcoesTiro;

void tratarOpcaoTiro(OPCOES_TIRO *opcao) {
	__try {
		switch(opcao->mobile) {
		case MB_BIG_FOOT:
			Form2->TC_BigFoot->Checked = opcao->tiroEspecial;
			break;
		case MB_TRICO:
			Form2->TC_Trico->Checked = opcao->tiroEspecial;
			break;
		case MB_MAGE:
			Form2->TC_Mage->Checked = opcao->tiroEspecial;
			break;
		case MB_TURTLE:
			Form2->TC_Turtle->Checked = opcao->tiroEspecial;
			break;
		case MB_NAK:
			Form2->TE_Nak->Checked = opcao->tiroEspecial;
			break;
		case MB_GRUB:
			Form2->TC_Grub->Checked = opcao->tiroEspecial;
			break;
		case MB_KASILON:
			Form2->TC_Kasildon->Checked = opcao->tiroEspecial;
			break;
		case MB_DRAGON:
			Form2->TC_Dragao->Checked = opcao->tiroEspecial;
			break;
		case MB_KNIGHT:
			// Form2->TC_Knight->Checked = opcao->tiroEspecial;
			break;
		case MB_RAON:
			Form2->TC_Raon->Checked = opcao->tiroEspecial;
			break;
		case MB_FRANK:
			Form2->TC_frank->Checked = opcao->tiroEspecial;
			break;
		default:
			break;
		}
	}
	__except (1) {
		debugar("Erro 7270");
	}
}

void mudarCor(int indice, DWORD cor) {
	switch(indice) {
	case 0:
		Form2->corLinha1->Selected = cor;
		break;
	case 1:
		Form2->corLinha2->Selected = cor;
		break;
	case 2:
		Form2->corEspelho->Selected = cor;
		break;
	case 3:
		Form2->corTornado->Selected = cor;
		break;
	default:
		break;
	}
}

void __stdcall PASCAL ProcessarComando(COMANDO_SUBPLUGIN *comando) {
	__try {
		// debugar("Processando comando no plugin %s", Hack_Name);
		if (comando != NULL) {
			switch(comando->tipo) {
				//
				// Ligar / Desligar
			case TP_LIGAR: {
					if (comando->valor != NULL) {
						if ((bool)comando->valor[0])
							Form2->Ligado->Checked = true;
						else
							Form2->Desligado->Checked = true;
					}
				}break;

				//
				// Modo de mira - Slice/Auto/Close/Far/Mouse/Random
			case TP_PERSONALIZAR_MODO_MIRA:
			case TP_PROXIMO_MODO_MIRA:
			case TP_MODO_MIRA_ANTERIOR: {
					// debugar("Mudando mira: %d", (char)comando->valor[0]);
					if (comando->valor != NULL)
						Form2->mudarModoMira((char)comando->valor[0]);
				}break;

			case TP_MODO_JUDAS: {
					if (comando->valor != NULL)
						Form2->modoJudas->Checked = (bool)comando->valor[0];
				}break;

				//
			case TP_MODO_GDI: {
					if (comando->valor != NULL)
						Form2->graficoAntigo->Checked = (bool)comando->valor[0];
				}break;
				//
			case TP_OPCAO_TIRO: {
					OPCOES_TIRO *opcao = (OPCOES_TIRO*) & comando->valor[0];
					tratarOpcaoTiro(opcao);
				}break;

				//
			/*case TP_MODO_DEBUG: {
					if (comando->valor != NULL)
						Form2->dbgCHK->Checked = (bool)comando->valor[0];
				}break; */
				//
			case TP_OPCAO_COR: {
					if (comando->valor != NULL) {
						OPCAO_COR *opcor = (OPCAO_COR*) & comando->valor[0];
						mudarCor(opcor->indice, opcor->cor);
					}
				}break;

			case TP_MENU_POS: {
					POINT *posicao = (POINT*) & comando->valor[0];
					gravarConfiguracao("menu_x", IntToStr((int)posicao->x));
					gravarConfiguracao("menu_y", IntToStr((int)posicao->y));
				}break;
			}
		}
	}
	__except (1) {
		debugar("Erro 7271");
	}
}

void __stdcall processar(PACOTE *pacote) {
	VMProtectBegin("processar");
	if (pacote != NULL) {
		switch(pacote->tipo) {
			//
			// Mensagem de texto
		case TP_MENSAGEM: {
				__try {
					char *msg = (char*) & pacote->buffer;
					//debugar("Servidor: %s", msg);
				}
				__except (1) {
					debugar("Erro 4748!");
				}
			}break;

		case TP_COMANDO: {
				COMANDO_SUBPLUGIN *comando = (COMANDO_SUBPLUGIN*) & pacote->buffer;
				ProcessarComando(comando);
			}break;

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
					//debugar("dir: %s", diretorio.t_str());
					FILE_SUBPLUGIN *fileSubplugin = (FILE_SUBPLUGIN*)malloc(sizeof(*fileSubplugin) + diretorio.Length() + 1);
					__try {
						memset(fileSubplugin, 0, sizeof(*fileSubplugin) + diretorio.Length() + 1);
						fileSubplugin->pluginID = Hack_Code;
						fileSubplugin->idioma = (BYTE)*idioma;
						memcpy(&fileSubplugin->nome, diretorio.c_str(), diretorio.Length());
//						debugar("enviando nome %s", diretorio.c_str());
						cliente->enviar(TP_SUBPLUGIN, (char*)fileSubplugin, sizeof(*fileSubplugin) + diretorio.Length());
						Form2->corLinha1Change(Form2->corLinha1);
						Form2->corLinha2Change(Form2->corLinha2);
						Form2->corEspelhoChange(Form2->corEspelho);
						Form2->corTornadoChange(Form2->corTornado);
						Form2->amostragemChange(Form2->amostragem);
						Form2->espessuraLinhaChange(Form2->espessuraLinha);
						Form2->TC_BigFootClick(Form2->TC_BigFoot);
						Form2->TC_DragaoClick(Form2->TC_Dragao);
						Form2->TC_frankClick(Form2->TC_frank);
						Form2->TC_GrubClick(Form2->TC_Grub);
						Form2->TC_KasildonClick(Form2->TC_Kasildon);
						Form2->TC_MageClick(Form2->TC_Mage);
						Form2->TC_MayaClick(Form2->TC_Maya);
						Form2->TC_RaonClick(Form2->TC_Raon);
						Form2->TC_TricoClick(Form2->TC_Trico);
						Form2->TC_TurtleClick(Form2->TC_Turtle);
						Form2->graficoAntigoClick(Form2->graficoAntigo);
						Form2->LigadoClick(Form2->Ligado);
						Form2->dbgCHKClick(Form2->dbgCHK);

						String xs = lerConfiguracao("menu_x");
						String ys = lerConfiguracao("menu_y");
						if (xs.Length() > 0 && ys.Length() > 0) {
							POINT menuPos;
							menuPos.x = StrToInt(xs);
							menuPos.y = StrToInt(ys);
							setarConfiguracao(TP_MENU_POS, (char*) & menuPos, sizeof menuPos);
						}
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
	VMProtectEnd();
}

// -------------------------------------------------------------------
// Pega o nome do arquivo de configurações
// -------------------------------------------------------------------
AnsiString __fastcall arquivoConfiguracoes() {
	VMProtectBegin("fileConfigs");
	TStringList *vazio = new TStringList();
	AnsiString arquivo = GetCurrentDir() + "\\shotmatrix.ini";
	if (!FileExists((AnsiString)arquivo)) {
		vazio->Text = "";
		vazio->SaveToFile((AnsiString)arquivo);
	}
	return arquivo;
	VMProtectEnd();
}

AnsiString __fastcall arquivoConfigCMX() {
	VMProtectBegin("configCMX");
	TStringList *vazio = new TStringList();
	AnsiString arquivo = GetCurrentDir() + "\\configs.ini";
	if (!FileExists((AnsiString)arquivo)) {
		vazio->Text = "";
		vazio->SaveToFile((AnsiString)arquivo);
	}
	return arquivo;
	VMProtectEnd();
}

// ---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner) : TForm(Owner) {
}

//
// Função que retorna a string com o status do hack
//
AnsiString GetStatus() {
	VMProtectBegin("getStatus");
	return(Form2->Ligado->Checked ? "ON" : "OFF");
	VMProtectEnd();
}

/*
//
//	Callback para encontrar a janela do jogo
//
BOOL EnumWindowsProc(HWND hWnd, long lParam) {
__try{
char Buff[255], NameOfClass[255];
GetWindowText(hWnd, Buff, 254);
GetClassName(hWnd, NameOfClass, 254);
if( strcmp(AnsiString(Buff).LowerCase().c_str(), "gunbound") == 0 ) {
if( strcmp(AnsiString(NameOfClass).LowerCase().c_str(), "softnyx") == 0 ) {
if(verificaObjetoBot() && usarInject)
BotInfos->janela = hWnd;
GBWindow = hWnd;
}
}
return TRUE;
}__except(1){
return TRUE;
}
}
 */

// ------------------------------------------------------------------
// Procura a janela do jogo
// ------------------------------------------------------------------
bool ProcuraJanelaTimerFlag = false;

void __fastcall TForm2::ProcuraJanelaTimerTimer(TObject *Sender) {
	/*
	//Se houver outro loop em execução, retorna
	if(ProcuraJanelaTimerFlag){
	return;
	}else
	ProcuraJanelaTimerFlag = true;

	__try {
	//Se o não houver um processo selecionado na matriz, retorna
	if(!MatrizInfo || MatrizInfo->TargetId == 0 || !verificaObjetoBot())
	return;

	__try {
	EnumWindows((WNDENUMPROC)EnumWindowsProc, 0);
	Sleep(5);
	Application->ProcessMessages();
	}__except(1) {
	//Se ocorreu um erro, seta o status do plugin como tal
	codigoErroPlugin = 1;
	}
	}
	__finally {
	ProcuraJanelaTimerFlag = false;
	}
	 */
}

// ------------------------------------------------------------------
// Timer de controle e amostragem do status do painel à direita
// ------------------------------------------------------------------
bool StatusTimerFlag = false;

void __fastcall TForm2::StatusTimerTimer(TObject *Sender) {
	// Mostra o nome do hack no painel
	sHack->Caption = Hack_Label;

	// Mostra o status do hack no painel
	// sStatus->Caption = GetStatus();
}

// Função que identifica cada atalho e realiza as ações para cada um
void __fastcall atalho(int index) {
	// Limpa as mensagens da tela, deste plugin

	// Identifica qual atalho foi digitado e realiza as ações. Lembrando que ao final
	// desta verificação é impresso na tela o status do hack, assim sendo, todos os
	// hacks possuem estes 2 indexes em comum. O Primeiro indentifica se é para ligar
	// ou desligar o hack. O ultimo, que não precisa ser identificado por si proprio,
	// podendo ser identificado pelo DEFAULT, apenas imprime o valor na tela, ou seja,
	// ele nao realiza nada.. na verdade nao existe, uma vez que obrigatoriamente a
	// funçao já imprime o status no final da verificação;
	switch(index) {
	case 0: {
			if (Form2->Ligado->Checked)
				Form2->Desligado->Checked = true;
			else
				Form2->Ligado->Checked = true;
		}break;
	case 1: {
		}break;

	default:
		break;
	}
}

// ------------------------------------------------------------------
// Timer de atualização dos dados do BOT
// ------------------------------------------------------------------
bool AtualizaBotTimerFlag = false;

//
// Verifica se o inteiro é valido
//
bool isValidInt(AnsiString s) {
	if (s == NULL || s.Length() == 0)
		return false;

	for (int i = 1; i <= s.Length(); i++) {
		char c = s[i];
		if (c < '0' || c > '9')
			return false;
	}
	return true;
}

// Gera uma string randômica
//
AnsiString GetRandomStr() {
	AnsiString letras = "abcdefghijklmnopqrstuvwxyz0123456789";
	AnsiString final = "";
	Randomize();
	for (int i = 1; i < 20; i++) {
		int k = Random(35) + 1;
		final = final + letras[k];
	}
	return final;
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
	return(float)temp;
}

DWORD pixelLocal = 0;
DWORD ultimaMudanca = 0;
DWORD quantidadeClicks = 0;

void TForm2::processaHotKey(int id) {

}

// ------------------------------------------------------------------
// Uma tecla de atalho foi pressionada
// ------------------------------------------------------------------
void TForm2::WMHotKey(TMessage& Message) {

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


void traduzPlugin(){

	if(Form2 != NULL){
		Form2->TabSheet1->Caption = getString(1);
		Form2->TabSheet3->Caption = getString(2);
		Form2->TabSheet9->Caption = getString(3);
		Form2->TabSheet2->Caption = getString(4);
		Form2->Label2->Caption = getString(5);
		Form2->Label7->Caption = getString(6);
		Form2->GroupBox3->Caption = getString(7);
		Form2->radioPerto->Caption = getString(8);
		Form2->radioLonge->Caption = getString(9);
		Form2->radioSlice->Caption = getString(10);
		Form2->radioMouse->Caption = getString(11);
		Form2->radioRandom->Caption = getString(12);
		Form2->radioPersonalizado->Caption = getString(13);
		Form2->TabSheet11->Caption = getString(14);
		Form2->TabSheet12->Caption = getString(15);
		Form2->TabSheet13->Caption = getString(16);
		Form2->Label15->Caption = getString(17);
		Form2->Label35->Caption = getString(18);
		Form2->Label13->Caption = getString(19);
		Form2->Label14->Caption = getString(20);
		Form2->Label28->Caption = getString(21);
		Form2->Label26->Caption = getString(22);
		Form2->graficoAntigo->Caption = getString(23);
		Form2->modoJudas->Caption = getString(24);
		Form2->Button4->Caption = getString(25);
		Form2->Button5->Caption = getString(26);
		Form2->Button7->Caption = getString(27);
		Form2->listaAtalhos->Items->Item[0]->Caption = getString(28);
		Form2->listaAtalhos->Items->Item[1]->Caption = getString(29);
		Form2->listaAtalhos->Items->Item[2]->Caption = getString(30);
		Form2->listaAtalhos->Items->Item[3]->Caption = getString(31);
		Form2->listaAtalhos->Items->Item[4]->Caption = getString(32);
		Form2->listaAtalhos->Items->Item[5]->Caption = getString(33);
		Form2->listaAtalhos->Items->Item[6]->Caption = getString(34);
		Form2->listaAtalhos->Items->Item[7]->Caption = getString(35);
		Form2->listaAtalhos->Items->Item[8]->Caption = getString(36);
		Form2->listaAtalhos->Items->Item[9]->Caption = getString(37);
		Form2->listaAtalhos->Items->Item[10]->Caption = getString(38);
		Form2->listaAtalhos->Items->Item[11]->Caption = getString(39);
		Form2->listaAtalhos->Items->Item[12]->Caption = getString(40);
		Form2->listaAtalhos->Items->Item[13]->Caption = getString(41);
		Form2->listaAtalhos->Items->Item[14]->Caption = getString(42);
		Form2->listaAtalhos->Items->Item[15]->Caption = getString(43);
		Form2->listaAtalhos->Items->Item[16]->Caption = getString(64);
		Form2->listaAtalhos->Items->Item[17]->Caption = getString(44);
		Form2->listaAtalhos->Items->Item[18]->Caption = getString(45);
/*		Form2->listaAtalhos->Columns->Items[1]->Caption = getString(63);
		Form2->TC_BigFoot->Caption = getString(46);
		Form2->TC_Mage->Caption = getString(47);
		Form2->TC_Trico->Caption = getString(48);
		Form2->TC_Turtle->Caption = getString(49);
		Form2->TC_Grub->Caption = getString(50);
		Form2->TC_Kasildon->Caption = getString(51);
		Form2->TC_Maya->Caption = getString(52);
		Form2->TC_Dragao->Caption = getString(53);
		Form2->TC_Raon->Caption = getString(54);
		Form2->TC_frank->Caption = getString(55);
		Form2->TE_Nak->Caption = getString(56);
		Form2->TE_ASate->Caption = getString(57);
		Form2->Marcar_SS->Caption = getString(58);
		Form2->TabSheet5->Caption = getString(59);
		Form2->TabSheet4->Caption = getString(60);
		Form2->Memo6->Text = getString(61);
		Form2->Memo5->Text = getString(62); */
	}

}

void __fastcall TForm2::FormCreate(TObject *Sender) {
	VMProtectBegin("createfrm");

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

	dbgCHK->Checked = (iniCFG->ReadInteger("configs", "modoDebug", 0) == 135);

	int indiceMira = iniCFG->ReadInteger("configs", "mira", AM_Slice);
	mudarModoMira((char)(indiceMira & 0xFF));
	DWORD cor = 0;
	if ((cor = iniCFG->ReadInteger("configs", "corLinha1", 0)) != 0)
		corLinha1->Selected = cor;
	if ((cor = iniCFG->ReadInteger("configs", "corLinha2", 0)) != 0)
		corLinha2->Selected = cor;
	if ((cor = iniCFG->ReadInteger("configs", "corEspelho", 0)) != 0)
		corEspelho->Selected = cor;
	if ((cor = iniCFG->ReadInteger("configs", "corTornado", 0)) != 0)
		corTornado->Selected = cor;

	int selecionado = 0;
	if ((selecionado = iniCFG->ReadInteger("configs", "amostragem", -1)) != -1)
		amostragem->ItemIndex = selecionado;
	if ((selecionado = iniCFG->ReadInteger("configs", "espessuraLinha", -1)) != -1)
		espessuraLinha->ItemIndex = selecionado;

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
				item->Caption = getString(65);
				item->SubItems->Add(atalho);
				item->SubItems->Add(atalhosCFG->Strings[i]);
				item->SubItems->Add(0);
			}
		}
	}
	__except (1) {
		debugar("Erro 9910");
	}

	for (int j = 1; j < listaAtalhos->Items->Count; j++) {
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
	VMProtectEnd();
}

/** ************************************************************
 * Evento FORM-DESTROY - desregistra os atalhos
 ************************************************************ */
void __fastcall TForm2::FormDestroy(TObject *Sender) {
	/* for(int i = 0; i < (sizeof(hotkeys)/4); i++ ) {
	UnregisterHotKey(Handle, hotkeys[i]);
	GlobalDeleteAtom(i);
	} */

	__try {
		WaitForSingleObject(threadConexao, 2000);
		CloseHandle(threadConexao);
	}
	__except (1) {
	}
}

bool TimerIntrodutorFlag = false;
bool injetado = false;

void __fastcall TForm2::injetaTimerTimer(TObject *Sender) {
//
////	VMProtectBegin("injeta");
	// if (TimerIntrodutorFlag) {
	// return;
	// }
	// else
	// TimerIntrodutorFlag = true;
	//
	// __try {
	// if (!cliente->conectado) {
	// __try {
	// int id = GetTargetProcessIdFromProcname(2,"gunbound.gme","GbTeenwc.txt");
	// if (id > 0) {
	// if (!injetado) {
	// Sleep(500);
	// //debugar("Injetado no id %d pelo plugin em %d", id, GetTickCount());
	// InjectDLL(id, InjectorName.c_str());
	// injetado = true;
	// // debugar("Injetado no GB!");
	// }
	// }
	// else {
	// injetado = false;
	// }
	// }
	// __except (1) {
	// debugar("erro 2402");
	// }
	// }
	// else {
	// injetado = false;
	// }
	// }
	// __finally {
	// TimerIntrodutorFlag = false;
	// }
	// //	VMProtectEnd();

}
// ---------------------------------------------------------------------------

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

void __fastcall TForm2::Timer2Timer(TObject *Sender) {

	if (!intimer)
		intimer = true;
	else
		return;

#ifdef DEBUGAR
	__try {
		HDC dc = 0;
		// HDC cDC = 0;
		int x = GetSystemMetrics(SM_CXSCREEN);
		int y = GetSystemMetrics(SM_CYSCREEN);
		HBITMAP hbmpScr;
		__try {
			dc = CreateDC("DISPLAY", "", "", NULL);
			// cDC = CreateCompatibleDC(dc);
			// HBITMAP hbmpOrig = CreateCompatibleBitmap(cDC, x, y);
			// hbmpScr= (HBITMAP) SelectObject(cDC,hbmpOrig);
			//
			if (dc == 0)
				return;
		}
		__except (1) {
			return;
		}

		__try {
			__try {
#pragma pack(push,1)
				int j = 0;
				__try {
					j = CMGetPixel(dc, Mouse->CursorPos.x, Mouse->CursorPos.y);
					// getAngleNew();
				}
				__except (1) {
					debugar("erro 7776");
				}

				DWORD cor = j;
				bool corF = ((((cor >> 16) & 0xFF) >= 0x90) && ((((cor & 0xFF00) >> 8) & 0xFF) >= 0xB0) && ((cor & 0xFF) >= 0xC0));
				// Edit1->Text = IntToStr((int)j);
				// if(BotInfos && (j & 0xF0F0F0) != 0x804030){
				char teste[100];
				int n = 0;
				n = sprintf(teste, "%8X        ", FixRGB(j));
				TextOut(dc, 10, 10, teste, 15);

				int b1 = (j & 0xFF);
				int b2 = ((j >> 8) & 0xFF);
				int b3 = ((j >> 16) & 0xFF);

				memset(teste, 0, 100);
				n = sprintf(teste, "[%s][%X][%X][%X]        ", corF ? "SIM" : "NAO", b1 & 0xFF, b2 & 0xFF, b3 & 0xFF);
				memset(&teste[n], 0x20, 100 - n - 1);
				bool txt = TextOut(dc, 10, 25, teste, 50);
				// bool res = BitBlt(dc, 0, 0, x, y, cDC, 0, 0,SRCCOPY);
				// debugar("dc: %X   cDC: %X", dc, cDC);
				// debugar("BitBlt: %d  - TextOut: %d", res, txt);
				// if(!res)
				// ErrorExit(TEXT("BitBlt"));
#pragma pack(pop)
				// }
			}
			__except (1) {
			}

			// j = GetPixel(dc, Mouse->CursorPos.x, Mouse->CursorPos.y);
			// Edit2->Text = IntToStr((int)j);
		}
		__finally {
			// SelectObject(cDC,hbmpScr);
			if (dc)
				DeleteDC(dc);
			// if(cDC)
			// DeleteDC(cDC);
		}
	}
	__finally {
		intimer = false;
	}
#endif

}
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
DWORD inicioTiro = 0;
DWORD tempoTiro = 0;

bool processandoConexao = false;

void __fastcall TForm2::timerAutoShotTimer(TObject *Sender) {
	if (!processandoConexao)
		processandoConexao = true;
	else
		return;

	__try {
		sPS->Caption = (cliente->conectado) ? "ON" : "OFF";
		sPS->Font->Color = (cliente->conectado) ? clGreen : clRed;
	}
	__finally {
		processandoConexao = false;
	}
}

// ---------------------------------------------------------------------------
void __fastcall TForm2::Button3Click(TObject *Sender) {
	VMProtectBegin("calibracaoteste");
	TReplaceFlags flag;
	AnsiString arquivo = ExtractFilePath(ParamStr(0)) + "calibracao.ini";
	TIniFile *iniFile = new TIniFile(arquivo);
	__try {
		for (int j = 0; j < 24; j++) {
			AnsiString s = "{ ";

			double gt = 0, pt = 0;
			for (int i = 0; i < 360; i += 10) {
				pt = iniFile->ReadFloat("M" + IntToStr(j), "P" + IntToStr(i), 0);
				gt = iniFile->ReadFloat("M" + IntToStr(j), "G" + IntToStr(i), 0);
				AnsiString virgula = ((s.Length() > 2) ? "," : "");
				AnsiString bloco = "{" + formataFloatEx(pt) + "," + formataFloatEx(gt) + "}";
				s += virgula + bloco;
			}
			s += " }";
			Memo4->Lines->Add(s);
		}
	}
	__finally {
		iniFile->Free();
	}
	VMProtectEnd();
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::listaAtalhosSelectItem(TObject *Sender, TListItem *Item, bool Selected) {
	HotKey1->HotKey = TextToShortCut(Item->SubItems->Strings[0]);
	labelItem->Caption = Item->Caption;
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::Button6Click(TObject *Sender) {
	VMProtectBegin("atalhos");
	if (listaAtalhos->ItemIndex >= 0 && listaAtalhos->ItemIndex < listaAtalhos->Items->Count) {
		// UnregisterHotKey(Handle, StrToInt(listaAtalhos->Items->Item[listaAtalhos->ItemIndex]->SubItems->Strings[2]));
		int atalhoInt = HotKey1->HotKey;
		for (int i = 0; i < listaAtalhos->Items->Count; i++) {
			int itemInt = StrToInt(listaAtalhos->Items->Item[i]->SubItems->Strings[2]);
			if (itemInt == atalhoInt) {
				MessageBoxA(0, getString(66).c_str(), getString(67).c_str(), 0);
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
	VMProtectEnd();
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::ComboKeyClick(TObject *Sender) {
	VMProtectBegin("mudaAtalho");
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
	VMProtectEnd();
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
	ShowMessage(getString(68));
}

// ---------------------------------------------------------------------------
char __fastcall TForm2::getModoMira() {
	char modoMira = 0;
	if (radioPerto->Checked) {
		modoMira = AM_Perto;
	}
	else if (radioLonge->Checked) {
		modoMira = AM_Longe;
	}
	else if (radioMouse->Checked) {
		modoMira = AM_Mouse;
	}
	else if (radioRandom->Checked) {
		modoMira = AM_Random;
	}
	else if (radioSlice->Checked) {
		modoMira = AM_Slice;
	}
	else {
		modoMira = AM_Auto;
	}
	return modoMira;
}

void __fastcall TForm2::radioMouseClick(TObject *Sender) {
	char modoMira = getModoMira();
	int mira = (int)(modoMira & 0xFF);
	iniCFG->WriteInteger("configs", "mira", mira);
	setarConfiguracao(TP_PERSONALIZAR_MODO_MIRA, &modoMira, 1);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::modoJudasClick(TObject *Sender) {
	char valor = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_MODO_JUDAS, &valor, 1);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::LigadoClick(TObject *Sender) {
	char valor = Ligado->Checked;
	setarConfiguracao(TP_LIGAR, &valor, 1);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::graficoAntigoClick(TObject *Sender) {
	char valor = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_MODO_GDI, &valor, 1);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::linhaTornadoClick(TObject *Sender) {
	char valor = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_LINHA_VERTICAL, &valor, 1);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_BigFootClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_BIG_FOOT;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_MageClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_MAGE;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_TricoClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_TRICO;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_TurtleClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_TURTLE;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_GrubClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_GRUB;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_KasildonClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_KASILON;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_MayaClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_MAYA;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_DragaoClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_DRAGON;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_RaonClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_RAON;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TC_frankClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_FRANK;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TE_NakClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_NAK;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::TE_ASateClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_ASATE;
	valor.tiroEspecial = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::Marcar_SSClick(TObject *Sender) {
	OPCOES_TIRO valor;
	valor.mobile = MB_TODOS;
	valor.marcarSS = gravarConfiguracaoCheckBox(Sender);
	setarConfiguracao(TP_OPCAO_TIRO, (char*) & valor, sizeof valor);
}
// -----------------------------------------------------------------------

void __fastcall TForm2::dbgCHKClick(TObject *Sender) {
	TCheckBox* check = dynamic_cast<TCheckBox*>(Sender);
	//if (check && check->Visible && check->Enabled) {
		char valor = check->Checked;//gravarConfiguracaoCheckBox(Sender);
		setarConfiguracao(TP_MODO_DEBUG, &valor, 1);
	//}
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::corLinha1Change(TObject *Sender) {
	OPCAO_COR valor;
	valor.indice = 0;
	valor.cor = gravarConfiguracaoCor(Sender);
	setarConfiguracao(TP_OPCAO_COR, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::corLinha2Change(TObject *Sender) {
	OPCAO_COR valor;
	valor.indice = 1;
	valor.cor = gravarConfiguracaoCor(Sender);
	setarConfiguracao(TP_OPCAO_COR, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::corEspelhoChange(TObject *Sender) {
	OPCAO_COR valor;
	valor.indice = 2;
	valor.cor = gravarConfiguracaoCor(Sender);
	setarConfiguracao(TP_OPCAO_COR, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::corTornadoChange(TObject *Sender) {
	OPCAO_COR valor;
	valor.indice = 3;
	valor.cor = gravarConfiguracaoCor(Sender);
	setarConfiguracao(TP_OPCAO_COR, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::amostragemChange(TObject *Sender) {
	char valor;
	switch(amostragem->ItemIndex) {
	case 0:
		valor = -1;
		break;
	case 1:
		valor = 2;
		break;
	case 2:
		valor = 3;
		break;
	case 3:
		valor = 5;
		break;
	case 4:
		valor = 10;
		break;
	default:
		valor = 0;
		break;
	}
	gravarConfiguracaoCombo(Sender);
	setarConfiguracao(TP_TEMPO_MENU, (char*) & valor, sizeof valor);
}
// ---------------------------------------------------------------------------

void __fastcall TForm2::espessuraLinhaChange(TObject *Sender) {
	char valor;
	switch(espessuraLinha->ItemIndex) {
	case 1:
		valor = 2;
		break;
	case 2:
		valor = 3;
		break;
	default:
		valor = 1;
		break;
	}
	gravarConfiguracaoCombo(Sender);
	setarConfiguracao(TP_ESPESSURA_LINHA, (char*) & valor, sizeof valor);
}

// ---------------------------------------------------------------------------
