#include "baseCentral.h"
// #include "..\mathUtils.h"
// #include "..\secret.h"

HANDLE closeFlag;
CRITICAL_SECTION flagConnect;
HANDLE *threads;
LISTA *subplugs = new LISTA(0);
LISTA *ponteiros = new LISTA(0);
LISTA *atalhos = new LISTA(0);
// LISTA *processos = new LISTA(0);
char *processoJogo;
BYTE indiceProcesso;
bool inicializado;
WSADATA dataWSA;
Cliente *cliente;
char *DLLPath;
char *diretorioBase;
char *DLLPath1;
HINSTANCE hInstanceDll;
char *NomeMatriz;

//
// Inicializa as variáveis e threads
//
void inicializarDLL() {
	static bool inicializado;
	if (!inicializado) {
		inicializado = true;

		closeFlag = CreateEvent(NULL, TRUE, FALSE, NULL);
		InitializeCriticalSection(&flagConnect);

		threads = new HANDLE[3];
		threads[0] = CreateThread(NULL, 0, ThreadConexaoMonitorador, NULL, 0, NULL);
		threads[1] = CreateThread(NULL, 0, ThreadEncontraProcessoMonitorador, NULL, 0, NULL);
		threads[2] = CreateThread(NULL, 0, ThreadAtalhosMonitorador, NULL, 0, NULL);
		SetThreadContext(threads[0], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
		SetThreadContext(threads[1], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
		SetThreadContext(threads[2], (CONTEXT*)THREAD_PRIORITY_BELOW_NORMAL);
	}
}

void __stdcall processarCliente(PACOTE *pacote) {
}

/*
void __stdcall enviar(int socket, TIPO_PACOTE tipo, char* buffer, int size) {
	servidor->enviar(socket, tipo, buffer, size);
} */

//
// Finaliza a DLL limpando as variáveis
//
void finalizarDLL() {
	static bool inicializado;
	if (!inicializado) {
		SetEvent(closeFlag);
		WaitForMultipleObjects(3, threads, true, INFINITE);
		DeleteCriticalSection(&flagConnect);
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
}

void __stdcall PASCAL OnClientDisconnect(ContextoCliente *contexto) {
	//debugar("Launcher desconectado do spy! ");
}

//
// Gera uma mensagem de erro
//
/*
void PerformError(char *msg, char *titulo) {
	if (!titulo)
		MessageBoxA(NULL, msg, "Erro", NULL);
	else
		MessageBoxA(NULL, msg, titulo, NULL);
	HANDLE proc = OpenProcess(PROCESS_TERMINATE, false, GetCurrentProcessId());
	TerminateProcess(proc, 0);
	CloseHandle(proc);
}      */

DWORD WINAPI ThreadAtalhosMonitorador(LPVOID lpParam) {
	/*
	DWORD ultimoAtalho = 0;
	DWORD momentoUltimoAtalho = 0;
	while (WaitForSingleObject(closeFlag, 10) != WAIT_OBJECT_0) {
	__try {
	if (inicializado) {
	int modAC = (((GetKeyState(18) & 0x80) == 0x80) << 1) + ((GetKeyState(17) & 0x80) == 0x80);
	int mod = (modAC << 1) + ((GetKeyState(16) & 0x80) == 0x80);
	bool encontrado = false;
	ATALHO atalho;
	atalho.mods = 2;
	atalho.tecla = 46;
	if (atalho.mods == mod && (GetKeyState(atalho.tecla) & 0x80) == 0x80) {
	encontrado = true;
	int numeroAtalho = ((mod << 13) + atalho.tecla);

	if (ultimoAtalho != numeroAtalho)
	momentoUltimoAtalho = GetTickCount();

	if (ultimoAtalho != numeroAtalho || (GetTickCount() - momentoUltimoAtalho) > 800) {
	ultimoAtalho = numeroAtalho;
	__try {
	inicializado = true;
	// debugar("Atalho 'Exit' acionado");
	// finalizarDLL();
	// SetEvent(closeFlag);
	// SetEvent(closeFlag);
	// DeleteCriticalSection(&flagConnect);
	// CloseHandle(closeFlag);
	// FreeLibrary(GetModuleHandle("msvsct.dll"));
	// UnloadSelf();]
	ExitProcess(0);
	} __except (1) {
	debugar("Erro 6418.");
	}
	}
	}
	if (!encontrado) {
	ultimoAtalho = 0;
	momentoUltimoAtalho = GetTickCount();
	}
	}
	} __except (1) {
	}
	}
	// debugar("retornando da ThreadAtalhos");
	 */
}

DWORD WINAPI ThreadEncontraProcessoMonitorador(LPVOID lpParam) {
	__try {
		bool injetado = false;
		while (WaitForSingleObject(closeFlag, 100) != WAIT_OBJECT_0) {
			if (!cliente->conectado) {
				if (processoJogo) {
					// for (int i = 0; i < processos->size(); i++) {
					// char *processo = (char*)processos->get(i);
					__try {
						int id = GetTargetProcessIdFromProcname(processoJogo, indiceProcesso);
						// debugar("Processo %d (%s)", id, processoJogo);
						if (id > 0) {
							if (!injetado) {
								Sleep(500);
								//debugar("Matriz: %s", NomeMatriz);
								InjectDLL(id, NomeMatriz);
								injetado = true;
								//debugar("Injetado no GB!");
							}
						} else {
							injetado = false;
						}
					} __except (1) {
						debugar("erro 2402");
					}
				}
				// }
			} else {
				injetado = false;
			}

		}
	} __except (1) {
		debugar("Erro 7716");
	}
}

//
// Atualiza os endereços de acordo com os dados de Ponteiro informados
//
DWORD WINAPI ThreadConexaoMonitorador(LPVOID lpParam) {
	__try {
		char *buf = strdup(diretorioBase);
		buf = (char*)realloc(buf, strlen(buf) + 12);
		char configsIniStr[12];
		char configsStr[8];
		char sesStr[4];
		constanteEx(200, &configsIniStr[0]);
		constanteEx(201, &configsStr[0]);
		constanteEx(202, &sesStr[0]);
		strcat(buf, &configsIniStr[0]);

		TCMIniFile *ini = new TCMIniFile(buf);
		__try {
			while (WaitForSingleObject(closeFlag, 800) != WAIT_OBJECT_0) {
				__try {
					if (cliente && !cliente->conectado && ini != NULL) {
						int portaServidor = ini->ReadInteger(&configsStr[0], &sesStr[0], 0);
						// debugar("Porta lida");
						// (porta != NULL)?*porta:1313;
						portaServidor = (int)((portaServidor - 37) / 9269);
						// debugar("Porta: %d", portaServidor);
						if (portaServidor >= 1313 && portaServidor <= 33999) {
							//__try {
								//debugar("Cliente tentando conectar na Porta: %d", portaServidor);
							  	cliente->conectar(portaServidor);
							//} __except (1) {
							//}
							if (cliente->conectado) {
								//debugar("Spy conectado na matriz em [%d]", portaServidor + 10);
								__try {
									//debugar("Enviando %d plugins...",subplugs->size());
									for (int i = 0; i < subplugs->size(); i++) {
										FILE_SUBPLUGIN *subplug = (FILE_SUBPLUGIN*)subplugs->get(i);
										if (subplug) {
										//debugar("Enviando plugins %d: %s", i, subplug->nome);
										cliente->enviar(TP_SUBPLUGIN, (char*)subplug, sizeof(*subplug) + strlen(subplug->nome));
										}
									}
									//debugar("Enviando ponteiros...");
									for (int i = 0; i < ponteiros->size(); i++) {
										DADOS_PONTEIRO *ponteiro = (DADOS_PONTEIRO*)ponteiros->get(i);
										if (ponteiro) {
										int len = (ponteiro->size <= 4) ? (sizeof*ponteiro) : ((sizeof*ponteiro) + ponteiro->size - 4);
										cliente->enviar(TP_PONTEIRO, (char*)ponteiro, len);
										}
									}

									//Sleep(1000);
									//debugar("Enviando atalhos...");
									/*for (int i = 0; i < atalhos->size(); i++) {
										__try {
										// debugar("Enviando atalho %d: %d", i, (atalhos->get(i)) ? ((ATALHO*)atalhos->get(i))->tecla : 0);
										ATALHO *atalho = (ATALHO*)atalhos->get(i);
										cliente->enviar(TP_ATALHO, (char*)atalho, sizeof*atalho);
										} __except (1) {
										debugar("Erro 1111");
										}
									}*/

									// Sleep(1000);
								} __except (1) {
									debugar("Erro 9342");
								}
							}
						} else {
							Sleep(2000);
						}
					}
				} __except (1) {
					debugar("Erro 7777");
				}
			}
		}__finally {
        	// Limpamos da memória para não deixar rastros
			memset(&configsStr[0],0,8);
			memset(&configsIniStr[0],0,8);
			memset(&sesStr[0],0,4);
			free(ini);
			free(buf);
		}
	} __except (1) {
		debugar("Erro 7717");
	}
}

PVOID GetAddress(char *lib, char *func) {
	__try {
		HANDLE libH = LoadLibrary(lib);
		PVOID address = (PVOID)GetProcAddress((HMODULE)libH, func);
		return address;
	} __except (1) {
	}
}

int __fastcall codigoToIndice(unsigned long codigo) {
	return(((codigo & 0xFFFF) ^ (codigo >> 0x10)) - 0xB308);
}

void __fastcall processar(PACOTE *pacote) {
	if (pacote != NULL) {
		switch(pacote->tipo) {

		case TP_IDIOMA: {
				BYTE *novoatalho = (BYTE*) & pacote->buffer;
			}break;

		case TP_DISCONNECT: {
				// debugar("Recebido pacote TP_DISCONNECT");
				//servidor->terminar();
				FreeLibrary(hInstanceDll);
			}break;
			//
			// Mensagem de texto
		case TP_MENSAGEM: {
				char *msg = (char*) & pacote->buffer;
			}break;

			//
			// Manda carregar um subplugin
		case TP_SUBPLUGIN: {
				__try {
					FILE_SUBPLUGIN *fileSubplugin = (FILE_SUBPLUGIN*) & pacote->buffer;

					FILE_SUBPLUGIN *dados = (FILE_SUBPLUGIN*) & pacote->buffer;
					int len = (sizeof*dados) + strlen(dados->nome) + 1;
					FILE_SUBPLUGIN *novosDados = (FILE_SUBPLUGIN*)malloc(len);
					memset(novosDados, 0, len);
					memcpy(novosDados, dados, len);


					//debugar("Recebido nome do subplugin: %s", dados->nome);

					int subplugID = codigoToIndice(fileSubplugin->pluginID);
					FILE_SUBPLUGIN *existente = (FILE_SUBPLUGIN*)subplugs->get(subplugID);
					if (existente)
						free(existente);
					subplugs->addAt(subplugID, (long*)novosDados);

					if (cliente->conectado)
						cliente->enviar(TP_SUBPLUGIN, (char*)novosDados, len);
				} __except (1) {
					debugar("Erro 1771");
				}
			}break;
		case TP_PROCESSO: {
				__try {
					PACOTE_PROCESSO *pacoteProcesso = (PACOTE_PROCESSO*) & pacote->buffer;
					PACOTE_PROCESSO *dados = (PACOTE_PROCESSO*) & pacote->buffer;
					switch(dados->idioma) {
					case 0: {
							if (!processoJogo) {
								int lenNome = strlen(&dados->nome[1]);
								processoJogo = (char*)malloc(lenNome + 1);
								indiceProcesso = *(BYTE*)(&dados->nome[0]);
								memset(processoJogo, 0, lenNome + 1);
								memcpy(&processoJogo[0], &dados->nome[1], lenNome);
								//debugar("Processo recebido: %s", processoJogo);
							}
						}break;
					case 1: {
							if (!NomeMatriz) {
                            	int lenNome = strlen(dados->nome);
								NomeMatriz = (char*)malloc(lenNome + 1);
								memset(NomeMatriz, 0, lenNome + 1);
								memcpy(&NomeMatriz[0], &dados->nomeEx, lenNome);
								//debugar("Nome matriz recebido: %s", NomeMatriz);
							}
						}break;
					default: {
						}break;
					}
					// processos->addAt(subplugID, (long*)nomeProc);

				} __except (1) {
					debugar("Erro 1777");
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
					memcpy(novosDados->valor, (char*) & dados->valorEx[0], len);

					 //debugar("Ponteiro %d recebido (%d)", novosDados->packetID, novosDados->pluginID);
					/*
					//debugar("Cliente enviando ponteiros %d ",ponteiros->size());
					if (cliente->conectado && len > 0) {
						cliente->enviar(TP_PONTEIRO, (char*)novosDados, len);
						Sleep(100);
					}
                    */
					//
					// Evitemos duplicidades na memoria
					for (int i = 0; i < ponteiros->size(); i++) {
						DADOS_PONTEIRO *dado = (DADOS_PONTEIRO*)ponteiros->get(i);
						if (dado != NULL && dado->pluginID == novosDados->pluginID && dado->packetID == novosDados->packetID) {
							ponteiros->set(i, (long*)novosDados);
							return;
						}
					}

					//
					// Não existe nenhum então adiciona
					ponteiros->add((long*)novosDados);

					if (novosDados) {
						int len = (novosDados->size <= 4) ? (sizeof*novosDados) : ((sizeof*novosDados) + novosDados->size - 4);
						cliente->enviar(TP_PONTEIRO, (char*)novosDados, len);
                    }
				} __except (1) {
					debugar("Erro 0342");
				}
			}break;

		case TP_COMANDO: {
				__try {
					COMANDO_SUBPLUGIN *comando = (COMANDO_SUBPLUGIN*) & pacote->buffer;
					if (cliente->conectado)
						cliente->enviar(TP_COMANDO, (char*)comando, pacote->len);

					// if (subplug && (DWORD)subplug->metodos && (DWORD)subplug->metodos->processarComando) {
					// typedef HRESULT(__stdcall PASCAL * TProcessarComando)(COMANDO_SUBPLUGIN * comando);
					// TProcessarComando processar = (TProcessarComando)subplug->metodos->processarComando;
					// processar(comando);
					// break;
					// }
				} __except (1) {
					debugar("Erro 1772");
				}
			}break;

		case TP_ATALHO: {
				__try {
					// #### RECARREGAR ATALHOS

					if (cliente->conectado)
						cliente->enviar(TP_ATALHO, (char*)(ATALHO*) & pacote->buffer, pacote->len);

					ATALHO *novoatalho = (ATALHO*) & pacote->buffer;
					ATALHO *atalho = NULL;
					for (int i = 0; i < atalhos->size(); i++) {
						atalho = (ATALHO*)atalhos->get(i);
						if (atalho->pluginID == novoatalho->pluginID && atalho->id == novoatalho->id) {
							atalho->mods = novoatalho->mods;
							atalho->tecla = novoatalho->tecla;
							return;
						}
					}
					atalho = (ATALHO*)malloc(sizeof*atalho);
					memset(atalho, 0, sizeof*atalho);
					memcpy(atalho, novoatalho, sizeof*atalho);
					atalhos->add((long*)atalho);

					if (cliente->conectado) {
						cliente->enviar(TP_ATALHO, (char*)atalho, sizeof*atalho);
					}
				} __except (1) {
					debugar("Erro 1773");
				}
			}break;

		default:
			break;
		}
	}

}
