//---------------------------------------------------------------------------

#ifndef constantesH
#define constantesH

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

extern int idioma;

char* constanteX(int id, int lpIdioma = -2);
char* constante(int id);
char* mutacao(char *valor);
char* constanteEx(int id, char* buffer);
void patchConstantes();
//---------------------------------------------------------------------------
#endif
