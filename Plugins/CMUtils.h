//---------------------------------------------------------------------------

#ifndef CMUtilsH
#define CMUtilsH

#include <windows.h>
#include <stdio.h>

bool MyExtractFilePath(char* valor, char *buffer);
char *CMIntToHex(DWORD valor, int formato = 8);
DWORD HexToInt(char *s);

char *StringToHex(char *valor);
char *HexToString(char *valor);
//---------------------------------------------------------------------------
#endif
