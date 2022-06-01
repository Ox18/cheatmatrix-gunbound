#ifndef PluginUtilsH
#define PluginUtilsH
#include "PluginClasses.h"
//#include <windef.h>
//#include <windows.h>

DWORD HexToInt(AnsiString s);
BOOL  IsDigit(char valor);
AnsiString IntToByteArray(INT64 valor);

BOOL __fastcall isValidScanChar(char valor, BOOL isHexa);
BOOL __fastcall isValidHexChar(char valor);
BOOL __fastcall isValidDecChar(char valor);
BOOL __fastcall isValidFloatChar(char valor);
long int __fastcall BinToInt(AnsiString Value);
AnsiString getDiretorioSubplugins();
AnsiString getDiretorioSubpluginsShort();
void decriptar(char * buffer, int sizeTotal, char *filename);
//---------------------------------------------------------------------------
#endif
