//---------------------------------------------------------------------------

#ifndef CMStrUtilsH
#define CMStrUtilsH

#include <windows.h>

int __fastcall CM_StrLen(char *valor);
bool __fastcall CM_StrCat(char *valor1, char *valor2, char *buffer);
char * __fastcall CM_LowerCase(char *valor);
char * __fastcall CM_UpperCase(char *valor);
char __fastcall CM_LowCase(char valor);
char __fastcall CM_UpCase(char valor);
wchar_t * __fastcall CM_StringToWideChar(char *valor);
//---------------------------------------------------------------------------
#endif
