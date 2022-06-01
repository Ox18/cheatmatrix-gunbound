//---------------------------------------------------------------------------

#ifndef mathUtilsH
#define mathUtilsH

#include <math.h>

const float pi = 3.141592654;

double __fastcall intToRadian(float valor);
float __fastcall radianToInt(float valor);
double __fastcall modulo(double valor);
bool __fastcall pressionado(int valor);
int __fastcall codigoToIndice(unsigned long codigo);

//---------------------------------------------------------------------------
#endif
