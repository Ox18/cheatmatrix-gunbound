//---------------------------------------------------------------------------

#pragma hdrstop

#include "mathUtils.h"

double __fastcall intToRadian(float valor) {
    return valor*pi/180;
}

float __fastcall radianToInt(float valor) {
    return valor*180/pi;
}

double __fastcall modulo(double valor) {
    return (valor >= 0) ? valor : -1*valor;
}

//
//    Verifica se a tecla informada está pressionada
//
bool __fastcall pressionado(int valor){
   return (valor < -1 || valor > 1);
}

double __fastcall getCos(int angulo){
   return cos(intToRadian(angulo));
}

double __fastcall getSin(int angulo){
   return sin(intToRadian(angulo));
}

int __fastcall codigoToIndice(unsigned long codigo){
	return (( (codigo & 0xFFFF) ^ (codigo >> 0x10)) - 0xB308);
}

//---------------------------------------------------------------------------

#pragma package(smart_init)
