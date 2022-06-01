//---------------------------------------------------------------------------

#ifndef PixelUnitH
#define PixelUnitH

#include <windows.h>
#include "..\osInfo.h"

typedef struct _RGB32
{
		 char B, G, R;
} TRGB32, *PRGB32;

enum CORES {
	red,
	green,
	blue,
	yellow,
	white,
	black
};

typedef TRGB32 TRGBArray[];
typedef TRGBArray* pRGBArray;

extern OS_INFO *sistema;

int __fastcall getAngleOld();
int __fastcall getAngleNew();
char __fastcall GetInterfaceGB();
DWORD __stdcall CMGetPixel(HDC dc, int x, int y);
char __fastcall ColorToByte(DWORD cor);
char __fastcall getnumeroNew(long int k);
char __fastcall getnumeroOld(long int k);
DWORD __fastcall FixRGB(DWORD valor);
int __fastcall GetShotMode(int valor);
pRGBArray __fastcall getLinha();
CORES getCorBase(DWORD cor);
bool isCorBase(DWORD cor, CORES corBase);

//---------------------------------------------------------------------------
#endif
