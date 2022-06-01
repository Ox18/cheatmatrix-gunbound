//---------------------------------------------------------------------------

#ifndef drawUtilsH
#define drawUtilsH

#include "windows.h"
#include <stdio.h>
#include <stdlib.h>
//#include <ddraw.h>

#pragma pack(push,1)

typedef struct {
	BYTE    IDLength;
	BYTE    ColorMapType;
	BYTE    ImageType;
	WORD    CMapStart;
	WORD    CMapLength;
	BYTE    CMapDepth;
	WORD    XOffset;
	WORD    YOffset;
	WORD    Width;
	WORD    Height;
	BYTE    PixelDepth;
	BYTE    ImageDescriptor;
}   TGA_Header;

HBITMAP Load32bppTgaFromMemory(const char* buffer, int bufSize, bool bPreMultiply);
HBITMAP Load32bppTga(const TCHAR * pFileName, bool bPreMultiply);
void AlphaDraw(HDC hDC, int x, int y, int width, int height, HBITMAP hBmp, double transparency);
bool SaveBMPFile(char *filename, HBITMAP bitmap, HDC bitmapDC, int width, int height);
bool ScreenCapture(int x, int y, int width, int height, char *filename);
bool isVermelho(DWORD cor);
//HRESULT DDCopyBitmap(IDirectDrawSurface *pdds, HBITMAP hbm, int dx, int dy);

#pragma pack(pop)

//---------------------------------------------------------------------------
#endif
