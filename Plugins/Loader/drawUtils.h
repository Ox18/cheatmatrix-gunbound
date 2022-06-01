//---------------------------------------------------------------------------

#ifndef drawUtilsH
#define drawUtilsH

#include "tipos.h"

HBITMAP Load32bppTga(const TCHAR * pFileName, bool bPreMultiply);
void AlphaDraw(HDC hDC, int x, int y, int width, int height, HBITMAP hBmp, double transparency);
HRESULT DDCopyBitmap(IDirectDrawSurface *pdds, HBITMAP hbm, int dx, int dy);

//---------------------------------------------------------------------------
#endif
