//---------------------------------------------------------------------------


#pragma hdrstop

#include "Suporte.h"

typedef DWORD TPixelLine[800];
typedef TPixelLine * PPixelLine;

BOOL SearchPixel(DWORD cor, std::vector<TPoint>& result, BOOL OnlyFirst)
{
	 DWORD inicio = GetTickCount();
	 result.clear();

	 HDC dc = CreateDC("DISPLAY", "", "", NULL);

	 Graphics::TBitmap *Bmp = new Graphics::TBitmap();

	 __try
	 {
			 Bmp->Width = 800;
			 Bmp->Height = 516;
			 BitBlt(Bmp->Canvas->Handle, 0, 0, 800, 516, dc, 0, 0, SRCCOPY);
			 //BitBlt(Form10->Image2->Canvas->Handle, 0, 0, 800, 516, dc, 0, 0, SRCCOPY);
			 Bmp->PixelFormat = pf32bit;

			 BOOL flag = false;
			 for(int i = 0; i < 516; i++)
			 {
					 PPixelLine linha = (PPixelLine)Bmp->ScanLine[i];
					 for(int j = 0; j < 800; j++)
					 {
							 DWORD px = (*((PPixelLine)(linha)))[j];
							 if(px == cor)
							 {
									 flag = true;
									 TPoint local;
									 local.x = j;
									 local.y = i;
									 result.push_back(local);
									 if(OnlyFirst)
									   break;
							 }
					 }
					 if(OnlyFirst)
						 if(flag)
								break;
			 }
			 if(!flag)
			 {
					result.clear();
			 }

			 return flag;
			 

			 DWORD tempo = (GetTickCount() - inicio);
       //Form10->Label18->Caption = IntToStr(tempo);

	 }
	 __finally
	 {
	     Bmp->Free();
			 DeleteDC(dc);
			 //ReleaseDC(h,dc);
	 }
	 
}

//---------------------------------------------------------------------------

#pragma package(smart_init)
