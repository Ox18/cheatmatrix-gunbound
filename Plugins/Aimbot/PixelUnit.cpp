//---------------------------------------------------------------------------


#pragma hdrstop

#include "PixelUnit.h"
#include "debugUtils.h"

#pragma package(smart_init)

OS_INFO *sistema;

typedef struct _Pixels {
	 DWORD n1;
	 DWORD n2;
}TPixels;

long __fastcall CMBinToInt(char *value, int size) {
  long result = 0;
  for(int i = 0; i < size; i++){
	 result = ((result << 1) + (value[i] & 0x1));
  }
  return result;
}

char __fastcall ColorToByte(DWORD cor) {
	 if(cor == 0)
		 return 0; else return 1;
}

char __fastcall getnumeroNew(long int k) {
	BYTE result = 0;
	switch(k)
	{
		case 0x73FF10: return 9;
		case 0x29658C: return 0;
		case 0x397963: return 1;
		case 0x428E4A: return 2;
		case 0x426D73: return 3;
		case 0x6BFFD6: return 4;
		case 0x000000: return 5;
		case 0x317152: return 6;
		case 0x183CDE: return 7;
		case 0x42828C: return 8;
		default: return 0;
	}
}

char __fastcall getnumeroOld(long k) {
    BYTE result = 0;
    switch(k)
	{
		case 55: return 9;
		case 61: return 0;
		case 3:  return 1;
		case 27: return 2;
		case 23: return 3;
		case 54: return 4;
		case 39: return 5;
		case 46: return 6;
		case 21: return 7;
		case 63: return 8;
		default: return 0;
	 }
	 return result;
}

DWORD __stdcall CMGetPixel(HDC dc, int x, int y) {
	__try{
		PVOID *pilha;
		pilha = (PVOID*)&dc;
		DWORD result;

		if(!sistema){
			sistema = new OS_INFO;
			*sistema = GetOS();
		}

		/*PRGB32 Pixel = 0;
		Graphics::TBitmap *Bmp = new Graphics::TBitmap();
		Bmp->Width = GetSystemMetrics(SM_CXSCREEN);;
		Bmp->Height = GetSystemMetrics(SM_CYSCREEN);;
		BitBlt(Bmp->Canvas->Handle, 0, 0, 1, 1, dc, x, y, SRCCOPY);
		Bmp->PixelFormat = pf32bit;
		Pixel = PRGB32(Bmp->ScanLine[0]);
		DWORD result = Pixel->B*0x10000+Pixel->G*0x100+Pixel->R;
		Pixel = NULL;
		Bmp->Free();     7FFE0300h call    dword ptr [edx]
		return result;    */
		if(sistema && sistema->arquitetura == SYS_64){
        	__try{
				switch(sistema->OS){
					case WIN_VISTA: {
						__asm{
							mov 	ecx, 0Bh
							mov     eax, 10C4h
							mov     edx, pilha
							call    large dword ptr fs:0C0h
							add 	esp, 4
							mov result, eax
						}
					} break;
					default: {
                        __asm{
							mov 	ecx, 0Bh
							mov     eax, 10BFh
							mov     edx, pilha
							call    large dword ptr fs:0C0h
							add 	esp, 4
							mov result, eax
						}
                    }  break;
				}
			}__except(1){
				debugar("Erro 0022");
				debugar("Pixel: %X", result);
			}
		}else{
			__try{
				switch(sistema->OS){
					case WIN_XP: {
						//debugar("[%d] Windows XP ou anteriores", sistema->versao);
						// Windows XP e anteriores
						__asm{
							mov     eax, 10BFh
							mov     edx, pilha
							int 	2eh
							mov 	result, eax
						}
					} break;

					case WIN_SERVER: case WIN_VISTA: {
						//debugar("[%d] Windows Server", sistema->versao);
                        // Windows Server 2008
						__asm{
							mov     eax, 10C6h
							mov     edx, pilha
							int 	2eh
							mov 	result, eax
						}
					} break;

					case WIN_7: {
						//debugar("[%d] Windows Vista/7", sistema->versao);
						// Windows Vista e Superiores
						__asm{
							mov     eax, 10C8h
							mov     edx, pilha
							int 	2eh
							mov 	result, eax
						}
					} break;

					default: {
						//debugar("[%d] Sistema desconhecido", sistema->versao);
						// Windows XP e anteriores
						__asm{
							mov     eax, 10BFh
							mov     edx, pilha
							int 	2eh
							mov 	result, eax
						}
					} break;
				}

			}__except(1){
				debugar("Erro 0020");
				debugar("Pixel: %X", result);
			}
		}
		return result;
	}__except(1){
		debugar("Erro 9998");
	}
}

char rgbToColor(PRGB32 valor) {
	 return valor->B*0x10000+valor->G*0x100+valor->R;
}

DWORD __fastcall FixRGB(DWORD valor) {
	DWORD a,b,c;
	a = (valor & 0xFF0000) >> 16;
	b = (valor & 0x00FF00) >> 8;
	c = (valor & 0x0000FF);
	return ((c << 16) + (b << 8) + a);
}

pRGBArray __fastcall getLinha(){
	/*Graphics::TBitmap *Bmp = new Graphics::TBitmap();
	Bmp->Width = 1;
	Bmp->Height = 1;
	BitBlt(Bmp->Canvas->Handle, 0, 0, 1, 1, dc, x, y, SRCCOPY);
	Bmp->PixelFormat = pf32bit;
	PRGBArray pixels = PRGB32(Bmp->ScanLine[0]);
    return pixels;     */

}

char __fastcall GetInterfaceGB() {
	int i,j;
	HDC dc = 0;
	const int Bx1 = 3;    //329;
	const int By1 = 570;  //538;

	__try
	{
		dc = CreateDC("DISPLAY", "", "", NULL);
		if(dc == 0) return 0;
	}__except(1){
    	return 0;
	}

	__try
	{
		i = CMGetPixel(dc, Bx1, By1);
		j = CMGetPixel(dc, 2, 525);
		//debugar("cor: %X - %d - %d", j, isCorBase(j, yellow), getCorBase(j));
		return ((isCorBase(j, yellow) || isCorBase(i, yellow)) ? 1 : 2);
	}
	__finally
	{
		if(dc)
			DeleteDC(dc);
	}
	return 0;
}

bool isCor(DWORD cor, char m1, char m2, char m3){
	int b1 = (cor & 0xFF) & 0xFF;
	int b2 = ((cor >> 8) & 0xFF) & 0xFF;
	int b3 = ((cor >> 16) & 0xFF) & 0xFF;
	return ((b1 >= m1) && (b2 >= m2) && (b3 >= m3));
}

int modulo(int valor){
	return ((valor < 0)?-1*valor:valor);
}

bool isCorBase(DWORD cor, CORES corBase){
	bool resultado = false;

	int r = (cor & 0xFF) & 0xFF;
	int g = ((cor >> 8) & 0xFF) & 0xFF;
	int b = ((cor >> 16) & 0xFF) & 0xFF;

	float total = r+g+b;
	total = ((total == 0)?1:total);

	int pR = ((float)r/(float)total * 100);
	int pG = ((float)g/(float)total * 100);
	int pB = ((float)b/(float)total * 100);

	switch(corBase){
		case yellow: {
			resultado = ( pR > pB && pG > pB && modulo(pR - pG) <= 10);
		} break;
		case red: {
			resultado = (pR > pB && pR > pG);//(pR >= 35 && (pB <= 32 && pG <= 32));
		} break;
		case blue: {
			resultado = (pB > pR && pB > pG); //(pB >= 35 && (pR <= 32 && pG <= 32));
		} break;
		case green: {
			resultado = (pG > pB && pG > pR); //(pG >= 35 && (pR <= 32 && pB <= 32));
		} break;
	}

	return resultado;
}

CORES getCorBase(DWORD cor){
	bool flag = false;

	int r = (cor & 0xFF) & 0xFF;
	int g = ((cor >> 8) & 0xFF) & 0xFF;
	int b = ((cor >> 16) & 0xFF) & 0xFF;

	float total = r+g+b;
	total = ((total == 0)?1:total);

	int pR = ((float)r/(float)total * 100);
	int pG = ((float)g/(float)total * 100);
	int pB = ((float)b/(float)total * 100);


	flag = ( pR > pB && pG > pB && modulo(pR - pG) <= 10);
	if(flag)
		return yellow;
	flag = (pR > pB && pR > pG);//(pR >= 35 && (pB <= 32 && pG <= 32));
	if(flag)
		return red;
	flag = (pB > pR && pB > pG); //(pB >= 35 && (pR <= 32 && pG <= 32));
	if(flag)
		return blue;
	flag = (pG > pB && pG > pR); //(pG >= 35 && (pR <= 32 && pB <= 32));
	if(flag)
		return green;
	if(r == g == b && r <= 0x8F)
        return black;
	return white;
}

int __fastcall getAngleNew() {
	//debugar("new");
	__try{
		HDC dc = 0;
		int k1 = 0, k2 = 0, j = 0;
		TPixels pixels[6];
		//memset(&pixels[0], 0, sizeof pixels);
		BOOL neg = false;
		short int flag = 1;

		const int Bx1 = 244;
		const int Bx2 = 231;
		const DWORD Basey = 536;
		const char XsN[6] = {0,1,2,3,4,2};
		const int YsN[6] = {0,0,0,0,0,-1};
		dc = CreateDC("DISPLAY", "", "", NULL);
		if(!dc) return 0;
		__try
		{
			j = CMGetPixel(dc, 223, 538);
			neg = isCor(j, 0xE0, 0xD0, 0x50);//FixRGB(j) == 0xFFEB73;
			if(neg) flag = -1;
			for(int i = 0; i < 6; i++){
				pixels[i].n1 = CMGetPixel(dc, Bx1+XsN[i], Basey+YsN[i]);
				pixels[i].n2 = CMGetPixel(dc, Bx2+XsN[i], Basey+YsN[i]);
			}
			k1 = (pixels[0].n1 ^ pixels[1].n1 ^ pixels[2].n1 ^ pixels[3].n1 ^ pixels[4].n1 ^ pixels[5].n1);
			k2 = (pixels[0].n2 ^ pixels[1].n2 ^ pixels[2].n2 ^ pixels[3].n2 ^ pixels[4].n2 ^ pixels[5].n2);
			j = (getnumeroNew(k2)*10+getnumeroNew(k1))*flag;
			return j;
		}
		__finally
		{
			if(dc)
				DeleteDC(dc);
		}

		return 0;
	}__except(1){
		debugar("Erro 8182");
	}
}

int __fastcall getAngleOld() {
	__try{
		//debugar("old");
		 HDC dc = 0;
		 int k1 = 0, k2 = 0, j = 0;
		 TPixels pixels[6];
		 BOOL neg = false;
		 char ns1[6];
		 char ns2[6];
		 short int flag = 1;

		 const Bx1 = 322;
		 const Bx2 = 315;
		 const Basey = 542;
		 const char XsN[6] = {0,3,0,3,1,1};
		 const char YsN[6] = {2,2,5,5,3,0};
		 dc = CreateDC("DISPLAY", "", "", NULL);
		 if(dc == 0) return 0;

		 __try {
			j = CMGetPixel( dc, 312, 545);
			neg = (ColorToByte(FixRGB(j)) == '1');
			if(neg) flag = -1;
			for(int i = 0; i < 6; i++) {
				j = CMGetPixel(dc, Bx1+XsN[i], Basey+YsN[i]); //Pixel base + offset
				ns1[i] = ColorToByte(FixRGB(j));
				j = CMGetPixel(dc, Bx2+XsN[i], Basey+YsN[i]); //Pixel base + offset
				ns2[i] = ColorToByte(FixRGB(j));
			}
			k1 = CMBinToInt(&ns1[0], 6);
			k2 = CMBinToInt(&ns2[0], 6);
			return (getnumeroOld(k2)*10 +getnumeroOld(k1))*flag;
		 } __finally {
			if(dc)
				DeleteDC(dc);
		 }
		 return 0;
	}__except(1){
		debugar("Erro 8183");
	}
}

int __fastcall GetShotMode(int valor) {
	__try{
		HDC dc = CreateDC("DISPLAY", "", "", NULL);

		__try
		{
			int k = valor;

			if(k == 2)
			{
				const DWORD cor = 0x522008;
				DWORD p1 = FixRGB(CMGetPixel(dc, 16, 536));// & 0xFF0000;
				DWORD p2 = FixRGB(CMGetPixel(dc, 26, 536));//  & 0xFF0000;
				DWORD p3 = FixRGB(CMGetPixel(dc, 36, 536));//  & 0xFF0000;

				if(p1 == cor || p2 == cor || p3 == cor)
					 return 0;

				p1 = FixRGB(CMGetPixel(dc, 59, 536));
				p2 = FixRGB(CMGetPixel(dc, 69, 536));
				p3 = FixRGB(CMGetPixel(dc, 79, 536));

				if(p1 == cor || p2 == cor || p3 == cor)
					 return 1;

				p1 = FixRGB(CMGetPixel(dc, 102, 536));
				p2 = FixRGB(CMGetPixel(dc, 112, 536));
				p3 = FixRGB(CMGetPixel(dc, 122, 536));

				if(p1 == cor || p2 == cor || p3 == cor)
					 return 2;
			}
			else
			{
				const DWORD cor1 = 0xF70000;
				const DWORD cor2 = 0xFF0000;
				const DWORD nil = 0xFFFFFF;

				DWORD p1 = FixRGB(CMGetPixel(dc, 212, 535));
				DWORD p2 = FixRGB(CMGetPixel(dc, 219, 532));
				DWORD p3 = FixRGB(CMGetPixel(dc, 227, 530));

				if( ((p1 & cor2) == cor1 && p1 != nil ) || ((p2 & cor2) == cor1 && p2 != nil) || ((p3 & cor2) == cor1 && p3 != nil) )
					 return 0;

				p1 = FixRGB(CMGetPixel(dc, 198, 560));
				p2 = FixRGB(CMGetPixel(dc, 198, 554));
				p3 = FixRGB(CMGetPixel(dc, 205, 548));

				if( ((p1 & cor2) == cor1 && p1 != nil ) || ((p2 & cor2) == cor1 && p2 != nil) || ((p3 & cor2) == cor1 && p3 != nil) )
					 return 1;

				p1 = FixRGB(CMGetPixel(dc, 193, 567));
				p2 = FixRGB(CMGetPixel(dc, 200, 581));
				p3 = FixRGB(CMGetPixel(dc, 195, 590));

				if( ((p1 & cor2) == cor2 && p1 != nil ) || ((p2 & cor2) == cor2 && p2 != nil) || ((p3 & cor2) == cor2 && p3 != nil) )
					 return 2;
			}
		}
		__finally
		{
			if(dc)
				DeleteDC(dc);
		}

		return 0;
	}__except(1){
		debugar("Erro 8184");
	}
}

//---------------------------------------------------------------------------


