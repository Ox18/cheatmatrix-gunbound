//---------------------------------------------------------------------------

#pragma hdrstop

#include "drawUtils.h"
#include "debugUtils.h"
#include <time.h>

/*
HRESULT DDCopyBitmap(IDirectDrawSurface *pdds, HBITMAP hbm, int dx, int dy) {
    HDC hdcImage;
    HDC hdc;
    HRESULT hr;
    HBITMAP hbmOld;
	//
    // select bitmap into a memoryDC so we can use it.
    //
    hdcImage = CreateCompatibleDC(NULL);
    hbmOld = (HBITMAP)SelectObject(hdcImage, hbm);

    if ((hr = pdds->GetDC(&hdc)) == DD_OK) {
        BitBlt(hdc, 0, 0, dx, dy, hdcImage, 0, 0, SRCCOPY);
        pdds->ReleaseDC(hdc);
    }

    SelectObject(hdcImage, hbmOld);
    DeleteDC(hdcImage);

    return hr;
} */

bool isVermelho(DWORD cor){
	int b1 = (cor & 0xFF) & 0xFF;
	int b2 = ((cor >> 8) & 0xFF) & 0xFF;
	int b3 = ((cor >> 16) & 0xFF) & 0xFF;
	return ((b1 >= 0xA0) && (b1 > b2) && (b1 > b3));
}

int GetFilePointer(HANDLE FileHandle){
	return SetFilePointer(FileHandle, 0, 0, FILE_CURRENT);
}

void SaveBitmap(char *szFilename,HBITMAP hBitmap){
	HDC                 hdc=NULL;
	FILE*               fp=NULL;
	LPVOID              pBuf=NULL;
	BITMAPINFO          bmpInfo;
	BITMAPFILEHEADER    bmpFileHeader;

	hdc=GetDC(NULL);
	ZeroMemory(&bmpInfo,sizeof(BITMAPINFO));
	bmpInfo.bmiHeader.biSize=sizeof(BITMAPINFOHEADER);
	GetDIBits(hdc,hBitmap,0,0,NULL,&bmpInfo,DIB_RGB_COLORS);

	if(bmpInfo.bmiHeader.biSizeImage <= 0)
	bmpInfo.bmiHeader.biSizeImage=bmpInfo.bmiHeader.biWidth*abs(bmpInfo.bmiHeader.biHeight)*(bmpInfo.bmiHeader.biBitCount+7)/8;

	if((pBuf=malloc(bmpInfo.bmiHeader.biSizeImage)) == NULL)
	{
	   //MessageBox(NULL,_T("Unable to Allocate Bitmap Memory"),_T("Error"),MB_OK|MB_IConerror);
	   return;
	}

	bmpInfo.bmiHeader.biCompression = BI_RGB;
	GetDIBits(hdc,hBitmap,0,bmpInfo.bmiHeader.biHeight,pBuf,&bmpInfo,DIB_RGB_COLORS);

	if((fp=fopen(szFilename,"wb"))==NULL)
	{
	   return;
	}

	bmpFileHeader.bfReserved1=0;
	bmpFileHeader.bfReserved2=0;
	bmpFileHeader.bfSize=sizeof(BITMAPFILEHEADER)+sizeof(BITMAPINFOHEADER)+bmpInfo.bmiHeader.biSizeImage;
	bmpFileHeader.bfType='MB';
	bmpFileHeader.bfOffBits=sizeof(BITMAPFILEHEADER)+sizeof(BITMAPINFOHEADER);
	fwrite(&bmpFileHeader,sizeof(BITMAPFILEHEADER),1,fp);
	fwrite(&bmpInfo.bmiHeader,sizeof(BITMAPINFOHEADER),1,fp);
	fwrite(pBuf,bmpInfo.bmiHeader.biSizeImage,1,fp);

	if(hdc)
	ReleaseDC(NULL,hdc);

	if(pBuf)
	free(pBuf);

	if(fp)
	fclose(fp);
}

bool ScreenCapture(int x, int y, int width, int height, char *diretorio){
	HDC hDesktopDC = CreateDC("DISPLAY", "", "", NULL);
	__try{
		/*HDC hDc = CreateCompatibleDC(dc);
		HBITMAP hBmp = CreateCompatibleBitmap(dc, width, height);
		SelectObject(hDc, hBmp);
		if(width == 0)
			width = GetSystemMetrics(SM_CXSCREEN);
		if(height == 0)
			height = GetSystemMetrics(SM_CYSCREEN);
		BitBlt(hDc, 0, 0, width, height, dc, x, y, SRCCOPY);
		bool ret = SaveBMPFile(filename, hBmp, hDc, width, height);
		DeleteObject(hBmp);
		return ret; */

		time_t rawtime;
  		struct tm * timeinfo;
		char tempoStr[20];
		char szFileName[512];
		time ( &rawtime );
		timeinfo = localtime ( &rawtime );
		strftime(tempoStr,20,"%d%m%y-%H%M%S",timeinfo);
		sprintf(szFileName,"%sscreenshots\\ss-%s.bmp", diretorio, tempoStr);
		debugar("tirando screenshot de %s", szFileName);

		//strcpy(szFileName,"ScreenShot.bmp");
		SetCursor(LoadCursor(NULL,IDC_WAIT));
		int nWidth=GetSystemMetrics(SM_CXSCREEN);
		int nHeight=GetSystemMetrics(SM_CYSCREEN);
		HDC hBmpFileDC=CreateCompatibleDC(hDesktopDC);
		HBITMAP hBmpFileBitmap=CreateCompatibleBitmap(hDesktopDC,nWidth,nHeight);
		HBITMAP hOldBitmap = (HBITMAP) SelectObject(hBmpFileDC,hBmpFileBitmap);
		BitBlt(hBmpFileDC,0,0,nWidth,nHeight,hDesktopDC,0,0,SRCCOPY);
		SelectObject(hBmpFileDC,hOldBitmap);

		SaveBitmap(szFileName,hBmpFileBitmap);

		DeleteDC(hBmpFileDC);
		DeleteObject(hBmpFileBitmap);
		SetCursor(LoadCursor(NULL,IDC_ARROW));
	}__finally {
		DeleteDC(hDesktopDC);
	}
}

//******************************************************************************
//    Carrega um bitmap para a memoria
//******************************************************************************
HBITMAP Load32bppTgaFromMemory(const char* buffer, int bufSize, bool bPreMultiply) {
	if ( !buffer || bufSize <= 0)
		return NULL;

    int contador = 0;

	TGA_Header *header = (TGA_Header *)buffer;
	//DWORD dwRead = 0;
	//ReadFile(handle, & header, sizeof(header), & dwRead, NULL);

	if ( (header->IDLength != 0) || (header->ColorMapType != 0) || (header->ImageType != 2) || (header->PixelDepth != 32) || (header->ImageDescriptor != 8) ) {
        //CloseHandle(handle);
		return NULL;
    }

    BITMAPINFO bmp = { { sizeof(BITMAPINFOHEADER), header->Width, header->Height, 1, 32 } };

	void * pBits = NULL;

	HBITMAP hBmp = CreateDIBSection(NULL, &bmp, DIB_RGB_COLORS, &pBits, NULL, NULL);

    if ( hBmp == NULL ) {
		//CloseHandle(handle);
		return NULL;
    }

	memcpy(pBits, &buffer[sizeof *header], header->Width * header->Height * 4);
	//ReadFile(handle, pBits, header.Width * header.Height * 4, & dwRead, NULL);

    //CloseHandle(handle);

    if ( bPreMultiply ) {
		for (int y = 0; y < header->Height; y++) {
            BYTE * pPixel = (BYTE *) pBits + header->Width * 4 * y;

            for (int x = 0; x < header->Width; x++) {
                pPixel[0] = pPixel[0] * pPixel[3] / 255;
                pPixel[1] = pPixel[1] * pPixel[3] / 255;
                pPixel[2] = pPixel[2] * pPixel[3] / 255;

                pPixel += 4;
            }
        }
    }
	return hBmp;
}

//******************************************************************************
//    Carrega um bitmap para a memoria
//******************************************************************************
HBITMAP Load32bppTga(const TCHAR * pFileName, bool bPreMultiply) {
    HANDLE handle = CreateFile(pFileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if ( handle == INVALID_HANDLE_VALUE )
        return NULL;

    TGA_Header header;
	DWORD dwRead = 0;
	ReadFile(handle, & header, sizeof(header), & dwRead, NULL);

	if ( (header.IDLength!=0) || (header.ColorMapType!=0) || (header.ImageType!=2) || (header.PixelDepth!=32) || (header.ImageDescriptor!=8) ) {
        CloseHandle(handle);
        return NULL;
    }

    BITMAPINFO bmp = { { sizeof(BITMAPINFOHEADER), header.Width, header.Height, 1, 32 } };

    void * pBits = NULL;

	HBITMAP hBmp = CreateDIBSection(NULL, &bmp, DIB_RGB_COLORS, &pBits, NULL, NULL);

    if ( hBmp == NULL ) {
        CloseHandle(handle);
        return NULL;
    }

    ReadFile(handle, pBits, header.Width * header.Height * 4, & dwRead, NULL);

    CloseHandle(handle);

    if ( bPreMultiply ) {
        for (int y=0; y < header.Height; y++) {
            BYTE * pPixel = (BYTE *) pBits + header.Width * 4 * y;

            for (int x=0; x<header.Width; x++) {
                pPixel[0] = pPixel[0] * pPixel[3] / 255;
                pPixel[1] = pPixel[1] * pPixel[3] / 255;
                pPixel[2] = pPixel[2] * pPixel[3] / 255;

                pPixel += 4;
            }
        }
    }
	return hBmp;
}

//******************************************************************************
//    Desenha o bitmap com transparencia
//******************************************************************************
void AlphaDraw(HDC hDC, int x, int y, int width, int height, HBITMAP hBmp, double transparency) {
    HDC     hMemDC = CreateCompatibleDC(hDC);
    HGDIOBJ hOld   = SelectObject(hMemDC, hBmp);

    double alphad = (transparency/100)*0xFF;
    int alpha = (int) alphad;


    HBRUSH hBrush = CreateSolidBrush(RGB(0xFF, 0xFF, 0));
    SelectObject(hDC, hBrush);
    //Ellipse(hDC, x, y, width*2, height * 2);
    SelectObject(hDC, GetStockObject(WHITE_BRUSH));
    DeleteObject(hBrush);

    //BitBlt(hDC, x, y, width, height, hMemDC, 0, 0, SRCCOPY);

    BLENDFUNCTION pixelblend = { AC_SRC_OVER, 0, 255, AC_SRC_ALPHA };

    //AlphaBlend(hDC, x, y+height, width, height, hMemDC, 0, 0, width, height, pixelblend);

    BLENDFUNCTION blend50 = { AC_SRC_OVER, 0, alpha, 0 };

    AlphaBlend(hDC, x, y, width, height, hMemDC, 0, 0, width, height, blend50);

    SelectObject(hMemDC, hOld);
    DeleteObject(hMemDC);
}



//---------------------------------------------------------------------------
#pragma package(smart_init)
