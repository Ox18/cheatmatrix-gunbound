//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit12.h"
//#include "..\..\Plugins\Aimbot\injunit.h"
#include "injector.h"
#include "..\..\Plugins\debugutils.h"
#include "PixelUnit.h"
#include <stdio.h>
#include <stdlib.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm12 *Form12;
//---------------------------------------------------------------------------
__fastcall TForm12::TForm12(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm12::Timer1Timer(TObject *Sender)
{
	HDC dc = 0;
	__try {
		dc = CreateDC("DISPLAY", "", "", NULL);

		if (dc == 0)
			return;

		__try {
			int j = 0, k = 0;
			j = CMGetPixel(dc, Mouse->CursorPos.x, Mouse->CursorPos.y);
			Panel1->Color = j;
			//char teste[100];
			//int n = sprintf(teste, "[%X]         ", j);
			//extOutA(dc, 10, 10, teste, n);
			Edit2->Text = IntToHex(j, 8);
		}
		__except (1) {
		}
	}__finally {
		if (dc)
			DeleteDC(dc);
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm12::FormCreate(TObject *Sender)
{
	CMGetPixel(0, 0, 0);
	Edit1->Text = IntToStr(sistema->versao);

	LoadLibraryA("ddraw.dll");

	//char strSerialNumber[16];
	//memset(strSerialNumber, 0, 16);

	//BOOL bRet = GetHDSerialCode(strSerialNumber);

	//Memo1->Lines->Add(strSerialNumber);
	//if (bRet)
	//	sprintf(strSerialNumber, "SerialNumber=%s\n", strSerialNumber);
}
//---------------------------------------------------------------------------

void __fastcall TForm12::Button1Click(TObject *Sender)
{

	int id = GetProcessID(((AnsiString)Edit4->Text).c_str());
	if(id > 0){

		UnicodeString nomeDll = UnicodeString(GetCurrentDir()+"\\"+Edit3->Text+".dll");

		wchar_t *nomeDllW = nomeDll.w_str();

		size_t convertedChars = 0;
		size_t  sizeInBytes = ((nomeDll.Length() + 1) * 2);
		char *nomeDllC = (char *)malloc(sizeInBytes);
		wcstombs(nomeDllC,nomeDllW, sizeInBytes);

		if(!FileExists(nomeDll)){
			debugar("O arquivo não existe: %s", nomeDll.c_str());
		} else{
			//LoadLibraryA(nomeDllC);
			InjectDLL(id, nomeDllC );
		}
	}

}
//---------------------------------------------------------------------------

