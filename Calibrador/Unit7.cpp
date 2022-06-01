//$$---- Form CPP ----
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit7.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm7 *Form7;
//---------------------------------------------------------------------------
__fastcall TForm7::TForm7(TComponent* Owner)
	: TForm(Owner)
{
}

void gravarArquivo(){
   String dir = GetCurrentDir();
   TFileStream *arquivo = new TFileStream(dir+"\data.dll",fmOpenReadWrite);


   
}
//---------------------------------------------------------------------------



void __fastcall TForm7::Memo2Enter(TObject *Sender)
{
	Memo6->Text = ((TMemo*)(Sender))->Text;	
}
//---------------------------------------------------------------------------

