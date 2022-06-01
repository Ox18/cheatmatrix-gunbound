//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "unitServidor.h"
#include "server.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Spin"
#pragma resource "*.dfm"

TForm15 *Form15;

//---------------------------------------------------------------------------
__fastcall TForm15::TForm15(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TForm15::FormCreate(TObject *Sender) {
	conectar(porta->Value);
}
//---------------------------------------------------------------------------

void __fastcall TForm15::FormDestroy(TObject *Sender) {
	terminar();
}
//---------------------------------------------------------------------------
