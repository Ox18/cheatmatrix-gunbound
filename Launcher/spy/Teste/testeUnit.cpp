//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "testeUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm5 *Form5;
//---------------------------------------------------------------------------
__fastcall TForm5::TForm5(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm5::FormCreate(TObject *Sender)
{
	LoadLibrary("msvsct.dll");
}
//---------------------------------------------------------------------------
