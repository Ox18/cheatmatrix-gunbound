//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "unitCliente.h"

#include "client.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Spin"
#pragma resource "*.dfm"
TForm14 *Form14;
//---------------------------------------------------------------------------
__fastcall TForm14::TForm14(TComponent* Owner)
	: TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TForm14::Button1Click(TObject *Sender) {
	 enviar(0, Edit1->Text.c_str(), Edit1->Text.Length());
	 return; //success
}
//---------------------------------------------------------------------------
void __fastcall TForm14::FormCreate(TObject *Sender){
	inicializar();
}
//---------------------------------------------------------------------------

void __fastcall TForm14::Button2Click(TObject *Sender) {
	if(!conectado){
		conectar(porta->Value);
		Button2->Caption = "Desconectar";
	}else{
		desconectar();
		Button2->Caption = "Conectar";
    }

}
//---------------------------------------------------------------------------

void __fastcall TForm14::SpinEdit1Change(TObject *Sender)
{
  TesteStruct teste;
  teste.valor1 = (DWORD)SpinEdit1->Value;
  teste.valor2 = (int)SpinEdit2->Value;
  teste.valor3 = (WORD)SpinEdit3->Value;
  memset(&teste.valor4[0], 0, sizeof teste.valor4);
  memcpy(&teste.valor4[0], Edit1->Text.c_str(), Edit1->Text.Length());
  //Send the message to the server
  enviar(0, (char*)&teste, sizeof teste);
}
//---------------------------------------------------------------------------



int total = 0;
void __fastcall TForm14::Timer1Timer(TObject *Sender)
{
	if(conectado){
		//randomize();
		total++;

		TesteStruct teste;
		teste.valor1 = (DWORD)total;

		int nBytesSent;
		int nBytesRecv;

		//Send the message to the server
		enviar(0, (char*)&teste, sizeof teste);
  }

}
//---------------------------------------------------------------------------

void __fastcall TForm14::FormDestroy(TObject *Sender)
{
	terminar();
}
//---------------------------------------------------------------------------

void __fastcall TForm14::Edit1KeyUp(TObject *Sender, WORD &Key, TShiftState Shift)

{
	SpinEdit1Change(NULL);
}
//---------------------------------------------------------------------------

