//---------------------------------------------------------------------------

#ifndef unitClienteH
#define unitClienteH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "Spin.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm14 : public TForm
{
__published:	// IDE-managed Components
	TButton *Button1;
	TMemo *Memo1;
	TEdit *Edit1;
	TSpinEdit *porta;
	TButton *Button2;
	TSpinEdit *SpinEdit1;
	TSpinEdit *SpinEdit2;
	TSpinEdit *SpinEdit3;
	TSpinEdit *SpinEdit4;
	TButton *Button3;
	TTimer *Timer1;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall SpinEdit1Change(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall Edit1KeyUp(TObject *Sender, WORD &Key, TShiftState Shift);
private:	// User declarations
public:		// User declarations
	__fastcall TForm14(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm14 *Form14;
//---------------------------------------------------------------------------
#endif
