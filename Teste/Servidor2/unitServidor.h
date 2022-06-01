//---------------------------------------------------------------------------

#ifndef unitServidorH
#define unitServidorH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "Spin.hpp"
//---------------------------------------------------------------------------
class TForm15 : public TForm
{
__published:	// IDE-managed Components
	TButton *Button1;
	TMemo *Memo1;
	TEdit *Edit1;
	TSpinEdit *porta;
	TSpinEdit *SpinEdit1;
	TSpinEdit *SpinEdit2;
	TSpinEdit *SpinEdit3;
	TSpinEdit *SpinEdit4;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm15(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm15 *Form15;
//---------------------------------------------------------------------------
#endif
