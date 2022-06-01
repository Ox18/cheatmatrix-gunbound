//---------------------------------------------------------------------------

#ifndef Unit12H
#define Unit12H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include "Spin.hpp"

//---------------------------------------------------------------------------
class TForm12 : public TForm
{
__published:	// IDE-managed Components
	TTimer *Timer1;
	TTrackBar *trackFV;
	TLabel *valorFVento;
	TLabel *Label1;
	TTrackBar *trackAV;
	TLabel *Label4;
	TLabel *valorAVento;
	TTrackBar *trackFB;
	TLabel *valorFBot;
	TLabel *Label3;
	TTrackBar *trackAB;
	TLabel *Label5;
	TLabel *valorABot;
	TShape *Shape1;
	TComboBox *comboMobiles;
	TTrackBar *trackTiro;
	TLabel *Label2;
	TLabel *valorTiro;
	TTimer *Timer2;
	TLabel *Label6;
	TLabel *valorFator;
	TSpinEdit *SpinEdit1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Timer2Timer(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm12(TComponent* Owner);

	void __fastcall AtualizarValores();
	void __stdcall Desenhar(HDC dc, char tipoDesenho);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm12 *Form12;
//---------------------------------------------------------------------------
#endif
