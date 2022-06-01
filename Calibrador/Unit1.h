//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <jpeg.hpp>
#include "Spin.hpp"
#include <XPMan.hpp>
#include <Grids.hpp>
#include <ComCtrls.hpp>
//#include "PluginCommon.h"
#include "PluginUtils.h"
#include "PixelUnit.h"
//---------------------------------------------------------------------------
class TForm10 : public TForm
{
__published:	// IDE-managed Components
	TSpinEdit *SpinEdit2;
	TSpinEdit *SpinEdit3;
	TSpinEdit *SpinEdit6;
	TSpinEdit *SpinEdit7;
	TLabel *Label6;
	TPageControl *PageControl1;
	TTabSheet *TabSheet1;
	TLabel *Label17;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label18;
	TLabel *Label19;
	TLabel *Label5;
	TLabel *Label7;
	TLabel *Label8;
	TLabel *Label9;
	TPanel *Panel1;
	TButton *Button2;
	TSpinEdit *SpinEdit1;
	TMemo *Memo1;
	TComboBox *ComboBox1;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TCheckBox *modoDebug;
	TSpinEdit *SpinEdit4;
	TSpinEdit *SpinEdit5;
	TButton *Button1;
	TCheckBox *CheckBox3;
	TCheckBox *CheckBox4;
	TCheckBox *CheckBox5;
	TEdit *Edit1;
	TCheckBox *CheckBox6;
	TMemo *Memo2;
	TMemo *Memo3;
	TMemo *erroLog;
	TButton *Button5;
	TButton *Button6;
	TButton *Button7;
	TCheckBox *CheckBox8;
	TCheckBox *CheckBox9;
	TCheckBox *CheckBox10;
	TCheckBox *CheckBox7;
	TCheckBox *freezaVento;
	TCheckBox *anyDC;
	TCheckBox *CheckBox11;
	TTimer *Timer1;
	TTimer *Timer2;
	TTimer *timerVento;
	TTimer *writeTimer;
	TXPManifest *XPManifest1;
	TTimer *drawTimer;
	TTimer *Timer6;
	TTimer *Timer3;
	TTabSheet *TabSheet2;
	TStringGrid *StringGrid1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ComboBox1Change(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall Timer2Timer(TObject *Sender);
	void __fastcall timerVentoTimer(TObject *Sender);
	void __fastcall writeTimerTimer(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall drawTimerTimer(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Timer6Timer(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall Timer3Timer(TObject *Sender);
	void __fastcall Button7Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm10(TComponent* Owner);

	void WMHotKey(TMessage& Message);
	BEGIN_MESSAGE_MAP
	MESSAGE_HANDLER(WM_HOTKEY, TMessage, WMHotKey);
  END_MESSAGE_MAP(TComponent)
};
//---------------------------------------------------------------------------
extern PACKAGE TForm10 *Form10;
void tocarBeep();
void tocarBeep2();
void atualizar();
//---------------------------------------------------------------------------
#endif
