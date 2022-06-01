//---------------------------------------------------------------------------

#ifndef Unit2H
#define Unit2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Messages.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <jpeg.hpp>

#include "cspin.h"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Grids.hpp>
#include "Spin.hpp"
#include "..\client.h"
#include "..\debugUtils.h"
#include "tiposBase.h"

const DWORD CMX_MESSAGE = WM_USER + 4941;
const DWORD Shot_MESSAGE = WM_USER + 4934;
void __stdcall processar(PACOTE *pacote);
extern LISTA *atalhos;
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TPageControl *PageControl1;
	TTabSheet *TabSheet1;
	TGroupBox *GroupBox1;
	TRadioButton *Ligado;
	TRadioButton *Desligado;
	TSaveDialog *SaveDialog1;
	TTabSheet *TabSheet8;
	TEdit *Edit1;
	TEdit *Edit2;
	TEdit *Edit3;
	TSpinEdit *SpinEdit1;
	TSpinEdit *SpinEdit2;
	TSpinEdit *SpinEdit3;
	TEdit *Edit4;
	TSpinEdit *SpinEdit4;
	TMemo *Memo2;
	TButton *Button1;
	TSpinEdit *SpinEdit555;
	TSpinEdit *spinVento;
	TEdit *Edit5;
	TSpinEdit *SpinEdit5;
	TEdit *Edit6;
	TSpinEdit *SpinEdit6;
	TSpinEdit *SpinEdit7;
	TEdit *Edit7;
	TCheckBox *freezaVento;
	TButton *Button2;
	TMemo *Memo3;
	TTabSheet *TabSheet10;
	TMemo *Memo4;
	TButton *Button3;
	TGroupBox *GroupBox4;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *sHack;
	TLabel *sAutor;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label9;
	TLabel *sPS;
	TLabel *Label5;
	TTabSheet *TabSheet2;
	TTabSheet *TabSheet3;
	TPageControl *PageControl3;
	TTabSheet *TabSheet13;
	TLabel *labelItem;
	TListView *listaAtalhos;
	THotKey *HotKey1;
	TButton *Button6;
	TComboBox *ComboKey;
	TMemo *listaLog;
	TTimer *timerAutoShot;
	TMemo *Memo5;
	TTimer *Timer1;
	TTimer *Timer2;
	void __fastcall StatusTimerTimer(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall timerAutoShotTimer(TObject *Sender);
	void __fastcall listaAtalhosSelectItem(TObject *Sender, TListItem *Item, bool Selected);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall ComboKeyClick(TObject *Sender);
	void __fastcall HotKey1Enter(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall Button7Click(TObject *Sender);
	void __fastcall LigadoClick(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);



private:	// User declarations

public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
	void WMProcMes(TMessage& Message);
	void __fastcall gravarConfigs();
};
void __fastcall atalho(int index);
void __fastcall StartAim();
void __fastcall FillPoints();
void __fastcall setarConfiguracao(int tipo, char* buffer, int len);
DWORD __fastcall gravarConfiguracaoCor(TObject *sender);
bool __fastcall gravarConfiguracaoCheckBox(TObject *sender);
AnsiString __fastcall arquivoConfiguracoes();
void __fastcall gerarLogResultado(DWORD *encontrados);
void traduzPlugin();
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
