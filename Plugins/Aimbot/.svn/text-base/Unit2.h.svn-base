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
#include "estruturaBot.h"
#include "tiposBase.h"

const DWORD CMX_MESSAGE = WM_USER + 4941;
const DWORD Shot_MESSAGE = WM_USER + 4934;
void __stdcall processar(PACOTE *pacote);
extern LISTA *atalhos;
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TTimer *ProcuraJanelaTimer;
	TTimer *StatusTimer;
	TTimer *AtualizaBotTimer;
	TTimer *injetaTimer;
	TPageControl *PageControl1;
	TTabSheet *TabSheet1;
	TGroupBox *GroupBox3;
	TRadioButton *radioPerto;
	TRadioButton *radioLonge;
	TRadioButton *radioSlice;
	TRadioButton *radioPersonalizado;
	TRadioButton *radioRandom;
	TRadioButton *radioMouse;
	TGroupBox *GroupBox1;
	TRadioButton *Ligado;
	TRadioButton *Desligado;
	TTabSheet *TabSheet3;
	TTabSheet *TabSheet2;
	TLabel *Label12;
	TTabSheet *TabSheet4;
	TMemo *Memo1;
	TSaveDialog *SaveDialog1;
	TTimer *tornadoTimer;
	TTabSheet *TabSheet7;
	TLabel *Label1;
	TTabSheet *TabSheet8;
	TEdit *Edit1;
	TEdit *Edit2;
	TEdit *Edit3;
	TTimer *Timer1;
	TSpinEdit *SpinEdit1;
	TSpinEdit *SpinEdit2;
	TSpinEdit *SpinEdit3;
	TTimer *Timer2;
	TEdit *Edit4;
	TSpinEdit *SpinEdit4;
	TTabSheet *TabSheet9;
	TCheckBox *TE_ASate;
	TCheckBox *Marcar_SS;
	TCheckBox *dbgCHK;
	TMemo *Memo2;
	TButton *Button1;
	TSpinEdit *SpinEdit555;
	TTimer *timerAutoShot;
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
	TTimer *Timer4;
	TTabSheet *TabSheet10;
	TMemo *Memo4;
	TButton *Button3;
	TMemo *Memo5;
	TCheckBox *TC_BigFoot;
	TCheckBox *TC_Mage;
	TCheckBox *TC_Trico;
	TCheckBox *TE_Nak;
	TCheckBox *TC_Grub;
	TCheckBox *TC_Kasildon;
	TCheckBox *TC_Dragao;
	TCheckBox *TC_Maya;
	TCheckBox *TC_Raon;
	TCheckBox *TC_frank;
	TPageControl *PageControl3;
	TTabSheet *TabSheet11;
	TLabel *Label15;
	TLabel *Label28;
	TLabel *Label26;
	TLabel *Label35;
	TLabel *Label13;
	TColorBox *corLinha1;
	TComboBox *amostragem;
	TComboBox *espessuraLinha;
	TColorBox *corLinha2;
	TColorBox *corEspelho;
	TTabSheet *TabSheet12;
	TCheckBox *graficoAntigo;
	TButton *Button4;
	TButton *Button5;
	TTabSheet *TabSheet13;
	TListView *listaAtalhos;
	THotKey *HotKey1;
	TButton *Button6;
	TLabel *labelItem;
	TComboBox *ComboKey;
	TBevel *Bevel3;
	TButton *Button7;
	TGroupBox *GroupBox4;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *sHack;
	TLabel *sAutor;
	TLabel *Label2;
	TLabel *Label3;
	TCheckBox *TC_Turtle;
	TLabel *Label9;
	TLabel *Label14;
	TColorBox *corTornado;
	TGroupBox *GroupBox2;
	TLabel *Label31;
	TLabel *Label32;
	TLabel *Label4;
	TLabel *Label10;
	TButton *Button8;
	TLabel *sPS;
	TLabel *Label5;
	TCheckBox *modoJudas;
	TTabSheet *TabSheet5;
	TMemo *Memo6;
	void __fastcall ProcuraJanelaTimerTimer(TObject *Sender);
	void __fastcall StatusTimerTimer(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall injetaTimerTimer(TObject *Sender);
	void __fastcall linhaTornadoClick(TObject *Sender);
	void __fastcall Timer2Timer(TObject *Sender);
	void __fastcall timerAutoShotTimer(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall listaAtalhosSelectItem(TObject *Sender, TListItem *Item, bool Selected);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall ComboKeyClick(TObject *Sender);
	void __fastcall HotKey1Enter(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall Button7Click(TObject *Sender);
	void __fastcall radioMouseClick(TObject *Sender);
	void __fastcall modoJudasClick(TObject *Sender);
	void __fastcall LigadoClick(TObject *Sender);
	void __fastcall graficoAntigoClick(TObject *Sender);
	void __fastcall TC_BigFootClick(TObject *Sender);
	void __fastcall TC_MageClick(TObject *Sender);
	void __fastcall TC_TricoClick(TObject *Sender);
	void __fastcall TC_TurtleClick(TObject *Sender);
	void __fastcall TC_GrubClick(TObject *Sender);
	void __fastcall TC_KasildonClick(TObject *Sender);
	void __fastcall TC_MayaClick(TObject *Sender);
	void __fastcall TC_DragaoClick(TObject *Sender);
	void __fastcall TC_RaonClick(TObject *Sender);
	void __fastcall TC_frankClick(TObject *Sender);
	void __fastcall TE_NakClick(TObject *Sender);
	void __fastcall TE_ASateClick(TObject *Sender);
	void __fastcall Marcar_SSClick(TObject *Sender);
	void __fastcall dbgCHKClick(TObject *Sender);
	void __fastcall corLinha1Change(TObject *Sender);
	void __fastcall corLinha2Change(TObject *Sender);
	void __fastcall corEspelhoChange(TObject *Sender);
	void __fastcall corTornadoChange(TObject *Sender);
	void __fastcall amostragemChange(TObject *Sender);
	void __fastcall espessuraLinhaChange(TObject *Sender);



private:	// User declarations

public:		// User declarations
	__fastcall TForm2(TComponent* Owner);
	void WMProcMes(TMessage& Message);
	void processaHotKey(int id);
	void __fastcall gravarConfigs();
	char __fastcall getModoMira();
	void __fastcall mudarModoMira(char indice);

	DWORD SPY_MESSAGE;

	void WMHotKey(TMessage& Message);

  BEGIN_MESSAGE_MAP
	MESSAGE_HANDLER(WM_HOTKEY, TMessage, WMHotKey);
	//MESSAGE_HANDLER(HWND_BROADCAST, TMessage, DefaultHandler2);
  END_MESSAGE_MAP(TComponent)
};
void __fastcall atalho(int index);
void __fastcall StartAim();
void __fastcall FillPoints();
void __fastcall setarConfiguracao(int tipo, char* buffer, int len);
DWORD __fastcall gravarConfiguracaoCor(TObject *sender);
bool __fastcall gravarConfiguracaoCheckBox(TObject *sender);
AnsiString __fastcall arquivoConfiguracoes();
void traduzPlugin();
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
