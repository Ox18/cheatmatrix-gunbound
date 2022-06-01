//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Verificador.h"
#include "Sysutils.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm5 *Form5;

/*
SC_HANDLE CM_SCM;
SC_HANDLE CM_SVC;
BOOL IsDriverLoaded() {
    BOOL result = false;
    DWORD bytesNeeded = 0;
    LPDWORD pBytesNeeded = &bytesNeeded;
    LPBYTE pBuffer = NULL;
    SERVICE_STATUS_PROCESS *dave = NULL;

	char *buf = (char *)malloc(MAX_PATH);
    //AnsiString diretorio = ExtractFilePath(ParamStr(0))+"Common\\Configs.ini";

    __try {
		char *valor = "mscoren";
		//MessageBoxA(0, valor, "", 0);
		__try {
			{
                CM_SCM = OpenSCManager(NULL, NULL, SC_MANAGER_ENUMERATE_SERVICE|GENERIC_READ);
                if(CM_SCM) {
                    CM_SVC = OpenService(CM_SCM, valor, SERVICE_ALL_ACCESS);

                    BOOL result2 = QueryServiceStatusEx(CM_SVC, SC_STATUS_PROCESS_INFO, NULL, 0,	pBytesNeeded);

                    pBuffer = (LPBYTE)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, *pBytesNeeded);
                    result2 = QueryServiceStatusEx(CM_SVC, SC_STATUS_PROCESS_INFO, pBuffer, *pBytesNeeded, pBytesNeeded);
                    dave = (SERVICE_STATUS_PROCESS *)pBuffer;
                    result = (dave->dwCurrentState == SERVICE_RUNNING);

                    HeapFree(GetProcessHeap(), 0, pBuffer);

                    if(CM_SVC)
                        CloseServiceHandle(CM_SVC);
                    if(CM_SCM)
                        CloseServiceHandle(CM_SCM);
                }
            }
        }
        __finally {

        }
    }
    __finally {

    }
    return result;
}
*/

//---------------------------------------------------------------------------
__fastcall TForm5::TForm5(TComponent* Owner)
	: TForm(Owner)
{

}
//---------------------------------------------------------------------------
void __fastcall TForm5::Timer1Timer(TObject *Sender)
{
 /* if(IsDriverLoaded())  {
	  Panel1->Color = clGreen;
	  Panel1->Caption = "driver encontrado";

  }else {
	  Panel1->Color = clRed;
	  Panel1->Caption = "nada encontrado";
  }
  */
}
//---------------------------------------------------------------------------
void __fastcall TForm5::Panel1Click(TObject *Sender)
{
/*
	__try{
		 if(IsDriverLoaded())  {
			  Panel1->Color = clGreen;
			  Panel1->Caption = "driver encontrado";

		  }else {
			  Panel1->Color = clRed;
			  Panel1->Caption = "nada encontrado";
		  }
	  }__except(1){
      		MessageBox(0,"Erro!","Erro",0);
	  }
	  */

}
//---------------------------------------------------------------------------

void __fastcall TForm5::FormCreate(TObject *Sender)
{
	MessageBox(0,"teste","testando",0);
}
//---------------------------------------------------------------------------

