//---------------------------------------------------------------------------

#pragma hdrstop

#include "secret.h"
#include "funcoes.h"
//---------------------------------------------------------------------------

#pragma package(smart_init)

bool HideDll(HINSTANCE instanceDLL){
	 ULONG_PTR ldr_addr;
	 PEB_LDR_DATA_SECRET* ldr_data;
	 LDR_MODULE_SECRET  *modulo, *prec, *next;

	 __try
	 {
		   //The asm code is only for IA-32 architecture
		   __asm mov eax, fs:[0x30]  //get il PEB ADDR
		   __asm add eax, 0xc
		   __asm mov eax,[eax]	  //get LoaderData ADDR
		   __asm mov ldr_addr, eax

		   ldr_data = (PEB_LDR_DATA_SECRET*)ldr_addr;  //init PEB_LDR_DATA struct.

		   modulo = (LDR_MODULE_SECRET*)ldr_data->InLoadOrderModuleList.Flink;

		   while(modulo->BaseAddress != 0)
		   {
				 if( (ULONG_PTR)modulo->BaseAddress == (DWORD)instanceDLL)
				 {
					  if(modulo->InInitializationOrderModuleList.Blink == NULL) return false;

					  //Get the precedent and the successive struct according to the initialization order
					  prec = (LDR_MODULE_SECRET*)(ULONG_PTR)((ULONG_PTR)modulo->InInitializationOrderModuleList.Blink - 16);
					  next = (LDR_MODULE_SECRET*)(ULONG_PTR)((ULONG_PTR)modulo->InInitializationOrderModuleList.Flink - 16);

					  //And change values
					  prec->InInitializationOrderModuleList.Flink = modulo->InInitializationOrderModuleList.Flink;
					  next->InInitializationOrderModuleList.Blink = modulo->InInitializationOrderModuleList.Blink;

					  //Now change  InLoad e InMem normally
					  prec = (LDR_MODULE_SECRET*)modulo->InLoadOrderModuleList.Blink;
					  next = (LDR_MODULE_SECRET*)modulo->InLoadOrderModuleList.Flink;

					  //Precedent struct
					  prec->InLoadOrderModuleList.Flink = modulo->InLoadOrderModuleList.Flink;
					  prec->InMemoryOrderModuleList.Flink = modulo->InMemoryOrderModuleList.Flink;

					  //Successive struct
					  next->InLoadOrderModuleList.Blink = modulo->InLoadOrderModuleList.Blink;
					  next->InMemoryOrderModuleList.Blink = modulo->InMemoryOrderModuleList.Blink;

					  //Now if you want: memset(modulo,0,sizeof(LDR_MODULE));
					  return true;
				 }

				 modulo = (LDR_MODULE_SECRET*)modulo->InLoadOrderModuleList.Flink;
		   }
	 }

	 __except(EXCEPTION_EXECUTE_HANDLER)
	 {
			  return false;
	 }
}

int GetCurrentProcessName(char **nome) {
	ULONG_PTR ldr_addr;
	PPEB_LDR_DATA ldr_data;
	PLDR_DATA_TABLE_ENTRY LdMod;
	LPSTR psznameA;

	// The asm code is only for IA-32 architecture
	__asm {
		mov eax, fs:[0x30]  //get the PEB ADDR
		add eax, 0xc
		mov eax, [eax] // get LoaderData ADDR
		mov ldr_addr, eax
	}

	__try
	{
		 ldr_data = (PPEB_LDR_DATA)ldr_addr;
		 LdMod = (PLDR_DATA_TABLE_ENTRY)ldr_data->InMemoryOrderModuleList.Flink;
		 if(LdMod->DllBase != 0){
			 UnicodeToAnsi(LdMod->FullDllName.Buffer, nome);
			 return strlen(*nome);
		 }
	}

	__except(EXCEPTION_EXECUTE_HANDLER)
	{
			 return (DWORD)false;
	}

	return 0;
}


