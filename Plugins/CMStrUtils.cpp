//---------------------------------------------------------------------------

#pragma hdrstop

#include "CMStrUtils.h"

int __fastcall CM_StrLen(char *valor)
{
	 if(!valor)
		 return 0;
		 
	 for(int i = 0; i < MAX_PATH; i++)
	 {
			 if(valor[i] == 0)
				 return i;
	 }

	 return 0;
}

bool __fastcall CM_StrCat(char *valor1, char *valor2, char *buffer)
{
		memcpy(buffer, valor1, strlen(valor1));
		buffer[strlen(valor1)] = 0;
		strcat(buffer, valor2);
		return true;
}

char __fastcall CM_UpCase(char valor)
{
		if ((valor >= 'a') && (valor <= 'z'))
				valor -= 32;

		return valor;
}

char __fastcall CM_LowCase(char valor)
{
		if ((valor >= 'A') && (valor <= 'Z'))
				valor += 32;
				
		return valor;
}

char * __fastcall CM_UpperCase(char *valor)
{
		int j = CM_StrLen(valor);

		char *result = new char[j];
    memset(result, 0x00, j);
		
		for(int i = 0; i < j; i++)
		{
				if ((valor[i] >= 'a') && (valor[i] <= 'z'))
						result[i] = valor[i] - 32;
				else
            result[i] = valor[i];
		}
		return result;
}

char * __fastcall CM_LowerCase(char *valor)
{
		int j = CM_StrLen(valor);

		char *result = new char[j];
		memset(result, 0x00, j);

		for(int i = 0; i < j; i++)
		{
				if ((valor[i] >= 'A') && (valor[i] <= 'Z'))
						result[i] = valor[i] + 32;
				else
						result[i] = valor[i];
		}
		return result;
}

wchar_t * __fastcall CM_StringToWideChar(char *valor)
{
    wchar_t w;
		int j = ((CM_StrLen(valor)*(sizeof w)) + (sizeof w));
		int k = CM_StrLen(valor);
		
		wchar_t *result = new wchar_t[j];
		memset(result, 0x00, j);

		mbstowcs(result, valor, k);

		return result;
}



//---------------------------------------------------------------------------
#pragma package(smart_init)
