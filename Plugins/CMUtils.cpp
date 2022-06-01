//---------------------------------------------------------------------------

#pragma hdrstop

#include "CMUtils.h"
#include "CMStrUtils.h"
#include <windows.h>
#include <stdlib.h>

const char hexes[17] = "0123456789ABCDEF\0";
const char extras[21] = "ghijklmnopqrsutvwxyz\0";

bool MyExtractFilePath(char* valor, char *buffer)
{
		if(strlen(valor) == 0)
			 return false;

		int i = 0;
		for( i = (strlen(valor)-2); i > 0; i--)
		{
				if( char(valor[i]) == 92 )
				{
						//buffer = (char *)malloc(i+2);
						memcpy(buffer, valor, i+1);
						buffer[i+1] = 0;
						return true;
				}
		}
		memcpy(buffer, valor, i+1);
		return true;
}

void LowCaseNow(char * valor)
{
	 int j = CM_StrLen(valor);
	 for( int i = 0; i < j; i++ )
			valor[i] = tolower(valor[i]);
}

DWORD HexToInt(char *s)
{
	char c;
	DWORD Result = 0;

	int j = CM_StrLen(s);

	char *r = new char[j+1];
	//s = CM_LowerCase(s);
	LowCaseNow(s);

	for( int i = 0; i < j; i++ )
  {
      Result *= 16;
			switch (s[i])
			{
				case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				{
					 Result += (char(s[i]) - char('0'));
				} break;

				case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
				{
					 Result += (char(s[i]) - char('a') + 10);
				} break;

				default: break;
			}
  }
  return Result;
}

char *CMIntToHex(DWORD valor, int formato)
{
	 //const char Hexs[16] = {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};

	 char *result = new char[formato];
	 memset(result, 0x00, formato);

	 for(int j = formato-1; j >= formato; j--)
	 {
			 int i = valor % 16;
			 valor = valor / 16;
			 result[j] = hexes[i];
	 }

   return result;
}

typedef struct _triChar
{
	 char c1;
	 char c2;
	 char c3;
} TTriChar;

TTriChar formatChar(char valor)
{
	 //char *result = new char[3];
	 //memset(result, 0x00, 3);
	 
	 randomize();
	 //result = CMIntToHex((DWORD)valor, 3);
	 char res[3];
	 sprintf(res, "%x", (((DWORD)(valor)) & 0xFF));
	 if(res[0] == 0) res[0] = extras[random(19)];
	 if(res[1] == 0) res[1] = extras[random(19)];
	 if(res[2] == 0) res[2] = extras[random(19)];
	 TTriChar result;
	 result.c1 = res[0];
	 result.c2 = res[1];
	 result.c3 = res[2];
	 return result;
}

char *StringToHex(char *valor)
{
	 int j = CM_StrLen(valor);
	 int k = j*3;

	 char *result = new char[k];
	 //memset(result, 0x00, k);

	 for( int i = 0; i < j; i++)
	 {
			TTriChar val = formatChar(valor[i]);
			result[(i*3)] = val.c1;
			result[(i*3)+1] = val.c2;
			result[(i*3)+2] = val.c3;
	 }
}

BOOL charIN(char *valor, char c)
{
	int j=CM_StrLen(valor);
	bool flag = false;
	for(int i=0; i < j; i++ )
	{
		if( CM_LowCase(valor[i]) == CM_LowCase(c) )
          flag = true;
	}
	return flag;
}


char *HexToString(char *valor)
{
	 int j = CM_StrLen(valor);

	 char *s = new char[j];
	 memset(s, 0x00, j);

	 for( int i = 0; i < j; i++)
	 {
				if( charIN((char*)&hexes[0], valor[i])  )
					s[i] = valor[i];
        else
					s[i] = '0';
	 }

	 int i = 0;

	 char *result = new char[(j/3)+1];
	 memset(result, 0x00, (j/3)+1);

	 int k = 0;
	 for( int i = 0; i < j; i+=3)
	 {
			char atual[4];// = new char[3];

			//memset(atual, 0x00, 3);

			atual[0] = s[i];
			atual[1] = s[i+1];
			atual[2] = s[i+2];
			atual[3] = 0;


			result[k] = (HexToInt(atual) & 0xFF);
			k++;
	 }

	 return result;
}
//---------------------------------------------------------------------------
#pragma package(smart_init)
