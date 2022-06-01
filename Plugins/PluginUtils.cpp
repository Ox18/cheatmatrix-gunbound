//---------------------------------------------------------------------------


#pragma hdrstop

#include "PluginUtils.h"
#include <graphics.hpp>
#include <math.h>
#include "globals.h"

#define ROTL32(x,y) ((x<<y)|(x>>(32-y)))
#define ROTR32(x,y) ((x>>y)|(x<<(32-y)))
#define ROTL24(x,y) ((x<<y)|(x>>(24-y)))
#define ROTR24(x,y) ((x>>y)|(x<<(24-y)))
#define ROTL16(x,y) ((x<<y)|(x>>(16-y)))
#define ROTR16(x,y) ((x>>y)|(x<<(16-y)))
#define ROTL8(x,y) ((x<<y)|(x>>(8-y)))
#define ROTR8(x,y) ((x>>y)|(x<<(8-y)))

AnsiString getDiretorioSubplugins(){
	TCHAR TempPath[MAX_PATH]=TEXT("");;
	GetTempPath(MAX_PATH, TempPath);
	GetLongPathNameA(TempPath, TempPath, MAX_PATH);
	String result(TempPath);
	return result;
}

AnsiString getDiretorioSubpluginsShort(){
	TCHAR TempPath[MAX_PATH]=TEXT("");;
	GetTempPath(MAX_PATH, TempPath);
	String result(TempPath);
	return result;
}

long int __fastcall BinToInt(AnsiString Value)
{
  int iValueSize = 0;
	long int Result = 0;
  iValueSize = Value.Length();
  for(int i = iValueSize; i >= 1; i--)
		 if(Value[i] == '1') Result += (1 << (iValueSize - i));
	return Result;
}

AnsiString IntToByteArray(INT64 valor)
{
   BYTE x,y;
   AnsiString result = "";
	while( valor > 0 )
	{
		x = (valor & 0xFF );
		result = result + IntToHex(x, 2).c_str() + " ";

		if( valor > 0x0F )
		   valor = (valor >>= 8);
        else
		   valor = (valor >>= 4);
	}
	return result;
}

DWORD HexToInt(AnsiString s)
{
  char c;
  DWORD Result = 0;
  s = s.UpperCase();
  for( BYTE i = 1; i <= s.Length(); i++ )
  {
    Result *= 16;
	switch (s[i])
	{
	  case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
	  {
		 Result += (BYTE(s[i]) - BYTE('0'));
	  } break;

	  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	  {
	     Result += (BYTE(s[i]) - BYTE('A') + 10);
	  } break;

	  default: break;
	}
  }
  return Result;
}

BOOL IsDigit(char valor)
{
   BOOL res = (BYTE(valor) > 47 && BYTE(valor) < 48);
   return res;
}

BOOL __fastcall isValidHexChar(char valor)
{
   valor = UpCase(valor);
   BOOL result = false;
   switch(valor)
   {
	  case '0': case '1': case '2': case '3': case '4': case '5': case
	  '6': case '7': case '8': case '9': case 'A': case 'B': case 'C':
	  case 'D': case 'E': case 'F': result = true; break;
	  default: break;
   }
   return result;
}

BOOL __fastcall isValidDecChar(char valor)
{
   BOOL result = false;
   switch(valor)
   {
	  case '0': case '1': case '2': case '3': case '4': case '5': case '6':
	  case '7': case '8': case '9': result = true; break;
	  default: break;//return false;
   }
   return result;
}

BOOL __fastcall isValidFloatChar(char valor)
{
   valor = UpCase(valor);
   BOOL result = false;
   switch(valor)
   {
	  case '0': case '1': case '2': case '3': case '4': case '5': case '6':
	  case '7': case '8': case '9': case '.': case '-': result = true;
	  default: break;
   }
   return result;
}

BOOL __fastcall isValidScanChar(char valor, BOOL isHexa)
{

	if(isHexa)
	  return (isValidHexChar(valor) || valor == '?' || valor == '*');
	else
	  return (isValidDecChar(valor) || valor == '?' || valor == '*');
}

void decriptar(char * buffer, int sizeTotal, char *filename)
{
	int encode = 0;
	unsigned key = decriptKey;
	srand(key);
	FILE* out;
	char tempfilename[9999]=TEXT("");
	sprintf(tempfilename,"%s",filename);
	if ((out = fopen(tempfilename, "wb")) == NULL) printf("ERROR: Could not open: %s\n",tempfilename);
	else
	{
		unsigned block;
		int count = 0;
		int magnitude, polarity;
		int a, b, c, d;
		while(true)
		{
			if(count < sizeTotal)
				a = buffer[count++] & 0xFF;
			else{
				a = EOF;
				break;
			}

			if(count < sizeTotal)
				b = buffer[count++] & 0xFF;
			else{
				b = EOF;
				break;
			}

			if(count < sizeTotal)
				c = buffer[count++] & 0xFF;
			else{
				c = EOF;
				break;
			}

			if(count < sizeTotal)
				d = buffer[count++] & 0xFF;
			else{
				d = EOF;
				break;
			}

			polarity = rand()%2 & 0xFF;
			magnitude = rand()%32 & 0xFF;
			block = ((d<<24) | (c<<16) |(b<<8) | a);
			if (polarity) block = ROTR32(block,magnitude);
			else block = ROTL32(block,magnitude);
			block ^= ((rand()%256<<24) | (rand()%256<<16) | (rand()%256<<8) | rand()%256);
			putc(block,out);
			putc(block=block>>8,out);
			putc(block=block>>8,out);
			putc(block=block>>8,out);
		}
		if (a != EOF && b != EOF && c != EOF && d == EOF)
		{
			polarity = rand()%2;
			magnitude = rand()%24;
			block = ((c<<16) |(b<<8) | a);
			if (polarity) block = ROTR24(block,magnitude);
			else block = ROTL24(block,magnitude);
			block ^= ((rand()%256<<16) | (rand()%256<<8) | rand()%256);
			putc(block,out);
			putc(block=block>>8,out);
			putc(block=block>>8,out);
		}
		else if (a != EOF && b != EOF && c == EOF)
		{
			polarity = rand()%2;
			magnitude = rand()%16;
			block = ((b<<8) | a);
			if (polarity) block = ROTR16(block,magnitude);
			else block = ROTL16(block,magnitude);
			block ^= ((rand()%256<<8) | rand()%256);
			putc(block,out);
			putc(block=block>>8,out);
		}
		else if (a != EOF && b == EOF)
		{
			polarity = rand()%2;
			magnitude = rand()%8;
			block = (a);
			if (polarity) block = ROTR8(block,magnitude);
			else block = ROTL8(block,magnitude);
			block ^= (rand()%256);
			putc(block,out);
		}
		//fclose(in);
		fclose(out);
		//SetFileAttributes(tempfilename, FILE_ATTRIBUTE_HIDDEN);

		//remove(filename);
		//rename(tempfilename,filename);
	}
}

//---------------------------------------------------------------------------

#pragma package(smart_init)
