//---------------------------------------------------------------------------

#pragma hdrstop

#include <tchar.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "..\globals.h"

#define ROTL32(x,y) ((x<<y)|(x>>(32-y)))
#define ROTR32(x,y) ((x>>y)|(x<<(32-y)))
#define ROTL24(x,y) ((x<<y)|(x>>(24-y)))
#define ROTR24(x,y) ((x>>y)|(x<<(24-y)))
#define ROTL16(x,y) ((x<<y)|(x>>(16-y)))
#define ROTR16(x,y) ((x>>y)|(x<<(16-y)))
#define ROTL8(x,y) ((x<<y)|(x>>(8-y)))
#define ROTR8(x,y) ((x>>y)|(x<<(8-y)))
//---------------------------------------------------------------------------

void XOR32(char * filename, unsigned key, int encode)
{
	srand(key);
	FILE * in, * out;
	char tempfilename[9999];
	sprintf(tempfilename,"%s%s",filename,".temp.dll");
	if ((in = fopen(filename, "rb")) == NULL)
		printf("ERROR: Could not open: %s\n",filename);
	else if ((out = fopen(tempfilename, "wb")) == NULL)
		printf("ERROR: Could not open: %s\n",tempfilename);
	else
	{
		unsigned block;
		int a, b, c, d, magnitude, polarity;
		if (encode)
		{
			while((a = fgetc(in)) != EOF && (b = fgetc(in)) != EOF && (c = fgetc(in)) != EOF && (d = fgetc(in)) != EOF)
			{
				polarity = rand()%2;
				magnitude = rand()%32;
				block = ((d<<24) | (c<<16) |(b<<8) | a);
				block ^= ((rand()%256<<24) | (rand()%256<<16) | (rand()%256<<8) | rand()%256);
				if (polarity) block = ROTL32(block,magnitude);
				else block = ROTR32(block,magnitude);
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
				block ^= ((rand()%256<<16) | (rand()%256<<8) | rand()%256);
				if (polarity) block = ROTL24(block,magnitude);
				else block = ROTR24(block,magnitude);
				putc(block,out);
				putc(block=block>>8,out);
				putc(block=block>>8,out);
			}
			else if (a != EOF && b != EOF && c == EOF)
			{
				polarity = rand()%2;
				magnitude = rand()%16;
				block = ((b<<8) | a);
				block ^= ((rand()%256<<8) | rand()%256);
				if (polarity) block = ROTL16(block,magnitude);
				else block = ROTR16(block,magnitude);
				putc(block,out);
				putc(block=block>>8,out);
			}
			else if (a != EOF && b == EOF)
			{
				polarity = rand()%2;
				magnitude = rand()%8;
				block = (a);
				block ^= (rand()%256);
				if (polarity) block = ROTL8(block,magnitude);
				else block = ROTR8(block,magnitude);
				putc(block,out);
			}
		}
		else
		{
			while((a = fgetc(in)) != EOF && (b = fgetc(in)) != EOF && (c = fgetc(in)) != EOF && (d = fgetc(in)) != EOF)
			{
				polarity = rand()%2;
				magnitude = rand()%32;
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
		}
		fclose(in);
		fclose(out);
		//remove(filename);
		//rename(tempfilename,filename);
	}
	printf("done!\n");
}

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
	   /*
    printf("x0r 32-bit File Encryption\n\n");
	if (argc==1) printf("ERROR: No file(s) supplied.\n");
	else
	{
		char ed[9999];
		char keys[9999];
		printf("List:\n");
		for (int n=1;n<argc;n++) printf("[%u] %s\n",n,argv[n]);
		printf("\nKey [0-%u]: ", pow(2,sizeof(unsigned)*8)-1);
		gets(keys);
		do
		{
			printf("\n[e]ncrypt [d]ecrypt?\n");
			gets(ed);
		} while (strcmp(ed,"e") != 0 && strcmp(ed,"d") != 0);
		printf("\nOne moment please. Processing...\nTo cancel, press ctrl + c\n\nStatus:\n");
		for (char * key = strtok(keys," "); key; key = strtok(NULL," "))
			for (int n=1;n<argc;n++) {	printf("[%u] %s...",n,argv[n]); XOR32(argv[n], decriptKey, (strcmp(ed,"e")?0:1)); }
	}
	system("pause");
	return 0;
            */
    if(argc >= 3){
		XOR32(argv[1],decriptKey,1);
	}else{
		//int key = decriptKey;
		//XOR32("H:\\Programacao\\MeusProjetos\\Cheating_Matrix\\Compiled\\Plugins\\subplugins\\spi.dll",key,true);
    }
	return 0;
}
//---------------------------------------------------------------------------
