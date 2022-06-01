#include <windows.h>
#pragma hdrstop

#include <tchar.h>
#include "debugUtils.h"

typedef struct ATALHO {
	unsigned long pluginID;
	int id;
	char mods;
	short tecla;
}ATALHO;
