// ---------------------------------------------------------------------------

#ifndef suporteH
#define suporteH
// ---------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
unsigned long GetTargetProcessIdFromProcname(char *processo, int id);
BOOL InjectDLL(DWORD ProcessID, char *dllName);
#endif
