//---------------------------------------------------------------------------

#ifndef InjUnitH
#define InjUnitH

BOOL LoadDll(char *procName, char *dllName);
BOOL InjectByCC(char *procName, char *dllName);
BOOL InjectDLL(DWORD ProcessID, char *dllName);
unsigned long GetTargetProcessIdFromProcname(int amount, ...);
unsigned long GetTargetThreadIdFromProcname(char *procName);
//---------------------------------------------------------------------------
#endif
