//---------------------------------------------------------------------------

#ifndef HookUnitH
#define HookUnitH

#include "Unit6.h"


 /*
void emptyFunc();
PVOID GetAddress(char *lib, char *func);
void InsertIt(int index, PVOID func, PVOID callback, PVOID tranpolin);
bool JoinStrings(char *valor1, char *valor2, char *buffer);

extern char *DLLPath;
extern HMODULE DriverLib;

extern HANDLE HT1;
extern int DT1;

extern HMODULE npsch;
extern DWORD lasttime;

extern MyCreateSurface_Type OldCreateSurface;
extern MyFlip_Type OldFlip;
extern MyBlt_Type OldBlt;
extern MyBltFast_Type OldBltFast;

extern DWORD*	g_pCreateSurface;
extern DWORD*	g_pBltX;
extern DWORD*  pCreateSurface;

extern HANDLE CM_SCM;
extern HANDLE CM_SVC;

extern Tfunctionhook hooks[TOTALHOOKS];
extern bool hooked;

extern std::vector <TSurfaces> surfaces;

//---------------------------------------------------------------------------
//Mapping

typedef HANDLE (__stdcall *TcmOpenFileMappingA)(DWORD dwDesiredAccess, BOOL bInheritHandle, char * lpName);
typedef HANDLE (__stdcall *TcmMapViewOfFile)( HANDLE hFileMappingObject, DWORD dwDesiredAccess, DWORD dwFileOffsetHigh, DWORD dwFileOffsetLow, DWORD dwNumberOfBytesToMap);

extern TcmOpenFileMappingA cmOpenFileMappingA;
extern TcmMapViewOfFile cmMapViewOfFile;
extern HMODULE cmlib;

//---------------------------------------------------------------------------

void StartMapping();
TPoint __fastcall GetCameraPos();
double __fastcall IntToRadian(int valor);
void __fastcall FillPoints(HDC dc);
void SendName(char * nome2);
PVOID GetAddress(char *lib, char *func);
void InsertIt(int index, PVOID func, PVOID callback, PVOID tranpolin);
void RemoveIt(int index);
void LoadFunctions(void);
int GetIdOfInterface(DWORD interf);
void ZeroValue(int index);
BOOL IsDriverLoaded();

            */
//---------------------------------------------------------------------------
#endif
