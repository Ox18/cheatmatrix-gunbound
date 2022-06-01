//** Injector.h: Header file
//**

DWORD GetProcessID(char* strProcessName);	
BOOL InjectDLL(DWORD dwPID, char* strHookDLL);		//** For single injection
BOOL GloballyInjectDLL(char* strHookDLL);			//** For global injection
BOOL WriteProcessBytes(HANDLE hProcess, LPVOID lpBaseAddress, LPCVOID lpBuffer, SIZE_T nSize);
