//---------------------------------------------------------------------------

#pragma hdrstop

#include "funcoes.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

HRESULT __fastcall UnicodeToAnsi(LPCOLESTR pszW, LPSTR* ppszA)
{

	ULONG cbAnsi, cCharacters;
	DWORD dwError;

	// If input is null then just return the same.
	if (pszW == NULL)
	{
		*ppszA = NULL;
		return NOERROR;
	}

	cCharacters = wcslen(pszW)+1;
	// Determine number of bytes to be allocated for ANSI string. An
	// ANSI string can have at most 2 bytes per character (for Double
	// Byte Character Strings.)
	cbAnsi = cCharacters*2;

	// Use of the OLE allocator is not required because the resultant
	// ANSI  string will never be passed to another COM component. You
	// can use your own allocator.
	*ppszA = (LPSTR) CoTaskMemAlloc(cbAnsi);
	if (NULL == *ppszA)
		return E_OUTOFMEMORY;

	// Convert to ANSI.
	if (0 == WideCharToMultiByte(CP_ACP, 0, pszW, cCharacters, *ppszA,
				  cbAnsi, NULL, NULL))
	{
		dwError = GetLastError();
		CoTaskMemFree(*ppszA);
		*ppszA = NULL;
		return HRESULT_FROM_WIN32(dwError);
	}

	return NOERROR;
}
