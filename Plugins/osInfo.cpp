#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#include <strsafe.h>
#include "osInfo.h"

#pragma comment(lib, "User32.lib")

#define BUFSIZE 256

typedef void(WINAPI * PGNSI)(LPSYSTEM_INFO);
typedef BOOL(WINAPI * PGPI)(DWORD, DWORD, DWORD, DWORD, PDWORD);

//
// Vers�o do sistema operacional
// Fonte: http://msdn.microsoft.com/en-us/library/ms724429%28VS.85%29.aspx
OS_INFO GetOS() {
	OSVERSIONINFOEX osvi;
	SYSTEM_INFO si;
	PGNSI pGNSI;
	PGPI pGPI;
	BOOL bOsVersionInfoEx;
	DWORD dwType;
	OS_INFO resultSys;
	resultSys.OS = WIN_DESCONHECIDO;
	resultSys.arquitetura = SYS_32;

	ZeroMemory(&si, sizeof(SYSTEM_INFO));
	ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));

	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

	if (!(bOsVersionInfoEx = GetVersionEx((OSVERSIONINFO*) & osvi)))
		return resultSys;

	// Call GetNativeSystemInfo if supported or GetSystemInfo otherwise.

	pGNSI = (PGNSI)GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "GetNativeSystemInfo");
	if (NULL != pGNSI)
		pGNSI(&si);
	else
		GetSystemInfo(&si);

	resultSys.versao = (osvi.dwMajorVersion*10000 + osvi.dwMinorVersion*100 + osvi.wProductType);
	resultSys.info = osvi;

	if (VER_PLATFORM_WIN32_NT == osvi.dwPlatformId && osvi.dwMajorVersion > 4) {
		if (osvi.dwMajorVersion == 6) {
			if (osvi.dwMinorVersion == 0) {
				if (osvi.wProductType == VER_NT_WORKSTATION)
					resultSys.OS = WIN_VISTA;
				else
					resultSys.OS = WIN_SERVER_2008;
			}

			if (osvi.dwMinorVersion == 1) {
				if (osvi.wProductType == VER_NT_WORKSTATION)
					resultSys.OS = WIN_7;
				else
					resultSys.OS = WIN_SERVER_2008; //_R2;
			}
		   /*
			pGPI = (PGPI)GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "GetProductInfo");

			pGPI(osvi.dwMajorVersion, osvi.dwMinorVersion, 0, 0, &dwType);

			switch(dwType) {
			case PRODUCT_ULTIMATE:
				StringCchCat(pszOS, BUFSIZE, TEXT("Ultimate Edition"));
				break;
			case PRODUCT_PROFESSIONAL:
				StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
				break;
			case PRODUCT_HOME_PREMIUM:
				StringCchCat(pszOS, BUFSIZE, TEXT("Home Premium Edition"));
				break;
			case PRODUCT_HOME_BASIC:
				StringCchCat(pszOS, BUFSIZE, TEXT("Home Basic Edition"));
				break;
			case PRODUCT_ENTERPRISE:
				StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
				break;
			case PRODUCT_BUSINESS:
				StringCchCat(pszOS, BUFSIZE, TEXT("Business Edition"));
				break;
			case PRODUCT_STARTER:
				StringCchCat(pszOS, BUFSIZE, TEXT("Starter Edition"));
				break;
			case PRODUCT_CLUSTER_SERVER:
				StringCchCat(pszOS, BUFSIZE, TEXT("Cluster Server Edition"));
				break;
			case PRODUCT_DATACENTER_SERVER:
				StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition"));
				break;
			case PRODUCT_DATACENTER_SERVER_CORE:
				StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition (core installation)"));
				break;
			case PRODUCT_ENTERPRISE_SERVER:
				StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
				break;
			case PRODUCT_ENTERPRISE_SERVER_CORE:
				StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition (core installation)"));
				break;
			case PRODUCT_ENTERPRISE_SERVER_IA64:
				StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition for Itanium-based Systems"));
				break;
			case PRODUCT_SMALLBUSINESS_SERVER:
				StringCchCat(pszOS, BUFSIZE, TEXT("Small Business Server"));
				break;
			case PRODUCT_SMALLBUSINESS_SERVER_PREMIUM:
				StringCchCat(pszOS, BUFSIZE, TEXT("Small Business Server Premium Edition"));
				break;
			case PRODUCT_STANDARD_SERVER:
				StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition"));
				break;
			case PRODUCT_STANDARD_SERVER_CORE:
				StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition (core installation)"));
				break;
			case PRODUCT_WEB_SERVER:
				StringCchCat(pszOS, BUFSIZE, TEXT("Web Server Edition"));
				break;
			} */
		}

		if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2) {
			if (GetSystemMetrics(SM_SERVERR2))
				resultSys.OS = WIN_SERVER_2003; //_R2;
			else if (osvi.wSuiteMask & VER_SUITE_STORAGE_SERVER)
				resultSys.OS = WIN_SERVER_2003; //StringCchCat(pszOS, BUFSIZE, TEXT("Windows Storage Server 2003"));
			else if (osvi.wSuiteMask & VER_SUITE_WH_SERVER)
				resultSys.OS = WIN_SERVER_2003; //StringCchCat(pszOS, BUFSIZE, TEXT("Windows Home Server"));
			else if (osvi.wProductType == VER_NT_WORKSTATION && si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64) {
				resultSys.OS = WIN_XP;//_64; //StringCchCat(pszOS, BUFSIZE, TEXT("Windows XP Professional x64 Edition"));
			}
			else
				resultSys.OS = WIN_SERVER_2003;
			/*
			// Test for the server type.
			if (osvi.wProductType != VER_NT_WORKSTATION) {
				if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64) {
					if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
						StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition for Itanium-based Systems"));
					else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
						StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition for Itanium-based Systems"));
				}

				else if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64) {
					if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
						StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter x64 Edition"));
					else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
						StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise x64 Edition"));
					else
						StringCchCat(pszOS, BUFSIZE, TEXT("Standard x64 Edition"));
				}

				else {
					if (osvi.wSuiteMask & VER_SUITE_COMPUTE_SERVER)
						StringCchCat(pszOS, BUFSIZE, TEXT("Compute Cluster Edition"));
					else if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
						StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Edition"));
					else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
						StringCchCat(pszOS, BUFSIZE, TEXT("Enterprise Edition"));
					else if (osvi.wSuiteMask & VER_SUITE_BLADE)
						StringCchCat(pszOS, BUFSIZE, TEXT("Web Edition"));
					else
						StringCchCat(pszOS, BUFSIZE, TEXT("Standard Edition"));
				}
			}  */
		}

		if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1) {
        	if (osvi.wProductType == VER_NT_SERVER)
				resultSys.OS = WIN_SERVER;
			else
				resultSys.OS = WIN_XP;

			//StringCchCat(pszOS, BUFSIZE, TEXT("Windows XP "));
			/*if (osvi.wSuiteMask & VER_SUITE_PERSONAL)
				StringCchCat(pszOS, BUFSIZE, TEXT("Home Edition"));
			else
				StringCchCat(pszOS, BUFSIZE, TEXT("Professional")); */
		}

		if (osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0) {
			resultSys.OS = WIN_2000; //StringCchCat(pszOS, BUFSIZE, TEXT("Windows 2000 "));
			/*
			if (osvi.wProductType == VER_NT_WORKSTATION) {
				StringCchCat(pszOS, BUFSIZE, TEXT("Professional"));
			}
			else {
				if (osvi.wSuiteMask & VER_SUITE_DATACENTER)
					StringCchCat(pszOS, BUFSIZE, TEXT("Datacenter Server"));
				else if (osvi.wSuiteMask & VER_SUITE_ENTERPRISE)
					StringCchCat(pszOS, BUFSIZE, TEXT("Advanced Server"));
				else
					StringCchCat(pszOS, BUFSIZE, TEXT("Server"));
			}  */
		}

		// Include service pack (if any) and build number.
		 /*
		if (_tcslen(osvi.szCSDVersion) > 0) {
			StringCchCat(pszOS, BUFSIZE, TEXT(" "));
			StringCchCat(pszOS, BUFSIZE, osvi.szCSDVersion);
		}

		TCHAR buf[80];

		StringCchPrintf(buf, 80, TEXT(" (build %d)"), osvi.dwBuildNumber);
		StringCchCat(pszOS, BUFSIZE, buf); */

		if (osvi.dwMajorVersion >= 6) {
			if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64)
				resultSys.arquitetura = SYS_64;
			else if (si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_INTEL)
				resultSys.arquitetura = SYS_32;
		}

		return resultSys;
	}

	else {
		//debugar("Esta vers�o do windows n�o � suportada!");
		return resultSys;
	}
}



