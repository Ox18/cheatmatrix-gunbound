//---------------------------------------------------------------------------

#ifndef SuporteH
#define SuporteH

#include <graphics.hpp>
#include <windows.h>
#include <vector>
using namespace std;

BOOL SearchPixel(DWORD cor, std::vector<TPoint>& result, BOOL OnlyFirst = false );
//---------------------------------------------------------------------------
#endif
