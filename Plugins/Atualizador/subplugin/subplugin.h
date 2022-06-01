//---------------------------------------------------------------------------

#ifndef subpluginH
#define subpluginH


#include <windows.h>
#define STRICT
#include <algorithm>
using std::min;
using std::max;
#include <gdiplus.h>
using namespace Gdiplus;
#pragma link "gdiplus.lib"

class ESTRUTURA_PLG {
	private:
		Gdiplus::Graphics *grafico;
		public:
		TEstruturaPLG(){
		   grafico = new Gdiplus::Graphics((HDC)0);
		};

		bool ligado;
};
//---------------------------------------------------------------------------
#endif
