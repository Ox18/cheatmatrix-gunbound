//---------------------------------------------------------------------------

#ifndef PointListUnitH
#define PointListUnitH

#include <windows.h>
#include <types.hpp>
#include <vector>

using namespace std;

class TPointList
{
	 protected:
			std::vector<TPoint> lista;
			void __fastcall Put(int Index, TPoint valor);
			TPoint __fastcall Get(int Index);
	 public:
			TPointList(){};
			TPoint operator[](int Index) { return Points[Index]; }
			void __fastcall Add(int x, int y);
			void __fastcall Add(TPoint valor);
			void __fastcall Delete(TPoint valor, BOOL OnlyFirst = false);
			void __fastcall Delete(int index);
			void __fastcall Clear();
			int __fastcall count();
	 public:
	 		__property TPoint Points[int Index] = {read=Get, write=Put/*, default*/};
};

//---------------------------------------------------------------------------
#endif
