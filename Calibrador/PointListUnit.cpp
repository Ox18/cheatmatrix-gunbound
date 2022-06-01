//---------------------------------------------------------------------------


#pragma hdrstop

#include "PointListUnit.h"

 TPoint __fastcall TPointList::Get(int Index)
 {
		 if(Index < lista.size())
				return lista[Index];

		 return TPoint(0,0);
 }

 void __fastcall TPointList::Put(int Index, TPoint valor)
 {
		 if(Index >= lista.size())
				lista.push_back(valor);
		 else
				lista[Index] = valor;
 }

 void __fastcall TPointList::Add(int x, int y)
 {
		 lista.push_back(TPoint(x,y));
 }

 void __fastcall TPointList::Add(TPoint valor)
 {
     lista.push_back(valor);
 }


 void __fastcall TPointList::Delete(TPoint valor, BOOL OnlyFirst)
 {

		 for(int i = (lista.size()-1); i >= 0; i--)
		 {
				if(lista[i].x == valor.x && lista[i].y == valor.y)
				{
						lista.erase(lista.begin()+i);
						if(OnlyFirst)
						   return;
				}
		 }
 }

 void __fastcall TPointList::Delete(int index)
 {
		 lista.erase(lista.begin()+index);
 }

 void __fastcall TPointList::Clear()
 {
      lista.clear();
 }

 int __fastcall TPointList::count()
 {
      return lista.size();
 }

//---------------------------------------------------------------------------

#pragma package(smart_init)
