#pragma hdrstop

#include "PluginClasses.h"
#include "tiposBase.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

/*
PAnsiChar TPlugin::SendMsg(TSwMessage Valor)
{
	typedef PAnsiChar (__stdcall *ImpFunc)(PAnsiChar p1);
	AnsiString result = "";
    
    ImpFunc Func = (ImpFunc)GetProcAddress(PluginHandle, "Actions");
	if( Func == NULL )
		return NULL;
		
		//result = Valor.ToText();
		return Func( Valor.ToText().c_str()  );
}

bool TPlugin::Visible()
{
		TSwMessage *msg;
		PAnsiChar res;
		BOOL result = false;
		msg->Add("Tipo", IntToStr(MT_Visible) );
		SendMsg(*msg);
		res = SendMsg(*msg);
		if( res == NULL )
		   return result;
		msg->Clear();
		msg->LoadFrom(*res);
		result = msg->FieldByName("Resposta").AsBoolean();
		return result;
}

VOID TPlugin::Show()  
{
		TSwMessage *msg;
		msg->Add("Tipo", IntToStr(MT_Show) );
		SendMsg(*msg);
}

void TPlugin::Hide()
{
		TSwMessage *M;
		M->Add("Tipo", IntToStr(MT_Hide) );
		SendMsg(*M);
}

void TPlugin::Close()
{
		TSwMessage *M;
		M->Add("Tipo", IntToStr(MT_Close) );
		SendMsg(*M);
		try
			{
					FreeLibrary(PluginHandle);
			}
		catch(int erro){} 
}

//
// TSwMessage
//

//
//	Adiciona um campo na Mensagem
//
//void TSwMessage::TSwMessage()	{}

void TSwMessage::Add(AnsiString Field, AnsiString Value)
	{
		PSwLine Msg;
		Msg = new TSwLine;
		Msg->Field = Field;
		Msg->Value = Value;
		fValue->Add(Msg);
	}

//
//	Inicializa Mensagem
//

void TSwMessage::Clear()
	{
			fValue->Clear();
	}

TSwMessage::TSwMessage()
{
   fValue = new TList();
}

TSwMessage::~TSwMessage()
{
   //fValue = new TList;
}  

//
//	Campo pelo nome
//

TSwField TSwMessage::FieldByName(AnsiString Name)
	{
		    int i;
			int valor;
			TSwField result;
			for( i = 0; i < fValue->Count; i++)
				{
					   //MessageBoxA(NULL, PSwLine(fValue->Items[i])->Field.LowerCase().c_str() , "1", 0);
					   if (PSwLine(fValue->Items[i])->Field.LowerCase() == Name.LowerCase())
					   //if( !(CompareText(PSwLine(fValue->Items[i])->Field, ) ))
							{
									result.fValue = *PSwLine(fValue->Items[i]);
							}
				}
			 //MessageBoxA(NULL, "NULL" , "1", 0);
			 return result;
	}

//
//	Carrega uma mensagem
//

void TSwMessage::LoadFrom(AnsiString Value)
	{
    AnsiString Texto, Field, fsepT, vsepT;
	int SepLen, SepPos;
    PSwLine Result;

     Texto = Value;
	 fsepT	= fSep;
	 vsepT	= vSep;
	 SepLen = fsepT.Length(); //strlen(fSep);

	 while( Texto.Pos(fsepT)  )
		 {
			 SepPos = Texto.Pos(fsepT);
				   //  IntToStr(SepPos)
			 Texto  = Texto.SubString(SepPos+SepLen, Texto.Length() - (SepPos+SepLen) + 1);
			 
			 if( Texto.Pos(vsepT) )
				 {
						Field = Texto.SubString(1, Texto.Pos(vsepT)-1);
						Value = Texto.SubString( Texto.Pos(vsepT) + SepLen , Texto.Pos(fsepT)-( Texto.Pos(vsepT) + SepLen));
						//MessageBoxA(NULL, Field.c_str() , Value.c_str(), 0);
						Result = new TSwLine;
						Result->Field = Field;
						Result->Value = Value;
						fValue->Add(Result);
				 }
		 }
		 //MessageBoxA(NULL, "FIM" , "1", 0);
	}

//
//	Prepara mensagem para enviar
//
AnsiString TSwMessage::ToText()
	{
      int i;
	  AnsiString fsepT, vsepT;
      AnsiString result = "";

			fsepT	= fSep; 
			vsepT	= vSep;

			for( i = 0; i < fValue->Count; i++ )
				{
                    result = result + fSep;
					result.Insert((*PSwLine(fValue->Items[i])).Field, result.Length());
					result = result + vSep;
					result.Insert((*PSwLine(fValue->Items[i])).Value, result.Length());
				}
			result = result + fSep;
			return result;
	}

//
// TSwField
//void TSwField::TSwField()	{	}

BOOL TSwField::AsBoolean()
	{
			return  StrToBool(fValue.Value);
	}

LONG TSwField::AsInteger()
	{
			return StrToFloat(fValue.Value);
	}

PAnsiChar TSwField::AsPointer()
	{
			return (PAnsiChar)(fValue.Value.c_str());
	}

AnsiString TSwField::AsString()
	{
			return fValue.Value;
	}
	*/


