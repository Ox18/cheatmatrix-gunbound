//---------------------------------------------------------------------------

#pragma hdrstop

#include "COM_Hook.h"

#define INITGUID

const DWORD SPY_MESSAGE = WM_USER + 49347;

//#pragma link "d3dim.lib"

/*

long g_nComObjsInUse=0;
//Class factories are COM objects, so they must
//support QueryInterface.
ClassFactory::ClassFactory()
{
		m_uRefCount = 0;
		InterlockedIncrement(&g_nComObjsInUse);
}

ClassFactory::~ClassFactory()
{
		InterlockedDecrement(&g_nComObjsInUse);
}

STDMETHODIMP_(ULONG) ClassFactory::Release(void)
{
		ULONG uRet = --m_uRefCount;
		if ( 0 == m_uRefCount )
        delete this;

		return uRet;
}

STDMETHODIMP ClassFactory::QueryInterface (REFIID iid, void ** ppv)
{
 	if ((iid == IID_IUnknown) || (iid == IID_IClassFactory))
			{
		*ppv = static_cast<IClassFactory*>(this) ; 
	    }
	else
	    {
		*ppv = NULL ;
		return E_NOINTERFACE ;
	    }
	reinterpret_cast<IUnknown*>(*ppv)->AddRef() ;
	return S_OK ;

}

//This method is called by COM or the client application directly
//to create a COM object.
STDMETHODIMP ClassFactory::CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppv)
{
				MyDirectDrawSurface* pMyDirectDrawSurface;
				HRESULT hrRet;

        if ( pUnkOuter != NULL )
					 return CLASS_E_NOAGGREGATION;

				*ppv = NULL;

				pMyDirectDrawSurface = new MyDirectDrawSurface;

				if ( pMyDirectDrawSurface == NULL )
					 return E_OUTOFMEMORY;

				hrRet = pMyDirectDrawSurface->QueryInterface ( riid, ppv );
				if ( FAILED(hrRet) )
					 delete pMyDirectDrawSurface;

		    return hrRet;
}

STDMETHODIMP ClassFactory::LockServer(int fLock)
{
		return E_NOTIMPL;
}
*/
//**********************************************************

MyDirectDrawSurface::MyDirectDrawSurface(LPVOID lpDDs)                                             //CLSCTX_INPROC_SERVER
{                                                                 //The com object is in another process = HOOK ^^
		/*HRESULT hr = CoCreateInstance( CLSID_DXSurface, NULL, CLSCTX_LOCAL_SERVER, IID_IDirectDrawSurface7, (LPVOID*) m_pDDS);
		m_RefCount = 0;
		InterlockedIncrement(&g_nComObjsInUse);*/
		m_pDDS = (IDirectDrawSurface7*) lpDDs;
}

MyDirectDrawSurface::~MyDirectDrawSurface()
{
    //free(m_pDDS);
		//m_pDDS->Release();
		//InterlockedDecrement(&g_nComObjsInUse);
}

//----------------------------------------

HRESULT __stdcall MyDirectDrawSurface::QueryInterface(const IID& iid, void** ppv)
{
		/*HRESULT hrRet = S_OK;

		if ( IsBadWritePtr( ppv, sizeof(void*) ))
				return E_POINTER;

		*ppv = NULL;

    if (iid == IID_IUnknown)
    {
				*ppv = static_cast<IUnknown*>(this) ;
    } 
    else if (iid == IID_IDirectDrawSurface7)
    {
				*ppv = static_cast<IDirectDrawSurface7*>(this) ;
    }
    else
    {
        hrRet = E_NOINTERFACE ;
    }

		if ( hrRet == S_OK )
		{
			((IUnknown*) *ppv)->AddRef();
		}
		// Incrementing the Reference count variable
    
		return hrRet ;*/
		return m_pDDS->QueryInterface(iid, ppv);
}

ULONG __stdcall MyDirectDrawSurface::AddRef()
{
		//return InterlockedIncrement(&m_RefCount);
		return m_pDDS->AddRef();
}

ULONG __stdcall MyDirectDrawSurface::Release()
{
		/*long nRefCount=0;
		nRefCount = InterlockedDecrement(&m_RefCount) ;
		if (nRefCount == 0) delete this;
		return nRefCount;*/
		return m_pDDS->Release();
}

//-----------------------------------------------
		HRESULT __stdcall MyDirectDrawSurface::AddAttachedSurface(LPDIRECTDRAWSURFACE7 lpDDSAttachedSurface)
		{
			 //SendMessage(HWND_BROADCAST, SPY_MESSAGE, 6, 4 );
			 return m_pDDS->AddAttachedSurface(lpDDSAttachedSurface);
		}

		 HRESULT __stdcall MyDirectDrawSurface::AddOverlayDirtyRect(LPRECT lpRect)
		{
			 return m_pDDS->AddOverlayDirtyRect(lpRect);
		}

		 HRESULT __stdcall MyDirectDrawSurface::Blt(LPRECT lpDestRect, LPDIRECTDRAWSURFACE7 lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPDDBLTFX lpDDBltFx)
		{
		   //SendMessage(HWND_BROADCAST, SPY_MESSAGE, 6, 3 );
			 return m_pDDS->Blt(lpDestRect, lpDDSrcSurface, lpSrcRect, dwFlags, lpDDBltFx);
		}

		 HRESULT __stdcall MyDirectDrawSurface::BltBatch(LPDDBLTBATCH lpDDBltBatch, DWORD dwCount, DWORD dwFlags)
		{
			 return m_pDDS->BltBatch(lpDDBltBatch, dwCount, dwFlags);
		}

		 HRESULT __stdcall MyDirectDrawSurface::BltFast(DWORD dwX, DWORD dwY, LPDIRECTDRAWSURFACE7 lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwTrans)
		{
			 return m_pDDS->BltFast(dwX, dwY, lpDDSrcSurface, lpSrcRect, dwTrans);
		}

		 HRESULT __stdcall MyDirectDrawSurface::ChangeUniquenessValue()
		{
			 return m_pDDS->ChangeUniquenessValue();
		}

		 HRESULT __stdcall MyDirectDrawSurface::DeleteAttachedSurface(DWORD dwFlags, LPDIRECTDRAWSURFACE7 lpDDSAttachedSurface)
		{
			 return m_pDDS->DeleteAttachedSurface(dwFlags, lpDDSAttachedSurface);
		}

		 HRESULT __stdcall MyDirectDrawSurface::EnumAttachedSurfaces(LPVOID lpContext, LPDDENUMSURFACESCALLBACK7 lpEnumSurfacesCallback)
		{
			 return m_pDDS->EnumAttachedSurfaces(lpContext, lpEnumSurfacesCallback);
		}

		 HRESULT __stdcall MyDirectDrawSurface::EnumOverlayZOrders(DWORD dwFlags, LPVOID lpContext, LPDDENUMSURFACESCALLBACK7 lpfnCallback)
		{
			 return m_pDDS->EnumOverlayZOrders(dwFlags, lpContext, lpfnCallback);
		}

		 HRESULT __stdcall MyDirectDrawSurface::FreePrivateData( REFGUID guidTag)
		{
			 return m_pDDS->FreePrivateData(guidTag);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetAttachedSurface(LPDDSCAPS2 lpDDSCaps, LPDIRECTDRAWSURFACE7 FAR *lplpDDAttachedSurface)
		{
			 return m_pDDS->GetAttachedSurface(lpDDSCaps, lplpDDAttachedSurface);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetBltStatus(DWORD dwFlags)
		{
       return m_pDDS->GetBltStatus(dwFlags);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetCaps(LPDDSCAPS2 lpDDSCaps)
		{
			 return m_pDDS->GetCaps(lpDDSCaps);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetClipper(LPDIRECTDRAWCLIPPER FAR *lplpDDClipper)
		{
			 return m_pDDS->GetClipper(lplpDDClipper);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetColorKey(DWORD dwFlags,LPDDCOLORKEY lpDDColorKey)
		{
			 return m_pDDS->GetColorKey(dwFlags, lpDDColorKey);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetDC(HDC FAR *lphDC)
		{
       return m_pDDS->GetDC(lphDC);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetDDInterface(LPVOID FAR *lplpDD)
		{
			 return m_pDDS->GetDDInterface(lplpDD);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetFlipStatus(DWORD dwFlags)
		{
			 return m_pDDS->GetFlipStatus(dwFlags);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetLOD(LPDWORD lpdwMaxLOD)
		{
			 return m_pDDS->GetLOD(lpdwMaxLOD);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetOverlayPosition(LPLONG lplX, LPLONG lplY)
		{
			 return m_pDDS->GetOverlayPosition(lplX, lplY);
		}

		 HRESULT __stdcall MyDirectDrawSurface::GetPalette(LPDIRECTDRAWPALETTE FAR *lplpDDPalette)
		{
       return m_pDDS->GetPalette(lplpDDPalette);
		}

		HRESULT __stdcall MyDirectDrawSurface::GetPixelFormat(LPDDPIXELFORMAT lpDDPixelFormat)
	 {
			 return m_pDDS->GetPixelFormat(lpDDPixelFormat);
	 }

		HRESULT __stdcall MyDirectDrawSurface::GetPriority(LPDWORD lpdwPriority)
	 {
			 return m_pDDS->GetPriority(lpdwPriority);
	 }

		HRESULT __stdcall MyDirectDrawSurface::GetPrivateData(REFGUID guidTag, LPVOID  lpBuffer, LPDWORD lpcbBufferSize)
	 {
			 return m_pDDS->GetPrivateData(guidTag, lpBuffer, lpcbBufferSize);
	 }

		HRESULT __stdcall MyDirectDrawSurface::GetSurfaceDesc(LPDDSURFACEDESC2 lpDDSurfaceDesc)
	 {
			return m_pDDS->GetSurfaceDesc(lpDDSurfaceDesc);
	 }

		HRESULT __stdcall MyDirectDrawSurface::GetUniquenessValue(LPDWORD lpValue)
	 {
			return m_pDDS->GetUniquenessValue(lpValue);
	 }

		HRESULT __stdcall MyDirectDrawSurface::Initialize(LPDIRECTDRAW lpDD, LPDDSURFACEDESC2 lpDDSurfaceDesc)
	 {
			return m_pDDS->Initialize(lpDD, lpDDSurfaceDesc);
	 }

		HRESULT __stdcall MyDirectDrawSurface::IsLost()
	 {
			return m_pDDS->IsLost();
	 }

		HRESULT __stdcall MyDirectDrawSurface::Lock(LPRECT lpDestRect, LPDDSURFACEDESC2 lpDDSurfaceDesc, DWORD dwFlags, HANDLE hEvent)
	 {
			return m_pDDS->Lock(lpDestRect, lpDDSurfaceDesc, dwFlags, hEvent);
	 }

		HRESULT __stdcall MyDirectDrawSurface::PageLock(DWORD dwFlags)
	 {
			return m_pDDS->PageLock(dwFlags);
	 }

		HRESULT __stdcall MyDirectDrawSurface::PageUnlock(DWORD dwFlags)
	 {
			return m_pDDS->PageUnlock(dwFlags);
	 } 

		HRESULT __stdcall MyDirectDrawSurface::ReleaseDC(HDC hDC)
	 {
			return m_pDDS->ReleaseDC(hDC);
	 }

		HRESULT __stdcall MyDirectDrawSurface::Restore()
	 {
			return m_pDDS->Restore();
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetClipper(LPDIRECTDRAWCLIPPER lpDDClipper)
	 {
			return m_pDDS->SetClipper(lpDDClipper);
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetColorKey(DWORD dwFlags, LPDDCOLORKEY lpDDColorKey)
	 {
			return m_pDDS->SetColorKey(dwFlags, lpDDColorKey);
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetLOD( DWORD dwMaxLOD)
	 {
			return m_pDDS->SetLOD(dwMaxLOD);
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetOverlayPosition(LONG lX, LONG lY)
	 {
			return m_pDDS->SetOverlayPosition(lX, lY);
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetPalette(LPDIRECTDRAWPALETTE lpDDPalette)
	 {
      return m_pDDS->SetPalette(lpDDPalette);
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetPriority(DWORD dwPriority)
	 {
			return m_pDDS->SetPriority(dwPriority);
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetPrivateData(REFGUID guidTag, LPVOID lpData, DWORD cbSize, DWORD   dwFlags)
	 {
			return m_pDDS->SetPrivateData(guidTag, lpData, cbSize, dwFlags);
	 }

		HRESULT __stdcall MyDirectDrawSurface::SetSurfaceDesc(LPDDSURFACEDESC2 lpddsd2, DWORD dwFlags)
	 {
			return m_pDDS->SetSurfaceDesc(lpddsd2, dwFlags);
	 }

	  HRESULT __stdcall MyDirectDrawSurface::Unlock(LPRECT lpRect)
	 {
      return m_pDDS->Unlock(lpRect);
	 }

		HRESULT __stdcall MyDirectDrawSurface::UpdateOverlay(LPRECT lpSrcRect, LPDIRECTDRAWSURFACE7 lpDDDestSurface, LPRECT lpDestRect, DWORD dwFlags, LPDDOVERLAYFX lpDDOverlayFx)
	 {
			return m_pDDS->UpdateOverlay(lpSrcRect, lpDDDestSurface, lpDestRect, dwFlags, lpDDOverlayFx);
	 }

		HRESULT __stdcall MyDirectDrawSurface::UpdateOverlayDisplay(DWORD dwFlags)
	 {
			return m_pDDS->UpdateOverlayDisplay(dwFlags);
	 }

		HRESULT __stdcall MyDirectDrawSurface::UpdateOverlayZOrder(DWORD dwFlags, LPDIRECTDRAWSURFACE7 lpDDSReference)
	 {
			return m_pDDS->UpdateOverlayZOrder(dwFlags, lpDDSReference);
	 }

	  HRESULT __stdcall MyDirectDrawSurface::Flip(LPDIRECTDRAWSURFACE7 lpDDSurfaceTargetOverride,  DWORD dwFlags)
	 {
				//SendMessage(HWND_BROADCAST, SPY_MESSAGE, 6, 5 );
				return m_pDDS->Flip(lpDDSurfaceTargetOverride, dwFlags);
	 }
//---------------------------------------------------------------------------
#pragma package(smart_init)
