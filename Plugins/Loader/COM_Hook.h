//---------------------------------------------------------------------------

#ifndef COM_HookH
#define COM_HookH

#include <windows.h>

#define INITGUID

#include <ddraw.h>
#include <dxtrans.h>
#include <objbase.h>

//#pragma comment(lib, "dxguid.lib")
//#pragma link "ddraw.lib"
//Class factories are used to create
//instantiate COM objects. We'll discuss
//in the following walk-through.
/*
extern long g_nComObjsInUse;

//extern ClassFactory g_CF;

struct ClassFactory: public IClassFactory
{
		public:
		ClassFactory();
		virtual ~ClassFactory();

		STDMETHODIMP QueryInterface(REFIID iid, void **ppv);
		STDMETHODIMP_(ULONG) AddRef(void){ return ++m_uRefCount;}
		STDMETHODIMP_(ULONG) Release(void);


// *** IClassFactory methods ***
		STDMETHODIMP CreateInstance(IUnknown *pUnkOuter, REFIID riid, void **ppv);
		STDMETHODIMP LockServer(int fLock);

		protected:
		ULONG m_uRefCount;
} ;
*/


//CLSCTX_INPROC_SERVER

extern const DWORD SPY_MESSAGE;

class MyDirectDrawSurface : IDirectDrawSurface7
{
		public:

		HRESULT __stdcall QueryInterface(const IID& iid, void** ppv) ;
		ULONG __stdcall AddRef();
		ULONG __stdcall Release(); 

		HRESULT __stdcall AddAttachedSurface(LPDIRECTDRAWSURFACE7 lpDDSAttachedSurface);
		HRESULT __stdcall AddOverlayDirtyRect(LPRECT lpRect);
		HRESULT __stdcall Blt(LPRECT lpDestRect, LPDIRECTDRAWSURFACE7 lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwFlags, LPDDBLTFX lpDDBltFx);
		HRESULT __stdcall BltBatch(LPDDBLTBATCH lpDDBltBatch, DWORD dwCount, DWORD dwFlags);
		HRESULT __stdcall BltFast(DWORD dwX, DWORD dwY, LPDIRECTDRAWSURFACE7 lpDDSrcSurface, LPRECT lpSrcRect, DWORD dwTrans);

		HRESULT __stdcall DeleteAttachedSurface(DWORD dwFlags, LPDIRECTDRAWSURFACE7 lpDDSAttachedSurface);
		HRESULT __stdcall EnumAttachedSurfaces(LPVOID lpContext, LPDDENUMSURFACESCALLBACK7 lpEnumSurfacesCallback);
		HRESULT __stdcall EnumOverlayZOrders(DWORD dwFlags, LPVOID lpContext, LPDDENUMSURFACESCALLBACK7 lpfnCallback);
		HRESULT __stdcall Flip(LPDIRECTDRAWSURFACE7 lpDDSurfaceTargetOverride,  DWORD dwFlags);
		HRESULT __stdcall GetAttachedSurface(LPDDSCAPS2 lpDDSCaps, LPDIRECTDRAWSURFACE7 FAR *lplpDDAttachedSurface);

		HRESULT __stdcall GetBltStatus(DWORD dwFlags);
		HRESULT __stdcall GetCaps(LPDDSCAPS2 lpDDSCaps);
		HRESULT __stdcall GetClipper(LPDIRECTDRAWCLIPPER FAR *lplpDDClipper);
		HRESULT __stdcall GetColorKey(DWORD dwFlags,LPDDCOLORKEY lpDDColorKey);
		HRESULT __stdcall GetDC(HDC FAR *lphDC);
		HRESULT __stdcall GetFlipStatus(DWORD dwFlags);

		HRESULT __stdcall GetOverlayPosition(LPLONG lplX, LPLONG lplY);
		HRESULT __stdcall GetPalette(LPDIRECTDRAWPALETTE FAR *lplpDDPalette);
		HRESULT __stdcall GetPixelFormat(LPDDPIXELFORMAT lpDDPixelFormat);

		HRESULT __stdcall GetSurfaceDesc(LPDDSURFACEDESC2 lpDDSurfaceDesc);
		HRESULT __stdcall Initialize(LPDIRECTDRAW lpDD, LPDDSURFACEDESC2 lpDDSurfaceDesc);
		HRESULT __stdcall IsLost();
		HRESULT __stdcall Lock(LPRECT lpDestRect, LPDDSURFACEDESC2 lpDDSurfaceDesc, DWORD dwFlags, HANDLE hEvent);
		HRESULT __stdcall ReleaseDC(HDC hDC);
		HRESULT __stdcall Restore();
		HRESULT __stdcall SetClipper(LPDIRECTDRAWCLIPPER lpDDClipper);
		HRESULT __stdcall SetColorKey(DWORD dwFlags, LPDDCOLORKEY lpDDColorKey);
		HRESULT __stdcall SetOverlayPosition(LONG lX, LONG lY);
		HRESULT __stdcall SetPalette(LPDIRECTDRAWPALETTE lpDDPalette);
		HRESULT __stdcall Unlock(LPRECT lpRect);
		HRESULT __stdcall UpdateOverlay(LPRECT lpSrcRect, LPDIRECTDRAWSURFACE7 lpDDDestSurface, LPRECT lpDestRect, DWORD dwFlags, LPDDOVERLAYFX lpDDOverlayFx);
		HRESULT __stdcall UpdateOverlayDisplay(DWORD dwFlags);
		HRESULT __stdcall UpdateOverlayZOrder(DWORD dwFlags, LPDIRECTDRAWSURFACE7 lpDDSReference);

		HRESULT __stdcall GetDDInterface(LPVOID FAR *lplpDD);
		HRESULT __stdcall PageLock(DWORD dwFlags);
		HRESULT __stdcall PageUnlock(DWORD dwFlags);

		HRESULT __stdcall SetSurfaceDesc(LPDDSURFACEDESC2 lpddsd2, DWORD dwFlags);

		HRESULT __stdcall SetPrivateData(REFGUID guidTag, LPVOID lpData, DWORD cbSize, DWORD   dwFlags);
		HRESULT __stdcall GetPrivateData(REFGUID guidTag, LPVOID  lpBuffer, LPDWORD lpcbBufferSize);
		HRESULT __stdcall FreePrivateData( REFGUID guidTag);
		HRESULT __stdcall GetUniquenessValue(LPDWORD lpValue);
		HRESULT __stdcall ChangeUniquenessValue();
		HRESULT __stdcall SetPriority(DWORD dwPriority);
		HRESULT __stdcall GetPriority(LPDWORD lpdwPriority);
		HRESULT __stdcall SetLOD( DWORD dwMaxLOD);
		HRESULT __stdcall GetLOD(LPDWORD lpdwMaxLOD);

		MyDirectDrawSurface(LPVOID lpDDs);
		~MyDirectDrawSurface();

		
	 private:
	 IDirectDrawSurface7* m_pDDS;

};


//---------------------------------------------------------------------------
#endif
