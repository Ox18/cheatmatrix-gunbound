unit GpsUnit;

{  GAME PROTECT SPY }

interface

uses utils, windows, sysutils;

Function CBTProc(nCode: integer; wParam: WPARAM; lParam: LPARAM): integer; stdcall;

implementation

{==============================================================================}
//                Função para inject por SetWindowsHookEx
{==============================================================================}

Function CBTProc(nCode: integer; wParam: WPARAM; lParam: LPARAM): integer; stdcall;
begin
   result := CallNextHookEx(0, nCode, wParam, lParam);
end;

{==============================================================================}
//                Função para inject por SetWindowsHookEx
{==============================================================================}

Function WinMain(hinstance, hprevinstance: HINST; lpcmdline: LPSTR; nshowcmd: integer): integer;
var msg: MSG;
    ex: WNDCLASSEX;
begin
	ex.cbSize := sizeof(WNDCLASSEX);
	ex.style := CS_HREDRAW|CS_VREDRAW|CS_OWNDC;
	ex.lpfnWndProc := WinProc;
	ex.cbClsExtra := 0;
	ex.cbWndExtra := 0;
	ex.hInstance := hinstance;
	ex.hIcon := LoadIcon(NULL, IDI_APPLICATION);
	ex.hCursor := LoadCursor(NULL, IDC_ARROW);
	ex.hbrBackground := (HBRUSH)GetStockObject(BLACK_BRUSH);
	ex.lpszMenuName := NULL;
	ex.lpszClassName := WNDCLASSNAME;
	ex.hIconSm = NULL;

	RegisterClassEx(&ex);

	// Create the window

	hwnd = CreateWindowEx(NULL,
 			      WNDCLASSNAME,
			      "Window",
			      WS_OVERLAPPEDWINDOW,
			      0, 0,
			      300, 300,
			      NULL,
			      NULL,
			      hinstance,
			      NULL);

	ShowWindow(hwnd, SW_SHOW);
	UpdateWindow(hwnd);

	// The message loop

	while(!quit)
	{
		if(PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE))
		{
			if(msg.message == WM_QUIT)
				quit = true;
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}
	return msg.lParam;
}


end.
