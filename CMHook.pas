unit CMHook;

interface

uses windows, sysutils, GenCodeHook
{$ifdef debug6}
,utils
{$endif}
;

var
Old_MessageBoxA : function(hWnd: HWND; lpText, lpCaption: PAnsiChar; uType: UINT): Integer; stdcall;
Old_MessageBoxW : function(hWnd: HWND; lpText, lpCaption: PWideChar; uType: UINT): Integer; stdcall;

Old_CreateRemoteThread : function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD;
                        lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD;
                        var lpThreadId: DWORD): THandle; stdcall;

Old_LoadLibraryA : function(lpLibFileName: PAnsiChar): HMODULE; stdcall;
Old_LoadLibraryW : function(lpLibFileName: PWideChar): HMODULE; stdcall;
Old_LoadLibraryExA : function(lpLibFileName: PAnsiChar; hFile: THandle; dwFlags: DWORD): HMODULE; stdcall;
Old_LoadLibraryExW : function(lpLibFileName: PWideChar; hFile: THandle; dwFlags: DWORD): HMODULE; stdcall;

Old_OpenProcess: Function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; stdcall;
Old_GetCurrentProcess: function: THandle; stdcall;
Old_GetCurrentProcessId: function: DWORD; stdcall;
Old_ExitProcess: Procedure(uExitCode: UINT); stdcall;
Old_TerminateProcess: function(hProcess: THandle; uExitCode: UINT): BOOL; stdcall;

Old_SendMessage: function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
Old_SendMessageA: function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
Old_SendMessageW: function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

Old_SendMessageTimeoutA: function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; var lpdwResult: DWORD): LRESULT; stdcall;
Old_SendMessageTimeoutW: function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; var lpdwResult: DWORD): LRESULT; stdcall;
Old_SendNotifyMessageA: function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
Old_SendNotifyMessageW: function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
Old_SendMessageCallbackA: function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; lpResultCallBack: TFNSendAsyncProc; dwData: DWORD): BOOL; stdcall;
Old_SendMessageCallbackW: function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; lpResultCallBack: TFNSendAsyncProc; dwData: DWORD): BOOL; stdcall;


Function LockCMFuntions: boolean;
procedure UnLockCMFuntions;

implementation

{&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&}
//
//                                      Inicio
//
{&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&}


{========== MessageBox  -  user32.dll =====================}

function New_MessageBoxA(hWnd: HWND; lpText, lpCaption: PAnsiChar; uType: UINT): Integer; stdcall;
var
  nAUx: Cardinal;
  wc:PWideChar;
begin
  //result := 0;
  result := 0; 
  if (pos('device context', LowerCase(strpas(lpText))) = 0) and (pos('unregistered', LowerCase(strpas(lpText))) = 0) and (pos('cryptor', LowerCase(strpas(lpText))) = 0) then
     Result := Old_MessageBoxA( hWnd, lpText, lpCaption, uType );
end;

function New_MessageBoxW(hWnd: HWND; lpText, lpCaption: PWideChar; uType: UINT): Integer; stdcall;
begin
  result := 0;
  if (pos('device context', lowercase(WideCharToString(lpText))) = 0) and (pos('unregistered', lowercase(WideCharToString(lpText))) = 0) and (pos('cryptor', lowercase(WideCharToString(lpText))) = 0) then
     Result := Old_MessageBoxW( hWnd, lpText, lpCaption, uType );
end;

{========== CreateRemoteThread  -  Kernel32.dll ==============}

function New_CreateRemoteThread(hProcess: THandle; lpThreadAttributes: Pointer;
  dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;
  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
begin
  Result := Old_CreateRemoteThread(hProcess, lpThreadAttributes, dwStackSize, lpStartAddress, lpParameter, dwCreationFlags, lpThreadId);
end;

{========== LoadLibrary  -  Kernel32.dll =====================}

function New_LoadLibraryA(lpLibFileName: PAnsiChar): HMODULE; stdcall;
begin
     if pos('npggnt.des', LowerCase(strpas(lpLibFileName)) ) > 0 then
        result := Old_LoadLibraryA( PAnsiChar( ExtractFilePath(ParamStr(0))+'ngpgnt.des' ) )
     else
        result := Old_LoadLibraryA(lpLibFileName);
end;

function New_LoadLibraryW(lpLibFileName: PWideChar): HMODULE; stdcall;
begin
     if pos('npggnt.des', LowerCase(WideCharToString(lpLibFileName)) ) > 0 then
        result := Old_LoadLibraryA( PAnsiChar( ExtractFilePath(ParamStr(0))+'ngpgnt.des' ) )
     else
        result := Old_LoadLibraryW(lpLibFileName);
end;

function New_LoadLibraryExA(lpLibFileName: PAnsiChar; hFile: THandle; dwFlags: DWORD): HMODULE; stdcall;
begin

     if pos('npggnt.des', LowerCase(strpas(lpLibFileName)) ) > 0 then
        result := Old_LoadLibraryA( PAnsiChar( ExtractFilePath(ParamStr(0))+'ngpgnt.des' ) )
     else
        result := Old_LoadLibraryExA(lpLibFileName, hFile, dwFlags);

 end;

function New_LoadLibraryExW(lpLibFileName: PWideChar; hFile: THandle; dwFlags: DWORD): HMODULE; stdcall;
begin
     if pos('npggnt.des', LowerCase(WideCharToString(lpLibFileName)) ) > 0 then
        result := Old_LoadLibraryA( PAnsiChar( ExtractFilePath(ParamStr(0))+'ngpgnt.des' ) )
     else
        result := Old_LoadLibraryExW(lpLibFileName, hFile, dwFlags);
end;

{========== ExitProcess  -  Kernel32.dll =====================}

procedure New_ExitProcess(uExitCode: UINT); stdcall;
begin
   {$ifdef debug6}
     PrintF('Try to exit process');
   {$endif}
end;

{========== Processes  -  Kernel32.dll =====================}

function New_OpenProcess(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; stdcall;
begin
    if dwProcessId <> Old_GetCurrentProcessId then
       result := Old_OpenProcess(dwDesiredAccess, bInheritHandle, dwProcessId);
end;

function New_GetCurrentProcess: THandle; stdcall;
begin
    result := 0;
end;

function New_GetCurrentProcessId: DWORD; stdcall;
begin
    result := 0;
end;

function New_TerminateProcess(hProcess: THandle; uExitCode: UINT): BOOL; stdcall;
begin
    result := true;
end;

{========== Messages  -  Kernel32.dll =====================}

function New_SendMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  result := 0;
end;

function New_SendMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
 begin
    result := 0;
 end;

 function New_SendMessageTimeoutA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; var lpdwResult: DWORD): LRESULT; stdcall;
 begin
    result := 0;
 end;

 function New_SendMessageTimeoutW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; fuFlags, uTimeout: UINT; var lpdwResult: DWORD): LRESULT; stdcall;
 begin
    result := 0;
 end;

 function New_SendNotifyMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
 begin
    result := true;
 end;

 function New_SendNotifyMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
 begin
    result := true;
 end;

 function New_SendMessageCallbackA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; lpResultCallBack: TFNSendAsyncProc; dwData: DWORD): BOOL; stdcall;
 begin
    result := true;
 end;

 function New_SendMessageCallbackW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; lpResultCallBack: TFNSendAsyncProc; dwData: DWORD): BOOL; stdcall;
 begin
    result := true;
 end;

{&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&}
//
//                                      FIM
//
{&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&}
Function LockCMFuntions: boolean;
var
   Module: HMODULE;
   A: Pointer;

   function GPA(Name: PAnsiChar): Pointer;
   begin
     Result := GetProcAddress(Module, Name);
   end;
begin
   result := true;

   { --- kernel32.dll ---}

   Module := GetModuleHandle('kernel32.dll');
   result := CreateGenericCodeHook( gpa('OpenProcess'), @New_OpenProcess, @Old_OpenProcess) and
             CreateGenericCodeHook( gpa('GetCurrentProcess'), @New_GetCurrentProcess, @Old_GetCurrentProcess) and
             CreateGenericCodeHook( gpa('GetCurrentProcessId'), @New_GetCurrentProcessId, @Old_GetCurrentProcessId) and
             CreateGenericCodeHook( gpa('ExitProcess'), @New_ExitProcess, @Old_ExitProcess) and
             CreateGenericCodeHook( gpa('TerminateProcess'), @New_TerminateProcess, @Old_TerminateProcess) and
             result;

   { --- user32.dll ---}

   Module := GetModuleHandle('user32.dll');
   result := CreateGenericCodeHook( gpa('SendMessageA'), @New_SendMessageA, @Old_SendMessageA) and
             CreateGenericCodeHook( gpa('SendMessageW'), @New_SendMessageW, @Old_SendMessageW) and
             CreateGenericCodeHook( gpa('SendMessageTimeoutA'), @New_SendMessageTimeoutA, @Old_SendMessageTimeoutA) and
             CreateGenericCodeHook( gpa('SendMessageTimeoutW'), @New_SendMessageTimeoutW, @Old_SendMessageTimeoutW) and
             CreateGenericCodeHook( gpa('SendNotifyMessageA'), @New_SendNotifyMessageA, @Old_SendNotifyMessageA) and
             CreateGenericCodeHook( gpa('SendNotifyMessageW'), @New_SendNotifyMessageW, @Old_SendNotifyMessageW) and
             CreateGenericCodeHook( gpa('SendMessageCallbackA'), @New_SendMessageCallbackA, @Old_SendMessageCallbackA) and
             CreateGenericCodeHook( gpa('SendMessageCallbackW'), @New_SendMessageCallbackW, @Old_SendMessageCallbackW) and
             result;
end;

procedure UnLockCMFuntions;
begin
  RemoveGenericCodeHook(@Old_OpenProcess);
  RemoveGenericCodeHook(@Old_GetCurrentProcess);
  RemoveGenericCodeHook(@Old_GetCurrentProcessId);
  RemoveGenericCodeHook(@Old_ExitProcess);
  RemoveGenericCodeHook(@Old_TerminateProcess);

  RemoveGenericCodeHook(@Old_SendMessageA);
  RemoveGenericCodeHook(@Old_SendMessageW);
  RemoveGenericCodeHook(@Old_SendMessageTimeoutA);
  RemoveGenericCodeHook(@Old_SendMessageTimeoutW);
  RemoveGenericCodeHook(@Old_SendNotifyMessageA);
  RemoveGenericCodeHook(@Old_SendMessageCallbackA);
  RemoveGenericCodeHook(@Old_SendMessageCallbackW);
end;



Function InstallPatch: boolean;
var
   Module: HMODULE;
   A: Pointer;

   function GPA(Name: PAnsiChar): Pointer;
   begin
     Result := GetProcAddress(Module, Name);
   end;
begin
   result := true;

   { --- user32.dll ---}

   Module := GetModuleHandle('user32.dll');
   result := CreateGenericCodeHook( gpa('MessageBoxA'), @New_MessageBoxA, @Old_MessageBoxA) and
             CreateGenericCodeHook( gpa('MessageBoxW'), @New_MessageBoxW, @Old_MessageBoxW);

   { --- kernel32.dll ---}

   Module := GetModuleHandle('kernel32.dll');
   result := CreateGenericCodeHook( gpa('LoadLibraryA'), @New_LoadLibraryA, @Old_LoadLibraryA) and
             CreateGenericCodeHook( gpa('LoadLibraryW'), @New_LoadLibraryW, @Old_LoadLibraryW) and
             CreateGenericCodeHook( gpa('LoadLibraryExA'), @New_LoadLibraryExA, @Old_LoadLibraryExA) and
             CreateGenericCodeHook( gpa('LoadLibraryExW'), @New_LoadLibraryExW, @Old_LoadLibraryExW) and
             result;

end;

//{$IfDef VER150}
initialization
  //InstallPatch;
  //{$EndIf}
end.
                                                                             1B39C47719F201439908E085E393938E2B63E17FECBDE36B4D72F7C
     