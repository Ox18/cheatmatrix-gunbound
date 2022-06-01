unit InjectUnit;

interface

uses windows, sysutils, utils, TlHelp32;

const
  CREATE_THREAD_ACCESS = (PROCESS_CREATE_THREAD or
                          PROCESS_QUERY_INFORMATION or
                          PROCESS_VM_OPERATION or
                          PROCESS_VM_WRITE or
                          PROCESS_VM_READ);

  Function InjectByCodeCave(PROCESS_NAME: AnsiString; DLL_NAME: AnsiString): boolean;
  Function InjectByCreateRemoteThread(PROCESS_NAME: AnsiString; DLL_NAME: AnsiString): boolean;
  Function InjectBySetWindowsHookEx(PROCESS_NAME: AnsiString; DLL_NAME: AnsiString): boolean;

implementation

{==============================================================================}
//                    Injeta pelo método CreateRemoteThread
{==============================================================================}

Function InjectByCreateRemoteThread(PROCESS_NAME: AnsiString; DLL_NAME: AnsiString): boolean;
    {-------------------}
    //  InjectDLL
    {-------------------}
    Function InjectDLL(ProcessID: cardinal; dllName: AnsiString): boolean;
    var  Proc: THandle;
         buf: array [0..50] of AnsiChar;
         RemoteString, LoadLibAddy: Pointer;
         x: cardinal;
    begin
        if(ProcessID = 0) then
        begin
            result := false;
            exit;
        end;

        Proc := OpenProcess(CREATE_THREAD_ACCESS, FALSE, ProcessID);

        if(Proc = 0) then
        begin
            result := false;
            exit;
        end;

        LoadLibAddy := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
        RemoteString := VirtualAllocEx(Proc, nil, length(DLL_NAME), MEM_RESERVE  or MEM_COMMIT, PAGE_READWRITE);
        WriteProcessMemory(Proc, RemoteString, pAnsiChar(dllName), length(dllName), x);
        CreateRemoteThread(Proc, nil, 0, LoadLibAddy, RemoteString, 0, x);
   
        CloseHandle(Proc);

        result := true;
    end;

    {-------------------}
    //  LoadDll
    {-------------------}
    Function LoadDll(procName, dllName: AnsiString ): boolean;
    var ProcID: cardinal;
    begin
        ProcID := 0;
        ProcID := GetProcessIdFromProcname(procName);
        if(not InjectDLL(ProcID, dllName)) then
            MessageBox(0, 'Process located, but injection failed', 'Loader', 0);
        result := InjectDLL(ProcID, dllName);
    end;
//###############################
begin
   result := false;
   if(IsWindowsNT) then
        result := LoadDll(PROCESS_NAME, DLL_NAME)
   else
        MessageBox(0, 'É necessário que seu sistema operacional seja Windows NT para executar esta ação!', 'Erro!', 0);
   if not result then
     MessageBox(0, 'Falha ao injetar','',0) else
     MessageBox(0, 'Injetado','',0);
end;

{==============================================================================}
//                    Injeta pelo método SetWindowsHookEx
{==============================================================================}

Function InjectBySetWindowsHookEx(PROCESS_NAME: AnsiString; DLL_NAME: AnsiString): boolean;
    Function LoadDll(procName, dllName: AnsiString) : boolean;
    var hDll: HMODULE;
        cbtProcAddr: Pointer;
        Id: cardinal;
    begin
        result := false;
        if DLL_NAME = '' then
           exit;
        hDll := LoadLibrary(CMStringToWideChar(dllName));
        cbtProcAddr := GetProcAddress(hDll, 'CBTProc');

        Id:=GetThreadIdFromProcName(procName);
        SetWindowsHookEx(WH_CBT, cbtProcAddr, hDll, id );

        result := TRUE;
    end;
begin
    LoadDll(PROCESS_NAME, DLL_NAME); 
end;

{==============================================================================}
//                    Injeta pelo método CodeCave
{==============================================================================}
Function OpenThread(dwDesiredAccess: dword; bInheritHandle: boolean; dwThreadId: dword): cardinal; stdcall; External 'Kernel32.dll';

Function InjectByCodeCave(PROCESS_NAME: AnsiString; DLL_NAME: AnsiString): boolean;
    Procedure loadDll;
    begin
       asm
          //   Armazena o endereço de retorno
          push $DEADBEEF
          //   Salva os flags e registradores
          pushfd
          pushad
          //   Armazena a string com o nome e o LoadLibrary
          push $DEADBEEF
          mov eax, $DEADBEEF
          //   chama o LoadLibrary com os parametros da string
          call eax
          //   Restaura os Registradores
          popad
          popfd
          //   Retorna o controle para o thread injetado
          ret
       end;
    end;

    Procedure loadDll_end;
    begin

    end;
var
    Stub: Pointer;
    dllString: pchar;
    wowID, threadID, stubLen, oldIP, oldprot, x: cardinal;
    loadLibAddy: cardinal;
    hProcess, hThread: THandle;
    ctx: CONTEXT;
const
   THREAD_GET_CONTEXT = $0008;
   THREAD_SET_CONTEXT = $0010;
   THREAD_SUSPEND_RESUME = $0002;
   THREAD_ALL_ACCESS = $1F03FF;
begin
   result := false;
   stubLen := cardinal(@loadDll_end) - cardinal(@loadDll);

   loadLibAddy := cardinal(GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA'));

   wowID    := GetProcessIdFromProcname(PROCESS_NAME);
   hProcess := OpenProcess((PROCESS_VM_WRITE or PROCESS_VM_OPERATION), false, wowID);

   dllString := VirtualAllocEx(hProcess, nil, (length(DLL_NAME) + 1), MEM_COMMIT, PAGE_READWRITE);
   stub      := VirtualAllocEx(hProcess, nil, stubLen + 1, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
   WriteProcessMemory(hProcess, dllString, Pchar(DLL_NAME), length(DLL_NAME), x);
   
   threadID := GetThreadIdFromProcname(PROCESS_NAME);
   hThread  := OpenThread(THREAD_ALL_ACCESS, false, threadID);
   if hThread = 0 then
   begin
      MessageBox(0, 'Falha ao injetar','',0);
      result := false;
      exit;
   end;

   SuspendThread(hThread);  

   ctx.ContextFlags := CONTEXT_CONTROL;
   GetThreadContext(hThread, ctx);
   oldIP   := ctx.Eip;
   ctx.Eip := cardinal(stub);
   ctx.ContextFlags := CONTEXT_CONTROL;

   VirtualProtect(@loadDll, stubLen, PAGE_EXECUTE_READWRITE, @oldprot);
   CopyMemory(Pointer(cardinal(@loadDll) + 1), @oldIP, 4);
   CopyMemory(Pointer(cardinal(@loadDll) + 8), @dllString, 4);
   CopyMemory(Pointer(cardinal(@loadDll) + 16), @loadLibAddy, 4);

   WriteProcessMemory(hProcess, stub, @loadDll, stubLen, x);
   SetThreadContext(hThread, ctx);

   ResumeThread(hThread);

   Sleep(8000);

   VirtualFreeEx(hProcess, dllString, length(DLL_NAME), MEM_DECOMMIT);
   VirtualFreeEx(hProcess, stub, stubLen, MEM_DECOMMIT);
   CloseHandle(hProcess);
   CloseHandle(hThread);

    result := true;
end;

end.
