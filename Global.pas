unit Global;

interface

uses windows;

type TReadProcessMemory=function(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
type TWriteProcessMemory=function(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
type TSuspendThread=function(hThread: THandle): DWORD; stdcall;
type TResumeThread=function(hThread: THandle): DWORD; stdcall;
type TOpenProcess=function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwProcessId: DWORD): THandle; stdcall;
type TCreateToolhelp32Snapshot=function(dwFlags, th32ProcessID: DWORD): THandle; stdcall;

type TProcess32First=function(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
type TProcess32Next=function(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
type TThread32First=function (hSnapshot: THandle; var lpte: TThreadEntry32): BOOL; stdcall;
type TThread32Next=function (hSnapshot: THandle; var lpte: TThreadENtry32): BOOL; stdcall;
type TModule32First=function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;
type TModule32Next=function (hSnapshot: THandle; var lpme: TModuleEntry32): BOOL; stdcall;

type TProtectMe=function(SW_WORD_0005ProcessID: dword; SW_WORD_0003,globalSW_WORD_0003:BOOL;list:pchar; listsize:dword):BOOL; stdcall;
type TSuspendThread=function(ThreadID:dword):boolean; stdcall;
type TResumeThread=function(ThreadID:dword):boolean; stdcall;
type TSuspendProcess=function(ProcessID:dword):boolean; stdcall;
type TResumeProcess=function(ProcessID:dword):boolean; stdcall;

type TVirtualProtect=function(lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
type TVirtualProtectEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL; stdcall;
type TVirtualQueryEx=function(hProcess: THandle; lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
type TVirtualAllocEx=function(hProcess: THandle; lpAddress: Pointer; dwSize, flAllocationType: DWORD; flProtect: DWORD): Pointer; stdcall;
type TCreateRemoteThread=function(hProcess: THandle; lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer;  dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall;
type TOpenThread=function(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):THANDLE; stdcall;
type TGetPEProcess=function(ProcessID:DWORD):DWORD; stdcall;

type TGetKProcAddress=function(s: pwidechar):pointer; stdcall;

Procedure ProtectProcess(processid: dword);
Procedure ProtectCM;

var
  ReadProcessMemory     :TReadProcessMemory;
  WriteProcessMemory    :TWriteProcessMemory;
  SuspendThread         :TSuspendThread;
  ResumeThread          :TResumeThread;
  OpenProcess           :TOpenProcess;

  CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;
  Process32First        :TProcess32First;
  Process32Next         :TProcess32Next;
  Thread32First         :TThread32First;
  Thread32Next          :TThread32Next;
  Module32First         :TModule32First;
  Module32Next          :TModule32Next;
  VirtualProtect        :TVirtualProtect;
  VirtualProtectEx      :TVirtualProtectEx;
  VirtualAllocEx        :TVirtualAllocEx;
  CreateRemoteThread    :TCreateRemoteThread;
  OpenThread            :TOpenThread;
  GetPEProcess          :TGetPEProcess;
  ProtectMe             :TProtectMe;
  GetProcessNameFromID  :TGetProcessNameFromID;
  GetProcessNameFromPEProcess:TGetProcessNameFromPEProcess;

  KernelOpenProcess       :TOpenProcess;
  KernelReadProcessMemory :TReadProcessMemory;
  KernelWriteProcessMemory:TWriteProcessMemory;
  KernelVirtualAllocEx    :TVirtualAllocEx;

  IsValidHandle           :TIsValidHandle;
  MakeWritable            :TMakeWritable;

  SWSuspendThread        :TSWKSuspendThread;
  SWResumeThread         :TSWKResumeThread;
  SWSuspendProcess       :TSWKSuspendProcess;
  SWResumeProcess        :TSWKResumeProcess;

  GetKProcAddress         :TGetKProcAddress;

var
  WindowsKernel: Thandle;
  SkyW4rriorKernel: Thandle;

implementation

procedure LoadApis;
begin
  if SkyW4rriorKernel=0 then
  begin
    SkyW4rriorKernel:= LoadLibrary(SWKdll);
    if SkyW4rriorKernel=0 then exit; //raise exception.Create('Failed to open CMHelp.dll');

    //the driver is loaded (I hope)

    KernelVirtualAllocEx:=GetProcAddress(SkyW4rriorkernel,'VAE');
    KernelOpenProcess:=GetProcAddress(SkyW4rriorkernel,'OP');
    KernelReadProcessMemory:=GetProcAddresS(SkyW4rriorkernel,'RPM');
    KernelWriteProcessMemory:=GetProcAddress(SkyW4rriorkernel,'WPM');

    GetPEProcess:=GetProcAddress(SkyW4rriorKernel,'GetPEProcess');
    GetPEThread:=GetProcAddress(SkyW4rriorKernel,'GetPEThread');
    GetSW_WORD_0007:=GetProcAddress(SkyW4rriorKernel,'GetSW_WORD_0007');
    GetThreadsProcessOffset:=GetProcAddress(SkyW4rriorKernel,'GetThreadsProcessOffset');
    GetThreadListEntryOffset:=GetProcAddress(SkyW4rriorKernel,'GetThreadListEntryOffset');
    GetSW_WORD_0008:=GetProcAddresS(SkyW4rriorKernel,'GetSW_WORD_0008');
    GetPhysicalAddress:=GetProcAddresS(SkyW4rriorKernel,'GetPhysicalAddress');
    GetCR4:=GetProcAddress(SkyW4rriorKernel,'GetCR4');
    GetCR3:=GetProcAddress(SkyW4rriorKernel,'GetCR3');
    SetCR3:=GetProcAddress(SkyW4rriorKernel,'SetCR3');
    GetSDT:=GetProcAddress(SkyW4rriorKernel,'GetSDT');
    GetSDTShadow:=GetProcAddress(SkyW4rriorKernel,'GetSDTShadow');

    setAlternateDebugMethod:=GetProcAddress(SkyW4rriorKernel,'setAlternateDebugMethod');
    getAlternateDebugMethod:=GetProcAddress(SkyW4rriorKernel,'getAlternateDebugMethod');
    DebugProcess:=GetProcAddress(SkyW4rriorKernel,'DebugProcess');
    StopDebugging:=GetProcAddress(SkyW4rriorKernel,'StopDebugging');
    StopRegisterChange:=GetProcAddress(SkyW4rriorKernel,'StopRegisterChange');
    RetrieveDebugData:=GetProcAddress(SkyW4rriorKernel,'RetrieveDebugData');
    ChangeRegOnBP:=GetProcAddress(SkyW4rriorKernel,'ChangeRegOnBP');
    StartProcessWatch:=GetProcAddress(SkyW4rriorKernel,'StartProcessWatch');
    WaitForProcessListData:=GetProcAddress(SkyW4rriorKernel,'WaitForProcessListData');
    GetProcessNameFromID:=GetProcAddress(SkyW4rriorKernel,'GetProcessNameFromID');
    GetProcessNameFromPEProcess:=GetProcAddress(SkyW4rriorKernel,'GetProcessNameFromPEProcess');
    IsValidHandle:=GetProcAddress(SkyW4rriorKernel,'IsValidHandle');
    GetIDTs:=GetProcAddress(SkyW4rriorKernel,'GetIDTs');

    GetIDTCurrentThread:=GetProcAddress(SkyW4rriorKernel,'GetIDTCurrentThread');
    MakeWritable:=GetProcAddress(SkyW4rriorKernel,'MakeWritable');
    GetLoadedState:=GetProcAddress(SkyW4rriorkernel,'GetLoadedState');
    SWKTest:=GetProcAddress(SkyW4rriorkernel,'test');
    useIOCTL:=GetProcAddress(SkyW4rriorkernel,'useIOCTL');

    SWKResumeThread:=GetProcAddress(SkyW4rriorKernel,'SWKResumeThread');
    SWKSuspendThread:=GetProcAddress(SkyW4rriorKernel,'SWKSuspendThread');

    SWKResumeProcess:=GetProcAddress(SkyW4rriorKernel,'SWKResumeProcess');
    SWKSuspendProcess:=GetProcAddress(SkyW4rriorKernel,'SWKSuspendProcess');

    KernelAlloc:=GetProcAddress(SkyW4rriorKernel,'KernelAlloc');
    GetKProcAddress:=GetProcAddress(SkyW4rriorkernel,'GetKProcAddress');

    GetSDTEntry:= GetProcAddress(SkyW4rriorKernel,'GetSDTEntry');
    SetSDTEntry:= GetProcAddress(SkyW4rriorKernel,'SetSDTEntry');
    GetSSDTEntry:=GetProcAddress(SkyW4rriorKernel,'GetSSDTEntry');
    SetSSDTEntry:=GetProcAddress(SkyW4rriorKernel,'SetSSDTEntry');


    CreateRemoteAPC:=GetProcAddress(SkyW4rriorKernel,'CreateRemoteAPC');

    {$ifdef cemain}
    if pluginhandler<>nil then
      pluginhandler.handlechangedpointers(0);
    {$endif}

  end;
end;


end.
