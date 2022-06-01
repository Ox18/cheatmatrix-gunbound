unit ProcListing;

interface

uses windows, sysutils, common, utils, constantes, cmclasses, classes;

const SystemProcessesAndThreadsInformation = 5;
const SystemHandleInformation              =	16;

type
TProcessList = TList;

PSYSTEM_HANDLE_INFORMATION = ^SYSTEM_HANDLE_INFORMATION;
SYSTEM_HANDLE_INFORMATION = packed record
   ProcessId: dword;
   ObjectTypeNumber: byte;
   Flags: byte;
   Handle: word;
   pObject: pointer;
   GrantedAccess: dword;
   end;

PSYSTEM_HANDLE_INFORMATION_EX = ^SYSTEM_HANDLE_INFORMATION_EX;
SYSTEM_HANDLE_INFORMATION_EX = packed record
   NumberOfHandles: dword;
   Information: array [0..0] of SYSTEM_HANDLE_INFORMATION;
   end;

PClientID = ^TClientID;
TClientID = packed record
 UniqueProcess:cardinal;
 UniqueThread:cardinal;
end;

PUnicodeString = ^TUnicodeString;
  TUnicodeString = packed record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
end;

PVM_COUNTERS = ^VM_COUNTERS;
VM_COUNTERS = packed record
   PeakVirtualSize,
   VirtualSize,
   PageFaultCount,
   PeakWorkingSetSize,
   WorkingSetSize,
   QuotaPeakPagedPoolUsage,
   QuotaPagedPoolUsage,
   QuotaPeakNonPagedPoolUsage,
   QuotaNonPagedPoolUsage,
   PagefileUsage,
   PeakPagefileUsage: dword;
  end;

PIO_COUNTERS = ^IO_COUNTERS;
IO_COUNTERS = packed record
   ReadOperationCount,
   WriteOperationCount,
   OtherOperationCount,
   ReadTransferCount,
   WriteTransferCount,
   OtherTransferCount: LARGE_INTEGER;
  end;

   PSYSTEM_THREADS = ^SYSTEM_THREADS;
SYSTEM_THREADS = packed record
  KernelTime,
  UserTime,
  CreateTime: LARGE_INTEGER;
  WaitTime: dword;
  StartAddress: pointer;
  ClientId: TClientId;
  Priority,
  BasePriority,
  ContextSwitchCount: dword;
  State: dword;
  WaitReason: dword;
 end;

PSYSTEM_PROCESSES = ^SYSTEM_PROCESSES;
SYSTEM_PROCESSES = packed record
   NextEntryDelta,
   ThreadCount: dword;
   Reserved1 : array [0..5] of dword;
   CreateTime,
   UserTime,
   KernelTime: LARGE_INTEGER;
   ProcessName: TUnicodeString;
   BasePriority: dword;
   ProcessId,
   InheritedFromProcessId,
   HandleCount: dword;
   Reserved2: array [0..1] of dword;
   VmCounters: VM_COUNTERS;
   IoCounters: IO_COUNTERS; // Windows 2000 only
   Threads: array [0..0] of SYSTEM_THREADS;
  end;

 PProcList = ^TProcList;
 TProcList = packed record
   NextItem: pointer;
   ProcName: array [0..MAX_PATH] of Char;
   ProcId: dword;
   ParrentId: dword;
 end;








var
    ProcessList: TProcessList;

    procedure GetSyscallProcessList;
    Function ZwQuerySystemInformation(ASystemInformationClass: dword;
                                  ASystemInformation: Pointer;
                                  ASystemInformationLength: dword;
                                  AReturnLength:PCardinal): NTStatus;
                                  stdcall;external 'ntdll.dll';
    Function XpZwQuerySystemInfoCall(ASystemInformationClass: dword;
                                 ASystemInformation: Pointer;
                                 ASystemInformationLength: dword;
                                 AReturnLength: pdword): dword; stdcall;
    Function Win2kZwQuerySystemInfoCall(ASystemInformationClass: dword;
                                    ASystemInformation: Pointer;
                                    ASystemInformationLength: dword;
                                    AReturnLength: pdword): dword; stdcall;

implementation

Function GetInfoTable(ATableType:dword):Pointer;
var
 mSize: dword;
 mPtr: pointer;
 St: NTStatus;
begin
 Result := nil;
 mSize := $4000;
 repeat
   mPtr := VirtualAlloc(nil, mSize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
   if mPtr = nil then Exit;
   St := ZwQuerySystemInformation(ATableType, mPtr, mSize, nil);
   if St = STATUS_INFO_LENGTH_MISMATCH then
      begin 
        VirtualFree(mPtr, 0, MEM_RELEASE);
        mSize := mSize * 2;
      end;
 until St <> STATUS_INFO_LENGTH_MISMATCH;
 if St = STATUS_SUCCESS
   then Result := mPtr
   else VirtualFree(mPtr, 0, MEM_RELEASE);
end;

procedure GetSyscallProcessList;
var
 Info: PSYSTEM_PROCESSES;
 NewItem: PProcList;// PProcessRecord;
 mPtr: pointer;
 mSize: dword;
 St: NTStatus;
begin
 mSize := $4000;
 repeat
  GetMem(mPtr, mSize);
  if IsWindowsNT then
    St := XpZwQuerySystemInfoCall(SystemProcessesAndThreadsInformation, mPtr, mSize, nil)
  else
    St := Win2kZwQuerySystemInfoCall(SystemProcessesAndThreadsInformation, mPtr, mSize, nil);
  if St = STATUS_INFO_LENGTH_MISMATCH then
    begin
      FreeMem(mPtr);
      mSize := mSize * 2;
    end;
 until St <> STATUS_INFO_LENGTH_MISMATCH;
 if St = STATUS_SUCCESS then
  begin
    Info := mPtr;
    repeat
     NewItem := new(PProcList);
     GetMem(NewItem, SizeOf(TProcList));
     ZeroMemory(NewItem, SizeOf(TProcList));
     lstrcpy(@NewItem^.ProcName, PChar(WideCharToString(Info^.ProcessName.Buffer)));
     NewItem^.ProcId  := Info^.ProcessId;
     NewItem^.ParrentId := Info^.InheritedFromProcessId;
     Info := pointer(dword(info) + info^.NextEntryDelta);
     ProcessList.Add(NewItem);
    until Info^.NextEntryDelta = 0;
  end;
 FreeMem(mPtr);
end;

procedure GetHandlesProcessList;
var
 Info: PSYSTEM_HANDLE_INFORMATION_EX;
 NewItem: PProcList;
 r: dword;
 OldPid: dword;
begin
  OldPid := 0;
  Info := GetInfoTable(SystemHandleInformation);
  if Info = nil then Exit;
  for r := 0 to Info^.NumberOfHandles do
    if Info^.Information[r].ProcessId <> OldPid then
     begin
       NewItem := new(PProcList);
       OldPid := Info^.Information[r].ProcessId;
       GetMem(NewItem, SizeOf(TProcessList));
       ZeroMemory(NewItem, SizeOf(TProcessList));
       NewItem^.ProcId   := OldPid;
       ProcessList.Add(NewItem);
     end;
  VirtualFree(Info, 0, MEM_RELEASE);
end;

Function XpZwQuerySystemInfoCall(ASystemInformationClass: dword;
                                 ASystemInformation: Pointer;
                                 ASystemInformationLength: dword;
                                 AReturnLength: pdword): dword; stdcall;
asm
 pop ebp
 mov eax, $AD
 call @SystemCall
 ret $10
 @SystemCall:
 mov edx, esp
 sysenter
end;

Function Win2kZwQuerySystemInfoCall(ASystemInformationClass: dword;
                                    ASystemInformation: Pointer;
                                    ASystemInformationLength: dword;
                                    AReturnLength: pdword): dword; stdcall;
asm
 pop ebp
 mov eax, $97
 lea edx, [esp + $04]
 int $2E
 ret $10
end;


initialization
  ProcessList := TProcessList.Create;
end.
