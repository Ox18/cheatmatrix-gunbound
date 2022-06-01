unit WrapperUnit;

interface

uses windows{, sysutils, constantes, utils, IOCTLs, classes};

const KeBufSize = 1024;

type
  NTSTATUS = cardinal;
  PVOID = Pointer;
  UShort  = Word;   // unsigned 16-bit
  Short   = Smallint; // signed 16-bit
  ULong   = Cardinal;
  Size_T  = Cardinal;
  PLARGE_INTEGER = ^LARGE_INTEGER;
  PSIZE_T = ^SIZE_T;

   TSectionInherit = (ViewNone,ViewShare,ViewUnmap);
   SECTION_INHERIT = TSectionInherit;

type
XPMBuf = record
   Result : boolean;
   ProcessBase: Pointer;
   Pid: cardinal;
   Address: Pointer;
   Size: Cardinal;
   Buffer: Pointer;
  end;
  PXPMBuf = ^XPMBuf;

type
TOPBuf = record
    Pid: cardinal;
    outBase: Pointer;
    Handle: Cardinal;
  end;
POPBuf = ^TOPBuf;

type
TVirtualProtectEx = record
	lpAddress: Pointer;
	dwSize: PCardinal;
	flNewProtect: Pcardinal;
	lpflOldProtect: Pointer;
	hProcess: THandle;
	result: Boolean;
end;
PVirtualProtectEx = ^TVirtualProtectEx;

type
ANSI_STRING = record
    Length: word;
    MaximumLength: word;
    Buffer: PAnsiChar;
end;
PANSI_STRING = ^ANSI_STRING;

TYPE
  PUNICODE_STRING = ^UNICODE_STRING;
  UNICODE_STRING = packed record
  Length : WORD;
  MaximumLength : WORD;
  Buffer : PWideChar;
  end;

type
OBJECT_ATTRIBUTES = record
    Length: cardinal;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: cardinal;
    SecurityDescriptor: PVOID;
    SecurityQualityOfService: PVOID;
end;
POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;

//Function FX(valor: integer): cardinal; stdcall;
Function F1(valor: integer): cardinal; stdcall;
Function F2(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
Function F3(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
Function F4(pid: cardinal; outBase: PPointer): cardinal; stdcall;
Function F5(hnd: hwnd): hdc; stdcall;
Function F6(index: cardinal; buffer: cardinal; size: cardinal): cardinal; stdcall;
Function F7(hProcess: Cardinal; lpAddress: Pointer; dwSize: cardinal; flNewProtect: DWORD; lpflOldProtect: Pointer): boolean; stdcall;
Function F8(dwDesiredAccess: DWORD; bInheritHandle: Boolean; lpName: PAnsiChar): THandle; stdcall;
Function F9(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: DWORD): Pointer; stdcall;
Function F10(hnd: hwnd): hdc; stdcall;
Function F11(hnd: hwnd): hdc; stdcall;

//Function CurrentDllDir: string;
//function SystemDir: String;
//function IsWindowsNT: Boolean;
Function NT_SUCCESS(valor: NTSTATUS):boolean;
Procedure BaseSetLastNTError(code: dword);
Function BaseGetNamedObjectDirectory: THandle;



var
  BaseNamedObjectDirectory: THandle;

Const DllCode = 860956;
      DrvIndex = $3000;
      ntdll = 'ntdll.dll';

      STATUS_BUFFER_OVERFLOW = $80000005;
      STATUS_SUCCESS = $00000000;
      OBJ_CASE_INSENSITIVE = $00000040;
      STANDARD_RIGHTS_REQUIRED = $000F0000;
      DIRECTORY_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or $F);
      OBJ_INHERIT = $00000002;

      NtCurrentProcess = $FFFFFFFF; 

implementation

procedure InitializeObjectAttributes(InitializedAttributes : POBJECT_ATTRIBUTES; pObjectName : PUNICODE_STRING; const uAttributes : ULONG; const hRootDirectory : THandle; pSecurityDescriptor : PSECURITY_DESCRIPTOR);
begin
  with InitializedAttributes^ do
    begin
      Length := SizeOf(OBJECT_ATTRIBUTES);
      ObjectName := pObjectName;
      Attributes := uAttributes;
      RootDirectory := hRootDirectory;
      SecurityDescriptor := pSecurityDescriptor;
      SecurityQualityOfService := nil;
    end;
end;

Function IntToStr(valor: integer): string;
var x, c: integer;
    m: integer;
begin
    result := '';
    while valor > 0 do
    begin
       x := valor mod 10;
       valor := valor div 10;
       c := ord('0')+x;
       result := chr(c)+result;
    end;
end;

//Function FX(valor: integer): cardinal; stdcall;
//begin
    //MessageBox(0, pchar('valor: '+IntToStr(valor)), 'teste',0);
//end;

{procedure FSC;
asm
  mov edx,esp
  sysenter
end;

function IsWindowsNT: Boolean;
begin
  Result:=Win32Platform = VER_PLATFORM_WIN32_NT;
end;

Procedure FixPath(var Path: string);
begin
   if Path[length(path)] <> '\' then  Path := Path + '\'
end;

function SystemDir: String;
var
  dir: array [0..MAX_PATH] of Char;
begin
  GetSystemDirectory(dir, MAX_PATH);
  Result := StrPas(dir);
  FixPath(result);
end;

Function CurrentDllDir: string;
var
  TheFileName : array[0..MAX_PATH] of char;
begin
  FillChar(TheFileName, sizeof(TheFileName), #0);
  GetModuleFileName(hInstance, TheFileName, sizeof(TheFileName));
  result := string(TheFileName);
end;   }

Function StrLen(valor: PAnsiChar): integer;
var i: integer;
begin
   for i := 0 to MAX_PATH-1 do
   begin
       if valor[i] = #0 then
       begin
          result := i+1;
          exit;
       end;
   end;
   result := 0;
end;


{*******************************************************************************}
//                            Identificador da dll
{*******************************************************************************}
Function F1(valor: integer): cardinal; stdcall;
begin

    result := 0;
    case valor of
      //1:  result  := (cardinal(@FX)  +456789);
      2:  result  := (cardinal(@F2)  +456789);
      3:  result  := (cardinal(@F3)  +456789);
      4:  result  := (cardinal(@F4)  +456789);
      5:  result  := (cardinal(@F5)  +456789);
      6:  result  := (cardinal(@F6)  +456789);
      7:  result  := (cardinal(@F7)  +456789);
      8:  result  := (cardinal(@F8)  +456789);
      9:  result  := (cardinal(@F9)  +456789);
      10: result  := (cardinal(@F10) +456789);
      11: result  := (cardinal(@F11) +456789);
    end;
    //result := DllCode;

end;

{*******************************************************************************}
//                            ReadProcessMemory
{*******************************************************************************}
Function F2(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
  Function EnterTheMatrix(param1: Pointer): boolean; stdcall;
  var pilha: ppointer;
  begin
    pilha := @param1;
    asm
      mov eax, DrvIndex
      add eax, 1
      mov edx, pilha
      int 2eh
      mov cardinal(result), eax
    end;
  end;
var buf: XPMBuf;
    i: integer;
    tmpSize: cardinal;
    resSize: cardinal;
    totSize: cardinal;
begin

    tmpSize := lpsize;
    totSize := 0;
    i := 0;
    while tmpSize > 0 do
    begin
        if tmpSize < KeBufSize then
           resSize := tmpSize
        else
           resSize := KeBufSize;

        tmpSize := tmpSize - resSize;

        buf.Size := resSize;
        buf.Buffer := Pointer(cardinal(lpBuffer)+totSize);
        buf.Address := Pointer(cardinal(lpAddress)+totSize);
        buf.Pid := lpProcessID;
        buf.ProcessBase := lpProcessBase;
        result := Boolean(integer(result)*integer(EnterTheMatrix(@buf)));
        totSize := totSize + resSize;
    end;

    //MessageBox(0,pchar('Endereço: '+inttostr(cardinal(lpAddress))),'',0);
    //result := EnterTheMatrix(@buf);
end;

{*******************************************************************************}
//                            WriteProcessMemory
{*******************************************************************************}
Function F3(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
  Function EnterTheMatrix(param1: Pointer): boolean; stdcall;
  var pilha: ppointer;
  begin
    pilha := @param1;
    asm
      mov eax, DrvIndex
      add eax, 2
      mov edx, pilha
      int 2eh
      mov cardinal(result), eax
    end;
  end;
var buf: XPMBuf;
    i: integer;
    tmpSize: cardinal;
    resSize: cardinal;
    totSize: cardinal;
begin
    buf.Pid := lpProcessID;
    buf.ProcessBase := lpProcessBase;
    tmpSize := lpsize;
    totSize := 0;
    i := 0;
    while tmpSize > 0 do
    begin
        if tmpSize < KeBufSize then
           resSize := tmpSize
        else
           resSize := KeBufSize;
           
        tmpSize := tmpSize - resSize;

        buf.Size := resSize;
        buf.Buffer := Pointer(cardinal(lpBuffer)+totSize);
        buf.Address := Pointer(cardinal(lpAddress)+totSize);
        result := Boolean(integer(result)*integer(EnterTheMatrix(@buf)));
        totSize := totSize + resSize;
    end;
    //result := EnterTheMatrix(@buf);
end;

{*******************************************************************************}
//                            OpenProcess
{*******************************************************************************}
Function F4(pid: cardinal; outBase: PPointer): cardinal; stdcall;
  Function EnterTheMatrix(Buf: POPBuf): cardinal; stdcall;
  var pilha: ppointer;
  begin
    {$I VMProtectBegin.inc} 
    pilha := @Buf;
    asm
      mov eax, DrvIndex
      add eax, 3
      mov edx, pilha
      int 2eh
      mov result, eax
    end;
    {$I VMProtectEnd.inc}
  end;
var buffer: TOPBuf;
begin
    buffer.Pid := pid;
    buffer.outBase := outBase;
    result := EnterTheMatrix(@buffer);
    //MessageBox(0,pchar(inttostr(cardinal(buffer.outBase^))),'',0);
   //MessageBox(0,pchar(inttostr(cardinal(buffer.outBase))),'',0);
end;

{*******************************************************************************}
//                                   GetDC
{*******************************************************************************}
Function F5(hnd: hwnd): hdc; stdcall;
  Function EnterTheMatrix(hnd: hwnd): hdc; stdcall;
  var pilha: ppointer;
  begin
    {$I VMProtectBegin.inc} 
    pilha := @hnd;
    asm
      mov eax, DrvIndex
      add eax, 4
      mov edx, pilha
      int 2eh
      mov result, eax
    end;
    {$I VMProtectEnd.inc}
  end;
begin
    result := EnterTheMatrix(hnd);
end;

{*******************************************************************************}
//                              GeneralHandler
{*******************************************************************************}
Function F6(index: cardinal; buffer: cardinal; size: cardinal): cardinal; stdcall;
  Function EnterTheMatrix(valor1: cardinal; valor2: cardinal; valor3: cardinal): cardinal; stdcall;
  var pilha: ppointer;
  begin
    {$I VMProtectBegin.inc}
    pilha := @valor1;
    asm
      mov eax, DrvIndex
      mov edx, pilha
      int 2eh
      mov result, eax
    end;
    {$I VMProtectEnd.inc}
  end;
begin
    result := EnterTheMatrix(index, buffer, size);
    //result := (valor1*1000)+valor2;
end;

{*******************************************************************************}
//                            VirtualProtectEx
{*******************************************************************************}
Function F7(hProcess: Cardinal; lpAddress: Pointer; dwSize: cardinal; flNewProtect: DWORD; lpflOldProtect: Pointer): boolean; stdcall;
  Function EnterTheMatrix(param1: Pointer): boolean; stdcall;
  var pilha: ppointer;
  begin
    {$I VMProtectBegin.inc} 
    pilha := @param1;
    asm
      mov eax, DrvIndex
      add eax, 5
      mov edx, pilha
      int 2eh
      mov cardinal(result), eax
    end;
    {$I VMProtectEnd.inc}
  end;
var buf: TVirtualProtectEx;
    i: integer;
    tmpSize: cardinal;
    resSize: cardinal;
    totSize: cardinal;

    Addr2: Pointer;
    Addr3: Pointer;
    Size2: PDWord;
begin
    Addr2 := lpAddress;
    Addr3 := @Addr2;

    Size2 := @dwSize;

    buf.lpAddress := @Addr3;
    buf.dwSize := @Size2;
    buf.flNewProtect := @flNewProtect;
    buf.lpflOldProtect := @lpflOldProtect;
    buf.hProcess := hProcess;
    result := Boolean(integer(EnterTheMatrix(@buf)));
    //result := EnterTheMatrix(@buf);
end;

Function RtlNtStatusToDosError(Status: NTSTATUS): cardinal; stdcall; external ntdll;
Procedure RtlInitAnsiString(DestinationString: PANSI_STRING; SourceString: PAnsiChar); stdcall; external ntdll;
Function RtlAnsiStringToUnicodeString( DestinationString: PUNICODE_STRING; SourceString: PANSI_STRING; AllocateDestinationString: boolean): NTSTATUS; stdcall; external ntdll;
Procedure RtlInitUnicodeString(DestinationString: PUNICODE_STRING; SourceString: PWideChar); stdcall; external ntdll;                                                            //PSECURITY_DESCRIPTOR
//Procedure InitializeObjectAttributes(InitializedAttributes: POBJECT_ATTRIBUTES; ObjectName: PUNICODE_STRING; Attributes: cardinal; RootDirectory: THandle; SecurityDescriptor: Pointer); stdcall; external ntdll;
Procedure RtlAcquirePebLock; stdcall; external ntdll;
Procedure RtlReleasePebLock; stdcall; external ntdll;
Function NtOpenDirectoryObject(DirectoryObjectHandle: PHandle; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES ): NTSTATUS; stdcall; external ntdll;
Function ZwMapViewOfSection(SectionHandle: THandle; ProcessHandle: THandle; BaseAddress: PPointer; ZeroBits: PDWord; CommitSize: SIZE_T; SectionOffset: PLARGE_INTEGER; ViewSize: PSIZE_T; InheritDisposition: SECTION_INHERIT; AllocationType: ULONG; Win32Protect: ULONG): NTSTATUS; stdcall; external ntdll;

Function NtOpenSection(SectionHandle: PHandle; DesiredAccess: ACCESS_MASK; ObjectAttributes:POBJECT_ATTRIBUTES ): NTSTATUS; stdcall; external ntdll;
//begin

//end;

{*******************************************************************************}
//                            OpenFileMappingW
{*******************************************************************************}
Function F8_(dwDesiredAccess: DWORD; bInheritHandle: Boolean; lpName: PWideChar): THandle; //stdcall;
  {Function EnterTheMatrix(param1: Pointer): boolean; stdcall;
  var pilha: ppointer;
  begin
    pilha := @param1;
    asm
      mov eax, DrvIndex
      add eax, 5
      mov edx, pilha
      int 2eh
      mov cardinal(result), eax
    end;
  end; }
var {buf: TVirtualProtectEx;
    i: integer;
    tmpSize: cardinal;
    resSize: cardinal;
    totSize: cardinal; }

    hBaseDir: THandle;
    UnicodeName: UNICODE_STRING;
    Attributes: DWORD;
    ObAttr: OBJECT_ATTRIBUTES;
    SectionHandle: THandle;
    Status: NTStatus;
begin
//{$I VMProtectBegin.inc} 
    result := 0;

    if(lpName = nil) then
    begin
        BaseSetLastNTError($C000000D);
        exit;
    end;

    hBaseDir := BaseGetNamedObjectDirectory;

    if bInheritHandle then
	     Attributes := OBJ_INHERIT;

    RtlInitUnicodeString(@UnicodeName, lpName);
    InitializeObjectAttributes(@ObAttr, @UnicodeName, Attributes, hBaseDir, nil);
    Status := NtOpenSection(@SectionHandle, SECTION_ALL_ACCESS, @ObAttr);

    if (not NT_SUCCESS(Status)) then
    begin
      BaseSetLastNTError(Status);
      result := 0;
      exit;
    end;

    result := SectionHandle;
 //{$I VMProtectEnd.inc}
end;

Procedure BaseSetLastNTError(code: dword);
begin
//{$I VMProtectBegin.inc}
    SetLastError( RtlNtStatusToDosError(code) );
//{$I VMProtectEnd.inc}
end;

Function NT_SUCCESS(valor: NTSTATUS):boolean;
begin
//{$I VMProtectBegin.inc}
   result := valor = STATUS_SUCCESS;
//{$I VMProtectEnd.inc}
end;

Function BaseGetNamedObjectDirectory: THandle;
var Obja: OBJECT_ATTRIBUTES;
    Status: NTSTATUS;
    DirAccess: ACCESS_MASK;
begin
//{$I VMProtectBegin.inc}
    if(BaseNamedObjectDirectory <> 0) then
    begin
        result := BaseNamedObjectDirectory;
        exit;
    end;

    RtlAcquirePebLock;
    InitializeObjectAttributes(@Obja, nil, OBJ_CASE_INSENSITIVE, 0, nil);
    DirAccess := DIRECTORY_ALL_ACCESS;

    Status := NtOpenDirectoryObject(@BaseNamedObjectDirectory, DirAccess, @Obja);
    if ( not NT_SUCCESS(Status) ) then
       BaseNamedObjectDirectory := 0;

    RtlReleasePebLock();
    result := BaseNamedObjectDirectory;
//{$I VMProtectEnd.inc}
end;

{*******************************************************************************}
//                            OpenFileMappingA
{*******************************************************************************}
Function F8(dwDesiredAccess: DWORD; bInheritHandle: Boolean; lpName: PAnsiChar): THandle; stdcall;
var nome: PANSI_STRING;
    nomew: PWideChar;//PUNICODE_STRING;
    status: NTSTATUS;
begin
//{$I VMProtectBegin.inc}
    result := 0;

    if(lpName = nil) then
    begin
        BaseSetLastNTError($C000000D);
        exit;
    end;

    GetMem(nomew, StrLen(lpName) * SizeOf(WideChar) + 1);
    StringToWideChar(lpName, nomew, StrLen(lpName) * SizeOf(WideChar) + 1);

    result := F8_(dwDesiredAccess, bInheritHandle, nomew); //OpenFileMappingW
//{$I VMProtectEnd.inc}
end;

{*******************************************************************************}
//                            MapViewOfFileEx
{*******************************************************************************}
Function F9_(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: DWORD; lpBaseAddress: DWORD): Pointer;// stdcall;
var Status: NTSTATUS;
    SectionOffset: LARGE_INTEGER;
    ViewSize: ULONG;
    Protect: ULONG;
    BaseAddress: Pointer;
begin
//{$I VMProtectBegin.inc}
   SectionOffset.LowPart := dwFileOffsetLow;
   SectionOffset.HighPart := dwFileOffsetHigh;

   if( ( dwDesiredAccess and FILE_MAP_WRITE) = FILE_MAP_WRITE ) then
	      Protect := PAGE_READWRITE
   else
   if ( (dwDesiredAccess and FILE_MAP_READ) = FILE_MAP_READ) then
	      Protect := PAGE_READONLY
   else
   if ((dwDesiredAccess and FILE_MAP_ALL_ACCESS) = FILE_MAP_ALL_ACCESS) then
	      Protect  := PAGE_READWRITE
   else
   if ((dwDesiredAccess and FILE_MAP_COPY) = FILE_MAP_COPY) then
	      Protect := PAGE_WRITECOPY
   else
	      Protect := PAGE_READWRITE;

   if(lpBaseAddress = 0) then
   	  BaseAddress := nil
   else
	    BaseAddress := Pointer(lpBaseAddress);
   
   Status := ZwMapViewOfSection(hFileMappingObject,  NtCurrentProcess,
			       @BaseAddress,
			       0,
			       dwNumberOfBytesToMap,
			       @SectionOffset,
			       @ViewSize,
			       ViewShare,
			       0,
			       Protect);
   if (not NT_SUCCESS(Status)) then
   begin
      BaseSetLastNTError(Status);
      result := 0;
      exit;
   end;

   result := BaseAddress;
//{$I VMProtectEnd.inc}
end;

{*******************************************************************************}
//                            MapViewOfFile
{*******************************************************************************}
Function F9(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: DWORD): Pointer; stdcall;
begin
//{$I VMProtectBegin.inc}
   result := F9_(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap, 0);
//{$I VMProtectEnd.inc}
end;

{*******************************************************************************}
//                                   GetWindowDC
{*******************************************************************************}
Function F10(hnd: hwnd): hdc; stdcall;
  Function EnterTheMatrix(hnd: hwnd): hdc; stdcall;
  var pilha: ppointer;
  begin
    //{$I VMProtectBegin.inc}
    pilha := @hnd;
    asm
      mov eax, DrvIndex
      add eax, 6
      mov edx, pilha
      int 2eh
      mov result, eax
    end;
    //{$I VMProtectEnd.inc}
  end;
begin
    result := EnterTheMatrix(hnd);
end;

Function F11(hnd: hwnd): hdc; stdcall;
begin
MessageBox(0,'teste','teste',0);
result := 7;
end;


end.
