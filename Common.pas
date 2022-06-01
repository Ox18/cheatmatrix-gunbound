unit Common;

interface

uses CMClasses, CMStatus, windows, langcontrol;

type
  TIdioma   = (PT, EN);

var
  PluginsLiberados: array of AnsiString;
  NomesPacotes: array of AnsiString;
  TempoPacotes: array of AnsiString;
  OutrosValores: array of AnsiString;
  ModuloSistema: AnsiString;
  WrapperMS: AnsiString;
  UpdateFile: AnsiString;
  Idioma: TIdioma = PT;

  //MatrizInfo: TMatrizData;
  calibracaoDLL: PByteArray;
  allowHide: boolean;
  //CMLanguage: TCMLanguage;

  PackProcList: array of TProcessName;
  CurrentPack: AnsiString;
  CurrentPackIndex: integer;
  ProtectPE: boolean;
  ForceProtection: boolean;
  CMDir: AnsiString;

  AutoCheckUpdate: boolean;
  UpdateMode: integer;

  ProcessHandle: cardinal;
  ProcessBase: Pointer;
  ProcessId: cardinal;
  MatrizHandle: cardinal;
  MatrizID: cardinal;
  MatrizWindow: cardinal;

  UseRing0: boolean;

  ProtectionLoaded: boolean = false;

  CM_SCM: cardinal;
  CM_DEV: THandle;
  CM_SVC: cardinal;

  ServiceName: String;
  SysFileName: String;

  MatrizModuleInfo: TCMProcessInfo;
  RegisteredUser: integer;
  Working : Pboolean;

  ConnectionPort: cardinal;
  //MutantPrefix: AnsiString;
  //MutantHandle: cardinal;
  //MutantName: AnsiString;
  //NomeDriver: AnsiString;
  Kernel32Handle: cardinal;
  User32Handle: cardinal;

  pTotalAtualn: int64 = 0;
  totalSizeAtual: int64 = 0;
  totalSizeParcial: int64 = 0;
  portaSubMatrix: integer = 1313;

  Updates: Array of TPluginUpdate;

  {----- Letreiro - Mensagem do status -----}
    Mensagem : TMessageList;
  {-----------------------------------------}

  ApiMode: TApiMode;

  implementation
(*
  Function CMReadProcessMemoryB(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall; 
  Function CMWriteProcessMemoryB(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
  Function CMOpenProcessB(pid: cardinal; outBase: Pointer): cardinal;  stdcall;
  Function CMGetDCB(hnd: hwnd): hdc; stdcall;
  Function CMGetWindowDCB(hnd: hwnd): hdc; stdcall;
  Function CMVirtualProtectB(hProcess: Cardinal; lpAddress: Pointer; dwSize: cardinal; flNewProtect: DWORD; lpflOldProtect: Pointer): boolean; stdcall;
  Function CMOpenFileMappingB(dwDesiredAccess: DWORD; bInheritHandle: Boolean; lpName: PAnsichar): THandle; stdcall;
  Function CMMapViewOfFileB(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: DWORD): Pointer; stdcall;
var
  CMReadProcessMemoryC: Function(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
  CMWriteProcessMemoryC: Function(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
  CMOpenProcessC: Function(pid: cardinal; outBase: Pointer): cardinal; stdcall;
  CMGetDCC: Function(hnd: hwnd): hdc; stdcall;
  CMGetWindowDCC: Function(hnd: hwnd): hdc; stdcall;
  CMGeralHandler: Function(index: cardinal; bufer: pointer; size: cardinal): cardinal; stdcall; 
  CMVirtualProtectC: Function(hProcess: Cardinal; lpAddress: Pointer; dwSize: cardinal; flNewProtect: DWORD; lpflOldProtect: Pointer): boolean; stdcall;
  CMOpenFileMappingC: Function (dwDesiredAccess: DWORD; bInheritHandle: Boolean; lpName: PAnsichar): THandle; stdcall;
  CMMapViewOfFileC: Function(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: DWORD): Pointer; stdcall;

implementation

  Function CMOpenFileMappingB(dwDesiredAccess: DWORD; bInheritHandle: Boolean; lpName: PAnsiChar): THandle; stdcall;
  begin
      if (UseRing0) then
      begin
         result := CMOpenFileMappingC(dwDesiredAccess, bInheritHandle, lpName);
      end else
      begin
         result := OpenFileMappingA(dwDesiredAccess, bInheritHandle, lpName);
      end;
  end;

  Function CMMapViewOfFileB(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: DWORD): Pointer; stdcall;
  begin
      if (UseRing0) then
      begin
         result := CMMapViewOfFileC(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap);
      end else
      begin
         result := MapViewOfFile(hFileMappingObject, dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap);
      end;
  end;

  Function CMReadProcessMemoryB(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
  var var1: cardinal;
  begin
      if (UseRing0) then
      begin
         result := CMReadProcessMemoryC(lpProcessID, lpProcessBase, lpAddress, lpBuffer, lpsize);
      end else
      begin
         result := ReadProcessMemory(ProcessHandle, lpAddress, lpBuffer, lpsize, var1);
      end;
  end;

  function CMWriteProcessMemoryB(lpProcessID: cardinal; lpProcessBase: Pointer; lpAddress: pointer; lpBuffer: pointer; lpsize: cardinal): boolean; stdcall;
  var var1: cardinal;
  begin
      if (UseRing0) then
      begin
         result := CMWriteProcessMemoryC(lpProcessID, lpProcessBase, lpAddress, lpBuffer, lpsize);
      end else
      begin
         result := WriteProcessMemory(ProcessHandle, lpAddress, lpBuffer, lpsize, var1);
      end;
  end;

  Function CMOpenProcessB(pid: cardinal; outBase: Pointer): cardinal; stdcall;
  begin
      if (UseRing0) then
      begin
         result := CMOpenProcessC(pid, outBase);
      end else
      begin
         result := OpenProcess(PROCESS_ALL_ACCESS, true, pid);
      end;
  end;

  Function CMGetDCB(hnd: hwnd): hdc; stdcall;
  begin
      if (UseRing0) then
      begin
         result := CMGetDCC(hnd);
      end else
      begin
         result := CMGetDCC(hnd);
      end;
  end;

  Function CMGetWindowDCB(hnd: hwnd): hdc; stdcall;
  begin
      if (UseRing0) then
      begin
         result := CMGetWindowDCC(hnd);
      end else
      begin
         result := GetWindowDC(hnd);
      end;
  end;

  Function CMVirtualProtectB(hProcess: Cardinal; lpAddress: Pointer; dwSize: cardinal; flNewProtect: DWORD; lpflOldProtect: Pointer): boolean; stdcall;
  begin
      if (UseRing0) then
      begin
         result := CMVirtualProtectC(hProcess,lpAddress, dwSize, flNewProtect, lpflOldProtect);
      end else
      begin
         result := VirtualProtectEx(hProcess,lpAddress, dwSize, flNewProtect, lpflOldProtect);
      end;
  end;
     *)
end.
