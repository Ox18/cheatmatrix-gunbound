unit CMClasses;

interface

uses windows, sysutils, classes;


type
  TProcNameInfo = record
     PID: cardinal;
     Nome: AnsiString;
     IsHidden: boolean;
  end;

type
  TAutoSelector = record
     pid: cardinal;
     indice: integer;
  end;

type
_MODULEINFO = packed record
    lpBaseOfDll: Pointer;
    SizeOfImage: DWORD;
    EntryPoint: Pointer;
  end;

 TModuleInfo = _MODULEINFO;
 LPMODULEINFO = ^_MODULEINFO;

 TCMProcessInfo = record
    BaseAddress: Cardinal;
    TotalSize: Cardinal;
    EntryPoint: Cardinal;
  end;

  TProcessName = record
     Pacote: AnsiString;
     Processos: array of AnsiString;
  end;

  TMatrizInfo = record
    Numero1: cardinal;
    Numero2: cardinal;
    String1: pAnsichar;
  end;

  TPluginUpdate = record
    Arquivo: AnsiString;
    Replaced: AnsiString;
    Data: AnsiString;
    Size: int64;
    Nivel: byte;
    UpVersion: integer;
    Exepts: array of AnsiString;
  end;

  TMbox = record
    texto: AnsiString;
    titulo: AnsiString;
    icone: AnsiString;
  end;

  TMDigital = record
    texto: AnsiString;
    cor: DWORD;
    repetir: integer;
    urgente: boolean;
    piscando: boolean;
  end;


  TApiMode = (AM_User, AM_Kernel, AM_Service);

  type TByteArray = array of byte;
  PByteArray = ^TByteArray;


//*****************************************************************************
//
//*****************************************************************************
var
  PSAPI: Cardinal;

    EnumProcessModules : function (hProcess: THandle; lphModule: LPDWORD; cb: DWORD;
  var lpcbNeeded: DWORD): BOOL; stdcall;

    GetModuleFileNameEx : function (hProcess: THandle; hModule: HMODULE;
  lpFilename: PChar; nSize: DWORD): DWORD;

    GetModuleInformation : function(hProcess: THandle; hModule: HMODULE;
      lpmodinfo: LPMODULEINFO; cb: DWORD): BOOL;

implementation

uses Utils;

initialization
begin

//{$I VMProtectBegin.inc}
    {
   try
       PSApi := LoadLibrary(PChar(PsapiDir));
       if PSApi <> 0 then
       begin
          try
            @EnumProcessModules := GetProcAddress(PSAPI, 'EnumProcessModules');
            @GetModuleFileNameEx := GetProcAddress(PSAPI, 'GetModuleFileNameEx');
            @GetModuleInformation := GetProcAddress(PSAPI, 'GetModuleInformation');
          except
              on e: exception do
              begin
                  MessageBox(0,PChar(Constante(0014, true)),'Erro',0);
              end;
          end;
       end;
   except
     on e: exception do
     begin
          MessageBox(0,PChar(Constante(0014, true)),'Erro',0);
     end;

   end;   }
//{$I VMProtectEnd.inc}

end;


end.
