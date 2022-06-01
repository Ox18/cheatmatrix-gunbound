unit Utils;

interface

uses Windows, Graphics, Classes, Messages, sysutils,{ CMClasses, }
TlHelp32, constantes, {common,} StdCtrls, Forms, hwid_impl;

const
 WM_Plugin = wm_user + 206;

var
  PsapiDir : AnsiString;

{----------------------------------------------------------------------------}
//                          Types
{----------------------------------------------------------------------------}

type
   TListOrderType = (OT_ByName, OT_ByID, OT_ByCreation);
    THackType = (HT_Hack = 5, HT_Control = 10);
Type
  XPMBuf = record
   Result : boolean;
   ProcessBase: Pointer;
   Pid: cardinal;
   Address: Pointer;
   Size: Cardinal;
   Buffer: Pointer;
  end;

  TOPBuf = record
    Pid: cardinal;
    outBase: Pointer;
    Handle: Cardinal;
  end;

type
  TFunctionData = record
      Offset: UINT;
      Ponteiro: ULONG;
      ParamSize: UCHAR;
      Data: array [0..14] of byte;
  end;
  PFunctionData = ^TFunctionData;

  TRequestData =
  (
      RD_CloseMatrix,
      RD_ReopenMatrix,
      RD_PutOnStatus,
      RD_ClosePlugins,
      RD_HidePlugins,
      RD_ReloadPlugins,
      RD_OpenPlugin,
      RD_ClosePlugin,
      RD_CloseProcess,
      RD_PauseProcess,
      RD_PauseThread,
      RD_AtachProcess
  );

 //_STARTUPINFOW = 

  
  TMatrizData = record
     Calibracoes: PByteArray;
     MatrizHandle: DWORD;
     DriverHandle: DWORD;
     //MatrizDir: array[0..500] of char;
     Opens: Integer;
     TargetHandle: DWORD;
     TargetId: DWORD;
     ProcessBase: PCardinal;
     Ligado: BOOLEAN;
  end;
  PTMatrizData = ^TMatrizData;

  TImageStatus = (IS_Normal, IS_Main, IS_Running, IS_Open);
  TMessageType = (MT_Close, MT_Hide, MT_Show, MT_Visible);

  TPluginRequest = packed record
     Tipo: TRequestData;
     Menssagem: ShortString;
     Complemento1: ShortString;
     Complemento2: ShortString;
     Complemento3: ShortString;
     Complemento4: ShortString;
     Complemento5: ShortString;
     Valor: integer;
  end;

  const
  ntdll = 'ntdll.dll';

type
  NTSTATUS = LongInt;

  TProcessInfoClass = (
    ProcessBasicInformation, ProcessQuotaLimits,
    ProcessIoCounters, ProcessVmCounters,
    ProcessTimes, ProcessBasePriority, ProcessRaisePriority,
    ProcessDebugPort, ProcessExceptionPort,
    ProcessAccessToken, ProcessLdtInformation,
    ProcessLdtSize, ProcessDefaultHardErrorMode,
    ProcessIoPortHandlers, ProcessPooledUsageAndLimits,
    ProcessWorkingSetWatch, ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup, ProcessPriorityClass,
    ProcessWx86Information, ProcessHandleCount,
    ProcessAffinityMask, ProcessPriorityBoost,
    ProcessDeviceMap, ProcessSessionInformation,
    ProcessForegroundInformation, ProcessWow64Information,
    MaxProcessInfoClass
  );
  PROCESSINFOCLASS = TProcessInfoClass;

  PPROCESS_BASIC_INFORMATION = ^PROCESS_BASIC_INFORMATION;

  PROCESS_BASIC_INFORMATION = packed record
    ExitStatus:         DWORD;
    PebBaseAddress:     Pointer;
    AffinityMask:       DWORD;
    BasePriority:       DWORD;
    UniqueProcessId:    DWORD;
    InheritedUniquePID: DWORD;
  end;

  TZwQueryInformationProcess = function(ProcessHandle: THandle;
                                   ProcessInformationClass: PROCESSINFOCLASS;
                                   var ProcessInformation: PROCESS_BASIC_INFORMATION;
                                   ProcessInformationLength: ULONG;
                                   var ReturnLength: ULONG): NTSTATUS; stdcall;

    function GetBuildInfo(full: boolean = false): AnsiString;
    Function InList(Valor: AnsiString; Lista: Tstrings):boolean;
    Function GetRandomString(Len: integer = 64):AnsiString;
    Procedure StartMapping;
    Function GetMatrix(Tipo: TRequestData; Mensagem: AnsiString; Valor: integer = 0): integer;
    Function GetColorLevel(WarnLevel: integer): TColor;
    function HexToInt(s: AnsiString): Longword;
    Procedure DissectNumber(N: double; var L: TStrings);
    Function Pot(Base, Expoente: double):double;
    function BinToInt(Value: AnsiString): Integer;
    Function IntToBin(n: integer): AnsiString;
    Function IsInt(s:AnsiString):boolean;
    //Function IsInt(valor: double): boolean; overload;
    //procedure Delay(msecs:integer);
    function GetHDCode : AnsiString;
    function GetVolumeSerial(Path: AnsiString): AnsiString;
    Function CMIntToHex(Valor: Int64; Formato: integer = 0):AnsiString;

    Function GetThreadIdFromProcname(procName: AnsiString): cardinal;
    Function GetThreadIdFromWindow(className: pchar; windowName: pchar): cardinal;
    //Function GetProcessIdFromFileName(fileName: string): cardinal;
    Function GetProcessIdFromProcname(procName: AnsiString; indice: integer): cardinal;
    Function GetProcessIdFromWindow(className: pchar; windowName: pchar): cardinal;
    function IsWindowsNT: Boolean;
    
    //Function GetProcessInfoNt(iProcId: Cardinal; DLLName: AnsiString = ''): TCMProcessInfo;
    //function GetProcessInfo9x(iProcId: Cardinal; DLLName: AnsiString = ''): TCMProcessInfo;
    function SystemDir: AnsiString;
    Procedure FixPath(var Path: AnsiString);
    Procedure printf(texto: AnsiString);
    Function GetRandomName(Size: integer):AnsiString;
    Function CheckValidPID(pid: cardinal): AnsiString;
    Function IntToByteArray(valor: int64): AnsiString;
    function CodeX(n: integer):boolean;
    Function CriaProcesso(Path:AnsiString; showtype:cardinal):boolean;
    Function CMPchar(s: AnsiString): PWideChar;
    Function FileNameWithoutExtension(nome: AnsiString): AnsiString;
    Function CMParamStr0: AnsiString;
    function HexToString(valor: AnsiString): AnsiString;
    Function stringToHex(valor: AnsiString):AnsiString;
    procedure Delay(dwMilliseconds: Longint);
    Function InjectDLLBase(proc: DWORD; dllName: PCHAR): boolean;
    //procedure Delay(tempo: Int64);
    //procedure Delay(dwMilliseconds: Longint);
    //function FloatToStr(Value: Extended; Formato: integer = 0): string; overload;

    const MapName = 'AD02992CEB206FF90';

var
  hMemFile: cardinal;
  MatrizInfo: TMatrizData;
  DebugLocation: Pointer;
  //Splash Screen
  SplashStatus: AnsiString;
  KeysOut: boolean = false;

  //Matriz DPR
  Antes: cardinal;
  Ordenagem: TListOrderType = OT_ByCreation;
  OrdemCrescente: Boolean;
  MatrizColor: TColor;

implementation

uses splashScreen;

procedure Delay(dwMilliseconds: Longint);
var
  iStart, iStop: DWORD;
begin
  iStart := GetTickCount;
  repeat
    iStop := GetTickCount;
    Application.ProcessMessages;
    Sleep(1); // addition from Christian Scheffler to avoid high CPU last
  until (iStop - iStart) >= dwMilliseconds;
end;

   {
procedure Delay(tempo: Int64);
var
  hrRes, hrT1, hrT2, dif: Int64;
begin
  if QueryPerformanceFrequency(hrRes) then
  begin
    QueryPerformanceCounter(hrT1);
    repeat
      QueryPerformanceCounter(hrT2);
      dif := (hrT2 - hrT1) * 10000000 div hrRes;
    until dif > tempo;
  end;
end;        }

function WindowsDirectory: AnsiString;
var
  WinDir: PChar;
begin
  WinDir := StrAlloc(MAX_PATH);
  GetWindowsDirectory(WinDir, MAX_PATH);
  Result := AnsiString(WinDir);
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  StrDispose(WinDir);
end;

Function CMParamStr0: AnsiString;
var
  TheFileName : array[0..MAX_PATH] of char;
begin
  FillChar(TheFileName, sizeof(TheFileName), #0);
  GetModuleFileName(hInstance, TheFileName, sizeof(TheFileName));
  result := AnsiString(TheFileName);
end;

Function FileNameWithoutExtension(nome: AnsiString): AnsiString;
var i:integer;
begin
    result := ExtractFilePath(nome);
    for i := length(result) downto 1 do
    begin
        if result[i] = '.' then
        begin
            result := copy(result, 1, i-1);
            exit;
        end;
    end;
end;

Function CMPchar(s: AnsiString): PWideChar;
begin
  result := AllocMem(length(s)+1);
  StringToWideChar(s, result, length(s)+2);
end;

Function IntToByteArray(valor: int64): AnsiString;
var x,y: byte;
begin

//{$I VMProtectBegin.inc}

    result := '';
    while valor > 0 do
    begin
        x := (valor and $FF );
        result := result + IntToHex(x, 2)+' ';

        if valor > $F then
           valor := valor shr 8
        else
           valor := valor shr 4;
    end;
//{$I VMProtectEnd.inc}
end;

Function GetRandomName(Size: integer):AnsiString;
var i,k,l: integer; v:boolean;
const Consoantes = 'bcdfghjklmnpqrstvwxz';
      Vogais = 'aeiouy';
begin

//{$I VMProtectBegin.inc}
  result := '';
  v := false;
  k := random(6)+5;
  for i:=1 to size do
  begin
      randomize;
      v := not v;
      l := random(3);
      if v then
        result := result + consoantes[random(length(consoantes)-1)+1]
      else
        result := result + vogais[random(length(vogais)-1)+1]
  end;

//{$I VMProtectEnd.inc}
end;

function CodeX(n: integer):boolean;
begin
    result := false;
    if n >=10 then
       result := true;
end;

function formatChar(valor: AnsiChar):AnsiString;
begin
   randomize;
   result := IntToHex(dword(valor),3);
   if(result[1] = '0') then result[1] := nh[random(19)+1];
   if(result[2] = '0') then result[2] := nh[random(19)+1];
   if(result[3] = '0') then result[3] := nh[random(19)+1]; 
end;

Function stringToHex(valor: AnsiString):AnsiString;
var i: integer;
begin
   result := '';
   for i:=1 to length(valor) do
   begin
      result := result + formatChar(valor[i]);
   end;
end;

function HexToString(valor: AnsiString): AnsiString;
var i: integer; s:AnsiString;
begin

//{$I VMProtectBegin.inc}
    s := '';
    for i := 1 to length(valor) do
    begin
        if( pos(valor[i], hexes) > 0)then
          s := s + valor[i]
        else
          s := s + '0';
    end;
    i := 1;
    result := '';
    while i < length(s) do
    begin
        result := result + AnsiChar(HexToInt(s[i]+s[i+1]+s[i+2]));
        inc(i,3);
    end;
//{$I VMProtectEnd.inc}
end;

(*****************************************************************************
*
*  01) NOME DA FUNÇAO --- printf
*
*  02) AÇÃO --- Imprime uma mensagem para fins de debug
*
*  03) PARÂMETROS --- TEXTO: Mensagem a ser impressa 
*
*****************************************************************************)

Procedure printf(texto: AnsiString);
begin
  {$IFDEF DEBUG6}
   if  DebugLocation <> nil then
   try
   TStrings(DebugLocation^).Add(texto);
   except
     on e:exception do
   end;
  {$ENDIF}
end;

{==============================================================================}
//        SystemDir - Pega diretório do sistema
{==============================================================================}

function SystemDir: AnsiString;
var
  dir: array [0..MAX_PATH] of Char;
begin
  GetSystemDirectory(dir, MAX_PATH);
  Result := StrPas(dir);
  FixPath(result);
end;

Procedure FixPath(var Path: AnsiString);
begin
   if Path[length(path)] <> '\' then  Path := Path + '\'
end;


Function CMIntToHex(Valor: Int64; Formato: integer = 0):AnsiString;
const Hexs: array[0..15] of AnsiChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var i:integer;
begin

//{$I VMProtectBegin.inc}

   result := '';
   while valor <> 0 do
   begin
       i := valor mod 16;
       valor := valor div 16;
       result := result + Hexs[i];
   end;
   if (formato > 0) and (length(result) > 0) then
      result := copy(result,1,formato);
//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//      Verifica se o texto forma um número (se é compativel com inttostr)
{==============================================================================}
Function IsInt(s:AnsiString):boolean;
var i:integer;
begin

//{$I VMProtectBegin.inc}

   result:=true;

   for i:=1 to length(s) do
   begin
       if  (not (s[i] in ['-','0'..'9'])) or ((s[i] = '-') and (i <> 1)) then
       begin
           result:=false;
           exit;
       end;
   end;
//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//                Calcula a potência de um número
{==============================================================================}
Function Pot(Base, Expoente: double):double;
begin
    if (base = 0) then
    begin
        result := 0;
        exit;
    end; 
    result := Exp(Expoente * Ln(Base))
end;

{==============================================================================}
//           Pega membros de uma combinação binária
{==============================================================================}
Procedure DissectNumber(N: double; var L: TStrings);
var
    i,j: integer;
begin

//{$I VMProtectBegin.inc}

  i := 0;
  L := TStringList.Create;
  L.Clear;
  while N > 0 do
  begin
      while N <= Pot(2,i) do
      begin
          inc(i);
      end;
      N := N - Pot(2,i-1);
      L.Add(FloatToStr(N));
      i := 0;
  end;
//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//                  Transforma de Haxadecimal para Decimal
{==============================================================================}

function HexToInt(s: AnsiString): Longword;
var
  b: Byte;
  c: AnsiChar;
begin

//{$I VMProtectBegin.inc}

  Result := 0;
  s := UpperCase(s);
  for b := 1 to Length(s) do
  begin
    Result := Result * 16;
    c := s[b];
    case c of
      '0'..'9': Inc(Result, Ord(c) - Ord('0'));
      'A'..'F': Inc(Result, Ord(c) - Ord('A') + 10);
      else
        raise EConvertError.Create('No Hex-Number');
    end;
  end;
//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//                  Transforma de Decimal para Binário
{==============================================================================}

Function IntToBin(n: integer): AnsiString;
{Converte um numero decimal em binário}
var
S: AnsiString;
i,j: integer;
begin
//{$I VMProtectBegin.inc}

result:='';
i:=n;
  while true do
  begin
     j:=i mod 2;
     result:=inttostr(j)+result;
     i:=i div 2;
     if i=0 then
       break;
  end;
  while(length(result)<7) do
    result:='0'+result;
//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//                  Transforma de Binário para Decimal
{==============================================================================}

function BinToInt(Value: AnsiString): Integer;
var
  i, iValueSize: Integer;
begin
  Result := 0;
  iValueSize := Length(Value);
  for i := iValueSize downto 1 do
    if Value[i] = '1' then Result := Result + (1 shl (iValueSize - i));
end;


{==============================================================================}
//           Retorna cor de acordo com o nivel de alerta - 1 a 8
{==============================================================================}

Function GetColorLevel(WarnLevel: integer): TColor;
var Valor: AnsiString;
begin
  result := clred;
  if WarnLevel = 0 then
     WarnLevel := 1;
  Valor:= '';
  Valor := '0000'+inttohex(16 - WarnLevel*2, 1)+'0FF';
  result := HexToInt(valor);
end;

{==============================================================================}
//                          Mapeia a memoria
{==============================================================================}

procedure StartMapping;
var currentdir:AnsiString;
begin
  {hMemFile := OpenFileMapping(FILE_MAP_WRITE, False, MapName);
  MatrizInfo := MapViewOfFile(hMemFile, FILE_MAP_WRITE, 0, 0, 0);
  if MatrizInfo = nil then
  begin
    hMemFile := CreateFileMappingA(MAXDWORD, nil, PAGE_READWRITE, 0, SizeOf(TMatrizData), MapName);
    MatrizInfo := MapViewOfFile(hMemFile, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    ZeroMemory(MatrizInfo, SizeOf(TMatrizData));
    currentdir := GetCurrentDir;
    //StrPCopy(MatrizInfo.MatrizDir, currentdir) ;
    MatrizInfo.Opens := 1;
    exit;
  end;
  inc(MatrizInfo.Opens);   }
end;

{-----------------------------------------------------------------------------}
//                          Gera uma random String
{-----------------------------------------------------------------------------}

Function GetRandomString(Len: integer = 64):AnsiString;
var s:AnsiString;
    i,j:integer;
begin
    Randomize;
    Result := '';
for j:= 1 to Len do
begin

    i := random(2);

    case i of
      0:
      begin
         s:= AnsiChar( 97 + random(26));
         if random(2) = 1 then
           s := UpperCase(s);
      end;
      1:
      begin
         s:= AnsiChar( 48 + random(10))
      end;
    end;
    result := result + s;
end;
end;

{-----------------------------------------------------------------------------}
//        Verifica se existe o Valor Informado na lista Informada
{-----------------------------------------------------------------------------}

Function InList(Valor: AnsiString; Lista: Tstrings):boolean;
var i:integer;
begin
//{$I VMProtectBegin.inc}

    Result := false;
    for i:= 0 to Lista.Count-1 do
    begin
        if trim(lowercase(Lista.Strings[i])) = trim(lowercase(valor)) then
        begin
           result := true;
           exit;
        end;
    end;
//{$I VMProtectEnd.inc}
end;

{-----------------------------------------------------------------------------}
//                       Versão do Programa
{-----------------------------------------------------------------------------}

function GetBuildInfo(full: boolean = false): AnsiString;
var V1,       // Major Version
    V2,       // Minor Version
    V3,       // Release
    V4: Word; // Build Number
    VerInfoSize, VerValueSize, Dummy : DWORD;
    VerInfo : Pointer;
    VerValue : PVSFixedFileInfo;
    FileNameVar : String;
    a,b,c: AnsiString;
begin

//{$I VMProtectBegin.inc}

   FileNameVar := ParamStr(0);
   VerInfoSize := GetFileVersionInfoSize(PChar(FileNameVar), Dummy);
   GetMem(VerInfo, VerInfoSize);
   GetFileVersionInfo(PChar(FileNameVar), 0, VerInfoSize, VerInfo);
   VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
   with VerValue^ do
     begin
     
        V1 := dwFileVersionMS shr 16;
        V2 := dwFileVersionMS and $FFFF;
        V3 := dwFileVersionLS shr 16;
        V4 := dwFileVersionLS and $FFFF;
     end;

   a := '';
   b := '';
   c := '';

   if(not full) then
   begin

   a := '.' + IntToStr(V2);

   if (V4 <> 0) and (V3 <> 0) then
      b := '.' + IntToStr(V3);

   if V4 <> 0 then
      c := '.' + IntToStr(V4);

   end else
   begin
      a := '.' + IntToStr(V2);
      b := '.' + IntToStr(V3);
      if(v4 <> 0) then
      c := '.' + IntToStr(V4);
   end;
      
   Result :=  IntToStr(V1) + a + b + c ;
   FreeMem(VerInfo, VerInfoSize);
//{$I VMProtectEnd.inc}
end;

{-----------------------------------------------------------------------------}
//                         Mensagem para Matriz
{-----------------------------------------------------------------------------}

Function GetMatrix(Tipo: TRequestData; Mensagem: AnsiString; Valor: integer = 0): integer;
var aCopyData: TCopyDataStruct;
    res: dword;
    Msg: TPluginRequest;
begin

//{$I VMProtectBegin.inc}
 if MatrizInfo.MatrizHandle >= 0 then
 begin
      //250 caracteres
      Msg.Tipo := Tipo;
      if length(Mensagem) > 250 then
      begin
          Msg.Menssagem := copy(Mensagem, 1, 250);
          Mensagem := copy(Mensagem, 251, length(Mensagem));
      end;
      //500 caracteres
      if length(Mensagem) > 250 then
      begin
          Msg.Complemento1 := copy(Mensagem, 1, 250);
          Mensagem := copy(Mensagem, 251, length(Mensagem));
      end;
      //750 caracteres
      if length(Mensagem) > 250 then
      begin
          Msg.Complemento2 := copy(Mensagem, 1, 250);
          Mensagem := copy(Mensagem, 251, length(Mensagem));
      end;
      //1000 caracteres
      if length(Mensagem) > 250 then
      begin
          Msg.Complemento3 := copy(Mensagem, 1, 250);
          Mensagem := copy(Mensagem, 251, length(Mensagem));
      end;
      //1250 caracteres
      if length(Mensagem) > 250 then
      begin
          Msg.Complemento4 := copy(Mensagem, 1, 250);
          Mensagem := copy(Mensagem, 251, length(Mensagem));
      end;
      //1500 caracteres
      if length(Mensagem) > 250 then
      begin
          Msg.Complemento5 := copy(Mensagem, 1, 250);
          Mensagem := copy(Mensagem, 251, length(Mensagem));
      end;

      Msg.Tipo := Tipo;

      with aCopyData do
      begin
        dwData := 0;
        cbData := sizeof(TPluginRequest);
        lpData := @Msg;
      end;
      SendMessageTimeout(MatrizInfo.MatrizHandle, WM_COPYDATA, 0, Longint(@aCopyData),SMTO_NORMAL or SMTO_ABORTIFHUNG,100,res);
  end;

//{$I VMProtectEnd.inc}
  //  SendMessage(MatrizInfo.MatrizHandle, PluginMessage, )
end;

{==============================================================================}
//                           Pega o codigo do HD
{==============================================================================}

function GetHDCode : AnsiString;
var resultado: tresults_array_dv;
begin
  getHardDriveComputerID(resultado);
  if length(resultado) > 0 then
    result := (trim(resultado[0].DriveSerialNumber))
  else
    result := 'padrao';
    //result := 'padrao';
(*
const IDENTIFY_BUFFER_SIZE = 512;
type
  TIDERegs = packed record
    bFeaturesReg     : BYTE; // Used for specifying SMART "commands".
    bSectorCountReg  : BYTE; // IDE sector count register
    bSectorNumberReg : BYTE; // IDE sector number register
    bCylLowReg       : BYTE; // IDE low order cylinder value
    bCylHighReg      : BYTE; // IDE high order cylinder value
    bDriveHeadReg    : BYTE; // IDE drive/head register
    bCommandReg      : BYTE; // Actual IDE command.
    bReserved        : BYTE; // reserved for future use.  Must be zero.
  end;
  TSendCmdInParams = packed record
    // Buffer size in bytes
    cBufferSize  : DWORD;
    // Structure with drive register values.
    irDriveRegs  : TIDERegs;
    // Physical drive number to send command to (0,1,2,3).
    bDriveNumber : BYTE;
    bReserved    : Array[0..2] of Byte;
    dwReserved   : Array[0..3] of DWORD;
    bBuffer      : Array[0..0] of Byte;  // Input buffer.
  end;
  TIdSector = packed record
    wGenConfig                 : Word;
    wNumCyls                   : Word;
    wReserved                  : Word;
    wNumHeads                  : Word;
    wBytesPerTrack             : Word;
    wBytesPerSector            : Word;
    wSectorsPerTrack           : Word;
    wVendorUnique              : Array[0..2] of Word;
    sSerialNumber              : Array[0..19] of CHAR;
    wBufferType                : Word;
    wBufferSize                : Word;
    wECCSize                   : Word;
    sFirmwareRev               : Array[0..7] of Char;
    sModelNumber               : Array[0..39] of Char;
    wMoreVendorUnique          : Word;
    wDoubleWordIO              : Word;
    wCapabilities              : Word;
    wReserved1                 : Word;
    wPIOTiming                 : Word;
    wDMATiming                 : Word;
    wBS                        : Word;
    wNumCurrentCyls            : Word;
    wNumCurrentHeads           : Word;
    wNumCurrentSectorsPerTrack : Word;
    ulCurrentSectorCapacity    : DWORD;
    wMultSectorStuff           : Word;
    ulTotalAddressableSectors  : DWORD;
    wSingleWordDMA             : Word;
    wMultiWordDMA              : Word;
    bReserved                  : Array[0..127] of BYTE;
  end;
  PIdSector = ^TIdSector;
  TDriverStatus = packed record
    // Error code from driver, or 0 if no error.
    bDriverError : Byte;
    // Contents of IDE Error register. Only valid when bDriverError is SMART_IDE_ERROR.
    bIDEStatus   : Byte;
    bReserved    : Array[0..1] of Byte;
    dwReserved   : Array[0..1] of DWORD;
  end;
  TSendCmdOutParams = packed record
    // Size of bBuffer in bytes
    cBufferSize  : DWORD;
    // Driver status structure.
    DriverStatus : TDriverStatus;
    // Buffer of arbitrary length in which to store the data read from the drive.
    bBuffer      : Array[0..0] of BYTE;
  end;

var hDevice : THandle;
    cbBytesReturned : DWORD;
    ptr : PChar;
    SCIP : TSendCmdInParams;
    aIdOutCmd : Array [0..(SizeOf(TSendCmdOutParams)+IDENTIFY_BUFFER_SIZE-1)-1] of Byte;
    IdOutCmd  : TSendCmdOutParams absolute aIdOutCmd;

  procedure ChangeByteOrder( var Data; Size : Integer );
  var ptr : PAnsiChar;
      i : Integer;
      c : AnsiChar;
  begin
    ptr := @Data;
    for i := 0 to (Size shr 1)-1 do
    begin
      c := ptr^;
      ptr^ := (ptr+1)^;
      (ptr+1)^ := c;
      Inc(ptr,2);
    end;
  end;

begin
//  result := 'WD-WXE309KA8460';
// exit;

  Result := ''; // return empty string on error
  if SysUtils.Win32Platform=VER_PLATFORM_WIN32_NT then // Windows NT, Windows 2000
    begin
      // warning! change name for other drives: ex.: second drive '\\.\PhysicalDrive1\'
      hDevice := CreateFile( '\\.\PhysicalDrive0', GENERIC_READ or GENERIC_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
    end
  else // Version Windows 95 OSR2, Windows 98
    hDevice := CreateFile( '\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0 );
  if hDevice=INVALID_HANDLE_VALUE then Exit;
  try
    FillChar(SCIP,SizeOf(TSendCmdInParams)-1,#0);
    FillChar(aIdOutCmd,SizeOf(aIdOutCmd),#0);
    cbBytesReturned := 0;
    // Set up data structures for IDENTIFY command.
    with SCIP do
    begin
      cBufferSize  := IDENTIFY_BUFFER_SIZE;
//      bDriveNumber := 0;
      with irDriveRegs do
      begin
        bSectorCountReg  := 1;
        bSectorNumberReg := 1;
//      if Win32Platform=VER_PLATFORM_WIN32_NT then bDriveHeadReg := $A0
//      else bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
        bDriveHeadReg    := $A0;
        bCommandReg      := $EC;
      end;
    end;
    if not DeviceIoControl( hDevice, $0007c088, @SCIP, SizeOf(TSendCmdInParams)-1,
      @aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, nil ) then Exit;
  finally
    CloseHandle(hDevice);
  end;
  with PIdSector(@IdOutCmd.bBuffer)^ do
  begin
    ChangeByteOrder( sSerialNumber, SizeOf(sSerialNumber) );
    (PAnsiChar(@sSerialNumber)+SizeOf(sSerialNumber))^ := #0;
    Result := trim(strpas(PAnsiChar(@sSerialNumber)));
  end;
   *)
end;

{==============================================================================}
//                       Delay = Sleep sem travar
{==============================================================================

procedure Delay(msecs:integer);
var
  iStart, iStop: DWORD;
begin
  iStart := GetTickCount;
  repeat
    iStop := GetTickCount;
    Application.ProcessMessages;
  until (iStop - iStart) >= msecs;
end;  }

{==============================================================================}
//               Pega o códico serial da partiçao
{==============================================================================}

function GetVolumeSerial(Path: AnsiString): AnsiString;
var
  VolumeSerialNumber: DWord;
  MaxCompLength: DWord;
  FileSysNameSize: DWord;
  FileSysNameBuffer: array [0..500] of Char;
  FileSystemFlags: DWord;
begin

//{$I VMProtectBegin.inc}
  if GetVolumeInformation(PChar(Path),nil,2,@VolumeSerialNumber,MaxCompLength,FileSystemFlags,
       FileSysNameBuffer,FileSysNameSize) then begin
    Result:=IntToHex(VolumeSerialNumber,8);
  end;

//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//               Pega o ID do processo pelo nome da janela
{==============================================================================}

Function GetProcessIdFromWindow(className: pchar; windowName: pchar): cardinal;
var procID: cardinal;
    targetWnd: HWND;
begin
    targetWnd := FindWindow(className, windowName);
    GetWindowThreadProcessId(targetWnd, @procId);
    result := procID;
end;

{==============================================================================}
//               Pega o ID do processo pelo nome do processo
{==============================================================================}

Function GetProcessIdFromProcname(procName: AnsiString; indice: integer): cardinal;
var pe: TProcessEntry32;
    thSnapshot: THandle;
    retval,ProcFound: boolean;
    total, j, n: integer;
    processosSistema: array[1..10] of cardinal;
    filhoDoSistema: boolean;
    cbRet: cardinal;
    handle: cardinal;

    {H: THandle;
    ZwQueryInformationProcess: TZwQueryInformationProcess;
    PBI: PROCESS_BASIC_INFORMATION;
    i: ULONG;
    FReady : NTSTATUS; }
    //PEB: TPeb;
begin

  if (indice = 0) then
    indice := 1;

    result := 0;
    total := 0;
   thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

   try
       if(thSnapshot = INVALID_HANDLE_VALUE) then
       begin
          result := 0;
          exit;
       end;

       pe.dwSize := sizeof(PROCESSENTRY32);

       retval := Process32First(thSnapshot, pe);

       n := 0;
       filhoDoSistema := false;
       while(retval) do
       begin
          //inc(n);
          //if(n <= 10) then
          //  processosSistema[n] = pe.th32ProcessID;

          {OutputDebugString(pe.szExeFile);
          //OutputDebugString(PChar(IntToStr(pe.dwSize)));
          //OutputDebugString(PChar(IntToStr(pe.cntUsage)));
          OutputDebugString(PChar(IntToStr(pe.th32ProcessID)));
          //OutputDebugString(PChar(IntToStr(pe.th32DefaultHeapID)));
          //OutputDebugString(PChar(IntToStr(pe.th32ModuleID)));
          OutputDebugString(PChar(IntToStr(pe.cntThreads)));
          OutputDebugString(PChar(IntToStr(pe.th32ParentProcessID)));
          //OutputDebugString(PChar(IntToStr(pe.pcPriClassBase)));
          //OutputDebugString(PChar(IntToStr(pe.dwFlags)));
          OutputDebugString('-------------------------');   }
          //OutputDebugString( PChar(String('['+IntToHex(pe.th32ProcessID,4)+'] '+pe.szExeFile+' ('+IntToStr(total)+')')) );
          if(LowerCase(Trim(pe.szExeFile)) = LowerCase(Trim(procName)) )  then
          begin
             {for j := 1 to 10 do
                 if pe.th32ParentProcessID = processosSistema[j] then
                 begin
                    filhoDoSistema := true;
                    break;
                 end;
             if not filhoDoSistema then
             begin
                 ProcFound := true;
                 inc(total);
                 result := pe.th32ProcessID;
             end;   }
                ProcFound := true;
                inc(total);
                result := pe.th32ProcessID;


                //OutputDebugString( PChar(String(pe.szExeFile+' - '+IntToHex(pe.th32ProcessID,8)+' ('+IntToStr(total)+')')) );
                //OutputDebugString( PChar(String('['+IntToHex(pe.th32ProcessID,4)+'] '+pe.szExeFile+' ('+IntToStr(total)+')')) );

                {H := LoadLibrary(ntdll);
                if H = 0 then RaiseLastOSError;
                try
                  @ZwQueryInformationProcess := GetProcAddress(H, 'ZwQueryInformationProcess');
                finally
                     FreeLibrary(H);
                end;

                if ZwQueryInformationProcess <> nil then
                begin
                  handle := OpenProcess(PROCESS_ALL_ACCESS, 0, pe.th32ProcessID);
                  ZeroMemory(@PBI, SizeOf(PBI));
                  FReady := ZwQueryInformationProcess(handle, ProcessBasicInformation, PBI, SizeOf(PBI), i) = 0;
                  pbi.PebBaseAddress
                end else
                  RaiseLastOSError; }
          end;

          retval    := Process32Next(thSnapshot, pe);
          pe.dwSize := sizeof(PROCESSENTRY32);
       end;

       if not ((ProcFound) and (total = indice)) then
         result := 0;
  finally
     CloseHandle(thSnapshot);
  end;
//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//               Pega o ID do processo pelo nome do processo
{==============================================================================}

Function CheckValidPID(pid: cardinal): AnsiString;
var pe: TProcessEntry32;
    thSnapshot: THandle;
    retval,ProcFound: boolean;
begin


//{$I VMProtectBegin.inc}
  result := '';
  try
     thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

     if(thSnapshot = INVALID_HANDLE_VALUE) then
        exit;

     pe.dwSize := sizeof(PROCESSENTRY32);

     retval := Process32First(thSnapshot, pe);
   
     while(retval) do
     begin
        if pe.th32ProcessID = pid then
        begin
            result := LowerCase(Trim(pe.szExeFile));
            exit;
        end;

        retval    := Process32Next(thSnapshot, pe);
        pe.dwSize := sizeof(PROCESSENTRY32);
     end;
  finally
     CloseHandle(thSnapshot);
  end;
//{$I VMProtectEnd.inc}

end;

{==============================================================================}
//               Pega o ID do processo pelo nome do arquivo
{==============================================================================}

{Function GetProcessIdFromFileName(fileName: AnsiString): cardinal;
var pe: TProcessEntry32;
    thSnapshot: THandle;
    retval,ProcFound: boolean;
begin
   thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

   if(thSnapshot = INVALID_HANDLE_VALUE) then
   begin
      result := 0;
      exit;
   end;

   pe.dwSize := sizeof(PROCESSENTRY32);

   retval := Process32First(thSnapshot, pe);

   while(retval) do
   begin

      if(LowerCase(Trim(pe.szExeFile)) = LowerCase(Trim(fileName)) )  then
      begin
         ProcFound := true;
         break;
      end;

      retval    := Process32Next(thSnapshot, pe);
      pe.dwSize := sizeof(PROCESSENTRY32);
   end;

   result := pe.th32ProcessID;
end;  }

{==============================================================================}
//               Pega o ID do thread pelo nome da janela
{==============================================================================}

Function GetThreadIdFromWindow(className: pchar; windowName: pchar): cardinal;
var targetWnd: HWND;
    hProcess: THandle;
    processId, pTID, threadID, x: cardinal;
begin

//{$I VMProtectBegin.inc}
    targetWnd := FindWindow(className, windowName);
    GetWindowThreadProcessId(targetWnd, @processId);

    asm
      mov eax, fs:[$18]
      add eax, 36
      mov [pTID], eax
    end;

    hProcess := OpenProcess(PROCESS_VM_READ, false, processID);
    ReadProcessMemory(hProcess, ptr(pTID), @threadID, 4, x);
    CloseHandle(hProcess);

    result := threadID;

//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//               Pega o ID do thread pelo nome do processo
{==============================================================================}

Function GetThreadIdFromProcname(procName: AnsiString): cardinal;
var pe: TProcessEntry32;
    thSnapshot, hProcess: THandle;
    retval, ProcFound: Boolean;
    pTID, threadID, x: cardinal;
begin

//{$I VMProtectBegin.inc}
   ProcFound := false;
   retval := false;

   thSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
   try
       if(thSnapshot = INVALID_HANDLE_VALUE) then
       begin
          //MessageBox(0, 'Error: unable to create toolhelp snapshot', 'Loader', 0);
          result := 0;
          exit;
       end;

       pe.dwSize := sizeof(PROCESSENTRY32);

       retval := Process32First(thSnapshot, pe);
       procName := LowerCase(Trim(procName));
   
       while(retval) do
       begin
          if LowerCase(Trim(pe.szExeFile)) = procName then
          begin
             ProcFound := true;
             break;
          end;

          retval    := Process32Next(thSnapshot, pe);
          pe.dwSize := sizeof(PROCESSENTRY32);
       end;

       if not ProcFound then
       begin
           result := 0;
       end;
   finally
      CloseHandle(thSnapshot);
   end;

   asm
      mov eax, fs:[$18]
      add eax, 36
      mov [pTID], eax
   end;

   hProcess := OpenProcess(PROCESS_VM_READ, false, pe.th32ProcessID);
   ReadProcessMemory(hProcess, ptr(pTID), @threadID, 4, x);
   CloseHandle(hProcess);

   result := threadID;

//{$I VMProtectEnd.inc}
end;

{==============================================================================}
//                        Verifica se é Windows NT
{==============================================================================}

function IsWindowsNT: Boolean;
begin
  Result:=Win32Platform = VER_PLATFORM_WIN32_NT;
end;

Function CriaProcesso(Path:AnsiString; showtype:cardinal):boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Diretorio: String;
begin

Diretorio := path;
//{$I VMProtectBegin.inc}
result:=true;
FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb          := SizeOf(StartupInfo);
  StartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOWNORMAL;
if not CreateProcess(nil,
    pchar(Diretorio), // pointer to command line AnsiString
    nil, // pointer to process security attributes
    nil, // pointer to thread security attributes
    False, // handle inheritance flag
    showtype or // creation flags
    HIGH_PRIORITY_CLASS,// NORMAL_PRIORITY_CLASS,
    nil, //pointer to new environment block
    nil, // pointer to current directory name
    StartupInfo, // pointer to STARTUPINFO
    ProcessInfo) then
    begin
      result:=false;
    end;

//{$I VMProtectEnd.inc}
end;

Function InjectDLLBase(proc: DWORD; dllName: PCHAR): boolean;
var
   buf: array[0..50] of char;
   RemoteString, LoadLibAddy: Pointer;
   writen: DWORD;
   theadCriado: THandle;
begin
   if(proc = 0) then
   begin
	    result := false;
      exit;
   end;

   LoadLibAddy := GetProcAddress(GetModuleHandle('kernel32.dll'), 'LoadLibraryA');
   RemoteString := VirtualAllocEx(Proc, nil, strlen(dllName), MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
   WriteProcessMemory(Proc, (RemoteString), dllName, strlen(dllName), writen);
   theadCriado := CreateRemoteThread(Proc, nil, 0, (LoadLibAddy), (RemoteString), 0, writen);

   result := (theadCriado <> 0);
end;



//initialization
//  PsapiDir := ExtractFilePath(ParamStr(0))+'\Libs\PsApi.dll';
end.
 